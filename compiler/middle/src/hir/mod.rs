pub mod attr;
pub mod check;
mod error;
pub mod errors;
mod scope;
mod syntax;
mod ty;

use {
    crate::{
        hir::{attr::MetaItem, scope::LoopData},
        idx::IndexVec,
        index_vec, man,
        mir::{
            self,
            mono::{Linkage, Visibility},
            ty::Abi,
            visit::{MutVisitor, TyContext, Visitor},
            CastKind, CodegenUnit, ConstValue, Infer, InstanceData, InstanceDef, Local, LocalDecl,
            Location, MonoItem, MonoItemData, Mutability, Operand, Place, PlaceElem, Rvalue,
            ScalarRepr, SourceInfo, Statement, StatementKind, SwitchTargets, Terminator,
            TerminatorKind, TyKind,
        },
        pretty::{FmtPrinter, Print, Printer},
        sess::ModuleType,
        sym,
        symbol::{Ident, Symbol},
        FxHashMap, Result, Span, Tx,
    },
    ::errors::{
        ariadne::{Color, Fmt},
        color, DiagnosticMessage, Handler, Level,
    },
    lexer::{BinOp, Lit, LitBool, LitInt, ReturnType, Spanned},
    smallvec::SmallVec,
    std::{collections::hash_map::Entry, fmt, iter, marker::PhantomData, mem, num::NonZeroU8},
};
pub use {scope::Scope, syntax::UnOp, ty::Ty};

#[derive(Debug, Copy, Clone)]
pub struct ForeignItem<'hir> {
    pub sig: FnSig<'hir>,
    pub span: Span,
}

#[derive(Debug, Copy, Clone)]
pub enum ItemKind<'hir> {
    Fn(FnSig<'hir>, &'hir [Stmt<'hir>]),
    Foreign { abi: Abi, items: &'hir [ForeignItem<'hir>] },
}

#[derive(Debug, Clone)]
pub struct Item<'hir> {
    pub attrs: &'hir [MetaItem],
    pub kind: ItemKind<'hir>,
    pub span: Span,
}

fn intern_meta<'tcx>(tcx: Tx<'tcx>, attrs: Vec<lexer::Attribute>) -> &'tcx [MetaItem] {
    tcx.arena.attrs.alloc_from_iter(
        attrs.into_iter().map(|attr| MetaItem::from_parse(attr.meta).expect("incompatible meta")),
    )
}

impl<'hir> Item<'hir> {
    pub fn analyze(tcx: Tx<'hir>, item: lexer::Item<'hir>) -> Self {
        use lexer::{ForeignMod, ItemFn};

        let meta;
        let span = item.span();
        let kind =
            match item {
                lexer::Item::Fn(ItemFn { sig, block, attrs }) => {
                    meta = intern_meta(tcx, attrs);
                    let sig = FnSig::analyze(tcx, &sig);
                    let stmts = tcx
                        .arena
                        .stmt
                        .alloc_from_iter(block.stmts.iter().map(|stmt| Stmt::analyze(tcx, stmt)));
                    item::Fn(sig, stmts)
                }
                lexer::Item::Foreign(ForeignMod { attrs, abi: _abi, items, .. }) => {
                    meta = intern_meta(tcx, attrs);
                    item::Foreign {
                        abi: Abi::C,
                        items: tcx.arena.ffi.alloc_from_iter(items.into_iter().map(|f| {
                            ForeignItem { sig: FnSig::analyze(tcx, &f.sig), span: f.span() }
                        })),
                    }
                }
            };

        Self { attrs: meta, kind, span }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ExprKind<'hir> {
    Lit(Lit<'hir>),
    Local(Ident),
    Unary(UnOp, &'hir Expr<'hir>),
    Binary(BinOp, &'hir Expr<'hir>, &'hir Expr<'hir>),
    Call(Ident, &'hir [Expr<'hir>]),
    Block(&'hir [Stmt<'hir>], Span),
    If(&'hir Expr<'hir>, &'hir Expr<'hir>, Option<&'hir Expr<'hir>>),
    Return(Option<&'hir Expr<'hir>>),
    Break(Option<&'hir Expr<'hir>>),
    Loop(&'hir [Stmt<'hir>], Span),
    Assign(&'hir Expr<'hir>, &'hir Expr<'hir>),
    AddrOf(Mutability, &'hir Expr<'hir>),
}

#[derive(Debug, Clone)]
pub struct Expr<'tcx> {
    pub kind: ExprKind<'tcx>,
    pub span: Span,
}

fn analyze_block<'tcx>(tcx: Tx<'tcx>, block: &lexer::Block<'tcx>) -> Expr<'tcx> {
    Expr {
        kind: expr::Block(
            tcx.arena.stmt.alloc_from_iter(block.stmts.iter().map(|stmt| Stmt::analyze(tcx, stmt))),
            block.span(),
        ),
        span: block.span(),
    }
}

impl<'tcx> Expr<'tcx> {
    pub fn analyze(tcx: Tx<'tcx>, expr: &lexer::Expr<'tcx>) -> Self {
        use lexer::expr::{
            Assign, Binary, Block, Break, Call, If, Loop, Paren, Reference, Return, TailCall, Unary,
        };

        let span = expr.span();
        let kind = match expr {
            lexer::Expr::Lit(lit) => expr::Lit(*lit),
            lexer::Expr::Paren(Paren { expr, .. }) => return Self::analyze(tcx, expr),
            lexer::Expr::Unary(Unary { op, expr }) => {
                expr::Unary(UnOp::from_parse(*op), tcx.arena.expr.alloc(Self::analyze(tcx, expr)))
            }
            lexer::Expr::Binary(Binary { left, op, right }) => expr::Binary(
                *op,
                tcx.arena.expr.alloc(Self::analyze(tcx, left)),
                tcx.arena.expr.alloc(Self::analyze(tcx, right)),
            ),
            lexer::Expr::Ident(str) => {
                expr::Local(Ident::new(Symbol::intern(str.ident()), str.span))
            }
            lexer::Expr::Call(Call { func, args, .. }) => {
                let box lexer::Expr::Ident(func) = func else { todo!() };
                expr::Call(
                    Ident::from_parse(*func),
                    tcx.arena
                        .expr
                        .alloc_from_iter(args.iter().map(|expr| Self::analyze(tcx, expr))),
                )
            }
            lexer::Expr::TailCall(TailCall { receiver, func, args, .. }) => {
                let recv = Self::analyze(tcx, receiver);
                expr::Call(
                    Ident::from_parse(*func),
                    if let Some((_, args)) = args {
                        tcx.arena.expr.alloc_from_iter(
                            iter::once(recv)
                                .chain(args.iter().map(|expr| Self::analyze(tcx, expr))),
                        )
                    } else {
                        tcx.arena.expr.alloc_from_iter([recv])
                    },
                )
            }
            lexer::Expr::Block(block) => return analyze_block(tcx, block),
            lexer::Expr::If(If { cond, then_branch, else_branch, .. }) => expr::If(
                tcx.arena.expr.alloc(Self::analyze(tcx, cond)),
                tcx.arena.expr.alloc(analyze_block(tcx, then_branch)),
                else_branch
                    .as_ref()
                    .map(|(_, block)| &*tcx.arena.expr.alloc(analyze_block(tcx, block))),
            ),
            lexer::Expr::Return(Return { expr, .. }) => expr::Return(
                expr.as_ref().map(|expr| &*tcx.arena.expr.alloc(Self::analyze(tcx, expr))),
            ),
            lexer::Expr::Break(Break { expr, .. }) => expr::Break(
                expr.as_ref().map(|expr| &*tcx.arena.expr.alloc(Self::analyze(tcx, expr))),
            ),
            lexer::Expr::Loop(Loop { loop_token, body: Block { stmts, .. } }) => expr::Loop(
                tcx.arena.stmt.alloc_from_iter(stmts.iter().map(|stmt| Stmt::analyze(tcx, stmt))),
                loop_token.span,
            ),
            lexer::Expr::Assign(Assign { left, right, .. }) => expr::Assign(
                tcx.arena.expr.alloc(Self::analyze(tcx, left)),
                tcx.arena.expr.alloc(Self::analyze(tcx, right)),
            ),
            lexer::Expr::Reference(Reference { mutability, expr, .. }) => expr::AddrOf(
                Mutability::from_bool(mutability.is_some()),
                tcx.arena.expr.alloc(Self::analyze(tcx, expr)),
            ),
        };
        Self { kind, span }
    }
}

#[derive(Debug, Clone)]
pub struct LocalStmt<'tcx> {
    pub mutability: Mutability,
    pub pat: Ident,
    pub init: &'tcx Expr<'tcx>,
}

#[derive(Debug, Clone)]
pub enum StmtKind<'tcx> {
    Local(LocalStmt<'tcx>),
    Expr(&'tcx Expr<'tcx>, /* no semi */ bool),
}

#[derive(Debug, Clone)]
pub struct Stmt<'tcx> {
    pub kind: StmtKind<'tcx>,
    pub span: Span,
}

impl<'tcx> Stmt<'tcx> {
    pub fn analyze(tcx: Tx<'tcx>, stmt: &lexer::Stmt<'tcx>) -> Self {
        let span = stmt.span();
        let kind = match stmt {
            lexer::Stmt::Local(local) => StmtKind::Local(LocalStmt {
                mutability: Mutability::from_bool(local.mutability.is_some()),
                pat: Ident::new(Symbol::intern(local.pat.ident()), local.pat.span),
                init: tcx.arena.expr.alloc(Expr::analyze(tcx, &local.expr)),
            }),
            lexer::Stmt::Expr(expr, semi) => {
                StmtKind::Expr(tcx.arena.expr.alloc(Expr::analyze(tcx, expr)), semi.is_none())
            }
        };
        Self { kind, span }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum FnRetTy<'hir> {
    Default(Span),
    Return(Ty<'hir>),
}

impl<'hir> FnRetTy<'hir> {
    pub fn ty(&self, tcx: Tx<'hir>) -> Ty<'hir> {
        match *self {
            FnRetTy::Default(span) => Ty::new(span, tcx.types.unit),
            FnRetTy::Return(ty) => ty,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FnDecl<'hir> {
    pub name: Symbol,
    pub inputs: &'hir [(Mutability, Symbol, Ty<'hir>)],
    pub output: FnRetTy<'hir>,
}

#[derive(Debug, Copy, Clone)]
pub struct FnSig<'hir> {
    pub abi: Abi,
    pub decl: FnDecl<'hir>,
    pub span: Span,
}

impl<'hir> FnSig<'hir> {
    #[deprecated(note = "temp solution")]
    pub fn ret_span(&self) -> Span {
        match self.decl.output {
            FnRetTy::Default(span) => span,
            FnRetTy::Return(ty) => ty.span,
        }
    }
}

fn analyze_hir_ty<'tcx>(tcx: Tx<'tcx>, ty: &lexer::Type<'tcx>) -> Ty<'tcx> {
    Ty::new(ty.span(), mir::Ty::analyze(tcx, ty))
}

impl<'tcx> FnDecl<'tcx> {
    pub fn analyze(tcx: Tx<'tcx>, sig: &lexer::Signature<'tcx>) -> Self {
        Self {
            name: Symbol::intern(sig.ident.ident()),
            inputs: tcx.arena.dropless.alloc_from_iter(sig.inputs.iter().map(|arg| {
                (
                    Mutability::from_bool(arg.mutability.is_some()),
                    Symbol::intern(arg.pat.ident()),
                    analyze_hir_ty(tcx, &arg.ty),
                )
            })),
            output: match &sig.output {
                ReturnType::Default => FnRetTy::Default({
                    // Let the place of return type be implied just next of parentheses
                    let Span { start, end, .. } = sig.paren.span.rt;
                    Span::new(start + 1, end + 1)
                }),
                ReturnType::Type(_, ty) => FnRetTy::Return(analyze_hir_ty(tcx, ty)),
            },
        }
    }
}

impl<'tcx> FnSig<'tcx> {
    pub fn analyze(tcx: Tx<'tcx>, sig: &lexer::Signature<'tcx>) -> Self {
        Self {
            abi: if sig.abi.is_some() { Abi::C } else { Abi::Zxc },
            decl: FnDecl::analyze(tcx, sig),
            span: sig.span(),
        }
    }
}

pub mod stmt {
    pub use super::StmtKind::*;
}

pub mod expr {
    pub use super::ExprKind::*;
}

pub mod item {
    pub use super::ItemKind::*;
}

#[derive(Debug)]
struct AnalyzeCx<'mir, 'hir> {
    tcx: Tx<'hir>,
    hix: &'mir HirCtx<'hir>,
    block: mir::BasicBlockData<'hir>,
    body: &'mir mut mir::Body<'hir>,
    scope: Option<&'hir mut Scope<'hir>>,

    vars: IndexVec<mir::InferId, Option<mir::Ty<'hir>>>,
    err: &'mir mut error::TyErrCtx,
}

impl<'mir, 'hir> AnalyzeCx<'mir, 'hir> {
    pub fn local_decls(&self) -> &mir::LocalDecls<'hir> {
        &self.body.local_decls
    }

    pub fn next_infer_var(&mut self) -> mir::InferId {
        self.vars.push(None)
    }

    pub fn scope(&mut self) -> &mut Scope<'hir> {
        self.scope.as_mut().expect("calling `enter_scope` without `exit_scope`")
    }

    fn enter_scope(&mut self) {
        let parent = self.scope.take().unwrap();
        let returned = parent.returned;
        let sig = parent.sig;
        let looped = parent.looped.clone();
        self.scope = Some(self.tcx.arena.scope.alloc(Scope {
            parent: Some(parent),
            looped,
            returned,
            sig,
            ..Default::default()
        }));
    }

    fn exit_scope(&mut self) {
        let scope = self.scope();

        let parent = scope.parent.take().unwrap();
        parent.looped = scope.looped.take();
        self.scope = Some(parent);
    }

    pub fn scoped<T>(&mut self, scope: impl FnOnce(&mut Self) -> T) -> T {
        self.enter_scope();
        let ret = scope(self);
        self.exit_scope();
        ret
    }

    pub fn end_of_block(
        &mut self,
        span: Span,
        terminator: TerminatorKind<'hir>,
    ) -> mir::BasicBlock {
        self.block.terminator =
            Some(Terminator { source_info: SourceInfo { span }, kind: terminator });

        let block = mem::take(&mut self.block);
        // note that the index of the next block is returned here.
        // Since this is usually what is interesting when we are guaranteed
        // to end a block with a terminator
        self.body.basic_blocks.push(block) + 1
    }

    pub fn end_of_block_dummy(&mut self) -> mir::BasicBlock {
        self.block.terminator = None;

        let block = mem::take(&mut self.block);
        self.body.basic_blocks.push(block)
    }

    pub fn current_block(&self) -> mir::BasicBlock {
        mir::BasicBlock::new(self.body.basic_blocks.len())
    }

    pub fn next_block(&self) -> mir::BasicBlock {
        mir::BasicBlock::new(self.body.basic_blocks.len() + 1)
    }

    pub fn assign_rvalue(&mut self, place: Place<'hir>, val: Rvalue<'hir>, span: Span) {
        #[cfg(debug_assertions)]
        {
            let tcx = self.tcx;
            if let Rvalue::Use(operand) = &val
                && operand.ty(&self.body.local_decls, tcx) == tcx.types.never
            {
                // try avoiding `never` assigns
                panic!("I don't think it's a good idea to allow it to be assigned to something")
            }
        }
        self.block.statements.push(Statement {
            source_info: SourceInfo { span },
            kind: StatementKind::Assign(place, val),
        })
    }

    pub fn push_rvalue(
        &mut self,
        ty: Ty<'hir>,
        rvalue: Rvalue<'hir>,
        mutability: Mutability,
        span: Span,
    ) -> (Ty<'hir>, Place<'hir>) {
        let place = Place::pure(self.body.local_decls.push(LocalDecl { mutability, ty: ty.kind }));
        self.block.statements.push(Statement {
            source_info: SourceInfo { span },
            kind: StatementKind::Assign(place, rvalue),
        });
        (ty, place)
    }

    pub fn push_temp_rvalue(
        &mut self,
        ty: Ty<'hir>,
        rvalue: Rvalue<'hir>,
        span: Span,
    ) -> (Ty<'hir>, Operand<'hir>) {
        let (ty, place) = self.push_rvalue(ty, rvalue, Mutability::Not, span);
        (ty, Operand::Copy(place))
    }

    pub fn typed_place(&mut self, ty: mir::Ty<'hir>) -> Place<'hir> {
        Place::pure(self.body.local_decls.push(LocalDecl { mutability: Mutability::Not, ty }))
    }

    pub fn unit_place(&mut self, span: Span) -> (Ty<'hir>, Operand<'hir>) {
        let unit = Ty { kind: self.tcx.types.unit, span };
        (unit, Operand::Const(ConstValue::Zst, unit.kind))
    }

    pub fn never_place(&mut self, span: Span) -> (Ty<'hir>, Operand<'hir>) {
        let never = Ty { kind: self.tcx.types.never, span };
        (never, Operand::Const(ConstValue::Zst, never.kind))
    }

    pub fn unit_const(&mut self, unit: Ty<'hir>) -> Operand<'hir> {
        Operand::Const(ConstValue::Zst, unit.kind)
    }

    pub fn bool(&self, discr: bool) -> Operand<'hir> {
        Operand::Const(
            ConstValue::Scalar(if discr { ScalarRepr::TRUE } else { ScalarRepr::FALSE }),
            self.tcx.types.bool,
        )
    }

    /// # Safety
    /// There's really nothing dangerous about it, but let `unsafe`
    /// emphasize the fine line of using the `never` type
    unsafe fn assert_same_types_allow_never(
        &mut self,
        a: Ty<'hir>,
        b: Ty<'hir>,
    ) -> Result<Ty<'hir>> {
        Ok(match (a.is_never(), b.is_never()) {
            (true, false) => b,
            (false, true) => a,
            _ => self.assert_same_types(a, b)?,
        })
    }

    pub fn infer_same_types(&mut self, a: Ty<'hir>, b: Ty<'hir>) -> Result<Ty<'hir>> {
        use mir::{Infer::Int, TyKind::Infer};

        let error_report =
            |a, b| errors::TypeMismatch { expect: self.hix.ty_msg(a), found: self.hix.ty_msg(b) };

        let mut infer = |var, to: Ty<'hir>, from: Ty<'hir>| {
            if let Some(ty) = self.vars[var] {
                self.assert_same_types(to, from.map(|_| ty)) // todo: can it cause stack overflow?
            } else {
                self.vars[var] = Some(to.kind);
                Ok(to)
            }
        };

        match (a.kind(), b.kind()) {
            (Infer(Int(_)), Infer(Int(var))) => {
                if self.vars[var].is_some() {
                    Err(self.err.emit(errors::InferInt { span: a.span }))
                } else {
                    self.vars[var] = Some(a.kind);
                    Ok(a)
                }
            }
            (TyKind::Int(_), Infer(Int(var))) => infer(var, a, b),
            (Infer(Int(var)), TyKind::Int(_)) => infer(var, b, a),
            _ => Err(self.err.emit(error_report(a, b))), // has no infer
        }
    }

    pub fn assert_same_types(&mut self, a: Ty<'hir>, b: Ty<'hir>) -> Result<Ty<'hir>> {
        let error_report =
            |a, b| errors::TypeMismatch { expect: self.hix.ty_msg(a), found: self.hix.ty_msg(b) };

        if a.needs_infer() || b.needs_infer() {
            return self.infer_same_types(a, b);
        }

        if a == b { Ok(a) } else { Err(self.err.emit(error_report(a, b))) }
    }
}

fn quoted(f: impl fmt::Display, color: Color) -> String {
    format!("`{f}`").fg(color).to_string()
}

fn goto_next(acx: &mut AnalyzeCx<'_, '_>) -> TerminatorKind<'static> {
    TerminatorKind::Goto { target: acx.next_block() }
}

/// Lazy mir operand
// type Lazy<'hir> =
//      impl FnOnce(&mut AnalyzeCx<'_, 'hir>) -> Result<(Ty<'hir>, Operand<'hir>)>;

macro_rules! lazy {
    (|$acx:ident| $expr:expr) => {{ (|$acx: &mut AnalyzeCx<'_, 'hir>| $expr) }};
}

// TODO: add opaque alias
fn analyze_branch_expr<'hir>(
    acx: &mut AnalyzeCx<'_, 'hir>,
    cond: &'hir Expr<'hir>,
    then_expr: impl FnOnce(&mut AnalyzeCx<'_, 'hir>) -> Result<(Ty<'hir>, Operand<'hir>)>,
    else_expr: Option<impl FnOnce(&mut AnalyzeCx<'_, 'hir>) -> Result<(Ty<'hir>, Operand<'hir>)>>,
) -> Result<(Ty<'hir>, Operand<'hir>)> {
    let (ty, discr) = analyze_expr(acx, cond)?;

    let bool = acx.tcx.types.bool;
    acx.assert_same_types(Ty::new(cond.span, bool), ty)?;

    acx.scoped(|acx| {
        let ret_place = acx.typed_place(acx.tcx.types.unit);
        let cond_block = acx.end_of_block_dummy();

        let then_entry = acx.current_block();
        let (then_ty, then) = then_expr(acx)?;
        if !then_ty.is_never() {
            acx.assign_rvalue(ret_place, Rvalue::Use(then), then_ty.span);
        }
        let then_block = acx.end_of_block_dummy();

        let else_entry = acx.current_block();
        let else_ty = if let Some(else_) = else_expr {
            let (ty, else_) = else_(acx)?;
            if !ty.is_never() {
                acx.assign_rvalue(ret_place, Rvalue::Use(else_), ty.span);
            }
            ty
        } else {
            Ty::new(then_ty.span, acx.tcx.types.unit)
        };

        // All the if blocks converge to this block
        let next_block = acx.next_block();
        acx.end_of_block(else_ty.span, TerminatorKind::Goto { target: next_block });

        let source_info = SourceInfo { span: then_ty.span };
        acx.body.basic_blocks[then_block].terminator =
            Some(Terminator { source_info, kind: TerminatorKind::Goto { target: next_block } });

        // Safety: we do not create zst constants if the expression type is reduced to `never'
        let mut ret_ty = unsafe { acx.assert_same_types_allow_never(else_ty, then_ty)? };
        acx.body.local_decls[ret_place.local].ty = ret_ty.kind;

        use std::cmp::{max, min};

        // TODO: move into `span` crate
        fn merge_span(a: Span, b: Span) -> Span {
            Span::new(min(a.start, b.start), max(a.end, b.end))
        }
        ret_ty.span = merge_span(else_ty.span, then_ty.span);

        let source_info = SourceInfo { span: cond.span };
        acx.body.basic_blocks[cond_block].terminator = Some(Terminator {
            source_info,
            kind: TerminatorKind::SwitchInt {
                discr,
                targets: SwitchTargets::static_if(0, else_entry, then_entry),
            },
        });

        Ok((ret_ty, Operand::Copy(ret_place)))
    })
}

fn const_eval_operand(
    op: mir::BinOp,
    ScalarRepr { data: a, size }: ScalarRepr,
    ScalarRepr { data: b, .. }: ScalarRepr,
    signed: bool,
) -> ScalarRepr {
    use mir::BinOp::*;

    fn bool(s: bool) -> ScalarRepr {
        if s { ScalarRepr::TRUE } else { ScalarRepr::FALSE }
    }

    let scalar = match op {
        Add | AddUnchecked => ScalarRepr { data: a.wrapping_add(b), size },
        Sub | SubUnchecked => ScalarRepr { data: a.wrapping_add(b), size },
        Mul | MulUnchecked => ScalarRepr { data: a.wrapping_add(b), size },
        Div => ScalarRepr { data: a.wrapping_div(b), size },
        Eq => bool(a == b),
        Lt => bool(a > b),
        Le => bool(a >= b),
        Ne => bool(a != b),
        Gt => bool(a < b),
        Ge => bool(a <= b),
    };

    let max = if signed {
        match size.get() {
            1 => i8::MAX as u128,
            2 => i16::MAX as u128,
            4 => i32::MAX as u128,
            8 => i64::MAX as u128,
            _ => unreachable!(),
        }
    } else {
        match size.get() {
            1 => u8::MAX as u128,
            2 => u16::MAX as u128,
            4 => u32::MAX as u128,
            8 => u64::MAX as u128,
            _ => unreachable!(),
        }
    };
    scalar
    // if scalar.data > max { None } else { Some(scalar) }
}

fn coerce_const_scalar<'tcx>(
    tcx: Tx<'tcx>,
    mut scalar: ScalarRepr,
    ty: mir::Ty<'tcx>,
) -> Operand<'tcx> {
    let size = tcx.layout_of(ty).layout.size;

    scalar.size = NonZeroU8::new(size.bytes() as u8).unwrap();
    Operand::Const(ConstValue::Scalar(scalar), ty)
}

pub fn binop_ty<'tcx>(
    acx: &mut AnalyzeCx<'_, 'tcx>,
    op: mir::BinOp,
    lhs: Ty<'tcx>,
    rhs: Ty<'tcx>,
) -> Result<Ty<'tcx>> {
    use mir::BinOp::*;

    let ty = acx.assert_same_types(lhs, rhs)?;
    Ok(match op {
        Add | Sub | Mul | Div | AddUnchecked | SubUnchecked | MulUnchecked => ty,
        Eq | Lt | Le | Ne | Ge | Gt => lhs.map(|_| acx.tcx.types.bool),
    })
}

fn analyze_expr<'hir>(
    acx: &mut AnalyzeCx<'_, 'hir>,
    expr: &Expr<'hir>,
) -> Result<(Ty<'hir>, Operand<'hir>)> {
    let (ty, rvalue) = match expr.kind {
        expr::Lit(Lit::Str(_)) | expr::Lit(Lit::Float(_)) => todo!(),
        expr::Lit(Lit::Int(LitInt { lit, span })) => {
            let ty = acx.tcx.intern_ty(TyKind::Infer(Infer::Int(acx.next_infer_var())));
            (Ty::new(span, ty), Operand::Const(ConstValue::Scalar(ScalarRepr::from(lit)), ty))
        }
        expr::Lit(Lit::Bool(LitBool { lit, span })) => {
            let ty = acx.tcx.types.bool;
            (
                Ty::new(span, ty),
                Operand::Const(
                    ConstValue::Scalar(if lit { ScalarRepr::TRUE } else { ScalarRepr::FALSE }),
                    ty,
                ),
            )
        }
        expr::Local(ident) => {
            let (ty, place) = acx.scope().get_var(ident.name).ok_or_else(|| {
                acx.err.emit(errors::NotFoundLocal { local: ident.name, span: ident.span })
            })?;
            if let Some(place) = place {
                (ty, Operand::Copy(place))
            } else if ty.is_zst() {
                (ty, Operand::Const(ConstValue::Zst, ty.kind))
            } else {
                panic!()
            }
        }
        expr::Assign(lvalue, rvalue) => {
            let (lty, lhs) = analyze_expr(acx, lvalue)?;
            let (rty, rhs) = analyze_expr(acx, rvalue)?;

            // it's either mutable reference or mutable place
            //
            if let Some(place) = lhs.place()
                && let decl = acx.local_decls()[place.local]
                && decl.ty.ref_mutability().unwrap_or_else(|| decl.mutability).is_mut()
            {
                let ty = acx.assert_same_types(lty, rty)?;
                acx.assign_rvalue(place, Rvalue::Use(rhs), ty.span);
                (ty, Operand::Copy(place))
            } else {
                return Err(acx.err.emit(errors::InvalidLvalue { span: lty.span }));
            }
        }
        expr::Unary(op, expr) => match op {
            UnOp::Deref => {
                let (ty, operand) = analyze_expr(acx, expr)?;
                if let Operand::Copy(place) = operand
                    && let Some(place_ty) = place.ty(acx.tcx, acx.local_decls()).builtin_deref(true)
                {
                    let place = place.project_deeper(acx.tcx, &[PlaceElem::Deref]);
                    (ty.map(|_| place_ty.ty), Operand::Copy(place))
                } else {
                    return Err(acx.err.emit(errors::InvalidDeref { ty: acx.hix.ty_msg(ty) }));
                }
            }
            mir => {
                let (ty, operand) = analyze_expr(acx, expr)?;
                acx.push_temp_rvalue(
                    ty,
                    Rvalue::UnaryOp(
                        match mir {
                            UnOp::Not => mir::UnOp::Not,
                            UnOp::Neg => mir::UnOp::Neg,
                            UnOp::Deref => unreachable!(),
                        },
                        operand,
                    ),
                    expr.span,
                )
            }
        },
        expr::Binary(op, lhs, rhs) => {
            if let Some(op) = mir::BinOp::from_parse(op) {
                let (lhs, a) = analyze_expr(acx, lhs)?;
                let (rhs, b) = analyze_expr(acx, rhs)?;

                let ty = binop_ty(acx, op, lhs, rhs)?;
                acx.push_temp_rvalue(ty, Rvalue::BinaryOp(op, a, b), expr.span)
            } else {
                let bool = Ty::new(rhs.span, acx.tcx.types.bool);

                match op {
                    BinOp::And(_) => analyze_branch_expr(
                        acx,
                        lhs,
                        lazy!(|acx| analyze_expr(acx, rhs)),
                        Some(lazy!(|acx| { Ok((bool, acx.bool(false))) })),
                    )?,
                    BinOp::Or(_) => analyze_branch_expr(
                        acx,
                        lhs,
                        lazy!(|acx| { Ok((bool, acx.bool(true),)) }),
                        Some(lazy!(|acx| analyze_expr(acx, rhs))),
                    )?,
                    _ => unreachable!(),
                }
            }
        }
        expr::Block(stmts, span) => {
            if let Some((tail, stmts)) = stmts.split_last() {
                acx.scoped(|acx| {
                    analyze_local_block_unclosed(acx, stmts)?;
                    Ok(analyze_stmt(acx, tail, goto_next)?.unwrap_or_else(|| {
                        if acx.scope().returned {
                            acx.never_place(span)
                        } else {
                            acx.unit_place(span)
                        }
                    }))
                })?
            } else {
                acx.unit_place(span)
            }
        }
        expr::Call(call, args) => {
            let operands =
                args.iter().map(|expr| analyze_expr(acx, expr)).collect::<Result<Vec<_>>>()?;
            let (types, args): (Vec<_>, Vec<_>) = operands.iter().copied().unzip();

            // TODO: avoid this allocation
            let types_mir: Vec<_> = types.iter().map(|&ty| ty.kind).collect();

            if [sym::i8, sym::i16, sym::i32, sym::i64, sym::isize].contains(&call.name) {
                let cast = match call.name {
                    sym::i8 => acx.tcx.types.i8,
                    sym::i16 => acx.tcx.types.i16,
                    sym::i32 => acx.tcx.types.i32,
                    sym::i64 => acx.tcx.types.i64,
                    sym::isize => acx.tcx.types.isize,
                    _ => unreachable!(),
                };

                if let [(from, operand)] = operands[..] {
                    let kind = CastKind::from_cast(from.kind, cast).ok_or_else(|| {
                        acx.err.emit(errors::NonPrimitiveCast {
                            span: from.span,
                            from: acx.hix.ty_fmt(from.kind),
                            cast: acx.hix.ty_fmt(cast),
                        })
                    })?;
                    acx.push_temp_rvalue(
                        Ty::new(from.span, cast),
                        Rvalue::Cast(kind, operand, cast),
                        expr.span,
                    )
                } else {
                    return Err(acx.err.emit(errors::MismatchFnSig {
                        expect: acx.hix.args_fmt(&[cast]),
                        found: acx.hix.args_fmt(&types_mir),
                        caller: call.span,
                        target: None,
                    }));
                }
            } else if let Some(&def) = acx.hix.decls.get(&call.name) {
                let InstanceData { sig, span, symbol, hsig, .. } = acx.hix.instances[def];
                assert_eq!(symbol, call.name); // todo: impossible?

                for (&ty, &pass) in types.iter().zip(sig.inputs()) {
                    // Now we have to use a fake span,
                    // later we will have to calculate it from the arguments
                    // TODO: fix this check
                    if acx.assert_same_types(ty, ty.map(|_| pass)).is_err()
                        || types.len() != sig.inputs().len()
                    {
                        return Err(acx.err.emit(errors::MismatchFnSig {
                            expect: acx.hix.args_fmt(sig.inputs()),
                            found: acx.hix.args_fmt(&types_mir),
                            caller: call.span,
                            target: Some(span),
                        }));
                    }
                }

                let dest = acx.typed_place(sig.output());
                acx.end_of_block(
                    expr.span,
                    TerminatorKind::Call {
                        func: Operand::Const(
                            ConstValue::Zst,
                            acx.tcx.intern_ty(mir::TyKind::FnDef(def)),
                        ), // now functions are ZSTs
                        args,
                        dest,
                        target: Some(acx.next_block()),
                        fn_span: span,
                    },
                );
                (Ty::new(hsig.ret_span(), sig.output()), Operand::Copy(dest))
            } else {
                return Err(acx.err.emit(errors::NotFoundLocal::ident(call)));
            }
        }
        expr::If(cond, then, else_) => analyze_branch_expr(
            acx,
            cond,
            lazy!(|acx| analyze_expr(acx, then)),
            else_.map(|else_| lazy!(|acx| analyze_expr(acx, else_))),
        )?,
        expr::Return(ret) => {
            if let Some(sig) = acx.scope().sig {
                let ret_ty = sig.decl.output.ty(acx.tcx);
                let (ty, ret) = if let Some(ret) = ret {
                    analyze_expr(acx, ret)?
                } else {
                    let unit = Ty::new(expr.span, acx.tcx.types.unit);
                    (unit, acx.unit_const(unit))
                };

                if ty.is_never() {
                    let _ = mem::take(&mut acx.block);
                } else {
                    let ty = acx.assert_same_types(ret_ty, ty)?;
                    make_return(acx, ty, ret);
                }
                acx.end_of_block(ty.span, TerminatorKind::Return);
                acx.scope().returned = true; // prevent next blocks from spawning in this scope

                let never = Ty::new(ty.span, acx.tcx.types.never);
                (never, acx.unit_const(never))
            } else {
                todo!()
            }
        }
        expr::Break(break_) => {
            let never = Ty::new(expr.span, acx.tcx.types.never);

            if let Some(mut looped) = acx.scope.as_ref().map(|s| s.looped.clone()).expect("TODO") {
                let breaking = acx.end_of_block_dummy();

                let expr =
                    if let Some(expr) = break_ { Some(analyze_expr(acx, expr)?.1) } else { None };
                looped.breaks.push((breaking, expr));
                acx.scope().looped = Some(looped);
            } else {
                todo!()
            }

            (never, Operand::Const(ConstValue::Zst, never.kind))
        }
        expr::Loop(stmts, span) => {
            let entry = acx.end_of_block(span, TerminatorKind::Goto { target: acx.next_block() });

            acx.scoped(|acx| {
                acx.scope().looped = Some(LoopData { breaks: Default::default() });

                let unit = Ty::new(expr.span, acx.tcx.types.unit);
                let (loop_ty, _) =
                    analyze_expr(acx, &Expr { kind: expr::Block(stmts, span), span })?;
                acx.assert_same_types(unit, loop_ty)?;

                let _looped = acx.end_of_block(expr.span, TerminatorKind::Goto { target: entry });
                let exit = acx.current_block();

                let looped = acx.scope().looped.take().unwrap();
                Ok(if looped.breaks.is_empty() {
                    let never = Ty::new(expr.span, acx.tcx.types.never);
                    (never, Operand::Const(ConstValue::Zst, never.kind))
                } else {
                    let mut infer = None;
                    // unit type used as placeholder
                    let break_place = acx.typed_place(acx.tcx.types.unit);

                    for (bb, operand) in looped.breaks {
                        if let Some(operand) = operand {
                            let break_ty =
                                Ty::new(expr.span, operand.ty(&acx.body.local_decls, acx.tcx));
                            if let Some(ty) = infer {
                                // TODO: propagate break span
                                acx.assert_same_types(ty, break_ty)?;
                            } else {
                                infer = Some(break_ty);
                            }

                            let source_info = SourceInfo { span: break_ty.span };
                            acx.body.basic_blocks[bb].statements.push(Statement {
                                source_info,
                                kind: StatementKind::Assign(break_place, Rvalue::Use(operand)),
                            });
                        } else {
                            acx.assert_same_types(infer.unwrap_or(unit), unit)?;
                        }

                        if let Some(infer) = infer {
                            acx.body.local_decls[break_place.local].ty = infer.kind;
                        }
                        acx.body.basic_blocks[bb].terminator = Some(Terminator {
                            source_info: SourceInfo { span: Span::splat(0) }, // TODO: provide span
                            kind: TerminatorKind::Goto { target: exit },
                        })
                    }
                    (infer.unwrap_or(unit), Operand::Copy(break_place))
                })
            })?
        }
        expr::AddrOf(mutability, expr) => {
            let (ty, operand) = analyze_expr(acx, expr)?;

            // fixme: Probably too long to find directly by scope locals map
            //  so we only do it if the mutability checking is really necessary
            // If it looks like a user-defined local. We should check its mutability
            if mutability.is_mut()
                && let Operand::Copy(place) = operand
                && acx.body.local_decls[place.local].mutability != mutability
                && let Some((&symbol, _)) = acx.scope().locals.iter().find(|(_, (_, p))| {
                    if let Some(p) = p { p.local == place.local } else { false }
                })
            {
                acx.err.emit(errors::MutBorrowImmut { local: symbol, borrow: expr.span });
            }

            // That's the limitation for today, deep type inference is forbidden.
            if ty.needs_infer() {
                return Err(acx.err.emit(errors::InferInt { span: ty.span }));
            }

            let (ty, place) = acx.push_rvalue(ty, Rvalue::Use(operand), mutability, ty.span);
            acx.push_temp_rvalue(
                ty.map(|ty| acx.tcx.intern_ty(TyKind::Ref(mutability, ty))),
                Rvalue::Ref(mutability, place),
                expr.span,
            )
        }
    };

    Ok((ty.with_span(expr.span), rvalue))
}

fn analyze_stmt<'hir>(
    acx: &mut AnalyzeCx<'_, 'hir>,
    stmt: &Stmt<'hir>,
    terminator: fn(&mut AnalyzeCx) -> TerminatorKind<'hir>,
) -> Result<Option<(Ty<'hir>, Operand<'hir>)>> {
    Ok(match stmt.kind {
        ref stmt @ (stmt::Local(LocalStmt { init: expr, .. }) | stmt::Expr(expr, _)) => {
            let (ty, operand) = analyze_expr(acx, expr)?;

            // Skip block termination of it is empty
            if !acx.block.statements.is_empty() {
                let terminator = terminator(acx);
                acx.end_of_block(ty.span, terminator);
            }

            match *stmt {
                StmtKind::Local(LocalStmt { mutability, pat, .. }) => {
                    if !ty.is_zst() {
                        let (ty, place) =
                            acx.push_rvalue(ty, Rvalue::Use(operand), mutability, ty.span);
                        acx.scope().declare_var(pat.name, (ty, Some(place)));
                        // close block to avoid multiple initialization before loops
                        // but now it is guaranteed by loops start block
                        // acx.end_of_block(TerminatorKind::Goto { target: acx.next_block() });
                    } else {
                        acx.scope().declare_var(pat.name, (ty, None));
                    }
                    None
                }
                StmtKind::Expr(_, true) => Some((ty, operand)),
                StmtKind::Expr(_, false) => None,
            }
        }
    })
}

fn analyze_local_block_unclosed<'hir>(
    acx: &mut AnalyzeCx<'_, 'hir>,
    stmts: &[Stmt<'hir>],
) -> Result<()> {
    for stmt in stmts {
        analyze_stmt(acx, stmt, goto_next)?;
    }
    Ok(())
}

fn make_return<'hir>(acx: &mut AnalyzeCx<'_, 'hir>, ty: Ty<'hir>, operand: Operand<'hir>) {
    acx.assign_rvalue(Place::pure(Local::RETURN_PLACE), Rvalue::Use(operand), ty.span);
}

fn analyze_body<'hir>(
    acx: &mut AnalyzeCx<'_, 'hir>,
    stmts: &[Stmt<'hir>],
    ret: Ty<'hir>,
) -> Result<()> {
    if let Some((last, stmts)) = stmts.split_last() {
        analyze_local_block_unclosed(acx, stmts)?;

        if let stmt::Expr(ret @ Expr { kind: expr::Return(_), .. }, _) = &last.kind {
            return analyze_expr(acx, ret).map(|_| ());
        }

        return Ok(if let stmt::Expr(expr, true) = &last.kind {
            let (ty, operand) = analyze_expr(acx, expr)?;
            let mut ret = unsafe { acx.assert_same_types_allow_never(ret, ty)? };
            ret.span = ty.span;

            if !ret.is_zst() {
                make_return(acx, ret, operand);
            }
            acx.end_of_block(ty.span, TerminatorKind::Return);
        } else {
            let stmt = analyze_stmt(acx, last, goto_next)?;
            assert!(stmt.is_none());

            if !acx.scope().returned {
                acx.assert_same_types(ret, Ty::new(last.span, acx.tcx.types.unit))?;
            }
            acx.end_of_block(last.span, TerminatorKind::Return);
        });
    } else {
        let span = acx.scope().sig.map(|sig| sig.span).unwrap_or(ret.span);
        acx.assert_same_types(ret, Ty::new(span, acx.tcx.types.unit))?;
    }
    Ok(())
}

fn analyze_fn_prelude<'hir>(acx: &mut AnalyzeCx<'_, 'hir>, sig: FnDecl<'hir>) -> Result<()> {
    acx.body.argc = sig.inputs.len();
    for &(mutability, pat, ty) in sig.inputs {
        let local = acx.body.local_decls.push(LocalDecl { mutability, ty: ty.kind });
        acx.scope().declare_var(pat, (ty, Some(Place::pure(local))));
    }
    // acx.end_of_block(TerminatorKind::Goto { target: acx.next_block() });

    Ok(())
}

pub fn analyze_fn_definition<'hir>(
    tcx: Tx<'hir>,
    hix: &HirCtx<'hir>,
    sig: FnSig<'hir>,
    stmts: &[Stmt<'hir>],
    err: &mut error::TyErrCtx,
) -> Result<mir::Body<'hir>> {
    let ret = match sig.decl.output {
        FnRetTy::Default(span) => Ty::new(span, tcx.types.unit),
        FnRetTy::Return(ty) => ty,
    };
    let mut body = mir::Body {
        // RETURN_PLACE now is only possible mutable place
        local_decls: index_vec![LocalDecl { mutability: Mutability::Mut, ty: ret.kind }],
        ..Default::default()
    };
    let mut acx = AnalyzeCx {
        tcx,
        hix,
        block: mir::BasicBlockData { statements: vec![], terminator: None },
        body: &mut body,
        scope: Some(tcx.empty_hir_scope(sig)),
        vars: Default::default(),
        err,
    };

    analyze_fn_prelude(&mut acx, sig.decl)?;
    analyze_body(&mut acx, stmts, ret)?;

    assert!(acx.block.statements.is_empty());

    if acx.body.basic_blocks.is_empty() {
        // allow dummy because it generated without sources
        acx.end_of_block(Span::splat(0), TerminatorKind::Return);
    }

    InferOverwrite { tcx, vars: acx.vars, err: acx.err }.visit_body(acx.body);

    Ok(body)
}

struct InferOverwrite<'err, 'tcx> {
    tcx: Tx<'tcx>,
    err: &'err mut error::TyErrCtx,
    vars: IndexVec<mir::InferId, Option<mir::Ty<'tcx>>>,
}

impl<'tcx> MutVisitor<'tcx> for InferOverwrite<'_, 'tcx> {
    fn tcx<'a>(&'a self) -> Tx<'tcx> {
        self.tcx
    }

    fn visit_ty(&mut self, ty: &mut mir::Ty<'tcx>, _: TyContext) {
        let i32 = self.tcx().types.i32;

        while let TyKind::Infer(Infer::Int(var)) = ty.kind()
            && let infer = self.vars[var].unwrap_or(i32)
            && infer != *ty
        {
            *ty = infer;
        }
        if ty.needs_infer() {
            // cyclic inferring now is unreachable
            return unreachable!();

            // limitation of this day
            // TODO: get span from source_info that should be storage at body
            // self.err.emit(errors::CantInferInt { span: Span::splat(0) });
        }
    }

    fn visit_constant(
        &mut self,
        (const_, ty): (&mut ConstValue, &mut mir::Ty<'tcx>),
        location: Location,
    ) {
        if let TyKind::Infer(Infer::Int(_)) = ty.kind()
            && let ConstValue::Scalar(repr) = const_
        {
            self.visit_ty(ty, TyContext::Location(location));

            if !ty.needs_infer() {
                repr.size = NonZeroU8::new(self.tcx().layout_of(*ty).size.bytes() as u8)
                    .expect("integral type sizes in bytes should be non zero and less than 255");
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct ItemData<'hir> {
    attrs: &'hir [MetaItem],
    span: Span,
}

#[derive(Debug)]
pub struct HirCtx<'hir> {
    pub tcx: Tx<'hir>,
    name: Symbol,
    pub decls: FxHashMap<Symbol, mir::DefId>, // later use modules

    pub defs: IndexVec<mir::DefId, mir::Body<'hir>>,
    pub instances: IndexVec<mir::DefId, InstanceData<'hir>>,
    pub err: error::TyErrCtx,
}

pub type Hx<'hir> = &'hir HirCtx<'hir>;

impl<'hir> HirCtx<'hir> {
    pub fn symbol_name(&'hir self, def: mir::DefId) -> mir::SymbolName<'hir> {
        mir::SymbolName::new(self.tcx, &man::compute_symbol_name(self, def))
    }

    pub fn as_codegen_unit(&self) -> CodegenUnit<'hir> {
        let items = self
            .defs
            .iter_enumerated()
            .map(|(def, _body)| {
                (
                    MonoItem { def: InstanceDef::Item(def), _marker: PhantomData },
                    MonoItemData {
                        inlined: false,
                        linkage: if self.instances[def].sig.abi == Abi::Zxc {
                            Linkage::Internal
                        } else {
                            Linkage::External
                        },
                        visibility: Visibility::Default,
                    },
                )
            })
            .collect();
        CodegenUnit { name: self.name, items }
    }

    pub fn attrs(&self, def: mir::DefId) -> &[MetaItem] {
        self.instances[def].attrs
    }

    pub fn def_span(&self, def: mir::DefId) -> Span {
        self.instances[def].span
    }

    pub fn entry_fn(&self) -> Option<(mir::DefId, EntryFnType)> {
        if !self.tcx.module_types().iter().any(|&ty| ty == ModuleType::Executable) {
            return None;
        }

        let mut start_fn = None;
        for &def in self.decls.values() {
            if attr::contains_name(self.attrs(def), sym::start) {
                if let Some(start) = start_fn {
                    // self.tcx.sess.emit_err(Error::DefinedMultiple {
                    //     name,
                    //     def: self.def_span(start),
                    //     redef: self.def_span(def),
                    // });
                    self.tcx.sess.emit_err(errors::MultipleStartFunctions {
                        label: self.def_span(def),
                        previous: self.def_span(start),
                    });
                } else {
                    start_fn = Some(def);
                }
            }
        }

        if let Some(start) = start_fn {
            Some((start, EntryFnType::Start))
        } else if let Some(&main) = self.decls.get(&sym::main) {
            Some((main, EntryFnType::Main))
        } else {
            self.tcx.sess.emit_err(errors::NoMainErr);
            None
        }
    }

    fn ty_msg(&self, ty: Ty<'hir>) -> (DiagnosticMessage, Span) {
        (self.ty_fmt(ty.kind), ty.span)
    }

    fn ty_fmt(&self, ty: mir::Ty<'hir>) -> DiagnosticMessage {
        let mut cx = FmtPrinter::new(self);
        ty.print(&mut cx).unwrap();
        quoted(cx.into_buf(), color::Magenta).into()
    }

    fn sig_fmt(&self, sig: mir::FnSig<'hir>) -> DiagnosticMessage {
        let mut cx = FmtPrinter::new(self);
        cx.print_fn_sig(sig.inputs(), sig.output()).unwrap();
        quoted(cx.into_buf(), color::Magenta).into()
    }

    fn args_fmt(&self, sig: &[mir::Ty<'hir>]) -> DiagnosticMessage {
        let mut cx = FmtPrinter::new(self);
        cx.print_fn_sig(sig, self.tcx.types.unit).unwrap();
        quoted(cx.into_buf(), color::Magenta).into()
    }
}

#[derive(Copy, Clone, PartialEq, Hash, Debug)]
pub enum EntryFnType {
    Main,
    Start,
}

impl<'hir> HirCtx<'hir> {
    pub fn pure(tcx: Tx<'hir>, name: Symbol, handler: Handler) -> Self {
        Self {
            tcx,
            name,
            decls: Default::default(),
            defs: Default::default(),
            instances: Default::default(),
            err: error::TyErrCtx { handler },
        }
    }
}

fn intern_decl<'tcx>(tcx: Tx<'tcx>, decl: FnDecl<'tcx>, abi: Abi) -> mir::FnSig<'tcx> {
    let io = decl
        .inputs
        .iter()
        .map(|(_, _, ty)| ty.kind)
        .chain(Some(match decl.output {
            FnRetTy::Default(_) => tcx.types.unit,
            FnRetTy::Return(ty) => ty.kind,
        }))
        .collect::<SmallVec<_, 8>>();
    mir::FnSig { inputs_and_output: tcx.mk_type_list(&io), abi }
}

pub fn analyze_module<'hir>(hix: &mut HirCtx<'hir>, items: &mut [Item<'hir>]) -> Result<()> {
    let mut defs = Vec::with_capacity(32);

    let mut define =
        |hsig @ FnSig { decl, abi, span }, body: Option<_>, attrs| match hix.decls.entry(decl.name)
        {
            Entry::Occupied(prev) => {
                hix.err.emit(errors::DefinedMultiple {
                    name: quoted(decl.name, color::Magenta).into(),
                    def: hix.instances[*prev.get()].span,
                    redef: span,
                });
            }
            Entry::Vacant(entry) => {
                let def = entry.insert(hix.instances.push(InstanceData {
                    symbol: decl.name,
                    sig: intern_decl(hix.tcx, decl, abi),
                    span,
                    hsig,
                    attrs,
                }));
                defs.push((*def, body));
            }
        };

    items.sort_unstable_by_key(|item| match item.kind {
        ItemKind::Fn(..) => 0,
        _ => 1, // each other has no body
    });

    for item in items {
        match item.kind {
            ItemKind::Fn(sig, body) => define(sig, Some((sig, body)), item.attrs),
            ItemKind::Foreign { abi, items } => {
                for ffi in items {
                    // Copy abi from foreign block to all items
                    define(FnSig { abi, ..ffi.sig }, None, item.attrs);
                }
            }
        };
    }

    // match hix.decls.get(&sym::main) {
    //     None => {
    //         hix.emit.error(Error::HasNoMain(Span::splat(
    //             0, /* add `span::whole`  to communicate with globals like symbol */
    //         )));
    //     }
    //     Some(&def) => {
    //         let InstanceData { sig, span, .. } = hix.instances[def];
    //         if !hix.tcx.sigs.main.contains(&sig) {
    //             hix.emit.error(Error::WrongMainSig { sig, span });
    //         }
    //     }
    // }

    let err = &mut hix.err as *mut _;

    let mut mir = IndexVec::<mir::DefId, _>::with_capacity(128);
    for (def, stmts) in defs {
        if let Some((sig, stmts)) = stmts {
            // Safety: todo
            let body = analyze_fn_definition(hix.tcx, hix, sig, stmts, unsafe { &mut *err })?;
            assert_eq!(def, mir.push(body));
        }
    }

    hix.defs.extend(mir);
    Ok(())
}

mod size_asserts {
    use {super::*, crate::assert_size};

    assert_size!(Expr<'_> as 64);
    assert_size!(ExprKind<'_> as 48);
}
