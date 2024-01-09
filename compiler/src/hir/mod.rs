mod error;
mod scope;
mod ty;

use {
    crate::{
        idx::IndexVec,
        index_vec,
        mir::{
            self,
            mono::{Linkage, Visibility},
            ty::Abi,
            CastKind, CodegenUnit, ConstValue, InstanceData, InstanceDef, Local, LocalDecl,
            MonoItem, MonoItemData, Mutability, Operand, Place, Rvalue, ScalarRepr, Statement,
            SwitchTargets, Terminator, TyKind,
        },
        sym,
        symbol::{Ident, Symbol},
        FxHashMap, Span, Tx,
    },
    lexer::{BinOp, Lit, LitBool, LitInt, ReturnType, Spanned, UnOp},
    smallvec::SmallVec,
    std::{collections::hash_map::Entry, iter, marker::PhantomData, mem, num::NonZeroU8},
};
pub use {
    error::{Error, ReportSettings, Result},
    scope::Scope,
    ty::Ty,
};

#[derive(Debug, Copy, Clone)]
pub enum ExprKind<'tcx> {
    Lit(Lit<'tcx>),
    Local(Ident),
    Unary(UnOp, &'tcx Expr<'tcx>),
    Binary(BinOp, &'tcx Expr<'tcx>, &'tcx Expr<'tcx>),
    Call(Ident, &'tcx [Expr<'tcx>]),
    Block(&'tcx [Stmt<'tcx>], Span),
    If(&'tcx Expr<'tcx>, &'tcx Expr<'tcx>, Option<&'tcx Expr<'tcx>>),
    Return(Option<&'tcx Expr<'tcx>>),
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
        use lexer::expr::{Binary, Call, If, Paren, Return, TailCall, Unary};

        let span = expr.span();
        let kind = match expr {
            lexer::Expr::Lit(lit) => expr::Lit(*lit),
            lexer::Expr::Paren(Paren { expr, .. }) => return Self::analyze(tcx, expr),
            lexer::Expr::Unary(Unary { op, expr }) => {
                expr::Unary(*op, tcx.arena.expr.alloc(Self::analyze(tcx, expr)))
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
        };
        Self { kind, span }
    }
}

#[derive(Debug, Clone)]
pub struct LocalStmt<'tcx> {
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
                (Mutability::Not, Symbol::intern(arg.pat.ident()), analyze_hir_ty(tcx, &arg.ty))
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

#[derive(Debug)]
struct AnalyzeCx<'mir, 'hir> {
    tcx: Tx<'hir>,
    hix: &'mir HirCtx<'hir>,
    block: mir::BasicBlockData<'hir>,
    body: &'mir mut mir::Body<'hir>,
    scope: Option<&'hir mut Scope<'hir>>,
}

impl<'mir, 'hir> AnalyzeCx<'mir, 'hir> {
    pub fn scope(&mut self) -> &mut Scope<'hir> {
        self.scope.as_mut().expect("calling `enter_scope` without `exit_scope`")
    }

    fn enter_scope(&mut self) {
        let parent = self.scope.take().unwrap();
        let returned = parent.returned;
        let sig = parent.sig;
        self.scope = Some(self.tcx.arena.scope.alloc(Scope {
            parent: Some(parent),
            returned,
            sig,
            locals: FxHashMap::default(),
        }));
    }

    fn exit_scope(&mut self) {
        let parent = self.scope().parent.take().expect("lmao");
        self.scope = Some(parent);
    }

    pub fn scoped<T>(&mut self, scope: impl FnOnce(&mut Self) -> T) -> T {
        self.enter_scope();
        let ret = scope(self);
        self.exit_scope();
        ret
    }

    pub fn end_of_block(&mut self, terminator: Terminator<'hir>) -> mir::BasicBlock {
        self.block.terminator = Some(terminator);

        let block = mem::take(&mut self.block);
        if !self.scope().was_returned() {
            self.body.basic_blocks.push(block)
        } else {
            self.body.basic_blocks.last_index().expect("empty body")
        }
    }

    pub fn end_of_block_dummy(&mut self) -> mir::BasicBlock {
        self.block.terminator = None;

        let block = mem::take(&mut self.block);
        if !self.scope().was_returned() {
            self.body.basic_blocks.push(block)
        } else {
            self.body.basic_blocks.last_index().expect("empty body")
        }
    }

    pub fn current_block(&self) -> mir::BasicBlock {
        mir::BasicBlock::new(self.body.basic_blocks.len())
    }

    pub fn next_block(&self) -> mir::BasicBlock {
        mir::BasicBlock::new(self.body.basic_blocks.len() + 1)
    }

    pub fn assign_rvalue(&mut self, place: Place<'hir>, val: Rvalue<'hir>) {
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
        self.block.statements.push(Statement::Assign(place, val))
    }

    pub fn push_rvalue(
        &mut self,
        ty: Ty<'hir>,
        rvalue: Rvalue<'hir>,
        mutability: Mutability,
    ) -> (Ty<'hir>, Place<'hir>) {
        // Simple one-level optimization of MIR
        match rvalue {
            Rvalue::Use(Operand::Copy(place)) => (ty, place),
            _ => {
                let place =
                    Place::pure(self.body.local_decls.push(LocalDecl { mutability, ty: ty.kind }));
                self.block.statements.push(Statement::Assign(place, rvalue));
                (ty, place)
            }
        }
    }

    pub fn push_temp_rvalue(
        &mut self,
        ty: Ty<'hir>,
        rvalue: Rvalue<'hir>,
    ) -> (Ty<'hir>, Operand<'hir>) {
        let (ty, place) = self.push_rvalue(ty, rvalue, Mutability::Not);
        (ty, Operand::Copy(place))
    }

    pub fn typed_place(&mut self, ty: mir::Ty<'hir>) -> Place<'hir> {
        Place::pure(self.body.local_decls.push(LocalDecl { mutability: Mutability::Not, ty }))
    }

    pub fn unit_place(&mut self, span: Span) -> (Ty<'hir>, Operand<'hir>) {
        let unit = Ty { kind: self.tcx.types.unit, span };
        (unit, Operand::Const(ConstValue::Zst, unit.kind))
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
}

/// # Safety
/// There's really nothing dangerous about it, but let `unsafe`
/// emphasize the fine line of using the `never` type
unsafe fn assert_same_types_allow_never<'hir>(a: Ty<'hir>, b: Ty<'hir>) -> Result<'hir, Ty<'hir>> {
    Ok(match (a.is_never(), b.is_never()) {
        (true, true) => a,
        (true, _) => b,
        (_, true) => a,
        _ => assert_same_types(a, b)?,
    })
}

fn assert_same_types<'hir>(a: Ty<'hir>, b: Ty<'hir>) -> Result<'hir, Ty<'hir>> {
    return if a == b { Ok(a) } else { Err(Error::TypeMismatch { expected: a, found: b }) };
}

fn goto_next(acx: &mut AnalyzeCx<'_, '_>) -> Terminator<'static> {
    Terminator::Goto { target: acx.next_block() }
}

/// Lazy mir operand
// type Lazy<'hir> =
//      impl FnOnce(&mut AnalyzeCx<'_, 'hir>) -> Result<'hir, (Ty<'hir>, Operand<'hir>)>;

macro_rules! lazy {
    (|$acx:ident| $expr:expr) => {{ (|$acx: &mut AnalyzeCx<'_, 'hir>| $expr) }};
}

// TODO: add opaque alias
fn analyze_branch_expr<'hir>(
    acx: &mut AnalyzeCx<'_, 'hir>,
    cond: &'hir Expr<'hir>,
    then_expr: impl FnOnce(&mut AnalyzeCx<'_, 'hir>) -> Result<'hir, (Ty<'hir>, Operand<'hir>)>,
    else_expr: Option<
        impl FnOnce(&mut AnalyzeCx<'_, 'hir>) -> Result<'hir, (Ty<'hir>, Operand<'hir>)>,
    >,
) -> Result<'hir, (Ty<'hir>, Operand<'hir>)> {
    let (ty, discr) = analyze_expr(acx, cond)?;

    let bool = acx.tcx.types.bool;
    assert_same_types(Ty::new(cond.span, bool), ty)?;

    acx.scoped(|acx| {
        let ret_place = acx.typed_place(acx.tcx.types.unit);
        let cond_block = acx.end_of_block_dummy();

        let then_entry = acx.current_block();
        let (then_ty, then) = then_expr(acx)?;
        if !then_ty.is_never() {
            acx.assign_rvalue(ret_place, Rvalue::Use(then));
        }
        let then_block = acx.end_of_block_dummy();

        let else_entry = acx.current_block();
        let else_ty = if let Some(else_) = else_expr {
            let (ty, else_) = else_(acx)?;
            if !ty.is_never() {
                acx.assign_rvalue(ret_place, Rvalue::Use(else_));
            }
            ty
        } else {
            Ty::new(then_ty.span, acx.tcx.types.unit)
        };

        // All the if blocks converge to this block
        let next_block = acx.next_block();
        acx.end_of_block(Terminator::Goto { target: next_block });
        acx.body.basic_blocks[then_block].terminator =
            Some(Terminator::Goto { target: next_block });

        // Safety: we do not create zst constants if the expression type is reduced to `never'
        let ret_ty = unsafe { assert_same_types_allow_never(else_ty, then_ty)? };
        acx.body.local_decls[ret_place.local].ty = ret_ty.kind;

        acx.body.basic_blocks[cond_block].terminator = Some(Terminator::SwitchInt {
            discr,
            targets: SwitchTargets::static_if(0, else_entry, then_entry),
        });

        Ok((ret_ty, Operand::Copy(ret_place)))
    })
}

fn const_eval_operand(
    op: mir::BinOp,
    ScalarRepr { data: a, size }: ScalarRepr,
    ScalarRepr { data: b, .. }: ScalarRepr,
    signed: bool,
) -> Option<ScalarRepr> {
    use mir::BinOp::*;

    fn bool(s: bool) -> ScalarRepr {
        if s { ScalarRepr::TRUE } else { ScalarRepr::FALSE }
    }

    let scalar = match op {
        Add | AddUnchecked => ScalarRepr { data: a.checked_add(b)?, size },
        Sub | SubUnchecked => ScalarRepr { data: a.checked_sub(b)?, size },
        Mul | MulUnchecked => ScalarRepr { data: a.checked_mul(b)?, size },
        Div => ScalarRepr { data: a.checked_div(b)?, size },
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
    if scalar.data > max { None } else { Some(scalar) }
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

fn analyze_expr<'hir>(
    acx: &mut AnalyzeCx<'_, 'hir>,
    expr: &Expr<'hir>,
) -> Result<'hir, (Ty<'hir>, Operand<'hir>)> {
    let (ty, rvalue) = match expr.kind {
        expr::Lit(Lit::Int(LitInt { lit, span })) => {
            let ty = acx.tcx.types.i32;
            (
                Ty::new(span, ty),
                Operand::Const(ConstValue::Scalar(ScalarRepr::from(lit as u32)), ty),
            )
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
            let (ty, place) = acx.scope().get_var(ident.name).ok_or(Error::NotFoundLocal(ident))?;
            if let Some(place) = place {
                (ty, Operand::Copy(place))
            } else {
                if ty.is_zst() { (ty, Operand::Const(ConstValue::Zst, ty.kind)) } else { panic!() }
            }
        }
        expr::Unary(op, expr) => match op {
            UnOp::Neg(_) => {
                let (ty, operand) = analyze_expr(acx, expr)?;
                acx.push_temp_rvalue(ty, Rvalue::UnaryOp(mir::UnOp::from_parse(op), operand))
            }
            _ => todo!(),
        },
        expr::Binary(op, lhs, rhs) => {
            if let Some(op) = mir::BinOp::from_parse(op) {
                let (lhs, a) = analyze_expr(acx, lhs)?;
                let (rhs, b) = analyze_expr(acx, rhs)?;

                if lhs.is_integer() && rhs.is_integer() {
                    let (a, b) = match (a, b) {
                        (
                            Operand::Const(ConstValue::Scalar(a), ty),
                            Operand::Const(ConstValue::Scalar(b), _),
                        ) => {
                            assert_eq!(lhs.kind, ty);
                            assert_same_types(lhs, rhs)?;

                            if op == mir::BinOp::Div && b.is_null() {
                                return Err(Error::ConstArithmetic {
                                    case: "this arithmetic operation will trapped at runtime",
                                    note: Some(format!(
                                        "attempt to divide {:#?} by zero",
                                        mir::consts::ConstInt::new(
                                            a,
                                            ty.is_signed(),
                                            ty.is_ptr_sized_int(),
                                        )
                                    )),
                                    span: expr.span,
                                });
                            }

                            return if let Some(s) = const_eval_operand(op, a, b, ty.is_signed()) {
                                Ok((lhs, Operand::Const(ConstValue::Scalar(s), ty)))
                            } else {
                                Err(Error::ConstArithmetic {
                                    case: "this arithmetic operation will overflow",
                                    note: None,
                                    span: expr.span,
                                })
                            };
                        }
                        // it should be unreachable
                        (a @ Operand::Const(_, _), b @ Operand::Const(_, _)) => {
                            assert_same_types(lhs, rhs)?;
                            (a, b) // add simple const evaluation
                        }
                        (Operand::Const(ConstValue::Scalar(a), _), b) => {
                            (coerce_const_scalar(acx.tcx, a, rhs.kind), b)
                        }
                        (a, Operand::Const(ConstValue::Scalar(b), _)) => {
                            (a, coerce_const_scalar(acx.tcx, b, lhs.kind))
                        }
                        (a, b) => {
                            assert_same_types(lhs, rhs)?;
                            (a, b)
                        }
                    };
                    // lhs is defining reality
                    let kind = op.ty(acx.tcx, lhs.kind, lhs.kind);
                    acx.push_temp_rvalue(Ty::new(lhs.span, kind), Rvalue::BinaryOp(op, a, b))
                } else {
                    assert_same_types(lhs, rhs)?;
                    let kind = op.ty(acx.tcx, lhs.kind, rhs.kind);
                    acx.push_temp_rvalue(Ty::new(lhs.span, kind), Rvalue::BinaryOp(op, a, b))
                }
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
                    Ok(analyze_stmt(acx, tail, Some(goto_next))?
                        .unwrap_or_else(|| acx.unit_place(span)))
                })?
            } else {
                acx.unit_place(span)
            }
        }
        expr::Call(call, args) => {
            let operands =
                args.iter().map(|expr| analyze_expr(acx, expr)).collect::<Result<Vec<_>>>()?;
            let types: Vec<_> = operands.iter().map(|(t, _)| *t).collect();
            let args: Vec<_> = operands.iter().map(|(_, o)| *o).collect();

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
                    let kind = CastKind::from_cast(from.kind, cast)
                        .ok_or(Error::NonPrimitiveCast { from, cast })?;
                    acx.push_temp_rvalue(
                        Ty::new(from.span, cast),
                        Rvalue::Cast(kind, operand, cast),
                    )
                } else {
                    return Err(Error::WrongFnArgs {
                        expect: vec![cast],
                        found: types,
                        caller: call.span,
                        target: None,
                    });
                }
            } else if let Some(&def) = acx.hix.decls.get(&call.name) {
                let InstanceData { sig, span, symbol, hsig } = acx.hix.instances[def];
                assert_eq!(symbol, call.name); // todo: impossible?

                if !types.iter().eq_by(sig.inputs(), |hir, mir| &hir.kind == mir) {
                    return Err(Error::WrongFnArgs {
                        expect: sig.inputs().iter().copied().collect(),
                        found: types,
                        caller: call.span,
                        target: Some(span),
                    });
                }

                let dest = acx.typed_place(sig.output());
                acx.end_of_block(Terminator::Call {
                    func: Operand::Const(
                        ConstValue::Zst,
                        acx.tcx.intern_ty(mir::TyKind::FnDef(def)),
                    ), // now functions are ZSTs
                    args,
                    dest,
                    target: Some(acx.next_block()),
                    fn_span: span,
                });
                (Ty::new(hsig.ret_span(), sig.output()), Operand::Copy(dest))
            } else {
                return Err(Error::NotFoundLocal(call));
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
                    let ty = assert_same_types(ret_ty, ty)?;
                    make_return(acx, ty, ret);
                }
                acx.end_of_block(Terminator::Return);
                acx.scope().returned = true; // prevent next blocks from spawning in this scope

                let never = Ty::new(ty.span, acx.tcx.types.never);
                (never, acx.unit_const(never))
            } else {
                return Err(todo!());
            }
        }
        panic => todo!("{panic:?}"),
    };

    Ok((ty.with_span(expr.span), rvalue))
}

fn analyze_stmt<'hir>(
    acx: &mut AnalyzeCx<'_, 'hir>,
    stmt: &Stmt<'hir>,
    terminator: Option<fn(&mut AnalyzeCx) -> Terminator<'hir>>,
) -> Result<'hir, Option<(Ty<'hir>, Operand<'hir>)>> {
    Ok(match stmt.kind {
        ref stmt @ (stmt::Local(LocalStmt { init: expr, .. }) | stmt::Expr(expr, _)) => {
            let (ty, operand) = analyze_expr(acx, expr)?;

            // Skip block termination of it is empty
            if !acx.block.statements.is_empty()
                && let Some(terminator) = terminator
            {
                let terminator = terminator(acx);
                acx.end_of_block(terminator);
            }

            match stmt {
                StmtKind::Local(LocalStmt { pat, .. }) => {
                    if !ty.is_zst() {
                        let (ty, place) =
                            acx.push_rvalue(ty, Rvalue::Use(operand), Mutability::Not);
                        acx.scope().declare_var(pat.name, (ty, Some(place)));
                    } else {
                        acx.scope().declare_var(pat.name, (ty, None));
                    }
                    None
                }
                StmtKind::Expr(_, _) => Some((ty, operand)),
            }
        }
    })
}

fn analyze_local_block_unclosed<'hir>(
    acx: &mut AnalyzeCx<'_, 'hir>,
    stmts: &[Stmt<'hir>],
) -> Result<'hir, ()> {
    for stmt in stmts {
        analyze_stmt(acx, stmt, Some(goto_next))?;
    }
    Ok(())
}

fn make_return<'hir>(acx: &mut AnalyzeCx<'_, 'hir>, _ty: Ty<'hir>, operand: Operand<'hir>) {
    if let Some(Statement::Assign(place, _)) = acx.block.statements.last_mut() {
        place.local = Local::RETURN_PLACE;
    } else {
        acx.block
            .statements
            .push(Statement::Assign(Place::pure(Local::RETURN_PLACE), Rvalue::Use(operand)))
    }
}

fn analyze_body<'hir>(
    acx: &mut AnalyzeCx<'_, 'hir>,
    stmts: &[Stmt<'hir>],
    ret: Ty<'hir>,
) -> Result<'hir, ()> {
    if let Some((last, stmts)) = stmts.split_last() {
        analyze_local_block_unclosed(acx, stmts)?;

        if let stmt::Expr(ret @ Expr { kind: expr::Return(_), .. }, _) = &last.kind {
            return analyze_expr(acx, ret).map(|_| ());
        }

        return Ok(if let stmt::Expr(expr, true) = &last.kind {
            let (ty, place) = analyze_expr(acx, expr)?;
            make_return(acx, assert_same_types(ret, ty)?, place);
            acx.end_of_block(Terminator::Return);
        } else {
            //if let Some((ty, place)) = analyze_stmt(acx, last, None)? {
            //    make_return(acx, assert_same_types(ret, ty)?, place);
            //} else {
            assert_same_types(ret, Ty::new(last.span, acx.tcx.types.unit))?;
            //}
            acx.end_of_block(Terminator::Return);
        });
    }
    Ok(())
}

fn analyze_fn_prelude<'hir>(acx: &mut AnalyzeCx<'_, 'hir>, sig: FnDecl<'hir>) -> Result<'hir, ()> {
    acx.body.argc = sig.inputs.len();
    for &(mutability, pat, ty) in sig.inputs {
        let local = acx.body.local_decls.push(LocalDecl { mutability, ty: ty.kind });
        acx.scope().declare_var(pat, (ty, Some(Place::pure(local))));
    }
    // acx.end_of_block(Terminator::Goto { target: acx.next_block() });

    Ok(())
}

pub fn analyze_fn_definition<'hir>(
    tcx: Tx<'hir>,
    hix: &HirCtx<'hir>,
    sig: FnSig<'hir>,
    stmts: &[Stmt<'hir>],
) -> Result<'hir, mir::Body<'hir>> {
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
    };

    analyze_fn_prelude(&mut acx, sig.decl)?;
    analyze_body(&mut acx, stmts, ret)?;

    assert!(acx.block.statements.is_empty());

    Ok(body)
}

#[derive(Debug)]
pub struct HirCtx<'hir> {
    pub tcx: Tx<'hir>,
    name: Symbol,
    pub decls: FxHashMap<Symbol, mir::DefId>, // later use modules

    pub defs: IndexVec<mir::DefId, mir::Body<'hir>>,
    pub instances: IndexVec<mir::DefId, InstanceData<'hir>>,
}

impl<'hir> HirCtx<'hir> {
    pub fn as_codegen_unit(&self) -> CodegenUnit<'hir> {
        let items = self
            .defs
            .iter_enumerated()
            .map(|(def, _body)| {
                (
                    MonoItem { def: InstanceDef::Item(def), _marker: PhantomData },
                    MonoItemData {
                        inlined: false,
                        linkage: Linkage::External,
                        visibility: Visibility::Default,
                    },
                )
            })
            .collect();
        CodegenUnit { name: self.name, items }
    }
}

impl<'hir> HirCtx<'hir> {
    pub fn pure(tcx: Tx<'hir>, name: Symbol) -> Self {
        Self {
            tcx,
            name,
            decls: Default::default(),
            defs: Default::default(),
            instances: Default::default(),
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

pub fn analyze_module<'hir>(
    hix: &mut HirCtx<'hir>,
    functions: &[(FnSig<'hir>, Box<[Stmt<'hir>]>)],
) -> Result<'hir, ()> {
    let mut defs = Vec::with_capacity(128);
    for &(hsig @ FnSig { decl, abi, span }, _) in functions {
        match hix.decls.entry(decl.name) {
            Entry::Occupied(prev) => {
                return Err(Error::DefinedMultiple {
                    name: decl.name,
                    definition: hix.instances[*prev.get()].span,
                });
            }
            Entry::Vacant(entry) => {
                let def = entry.insert(hix.instances.push(InstanceData {
                    symbol: decl.name,
                    sig: intern_decl(hix.tcx, decl, abi),
                    span,
                    hsig,
                }));
                defs.push(*def);
            }
        }
    }

    match hix.decls.get(&sym::main) {
        None => {
            return Err(Error::HasNoMain(Span::splat(
                0, /* add `span::whole`  to communicate with globals like symbol */
            )));
        }
        Some(&def) => {
            let InstanceData { sig, span, .. } = hix.instances[def];
            if !hix.tcx.sigs.main.contains(&sig) {
                return Err(Error::WrongMainSig { sig, span });
            }
        }
    }

    let mut mir = IndexVec::<mir::DefId, _>::with_capacity(128);
    for (def, (sig, stmts)) in defs.into_iter().zip(functions) {
        let body = analyze_fn_definition(hix.tcx, hix, *sig, stmts)?;
        assert_eq!(def, mir.push(body));
    }

    hix.defs.extend(mir);
    Ok(())
}

mod size_asserts {
    use {super::*, crate::assert_size};

    assert_size!(Expr<'_> as 64);
    assert_size!(ExprKind<'_> as 48);
}
