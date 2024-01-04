mod error;
mod scope;
mod ty;

use {
    crate::{
        mir::{
            self, ty::Abi, CastKind, ConstValue, InstanceData, Local, LocalDecl, Mutability,
            Operand, Place, Rvalue, ScalarRepr, Statement, Terminator,
        },
        sym,
        symbol::{Ident, Symbol},
        FxHashMap, Span, Tx,
    },
    index_vec::{index_vec, IndexVec},
    lexer::{BinOp, Lit, LitInt, ReturnType, Spanned, UnOp},
    smallvec::SmallVec,
    std::{collections::hash_map::Entry, iter, mem},
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
}

#[derive(Debug, Clone)]
pub struct Expr<'tcx> {
    pub kind: ExprKind<'tcx>,
    pub span: Span,
}

impl<'tcx> Expr<'tcx> {
    pub fn analyze(tcx: Tx<'tcx>, expr: &lexer::Expr<'tcx>) -> Self {
        use lexer::expr::{Binary, Block, Paren, TailCall, Unary};

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
            lexer::Expr::Block(block @ Block { stmts, .. }) => expr::Block(
                tcx.arena.stmt.alloc_from_iter(stmts.iter().map(|stmt| Stmt::analyze(tcx, stmt))),
                block.span(),
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
    Expr(&'tcx Expr<'tcx>, /* semi */ bool),
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
        self.scope.as_mut().unwrap()
    }

    fn enter_scope(&mut self) {
        let parent = self.scope.take();
        self.scope =
            Some(self.tcx.arena.scope.alloc(Scope { parent, locals: FxHashMap::default() }))
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
        self.body.basic_blocks.push(mem::replace(
            &mut self.block,
            mir::BasicBlockData { statements: vec![], terminator: None },
        ))
    }

    pub fn next_block(&self) -> mir::BasicBlock {
        mir::BasicBlock::new(self.body.basic_blocks.len() + 1)
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
}

fn assert_same_types<'hir>(a: Ty<'hir>, b: Ty<'hir>) -> Result<'hir, Ty<'hir>> {
    if a == b { Ok(a) } else { Err(Error::TypeMismatch { expected: a, found: b }) }
}

fn goto_next(acx: &mut AnalyzeCx<'_, '_>) -> Terminator<'static> {
    Terminator::Goto { target: acx.next_block() }
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
        expr::Local(ident) => {
            let (ty, place) = acx.scope().get_var(ident.name).ok_or(Error::NotFoundLocal(ident))?;
            (ty, Operand::Copy(place))
        }
        expr::Unary(op, expr) => match op {
            UnOp::Neg(_) => {
                let (ty, operand) = analyze_expr(acx, expr)?;
                acx.push_temp_rvalue(ty, Rvalue::UnaryOp(op, operand))
            }
            _ => todo!(),
        },
        expr::Binary(op, lhs, rhs) => {
            let (lhs, a) = analyze_expr(acx, lhs)?;
            let (rhs, b) = analyze_expr(acx, rhs)?;

            acx.push_temp_rvalue(assert_same_types(lhs, rhs)?, Rvalue::BinaryOp(op, a, b))
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
                    let (ty, place) = acx.push_rvalue(ty, Rvalue::Use(operand), Mutability::Not);
                    acx.scope().declare_var(pat.name, (ty, place));
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
        if let stmt::Expr(_, true) = &stmt.kind {
            panic!()
        }
        analyze_stmt(acx, stmt, Some(goto_next))?;
    }
    Ok(())
}

fn make_return<'hir>(acx: &mut AnalyzeCx<'_, 'hir>, _ty: Ty<'hir>, operand: Operand<'hir>) {
    acx.block
        .statements
        .push(Statement::Assign(Place::pure(Local::RETURN_PLACE), Rvalue::Use(operand)))
}

fn analyze_body<'hir>(
    acx: &mut AnalyzeCx<'_, 'hir>,
    stmts: &[Stmt<'hir>],
    ret: Ty<'hir>,
) -> Result<'hir, ()> {
    if let Some((last, stmts)) = stmts.split_last() {
        analyze_local_block_unclosed(acx, stmts)?;

        return Ok(if let stmt::Expr(expr, true) = &last.kind {
            let (ty, place) = analyze_expr(acx, expr)?;
            make_return(acx, assert_same_types(ret, ty)?, place);
            acx.end_of_block(Terminator::Return);
        } else {
            if let Some((ty, place)) = analyze_stmt(acx, last, None)? {
                make_return(acx, assert_same_types(ret, ty)?, place);
            } else {
                assert_same_types(ret, Ty::new(last.span, acx.tcx.types.unit))?;
            }
            acx.end_of_block(Terminator::Return);
        });
    }
    Ok(())
}

fn analyze_fn_prelude<'hir>(acx: &mut AnalyzeCx<'_, 'hir>, sig: FnDecl<'hir>) -> Result<'hir, ()> {
    acx.body.argc = sig.inputs.len();
    for &(mutability, pat, ty) in sig.inputs {
        let local = acx.body.local_decls.push(LocalDecl { mutability, ty: ty.kind });
        acx.scope().declare_var(pat, (ty, Place::pure(local)));
    }
    // acx.end_of_block(Terminator::Goto { target: acx.next_block() });

    Ok(())
}

pub fn analyze_fn_definition<'hir>(
    tcx: Tx<'hir>,
    hix: &HirCtx<'hir>,
    sig: FnDecl<'hir>,
    stmts: &[Stmt<'hir>],
) -> Result<'hir, mir::Body<'hir>> {
    let ret = match sig.output {
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
        scope: Some(tcx.empty_hir_scope()),
    };

    analyze_fn_prelude(&mut acx, sig)?;
    analyze_body(&mut acx, stmts, ret)?;

    assert!(acx.block.statements.is_empty());

    Ok(body)
}

#[derive(Debug)]
pub struct HirCtx<'hir> {
    pub tcx: Tx<'hir>,
    pub decls: FxHashMap<Symbol, mir::DefId>, // later use modules
    pub instances: IndexVec<mir::DefId, InstanceData<'hir>>,
}

impl<'hir> HirCtx<'hir> {
    pub fn pure(tcx: Tx<'hir>) -> Self {
        Self { tcx, decls: Default::default(), instances: Default::default() }
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
) -> Result<'hir, IndexVec<mir::DefId, mir::Body<'hir>>> {
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
        let body = analyze_fn_definition(hix.tcx, hix, sig.decl, stmts)?;
        assert_eq!(def, mir.push(body));
    }

    Ok(mir)
}
