mod error;
mod fmt;
mod scope;
mod ty;

use {
    ariadne::{Color, Report, ReportKind},
    index_vec::index_vec,
    std::{collections::HashMap, mem},
};
pub use {
    error::{Error, ReportSettings, Result},
    scope::Scope,
    ty::Ty,
};

use crate::{
    codegen::{
        mir::{
            self, ConstValue, Local, LocalDecl, Operand, Place, Rvalue, ScalarRepr, Statement,
            Terminator,
        },
        Arena, Session, Tx, TyCtx,
    },
    lexer::{Ident, Lit, LitInt},
    parse::{self, BinOp, Spanned, UnOp},
    Span,
};

#[derive(Debug, Clone)]
pub enum ExprKind<'tcx> {
    Lit(Lit<'tcx>),
    Local(Ident<'tcx>),
    Unary(UnOp, &'tcx Expr<'tcx>),
    Binary(BinOp, &'tcx Expr<'tcx>, &'tcx Expr<'tcx>),
    Call(&'tcx str, &'tcx [Expr<'tcx>]),
    Block(&'tcx [Stmt<'tcx>], Span),
}

#[derive(Debug, Clone)]
pub struct Expr<'tcx> {
    pub kind: ExprKind<'tcx>,
    pub span: Span,
}

impl<'tcx> Expr<'tcx> {
    pub fn analyze(tcx: Tx<'tcx>, expr: &parse::Expr<'tcx>) -> Self {
        use crate::parse::expr::{Binary, Block, Paren, TailCall, Unary};

        let span = expr.span();
        let kind = match expr {
            parse::Expr::Lit(lit) => expr::Lit(*lit),
            parse::Expr::Paren(Paren { expr, .. }) => return Self::analyze(tcx, expr),
            parse::Expr::Unary(Unary { op, expr }) => {
                expr::Unary(*op, tcx.arena.expr.alloc(Self::analyze(tcx, expr)))
            }
            parse::Expr::Binary(Binary { left, op, right }) => expr::Binary(
                *op,
                tcx.arena.expr.alloc(Self::analyze(tcx, left)),
                tcx.arena.expr.alloc(Self::analyze(tcx, right)),
            ),
            parse::Expr::Ident(str) => expr::Local(*str),
            parse::Expr::TailCall(TailCall { receiver, func, args, .. }) => {
                assert!(args.is_none());

                expr::Call(
                    func.ident(),
                    tcx.arena.expr.alloc_from_iter([Self::analyze(tcx, receiver)]),
                )
            }
            parse::Expr::Block(block @ Block { stmts, .. }) => expr::Block(
                tcx.arena.stmt.alloc_from_iter(stmts.iter().map(|stmt| Stmt::analyze(tcx, stmt))),
                block.span(),
            ),
        };
        Self { kind, span }
    }
}

#[derive(Debug, Clone)]
pub struct LocalStmt<'tcx> {
    pub pat: Ident<'tcx>,
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
    pub fn analyze(tcx: Tx<'tcx>, stmt: &parse::Stmt<'tcx>) -> Self {
        let span = stmt.span();
        let kind = match stmt {
            parse::Stmt::Local(local) => StmtKind::Local(LocalStmt {
                pat: local.pat,
                init: tcx.arena.expr.alloc(Expr::analyze(tcx, &local.expr)),
            }),
            parse::Stmt::Expr(expr, semi) => {
                StmtKind::Expr(tcx.arena.expr.alloc(Expr::analyze(tcx, &expr)), semi.is_none())
            }
        };
        Self { kind, span }
    }
}

pub mod stmt {
    pub use super::StmtKind::*;
}

pub mod expr {
    pub use super::ExprKind::*;
}

struct AnalyzeCx<'mir, 'hir> {
    tcx: Tx<'hir>,
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
        self.scope = Some(self.tcx.arena.scope.alloc(Scope { parent, inner: HashMap::new() }))
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

    pub fn end_of_block(&mut self, terminator: Terminator) -> mir::BasicBlock {
        self.block.terminator = Some(terminator);
        self.body.basic_blocks.push(mem::replace(
            &mut self.block,
            mir::BasicBlockData { statements: vec![], terminator: None },
        ))
    }

    pub fn next_block(&self) -> mir::BasicBlock {
        mir::BasicBlock::new(self.body.basic_blocks.len() + 1)
    }

    pub fn push_rvalue(&mut self, ty: Ty<'hir>, rvalue: Rvalue<'hir>) -> (Ty<'hir>, Place<'hir>) {
        // Simple one-level optimization of MIR
        match rvalue {
            Rvalue::Use(Operand::Copy(place)) => (ty, place),
            _ => {
                let place = Place::pure(self.body.local_decls.push(LocalDecl { ty: ty.kind }));
                self.block.statements.push(Statement::Assign(place, rvalue));
                (ty, place)
            }
        }
    }

    pub fn unit_place(&mut self, span: Span) -> (Ty<'hir>, Place<'hir>) {
        let unit = Ty { kind: self.tcx.types.unit, span };
        self.push_rvalue(unit, Rvalue::Use(Operand::Const(ConstValue::Zst, unit.kind)))
    }
}

fn assert_same_types<'hir>(a: Ty<'hir>, b: Ty<'hir>) -> Result<'hir, Ty<'hir>> {
    if a == b { Ok(a) } else { Err(Error::TypeMismatch { expected: a, found: b }) }
}

fn goto_next(acx: &mut AnalyzeCx<'_, '_>) -> Terminator {
    Terminator::Goto { target: acx.next_block() }
}

fn analyze_expr<'hir>(
    acx: &mut AnalyzeCx<'_, 'hir>,
    expr: &Expr<'hir>,
) -> Result<'hir, (Ty<'hir>, Place<'hir>)> {
    let (ty, rvalue) = match &expr.kind {
        expr::Lit(Lit::Int(LitInt { lit, span })) => {
            let ty = acx.tcx.types.i64;
            acx.push_rvalue(
                Ty::new(*span, ty),
                Rvalue::Use(Operand::Const(ConstValue::Scalar(ScalarRepr::from(*lit)), ty)),
            )
        }
        expr::Local(name) => {
            acx.scope().get_var(name.ident()).ok_or(Error::NotFoundLocal(*name))?
        }
        expr::Unary(op, expr) => match op {
            UnOp::Neg(_) => {
                let (ty, place) = analyze_expr(acx, expr)?;

                acx.push_rvalue(ty, Rvalue::UnaryOp(*op, Operand::Copy(place)))
            }
            _ => todo!(),
        },
        expr::Binary(op, lhs, rhs) => {
            let (lhs, a) = analyze_expr(acx, lhs)?;
            let (rhs, b) = analyze_expr(acx, rhs)?;

            acx.push_rvalue(
                assert_same_types(lhs, rhs)?,
                Rvalue::BinaryOp(*op, Operand::Copy(a), Operand::Copy(b)),
            )
        }
        expr::Block(stmts, span) => {
            if let Some((tail, stmts)) = stmts.split_last() {
                acx.scoped(|acx| {
                    analyze_local_block_unclosed(acx, stmts)?;
                    Ok(analyze_stmt(acx, tail, Some(goto_next))?
                        .unwrap_or_else(|| acx.unit_place(*span)))
                })?
            } else {
                acx.unit_place(*span)
            }
        }
        panic => todo!("{panic:?}"),
    };

    Ok((ty.with_span(expr.span), rvalue))
}

fn analyze_stmt<'hir>(
    acx: &mut AnalyzeCx<'_, 'hir>,
    stmt: &Stmt<'hir>,
    terminator: Option<fn(&mut AnalyzeCx) -> Terminator>,
) -> Result<'hir, Option<(Ty<'hir>, Place<'hir>)>> {
    Ok(match stmt.kind {
        ref stmt @ (stmt::Local(LocalStmt { init: expr, .. }) | stmt::Expr(expr, _)) => {
            let (ty, place) = analyze_expr(acx, expr)?;

            // Skip block termination of it is empty
            if !acx.block.statements.is_empty()
                && let Some(terminator) = terminator
            {
                let terminator = terminator(acx);
                acx.end_of_block(terminator);
            }

            match stmt {
                StmtKind::Local(LocalStmt { pat, .. }) => {
                    acx.scope().declare_var(pat.ident(), (ty, place));
                    None
                }
                StmtKind::Expr(_, _) => Some((ty, place)),
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

fn make_return<'hir>(acx: &mut AnalyzeCx<'_, 'hir>, ty: Ty<'hir>, place: Place<'hir>) {
    acx.block.statements.push(Statement::Assign(
        Place::pure(Local::RETURN_PLACE),
        Rvalue::Use(Operand::Copy(place)),
    ))
}

fn analyze_body<'hir>(
    acx: &mut AnalyzeCx<'_, 'hir>,
    stmts: &[Stmt<'hir>],
    mut start: mir::BasicBlock,
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
            }
            acx.end_of_block(Terminator::Return);
        });
    }
    Ok(())
}

#[test]
fn analyze() {
    use crate::{parse::ParseBuffer, util::lex_it};

    let src;
    let mut input = lex_it!(
        r#"
        {
             let x = 12;
             let y = {
                let x = 13;
                x * 10
             };
             let z = x;
             // let y = z;
             let _ = x + y;
        }
        "#
        in src
    );
    let src = src.to_string();
    // let Block { stmts, .. } = input.parse()?;
    let parse::Block { stmts, .. } = input.parse().unwrap();

    let arena = Arena::default();
    let tcx = TyCtx::enter(&arena, Session {});
    let mut body = mir::Body {
        argc: 0,
        local_decls: index_vec![LocalDecl { ty: tcx.types.unit }],
        basic_blocks: Default::default(),
    };

    {
        let scope = tcx.arena.scope.alloc(Scope::new());
        let mut acx = AnalyzeCx {
            tcx: &tcx,
            block: mir::BasicBlockData { statements: vec![], terminator: None },
            body: &mut body,
            scope: Some(scope),
        };

        let stmts = stmts.iter().map(|stmt| Stmt::analyze(&tcx, stmt)).collect::<Vec<_>>();
        let _ = analyze_body(
            &mut acx,
            &stmts[..],
            mir::BasicBlock::from_usize(0),
            Ty { kind: tcx.types.unit, span: Span::splat(0) },
        )
        .unwrap_or_else(|err| {
            let settings =
                ReportSettings { err_kw: Color::Magenta, err: Color::Red, kw: Color::Green };
            let (code, reason, labels) = err.report("src/sample.src", settings);
            let mut report = Report::build(ReportKind::Error, "src/sample.src", 5)
                .with_code(code)
                .with_message(reason);
            for label in labels {
                report = report.with_label(label);
            }
            report
                .with_config(
                    ariadne::Config::default()
                        // .with_cross_gap(true)
                        .with_underlines(true),
                )
                .finish()
                .print(("src/sample.src", ariadne::Source::from(src)))
                .unwrap();
            panic!();
        });
    }

    fmt::mir_body(&body);
    mir::codegen::compile_fn(&tcx, &body);
}
