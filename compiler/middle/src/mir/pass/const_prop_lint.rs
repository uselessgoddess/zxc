use {
    crate::{
        mir::{
            interpret::{self, InterpCx, InterpResult, Scalar, Value},
            lint::errors::OverflowingLiterals,
            pass::{errors::AssertLint, MirLint},
            ty,
            visit::Visitor,
            AssertKind, BasicBlock, BinOp, Body, ConstValue, Local, Location, Operand, Place,
            Rvalue, ScalarRepr, SourceInfo, Statement, StatementKind, Terminator, TerminatorKind,
            Ty, UnOp, START_BLOCK,
        },
        BitSet, Tx,
    },
    abi::Integer,
    lexer::Span,
    lint::builtin,
    std::fmt::Debug,
};

pub struct ConstPropLint;

impl<'tcx> MirLint<'tcx> for ConstPropLint {
    fn run_lint(&self, tcx: Tx<'tcx>, body: &Body<'tcx>) {
        ConstPropagator::new(tcx, body).visit_body(body);
    }
}

struct ConstPropagator<'mir, 'tcx> {
    icx: InterpCx<'mir, 'tcx>,
    tcx: Tx<'tcx>,
    mir: &'mir Body<'tcx>,
    worklist: Vec<BasicBlock>,
    visited_blocks: BitSet<BasicBlock>,
}

impl<'mir, 'tcx> ConstPropagator<'mir, 'tcx> {
    fn new(tcx: Tx<'tcx>, body: &'mir Body<'tcx>) -> Self {
        let mut icx = InterpCx::new(tcx, body);

        for local in &mut icx.frame.locals {
            local.make_live_uninit();
        }

        Self {
            icx,
            tcx,
            mir: body,
            worklist: vec![START_BLOCK],
            visited_blocks: BitSet::new_empty(body.basic_blocks.len()),
        }
    }

    fn icx<F, T>(&mut self, f: F) -> Option<T>
    where
        F: FnOnce(&mut InterpCx<'mir, 'tcx>) -> InterpResult<T>,
    {
        match f(&mut self.icx) {
            Ok(val) => Some(val),
            Err(_error) => None,
        }
    }

    fn eval_const(&mut self, constant: (ConstValue, Ty<'tcx>)) -> Option<Value<'tcx>> {
        self.icx(|icx| icx.eval_const(constant, None))
    }

    fn eval_place_copy(&mut self, place: Place<'tcx>) -> Option<Value<'tcx>> {
        self.icx(|icx| icx.eval_place_copy(place, None))
    }

    fn eval_place(&mut self, place: Place<'tcx>) -> Option<interpret::Place<'tcx>> {
        self.icx(|icx| icx.eval_place(place))
    }

    fn eval_operand(&mut self, operand: Operand<'tcx>) -> Option<Value<'tcx>> {
        match operand {
            Operand::Copy(place) => self.eval_place_copy(place),
            Operand::Const(const_, ty) => self.eval_const((const_, ty)),
        }
    }

    fn report_assert(&self, source_info: &SourceInfo, lint: AssertLint<impl Debug>) {
        self.tcx.emit_spanned_lint(source_info.span, lint.lint(), lint);
    }

    fn check_overflowing_literals(
        &mut self,
        negated: bool,
        ty: Ty<'tcx>,
        repr: ScalarRepr,
        location: Location,
    ) {
        fn from_int(tcx: Tx<'_>, ty: ty::IntTy) -> Integer {
            use Integer::*;

            match ty {
                ty::IntTy::I8 => I8,
                ty::IntTy::I16 => I16,
                ty::IntTy::I32 => I32,
                ty::IntTy::I64 => I64,
                ty::IntTy::Isize => tcx.data_layout().ptr_sized_integer(),
            }
        }

        let (min, max, ty) = match ty.kind() {
            ty::Int(ty) => {
                let size = from_int(self.tcx, ty).size();
                (size.signed_int_min(), size.signed_int_max() as u128, ty.name_str())
            }
            _ => return,
        };

        let span = self.mir.source_info(location).span;
        let lit =
            self.tcx.sess.source_map().span_to_snippet(span).expect("must get snip from literal");

        if (negated && repr.data > max + 1) || (!negated && repr.data > max) {
            self.tcx.emit_spanned_lint(
                span,
                builtin::overflowing_literals,
                OverflowingLiterals { ty, span, lit, min, max },
            );
        }
    }

    fn check_binary_op(
        &mut self,
        op: BinOp,
        left: Operand<'tcx>,
        right: Operand<'tcx>,
        location: Location,
    ) -> Option<()> {
        let a = self.icx(|icx| icx.read_immediate(icx.eval_operand(left, None)?));
        let b = self.icx(|icx| icx.read_immediate(icx.eval_operand(right, None)?));

        if let (Some(a), Some(b)) = (a, b)
            && self.icx(|icx| {
                let (_, overflow) = icx.overflowing_binary_op(op, a, b)?;
                Ok(overflow)
            })?
        {
            let source_info = self.mir.source_info(location);
            self.report_assert(
                source_info,
                AssertLint::ArithmeticOverflow(
                    source_info.span,
                    AssertKind::Overflow(op, a.to_const_int(), b.to_const_int()),
                ),
            );
            return None;
        }
        Some(())
    }

    fn check_rvalue(&mut self, rvalue: &Rvalue<'tcx>, location: Location) -> Option<()> {
        match *rvalue {
            Rvalue::Use(_)
            | Rvalue::UseDeref(_)
            | Rvalue::Ref(_, _)
            | Rvalue::UnaryOp(_, _)
            | Rvalue::Cast(_, _, _)
            | Rvalue::AddrOf(_, _) => {}
            Rvalue::BinaryOp(op, left, right) => {
                self.check_binary_op(op, left, right, location)?;
            }
        }

        Some(())
    }

    fn remove_const(&mut self, local: Local) {
        self.icx.frame.locals[local].make_live_uninit();
    }
}

impl<'tcx> Visitor<'tcx> for ConstPropagator<'_, 'tcx> {
    fn visit_body(&mut self, body: &Body<'tcx>) {
        while let Some(bb) = self.worklist.pop() {
            if !self.visited_blocks.insert(bb) {
                continue;
            }
            self.visit_basic_block_data(bb, &body.basic_blocks[bb]);
        }
    }

    fn visit_constant(&mut self, (const_, ty): (&ConstValue, &Ty<'tcx>), location: Location) {
        self.super_constant((const_, ty), location);
        self.eval_const((*const_, *ty));
    }

    fn visit_assign(&mut self, place: &Place<'tcx>, rvalue: &Rvalue<'tcx>, location: Location) {
        self.super_assign(place, rvalue, location);

        self.check_rvalue(rvalue, location);
    }

    fn visit_rvalue(&mut self, rvalue: &Rvalue<'tcx>, location: Location) {
        match rvalue {
            Rvalue::Use(Operand::Const(ConstValue::Scalar(repr), ty)) => {
                self.check_overflowing_literals(false, *ty, *repr, location)
            }
            Rvalue::UnaryOp(UnOp::Neg, Operand::Const(ConstValue::Scalar(repr), ty)) => {
                self.check_overflowing_literals(true, *ty, *repr, location)
            }
            _ => {}
        }
    }

    fn visit_statement(&mut self, statement: &Statement<'tcx>, location: Location) {
        self.super_statement(statement, location);

        if let StatementKind::Assign(place, _) = statement.kind
            && self.icx(|icx| icx.eval_statement(statement)).is_none()
        {
            self.remove_const(place.local);
        }
    }

    fn visit_terminator(&mut self, terminator: &Terminator<'tcx>, location: Location) {
        self.super_terminator(terminator, location);

        match terminator.kind {
            TerminatorKind::Goto { .. }
            | TerminatorKind::Return
            | TerminatorKind::Unreachable
            | TerminatorKind::Call { .. } => {}
            TerminatorKind::SwitchInt { discr, ref targets } => {
                if let Some(value) = self.eval_operand(discr)
                    && let Some(value) = self.icx(|icx| icx.read_scalar(value))
                    && let Ok(value) = value.try_to_int()
                    && let Ok(value) = value.to_bits(value.size())
                {
                    return self.worklist.push(targets.target_for_value(value));
                }
            }
        }

        self.worklist.extend(terminator.successors());
    }
}
