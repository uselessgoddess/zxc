use {
    crate::{
        mir::{
            lint::errors::OverflowingLiterals,
            pass::MirLint,
            ty,
            visit::{MutVisitor, Visitor},
            Body, ConstValue, Location, MirPass, Operand, Rvalue, ScalarRepr, Ty, UnOp,
        },
        Tx,
    },
    abi::Integer::{self, *},
    lint::builtin,
};

pub struct NormalizeLiterals;

impl<'tcx> MirPass<'tcx> for NormalizeLiterals {
    fn run_pass(&self, tcx: Tx<'tcx>, body: &mut Body<'tcx>) {
        Literals { tcx }.visit_body_preserves_cfg(body)
    }
}

pub struct Literals<'tcx> {
    tcx: Tx<'tcx>,
}

impl<'tcx> MutVisitor<'tcx> for Literals<'tcx> {
    fn tcx(&self) -> Tx<'tcx> {
        self.tcx
    }

    fn visit_rvalue(&mut self, rvalue: &mut Rvalue<'tcx>, _: Location) {
        match rvalue {
            Rvalue::Use(Operand::Const(ConstValue::Scalar(repr), ty)) => {
                repr.data = repr.size().truncate(repr.data);
            }
            _ => {}
        }
    }
}

pub struct NormalizeLiteralsLint;

impl<'tcx> MirLint<'tcx> for NormalizeLiteralsLint {
    fn run_lint(&self, tcx: Tx<'tcx>, mir: &Body<'tcx>) {
        LiteralsLint { tcx, mir }.visit_body(mir)
    }
}

pub struct LiteralsLint<'mir, 'tcx> {
    tcx: Tx<'tcx>,
    mir: &'mir Body<'tcx>,
}

impl<'tcx> LiteralsLint<'_, 'tcx> {
    fn check_overflowing_literals(
        &mut self,
        negated: bool,
        ty: Ty<'tcx>,
        repr: ScalarRepr,
        location: Location,
    ) {
        fn from_int(tcx: Tx<'_>, ty: ty::IntTy) -> Integer {
            match ty {
                ty::IntTy::I8 => I8,
                ty::IntTy::I16 => I16,
                ty::IntTy::I32 => I32,
                ty::IntTy::I64 => I64,
                ty::IntTy::Isize => tcx.data_layout().ptr_sized_integer(),
            }
        }

        fn from_uint(tcx: Tx<'_>, ty: ty::UintTy) -> Integer {
            match ty {
                ty::UintTy::U8 => I8,
                ty::UintTy::U16 => I16,
                ty::UintTy::U32 => I32,
                ty::UintTy::U64 => I64,
                ty::UintTy::Usize => tcx.data_layout().ptr_sized_integer(),
            }
        }

        let (min, max, ty) = match ty.kind() {
            ty::Int(ty) => {
                let size = from_int(self.tcx, ty).size();
                (size.signed_int_min(), size.signed_int_max() as u128, ty.name_str())
            }
            ty::Uint(ty) => {
                let size = from_uint(self.tcx, ty).size();
                (0, size.unsigned_int_max(), ty.name_str())
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
}

impl<'tcx> Visitor<'tcx> for LiteralsLint<'_, 'tcx> {
    fn visit_rvalue(&mut self, rvalue: &Rvalue<'tcx>, location: Location) {
        let negated = matches!(rvalue, Rvalue::UnaryOp(..));
        match rvalue {
            Rvalue::Use(Operand::Const(ConstValue::Scalar(repr), ty))
            | Rvalue::UnaryOp(UnOp::Neg, Operand::Const(ConstValue::Scalar(repr), ty)) => {
                self.check_overflowing_literals(negated, *ty, *repr, location);
            }
            _ => {}
        }
    }
}
