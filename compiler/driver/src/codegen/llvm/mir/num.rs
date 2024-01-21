use {
    crate::codegen::llvm::Bx,
    llvm::{IntPredicate, Value},
    middle::mir::{BinOp, Ty},
};

pub fn binop_to_intcc(op: BinOp, signed: bool) -> IntPredicate {
    match op {
        BinOp::Eq => IntPredicate::IntEQ,
        BinOp::Ne => IntPredicate::IntNE,
        BinOp::Lt => {
            if signed {
                IntPredicate::IntSLT
            } else {
                IntPredicate::IntULT
            }
        }
        BinOp::Le => {
            if signed {
                IntPredicate::IntSLE
            } else {
                IntPredicate::IntULE
            }
        }
        BinOp::Gt => {
            if signed {
                IntPredicate::IntSGT
            } else {
                IntPredicate::IntUGT
            }
        }
        BinOp::Ge => {
            if signed {
                IntPredicate::IntSGE
            } else {
                IntPredicate::IntUGE
            }
        }
        op => unreachable!("{op:?}"),
    }
}

pub fn codegen_int_binop<'ll, 'tcx>(
    bx: &mut Bx<'_, 'll, 'tcx>,
    op: BinOp,
    lhs: &'ll Value,
    rhs: &'ll Value,
    ty: Ty<'tcx>,
) -> &'ll Value {
    let signed = ty.is_signed();
    let routine = match (op, signed) {
        (BinOp::Add, _) => Bx::add,
        (BinOp::Sub, _) => Bx::sub,
        (BinOp::Mul, _) => Bx::mul,
        (BinOp::Div, true) => Bx::sdiv,
        (BinOp::Div, false) => Bx::udiv,
        (BinOp::AddUnchecked, true) => Bx::unchecked_sadd,
        (BinOp::AddUnchecked, false) => Bx::unchecked_uadd,
        (BinOp::SubUnchecked, true) => Bx::unchecked_ssub,
        (BinOp::SubUnchecked, false) => Bx::unchecked_usub,
        (BinOp::MulUnchecked, true) => Bx::unchecked_smul,
        (BinOp::MulUnchecked, false) => Bx::unchecked_umul,

        (BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge, signed) => {
            return bx.icmp(binop_to_intcc(op, signed), lhs, rhs);
        }
    };
    routine(bx, lhs, rhs)
}
