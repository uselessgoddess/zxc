use {
    super::{type_sign, value::CValue, FunctionCx},
    compiler::mir::BinOp,
    cranelift::prelude::{InstBuilder, IntCC},
};

pub(crate) fn bin_op_to_int_cc(bin_op: BinOp, signed: bool) -> Option<IntCC> {
    use {BinOp::*, IntCC::*};

    Some(match bin_op {
        Eq => Equal,
        Lt => {
            if signed {
                SignedLessThan
            } else {
                UnsignedLessThan
            }
        }
        Le => {
            if signed {
                SignedLessThanOrEqual
            } else {
                UnsignedLessThanOrEqual
            }
        }
        Ne => NotEqual,
        Ge => {
            if signed {
                SignedGreaterThanOrEqual
            } else {
                UnsignedGreaterThanOrEqual
            }
        }
        Gt => {
            if signed {
                SignedGreaterThan
            } else {
                UnsignedGreaterThan
            }
        }
        _ => return None,
    })
}

pub(crate) fn codegen_bool_binop<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    bin_op: BinOp,
    lhs: CValue<'tcx>,
    rhs: CValue<'tcx>,
) -> CValue<'tcx> {
    let lhs = lhs.load_scalar(fx);
    let rhs = rhs.load_scalar(fx);

    let _b = fx.bcx.ins();
    let res = match bin_op {
        _ => unreachable!("{:?}({:?}, {:?})", bin_op, lhs, rhs),
    };

    CValue::by_val(res, fx.tcx.layout_of(fx.tcx.types.bool))
}

pub fn codegen_int_binop<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    bin_op: BinOp,
    lhs: CValue<'tcx>,
    rhs: CValue<'tcx>,
) -> CValue<'tcx> {
    assert_eq!(lhs.layout().ty, rhs.layout().ty, "int binop requires lhs and rhs of same type");

    let signed = type_sign(lhs.layout().ty);
    let layout = lhs.layout();
    let lhs = lhs.load_scalar(fx);
    let rhs = rhs.load_scalar(fx);
    let b = fx.bcx.ins();
    // FIXME trap on overflow for the Unchecked versions
    let val = match bin_op {
        BinOp::Add | BinOp::AddUnchecked => b.iadd(lhs, rhs),
        BinOp::Sub | BinOp::SubUnchecked => b.isub(lhs, rhs),
        BinOp::Mul | BinOp::MulUnchecked => b.imul(lhs, rhs),
        BinOp::Div => {
            if signed {
                b.sdiv(lhs, rhs)
            } else {
                b.udiv(lhs, rhs)
            }
        }
        _ => unreachable!(),
    };
    CValue::by_val(val, layout)
}
