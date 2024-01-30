use {
    super::{type_sign, value::CValue, FunctionCx},
    cranelift::prelude::{InstBuilder, IntCC, Value},
    middle::mir::BinOp,
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

fn codegen_compare_bin_op<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    bin_op: BinOp,
    signed: bool,
    lhs: Value,
    rhs: Value,
) -> CValue<'tcx> {
    let intcc = bin_op_to_int_cc(bin_op, signed).unwrap();
    let val = fx.bcx.ins().icmp(intcc, lhs, rhs);
    CValue::by_val(val, fx.tcx.layout_of(fx.tcx.types.bool))
}

pub fn codegen_ptr_binop<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    bin_op: BinOp,
    lhs: CValue<'tcx>,
    rhs: CValue<'tcx>,
) -> CValue<'tcx> {
    match bin_op {
        BinOp::Eq | BinOp::Lt | BinOp::Le | BinOp::Ne | BinOp::Ge | BinOp::Gt => {
            let (lhs, rhs) = (lhs.load_scalar(fx), rhs.load_scalar(fx));
            codegen_compare_bin_op(fx, bin_op, false, lhs, rhs)
        }
        BinOp::Offset => {
            let pointee = lhs.layout().ty.builtin_deref(true).unwrap().ty;
            let pointee_size = fx.tcx.layout_of(pointee).size.bytes();

            let (ptr, delta) = (lhs, rhs.load_scalar(fx));
            let delta = fx.bcx.ins().imul_imm(delta, pointee_size as i64);

            let ptr_val = ptr.load_scalar(fx);
            CValue::by_val(fx.bcx.ins().iadd(ptr_val, delta), ptr.layout())
        }
        _ => unreachable!("{:?}({:?}, {:?})", bin_op, lhs, rhs),
    }
}
