use {
    crate::{
        codegen::{ty, CValue, FunctionCx, Ty},
        parse::BinOp,
    },
    cranelift::prelude::InstBuilder,
};

pub(crate) fn type_sign(ty: Ty<'_>) -> bool {
    match ty.kind() {
        ty::Int(..) => true,
        _ => todo!(),
    }
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
        BinOp::Add(_) => b.iadd(lhs, rhs),
        BinOp::Sub(_) => b.isub(lhs, rhs),
        BinOp::Mul(_) => b.imul(lhs, rhs),
        BinOp::Div(_) => {
            if signed {
                b.sdiv(lhs, rhs)
            } else {
                b.udiv(lhs, rhs)
            }
        }
        _ => todo!(),
    };

    CValue::by_val(val, layout)
}
