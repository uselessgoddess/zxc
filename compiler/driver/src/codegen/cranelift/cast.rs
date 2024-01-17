use {
    super::FunctionCx,
    cranelift::prelude::{InstBuilder, Type, Value},
};

pub(crate) fn clif_intcast(
    fx: &mut FunctionCx<'_, '_, '_>,
    val: Value,
    to: Type,
    signed: bool,
) -> Value {
    let from = fx.bcx.func.dfg.value_type(val);
    match (from, to) {
        // equal
        (_, _) if from == to => val,

        // extend
        (_, _) if to.wider_or_equal(from) => {
            if signed {
                fx.bcx.ins().sextend(to, val)
            } else {
                fx.bcx.ins().uextend(to, val)
            }
        }

        // reduce
        (_, _) => fx.bcx.ins().ireduce(to, val),
    }
}
