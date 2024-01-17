use crate::{
    mir::{Body, ConstValue, MirPass, Operand, Terminator},
    Tx,
};

pub enum SimplifyConstCondition {
    AfterConstProp,
    Final,
}
impl<'tcx> MirPass<'tcx> for SimplifyConstCondition {
    fn name(&self) -> &'static str {
        match self {
            SimplifyConstCondition::AfterConstProp => "SimplifyConstCondition-after-const-prop",
            SimplifyConstCondition::Final => "SimplifyConstCondition-final",
        }
    }

    fn run_pass(&self, _: Tx<'tcx>, body: &mut Body<'tcx>) {
        for block in &mut body.basic_blocks {
            let terminator = block.terminator_mut();

            *terminator = match terminator {
                Terminator::SwitchInt {
                    discr: Operand::Const(const_, _ty), ref targets, ..
                } => {
                    let constant = match const_ {
                        ConstValue::Scalar(scalar) => Some(scalar.data),
                        ConstValue::Zst => None,
                    };
                    if let Some(constant) = constant {
                        let target = targets.target_for_value(constant);
                        Terminator::Goto { target }
                    } else {
                        continue;
                    }
                }
                _ => continue,
            };
        }
    }
}
