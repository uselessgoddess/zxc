use crate::codegen::{ty, Ty};

pub(crate) fn type_sign(ty: Ty<'_>) -> bool {
    match ty.kind() {
        ty::Int(..) => true,
        _ => todo!(),
    }
}
