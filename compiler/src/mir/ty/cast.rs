use crate::mir::{ty, CastKind, Ty};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IntTy {
    U(ty::UintTy),
    I,
    Bool,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CastTy {
    Int(IntTy),
}

impl CastKind {
    fn cast_ty(t: Ty) -> Option<CastTy> {
        match t.kind() {
            ty::Bool => Some(CastTy::Int(IntTy::Bool)),
            ty::Int(_) => Some(CastTy::Int(IntTy::I)),
            _ => None,
        }
    }

    pub fn from_cast<'tcx>(from: Ty<'tcx>, cast: Ty<'tcx>) -> Option<Self> {
        let from = Self::cast_ty(from)?;
        let cast = Self::cast_ty(cast)?;

        Some(match (from, cast) {
            (CastTy::Int(_), CastTy::Int(_)) => Self::IntToInt,
            _ => todo!(),
        })
    }
}
