use crate::mir::{ty, CastKind, Mutability, Ty};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IntTy {
    U,
    I,
    Bool,
}

impl IntTy {
    pub fn is_signed(&self) -> bool {
        matches!(self, Self::I)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CastTy<'tcx> {
    Int(IntTy),
    Ptr(Mutability, Ty<'tcx>),
}

impl<'tcx> From<Ty<'tcx>> for CastTy<'tcx> {
    fn from(value: Ty<'tcx>) -> Self {
        CastKind::cast_ty(value).expect("bad type for cast")
    }
}

impl CastKind {
    fn cast_ty(t: Ty) -> Option<CastTy> {
        match t.kind() {
            ty::Bool => Some(CastTy::Int(IntTy::Bool)),
            ty::Int(_) => Some(CastTy::Int(IntTy::I)),
            ty::Uint(_) => Some(CastTy::Int(IntTy::U)),
            ty::Ptr(mutbl, ty) => Some(CastTy::Ptr(mutbl, ty)),
            _ => None,
        }
    }

    pub fn from_cast<'tcx>(from: Ty<'tcx>, cast: Ty<'tcx>) -> Option<Self> {
        let from = Self::cast_ty(from)?;
        let cast = Self::cast_ty(cast)?;

        Some(match (from, cast) {
            (CastTy::Int(_), CastTy::Int(_)) => Self::IntToInt,
            (CastTy::Ptr(..), CastTy::Ptr(..)) => Self::PtrToPtr,
            (CastTy::Ptr(..), CastTy::Int(..)) => Self::PtrToAddr,
            (CastTy::Int(..), CastTy::Ptr(..)) => Self::AddrToPtr,
            _ => panic!("non-castable {from:?} as {cast:?}"),
        })
    }
}
