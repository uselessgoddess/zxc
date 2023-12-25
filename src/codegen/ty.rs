use {
    crate::codegen::{list::List, Interned},
    cranelift::prelude::types,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum IntTy {
    I64,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum TyKind<'cx> {
    Int(IntTy),
    Tuple(&'cx List<Ty<'cx>>),
}

pub type Ty<'cx> = Interned<'cx, TyKind<'cx>>;

impl<'cx> Ty<'cx> {
    pub fn kind(&self) -> TyKind<'cx> {
        *self.0
    }
}

use crate::codegen::Tx;

pub use TyKind::*;

pub(crate) fn clif_type_from_ty<'tcx>(tcx: Tx<'tcx>, ty: Ty<'tcx>) -> Option<types::Type> {
    Some(match ty.kind() {
        Int(size) => match size {
            IntTy::I64 => types::I64,
        },
        _ => return None,
    })
}
