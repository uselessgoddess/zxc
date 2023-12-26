use {
    crate::codegen::{list::List, Interned},
    cranelift::prelude::types,
    std::{fmt, fmt::Formatter},
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum IntTy {
    I8,
    I16,
    I32,
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

impl fmt::Display for Ty<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.kind() {
            Int(int) => match int {
                IntTy::I8 => f.write_str("i8"),
                IntTy::I16 => f.write_str("i16"),
                IntTy::I32 => f.write_str("i32"),
                IntTy::I64 => f.write_str("i64"),
            },
            Tuple(list) => {
                if list.is_empty() {
                    f.write_str("@unit")
                } else {
                    let mut tuple = f.debug_tuple("");
                    for ty in list {
                        tuple.field(&ty);
                    }
                    tuple.finish()
                }
            }
        }
    }
}

use crate::codegen::Tx;

pub use TyKind::*;

pub(crate) fn clif_type_from_ty<'tcx>(tcx: Tx<'tcx>, ty: Ty<'tcx>) -> Option<types::Type> {
    Some(match ty.kind() {
        Int(size) => match size {
            IntTy::I8 => types::I8,
            IntTy::I16 => types::I16,
            IntTy::I32 => types::I32,
            IntTy::I64 => types::I64,
        },
        _ => return None,
    })
}
