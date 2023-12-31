pub mod cast;
mod list;
mod sty;

use {
    smallvec::SmallVec,
    std::{fmt, fmt::Formatter},
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum IntTy {
    Isize,
    I8,
    I16,
    I32,
    I64,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum UintTy {
    Usize,
    U8,
    U16,
    U32,
    U64,
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

    pub fn analyze(tcx: Tx<'cx>, ty: &lexer::Type<'cx>) -> Ty<'cx> {
        use lexer::ty::{Paren, Tuple, Type};

        match ty {
            Type::Ident(name) => match name.ident() {
                "i8" => tcx.types.i8,
                "i16" => tcx.types.i16,
                "i32" => tcx.types.i32,
                "i64" => tcx.types.i64,
                _ => todo!(),
            },
            Type::Paren(Paren { item, .. }) => Self::analyze(tcx, item),
            Type::Tuple(Tuple { items, .. }) => tcx.intern.intern_ty(
                tcx.arena,
                self::Tuple(tcx.mk_type_list(
                    // TODO: add size hint optimizations because tuple's size usually less than 8
                    &items.iter().map(|ty| Self::analyze(tcx, ty)).collect::<SmallVec<_, 8>>(),
                )),
            ),
        }
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
                _ => todo!(),
            },
            Tuple(list) => {
                if list.is_empty() {
                    f.write_str("@unit")
                } else {
                    write!(f, "({})", util::join_fmt(list, Ty::to_string))
                }
            }
        }
    }
}

use crate::{tcx::Interned, util, Tx};
pub use {list::List, sty::*, TyKind::*};
