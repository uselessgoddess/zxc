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

impl IntTy {
    pub fn name_str(&self) -> &'static str {
        match *self {
            IntTy::Isize => "isize",
            IntTy::I8 => "i8",
            IntTy::I16 => "i16",
            IntTy::I32 => "i32",
            IntTy::I64 => "i64",
        }
    }

    pub fn name(&self) -> Symbol {
        match *self {
            IntTy::Isize => sym::isize,
            IntTy::I8 => sym::i8,
            IntTy::I16 => sym::i16,
            IntTy::I32 => sym::i32,
            IntTy::I64 => sym::i64,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum UintTy {
    Usize,
    U8,
    U16,
    U32,
    U64,
}

impl UintTy {
    pub fn name_str(&self) -> &'static str {
        match *self {
            UintTy::Usize => "usize",
            UintTy::U8 => "u8",
            UintTy::U16 => "u16",
            UintTy::U32 => "u32",
            UintTy::U64 => "u64",
        }
    }

    pub fn name(&self) -> Symbol {
        todo!()
        // match *self {
        //     UintTy::Usize => sym::usize,
        //     UintTy::U8 => sym::u8,
        //     UintTy::U16 => sym::u16,
        //     UintTy::U32 => sym::u32,
        //     UintTy::U64 => sym::u64,
        // }
    }
}

pub struct TypeAndMut<'tcx> {
    pub ty: Ty<'tcx>,
    pub mutbl: Mutability,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum TyKind<'cx> {
    Bool,
    Int(IntTy),
    Tuple(&'cx List<Ty<'cx>>),
    Ref(Mutability, Ty<'cx>),
    FnDef(mir::DefId), // has no generics now
    Never,
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
                "isize" => tcx.types.isize,
                _ => todo!(),
            },
            Type::Reference(ty::Reference { mutability, ty, .. }) => tcx.intern_ty(Ref(
                Mutability::from_bool(mutability.is_some()),
                Self::analyze(tcx, ty),
            )),
            Type::Paren(Paren { item, .. }) => Self::analyze(tcx, item),
            Type::Tuple(Tuple { items, .. }) => tcx.intern.intern_ty(
                tcx.arena,
                self::Tuple(tcx.mk_type_list(
                    // TODO: add size hint optimizations because tuple's size usually less than 8
                    &items.iter().map(|ty| Self::analyze(tcx, ty)).collect::<SmallVec<_, 8>>(),
                )),
            ),
            Type::Never(_) => tcx.types.never,
        }
    }

    pub fn fn_sig(self, hix: &HirCtx<'cx>) -> FnSig<'cx> {
        match self.kind() {
            FnDef(def) => hix.instances[def].sig,
            _ => panic!("Ty::fn_sig() called on non-fn type: {:?}", self),
        }
    }

    #[inline]
    pub fn is_bool(&self) -> bool {
        matches!(self.kind(), Bool)
    }

    #[inline]
    pub fn is_never(&self) -> bool {
        matches!(self.kind(), Never)
    }

    #[inline]
    pub fn is_unit(&self) -> bool {
        match self.kind() {
            Tuple(list) => list.is_empty(),
            _ => false,
        }
    }

    #[inline]
    pub fn is_zst(&self) -> bool {
        self.is_unit() || self.is_never()
    }

    #[inline]
    pub fn is_primitive(&self) -> bool {
        matches!(self.kind(), Bool | Int(_))
    }

    #[inline]
    pub fn is_integer(&self) -> bool {
        matches!(self.kind(), Int(_))
    }

    pub fn is_signed(self) -> bool {
        matches!(self.kind(), Int(_))
    }

    pub fn is_ptr_sized_int(&self) -> bool {
        matches!(self.kind(), Int(IntTy::Isize) /* | Uint(ty::UintTy::Usize)*/)
    }

    pub fn builtin_deref(self, _explicit: bool) -> Option<TypeAndMut<'cx>> {
        match self.kind() {
            Ref(mutbl, ty) => Some(TypeAndMut { ty, mutbl }),
            _ => None,
        }
    }

    #[inline]
    pub fn ref_mutability(self) -> Option<Mutability> {
        match self.kind() {
            Ref(mutability, _) => Some(mutability),
            _ => None,
        }
    }
}

impl fmt::Debug for Ty<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.kind() {
            Bool => f.write_str("bool"),
            Int(int) => match int {
                IntTy::I8 => f.write_str("i8"),
                IntTy::I16 => f.write_str("i16"),
                IntTy::I32 => f.write_str("i32"),
                IntTy::I64 => f.write_str("i64"),
                IntTy::Isize => f.write_str("isize"),
            },
            Tuple(list) => {
                if list.is_empty() {
                    f.write_str("@unit")
                } else {
                    write!(f, "({})", util::join_fmt_debug(list))
                }
            }
            Ref(mutbl, ty) => {
                write!(f, "&{}{ty:?}", mutbl.prefix_str())
            }
            FnDef(def) => {
                write!(f, "fn({def:?})")
            }
            Never => f.write_str("!"),
        }
    }
}

use {
    crate::{
        hir::HirCtx,
        mir::{self, Mutability},
        symbol::{sym, Symbol},
        tcx::Interned,
        util, Tx,
    },
    lexer::ty,
};
pub use {list::List, sty::*, TyKind::*};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct TyMut<'tcx> {
    pub ty: Ty<'tcx>,
    pub mutbl: mir::Mutability,
}
