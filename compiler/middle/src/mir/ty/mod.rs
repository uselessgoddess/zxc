pub mod cast;
mod list;
mod sty;

use {
    smallvec::{smallvec, SmallVec},
    std::{
        fmt,
        fmt::{Formatter, Write},
    },
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

idx::define_index! {
    #[derive(Debug)]
    pub struct InferId = u32;
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Infer {
    /// `{integer}`
    Int(InferId),
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum TyKind<'cx> {
    Bool,
    Int(IntTy),
    Uint(UintTy),
    Tuple(&'cx List<Ty<'cx>>),
    Ref(Mutability, Ty<'cx>),
    Ptr(Mutability, Ty<'cx>),
    FnDef(mir::DefId), // has no generics now
    Infer(Infer),
    Never,
}

pub type Ty<'cx> = Interned<'cx, TyKind<'cx>>;

impl<'cx> Ty<'cx> {
    pub fn new(tcx: Tx<'cx>, kind: TyKind<'cx>) -> Ty<'cx> {
        tcx.intern_ty(kind)
    }

    #[inline]
    pub fn new_ptr(tcx: Tx<'cx>, mutbl: Mutability, ty: Ty<'cx>) -> Ty<'cx> {
        Ty::new(tcx, Ptr(mutbl, ty))
    }

    pub fn kind(&self) -> TyKind<'cx> {
        *self.0
    }

    pub fn analyze(tcx: Tx<'cx>, ty: &lexer::Type<'cx>) -> Ty<'cx> {
        use lexer::ty::{Paren, Tuple, Type};

        match ty {
            Type::Ident(name) => match name.ident() {
                "bool" => tcx.types.bool,
                "i8" => tcx.types.i8,
                "i16" => tcx.types.i16,
                "i32" => tcx.types.i32,
                "i64" => tcx.types.i64,
                "isize" => tcx.types.isize,
                "u8" => tcx.types.u8,
                "u16" => tcx.types.u16,
                "u32" => tcx.types.u32,
                "u64" => tcx.types.u64,
                "usize" => tcx.types.usize,
                _ => todo!(),
            },
            Type::Reference(ty::Reference { mutability, ty, .. }) => tcx.intern_ty(Ref(
                Mutability::from_bool(mutability.is_some()),
                Self::analyze(tcx, ty),
            )),
            Type::Pointer(ty::Pointer { qual, ty, .. }) => {
                tcx.intern_ty(Ptr(Mutability::from_bool(qual.is_mut()), Self::analyze(tcx, ty)))
            }
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
        matches!(self.kind(), Bool | Int(_) | Uint(_))
    }

    #[inline]
    pub fn is_integer(&self) -> bool {
        matches!(self.kind(), Int(_) | Uint(_) | Infer(Infer::Int(_)))
    }

    #[inline]
    pub fn is_unsafe_ptr(self) -> bool {
        matches!(self.kind(), Ptr(..))
    }

    pub fn is_signed(self) -> bool {
        matches!(self.kind(), Int(_))
    }

    pub fn is_ptr_sized(&self) -> bool {
        matches!(self.kind(), Int(IntTy::Isize) /* | Uint(ty::UintTy::Usize)*/)
    }

    #[inline]
    pub fn needs_infer(&self) -> bool {
        matches!(self.kind(), Infer(_))
    }

    pub fn builtin_deref(self, explicit: bool) -> Option<TypeAndMut<'cx>> {
        match self.kind() {
            Ref(mutbl, ty) => Some(TypeAndMut { mutbl, ty }),
            Ptr(mutbl, ty) if explicit => Some(TypeAndMut { mutbl, ty }),
            _ => None,
        }
    }

    #[inline]
    pub fn ref_mutability(self) -> Option<Mutability> {
        match self.kind() {
            Ref(mutability, _) | Ptr(mutability, _) => Some(mutability),
            _ => None,
        }
    }

    pub fn walk(self) -> TypeWalker<'cx> {
        TypeWalker::new(self)
    }
}

pub struct TypeWalker<'tcx> {
    stack: SmallVec<Ty<'tcx>, 8>,
    visited: FxHashSet<Ty<'tcx>>, // todo: use small storage optimized hash map
}

impl<'tcx> TypeWalker<'tcx> {
    pub fn new(root: Ty<'tcx>) -> Self {
        Self { stack: smallvec![root], visited: Default::default() }
    }

    fn push_impl(stack: &mut SmallVec<Ty<'tcx>, 8>, parent: Ty<'tcx>) {
        match parent.kind() {
            Bool | Int(_) | Uint(_) | FnDef(_) | Infer(_) | Never => {}
            Ref(_, ty) | Ptr(_, ty) => stack.push(ty),
            Tuple(_) => todo!(),
        }
    }
}

impl<'tcx> Iterator for TypeWalker<'tcx> {
    type Item = Ty<'tcx>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next = self.stack.pop()?;
            if self.visited.insert(next) {
                Self::push_impl(&mut self.stack, next);
                return Some(next);
            }
        }
    }
}

impl fmt::Debug for Ty<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.kind() {
            Bool => f.write_str("bool"),
            Int(int) => f.write_str(int.name_str()),
            Uint(int) => f.write_str(int.name_str()),
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
            Ptr(mutbl, ty) => {
                write!(
                    f,
                    "*{} {ty:?}",
                    match mutbl {
                        Mutability::Not => "const",
                        Mutability::Mut => "mut",
                    }
                )
            }
            FnDef(def) => {
                write!(f, "fn({def:?})")
            }
            Never => f.write_str("!"),
            Infer(_) => f.write_str("{integer}"),
        }
    }
}

use {
    crate::{
        hir::HirCtx,
        idx,
        mir::{self, Mutability},
        symbol::{sym, Symbol},
        tcx::Interned,
        util, FxHashSet, Tx,
    },
    lexer::ty,
};
pub use {list::List, sty::*, TyKind::*};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct TyMut<'tcx> {
    pub ty: Ty<'tcx>,
    pub mutbl: mir::Mutability,
}
