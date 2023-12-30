use {
    crate::{
        codegen::{abi::Size, list::List, mir::Ty},
        parse::{BinOp, UnOp},
    },
    index_vec::IndexVec,
    std::num::NonZeroU8,
};

index_vec::define_index_type! {
    pub struct BasicBlock = u32;
}

index_vec::define_index_type! {
    pub struct Local = u32;
}

impl Local {
    pub const RETURN_PLACE: Self = Self::from_usize_unchecked(0);
}

#[derive(Copy, Clone)]
pub enum Terminator {
    Goto { target: BasicBlock },
    Return,
    Unreachable,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum PlaceElem<'tcx> {
    Subtype(Ty<'tcx>), // also applicable for non-transmuting types
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Place<'tcx> {
    pub local: Local,
    pub projection: &'tcx List<PlaceElem<'tcx>>,
}

impl<'tcx> Place<'tcx> {
    pub fn pure(local: Local) -> Self {
        Self { local, projection: List::empty() }
    }
}

#[derive(Clone, Copy)]
pub enum Operand<'tcx> {
    Copy(Place<'tcx>),
    Const(ConstValue, Ty<'tcx>),
}

#[derive(Debug, Copy, Clone)]
pub struct ScalarRepr {
    data: u128,
    size: NonZeroU8,
}

impl ScalarRepr {
    pub fn size(&self) -> Size {
        Size::from_bytes(self.size.get() as u64)
    }

    pub fn to_bits(self, target_size: Size) -> Result<u128, Size> {
        assert_ne!(target_size.bytes(), 0, "you should never look at the bits of a ZST");
        if target_size.bytes() == u64::from(self.size.get()) {
            self.check_data();
            Ok(self.data)
        } else {
            Err(self.size())
        }
    }

    #[inline(always)]
    fn check_data(self) {
        // Using a block `{self.data}` here to force a copy instead of using `self.data`
        // directly, because `debug_assert_eq` takes references to its arguments and formatting
        // arguments and would thus borrow `self.data`. Since `Self`
        // is a packed struct, that would create a possibly unaligned reference, which
        // is UB.
        debug_assert_eq!(
            self.size().truncate(self.data),
            { self.data },
            "Scalar value {:#x} exceeds size of {} bytes",
            { self.data },
            self.size
        );
    }
}

macro_rules! from {
    ($($ty:ty)*) => {$(
        impl From<$ty> for ScalarRepr {
            #[inline]
            fn from(u: $ty) -> Self {
                Self {
                    data: u128::from(u),
                    size: NonZeroU8::new(std::mem::size_of::<$ty>() as u8).unwrap(),
                }
            }
        }
    )*}
}

from!(u8 u16 u32 u64);

#[derive(Debug, Copy, Clone)]
pub enum ConstValue {
    Scalar(ScalarRepr),
    Zst,
}

#[derive(Clone, Copy, Debug)]
pub enum CastKind {
    IntToInt,
}

#[derive(Copy, Clone)]
pub enum Rvalue<'tcx> {
    Use(Operand<'tcx>),
    UseDeref(Place<'tcx>),
    UnaryOp(UnOp, Operand<'tcx>),
    BinaryOp(BinOp, Operand<'tcx>, Operand<'tcx>),
    Cast(CastKind, Operand<'tcx>, Ty<'tcx>),
}

#[derive(Clone)]
pub enum Statement<'tcx> {
    Assign(Place<'tcx>, Rvalue<'tcx>),
    Nop,
}

#[derive(Debug, Clone)]
pub struct BasicBlockData<'tcx> {
    pub statements: Vec<Statement<'tcx>>,
    pub terminator: Option<Terminator>,
}

impl BasicBlockData<'_> {
    pub fn terminator(&self) -> Terminator {
        self.terminator.expect("invalid hir analyzing")
    }
}

#[derive(Debug, Clone)]
pub struct LocalDecl<'tcx> {
    pub ty: Ty<'tcx>,
}

#[derive(Debug, Clone)]
pub struct Body<'tcx> {
    pub argc: usize,
    pub local_decls: IndexVec<Local, LocalDecl<'tcx>>,
    pub basic_blocks: IndexVec<BasicBlock, BasicBlockData<'tcx>>,
}

impl<'tcx> Body<'tcx> {
    #[inline]
    pub fn vars_and_temps_iter(
        &self,
    ) -> impl DoubleEndedIterator<Item = Local> + ExactSizeIterator {
        (self.argc + 1..self.local_decls.len()).map(Local::new)
    }
}
