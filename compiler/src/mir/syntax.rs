use {
    crate::{
        abi::Size,
        index,
        mir::{self, ty::List, Ty},
        Tx,
    },
    index_vec::IndexVec,
    lexer::{Span, UnOp},
    smallvec::{smallvec, SmallVec},
    std::{
        borrow::Cow,
        fmt::{self, Formatter},
        io::Read,
        iter,
        num::NonZeroU8,
    },
};

index::define_index! {
    pub struct BasicBlock = u32;
}

impl fmt::Debug for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "bb{}", self.raw())
    }
}

index_vec::define_index_type! {
    pub struct Local = u32;
}

impl BasicBlock {
    pub const START_BLOCK: Self = Self::from_usize_unchecked(0);
}

impl Local {
    pub const RETURN_PLACE: Self = Self::from_usize_unchecked(0);
}

// SmallVec values for usages `SwitchTargets` as `if`
#[derive(Debug, Clone, PartialEq)]
pub struct SwitchTargets {
    pub values: SmallVec<u128, 1>,
    pub targets: SmallVec<BasicBlock, 2>,
}

impl SwitchTargets {
    pub fn static_if(value: u128, then: BasicBlock, else_: BasicBlock) -> Self {
        Self { values: smallvec![value], targets: smallvec![then, else_] }
    }

    pub fn as_static_if(&self) -> Option<(u128, BasicBlock, BasicBlock)> {
        if let &[value] = &self.values[..]
            && let &[then, else_] = &self.targets[..]
        {
            Some((value, then, else_))
        } else {
            None
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (u128, BasicBlock)> + ExactSizeIterator + '_ {
        iter::zip(&self.values, &self.targets).map(|(a, b)| (*a, *b))
    }
}

#[derive(Debug, Clone)]
pub enum Terminator<'tcx> {
    Goto {
        target: BasicBlock,
    },
    SwitchInt {
        discr: Operand<'tcx>,
        targets: SwitchTargets,
    },
    Return,
    Unreachable,
    Call {
        func: Operand<'tcx>,
        args: Vec<Operand<'tcx>>, // SmallVec?
        dest: Place<'tcx>,
        target: Option<BasicBlock>,
        fn_span: Span,
    },
}

pub type Successors<'a> = impl DoubleEndedIterator<Item = BasicBlock> + 'a;

impl<'tcx> Terminator<'tcx> {
    pub fn successors(&self) -> Successors<'_> {
        use Terminator::*;

        let tail_slice = (&[]).into_iter().copied();
        match *self {
            Call { target: Some(t), .. } => Some(t).into_iter().chain(tail_slice),
            Goto { target: t } => Some(t).into_iter().chain(tail_slice),
            Return | Unreachable | Call { target: None, .. } => None.into_iter().chain(tail_slice),
            SwitchInt { ref targets, .. } => {
                None.into_iter().chain(targets.targets.iter().copied())
            }
        }
    }

    pub fn fmt_successor_labels(&self) -> Vec<Cow<'static, str>> {
        use Terminator::*;

        match *self {
            Return | Unreachable => vec![],
            Goto { .. } => vec!["".into()],
            SwitchInt { ref targets, .. } => targets
                .values
                .iter()
                .map(u128::to_string)
                .map(Cow::Owned)
                .chain(iter::once("otherwise".into()))
                .collect(),
            Call { target: Some(_), .. } => vec!["return".into()],
            Call { target: None, .. } => vec![],
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum PlaceElem<'tcx> {
    Subtype(Ty<'tcx>), // also applicable for non-transmuting types
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Place<'tcx> {
    pub local: Local,
    pub projection: &'tcx List<PlaceElem<'tcx>>,
}

impl<'tcx> Place<'tcx> {
    pub fn pure(local: Local) -> Self {
        Self { local, projection: List::empty() }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operand<'tcx> {
    Copy(Place<'tcx>),
    Const(ConstValue, Ty<'tcx>),
}

impl<'tcx> Operand<'tcx> {
    pub fn ty(&self, decls: &IndexVec<Local, LocalDecl<'tcx>>, _tcx: Tx<'tcx>) -> Ty<'tcx> {
        // TODO: later also use `projection`
        match *self {
            Operand::Copy(place) => decls[place.local].ty,
            Operand::Const(_, ty) => ty,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ScalarRepr {
    pub(crate) data: u128,
    pub(crate) size: NonZeroU8,
}

impl ScalarRepr {
    pub const TRUE: ScalarRepr = ScalarRepr { data: 1_u128, size: NonZeroU8::new(1).unwrap() };
    pub const FALSE: ScalarRepr = ScalarRepr { data: 0_u128, size: NonZeroU8::new(1).unwrap() };

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

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum BinOp {
    Add,
    AddUnchecked,
    Sub,
    SubUnchecked,
    Mul,
    MulUnchecked,
    Div,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
}

impl BinOp {
    pub fn from_parse(op: lexer::BinOp) -> Option<Self> {
        use lexer::BinOp;
        Some(match op {
            BinOp::Add(_) => Self::Add,
            BinOp::Sub(_) => Self::Sub,
            BinOp::Mul(_) => Self::Mul,
            BinOp::Div(_) => Self::Div,
            BinOp::Eq(_) => Self::Eq,
            BinOp::Ne(_) => Self::Ne,
            BinOp::Le(_) => Self::Le,
            BinOp::Lt(_) => Self::Lt,
            BinOp::Ge(_) => Self::Ge,
            BinOp::Gt(_) => Self::Gt,
            _ => return None,
        })
    }

    pub fn ty<'tcx>(&self, tcx: Tx<'tcx>, lhs: Ty<'tcx>, rhs: Ty<'tcx>) -> Ty<'tcx> {
        match *self {
            BinOp::Add
            | BinOp::AddUnchecked
            | BinOp::Sub
            | BinOp::SubUnchecked
            | BinOp::Mul
            | BinOp::MulUnchecked
            | BinOp::Div => {
                assert_eq!(lhs, rhs);
                lhs
            }
            BinOp::Eq | BinOp::Lt | BinOp::Le | BinOp::Ne | BinOp::Ge | BinOp::Gt => tcx.types.bool,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Rvalue<'tcx> {
    Use(Operand<'tcx>),
    UseDeref(Place<'tcx>),
    UnaryOp(UnOp, Operand<'tcx>),
    BinaryOp(BinOp, Operand<'tcx>, Operand<'tcx>),
    Cast(CastKind, Operand<'tcx>, Ty<'tcx>),
}

#[derive(Debug, Clone)]
pub enum Statement<'tcx> {
    Assign(Place<'tcx>, Rvalue<'tcx>),
    Nop,
}

#[derive(Debug, Clone)]
pub struct BasicBlockData<'tcx> {
    pub statements: Vec<Statement<'tcx>>,
    pub terminator: Option<Terminator<'tcx>>,
}

impl<'tcx> BasicBlockData<'tcx> {
    pub fn terminator(&self) -> &Terminator<'tcx> {
        self.terminator.as_ref().expect("invalid hir analyzing")
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Copy)]
pub enum Mutability {
    Not, // `Not` less than `Mut`
    Mut,
}

#[derive(Debug, Copy, Clone)]
pub struct LocalDecl<'tcx> {
    pub mutability: Mutability,
    pub ty: Ty<'tcx>,
}

#[derive(Debug, Clone, Default)]
pub struct Body<'tcx> {
    pub argc: usize,
    pub local_decls: IndexVec<Local, LocalDecl<'tcx>>,
    pub basic_blocks: IndexVec<BasicBlock, BasicBlockData<'tcx>>,
}

impl<'tcx> Body<'tcx> {
    #[inline]
    pub fn args_iter(&self) -> impl ExactSizeIterator<Item = Local> {
        (1..self.argc + 1).map(Local::new)
    }

    #[inline]
    pub fn vars_and_temps_iter(&self) -> impl ExactSizeIterator<Item = Local> {
        (self.argc + 1..self.local_decls.len()).map(Local::new)
    }
}
