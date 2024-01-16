use {
    crate::{
        abi::Size,
        idx::{self, IndexVec},
        mir::{ty::List, Ty, RETURN_PLACE},
        Tx,
    },
    lexer::Span,
    smallvec::{smallvec, SmallVec},
    std::{
        borrow::Cow,
        fmt::{self, Formatter},
        iter,
        num::NonZeroU8,
        slice,
    },
};

idx::define_index! {
    pub struct BasicBlock = u32;
}

impl fmt::Debug for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "bb{}", self.raw())
    }
}

idx::define_index! {
    #[derive(Debug)]
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
    pub fn new(targets: impl Iterator<Item = (u128, BasicBlock)>, otherwise: BasicBlock) -> Self {
        let (values, mut targets): (SmallVec<_, 1>, SmallVec<_, 2>) = targets.unzip();
        targets.push(otherwise);
        Self { values, targets }
    }

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

    pub fn otherwise(&self) -> BasicBlock {
        *self.targets.last().unwrap()
    }

    pub fn iter(&self) -> impl Iterator<Item = (u128, BasicBlock)> + ExactSizeIterator + '_ {
        iter::zip(&self.values, &self.targets).map(|(a, b)| (*a, *b))
    }

    pub fn target_for_value(&self, value: u128) -> BasicBlock {
        self.iter().find_map(|(v, t)| (v == value).then_some(t)).unwrap_or_else(|| self.otherwise())
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
pub type SuccessorsMut<'a> =
    iter::Chain<std::option::IntoIter<&'a mut BasicBlock>, slice::IterMut<'a, BasicBlock>>;

impl<'tcx> Terminator<'tcx> {
    pub fn successors(&self) -> Successors<'_> {
        use Terminator::*;

        let tail_slice = [].iter().copied();
        match *self {
            Call { target: Some(t), .. } => Some(t).into_iter().chain(tail_slice),
            Goto { target: t } => Some(t).into_iter().chain(tail_slice),
            Return | Unreachable | Call { target: None, .. } => None.into_iter().chain(tail_slice),
            SwitchInt { ref targets, .. } => {
                None.into_iter().chain(targets.targets.iter().copied())
            }
        }
    }

    pub fn successors_mut(&mut self) -> SuccessorsMut<'_> {
        use Terminator::*;

        let tail_slice = [].iter_mut();
        match self {
            Call { target: Some(t), .. } => Some(t).into_iter().chain(tail_slice),
            Goto { target: t } => Some(t).into_iter().chain(tail_slice),
            Return | Unreachable | Call { target: None, .. } => None.into_iter().chain(tail_slice),
            SwitchInt { targets, .. } => None.into_iter().chain(targets.targets.iter_mut()),
        }
    }

    pub fn as_goto(&self) -> Option<BasicBlock> {
        match self {
            Terminator::Goto { target } => Some(*target),
            _ => None,
        }
    }

    pub fn as_switch(&self) -> Option<(&Operand<'tcx>, &SwitchTargets)> {
        match self {
            Terminator::SwitchInt { discr, targets } => Some((discr, targets)),
            _ => None,
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
    Deref,
}

impl<'tcx> PlaceElem<'tcx> {
    pub fn is_indirect(&self) -> bool {
        match self {
            PlaceElem::Subtype(_) => false,
            PlaceElem::Deref => true,
        }
    }
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

    pub fn as_ref(&self) -> PlaceRef<'tcx> {
        PlaceRef { local: self.local, projection: self.projection }
    }

    pub fn ty(&self, local_decls: &LocalDecls<'tcx>, _tcx: Tx<'tcx>) -> Ty<'tcx> {
        assert!(self.projection.is_empty());

        local_decls[self.local].ty
        // projection
        //     .iter()
        //     .fold(local_decls[local].ty, |place_ty, &elem| place_ty.projection_ty(tcx, elem))
    }

    pub fn is_indirect(&self) -> bool {
        self.projection.iter().any(|elem| elem.is_indirect())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct PlaceRef<'tcx> {
    pub local: Local,
    pub projection: &'tcx [PlaceElem<'tcx>],
}

impl<'tcx> PlaceRef<'tcx> {
    #[inline]
    pub fn iter_projections(
        self,
    ) -> impl Iterator<Item = (PlaceRef<'tcx>, PlaceElem<'tcx>)> + DoubleEndedIterator {
        self.projection.iter().enumerate().map(move |(i, proj)| {
            let base = PlaceRef { local: self.local, projection: &self.projection[..i] };
            (base, *proj)
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operand<'tcx> {
    Copy(Place<'tcx>),
    Const(ConstValue, Ty<'tcx>),
}

impl<'tcx> Operand<'tcx> {
    pub fn place(&self) -> Option<Place<'tcx>> {
        match self {
            Operand::Copy(place) => Some(*place),
            Operand::Const(_, _) => None,
        }
    }

    pub fn constant(&self) -> Option<(&ConstValue, &Ty<'tcx>)> {
        match self {
            Operand::Const(x, ty) => Some((x, ty)),
            Operand::Copy(_) => None,
        }
    }
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

    #[inline]
    pub fn is_null(self) -> bool {
        self.data == 0
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

impl TryFrom<ScalarRepr> for bool {
    type Error = Size;
    #[inline]
    fn try_from(int: ScalarRepr) -> Result<Self, Size> {
        int.to_bits(Size::from_bytes(1)).and_then(|u| match u {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(Size::from_bytes(1)),
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ConstValue {
    Scalar(ScalarRepr),
    Zst,
}

impl ConstValue {
    #[inline]
    pub fn try_to_scalar(&self) -> Option<ScalarRepr> {
        match *self {
            ConstValue::Zst => None,
            ConstValue::Scalar(val) => Some(val),
        }
    }

    pub fn try_to_bool(&self) -> Option<bool> {
        self.try_to_scalar()?.try_into().ok()
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum UnOp {
    Not,
    Neg,
}

impl UnOp {
    pub fn from_parse(op: lexer::UnOp) -> Self {
        use lexer::UnOp;

        match op {
            UnOp::Not(_) => Self::Not,
            UnOp::Neg(_) => Self::Neg,
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

impl<'tcx> Rvalue<'tcx> {
    #[inline]
    pub fn is_safe_to_remove(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone)]
pub enum Statement<'tcx> {
    Assign(Place<'tcx>, Rvalue<'tcx>),
    Nop,
}

impl<'tcx> Statement<'tcx> {
    pub fn as_assign(&self) -> Option<(&Place<'tcx>, &Rvalue<'tcx>)> {
        match self {
            Statement::Assign(place, rvalue) => Some((place, rvalue)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct BasicBlockData<'tcx> {
    pub statements: Vec<Statement<'tcx>>,
    pub terminator: Option<Terminator<'tcx>>,
}

impl<'tcx> BasicBlockData<'tcx> {
    #[track_caller]
    pub fn terminator(&self) -> &Terminator<'tcx> {
        self.terminator.as_ref().expect("invalid hir analyzing")
    }

    #[track_caller]
    pub fn terminator_mut(&mut self) -> &mut Terminator<'tcx> {
        self.terminator.as_mut().expect("invalid hir analyzing")
    }

    pub fn is_empty_unreachable(&self) -> bool {
        self.statements.is_empty() && matches!(self.terminator(), Terminator::Unreachable)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Copy)]
pub enum Mutability {
    Not, // `Not` less than `Mut`
    Mut,
}

impl Mutability {
    pub fn from_bool(is_mut: bool) -> Self {
        if is_mut { Mutability::Mut } else { Mutability::Not }
    }

    pub fn is_mut(&self) -> bool {
        matches!(self, Self::Mut)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct LocalDecl<'tcx> {
    pub mutability: Mutability,
    pub ty: Ty<'tcx>,
}

pub type LocalDecls<'tcx> = IndexVec<Local, LocalDecl<'tcx>>;

#[derive(Debug, Clone, Default)]
pub struct Body<'tcx> {
    pub pass_count: usize,
    pub argc: usize,
    pub local_decls: LocalDecls<'tcx>,
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

    #[inline]
    pub fn return_ty(&self) -> Ty<'tcx> {
        self.local_decls[RETURN_PLACE].ty
    }
}
