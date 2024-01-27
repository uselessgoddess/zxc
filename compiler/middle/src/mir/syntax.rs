use {
    crate::{
        abi::Size,
        idx::{self, IndexVec},
        mir::{ty::List, Location, Ty, RETURN_PLACE},
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
pub struct Terminator<'tcx> {
    pub source_info: SourceInfo,
    pub kind: TerminatorKind<'tcx>,
}

impl<'tcx> Terminator<'tcx> {
    pub fn successors(&self) -> Successors<'_> {
        self.kind.successors()
    }

    pub fn successors_mut(&mut self) -> SuccessorsMut<'_> {
        self.kind.successors_mut()
    }
}

#[derive(Debug, Clone)]
pub enum TerminatorKind<'tcx> {
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

impl<'tcx> TerminatorKind<'tcx> {
    pub fn successors(&self) -> Successors<'_> {
        use TerminatorKind::*;

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
        use TerminatorKind::*;

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
            TerminatorKind::Goto { target } => Some(*target),
            _ => None,
        }
    }

    pub fn as_switch(&self) -> Option<(&Operand<'tcx>, &SwitchTargets)> {
        match self {
            TerminatorKind::SwitchInt { discr, targets } => Some((discr, targets)),
            _ => None,
        }
    }

    pub fn fmt_successor_labels(&self) -> Vec<Cow<'static, str>> {
        use TerminatorKind::*;

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
    pub fn return_place() -> Place<'tcx> {
        Self::pure(RETURN_PLACE)
    }

    pub fn pure(local: Local) -> Self {
        Self { local, projection: List::empty() }
    }

    pub fn as_ref(&self) -> PlaceRef<'tcx> {
        PlaceRef { local: self.local, projection: self.projection }
    }

    pub fn as_local(&self) -> Option<Local> {
        self.as_ref().as_local()
    }

    pub fn is_indirect(&self) -> bool {
        self.projection.iter().any(|elem| elem.is_indirect())
    }

    pub fn as_deref(&self) -> Option<Local> {
        self.as_ref().as_deref()
    }

    pub fn local_or_deref(&self) -> Option<Local> {
        self.as_ref().local_or_deref()
    }

    pub fn project_deeper(self, tcx: Tx<'tcx>, projections: &[PlaceElem<'tcx>]) -> Place<'tcx> {
        self.as_ref().project_deeper(tcx, projections)
    }

    pub fn ty(&self, tcx: Tx<'tcx>, local_decls: &LocalDecls<'tcx>) -> Ty<'tcx> {
        self.as_ref().ty(tcx, local_decls)
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

    pub fn as_local(&self) -> Option<Local> {
        match *self {
            PlaceRef { local, projection: [] } => Some(local),
            _ => None,
        }
    }

    pub fn as_deref(&self) -> Option<Local> {
        match *self {
            PlaceRef { local, projection: [PlaceElem::Deref] } => Some(local),
            _ => None,
        }
    }

    pub fn local_or_deref(&self) -> Option<Local> {
        match *self {
            PlaceRef { local, projection: [] }
            | PlaceRef { local, projection: [PlaceElem::Deref] } => Some(local),
            _ => None,
        }
    }

    pub fn project_deeper(self, tcx: Tx<'tcx>, projections: &[PlaceElem<'tcx>]) -> Place<'tcx> {
        let mut v: Vec<_>;

        let projections = if self.projection.is_empty() {
            projections
        } else {
            v = Vec::with_capacity(self.projection.len() + projections.len());
            v.extend(self.projection);
            v.extend(projections);
            &v[..]
        };

        Place { local: self.local, projection: tcx.mk_place_elems(projections) }
    }

    pub fn ty(&self, tcx: Tx<'tcx>, local_decls: &LocalDecls<'tcx>) -> Ty<'tcx> {
        self.projection
            .iter()
            .fold(local_decls[self.local].ty, |ty, &elem| Self::projection_ty(tcx, ty, elem))
    }

    fn projection_ty(_tcx: Tx<'tcx>, ty: Ty<'tcx>, elem: PlaceElem<'tcx>) -> Ty<'tcx> {
        match elem {
            PlaceElem::Deref => {
                ty.builtin_deref(true)
                    .unwrap_or_else(|| {
                        panic!("deref projection of non-dereferenceable ty {:?}", ty)
                    })
                    .ty
            }
            PlaceElem::Subtype(ty) => ty,
        }
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
    #[cfg_attr(debug_assertions, track_caller)]
    pub fn assert_bits(self, target_size: Size) -> u128 {
        self.to_bits(target_size)
            .unwrap_or_else(|_| panic!("assertion failed: {self:?} fits {target_size:?}"))
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

    #[inline]
    pub fn try_from_uint(data: u128, size: Size) -> Option<Self> {
        if size.truncate(data) == data {
            Some(Self { data, size: NonZeroU8::new(size.bytes() as u8).unwrap() })
        } else {
            None
        }
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

from!(bool u8 u16 u32 u64);

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

    #[deprecated(note = "move into hir::BinOp")]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Eq => "==",
            Self::Lt => "<",
            Self::Le => "<=",
            Self::Ne => "!=",
            Self::Ge => ">=",
            Self::Gt => ">",
            _ => unreachable!(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Debug, Copy, Clone)]
pub enum Rvalue<'tcx> {
    Use(Operand<'tcx>),
    UseDeref(Place<'tcx>),
    Ref(Mutability, Place<'tcx>),
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

#[derive(Debug, Copy, Clone)]
pub struct SourceInfo {
    /// The source span for the AST pertaining to this MIR.
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Statement<'tcx> {
    pub source_info: SourceInfo,
    pub kind: StatementKind<'tcx>,
}

impl<'tcx> Statement<'tcx> {
    pub fn make_nop(&mut self) {
        self.kind = StatementKind::Nop;
    }
}

#[derive(Debug, Clone)]
pub enum StatementKind<'tcx> {
    Assign(Place<'tcx>, Rvalue<'tcx>),
    Nop,
}

impl<'tcx> StatementKind<'tcx> {
    pub fn as_assign(&self) -> Option<(&Place<'tcx>, &Rvalue<'tcx>)> {
        match self {
            StatementKind::Assign(place, rvalue) => Some((place, rvalue)),
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
        self.statements.is_empty() && matches!(self.terminator().kind, TerminatorKind::Unreachable)
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

    pub fn prefix_str(&self) -> &'static str {
        match self {
            Mutability::Not => "",
            Mutability::Mut => "mut ",
        }
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
    pub basic_blocks: IndexVec<BasicBlock, BasicBlockData<'tcx>>,
    pub local_decls: LocalDecls<'tcx>,
    pub argc: usize,
    pub pass_count: usize,
}

impl<'tcx> Body<'tcx> {
    pub fn source_info(&self, Location { block, statement_index: idx }: Location) -> &SourceInfo {
        let block = &self.basic_blocks[block];
        let stmts = &block.statements;
        if idx < stmts.len() {
            &stmts[idx].source_info
        } else {
            assert_eq!(idx, stmts.len());
            &block.terminator().source_info
        }
    }

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

#[derive(Clone, Hash, PartialEq, Debug)]
pub enum AssertKind<O> {
    Overflow(BinOp, O, O),
}
