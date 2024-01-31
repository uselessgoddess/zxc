pub mod consts;
pub mod errors;
pub mod interpret;
pub mod mono;
pub mod pass;
pub mod pretty;
mod syntax;
pub mod traversal;
pub mod ty;
pub mod visit;

pub mod lint {
    pub mod errors;
}

use {
    crate::{
        hir::{self, attr},
        idx, Symbol, Tx,
    },
    lexer::Span,
    std::{fmt, marker::PhantomData},
};
pub use {
    mono::{CodegenUnit, MonoItem, MonoItemData},
    pass::MirPass,
    pretty::{write_mir_body_pretty, write_mir_pretty},
    syntax::{
        AssertKind, BasicBlock, BasicBlockData, BinOp, Body, CastKind, ConstValue, Local,
        LocalDecl, LocalDecls, Mutability, Operand, Place, PlaceElem, PlaceRef, Rvalue, ScalarRepr,
        SourceInfo, Statement, StatementKind, SwitchTargets, Terminator, TerminatorKind, UnOp,
    },
    ty::{cast, FnSig, Infer, InferId, IntTy, Ty, TyKind, UintTy},
};

pub const START_BLOCK: BasicBlock = BasicBlock::START_BLOCK;
pub const RETURN_PLACE: Local = Local::RETURN_PLACE;

idx::define_index! {
    #[derive(Debug)]
    pub struct DefId = u32;
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub enum InstanceDef {
    Item(DefId),
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub struct Instance<'tcx> {
    pub def: InstanceDef,
    pub(crate) _marker: PhantomData<&'tcx ()>,
}

impl Instance<'_> {
    pub fn def(def: InstanceDef) -> Self {
        Self { def, _marker: PhantomData }
    }
}

#[derive(Debug, Clone)]
pub struct InstanceData<'tcx> {
    pub sig: FnSig<'tcx>,
    pub symbol: Symbol,

    // hir data
    pub span: Span,
    pub hsig: hir::FnSig<'tcx>,
    pub attrs: &'tcx [attr::MetaItem],
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct Location {
    /// The block that the location is within.
    pub block: BasicBlock,

    pub statement_index: usize,
}

impl Location {
    pub const START: Location = Location { block: START_BLOCK, statement_index: 0 };
}

impl fmt::Debug for Location {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{:?}[{}]", self.block, self.statement_index)
    }
}

pub struct SymbolName<'tcx> {
    pub name: &'tcx str,
}

impl<'tcx> SymbolName<'tcx> {
    pub fn new(tcx: Tx<'tcx>, name: &str) -> SymbolName<'tcx> {
        SymbolName {
            name: unsafe {
                std::str::from_utf8_unchecked(tcx.arena.dropless.alloc_slice(name.as_bytes()))
            },
        }
    }
}
