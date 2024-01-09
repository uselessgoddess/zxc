pub mod consts;
pub mod mono;
pub mod pass;
pub mod pretty;
mod syntax;
pub mod ty;

use {
    crate::{hir, Symbol},
    lexer::Span,
    std::marker::PhantomData,
};
pub use {
    mono::{CodegenUnit, MonoItem, MonoItemData},
    pass::MirPass,
    pretty::{write_mir_body_pretty, write_mir_pretty},
    syntax::{
        BasicBlock, BasicBlockData, BinOp, Body, CastKind, ConstValue, Local, LocalDecl,
        LocalDecls, Mutability, Operand, Place, PlaceElem, Rvalue, ScalarRepr, Statement,
        SwitchTargets, Terminator, UnOp,
    },
    ty::{cast, FnSig, IntTy, Ty, TyKind, UintTy},
};

pub const START_BLOCK: BasicBlock = BasicBlock::START_BLOCK;

index_vec::define_index_type! {
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

#[derive(Debug, Clone)]
pub struct InstanceData<'tcx> {
    pub sig: FnSig<'tcx>,
    pub symbol: Symbol,

    // hir data
    pub span: Span,
    pub hsig: hir::FnSig<'tcx>,
}
