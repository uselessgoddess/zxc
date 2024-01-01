pub mod pretty;
mod syntax;
pub mod ty;

use {crate::symbol::Symbol, lexer::Span, std::marker::PhantomData};
pub use {
    pretty::write_mir_body_pretty,
    syntax::{
        BasicBlock, BasicBlockData, Body, CastKind, ConstValue, Local, LocalDecl, Operand, Place,
        PlaceElem, Rvalue, ScalarRepr, Statement, Terminator,
    },
    ty::{cast, FnSig, IntTy, Ty, TyKind, UintTy},
};

index_vec::define_index_type! {
    pub struct DefId = u32;
}

#[derive(Debug, Copy, Clone)]
pub enum InstanceDef {
    Item(DefId),
}

#[derive(Debug, Copy, Clone)]
pub struct Instance<'tcx> {
    pub def: InstanceDef,
    _marker: PhantomData<&'tcx ()>,
}

#[derive(Debug, Clone)]
pub struct InstanceData<'tcx> {
    pub sig: FnSig<'tcx>,
    pub symbol: Symbol,

    // hir data
    pub span: Span,
}
