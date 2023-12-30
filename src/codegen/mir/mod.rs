pub mod codegen;
pub mod pretty;
mod syntax;
pub mod ty;

pub use {
    pretty::write_mir_body_pretty,
    syntax::{
        BasicBlock, BasicBlockData, Body, CastKind, ConstValue, Local, LocalDecl, Operand, Place,
        PlaceElem, Rvalue, ScalarRepr, Statement, Terminator,
    },
    ty::{cast, IntTy, Ty, TyKind, UintTy},
};
