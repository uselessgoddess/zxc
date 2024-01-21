mod compiler;

use {
    crate::{mir, tcx::Interned},
    std::ops::Deref,
};

pub use abi::*;

pub type Layout<'cx> = Interned<'cx, LayoutKind>;

#[derive(Debug, Copy, Clone)]
pub struct ArgAbi<'tcx> {
    pub ty: TyAbi<'tcx>,
    pub mode: PassMode,
}

#[derive(Debug, Clone)]
pub struct FnAbi<'tcx> {
    pub args: Box<[ArgAbi<'tcx>]>,
    pub ret: ArgAbi<'tcx>,
    pub conv: Conv,
}

#[derive(Debug, Copy, Clone)]
pub struct TyAbi<'tcx> {
    pub ty: mir::Ty<'tcx>,
    pub layout: Layout<'tcx>,
}

impl<'tcx> Deref for TyAbi<'tcx> {
    type Target = Layout<'tcx>;

    fn deref(&self) -> &Self::Target {
        &self.layout
    }
}
