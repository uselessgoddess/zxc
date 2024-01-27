mod compiler;

use {
    crate::{mir, tcx::Interned, Tx},
    std::ops::Deref,
};

use crate::mir::{ty, Mutability, TyKind};
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct TyAbi<'tcx> {
    pub ty: mir::Ty<'tcx>,
    pub layout: Layout<'tcx>,
}

impl<'tcx> TyAbi<'tcx> {
    pub fn pointee_info(&self, tcx: Tx<'tcx>) -> Option<PointeeInfo> {
        match self.ty.kind() {
            ty::Ref(mt, ty) => {
                let kind = match mt {
                    Mutability::Not => PointerKind::Shared,
                    Mutability::Mut => PointerKind::Mutable,
                };
                let layout = tcx.layout_of(ty);
                Some(PointeeInfo { size: layout.size, align: layout.align, safe: Some(kind) })
            }
            _ => None,
        }
    }
}

impl<'tcx> Deref for TyAbi<'tcx> {
    type Target = Layout<'tcx>;

    fn deref(&self) -> &Self::Target {
        &self.layout
    }
}
