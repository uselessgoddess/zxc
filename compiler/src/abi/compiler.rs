use crate::{
    abi::*,
    mir::{self, IntTy, Ty, TyKind},
    tcx::TyCtx,
};

impl<'tcx> TyCtx<'tcx> {
    pub fn layout_of(&self, ty: Ty<'tcx>) -> TyAbi<'tcx> {
        // Safety: compiler intrinsics
        let scalar = |ty, sign, (size, align)| LayoutKind {
            abi: Abi::Scalar(Scalar::Int(ty, sign)),
            size: Size::from_bytes(size),
            align: Align::from_bytes(align).expect("compiler query"),
            shape: FieldsShape::Primitive,
        };

        // Safety: compiler intrinsics
        let layout = self.intern.intern_layout(
            self.arena,
            match ty.kind() {
                TyKind::Int(int) => match int {
                    IntTy::I8 => scalar(Integer::I8, true, (1, 1)),
                    IntTy::I16 => scalar(Integer::I16, true, (2, 2)),
                    IntTy::I32 => scalar(Integer::I32, true, (4, 4)),
                    IntTy::I64 => scalar(Integer::I64, true, (8, 8)),
                    _ => todo!(),
                },
                TyKind::Tuple(list) => {
                    if list.is_empty() {
                        LayoutKind {
                            abi: Abi::Aggregate,
                            size: Size::ZERO,
                            align: Align::from_bytes(1).expect("compiler query"),
                            shape: FieldsShape::Primitive,
                        }
                    } else {
                        todo!()
                    }
                }
            },
        );
        TyAbi { ty, layout }
    }

    pub fn fn_abi_of_sig(&self, sig: mir::FnSig<'tcx>) -> FnAbi<'tcx> {
        let probe_abi = |tcx, ty| ArgAbi {
            ty: self.layout_of(ty),
            mode: match ty.kind() {
                TyKind::Int(_) => PassMode::Direct,
                TyKind::Tuple(types) => {
                    if types.is_empty() {
                        PassMode::Ignore
                    } else {
                        PassMode::Direct
                    }
                }
            },
        };

        // FIXME: now `sig.` doesn't affect on abi inferring
        FnAbi {
            args: sig.inputs().iter().map(|&ty| probe_abi(self, ty)).collect(),
            ret: probe_abi(self, sig.output()),
        }
    }
}
