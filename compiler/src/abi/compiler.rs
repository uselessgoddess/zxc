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

        pub const ZST_LAYOUT: LayoutKind = LayoutKind {
            abi: Abi::Aggregate,
            size: Size::ZERO,
            align: if let Ok(align) = Align::from_bytes(1) { align } else { todo!() },
            shape: FieldsShape::Primitive,
        };

        // Safety: compiler intrinsics
        let layout = self.intern.intern_layout(
            self.arena,
            match ty.kind() {
                TyKind::Bool => scalar(Integer::I8, false, (1, 1)),
                TyKind::Int(int) => match int {
                    IntTy::I8 => scalar(Integer::I8, true, (1, 1)),
                    IntTy::I16 => scalar(Integer::I16, true, (2, 2)),
                    IntTy::I32 => scalar(Integer::I32, true, (4, 4)),
                    IntTy::I64 => scalar(Integer::I64, true, (8, 8)),
                    IntTy::Isize => scalar(Integer::I64, true, (8, 8)),
                    _ => todo!(),
                },
                TyKind::Tuple(list) => {
                    if list.is_empty() {
                        ZST_LAYOUT
                    } else {
                        todo!()
                    }
                }
                TyKind::FnDef(_) => ZST_LAYOUT,
            },
        );
        TyAbi { ty, layout }
    }

    pub fn fn_abi_of_sig(&self, sig: mir::FnSig<'tcx>) -> FnAbi<'tcx> {
        let probe_abi = |_tcx, ty| ArgAbi {
            ty: self.layout_of(ty),
            mode: match ty.kind() {
                TyKind::Bool | TyKind::Int(_) => PassMode::Direct,
                TyKind::Tuple(types) => {
                    if types.is_empty() {
                        PassMode::Ignore
                    } else {
                        PassMode::Direct
                    }
                }
                TyKind::FnDef(_) => PassMode::Ignore,
            },
        };

        // FIXME: now `sig.` doesn't affect on abi inferring
        FnAbi {
            args: sig.inputs().iter().map(|&ty| probe_abi(self, ty)).collect(),
            ret: probe_abi(self, sig.output()),
            conv: match sig.abi {
                mir::ty::Abi::Zxc => Conv::Zxc,
                mir::ty::Abi::C => Conv::C,
            },
        }
    }
}
