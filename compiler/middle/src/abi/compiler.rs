use crate::{
    abi::*,
    mir::{self, IntTy, Ty, TyKind},
    tcx::TyCtx,
};

use {Integer::*, Primitive::*};

pub fn ptr_sized(dl: &TargetDataLayout) -> Integer {
    match dl.pointer_size.bits() {
        16 => I16,
        32 => I32,
        64 => I64,
        bits => panic!("ptr_sized: unknown pointer bit size {bits}"),
    }
}

impl<'tcx> TyCtx<'tcx> {
    pub fn data_layout(&self) -> &TargetDataLayout {
        &self.sess.target.data_layout
    }

    #[cfg_attr(debug_assertions, track_caller)]
    pub fn layout_of(&self, ty: Ty<'tcx>) -> TyAbi<'tcx> {
        let dl = self.data_layout();

        let scalar = |value: Primitive| {
            LayoutKind::scalar(
                dl,
                Scalar::Initialized { value, valid: WrappingRange::full(value.size(dl)) },
            )
        };
        let zst_layout = LayoutKind {
            abi: Abi::Aggregate,
            size: Size::ZERO,
            align: dl.i8_align.abi,
            shape: FieldsShape::Primitive,
        };
        let never_layout = LayoutKind { abi: Abi::Uninhabited, ..zst_layout.clone() };

        let layout = self.intern.intern_layout(
            self.arena,
            match ty.kind() {
                TyKind::Bool => LayoutKind::scalar(
                    dl,
                    Scalar::Initialized {
                        value: Int(I8, false),
                        valid: WrappingRange { start: 0, end: 1 },
                    },
                ),
                TyKind::Int(int) => match int {
                    IntTy::I8 => scalar(Int(I8, true)),
                    IntTy::I16 => scalar(Int(I16, true)),
                    IntTy::I32 => scalar(Int(I32, true)),
                    IntTy::I64 => scalar(Int(I64, true)),
                    IntTy::Isize => scalar(Int(ptr_sized(dl), true)),
                },
                TyKind::Tuple(list) => {
                    if list.is_empty() {
                        zst_layout
                    } else {
                        todo!()
                    }
                }
                TyKind::Ref(_, _) => {
                    let mut ptr = scalar(Pointer);

                    // always if ref
                    if let Abi::Scalar(Scalar::Initialized { valid, .. }) = &mut ptr.abi {
                        valid.start = 1;
                    }

                    ptr
                }
                TyKind::FnDef(_) => zst_layout,
                TyKind::Never => never_layout,
                TyKind::Infer(_) => panic!("Tx::layout_of: unexpected type `{ty:?}`"),
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
                TyKind::Ref(_, _) => PassMode::Direct,
                TyKind::FnDef(_) => PassMode::Ignore,
                TyKind::Never => PassMode::Ignore,
                TyKind::Infer(_) => unreachable!(),
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
