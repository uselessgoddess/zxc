use {
    super::FunctionCx,
    cranelift::{
        codegen::ir::{immediates::Offset32, StackSlot},
        prelude::{InstBuilder, MemFlags, Type, Value, Variable},
    },
    middle::{
        abi::{Abi, Align, Size, TyAbi},
        mir::{ty, Local, ScalarRepr},
    },
};

#[derive(Debug, Copy, Clone)] // FIXME: want be `Clone`
pub(crate) struct CValue<'tcx> {
    inner: CValueInner,
    layout: TyAbi<'tcx>,
}

#[derive(Debug, Copy, Clone)]
enum CValueInner {
    ByRef(Pointer),
    ByVal(Value),
}

impl<'tcx> CValue<'tcx> {
    pub fn layout(&self) -> TyAbi<'tcx> {
        self.layout
    }

    pub fn by_val(value: Value, layout: TyAbi<'tcx>) -> CValue<'tcx> {
        CValue { inner: CValueInner::ByVal(value), layout }
    }

    pub fn by_ref(ptr: Pointer, layout: TyAbi<'tcx>) -> CValue<'tcx> {
        CValue { inner: CValueInner::ByRef(ptr), layout }
    }

    pub(crate) fn try_to_ptr(self) -> Option<Pointer> {
        match self.inner {
            CValueInner::ByRef(ptr) => Some(ptr),
            CValueInner::ByVal(_) => None,
        }
    }

    pub fn load_scalar(self, fx: &mut FunctionCx<'_, '_, 'tcx>) -> Value {
        let layout = self.layout();
        match self.inner {
            CValueInner::ByRef(ptr) => {
                let clif_ty = match layout.layout.abi {
                    Abi::Scalar(scalar) => super::scalar_to_clif(fx.tcx, scalar),
                    _ => unreachable!("{:?}", layout.ty),
                };
                ptr.load(fx, clif_ty, MemFlags::new().with_notrap())
            }
            CValueInner::ByVal(value) => value,
        }
    }

    pub(crate) fn const_val(
        fx: &mut FunctionCx<'_, '_, 'tcx>,
        abi: TyAbi<'tcx>,
        const_val: ScalarRepr,
    ) -> CValue<'tcx> {
        assert_eq!(const_val.size(), abi.layout.size, "{:#?}: {:?}", const_val, abi);

        let clif_ty = fx.clif_type(abi.ty).unwrap();

        // if let ty::Bool = abi.ty.kind() {
        //     assert!(
        //         const_val == ty::ScalarInt::FALSE || const_val == ty::ScalarInt::TRUE,
        //         "Invalid bool 0x{:032X}",
        //         const_val
        //     );
        // }

        let val = match abi.ty.kind() {
            ty::Bool | ty::Int(_) => {
                let raw_val =
                    const_val.size().truncate(const_val.to_bits(abi.layout.size).unwrap());
                fx.bcx.ins().iconst(clif_ty, raw_val as i64)
            }
            _ => panic!(
                "CValue::const_val for non bool/char/float/integer/pointer \
                type {:?} is not allowed",
                abi.ty
            ),
        };

        // let val = match layout.ty.kind() {
        //     ty::Uint(UintTy::U128) | ty::Int(IntTy::I128) => {
        //         let const_val = const_val.to_bits(layout.size).unwrap();
        //         let lsb = fx.bcx.ins().iconst(types::I64, const_val as u64 as i64);
        //         let msb = fx.bcx.ins().iconst(types::I64, (const_val >> 64) as u64 as i64);
        //         fx.bcx.ins().iconcat(lsb, msb)
        //     }
        //     ty::Bool
        //     | ty::Char
        //     | ty::Uint(_)
        //     | ty::Int(_)
        //     | ty::Ref(..)
        //     | ty::RawPtr(..)
        //     | ty::FnPtr(..) => {
        //         let raw_val = const_val.size().truncate(const_val.to_bits(layout.size).unwrap());
        //         fx.bcx.ins().iconst(clif_ty, raw_val as i64)
        //     }
        //     ty::Float(FloatTy::F32) => {
        //         fx.bcx.ins().f32const(Ieee32::with_bits(u32::try_from(const_val).unwrap()))
        //     }
        //     ty::Float(FloatTy::F64) => {
        //         fx.bcx.ins().f64const(Ieee64::with_bits(u64::try_from(const_val).unwrap()))
        //     }
        //     _ => panic!(
        //         "CValue::const_val for non bool/char/float/integer/pointer
        // type {:?} is not allowed",
        //         layout.ty
        //     ),
        // };

        CValue::by_val(val, abi)
    }

    pub(crate) fn force_stack(self, fx: &mut FunctionCx<'_, '_, 'tcx>) -> Pointer {
        let layout = self.layout();
        match self.inner {
            CValueInner::ByRef(ptr) => ptr,
            CValueInner::ByVal(_) => {
                let place = CPlace::new_stack_slot(fx, layout);
                place.write_cvalue(fx, self);
                place.to_ptr()
            }
        }
    }
}

#[derive(Debug, Copy, Clone)] // FIXME: want be `Clone`
pub(crate) struct CPlace<'tcx> {
    inner: CPlaceInner,
    layout: TyAbi<'tcx>,
}

#[derive(Debug, Copy, Clone)]
enum CPlaceInner {
    Var(Local, Variable),
    Addr(Pointer),
}

impl<'tcx> CPlace<'tcx> {
    pub fn layout(&self) -> TyAbi<'tcx> {
        self.layout
    }

    pub(crate) fn for_ptr(ptr: Pointer, layout: TyAbi<'tcx>) -> CPlace<'tcx> {
        CPlace { inner: CPlaceInner::Addr(ptr), layout }
    }

    pub(crate) fn new_stack_slot(
        fx: &mut FunctionCx<'_, '_, 'tcx>,
        abi @ TyAbi { layout, .. }: TyAbi<'tcx>,
    ) -> CPlace<'tcx> {
        if layout.size.bytes() == 0 {
            return CPlace {
                inner: CPlaceInner::Addr(Pointer::dangling(
                    layout.align, /* FIXME: possible to add `prefer` align */
                )),
                layout: abi,
            };
        }

        if layout.size.bytes() >= u64::from(u32::MAX - 16) {
            panic!("too big for stack");
        }

        let stack_slot = fx.create_stack_slot(
            u32::try_from(layout.size.bytes()).unwrap(),
            /* FIXME: possible to add `prefer` align */
            u32::try_from(layout.align.bytes()).unwrap(),
        );
        CPlace { inner: CPlaceInner::Addr(stack_slot), layout: abi }
    }

    pub fn new_var(
        fx: &mut FunctionCx<'_, '_, 'tcx>,
        local: Local,
        layout: TyAbi<'tcx>,
    ) -> CPlace<'tcx> {
        let var = Variable::from_u32(fx.next_ssa());
        fx.bcx.declare_var(var, fx.clif_type(layout.ty).expect("LMAO"));
        CPlace { inner: CPlaceInner::Var(local, var), layout }
    }

    pub(crate) fn to_ptr(self) -> Pointer {
        match self.inner {
            CPlaceInner::Addr(ptr) => ptr,
            CPlaceInner::Var(_, _) => {
                panic!("Expected CPlace::Addr, found {:?}", self)
            }
        }
    }

    pub(crate) fn to_cvalue(self, fx: &mut FunctionCx<'_, '_, 'tcx>) -> CValue<'tcx> {
        let layout = self.layout();
        match self.inner {
            CPlaceInner::Var(_local, var) => {
                let val = fx.bcx.use_var(var);
                CValue::by_val(val, layout)
            }

            CPlaceInner::Addr(ptr) => CValue::by_ref(ptr, layout),
        }
    }

    pub(crate) fn write_cvalue(self, fx: &mut FunctionCx<'_, '_, 'tcx>, from: CValue<'tcx>) {
        fn transmute_scalar(
            fx: &mut FunctionCx<'_, '_, '_>,
            var: Variable,
            data: Value,
            dst_ty: Type,
        ) {
            let src_ty = fx.bcx.func.dfg.value_type(data);
            assert_eq!(
                src_ty.bytes(),
                dst_ty.bytes(),
                "write_cvalue_transmute: {:?} -> {:?}",
                src_ty,
                dst_ty,
            );
            let data = match (src_ty, dst_ty) {
                (_, _) if src_ty == dst_ty => data,

                _ => unreachable!("write_cvalue_transmute: {:?} -> {:?}", src_ty, dst_ty),
            };
            //fx.bcx.set_val_label(data, cranelift_codegen::ir::ValueLabel::new(var.index()));
            fx.bcx.def_var(var, data);
        }

        assert_eq!(self.layout().layout.size, from.layout().layout.size);

        let dst_layout = self.layout();
        match self.inner {
            CPlaceInner::Var(_local, var) => {
                let data = match from.layout().layout.abi {
                    Abi::Scalar(_) => {
                        CValue { inner: from.inner, layout: dst_layout }.load_scalar(fx)
                    }
                    _ => {
                        let ptr = from.force_stack(fx);
                        CValue { inner: CValueInner::ByRef(ptr), layout: dst_layout }
                            .load_scalar(fx)
                    }
                };
                let dst_ty = fx.clif_type(self.layout().ty).unwrap();
                transmute_scalar(fx, var, data, dst_ty);
            }
            CPlaceInner::Addr(to_ptr) => {
                if dst_layout.layout.size == Size::ZERO {
                    return;
                }

                let mut flags = MemFlags::new();
                flags.set_notrap();

                match from.inner {
                    CValueInner::ByVal(val) => {
                        to_ptr.store(fx, val, flags);
                    }
                    CValueInner::ByRef(from_ptr) => {
                        if let Abi::Scalar(_) = from.layout().layout.abi {
                            let val = from.load_scalar(fx);
                            to_ptr.store(fx, val, flags);
                            return;
                        }

                        let from_addr = from_ptr.get_addr(fx);
                        let to_addr = to_ptr.get_addr(fx);
                        let src_layout = from.layout();
                        let size = dst_layout.layout.size.bytes();
                        let src_align = src_layout.layout.align.bytes() as u8;
                        let dst_align = dst_layout.layout.align.bytes() as u8;
                        fx.bcx.emit_small_memory_copy(
                            fx.target_config,
                            to_addr,
                            from_addr,
                            size,
                            dst_align,
                            src_align,
                            true,
                            flags,
                        );
                    }
                }
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct Pointer {
    base: PointerBase,
    offset: Offset32,
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum PointerBase {
    Addr(Value),
    Stack(StackSlot),
    Dangling(Align),
}

impl Pointer {
    pub fn new(addr: Value) -> Self {
        Pointer { base: PointerBase::Addr(addr), offset: Offset32::new(0) }
    }

    pub fn stack_slot(stack_slot: StackSlot) -> Self {
        Pointer { base: PointerBase::Stack(stack_slot), offset: Offset32::new(0) }
    }

    pub(crate) fn dangling(align: Align) -> Self {
        Pointer { base: PointerBase::Dangling(align), offset: Offset32::new(0) }
    }

    pub(crate) fn get_addr(self, fx: &mut FunctionCx<'_, '_, '_>) -> Value {
        match self.base {
            PointerBase::Addr(base_addr) => {
                let offset: i64 = self.offset.into();
                if offset == 0 { base_addr } else { fx.bcx.ins().iadd_imm(base_addr, offset) }
            }
            PointerBase::Stack(stack_slot) => {
                fx.bcx.ins().stack_addr(fx.ptr_type, stack_slot, self.offset)
            }
            PointerBase::Dangling(align) => {
                fx.bcx.ins().iconst(fx.ptr_type, i64::try_from(align.bytes()).unwrap())
            }
        }
    }

    pub fn load(self, fx: &mut FunctionCx<'_, '_, '_>, ty: Type, flags: MemFlags) -> Value {
        match self.base {
            PointerBase::Addr(base_addr) => fx.bcx.ins().load(ty, flags, base_addr, self.offset),
            PointerBase::Stack(stack_slot) => fx.bcx.ins().stack_load(ty, stack_slot, self.offset),
            PointerBase::Dangling(_) => unreachable!(),
        }
    }

    pub fn store(self, fx: &mut FunctionCx<'_, '_, '_>, value: Value, flags: MemFlags) {
        match self.base {
            PointerBase::Addr(base_addr) => {
                fx.bcx.ins().store(flags, value, base_addr, self.offset);
            }
            PointerBase::Stack(stack_slot) => {
                fx.bcx.ins().stack_store(value, stack_slot, self.offset);
            }
            PointerBase::Dangling(_) => unreachable!(),
        }
    }
}
