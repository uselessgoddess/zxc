use {
    super::{base, base::FunctionCx},
    crate::codegen::{llvm::Bx, ssa::MemFlags},
    llvm::Value,
    middle::{
        abi,
        abi::{Abi, Align, Primitive, TyAbi},
        mir::{ConstValue, Ty},
    },
    std::fmt,
};

#[derive(Copy, Clone)]
pub struct LValue<'ll, 'tcx> {
    pub(crate) repr: LValueRepr<'ll>,
    pub(crate) layout: TyAbi<'tcx>,
}

#[derive(Debug, Copy, Clone)]
pub enum LValueRepr<'ll> {
    ByRef(&'ll Value, Align),
    ByVal(&'ll Value),
    Zst,
}

impl<'ll> LValueRepr<'ll> {
    pub fn store<'tcx>(self, bx: &mut Bx<'_, 'll, 'tcx>, dest: LPlace<'ll, 'tcx>) {
        self.store_with_flags(bx, dest, MemFlags::empty());
    }

    pub fn store_with_flags<'tcx>(
        self,
        bcx: &mut Bx<'_, 'll, 'tcx>,
        dest: LPlace<'ll, 'tcx>,
        flags: MemFlags,
    ) {
        match self {
            LValueRepr::Zst => {}
            LValueRepr::ByRef(r, align) => {
                base::memcpy_ty(bcx, dest.llval, dest.align, r, align, dest.layout, flags)
            }
            LValueRepr::ByVal(scalar) => {
                let val = bcx.from_immediate(scalar);
                bcx.store_with_flags(val, dest.llval, dest.align, flags);
            }
        }
    }
}

impl<'ll, 'tcx> LValue<'ll, 'tcx> {
    pub fn zst(layout: TyAbi<'tcx>) -> Self {
        assert!(layout.is_zst());
        Self { repr: LValueRepr::Zst, layout }
    }

    pub fn by_val(llval: &'ll Value, layout: TyAbi<'tcx>) -> Self {
        Self { repr: LValueRepr::ByVal(llval), layout }
    }

    pub fn load_scalar(self) -> &'ll Value {
        match self.repr {
            LValueRepr::ByVal(value) => value,
            _ => panic!("not scalar: {self:?}"),
        }
    }

    pub fn from_const(
        fx: &mut FunctionCx<'_, 'll, 'tcx>,
        const_: ConstValue,
        ty: Ty<'tcx>,
    ) -> LValue<'ll, 'tcx> {
        let abi = fx.hix.tcx.layout_of(ty);

        match const_ {
            ConstValue::Zst => LValue::zst(abi),
            ConstValue::Scalar(int) => {
                let Abi::Scalar(scalar) = abi.abi else {
                    panic!("from_const: invalid ByVal layout: {abi:#?}");
                };
                let int = fx.cx.scalar_to_backend(int, scalar, fx.cx.immediate_type_of(abi.layout));
                LValue { repr: LValueRepr::ByVal(int), layout: abi }
            }
        }
    }

    pub fn deref(self, fx: &mut FunctionCx<'_, 'll, 'tcx>) -> LPlace<'ll, 'tcx> {
        let layout = fx.cx.tcx.layout_of(
            self.layout
                .ty
                .builtin_deref(true)
                .unwrap_or_else(|| panic!("deref of non-pointer {self:?}"))
                .ty,
        );
        let (llptr) = match self.repr {
            LValueRepr::ByVal(llptr) => llptr,
            LValueRepr::ByRef(..) => panic!("Deref of by-ref operand {self:?}"),
            LValueRepr::Zst => panic!("Deref of ZST operand {self:?}"),
        };
        LPlace { llval: llptr, layout, align: layout.align }
    }
}

impl fmt::Debug for LValue<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "OperandRef({:?} @ {:?})", self.repr, self.layout)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct LPlace<'ll, 'tcx> {
    /// A pointer to the place.
    pub llval: &'ll Value,
    pub layout: TyAbi<'tcx>,
    pub align: Align,
}

fn is_llvm_immediate(layout: abi::Layout<'_>) -> bool {
    match layout.abi {
        Abi::Scalar(_) => true,
        Abi::Uninhabited | Abi::Aggregate => false,
    }
}

impl<'ll, 'tcx> LPlace<'ll, 'tcx> {
    pub fn new_sized(llval: &'ll Value, layout: TyAbi<'tcx>) -> Self {
        assert!(layout.is_sized());
        Self { llval, layout, align: layout.align } // TODO: use `.abi` align
    }

    pub fn new_sized_aligned(llval: &'ll Value, layout: TyAbi<'tcx>, align: Align) -> Self {
        assert!(layout.is_sized());
        Self { llval, layout, align }
    }

    pub fn alloca(fx: &mut FunctionCx<'_, 'll, 'tcx>, layout: TyAbi<'tcx>) -> Self {
        Self::alloca_aligned(fx, layout, layout.align)
    }

    pub fn store_lvalue(self, fx: &mut FunctionCx<'_, 'll, 'tcx>, from: LValue<'ll, 'tcx>) {
        from.repr.store(&mut fx.bcx, self);
    }

    pub fn alloca_aligned(
        fx: &mut FunctionCx<'_, 'll, 'tcx>,
        ty: TyAbi<'tcx>,
        align: Align,
    ) -> Self {
        assert!(ty.is_sized(), "tried to statically allocate unsized place");

        let tmp = fx.bcx.alloca(fx.cx.type_of(ty.layout), align);
        Self::new_sized_aligned(tmp, ty, align)
    }

    pub fn load_lvalue(self, fx: &mut FunctionCx<'_, 'll, 'tcx>) -> LValue<'ll, 'tcx> {
        let layout = self.layout;

        if layout.is_zst() {
            return LValue::zst(layout);
        }

        let (ty, lay) = (layout.ty, layout.layout);

        if is_llvm_immediate(lay) {
            let llty = fx.cx.type_of(lay);

            let llval = unsafe {
                if let Some(global) = llvm::LLVMIsAGlobalVariable(self.llval)
                    && llvm::LLVMIsGlobalConstant(global) == llvm::True
                    && let Some(init) = llvm::LLVMGetInitializer(global)
                    && init.type_of() == llty
                {
                    init
                } else {
                    let load = fx.bcx.load(llty, self.llval, self.align);
                    if let Abi::Scalar(scalar) = self.layout.abi {
                        scalar_assumes(&mut fx.bcx, load, scalar, self.layout);
                    }
                    load
                }
            };

            LValue { repr: LValueRepr::ByVal(fx.bcx.to_immediate(llval, lay)), layout }
        } else {
            LValue { repr: LValueRepr::ByRef(self.llval, self.align), layout }
        }
    }
}

pub fn scalar_assumes<'ll, 'tcx>(
    bcx: &mut Bx<'_, 'll, 'tcx>,
    load: &'ll Value,
    scalar: abi::Scalar,
    layout: TyAbi<'tcx>,
) {
    // if is non-uninit
    bcx.assume_noundef(load);

    let dl = &bcx.cx.tcx.sess.target.data_layout;
    match scalar.primitive() {
        int @ Primitive::Int(..) => {
            // TODO: add uninitialized assumes in the future
            if let abi::Scalar::Initialized { valid, .. } = scalar
                && !valid.is_full_for(int.size(dl))
            {
                bcx.assume_range(load, valid)
            }
        }
        Primitive::Pointer => {
            if let abi::Scalar::Initialized { valid, .. } = scalar
                && !valid.contains(0)
            {
                bcx.assume_nonnull(load);
            }

            if let Some(pointee) = layout.pointee_info(bcx.cx.tcx)
                && pointee.safe.is_some()
            {
                bcx.assume_align(load, pointee.align);
            }
        }
        Primitive::F32 | Primitive::F64 => {}
    }
}
