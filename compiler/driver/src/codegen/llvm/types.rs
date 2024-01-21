use {
    super::CodegenCx,
    crate::codegen::ssa,
    llvm::{Type, Value},
    middle::{
        abi,
        abi::{
            Abi, Align, FieldsShape, Integer, Layout,
            Primitive::{self, Int},
            Scalar, Size,
        },
        mir::ScalarRepr,
    },
};

impl<'ll, 'tcx> CodegenCx<'ll, 'tcx> {
    pub fn const_null(&self, t: &'ll Type) -> &'ll Value {
        unsafe { llvm::LLVMConstNull(t) }
    }

    pub fn const_int(&self, t: &'ll Type, i: i64) -> &'ll Value {
        unsafe { llvm::LLVMConstInt(t, i as u64, llvm::True) }
    }

    pub fn const_uint(&self, t: &'ll Type, i: u64) -> &'ll Value {
        unsafe { llvm::LLVMConstInt(t, i, llvm::False) }
    }

    pub fn const_bool(&self, val: bool) -> &'ll Value {
        self.const_uint(self.type_i1(), val as u64)
    }

    pub fn const_i8(&self, i: i16) -> &'ll Value {
        self.const_int(self.type_i8(), i as i64)
    }

    pub fn const_i16(&self, i: i16) -> &'ll Value {
        self.const_int(self.type_i16(), i as i64)
    }

    pub fn const_i32(&self, i: i32) -> &'ll Value {
        self.const_int(self.type_i32(), i as i64)
    }

    pub fn const_u32(&self, i: u32) -> &'ll Value {
        self.const_uint(self.type_i32(), i as u64)
    }

    pub fn const_u64(&self, i: u64) -> &'ll Value {
        self.const_uint(self.type_i64(), i)
    }

    pub fn const_usize(&self, i: u64) -> &'ll Value {
        let bit_size = self.tcx.sess.target.data_layout.pointer_size.bits();
        if bit_size < 64 {
            // make sure it doesn't overflow
            assert!(i < (1 << bit_size));
        }

        self.const_uint(self.type_isize(), i)
    }

    pub fn type_i1(&self) -> &'ll Type {
        unsafe { llvm::LLVMInt1TypeInContext(self.llcx) }
    }

    pub fn type_i8(&self) -> &'ll Type {
        unsafe { llvm::LLVMInt8TypeInContext(self.llcx) }
    }

    pub fn type_i16(&self) -> &'ll Type {
        unsafe { llvm::LLVMInt16TypeInContext(self.llcx) }
    }

    pub fn type_i32(&self) -> &'ll Type {
        unsafe { llvm::LLVMInt32TypeInContext(self.llcx) }
    }

    pub fn type_i64(&self) -> &'ll Type {
        unsafe { llvm::LLVMInt64TypeInContext(self.llcx) }
    }

    pub fn type_void(&self) -> &'ll Type {
        unsafe { llvm::LLVMVoidTypeInContext(self.llcx) }
    }

    pub fn type_array(&self, ty: &'ll Type, len: u64) -> &'ll Type {
        unsafe { llvm::LLVMRustArrayType(ty, len) }
    }

    pub fn type_struct(&self, els: &[&'ll Type], packed: bool) -> &'ll Type {
        unsafe {
            llvm::LLVMStructTypeInContext(self.llcx, els.as_ptr(), els.len() as _, packed as _)
        }
    }

    fn type_padding_filler(&self, size: Size, align: Align) -> &'ll Type {
        let unit = Integer::approximate_align(&self.tcx.sess.target.data_layout, align);
        let size = size.bytes();
        let unit_size = unit.size().bytes();
        assert_eq!(size % unit_size, 0);
        self.type_array(self.type_from_integer(unit), size / unit_size)
    }

    pub fn type_of(&self, layout: Layout<'tcx>) -> &'ll Type {
        if let Abi::Scalar(scalar) = layout.abi {
            return scalar_to_llvm(self, scalar);
        }

        match layout.shape {
            FieldsShape::Primitive => {
                let fill = self.type_padding_filler(layout.size, layout.align);
                let packed = false;
                self.type_struct(&[fill], packed)
            }
            FieldsShape::Arbitrary { .. } => todo!(),
        }
    }

    pub fn immediate_type_of(&self, layout: Layout<'tcx>) -> &'ll Type {
        if let Abi::Scalar(scalar) = layout.abi
            && scalar.is_bool()
        {
            self.type_i1()
        } else {
            self.type_of(layout)
        }
    }

    pub fn type_func(&self, args: &[&'ll Type], ret: &'ll Type) -> &'ll Type {
        unsafe { llvm::LLVMFunctionType(ret, args.as_ptr(), args.len() as _, llvm::False) }
    }

    pub fn type_ptr(&self) -> &'ll Type {
        unsafe { llvm::LLVMPointerTypeInContext(self.llcx, 0) }
    }

    pub fn type_isize(&self) -> &'ll Type {
        match self.tcx.sess.target.pointer_width {
            16 => self.type_i16(),
            32 => self.type_i32(),
            64 => self.type_i64(),
            width => panic!("Unsupported width: {width}"),
        }
    }

    fn type_ix(&self, num_bits: u64) -> &'ll Type {
        unsafe { llvm::LLVMIntTypeInContext(self.llcx, num_bits as _) }
    }

    fn type_from_integer(&self, i: Integer) -> &'ll Type {
        match i {
            Integer::I8 => self.type_i8(),
            Integer::I16 => self.type_i16(),
            Integer::I32 => self.type_i32(),
            Integer::I64 => self.type_i64(),
        }
    }

    pub fn type_kind(&self, ty: &'ll Type) -> ssa::TypeKind {
        unsafe { llvm::LLVMRustGetTypeKind(ty).into() }
    }

    fn const_bitcast(&self, val: &'ll Value, ty: &'ll Type) -> &'ll Value {
        unsafe { llvm::LLVMConstBitCast(val, ty) }
    }

    pub fn func_params_types(&self, ty: &'ll Type) -> Vec<&'ll Type> {
        unsafe {
            let n_args = llvm::LLVMCountParamTypes(ty) as usize;
            let mut args = Vec::with_capacity(n_args);
            llvm::LLVMGetParamTypes(ty, args.as_mut_ptr());
            args.set_len(n_args);
            args
        }
    }

    pub fn scalar_to_backend(
        &self,
        int: ScalarRepr,
        layout: Scalar,
        llty: &'ll Type,
    ) -> &'ll Value {
        let dl = &self.tcx.sess.target.data_layout;

        let bits = if layout.is_bool() { 1 } else { layout.size(dl).bits() };
        let data = int.assert_bits(layout.size(dl));
        let llval = self.const_uint(self.type_ix(bits), data as u64); // TODO: Loss! Fix later

        if matches!(layout.primitive(), Primitive::Pointer) {
            unsafe { llvm::LLVMConstIntToPtr(llval, llty) }
        } else {
            self.const_bitcast(llval, llty)
        }
    }

    pub fn const_uint_big(&self, t: &'ll Type, u: u128) -> &'ll Value {
        unsafe {
            let words = [u as u64, (u >> 64) as u64];
            llvm::LLVMConstIntOfArbitraryPrecision(t, 2, words.as_ptr())
        }
    }
}

fn scalar_to_llvm<'ll>(cx: &CodegenCx<'ll, '_>, scalar: Scalar) -> &'ll Type {
    match scalar.primitive() {
        Int(i, _) => cx.type_from_integer(i),
        _ => todo!(),
    }
}
