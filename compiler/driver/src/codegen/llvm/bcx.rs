use {
    super::{cstr, CodegenCx},
    crate::codegen::{
        llvm::value::{LPlace, LValueRepr},
        ssa::{MemFlags, TypeKind},
    },
    llvm::{BasicBlock, Type, Value},
    middle::{
        abi,
        abi::{Abi, Align, ArgAbi, FnAbi, PassMode, WrappingRange},
    },
    std::{
        borrow::Cow,
        ffi::{c_char, CStr},
        iter, ptr,
    },
};

pub struct Bx<'a, 'll, 'tcx> {
    pub ll: &'ll mut llvm::Builder<'ll>,
    pub cx: &'a CodegenCx<'ll, 'tcx>,
    pub func: &'ll Value,
}

const EMPTY_C_STR: &CStr = unsafe { CStr::from_bytes_with_nul_unchecked(b"\0") };
const UNNAMED: *const c_char = EMPTY_C_STR.as_ptr();

impl<'a, 'll, 'tcx> Bx<'a, 'll, 'tcx> {
    pub fn build(cx: &'a CodegenCx<'ll, 'tcx>, func: &'ll Value) -> Self {
        unsafe { Bx { ll: llvm::LLVMCreateBuilderInContext(cx.llcx), cx, func } }
    }

    fn position_at_start(&mut self, llbb: &'ll BasicBlock) {
        unsafe {
            llvm::LLVMRustPositionBuilderAtStart(self.ll, llbb);
        }
    }

    pub fn append_block(&mut self, name: &str) -> &'ll BasicBlock {
        unsafe {
            let name = cstr(name);
            llvm::LLVMAppendBasicBlockInContext(self.cx.llcx, self.func, name.as_ptr())
        }
    }

    pub fn switch_to_block(&mut self, block: &'ll BasicBlock) {
        unsafe {
            llvm::LLVMPositionBuilderAtEnd(self.ll, block);
        }
    }

    pub fn ret_void(&mut self) {
        unsafe {
            llvm::LLVMBuildRetVoid(self.ll);
        }
    }

    pub fn ret(&mut self, v: &'ll Value) {
        unsafe {
            llvm::LLVMBuildRet(self.ll, v);
        }
    }

    pub fn unreachable(&mut self) {
        unsafe {
            llvm::LLVMBuildUnreachable(self.ll);
        }
    }

    pub fn alloca(&mut self, ty: &'ll Type, align: Align) -> &'ll Value {
        let mut bx = Self::build(self.cx, self.func);
        bx.position_at_start(unsafe { llvm::LLVMGetFirstBasicBlock(self.func) });
        unsafe {
            let alloca = llvm::LLVMBuildAlloca(bx.ll, ty, UNNAMED);
            llvm::LLVMSetAlignment(alloca, align.bytes() as _);
            alloca
        }
    }

    pub fn store(&mut self, val: &'ll Value, ptr: &'ll Value, align: Align) -> &'ll Value {
        self.store_with_flags(val, ptr, align, MemFlags::empty())
    }

    pub fn store_with_flags(
        &mut self,
        val: &'ll Value,
        ptr: &'ll Value,
        align: Align,
        flags: MemFlags,
    ) -> &'ll Value {
        assert_eq!(self.cx.type_kind(ptr.type_of()), TypeKind::Pointer);
        unsafe {
            let store = llvm::LLVMBuildStore(self.ll, val, ptr);
            let align = if flags.contains(MemFlags::UNALIGNED) { 1 } else { align.bytes() };
            llvm::LLVMSetAlignment(store, align as _);
            if flags.contains(MemFlags::VOLATILE) {
                llvm::LLVMSetVolatile(store, llvm::True);
            }
            if flags.contains(MemFlags::NONTEMPORAL) {
                todo!()
            }
            store
        }
    }

    pub fn store_fn_arg(&mut self, idx: &mut usize, arg: ArgAbi<'tcx>, dest: LPlace<'ll, 'tcx>) {
        let mut next = || {
            let val = self.get_param(*idx);
            *idx += 1;
            val
        };

        match arg.mode {
            PassMode::Ignore => {}
            PassMode::Direct => self.store_arg_abi(arg, next(), dest),
        }
    }

    pub fn store_arg_abi(&mut self, arg: ArgAbi<'tcx>, val: &'ll Value, dest: LPlace<'ll, 'tcx>) {
        if let PassMode::Ignore = arg.mode {
            return;
        }

        LValueRepr::ByVal(val).store(self, dest)
    }

    pub fn call_intrinsic(&mut self, intrinsic: &str, args: &[&'ll Value]) -> &'ll Value {
        let (ty, f) = self.cx.get_intrinsic(intrinsic);
        self.call(ty, None, None, f, args, None)
    }

    pub fn abort(&mut self) {
        self.call_intrinsic("llvm.trap", &[]);
    }

    pub fn get_param(&self, index: usize) -> &'ll Value {
        unsafe {
            let index = index as _;
            let params = llvm::LLVMCountParams(self.func);
            assert!(
                index < params,
                "out of bounds argument access: {index} out of {params} arguments"
            );
            llvm::LLVMGetParam(self.func, index)
        }
    }

    pub fn bitcast(&mut self, val: &'ll Value, dest: &'ll Type) -> &'ll Value {
        unsafe { llvm::LLVMBuildBitCast(self.ll, val, dest, UNNAMED) }
    }

    pub fn intcast(&mut self, val: &'ll Value, dest_ty: &'ll Type, signed: bool) -> &'ll Value {
        unsafe {
            llvm::LLVMBuildIntCast2(
                self.ll,
                val,
                dest_ty,
                if signed { llvm::True } else { llvm::False },
                UNNAMED,
            )
        }
    }

    fn check_call<'b>(
        &mut self,
        cause: &str,
        fn_ty: &'ll Type,
        llfn: &'ll Value,
        args: &'b [&'ll Value],
    ) -> Cow<'b, [&'ll Value]> {
        assert!(
            self.cx.type_kind(fn_ty) == TypeKind::Function,
            "builder::{cause} not passed a function, but {fn_ty:?}"
        );

        let param_tys = self.cx.func_params_types(fn_ty);

        let all_args_match = iter::zip(&param_tys, args.iter().copied().map(Value::type_of))
            .all(|(expected_ty, actual_ty)| *expected_ty == actual_ty);

        if all_args_match {
            return Cow::Borrowed(args);
        }

        let casted_args: Vec<_> = iter::zip(param_tys, args)
            .enumerate()
            .map(|(i, (expected_ty, &actual_val))| {
                let actual_ty = actual_val.type_of();
                if expected_ty != actual_ty {
                    if cfg!(debug_assertions) {
                        panic!(
                            "type mismatch in function call of {:?}. \
                            Expected {:?} for param {}, got {:?}",
                            llfn, expected_ty, i, actual_ty,
                        );
                    }
                    self.bitcast(actual_val, expected_ty)
                } else {
                    actual_val
                }
            })
            .collect();

        Cow::Owned(casted_args)
    }

    pub fn call(
        &mut self,
        llty: &'ll Type,
        _fn_attrs: Option<()>,
        _fn_abi: Option<&FnAbi<'tcx>>,
        llfn: &'ll Value,
        args: &[&'ll Value],
        _funclet: Option<()>,
    ) -> &'ll Value {
        let args = self.check_call("call", llty, llfn, args);
        // requires capacity to the satisfy unsafe
        let mut bundles = Vec::with_capacity(1);
        let call = unsafe {
            llvm::LLVMRustBuildCall(
                self.ll,
                llty,
                llfn,
                args.as_ptr() as *const &Value,
                args.len() as _,
                bundles.as_ptr(),
                bundles.len() as _,
            )
        };
        call
    }

    pub fn br(&mut self, dest: &'ll BasicBlock) {
        unsafe {
            llvm::LLVMBuildBr(self.ll, dest);
        }
    }

    pub fn cond_br(
        &mut self,
        cond: &'ll Value,
        then_llbb: &'ll BasicBlock,
        else_llbb: &'ll BasicBlock,
    ) {
        unsafe {
            llvm::LLVMBuildCondBr(self.ll, cond, then_llbb, else_llbb);
        }
    }

    pub fn switch(
        &mut self,
        v: &'ll Value,
        else_llbb: &'ll BasicBlock,
        cases: impl ExactSizeIterator<Item = (u128, &'ll BasicBlock)>,
    ) {
        let switch = unsafe { llvm::LLVMBuildSwitch(self.ll, v, else_llbb, cases.len() as _) };
        for (at, dest) in cases {
            let on_val = self.cx.const_uint(v.type_of(), at as u64); // fixme: Loss!
            unsafe { llvm::LLVMAddCase(switch, on_val, dest) }
        }
    }

    pub fn load(&mut self, ty: &'ll Type, ptr: &'ll Value, align: Align) -> &'ll Value {
        unsafe {
            let load = llvm::LLVMBuildLoad2(self.ll, ty, ptr, UNNAMED);
            llvm::LLVMSetAlignment(load, align.bytes() as _);
            load
        }
    }

    pub fn icmp(&mut self, op: llvm::IntPredicate, lhs: &'ll Value, rhs: &'ll Value) -> &'ll Value {
        unsafe { llvm::LLVMBuildICmp(self.ll, op as _, lhs, rhs, UNNAMED) }
    }

    pub fn memcpy(
        &mut self,
        dst: &'ll Value,
        dst_align: Align,
        src: &'ll Value,
        src_align: Align,
        size: &'ll Value,
        flags: MemFlags,
    ) {
        assert!(!flags.contains(MemFlags::NONTEMPORAL), "non-temporal memcpy not supported");
        let size = self.intcast(size, self.cx.type_isize(), false);
        let is_volatile = flags.contains(MemFlags::VOLATILE);
        unsafe {
            llvm::LLVMRustBuildMemCpy(
                self.ll,
                dst,
                dst_align.bytes() as _,
                src,
                src_align.bytes() as _,
                size,
                is_volatile,
            );
        }
    }

    pub fn zext(&mut self, val: &'ll Value, dest_ty: &'ll Type) -> &'ll Value {
        unsafe { llvm::LLVMBuildZExt(self.ll, val, dest_ty, UNNAMED) }
    }

    fn trunc(&mut self, val: &'ll Value, dest_ty: &'ll Type) -> &'ll Value {
        unsafe { llvm::LLVMBuildTrunc(self.ll, val, dest_ty, UNNAMED) }
    }

    fn sext(&mut self, val: &'ll Value, dest_ty: &'ll Type) -> &'ll Value {
        unsafe { llvm::LLVMBuildSExt(self.ll, val, dest_ty, UNNAMED) }
    }

    pub fn from_immediate(&mut self, val: &'ll Value) -> &'ll Value {
        if val.type_of() == self.cx.type_i1() { self.zext(val, self.cx.type_i8()) } else { val }
    }

    pub fn to_immediate(&mut self, val: &'ll Value, layout: abi::Layout<'tcx>) -> &'ll Value {
        if let Abi::Scalar(scalar) = layout.abi {
            self.to_immediate_scalar(val, scalar)
        } else {
            val
        }
    }

    fn to_immediate_scalar(&mut self, val: &'ll Value, scalar: abi::Scalar) -> &'ll Value {
        if scalar.is_bool() { self.trunc(val, self.cx.type_i1()) } else { val }
    }

    /* assumes guarantees */

    pub fn assume_range(&mut self, load: &'ll Value, range: WrappingRange) {
        unsafe {
            let llty = load.type_of();
            let v = [
                self.cx.const_uint_big(llty, range.start),
                self.cx.const_uint_big(llty, range.end.wrapping_add(1)),
            ];

            llvm::LLVMSetMetadata(
                load,
                llvm::MD_range as _,
                llvm::LLVMMDNodeInContext(self.cx.llcx, v.as_ptr(), v.len() as _),
            );
        }
    }

    pub fn assume_noundef(&mut self, load: &'ll Value) {
        unsafe {
            llvm::LLVMSetMetadata(
                load,
                llvm::MD_noundef as _,
                llvm::LLVMMDNodeInContext(self.cx.llcx, ptr::null(), 0),
            );
        }
    }

    pub fn assume_nonnull(&mut self, load: &'ll Value) {
        unsafe {
            llvm::LLVMSetMetadata(
                load,
                llvm::MD_nonnull as _,
                llvm::LLVMMDNodeInContext(self.cx.llcx, ptr::null(), 0),
            );
        }
    }

    pub fn assume_align(&mut self, load: &'ll Value, align: Align) {
        unsafe {
            let v = [self.cx.const_u64(align.bytes())];

            llvm::LLVMSetMetadata(
                load,
                llvm::MD_align as _,
                llvm::LLVMMDNodeInContext(self.cx.llcx, v.as_ptr(), v.len() as _),
            );
        }
    }
}

macro_rules! pure_instructions {
    ($($name:ident($($arg:ident),*) => $llvm_capi:ident),+ $(,)?) => {
        $(pub fn $name(&mut self, $($arg: &'ll Value),*) -> &'ll Value {
            unsafe {
                llvm::$llvm_capi(self.ll, $($arg,)* UNNAMED)
            }
        })+
    }
}

impl<'a, 'll, 'tcx> Bx<'a, 'll, 'tcx> {
    pure_instructions! {
        add(a, b) => LLVMBuildAdd,
        fadd(a, b) => LLVMBuildFAdd,
        sub(a, b) => LLVMBuildSub,
        fsub(a, b) => LLVMBuildFSub,
        mul(a, b) => LLVMBuildMul,
        fmul(a, b) => LLVMBuildFMul,
        udiv(a, b) => LLVMBuildUDiv,
        exactudiv(a, b) => LLVMBuildExactUDiv,
        sdiv(a, b) => LLVMBuildSDiv,
        exactsdiv(a, b) => LLVMBuildExactSDiv,
        fdiv(a, b) => LLVMBuildFDiv,
        urem(a, b) => LLVMBuildURem,
        srem(a, b) => LLVMBuildSRem,
        frem(a, b) => LLVMBuildFRem,
        shl(a, b) => LLVMBuildShl,
        lshr(a, b) => LLVMBuildLShr,
        ashr(a, b) => LLVMBuildAShr,
        and(a, b) => LLVMBuildAnd,
        or(a, b) => LLVMBuildOr,
        xor(a, b) => LLVMBuildXor,
        neg(x) => LLVMBuildNeg,
        fneg(x) => LLVMBuildFNeg,
        not(x) => LLVMBuildNot,
        unchecked_sadd(x, y) => LLVMBuildNSWAdd,
        unchecked_uadd(x, y) => LLVMBuildNUWAdd,
        unchecked_ssub(x, y) => LLVMBuildNSWSub,
        unchecked_usub(x, y) => LLVMBuildNUWSub,
        unchecked_smul(x, y) => LLVMBuildNSWMul,
        unchecked_umul(x, y) => LLVMBuildNUWMul,
    }
}
