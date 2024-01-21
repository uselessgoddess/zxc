use {
    super::CodegenCx,
    llvm::Type,
    middle::abi::{Abi, Conv, FnAbi, PassMode},
};

pub trait FnAbiLlvmExt<'ll, 'tcx> {
    fn llvm_type(&self, cx: &CodegenCx<'ll, 'tcx>) -> &'ll Type;
    fn llvm_cconv(&self) -> llvm::CallConv;
}

impl<'ll, 'tcx> FnAbiLlvmExt<'ll, 'tcx> for FnAbi<'tcx> {
    fn llvm_type(&self, cx: &CodegenCx<'ll, 'tcx>) -> &'ll Type {
        let args = &self.args;

        let mut llargs_types = Vec::with_capacity(args.len());
        let llret_ty = match &self.ret.mode {
            PassMode::Ignore => cx.type_void(),
            PassMode::Direct => cx.immediate_type_of(self.ret.ty.layout),
        };

        for arg in args.iter() {
            let layout = arg.ty.layout;
            let llarg_ty = match &arg.mode {
                PassMode::Ignore => continue,
                PassMode::Direct => {
                    if let Abi::Aggregate { .. } = layout.abi {
                        assert!(
                            layout.is_sized(),
                            "`PassMode::Direct` for unsized type: {:?}",
                            arg.ty
                        );
                    }
                    cx.immediate_type_of(layout)
                }
            };
            llargs_types.push(llarg_ty);
        }
        cx.type_func(&llargs_types, llret_ty)
    }

    fn llvm_cconv(&self) -> llvm::CallConv {
        match self.conv {
            Conv::C | Conv::Zxc => llvm::CallConv::CCallConv,
        }
    }
}
