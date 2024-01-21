use {
    super::{abi::FnAbiLlvmExt, Bx, CodegenCx, ModuleLlvm},
    crate::codegen::{
        llvm::value::{LPlace, LValue},
        ssa::{MemFlags, ModuleCodegen},
    },
    llvm::{BasicBlock, Type, Value},
    middle::{
        abi::{self, Align, FnAbi, PassMode, TyAbi},
        hir::Hx,
        mir::{
            self,
            mono::{Linkage, Visibility},
            BasicBlockData, CodegenUnit, ConstValue, Instance, InstanceDef, Operand, PlaceElem,
            Rvalue, Statement, Terminator, Ty,
        },
        BitSet, IndexVec,
    },
    std::{ffi::CString, iter},
};

pub fn declare_raw_fn<'ll>(
    cx: &CodegenCx<'ll, '_>,
    name: &str,
    callconv: llvm::CallConv,
    unnamed: llvm::UnnamedAddr,
    visibility: llvm::Visibility,
    ty: &'ll Type,
) -> &'ll Value {
    let llfn = unsafe {
        llvm::LLVMRustGetOrInsertFunction(cx.llmod, name.as_ptr().cast(), name.len(), ty)
    };

    llvm::SetFunctionCallConv(llfn, callconv);
    llvm::SetUnnamedAddress(llfn, unnamed);
    llvm::set_visibility(llfn, visibility);

    llfn
}

pub fn get_fn<'ll, 'tcx>(cx: &CodegenCx<'ll, 'tcx>, instance: Instance<'tcx>) -> &'ll Value {
    if let Some(&llfn) = cx.instances.borrow().get(&instance) {
        return llfn;
    }

    let InstanceDef::Item(def) = instance.def;

    let sym = cx.hix.symbol_name(def).name;
    let fn_abi = cx.tcx.fn_abi_of_sig(cx.hix.instances[def].sig);

    let llfn = if let Some(llfn) = cx.get_declared_value(sym) {
        llfn
    } else {
        let llfn = cx.declare_fn(sym, &fn_abi, Some(instance));

        unsafe {
            llvm::LLVMRustSetLinkage(llfn, llvm::Linkage::ExternalLinkage);
        }

        llfn
    };

    cx.instances.borrow_mut().insert(instance, llfn);

    llfn
}

impl<'ll, 'tcx> CodegenCx<'ll, 'tcx> {
    pub fn declare_fn(
        &self,
        name: &str,
        fn_abi: &FnAbi<'tcx>,
        _instance: Option<Instance<'tcx>>,
    ) -> &'ll Value {
        declare_raw_fn(
            self,
            name,
            fn_abi.llvm_cconv(),
            llvm::UnnamedAddr::Global,
            llvm::Visibility::Default,
            fn_abi.llvm_type(self),
        )
    }

    pub fn predefine_fn(
        &mut self,
        instance: Instance<'tcx>,
        linkage: Linkage,
        visibility: Visibility,
        symbol: &str,
    ) {
        let hix = self.hix;

        let InstanceDef::Item(def) = instance.def;
        let fn_abi = self.tcx.fn_abi_of_sig(hix.instances[def].sig);

        let lldecl = self.declare_fn(symbol, &fn_abi, Some(instance));
        unsafe {
            llvm::LLVMRustSetLinkage(lldecl, linkage_to_llvm(linkage));
            llvm::LLVMRustSetVisibility(lldecl, visibility_to_llvm(visibility));
        }

        // if todo!() {
        //     llvm::LLVMRustSetDSOLocal(lldecl, true);
        // }

        self.instances.borrow_mut().insert(instance, lldecl);
    }

    pub fn define_fn(&mut self, instance: Instance<'tcx>) {
        super::mir::codegen_mir(self, instance);
    }
}

fn scalar_copy_llvm_type<'ll, 'tcx>(
    cx: &CodegenCx<'ll, 'tcx>,
    layout: abi::Layout<'tcx>,
) -> Option<&'ll Type> {
    debug_assert!(layout.is_sized());

    let threshold = cx.tcx.sess.target.data_layout.pointer_size * 4;
    if layout.size > threshold {
        return None;
    }

    None
}

pub fn memcpy_ty<'ll, 'tcx>(
    bcx: &mut Bx<'_, 'll, 'tcx>,
    dst: &'ll Value,
    dst_align: Align,
    src: &'ll Value,
    src_align: Align,
    ty: TyAbi<'tcx>,
    flags: MemFlags,
) {
    let size = ty.size.bytes();
    if size == 0 {
        return;
    }

    if flags == MemFlags::empty()
        && let Some(bty) = scalar_copy_llvm_type(bcx.cx, ty.layout)
    {
        let temp = bcx.load(bty, src, src_align);
        bcx.store(temp, dst, dst_align);
    } else {
        bcx.memcpy(dst, dst_align, src, src_align, bcx.cx.const_usize(size), flags);
    }
}

use super::mir::{Cached, LLocal};

pub struct FunctionCx<'a, 'll, 'tcx> {
    pub hix: Hx<'tcx>,
    pub cx: &'a CodegenCx<'ll, 'tcx>,
    pub bcx: Bx<'a, 'll, 'tcx>,
    pub func: &'ll Value,
    pub mir: &'a mir::Body<'tcx>,
    pub fn_abi: &'a FnAbi<'tcx>,

    pub locals: IndexVec<mir::Local, LLocal<'ll, 'tcx>>,
    pub blocks: IndexVec<mir::BasicBlock, Cached<&'ll BasicBlock>>,
}

impl<'a, 'll, 'tcx> FunctionCx<'a, 'll, 'tcx> {
    pub(crate) fn local_place(&mut self, local: mir::Local) -> LLocal<'ll, 'tcx> {
        *self.locals.get(local).unwrap_or_else(|| {
            panic!("Local {:?} doesn't exist", local);
        })
    }

    pub fn try_block(&mut self, bb: mir::BasicBlock) -> Option<&'ll BasicBlock> {
        match self.blocks[bb] {
            Cached::None => {
                // TODO: check `fewer_names`
                let llbb = self.bcx.append_block(&format!("{bb:?}"));
                self.blocks[bb] = Cached::Some(llbb);
                Some(llbb)
            }
            Cached::Some(llbb) => Some(llbb),
            Cached::Skip => None,
        }
    }

    pub fn block(&mut self, bb: mir::BasicBlock) -> &'ll BasicBlock {
        self.try_block(bb).unwrap()
    }
}

pub fn visibility_to_llvm(linkage: Visibility) -> llvm::Visibility {
    match linkage {
        Visibility::Default => llvm::Visibility::Default,
        Visibility::Hidden => llvm::Visibility::Hidden,
    }
}

pub fn linkage_to_llvm(linkage: Linkage) -> llvm::Linkage {
    match linkage {
        Linkage::External => llvm::Linkage::ExternalLinkage,
        Linkage::AvailableExternally => llvm::Linkage::AvailableExternallyLinkage,
        Linkage::LinkOnceAny => llvm::Linkage::LinkOnceAnyLinkage,
        Linkage::LinkOnceODR => llvm::Linkage::LinkOnceODRLinkage,
        Linkage::WeakAny => llvm::Linkage::WeakAnyLinkage,
        Linkage::WeakODR => llvm::Linkage::WeakODRLinkage,
        Linkage::Appending => llvm::Linkage::AppendingLinkage,
        Linkage::Internal => llvm::Linkage::InternalLinkage,
        Linkage::Private => llvm::Linkage::PrivateLinkage,
        Linkage::ExternalWeak => llvm::Linkage::ExternalWeakLinkage,
        Linkage::Common => llvm::Linkage::CommonLinkage,
    }
}

pub fn compile_codegen_unit<'tcx>(
    hix: Hx<'tcx>,
    cgu: CodegenUnit<'tcx>,
) -> ModuleCodegen<ModuleLlvm> {
    let llvm_module = ModuleLlvm::new(hix.tcx, cgu.name.as_str());
    {
        let mut cx = CodegenCx::new(hix, &cgu, &llvm_module);

        for (&mono_item, &data) in cx.cgu.items.iter() {
            let symbol = mono_item.symbol_name(hix).name;
            cx.predefine_fn(mono_item, data.linkage, data.visibility, symbol);
        }

        for (&mono_item, _) in cx.cgu.items.iter() {
            cx.define_fn(mono_item);
        }

        super::shim::maybe_create_entry_wrapper(&mut cx);
    }

    ModuleCodegen { name: cgu.name.to_string(), cg: llvm_module }
}
