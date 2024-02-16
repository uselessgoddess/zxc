use {
    super::{cstr, errors::WithLlvmError, util},
    middle::{
        abi::TargetDataLayout,
        hir::Hx,
        mir::{CodegenUnit, Instance},
        sess::ModuleType,
        spec::RelocModel,
        FxHashMap, Session, Tx,
    },
    std::{cell::RefCell, fmt, mem::ManuallyDrop, path::PathBuf},
};

pub struct ModuleLlvm {
    pub llcx: &'static mut llvm::Context,
    llmod_raw: *const llvm::Module,
    // This field is `ManuallyDrop` because it is important that the `TargetMachine`
    // is disposed prior to the `Context` being disposed otherwise UAFs can occur.
    pub machine: ManuallyDrop<OwnedTargetMachine>,
}

unsafe impl Send for ModuleLlvm {}
unsafe impl Sync for ModuleLlvm {}

impl fmt::Debug for ModuleLlvm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ModuleLlvm {{ .. }}")
    }
}

use super::back::OwnedTargetMachine;

fn to_llvm_opt_settings(opt: OptLevel) -> (llvm::CodeGenOptLevel, llvm::CodeGenOptSize) {
    use OptLevel::*;
    match opt {
        No => (llvm::CodeGenOptLevel::None, llvm::CodeGenOptSizeNone),
        Less => (llvm::CodeGenOptLevel::Less, llvm::CodeGenOptSizeNone),
        Default => (llvm::CodeGenOptLevel::Default, llvm::CodeGenOptSizeNone),
        Aggressive => (llvm::CodeGenOptLevel::Aggressive, llvm::CodeGenOptSizeNone),
        Size => (llvm::CodeGenOptLevel::Default, llvm::CodeGenOptSizeDefault),
        SizeMin => (llvm::CodeGenOptLevel::Default, llvm::CodeGenOptSizeAggressive),
    }
}

fn to_llvm_relocation_model(relocation_model: RelocModel) -> llvm::RelocModel {
    match relocation_model {
        RelocModel::Static => llvm::RelocModel::Static,
        // LLVM doesn't have PIE relocation model, it represents PIE as PIC with an extra attribute.
        RelocModel::Pic | RelocModel::Pie => llvm::RelocModel::PIC,
        RelocModel::DynamicNoPic => llvm::RelocModel::DynamicNoPic,
        RelocModel::Ropi => llvm::RelocModel::ROPI,
        RelocModel::Rwpi => llvm::RelocModel::RWPI,
        RelocModel::RopiRwpi => llvm::RelocModel::ROPI_RWPI,
    }
}

pub fn target_machine_factory(
    sess: &Session,
    opt_level: OptLevel,
    target_features: &[String],
    output_obj_file: Option<PathBuf>,
) -> Result<OwnedTargetMachine, LlvmError<'static>> {
    let use_softfp = false;

    let (opt_level, _) = to_llvm_opt_settings(opt_level);
    let reloc_model = to_llvm_relocation_model(sess.relocation_model());

    let function_sections = false;
    let data_sections = function_sections;
    let unique_section_names = true;

    let code_model = llvm::CodeModel::None;

    let singlethread = false;

    let triple = CString::new(&*sess.target.triple).unwrap();
    let cpu = CString::new(util::target_cpu(sess)).unwrap();
    let features = CString::new(target_features.join(",")).unwrap();
    let abi = CString::new("").unwrap(); // no provided now

    let trap_unreachable = true;
    let asm_comments = false;
    let relax_elf_relocations = false;
    let emit_stack_size_section = false;
    let use_init_array = true;
    let force_emulated_tls = false;

    let path_to_cstring_helper = |path: Option<PathBuf>| -> CString {
        let path = path.unwrap_or_default();
        CString::new(path.to_str().unwrap()).unwrap()
    };

    let split_dwarf_file = path_to_cstring_helper(None);
    let debuginfo_compression = path_to_cstring_helper(None);
    let output_obj_file = path_to_cstring_helper(output_obj_file);

    OwnedTargetMachine::new(
        &triple,
        &cpu,
        &features,
        &abi,
        code_model,
        reloc_model,
        opt_level,
        use_softfp,
        function_sections,
        data_sections,
        unique_section_names,
        trap_unreachable,
        singlethread,
        asm_comments,
        emit_stack_size_section,
        relax_elf_relocations,
        use_init_array,
        &split_dwarf_file,
        &output_obj_file,
        &debuginfo_compression,
        force_emulated_tls,
        &[0],
    )
}

pub fn llvm_err(handler: &Handler, err: LlvmError) -> FatalError {
    match llvm::last_error() {
        Some(llvm_err) => handler.emit_almost_fatal(WithLlvmError(err, llvm_err)),
        None => handler.emit_almost_fatal(err),
    }
}

pub fn create_target_machine(tcx: Tx<'_>, mod_name: &str) -> OwnedTargetMachine {
    let output_obj_file =
        Some(tcx.output_filenames().temp_path(OutputType::Object, Some(mod_name)));

    target_machine_factory(tcx.sess, tcx.sess.opts.C.opt_level, &[], output_obj_file)
        .unwrap_or_else(|err| llvm_err(tcx.sess.diagnostic(), err).raise())
}

use {
    crate::codegen::llvm::errors::LlvmError,
    llvm::{Type, Value},
    middle::{
        errors::Handler,
        sess::{OptLevel, OutputType},
        FatalError,
    },
    std::{
        ffi::{CStr, CString},
        str,
    },
};

pub unsafe fn create_module<'ll>(
    tcx: Tx<'_>,
    llcx: &'ll llvm::Context,
    name: &str,
) -> (&'ll llvm::Module, OwnedTargetMachine) {
    let sess = tcx.sess;
    let mod_name = cstr(name);
    let llmod = llvm::LLVMModuleCreateWithNameInContext(mod_name.as_ptr(), llcx);

    let target_data_layout = sess.target.data_layout.clone();

    let machine = create_target_machine(tcx, name);
    llvm::LLVMRustSetDataLayoutFromTargetMachine(llmod, &machine);

    let llvm_data_layout = llvm::LLVMGetDataLayoutStr(llmod);
    let llvm_data_layout = str::from_utf8(CStr::from_ptr(llvm_data_layout).to_bytes())
        .expect("got a non-UTF8 data-layout from LLVM");

    if cfg!(debug_assertions)
        && target_data_layout
            != TargetDataLayout::parse_from_llvm_datalayout_string(llvm_data_layout).unwrap()
    {
        panic!(
            "data-layout for target `{}`, \
                  differs from LLVM target's `{}` default layout, `{llvm_data_layout}`",
            sess.opts.triple, sess.target.triple,
        );
    }

    let data_layout = cstr(&llvm_data_layout);
    llvm::LLVMSetDataLayout(llmod, data_layout.as_ptr());

    let llvm_target = cstr(&sess.target.triple);
    llvm::LLVMRustSetNormalizedTarget(llmod, llvm_target.as_ptr());

    if let reloc_model @ (RelocModel::Pic | RelocModel::Pie) = sess.relocation_model() {
        llvm::LLVMRustSetModulePICLevel(llmod);
        // PIE is potentially more effective than PIC, but can only be used in executables.
        // If all our outputs are executables, then we can relax PIC to PIE.
        if reloc_model == RelocModel::Pie
            || tcx.module_types().iter().all(|ty| *ty == ModuleType::Executable)
        {
            llvm::LLVMRustSetModulePIELevel(llmod);
        }
    }

    // Does not support code models now
    llvm::LLVMRustSetModuleCodeModel(llmod, llvm::CodeModel::None);

    let zxc_producer = format!("zxc version {}", "0.0.0");
    let name_metadata = llvm::LLVMMDStringInContext(
        llcx,
        zxc_producer.as_ptr().cast(),
        zxc_producer.as_bytes().len() as _,
    );
    let ident = cstr("llvm.ident");
    llvm::LLVMAddNamedMetadataOperand(
        llmod,
        ident.as_ptr(),
        llvm::LLVMMDNodeInContext(llcx, &name_metadata, 1),
    );

    (llmod, machine)
}

impl ModuleLlvm {
    pub fn new(tcx: Tx<'_>, mod_name: &str) -> Self {
        unsafe {
            let llcx = llvm::LLVMRustContextCreate(tcx.sess.fewer_names());
            let (llmod_raw, machine) = create_module(tcx, llcx, mod_name);
            ModuleLlvm { llmod_raw, llcx, machine: ManuallyDrop::new(machine) }
        }
    }

    pub fn llmod(&self) -> &llvm::Module {
        unsafe { &*self.llmod_raw }
    }
}

pub struct CodegenCx<'ll, 'tcx> {
    pub tcx: Tx<'tcx>,
    pub hix: Hx<'tcx>,

    pub llmod: &'ll llvm::Module,
    pub llcx: &'ll llvm::Context,

    pub cgu: &'ll CodegenUnit<'tcx>,
    pub instances: RefCell<FxHashMap<Instance<'tcx>, &'ll Value>>,
    intrinsics: RefCell<FxHashMap<&'static str, (&'ll Type, &'ll Value)>>,
}

impl<'ll, 'tcx> CodegenCx<'ll, 'tcx> {
    pub(crate) fn new(
        hix: Hx<'tcx>,
        cgu: &'ll CodegenUnit<'tcx>,
        llvm_module: &'ll ModuleLlvm,
    ) -> Self {
        let (llcx, llmod) = (&*llvm_module.llcx, llvm_module.llmod());
        CodegenCx {
            hix,
            tcx: hix.tcx,
            llmod,
            llcx,
            cgu,
            instances: Default::default(),
            intrinsics: Default::default(),
        }
    }

    pub fn get_intrinsic(&self, key: &str) -> (&'ll Type, &'ll Value) {
        if let Some(v) = self.intrinsics.borrow().get(key).cloned() {
            return v;
        }

        self.declare_intrinsic(key).unwrap_or_else(|| panic!("unknown intrinsic '{}'", key))
    }

    fn insert_intrinsic(
        &self,
        name: &'static str,
        args: Option<&[&'ll Type]>,
        ret: &'ll Type,
    ) -> (&'ll Type, &'ll Value) {
        let fn_ty = if let Some(args) = args {
            self.type_func(args, ret)
        } else {
            todo!("variadic intrinsic")
        };
        let f = self.declare_cfn(name, llvm::UnnamedAddr::No, fn_ty);
        self.intrinsics.borrow_mut().insert(name, (fn_ty, f));
        (fn_ty, f)
    }

    fn declare_intrinsic(&self, key: &str) -> Option<(&'ll Type, &'ll Value)> {
        macro_rules! ifn {
            ($name:expr, fn() -> $ret:expr) => (
                if key == $name {
                    return Some(self.insert_intrinsic($name, Some(&[]), $ret));
                }
            );
            ($name:expr, fn($($arg:expr),*) -> $ret:expr) => (
                if key == $name {
                    return Some(self.insert_intrinsic($name, Some(&[$($arg),*]), $ret));
                }
            );
        }

        let void = self.type_void();

        ifn!("llvm.trap", fn() -> void);

        None
    }

    pub fn get_declared_value(&self, name: &str) -> Option<&'ll Value> {
        unsafe { llvm::LLVMRustGetNamedValue(self.llmod, name.as_ptr().cast(), name.len()) }
    }

    pub fn declare_entry_fn(
        &self,
        name: &str,
        callconv: llvm::CallConv,
        unnamed: llvm::UnnamedAddr,
        fn_type: &'ll Type,
    ) -> &'ll Value {
        let visibility = llvm::Visibility::Default;
        super::base::declare_raw_fn(self, name, callconv, unnamed, visibility, fn_type)
    }

    pub fn declare_cfn(
        &self,
        name: &str,
        unnamed: llvm::UnnamedAddr,
        fn_type: &'ll Type,
    ) -> &'ll Value {
        let visibility = llvm::Visibility::Default;
        super::base::declare_raw_fn(
            self,
            name,
            llvm::CallConv::CCallConv,
            unnamed,
            visibility,
            fn_type,
        )
    }

    pub fn declare_c_main(&self, fn_type: &'ll Type) -> Option<&'ll Value> {
        let entry_name = "main";
        if self.get_declared_value(entry_name).is_none() {
            Some(self.declare_entry_fn(
                entry_name,
                llvm::CallConv::CCallConv, // override from target
                llvm::UnnamedAddr::Global,
                fn_type,
            ))
        } else {
            None
        }
    }
}
