use {
    super::ssa::{CodegenResults, CompiledModule, ModuleCodegen, ModuleInfo},
    middle::{
        hir::Hx,
        mir::CodegenUnit,
        rayon::prelude::*,
        sess::{OutFileName, OutputFilenames},
        Session,
    },
    std::{any::Any, ffi::CString, fs, io, path::Path, ptr},
};

mod back {
    pub mod target;

    pub use target::OwnedTargetMachine;
}

mod abi;
mod base;
mod bcx;
mod common;
mod context;
mod errors;
mod mir;
mod shim;
mod types;
mod util;
mod value;

use middle::{
    errors::Handler,
    sess::{OptLevel, OutputType},
    FatalError, Tx,
};
pub(crate) use {
    base::{compile_codegen_unit, FunctionCx},
    bcx::Bx,
    context::{llvm_err, CodegenCx, ModuleLlvm},
    value::{LPlace, LValue, LValueRepr},
};

// TODO: use custom CStr type
fn cstr(str: &str) -> CString {
    CString::new(str).unwrap()
}

pub struct CodegenContext<'tcx> {
    outputs: &'tcx OutputFilenames,
    module: ModuleConfig,
}

#[derive(Clone, Copy, PartialEq)]
pub enum EmitObj {
    None,
    Object(BitcodeSection),
}

#[derive(Clone, Copy, PartialEq)]
pub enum BitcodeSection {
    None,
    Full,
}

pub struct ModuleConfig {
    pub opt_level: Option<OptLevel>,

    pub emit_ir: bool,
    pub emit_obj: EmitObj,
}

impl ModuleConfig {
    pub fn new(sess: &Session) -> ModuleConfig {
        let opt_level = sess.opts.C.opt_level;

        let should_emit = sess.opts.output_types.contains_key(&OutputType::Exe)
            || sess.opts.output_types.contains_key(&OutputType::Object)
            || true;
        let emit_obj =
            if should_emit { EmitObj::Object(BitcodeSection::None) } else { EmitObj::None };

        ModuleConfig {
            opt_level: Some(opt_level),
            emit_ir: sess.opts.output_types.contains_key(&OutputType::LlvmAssembly),
            emit_obj,
        }
    }
}

impl CodegenContext<'_> {
    pub fn config(&self) -> &ModuleConfig {
        &self.module
    }
}

#[derive(Debug)]
pub struct OngoingCodegen {
    modules: Vec<ModuleCodegen<ModuleLlvm>>,
    info: ModuleInfo,
}

fn driver<'tcx>(hix: Hx<'tcx>, cgus: Vec<CodegenUnit<'tcx>>) -> OngoingCodegen {
    let target_cpu = hix.tcx.sess.target.cpu.to_string();
    let native_libs = cgus.iter().flat_map(|cgu| &cgu.native_libs).copied().collect();
    OngoingCodegen {
        modules: cgus.into_par_iter().map(|cgu| compile_codegen_unit(hix, cgu)).collect(),
        info: ModuleInfo::new(hix.tcx, target_cpu, native_libs),
    }
}

pub fn copy_to_stdout(from: &Path) -> io::Result<()> {
    let file = fs::File::open(from)?;
    io::copy(&mut io::BufReader::new(file), &mut io::stdout())?;
    Ok(())
}

impl OngoingCodegen {
    pub fn join(self, sess: &Session, outputs: &OutputFilenames) -> CodegenResults {
        let cgx = CodegenContext { outputs, module: ModuleConfig::new(sess) };

        let mut compiled_modules = vec![];

        let result: Result<_, FatalError> = try {
            for module in self.modules {
                unsafe {
                    optimize(&cgx, sess.diagnostic(), &module, cgx.config())?;
                }

                compiled_modules.push(codegen(&cgx, sess.diagnostic(), module, cgx.config())?);
            }
        };

        if result.is_err() {
            sess.abort_if_errors();
        }

        CodegenResults { modules: compiled_modules, info: self.info }
    }
}

// skip
unsafe fn optimize(
    cgx: &CodegenContext<'_>,
    handler: &Handler,
    module: &ModuleCodegen<ModuleLlvm>,
    config: &ModuleConfig,
) -> Result<(), FatalError> {
    llvm_optimize(
        cgx,
        handler,
        module,
        config,
        config.opt_level.unwrap_or(OptLevel::No),
        llvm::OptStage::ThinLTO,
    )
}

pub(crate) unsafe fn llvm_optimize(
    cgx: &CodegenContext<'_>,
    handler: &Handler,
    module: &ModuleCodegen<ModuleLlvm>,
    config: &ModuleConfig,
    opt_level: OptLevel,
    opt_stage: llvm::OptStage,
) -> Result<(), FatalError> {
    fn to_pass_builder_opt_level(opt: OptLevel) -> llvm::PassBuilderOptLevel {
        match opt {
            OptLevel::No => llvm::PassBuilderOptLevel::O0,
            OptLevel::Less => llvm::PassBuilderOptLevel::O1,
            OptLevel::Default => llvm::PassBuilderOptLevel::O2,
            OptLevel::Aggressive => llvm::PassBuilderOptLevel::O3,
            OptLevel::Size => llvm::PassBuilderOptLevel::Os,
            OptLevel::SizeMin => llvm::PassBuilderOptLevel::Oz,
        }
    }

    use std::ffi::{c_char, c_void};

    pub unsafe extern "C" fn selfprofile_before_pass_callback(
        llvm_self_profiler: *mut c_void,
        pass_name: *const c_char,
        ir_name: *const c_char,
    ) {
    }

    pub unsafe extern "C" fn selfprofile_after_pass_callback(llvm_self_profiler: *mut c_void) {}

    let extra_passes = "".to_string();
    let llvm_plugins = "".to_string();

    llvm::LLVMRustOptimize(
        module.cg.llmod(),
        &module.cg.machine,
        to_pass_builder_opt_level(opt_level),
        opt_stage,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        false,
        None,
        ptr::null(),
        ptr::null(),
        false,
        ptr::null(),
        false,
        ptr::null(),
        false,
        ptr::null_mut(),
        selfprofile_before_pass_callback,
        selfprofile_after_pass_callback,
        extra_passes.as_ptr().cast(),
        extra_passes.len(),
        llvm_plugins.as_ptr().cast(),
        llvm_plugins.len(),
    )
    .into_result()
    .map_err(|()| llvm_err(handler, errors::LlvmError::RunLlvmPasses))
}

fn codegen(
    cgx: &CodegenContext<'_>,
    handler: &Handler,
    module: ModuleCodegen<ModuleLlvm>,
    config: &ModuleConfig,
) -> Result<CompiledModule, FatalError> {
    unsafe fn with_codegen<'ll, F, R>(
        tm: &'ll llvm::TargetMachine,
        llmod: &'ll llvm::Module,
        no_builtins: bool,
        f: F,
    ) -> R
    where
        F: FnOnce(&'ll mut llvm::PassManager<'ll>) -> R,
    {
        let cpm = llvm::LLVMCreatePassManager();
        llvm::LLVMAddAnalysisPasses(tm, cpm);
        llvm::LLVMRustAddLibraryInfo(cpm, llmod, no_builtins);
        f(cpm)
    }

    let llmod = module.cg.llmod();
    let llcx = &*module.cg.llcx;
    let tm = &*module.cg.machine;
    let module_name = Some(&module.name[..]);

    let obj_out = cgx.outputs.temp_path(OutputType::Object, module_name);

    // emit ir
    if config.emit_ir {
        use std::{ffi::c_char, io::Write, slice, str};

        extern "C" fn demangle_callback(
            input_ptr: *const c_char,
            input_len: usize,
            output_ptr: *mut c_char,
            output_len: usize,
        ) -> usize {
            let input = unsafe { slice::from_raw_parts(input_ptr as *const u8, input_len) };

            let Ok(input) = str::from_utf8(input) else { return 0 };

            let mut output =
                unsafe { slice::from_raw_parts_mut(output_ptr as *mut u8, output_len) };
            if write!(output, "{input}").is_err() {
                return 0;
            }

            input.len()
        }

        let out = cgx.outputs.temp_path(OutputType::LlvmAssembly, module_name);
        let out_c = path_to_c_string(&out);

        unsafe {
            llvm::LLVMRustPrintModule(llmod, out_c.as_ptr(), demangle_callback)
                .into_result()
                .map_err(|()| llvm_err(handler, errors::LlvmError::WriteIr { path: &out }))?;
        }
    }

    if let EmitObj::Object(_) = config.emit_obj {
        unsafe {
            with_codegen(tm, llmod, true, |cpm| {
                write_output_file(
                    handler,
                    tm,
                    cpm,
                    llmod,
                    &obj_out,
                    None,
                    llvm::FileType::ObjectFile,
                )
            })?;
        }
    }

    Ok(module.into_compiled_module(true, cgx.outputs))
}

fn path_to_c_string(p: &Path) -> CString {
    CString::new(p.to_str().unwrap()).unwrap()
}

pub fn write_output_file<'ll>(
    handler: &Handler,
    target: &'ll llvm::TargetMachine,
    pm: &llvm::PassManager<'ll>,
    m: &'ll llvm::Module,
    output: &Path,
    _dwo_output: Option<&Path>,
    file_type: llvm::FileType,
) -> Result<(), FatalError> {
    unsafe {
        let output_c = path_to_c_string(output);
        let result =
            llvm::LLVMRustWriteOutputFile(target, pm, m, output_c.as_ptr(), ptr::null(), file_type);

        result
            .into_result()
            .map_err(|()| llvm_err(handler, errors::LlvmError::WriteOutput { path: output }))
    }
}

pub struct LlvmBackend;

impl super::ssa::CodegenBackend for LlvmBackend {
    fn init(&self, sess: &Session) {
        util::init(sess);
    }

    fn codegen_module<'tcx>(
        &self,
        hix: Hx<'tcx>,
        mono_items: Vec<CodegenUnit<'tcx>>,
    ) -> Box<dyn Any> {
        Box::new(driver(hix, mono_items))
    }

    fn join_codegen(
        &self,
        sess: &Session,
        ongoing: Box<dyn Any>,
        outputs: &OutputFilenames,
    ) -> CodegenResults {
        ongoing.downcast::<OngoingCodegen>().unwrap().join(sess, outputs)
    }

    fn link_binary(
        &self,
        sess: &Session,
        codegen_results: CodegenResults,
        outputs: &OutputFilenames,
    ) {
        super::ssa::link::link_binary(sess, &codegen_results, outputs)
    }
}
