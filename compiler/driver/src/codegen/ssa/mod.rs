use {
    middle::{
        hir::Hx,
        mir,
        sess::{OutputFilenames, OutputType},
        Session,
    },
    std::any::Any,
};

mod common;
pub(crate) mod errors;
pub mod link;

pub use {
    common::TypeKind,
    link::{CodegenResults, CompiledModule, ModuleInfo},
};

#[derive(Debug)]
pub struct ModuleCodegen<M> {
    pub name: String,
    pub cg: M,
}

impl<M> ModuleCodegen<M> {
    pub fn into_compiled_module(self, emit_obj: bool, outputs: &OutputFilenames) -> CompiledModule {
        let object = emit_obj.then(|| outputs.temp_path(OutputType::Object, Some(&self.name)));

        CompiledModule { name: self.name, object }
    }
}

pub trait CodegenBackend {
    fn init(&self, _sess: &Session) {}

    fn codegen_module<'tcx>(
        &self,
        hix: Hx<'tcx>,
        mono_items: Vec<mir::CodegenUnit<'tcx>>,
    ) -> Box<dyn Any>;

    fn join_codegen(
        &self,
        sess: &Session,
        ongoing: Box<dyn Any>,
        outputs: &OutputFilenames,
    ) -> CodegenResults;

    fn link_binary(
        &self,
        sess: &Session,
        codegen_results: CodegenResults,
        outputs: &OutputFilenames,
    );
}

bitflags::bitflags! {
    #[derive(PartialEq)]
    pub struct MemFlags: u8 {
        const VOLATILE = 1 << 0;
        const NONTEMPORAL = 1 << 1;
        const UNALIGNED = 1 << 2;
    }
}
