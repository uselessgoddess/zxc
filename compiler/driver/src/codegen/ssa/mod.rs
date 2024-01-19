use {
    middle::{hir::Hx, mir, sess::OutputFilenames, Session},
    std::any::Any,
};

pub(crate) mod errors;
pub mod link;

pub use link::{CodegenResults, CompiledModule, ModuleInfo};

pub trait CodegenBackend {
    fn init(&self, _sess: &Session) {}

    fn codegen_module<'tcx>(
        &self,
        hix: Hx<'tcx>,
        mono_items: &[mir::CodegenUnit<'tcx>],
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
