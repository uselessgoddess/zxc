use {
    crate::codegen::ssa::CodegenResults,
    middle::{hir::Hx, mir::CodegenUnit, sess::OutputFilenames, Session},
    std::any::Any,
};

mod util;

pub struct LlvmBackend;

impl super::ssa::CodegenBackend for LlvmBackend {
    fn init(&self, sess: &Session) {
        util::init(sess);
    }

    fn codegen_module<'tcx>(
        &self,
        hix: Hx<'tcx>,
        mono_items: &[CodegenUnit<'tcx>],
    ) -> Box<dyn Any> {
        todo!()
    }

    fn join_codegen(
        &self,
        sess: &Session,
        ongoing: Box<dyn Any>,
        _outputs: &OutputFilenames,
    ) -> CodegenResults {
        todo!()
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
