use errors::{DiagnosticBuilder, ErrorGuaranteed, Handler, IntoDiagnostic};

#[derive(Debug)]
pub struct TyErrCtx {
    pub(crate) handler: Handler,
}

impl TyErrCtx {
    pub fn emit<'a>(&'a mut self, db: impl IntoDiagnostic<'a>) -> ErrorGuaranteed {
        db.into_diagnostic(&self.handler).emit()
    }

    pub fn struct_diag<'a>(
        &'a mut self,
        db: impl IntoDiagnostic<'a>,
    ) -> DiagnosticBuilder<'a, ErrorGuaranteed> {
        db.into_diagnostic(&self.handler)
    }

    pub fn abort_if_errors(&mut self) {
        self.handler.abort_if_errors()
    }
}
