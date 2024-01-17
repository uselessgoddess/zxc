use errors::{ErrorGuaranteed, Handler, IntoDiagnostic};

#[derive(Debug)]
pub struct TyErrCtx {
    pub(crate) handler: Handler,
}

impl TyErrCtx {
    pub fn emit<'a>(&'a mut self, db: impl IntoDiagnostic<'a>) -> ErrorGuaranteed {
        db.into_diagnostic(&self.handler).emit()
    }

    pub fn abort_if_errors(&mut self) {
        let report = match self.handler.err_count() {
            0 => return,
            1 => "aborting due to previous error".into(),
            n => format!("aborting due to {n} previous errors"),
        };
        self.handler.struct_err(report).emit();
        self.handler.abort_if_errors()
    }
}
