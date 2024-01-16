use {
    crate::{Diagnostic, DiagnosticBuilder, DiagnosticMessage, Emission, Handler, Level},
    std::marker::PhantomData,
};

#[derive(Copy, Clone, Debug)]
#[must_use]
pub struct FatalError;

pub struct FatalErrorMarker;

// Don't implement Send on FatalError. This makes it impossible to `panic_any!(FatalError)`.
// We don't want to invoke the panic handler and print a backtrace for fatal errors.
impl !Send for FatalError {}

impl FatalError {
    pub fn raise(self) -> ! {
        std::panic::resume_unwind(Box::new(FatalErrorMarker))
    }
}

impl<'a> DiagnosticBuilder<'a, FatalError> {
    pub(crate) fn new_almost_fatal(
        handler: &'a Handler,
        message: impl Into<DiagnosticMessage>,
    ) -> Self {
        Self {
            handler,
            diagnostic: Diagnostic::new_with_code(Level::Fatal, message.into()),
            _marker: PhantomData,
        }
    }
}

impl Emission for FatalError {
    fn emit_guarantee(db: DiagnosticBuilder<'_, Self>) -> Self {
        db.handler.emit_diagnostic(db.diagnostic);
        FatalError
    }

    fn make_guarantee(
        handler: &Handler,
        message: DiagnosticMessage,
    ) -> DiagnosticBuilder<'_, Self> {
        DiagnosticBuilder::new_almost_fatal(handler, message)
    }
}

impl std::fmt::Display for FatalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fatal error")
    }
}

impl std::error::Error for FatalError {}
