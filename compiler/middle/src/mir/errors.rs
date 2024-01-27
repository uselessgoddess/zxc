use errors::DiagnosticMessage;

pub struct LazyDiagnostic {
    pub msg: fn() -> DiagnosticMessage,
}
