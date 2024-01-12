use compiler::sess::{DiagnosticBuilder, Emission, Handler, IntoDiagnostic};

pub struct LinkerFileStem;

impl IntoDiagnostic<'_, !> for LinkerFileStem {
    fn into_diagnostic(self, handler: &Handler) -> DiagnosticBuilder<'_, !> {
        handler.struct_fatal("couldn't extract file stem from specified linker")
    }
}
