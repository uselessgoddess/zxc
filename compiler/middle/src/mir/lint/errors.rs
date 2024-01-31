use {
    errors::{DiagnosticBuilder, DiagnosticMessage},
    lexer::Span,
    lint::DecorateLint,
};

pub struct OverflowingLiterals<'a> {
    pub ty: &'a str,
    pub span: Span,
    pub lit: String,
    pub min: i128,
    pub max: u128,
}

impl<'a> DecorateLint<'a> for OverflowingLiterals<'_> {
    fn decorate_lint(self, diag: &mut DiagnosticBuilder<'a, ()>) {
        let Self { ty, span, lit, min, max } = self;
        diag.primary_span(None, span);
        diag.note(format!(
            "the literal `{lit}{ty}` does not fit `{ty}` whose range is `{min}..={max}`"
        ));
    }

    fn message(&self) -> DiagnosticMessage {
        format!("literal out of range for `{}`", self.ty).into()
    }
}
