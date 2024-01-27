use {
    crate::mir::AssertKind,
    errors::{DiagnosticBuilder, DiagnosticMessage},
    lexer::Span,
    lint::{DecorateLint, Lint},
    std::fmt,
};

pub enum AssertLint<P> {
    ArithmeticOverflow(Span, AssertKind<P>),
}

impl<P> AssertLint<P> {
    pub fn lint(&self) -> &'static Lint {
        match *self {
            AssertLint::ArithmeticOverflow(..) => lint::builtin::arithmetic_overflow,
        }
    }
    pub fn span(&self) -> Span {
        match *self {
            AssertLint::ArithmeticOverflow(s, _) => s,
        }
    }
    pub fn kind(self) -> AssertKind<P> {
        match self {
            AssertLint::ArithmeticOverflow(_, p) => p,
        }
    }
}

impl<'a, P: fmt::Debug> DecorateLint<'a> for AssertLint<P> {
    fn decorate_lint(self, diag: &mut DiagnosticBuilder<'a>) {
        let span = self.span();
        let message = match self.kind() {
            AssertKind::Overflow(op, left, right) => {
                let op = op.as_str();
                format!("attempt to compute `{left:#?} {op} {right:#?}`, which would overflow")
            }
        };
        diag.span_label(span, message);
    }

    fn message(&self) -> DiagnosticMessage {
        match self {
            AssertLint::ArithmeticOverflow(..) => "this arithmetic operation will overflow",
        }
        .into()
    }
}
