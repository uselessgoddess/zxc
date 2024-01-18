use {
    crate::{mir, Span},
    std::ops::Deref,
};

#[derive(Debug)]
pub struct Ty<'hir> {
    pub kind: mir::Ty<'hir>,
    pub span: Span,
}

impl<'hir> Ty<'hir> {
    pub fn map<F>(mut self, f: F) -> Self
    where
        F: FnOnce(mir::Ty<'hir>) -> mir::Ty<'hir>,
    {
        self.kind = f(self.kind);
        self
    }
}

impl Clone for Ty<'_> {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for Ty<'_> {}

impl<'hir> Ty<'hir> {
    pub fn new(span: Span, kind: mir::Ty<'hir>) -> Self {
        Self { kind, span }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }
}

impl PartialEq for Ty<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.kind.eq(&other.kind)
    }
}

impl Eq for Ty<'_> {}

impl<'hir> Deref for Ty<'hir> {
    type Target = mir::Ty<'hir>;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}
