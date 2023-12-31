use {
    super::{Error, Result, Ty},
    crate::{mir, symbol::Symbol, FxHashMap},
};

pub struct Scope<'hir> {
    pub(crate) parent: Option<&'hir mut Self>,
    pub(crate) inner: FxHashMap<Symbol, (Ty<'hir>, mir::Place<'hir>)>,
}

impl<'hir> Scope<'hir> {
    pub fn new() -> Self {
        Self { parent: None, inner: FxHashMap::default() }
    }

    pub fn declare_var(&mut self, symbol: Symbol, ty: (Ty<'hir>, mir::Place<'hir>)) {
        self.inner.insert(symbol, ty);
    }

    pub fn get_var(&self, symbol: Symbol) -> Option<(Ty<'hir>, mir::Place<'hir>)> {
        match self.inner.get(&symbol) {
            Some(found) => Some(*found),
            None => {
                if let Some(parent) = &self.parent {
                    parent.get_var(symbol)
                } else {
                    None
                }
            }
        }
    }
}
