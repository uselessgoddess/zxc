use {
    super::Ty,
    crate::{
        hir,
        mir::{self},
        FxHashMap, Symbol,
    },
};

#[derive(Debug)]
pub struct Scope<'hir> {
    pub(crate) parent: Option<&'hir mut Self>,

    pub(crate) returned: bool,
    pub(crate) sig: Option<hir::FnSig<'hir>>,
    pub(crate) locals: FxHashMap<Symbol, (Ty<'hir>, Option<mir::Place<'hir>>)>,
}

impl<'hir> Scope<'hir> {
    pub fn new(sig: Option<hir::FnSig<'hir>>) -> Self {
        Self { parent: None, returned: false, sig, locals: Default::default() }
    }

    pub fn was_returned(&self) -> bool {
        self.returned
    }

    pub fn declare_var(&mut self, symbol: Symbol, ty: (Ty<'hir>, Option<mir::Place<'hir>>)) {
        self.locals.insert(symbol, ty);
    }

    pub fn get_var(&self, symbol: Symbol) -> Option<(Ty<'hir>, Option<mir::Place<'hir>>)> {
        match self.locals.get(&symbol) {
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
