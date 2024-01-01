use {
    super::{Error, Result, Ty},
    crate::{
        mir::{self, InstanceData},
        symbol::Symbol,
        FxHashMap,
    },
    index_vec::IndexVec,
};

#[derive(Debug)]
pub struct Scope<'hir> {
    pub(crate) parent: Option<&'hir mut Self>,
    pub(crate) locals: FxHashMap<Symbol, (Ty<'hir>, mir::Place<'hir>)>,
}

impl<'hir> Scope<'hir> {
    pub fn new() -> Self {
        Self { parent: None, locals: Default::default() }
    }

    pub fn declare_var(&mut self, symbol: Symbol, ty: (Ty<'hir>, mir::Place<'hir>)) {
        self.locals.insert(symbol, ty);
    }

    pub fn get_var(&self, symbol: Symbol) -> Option<(Ty<'hir>, mir::Place<'hir>)> {
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
