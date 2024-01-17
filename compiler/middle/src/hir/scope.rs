use {
    super::Ty,
    crate::{
        hir,
        mir::{self},
        FxHashMap, Symbol,
    },
    smallvec::SmallVec,
};

#[derive(Debug, Default, Clone)]
pub struct LoopData<'tcx> {
    pub breaks: SmallVec<(mir::BasicBlock, Option<mir::Operand<'tcx>>), 1>,
}

#[derive(Debug, Default)]
pub struct Scope<'hir> {
    pub parent: Option<&'hir mut Self>,

    pub returned: bool,
    pub looped: Option<LoopData<'hir>>,
    pub sig: Option<hir::FnSig<'hir>>,
    pub locals: FxHashMap<Symbol, (Ty<'hir>, Option<mir::Place<'hir>>)>,
}

impl<'hir> Scope<'hir> {
    pub fn new(sig: Option<hir::FnSig<'hir>>) -> Self {
        Self { sig, ..Default::default() }
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
