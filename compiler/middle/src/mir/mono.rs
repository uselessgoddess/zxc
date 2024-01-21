use crate::{
    fx::FxHashMap,
    hir::Hx,
    mir::{self, InstanceDef, SymbolName},
    symbol::Symbol,
};

pub type MonoItem<'tcx> = mir::Instance<'tcx>;

impl<'tcx> MonoItem<'tcx> {
    pub fn symbol_name(&self, hix: Hx<'tcx>) -> SymbolName<'tcx> {
        match self.def {
            InstanceDef::Item(instance) => hix.symbol_name(instance),
        }
    }
}

pub struct CodegenUnit<'tcx> {
    pub name: Symbol,
    pub items: FxHashMap<MonoItem<'tcx>, MonoItemData>,
}

impl<'tcx> CodegenUnit<'tcx> {}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Linkage {
    External,
    AvailableExternally,
    LinkOnceAny,
    LinkOnceODR,
    WeakAny,
    WeakODR,
    Appending,
    Internal,
    Private,
    ExternalWeak,
    Common,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Visibility {
    Default,
    Hidden,
}

#[derive(Debug, Copy, Clone)]
pub struct MonoItemData {
    pub inlined: bool,
    pub linkage: Linkage,
    pub visibility: Visibility,
}
