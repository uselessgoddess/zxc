use crate::{fx::FxHashMap, mir, symbol::Symbol};

pub type MonoItem<'tcx> = mir::Instance<'tcx>;

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

pub struct MonoItemData {
    pub inlined: bool,
    pub linkage: Linkage,
    pub visibility: Visibility,
}
