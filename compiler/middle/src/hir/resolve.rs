use {
    crate::{
        hir::{self, Vis},
        idx, mir,
        symbol::{kw, sym, Symbol},
        FxHashMap,
    },
    std::fmt,
};

idx::define_index! {
    pub struct ModId = u32;
}

impl fmt::Debug for ModId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "mod::{}", self.raw())
    }
}

pub const ROOT_MODULE: ModId = ModId::from_usize_unchecked(0);

#[derive(Debug)]
pub struct ModuleData {
    pub vis: Vis,
    pub name: Symbol,
    pub parent: Option<ModId>,
    pub defs: FxHashMap<Symbol, mir::DefId>,
    pub mods: FxHashMap<Symbol, ModId>,
    pub native_libs: Vec<Symbol>,
}

impl Default for ModuleData {
    fn default() -> Self {
        Self {
            parent: None,
            vis: Vis::Inherit,
            name: kw::Empty,
            defs: Default::default(),
            mods: Default::default(),
            native_libs: Default::default(),
        }
    }
}
