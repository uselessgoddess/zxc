use crate::{hir::Hx, mir::DefId};

pub fn compute_symbol_name(hix: Hx<'_>, def: DefId) -> String {
    let name = hix.instances[def].symbol.to_string();

    // possible is foreign item
    if hix.defs.get(def).is_none() {
        return name;
    }

    // very simple mangling
    format!("_Z{name}")
}
