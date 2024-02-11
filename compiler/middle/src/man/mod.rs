use crate::{hir::Hx, mir::DefId};

pub fn compute_symbol_name(hix: Hx<'_>, def: DefId) -> String {
    let name = hix.instances[def].symbol.to_string();

    // possible is foreign item
    if hix.defs.get(def).is_none() {
        return name;
    }

    let mut mods = Vec::new();

    let mut root = Some(hix.instances[def].parent);
    while let Some(mod_id) = root
        && let name = hix.mods[mod_id].name.as_str()
    {
        mods.push(name);
        root = hix.mods[mod_id].parent;
    }

    let mods =
        mods.into_iter().rev().fold(String::new(), |buf, seg| buf + &format!("{}{seg}", seg.len()));
    // very simple mangling
    format!("_Z{mods}{name}")
}
