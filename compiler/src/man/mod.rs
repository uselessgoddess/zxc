use crate::{hir::Hx, mir::DefId};

pub fn compute_symbol_name<'tcx>(hix: Hx<'tcx>, def: DefId) -> String {
    let name = hix.instances[def].symbol.to_string();

    // very simple mangling
    format!("_Z{name}")
}
