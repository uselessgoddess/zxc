use crate::{
    hir,
    hir::{EntryFnType, HirCtx},
    mir,
};

pub fn post_typeck(hix: &mut HirCtx<'_>) {
    check_entry_fn(hix);
}

pub(crate) fn check_entry_fn(hix: &mut HirCtx<'_>) {
    match hix.entry_fn() {
        Some((def_id, EntryFnType::Main)) => check_main(hix, def_id),
        Some((def_id, EntryFnType::Start)) => check_start(hix, def_id),
        _ => {}
    }
}

fn check_main(hix: &mut HirCtx<'_>, def: mir::DefId) {
    let main = &hix.instances[def];

    if main.sig != hix.tcx.sigs.main {
        hix.err.emit(hir::errors::MismatchMainSig {
            expect: hix.sig_fmt(hix.tcx.sigs.main.inputs()),
            found: hix.sig_fmt(main.sig.inputs()),
            span: main.span,
        });
    }
}

fn check_start(hix: &mut HirCtx<'_>, def: mir::DefId) {
    let main = &hix.instances[def];

    if main.sig != hix.tcx.sigs.start {
        hix.err.emit(hir::errors::MismatchStartSig {
            expect: hix.sig_fmt(hix.tcx.sigs.main.inputs()),
            found: hix.sig_fmt(main.sig.inputs()),
            span: main.span,
        });
    }
}
