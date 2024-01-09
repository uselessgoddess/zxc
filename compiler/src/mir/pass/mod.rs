mod dump;
mod simplify;

pub use {dump::emit_mir, simplify::Simplify};

use crate::{
    hir::HirCtx,
    mir::{self, Body, Terminator, START_BLOCK},
    Session, Tx,
};

pub trait MirPass<'tcx> {
    fn name(&self) -> &'static str {
        let name = std::any::type_name::<Self>();
        if let Some((_, tail)) = name.rsplit_once(':') { tail } else { name }
    }

    /// Returns `true` if this pass is enabled with the current combination of compiler flags.
    fn is_enabled(&self, _sess: &Session) -> bool {
        true
    }

    fn run_pass(&self, tcx: Tx<'tcx>, body: &mut Body<'tcx>);
}

impl<'tcx> HirCtx<'tcx> {
    pub fn optimized_mir(&self, def: mir::DefId) -> &Body<'tcx> {
        self.tcx.arena.body.alloc(self.inner_optimized_mir(def))
    }

    fn inner_optimized_mir(&self, def: mir::DefId) -> Body<'tcx> {
        let mut body = self.defs[def].clone();

        if let Terminator::Unreachable = body.basic_blocks[START_BLOCK].terminator()
            && body.basic_blocks[START_BLOCK].statements.is_empty()
        {
            return body;
        }

        run_optimization_passes(self.tcx, &mut body);

        body
    }
}

fn run_optimization_passes<'tcx>(tcx: Tx<'tcx>, body: &mut Body<'tcx>) {
    run_passes(tcx, body, &[&Simplify]);
}

pub fn should_run_pass<'tcx, P: MirPass<'tcx> + ?Sized>(tcx: Tx<'tcx>, pass: &P) -> bool {
    let name = pass.name();

    let overridden_passes = &tcx.sess.opts.C.mir_enable_passes;
    let overridden =
        overridden_passes.iter().rev().find(|(s, _)| s == &*name).map(|(_, polarity)| *polarity);
    overridden.unwrap_or_else(|| pass.is_enabled(&tcx.sess))
}

fn run_passes<'tcx>(tcx: Tx<'tcx>, body: &mut Body<'tcx>, passes: &[&dyn MirPass<'tcx>]) {
    for pass in passes {
        let _ = pass.name();

        if !should_run_pass(tcx, *pass) {
            continue;
        }

        pass.run_pass(tcx, body);
        body.pass_count += 1;
    }
}
