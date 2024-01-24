mod const_goto;
mod dump;
mod multiple_return;
mod peep_simplify;
mod simplify;
mod simplify_branches;
mod simplify_int_cmp;

pub use dump::emit_mir;

use crate::{
    hir::HirCtx,
    mir::{self, Body, TerminatorKind, START_BLOCK},
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
    pub fn assume_optimized_mir(&self, def: mir::DefId) -> &Body<'tcx> {
        &self.defs[def]
    }

    pub fn optimized_mir(&self, def: mir::DefId) -> Body<'tcx> {
        self.inner_optimized_mir(def)
    }

    fn inner_optimized_mir(&self, def: mir::DefId) -> Body<'tcx> {
        let mut body = self.defs[def].clone();

        if let TerminatorKind::Unreachable = body.basic_blocks[START_BLOCK].terminator().kind
            && body.basic_blocks[START_BLOCK].statements.is_empty()
        {
            return body;
        }

        run_optimization_passes(self.tcx, &mut body);

        body
    }
}

pub struct WithMinOptLevel<T>(pub u32, pub T);

impl<'tcx, T> MirPass<'tcx> for WithMinOptLevel<T>
where
    T: MirPass<'tcx>,
{
    fn name(&self) -> &'static str {
        self.1.name()
    }

    fn is_enabled(&self, sess: &Session) -> bool {
        sess.mir_opt_level() >= self.0 as usize
    }

    fn run_pass(&self, tcx: Tx<'tcx>, body: &mut Body<'tcx>) {
        self.1.run_pass(tcx, body)
    }
}

fn run_optimization_passes<'tcx>(tcx: Tx<'tcx>, body: &mut Body<'tcx>) {
    fn o1<T>(x: T) -> WithMinOptLevel<T> {
        WithMinOptLevel(1, x)
    }

    run_passes(
        tcx,
        body,
        &[
            &o1(simplify::SimplifyCfg::EarlyOpt),
            &multiple_return::MultipleReturnTerminators,
            &peep_simplify::PeepSimplify,
            &const_goto::ConstGoto,
            &simplify_branches::SimplifyConstCondition::AfterConstProp,
            &simplify::SimplifyLocals::AfterGVN,
            &simplify_int_cmp::SimplifyIntegralCond,
            &simplify_branches::SimplifyConstCondition::Final,
            &simplify::SimplifyLocals::Final,
            &o1(simplify::SimplifyCfg::Final),
        ],
    );
}

pub fn should_run_pass<'tcx, P: MirPass<'tcx> + ?Sized>(tcx: Tx<'tcx>, pass: &P) -> bool {
    let name = pass.name();

    let overridden_passes = &tcx.sess.opts.C.mir_enable_passes;
    let overridden =
        overridden_passes.iter().rev().find(|(s, _)| s == name).map(|(_, polarity)| *polarity);
    overridden.unwrap_or_else(|| pass.is_enabled(tcx.sess))
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
