mod const_goto;
mod const_prop_lint;
mod copy_prop;
mod dump;
mod errors;
mod multiple_return;
mod normalize;
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

pub trait MirLint<'tcx> {
    fn name(&self) -> &'static str {
        let name = std::any::type_name::<Self>();
        if let Some((_, tail)) = name.rsplit_once(':') { tail } else { name }
    }

    /// Returns `true` if this lint is enabled with the current combination of compiler flags.
    fn is_enabled(&self, _sess: &Session) -> bool {
        true
    }

    fn run_lint(&self, tcx: Tx<'tcx>, body: &Body<'tcx>);
}

#[derive(Debug, Clone)]
pub struct Lint<T>(pub T);

impl<'tcx, T> MirPass<'tcx> for Lint<T>
where
    T: MirLint<'tcx>,
{
    fn name(&self) -> &'static str {
        self.0.name()
    }

    fn is_enabled(&self, sess: &Session) -> bool {
        self.0.is_enabled(sess)
    }

    fn run_pass(&self, tcx: Tx<'tcx>, body: &mut Body<'tcx>) {
        self.0.run_lint(tcx, body)
    }
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

        run_lowering_passes(self.tcx, &mut body);
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

fn run_lowering_passes<'tcx>(tcx: Tx<'tcx>, body: &mut Body<'tcx>) {
    run_required_passes(
        tcx,
        body,
        &[
            &Lint(const_prop_lint::ConstPropLint),
            &Lint(normalize::NormalizeLiteralsLint),
            &normalize::NormalizeLiterals,
            // todo...
        ],
    )
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
            &copy_prop::CopyProp,
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

fn run_required_passes<'tcx>(tcx: Tx<'tcx>, body: &mut Body<'tcx>, passes: &[&dyn MirPass<'tcx>]) {
    for pass in passes {
        pass.run_pass(tcx, body);
        body.pass_count += 1;
    }
}
