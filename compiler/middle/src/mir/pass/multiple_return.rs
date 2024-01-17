use crate::{
    idx::BitSet,
    mir::{pass::simplify, Body, MirPass, Terminator},
    Session, Tx,
};

pub struct MultipleReturnTerminators;

impl<'tcx> MirPass<'tcx> for MultipleReturnTerminators {
    fn is_enabled(&self, sess: &Session) -> bool {
        sess.mir_opt_level() >= 4
    }

    fn run_pass(&self, _: Tx<'tcx>, body: &mut Body<'tcx>) {
        // find basic blocks with no statement and a return terminator
        let mut bbs_simple_returns = BitSet::new_empty(body.basic_blocks.len());
        let bbs = &mut body.basic_blocks;
        for idx in bbs.indices() {
            if bbs[idx].statements.is_empty()
                && let Terminator::Return = bbs[idx].terminator()
            {
                bbs_simple_returns.insert(idx);
            }
        }

        for bb in bbs {
            if let Terminator::Goto { target } = *bb.terminator() {
                if bbs_simple_returns.contains(target) {
                    *bb.terminator_mut() = Terminator::Return;
                }
            }
        }

        simplify::remove_dead_blocks(body)
    }
}