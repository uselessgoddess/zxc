use {
    super::simplify,
    crate::{
        mir::{visit::Visitor, BasicBlock, Body, MirPass, Statement, *},
        Session, Tx,
    },
};

pub struct ConstGoto;

impl<'tcx> MirPass<'tcx> for ConstGoto {
    fn is_enabled(&self, sess: &Session) -> bool {
        sess.mir_opt_level() >= 2
    }

    fn run_pass(&self, tcx: Tx<'tcx>, body: &mut Body<'tcx>) {
        let mut finder = ConstGotoOptimizationFinder { tcx, body, optimizations: vec![] };
        finder.visit_body(body);

        let should_simplify = !finder.optimizations.is_empty();

        for opt in finder.optimizations {
            let block = &mut body.basic_blocks[opt.bb_with_goto];
            block.statements.extend(opt.stmts_move_up);
            *block.terminator_mut() = Terminator::Goto { target: opt.target_to_use_in_goto };
        }

        if should_simplify {
            simplify::simplify_cfg(tcx, body);
            simplify::simplify_locals(tcx, body);
        }
    }
}

impl<'tcx> Visitor<'tcx> for ConstGotoOptimizationFinder<'_, 'tcx> {
    fn visit_terminator(&mut self, terminator: &Terminator<'tcx>, location: Location) {
        let _: Option<_> = try {
            let target = terminator.as_goto()?;

            // We only apply this optimization if the last statement is a const assignment
            let last_statement = self.body.basic_blocks[location.block].statements.last()?;

            if let (place, Rvalue::Use(Operand::Const(const_, ty))) = last_statement.as_assign()? {
                // We found a constant being assigned to `place`.
                // Now check that the target of this Goto switches on this place.
                let target_bb = &self.body.basic_blocks[target];

                // The `StorageDead(..)` statement does not affect the functionality of mir.
                // We can move this part of the statement up to the predecessor.
                let stmts_move_up = Vec::new();

                let target_bb_terminator = target_bb.terminator();
                let (discr, targets) = target_bb_terminator.as_switch()?;
                if discr.place() == Some(*place) {
                    let switch_ty = place.ty(self.tcx, &self.body.local_decls);
                    debug_assert_eq!(switch_ty, *ty);
                    // We now know that the Switch matches on the const place
                    // Now find which value in the Switch matches the const value.
                    let const_value = match const_ {
                        ConstValue::Scalar(scalar) => Some(scalar.data),
                        ConstValue::Zst => None,
                    }?;
                    let target_to_use_in_goto = targets.target_for_value(const_value);
                    self.optimizations.push(OptimizationToApply {
                        bb_with_goto: location.block,
                        target_to_use_in_goto,
                        stmts_move_up,
                    });
                }
            }
            Some(())
        };

        self.super_terminator(terminator, location);
    }
}

struct OptimizationToApply<'tcx> {
    bb_with_goto: BasicBlock,
    target_to_use_in_goto: BasicBlock,
    stmts_move_up: Vec<Statement<'tcx>>,
}

pub struct ConstGotoOptimizationFinder<'a, 'tcx> {
    tcx: Tx<'tcx>,
    body: &'a Body<'tcx>,
    optimizations: Vec<OptimizationToApply<'tcx>>,
}
