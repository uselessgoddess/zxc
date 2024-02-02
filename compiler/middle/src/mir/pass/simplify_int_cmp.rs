use crate::{
    mir::{
        BasicBlock, BinOp, Body, MirPass, Operand, Place, Rvalue, ScalarRepr, StatementKind,
        SwitchTargets, TerminatorKind, Ty,
    },
    Session, Tx,
};

pub struct SimplifyIntegralCond;

impl<'tcx> MirPass<'tcx> for SimplifyIntegralCond {
    fn is_enabled(&self, sess: &Session) -> bool {
        sess.mir_opt_level() > 0
    }

    fn run_pass(&self, tcx: Tx<'tcx>, body: &mut Body<'tcx>) {
        let opts = OptimizationFinder { body }.find_optimizations();

        for OptInfo { op, cond, bb, branch: (scalar, ty), switch_on, mut targets } in opts {
            let bb = &mut body.basic_blocks.as_mut()[bb];
            let new_branch = { scalar.assert_bits(tcx.layout_of(ty).layout.size) };

            let first_is_zero = targets.values[0] == 0;
            if let (BinOp::Eq, true) | (BinOp::Ne, false) = (op, first_is_zero) {
                targets.targets.swap(0, 1)
            }

            bb.statements[cond].make_nop();

            let [cond, otherwise] = match targets.targets[..] {
                [a, b] => [a, b],
                ref e => panic!("expected 2 switch targets, got: {e:?}"),
            };

            let targets = SwitchTargets::static_if(new_branch, cond, otherwise);
            bb.terminator_mut().kind =
                TerminatorKind::SwitchInt { discr: Operand::Copy(switch_on), targets };
        }
    }
}

struct OptimizationFinder<'a, 'tcx> {
    body: &'a Body<'tcx>,
}

fn branch_info<'tcx>(
    lhs: Operand<'tcx>,
    rhs: Operand<'tcx>,
) -> Option<(ScalarRepr, Ty<'tcx>, Place<'tcx>)> {
    use Operand::{Const, Copy};

    if let (Const(branch, ty), Copy(switch_on)) | (Copy(switch_on), Const(branch, ty)) = (lhs, rhs)
        && ty.is_integer()
    {
        Some((branch.try_to_scalar()?, ty, switch_on))
    } else {
        None
    }
}

impl<'tcx> OptimizationFinder<'_, 'tcx> {
    fn find_optimizations(&self) -> Vec<OptInfo<'tcx>> {
        self.body
            .basic_blocks
            .iter_enumerated()
            .filter_map(|(bb, block)| {
                let TerminatorKind::SwitchInt { discr: Operand::Copy(switch_on), ref targets } =
                    block.terminator().kind
                else {
                    return None;
                };
                block.statements.iter().enumerate().rev().find_map(|(stmt_idx, stmt)| {
                    if let StatementKind::Assign(lhs, rhs) = stmt.kind
                        && lhs == switch_on
                        && let Rvalue::BinaryOp(op @ (BinOp::Eq | BinOp::Ne), lhs, rhs) = rhs
                    {
                        let (scalar, ty, switch_on) = branch_info(lhs, rhs)?;
                        Some(OptInfo {
                            bb,
                            op,
                            cond: stmt_idx,
                            switch_on,
                            branch: (scalar, ty),
                            targets: targets.clone(),
                        })
                    } else {
                        None
                    }
                })
            })
            .collect()
    }
}

#[derive(Debug)]
struct OptInfo<'tcx> {
    op: BinOp,
    cond: usize,
    bb: BasicBlock,
    branch: (ScalarRepr, Ty<'tcx>),
    switch_on: Place<'tcx>,
    targets: SwitchTargets,
}
