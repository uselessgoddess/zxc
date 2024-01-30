use crate::{
    mir::{
        visit::MutVisitor, Body, Location, MirPass, Operand, Place, Rvalue, Statement,
        StatementKind,
    },
    Session, Tx,
};

pub struct CopyProp;

impl<'tcx> MirPass<'tcx> for CopyProp {
    fn is_enabled(&self, sess: &Session) -> bool {
        sess.mir_opt_level() >= 1
    }

    fn run_pass(&self, tcx: Tx<'tcx>, body: &mut Body<'tcx>) {
        Replacer { tcx }.visit_body_preserves_cfg(body)
    }
}

pub struct Replacer<'tcx> {
    tcx: Tx<'tcx>,
}

impl<'tcx> MutVisitor<'tcx> for Replacer<'tcx> {
    fn tcx(&self) -> Tx<'tcx> {
        self.tcx
    }

    fn visit_statement(&mut self, statement: &mut Statement<'tcx>, location: Location) {
        self.super_statement(statement, location);

        if let StatementKind::Assign(lhs, rvalue) = statement.kind
            && let Rvalue::Use(Operand::Copy(rhs)) | Rvalue::UseDeref(rhs) = rvalue
            && lhs == rhs
        {
            statement.make_nop();
        }
    }
}
