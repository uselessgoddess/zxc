use {
    super::simplify,
    crate::{
        mir::{BinOp, Body, LocalDecls, MirPass, Operand, Rvalue, Statement, UnOp},
        Session, Tx,
    },
};

pub struct PeepSimplify;

impl<'tcx> MirPass<'tcx> for PeepSimplify {
    fn is_enabled(&self, sess: &Session) -> bool {
        sess.mir_opt_level() > 0
    }

    fn run_pass(&self, tcx: Tx<'tcx>, body: &mut Body<'tcx>) {
        let ctx = SimplifyContext { tcx, local_decls: &body.local_decls };

        for block in &mut body.basic_blocks {
            for statement in block.statements.iter_mut() {
                match statement {
                    Statement::Assign(_, rvalue) => {
                        ctx.simplify_bool_cmp(rvalue);
                        ctx.simplify_cast(rvalue);
                    }
                    _ => {}
                }
            }
            simplify::simplify_duplicate_switch_targets(block.terminator_mut());
        }
    }
}

struct SimplifyContext<'tcx, 'l> {
    tcx: Tx<'tcx>,
    local_decls: &'l LocalDecls<'tcx>,
}

impl<'tcx> SimplifyContext<'tcx, '_> {
    fn try_eval_bool(&self, x: &Operand<'_>) -> Option<bool> {
        if let Some((x, ty)) = x.constant()
            && ty.is_bool()
        {
            x.try_to_bool()
        } else {
            None
        }
    }

    fn simplify_bool_cmp(&self, rvalue: &mut Rvalue<'tcx>) {
        match rvalue {
            Rvalue::BinaryOp(op @ (BinOp::Eq | BinOp::Ne), a, b) => {
                let new = match (op, self.try_eval_bool(a), self.try_eval_bool(b)) {
                    // Transform "Eq(a, true)" ==> "a"
                    (BinOp::Eq, _, Some(true)) => Some(Rvalue::Use(a.clone())),

                    // Transform "Ne(a, false)" ==> "a"
                    (BinOp::Ne, _, Some(false)) => Some(Rvalue::Use(a.clone())),

                    // Transform "Eq(true, b)" ==> "b"
                    (BinOp::Eq, Some(true), _) => Some(Rvalue::Use(b.clone())),

                    // Transform "Ne(false, b)" ==> "b"
                    (BinOp::Ne, Some(false), _) => Some(Rvalue::Use(b.clone())),

                    // Transform "Eq(false, b)" ==> "Not(b)"
                    (BinOp::Eq, Some(false), _) => Some(Rvalue::UnaryOp(UnOp::Not, b.clone())),

                    // Transform "Ne(true, b)" ==> "Not(b)"
                    (BinOp::Ne, Some(true), _) => Some(Rvalue::UnaryOp(UnOp::Not, b.clone())),

                    // Transform "Eq(a, false)" ==> "Not(a)"
                    (BinOp::Eq, _, Some(false)) => Some(Rvalue::UnaryOp(UnOp::Not, a.clone())),

                    // Transform "Ne(a, true)" ==> "Not(a)"
                    (BinOp::Ne, _, Some(true)) => Some(Rvalue::UnaryOp(UnOp::Not, a.clone())),

                    _ => None,
                };

                if let Some(new) = new {
                    *rvalue = new;
                }
            }
            _ => {}
        }
    }

    fn simplify_cast(&self, rvalue: &mut Rvalue<'tcx>) {
        if let Rvalue::Cast(kind, operand, cast_ty) = rvalue {
            if operand.ty(self.local_decls, self.tcx) == *cast_ty {
                *rvalue = Rvalue::Use(operand.clone());
            }
            // else if let Some((ConstValue::Scalar(scalar), _)) = operand.constant()
            //     && *kind == CastKind::IntToInt
            // {
            //     debug_assert!(cast_ty.is_integer());
            //
            //     let size = self.tcx.layout_of(*cast_ty).layout.size;
            //     *rvalue = Rvalue::Use(Operand::Const(
            //         (ConstValue::Scalar(ScalarRepr {
            //             data: size.reduce(scalar.data, cast_ty.is_signed()),
            //             size: NonZeroU8::new(size.bytes() as u8).unwrap(),
            //         })),
            //         *cast_ty,
            //     ))
            // }
        }
    }
}
