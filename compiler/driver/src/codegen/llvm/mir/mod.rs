mod analyze;
mod block;
mod num;

use {
    super::{base, Bx, CodegenCx, FunctionCx},
    crate::codegen::llvm::{LPlace, LValue, LValueRepr},
    middle::{
        abi::{PassMode, TyAbi},
        mir::{
            self, ty, BasicBlockData, CastKind, Instance, InstanceDef, Operand, PlaceElem, Rvalue,
            Statement, StatementKind, TerminatorKind, Ty, UnOp,
        },
        BitSet,
    },
    std::iter,
};

impl<'a, 'll, 'tcx> FunctionCx<'a, 'll, 'tcx> {
    fn probe_direct_consume(&mut self, place: mir::Place<'tcx>) -> Option<LValue<'ll, 'tcx>> {
        match self.locals[place.local] {
            LLocal::Value(val) => {
                // Moves out of scalar and scalar pair fields are trivial.
                for _ in place.projection {
                    panic!();
                }

                Some(val)
            }
            LLocal::Pending => {
                panic!("use of {place:?} before def");
            }
            LLocal::Place(..) => None,
        }
    }

    pub fn codegen_consume(&mut self, place: mir::Place<'tcx>) -> LValue<'ll, 'tcx> {
        let tcx = self.cx.tcx;

        let ty = place.ty(tcx, &self.mir.local_decls);
        let layout = tcx.layout_of(ty);

        if layout.is_zst() {
            return LValue::zst(layout);
        }

        if let Some(probe) = self.probe_direct_consume(place) {
            return probe;
        }
        // else operand is place
        let place = codegen_place(self, place);
        place.load_lvalue(self)
    }
}

pub enum Cached<T> {
    /// Not yet created.
    None,

    /// Has been created.
    Some(T),

    /// Nothing created yet, and nothing should be.
    Skip,
}

#[derive(Debug, Copy, Clone)]
pub enum LLocal<'ll, 'tcx> {
    Place(LPlace<'ll, 'tcx>),
    Value(LValue<'ll, 'tcx>),
    Pending,
}

impl<'ll, 'tcx> LLocal<'ll, 'tcx> {
    fn zst_or_pending(layout: TyAbi<'tcx>) -> Self {
        if layout.is_zst() { Self::Value(LValue::zst(layout)) } else { Self::Pending }
    }
}

fn codegen_place<'ll, 'tcx>(
    fx: &mut FunctionCx<'_, 'll, 'tcx>,
    place: mir::Place<'tcx>,
) -> LPlace<'ll, 'tcx> {
    let mut lplace = match fx.local_place(place.local) {
        LLocal::Place(place) => place,
        LLocal::Value(_) => {
            panic!("using operand local {place:?} as place")
        }
        LLocal::Pending => {
            panic!("using still-pending operand local {place:?} as place")
        }
    };

    for elem in place.projection {
        match elem {
            PlaceElem::Deref => {
                lplace = lplace.load_lvalue(fx).deref(fx);
            }
            PlaceElem::Subtype(_) => todo!(),
        }
    }

    lplace
}

fn codegen_operand<'ll, 'tcx>(
    fx: &mut FunctionCx<'_, 'll, 'tcx>,
    operand: Operand<'tcx>,
) -> LValue<'ll, 'tcx> {
    match operand {
        Operand::Copy(place) => fx.codegen_consume(place),
        Operand::Const(const_, ty) => LValue::from_const(fx, const_, ty),
    }
}

fn is_produce_operand<'tcx>(fx: &FunctionCx<'_, '_, 'tcx>, rvalue: Rvalue<'tcx>) -> bool {
    true
}

fn codegen_rvalue<'ll, 'tcx>(
    fx: &mut FunctionCx<'_, 'll, 'tcx>,
    place: LPlace<'ll, 'tcx>,
    rvalue: Rvalue<'tcx>,
) {
    assert!(is_produce_operand(fx, rvalue));

    let temp = codegen_rvalue_operand(fx, rvalue);
    place.store_lvalue(fx, temp);
}

fn codegen_rvalue_operand<'ll, 'tcx>(
    fx: &mut FunctionCx<'_, 'll, 'tcx>,
    rvalue: Rvalue<'tcx>,
) -> LValue<'ll, 'tcx> {
    let tcx = fx.cx.tcx;

    match rvalue {
        Rvalue::Use(operand) => codegen_operand(fx, operand),
        Rvalue::UseDeref(_) => todo!(),
        Rvalue::Ref(mutbl, place) => {
            let place = codegen_place(fx, place);
            let ty = place.layout.ty;
            LValue {
                repr: LValueRepr::ByVal(place.llval),
                layout: tcx.layout_of(Ty::new(tcx, ty::Ref(mutbl, ty))),
            }
        }
        Rvalue::UnaryOp(op, operand) => {
            let operand = codegen_operand(fx, operand);
            let llval = operand.load_scalar();
            LValue::by_val(
                match op {
                    UnOp::Not => fx.bcx.not(llval),
                    UnOp::Neg => fx.bcx.neg(llval),
                },
                operand.layout,
            )
        }
        Rvalue::BinaryOp(op, lhs, rhs) => {
            let lhs = codegen_operand(fx, lhs);
            let rhs = codegen_operand(fx, rhs);
            let (lty, rty) = (lhs.layout.ty, rhs.layout.ty);
            let llval = match (lhs.repr, rhs.repr) {
                (LValueRepr::ByVal(lhs_val), LValueRepr::ByVal(rhs_val)) => {
                    num::codegen_int_binop(&mut fx.bcx, op, lhs_val, rhs_val, lty)
                }
                _ => unreachable!(),
            };
            LValue {
                repr: LValueRepr::ByVal(llval),
                layout: fx.hix.tcx.layout_of(op.ty(fx.hix.tcx, lty, rty)),
            }
        }
        Rvalue::Cast(kind, src, dst) => {
            let src = codegen_operand(fx, src);
            let dst = fx.cx.tcx.layout_of(dst);

            let val = match kind {
                CastKind::IntToInt => {
                    let lldst = fx.cx.immediate_type_of(dst.layout);
                    fx.bcx.intcast(src.load_scalar(), lldst, src.layout.ty.is_signed())
                }
            };
            LValue::by_val(val, dst)
        }
    }
}

fn codegen_stmt<'tcx>(fx: &mut FunctionCx<'_, '_, 'tcx>, stmt: &Statement<'tcx>) {
    match stmt.kind {
        StatementKind::Assign(dest, rvalue) => {
            if let Some(local) = dest.as_local() {
                match fx.locals[local] {
                    LLocal::Place(place) => codegen_rvalue(fx, place, rvalue),
                    LLocal::Pending => {
                        let operand = codegen_rvalue_operand(fx, rvalue);
                        fx.locals[local] = LLocal::Value(operand);
                    }
                    LLocal::Value(operand) => {
                        if !operand.layout.is_zst() {
                            panic!("operand {rvalue:?} already assigned")
                        }
                        codegen_rvalue_operand(fx, rvalue);
                    }
                }
            } else {
                let place = codegen_place(fx, dest);
                codegen_rvalue(fx, place, rvalue);
            }
        }
        StatementKind::Nop => {}
    }
}

fn codegen_return_terminator(fx: &mut FunctionCx<'_, '_, '_>) {
    if fx.fn_abi.ret.ty.layout.is_uninhabited() {
        fx.bcx.abort();
        fx.bcx.unreachable();
        return;
    }

    let llval = match &fx.fn_abi.ret.mode {
        PassMode::Ignore => {
            fx.bcx.ret_void();
            return;
        }
        PassMode::Direct => fx.codegen_consume(mir::Place::return_place()).load_scalar(),
    };
    fx.bcx.ret(llval);
}

fn codegen_block<'tcx>(fx: &mut FunctionCx<'_, '_, 'tcx>, bb: mir::BasicBlock) {
    let bb_data @ BasicBlockData { statements, .. } = &fx.mir.basic_blocks[bb];
    let block = fx.block(bb); // init block
    fx.bcx.switch_to_block(block);

    for stmt in &bb_data.statements {
        codegen_stmt(fx, stmt);
    }

    match bb_data.terminator().kind {
        TerminatorKind::Unreachable => fx.bcx.unreachable(),
        TerminatorKind::Goto { target } => {
            let block = fx.block(target);
            fx.bcx.br(block);
        }
        TerminatorKind::Return => codegen_return_terminator(fx),
        TerminatorKind::SwitchInt { discr, ref targets } => {
            block::codegen_switch_terminator(fx, discr, targets)
        }
        TerminatorKind::Call { func, ref args, dest, target, fn_span } => {
            block::codegen_call_terminator(fx, func, args, dest, target)
        }
        _ => todo!(),
    }
}

fn arg_local_refs<'ll, 'tcx>(
    fx: &mut FunctionCx<'_, 'll, 'tcx>,
    locals: &BitSet<mir::Local>,
) -> Vec<LLocal<'ll, 'tcx>> {
    let mir = fx.mir;
    let mut llarg_idx = 0;

    mir.args_iter()
        .enumerate()
        .map(|(idx, local)| {
            let arg = fx.fn_abi.args[idx];

            if !locals.contains(local) {
                LLocal::Value(match arg.mode {
                    PassMode::Ignore => LValue::zst(arg.ty),
                    PassMode::Direct => {
                        let llarg = fx.bcx.get_param(llarg_idx);
                        llarg_idx += 1;
                        LValue::by_val(llarg, arg.ty)
                    }
                })
            } else {
                let tmp = LPlace::alloca(fx, arg.ty);
                fx.bcx.store_fn_arg(&mut llarg_idx, arg, tmp);
                LLocal::Place(tmp)
            }
        })
        .collect::<Vec<_>>()
}

pub fn codegen_mir<'tcx>(cx: &mut CodegenCx<'_, 'tcx>, instance: Instance<'tcx>) {
    let hix = cx.hix;

    let InstanceDef::Item(def) = instance.def;
    let llfn = base::get_fn(cx, instance);
    let mir = hix.assume_optimized_mir(def);

    let mut bcx = Bx::build(cx, llfn);
    let start_llbb = bcx.append_block("start");
    bcx.switch_to_block(start_llbb);

    let cached = mir
        .basic_blocks
        .indices()
        .map(|bb| if bb == mir::START_BLOCK { Cached::Some(start_llbb) } else { Cached::None })
        .collect();

    let fn_abi = &cx.tcx.fn_abi_of_sig(hix.instances[def].sig);
    let mut fx = FunctionCx {
        bcx,
        cx,
        hix: cx.hix,
        mir,
        fn_abi,
        func: llfn,
        blocks: cached,
        locals: Default::default(),
    };

    let non_ssa = analyze::non_ssa(&fx);
    let local_values = {
        let args = arg_local_refs(&mut fx, &non_ssa);

        let mut allocate_local = |local| {
            let decl = &mir.local_decls[local];
            let layout = fx.hix.tcx.layout_of(decl.ty);

            if non_ssa.contains(local) {
                if layout.is_sized() {
                    LLocal::Place(LPlace::alloca(&mut fx, layout))
                } else {
                    unreachable!()
                }
            } else {
                LLocal::zst_or_pending(layout)
            }
        };

        let ret = allocate_local(mir::RETURN_PLACE);
        iter::once(ret)
            .chain(args.into_iter())
            .chain(mir.vars_and_temps_iter().map(allocate_local))
            .collect()
    };
    fx.locals = local_values;

    for (bb, _) in mir.basic_blocks.iter_enumerated() {
        codegen_block(&mut fx, bb);
    }
}
