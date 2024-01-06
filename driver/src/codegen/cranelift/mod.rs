use {
    self::value::{CPlace, CValue, Pointer},
    compiler::{
        abi::{Abi, ArgAbi, Conv, FnAbi, Integer, PassMode, Scalar, TyAbi},
        hir::HirCtx,
        mir::{
            ty, BasicBlock, BasicBlockData, BinOp, Body, ConstValue, DefId, IntTy, Local, Operand,
            Place, Rvalue, Statement, Terminator, Ty,
        },
        Session, Tx,
    },
    cranelift::{
        codegen::ir::{Function, UserFuncName},
        prelude::{
            isa, types, AbiParam, Block, FunctionBuilder, FunctionBuilderContext, InstBuilder,
            MemFlags, Signature, StackSlotData, StackSlotKind, TrapCode, Value,
        },
    },
    cranelift_module::{FuncId, Linkage, Module},
    index_vec::IndexVec,
    lexer::UnOp,
    target_lexicon::PointerWidth,
};

mod abi;
mod cast;
mod num;
mod ssa;
mod value;

pub struct FunctionCx<'m, 'cl, 'tcx: 'm> {
    tcx: Tx<'tcx>,
    hix: &'cl HirCtx<'tcx>,
    mir: &'cl Body<'tcx>,
    bcx: FunctionBuilder<'cl>,

    block_map: IndexVec<BasicBlock, Block>,
    local_map: IndexVec<Local, CPlace<'tcx>>,

    target_config: isa::TargetFrontendConfig,
    ptr_type: types::Type,

    next_ssa: u32,
    module: &'m mut dyn Module,
}

impl<'m, 'cl, 'tcx: 'm> FunctionCx<'m, 'cl, 'tcx> {
    pub(crate) fn block(&self, bb: BasicBlock) -> Block {
        *self.block_map.get(bb).unwrap()
    }

    pub(crate) fn local_place(&mut self, local: Local) -> CPlace<'tcx> {
        *self.local_map.get(local).unwrap_or_else(|| {
            panic!("Local {:?} doesn't exist", local);
        })
    }

    pub fn next_ssa(&mut self) -> u32 {
        let ret = self.next_ssa;
        self.next_ssa += 1;
        ret
    }

    fn clif_type(&self, ty: Ty<'tcx>) -> Option<types::Type> {
        clif_type_from_ty(self.tcx, ty)
    }

    pub(crate) fn create_stack_slot(&mut self, size: u32, align: u32) -> Pointer {
        if align <= 16 {
            let stack_slot = self.bcx.create_sized_stack_slot(StackSlotData {
                kind: StackSlotKind::ExplicitSlot,
                // FIXME Don't force the size to a multiple of 16 bytes once Cranelift gets a way to
                //  specify stack slot alignment.
                size: (size + 15) / 16 * 16,
            });
            Pointer::stack_slot(stack_slot)
        } else {
            // Alignment is too big to handle using the above hack. Dynamically realign a stack slot
            // instead. This wastes some space for the realignment.
            let base_ptr = self.create_stack_slot(size + align, 16).get_addr(self);
            let misalign_offset = self.bcx.ins().urem_imm(base_ptr, i64::from(align));
            let realign_offset = self.bcx.ins().irsub_imm(misalign_offset, i64::from(align));
            Pointer::new(self.bcx.ins().iadd(base_ptr, realign_offset))
        }
    }
}

pub(crate) fn clif_type_from_ty<'tcx>(tcx: Tx<'tcx>, ty: Ty<'tcx>) -> Option<types::Type> {
    Some(match ty.kind() {
        ty::Bool => types::I8,
        ty::Int(size) => match size {
            IntTy::I8 => types::I8,
            IntTy::I16 => types::I16,
            IntTy::I32 => types::I32,
            IntTy::I64 => types::I64,
            IntTy::Isize => pointer_ty(tcx),
            _ => todo!(),
        },
        _ => return None,
    })
}

fn make_local_place<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    local: Local,
    abi: TyAbi<'tcx>,
    is_ssa: bool,
) -> CPlace<'tcx> {
    let place =
        if is_ssa { CPlace::new_var(fx, local, abi) } else { CPlace::new_stack_slot(fx, abi) };

    place
}

pub(crate) fn type_sign(ty: Ty<'_>) -> bool {
    match ty.kind() {
        ty::Bool => false,
        ty::Int(..) => true,
        _ => todo!(),
    }
}

pub(crate) fn pointer_ty(tcx: Tx<'_>) -> types::Type {
    // match tcx.data_layout.pointer_size.bits() {
    //     16 => types::I16,
    //     32 => types::I32,
    //     64 => types::I64,
    //     bits => todo!("unknown bits: {bits}"),
    // }
    types::I64
}

pub(crate) fn scalar_to_clif(tcx: Tx<'_>, scalar: Scalar) -> types::Type {
    match scalar {
        Scalar::Int(int, _sign) => match int {
            Integer::I8 => types::I8,
            Integer::I16 => types::I16,
            Integer::I32 => types::I32,
            Integer::I64 => types::I64,
        },
        Scalar::F32 => types::F32,
        Scalar::F64 => types::F64,
        Scalar::Pointer => pointer_ty(tcx),
    }
}

pub fn codegen_binop<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    bin_op: BinOp,
    lhs: CValue<'tcx>,
    rhs: CValue<'tcx>,
) -> CValue<'tcx> {
    if let BinOp::Eq | BinOp::Lt | BinOp::Le | BinOp::Ne | BinOp::Ge | BinOp::Gt = bin_op
        && let ty::Bool | ty::Int(_) = lhs.layout().ty.kind()
    {
        let signed = type_sign(lhs.layout().ty);
        let lhs = lhs.load_scalar(fx);
        let rhs = rhs.load_scalar(fx);

        let int_cc = num::bin_op_to_int_cc(bin_op, signed).unwrap();
        let val = fx.bcx.ins().icmp(int_cc, lhs, rhs);
        return CValue::by_val(val, fx.tcx.layout_of(fx.tcx.types.bool));
    }

    match lhs.layout().ty.kind() {
        ty::Bool => num::codegen_bool_binop(fx, bin_op, lhs, rhs),
        ty::Int(_) => num::codegen_int_binop(fx, bin_op, lhs, rhs),
        _ => unreachable!("{:?}({:?}, {:?})", bin_op, lhs.layout().ty, rhs.layout().ty),
    }
}

pub(crate) fn codegen_const_value<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    const_val: ConstValue,
    ty: Ty<'tcx>,
) -> CValue<'tcx> {
    let abi = fx.tcx.layout_of(ty);

    if abi.layout.is_zst() {
        return CValue::by_ref(Pointer::dangling(abi.layout.align), abi);
    }

    match const_val {
        ConstValue::Zst => unreachable!(), // we already handled ZST above
        ConstValue::Scalar(int) => {
            if fx.clif_type(abi.ty).is_some() {
                return CValue::const_val(fx, abi, int);
            } else {
                let raw_val = int.size().truncate(int.to_bits(int.size()).unwrap());
                let val = match int.size().bytes() {
                    1 => fx.bcx.ins().iconst(types::I8, raw_val as i64),
                    2 => fx.bcx.ins().iconst(types::I16, raw_val as i64),
                    4 => fx.bcx.ins().iconst(types::I32, raw_val as i64),
                    8 => fx.bcx.ins().iconst(types::I64, raw_val as i64),
                    16 => {
                        let lsb = fx.bcx.ins().iconst(types::I64, raw_val as u64 as i64);
                        let msb = fx.bcx.ins().iconst(types::I64, (raw_val >> 64) as u64 as i64);
                        fx.bcx.ins().iconcat(lsb, msb)
                    }
                    _ => unreachable!(),
                };

                // FIXME avoid this extra copy to the stack and directly write to the final
                // destination
                let place = CPlace::new_stack_slot(fx, abi);
                place.to_ptr().store(fx, val, MemFlags::trusted());
                place.to_cvalue(fx)
            }
        }
    }
}

pub(crate) fn codegen_place<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    place: Place<'tcx>,
) -> CPlace<'tcx> {
    assert!(place.projection.is_empty());

    fx.local_place(place.local)
}

pub(crate) fn codegen_operand<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    operand: Operand<'tcx>,
) -> CValue<'tcx> {
    match operand {
        Operand::Copy(place) => {
            let place = codegen_place(fx, place);
            place.to_cvalue(fx)
        }
        Operand::Const(const_, ty) => codegen_const_value(fx, const_, ty),
    }
}

fn codegen_stmt<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    #[allow(unused_variables)] cur_block: Block,
    stmt: &Statement<'tcx>,
) {
    match stmt {
        Statement::Assign(place, rvalue) => {
            let place = codegen_place(fx, *place);
            let dst_ty = place.layout();
            match *rvalue {
                Rvalue::Use(operand) => {
                    let val = codegen_operand(fx, operand);
                    place.write_cvalue(fx, val);
                }
                Rvalue::UseDeref(_) => todo!(),
                Rvalue::BinaryOp(op, lhs, rhs) => {
                    let lhs = codegen_operand(fx, lhs);
                    let rhs = codegen_operand(fx, rhs);

                    let val = codegen_binop(fx, op, lhs, rhs);
                    place.write_cvalue(fx, val);
                }
                Rvalue::UnaryOp(op, operand) => {
                    let operand = codegen_operand(fx, operand);
                    let layout = operand.layout();
                    let val = operand.load_scalar(fx);
                    let res = match op {
                        UnOp::Not(_) => match layout.ty.kind() {
                            ty::Int(_) => CValue::by_val(fx.bcx.ins().bnot(val), layout),
                            _ => unreachable!("un op Not for {:?}", layout.ty),
                        },
                        UnOp::Neg(_) => match layout.ty.kind() {
                            ty::Int(_) => CValue::by_val(fx.bcx.ins().ineg(val), layout),
                            _ => unreachable!("un op Neg for {:?}", layout.ty),
                        },
                    };
                    place.write_cvalue(fx, res);
                }
                Rvalue::Cast(kind, operand, cast_ty) => {
                    let operand = codegen_operand(fx, operand);
                    let from_ty = operand.layout().ty;

                    // TODO: only supports ints
                    let clif_ty = fx.clif_type(cast_ty).unwrap();
                    let from = operand.load_scalar(fx);
                    let val = cast::clif_intcast(fx, from, clif_ty, type_sign(from_ty));
                    place.write_cvalue(fx, CValue::by_val(val, dst_ty))
                }
            }
        }
        Statement::Nop => {}
    }
}

fn codegen_block<'tcx>(fx: &mut FunctionCx<'_, '_, 'tcx>, abi: FnAbi<'tcx>) {
    for (bb, bb_data @ BasicBlockData { statements, .. }) in fx.mir.basic_blocks.iter_enumerated() {
        let block = fx.block(bb);
        fx.bcx.switch_to_block(block);

        for stmt in statements {
            codegen_stmt(fx, block, stmt);
        }

        match *bb_data.terminator() {
            Terminator::Goto { target } => {
                let block = fx.block(target);
                fx.bcx.ins().jump(block, &[]);
            }
            Terminator::Return => codegen_fn_return(fx, abi.ret),
            Terminator::Unreachable => {
                fx.bcx.ins().trap(TrapCode::UnreachableCodeReached);
            }
            Terminator::Call { func, ref args, dest, target, .. } => {
                abi::codegen_terminator_call(fx, func, args, dest, target)
            }
            Terminator::SwitchInt { discr, ref targets } => {
                let discr = codegen_operand(fx, discr);
                let discr = discr.load_scalar(fx);

                if let Some((test, then, otherwise)) = targets.as_static_if() {
                    let then_block = fx.block(then);
                    let else_block = fx.block(otherwise);

                    let test_zero = match test {
                        0 => true,
                        1 => false,
                        _ => unreachable!("{targets:?}"),
                    };

                    if test_zero {
                        fx.bcx.ins().brif(discr, else_block, &[], then_block, &[]);
                    } else {
                        fx.bcx.ins().brif(discr, then_block, &[], else_block, &[]);
                    }
                } else {
                    todo!("complex switches")
                }
            }
        }
    }
}

fn codegen_fn_return<'tcx>(fx: &mut FunctionCx<'_, '_, 'tcx>, ret: ArgAbi<'tcx>) {
    match ret.mode {
        PassMode::Ignore => {
            fx.bcx.ins().return_(&[]);
        }
        PassMode::Direct => {
            let place = fx.local_place(Local::RETURN_PLACE);
            let ret_val = place.to_cvalue(fx).load_scalar(fx);
            fx.bcx.ins().return_(&[ret_val]);
        }
    }
}

pub(super) fn value_for_param<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    // local: Option<Local>,
    // local_field: Option<usize>,
    arg_abi: ArgAbi<'tcx>,
    block_params_iter: &mut impl Iterator<Item = Value>,
) -> Option<CValue<'tcx>> {
    let block_param = param_from_abi(fx.tcx, arg_abi).map(|abi_param| {
        let block_param = block_params_iter.next().unwrap();
        assert_eq!(fx.bcx.func.dfg.value_type(block_param), abi_param.value_type);
        block_param
    });

    match arg_abi.mode {
        PassMode::Ignore => None,
        PassMode::Direct => Some(CValue::by_val(block_param.unwrap(), arg_abi.ty)),
    }
}

fn param_from_abi<'tcx>(tcx: Tx<'tcx>, abi: ArgAbi<'tcx>) -> Option<AbiParam> {
    match abi.mode {
        PassMode::Ignore => None,
        PassMode::Direct => Some(match abi.ty.layout.abi {
            Abi::Scalar(scalar) => AbiParam::new(scalar_to_clif(tcx, scalar)),
            Abi::Aggregate => todo!(),
        }),
    }
}

fn conv_to_call_conv(_: &Session, _: Conv, default_call_conv: isa::CallConv) -> isa::CallConv {
    default_call_conv
}

fn sig_from_abi<'tcx>(tcx: Tx<'tcx>, default: isa::CallConv, abi: &FnAbi<'tcx>) -> Signature {
    let call_conv = conv_to_call_conv(tcx.sess, abi.conv, default);
    Signature {
        params: abi.args.iter().flat_map(|&arg| param_from_abi(tcx, arg)).collect(),
        returns: Vec::from_iter(param_from_abi(tcx, abi.ret)),
        call_conv,
    }
}

pub(crate) fn compile_fn<'tcx>(
    tcx: Tx<'tcx>,
    hix: &HirCtx<'tcx>,
    def: DefId,
    mir: &Body<'tcx>,
    module: &mut dyn Module,
) -> (FuncId, Function) {
    let fn_abi = tcx.fn_abi_of_sig(hix.instances[def].sig);
    let mut fn_ctx = FunctionBuilderContext::new();

    let sig = sig_from_abi(tcx, module.target_config().default_call_conv, &fn_abi);
    let id = module
        .declare_function(
            hix.instances[def].symbol.as_str(),
            if hix.instances[def].sig.abi == ty::Abi::Zxc {
                Linkage::Local
            } else {
                Linkage::Export
            },
            &sig,
        )
        .expect("lmao bro, fake mir generation");
    let mut fn_ = Function::with_name_signature(UserFuncName::user(0, id.as_u32()), sig);

    let mut bcx = FunctionBuilder::new(&mut fn_, &mut fn_ctx);

    let start_block = bcx.create_block();
    let block_map: IndexVec<BasicBlock, Block> =
        (0..mir.basic_blocks.len()).map(|_| bcx.create_block()).collect();

    let target = isa::TargetFrontendConfig {
        default_call_conv: isa::CallConv::SystemV,
        pointer_width: PointerWidth::U64,
    };
    let mut fx = FunctionCx {
        tcx,
        hix,
        bcx,
        mir,
        block_map,
        target_config: target,
        local_map: IndexVec::with_capacity(mir.local_decls.len()),
        ptr_type: types::I64,
        next_ssa: 0,
        module,
    };

    let fn_sig = hix.instances[def].sig;
    {
        fx.bcx.append_block_params_for_function_params(start_block);
        fx.bcx.switch_to_block(start_block);
        fx.bcx.ins().nop();

        let fx = &mut fx;
        let ssa_analyzed = ssa::analyze(fx);

        #[allow(clippy::unnecessary_to_owned)]
        let mut block_params_iter = fx.bcx.func.dfg.block_params(start_block).to_vec().into_iter();
        let func_params = fx
            .mir
            .args_iter()
            .zip(fn_abi.args.iter())
            .map(|(local, &abi)| {
                (
                    local,
                    value_for_param(fx, abi, &mut block_params_iter),
                    fx.mir.local_decls[local].ty,
                )
            })
            .collect::<Vec<_>>();

        let ret = fn_sig.output();
        let ret = make_local_place(
            fx,
            Local::RETURN_PLACE,
            fx.tcx.layout_of(ret),
            ssa_analyzed[Local::RETURN_PLACE].is_ssa(fx, ret),
        );
        assert_eq!(fx.local_map.push(ret), Local::RETURN_PLACE);

        for (local, arg, ty) in func_params {
            if let Some(arg) = arg
                && let Some(addr) = arg.try_to_ptr()
            {
                let place = CPlace::for_ptr(addr, arg.layout());
                assert_eq!(fx.local_map.push(place), local);

                continue;
            }

            let is_ssa = ssa_analyzed[local].is_ssa(fx, ty);
            let place = make_local_place(fx, local, fx.tcx.layout_of(ty), is_ssa);
            assert_eq!(fx.local_map.push(place), local);

            if let Some(param) = arg {
                place.write_cvalue(fx, param);
            }
        }

        for local in fx.mir.vars_and_temps_iter() {
            let ty = fx.mir.local_decls[local].ty;
            let place = make_local_place(
                fx,
                local,
                fx.tcx.layout_of(ty),
                ssa_analyzed[local].is_ssa(fx, ty),
            );
            assert_eq!(fx.local_map.push(place), local);
        }

        let start = fx.block(BasicBlock::START_BLOCK);
        fx.bcx.ins().jump(start, &[]);
    }

    codegen_block(&mut fx, fn_abi);

    fx.bcx.seal_all_blocks();
    fx.bcx.finalize();

    (id, fn_)
}
