use {
    self::value::{CPlace, CValue, Pointer},
    crate::codegen::ssa::CodegenResults,
    cranelift::{
        codegen::{
            ir::{Function, UserFuncName},
            CodegenError, Context,
        },
        frontend,
        prelude::{
            isa,
            settings::{self, SetError},
            types, AbiParam, Block, Configurable, FunctionBuilder, FunctionBuilderContext,
            InstBuilder, IntCC, MemFlags, Signature, StackSlotData, StackSlotKind, TrapCode, Value,
        },
    },
    cranelift_module::{FuncId, Linkage, Module, ModuleError},
    middle::{
        abi::{Abi, ArgAbi, Conv, FnAbi, Integer, PassMode, Scalar, TyAbi},
        mir::{
            ty, BasicBlock, BasicBlockData, BinOp, Body, CodegenUnit, ConstValue, DefId, IntTy,
            Local, Operand, Place, Rvalue, StatementKind, TerminatorKind, Ty, UnOp,
        },
        sess::{OptLevel, OutputFilenames},
        IndexVec, Session, Tx,
    },
    std::{any::Any, fs::File, sync::Arc},
    target_lexicon::PointerWidth,
};

mod abi;
mod cast;
mod num;
mod pretty;
mod shim;
mod ssa;
mod value;

pub struct FunctionCx<'m, 'cl, 'tcx: 'm> {
    tcx: Tx<'tcx>,
    hix: Hx<'tcx>,
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
        },
        ty::Ref(..) | ty::Ptr(..) => pointer_ty(tcx),
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
        ty::Bool | ty::Ref(..) | ty::Ptr(..) => false,
        ty::Int(..) => true,
        _ => todo!(),
    }
}

pub(crate) fn pointer_ty(tcx: Tx<'_>) -> types::Type {
    match tcx.sess.target.pointer_width {
        16 => types::I16,
        32 => types::I32,
        64 => types::I64,
        bits => todo!("unknown bits: {bits}"),
    }
}

pub(crate) fn scalar_to_clif(tcx: Tx<'_>, scalar: Scalar) -> types::Type {
    match scalar.primitive() {
        Primitive::Int(int, _sign) => match int {
            Integer::I8 => types::I8,
            Integer::I16 => types::I16,
            Integer::I32 => types::I32,
            Integer::I64 => types::I64,
        },
        Primitive::F32 => types::F32,
        Primitive::F64 => types::F64,
        Primitive::Pointer => pointer_ty(tcx),
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
        ty::Ptr(..) => num::codegen_ptr_binop(fx, bin_op, lhs, rhs),
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
    let mut cplace = fx.local_place(place.local);

    for elem in place.projection {
        match elem {
            PlaceElem::Deref => {
                cplace = cplace.place_deref(fx);
            }
            PlaceElem::Subtype(_) => todo!(),
        }
    }

    cplace
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
    match stmt.kind {
        StatementKind::Assign(place, rvalue) => {
            let lvalue = codegen_place(fx, place);
            let dst_ty = lvalue.layout();
            match rvalue {
                Rvalue::Use(operand) => {
                    let val = codegen_operand(fx, operand);
                    lvalue.write_cvalue(fx, val);
                }
                Rvalue::Ref(_, place) | Rvalue::AddrOf(_, place) => {
                    let place = codegen_place(fx, place);
                    let ref_ = place.place_ref(fx, lvalue.layout());
                    lvalue.write_cvalue(fx, ref_);
                }
                Rvalue::UseDeref(_) => todo!(),
                Rvalue::BinaryOp(op, lhs, rhs) => {
                    let lhs = codegen_operand(fx, lhs);
                    let rhs = codegen_operand(fx, rhs);

                    let val = codegen_binop(fx, op, lhs, rhs);
                    lvalue.write_cvalue(fx, val);
                }
                Rvalue::UnaryOp(op, operand) => {
                    let operand = codegen_operand(fx, operand);
                    let layout = operand.layout();
                    let val = operand.load_scalar(fx);
                    let res = match op {
                        UnOp::Not => match layout.ty.kind() {
                            ty::Int(_) => CValue::by_val(fx.bcx.ins().bnot(val), layout),
                            ty::Bool => {
                                let res = fx.bcx.ins().icmp_imm(IntCC::Equal, val, 0);
                                CValue::by_val(res, layout)
                            }
                            _ => unreachable!("un op Not for {:?}", layout.ty),
                        },
                        UnOp::Neg => match layout.ty.kind() {
                            ty::Int(_) => CValue::by_val(fx.bcx.ins().ineg(val), layout),
                            _ => unreachable!("un op Neg for {:?}", layout.ty),
                        },
                    };
                    lvalue.write_cvalue(fx, res);
                }
                Rvalue::Cast(_kind, operand, cast_ty) => {
                    let operand = codegen_operand(fx, operand);
                    let from_ty = operand.layout().ty;

                    // TODO: only supports ints
                    let clif_ty = fx.clif_type(cast_ty).unwrap();
                    let from = operand.load_scalar(fx);
                    let val = cast::clif_intcast(fx, from, clif_ty, type_sign(from_ty));
                    lvalue.write_cvalue(fx, CValue::by_val(val, dst_ty))
                }
            }
        }
        StatementKind::Nop => {}
    }
}

fn codegen_block<'tcx>(fx: &mut FunctionCx<'_, '_, 'tcx>, abi: FnAbi<'tcx>) {
    for (bb, bb_data @ BasicBlockData { statements, .. }) in fx.mir.basic_blocks.iter_enumerated() {
        let block = fx.block(bb);
        fx.bcx.switch_to_block(block);

        for stmt in statements {
            codegen_stmt(fx, block, stmt);
        }

        match bb_data.terminator().kind {
            TerminatorKind::Goto { target } => {
                let block = fx.block(target);
                fx.bcx.ins().jump(block, &[]);
            }
            TerminatorKind::Return => codegen_fn_return(fx, abi.ret),
            TerminatorKind::Unreachable => {
                fx.bcx.ins().trap(TrapCode::UnreachableCodeReached);
            }
            TerminatorKind::Call { func, ref args, dest, target, .. } => {
                abi::codegen_terminator_call(fx, func, args, dest, target)
            }
            TerminatorKind::SwitchInt { discr, ref targets } => {
                let discr = codegen_operand(fx, discr);
                let (discr, switch_ty) = (discr.load_scalar(fx), discr.layout().ty);

                if let Some((test, then, otherwise)) = targets.as_static_if()
                    && switch_ty == fx.tcx.types.bool
                {
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
                    let mut switch = frontend::Switch::new();
                    for (value, block) in targets.iter() {
                        switch.set_entry(value, fx.block(block));
                    }
                    let otherwise = fx.block(targets.otherwise());
                    switch.emit(&mut fx.bcx, discr, otherwise);
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
            Abi::Uninhabited => todo!(),
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

pub(crate) fn codegen_fn(
    hix: Hx,
    def: DefId,
    _mono_item: &MonoItemData,
    module: &mut dyn Module,
) -> (Symbol, FuncId, Function) {
    let tcx = hix.tcx;
    let fn_abi = tcx.fn_abi_of_sig(hix.instances[def].sig);
    let mut fn_ctx = FunctionBuilderContext::new();

    let sig = sig_from_abi(tcx, module.target_config().default_call_conv, &fn_abi);
    let id = module
        .declare_function(
            hix.symbol_name(def).name,
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

    let mir = hix.assume_optimized_mir(def);
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

    (hix.instances[def].symbol, id, fn_)
}

use super::ssa::{CompiledModule, ModuleInfo};

struct CodegenModule {
    regular: CompiledModule,
}

pub(crate) struct CodegenData {
    modules: Vec<Result<CodegenModule, String>>,
    info: ModuleInfo,
}

impl CodegenData {
    fn join(self, sess: &Session) -> CodegenResults {
        let mut modules = Vec::with_capacity(32);
        for module in self.modules {
            match module {
                Ok(module) => modules.push(module.regular),
                Err(err) => sess.fatal(err),
            }
        }

        sess.abort_if_errors();

        CodegenResults { modules, info: self.info }
    }
}

fn build_isa(sess: &Session) -> Result<Arc<dyn isa::TargetIsa + 'static>, SetError> {
    let target_triple: target_lexicon::Triple = match sess.target.triple.parse() {
        Ok(triple) => triple,
        Err(err) => sess.fatal(format!("target not recognized: {}", err)),
    };

    let mut flags = settings::builder();
    flags.enable("is_pic")?;
    flags.set("enable_llvm_abi_extensions", "true")?;

    match sess.opts.C.opt_level {
        OptLevel::No => {
            flags.set("opt_level", "none")?;
        }
        OptLevel::Less | OptLevel::Default => {}
        OptLevel::Size | OptLevel::SizeMin | OptLevel::Aggressive => {
            flags.set("opt_level", "speed_and_size")?;
        }
    }

    if let target_lexicon::Architecture::Aarch64(_)
    | target_lexicon::Architecture::Riscv64(_)
    | target_lexicon::Architecture::X86_64 = target_triple.architecture
    {
        // Windows depends on stack probes to grow the committed part of the stack.
        // On other platforms it helps prevents stack smashing.
        flags.enable("enable_probestack")?;
        flags.set("probestack_strategy", "inline")?;
    } else {
        // __cranelift_probestack is not provided and inline stack probes are only supported on
        // AArch64, Riscv64 and x86_64.
        flags.set("enable_probestack", "false")?;
    }

    let flags = settings::Flags::new(flags);

    let isa_builder = match None {
        Some("native") => cranelift_native::builder_with_options(true).unwrap(),
        Some(value) => {
            let mut builder = isa::lookup(target_triple.clone()).unwrap_or_else(|err| {
                sess.fatal(format!("can't compile for {target_triple}: {err}"));
            });
            if builder.enable(value).is_err() {
                sess.fatal("the specified target cpu isn't currently supported by Cranelift.");
            }
            builder
        }
        None => {
            let mut builder = isa::lookup(target_triple.clone()).unwrap_or_else(|err| {
                sess.fatal(format!("can't compile for {target_triple}: {err}"));
            });
            if target_triple.architecture == target_lexicon::Architecture::X86_64 {
                // Don't use "haswell" as the default, as it implies `has_lzcnt`.
                // macOS CI is still at Ivy Bridge EP, so `lzcnt` is interpreted as `bsr`.
                builder.enable("nehalem").unwrap();
            }
            builder
        }
    };

    isa_builder.finish(flags).map_err(|err| sess.fatal(format!("failed to build TargetIsa: {err}")))
}

use {
    cranelift_object::{ObjectBuilder, ObjectModule},
    middle::{
        abi::Primitive,
        hir::Hx,
        mir::{InstanceDef, MonoItem, MonoItemData, PlaceElem, Statement},
        sess::OutputType,
        symbol::Symbol,
    },
};

fn make_module(sess: &Session, name: String) -> ObjectModule {
    let isa = build_isa(sess).expect("unreachable error");

    let mut builder =
        ObjectBuilder::new(isa, name + ".o", cranelift_module::default_libcall_names()).unwrap();
    builder.per_function_section(false);
    ObjectModule::new(builder)
}

fn emit_module(
    name: String,
    object: cranelift_object::object::write::Object<'_>,
    output_filenames: &OutputFilenames,
) -> Result<CompiledModule, String> {
    let tmp_file = output_filenames.temp_path(OutputType::Object, Some(&name));
    let mut file = match File::create(&tmp_file) {
        Ok(file) => file,
        Err(err) => return Err(format!("error creating object file: {}", err)),
    };
    if let Err(err) = object.write_stream(&mut file) {
        return Err(format!("error writing object file: {}", err));
    }

    Ok(CompiledModule { name, object: Some(tmp_file) })
}

fn emit_cgu(
    name: String,
    module: ObjectModule,
    output_filenames: &OutputFilenames,
) -> Result<CodegenModule, String> {
    let product = module.finish();

    Ok(CodegenModule { regular: emit_module(name, product.object, output_filenames)? })
}

fn module_codegen<'tcx>(hix: Hx<'tcx>, cgu: &CodegenUnit<'tcx>) -> Result<CodegenModule, String> {
    let mut module = make_module(hix.tcx.sess, cgu.name.as_str().to_string());

    let mut cg_functions = Vec::with_capacity(32);
    for (&MonoItem { def: InstanceDef::Item(def), .. }, mono_item) in &cgu.items {
        cg_functions.push(codegen_fn(hix, def, mono_item, &mut module));
    }

    shim::maybe_create_entry_wrapper(hix, &mut module);

    let mut ctx = Context::new();
    for (symbol, id, func) in cg_functions {
        if hix.tcx.sess.opts.output_types.contains_key(&OutputType::LlvmAssembly) {
            pretty::write_clif_file(
                hix.tcx.output_filenames(),
                symbol.as_str(),
                None,
                module.isa(),
                &func,
            );
        }

        ctx.clear();
        ctx.func = func;

        match module.define_function(id, &mut ctx) {
            Err(ModuleError::Compilation(CodegenError::ImplLimitExceeded)) => {
                hix.tcx.sess.fatal(format!(
                    "backend implementation limit exceeded while compiling {symbol}",
                ));
            }
            Err(err) => panic!("Error while defining {symbol}: {err:?}"),
            _ => {}
        }

        if hix.tcx.sess.opts.output_types.contains_key(&OutputType::LlvmAssembly) {
            pretty::write_clif_file(
                hix.tcx.output_filenames(),
                symbol.as_str(),
                Some("opt"),
                module.isa(),
                &ctx.func,
            );
        }
    }

    let cgu_name = cgu.name.as_str().to_owned();
    emit_cgu(cgu_name, module, hix.tcx.output_filenames())
}

fn driver<'tcx>(hix: Hx<'tcx>, cgus: &[CodegenUnit<'tcx>]) -> CodegenData {
    let [unit] = cgus else { todo!() };

    let target_cpu = hix.tcx.sess.target.cpu.to_string();
    let module = module_codegen(hix, unit);

    CodegenData { modules: vec![module], info: ModuleInfo::new(hix.tcx, target_cpu) }
}

pub struct CraneliftBackend;

impl super::ssa::CodegenBackend for CraneliftBackend {
    fn codegen_module<'tcx>(
        &self,
        hix: Hx<'tcx>,
        mono_items: Vec<CodegenUnit<'tcx>>,
    ) -> Box<dyn Any> {
        Box::new(driver(hix, &mono_items))
    }

    fn join_codegen(
        &self,
        sess: &Session,
        ongoing: Box<dyn Any>,
        _outputs: &OutputFilenames,
    ) -> CodegenResults {
        ongoing.downcast::<CodegenData>().unwrap().join(sess)
    }

    fn link_binary(
        &self,
        sess: &Session,
        codegen_results: CodegenResults,
        outputs: &OutputFilenames,
    ) {
        super::ssa::link::link_binary(sess, &codegen_results, outputs)
    }
}
