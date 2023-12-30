mod cast;
mod ssa;

use {
    crate::{
        codegen::{
            abi::{Abi, Align, PassMode, Size, TyAbi},
            list::List,
            mir::{
                codegen::cast::clif_intcast, BasicBlock, BasicBlockData, Body, ConstValue, Local,
                LocalDecl, Operand, Place, Rvalue, ScalarRepr, Statement, Terminator,
            },
            scalar_to_clif, ty, Arena, Session, Tx, Ty, TyCtx, TyKind,
        },
        parse::{BinOp, UnOp},
    },
    cranelift::{
        codegen::{
            self,
            ir::{immediates::Offset32, Function, StackSlot, UserFuncName},
            isa::lookup_by_name,
            settings,
        },
        prelude::{
            isa, types, Block, FunctionBuilder, FunctionBuilderContext, InstBuilder, MemFlags,
            Signature, StackSlotData, StackSlotKind, TrapCode, Type, Value, Variable,
        },
    },
    index_vec::IndexVec,
    std::marker::PhantomData,
    target_lexicon::PointerWidth,
};

pub struct FunctionCx<'m, 'cl, 'tcx: 'm> {
    tcx: Tx<'tcx>,
    bcx: FunctionBuilder<'cl>,
    mir: &'tcx Body<'tcx>,

    block_map: IndexVec<BasicBlock, Block>,
    local_map: IndexVec<Local, CPlace<'tcx>>,

    target_config: isa::TargetFrontendConfig,
    ptr_type: types::Type,

    next_ssa: u32,
    _marker: PhantomData<&'m ()>,
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
        ty::clif_type_from_ty(self.tcx, ty)
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
        ty::Int(..) => true,
        _ => todo!(),
    }
}

pub fn codegen_int_binop<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    bin_op: BinOp,
    lhs: CValue<'tcx>,
    rhs: CValue<'tcx>,
) -> CValue<'tcx> {
    assert_eq!(lhs.layout().ty, rhs.layout().ty, "int binop requires lhs and rhs of same type");

    let signed = type_sign(lhs.layout().ty);
    let layout = lhs.layout();
    let lhs = lhs.load_scalar(fx);
    let rhs = rhs.load_scalar(fx);
    let b = fx.bcx.ins();
    // FIXME trap on overflow for the Unchecked versions
    let val = match bin_op {
        BinOp::Add(_) => b.iadd(lhs, rhs),
        BinOp::Sub(_) => b.isub(lhs, rhs),
        BinOp::Mul(_) => b.imul(lhs, rhs),
        BinOp::Div(_) => {
            if signed {
                b.sdiv(lhs, rhs)
            } else {
                b.udiv(lhs, rhs)
            }
        }
        _ => todo!(),
    };
    CValue::by_val(val, layout)
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
    operand: &Operand<'tcx>,
) -> CValue<'tcx> {
    match *operand {
        Operand::Copy(ref place) => {
            let place = codegen_place(fx, *place);
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
                Rvalue::Use(ref operand) => {
                    let val = codegen_operand(fx, operand);
                    place.write_cvalue(fx, val);
                }
                Rvalue::UseDeref(_) => todo!(),
                Rvalue::BinaryOp(op, ref lhs, ref rhs) => {
                    let lhs = codegen_operand(fx, lhs);
                    let rhs = codegen_operand(fx, rhs);

                    let val = codegen_int_binop(fx, op, lhs, rhs);
                    place.write_cvalue(fx, val);
                }
                Rvalue::UnaryOp(op, ref operand) => {
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
                Rvalue::Cast(kind, ref operand, cast_ty) => {
                    let operand = codegen_operand(fx, operand);
                    let from_ty = operand.layout().ty;

                    // TODO: only supports ints
                    let clif_ty = fx.clif_type(cast_ty).unwrap();
                    let from = operand.load_scalar(fx);
                    let val = clif_intcast(fx, from, clif_ty, type_sign(from_ty));
                    place.write_cvalue(fx, CValue::by_val(val, dst_ty))
                }
            }
        }
        Statement::Nop => {}
    }
}

fn codegen_block(fx: &mut FunctionCx<'_, '_, '_>, start: Block) {
    for (bb, bb_data @ BasicBlockData { statements, .. }) in fx.mir.basic_blocks.iter_enumerated() {
        let block = fx.block(bb);
        fx.bcx.switch_to_block(block);

        for stmt in statements {
            codegen_stmt(fx, block, stmt);
        }

        match bb_data.terminator() {
            Terminator::Goto { target } => {
                let block = fx.block(target);
                fx.bcx.ins().jump(block, &[]);
            }
            Terminator::Return => codegen_fn_return(fx),
            Terminator::Unreachable => {
                fx.bcx.ins().trap(TrapCode::UnreachableCodeReached);
            }
        }
    }
}

fn codegen_fn_return(fx: &mut FunctionCx<'_, '_, '_>) {
    match PassMode::Ignore {
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

pub(crate) fn compile_fn<'tcx>(tcx: Tx<'tcx>, mir: &'tcx Body<'tcx>) {
    let mut fn_ctx = FunctionBuilderContext::new();
    let mut fn_ = Function::with_name_signature(
        UserFuncName::user(0, 0),
        Signature::new(isa::CallConv::Fast),
    );

    let mut bcx = FunctionBuilder::new(&mut fn_, &mut fn_ctx);

    let start_block = bcx.create_block();
    let block_map: IndexVec<BasicBlock, Block> =
        (0..mir.basic_blocks.len()).map(|_| bcx.create_block()).collect();

    let target = isa::TargetFrontendConfig {
        default_call_conv: isa::CallConv::Fast,
        pointer_width: PointerWidth::U64,
    };
    let mut fx = FunctionCx {
        tcx,
        bcx,
        mir,
        block_map,
        target_config: target,
        local_map: IndexVec::with_capacity(mir.local_decls.len()),
        ptr_type: types::I64,
        next_ssa: 0,
        _marker: PhantomData,
    };

    {
        let fx = &mut fx;
        let ssa_analyzed = ssa::analyze(fx);

        let ret = fx.tcx.types.unit;
        let ret = make_local_place(
            fx,
            Local::RETURN_PLACE,
            fx.tcx.layout_of(ret),
            ssa_analyzed[Local::RETURN_PLACE].is_ssa(fx, ret),
        );
        assert_eq!(fx.local_map.push(ret), Local::RETURN_PLACE);

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
    }

    codegen_block(&mut fx, start_block);

    fx.bcx.seal_all_blocks();
    fx.bcx.finalize();

    {
        let mut builder = settings::builder();
        // builder.set("opt_level", "speed_and_size").unwrap();
        let flags = settings::Flags::new(builder);

        let isa = lookup_by_name("x86_64").unwrap();
        let isa = isa.finish(flags).unwrap();
        let mut ctx = codegen::Context::for_function(fn_);
        // ctx.optimize(&*isa).unwrap();

        println!("{}", ctx.func);
    }
}

#[test]
fn codegen() {
    let arena = Arena::default();
    let tcx = &TyCtx::enter(&arena, Session {});

    let mut basic_blocks = IndexVec::new();

    basic_blocks.push(BasicBlockData {
        statements: vec![
            Statement::Assign(
                Place { local: Local::new(1), projection: List::empty() },
                Rvalue::Use(Operand::Const(
                    ConstValue::Scalar(ScalarRepr::from(12u64)),
                    tcx.types.i64,
                )),
            ),
            Statement::Assign(
                Place { local: Local::new(1), projection: List::empty() },
                Rvalue::Use(Operand::Copy(Place {
                    local: Local::new(1),
                    projection: List::empty(),
                })),
            ),
        ],
        terminator: Some(Terminator::Return),
    });

    let mut local_decls = IndexVec::new();

    local_decls.push(LocalDecl { ty: tcx.types.unit }); // return type
    local_decls.push(LocalDecl { ty: tcx.types.i64 });

    let body = Body { argc: 0, basic_blocks, local_decls };
    compile_fn(tcx, &body);
}

#[derive(Debug, Copy, Clone)] // FIXME: want be `Clone`
pub(crate) struct CValue<'tcx> {
    inner: CValueInner,
    layout: TyAbi<'tcx>,
}

#[derive(Debug, Copy, Clone)]
enum CValueInner {
    ByRef(Pointer),
    ByVal(Value),
}

impl<'tcx> CValue<'tcx> {
    pub fn layout(&self) -> TyAbi<'tcx> {
        self.layout
    }

    pub fn by_val(value: Value, layout: TyAbi<'tcx>) -> CValue<'tcx> {
        CValue { inner: CValueInner::ByVal(value), layout }
    }

    pub fn by_ref(ptr: Pointer, layout: TyAbi<'tcx>) -> CValue<'tcx> {
        CValue { inner: CValueInner::ByRef(ptr), layout }
    }

    pub fn load_scalar(self, fx: &mut FunctionCx<'_, '_, 'tcx>) -> Value {
        let layout = self.layout();
        match self.inner {
            CValueInner::ByRef(ptr) => {
                let clif_ty = match layout.layout.abi {
                    Abi::Scalar(scalar) => scalar_to_clif(fx.tcx, scalar),
                    _ => unreachable!("{:?}", layout.ty),
                };
                ptr.load(fx, clif_ty, MemFlags::new().with_notrap())
            }
            CValueInner::ByVal(value) => value,
        }
    }

    fn const_val(
        fx: &mut FunctionCx<'_, '_, 'tcx>,
        abi: TyAbi<'tcx>,
        const_val: ScalarRepr,
    ) -> CValue<'tcx> {
        assert_eq!(const_val.size(), abi.layout.size, "{:#?}: {:?}", const_val, abi);

        let clif_ty = fx.clif_type(abi.ty).unwrap();

        // if let ty::Bool = abi.ty.kind() {
        //     assert!(
        //         const_val == ty::ScalarInt::FALSE || const_val == ty::ScalarInt::TRUE,
        //         "Invalid bool 0x{:032X}",
        //         const_val
        //     );
        // }

        let val = match abi.ty.kind() {
            TyKind::Int(_) => {
                let raw_val =
                    const_val.size().truncate(const_val.to_bits(abi.layout.size).unwrap());
                fx.bcx.ins().iconst(clif_ty, raw_val as i64)
            }
            _ => panic!(
                "CValue::const_val for non bool/char/float/integer/pointer \
                type {:?} is not allowed",
                abi.ty
            ),
        };

        // let val = match layout.ty.kind() {
        //     ty::Uint(UintTy::U128) | ty::Int(IntTy::I128) => {
        //         let const_val = const_val.to_bits(layout.size).unwrap();
        //         let lsb = fx.bcx.ins().iconst(types::I64, const_val as u64 as i64);
        //         let msb = fx.bcx.ins().iconst(types::I64, (const_val >> 64) as u64 as i64);
        //         fx.bcx.ins().iconcat(lsb, msb)
        //     }
        //     ty::Bool
        //     | ty::Char
        //     | ty::Uint(_)
        //     | ty::Int(_)
        //     | ty::Ref(..)
        //     | ty::RawPtr(..)
        //     | ty::FnPtr(..) => {
        //         let raw_val = const_val.size().truncate(const_val.to_bits(layout.size).unwrap());
        //         fx.bcx.ins().iconst(clif_ty, raw_val as i64)
        //     }
        //     ty::Float(FloatTy::F32) => {
        //         fx.bcx.ins().f32const(Ieee32::with_bits(u32::try_from(const_val).unwrap()))
        //     }
        //     ty::Float(FloatTy::F64) => {
        //         fx.bcx.ins().f64const(Ieee64::with_bits(u64::try_from(const_val).unwrap()))
        //     }
        //     _ => panic!(
        //         "CValue::const_val for non bool/char/float/integer/pointer
        // type {:?} is not allowed",
        //         layout.ty
        //     ),
        // };

        CValue::by_val(val, abi)
    }

    pub(crate) fn force_stack(self, fx: &mut FunctionCx<'_, '_, 'tcx>) -> Pointer {
        let layout = self.layout();
        match self.inner {
            CValueInner::ByRef(ptr) => ptr,
            CValueInner::ByVal(_) => {
                let place = CPlace::new_stack_slot(fx, layout);
                place.write_cvalue(fx, self);
                place.to_ptr()
            }
        }
    }
}

#[derive(Debug, Copy, Clone)] // FIXME: want be `Clone`
pub(crate) struct CPlace<'tcx> {
    inner: CPlaceInner,
    layout: TyAbi<'tcx>,
}

#[derive(Debug, Copy, Clone)]
enum CPlaceInner {
    Var(Local, Variable),
    Addr(Pointer),
}

impl<'tcx> CPlace<'tcx> {
    pub fn layout(&self) -> TyAbi<'tcx> {
        self.layout
    }

    fn new_stack_slot(
        fx: &mut FunctionCx<'_, '_, 'tcx>,
        abi @ TyAbi { layout, .. }: TyAbi<'tcx>,
    ) -> CPlace<'tcx> {
        if layout.size.bytes() == 0 {
            return CPlace {
                inner: CPlaceInner::Addr(Pointer::dangling(
                    layout.align, /* FIXME: possible to add `prefer` align */
                )),
                layout: abi,
            };
        }

        if layout.size.bytes() >= u64::from(u32::MAX - 16) {
            panic!("too big for stack");
        }

        let stack_slot = fx.create_stack_slot(
            u32::try_from(layout.size.bytes()).unwrap(),
            /* FIXME: possible to add `prefer` align */
            u32::try_from(layout.align.bytes()).unwrap(),
        );
        CPlace { inner: CPlaceInner::Addr(stack_slot), layout: abi }
    }

    pub fn new_var(
        fx: &mut FunctionCx<'_, '_, 'tcx>,
        local: Local,
        layout: TyAbi<'tcx>,
    ) -> CPlace<'tcx> {
        let var = Variable::from_u32(fx.next_ssa());
        fx.bcx.declare_var(var, fx.clif_type(layout.ty).expect("LMAO"));
        CPlace { inner: CPlaceInner::Var(local, var), layout }
    }

    pub(crate) fn into_value(self, fx: &mut FunctionCx<'_, '_, 'tcx>) -> CValue<'tcx> {
        let layout = self.layout();
        match self.inner {
            CPlaceInner::Var(_local, var) => {
                let val = fx.bcx.use_var(var);
                CValue::by_val(val, layout)
            }
            CPlaceInner::Addr(ptr) => CValue::by_ref(ptr, layout),
        }
    }

    fn to_ptr(self) -> Pointer {
        match self.inner {
            CPlaceInner::Addr(ptr) => ptr,
            CPlaceInner::Var(_, _) => {
                panic!("Expected CPlace::Addr, found {:?}", self)
            }
        }
    }

    fn to_cvalue(self, fx: &mut FunctionCx<'_, '_, 'tcx>) -> CValue<'tcx> {
        let layout = self.layout();
        match self.inner {
            CPlaceInner::Var(_local, var) => {
                let val = fx.bcx.use_var(var);
                CValue::by_val(val, layout)
            }

            CPlaceInner::Addr(ptr) => CValue::by_ref(ptr, layout),
        }
    }

    fn write_cvalue(self, fx: &mut FunctionCx<'_, '_, 'tcx>, from: CValue<'tcx>) {
        fn transmute_scalar(
            fx: &mut FunctionCx<'_, '_, '_>,
            var: Variable,
            data: Value,
            dst_ty: Type,
        ) {
            let src_ty = fx.bcx.func.dfg.value_type(data);
            assert_eq!(
                src_ty.bytes(),
                dst_ty.bytes(),
                "write_cvalue_transmute: {:?} -> {:?}",
                src_ty,
                dst_ty,
            );
            let data = match (src_ty, dst_ty) {
                (_, _) if src_ty == dst_ty => data,

                _ => unreachable!("write_cvalue_transmute: {:?} -> {:?}", src_ty, dst_ty),
            };
            //fx.bcx.set_val_label(data, cranelift_codegen::ir::ValueLabel::new(var.index()));
            fx.bcx.def_var(var, data);
        }

        assert_eq!(self.layout().layout.size, from.layout().layout.size);

        let dst_layout = self.layout();
        match self.inner {
            CPlaceInner::Var(_local, var) => {
                let data = match from.layout().layout.abi {
                    Abi::Scalar(_) => {
                        CValue { inner: from.inner, layout: dst_layout }.load_scalar(fx)
                    }
                    _ => {
                        let ptr = from.force_stack(fx);
                        CValue { inner: CValueInner::ByRef(ptr), layout: dst_layout }
                            .load_scalar(fx)
                    }
                };
                let dst_ty = fx.clif_type(self.layout().ty).unwrap();
                transmute_scalar(fx, var, data, dst_ty);
            }
            CPlaceInner::Addr(to_ptr) => {
                if dst_layout.layout.size == Size::ZERO {
                    return;
                }

                let mut flags = MemFlags::new();
                flags.set_notrap();

                match from.inner {
                    CValueInner::ByVal(val) => {
                        to_ptr.store(fx, val, flags);
                    }
                    CValueInner::ByRef(from_ptr) => {
                        if let Abi::Scalar(_) = from.layout().layout.abi {
                            let val = from.load_scalar(fx);
                            to_ptr.store(fx, val, flags);
                            return;
                        }

                        let from_addr = from_ptr.get_addr(fx);
                        let to_addr = to_ptr.get_addr(fx);
                        let src_layout = from.layout();
                        let size = dst_layout.layout.size.bytes();
                        let src_align = src_layout.layout.align.bytes() as u8;
                        let dst_align = dst_layout.layout.align.bytes() as u8;
                        fx.bcx.emit_small_memory_copy(
                            fx.target_config,
                            to_addr,
                            from_addr,
                            size,
                            dst_align,
                            src_align,
                            true,
                            flags,
                        );
                    }
                }
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct Pointer {
    base: PointerBase,
    offset: Offset32,
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum PointerBase {
    Addr(Value),
    Stack(StackSlot),
    Dangling(Align),
}

impl Pointer {
    pub fn new(addr: Value) -> Self {
        Pointer { base: PointerBase::Addr(addr), offset: Offset32::new(0) }
    }

    pub fn stack_slot(stack_slot: StackSlot) -> Self {
        Pointer { base: PointerBase::Stack(stack_slot), offset: Offset32::new(0) }
    }

    fn dangling(align: Align) -> Self {
        Pointer { base: PointerBase::Dangling(align), offset: Offset32::new(0) }
    }

    fn get_addr(self, fx: &mut FunctionCx<'_, '_, '_>) -> Value {
        match self.base {
            PointerBase::Addr(base_addr) => {
                let offset: i64 = self.offset.into();
                if offset == 0 { base_addr } else { fx.bcx.ins().iadd_imm(base_addr, offset) }
            }
            PointerBase::Stack(stack_slot) => {
                fx.bcx.ins().stack_addr(fx.ptr_type, stack_slot, self.offset)
            }
            PointerBase::Dangling(align) => {
                fx.bcx.ins().iconst(fx.ptr_type, i64::try_from(align.bytes()).unwrap())
            }
        }
    }

    pub fn load(self, fx: &mut FunctionCx<'_, '_, '_>, ty: types::Type, flags: MemFlags) -> Value {
        match self.base {
            PointerBase::Addr(base_addr) => fx.bcx.ins().load(ty, flags, base_addr, self.offset),
            PointerBase::Stack(stack_slot) => fx.bcx.ins().stack_load(ty, stack_slot, self.offset),
            PointerBase::Dangling(_) => unreachable!(),
        }
    }

    pub fn store(self, fx: &mut FunctionCx<'_, '_, '_>, value: Value, flags: MemFlags) {
        match self.base {
            PointerBase::Addr(base_addr) => {
                fx.bcx.ins().store(flags, value, base_addr, self.offset);
            }
            PointerBase::Stack(stack_slot) => {
                fx.bcx.ins().stack_store(value, stack_slot, self.offset);
            }
            PointerBase::Dangling(_) => unreachable!(),
        }
    }
}
