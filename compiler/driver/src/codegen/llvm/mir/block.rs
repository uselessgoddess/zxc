use {
    super::{FunctionCx, LPlace, LValue, LValueRepr},
    crate::codegen::llvm::{abi::FnAbiLlvmExt, mir::LLocal},
    llvm::{IntPredicate, Value},
    middle::{
        abi::{Abi, ArgAbi, PassMode, WrappingRange},
        mir::{self, ty, Instance, InstanceDef, Operand, Place, SwitchTargets},
    },
};

pub fn codegen_switch_terminator<'ll, 'tcx>(
    fx: &mut FunctionCx<'_, 'll, 'tcx>,
    discr: Operand<'tcx>,
    targets: &SwitchTargets,
) {
    let discr = super::codegen_operand(fx, discr);
    let (discr, switch_ty) = (discr.load_scalar(), discr.layout.ty);

    if let Some((test, then, otherwise)) = targets.as_static_if() {
        let lltrue = fx.block(then); // todo: generate landing pads if possible
        let llfalse = fx.block(otherwise); // todo: generate landing pads if possible

        if switch_ty.is_bool() {
            match test {
                0 => fx.bcx.cond_br(discr, llfalse, lltrue),
                1 => fx.bcx.cond_br(discr, lltrue, llfalse),
                _ => unreachable!(),
            }
        } else {
            let switch_llty = fx.cx.immediate_type_of(fx.cx.tcx.layout_of(switch_ty).layout);
            let llval = fx.cx.const_uint(switch_llty, test as u64); // fixme: Loss comparison
            let cond = fx.bcx.icmp(IntPredicate::IntEQ, discr, llval);
            fx.bcx.cond_br(cond, lltrue, llfalse);
        }
    } else {
        // FIXME: avoid vector creating by fixing borrowck
        let avoid =
            targets.iter().map(|(value, target)| (value, fx.block(target))).collect::<Vec<_>>();
        let otherwise = fx.block(targets.otherwise());
        fx.bcx.switch(discr, otherwise, avoid.into_iter());
    }
}

enum RetDest<'ll, 'tcx> {
    Nothing,
    Direct(mir::Local),
    Store(LPlace<'ll, 'tcx>),
}

fn codegen_ret_dest<'ll, 'tcx>(
    fx: &mut FunctionCx<'_, 'll, 'tcx>,
    dest: Place<'tcx>,
    ret: ArgAbi<'tcx>,
) -> RetDest<'ll, 'tcx> {
    if let PassMode::Ignore = ret.mode {
        return RetDest::Nothing;
    }
    let dest = if let Some(local) = dest.as_local() {
        match fx.locals[local] {
            LLocal::Value(_) => panic!("place local already assigned to"),
            LLocal::Pending => return RetDest::Direct(local),
            LLocal::Place(dest) => dest,
        }
    } else {
        super::codegen_place(fx, dest.as_ref())
    };

    RetDest::Store(dest)
}

fn codegen_argument<'ll, 'tcx>(
    fx: &mut FunctionCx<'_, 'll, 'tcx>,
    value: LValue<'ll, 'tcx>,
    arg: ArgAbi<'tcx>,
    llargs: &mut Vec<&'ll Value>,
) {
    if let PassMode::Ignore = arg.mode {
        return;
    }

    let (mut llval, align, by_ref) = match value.repr {
        LValueRepr::ByVal(_) => (value.load_scalar(), arg.ty.align, false),
        LValueRepr::ByRef(llval, align) => (llval, align, true),
        _ => unreachable!(),
    };

    if by_ref {
        llval = fx.bcx.load(fx.cx.type_of(arg.ty.layout), llval, align);
        if let Abi::Scalar(scalar) = arg.ty.abi
            && scalar.is_bool()
        {
            fx.bcx.assume_range(llval, WrappingRange { start: 0, end: 1 });
        }
        llval = fx.bcx.to_immediate(llval, arg.ty.layout);
    }

    llargs.push(llval);
}

fn codegen_return<'ll, 'tcx>(
    fx: &mut FunctionCx<'_, 'll, 'tcx>,
    dest: RetDest<'ll, 'tcx>,
    ret: ArgAbi<'tcx>,
    llval: &'ll Value,
) {
    match dest {
        RetDest::Nothing => {}
        RetDest::Store(dest) => fx.bcx.store_arg_abi(ret, llval, dest),
        RetDest::Direct(local) => {
            let operand = LValue::by_val(llval, ret.ty);
            fx.locals[local] = LLocal::Value(operand);
        }
    }
}

pub fn codegen_call_terminator<'ll, 'tcx>(
    fx: &mut FunctionCx<'_, 'll, 'tcx>,
    func: Operand<'tcx>,
    args: &[Operand<'tcx>],
    dest: Place<'tcx>,
    target: Option<mir::BasicBlock>,
) {
    let callee = super::codegen_operand(fx, func);
    let (Some(def), None) = (match callee.layout.ty.kind() {
        ty::FnDef(def) => (Some(def), None::<()>),
        _ => panic!("{:?} is not callable", callee.layout.ty),
    }) else {
        unreachable!()
    };

    let sig = callee.layout.ty.fn_sig(fx.hix);
    let fn_abi = fx.cx.tcx.fn_abi_of_sig(sig);

    let mut llargs = Vec::with_capacity(fn_abi.args.len());

    let ret_dest =
        if target.is_some() { codegen_ret_dest(fx, dest, fn_abi.ret) } else { RetDest::Nothing };

    let mut copied_constant_arguments = vec![];
    for (idx, &arg) in args.iter().enumerate() {
        let mut operand = super::codegen_operand(fx, arg);

        // The callee needs to own the argument memory if we pass it
        // by-ref, so make a local copy of non-immediate constants.
        if let (Operand::Copy(_) | Operand::Const(_, _), LValueRepr::ByRef(..)) =
            (arg, operand.repr)
        {
            let tmp = LPlace::alloca(fx, operand.layout);
            tmp.store_lvalue(fx, operand);
            operand.repr = LValueRepr::ByRef(tmp.llval, tmp.align);
            copied_constant_arguments.push(tmp);
        }

        codegen_argument(fx, operand, fn_abi.args[idx], &mut llargs);
    }

    let fn_ptr = super::base::get_fn(fx.cx, Instance::def(InstanceDef::Item(def)));
    let fn_ty = fn_abi.llvm_type(fx.cx);

    let llret = fx.bcx.call(fn_ty, None, Some(&fn_abi), fn_ptr, &llargs, None);

    if let Some(target) = target {
        codegen_return(fx, ret_dest, fn_abi.ret, llret);
        let block = fx.block(target);
        fx.bcx.br(block);
    } else {
        fx.bcx.unreachable();
    }
}
