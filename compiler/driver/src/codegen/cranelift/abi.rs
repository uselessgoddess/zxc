use {
    super::{codegen_operand, codegen_place, sig_from_abi, CPlace, CValue, FunctionCx},
    cranelift::{
        codegen::ir::{FuncRef, Inst, SigRef},
        prelude::{InstBuilder, TrapCode, Value},
    },
    cranelift_module::{FuncId, Linkage, ModuleError},
    middle::{
        abi::{ArgAbi, PassMode},
        mir::{self, ty, BasicBlock, Operand, Place},
    },
};

impl<'m, 'cl, 'tcx: 'm> FunctionCx<'m, 'cl, 'tcx> {
    fn import_function(&mut self, def: mir::DefId) -> FuncId {
        let name = self.hix.symbol_name(def).name;
        let sig = sig_from_abi(
            self.tcx,
            self.module.target_config().default_call_conv,
            &self.tcx.fn_abi_of_sig(self.hix.instances[def].sig),
        );
        match self.module.declare_function(name, Linkage::Import, &sig) {
            Ok(func_id) => func_id,
            Err(ModuleError::IncompatibleDeclaration(_)) => self.tcx.fatal(format!(
                "attempt to declare `{name}` as function, but it was already declared as static"
            )),
            Err(ModuleError::IncompatibleSignature(_, prev_sig, new_sig)) => {
                self.tcx.fatal(format!(
                    "attempt to declare `{name}` with signature {new_sig:?}, \
                    but it was already declared with signature {prev_sig:?}"
                ))
            }
            Err(err) => panic!("{err:?}"),
        }
    }

    fn get_function_ref(&mut self, def: mir::DefId) -> FuncRef {
        let func_id = self.import_function(def);
        self.module.declare_func_in_func(func_id, self.bcx.func)
    }
}

pub(super) fn codegen_with_call_return<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    ret_abi: &ArgAbi<'tcx>,
    ret_place: CPlace<'tcx>,
    f: impl FnOnce(&mut FunctionCx<'_, '_, 'tcx>) -> Inst,
) {
    let call_inst = f(fx);

    match ret_abi.mode {
        PassMode::Ignore => {}
        PassMode::Direct => {
            let ret_val = fx.bcx.inst_results(call_inst)[0];
            ret_place.write_cvalue(fx, CValue::by_val(ret_val, ret_abi.ty));
        }
    }
}

pub fn codegen_terminator_call<'tcx>(
    fx: &mut FunctionCx<'_, '_, 'tcx>,
    func: Operand<'tcx>,
    args: &[Operand<'tcx>],
    dest: Place<'tcx>,
    target: Option<BasicBlock>,
) {
    let func = codegen_operand(fx, func);
    let fn_sig = func.layout().ty.fn_sig(fx.hix);
    let ret_place = codegen_place(fx, dest);

    let instance = if let ty::FnDef(def) = func.layout().ty.kind() { Some(def) } else { None };

    let fn_abi = fx.tcx.fn_abi_of_sig(fn_sig);
    let args = args.iter().copied().map(|arg| codegen_operand(fx, arg)).collect::<Vec<_>>();

    assert_eq!(fn_abi.args.len(), args.len());

    enum CallTarget {
        Direct(FuncRef),
        Indirect(SigRef, Value), // not now
    }

    let func_ref = match instance {
        Some(def) => {
            let func_ref = fx.get_function_ref(def);
            CallTarget::Direct(func_ref)
        }
        None => {
            let func = func.load_scalar(fx);
            let sig = sig_from_abi(fx.tcx, fx.target_config.default_call_conv, &fn_abi);
            let sig = fx.bcx.import_signature(sig);

            CallTarget::Indirect(sig, func)
        }
    };

    codegen_with_call_return(fx, &fn_abi.ret, ret_place, |fx| {
        let call_args = args
            .into_iter()
            .enumerate()
            .flat_map(|(i, arg)| match fn_abi.args[i].mode {
                PassMode::Ignore => None,
                PassMode::Direct => Some(arg.load_scalar(fx)),
            })
            .collect::<Vec<_>>();

        let call_inst = match func_ref {
            CallTarget::Direct(func_ref) => fx.bcx.ins().call(func_ref, &call_args),
            _ => todo!(),
        };

        call_inst
    });

    if let Some(dest) = target {
        let ret_block = fx.block(dest);
        fx.bcx.ins().jump(ret_block, &[]);
    } else {
        fx.bcx.ins().trap(TrapCode::UnreachableCodeReached);
    }
}
