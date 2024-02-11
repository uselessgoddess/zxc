use {
    cranelift::{
        codegen::Context,
        prelude::{AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder, Signature},
    },
    cranelift_module::{Linkage, Module},
    middle::{
        abi::Conv,
        hir::{self, Hx},
        mir,
    },
};

pub fn create_entry_wrapper(hix: Hx, m: &mut dyn Module, main_def: mir::DefId, is_main: bool) {
    let tcx = hix.tcx;
    let default_call_conv = m.target_config().default_call_conv;
    let isize = m.target_config().pointer_type();
    let cmain_sig = Signature {
        params: vec![AbiParam::new(isize), AbiParam::new(isize)],
        returns: vec![AbiParam::new(isize)],
        call_conv: super::conv_to_call_conv(hix.tcx.sess, Conv::C, default_call_conv),
    };

    let entry_name = "main";
    let cmain_id = match m.declare_function(entry_name, Linkage::Export, &cmain_sig) {
        Ok(func_id) => func_id,
        Err(err) => {
            tcx.sess.fatal(format!("entry symbol `{entry_name}` declared multiple times: {err}"));
        }
    };

    let sig = hix.instances[main_def].sig;
    let main_sig = super::sig_from_abi(tcx, default_call_conv, &tcx.fn_abi_of_sig(sig));
    let main_id =
        m.declare_function(hix.symbol_name(main_def).name, Linkage::Import, &main_sig).unwrap();

    let mut ctx = Context::new();
    ctx.func.signature = cmain_sig;
    {
        let mut func_ctx = FunctionBuilderContext::new();
        let mut bcx = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);

        let block = bcx.create_block();
        bcx.switch_to_block(block);

        let argc = bcx.append_block_param(block, isize);
        let argv = bcx.append_block_param(block, isize);

        let main_ref = m.declare_func_in_func(main_id, bcx.func);

        let ret = if !is_main {
            // using user-defined start fn
            let call_inst = bcx.ins().call(main_ref, &[argc, argv]);
            bcx.inst_results(call_inst)[0]
        } else {
            // ignore argc, argv -- `std` has no entry function now
            bcx.ins().call(main_ref, &[]);
            bcx.ins().iconst(isize, 0)
        };

        bcx.ins().return_(&[ret]);
        bcx.seal_all_blocks();
        bcx.finalize();
    }

    if let Err(err) = m.define_function(cmain_id, &mut ctx) {
        tcx.sess.fatal(format!("entry symbol `{entry_name}` defined multiple times: {err}"));
    }
}

// TODO: move into generic codegen
pub fn maybe_create_entry_wrapper(hix: Hx, cgu: &mir::CodegenUnit, module: &mut dyn Module) {
    if !cgu.primary {
        return;
    }
    let Some((main_def, entry)) = hix.entry_fn() else { return };

    create_entry_wrapper(hix, module, main_def, entry == hir::EntryFnType::Main)
}
