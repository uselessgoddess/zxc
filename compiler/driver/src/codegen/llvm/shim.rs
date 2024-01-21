use {
    super::{base::get_fn, Bx, CodegenCx},
    middle::{
        hir,
        mir::{self, Instance, InstanceDef},
    },
};

pub fn create_entry_wrapper<'tcx>(
    cx: &mut CodegenCx<'_, 'tcx>,
    main_def: mir::DefId,
    is_main: bool,
) {
    let (hix, _tcx) = (cx.hix, cx.tcx);

    let main_fn = Instance::def(InstanceDef::Item(main_def));
    let main_ll = get_fn(cx, main_fn);

    let (isize, ptr) = (cx.type_isize(), cx.type_ptr());
    let llty = cx.type_func(&[isize, ptr], isize);

    let Some(llfn) = cx.declare_c_main(llty) else {
        cx.tcx
            .sess
            .emit_fatal(hir::errors::MultipleMainFunctions { span: hix.instances[main_def].span })
    };

    let mut bx = Bx::build(&cx, llfn);
    let llbb = bx.append_block("start");
    bx.switch_to_block(llbb);

    if !is_main {
        let fn_ty = cx.type_func(&[isize, ptr], isize);

        let argc = bx.intcast(bx.get_param(0), cx.type_isize(), true);
        let argv = bx.get_param(1);

        let result = bx.call(fn_ty, None, None, main_ll, &[argc, argv], None);
        let cast = bx.intcast(result, cx.type_isize(), true);
        bx.ret(cast);
    } else {
        let fn_ty = cx.type_func(&[], cx.type_void());

        let _ = bx.call(fn_ty, None, None, main_ll, &[], None);
        bx.ret(cx.const_int(cx.type_isize(), 0))
    };
}

pub fn maybe_create_entry_wrapper(cx: &mut CodegenCx<'_, '_>) {
    let Some((main_def, entry)) = cx.hix.entry_fn() else { return };

    create_entry_wrapper(cx, main_def, entry == hir::EntryFnType::Main)
}
