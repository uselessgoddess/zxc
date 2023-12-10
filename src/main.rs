mod ast;
mod lexer;
mod parser;

pub use lexer::{lexer, Lex, Number, Span};

use inkwell::{
    context::Context,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    },
    OptimizationLevel,
};

fn main() {}

#[cfg(non)]
fn _test() {
    let ctx = Context::create();
    let builder = ctx.create_builder();
    let module = ctx.create_module("test");

    let i64t = ctx.i64_type();

    {
        let fnt = i64t.fn_type(&[i64t.into(), i64t.into()], false);
        let func = module.add_function("square", fnt, None);

        builder.position_at_end(ctx.append_basic_block(func, "entry"));

        let x = func.get_nth_param(0).unwrap().into_int_value();
        let y = func.get_nth_param(1).unwrap().into_int_value();

        let sum = builder.build_int_add(x, y, "square");
        let _ = builder.build_return(Some(&sum));

        func.print_to_stderr();
    }

    {
        let fnt = i64t.fn_type(&[], false);
        let func = module.add_function("triangle", fnt, None);

        builder.position_at_end(ctx.append_basic_block(func, "entry"));

        let a = i64t.const_int(228, false);
        let b = i64t.const_int(1337, false);
        let c = i64t.const_int(177013, false);

        let sum = builder.build_int_add(a, b, "first");
        let sum = builder.build_int_add(sum, c, "second");
        let _ = builder.build_return(Some(&sum));

        func.print_to_stderr();
    }

    Target::initialize_x86(&InitializationConfig { ..Default::default() });

    let triplet = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triplet).unwrap();

    let machine = target
        .create_target_machine(
            &TargetTriple::create("x86_64-pc-windows-gnu"),
            "x86-64",
            "+avx2",
            OptimizationLevel::None,
            RelocMode::Default,
            CodeModel::Small,
        )
        .unwrap();

    machine.write_to_file(&module, FileType::Assembly, "a.out".as_ref()).unwrap();
}
