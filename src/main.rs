#![feature(let_chains, slice_ptr_len, slice_ptr_get, vec_into_raw_parts)]
#![allow(clippy::unit_arg, clippy::let_unit_value)]

#[macro_use]
pub mod ast;
pub mod lexer;
#[macro_use]
pub mod parse;
mod codegen;

pub use lexer::{Lex, Span};

// TODO: move from `ast` into this
pub mod token {
    use super::parse;

    pub use parse::delim::{Brace, Bracket, Paren};
}

#[cfg(test)]
mod util {
    #[rustfmt::skip]
    macro_rules! lex_it {
        ($src:literal) => {{ 
            use chumsky::Parser;
            ParseBuffer::new(crate::lexer::lexer().parse($src).into_result().unwrap()) 
        }};
    }

    pub(crate) use lex_it;
}

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
