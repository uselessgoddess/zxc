#![feature(let_chains, try_blocks)]

mod codegen;

use {
    cranelift::{
        codegen::{isa, Context},
        prelude::{settings, Configurable},
    },
    cranelift_module::{self as module, Linkage, Module},
    cranelift_object::{ObjectBuilder, ObjectModule},
};

use {
    compiler::{
        hir::{self, FnDecl, FnRetTy, FnSig, HirCtx, ReportSettings, Stmt},
        mir::{self, ty::Abi},
        sess::SessionGlobals,
        Arena, Session, TyCtx,
    },
    lexer::{Block, ItemFn, ParseBuffer},
};

use {
    ariadne::{Color, Report, ReportKind},
    chumsky::Parser,
    std::{
        error::Error,
        fs,
        io::{self, Write},
        process::Command,
    },
    yansi::Paint,
};

fn driver() -> Result<(), Box<dyn Error>> {
    let src = r#"
extern "C" fn main(argc: isize, argv: isize) -> isize {
    argc + 200.isize
}
        "#;
    println!("{}", Paint::magenta(src));
    let mut input = ParseBuffer::new(lexer::lexer().parse(src).unwrap());
    // let Block { stmts, .. } = input.parse()?;

    let arena = Arena::default();
    let tcx = {
        let sess = Session {};
        TyCtx::enter(&arena, sess)
    };

    let mut items = Vec::new();
    while let Ok(ItemFn { sig, block: Block { stmts, .. } }) = input.parse::<ItemFn>() {
        let sig = FnSig::analyze(&tcx, &sig);
        let stmts = stmts
            .iter()
            .map(|stmt| Stmt::analyze(&tcx, stmt))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        items.push((sig, stmts));
    }

    let mut hix = HirCtx::pure(&tcx);
    let mir = match hir::analyze_module(&mut hix, &items) {
        Ok(mir) => mir,
        Err(err) => {
            let settings =
                ReportSettings { err_kw: Color::Magenta, err: Color::Red, kw: Color::Magenta };
            let (code, reason, labels) = err.report("src/sample.src", settings);
            let mut report = Report::build(ReportKind::Error, "src/sample.src", 5)
                .with_code(code)
                .with_message(reason);
            for label in labels {
                report = report.with_label(label.with_color(Color::Red));
            }
            report
                .with_config(
                    ariadne::Config::default()
                        // .with_cross_gap(true)
                        .with_underlines(true),
                )
                .finish()
                .print(("src/sample.src", ariadne::Source::from(src)))
                .unwrap();
            panic!();
        }
    };

    let mut builder = settings::builder();
    // builder.set("opt_level", "speed_and_size")?;
    let flags = settings::Flags::new(builder);
    //
    let isa = isa::lookup(target_lexicon::HOST)?;
    let isa = isa.finish(flags)?;

    let mut module =
        ObjectModule::new(ObjectBuilder::new(isa, "main.object", module::default_libcall_names())?);

    for (def, mir) in mir.iter_enumerated() {
        let mut place = Vec::new();

        let symbol = hix.instances[def].symbol;
        writeln!(
            &mut place,
            "{}",
            Paint::cyan(&format!("MIR --> ({}):", hix.instances[def].symbol))
        )?;
        mir::write_mir_body_pretty(&tcx, &mir, &mut place)?;

        let func = codegen::cranelift::compile_fn(&tcx, hix.instances[def].sig, &mir);
        let mut ctx = Context::for_function(func);

        ctx.set_disasm(true);

        let _ = ctx.optimize(module.isa())?;
        writeln!(&mut place, "{} \n{}", Paint::cyan("Cranelift IR -->:"), &ctx.func)?;
        let id = module.declare_function(
            symbol.as_str(),
            if hix.instances[def].sig.abi == Abi::Zxc { Linkage::Local } else { Linkage::Export },
            &ctx.func.signature,
        )?;
        module.define_function(id, &mut ctx)?;

        writeln!(
            &mut place,
            "{}\n{}",
            Paint::cyan("Assembly -->:"),
            ctx.compiled_code().unwrap().vcode.as_ref().unwrap()
        )?;

        println!("{}", String::from_utf8_lossy(&place).replace("\n", "\n    "));
    }

    fs::write("zxc.emit", module.finish().emit()?)?;

    let out = Command::new("gcc").args(["zxc.emit", "-o", "zxc.out"]).output()?;
    if !out.stderr.is_empty() {
        print!("Linker out: {}", String::from_utf8_lossy(&out.stderr));
    }

    Ok(())
}

fn main() {
    unsafe {
        compiler::sess::in_session_globals(SessionGlobals::default(), || {
            driver().unwrap();
        });
    }
}
