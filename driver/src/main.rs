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
        hir::{self, FnSig, HirCtx, ReportSettings, Stmt},
        mir::{self},
        sess::SessionGlobals,
        Arena, Session, TyCtx,
    },
    lexer::{Block, ItemFn, ParseBuffer},
};

use {
    ariadne::{Color, Report, ReportKind},
    chumsky::Parser,
    compiler::{
        mir::{pretty, ty::Abi},
        par::{self, WorkerLocal},
        sess,
    },
    std::{error::Error, fmt::Write, fs, process::Command},
    yansi::Paint,
};

fn driver<'tcx>(tcx: &'tcx TyCtx<'tcx>) -> Result<(), Box<dyn Error>> {
    let src = r#"
fn narrow(x: i64) -> isize {
    x.i8.isize
}

extern "C" fn main(argc: isize, argv: isize) -> isize {
    argc + 12.narrow
}
        "#;
    println!("{}", Paint::magenta(src));
    let mut input = ParseBuffer::new(lexer::lexer().parse(src).unwrap());
    // let Block { stmts, .. } = input.parse()?;

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
    builder.set("opt_level", "speed_and_size")?;
    let flags = settings::Flags::new(builder);
    //
    let isa = isa::lookup(target_lexicon::HOST)?;
    let isa = isa.finish(flags)?;

    let mut module =
        ObjectModule::new(ObjectBuilder::new(isa, "main.object", module::default_libcall_names())?);

    for (def, mir) in mir.iter_enumerated() {
        let mut printer = pretty::FmtPrinter::new(&hix);
        writeln!(
            &mut printer,
            "{}",
            Paint::cyan(&format!("MIR --> ({}):", hix.instances[def].symbol))
        )?;
        mir::write_mir_pretty(def, &mir, &mut printer)?;

        let (id, func) = codegen::cranelift::compile_fn(&tcx, &hix, def, &mir, &mut module);
        let mut ctx = Context::for_function(func);

        ctx.set_disasm(true);
        ctx.optimize(module.isa())?;

        writeln!(&mut printer, "{} \n{}", Paint::cyan("Cranelift IR -->:"), &ctx.func)?;
        module.define_function(id, &mut ctx)?;

        writeln!(
            &mut printer,
            "{}\n{}",
            Paint::cyan("Assembly -->:"),
            ctx.compiled_code().unwrap().vcode.as_ref().unwrap()
        )?;

        println!("{}", printer.into_buf().replace("\n", "\n"));
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
        sess::in_session_globals(SessionGlobals::default(), || {
            // add each registry for each rayon thread
            par::Registry::register(&par::Registry::new(32)); // better case is 1

            let sess = Session {};
            let arena = WorkerLocal::new(|_| Arena::default());
            let tcx = TyCtx::enter(&arena, &sess);

            driver(&tcx).unwrap()
        });
    }
}
