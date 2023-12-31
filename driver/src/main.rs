#![feature(let_chains)]

mod codegen;

use {
    cranelift::{codegen::Context, prelude::settings},
    cranelift_module::{self as module, Linkage, Module},
    cranelift_object::{ObjectBuilder, ObjectModule},
    std::{fs, process::Command},
    yansi::Paint,
};

use {
    ariadne::{Color, Report, ReportKind},
    chumsky::Parser,
    compiler::{
        hir::{self, FnDecl, FnRetTy, ReportSettings, Stmt},
        mir::{self, ty::Abi},
        sess::SessionGlobals,
        Arena, Session, TyCtx,
    },
    cranelift::codegen::isa,
    lexer::{Block, ItemFn, ParseBuffer},
    std::{error::Error, io},
};

fn driver() -> Result<(), Box<dyn Error>> {
    let src = r#"
fn main(argc: i32, argv: i32) -> i8 {
    // let x = argc;
    // let y = argc * 2.i32;
    // let z = (x + y) * x;
    argc.i8
}
        "#;
    println!("{}{}", Paint::cyan("Source code -->:"), src);
    let mut input = ParseBuffer::new(lexer::lexer().parse(src).unwrap());
    // let Block { stmts, .. } = input.parse()?;
    let ItemFn { sig, block: Block { stmts, .. } } = input.parse()?;

    let arena = Arena::default();
    let tcx = {
        let sess = Session {};
        TyCtx::enter(&arena, sess)
    };

    let decl = FnDecl::analyze(&tcx, &sig);
    let body = {
        let stmts = stmts.iter().map(|stmt| Stmt::analyze(&tcx, stmt)).collect::<Vec<_>>();
        hir::analyze_fn_definition(&tcx, decl, &stmts).unwrap_or_else(|err| {
            let settings =
                ReportSettings { err_kw: Color::Magenta, err: Color::Red, kw: Color::Green };
            let (code, reason, labels) = err.report("src/sample.src", settings);
            let mut report = Report::build(ReportKind::Error, "src/sample.src", 5)
                .with_code(code)
                .with_message(reason);
            for label in labels {
                report = report.with_label(label);
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
        })
    };

    let sig = {
        let io = decl
            .inputs
            .iter()
            .map(|(_, ty)| ty.kind)
            .chain(Some(match decl.output {
                FnRetTy::Default(_) => tcx.types.unit,
                FnRetTy::Return(ty) => ty.kind,
            }))
            .collect::<Vec<_>>();
        mir::FnSig { inputs_and_output: tcx.mk_type_list(&io), abi: Abi::Zxc }
    };

    println!("{}", Paint::cyan("MIR -->:"));
    mir::write_mir_body_pretty(&tcx, &body, &mut io::stdout())?;

    let builder = settings::builder();
    let flags = settings::Flags::new(builder);
    //
    let isa = isa::lookup(target_lexicon::HOST)?;
    let isa = isa.finish(flags)?;
    //let mut ctx = codegen::Context::for_function(fn_);
    //ctx.optimize(&*isa).unwrap();
    //
    //println!("{}", ctx.func);
    //cranelift::codegen::verify_function(&ctx.func, &*isa)?;

    let func = codegen::cranelift::compile_fn(&tcx, sig, &body);
    let mut ctx = Context::for_function(func);
    let mut module =
        ObjectModule::new(ObjectBuilder::new(isa, "main.object", module::default_libcall_names())?);

    ctx.set_disasm(true);

    println!("{} \n{}", Paint::cyan("Cranelift IR -->:"), &ctx.func);
    let id = module.declare_function("WinMain", Linkage::Export, &ctx.func.signature)?;
    module.define_function(id, &mut ctx)?;

    let compiled = module.finish();
    println!(
        "{}\n{}",
        Paint::cyan("Assembly -->:"),
        ctx.compiled_code().unwrap().vcode.as_ref().unwrap()
    );

    fs::write("zxc.emit", compiled.emit()?)?;

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
