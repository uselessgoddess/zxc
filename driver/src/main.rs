#![feature(let_chains, try_blocks, never_type)]

mod cli;
mod codegen;
mod error;
mod interface;

use {
    anyhow::Context as _,
    cranelift::{
        codegen::{isa, Context},
        prelude::{settings, Configurable},
    },
    cranelift_module::{self as module, FuncOrDataId, Module},
    cranelift_object::{ObjectBuilder, ObjectModule},
    std::{
        collections::HashMap,
        fs::File,
        path::{Path, PathBuf},
    },
};

use {
    compiler::{
        hir::{self, FnSig, HirCtx, ReportSettings, Stmt},
        mir, Tx, TyCtx,
    },
    lexer::{Block, ItemFn, ParseBuffer},
};

use {
    chumsky::Parser,
    compiler::{
        ariadne::{self, Color, Report, ReportKind},
        mir::pretty,
        par::WorkerLocal,
    },
    std::{fmt::Write as _, fs, io::Write as _, process::Command},
    yansi::Paint,
};

pub type Error = anyhow::Error;

fn driver_impl<'tcx>(
    tcx: Tx<'tcx>,
    (src, name): (&'tcx str, &'tcx str),
    out: &Path,
    artifact: &Path,
) -> Result<Vec<u8>, Error> {
    let mut input = ParseBuffer::new(lexer::lexer().parse(src).unwrap());

    let mut items = Vec::new();
    while !input.is_empty() {
        let ItemFn { sig, block: Block { stmts, .. } } = input.parse()?;
        let sig = FnSig::analyze(tcx, &sig);
        let stmts = stmts
            .iter()
            .map(|stmt| Stmt::analyze(tcx, stmt))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        items.push((sig, stmts));
    }

    let mut hix = HirCtx::pure(tcx);
    let mir = match hir::analyze_module(&mut hix, &items) {
        Ok(mir) => mir,
        Err(err) => {
            let settings =
                ReportSettings { err_kw: Color::Magenta, err: Color::Red, kw: Color::Magenta };
            let (code, reason, labels) = err.report(&hix, name, settings);
            let mut report =
                Report::build(ReportKind::Error, name, 5).with_code(code).with_message(reason);
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
                .print((name, ariadne::Source::from(src)))
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

    let emit_clif = tcx.sess.opts.emit.contains(Emit::IR);
    let mut contexts = HashMap::with_capacity(128);

    for (def, mir) in mir.iter_enumerated() {
        let (id, func) = codegen::cranelift::compile_fn(tcx, &hix, def, mir, &mut module);

        let mut ctx = Context::for_function(func);
        // ctx.set_disasm(true);
        ctx.optimize(module.isa())?;
        module.define_function(id, &mut ctx)?;

        if emit_clif {
            contexts.insert(id, ctx.func);
        }
    }

    use sess::Emit;

    if tcx.sess.opts.emit.contains(Emit::MIR) {
        let path = out.join(artifact.with_extension("mir"));
        let mut file = File::create(&path).with_context(|| format!("failed to create {path:?}"))?;

        for (def, mir) in mir.iter_enumerated() {
            let mut printer = pretty::FmtPrinter::new(&hix);
            mir::write_mir_pretty(def, mir, &mut printer)?;

            write!(file, "{}", printer.into_buf())?;
        }
    }

    if emit_clif {
        let path = out.join(artifact.with_extension("clif"));
        let mut file = File::create(&path).with_context(|| format!("failed to create {path:?}"))?;

        for (def, _) in mir.iter_enumerated() {
            let Some(FuncOrDataId::Func(func)) =
                module.get_name(hix.instances[def].symbol.as_str())
            else {
                panic!()
            };

            writeln!(file, "{}", &contexts[&func])?;
            // println!("{}", printer.into_buf().replace('\n', "\n    "));
        }
    }

    Ok(module.finish().emit()?)
}

fn parent_dir(path: &Path) -> &Path {
    if let Some(parent) = path.parent() { parent } else { path }
}

fn driver(early: &EarlyErrorHandler, compiler: &interface::Compiler) -> Result<(), Error> {
    let arena = WorkerLocal::new(|_| Default::default());
    let tcx = TyCtx::enter(&arena, &compiler.sess);

    let input = &tcx.sess.io.input;
    let src = fs::read_to_string(input)?;

    println!("{}", Paint::magenta(&src));

    let sess = &tcx.sess;
    let art = PathBuf::from(input.file_name().unwrap());
    let out =
        if let Some(out) = &sess.io.output_file { out.clone() } else { art.with_extension("out") };
    let out_dir = if let Some(dir) = &sess.io.output_dir {
        fs::create_dir_all(dir)?;
        dir.clone()
    } else {
        parent_dir(&out).into()
    };
    let temps = sess.opts.Z.temps_dir.as_ref().map(PathBuf::from).unwrap_or(out_dir.clone());
    fs::create_dir_all(&temps)?;
    let out = out_dir.join(out);

    let emit = temps.join(art.with_extension("emit"));
    let name =
        input.as_os_str().to_str().unwrap_or_else(|| early.early_fatal("non UTF-8 file name"));
    fs::write(&emit, driver_impl(&tcx, (&src, name), &out_dir, &art)?)?;

    let out = Command::new("gcc").arg(emit).arg("-o").arg(out).output()?;
    if !out.stderr.is_empty() {
        print!("Linker out: {}", String::from_utf8_lossy(&out.stderr));
    }

    Ok(())
}

use {
    crate::{
        cli::{Args, Emit},
        error::EarlyErrorHandler,
        interface::Config,
    },
    compiler::sess::{self, Options},
};

fn main() {
    use {clap::Parser, std::error::Error};

    let early = EarlyErrorHandler;

    let Args { input, output, output_dir, c_flags, z_flags, emit } = Args::parse();
    let c_opts = cli::build_options(&early, &c_flags, sess::C_OPTIONS, "C", "codegen");
    let z_opts = cli::build_options(&early, &z_flags, sess::Z_OPTIONS, "Z", "compiler");

    let mut emit_flags = sess::Emit::None;
    for e in emit {
        emit_flags |= match e {
            Emit::Mir => sess::Emit::MIR,
            Emit::IR => sess::Emit::IR,
        };
    }
    let config = Config {
        opts: Options { Z: z_opts, C: c_opts, emit: emit_flags },
        input,
        output_dir,
        output_file: output,
    };
    println!("{config:#?}");

    interface::run_compiler(config, |compiler| {
        if let Err(err) = driver(&early, compiler) {
            early.early_fatal(err.to_string())
        }
    });
}
