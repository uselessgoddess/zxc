#![feature(let_chains, try_blocks, never_type)]

mod cli;
mod codegen;
mod error;
mod interface;
mod style;

use {
    anyhow::Context as _,
    concolor::{ColorChoice, Stream},
    cranelift::{
        codegen::{isa, Context},
        prelude::{settings, Configurable},
    },
    cranelift_module::{self as module, FuncOrDataId, Module},
    cranelift_object::{ObjectBuilder, ObjectModule},
};

use compiler::{
    ariadne::{self, Color, Label, Report, ReportKind},
    hir::{self, FnSig, HirCtx, ReportSettings, Stmt},
    mir::{self, pretty, InstanceDef, MirPass},
    par::WorkerLocal,
    rayon::prelude::*,
    sess::{self, Options, OutFileName, OutputType},
    symbol::Symbol,
    Tx, TyCtx,
};

use {
    chumsky::Parser,
    lexer::{Block, ItemFn, ParseBuffer},
    std::{
        collections::{BTreeMap, HashMap},
        fmt::{self, Write as _},
        fs::{self, File},
        io::{self, Write as _},
        path::{Path, PathBuf},
        process::{self, Command},
    },
};

#[derive(Debug)]
pub enum Error {
    Error(anyhow::Error),
    Guaranteed,
}

macro_rules! impl_error {
    ($($error:ty)*) => {$(
        impl From<$error> for Error {
            fn from(error: $error) -> Self {
                Error::Error(error.into())
            }
        }
    )*};
}

impl_error! {
    anyhow::Error
    io::Error
    fmt::Error
    isa::LookupError
    settings::SetError
    cranelift_object::object::write::Error
    cranelift::codegen::CodegenError
    cranelift_module::ModuleError
}

fn parse_items<'lex>(
    tcx: Tx<'lex>,
    input: &mut ParseBuffer<'lex>,
) -> Result<Vec<(FnSig<'lex>, Box<[Stmt<'lex>]>)>, lexer::Error> {
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
    Ok(items)
}

fn driver_impl<'tcx>(tcx: Tx<'tcx>, src: &'tcx str, name: &'tcx str) -> Result<Vec<u8>, Error> {
    let mut input = ParseBuffer::new(lexer::lexer().parse(src).unwrap());

    let items = parse_items(tcx, &mut input).or_else(|err| {
        Report::build(ReportKind::Error, name, 5)
            .with_message(err.to_string())
            .with_label(Label::new((name, err.span.into_range())).with_color(Color::Red))
            .finish()
            .eprint((name, ariadne::Source::from(src)))
            .unwrap();
        Err(Error::Guaranteed)
    })?;

    let mut hix = HirCtx::pure(tcx, Symbol::intern("main-cgu"));
    match hir::analyze_module(&mut hix, &items) {
        Ok(_) => {}
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
                .eprint((name, ariadne::Source::from(src)))
                .unwrap();
            return Err(Error::Guaranteed);
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

    let cgu = hix.as_codegen_unit();
    let mir = cgu
        .items
        .par_iter()
        .map(|(def, _)| {
            let InstanceDef::Item(def) = def.def;
            (def, hix.optimized_mir(def))
        })
        .collect::<Vec<_>>();
    mir::pass::emit_mir(&hix, &mir)?;

    for (def, mir) in mir {
        let (id, func) = codegen::cranelift::compile_fn(tcx, &hix, def, mir, &mut module);

        module.define_function(id, &mut Context::for_function(func))?;
    }

    //if tcx.sess.opts.emit.contains(Emit::MIR) {
    //    let path = out.join(artifact.with_extension("mir"));
    //    let mut file = File::create(&path).with_context(|| format!("failed to create {path:?}"))?;
    //
    //    for (def, _) in &cgu.items {
    //        let mut printer = pretty::FmtPrinter::new(&hix);
    //        mir::write_mir_pretty(def, mir, &mut printer)?;
    //
    //        write!(file, "{}", printer.into_buf())?;
    //    }
    //}

    //let emit_clif = tcx.sess.opts.emit.contains(Emit::IR);
    //let mut contexts = HashMap::with_capacity(128);

    //if emit_clif {
    //    let path = out.join(artifact.with_extension("clif"));
    //    let mut file = File::create(&path).with_context(|| format!("failed to create {path:?}"))?;
    //
    //    for (def, _) in mir.iter_enumerated() {
    //        let Some(FuncOrDataId::Func(func)) =
    //            module.get_name(hix.instances[def].symbol.as_str())
    //        else {
    //            panic!()
    //        };
    //
    //        writeln!(file, "{}", &contexts[&func])?;
    //        // println!("{}", printer.into_buf().replace('\n', "\n    "));
    //    }
    //}

    Ok(module.finish().emit()?)
}

fn parent_dir(path: &Path) -> &Path {
    if let Some(parent) = path.parent() { parent } else { path }
}

fn driver(early: &EarlyErrorHandler, compiler: &interface::Compiler) -> Result<(), Error> {
    let sess = &compiler.sess;
    let input = &sess.io.input;
    let src = fs::read_to_string(input).with_context(|| format!("{}", input.display()))?;

    if concolor::get(Stream::Stdout).ansi_color() {
        println!("{}", yansi::Paint::magenta(&src));
    } else {
        println!("{src}");
    }

    let art = PathBuf::from(input.file_name().unwrap());
    let out =
        if let Some(out) = &sess.io.output_file { out.clone() } else { art.with_extension("out") };
    let art = PathBuf::from(out.file_stem().unwrap());

    let out_dir = if let Some(dir) = &sess.io.output_dir {
        fs::create_dir_all(dir).with_context(|| format!("{}", dir.display()))?;
        dir.clone()
    } else {
        parent_dir(&out).into()
    };
    let temps = sess.opts.Z.temps_dir.as_ref().map(PathBuf::from).unwrap_or(out_dir.clone());
    fs::create_dir_all(&temps).with_context(|| format!("{}", temps.display()))?;
    let out = out_dir.join(out);

    let name = input
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap_or_else(|| early.early_fatal("non UTF-8 file name"));
    let emit = temps.join(art.with_extension("emit"));

    let output = sess::OutputFilenames {
        out_directory: out_dir,
        file_stem: art.file_stem().unwrap().to_str().unwrap().to_owned(),
        single_output_file: None,
        temps_directory: temps,
        outputs: BTreeMap::new(),
    };

    // FIXME: strainge borrow checker error - suppress by leaking
    let arena = Box::leak(Box::new(WorkerLocal::new(|_| Default::default())));
    let tcx = TyCtx::enter(&arena, &compiler.sess, output);
    fs::write(&emit, driver_impl(&tcx, &src, name)?)
        .with_context(|| format!("{}", emit.display()))?;

    let out = Command::new("gcc").arg(emit).arg("-o").arg(out).output()?;
    if !out.stderr.is_empty() {
        print!("Linker out: {}", String::from_utf8_lossy(&out.stderr));
    }

    Ok(())
}

use crate::{
    cli::{Args, Emit},
    error::EarlyErrorHandler,
    interface::Config,
};

fn main() {
    use clap::Parser;

    let early = EarlyErrorHandler;

    let Args { input, output, output_dir, color, c_flags, z_flags, emit } = Args::parse();
    concolor::set(match color.unwrap_or(cli::Color::Auto) {
        cli::Color::Auto => ColorChoice::Auto,
        cli::Color::Always => ColorChoice::Always,
        cli::Color::Never => ColorChoice::Never,
    });

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
            if let Error::Error(err) = err {
                early.early_fatal(err.to_string())
            }
            process::exit(1)
        }
    });
}
