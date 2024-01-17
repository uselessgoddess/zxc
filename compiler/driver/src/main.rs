#![feature(let_chains, try_blocks, never_type, associated_type_bounds)]
#![allow(clippy::let_unit_value, clippy::manual_range_contains)]

mod cli;
mod codegen;
mod interface;
mod style;

use {
    anyhow::Context as _,
    concolor::{ColorChoice, Stream},
    cranelift::{codegen::isa, prelude::settings},
};

use middle::{
    ariadne::{self, Color, Label, Report, ReportKind},
    errors::{color, DynEmitter, EmitterWriter, Handler, SourceFile, SourceMap},
    hir::{self, HirCtx, Hx},
    mir::{self, InstanceDef},
    par::WorkerLocal,
    rayon::prelude::*,
    sess::{self, EarlyErrorHandler, Options},
    symbol::Symbol,
    ErrorGuaranteed, Tx, TyCtx,
};

use {
    chumsky::Parser,
    lexer::ParseBuffer,
    std::{
        collections::BTreeMap,
        fmt, fs, io,
        path::{Path, PathBuf},
        process,
        sync::Arc,
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

impl From<ErrorGuaranteed> for Error {
    fn from(_: ErrorGuaranteed) -> Self {
        Self::Guaranteed
    }
}

type Result<T, E = Error> = std::result::Result<T, E>;

fn parse_items<'lex>(
    tcx: Tx<'lex>,
    input: &mut ParseBuffer<'lex>,
) -> Result<Vec<hir::Item<'lex>>, lexer::Error> {
    let mut items = Vec::new();
    while !input.is_empty() {
        items.push(hir::Item::analyze(tcx, input.parse()?));
    }
    Ok(items)
}

fn driver_impl<'tcx>(
    tcx: Tx<'tcx>,
    hix: &'tcx mut HirCtx<'tcx>,
    src: &'tcx str,
    name: &'tcx str,
) -> Result<(), Error> {
    let mut input = ParseBuffer::new(lexer::lexer().parse(src).unwrap());

    let mut items = parse_items(tcx, &mut input).map_err(|err| {
        Report::build(ReportKind::Error, name, 5)
            .with_message(err.to_string())
            .with_label(Label::new((name, err.span.into_range())).with_color(Color::Red))
            .finish()
            .eprint((name, ariadne::Source::from(src)))
            .unwrap();
        Error::Guaranteed
    })?;

    let _: Result<_> = try {
        hir::analyze_module(hix, &mut items)?;
        hir::check::post_typeck(hix);
    };
    tcx.sess.abort_if_errors();
    hix.err.abort_if_errors();

    let cgu = hix.as_codegen_unit();
    let mir = cgu
        .items
        .par_iter()
        .map(|(def, _)| {
            let InstanceDef::Item(def) = def.def;
            (def, hix.optimized_mir(def))
        })
        .collect::<Vec<_>>();
    mir::pass::emit_mir(hix, &mir)?;

    let codegen = codegen::cranelift::CraneliftBackend;

    let ongoing = codegen.codegen_module(hix, &[cgu]);
    let results = codegen.join_codegen(tcx.sess, ongoing, tcx.output_filenames());
    let _ = codegen.link_binary(tcx.sess, results, tcx.output_filenames());

    Ok(())
}

fn parent_dir(path: &Path) -> &Path {
    if let Some(parent) = path.parent() { parent } else { path }
}

fn driver(
    early: &EarlyErrorHandler,
    compiler: &interface::Compiler,
    source_map: Arc<SourceMap>,
) -> Result<(), Error> {
    let sess = &compiler.sess;
    let input = &sess.io.input;
    let src = fs::read_to_string(input).with_context(|| format!("{}", input.display()))?;

    if concolor::get(Stream::Stdout).color() {
        use ariadne::Fmt;

        println!("{}", src.clone().fg(color::Magenta));
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

    let name = input
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap_or_else(|| early.early_error("non UTF-8 file name"));
    source_map.sources.write().push(SourceFile { name: PathBuf::from(input), src: src.to_owned() });

    let outputs = sess::OutputFilenames {
        out_directory: out_dir,
        file_stem: art.file_stem().unwrap().to_str().unwrap().to_owned(),
        single_output_file: None,
        temps_directory: Some(temps),
        outputs: BTreeMap::new(),
    };

    // FIXME: strainge borrow checker error - suppress by leaking
    let arena = Box::leak(Box::new(WorkerLocal::new(|_| Default::default())));
    let tcx = TyCtx::enter(arena, &compiler.sess, outputs);
    let mut hix = HirCtx::pure(
        &tcx,
        Symbol::intern("main"),
        Handler::with_emitter(default_emitter(source_map)),
    );
    driver_impl(&tcx, &mut hix, &src, name)?;

    Ok(())
}

fn default_emitter(source_map: Arc<SourceMap>) -> Box<DynEmitter> {
    Box::new(EmitterWriter::stderr(Some(source_map)))
}

use crate::{
    cli::{Args, Emit},
    codegen::ssa::CodegenBackend,
    interface::Config,
};

fn main() {
    use clap::Parser;

    let early = EarlyErrorHandler::new();

    let Args { input, output, output_dir, color, c_flags, z_flags, emit, target } = Args::parse();
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

    let source_map = Arc::new(SourceMap { sources: vec![].into() });
    let mut config = Config {
        opts: Options { Z: z_opts, C: c_opts, emit: emit_flags, ..Default::default() },
        input,
        output_dir,
        output_file: output,
        source_map: source_map.clone(),
    };
    if let Some(target) = target {
        config.opts.triple = target;
    }
    println!("{config:#?}");

    interface::run_compiler(config, |compiler| {
        if let Err(err) = driver(&early, compiler, source_map) {
            if let Error::Error(err) = err {
                early.early_error(err.to_string())
            }
            process::exit(101)
        }
    });
}
