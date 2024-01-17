use {
    middle::{
        errors::SourceMap,
        par,
        rayon::ThreadPoolBuilder,
        sess::{self, CompilerIO, Options},
        tls::{self, SessionGlobals},
        Session,
    },
    std::{path::PathBuf, sync::Arc},
};

#[derive(Debug)]
pub struct Compiler {
    pub(crate) sess: Arc<Session>,
    // codegen_backend: Arc<dyn CodegenBackend>,
}

#[derive(Debug)]
pub struct Config {
    pub opts: Options,
    pub input: PathBuf,
    pub output_dir: Option<PathBuf>,
    pub output_file: Option<PathBuf>,
    pub source_map: Arc<SourceMap>,
}

pub fn run_thread_pool_with_globals<F: FnOnce() -> R + Send, R: Send>(threads: usize, f: F) -> R {
    let threads = if threads == 0 { num_cpus::get() } else { threads };

    let registry = par::Registry::new(threads);

    let globals = &SessionGlobals::default();
    ThreadPoolBuilder::new()
        .thread_name(|_| "zxc".into())
        .num_threads(threads)
        .build_scoped(
            |thread| {
                registry.register();
                unsafe { tls::in_session_globals(globals, || thread.run()) }
            },
            |pool| pool.install(f),
        )
        .unwrap()
}

pub fn run_compiler<R: Send>(config: Config, f: impl FnOnce(&Compiler) -> R + Send) -> R {
    let temps_dir = config.opts.Z.temps_dir.as_deref().map(PathBuf::from);

    run_thread_pool_with_globals(config.opts.Z.threads, || {
        f(&Compiler {
            sess: Arc::new(sess::build_session(
                config.opts,
                CompilerIO {
                    input: config.input,
                    output_dir: config.output_dir,
                    output_file: config.output_file,
                    temps_dir,
                },
                config.source_map,
            )),
        })
    })
}
