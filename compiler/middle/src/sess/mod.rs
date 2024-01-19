mod diagnostic;
mod errors;
pub mod output;

use std::{collections::BTreeMap, ffi::OsStr, sync::Arc};
pub use {
    diagnostic::{
        Diagnostic, DiagnosticBuilder, DynEmitter, EarlyErrorHandler, Emission, Emitter,
        EmitterWriter, Handler, IntoDiagnostic, Level, Noted, Style, SubDiagnostic,
    },
    output::{
        check_file_is_writeable, filename_for_input, out_filename, OutFileName, OutputFilenames,
        OutputType,
    },
};

use {
    crate::{
        spec,
        spec::{RelocModel, Target},
    },
    ::errors::{DiagnosticMessage, ErrorGuaranteed, SourceMap},
    bitflags::bitflags,
    std::{
        env,
        fmt::{self, Formatter},
        path::PathBuf,
    },
};

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
pub enum OptLevel {
    No,         // -O0
    Less,       // -O1
    Default,    // -O2
    Aggressive, // -O3
    Size,       // -Os
    SizeMin,    // -Oz
}

#[derive(Copy, PartialEq, PartialOrd, Clone, Ord, Eq, Hash, Debug)]
pub enum ModuleType {
    Executable,
    Dylib,
    Staticlib,
}

#[allow(dead_code)]
mod parse {
    use {
        crate::sess::OptLevel,
        std::{path::PathBuf, str::FromStr},
    };

    pub fn parse_bool(slot: &mut bool, v: Option<&str>) -> bool {
        match v {
            Some("y") | Some("yes") | Some("on") | Some("true") | None => {
                *slot = true;
                true
            }
            Some("n") | Some("no") | Some("off") | Some("false") => {
                *slot = false;
                true
            }
            _ => false,
        }
    }

    pub fn parse_number<T: Copy + FromStr>(slot: &mut T, v: Option<&str>) -> bool {
        match v.and_then(|s| s.parse().ok()) {
            Some(i) => {
                *slot = i;
                true
            }
            None => false,
        }
    }

    pub fn parse_opt_level(slot: &mut OptLevel, v: Option<&str>) -> bool {
        match v {
            Some(opt) => {
                *slot = match opt {
                    "0" => OptLevel::No,
                    "1" => OptLevel::Less,
                    "2" => OptLevel::Default,
                    "3" => OptLevel::Aggressive,
                    "s" => OptLevel::Size,
                    "z" => OptLevel::SizeMin,
                    _ => return false,
                };
                true
            }
            None => false,
        }
    }

    pub fn parse_opt_string(slot: &mut Option<String>, v: Option<&str>) -> bool {
        match v {
            Some(s) => {
                *slot = Some(s.to_string());
                true
            }
            None => false,
        }
    }

    pub fn parse_opt_number<T: Copy + FromStr>(slot: &mut Option<T>, v: Option<&str>) -> bool {
        match v {
            Some(s) => {
                *slot = s.parse().ok();
                slot.is_some()
            }
            None => false,
        }
    }

    pub fn parse_list_with_polarity(slot: &mut Vec<(String, bool)>, v: Option<&str>) -> bool {
        match v {
            Some(s) => {
                for s in s.split(',') {
                    let Some(pass_name) = s.strip_prefix(&['+', '-'][..]) else { return false };
                    slot.push((pass_name.to_string(), &s[..1] == "+"));
                }
                true
            }
            None => false,
        }
    }

    pub fn parse_opt_pathbuf(slot: &mut Option<PathBuf>, v: Option<&str>) -> bool {
        match v {
            Some(s) => {
                *slot = Some(PathBuf::from(s));
                true
            }
            None => false,
        }
    }
}

#[allow(non_upper_case_globals, dead_code)]
mod desc {
    pub const parse_string: &str = "a string";
    pub const parse_opt_string: &str = parse_string;
    pub const parse_opt_pathbuf: &str = "a path";
    pub const parse_bool: &str = "one of: `y`, `yes`, `on`, `true`, `n`, `no`, `off` or `false`";
    pub const parse_number: &str = "a number";
    pub const parse_opt_number: &str = parse_number;
    pub const parse_opt_level: &str = "optimization level needs to be between 0-3, s or z)";
    pub const parse_list_with_polarity: &str =
        "a comma-separated list of strings, with elements beginning with + or -";
}

pub type OptionSetter<O> = fn(&mut O, v: Option<&str>) -> bool;
pub type OptionDescrs<O> = &'static [(&'static str, OptionSetter<O>, &'static str, &'static str)];

macro_rules! options {
    ($struct_name:ident, $stat:ident, $optmod:ident,
     $($( #[$attr:meta] )* $opt:ident : $t:ty = (
        $init:expr,
        $parse:ident,
        $desc:expr)
     ),* ,) => (

        #[derive(Clone, Debug)]
        pub struct $struct_name { $( $( #[$attr] )* pub $opt: $t),* }

        impl Default for $struct_name {
            fn default() -> $struct_name {
                $struct_name { $($opt: $init),* }
            }
        }

        pub const $stat: OptionDescrs<$struct_name> =
            &[ $( (stringify!($opt), $optmod::$opt, desc::$parse, $desc) ),* ];

        mod $optmod {$(
            pub(super) fn $opt(cg: &mut super::$struct_name, v: Option<&str>) -> bool {
                super::parse::$parse(&mut cg.$opt, v)
            }
        )*}
    );
}

options! {
    CompilerOptions, Z_OPTIONS, zopts,

    threads: usize = (0, parse_number, "use a thread pool with N threads"),
    temps_dir: Option<String> = (None, parse_opt_string,
        "the directory the intermediate files are written to"),
}

options! {
    CodegenOptions, C_OPTIONS, copts,

    linker: Option<PathBuf> = (None, parse_opt_pathbuf,
        "system linker to link outputs with"),
    opt_level: OptLevel = (OptLevel::No, parse_opt_level,
        "optimization level (0-3, s, or z; default: 0)"),
    mir_opt_level: Option<usize> = (None, parse_opt_number,
        "MIR optimization level (0-2; default determined by `opt_level`)"),
    mir_enable_passes: Vec<(String, bool)> = (Vec::new(), parse_list_with_polarity,
        "use like `-Zmir-enable-passes=+SimplifyBranches,-Simplify`. Forces the \
        specified passes to be enabled, overriding all other checks. In particular, this will \
        enable unsound (known-buggy and hence usually disabled) passes without further warning! \
        Passes that are not specified are enabled or disabled by other flags as usual."),
}

#[allow(non_snake_case)]
#[derive(Debug)]
pub struct Options {
    pub Z: CompilerOptions,
    pub C: CodegenOptions,
    pub triple: String,
    pub module_types: Vec<ModuleType>,
    pub output_types: BTreeMap<OutputType, Option<OutFileName>>,
}

pub fn host_triple() -> String {
    target_lexicon::HOST.to_string()
}

impl Default for Options {
    fn default() -> Self {
        Self {
            Z: Default::default(),
            C: Default::default(),
            triple: host_triple(),
            module_types: Vec::new(),
            output_types: Default::default(),
        }
    }
}

#[derive(Debug)]
pub struct Session {
    pub target: Target,
    pub host: Target,
    pub opts: Options,

    pub io: CompilerIO,
    handler: Handler,
}

impl Session {
    pub fn diagnostic(&self) -> &Handler {
        &self.handler
    }

    pub fn abort_if_errors(&self) {
        self.diagnostic().abort_if_errors()
    }

    pub fn emit_err<'a>(&'a self, err: impl IntoDiagnostic<'a>) -> ErrorGuaranteed {
        self.diagnostic().emit_err(err)
    }

    pub fn emit_note<'a>(&'a self, err: impl IntoDiagnostic<'a, Noted>) -> Noted {
        self.diagnostic().emit_note(err)
    }

    pub fn fatal(&self, msg: impl Into<DiagnosticMessage>) -> ! {
        self.handler.fatal(msg).raise()
    }

    pub fn mir_opt_level(&self) -> usize {
        self.opts
            .C
            .mir_opt_level
            .unwrap_or_else(|| if self.opts.C.opt_level != OptLevel::No { 2 } else { 1 })
    }

    pub fn emit_fatal<'a>(&'a self, fatal: impl IntoDiagnostic<'a, !>) -> ! {
        fatal.into_diagnostic(&self.handler).emit()
    }

    #[rustfmt::skip]
    pub fn inner_tools_path(&self, self_contained: bool) -> Vec<PathBuf> {
        let p = PathBuf::from_iter([
            env::current_exe().unwrap()
        ]);
        if self_contained { vec![p.clone(), p.join("self-contained")] } else { vec![p] }
    }

    pub fn relocation_model(&self) -> RelocModel {
        self.target.relocation_model
    }

    pub fn crt_static(&self) -> bool {
        if !self.target.crt_static_respected {
            // If the target does not opt in to crt-static support, use its default.
            return self.target.crt_static_default;
        }

        // let requested_features = self.opts.cg.target_feature.split(',');
        // let found_negative = requested_features.clone().any(|r| r == "-crt-static");
        // let found_positive = requested_features.clone().any(|r| r == "+crt-static");

        let found_negative = false;
        let found_positive = false;

        if found_positive || found_negative {
            found_positive
        } else {
            self.target.crt_static_default
        }
    }

    pub fn local_module_name(&self) -> String {
        let validate = |s: String| {
            validate_module_name(self, &s);
            s
        };

        if let Some(s) = self.io.input.file_stem().and_then(OsStr::to_str) {
            if s.starts_with('-') {
                self.emit_err(errors::ModuleNameInvalid { name: s });
            } else {
                return validate(s.replace('-', "_"));
            }
        }

        "zxc_out".into()
    }
}

pub fn validate_module_name(sess: &Session, s: &str) {
    let mut err_count = 0;
    {
        if s.is_empty() {
            err_count += 1;
            sess.emit_err(errors::ModuleNameEmpty);
        }
        for c in s.chars() {
            if c.is_alphanumeric() || c == '_' {
                continue;
            }
            err_count += 1;
            sess.emit_err(errors::InvalidCharacterInModuleName { character: c, module_name: s });
        }
    }

    if err_count > 0 {
        sess.abort_if_errors();
    }
}

#[derive(Debug)]
pub struct CompilerIO {
    pub input: PathBuf,
    pub output_dir: Option<PathBuf>,
    pub output_file: Option<PathBuf>,
    pub temps_dir: Option<PathBuf>,
}

pub fn build_session(opts: Options, io: CompilerIO, sm: Arc<SourceMap>) -> Session {
    let host = Target::expect_builtin(&host_triple()).unwrap();

    let emitter = Box::new(EmitterWriter::stderr(Some(sm)));
    Session { target: host.clone(), host, opts, io, handler: Handler::with_emitter(emitter) }
}
