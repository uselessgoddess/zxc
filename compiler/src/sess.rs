use {
    crate::{spec, spec::Target},
    bitflags::bitflags,
    std::{
        collections::BTreeMap,
        fmt::{self, Formatter},
        io::{self, Write},
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

mod parse {
    use {crate::sess::OptLevel, std::str::FromStr};

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
}

#[allow(non_upper_case_globals)]
mod desc {
    pub const parse_string: &str = "a string";
    pub const parse_opt_string: &str = parse_string;
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

bitflags! {
    #[derive(Copy, Clone)]
    pub struct Emit: u8 {
        const None = 0;
        const IR   = 1 << 0;
        const MIR  = 1 << 1;
    }
}

impl Emit {
    fn fmt_inner(self) -> &'static str {
        const NONE: u8 = Emit::bits(&Emit::None);
        const MIR: u8 = Emit::bits(&Emit::MIR);
        const IR: u8 = Emit::bits(&Emit::IR);

        match self.0.0 {
            NONE => "None",
            MIR => "Mir",
            IR => "IR",
            _ => unreachable!(),
        }
    }
}

impl fmt::Debug for Emit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut flags = self.iter();
        write!(f, "Emit(")?;
        if let Some(first) = flags.next() {
            f.write_str(first.fmt_inner())?;
            for elem in flags {
                f.write_str("|")?;
                f.write_str(elem.fmt_inner())?;
            }
        } else {
            f.write_str("None")?;
        }

        write!(f, ")")
    }
}

#[allow(non_snake_case)]
#[derive(Debug)]
pub struct Options {
    pub Z: CompilerOptions,
    pub C: CodegenOptions,
    pub emit: Emit,
}

impl Default for Options {
    fn default() -> Self {
        Self { Z: Default::default(), C: Default::default(), emit: Emit::None }
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
    pub fn mir_opt_level(&self) -> usize {
        self.opts
            .C
            .mir_opt_level
            .unwrap_or_else(|| if self.opts.C.opt_level != OptLevel::No { 2 } else { 1 })
    }

    pub fn emit_fatal<'a>(&'a self, fatal: impl IntoDiagnostic<'a, !>) -> ! {
        fatal.into_diagnostic(&self.handler).emit()
    }
}

#[derive(Debug)]
pub struct CompilerIO {
    pub input: PathBuf,
    pub output_dir: Option<PathBuf>,
    pub output_file: Option<PathBuf>,
    pub temps_dir: Option<PathBuf>,
}

pub fn build_session(opts: Options, io: CompilerIO) -> Session {
    let host = spec::targets::x86_64_windows_gnu::target();

    Session {
        target: host.clone(),
        host,
        opts,
        io,
        handler: Handler::with_emitter(Box::new(EmitterWriter::stderr())),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum OutputType {
    Bitcode,
    Mir,
    Object,
    Exe,
}

impl OutputType {
    pub fn extension(&self) -> &'static str {
        match *self {
            OutputType::Bitcode => "bc",
            OutputType::Mir => "mir",
            OutputType::Object => "o",
            OutputType::Exe => "",
        }
    }
}

#[derive(Debug, Clone)]
pub enum OutFileName {
    Real(PathBuf),
    Stdout,
}

pub struct OutputFilenames {
    pub out_directory: PathBuf,
    pub file_stem: String,
    pub single_output_file: Option<OutFileName>,
    pub temps_directory: PathBuf,
    pub outputs: BTreeMap<OutputType, OutFileName>,
}

impl OutputFilenames {
    pub fn path(&self, flavor: OutputType) -> OutFileName {
        self.outputs
            .get(&flavor)
            .map(|p| p.to_owned())
            .or_else(|| self.single_output_file.clone())
            .unwrap_or_else(|| OutFileName::Real(self.output_path(flavor)))
    }

    pub fn output_path(&self, flavor: OutputType) -> PathBuf {
        self.with_directory_and_extension(&self.out_directory, flavor.extension())
    }

    fn with_directory_and_extension(&self, directory: &PathBuf, extension: &str) -> PathBuf {
        let mut path = directory.join(&self.file_stem);
        path.set_extension(extension);
        path
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Hash)]
pub enum Style {
    NoStyle,
    Level(Level),
}

#[derive(Copy, PartialEq, Eq, Clone, Hash, Debug)]
pub enum Level {
    Bug,
    Fatal,
    Error,
}

impl Level {
    pub fn to_str(self) -> &'static str {
        match self {
            Level::Bug => "error: internal compiler error",
            Level::Fatal | Level::Error { .. } => "error",
        }
    }
}

#[must_use]
#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub(crate) level: Level,
    pub message: Vec<(String, Style)>,
}

impl Diagnostic {
    pub fn is_error(&self) -> bool {
        match self.level {
            Level::Bug | Level::Fatal | Level::Error => true,
        }
    }
}

impl Diagnostic {
    #[track_caller]
    pub fn new_with_code(level: Level, message: String) -> Self {
        Diagnostic { level, message: vec![(message.into(), Style::NoStyle)] }
    }
}

pub type DynEmitter = dyn Emitter + Send;

struct HandlerInner {
    emitter: Box<DynEmitter>,
    errors: usize,
}

pub struct Handler {
    inner: Lock<HandlerInner>,
}

impl fmt::Debug for Handler {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Handler {{ .. }}")
    }
}

impl HandlerInner {
    pub fn emit_diagnostic(&mut self, diagnostic: Diagnostic) {
        if diagnostic.is_error() {
            self.errors += 1;
        }

        self.emitter.emit_diagnostic(diagnostic);
    }
}

impl Handler {
    pub fn with_emitter(emitter: Box<DynEmitter>) -> Self {
        Self { inner: Lock::new(HandlerInner { emitter, errors: 0 }) }
    }

    pub fn emit_diagnostic(&self, diagnostic: Diagnostic) {
        self.inner.lock().emit_diagnostic(diagnostic)
    }

    pub fn struct_fatal(&self, msg: impl Into<String>) -> DiagnosticBuilder<'_, !> {
        DiagnosticBuilder::new_fatal(self, msg.into())
    }
}

pub trait Emitter {
    fn emit_diagnostic(&mut self, diag: Diagnostic);
}

pub trait Emission: Sized {
    fn emit_guarantee(diagnostic: DiagnosticBuilder<'_, Self>) -> Self;
    fn make_guarantee(handler: &Handler, message: String) -> DiagnosticBuilder<'_, Self>;
}

impl Emission for ! {
    fn emit_guarantee(diagnostic: DiagnosticBuilder<'_, Self>) -> Self {
        diagnostic.handler.emit_diagnostic(diagnostic.diagnostic);

        crate::FatalError.raise()
    }

    fn make_guarantee(handler: &Handler, message: String) -> DiagnosticBuilder<'_, Self> {
        DiagnosticBuilder::new_fatal(handler, message)
    }
}

impl Emission for () {
    fn emit_guarantee(diagnostic: DiagnosticBuilder<'_, Self>) -> Self {
        diagnostic.handler.emit_diagnostic(diagnostic.diagnostic);
    }

    fn make_guarantee(handler: &Handler, message: String) -> DiagnosticBuilder<'_, Self> {
        DiagnosticBuilder::new(handler, Level::Fatal, message)
    }
}

use {crate::par::Lock, std::marker::PhantomData};

pub struct DiagnosticBuilder<'h, E: Emission> {
    handler: &'h Handler,
    diagnostic: Diagnostic,
    _marker: PhantomData<E>,
}

impl<'a, E: Emission> DiagnosticBuilder<'a, E> {
    #[track_caller]
    pub fn emit(self) -> E {
        E::emit_guarantee(self)
    }
}

impl<'h> DiagnosticBuilder<'h, !> {
    #[track_caller]
    pub(crate) fn new_fatal(handler: &'h Handler, message: String) -> Self {
        Self {
            handler,
            diagnostic: Diagnostic::new_with_code(Level::Fatal, message),
            _marker: PhantomData,
        }
    }
}

impl<'h> DiagnosticBuilder<'h, ()> {
    pub(crate) fn new(handler: &'h Handler, level: Level, message: String) -> Self {
        Self {
            handler,
            diagnostic: Diagnostic::new_with_code(level, message),
            _marker: PhantomData,
        }
    }
}

pub struct EarlyErrorHandler {
    handler: Handler,
}

impl EarlyErrorHandler {
    pub fn new() -> Self {
        Self { handler: Handler::with_emitter(Box::new(EmitterWriter::stderr())) }
    }
}

impl EarlyErrorHandler {
    pub fn early_error(&self, msg: impl Into<String>) -> ! {
        self.handler.struct_fatal(msg).emit()
    }
}

fn normalize_whitespace(str: &str) -> String {
    const REPLACEMENTS: &[(char, &str)] = &[
        ('\t', "    "),
        ('\u{200D}', ""),
        ('\u{202A}', ""),
        ('\u{202B}', ""),
        ('\u{202D}', ""),
        ('\u{202E}', ""),
        ('\u{2066}', ""),
        ('\u{2067}', ""),
        ('\u{2068}', ""),
        ('\u{202C}', ""),
        ('\u{2069}', ""),
    ];

    let mut s = str.to_string();
    for (c, replacement) in REPLACEMENTS {
        s = s.replace(*c, replacement);
    }
    s
}

pub struct EmitterWriter {
    dest: Box<dyn Write + Send>,
}

impl EmitterWriter {
    pub fn stderr() -> Self {
        Self { dest: Box::new(io::stderr()) }
    }

    fn emit_diagnostic_default(&mut self, diag: Diagnostic) -> io::Result<()> {
        let mut buf = Vec::with_capacity(32);

        write!(buf, "{}: ", diag.level.to_str())?;

        for (text, _style) in diag.message.iter() {
            let text = &normalize_whitespace(&text);
            for line in text.lines() {
                write!(buf, "{line}")?;
            }
        }
        writeln!(buf)?;

        self.dest.write(&buf)?;
        Ok(())
    }
}

impl Emitter for EmitterWriter {
    fn emit_diagnostic(&mut self, diag: Diagnostic) {
        match self.emit_diagnostic_default(diag) {
            Ok(_) => {}
            Err(err) => panic!("failed to emit error: {err}"),
        }
    }
}

pub trait IntoDiagnostic<'a, T: Emission = ()> {
    fn into_diagnostic(self, handler: &'a Handler) -> DiagnosticBuilder<'a, T>;
}
