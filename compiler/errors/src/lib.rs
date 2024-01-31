#![feature(negative_impls, never_type)]

mod code;
mod fatal;

use parking_lot::{Mutex, RwLock};

#[derive(Copy, Clone, Debug, PartialEq, Hash)]
pub enum Style {
    NoStyle,
    Level(Level),
}

impl Style {
    fn as_color(&self) -> Color {
        match self {
            Style::NoStyle => Color::Primary,
            Style::Level(level) => level.as_color(),
        }
    }
}

#[derive(Copy, PartialEq, Eq, Clone, Hash, Debug)]
pub enum Level {
    Bug,
    Fatal,
    Error { lint: bool },
    Warn,
    Note,
    Help,
    Allow,
}

#[allow(non_upper_case_globals)]
pub mod color {
    use yansi::Color;

    pub const Red: Color = Color::Rgb(239, 89, 111);
    pub const Cyan: Color = Color::Rgb(86, 182, 194);
    pub const Crab: Color = Color::Rgb(208, 154, 102);
    pub const Sand: Color = Color::Rgb(229, 192, 123);
    pub const Green: Color = Color::Rgb(137, 202, 120);
    pub const Magenta: Color = Color::Rgb(198, 120, 221);
}

impl Level {
    fn as_color(&self) -> Color {
        match self {
            Level::Bug | Level::Fatal | Level::Error { .. } => color::Red,
            Level::Warn => color::Sand,
            Level::Note => color::Cyan,
            Level::Help => color::Green,
            Level::Allow => unreachable!(),
        }
    }
}

impl Level {
    pub fn to_str(self) -> &'static str {
        match self {
            Level::Bug => "error: internal compiler error",
            Level::Fatal | Level::Error { .. } => "error",
            Level::Warn => "warning",
            Level::Note => "note",
            Level::Help => "help",
            Level::Allow => unreachable!(),
        }
    }

    pub fn underline(&self) -> char {
        match self {
            Level::Bug | Level::Fatal | Level::Error { .. } => '^',
            Level::Warn => '^',
            Level::Note => '~',
            Level::Help => '-',
            Level::Allow => unreachable!(),
        }
    }
}

use lexer::Span;

pub type DiagnosticMessage = Cow<'static, str>;

#[derive(Clone, Debug)]
pub struct SubDiagnostic {
    pub level: Level,
    pub message: (DiagnosticMessage, Style),
}

#[must_use]
#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub code: Option<Code>,
    pub(crate) level: Level,
    pub span: Option<Span>,
    pub message: (DiagnosticMessage, Style),
    pub labels: Vec<(DiagnosticMessage, Span)>,
    pub primary: Vec<(Option<DiagnosticMessage>, Option<Level>, Span)>,
    pub children: Vec<SubDiagnostic>,
}

impl Diagnostic {
    pub fn is_error(&self) -> bool {
        match self.level {
            Level::Bug | Level::Fatal | Level::Error { .. } => true,
            Level::Note | Level::Help | Level::Warn | Level::Allow => false,
        }
    }

    pub fn sub(&mut self, level: Level, message: DiagnosticMessage) {
        let sub = SubDiagnostic { level, message: (message, Style::NoStyle) };
        self.children.push(sub);
    }

    pub fn note(&mut self, msg: impl Into<DiagnosticMessage>) -> &mut Self {
        self.sub(Level::Note, msg.into());
        self
    }

    pub fn help(&mut self, msg: impl Into<DiagnosticMessage>) -> &mut Self {
        self.sub(Level::Help, msg.into());
        self
    }

    pub fn primary(
        &mut self,
        level: impl Into<Option<Level>>,
        span: Span,
        msg: impl Into<DiagnosticMessage>,
    ) -> &mut Self {
        self.primary.push((Some(msg.into()), level.into(), span));
        self
    }

    pub fn primary_span(&mut self, level: impl Into<Option<Level>>, span: Span) -> &mut Self {
        self.primary.push((None, level.into(), span));
        self
    }

    pub fn span_label(&mut self, span: Span, msg: impl Into<DiagnosticMessage>) -> &mut Self {
        self.labels.push((msg.into(), span));
        self
    }

    pub fn code(&mut self, code: Code) -> &mut Self {
        self.code = Some(code);
        self
    }
}

impl Diagnostic {
    #[track_caller]
    pub fn new_with_code(level: Level, message: DiagnosticMessage) -> Self {
        Diagnostic {
            code: None,
            level,
            span: None,
            message: (message, Style::NoStyle),
            labels: vec![],
            primary: vec![],
            children: vec![],
        }
    }
}

pub type DynEmitter = dyn Emitter + Send;

type Hash128 = u128;

use std::collections::HashSet;

pub struct HandlerInner {
    emitted_diagnostics: HashSet<Hash128>, // TODO: reorganize project to use `FxHashMap`
    emitter: Box<DynEmitter>,
    errors: usize,
    lints: usize,
}

pub struct Handler {
    inner: Mutex<HandlerInner>,
}

impl fmt::Debug for Handler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Handler {{ .. }}")
    }
}

impl HandlerInner {
    pub fn emit_diagnostic(&mut self, diagnostic: Diagnostic) -> Option<ErrorGuaranteed> {
        let mut guaranteed = None;

        if let Level::Allow = diagnostic.level {
            return None;
        }

        if diagnostic.is_error() {
            if let Level::Error { lint: true } = diagnostic.level {
                self.lints += 1;
            } else {
                self.errors += 1;
            }

            #[allow(deprecated)]
            {
                guaranteed = Some(ErrorGuaranteed::unchecked());
            }
        }

        self.emitter.emit_diagnostic(diagnostic);

        guaranteed
    }

    fn has_errors(&self) -> bool {
        self.errors > 0
    }

    fn err_count(&self) -> usize {
        self.errors
    }

    fn lint_count(&self) -> usize {
        self.lints
    }

    fn has_errors_or_lint_errors(&self) -> bool {
        self.has_errors() || self.lints > 0
    }

    fn abort_if_errors(&mut self) {
        if self.has_errors() {
            FatalError.raise();
        }
    }

    /// Emit an error; level should be `Error` or `Fatal`.
    fn emit(&mut self, level: Level, msg: impl Into<DiagnosticMessage>) -> ErrorGuaranteed {
        self.emit_diagnostic(Diagnostic::new_with_code(level, msg.into())).unwrap()
    }

    pub fn fatal(&mut self, msg: impl Into<DiagnosticMessage>) -> FatalError {
        self.emit(Level::Fatal, msg);
        FatalError
    }
}

impl Handler {
    pub fn with_emitter(emitter: Box<DynEmitter>) -> Self {
        Self {
            inner: Mutex::new(HandlerInner {
                emitted_diagnostics: Default::default(),
                emitter,
                errors: 0,
                lints: 0,
            }),
        }
    }

    pub fn has_errors_or_lint_errors(&self) -> Option<ErrorGuaranteed> {
        self.inner.lock().has_errors_or_lint_errors().then(|| {
            #[allow(deprecated)]
            ErrorGuaranteed::unchecked()
        })
    }

    pub fn emit_diagnostic(&self, diagnostic: Diagnostic) -> Option<ErrorGuaranteed> {
        self.inner.lock().emit_diagnostic(diagnostic)
    }

    pub fn emit_err<'a>(&'a self, err: impl IntoDiagnostic<'a>) -> ErrorGuaranteed {
        err.into_diagnostic(self).emit()
    }

    pub fn emit_almost_fatal<'a>(&'a self, err: impl IntoDiagnostic<'a, FatalError>) -> FatalError {
        err.into_diagnostic(self).emit()
    }

    pub fn emit_note<'a>(&'a self, err: impl IntoDiagnostic<'a, Noted>) -> Noted {
        err.into_diagnostic(self).emit()
    }

    pub fn struct_fatal(&self, msg: impl Into<DiagnosticMessage>) -> DiagnosticBuilder<'_, !> {
        DiagnosticBuilder::new_fatal(self, msg.into())
    }

    pub fn struct_err(
        &self,
        msg: impl Into<DiagnosticMessage>,
    ) -> DiagnosticBuilder<'_, ErrorGuaranteed> {
        DiagnosticBuilder::new_guaranteeing_error(self, msg.into())
    }

    pub fn struct_err_lint(&self, msg: impl Into<DiagnosticMessage>) -> DiagnosticBuilder<'_, ()> {
        DiagnosticBuilder::new(self, Level::Error { lint: true }, msg.into())
    }

    pub fn struct_warn(&self, msg: impl Into<DiagnosticMessage>) -> DiagnosticBuilder<'_, ()> {
        DiagnosticBuilder::new(self, Level::Warn, msg.into())
    }

    pub fn struct_diagnostic<E: Emission>(
        &self,
        msg: impl Into<DiagnosticMessage>,
    ) -> DiagnosticBuilder<'_, E> {
        E::make_guarantee(self, msg.into())
    }

    pub fn fatal(&self, msg: impl Into<DiagnosticMessage>) -> FatalError {
        self.inner.lock().fatal(msg)
    }

    pub fn abort_if_errors(&self) {
        let report = match self.err_count() {
            0 => return,
            1 => "aborting due to previous error".into(),
            n => format!("aborting due to {n} previous errors"),
        };
        self.struct_err(report).emit();
        self.inner.lock().abort_if_errors()
    }

    pub fn has_errors(&self) -> Option<ErrorGuaranteed> {
        self.inner.lock().has_errors().then(|| {
            #[allow(deprecated)]
            ErrorGuaranteed::unchecked()
        })
    }

    pub fn err_count(&self) -> usize {
        self.inner.lock().err_count()
    }

    pub fn lint_count(&self) -> usize {
        self.inner.lock().lint_count()
    }
}

pub trait Emitter {
    fn emit_diagnostic(&mut self, diag: Diagnostic);
    fn source_map(&self) -> Option<&Arc<SourceMap>>;
}

pub trait Emission: Sized {
    fn emit_guarantee(diagnostic: DiagnosticBuilder<'_, Self>) -> Self;
    fn make_guarantee(handler: &Handler, message: DiagnosticMessage)
    -> DiagnosticBuilder<'_, Self>;
}

impl Emission for ! {
    fn emit_guarantee(diagnostic: DiagnosticBuilder<'_, Self>) -> Self {
        diagnostic.handler.emit_diagnostic(diagnostic.diagnostic);
        FatalError.raise()
    }

    fn make_guarantee(
        handler: &Handler,
        message: DiagnosticMessage,
    ) -> DiagnosticBuilder<'_, Self> {
        DiagnosticBuilder::new_fatal(handler, message)
    }
}

impl Emission for () {
    fn emit_guarantee(diagnostic: DiagnosticBuilder<'_, Self>) -> Self {
        diagnostic.handler.emit_diagnostic(diagnostic.diagnostic);
    }

    fn make_guarantee(
        handler: &Handler,
        message: DiagnosticMessage,
    ) -> DiagnosticBuilder<'_, Self> {
        DiagnosticBuilder::new(handler, Level::Fatal, message)
    }
}

use std::{
    borrow::Cow,
    fmt,
    io::{self, Write},
    marker::PhantomData,
    ops::{Deref, DerefMut},
    path::PathBuf,
    sync::Arc,
};

pub struct DiagnosticBuilder<'h, E = ()> {
    pub handler: &'h Handler,
    pub diagnostic: Diagnostic,
    pub(crate) _marker: PhantomData<E>,
}

impl<E> Deref for DiagnosticBuilder<'_, E> {
    type Target = Diagnostic;

    fn deref(&self) -> &Self::Target {
        &self.diagnostic
    }
}

impl<E> DerefMut for DiagnosticBuilder<'_, E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.diagnostic
    }
}

impl<'a, E: Emission> DiagnosticBuilder<'a, E> {
    #[track_caller]
    pub fn emit(self) -> E {
        E::emit_guarantee(self)
    }
}

impl<'h> DiagnosticBuilder<'h, !> {
    #[track_caller]
    pub(crate) fn new_fatal(handler: &'h Handler, message: DiagnosticMessage) -> Self {
        Self {
            handler,
            diagnostic: Diagnostic::new_with_code(Level::Fatal, message),
            _marker: PhantomData,
        }
    }
}

impl<'h> DiagnosticBuilder<'h, ()> {
    pub(crate) fn new(handler: &'h Handler, level: Level, message: DiagnosticMessage) -> Self {
        Self {
            handler,
            diagnostic: Diagnostic::new_with_code(level, message),
            _marker: PhantomData,
        }
    }
}

#[derive(Copy, Clone)]
pub struct Noted;

impl<'a> DiagnosticBuilder<'a, Noted> {
    pub(crate) fn new_note(handler: &'a Handler, message: DiagnosticMessage) -> Self {
        Self {
            handler,
            diagnostic: Diagnostic::new_with_code(Level::Note, message),
            _marker: PhantomData,
        }
    }
}

impl Emission for Noted {
    fn emit_guarantee(diagnostic: DiagnosticBuilder<'_, Self>) -> Self {
        diagnostic.handler.emit_diagnostic(diagnostic.diagnostic);
        Noted
    }

    fn make_guarantee(
        handler: &Handler,
        message: DiagnosticMessage,
    ) -> DiagnosticBuilder<'_, Self> {
        DiagnosticBuilder::new_note(handler, message)
    }
}

pub struct EarlyErrorHandler {
    handler: Handler,
}

impl EarlyErrorHandler {
    pub fn new() -> Self {
        Self { handler: Handler::with_emitter(Box::new(EmitterWriter::stderr(None))) }
    }
}

impl EarlyErrorHandler {
    pub fn early_error(&self, msg: impl Into<DiagnosticMessage>) -> ! {
        self.handler.struct_fatal(msg).emit()
    }

    pub fn early_warn(&self, msg: impl Into<DiagnosticMessage>) {
        self.handler.struct_warn(msg).emit()
    }
}

impl Default for EarlyErrorHandler {
    fn default() -> Self {
        Self::new()
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
    sm: Option<Arc<SourceMap>>,
}

impl EmitterWriter {
    pub fn stderr(sm: Option<Arc<SourceMap>>) -> Self {
        Self { dest: Box::new(io::stderr()), sm }
    }
}

// Reversed log10
fn num_decimal_digits(num: usize) -> usize {
    #[cfg(target_pointer_width = "64")]
    const MAX_DIGITS: usize = 20;

    #[cfg(target_pointer_width = "32")]
    const MAX_DIGITS: usize = 10;

    #[cfg(target_pointer_width = "16")]
    const MAX_DIGITS: usize = 5;

    let mut lim = 10;
    for num_digits in 1..MAX_DIGITS {
        if num < lim {
            return num_digits;
        }
        lim = lim.wrapping_mul(10);
    }
    MAX_DIGITS
}

use ariadne::{Color, Config, Label, LabelAttach, Report, ReportKind, Source};

#[derive(Debug)]
pub struct SourceFile {
    pub name: PathBuf,
    pub src: String,
}

#[derive(Debug)]
pub struct SourceMap {
    pub sources: RwLock<Vec<SourceFile>>,
}

impl SourceMap {
    fn span_to_source<F, T>(&self, span: Span, extract_source: F) -> Option<T>
    where
        F: Fn(&str, usize, usize) -> Option<T>,
    {
        // FIXME: source map has exactly one file now
        let src = &self.sources.read()[0].src;
        extract_source(src, span.start, span.end)
    }

    pub fn span_to_snippet(&self, sp: Span) -> Option<String> {
        self.span_to_source(sp, |src, start, end| src.get(start..end).map(ToString::to_string))
    }

    pub fn span_to_filename(&self) -> String {
        self.sources.read()[0].name.file_name().unwrap().to_string_lossy().to_string()
    }

    pub fn span_to_content(&self) -> String {
        self.sources.read()[0].src.clone()
    }

    fn reloc_span(&self, span: Span) -> Span {
        span // we have only one src file
    }
}

impl EmitterWriter {
    fn emit_default(
        &mut self,
        code: Option<Code>,
        span: Option<Span>,
        level: Level,
        (message, style): &(DiagnosticMessage, Style),
        labels: &[(DiagnosticMessage, Span)],
        primary: &[(Option<DiagnosticMessage>, Option<Level>, Span)],
        children: &[SubDiagnostic],
        is_primary: bool,
    ) -> fmt::Result {
        use {ariadne::Fmt, std::fmt::Write};

        let (src_id, src) = if let Some(sm) = self.source_map() {
            // source map has only one file
            (sm.span_to_filename(), Source::from(sm.span_to_content()))
        } else {
            ("<unknown>".into(), Source::from(String::new()))
        };

        let color = level.as_color();
        let label = if !is_primary {
            format!("{}{}", "   = ", level.to_str())
        } else {
            level.to_str().to_string()
        };
        let primary_span = span
            .or_else(|| primary.first().map(|(_, _, s)| *s))
            .or_else(|| labels.first().map(|(_, s)| *s))
            .unwrap_or(Span::splat(0));
        let mut report =
            Report::build(ReportKind::Custom(&label, color), &src_id, primary_span.start)
                .with_config(Config::default().with_label_attach(LabelAttach::End));

        if let Some(code) = code {
            report = report.with_code(code);
        }
        if is_primary {
            report = report.with_bold(true);
        }

        for (message, span) in labels {
            let color = level.as_color();
            report.add_label(
                Label::new((&src_id, span.into_range()))
                    .with_underline(level.underline())
                    .with_color(color)
                    .with_message(message.fg(color)),
            );
        }
        for (message, local, span) in primary {
            let level = local.unwrap_or(level);
            let color = level.as_color();
            report.add_label({
                let mut label = Label::new((&src_id, span.into_range()))
                    .with_underline(level.underline())
                    .with_color(level.as_color());
                if let Some(message) = message {
                    label = label.with_message(message.fg(color));
                }
                label
            });
        }
        let msg = normalize_whitespace(message).fg(style.as_color());
        report.with_message(msg).finish().write((&src_id, src), &mut self.dest).unwrap();

        for children in children {
            self.emit_default(None, None, children.level, &children.message, &[], &[], &[], false)?;
        }

        Ok(())
    }
}

impl Emitter for EmitterWriter {
    fn emit_diagnostic(
        &mut self,
        Diagnostic { span, code, level, ref message, ref labels, ref primary, ref children }: Diagnostic,
    ) {
        self.emit_default(code, span, level, message, labels, primary, children, true).unwrap();
    }

    fn source_map(&self) -> Option<&Arc<SourceMap>> {
        self.sm.as_ref()
    }
}

pub trait IntoDiagnostic<'a, T: Emission = ErrorGuaranteed> {
    fn into_diagnostic(self, handler: &'a Handler) -> DiagnosticBuilder<'a, T>;
}

pub struct ErrorGuaranteed(());

impl ErrorGuaranteed {
    #[deprecated]
    pub(crate) fn unchecked() -> Self {
        Self(())
    }
}

impl Emission for ErrorGuaranteed {
    fn emit_guarantee(db: DiagnosticBuilder<'_, Self>) -> Self {
        assert!(
            db.diagnostic.is_error(),
            "emitted non-error ({:?}) diagnostic \
                     from `DiagnosticBuilder<ErrorGuaranteed>`",
            db.diagnostic.level
        );
        db.handler.emit_diagnostic(db.diagnostic).unwrap()
    }

    fn make_guarantee(
        handler: &Handler,
        message: DiagnosticMessage,
    ) -> DiagnosticBuilder<'_, Self> {
        DiagnosticBuilder::new_guaranteeing_error(handler, message)
    }
}

impl<'a> DiagnosticBuilder<'a, ErrorGuaranteed> {
    #[track_caller]
    pub(crate) fn new_guaranteeing_error<M: Into<DiagnosticMessage>>(
        handler: &'a Handler,
        message: M,
    ) -> Self {
        Self {
            handler,
            diagnostic: Diagnostic::new_with_code(Level::Error { lint: false }, message.into()),
            _marker: PhantomData,
        }
    }
}

pub use {
    ariadne,
    code::Code,
    fatal::{FatalError, FatalErrorMarker},
};
