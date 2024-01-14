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
    Note,
}

impl Level {
    pub fn to_str(self) -> &'static str {
        match self {
            Level::Bug => "error: internal compiler error",
            Level::Fatal | Level::Error { .. } => "error",
            Level::Note => "note",
        }
    }
}

#[derive(Clone, Debug)]
pub struct SubDiagnostic {
    pub level: Level,
    pub message: Vec<(String, Style)>,
}

#[must_use]
#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub(crate) level: Level,
    pub message: Vec<(String, Style)>,
    pub children: Vec<SubDiagnostic>,
}

impl Diagnostic {
    pub fn is_error(&self) -> bool {
        match self.level {
            Level::Bug | Level::Fatal | Level::Error => true,
            Level::Note => false,
        }
    }

    pub fn sub(&mut self, level: Level, message: String) {
        let sub = SubDiagnostic { level, message: vec![(message, Style::NoStyle)] };
        self.children.push(sub);
    }

    pub fn note(&mut self, msg: impl Into<String>) -> &mut Self {
        self.sub(Level::Note, msg.into());
        self
    }
}

impl Diagnostic {
    #[track_caller]
    pub fn new_with_code(level: Level, message: String) -> Self {
        Diagnostic { level, message: vec![(message.into(), Style::NoStyle)], children: vec![] }
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

    fn has_errors(&self) -> bool {
        self.errors > 0
    }

    fn abort_if_errors(&mut self) {
        if self.has_errors() {
            crate::FatalError.raise();
        }
    }

    fn emit(&mut self, level: Level, msg: impl Into<String>) {
        self.emit_diagnostic(Diagnostic::new_with_code(level, msg.into()))
    }

    pub fn fatal(&mut self, msg: impl Into<String>) -> FatalError {
        self.emit(Level::Fatal, msg);
        FatalError
    }
}

impl Handler {
    pub fn with_emitter(emitter: Box<DynEmitter>) -> Self {
        Self { inner: Lock::new(HandlerInner { emitter, errors: 0 }) }
    }

    pub fn emit_diagnostic(&self, diagnostic: Diagnostic) {
        self.inner.lock().emit_diagnostic(diagnostic)
    }

    pub fn emit_err<'a>(&'a self, err: impl IntoDiagnostic<'a>) {
        err.into_diagnostic(self).emit()
    }

    pub fn emit_note<'a>(&'a self, err: impl IntoDiagnostic<'a, Noted>) -> Noted {
        err.into_diagnostic(self).emit()
    }

    pub fn struct_fatal(&self, msg: impl Into<String>) -> DiagnosticBuilder<'_, !> {
        DiagnosticBuilder::new_fatal(self, msg.into())
    }

    pub fn struct_err(&self, msg: impl Into<String>) -> DiagnosticBuilder<'_, ()> {
        DiagnosticBuilder::new(self, Level::Error, msg.into())
    }

    pub fn struct_diagnostic<E: Emission>(
        &self,
        msg: impl Into<String>,
    ) -> DiagnosticBuilder<'_, E> {
        E::make_guarantee(self, msg.into())
    }

    pub fn fatal(&self, msg: impl Into<String>) -> FatalError {
        self.inner.lock().fatal(msg)
    }

    pub fn abort_if_errors(&self) {
        self.inner.lock().abort_if_errors()
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

use {
    crate::{par::Lock, FatalError},
    std::{
        fmt,
        io::{self, Write},
        marker::PhantomData,
        ops::{Deref, DerefMut},
    },
};

pub struct DiagnosticBuilder<'h, E> {
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

#[derive(Copy, Clone)]
pub struct Noted;

impl<'a> DiagnosticBuilder<'a, Noted> {
    pub(crate) fn new_note(handler: &'a Handler, message: String) -> Self {
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

    fn make_guarantee(handler: &Handler, message: String) -> DiagnosticBuilder<'_, Self> {
        DiagnosticBuilder::new_note(handler, message)
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

    fn msg_to_buffer(
        &self,
        buf: &mut Vec<u8>,
        msg: &[(String, Style)],
        padding: usize,
        label: &str,
        override_style: Option<Style>,
    ) -> io::Result<()> {
        let padding = " ".repeat(padding + label.len() + 5);

        fn style_or_override(style: Style, override_: Option<Style>) -> Style {
            match (style, override_) {
                (Style::NoStyle, Some(override_)) => override_,
                _ => style,
            }
        }

        let mut line_number = 0;
        for (text, _) in msg.iter() {
            let text = &normalize_whitespace(&text);
            let lines = text.split('\n').collect::<Vec<_>>();
            if lines.len() > 1 {
                for (i, line) in lines.iter().enumerate() {
                    if i != 0 {
                        line_number += 1;
                        write!(buf, "{padding}")?;
                    }
                    writeln!(buf, "{line}")?;
                }
            } else {
                writeln!(buf, "{text}")?;
            }
        }
        Ok(())
    }

    fn emit_diagnostic_default(
        &mut self,
        msg: &[(String, Style)],
        level: Level,
        max_line_num_len: usize,
        is_secondary: bool,
    ) -> io::Result<()> {
        let mut buf = Vec::with_capacity(32);

        if is_secondary {
            write!(buf, " ")?;
            write!(buf, "{}: ", level.to_str())?;
            self.msg_to_buffer(&mut buf, msg, max_line_num_len, "note", None)?;
        } else {
            write!(buf, "{}: ", level.to_str())?;

            for (text, _style) in msg.iter() {
                let text = &normalize_whitespace(&text);
                for line in text.lines() {
                    write!(buf, "{line}")?;
                }
            }
            writeln!(buf)?;
        }
        self.dest.write(&buf)?;

        //let child = diag.children;
        //for (pos, line) in rendered_buffer.iter().enumerate() {
        //    for part in line {
        //        let style = part.style.color_spec(*lvl);
        //        dst.set_color(&style)?;
        //        write!(dst, "{}", part.text)?;
        //        dst.reset()?;
        //    }
        //    if !short_message && (!lvl.is_failure_note() || pos != rendered_buffer.len() - 1) {
        //        writeln!(dst)?;
        //    }
        //}

        Ok(())
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

impl Emitter for EmitterWriter {
    fn emit_diagnostic(&mut self, diag: Diagnostic) {
        let max_line_num_len = 2;

        match self.emit_diagnostic_default(&diag.message, diag.level, max_line_num_len, false) {
            Ok(_) => {
                if !diag.children.is_empty() {
                    for child in diag.children {
                        if let Err(err) = self.emit_diagnostic_default(
                            &child.message,
                            child.level,
                            max_line_num_len,
                            true,
                        ) {
                            panic!("failed to emit error: {err}");
                        }
                    }
                }
            }
            Err(err) => panic!("failed to emit error: {err}"),
        }
    }
}

pub trait IntoDiagnostic<'a, T: Emission = ()> {
    fn into_diagnostic(self, handler: &'a Handler) -> DiagnosticBuilder<'a, T>;
}
