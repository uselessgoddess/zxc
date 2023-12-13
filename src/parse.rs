use {
    crate::{lexer::Token, Lex, Span},
    std::{
        error,
        fmt::{self, Formatter},
        mem::{self, MaybeUninit},
    },
};

pub enum Sealed {}

pub trait Peek {
    type Token: Token;
}

// TODO(doc): Add notes about this
impl<T: Token, F: FnOnce(Sealed) -> T> Peek for F {
    type Token = T;
}

#[derive(Debug)]
pub enum ErrorKind {
    Eof,
    Expect(&'static str),
    Expected { expected: Vec<&'static str>, found: &'static str },
    Custom(String),
}

#[derive(Debug)]
pub struct Error {
    pub repr: String,
    pub span: Span,
}

impl Error {
    pub fn new(repr: String, span: Span) -> Self {
        Self { repr, span }
    }

    pub fn eof(span: Span) -> Self {
        Self::new("end of input".to_string(), span)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.repr)
    }
}

impl error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;

pub trait Parse<'lex>: Sized {
    fn parse(input: &mut ParseStream<'_, 'lex>) -> Result<Self>;
}

#[rustfmt::skip]
mod hint {
    pub unsafe fn outlive<'ex, T: ?Sized>(ptr: *const T) -> &'ex T { &*ptr }
    pub unsafe fn outlive_mut<'ex, T: ?Sized>(ptr: *mut T) -> &'ex mut T { &mut *ptr }
}

pub struct ParseStream<'lex, 'place: 'lex> {
    step: unsafe fn(&MaybeUninit<(Lex<'lex>, Span)>) -> (Lex<'lex>, Span),
    tokens: &'place mut [MaybeUninit<(Lex<'lex>, Span)>],
    cursor: usize,
    span: Span,
}

impl<'lex, 'place: 'lex> ParseStream<'lex, 'place> {
    pub fn new(tokens: &'place mut [(Lex<'lex>, Span)]) -> Self {
        // Safety: safe because works as coercion `Lex` -> `MaybeUninit<Lex>`
        // https://doc.rust-lang.org/std/mem/union.MaybeUninit.html#impl-MaybeUninit%3C%5BT;+N%5D%3E
        let tokens = unsafe { mem::transmute(tokens) };
        Self { step: MaybeUninit::assume_init_read, tokens, cursor: 0, span: Span::splat(0) }
    }

    pub fn step<F, P>(&mut self, parse: F) -> Result<P>
    where
        F: FnOnce(&mut ParseStream<'lex, 'place>) -> Result<P>,
    {
        let mut rest = unsafe {
            ParseStream {
                step: |lex| MaybeUninit::assume_init_ref(lex).clone(),
                tokens: hint::outlive_mut(self.tokens),
                cursor: self.cursor,
                span: self.span,
            }
        };

        let parsed = parse(&mut rest)?;
        self.cursor = rest.cursor;
        Ok(parsed)
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn error(&self, message: impl fmt::Display) -> Error {
        Error::new(message.to_string(), self.span())
    }

    pub fn current(&self) -> Option<&'place (Lex<'lex>, Span)> {
        // Safety: we unitialize after `next_lex`
        unsafe {
            self.tokens
                .get(self.cursor)
                .map(|cell| hint::outlive(MaybeUninit::assume_init_ref(cell)))
        }
    }

    pub fn peek<P: Peek>(&self, peek: P) -> bool {
        let _ = peek;
        if let Some((lex, _)) = self.current() { P::Token::peek(lex) } else { false }
    }

    pub fn parse<P: Parse<'lex>>(&mut self) -> Result<P> {
        P::parse(self)
    }

    pub(crate) fn next_lex(&mut self) -> Result<(Lex<'lex>, Span)> {
        if self.cursor == self.tokens.len() {
            Err(Error::eof(self.span))
        } else {
            self.cursor += 1;
            // Safety: unitialize after step next
            unsafe {
                let cell = self.tokens.get(self.cursor - 1).unwrap_or_else(|| todo!());
                Ok((self.step)(cell)).inspect(|(_, span)| self.span = *span)
            }
        }
    }
}
