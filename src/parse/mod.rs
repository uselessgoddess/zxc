mod delim;
mod expr;

use {
    crate::{lexer::Token, Lex, Span},
    std::{
        error,
        fmt::{self, Formatter},
        mem::{self, MaybeUninit},
    },
};

pub enum PhantomPeek {}

pub trait Peek: Copy {
    type Token: Token;
}

// TODO(doc): Add notes about this
impl<T: Token, F: FnOnce(PhantomPeek) -> T + Copy> Peek for F {
    type Token = T;
}

macro_rules! define_token {
    { $($pat:pat_param in $ty:ident $(<$($lifetimes:lifetime),+>)? => $display:literal)* } => {$(
        impl$(<$($lifetimes),+>)? Token for $ty$(<$($lifetimes),+>)? {
            fn peek(lex: &Lex<'_>) -> bool {
                matches!(lex, $pat)
            }

            fn display() -> &'static str {
                $display
            }
        }

        #[allow(non_snake_case)]
        pub fn $ty $(<$($lifetimes),+>)?(peek: $crate::parse::PhantomPeek)
            -> $ty$(<$($lifetimes),+>)? {
            match peek {}
        }
    )*};
}

pub(crate) use define_token;

#[derive(Debug)]
pub enum ErrorKind {
    Eof,
    Expect(&'static str),
    Expected { expected: Vec<&'static str>, found: &'static str },
    Custom(String),
}

#[derive(Debug, Clone)]
pub struct Error {
    pub repr: String,
    pub span: Span,
}

impl Error {
    pub fn new(span: Span, repr: impl fmt::Display) -> Self {
        Self { repr: repr.to_string(), span }
    }

    pub fn eof(span: Span) -> Self {
        Self::new(span, "end of input")
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.repr)
    }
}

impl error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;

pub trait Parse<'lex>: Sized + 'lex {
    fn parse(input: &mut ParseStream<'lex, 'lex>) -> Result<Self>;
}

#[rustfmt::skip]
mod hint {
    pub unsafe fn outlive<'ex, T: ?Sized>(ptr: *const T) -> &'ex T { &*ptr }
    pub unsafe fn outlive_mut<'ex, T: ?Sized>(ptr: *mut T) -> &'ex mut T { &mut *ptr }
}

type Walker<'lex> = unsafe fn(&MaybeUninit<(Lex<'lex>, Span)>) -> (Lex<'lex>, Span);

unsafe fn clone_walker<'lex>(lex: &MaybeUninit<(Lex<'lex>, Span)>) -> (Lex<'lex>, Span) {
    MaybeUninit::assume_init_ref(lex).clone()
}

pub struct ParseStream<'lex, 'place: 'lex> {
    walker: Walker<'lex>,
    tokens: &'place mut [MaybeUninit<(Lex<'lex>, Span)>],
    cursor: usize,
    span: Span,
}

impl<'lex, 'place: 'lex> ParseStream<'lex, 'place> {
    pub fn new(tokens: &'place mut [(Lex<'lex>, Span)]) -> Self {
        // Safety: safe because works as coercion `Lex` -> `MaybeUninit<Lex>`
        // https://doc.rust-lang.org/std/mem/union.MaybeUninit.html#impl-MaybeUninit%3C%5BT;+N%5D%3E
        let tokens = unsafe { mem::transmute(tokens) };
        Self { walker: MaybeUninit::assume_init_read, tokens, cursor: 0, span: Span::splat(0) }
    }

    pub fn step<F, P>(&mut self, parse: F) -> Result<P>
    where
        F: FnOnce(&mut ParseStream<'lex, 'place>) -> Result<P>,
    {
        let mut rest = unsafe {
            ParseStream {
                walker: clone_walker,
                tokens: hint::outlive_mut(self.tokens),
                cursor: self.cursor,
                span: self.span,
            }
        };

        let parsed = parse(&mut rest)?;
        self.cursor = rest.cursor;
        Ok(parsed)
    }

    pub fn error(&self, message: impl fmt::Display) -> Error {
        Error::new(self.span, message.to_string())
    }

    pub fn predict(&self) -> Option<&'place (Lex<'lex>, Span)> {
        // Safety: we unitialize after `next_lex`
        unsafe {
            self.tokens
                .get(self.cursor)
                .map(|cell| hint::outlive(MaybeUninit::assume_init_ref(cell)))
        }
    }

    pub fn peek<P: Peek>(&self, peek: P) -> bool {
        let _ = peek;
        self.peek_raw::<P::Token>()
    }

    pub fn peek_raw<T: Token>(&self) -> bool {
        if let Some((lex, _)) = self.predict() { T::peek(lex) } else { false }
    }

    pub fn parse<P: Parse<'lex>>(&mut self) -> Result<P> {
        P::parse(self)
    }

    pub fn lookahead(&mut self) -> Lookahead<'lex, 'place> {
        Lookahead { scope: self.span, peek: self.predict().map(|(lex, _)| lex), peeks: Vec::new() }
    }

    fn next_lex_impl(&mut self, walker: Walker<'lex>) -> Result<(Lex<'lex>, Span)> {
        if self.cursor == self.tokens.len() {
            Err(Error::eof(self.span))
        } else {
            self.cursor += 1;
            // Safety: unitialize after step next
            unsafe {
                let cell = self.tokens.get(self.cursor - 1).unwrap_or_else(|| todo!());
                Ok(walker(cell)).inspect(|(_, span)| self.span = *span)
            }
        }
    }

    pub(crate) fn next_lex_soft(&mut self) -> Result<()> {
        unsafe {
            if self.cursor == self.tokens.len() {
                Err(Error::eof(self.span))
            } else {
                let (_, span) = self.tokens[self.cursor].assume_init_ref();
                self.span = *span;
                Ok(self.cursor += 1)
            }
        }
    }

    pub(crate) fn next_lex(&mut self) -> Result<(Lex<'lex>, Span)> {
        self.next_lex_impl(self.walker)
    }

    pub(crate) fn next_lex_clone(&mut self) -> Result<(Lex<'lex>, Span)> {
        self.next_lex_impl(clone_walker)
    }

    pub(crate) fn parse_delimited<Lt: Token + Parse<'lex>, Rt: Token + Parse<'lex>>(
        &mut self,
    ) -> Result<((Lt, Rt), ParseStream<'lex, 'place>)> {
        let anchor = self.cursor;

        let lt: Lt = self.parse()?;

        let mut balance: usize = 1;
        loop {
            if self.peek_raw::<Rt>() {
                balance -= 1;
            } else if self.peek_raw::<Lt>() {
                balance += 1;
            }

            if balance == 0 {
                break;
            }
            let _ = self.next_lex_soft()?;
        }

        let rt: Rt = self.parse()?;

        // Safety: TODO
        let tokens = unsafe {
            let (delimited, new) = hint::outlive_mut(self).tokens.split_at_mut(self.cursor);
            self.tokens = new;
            &mut delimited[anchor + 1..self.cursor - 1]
        };
        self.cursor = 0;

        Ok(((lt, rt), ParseStream { walker: self.walker, tokens, cursor: 0, span: self.span }))
    }
}

pub struct Lookahead<'lex, 'place: 'lex> {
    scope: Span,
    peek: Option<&'place Lex<'lex>>,
    peeks: Vec<&'static str>,
}

impl<'lex, 'place: 'lex> Lookahead<'lex, 'place> {
    pub fn peek<P: Peek>(&mut self, peek: P) -> bool {
        let _ = peek;

        if let Some(lex) = self.peek
            && P::Token::peek(lex)
        {
            true
        } else {
            self.peeks.push(P::Token::display());
            false
        }
    }

    pub fn error(self) -> Error {
        match self.peeks[..] {
            [] => {
                if self.peek.is_none() {
                    Error::eof(self.scope)
                } else {
                    Error::new(self.scope, "unexpected token")
                }
            }
            [expected] => Error::new(self.scope, format!("expected {expected}")),
            [a, b] => Error::new(self.scope, format!("expected {a} or {b}")),
            ref peeks => Error::new(self.scope, format!("expected one of: {}", peeks.join(", "))),
        }
    }
}
