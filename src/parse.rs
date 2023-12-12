use {
    crate::{lexer::Token, Lex, Span},
    std::{
        marker::PhantomData,
        mem::{self, MaybeUninit},
        slice,
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
pub struct Error {
    pub message: String,
    pub span: Span,
}

impl Error {
    pub fn new(message: String, span: Span) -> Self {
        Self { message, span }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub trait Parse: Sized {
    fn parse(input: &mut ParseStream<'_, '_>) -> Result<Self>;
}

#[rustfmt::skip]
mod hint {
    pub unsafe fn outlive<'ex, T: ?Sized>(ptr: *const T) -> &'ex T { &*ptr }
    pub unsafe fn outlive_mut<'ex, T: ?Sized>(ptr: *mut T) -> &'ex mut T { &mut *ptr }
}

pub struct ParseStream<'lex, 'place: 'lex> {
    step: unsafe fn(&MaybeUninit<Lex<'lex>>) -> Lex<'lex>,
    tokens: &'place mut [MaybeUninit<Lex<'lex>>],
    cursor: usize,
}

impl<'lex, 'place: 'lex> ParseStream<'lex, 'place> {
    pub fn new(tokens: &'place mut [Lex<'lex>]) -> Self {
        // Safety: safe because works as coercion `Lex` -> `MaybeUninit<Lex>`
        // https://doc.rust-lang.org/std/mem/union.MaybeUninit.html#impl-MaybeUninit%3C%5BT;+N%5D%3E
        unsafe {
            Self { step: MaybeUninit::assume_init_read, tokens: mem::transmute(tokens), cursor: 0 }
        }
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
            }
        };

        let parsed = parse(&mut rest)?;
        self.cursor = rest.cursor;
        Ok(parsed)
    }

    pub fn current(&self) -> Option<&'place Lex<'lex>> {
        // Safety: we unitialize after `next_lex`
        unsafe {
            self.tokens
                .get(self.cursor)
                .map(|cell| hint::outlive(MaybeUninit::assume_init_ref(cell)))
        }
    }

    pub fn peek<P: Peek>(&self, peek: P) -> bool {
        let _ = peek;
        if let Some(lex) = self.current() { P::Token::peek(lex) } else { false }
    }

    pub fn parse<P: Parse>(&mut self) -> Result<P> {
        P::parse(self)
    }

    pub(crate) fn next_lex(&mut self) -> Option<Lex<'lex>> {
        if self.cursor == self.tokens.len() {
            None
        } else {
            self.cursor += 1;
            // Safety: unitialize after step next
            unsafe {
                let cell = self.tokens.get(self.cursor - 1)?;
                Some((self.step)(cell))
            }
        }
    }
}
