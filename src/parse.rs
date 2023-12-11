use {
    crate::{lexer::Token, Lex, Span},
    std::{mem, mem::MaybeUninit},
};

pub enum Sealed {}

pub trait Peek {
    type Token: Token;
}

// TODO(doc): Add notes about this
impl<T: Token, F: FnOnce(Sealed) -> T> Peek for F {
    type Token = T;
}

pub struct Error {
    message: String,
    span: Span,
}

impl Error {
    pub fn new(message: String, span: Span) -> Self {
        Self { message, span }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub trait Parse: Sized {
    fn parse(input: &mut ParseStream<'_>) -> Result<Self>;
}

pub struct ParseStream<'a> {
    tokens: &'a mut [MaybeUninit<Lex<'a>>],
    cursor: usize,
}

impl<'a> ParseStream<'a> {
    pub fn new(tokens: &'a mut [Lex<'a>]) -> Self {
        // Safety: safe because works as coercion `Lex` -> `MaybeUninit<Lex>`
        // https://doc.rust-lang.org/std/mem/union.MaybeUninit.html#impl-MaybeUninit%3C%5BT;+N%5D%3E
        unsafe { Self { tokens: mem::transmute(tokens), cursor: 0 } }
    }

    pub fn current(&self) -> Option<&Lex<'a>> {
        // Safety: we unitialize after `next_lex`
        unsafe { self.tokens.get(self.cursor).map(|cell| MaybeUninit::assume_init_ref(cell)) }
    }

    pub fn peek<P: Peek>(&self, peek: P) -> bool {
        let _ = peek;
        if let Some(lex) = self.current() { P::Token::peek(lex) } else { false }
    }

    pub fn parse<P: Parse>(&mut self) -> Result<P> {
        P::parse(self)
    }

    pub(crate) fn next_lex(&mut self) -> Option<Lex<'a>> {
        if self.cursor == self.tokens.len() {
            None
        } else {
            self.cursor += 1;
            // Safety: unitialize after step next
            unsafe {
                let cell = self.tokens.get(self.cursor - 1)?;
                Some(MaybeUninit::assume_init_read(cell))
            }
        }
    }
}
