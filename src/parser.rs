use crate::{lexer, Lex, Span};

pub trait Peek {
    fn peek(self, token: &Lex<'_>) -> bool;
}

// impl<'a> Peek for Lex<'a> {
//     fn peek(self, token: &Lex<'_>) -> bool {
//         &self == token
//     }
// }

impl<F> Peek for F
where
    F: for<'any> FnOnce(&Lex<'any>) -> bool,
{
    fn peek(self, token: &Lex<'_>) -> bool {
        self(token)
    }
}

trait Parse {
    fn parse(input: &ParseStream<'_>) -> Self;
}

pub struct ParseStream<'a> {
    tokens: Vec<(Lex<'a>, Span)>, // later use partially uninit vector
    cursor: usize,
}

impl<'a> ParseStream<'a> {
    pub fn new(tokens: Vec<(Lex<'a>, Span)>) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub fn current(&self) -> Option<&(Lex<'a>, Span)> {
        self.tokens.get(self.cursor)
    }

    pub fn peek<P: Peek>(&self, peek: P) -> bool {
        let Some((token, _)) = self.current() else { return false };
        peek.peek(token)
    }

    #[rustfmt::skip]
    pub(/* in crate */ crate) fn next_token<'parse>(
        &'parse mut self,
    ) -> Option<&'parse (Lex<'a>, Span)> {
        if self.cursor == self.tokens.len() {
            None
        } else {
            self.cursor += 1;
            self.tokens.get(self.cursor - 1)
        }
    }
}
