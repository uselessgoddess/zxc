#![feature(
    let_chains,
    slice_ptr_len,
    slice_ptr_get,
)]
#![allow(internal_features)]
#![allow(clippy::unit_arg, clippy::let_unit_value)]

#[macro_use]
pub mod ast;

#[macro_use]
pub mod parse;
pub mod lexer;

pub use lexer::{Lex, Span};

// TODO: move from `ast` into this
pub mod token {
    use super::parse;

    pub use parse::delim::{Brace, Bracket, Paren};
}

#[cfg(test)]
mod util {
    #[rustfmt::skip]
    macro_rules! lex_it {
        ($src:literal) => {{ 
            use chumsky::Parser;
            $crate::parse::ParseBuffer::new(crate::lexer::lexer()
                .parse($src).into_result().unwrap())
        }};
        ($src:literal in $var:ident) => {{ 
            $var = $src;
            use chumsky::Parser;
            $crate::parse::ParseBuffer::new(crate::lexer::lexer()
                .parse($src).into_result().unwrap())
        }};
    }

    pub(crate) use lex_it;
}

pub use lexer::*;
pub use parse::*;