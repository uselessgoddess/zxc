#![feature(
    let_chains,
    slice_ptr_len,
    slice_ptr_get,
    vec_into_raw_parts,
    dropck_eyepatch,
    extern_types,
    new_uninit,
    maybe_uninit_slice,
    strict_provenance,
    hash_raw_entry,
    core_intrinsics,
    mem_copy_fn
)]
#![allow(internal_features)]
#![allow(clippy::unit_arg, clippy::let_unit_value)]

extern crate core;

#[macro_use]
pub mod ast;
pub mod lexer;
#[macro_use]
pub mod parse;
mod codegen;

pub use lexer::{Lex, Span};

// TODO: move from `ast` into this
pub mod token {
    use super::parse;

    pub use parse::delim::{Brace, Bracket, Paren};
}

fn main() {}

#[cfg(test)]
mod util {
    #[rustfmt::skip]
    macro_rules! lex_it {
        ($src:literal) => {{ 
            use chumsky::Parser;
            ParseBuffer::new(crate::lexer::lexer().parse($src).into_result().unwrap()) 
        }};
    }

    pub(crate) use lex_it;
}
