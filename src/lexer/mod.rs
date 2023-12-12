mod lexer;

use chumsky::prelude::SimpleSpan;

pub trait Token {
    fn peek(lex: &Lex<'_>) -> bool;
    fn display() -> &'static str;
}

pub mod ast {
    macro_rules! define_tokens {
        ($(
            $space:ident($ty:ty) { $($token:literal => $name:ident)* }
        )*) => {$(
            #[derive(Clone, Debug)]
            pub enum $space {
                $($name($name),)*
            }

            impl $space {
                pub fn new(token: $ty, span: Span) -> Option<Self> {
                    match token {
                        $( $token => Some(Self::$name($name { span })),)*
                        _ => None,
                    }
                }
            }

            $(
                impl From<$name> for $space {
                    fn from(token: $name) -> Self {
                         Self::$name(token)
                    }
                }
            )*

            $(
                #[derive(Clone)]
                pub struct $name {
                    pub span: Span,
                }

                #[allow(non_snake_case, dead_code)]
                pub fn $name(_: crate::parse::Sealed) -> $name {
                    unreachable!()
                }

                impl super::Token for $name {
                    fn peek(lex: &Lex<'_>) -> bool {
                        if let Lex::$space(inner) = lex {
                            matches!(inner, $space::$name(_))
                        } else {
                            false
                        }
                    }

                    fn display() -> &'static str {
                        concat!('`', $token, '`')
                    }
                }

                impl fmt::Debug for $name {
                    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                        write!(f, "{:?}", self.span)
                    }
                }
            )*
        )*};
    }

    use {
        super::{Lex, Span},
        std::fmt,
    };

    define_tokens! {
        Token(&str) {
            "let" => Let
            "fn" => Fn
        }

        Punct(&str) {
            "+" => Plus
            "-" => Minus
            "*" => Mul
            "/" => Div
            "=" => Eq
            "!" => Not

            "==" => EqEq
        }

        Delim(char) {
            '{' => Brace1
            '}' => Brace2
            '[' => Bracket1
            ']' => Bracket2
            '(' => Parenthesis1
            ')' => Parenthesis2
        }
    }
}

pub type Span = SimpleSpan<usize>;

#[derive(Clone, Debug)]
pub enum Number {
    Float(f64),
    Uint(u64),
}

#[derive(Clone, Debug)]
pub enum Lit<'a> {
    Str(LitStr<'a>),
    Int(LitInt),
    Float(LitFloat),
    Bool(LitBool),
}

#[derive(Clone, Debug)]
pub enum Lex<'src> {
    Lit(Lit<'src>),
    Ident(Ident<'src>),
    Delim(ast::Delim),
    Punct(ast::Punct),
    Token(ast::Token),
}

#[derive(Clone, Debug)]
pub struct Ident<'a> {
    ident: &'a str,
    pub span: Span,
}

impl Token for Ident<'_> {
    fn peek(lex: &Lex<'_>) -> bool {
        matches!(lex, Lex::Ident(_))
    }

    fn display() -> &'static str {
        "identifier"
    }
}

impl<'a> Ident<'a> {
    pub unsafe fn new_unchecked(ident: &'a str, span: Span) -> Self {
        Self { ident, span }
    }

    pub fn ident(&self) -> &str {
        self.ident
    }
}

#[derive(Clone, Debug)]
pub struct LitStr<'a> {
    pub lit: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct LitInt {
    pub lit: u64,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct LitFloat {
    pub lit: f64,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct LitBool {
    pub lit: bool,
    pub span: Span,
}

use crate::{
    parse,
    parse::{Parse, ParseStream},
};
pub use lexer::lexer;
