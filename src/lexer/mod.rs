mod lexer;
mod token;

use {chumsky::prelude::SimpleSpan, std::borrow::Cow};

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

                impl<'lex> Parse<'lex> for $name {
                    fn parse(input: &mut ParseStream<'lex, '_>) -> parse::Result<Self> {
                        input.step(|step| match step.next_lex()? {
                            (Lex::$space($space::$name(lex)), _) => Ok(lex),
                            _ => Err(step.error(format_args!("expected `{}`", $token))),
                        })
                    }
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
        super::{parse, Lex, Parse, ParseStream, Span},
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
            "==" => EqEq
            "!=" => NotEq

            "|" => Or
            "||" => OrOr
        }

        Delim(char) {
            ';' => Semi
            ',' => Comma
            '{' => Brace1
            '}' => Brace2
            '[' => Bracket1
            ']' => Bracket2
            '(' => Paren1
            ')' => Paren2
        }
    }
}

pub type Span = SimpleSpan<usize>;

#[derive(Clone, Debug)]
pub enum Number {
    Float(f64),
    Uint(u64),
}

ast_enum_of_structs! {
    #[derive(Clone, Debug)]
    pub enum Lit<'a> {
        Str(LitStr<'a>),
        Int(LitInt),
        Float(LitFloat),
        Bool(LitBool),
    }
}

#[derive(Clone, Debug)]
pub enum Lex<'src> {
    Lit(Lit<'src>),
    Ident(Ident<'src>),
    Delim(ast::Delim),
    Punct(ast::Punct),
    Token(ast::Token),
}

impl<'lex> Parse<'lex> for Lex<'lex> {
    fn parse(input: &mut ParseStream<'lex, 'lex>) -> parse::Result<Self> {
        input.next_lex().map(|(lex, _)| lex)
    }
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

impl<'lex> Parse<'lex> for Ident<'lex> {
    fn parse(input: &mut ParseStream<'lex, '_>) -> parse::Result<Self> {
        input.step(|step| match step.next_lex()? {
            (Lex::Ident(ident), _) => Ok(ident),
            _ => Err(step.error("expected identifier")),
        })
    }
}

#[derive(Clone, Debug)]
pub struct LitStr<'a> {
    pub lit: Cow<'a, str>,
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

macro_rules! impl_parse {
    ($($name:ty => { $($pat:tt)* } in $err:literal)*) => {$(
        impl<'lex> Parse<'lex> for $name {
            fn parse(input: &mut ParseStream<'lex, '_>) -> parse::Result<Self> {
                input.step(|step| match step.next_lex()? {
                    $($pat)*,
                    _ => Err(step.error($err)),
                })
            }
        }
    )*};
}

impl_parse! {
    LitFloat    => { (Lex::Lit(Lit::Float(lex)), _) => Ok(lex) } in "expected float literal"
    LitInt      => { (Lex::Lit(Lit::Int(  lex)), _) => Ok(lex) } in "expected integer literal"
    LitBool     => { (Lex::Lit(Lit::Bool( lex)), _) => Ok(lex) } in "expected bool literal"
}

impl<'lex> Parse<'lex> for LitStr<'lex> {
    fn parse(input: &mut ParseStream<'lex, '_>) -> parse::Result<Self> {
        input.step(|step| match step.next_lex()? {
            (Lex::Lit(Lit::Str(str)), _) => Ok(str),
            _ => Err(step.error("expected string literal")),
        })
    }
}

use crate::{
    parse,
    parse::{Parse, ParseStream},
};
pub use lexer::lexer;
