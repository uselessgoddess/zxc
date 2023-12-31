mod lex;
mod token;

use chumsky::prelude::SimpleSpan;

pub trait Token {
    fn peek(input: &ParseBuffer) -> bool;
    fn display() -> &'static str;
}

pub mod ast {
    macro_rules! define_tokens {
        ($(
            $space:ident($ty:ty) { $($token:literal => $name:ident)* }
        )*) => {$(

            ast_enum_of_structs! {
                #[derive(Copy, Clone, Debug)]
                pub enum $space {
                    $($name($name),)*
                }
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
                #[derive(Copy, Clone)]
                pub struct $name {
                    pub span: Span,
                }

                #[allow(non_snake_case, dead_code)]
                pub fn $name(peek: crate::parse::PhantomPeek) -> $name {
                    match peek {}
                }

                impl<'lex> Parse<'lex> for $name {
                    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
                        input.step(|step| match step.next_lex()? {
                            (Lex::$space($space::$name(lex)), _) => Ok(lex),
                            _ => Err(step.error(format_args!("expected `{}`", $token))),
                        })
                    }
                }


                impl super::Token for $name {
                    fn peek(input: &ParseBuffer) -> bool {
                        if let Some((Lex::$space(inner), _)) = input.predict() {
                            matches!(inner, $space::$name(_))
                        } else {
                            false
                        }
                    }

                    fn display() -> &'static str {
                        concat!('`', $token, '`')
                    }
                }

                impl $crate::parse::Spanned for $name {
                    fn span(&self) -> $crate::Span {
                        self.span
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
        super::{parse, Ident, Lit, Parse, ParseBuffer, Span},
        std::fmt,
    };

    define_tokens! {
        Token(&str) {
            "extern" => Extern
            "let" => Let
            "fn" => Fn
        }

        Punct(&str) {
            "+" => Plus
            "-" => Minus
            "/" => Slash
            "*" => Star

            "=" => Eq
            "==" => EqEq
            "!=" => NotEq

            "!" => Not
            "|" => Or
            "||" => OrOr

            "->" => RArrow
        }

        Delim(char) {
            '.' => Dot
            ':' => Colon
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

    ast_enum_of_structs! {
        #[derive(Clone, Debug)]
        pub enum Lex<'src> {
            Lit(Lit<'src>),
            Ident(Ident<'src>),
            Delim(Delim),
            Punct(Punct),
            Token(Token),
        }
    }
}

pub use ast::Lex;
pub type Span = SimpleSpan<usize>;

#[derive(Clone, Debug)]
pub enum Number {
    Float(f64),
    Uint(u64),
}

ast_enum_of_structs! {
    #[derive(Debug, Copy, Clone)]
    pub enum Lit<'a> {
        Str(LitStr<'a>),
        Int(LitInt),
        Float(LitFloat),
        Bool(LitBool),
    }
}

impl<'lex> Parse<'lex> for Lex<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        input.next_lex().map(|(lex, _)| lex)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Ident<'a> {
    ident: &'a str,
    pub span: Span,
}

impl<'a> Ident<'a> {
    pub unsafe fn new_unchecked(ident: &'a str, span: Span) -> Self {
        Self { ident, span }
    }

    pub fn ident(&self) -> &'a str {
        self.ident
    }
}

impl<'lex> Parse<'lex> for Ident<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        input.step(|step| match step.next_lex()? {
            (Lex::Ident(ident), _) => Ok(ident),
            _ => Err(step.error("expected identifier")),
        })
    }
}

#[derive(Copy, Clone, Debug)]
pub struct LitStr<'a> {
    pub lit: &'a str,
    pub span: Span,
}

#[derive(Copy, Clone, Debug)]
pub struct LitInt {
    pub lit: u64,
    pub span: Span,
}

#[derive(Copy, Clone, Debug)]
pub struct LitFloat {
    pub lit: f64,
    pub span: Span,
}

#[derive(Copy, Clone, Debug)]
pub struct LitBool {
    pub lit: bool,
    pub span: Span,
}

macro_rules! impl_parse {
    ($($name:ty => { $($pat:tt)* } in $err:literal)*) => {$(
        impl<'lex> Parse<'lex> for $name {
            fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
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

define_token! {
    Lex::Ident(_) in Ident<'a> => "identifier"
    Lex::Lit(_) in Lit<'a> => "literal"
    Lex::Lit(Lit::Str(_)) in LitStr<'a> => "string literal"
}

impl<'lex> Parse<'lex> for LitStr<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        input.step(|step| match step.next_lex()? {
            (Lex::Lit(Lit::Str(str)), _) => Ok(str),
            _ => Err(step.error("expected string literal")),
        })
    }
}

use crate::{
    parse,
    parse::{define_token, Parse, ParseBuffer},
};
pub use lex::lexer;
