use {
    crate::parser::ParseStream,
    chumsky::{
        extra,
        prelude::{any, end, just, none_of, one_of, skip_then_retry_until, Rich, SimpleSpan},
        text, IterParser, Parser,
    },
};

#[derive(Clone, Debug, PartialEq)]
pub enum Number {
    Float(f64),
    Uint(u64),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Lex<'src> {
    Bool(bool),
    Num(Number),
    Str(&'src str),
    Ident(&'src str),
    Delim(ast::Delim),
    Punct(ast::Punct),
    Token(ast::Token),
}

pub mod ast {
    macro_rules! define_tokens {
        ($(
            $space:ident($ty:ty) { $($token:literal => $name:ident)* }
        )*) => {$(
            #[derive(Clone, Debug, PartialEq)]
            pub enum $space {
                $($name,)*
            }

            impl $space {
                pub fn new(token: $ty) -> Option<Self> {
                    match token {
                        $( $token => Some(Self::$name), )*
                        _ => None,
                    }
                }
            }

            $(
                impl From<$name> for $space {
                    fn from(_: $name) -> Self {
                         Self::$name
                    }
                }
            )*

            $(
                impl Peek for $name {
                    fn peek(self, token: &super::Lex<'_>) -> bool {
                        if let super::Lex::$space(inner) = token {
                            matches!(inner, $space::$name)
                        } else {
                            false
                        }
                    }
                }
            )*

            $(
                #[derive(Clone, PartialEq)]
                pub struct $name;

                impl fmt::Debug for $name {
                    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                        f.write_str(stringify!($name))
                    }
                }
            )*
        )*};
    }

    use {crate::parser::Peek, std::fmt};

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

pub fn lexer<'src>()
-> impl Parser<'src, &'src str, Vec<(Lex<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    let num = text::int(10)
        .then(
            just('.')
                .then(text::digits(10))
                .or_not()
                .to_slice()
                .from_str()
                .unwrapped()
                .map(Number::Uint),
        )
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Number::Float)
        .map(Lex::Num);

    // A parser for strings
    let str_ = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .to_slice()
        .map(Lex::Str);

    // A parser for operators
    let op = one_of("+*-/!=")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(ast::Punct::new)
        .unwrapped()
        .map(Lex::Punct);

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = one_of("()[]{};,").map(ast::Delim::new).unwrapped().map(Lex::Delim);

    // A parser for identifiers and keywords
    let ident = text::ascii::ident().map(|ident: &str| match ident {
        "true" => Lex::Bool(true),
        "false" => Lex::Bool(false),
        other => ast::Token::new(other).map(Lex::Token).unwrap_or(Lex::Ident(ident)),
    });

    // A single token can be one of the above
    let token = num.or(str_).or(op).or(ctrl).or(ident);

    let comment = just("//").then(any().and_is(just('\n').not()).repeated()).padded();

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

#[test]
fn lex_test() {
    use crate::parser::ParseStream;

    let src = "let x = 12 + null + le + 12.0";
    // println!("{:?}", lexer().parse(src));
    //println!("{:?}", lexer().parse(src))
    let x = lexer().parse(src);
    let mut buf = ParseStream::new(x.unwrap());

    println!("{}", buf.peek(ast::Let));
    println!("{:?}", buf.next_token());
    println!("{}", buf.peek(ast::Let));
}
