#[macro_export]
macro_rules! delimited {
    ($content:ident($lt:ty, $rt:ty) in $input:expr) => {{
        let (parens, buf) =
            $crate::parse::ParseBuffer::parse_delimited::<$lt, $rt>($input).unwrap();
        $content = buf;
        parens
    }};
}

#[macro_export]
macro_rules! parenthesized {
    ($content:ident in $input:expr) => {{
        let (lt, rt) = $crate::delimited!(
            $content($crate::lexer::ast::Paren1, $crate::lexer::ast::Paren2) in $input
        );
        $crate::token::Paren {
            span: $crate::parse::DelimSpan {
                lt: lt.span,
                rt: rt.span,
                block: $crate::parse::lookahead::predict_span(&mut $content),
            }
        }
    }};
}

#[macro_export]
macro_rules! braced {
    ($content:ident in $input:expr) => {{
        let (lt, rt) = $crate::delimited!(
            $content($crate::lexer::ast::Brace1, $crate::lexer::ast::Brace2) in $input
        );
        $crate::token::Brace {
            span: $crate::parse::DelimSpan {
                lt: lt.span,
                rt: rt.span,
                block: $crate::parse::lookahead::predict_span(&mut $content),
            }
        }
    }};
}

#[macro_export]
macro_rules! bracketed {
    ($content:ident in $input:expr) => {{
        let (lt, rt) = $crate::delimited!(
            $content($crate::lexer::ast::Bracket1, $crate::lexer::ast::Bracket2) in $input
        );
        $crate::token::Bracket {
            span: $crate::parse::DelimSpan {
                lt: lt.span,
                rt: rt.span,
                block: $crate::parse::lookahead::predict_span(&mut $content),
            }
        }
    }};
}

pub mod lookahead {
    use crate::{
        parse::{Parse, ParseBuffer, Peek},
        Span,
    };

    pub fn is_delimiter<'lex, Lt: Peek, Rt: Peek>(
        input: &ParseBuffer<'lex>,
        (_, _): (Lt, Rt),
    ) -> bool
    where
        Lt::Token: Parse<'lex>,
        Rt::Token: Parse<'lex>,
    {
        input.scan(|scan| scan.parse_delimited::<Lt::Token, Rt::Token>().is_ok())
    }

    use std::cmp;

    pub fn predict_span(input: &mut ParseBuffer) -> Span {
        input.scan(|fork| {
            let Ok(mut span) = fork.skip_next() else {
                return Span::splat(0);
            };
            while let Ok(Span { start, end, .. }) = fork.skip_next() {
                span.start = cmp::min(span.start, start);
                span.end = cmp::max(span.start, end);
            }
            span
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub struct DelimSpan {
    pub lt: Span,
    pub block: Span,
    pub rt: Span,
}

macro_rules! define_delimiters {
    ($($lt:ident..$rt:ident pub struct $name:ident)*) => {$(
        #[derive(Clone)]
        pub struct $name {
            pub span: DelimSpan,
        }

        #[allow(non_snake_case)]
        pub fn $name(peek: $crate::parse::PhantomPeek) -> $name {
            match peek {}
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                f.debug_struct(stringify!($name))
                    .field("lt", &self.span.lt)
                    .field("block", &self.span.block)
                    .field("rt", &self.span.rt)
                    .finish()
            }
        }
    )*};
}

impl Token for Paren {
    fn peek(input: &ParseBuffer) -> bool {
        lookahead::is_delimiter(input, (Paren1, Paren2))
    }

    fn display() -> &'static str {
        "parentheses"
    }
}

impl Token for Brace {
    fn peek(input: &ParseBuffer) -> bool {
        lookahead::is_delimiter(input, (Brace1, Brace2))
    }

    fn display() -> &'static str {
        "curly braces"
    }
}

impl Token for Bracket {
    fn peek(input: &ParseBuffer) -> bool {
        lookahead::is_delimiter(input, (Bracket1, Bracket2))
    }

    fn display() -> &'static str {
        "square brackets"
    }
}

use {
    crate::lexer::ast::{Brace1, Brace2, Bracket1, Bracket2, Paren1, Paren2},
    std::{fmt, fmt::Formatter},
};

define_delimiters! {
    Brace1..Brace2       pub struct Brace
    Bracket1..Bracket2   pub struct Bracket
    Paren1..Paren2       pub struct Paren
}

use crate::{lexer::Token, parse::ParseBuffer, Span};

#[cfg(test)]
use {
    crate::{
        lexer::{ast, Lit},
        parse, token, Lex,
    },
    chumsky::Parser,
};

#[test]
fn parens() -> parse::Result<()> {
    let src = "((1 + 2) + (3 + 4))";
    let parsed = crate::lexer::lexer().parse(src).into_result().unwrap();
    let mut input = ParseBuffer::new(parsed);

    pub(crate) fn paren<'lex>(content: &mut ParseBuffer<'lex>) -> parse::Result<ParseBuffer<'lex>> {
        let mut paren;
        parenthesized!(paren in content);
        Ok(paren)
    }

    let mut content;
    parenthesized!(content in &mut input);

    let inner = |input: &mut ParseBuffer| {
        let a: Lit = input.parse().unwrap();
        let plus: ast::Plus = input.parse().unwrap();
        let b: Lit = input.parse().unwrap();

        println!("{:?}", (a, plus, b));
    };

    let mut first = paren(&mut content)?;
    inner(&mut first);

    content.parse::<ast::Plus>()?;

    let mut second = paren(&mut content)?;
    inner(&mut second);

    Ok(())
}

#[test]
fn delimited() -> parse::Result<()> {
    let src = "|a, b|";

    let parsed = crate::lexer::lexer().parse(src).into_result().unwrap();
    let mut input = ParseBuffer::new(parsed);

    let mut content;
    delimited!(content(Token![|], Token![|]) in &mut input);

    while let Ok(lex) = content.parse::<Lex>() {
        println!("{lex:?}");
    }

    Ok(())
}

#[cfg(test)]
use crate::{util::lex_it, Token};

#[test]
fn peek_paren() -> parse::Result<()> {
    let input = lex_it!("(a, b)");
    assert!(input.peek(token::Paren));

    {
        let input = lex_it!("[a, b]");
        assert!(!input.peek(token::Paren));

        let input = lex_it!("(a, b");
        assert!(!input.peek(token::Paren));

        let input = lex_it!("a, b)");
        assert!(!input.peek(token::Paren));
    }

    Ok(())
}

#[test]
fn lookahead() {
    // TODO: extract into function
    let src = "|";

    let parsed = crate::lexer::lexer().parse(src).into_result().unwrap();
    let mut input = ParseBuffer::new(parsed);

    let mut lookahead = input.lookahead();
    lookahead.peek(Token![+]);
    lookahead.peek(Token![-]);

    println!("{}", lookahead.error());
}

#[test]
fn fork() {
    let mut input = lex_it!("(123.321)");

    let do_parse = if input.peek(token::Paren) {
        input.fork(|fork, mut advance| {
            let mut content;
            parenthesized!(content in fork);
            if content.parse::<Lit>().is_ok() && content.is_empty() {
                advance.to(fork);
                true
            } else {
                false
            }
        })
    } else {
        false
    };

    assert!(do_parse);
    assert!(input.is_empty());
}
