#[macro_export]
macro_rules! delimited {
    ($content:ident($lt:ty, $rt:ty) in $input:expr) => {{
        let (parens, buf) =
            $crate::parse::ParseStream::parse_delimited::<$lt, $rt>($input).unwrap();
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
    ($content:ident in $input:expr) => {
        $crate::delimited!(
            $content($crate::lexer::ast::Brace1, $crate::lexer::ast::Brace2) in $input
        );
    };
}

#[macro_export]
macro_rules! bracketed {
    ($content:ident in $input:expr) => {
        $crate::delimited!(
            $content($crate::lexer::ast::Bracket1, $crate::lexer::ast::Bracket2) in $input
        );
    };
}

pub mod lookahead {
    use crate::{
        parse::{Parse, ParseStream, Peek},
        Span,
    };

    pub fn is_delimiter<'lex, Lt: Peek, Rt: Peek>(
        input: &ParseStream<'lex, 'lex>,
        (_, _): (Lt, Rt),
    ) -> bool
    where
        Lt::Token: Parse<'lex>,
        Rt::Token: Parse<'lex>,
    {
        input.fork(|fork| fork.parse_delimited::<Lt::Token, Rt::Token>().is_ok())
    }

    use std::cmp;

    pub fn predict_span<'lex>(input: &mut ParseStream<'lex, 'lex>) -> Span {
        input.fork(|fork| {
            let Ok(mut span) = fork.next_lex_soft() else { return Span::splat(0) };

            while let Ok(Span { start, end, .. }) = fork.next_lex_soft() {
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
    fn peek<'a>(input: &ParseStream<'a, 'a>) -> bool {
        lookahead::is_delimiter(input, (Paren1, Paren2))
    }

    fn display() -> &'static str {
        "parentheses"
    }
}

impl Token for Brace {
    fn peek<'a>(input: &ParseStream<'a, 'a>) -> bool {
        lookahead::is_delimiter(input, (Brace1, Brace2))
    }

    fn display() -> &'static str {
        "curly braces"
    }
}

impl Token for Bracket {
    fn peek<'a>(input: &ParseStream<'a, 'a>) -> bool {
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

use {
    crate::{
        lexer::{ast, Lit, Token},
        parse::{self, ParseStream},
        token, Lex, Span, Token,
    },
    chumsky::Parser,
};

#[test]
fn parens() -> parse::Result<()> {
    let src = "((1 + 2) + (3 + 4))";
    let mut parsed = crate::lexer::lexer().parse(src).into_result().unwrap();
    let mut input = ParseStream::new(&mut parsed[..]);

    pub(crate) fn paren<'lex, 'place: 'lex>(
        content: &mut ParseStream<'lex, 'place>,
    ) -> parse::Result<ParseStream<'lex, 'place>> {
        let mut paren;
        parenthesized!(paren in content);
        Ok(paren)
    }

    let mut content;
    parenthesized!(content in &mut input);

    let inner = |input: &mut ParseStream| {
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

    let mut parsed = crate::lexer::lexer().parse(src).into_result().unwrap();
    let mut input = ParseStream::new(&mut parsed[..]);

    let mut content;
    delimited!(content(Token![|], Token![|]) in &mut input);

    while let Ok(lex) = content.parse::<Lex>() {
        println!("{lex:?}");
    }

    Ok(())
}

macro_rules! lex_it {
    ($src:literal in $ident:ident) => {
        let mut __parsed = crate::lexer::lexer().parse($src).into_result().unwrap();
        let mut $ident = ParseStream::new(&mut __parsed[..]);
    };
}

#[test]
fn peek_paren() -> parse::Result<()> {
    lex_it!("(a, b)" in input);
    assert!(input.peek(token::Paren));

    {
        lex_it!("[a, b]" in input);
        assert!(!input.peek(token::Paren));

        lex_it!("(a, b" in input);
        assert!(!input.peek(token::Paren));

        lex_it!("a, b)" in input);
        assert!(!input.peek(token::Paren));
    }

    Ok(())
}

#[test]
fn lookahead() {
    // TODO: extract into function
    let src = "|";

    let mut parsed = crate::lexer::lexer().parse(src).into_result().unwrap();
    let mut input = ParseStream::new(&mut parsed[..]);

    let mut lookahead = input.lookahead();
    lookahead.peek(Token![+]);
    lookahead.peek(Token![-]);

    println!("{}", lookahead.error());
}
