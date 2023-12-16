macro_rules! delimited {
    ($content:ident($lt:ty, $rt:ty) in $input:expr) => {{
        let (parens, buf) = $crate::parse::ParseStream::parse_delimited::<$lt, $rt>($input)?;
        $content = buf;
        parens
    }};
}

macro_rules! parenthesized {
    ($content:ident in $input:expr) => {
        delimited!($content($crate::lexer::ast::Paren1, $crate::lexer::ast::Paren2) in $input);
    };
}

use {
    crate::{
        lexer::{ast, Lit},
        parse::{self, ParseStream},
        Lex, Token,
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
        let paren;
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
