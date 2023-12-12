use {
    super::{ast, Ident, Lex, Lit, LitBool, LitFloat, LitInt, LitStr, Span},
    crate::parse,
    chumsky::{extra, prelude::*, text, IterParser, Parser},
    std::mem,
};

pub fn lexer<'src>()
-> impl Parser<'src, &'src str, Vec<Lex<'src>>, extra::Err<Rich<'src, char, Span>>> {
    let num = text::int(10)
        .then(
            just('.')
                .then(text::digits(10))
                .or_not()
                .to_slice()
                .from_str()
                .unwrapped()
                .map_with(|num, e| Lex::Lit(Lit::Int(LitInt { lit: num, span: e.span() }))),
        )
        .to_slice()
        .from_str()
        .unwrapped()
        .map_with(|num, e| Lex::Lit(Lit::Float(LitFloat { lit: num, span: e.span() })));

    // A parser for strings
    let str_ = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .to_slice()
        .map_with(|str, e| Lex::Lit(Lit::Str(LitStr { lit: str, span: e.span() })));

    // A parser for operators
    let punct = one_of("+*-/!=")
        .repeated()
        .at_least(1)
        .to_slice()
        .map_with(|punct, e| ast::Punct::new(punct, e.span()))
        .unwrapped()
        .map(Lex::Punct);

    // A parser for control characters (delimiters, semicolons, etc.)
    let delim = one_of("()[]{};,")
        .map_with(|delim, e| ast::Delim::new(delim, e.span()))
        .unwrapped()
        .map(Lex::Delim);

    // A parser for identifiers and keywords
    let ident = text::ascii::ident().map_with(|ident: &str, e| {
        let span = e.span();
        match ident {
            "true" => Lex::Lit(Lit::Bool(LitBool { lit: true, span })),
            "false" => Lex::Lit(Lit::Bool(LitBool { lit: false, span })),
            other => ast::Token::new(other, span)
                .map(Lex::Token)
                // Safety: TODO
                .unwrap_or(Lex::Ident(unsafe { Ident::new_unchecked(other, span) })),
        }
    });

    // A single token can be one of the above
    let token = num.or(str_).or(punct).or(delim).or(ident);

    let comment = just("//").then(any().and_is(just('\n').not()).repeated()).padded();

    token
        // .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

#[test]
fn lex_test() {
    use crate::parse::ParseStream;

    println!("{}", mem::size_of::<Lex>());

    let src = "let x = 12 + null == le + 12.0001";
    let mut parsed = lexer().parse(src).unwrap();
    println!("{:#?}", parsed);

    let mut buf = ParseStream::new(&mut parsed[..]);

    println!("{}", buf.peek(ast::Let));
    println!("{:?}", buf.next_lex());
    println!("{}", buf.peek(ast::Let));

    let lex = buf.step(|step| {
        step.next_lex();
        step.next_lex();
        step.next_lex();
        step.next_lex();
        // x = Some(step);
        Ok(())
        // Err::<Lex, _>(parse::Error { message: "".to_string(), span: Span::new(0, 0) })
    });
    println!("{:?}", lex);
    println!("{:?}", buf.current());
}
