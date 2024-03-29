use {
    super::{ast, Ident, Lex, Lit, LitBool, LitFloat, LitInt, LitStr, Span},
    chumsky::{extra, prelude::*, text, IterParser, Parser},
    std::num::IntErrorKind,
};

pub fn lexer<'src>()
-> impl Parser<'src, &'src str, Vec<(Lex<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    let num = text::digits(10).then(just('.').then(text::digits(10)).or_not()).to_slice().try_map(
        |num: &str, span| {
            Ok(match num.parse::<u64>() {
                Ok(lit) => Lex::Lit(Lit::Int(LitInt { lit, span })),
                Err(err) => {
                    if let IntErrorKind::PosOverflow | IntErrorKind::NegOverflow = err.kind() {
                        return Err(Rich::custom(span, err));
                    } else {
                        Lex::Lit(Lit::Float(LitFloat {
                            lit: num.parse::<f64>().map_err(|err| Rich::custom(span, err))?,
                            span,
                        }))
                    }
                }
            })
        },
    );

    // A parser for strings
    let str_ = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .to_slice()
        .map_with(|str, e| Lex::Lit(Lit::Str(LitStr { lit: str, span: e.span() })));

    // A parser for operators
    let punct = one_of("+*-/!=|&<>#:") // TODO: make extendable
        .repeated()
        .at_least(1)
        .to_slice()
        .map_with(|punct, e| ast::Punct::new(punct, e.span()))
        .unwrapped()
        .map(Lex::Punct);

    // A parser for control characters (delimiters, semicolons, etc.)
    let delim = one_of("()[]{};,.")
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
    // let token = ident.or(delim).or(punct).or(str_).or(num);

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
    use {crate::parse::ParseBuffer, std::mem};

    println!("{}", mem::size_of::<Lex>());

    // let src = "let x = 12 + null == le + 12.0001;";
    let src = "let x = 12 + null == le + 12.0001;";
    let parsed = lexer().parse(src).into_result().unwrap();
    println!("{:#?}", parsed);

    let mut buf = ParseBuffer::new(parsed);

    println!("{}", buf.peek(ast::Let));
    println!("{:?}", buf.next_lex());
    println!("{}", buf.peek(ast::Let));

    let lex = buf.step(|step| {
        step.next_lex()?;
        step.next_lex()?;
        step.next_lex()?;
        step.next_lex()?;
        // x = Some(step);
        Ok(())
        // Err::<Lex, _>(parse::Error { message: "".to_string(), span: Span::new(0, 0) })
    });
    println!("{:?}", lex);
    println!("{:?}", buf.predict());
}

#[test]
fn parse() {
    use crate::parse::ParseBuffer;

    let src = "let x = 12;";
    let parsed = lexer().parse(src).into_result().unwrap();
    println!("{:#?}", parsed);

    let mut buf = ParseBuffer::new(parsed);

    let err = buf
        .step(|step| {
            let _let: ast::Let = step.parse()?;
            let ident: Ident = step.parse()?;
            let eq: ast::Eq = step.parse()?;
            let float: LitFloat = step.parse()?;
            let semi: ast::Semi = step.parse()?;
            Ok((_let, ident, eq, float, semi))
        })
        .unwrap_err();

    println!("{err:?}");
}

#[test]
#[should_panic]
#[allow(clippy::diverging_sub_expression)]
fn token() {
    use crate::Token;

    let _ = Token![let];
    let _: Token![let] = panic!();
}
