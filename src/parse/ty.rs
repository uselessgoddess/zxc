use crate::{
    lexer::Ident,
    parenthesized,
    parse::{self, Parse, ParseBuffer, Punctuated},
    token,
    util::lex_it,
    Token,
};

ast_enum_of_structs! {
    #[derive(Debug, Clone)]
    pub enum Type<'lex> {
        Ident(Ident<'lex>),
        Paren(Paren<'lex>),
        Tuple(Tuple<'lex>),
    }
}

#[derive(Debug, Clone)]
pub struct Paren<'lex> {
    pub paren: token::Paren,
    pub item: Box<Type<'lex>>,
}

#[derive(Debug, Clone)]
pub struct Tuple<'lex> {
    pub paren: token::Paren,
    pub items: Punctuated<Type<'lex>, Token![,]>,
}

impl<'lex> Parse<'lex> for Type<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        parse_type(input)
    }
}

fn parse_type<'lex>(input: &mut ParseBuffer<'lex>) -> parse::Result<Type<'lex>> {
    let mut lookahead = input.lookahead();

    if lookahead.peek(token::Paren) {
        let mut content;
        let paren = parenthesized!(content in input);
        if content.is_empty() {
            return Ok(Type::Tuple(Tuple { paren, items: Punctuated::new() }));
        }

        let first: Type = content.parse()?;
        Ok(if content.peek(Token![,]) {
            Type::Tuple(Tuple {
                paren,
                items: {
                    let mut items = Punctuated::new();
                    items.push_value(first);
                    items.push_punct(content.parse()?);
                    while !content.is_empty() {
                        items.push_value(content.parse()?);
                        if content.is_empty() {
                            break;
                        }
                        items.push_punct(content.parse()?);
                    }
                    items
                },
            })
        } else {
            Type::Paren(Paren { paren, item: Box::new(first) })
        })
    } else if lookahead.peek(Ident) {
        input.parse().map(Type::Ident)
    } else {
        Err(lookahead.error())
    }
}

#[test]
fn typee() {
    use crate::util::lex_it;

    let mut input = lex_it!("(t1, t2, (t3), (t4, ), )");
    let ty: Type = input.parse().unwrap();

    println!("{ty:#?}");
}
