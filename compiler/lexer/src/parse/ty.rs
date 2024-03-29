use crate::{
    lexer::Ident,
    parenthesized,
    parse::{self, Parse, ParseBuffer, Punctuated},
    token, Token,
};

ast_enum_of_structs! {
    #[derive(Debug, Clone)]
    pub enum Type<'lex> {
        Ident(Ident<'lex>),
        Pointer(Pointer<'lex>),
        Reference(Reference<'lex>),
        Paren(Paren<'lex>),
        Tuple(Tuple<'lex>),
        Never(Never),
    }
}

pub mod ptr {
    use crate::{parse, Parse, ParseBuffer, Token};

    #[derive(Debug, Clone)]
    pub enum Mutability {
        Const(Token![const]),
        Mut(Token![mut]),
    }

    impl Mutability {
        pub fn is_mut(&self) -> bool {
            matches!(self, Self::Mut(_))
        }
    }

    impl<'lex> Parse<'lex> for Mutability {
        fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
            let mut lookahead = input.lookahead();
            if lookahead.peek(Token![const]) {
                input.parse().map(Mutability::Const)
            } else if lookahead.peek(Token![mut]) {
                input.parse().map(Mutability::Mut)
            } else {
                Err(lookahead.error())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Pointer<'lex> {
    pub star: Token![*],
    pub qual: ptr::Mutability,
    pub ty: Box<Type<'lex>>,
}

impl<'lex> Parse<'lex> for Pointer<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        Ok(Self { star: input.parse()?, qual: input.parse()?, ty: input.parse()? })
    }
}

#[derive(Debug, Clone)]
pub struct Never {
    pub bang: Token![!],
}

impl<'lex> Parse<'lex> for Never {
    fn parse(input: &mut ParseBuffer<'lex>) -> crate::Result<Self> {
        Ok(Self { bang: input.parse()? })
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

#[derive(Debug, Clone)]
pub struct Reference<'a> {
    pub ref_token: Token![&],
    pub mutability: Option<Token![mut]>,
    pub ty: Box<Type<'a>>,
}

impl<'lex> Parse<'lex> for Type<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        parse_type(input)
    }
}

impl<'lex> Parse<'lex> for Reference<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        Ok(Reference { ref_token: input.parse()?, mutability: input.parse()?, ty: input.parse()? })
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
    } else if lookahead.peek(Token![&]) {
        input.parse().map(Type::Reference)
    } else if lookahead.peek(Token![!]) {
        input.parse().map(Type::Never)
    } else if lookahead.peek(Token![*]) {
        input.parse().map(Type::Pointer)
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
