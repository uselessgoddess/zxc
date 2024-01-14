use crate::{
    bracketed, parenthesized, parse, token, Error, Expr, Ident, Parse, ParseBuffer, Token,
};

ast_enum_of_structs! {
    #[derive(Debug)]
    pub enum Meta<'lex> {
        Word(Ident<'lex>),
        List(List<'lex>),
        Value(NameValue<'lex>),
    }
}

impl<'lex> Parse<'lex> for Meta<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        let ident = input.parse()?;
        Ok(if input.peek(token::Paren) {
            let mut content;
            Meta::List(List { ident, paren: parenthesized!(content in input), items: content })
        } else if input.peek(Token![=]) {
            Meta::Value(NameValue { ident, eq: input.parse()?, value: input.parse()? })
        } else {
            Meta::Word(ident)
        })
    }
}

impl<'lex> Meta<'lex> {
    pub fn path(&self) -> Ident<'lex> {
        match self {
            Meta::Word(ident) => *ident,
            Meta::List(meta) => meta.ident,
            Meta::Value(meta) => meta.ident,
        }
    }
}

impl<'lex> List<'lex> {
    pub fn parse_args<T: Parse<'lex>>(&mut self) -> parse::Result<T> {
        let parse = self.items.parse()?;
        if !self.items.is_empty() {
            Err(Error::new(
                self.items.span,
                "unexpected token: `parse_args` must parse whole input",
            ))
        } else {
            Ok(parse)
        }
    }
}

#[derive(Debug)]
pub struct List<'lex> {
    pub ident: Ident<'lex>,
    pub paren: token::Paren,
    pub items: ParseBuffer<'lex>,
}

#[derive(Debug, Clone)]
pub struct NameValue<'lex> {
    pub ident: Ident<'lex>,
    pub eq: Token![=],
    pub value: Expr<'lex>,
}

#[derive(Debug)]
pub struct Attribute<'lex> {
    pub pound: Token![#],
    pub bracket: token::Bracket,
    pub meta: Meta<'lex>,
}

impl<'lex> Attribute<'lex> {
    pub fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Vec<Self>> {
        let mut attrs = Vec::new();
        while input.peek(Token![#]) {
            let mut content;
            attrs.push(Attribute {
                pound: input.parse()?,
                bracket: bracketed!(content in input),
                meta: content.parse()?,
            })
        }
        Ok(attrs)
    }
}

#[cfg(test)]
use crate::util::lex_it;

#[test]
fn parse_meta() {
    if let Meta::List(mut list) = lex_it!("link(name = \"c\")").parse().unwrap()
        && let Expr::Assign(_) = list.parse_args().unwrap()
    {
    } else {
        panic!()
    }
}

#[test]
fn parse_attrs() {
    let attrs =
        lex_it!("#[link(name = \"c\")]#[link_name = \"puts\"]").do_in(Attribute::parse).unwrap();
    println!("{attrs:#?}");
}
