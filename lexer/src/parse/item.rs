use crate::{
    lexer::{Ident, LitStr},
    parenthesized,
    parse::{self, Block, Parse, ParseBuffer, Punctuated, Type},
    token, Token,
};

#[derive(Debug, Clone)]
pub struct Abi<'lex> {
    pub extern_token: Token![extern],
    pub name: Option<LitStr<'lex>>,
}

impl<'lex> Parse<'lex> for Abi<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        Ok(Abi { extern_token: input.parse()?, name: input.parse()? })
    }
}

impl<'lex> Parse<'lex> for Option<Abi<'lex>> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        if input.peek(Token![extern]) { input.parse().map(Some) } else { Ok(None) }
    }
}

#[derive(Debug, Clone)]
pub struct FnArg<'lex> {
    pub mutability: Option<Token![mut]>,
    pub pat: Ident<'lex>,
    pub colon: Token![:],
    pub ty: Box<Type<'lex>>,
}

impl<'lex> Parse<'lex> for FnArg<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        Ok(Self {
            mutability: input.parse()?,
            pat: input.parse()?,
            colon: input.parse()?,
            ty: input.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum ReturnType<'lex> {
    Default,
    Type(Token![->], Box<Type<'lex>>),
}

impl<'lex> Parse<'lex> for ReturnType<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        if input.peek(Token![->]) {
            let arrow = input.parse()?;
            let ty = input.parse()?;
            Ok(ReturnType::Type(arrow, Box::new(ty)))
        } else {
            Ok(ReturnType::Default)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Signature<'lex> {
    pub abi: Option<Abi<'lex>>,
    pub fn_token: Token![fn],
    pub ident: Ident<'lex>,
    pub paren: token::Paren,
    pub inputs: Punctuated<FnArg<'lex>, Token![,]>,
    pub output: ReturnType<'lex>,
}

impl<'lex> Parse<'lex> for Signature<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        let mut content;
        Ok(Self {
            abi: input.parse()?,
            fn_token: input.parse()?,
            ident: input.parse()?,
            paren: parenthesized!(content in input),
            inputs: content.parse_terminated(FnArg::parse, Token![,])?,
            output: input.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ItemFn<'lex> {
    pub sig: Signature<'lex>,
    pub block: Block<'lex>,
}

impl<'lex> Parse<'lex> for ItemFn<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        Ok(Self { sig: input.parse()?, block: input.parse()? })
    }
}

#[test]
fn fn_sig() {
    use crate::util::lex_it;

    let mut input = lex_it!("extern \"C\" fn foo(x: i32) -> (i32, i32, ())");
    let fn_sig: Signature = input.parse().unwrap();

    if let ReturnType::Type(_, ty) = fn_sig.output
        && let Type::Tuple(tuple) = &*ty
        && tuple.items.len() == 3
    {
    } else {
        panic!()
    }
}
