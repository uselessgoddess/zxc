use crate::{
    braced,
    lexer::{Ident, LitStr},
    parenthesized,
    parse::{self, Block, Parse, ParseBuffer, Punctuated, Type},
    token, Attribute, Span, Token,
};

#[derive(Debug, Clone)]
pub enum Visibility {
    Public(Token![pub]),
    Inherit,
}

impl Visibility {
    pub(crate) fn opt_span(&self) -> Option<Span> {
        match self {
            Visibility::Public(token) => Some(token.span),
            Visibility::Inherit => None,
        }
    }
}

impl<'lex> Parse<'lex> for Visibility {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        Ok(if input.peek(Token![pub]) { Self::Public(input.parse()?) } else { Self::Inherit })
    }
}

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

#[derive(Debug)]
pub struct ItemFn<'lex> {
    pub attrs: Vec<Attribute<'lex>>,
    pub vis: Visibility,
    pub sig: Signature<'lex>,
    pub block: Block<'lex>,
}

impl<'lex> Parse<'lex> for ItemFn<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        Ok(Self {
            attrs: input.do_in(Attribute::parse)?,
            vis: input.parse()?,
            sig: input.parse()?,
            block: input.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ForeignItem<'lex> {
    pub vis: Visibility,
    pub sig: Signature<'lex>,
    pub semi: Token![;],
}

impl<'lex> Parse<'lex> for ForeignItem<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        Ok(Self { vis: input.parse()?, sig: input.parse()?, semi: input.parse()? })
    }
}

#[derive(Debug)]
pub struct ForeignMod<'lex> {
    pub attrs: Vec<Attribute<'lex>>,
    pub abi: Abi<'lex>,
    pub brace: token::Brace,
    pub items: Vec<ForeignItem<'lex>>,
}

impl<'lex> Parse<'lex> for ForeignMod<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        let mut content;
        Ok(Self {
            attrs: input.do_in(Attribute::parse)?,
            abi: input.parse()?,
            brace: braced!(content in input),
            items: {
                let mut items = Vec::new();
                while !content.is_empty() {
                    items.push(content.parse()?);
                }
                items
            },
        })
    }
}

ast_enum_of_structs! {
    #[derive(Debug)]
    pub enum Item<'lex> {
        Fn(ItemFn<'lex>),
        Mod(Mod<'lex>),
        Foreign(ForeignMod<'lex>),
    }
}

impl<'lex> Parse<'lex> for Item<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        let attrs = input.do_in(Attribute::parse)?;
        let mut item = rest_items(input)?;
        match &mut item {
            Item::Fn(item) => item.attrs = attrs,
            Item::Foreign(item) => item.attrs = attrs,
            Item::Mod(_) => {}
        }
        Ok(item)
    }
}

fn peek_signature(input: &ParseBuffer<'_>) -> bool {
    input.scan(|fork| fork.parse::<Option<Abi>>().is_ok() && fork.peek(Token![fn]))
}

fn rest_items<'lex>(input: &mut ParseBuffer<'lex>) -> parse::Result<Item<'lex>> {
    let vis = input.parse().unwrap_or(Visibility::Inherit);

    let peek_sig = peek_signature(input);
    let mut lookahead = input.lookahead();

    if lookahead.peek(Token![fn]) || peek_sig {
        Ok(Item::Fn(ItemFn { attrs: vec![], vis, sig: input.parse()?, block: input.parse()? }))
    } else if lookahead.peek(Token![mod]) {
        parse_rest_mod(input, vis).map(Item::Mod)
    } else if lookahead.peek(Token![extern]) {
        input.parse().map(Item::Foreign)
    } else {
        Err(lookahead.error())
    }
}

#[derive(Debug)]
pub struct Mod<'lex> {
    pub vis: Visibility,
    pub mod_token: Token![mod],
    pub ident: Ident<'lex>,
    pub content: Option<(token::Brace, Vec<Item<'lex>>)>,
    pub semi: Option<Token![;]>,
}

impl<'lex> Parse<'lex> for Mod<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        let vis = input.parse()?;
        parse_rest_mod(input, vis)
    }
}

fn parse_rest_mod<'lex>(
    input: &mut ParseBuffer<'lex>,
    vis: Visibility,
) -> parse::Result<Mod<'lex>> {
    let mod_token = input.parse()?;
    let ident = input.parse()?;

    let mut lookahead = input.lookahead();
    if lookahead.peek(Token![;]) {
        Ok(Mod { vis, mod_token, ident, content: None, semi: Some(input.parse()?) })
    } else if lookahead.peek(token::Brace) {
        let mut content;
        let brace = braced!(content in input);

        let mut items = Vec::new();
        while !content.is_empty() {
            items.push(content.parse()?);
        }
        Ok(Mod { vis, mod_token, ident, content: Some((brace, items)), semi: None })
    } else {
        Err(lookahead.error())
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

#[test]
fn foreign_mod() {
    use crate::util::lex_it;

    let foreign: ForeignMod = lex_it!(
        r#"
        #[link(name = "msvcrt")]
        #[link(name = "c", kind = "static")]
        extern "C" {
            fn putchar(_: i32) -> i32;
            fn abort() -> !;
        }
        "#
    )
    .parse()
    .unwrap();

    assert!(foreign.attrs.len() == 2);
    assert!(foreign.items.len() == 2);
}

#[test]
fn mod_mod() {
    use crate::util::lex_it;

    let m: Mod = lex_it!(r#"mod foo { fn foo() {} }"#).parse().unwrap();
    assert!(m.semi.is_none());

    let m: Mod = lex_it!(r#"mod foo;"#).parse().unwrap();
    assert!(m.content.is_none());
}
