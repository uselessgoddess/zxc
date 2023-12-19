use crate::{
    braced,
    lexer::{
        ast::{self, Paren1, Paren2, Punct},
        Ident, Lit,
    },
    parenthesized,
    parse::{self, Parse, ParseBuffer},
    token, Lex, Span, Token,
};

#[derive(Debug, Clone)]
pub enum BinOp {
    Add(Token![+]),
    Sub(Token![-]),
    Mul(Token![*]),
    Div(Token![/]),
    Or(Token![||]),
    BitOr(Token![|]),
}

impl<'lex> Parse<'lex> for BinOp {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        let (Lex::Punct(punct), _) = input.next_lex()? else {
            return Err(input.error("expected binary operator"));
        };
        match punct {
            Punct::Plus(op) => Ok(BinOp::Add(op)),
            Punct::Minus(op) => Ok(BinOp::Sub(op)),
            Punct::Slash(op) => Ok(BinOp::Div(op)),
            Punct::Star(op) => Ok(BinOp::Mul(op)),
            Punct::Or(op) => Ok(BinOp::BitOr(op)),
            Punct::OrOr(op) => Ok(BinOp::Or(op)),
            _ => Err(input.error("expected binary operator")),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnOp {
    Not(Token![!]),
    Neg(Token![-]),
}

impl<'lex> Parse<'lex> for UnOp {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        // TODO: add lookahead implementation to `BinOp`
        let mut lookahead = input.lookahead();
        if lookahead.peek(Token![!]) {
            input.parse().map(UnOp::Not)
        } else if lookahead.peek(Token![-]) {
            input.parse().map(UnOp::Neg)
        } else {
            Err(lookahead.error())
        }
    }
}

ast_enum_of_structs! {
    #[derive(Debug, Clone)]
    pub enum Expr<'a> {
        Lit(Lit<'a>),
        Paren(Paren<'a>),
        Unary(Unary<'a>),
        Binary(Binary<'a>),
    }
}

#[derive(Debug, Clone)]
pub struct Paren<'a> {
    pub paren: token::Paren,
    pub expr: Box<Expr<'a>>,
}

#[derive(Debug, Clone)]
pub struct Binary<'a> {
    pub left: Box<Expr<'a>>,
    pub op: BinOp,
    pub right: Box<Expr<'a>>,
}

#[derive(Debug, Clone)]
pub struct Unary<'a> {
    pub op: UnOp,
    pub expr: Box<Expr<'a>>,
}

impl<'lex> Parse<'lex> for Expr<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        todo!()
    }
}

impl<'lex> Parse<'lex> for Paren<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        todo!()
    }
}

impl<'lex> Parse<'lex> for Unary<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        todo!()
    }
}

fn expr_paren<'lex>(input: &mut ParseBuffer<'lex>) -> parse::Result<Paren<'lex>> {
    let mut content;
    Ok(Paren { paren: parenthesized!(content in input), expr: content.parse()? })
}

fn unary_expr<'lex>(input: &mut ParseBuffer<'lex>) -> parse::Result<Expr<'lex>> {
    if input.peek(Token![!]) || input.peek(Token![-]) {
        Ok(Expr::Unary(Unary { op: input.parse()?, expr: Box::new(unary_expr(input)?) }))
    } else {
        trailer_expr(input)
    }
}

fn trailer_expr<'lex>(input: &mut ParseBuffer<'lex>) -> parse::Result<Expr<'lex>> {
    let e = atom_expr(input)?;

    loop {
        // Doesn't allow trailer expressions now
        // later there will be `func(a) |..| { b }` syntax
        break;
    }

    Ok(e)
}

fn atom_expr<'lex>(input: &mut ParseBuffer<'lex>) -> parse::Result<Expr<'lex>> {
    if input.peek(Lit) {
        input.parse().map(Expr::Lit)
    } else if input.peek(token::Paren) {
        input.custom(expr_paren).map(Expr::Paren)
    }
    // else if input.peek(Ident)
    //     || input.peek(Token![::])
    //     || input.peek(Token![<])
    //     || input.peek(Token![self])
    //     || input.peek(Token![Self])
    //     || input.peek(Token![super])
    //     || input.peek(Token![crate])
    // {
    //     path_or_macro_or_struct(input)
    // }
    else if input.is_empty() {
        Err(input.error("expected an expression"))
    } else {
        // if input.peek(token::Brace) {
        //     let scan = input.fork();
        //     let mut content;
        //     braced!(content in scan);
        //     if content.parse::<Expr>().is_ok() && content.is_empty() {
        //         let expr_block = verbatim::between(input, &scan);
        //         input.advance_to(&scan);
        //         return Ok(Expr::Verbatim(expr_block));
        //     }
        // }
        Err(input.error("unsupported expression"))
    }
}

#[test]
fn expr() {}
