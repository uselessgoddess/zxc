use crate::{
    lexer::{Ident, Lit},
    parenthesized,
    parse::{self, Parse, ParseBuffer},
    token, Token,
};

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    Any,
    Assign,
    // Range,
    // Or,
    // And,
    Compare,
    // BitOr,
    // BitXor,
    // BitAnd,
    // Shift,
    Arithmetic,
    Term,
    // Cast,
}

impl Precedence {
    fn of(op: &BinOp) -> Self {
        match op {
            BinOp::Add(_) | BinOp::Sub(_) => Precedence::Arithmetic,
            BinOp::Mul(_) | BinOp::Div(_) => Precedence::Term,
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add(Token![+]),
    Sub(Token![-]),
    Mul(Token![*]),
    Div(Token![/]),
}

macro_rules! select_op {
    (for($ahead:expr => $input:expr) $( $peek:expr => $map:expr ,)* _ => $err:expr) => {
        $(
            if $ahead.peek($peek) {
                $input.parse().map($map)
            } else
        )*
        { Err($err) }
    };
}

impl<'lex> Parse<'lex> for BinOp {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        let mut lookahead = input.lookahead();

        select_op! { for(lookahead => input)
            Token![+] => BinOp::Add,
            Token![-] => BinOp::Sub,
            Token![*] => BinOp::Mul,
            Token![/] => BinOp::Div,
            // Token![|] => BinOp::Or,
            // Token![||] => BinOp::BitOr,
            _ => lookahead.error()
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
        Let(Let<'a>),
        Paren(Paren<'a>),
        Unary(Unary<'a>),
        Binary(Binary<'a>),
    }
}

#[derive(Debug, Clone)]
pub struct Let<'a> {
    pub let_token: Token![let],
    pub pat: Ident<'a>,
    pub eq_token: Token![=],
    pub expr: Box<Expr<'a>>,
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
        let lhs = unary_expr(input)?;
        parse_expr(input, lhs, Precedence::Any)
    }
}

impl<'lex> Parse<'lex> for Let<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        Ok(Let {
            let_token: input.parse()?,
            pat: input.parse()?,
            eq_token: input.parse()?,
            expr: Box::new({
                let lhs = unary_expr(input)?;
                parse_expr(input, lhs, Precedence::Compare)?
            }),
        })
    }
}

impl<'lex> Parse<'lex> for Paren<'lex> {
    fn parse(_: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        todo!()
    }
}

impl<'lex> Parse<'lex> for Unary<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        Ok(Self { op: input.parse()?, expr: Box::new(unary_expr(input)?) })
    }
}

fn expr_paren<'lex>(input: &mut ParseBuffer<'lex>) -> parse::Result<Paren<'lex>> {
    let mut content;
    Ok(Paren { paren: parenthesized!(content in input), expr: content.parse()? })
}

fn peek_precedence(input: &mut ParseBuffer) -> Precedence {
    input.scan(|scan| if let Ok(op) = scan.parse() { Precedence::of(&op) } else { Precedence::Any })
}

fn parse_expr<'lex>(
    input: &mut ParseBuffer<'lex>,
    mut lhs: Expr<'lex>,
    base: Precedence,
) -> parse::Result<Expr<'lex>> {
    loop {
        let op = input.fork(|ahead, mut advance| match ahead.parse::<BinOp>() {
            Ok(op) if Precedence::of(&op) >= base => {
                advance.to(ahead);
                Some(op)
            }
            _ => None,
        });

        if let Some(op) = op {
            let precedence = Precedence::of(&op);
            let mut rhs = unary_expr(input)?;
            loop {
                let next = peek_precedence(input);
                if next > precedence || next == precedence && precedence == Precedence::Assign {
                    rhs = parse_expr(input, rhs, next)?;
                } else {
                    break;
                }
            }
            lhs = Expr::Binary(Binary { left: Box::new(lhs), op, right: Box::new(rhs) });
        } else {
            break;
        }
    }
    Ok(lhs)
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

    #[allow(clippy::never_loop)]
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
    } else if input.peek(Token![let]) {
        input.parse().map(Expr::Let)
    } else if input.is_empty() {
        Err(input.error("expected an expression"))
    } else {
        Err(input.error("unsupported expression"))
    }
}

#[cfg(test)]
use crate::util::lex_it;

#[test]
#[should_panic]
fn bit_op() {
    let mut input = lex_it!("!");
    input.parse::<BinOp>().unwrap();
}

#[test]
fn unary() {
    let mut input = lex_it!("let x = 12 + 3");
    let expr: Expr = input.parse().unwrap();

    println!("{expr:#?}");
}
