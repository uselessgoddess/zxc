use crate::{
    braced,
    lexer::{Ident, Lit},
    parenthesized,
    parse::{self, Parse, ParseBuffer, Punctuated},
    token::{self},
    Error, Path, Token, Type,
};

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    Any,
    Assign,
    Or,
    And,
    Compare,
    // BitOr,
    // BitXor,
    // BitAnd,
    // Shift,
    Arithmetic,
    Term,
    Cast,
}

impl Precedence {
    fn of(op: &BinOp) -> Self {
        match op {
            BinOp::Add(_) | BinOp::Sub(_) => Precedence::Arithmetic,
            BinOp::Mul(_) | BinOp::Div(_) => Precedence::Term,
            BinOp::Eq(_)
            | BinOp::Lt(_)
            | BinOp::Le(_)
            | BinOp::Ne(_)
            | BinOp::Ge(_)
            | BinOp::Gt(_) => Precedence::Compare,
            BinOp::Or(_) => Precedence::Or,
            BinOp::And(_) => Precedence::And,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum BinOp {
    Add(Token![+]),
    Sub(Token![-]),
    Mul(Token![*]),
    Div(Token![/]),

    And(Token![&&]),
    Or(Token![||]),

    Eq(Token![==]),
    Ne(Token![!=]),
    Le(Token![<=]),
    Lt(Token![<]),
    Ge(Token![>=]),
    Gt(Token![>]),
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

            Token![&&] => BinOp::And,
            Token![||] => BinOp::Or,

            Token![==] => BinOp::Eq,
            Token![!=] => BinOp::Ne,
            Token![<=] => BinOp::Le,
            Token![<] => BinOp::Lt,
            Token![>=] => BinOp::Ge,
            Token![>] => BinOp::Gt,
            _ => lookahead.error()
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum UnOp {
    Deref(Token![*]),
    Not(Token![!]),
    Neg(Token![-]),
}

impl<'lex> Parse<'lex> for UnOp {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        // TODO: add lookahead implementation to `BinOp`
        let mut lookahead = input.lookahead();
        if lookahead.peek(Token![*]) {
            input.parse().map(UnOp::Deref)
        } else if lookahead.peek(Token![!]) {
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
        Path(Path<'a>),
        Block(Block<'a>),
        Paren(Paren<'a>),
        Unary(Unary<'a>),
        Binary(Binary<'a>),
        Return(Return<'a>),
        Cast(Cast<'a>),
        Call(Call<'a>),
        TailCall(TailCall<'a>),
        If(If<'a>),
        Break(Break<'a>),
        Loop(Loop<'a>),
        Assign(Assign<'a>),
        Reference(Reference<'a>),
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

#[derive(Debug, Clone)]
pub struct Return<'a> {
    pub return_token: Token![return],
    pub expr: Option<Box<Expr<'a>>>,
}

#[derive(Debug, Clone)]
pub struct Cast<'a> {
    pub expr: Box<Expr<'a>>,
    pub as_token: Token![as],
    pub ty: Box<Type<'a>>,
}

#[derive(Debug, Clone)]
pub struct Call<'a> {
    pub func: Box<Expr<'a>>,
    pub paren: token::Paren,
    pub args: Punctuated<Expr<'a>, Token![,]>,
}

#[derive(Debug, Clone)]
pub struct TailCall<'a> {
    pub receiver: Box<Expr<'a>>,
    pub dot: Token![.],
    pub func: Path<'a>,
    pub args: Option<(token::Paren, Punctuated<Expr<'a>, Token![,]>)>,
}

#[derive(Debug, Clone)]
pub struct If<'a> {
    pub if_token: Token![if],
    pub cond: Box<Expr<'a>>,
    pub then_branch: Block<'a>,
    pub else_branch: Option<(Token![else], Block<'a>)>,
}

#[derive(Debug, Clone)]
pub struct Break<'a> {
    pub break_token: Token![break],
    pub expr: Option<Box<Expr<'a>>>,
}

#[derive(Debug, Clone)]
pub struct Loop<'a> {
    pub loop_token: Token![loop],
    pub body: Block<'a>,
}

#[derive(Debug, Clone)]
pub struct Assign<'a> {
    pub left: Box<Expr<'a>>,
    pub eq_token: Token![=],
    pub right: Box<Expr<'a>>,
}

#[derive(Debug, Clone)]
pub struct Reference<'a> {
    pub ref_token: Token![&],
    pub mutability: Option<Token![mut]>,
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

impl<'lex> Parse<'lex> for If<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        Ok(Self {
            if_token: input.parse()?,
            cond: Box::new(input.parse()?),
            then_branch: input.parse()?,
            else_branch: if input.peek(Token![else]) {
                Some((input.parse()?, input.parse()?))
            } else {
                None
            },
        })
    }
}

impl<'lex> Parse<'lex> for Loop<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        Ok(Self { loop_token: input.parse()?, body: input.parse()? })
    }
}

impl<'lex> Parse<'lex> for Reference<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        Ok(Self { ref_token: input.parse()?, mutability: input.parse()?, expr: input.parse()? })
    }
}

fn else_block<'lex>(
    input: &mut ParseBuffer<'lex>,
) -> parse::Result<(Token![else], Box<Expr<'lex>>)> {
    let else_token: Token![else] = input.parse()?;

    let mut lookahead = input.lookahead();
    let else_branch = if lookahead.peek(Token![if]) {
        input.parse().map(Expr::If)?
    } else if lookahead.peek(token::Brace) {
        Expr::Block(input.parse()?)
    } else {
        return Err(lookahead.error());
    };

    Ok((else_token, Box::new(else_branch)))
}

fn expr_paren<'lex>(input: &mut ParseBuffer<'lex>) -> parse::Result<Paren<'lex>> {
    let mut content;
    Ok(Paren { paren: parenthesized!(content in input), expr: content.parse()? })
}

fn peek_precedence(input: &mut ParseBuffer) -> Precedence {
    input.scan(|scan| {
        if let Ok(op) = scan.parse() {
            Precedence::of(&op)
        } else if input.peek(Token![=]) {
            Precedence::Assign
        } else if input.peek(Token![as]) {
            Precedence::Cast
        } else {
            Precedence::Any
        }
    })
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
        } else if Precedence::Assign >= base && input.peek(Token![=]) {
            let eq_token: Token![=] = input.parse()?;
            let mut rhs = unary_expr(input)?;
            loop {
                let next = peek_precedence(input);
                if next >= Precedence::Assign {
                    rhs = parse_expr(input, rhs, next)?;
                } else {
                    break;
                }
            }
            lhs = Expr::Assign(Assign { left: Box::new(lhs), eq_token, right: Box::new(rhs) });
        } else if Precedence::Cast >= base && input.peek(Token![as]) {
            lhs = Expr::Cast(Cast {
                expr: Box::new(lhs),
                as_token: input.parse()?,
                ty: input.parse()?,
            });
        } else {
            break;
        }
    }
    Ok(lhs)
}

fn unary_expr<'lex>(input: &mut ParseBuffer<'lex>) -> parse::Result<Expr<'lex>> {
    if input.peek(Token![&]) {
        Ok(Expr::Reference(Reference {
            ref_token: input.parse()?,
            mutability: input.parse()?,
            expr: Box::new(unary_expr(input)?),
        }))
    } else if input.peek(Token![!]) || input.peek(Token![-]) || input.peek(Token![*]) {
        Ok(Expr::Unary(Unary { op: input.parse()?, expr: Box::new(unary_expr(input)?) }))
    } else {
        trailer_expr(input)
    }
}

#[rustfmt::skip]
fn can_begin_expr(input: &ParseBuffer) -> bool {
    input.peek(Ident) // value name or keyword
        || input.peek(token::Paren) // tuple
        || input.peek(token::Bracket) // array
        || input.peek(token::Brace) // block
        || input.peek(Lit) // literal
        || input.peek(Token![!]) // operator not
        || input.peek(Token![-]) // unary minus
        || input.peek(Token![*]) // dereference
        //    || input.peek(Token![|]) // closure
        //    || input.peek(Token![&]) // reference
        // || input.peek(Token![..]) // range notation
        || input.peek(Token![<]) // associated path
    // || input.peek(Token![::]) // global path
    // || input.peek(Lifetime) // labeled loop
    //|| input.peek(Token![#]) // expression attributes
}

fn expr_return<'lex>(input: &mut ParseBuffer<'lex>) -> parse::Result<Return<'lex>> {
    Ok(Return {
        return_token: input.parse()?,
        expr: {
            if can_begin_expr(input) {
                // NOTE: return is greedy and eats blocks after it even when in a
                // position where structs are not allowed, such as in if statement
                // conditions. For example:
                //
                // if return { print("A") } {} // Prints "A"
                Some(Box::new(input.parse()?))
            } else {
                None
            }
        },
    })
}

fn expr_break<'lex>(input: &mut ParseBuffer<'lex>) -> parse::Result<Break<'lex>> {
    let break_token: Token![break] = input.parse()?;
    Ok(Break {
        break_token,
        expr: if can_begin_expr(input) { Some(Box::new(input.parse()?)) } else { None },
    })
}

fn trailer_expr<'lex>(input: &mut ParseBuffer<'lex>) -> parse::Result<Expr<'lex>> {
    let mut e = atom_expr(input)?;

    loop {
        // Doesn't allow trailer expressions now
        // later there will be `func(a) |..| { b }` syntax

        if input.peek(token::Paren) {
            let mut content;
            e = Expr::Call(Call {
                func: Box::new(e),
                paren: parenthesized!(content in input),
                args: content.parse_terminated(Expr::parse, Token![,])?,
            });
        } else if input.peek(Token![.]) {
            e = Expr::TailCall(TailCall {
                receiver: Box::new(e),
                dot: input.parse()?,
                func: input.parse()?,
                args: {
                    if input.peek(token::Paren) {
                        let mut content;
                        Some((
                            parenthesized!(content in input),
                            content.parse_terminated(Expr::parse, Token![,])?,
                        ))
                    } else {
                        None
                    }
                },
            });
        } else {
            break;
        }
    }

    Ok(e)
}

fn atom_expr<'lex>(input: &mut ParseBuffer<'lex>) -> parse::Result<Expr<'lex>> {
    if input.peek(Ident) {
        input.parse().map(Expr::Path)
    } else if input.peek(Lit) {
        input.parse().map(Expr::Lit)
    } else if input.peek(token::Paren) {
        input.custom(expr_paren).map(Expr::Paren)
    } else if input.peek(token::Brace) {
        input.custom(Block::parse).map(Expr::Block)
    } else if input.peek(Token![break]) {
        expr_break(input).map(Expr::Break)
    } else if input.peek(Token![return]) {
        expr_return(input).map(Expr::Return)
    } else if input.peek(Token![if]) {
        input.parse().map(Expr::If)
    } else if input.peek(Token![loop]) {
        input.parse().map(Expr::Loop)
    } else if input.is_empty() {
        Err(input.error("expected an expression"))
    } else {
        Err(input.error("unsupported expression"))
    }
}

#[derive(Debug, Clone)]
pub struct Local<'lex> {
    pub let_token: Token![let],
    pub mutability: Option<Token![mut]>,
    pub pat: Ident<'lex>,
    pub eq: Token![=],
    pub expr: Box<Expr<'lex>>,
    pub semi: Token![;],
}

#[derive(Debug, Clone)]
pub enum Stmt<'lex> {
    Local(Local<'lex>),
    Expr(Expr<'lex>, Option<Token![;]>),
}

impl<'lex> Parse<'lex> for Stmt<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        parse_stmt(input, false)
    }
}

fn parse_stmt<'lex>(input: &mut ParseBuffer<'lex>, no_semi: bool) -> parse::Result<Stmt<'lex>> {
    if input.peek(Token![let]) {
        Ok(Stmt::Local(Local {
            let_token: input.parse()?,
            mutability: input.parse()?,
            pat: input.parse()?,
            eq: input.parse()?,
            expr: input.parse()?,
            semi: input.parse()?,
        }))
    } else {
        stmt_expr(input, no_semi)
    }
}

fn stmt_expr<'lex>(input: &mut ParseBuffer<'lex>, no_semi: bool) -> parse::Result<Stmt<'lex>> {
    let expr: Expr = input.parse()?;
    let semi: Option<Token![;]> = input.parse()?;

    if semi.is_some() {
        Ok(Stmt::Expr(expr, semi))
    } else if no_semi || !requires_terminator(&expr) {
        Ok(Stmt::Expr(expr, None))
    } else {
        Err(input.error("expected semicolon"))
    }
}

#[derive(Debug, Clone)]
pub struct Block<'lex> {
    pub brace: token::Brace,
    pub stmts: Vec<Stmt<'lex>>,
}

impl<'lex> Parse<'lex> for Block<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        let mut content;
        Ok(Block { brace: braced!(content in input), stmts: content.do_in(Block::parse_within)? })
    }
}

impl Block<'_> {
    pub fn parse_within<'lex>(input: &mut ParseBuffer<'lex>) -> parse::Result<Vec<Stmt<'lex>>> {
        let mut stmts = Vec::new();
        loop {
            // while let semi @ Some(_) = input.parse()? {
            //     stmts.push(Stmt::Expr(Expr::Verbatim(TokenStream::new()), semi));
            // }
            if input.is_empty() {
                break;
            }
            let stmt = parse_stmt(input, true)?;
            let requires_semicolon = match &stmt {
                Stmt::Expr(stmt, None) => requires_terminator(stmt),
                _ => false,
            };
            stmts.push(stmt);
            if input.is_empty() {
                break;
            } else if requires_semicolon {
                return Err(input.error("unexpected token, expected `;`"));
            }
        }
        Ok(stmts)
    }
}

fn requires_terminator(expr: &Expr<'_>) -> bool {
    !matches!(expr, Expr::If(_) | Expr::Loop(_))
}

impl<'lex> Parse<'lex> for Assign<'lex> {
    fn parse(input: &mut ParseBuffer<'lex>) -> parse::Result<Self> {
        use crate::parse::Spanned;

        let expr = input.parse()?;
        match expr {
            Expr::Assign(inner) => Ok(inner),
            _ => Err(Error::new(expr.span(), "expected assignment expression")),
        }
    }
}

#[cfg(test)]
use {crate::util::lex_it, std::assert_matches::assert_matches};

#[test]
#[should_panic]
fn bit_op() {
    let mut input = lex_it!("!");
    input.parse::<BinOp>().unwrap();
}

#[test]
fn unary() {
    let mut input = lex_it!("- - 3");
    let expr: Expr = input.parse().unwrap();

    println!("{expr:#?}");
}

#[test]
fn tail_call() {
    {
        let mut input = lex_it!("12.i64");
        let expr: Expr = input.parse().unwrap();

        println!("{expr:#?}");
    }
    {
        let mut input = lex_it!("12.i64(12 + 32, 234)");
        let expr: Expr = input.parse().unwrap();

        println!("{expr:#?}");
    }
}

#[test]
fn if_expr() {
    let expr: If = lex_it!("if x + 1 == y * 2 {  }").parse().unwrap();
    assert!(expr.else_branch.is_none());

    let expr: If = lex_it!("if x {} else {}").parse().unwrap();
    assert!(expr.else_branch.is_some());
}

#[test]
fn bool_expr() {
    let expr: Expr = lex_it!("true && false").parse().unwrap();

    if let Expr::Binary(Binary { left, op, right }) = expr
        && let Expr::Lit(_) = &*left
        && let BinOp::And(_) = op
        && let Expr::Lit(_) = &*right
    {
    } else {
        panic!()
    }
}

#[test]
fn return_expr() {
    let expr: Expr = lex_it!("return").parse().unwrap();
    assert_matches!(expr, Expr::Return(Return { expr, .. }) if expr.is_none());

    let expr: Expr = lex_it!("return print(\"Hi\")").parse().unwrap();
    assert_matches!(expr, Expr::Return(Return { expr: Some(box Expr::Call(Call { .. })), .. }));
}

#[test]
fn loop_expr() {
    let expr: Expr = lex_it!("loop { 12 } + loop { break 12; }").parse().unwrap();
    assert_matches!(
        expr,
        Expr::Binary(Binary {
            left: box Expr::Loop(Loop { .. }),
            right: box Expr::Loop(Loop { .. }),
            ..
        })
    );
}

#[test]
fn ref_expr() {
    let expr: Expr = lex_it!("&mut 12 + &12").parse().unwrap();
    if let Expr::Binary(Binary {
        left: box Expr::Reference(Reference { mutability: Some(_), expr: box Expr::Lit(_), .. }),
        right: box Expr::Reference(Reference { mutability: None, expr: box Expr::Lit(_), .. }),
        ..
    }) = expr
    {
    } else {
        panic!()
    }
}

#[test]
fn as_expr() {
    let expr: Expr = lex_it!("&mut 12 as *mut i32").parse().unwrap();
    assert_matches!(
        expr,
        Expr::Cast(Cast { expr: box Expr::Reference(..), ty: box Type::Pointer(..), .. })
    );
}

#[test]
fn as_precedence() {
    let expr: Expr = lex_it!("x == x as i32").parse().unwrap();
    assert_matches!(expr, Expr::Binary(..));
}
