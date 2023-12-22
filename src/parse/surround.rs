use crate::{
    parenthesized,
    parse::{expr, DelimSpan, ParseBuffer},
    Span,
};

pub trait Spanned {
    fn span(&self) -> Span;
}

macro_rules! simple_span {
    ($($ty:ty)*) => {$(
        impl Spanned for $ty {
            fn span(&self) -> Span { self.span }
        }
    )*};
}

simple_span! {
    crate::lexer::LitFloat
    crate::lexer::LitBool
    crate::lexer::LitInt
    crate::lexer::LitStr<'_>

    crate::lexer::Ident<'_>
}

fn lookahead_span(lo: Span, hi: Span) -> Span {
    Span::new(lo.start, hi.end)
}

impl Spanned for expr::Paren<'_> {
    fn span(&self) -> Span {
        let DelimSpan { lt, rt, .. } = self.paren.span;
        lookahead_span(lt, rt)
    }
}

impl Spanned for expr::Unary<'_> {
    fn span(&self) -> Span {
        // FIXME: add implementation for `UnOp`
        //  lookahead_span(self.op.span(), self.expr.span())
        self.expr.span()
    }
}

impl Spanned for expr::Binary<'_> {
    fn span(&self) -> Span {
        lookahead_span(self.left.span(), self.right.span())
    }
}

#[test]
fn spanned() {
    use crate::{parenthesized, parse::Expr, util::lex_it};

    let mut input = lex_it!("(1 + 3)");

    let mut content;
    parenthesized!(content in &mut input);

    println!("{}", content.parse::<Expr>().unwrap().span())
}
