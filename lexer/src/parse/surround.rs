use crate::{
    attr,
    parse::{self, expr, ty, Abi, DelimSpan, ReturnType, Signature, Stmt},
    token, Assign, Break, ForeignItem, ForeignMod, If, ItemFn, Loop, Span,
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

pub(crate) fn lookahead_span(lo: Span, hi: Span) -> Span {
    Span::new(lo.start, hi.end)
}

impl Spanned for token::Paren {
    fn span(&self) -> Span {
        let DelimSpan { lt, rt, .. } = self.span;
        lookahead_span(lt, rt)
    }
}

impl Spanned for token::Brace {
    fn span(&self) -> Span {
        let DelimSpan { lt, rt, .. } = self.span;
        lookahead_span(lt, rt)
    }
}

impl Spanned for token::Bracket {
    fn span(&self) -> Span {
        let DelimSpan { lt, rt, .. } = self.span;
        lookahead_span(lt, rt)
    }
}

impl Spanned for expr::Paren<'_> {
    fn span(&self) -> Span {
        self.paren.span()
    }
}

impl Spanned for expr::Block<'_> {
    fn span(&self) -> Span {
        let DelimSpan { lt, rt, .. } = self.brace.span;
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

impl Spanned for expr::Return<'_> {
    fn span(&self) -> Span {
        if let Some(expr) = &self.expr {
            lookahead_span(self.return_token.span, expr.span())
        } else {
            self.return_token.span
        }
    }
}

impl Spanned for expr::Call<'_> {
    fn span(&self) -> Span {
        lookahead_span(self.func.span(), self.paren.span())
    }
}

impl Spanned for expr::TailCall<'_> {
    fn span(&self) -> Span {
        lookahead_span(
            self.receiver.span(),
            if let Some((paren, _)) = &self.args { paren.span.rt } else { self.func.span },
        )
    }
}

impl Spanned for parse::Local<'_> {
    fn span(&self) -> Span {
        lookahead_span(self.let_token.span, self.semi.span)
    }
}

impl Spanned for Stmt<'_> {
    fn span(&self) -> Span {
        match self {
            Stmt::Local(local) => local.span(),
            Stmt::Expr(expr, semi) => {
                if let Some(semi) = semi {
                    lookahead_span(expr.span(), semi.span)
                } else {
                    expr.span()
                }
            }
        }
    }
}

impl Spanned for ty::Tuple<'_> {
    fn span(&self) -> Span {
        self.paren.span()
    }
}

impl Spanned for ty::Paren<'_> {
    fn span(&self) -> Span {
        self.paren.span()
    }
}

impl Spanned for Abi<'_> {
    fn span(&self) -> Span {
        let base = self.extern_token.span;
        lookahead_span(base, self.name.as_ref().map(Spanned::span).unwrap_or(base))
    }
}

impl Spanned for Signature<'_> {
    fn span(&self) -> Span {
        let lo = self.abi.as_ref().map(Spanned::span).unwrap_or(self.fn_token.span);
        let hi = match &self.output {
            ReturnType::Default => self.paren.span(),
            ReturnType::Type(_, ty) => ty.span(),
        };
        lookahead_span(lo, hi)
    }
}

impl Spanned for If<'_> {
    fn span(&self) -> Span {
        let hi = if let Some((_, expr)) = &self.else_branch {
            expr.span()
        } else {
            self.then_branch.span()
        };
        lookahead_span(self.if_token.span, hi)
    }
}

impl Spanned for Break<'_> {
    fn span(&self) -> Span {
        if let Some(expr) = &self.expr {
            lookahead_span(self.break_token.span, expr.span())
        } else {
            self.break_token.span
        }
    }
}

impl Spanned for Loop<'_> {
    fn span(&self) -> Span {
        lookahead_span(self.loop_token.span, self.body.span())
    }
}

impl Spanned for Assign<'_> {
    fn span(&self) -> Span {
        lookahead_span(self.left.span(), self.right.span())
    }
}

impl Spanned for attr::List<'_> {
    fn span(&self) -> Span {
        lookahead_span(self.ident.span, self.paren.span())
    }
}

impl Spanned for attr::NameValue<'_> {
    fn span(&self) -> Span {
        lookahead_span(self.ident.span, self.value.span())
    }
}

impl Spanned for attr::Attribute<'_> {
    fn span(&self) -> Span {
        lookahead_span(self.pound.span, self.bracket.span())
    }
}

impl Spanned for ty::Never {
    fn span(&self) -> Span {
        self.bang.span
    }
}

impl Spanned for ItemFn<'_> {
    fn span(&self) -> Span {
        lookahead_span(self.sig.span(), self.block.span())
    }
}

impl Spanned for ForeignItem<'_> {
    fn span(&self) -> Span {
        lookahead_span(self.sig.span(), self.semi.span)
    }
}

impl Spanned for ForeignMod<'_> {
    fn span(&self) -> Span {
        let lo =
            if let Some(first) = self.attrs.first() { first.pound.span } else { self.abi.span() };
        lookahead_span(lo, self.brace.span())
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
