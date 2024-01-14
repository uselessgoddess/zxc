use {
    crate::symbol::{Ident, Symbol},
    lexer::{Expr, Lit, Meta, Parse, Punctuated, Span, Spanned, Token},
};

#[derive(Debug, Clone)]
pub struct MetaItem {
    pub path: Ident,
    pub kind: MetaItemKind,
    pub span: Span,
}

impl MetaItem {
    pub fn from_parse(meta: Meta) -> Option<Self> {
        let span = meta.span();
        let (path, kind) = match meta {
            Meta::Word(path) => (path, MetaItemKind::Word),
            Meta::List(mut list) => (list.ident, {
                let metas =
                    Punctuated::<_, Token![,]>::parse_terminated_with(&mut list.items, Meta::parse)
                        .ok()?;
                MetaItemKind::List(
                    metas
                        .into_iter()
                        .map(|meta| {
                            Some(match meta {
                                Meta::Word(path) => NestedMeta::Word(Ident::from_parse(path)),
                                Meta::Value(nv) => NestedMeta::NameValue(
                                    Ident::from_parse(nv.ident),
                                    meta_lit(nv.value)?,
                                ),
                                _ => todo!(),
                            })
                        })
                        .collect::<Option<Vec<_>>>()?,
                )
            }),
            Meta::Value(nv) => (nv.ident, MetaItemKind::NameValue(meta_lit(nv.value)?)),
        };
        Some(MetaItem { path: Ident::from_parse(path), kind, span })
    }
}

fn meta_lit(value: Expr<'_>) -> Option<MetaLit> {
    if let Expr::Lit(lit) = value
        && let Lit::Str(str) = lit
    {
        Some(MetaLit { repr: Symbol::intern(str.lit), span: str.span })
    } else {
        None
    }
}

#[derive(Debug, Clone)]
pub enum MetaItemKind {
    Word,
    List(Vec<NestedMeta>),
    NameValue(MetaLit),
}

#[derive(Debug, Clone)]
pub enum NestedMeta {
    Word(Ident),
    NameValue(Ident, MetaLit),
}

#[derive(Debug, Clone)]
pub struct MetaLit {
    pub repr: Symbol,
    pub span: Span,
}
