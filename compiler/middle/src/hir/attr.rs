use {
    crate::symbol::{Ident, Symbol},
    lexer::{Expr, Lit, Meta, Parse, Punctuated, Span, Spanned, Token},
};

pub mod meta {
    pub use super::MetaItemKind::{List, NameValue, Word};
}

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

    #[inline]
    pub fn has_name(&self, name: Symbol) -> bool {
        self.path.name == name
    }
}

fn meta_lit(value: Expr<'_>) -> Option<MetaLit> {
    if let Expr::Lit(lit) = value
        && let Lit::Str(str) = lit
    {
        Some(MetaLit {
            // TODO: simple trimming quotes to allow prove concept of extern blocks linking naming
            repr: Symbol::intern(str.lit.trim_matches('"').trim_end_matches('"')),
            span: str.span,
        })
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

pub fn filter_by_name(attrs: &[MetaItem], name: Symbol) -> impl Iterator<Item = &MetaItem> {
    attrs.iter().filter(move |attr| attr.has_name(name))
}

pub fn find_by_name(attrs: &[MetaItem], name: Symbol) -> Option<&MetaItem> {
    filter_by_name(attrs, name).next()
}

pub fn contains_name(attrs: &[MetaItem], name: Symbol) -> bool {
    find_by_name(attrs, name).is_some()
}
