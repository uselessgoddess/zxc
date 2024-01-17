use {
    proc_macro2::{Ident, Span},
    quote::quote,
    std::collections::HashMap,
    syn::{
        braced,
        parse::{Parse, ParseStream},
        parse_macro_input,
        punctuated::Punctuated,
        Expr, Lit, LitStr, Result, Token,
    },
};

mod kw {
    syn::custom_keyword!(Keywords);
    syn::custom_keyword!(Symbols);
}

struct Keyword {
    name: Ident,
    value: LitStr,
}

impl Parse for Keyword {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![:]>()?;
        let value = input.parse()?;

        Ok(Keyword { name, value })
    }
}

enum Value {
    SameAsName,
    String(LitStr),
}

impl Parse for Value {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        if let Expr::Lit(expr) = input.parse()?
            && let Lit::Str(lit) = expr.lit
        {
            Ok(Value::String(lit))
        } else {
            todo!()
        }
    }
}

struct Symbol {
    name: Ident,
    value: Value,
}

impl Parse for Symbol {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let name = input.parse()?;
        let colon_token: Option<Token![:]> = input.parse()?;
        let value = if colon_token.is_some() { input.parse()? } else { Value::SameAsName };

        Ok(Symbol { name, value })
    }
}

struct Input {
    keywords: Punctuated<Keyword, Token![,]>,
    symbols: Punctuated<Symbol, Token![,]>,
}

impl Parse for Input {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        input.parse::<kw::Keywords>()?;
        let content;
        braced!(content in input);
        let keywords = Punctuated::parse_terminated(&content)?;

        input.parse::<kw::Symbols>()?;
        let content;
        braced!(content in input);
        let symbols = Punctuated::parse_terminated(&content)?;

        Ok(Input { keywords, symbols })
    }
}

struct Interned {
    idx: u32,
    span: Span,
}

struct Entries {
    map: HashMap<String, Interned>,
}

impl Entries {
    fn with_capacity(capacity: usize) -> Self {
        Entries { map: HashMap::with_capacity(capacity) }
    }

    fn insert(&mut self, span: Span, str: &str) -> u32 {
        if let Some(prev) = self.map.get(str) {
            todo!("symbol is duplicated")
        } else {
            let idx = self.len();
            self.map.insert(str.to_string(), Interned { idx, span });
            idx
        }
    }

    fn len(&self) -> u32 {
        u32::try_from(self.map.len()).expect("way too many symbols")
    }
}

pub fn symbols(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let Input { keywords, symbols } = parse_macro_input!(input as Input);

    let mut keyword_stream = quote! {};
    let mut symbols_stream = quote! {};
    let mut prefill_stream = quote! {};
    let mut entries = Entries::with_capacity(keywords.len() + symbols.len() + 10);

    for Keyword { name, value } in keywords.iter() {
        let idx = entries.insert(name.span(), &value.value());
        prefill_stream.extend(quote! {
            #value,
        });
        keyword_stream.extend(quote! {
            pub const #name: Symbol = Symbol::from_raw_unchecked(#idx);
        });
    }

    let mut prev: Option<(Span, String)> = None;
    for Symbol { name, value } in symbols.iter() {
        let span = name.span();
        if let Some((span, str)) = prev
            && name.to_string() < str
        {
            todo!("Symbol `{}` must precede `{str}", name.to_string())
        }
        prev = Some((span, name.to_string()));

        let value = match value {
            Value::SameAsName => name.to_string(),
            Value::String(lit) => lit.value(),
        };
        let idx = entries.insert(span, &value);
        prefill_stream.extend(quote! {
            #value,
        });
        symbols_stream.extend(quote! {
            pub const #name: Symbol = Symbol::from_raw_unchecked(#idx);
        });
    }

    for n in (0..10).map(|x| x.to_string()) {
        entries.insert(Span::call_site(), &n);
        prefill_stream.extend(quote! {
            #n,
        });
    }

    let digits_base = entries.map["0"].idx;
    let count = entries.len();
    let tokens = quote! {
        const SYMBOL_DIGITS_BASE: u32 = #digits_base;
        const PREINTERNED_SYMBOLS: u32 = #count;

        #[allow(non_upper_case_globals)]
        mod kw_generated {
            use super::Symbol;
            #keyword_stream
        }

        #[allow(non_upper_case_globals)]
        pub mod sym_generated {
            use super::Symbol;
            #symbols_stream
        }

        impl Interner {
            pub(crate) fn fresh() -> Self {
                Interner::prefill(&[
                    #prefill_stream
                ])
            }
        }
    };

    tokens.into()
}
