use {
    crate::{fx::FxIndexSet, idx, index, tls::with_session_globals, DroplessArena, Lock},
    lexer::Span,
    std::{fmt, mem, str},
};

idx::define_index! {
    pub struct Symbol = u32;
}

impl Symbol {
    pub fn intern(str: &str) -> Symbol {
        with_session_globals(|globals| globals.symbol_interner.intern(str))
    }

    pub fn as_str(&self) -> &'static str {
        with_session_globals(|globals| unsafe {
            mem::transmute(globals.symbol_interner.get(*self))
        })
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.as_str(), f)
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.as_str(), f)
    }
}

pub(crate) struct Interner(Lock<InternerInner>);

struct InternerInner {
    arena: DroplessArena,
    strings: FxIndexSet<&'static str>,
}

impl Interner {
    pub(crate) fn prefill(init: &[&'static str]) -> Self {
        Self(Lock::new(InternerInner {
            arena: Default::default(),
            strings: init.iter().copied().collect(),
        }))
    }

    #[inline]
    fn intern(&self, string: &str) -> Symbol {
        let mut inner = self.0.lock();
        if let Some(idx) = inner.strings.get_index_of(string) {
            return Symbol::new(idx);
        }

        let string: &str =
            unsafe { str::from_utf8_unchecked(inner.arena.alloc_slice(string.as_bytes())) };
        let string: &'static str = unsafe { &*(string as *const str) };

        let (idx, is_new) = inner.strings.insert_full(string);
        debug_assert!(is_new);

        Symbol::new(idx)
    }

    fn get(&self, symbol: Symbol) -> &str {
        self.0.lock().strings.get_index(symbol.index()).unwrap()
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl Ident {
    #[inline]
    pub const fn new(name: Symbol, span: Span) -> Ident {
        Ident { name, span }
    }

    pub fn from_parse(ident: lexer::Ident<'_>) -> Ident {
        Self::new(Symbol::intern(ident.ident()), ident.span)
    }

    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.name, f)
    }
}

macros::symbols! {
    Keywords {
        Empty:              "",

        Else:               "else",
        Let:                "let",
        Loop:               "loop",
        Fn:                 "fn",
        For:                "for",
        If:                 "if",
        Break:              "break",
        False:              "false",
        True:               "true",
        Return:             "return",
    }

    Symbols {
        i16,
        i32,
        i64,
        i8,
        isize,
        main,
        start,
    }
}

pub mod kw {
    pub use super::kw_generated::*;
}

pub mod sym {
    use crate::Symbol;

    pub use super::sym_generated::*;

    pub fn integer<N: TryInto<usize> + Copy + ToString>(n: N) -> Symbol {
        if let Ok(idx) = n.try_into() {
            if idx < 10 {
                return Symbol::from_raw(super::SYMBOL_DIGITS_BASE + idx as u32);
            }
        }
        Symbol::intern(&n.to_string())
    }
}
