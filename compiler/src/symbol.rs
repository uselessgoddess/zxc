use {
    crate::{fx::FxIndexSet, sess::with_session_globals, DroplessArena, Lock},
    lexer::Span,
    std::{fmt, mem, str},
};

index_vec::define_index_type! {
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

    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.name, f)
    }
}

// macro_rules! symbols {
//     // (in $mod:ident { $kw:ident: $str:literal, }) => {$(
//     //     pub mod $mod {
// //
//     //     }
//     // )*};
//
//     ($kw:ident: $str:literal) => {
//         impl Interner {
//             pub(crate) fn fresh() -> Self {
//                 Interner::prefill(&[
//                     $($str),*
//                 ])
//             }
//         }
//
//         $(
//             pub mod kw {
//
//             }
//         )*
//     };
// }
