use crate::symbol;

pub struct SessionGlobals {
    pub(crate) symbol_interner: symbol::Interner,
}

impl Default for SessionGlobals {
    fn default() -> Self {
        Self { symbol_interner: symbol::Interner::fresh() }
    }
}

pub fn with_session_globals<R, F>(f: F) -> R
where
    F: FnOnce(&SessionGlobals) -> R,
{
    SESSION_GLOBALS.with(f)
}

scoped_tls::scoped_thread_local!(static SESSION_GLOBALS: SessionGlobals);

pub unsafe fn in_session_globals<R>(globals: SessionGlobals, f: impl FnOnce() -> R) -> R {
    assert!(
        !SESSION_GLOBALS.is_set(),
        "SESSION_GLOBALS should never be overwritten! \
         Use another thread if you need another SessionGlobals"
    );
    SESSION_GLOBALS.set(&globals, f)
}
