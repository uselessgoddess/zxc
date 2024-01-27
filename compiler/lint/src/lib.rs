#![feature(let_chains)]

pub mod builtin;
mod levels;

use errors::{DiagnosticBuilder, DiagnosticMessage};
pub use levels::{LintId, LintStore};

#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum Level {
    Allow,
    Warn,
    Deny,
}

impl Level {
    pub fn as_str(&self) -> &'static str {
        match self {
            Level::Allow => "allow",
            Level::Warn => "warn",
            Level::Deny => "deny",
        }
    }

    pub fn as_cmd_flag(self) -> &'static str {
        match self {
            Level::Allow => "-A",
            Level::Warn => "-W",
            Level::Deny => "-D",
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Lint {
    pub name: &'static str,
    pub default_level: Level,
    pub desc: &'static str,
}

impl Lint {
    pub fn name_lower(&self) -> String {
        self.name.to_ascii_lowercase()
    }

    pub const fn default_in_macro() -> Self {
        Self { name: "", default_level: Level::Deny, desc: "" }
    }
}

#[macro_export]
macro_rules! declare_lint {
    ($vis: vis $NAME: ident, $Level: ident, $desc: expr) => (
        #[allow(non_upper_case_globals)]
        $vis static $NAME: &$crate::Lint = &$crate::Lint {
            name: stringify!($NAME),
            default_level: $crate::Level::$Level,
            desc: $desc,
            ..$crate::Lint::default_in_macro()
        };
    );
}

#[macro_export]
macro_rules! declare_lints_pass {
    ($name:ident => [$($lint:expr),* $(,)?]) => {
        #[allow(non_upper_case_globals)]
        pub static $name: &[&'static $crate::Lint] = &[$($lint),*];
    };
}

// TODO: move out of here
pub fn register_lints(store: &mut LintStore) {
    store.register_lints(builtin::HardwiredLints);
}

pub trait DecorateLint<'a> {
    fn decorate_lint(self, diag: &mut DiagnosticBuilder<'a, ()>);
    fn message(&self) -> DiagnosticMessage;
}
