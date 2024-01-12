use {
    crate::{
        abi,
        abi::{Endian, TargetDataLayout},
    },
    std::ops::{Deref, DerefMut},
};

mod base;
pub mod targets;

/// Linker is called through a C/C++ compiler.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Cc {
    Yes,
    No,
}

/// Linker is LLD.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Lld {
    Yes,
    No,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum LinkerFlavor {
    Gnu(Cc, Lld),
    Msvc(Lld),
}

impl LinkerFlavor {
    fn infer_linker_hints(linker_stem: &str) -> (Option<Cc>, Option<Lld>) {
        let stem = linker_stem
            .rsplit_once('-')
            .and_then(|(lhs, rhs)| rhs.chars().all(char::is_numeric).then_some(lhs))
            .unwrap_or(linker_stem);

        if stem == "emcc"
            || stem == "gcc"
            || stem.ends_with("-gcc")
            || stem == "g++"
            || stem.ends_with("-g++")
            || stem == "clang"
            || stem.ends_with("-clang")
            || stem == "clang++"
            || stem.ends_with("-clang++")
        {
            (Some(Cc::Yes), Some(Lld::No))
        } else if stem == "wasm-ld"
            || stem.ends_with("-wasm-ld")
            || stem == "ld.lld"
            || stem == "lld"
            || stem == "rust-lld"
            || stem == "lld-link"
        {
            (Some(Cc::No), Some(Lld::Yes))
        } else if stem == "ld" || stem.ends_with("-ld") || stem == "link" {
            (Some(Cc::No), Some(Lld::No))
        } else {
            (None, None)
        }
    }

    fn with_hints(self, (cc_hint, lld_hint): (Option<Cc>, Option<Lld>)) -> LinkerFlavor {
        match self {
            LinkerFlavor::Gnu(cc, lld) => {
                LinkerFlavor::Gnu(cc_hint.unwrap_or(cc), lld_hint.unwrap_or(lld))
            }
            LinkerFlavor::Msvc(lld) => LinkerFlavor::Msvc(lld_hint.unwrap_or(lld)),
        }
    }

    pub fn with_linker_hints(self, linker_stem: &str) -> LinkerFlavor {
        self.with_hints(LinkerFlavor::infer_linker_hints(linker_stem))
    }
}

type Cow<T> = std::borrow::Cow<'static, T>;

#[derive(PartialEq, Clone, Debug)]
pub struct Target {
    pub pointer_width: u32,
    pub triple: Cow<str>,
    pub arch: Cow<str>,
    pub data_layout: TargetDataLayout,
    pub options: TargetOptions,
}

impl Deref for Target {
    type Target = TargetOptions;

    fn deref(&self) -> &Self::Target {
        &self.options
    }
}

impl DerefMut for Target {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.options
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct TargetOptions {
    pub endian: Endian,
    pub os: Cow<str>,
    pub env: Cow<str>,
    pub vendor: Cow<str>,
    pub linker: Option<Cow<str>>,
    pub linker_flavor: LinkerFlavor,
    pub cpu: Cow<str>,
}

impl Default for TargetOptions {
    fn default() -> Self {
        Self {
            endian: Endian::Little,
            os: "none".into(),
            env: "".into(),
            vendor: "".into(),
            linker: None,
            linker_flavor: LinkerFlavor::Gnu(Cc::Yes, Lld::No),
            cpu: "generic".into(),
        }
    }
}
