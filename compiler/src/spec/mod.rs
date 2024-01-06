use crate::{
    abi,
    abi::{Endian, TargetDataLayout},
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
