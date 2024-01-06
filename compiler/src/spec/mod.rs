use {
    crate::{
        abi,
        abi::{Endian, TargetDataLayout},
    },
    std::borrow::Cow,
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

#[derive(PartialEq, Clone, Debug)]
pub struct Target {
    pub pointer_width: u32,
    pub triple: &'static str,
    pub arch: &'static str,
    pub data_layout: TargetDataLayout,
    pub options: TargetOptions,
}

#[derive(PartialEq, Clone, Debug)]
pub struct TargetOptions {
    pub endian: Endian,
    pub os: Cow<'static, str>,
    pub env: Cow<'static, str>,
    pub vendor: Cow<'static, str>,
    pub linker: Option<Cow<'static, str>>,
    pub linker_flavor: LinkerFlavor,
    pub cpu: Cow<'static, str>,
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
