use {
    crate::abi::{Endian, TargetDataLayout},
    std::{
        collections::BTreeMap,
        ops::{Deref, DerefMut},
    },
};

mod base;

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
    pub fn lld_flavor(self) -> LldFlavor {
        match self {
            LinkerFlavor::Gnu(..) => LldFlavor::Ld,
            LinkerFlavor::Msvc(..) => LldFlavor::Link,
        }
    }

    pub fn is_gnu(&self) -> bool {
        matches!(self, LinkerFlavor::Gnu(..))
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum LldFlavor {
    Wasm,
    Ld64,
    Ld,
    Link,
}

impl LldFlavor {
    pub fn as_str(&self) -> &'static str {
        match self {
            LldFlavor::Wasm => "wasm",
            LldFlavor::Ld64 => "darwin",
            LldFlavor::Ld => "gnu",
            LldFlavor::Link => "link",
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        Some(match s {
            "darwin" => LldFlavor::Ld64,
            "gnu" => LldFlavor::Ld,
            "link" => LldFlavor::Link,
            "wasm" => LldFlavor::Wasm,
            _ => return None,
        })
    }
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

impl Target {
    pub fn expect_builtin(target: &str) -> Option<Target> {
        load_builtin(target)
    }
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

    pub pre_link_args: LinkArgs,
    pub late_link_args: LinkArgs,
    pub post_link_args: LinkArgs,

    pub executables: bool,

    /// String to prepend to the name of every dynamic library. Defaults to "lib".
    pub dll_prefix: Cow<str>,
    /// String to append to the name of every dynamic library. Defaults to ".so".
    pub dll_suffix: Cow<str>,
    /// String to append to the name of every executable.
    pub exe_suffix: Cow<str>,
    /// String to prepend to the name of every static library. Defaults to "lib".
    pub staticlib_prefix: Cow<str>,
    /// String to append to the name of every static library. Defaults to ".a".
    pub staticlib_suffix: Cow<str>,

    pub relocation_model: RelocModel,
    pub crt_static_respected: bool,
    pub crt_static_default: bool,
    pub crt_static_allows_dylibs: bool,
    pub position_independent_executables: bool,
    pub static_position_independent_executables: bool,

    pub is_like_msvc: bool,
    pub is_like_windows: bool,
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

            pre_link_args: LinkArgs::new(),
            late_link_args: LinkArgs::new(),
            post_link_args: LinkArgs::new(),

            executables: true,
            dll_prefix: "lib".into(),
            dll_suffix: ".so".into(),
            exe_suffix: "".into(),
            staticlib_prefix: "lib".into(),
            staticlib_suffix: ".a".into(),

            relocation_model: RelocModel::Pic,
            crt_static_respected: false,
            crt_static_default: false,
            crt_static_allows_dylibs: false,
            position_independent_executables: false,
            static_position_independent_executables: false,

            is_like_msvc: false,
            is_like_windows: false,
        }
    }
}

pub type LinkArgs = BTreeMap<LinkerFlavor, Vec<Cow<str>>>;

fn add_link_args_iter(
    link_args: &mut LinkArgs,
    flavor: LinkerFlavor,
    args: impl Iterator<Item = Cow<str>> + Clone,
) {
    let mut insert = |flavor| link_args.entry(flavor).or_default().extend(args.clone());
    insert(flavor);
    match flavor {
        LinkerFlavor::Gnu(cc, lld) => {
            assert_eq!(lld, Lld::No);
            insert(LinkerFlavor::Gnu(cc, Lld::Yes));
        }
        LinkerFlavor::Msvc(lld) => {
            assert_eq!(lld, Lld::No);
            insert(LinkerFlavor::Msvc(Lld::Yes));
        }
    }
}

fn add_link_args(link_args: &mut LinkArgs, flavor: LinkerFlavor, args: &[&'static str]) {
    add_link_args_iter(link_args, flavor, args.iter().copied().map(Cow::Borrowed))
}

impl TargetOptions {
    fn link_args(flavor: LinkerFlavor, args: &[&'static str]) -> LinkArgs {
        let mut link_args = LinkArgs::new();
        add_link_args(&mut link_args, flavor, args);
        link_args
    }

    fn add_pre_link_args(&mut self, flavor: LinkerFlavor, args: &[&'static str]) {
        add_link_args(&mut self.pre_link_args, flavor, args);
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum LinkOutputKind {
    /// Dynamically linked non position-independent executable.
    DynamicNoPicExe,
    /// Dynamically linked position-independent executable.
    DynamicPicExe,
    /// Statically linked non position-independent executable.
    StaticNoPicExe,
    /// Statically linked position-independent executable.
    StaticPicExe,
    /// Regular dynamic library ("dynamically linked").
    DynamicDylib,
    /// Dynamic library with bundled libc ("statically linked").
    StaticDylib,
    /// WASI module with a lifetime past the _initialize entry point
    WasiReactorExe,
}

#[derive(Clone, Copy, PartialEq, Hash, Debug)]
pub enum RelocModel {
    Static,
    Pic,
    Pie,
    DynamicNoPic,
    Ropi,
    Rwpi,
    RopiRwpi,
}

macro_rules! supported_targets {
    ( $(($triple:literal, $module:ident),)+ ) => {
        mod targets {
            $(pub(crate) mod $module;)+
        }

        /// List of supported targets
        pub const TARGETS: &[&str] = &[$($triple),+];

        fn load_builtin(target: &str) -> Option<Target> {
            let mut t = match target {
                $( $triple => targets::$module::target(), )+
                _ => return None,
            };
            Some(t)
        }

        #[cfg(test)]
        mod tests {
            mod imp;

            // Cannot put this into a separate file without duplication, make an exception.
            $(
                #[test] // `#[test]`
                fn $module() {
                    imp::test_target(crate::spec::targets::$module::target());
                }
            )+
        }
    };
}

supported_targets! {
    ("x86_64-pc-windows-gnu", x86_64_pc_windows_gnu),
    ("x86_64-pc-windows-msvc", x86_64_pc_windows_msvc),
}
