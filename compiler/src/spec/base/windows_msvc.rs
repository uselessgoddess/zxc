use crate::spec::{LinkerFlavor, Lld, TargetOptions};

pub fn opts() -> TargetOptions {
    let base = super::msvc::opts();

    // TODO: Now zxc doesn't have a real `std`, so we have to allow the `main` definition
    let late_link_args = TargetOptions::link_args(LinkerFlavor::Msvc(Lld::No), &["msvcrt.lib"]);

    TargetOptions {
        os: "windows".into(),
        env: "msvc".into(),
        vendor: "pc".into(),
        dll_prefix: "".into(),
        dll_suffix: ".dll".into(),
        exe_suffix: ".exe".into(),
        staticlib_prefix: "".into(),
        staticlib_suffix: ".lib".into(),
        crt_static_allows_dylibs: true,
        crt_static_respected: true,
        late_link_args,
        ..base
    }
}
