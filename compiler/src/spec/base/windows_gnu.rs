use crate::spec::{add_link_args, Cc, LinkerFlavor, Lld, TargetOptions};

#[rustfmt::skip]
pub fn opts() -> TargetOptions {
    let mut pre_link_args = TargetOptions::link_args(
        LinkerFlavor::Gnu(Cc::No, Lld::No),
        &[
            // Enable ASLR
            "--dynamicbase",
            // ASLR will rebase it anyway so leaving that option enabled only leads to confusion
            "--disable-auto-image-base",
        ],
    );
    add_link_args(
        &mut pre_link_args,
        LinkerFlavor::Gnu(Cc::Yes, Lld::No),
        &[
            // Tell GCC to avoid linker plugins
            "-fno-use-linker-plugin",
            "-Wl,--dynamicbase",
            "-Wl,--disable-auto-image-base",
        ],
    );

    let mingw_libs = &[
        "-lmsvcrt",
        "-lmingwex",
        "-lmingw32",
        "-lgcc",
        "-lmsvcrt",
        "-luser32",
        "-lkernel32",
    ];
    let mut late_link_args =
        TargetOptions::link_args(LinkerFlavor::Gnu(Cc::No, Lld::No), mingw_libs);
    add_link_args(&mut late_link_args, LinkerFlavor::Gnu(Cc::Yes, Lld::No), mingw_libs);
    
    TargetOptions {
        os: "windows".into(),
        env: "gnu".into(),
        vendor: "pc".into(),
        linker: Some("gcc".into()),

        dll_prefix: "".into(),
        dll_suffix: ".dll".into(),
        exe_suffix: ".exe".into(),
        is_like_windows: true,
        pre_link_args,
        late_link_args,
        ..Default::default()
    }
}
