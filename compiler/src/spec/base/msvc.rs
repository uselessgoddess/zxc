use crate::spec::{LinkerFlavor, Lld, TargetOptions};

pub fn opts() -> TargetOptions {
    // Suppress the verbose logo and authorship debugging output, which would needlessly
    // clog any log files.
    let pre_link_args = TargetOptions::link_args(LinkerFlavor::Msvc(Lld::No), &["/NOLOGO"]);

    TargetOptions {
        linker_flavor: LinkerFlavor::Msvc(Lld::No),
        is_like_windows: true,
        is_like_msvc: true,
        pre_link_args,
        ..Default::default()
    }
}
