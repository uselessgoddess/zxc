use {
    crate::spec::{base, Cc, LinkerFlavor, Lld, Target},
    macros::target_data_layout,
};

pub fn target() -> Target {
    let mut base = base::linux_gnu::opts();
    base.cpu = "x86-64".into();
    base.add_pre_link_args(LinkerFlavor::Gnu(Cc::Yes, Lld::No), &["-m64"]);
    base.static_position_independent_executables = true;

    Target {
        triple: "x86_64-unknown-linux-gnu".into(),
        pointer_width: 64,
        data_layout: target_data_layout!(
            "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
        ),
        arch: "x86_64".into(),
        options: base,
    }
}
