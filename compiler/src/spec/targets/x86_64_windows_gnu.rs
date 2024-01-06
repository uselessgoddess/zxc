use {
    crate::spec::{base, Target},
    macros::target_data_layout,
};

pub fn target() -> Target {
    let mut base = base::windows_gnu::opts();
    base.cpu = "x86-64".into();
    base.linker = Some("x86_64-w64-mingw32-gcc".into());

    Target {
        triple: "x86_64-pc-windows-gnu".into(),
        pointer_width: 64,
        data_layout: target_data_layout!(
            "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
        ),
        arch: "x86_64".into(),
        options: base,
    }
}
