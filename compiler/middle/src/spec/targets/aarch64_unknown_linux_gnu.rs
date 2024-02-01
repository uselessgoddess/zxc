use {
    crate::spec::{base, Target, TargetOptions},
    macros::target_data_layout,
};

pub fn target() -> Target {
    Target {
        triple: "aarch64-unknown-linux-gnu".into(),
        pointer_width: 64,
        data_layout: target_data_layout!("e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"),
        arch: "aarch64".into(),
        options: TargetOptions { ..base::linux_gnu::opts() },
    }
}
