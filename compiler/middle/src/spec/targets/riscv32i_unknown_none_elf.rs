use {
    crate::spec::{Cc, LinkerFlavor, Lld, RelocModel, Target, TargetOptions},
    macros::target_data_layout,
};

pub(crate) fn target() -> Target {
    Target {
        triple: "riscv32".into(),
        data_layout: target_data_layout!("e-m:e-p:32:32-i64:64-n32-S128"),
        pointer_width: 32,
        arch: "riscv32".into(),
        options: TargetOptions {
            linker_flavor: LinkerFlavor::Gnu(Cc::No, Lld::Yes),
            linker: Some("rust-lld".into()),
            cpu: "generic-rv32".into(),
            relocation_model: RelocModel::Static,
            ..Default::default()
        },
    }
}
