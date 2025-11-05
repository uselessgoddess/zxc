use {
    crate::spec::{base, Cc, LinkerFlavor, Lld, Target},
    macros::target_data_layout,
};

pub fn target() -> Target {
    let mut base = base::none::opts();
    base.cpu = "generic-rv32".into();
    base.linker_flavor = LinkerFlavor::Gnu(Cc::No, Lld::Yes);
    base.linker = Some("rust-lld".into());
    base.add_pre_link_args(LinkerFlavor::Gnu(Cc::No, Lld::No), &["-m", "elf32lriscv"]);
    base.add_pre_link_args(LinkerFlavor::Gnu(Cc::No, Lld::Yes), &["-m", "elf32lriscv"]);

    Target {
        triple: "riscv32i-unknown-none-elf".into(),
        pointer_width: 32,
        data_layout: target_data_layout!(
            "e-m:e-p:32:32-i64:64-n32-S128"
        ),
        arch: "riscv32".into(),
        options: base,
    }
}
