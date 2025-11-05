use crate::spec::{Cc, LinkerFlavor, Lld, TargetOptions};

pub fn opts() -> TargetOptions {
    TargetOptions {
        os: "none".into(),
        linker_flavor: LinkerFlavor::Gnu(Cc::No, Lld::Yes),
        linker: Some("rust-lld".into()),
        executables: true,
        ..Default::default()
    }
}
