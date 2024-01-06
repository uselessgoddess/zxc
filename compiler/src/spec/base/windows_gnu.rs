use crate::spec::{base, TargetOptions};

#[rustfmt::skip]
pub fn opts() -> TargetOptions {
    TargetOptions {
        os: "windows".into(),
        env: "gnu".into(),
        vendor: "pc".into(),
        linker: Some("gcc".into()),
        ..Default::default()
    }
}
