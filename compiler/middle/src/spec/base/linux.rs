use crate::spec::TargetOptions;

pub fn opts() -> TargetOptions {
    TargetOptions {
        os: "linux".into(),
        position_independent_executables: true,
        crt_static_respected: true,
        ..Default::default()
    }
}
