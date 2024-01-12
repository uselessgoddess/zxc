use {
    super::errors,
    compiler::{
        spec::{Cc, LinkerFlavor, Lld},
        Session,
    },
    std::path::PathBuf,
};

pub fn linker_and_flavor(sess: &Session) -> (PathBuf, LinkerFlavor) {
    fn infer_from(
        sess: &Session,
        linker: Option<PathBuf>,
        flavor: Option<LinkerFlavor>,
    ) -> Option<(PathBuf, LinkerFlavor)> {
        match (linker, flavor) {
            (Some(linker), Some(flavor)) => Some((linker, flavor)),
            // only the linker flavor is known; use the default linker for the selected flavor
            (None, Some(flavor)) => Some((
                PathBuf::from(match flavor {
                    LinkerFlavor::Gnu(Cc::Yes, _) => {
                        if cfg!(any(target_os = "solaris", target_os = "illumos")) {
                            // On historical Solaris systems, "cc" may have
                            // been Sun Studio, which is not flag-compatible
                            // with "gcc". This history casts a long shadow,
                            // and many modern illumos distributions today
                            // ship GCC as "gcc" without also making it
                            // available as "cc".
                            "gcc"
                        } else {
                            "cc"
                        }
                    }
                    LinkerFlavor::Gnu(_, Lld::Yes) | LinkerFlavor::Msvc(Lld::Yes) => "lld",
                    LinkerFlavor::Gnu(..) => "ld",
                    LinkerFlavor::Msvc(..) => "link.exe",
                }),
                flavor,
            )),
            (Some(linker), None) => {
                let stem = linker.file_stem().and_then(|stem| stem.to_str()).unwrap_or_else(|| {
                    sess.emit_fatal(errors::LinkerFileStem);
                });
                let flavor = sess.target.linker_flavor.with_linker_hints(stem);
                Some((linker, flavor))
            }
            (None, None) => None,
        }
    }

    if let Some(ret) = infer_from(
        sess,
        sess.target.linker.as_deref().map(PathBuf::from),
        Some(sess.target.linker_flavor),
    ) {
        return ret;
    }

    panic!("Not enough information provided to determine how to invoke the linker");
}
