use {
    crate::spec::{Cc, LinkerFlavor, Lld, RelocModel, Target},
    std::assert_matches::assert_matches,
};

pub(super) fn test_target(mut target: Target) {
    target.check_consistency();
}

impl Target {
    fn check_consistency(&self) {
        assert_eq!(self.is_like_windows, self.os == "windows" || self.os == "uefi");
        if self.is_like_msvc {
            assert!(self.is_like_windows);
        }

        assert_eq!(self.is_like_msvc, matches!(self.linker_flavor, LinkerFlavor::Msvc(..)));

        for args in [&self.pre_link_args, &self.late_link_args, &self.post_link_args] {
            for (&flavor, flavor_args) in args {
                assert!(!flavor_args.is_empty());
                match self.linker_flavor {
                    LinkerFlavor::Gnu(..) => {
                        assert_matches!(flavor, LinkerFlavor::Gnu(..));
                    }
                    LinkerFlavor::Msvc(..) => {
                        assert_matches!(flavor, LinkerFlavor::Msvc(..))
                    }
                }

                let check_noncc = |noncc_flavor| {
                    if let Some(noncc_args) = args.get(&noncc_flavor) {
                        for arg in flavor_args {
                            if let Some(suffix) = arg.strip_prefix("-Wl,") {
                                assert!(noncc_args.iter().any(|a| a == suffix));
                            }
                        }
                    }
                };

                match self.linker_flavor {
                    LinkerFlavor::Gnu(Cc::Yes, lld) => check_noncc(LinkerFlavor::Gnu(Cc::No, lld)),
                    _ => {}
                }
            }

            for cc in [Cc::No, Cc::Yes] {
                assert_eq!(
                    args.get(&LinkerFlavor::Gnu(cc, Lld::No)),
                    args.get(&LinkerFlavor::Gnu(cc, Lld::Yes)),
                );
            }
            assert_eq!(
                args.get(&LinkerFlavor::Msvc(Lld::No)),
                args.get(&LinkerFlavor::Msvc(Lld::Yes)),
            );
        }

        assert_ne!(self.vendor, "");
        assert_ne!(self.os, "");
        if !self.can_use_os_unknown() {
            assert_ne!(self.os, "unknown");
        }

        // if self.os == "none" && (self.arch != "bpf" && self.arch != "hexagon") {
        //     assert!(!self.dynamic_linking);
        // }
        // if self.only_cdylib
        //     || self.crt_static_allows_dylibs
        //     || !self.late_link_args_dynamic.is_empty()
        // {
        //     assert!(self.dynamic_linking);
        // }
        // if self.dynamic_linking && !(self.is_like_wasm && self.os != "emscripten") {
        //     assert_eq!(self.relocation_model, RelocModel::Pic);
        // }
        if self.position_independent_executables {
            assert_eq!(self.relocation_model, RelocModel::Pic);
        }
        // if self.relocation_model == RelocModel::Pic && self.os != "uefi" {
        //     assert!(self.dynamic_linking || self.position_independent_executables);
        // }
        if self.static_position_independent_executables {
            assert!(self.position_independent_executables);
        }
        if self.position_independent_executables {
            assert!(self.executables);
        }

        if self.crt_static_default || self.crt_static_allows_dylibs {
            assert!(self.crt_static_respected);
        }
    }

    fn can_use_os_unknown(&self) -> bool {
        self.triple == "wasm32-unknown-unknown"
            || self.triple == "wasm64-unknown-unknown"
            || (self.env == "sgx" && self.vendor == "fortanix")
    }
}
