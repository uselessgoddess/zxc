use {
    super::command::Command,
    std::{
        env,
        ffi::{OsStr, OsString},
        iter,
        path::Path,
    },
};

pub trait Linker {
    fn is_cc(&self) -> bool {
        false
    }

    fn cmd(&mut self) -> &mut Command;
    fn add_object(&mut self, path: &Path);
    fn link_dylib(&mut self, lib: &str);
    fn output_filename(&mut self, path: &Path);
    fn set_output_kind(&mut self, kind: LinkOutputKind, out: &Path);
    fn gc_sections(&mut self, keep_metadata: bool);
    fn no_gc_sections(&mut self);
    fn optimize(&mut self);
}

macro_rules! generate {
    ($($ty:ty)*) => { $(
        impl $ty {
            pub fn verbatim_args(&mut self, args: impl IntoIterator<Item: AsRef<OsStr>>) {
                for arg in args {
                    self.cmd().arg(arg);
                }
            }

            pub fn link_args(
                &mut self,
                args: impl IntoIterator<Item: AsRef<OsStr>, IntoIter: ExactSizeIterator>,
            ) {
               let args = args.into_iter();
                if !self.is_cc() {
                    self.verbatim_args(args);
                } else if args.len() != 0 {
                    let mut combined_arg = OsString::from("-Wl");
                    for arg in args {
                        combined_arg.push(",");
                        combined_arg.push(arg);
                    }
                    self.cmd().arg(combined_arg);
                }
            }

            pub fn link_arg(&mut self, arg: impl AsRef<OsStr>) {
                self.link_args(iter::once(arg))
            }

            pub fn link_or_cc_args(&mut self, args: impl IntoIterator<Item: AsRef<OsStr>>) {
                self.verbatim_args(args)
            }

            pub fn link_or_cc_arg(&mut self, arg: impl AsRef<OsStr>) {
                self.link_or_cc_args(iter::once(arg))
            }
        }
    )* }
}

generate! {
    GccLinker<'_>
    MsvcLinker<'_>
    dyn Linker + '_
}

impl dyn Linker + '_ {
    pub fn args(&mut self, args: impl IntoIterator<Item: AsRef<OsStr>>) {
        self.cmd().args(args);
    }
}

use cc::windows_registry;

use middle::{
    Session,
    sess::OptLevel,
    spec::{Cc, LinkOutputKind, LinkerFlavor, Lld},
};

/// Disables non-English messages from localized linkers.
/// Such messages may cause issues with text encoding on Windows (#35785)
/// and prevent inspection of linker output in case of errors, which we occasionally do.
/// This should be acceptable because other messages from rustc are in English anyway,
/// and may also be desirable to improve searchability of the linker diagnostics.
pub fn disable_localization(linker: &mut Command) {
    // No harm in setting both env vars simultaneously.
    // Unix-style linkers.
    linker.env("LC_ALL", "C");
    // MSVC's `link.exe`.
    linker.env("VSLANG", "1033");
}

pub fn get_linker<'a>(
    sess: &'a Session,
    linker: &Path,
    flavor: LinkerFlavor,
    self_contained: bool,
    target_cpu: &'a str,
) -> Box<dyn Linker + 'a> {
    let msvc_tool = windows_registry::find_tool(&sess.opts.triple, "link.exe");

    // If our linker looks like a batch script on Windows then to execute this
    // we'll need to spawn `cmd` explicitly. This is primarily done to handle
    // emscripten where the linker is `emcc.bat` and needs to be spawned as
    // `cmd /c emcc.bat ...`.
    //
    // This worked historically but is needed manually since #42436 (regression
    // was tagged as #42791) and some more info can be found on #44443 for
    // emscripten itself.
    let mut cmd = match linker.to_str() {
        Some(linker) if cfg!(windows) && linker.ends_with(".bat") => Command::bat_script(linker),
        _ => match flavor {
            LinkerFlavor::Gnu(Cc::No, Lld::Yes) | LinkerFlavor::Msvc(Lld::Yes) => {
                Command::lld(linker, flavor.lld_flavor())
            }
            LinkerFlavor::Msvc(Lld::No)
                if sess.opts.C.linker.is_none() && sess.target.linker.is_none() =>
            {
                Command::new(msvc_tool.as_ref().map_or(linker, |t| t.path()))
            }
            _ => Command::new(linker),
        },
    };

    // UWP apps have API restrictions enforced during Store submissions.
    // To comply with the Windows App Certification Kit,
    // MSVC needs to link with the Store versions of the runtime libraries (vcruntime, msvcrt, etc).
    let t = &sess.target;
    if matches!(flavor, LinkerFlavor::Msvc(..)) && t.vendor == "uwp" {
        if let Some(ref tool) = msvc_tool {
            let original_path = tool.path();
            if let Some(root_lib_path) = original_path.ancestors().nth(4) {
                let arch = match t.arch.as_ref() {
                    "x86_64" => Some("x64"),
                    "x86" => Some("x86"),
                    "aarch64" => Some("arm64"),
                    "arm" => Some("arm"),
                    _ => None,
                };
                if let Some(ref a) = arch {
                    // FIXME: Move this to `fn linker_with_args`.
                    let mut arg = OsString::from("/LIBPATH:");
                    arg.push(format!("{}\\lib\\{}\\store", root_lib_path.display(), a));
                    cmd.arg(&arg);
                } else {
                    panic!("arch is not supported");
                }
            } else {
                panic!("MSVC root path lib location not found");
            }
        } else {
            panic!("link.exe not found");
        }
    }

    // The compiler's sysroot often has some bundled tools, so add it to the
    // PATH for the child.
    let mut new_path = sess.inner_tools_path(self_contained);
    let mut msvc_changed_path = false;
    if sess.target.is_like_msvc {
        if let Some(ref tool) = msvc_tool {
            cmd.args(tool.args());
            for (k, v) in tool.env() {
                if k == "PATH" {
                    new_path.extend(env::split_paths(v));
                    msvc_changed_path = true;
                } else {
                    cmd.env(k, v);
                }
            }
        }
    }

    if !msvc_changed_path {
        if let Some(path) = env::var_os("PATH") {
            new_path.extend(env::split_paths(&path));
        }
    }
    cmd.env("PATH", env::join_paths(new_path).unwrap());

    // FIXME: Move `/LIBPATH` addition for uwp targets from the linker construction
    // to the linker args construction.
    assert!(cmd.get_args().is_empty() || sess.target.vendor == "uwp");
    match flavor {
        LinkerFlavor::Gnu(cc, _) => Box::new(GccLinker {
            cmd,
            sess,
            target_cpu,
            hinted_static: None,
            is_ld: cc == Cc::No,
            is_gnu: flavor.is_gnu(),
        }) as Box<dyn Linker>,
        LinkerFlavor::Msvc(..) => Box::new(MsvcLinker { cmd, sess }) as Box<dyn Linker>,
    }
}

pub struct GccLinker<'a> {
    cmd: Command,
    sess: &'a Session,
    target_cpu: &'a str,
    hinted_static: Option<bool>, // Keeps track of the current hinting mode.
    is_ld: bool,                 // Link as ld
    is_gnu: bool,
}

impl<'a> Linker for GccLinker<'a> {
    fn is_cc(&self) -> bool {
        !self.is_ld
    }

    fn cmd(&mut self) -> &mut Command {
        &mut self.cmd
    }

    fn add_object(&mut self, path: &Path) {
        self.cmd.arg(path);
    }

    fn output_filename(&mut self, path: &Path) {
        self.cmd.arg("-o").arg(path);
    }

    fn link_dylib(&mut self, lib: &str) {
        self.cmd.arg(format!("-l{lib}"));
    }

    fn set_output_kind(&mut self, kind: LinkOutputKind, out: &Path) {
        match kind {
            LinkOutputKind::DynamicNoPicExe => {
                if !self.is_ld && self.is_gnu {
                    self.cmd.arg("-no-pie");
                }
            }
            LinkOutputKind::DynamicPicExe => {
                if !self.sess.target.is_like_windows {
                    self.cmd.arg("-pie");
                }
            }
            LinkOutputKind::StaticNoPicExe => {
                self.cmd.arg("-static");
                if !self.is_ld && self.is_gnu {
                    self.cmd.arg("-no-pie");
                }
            }
            LinkOutputKind::StaticPicExe => {
                if !self.is_ld {
                    self.cmd.arg("-static-pie");
                } else {
                    self.cmd.args(&["-static", "-pie", "--no-dynamic-linker", "-z", "text"]);
                }
            }
            LinkOutputKind::DynamicDylib => self.build_dylib(out),
            LinkOutputKind::StaticDylib => {
                self.cmd.arg("-static");
                self.build_dylib(out);
            }
            LinkOutputKind::WasiReactorExe => {
                self.link_args(&["--entry", "_initialize"]);
            }
        }
    }

    fn gc_sections(&mut self, keep_metadata: bool) {
        if self.is_gnu && !keep_metadata {
            self.link_arg("--gc-sections");
        }
    }

    fn no_gc_sections(&mut self) {
        if self.is_gnu {
            self.link_arg("--no-gc-sections");
        }
    }

    fn optimize(&mut self) {
        if self.is_gnu
            && let OptLevel::Default | OptLevel::Aggressive = self.sess.opts.C.opt_level
        {
            self.link_arg("-O1");
        }
    }
}

impl<'a> GccLinker<'a> {
    fn build_dylib(&mut self, out: &Path) {
        if false {
            todo!("osx")
        } else {
            self.link_or_cc_arg("-shared");
            if let Some(name) = out.file_name() {
                if self.sess.target.is_like_windows {
                    let mut implib = OsString::from(&*self.sess.target.staticlib_prefix);
                    implib.push(name);
                    implib.push(&*self.sess.target.staticlib_suffix);

                    let mut out_implib = OsString::from("--out-implib=");
                    out_implib.push(out.with_file_name(implib));
                    self.link_arg(out_implib);
                } else {
                    let mut soname = OsString::from("-soname=");
                    soname.push(name);
                    self.link_arg(soname);
                }
            }
        }
    }
}

pub struct MsvcLinker<'a> {
    cmd: Command,
    sess: &'a Session,
}

impl<'a> Linker for MsvcLinker<'a> {
    fn cmd(&mut self) -> &mut Command {
        &mut self.cmd
    }

    fn add_object(&mut self, path: &Path) {
        self.cmd.arg(path);
    }

    fn output_filename(&mut self, path: &Path) {
        let mut arg = OsString::from("/OUT:");
        arg.push(path);
        self.cmd.arg(&arg);
    }

    fn link_dylib(&mut self, lib: &str) {
        self.cmd.arg(format!("{lib}.lib"));
    }

    fn set_output_kind(&mut self, output_kind: LinkOutputKind, out_filename: &Path) {
        match output_kind {
            LinkOutputKind::DynamicNoPicExe
            | LinkOutputKind::DynamicPicExe
            | LinkOutputKind::StaticNoPicExe
            | LinkOutputKind::StaticPicExe => {}
            LinkOutputKind::DynamicDylib | LinkOutputKind::StaticDylib => {
                self.cmd.arg("/DLL");
                let mut arg: OsString = "/IMPLIB:".into();
                arg.push(out_filename.with_extension("dll.lib"));
                self.cmd.arg(arg);
            }
            LinkOutputKind::WasiReactorExe => todo!(),
        }
    }

    fn gc_sections(&mut self, _keep_metadata: bool) {
        // MSVC's ICF link optimization is slow for debug builds
        if self.sess.opts.C.opt_level != OptLevel::No {
            self.cmd.arg("/OPT:REF,ICF");
        } else {
            self.cmd.arg("/OPT:REF,NOICF");
        }
    }

    fn no_gc_sections(&mut self) {
        self.cmd.arg("/OPT:NOREF,NOICF");
    }

    fn optimize(&mut self) {
        // nothing
    }
}
