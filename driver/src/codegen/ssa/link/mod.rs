mod command;
mod linker;

use {
    super::errors,
    crate::codegen::ssa::link::linker::Linker,
    cc::windows_registry,
    command::Command,
    compiler::{
        sess::{
            self, output, DiagnosticBuilder, Handler, IntoDiagnostic, ModuleType, OutputFilenames,
            OutputType,
        },
        spec::{Cc, LinkOutputKind, LinkerFlavor, Lld, RelocModel},
        symbol::Symbol,
        ErrorGuaranteed, Session, Tx,
    },
    std::{
        fs, io, mem,
        ops::Deref,
        path::{Path, PathBuf},
        process::{ExitStatus, Output, Stdio},
    },
};

fn collect_module_types(sess: &Session) -> Vec<ModuleType> {
    vec![output::default_output_for_target(sess)]
}

#[derive(Debug)]
pub struct ModuleInfo {
    pub target_cpu: String,
    pub module_types: Vec<ModuleType>,
    pub local_module_name: Symbol,
}

impl ModuleInfo {
    pub fn new(tcx: Tx, target_cpu: String) -> Self {
        ModuleInfo {
            target_cpu,
            module_types: collect_module_types(tcx.sess),
            local_module_name: Symbol::intern(&tcx.sess.local_module_name()),
        }
    }
}

#[derive(Debug)]
pub struct CompiledModule {
    pub name: String,
    pub object: Option<PathBuf>,
}

#[derive(Debug)]
pub struct CodegenResults {
    pub modules: Vec<CompiledModule>,
    pub info: ModuleInfo,
}

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
                    LinkerFlavor::Gnu(Cc::Yes, _) => "cc",
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

fn link_output_kind(sess: &Session, crate_type: ModuleType) -> LinkOutputKind {
    let kind = match (crate_type, sess.crt_static(), sess.relocation_model()) {
        (ModuleType::Executable, false, RelocModel::Pic | RelocModel::Pie) => {
            LinkOutputKind::DynamicPicExe
        }
        (ModuleType::Executable, false, _) => LinkOutputKind::DynamicNoPicExe,
        (ModuleType::Executable, true, RelocModel::Pic | RelocModel::Pie) => {
            LinkOutputKind::StaticPicExe
        }
        (ModuleType::Executable, true, _) => LinkOutputKind::StaticNoPicExe,
        (_, true, _) => LinkOutputKind::StaticDylib,
        (_, false, _) => LinkOutputKind::DynamicDylib,
    };

    // Adjust the output kind to target capabilities.
    let opts = &sess.target;
    let pic_exe_supported = opts.position_independent_executables;
    let static_pic_exe_supported = opts.static_position_independent_executables;
    let static_dylib_supported = opts.crt_static_allows_dylibs;
    match kind {
        LinkOutputKind::DynamicPicExe if !pic_exe_supported => LinkOutputKind::DynamicNoPicExe,
        LinkOutputKind::StaticPicExe if !static_pic_exe_supported => LinkOutputKind::StaticNoPicExe,
        LinkOutputKind::StaticDylib if !static_dylib_supported => LinkOutputKind::DynamicDylib,
        _ => kind,
    }
}

#[allow(clippy::too_many_arguments)]
fn add_order_independent_options(
    cmd: &mut dyn Linker,
    _sess: &Session,
    link_output_kind: LinkOutputKind,
    _flavor: LinkerFlavor,
    module_type: ModuleType,
    _codegen_results: &CodegenResults,
    out_filename: &Path,
    _tmpdir: &Path,
) {
    cmd.output_filename(out_filename);

    if module_type != ModuleType::Executable {
        cmd.gc_sections(module_type == ModuleType::Dylib);
    } else {
        cmd.no_gc_sections();
    }

    cmd.set_output_kind(link_output_kind, out_filename);

    cmd.optimize();
}

fn add_pre_link_args(cmd: &mut dyn Linker, sess: &Session, flavor: LinkerFlavor) {
    if let Some(args) = sess.target.pre_link_args.get(&flavor) {
        cmd.args(args.iter().map(Deref::deref));
    }
}

fn add_late_link_args(
    cmd: &mut dyn Linker,
    sess: &Session,
    flavor: LinkerFlavor,
    _module_type: ModuleType,
    _codegen_results: &CodegenResults,
) {
    if let Some(args) = sess.target.late_link_args.get(&flavor) {
        cmd.args(args.iter().map(Deref::deref));
    }
}

fn linker_with_args(
    path: &Path,
    flavor: LinkerFlavor,
    sess: &Session,
    module_type: ModuleType,
    tmpdir: &Path,
    out_filename: &Path,
    codegen_results: &CodegenResults,
) -> Command {
    let cmd = &mut *linker::get_linker(sess, path, flavor, false, &codegen_results.info.target_cpu);
    let link_output_kind = link_output_kind(sess, module_type);

    add_pre_link_args(cmd, sess, flavor);

    for obj in codegen_results.modules.iter().filter_map(|m| m.object.as_ref()) {
        cmd.add_object(obj);
    }

    add_late_link_args(cmd, sess, flavor, module_type, codegen_results);

    add_order_independent_options(
        cmd,
        sess,
        link_output_kind,
        flavor,
        module_type,
        codegen_results,
        out_filename,
        tmpdir,
    );

    mem::replace(cmd.cmd(), Command::new(""))
}

fn exec_linker(
    _sess: &Session,
    cmd: &Command,
    out_filename: &Path,
    _tmpdir: &Path,
) -> io::Result<Output> {
    #[cfg(not(windows))]
    fn flush_linked_file(_: &io::Result<Output>, _: &Path) -> io::Result<()> {
        Ok(())
    }

    #[cfg(windows)]
    fn flush_linked_file(output: &io::Result<Output>, out_filename: &Path) -> io::Result<()> {
        if let Ok(out) = output {
            if out.status.success() {
                if let Ok(of) = fs::OpenOptions::new().write(true).open(out_filename) {
                    of.sync_all()?;
                }
            }
        }

        Ok(())
    }

    let child = cmd.command().stdout(Stdio::piped()).stderr(Stdio::piped()).spawn()?;
    let output = child.wait_with_output();
    flush_linked_file(&output, out_filename)?;
    output
}

#[cfg(not(windows))]
fn escape_linker_output(s: &[u8], _flavour: LinkerFlavor) -> String {
    escape_string(s)
}

fn escape_string(s: &[u8]) -> String {
    match std::str::from_utf8(s) {
        Ok(s) => s.to_owned(),
        Err(_) => format!("Non-UTF-8 output: {}", s.escape_ascii()),
    }
}

#[cfg(windows)]
#[rustfmt::skip]
mod win {
    use windows::Win32::Globalization::{
        GetLocaleInfoEx, MultiByteToWideChar, CP_OEMCP, LOCALE_IUSEUTF8LEGACYOEMCP,
        LOCALE_NAME_SYSTEM_DEFAULT, LOCALE_RETURN_NUMBER, MB_ERR_INVALID_CHARS,
    };

    /// Get the Windows system OEM code page. This is most notably the code page
    /// used for link.exe's output.
    pub fn oem_code_page() -> u32 {
        unsafe {
            let mut cp: u32 = 0;
            // We're using the `LOCALE_RETURN_NUMBER` flag to return a u32.
            // But the API requires us to pass the data as though it's a [u16] string.
            let len = std::mem::size_of::<u32>() / std::mem::size_of::<u16>();
            let data = std::slice::from_raw_parts_mut(&mut cp as *mut u32 as *mut u16, len);
            let len_written = GetLocaleInfoEx(
                LOCALE_NAME_SYSTEM_DEFAULT,
                LOCALE_IUSEUTF8LEGACYOEMCP | LOCALE_RETURN_NUMBER,
                Some(data),
            );
            if len_written as usize == len { cp } else { CP_OEMCP }
        }
    }
    /// Try to convert a multi-byte string to a UTF-8 string using the given code page
    /// The string does not need to be null terminated.
    ///
    /// This is implemented as a wrapper around `MultiByteToWideChar`.
    /// See <https://learn.microsoft.com/en-us/windows/win32/api/stringapiset/nf-stringapiset-multibytetowidechar>
    ///
    /// It will fail if the multi-byte string is longer than `i32::MAX` or if it contains
    /// any invalid bytes for the expected encoding.
    pub fn locale_byte_str_to_string(s: &[u8], code_page: u32) -> Option<String> {
        // `MultiByteToWideChar` requires a length to be a "positive integer".
        if s.len() > isize::MAX as usize {
            return None;
        }
        // Error if the string is not valid for the expected code page.
        let flags = MB_ERR_INVALID_CHARS;
        // Call MultiByteToWideChar twice.
        // First to calculate the length then to convert the string.
        let mut len = unsafe { MultiByteToWideChar(code_page, flags, s, None) };
        if len > 0 {
            let mut utf16 = vec![0; len as usize];
            len = unsafe { MultiByteToWideChar(code_page, flags, s, Some(&mut utf16)) };
            if len > 0 {
                return utf16.get(..len as usize).map(String::from_utf16_lossy);
            }
        }
        None
    }
}

/// If the output of the msvc linker is not UTF-8 and the host is Windows,
/// then try to convert the string from the OEM encoding.
#[cfg(windows)]
fn escape_linker_output(s: &[u8], flavour: LinkerFlavor) -> String {
    // This only applies to the actual MSVC linker.
    if flavour != LinkerFlavor::Msvc(Lld::No) {
        return escape_string(s);
    }
    match std::str::from_utf8(s) {
        Ok(s) => s.to_owned(),
        Err(_) => match win::locale_byte_str_to_string(s, win::oem_code_page()) {
            Some(s) => s,
            // The string is not UTF-8 and isn't valid for the OEM code page
            None => format!("Non-UTF-8 output: {}", s.escape_ascii()),
        },
    }
}

pub struct LinkingFailed<'a> {
    pub linker_path: &'a Path,
    pub exit_status: ExitStatus,
    pub command: &'a Command,
    pub escaped_output: String,
}

impl IntoDiagnostic<'_> for LinkingFailed<'_> {
    fn into_diagnostic(self, handler: &Handler) -> DiagnosticBuilder<'_, ErrorGuaranteed> {
        let mut db = handler.struct_err(format!(
            "linking with `{}` failed: {}",
            self.linker_path.display(),
            self.exit_status,
        ));

        db.note(format!("{:?}", self.command)).note(self.escaped_output);

        db
    }
}

fn link_natively(
    sess: &Session,
    module_type: ModuleType,
    out_filename: &Path,
    codegen_results: &CodegenResults,
    tmpdir: &Path,
) {
    let (linker_path, flavor) = linker_and_flavor(sess);
    let mut cmd = linker_with_args(
        &linker_path,
        flavor,
        sess,
        module_type,
        tmpdir,
        out_filename,
        codegen_results,
    );

    linker::disable_localization(&mut cmd);

    match exec_linker(sess, &cmd, out_filename, tmpdir) {
        Ok(link) => {
            if !link.status.success() {
                let mut output = link.stderr.clone();
                output.extend_from_slice(&link.stdout);
                let escaped_output = escape_linker_output(&output, flavor);

                sess.emit_err(LinkingFailed {
                    linker_path: &linker_path,
                    exit_status: link.status,
                    command: &cmd,
                    escaped_output,
                });

                if let Some(code) = link.status.code() {
                    if sess.target.is_like_msvc
                        && flavor == LinkerFlavor::Msvc(Lld::No)
                        // && sess.opts.C.linker.is_none()
                        && linker_path.to_str() == Some("link.exe")
                        && (code < 1000 || code > 9999)
                    {
                        let is_vs_installed = windows_registry::find_vs_version().is_ok();
                        let has_linker =
                            windows_registry::find_tool(&sess.opts.triple, "link.exe").is_some();

                        sess.emit_note(errors::LinkExeUnexpectedError);
                        if is_vs_installed && has_linker {
                            // the linker is broken
                            sess.emit_note(errors::RepairVSBuildTools);
                            sess.emit_note(errors::MissingCppBuildToolComponent);
                        } else if is_vs_installed {
                            // the linker is not installed
                            sess.emit_note(errors::SelectCppBuildToolWorkload);
                        } else {
                            // visual studio is not installed
                            sess.emit_note(errors::VisualStudioNotInstalled);
                        }
                    }
                }
            }
        }
        Err(e) => {
            if e.kind() == io::ErrorKind::NotFound {
                sess.emit_err(errors::LinkerNotFound { linker_path, error: e });

                if sess.target.is_like_msvc {
                    sess.emit_note(errors::MsvcMissingLinker);
                    sess.emit_note(errors::CheckInstalledVisualStudio);
                    sess.emit_note(errors::InsufficientVSCodeProduct);
                }
            } else {
                sess.emit_err(errors::UnableToExeLinker {
                    linker_path,
                    error: e,
                    command_formatted: format!("{cmd:?}"),
                });
            }
        }
    }

    sess.abort_if_errors();
}

pub fn link_binary(sess: &Session, codegen_results: &CodegenResults, outputs: &OutputFilenames) {
    for &module_type in &codegen_results.info.module_types {
        let tmpdir = tempfile::Builder::new()
            .prefix("zxc")
            .tempdir()
            .unwrap_or_else(|error| sess.emit_fatal(errors::CreateTempDir { error }));
        let path = compiler::MaybeTempDir::new(tmpdir, false);
        let output =
            sess::out_filename(sess, module_type, outputs, codegen_results.info.local_module_name);
        let module_name = codegen_results.info.local_module_name.as_str();
        let out_filename = output.file_for_writing(outputs, OutputType::Exe, Some(module_name));

        link_natively(sess, module_type, &out_filename, codegen_results, path.as_ref());
    }
}
