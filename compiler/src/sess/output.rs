use {
    super::errors,
    crate::{sess::ModuleType, symbol::Symbol, Session},
    std::{
        collections::BTreeMap,
        path::{Path, PathBuf},
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum OutputType {
    Bitcode,
    Mir,
    Object,
    Exe,
}

impl OutputType {
    pub fn extension(&self) -> &'static str {
        match *self {
            OutputType::Bitcode => "bc",
            OutputType::Mir => "mir",
            OutputType::Object => "o",
            OutputType::Exe => "",
        }
    }
}

#[derive(Debug, Clone)]
pub enum OutFileName {
    Real(PathBuf),
    Stdout,
}

impl OutFileName {
    pub fn file_for_writing(
        &self,
        outputs: &OutputFilenames,
        flavor: OutputType,
        codegen_unit_name: Option<&str>,
    ) -> PathBuf {
        match *self {
            OutFileName::Real(ref path) => path.clone(),
            OutFileName::Stdout => outputs.temp_path(flavor, codegen_unit_name),
        }
    }
}

pub struct OutputFilenames {
    pub out_directory: PathBuf,
    pub file_stem: String,
    pub single_output_file: Option<OutFileName>,
    pub temps_directory: Option<PathBuf>,
    pub outputs: BTreeMap<OutputType, OutFileName>,
}

impl OutputFilenames {
    pub fn path(&self, flavor: OutputType) -> OutFileName {
        self.outputs
            .get(&flavor)
            .map(|p| p.to_owned())
            .or_else(|| self.single_output_file.clone())
            .unwrap_or_else(|| OutFileName::Real(self.output_path(flavor)))
    }

    pub fn output_path(&self, flavor: OutputType) -> PathBuf {
        self.with_directory_and_extension(&self.out_directory, flavor.extension())
    }

    fn with_directory_and_extension(&self, directory: &Path, extension: &str) -> PathBuf {
        let mut path = directory.join(&self.file_stem);
        path.set_extension(extension);
        path
    }

    pub fn temp_path(&self, flavor: OutputType, codegen_unit_name: Option<&str>) -> PathBuf {
        let extension = flavor.extension();
        self.temp_path_ext(extension, codegen_unit_name)
    }

    pub fn temp_path_ext(&self, ext: &str, codegen_unit_name: Option<&str>) -> PathBuf {
        let mut extension = String::new();

        if let Some(codegen_unit_name) = codegen_unit_name {
            extension.push_str(codegen_unit_name);
        }

        if !ext.is_empty() {
            if !extension.is_empty() {
                extension.push('.');
                extension.push_str("zcgu");
                extension.push('.');
            }

            extension.push_str(ext);
        }

        let temps_directory = self.temps_directory.as_ref().unwrap_or(&self.out_directory);

        self.with_directory_and_extension(temps_directory, &extension)
    }
}

pub fn filename_for_input(
    sess: &Session,
    module_type: ModuleType,
    module_name: Symbol,
    outputs: &OutputFilenames,
) -> OutFileName {
    let libname = module_name;

    match module_type {
        ModuleType::Dylib => {
            let (prefix, suffix) = (&sess.target.dll_prefix, &sess.target.dll_suffix);
            OutFileName::Real(outputs.out_directory.join(format!("{prefix}{libname}{suffix}")))
        }
        ModuleType::Staticlib => {
            let (prefix, suffix) = (&sess.target.staticlib_prefix, &sess.target.staticlib_suffix);
            OutFileName::Real(outputs.out_directory.join(format!("{prefix}{libname}{suffix}")))
        }
        ModuleType::Executable => {
            let suffix = &sess.target.exe_suffix;
            let out_filename = outputs.path(OutputType::Exe);
            if let OutFileName::Real(ref path) = out_filename {
                if suffix.is_empty() {
                    out_filename
                } else {
                    OutFileName::Real(path.with_extension(&suffix[1..]))
                }
            } else {
                out_filename
            }
        }
    }
}

pub fn check_file_is_writeable(file: &Path, sess: &Session) {
    fn is_writeable(p: &Path) -> bool {
        match p.metadata() {
            Err(..) => true,
            Ok(m) => !m.permissions().readonly(),
        }
    }

    if !is_writeable(file) {
        sess.emit_fatal(errors::FileIsNotWriteable { file });
    }
}

pub fn out_filename(
    sess: &Session,
    module_type: ModuleType,
    outputs: &OutputFilenames,
    crate_name: Symbol,
) -> OutFileName {
    let default_filename = filename_for_input(sess, module_type, crate_name, outputs);
    let out_filename = outputs
        .outputs
        .get(&OutputType::Exe)
        .map(|s| s.to_owned())
        .or_else(|| outputs.single_output_file.clone())
        .unwrap_or(default_filename);

    if let OutFileName::Real(ref path) = out_filename {
        check_file_is_writeable(path, sess);
    }

    out_filename
}

pub fn default_output_for_target(sess: &Session) -> ModuleType {
    if !sess.target.executables { ModuleType::Staticlib } else { ModuleType::Executable }
}
