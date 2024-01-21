use {
    middle::{
        diagnostic,
        errors::{DiagnosticBuilder, Emission, Handler, IntoDiagnostic},
        ErrorGuaranteed,
    },
    std::{ffi::CString, io, path::Path},
};

pub enum LlvmError<'a> {
    WriteOutput { path: &'a Path },
    CreateTargetMachine { triple: CString },
    RunLlvmPasses,
    SerializeModule { name: &'a str },
    WriteIr { path: &'a Path },
    PrepareThinLtoContext,
    LoadBitcode { name: CString },
    WriteThinLtoKey { err: io::Error },
    MultipleSourceDiCompileUnit,
    PrepareThinLtoModule,
    ParseBitcode,
}

impl LlvmError<'_> {
    fn error_prefix(&self) -> String {
        match self {
            LlvmError::WriteOutput { path } => {
                format!("could not write output to {}", path.display())
            }
            LlvmError::CreateTargetMachine { triple } => {
                format!(
                    "could not create LLVM TargetMachine for triple: {}",
                    triple.to_string_lossy()
                )
            }
            LlvmError::RunLlvmPasses => "failed to run LLVM passes".into(),
            LlvmError::SerializeModule { name } => format!("failed to serialize module {name}"),
            LlvmError::WriteIr { path } => {
                format!("failed to write LLVM IR to {}", path.display())
            }
            LlvmError::PrepareThinLtoContext => "failed to prepare thin LTO context".into(),
            LlvmError::LoadBitcode { name } => {
                format!("failed to load bitcode of module \"{}\"", name.to_string_lossy())
            }
            LlvmError::WriteThinLtoKey { err } => {
                format!("error while writing ThinLTO key data: {err}")
            }
            LlvmError::MultipleSourceDiCompileUnit => "multiple source DICompileUnits found".into(),
            LlvmError::PrepareThinLtoModule => "failed to prepare thin LTO module".into(),
            LlvmError::ParseBitcode => "failed to parse bitcode for LTO module".into(),
        }
    }
}

impl<'a, E: Emission> IntoDiagnostic<'a, E> for LlvmError<'a> {
    fn into_diagnostic(self, handler: &'a Handler) -> DiagnosticBuilder<'a, E> {
        handler.struct_diagnostic(self.error_prefix())
    }
}

pub struct WithLlvmError<'a>(pub LlvmError<'a>, pub String);

impl<'a, E: Emission> IntoDiagnostic<'a, E> for WithLlvmError<'a> {
    fn into_diagnostic(self, handler: &'a Handler) -> DiagnosticBuilder<'a, E> {
        handler.struct_diagnostic(format!("{}: {}", self.0.error_prefix(), self.1))
    }
}

diagnostic! {
    ["could not copy {:?} to {:?}: {}", from, to, error]
    pub struct CopyPath<'a> {
        pub from: &'a Path,
        pub to: &'a Path,
        pub error: io::Error,
    }
}
