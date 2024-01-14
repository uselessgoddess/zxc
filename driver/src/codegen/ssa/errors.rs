use {
    compiler::{
        diagnostic,
        sess::{DiagnosticBuilder, Emission, Handler, IntoDiagnostic},
    },
    std::{io, path::PathBuf},
};

diagnostic! {
    ["couldn't extract file stem from specified linker"]
    pub struct LinkerFileStem;
}

diagnostic! {
    ["`link.exe` returned an unexpected error"]
    pub struct LinkExeUnexpectedError;
}

diagnostic! {
    ["the Visual Studio build tools may need to be repaired using the Visual Studio installer"]
    pub struct RepairVSBuildTools;
}

diagnostic! {
    ["the Visual Studio build tools may need to be repaired using the Visual Studio installer"]
    pub struct MissingCppBuildToolComponent;
}

diagnostic! {
    ["in the Visual Studio installer, ensure the \"C++ build tools\" workload is selected"]
    pub struct SelectCppBuildToolWorkload;
}

diagnostic! {
    ["you may need to install Visual Studio build tools with the \"C++ build tools\" workload"]
    pub struct VisualStudioNotInstalled;
}

diagnostic! {
    ["the msvc targets depend on the msvc linker but `link.exe` was not found"]
    pub struct MsvcMissingLinker;
}

diagnostic! {
    ["please ensure that Visual Studio 2017 or later, or Build Tools for Visual Studio \
    were installed with the Visual C++ option."]
    pub struct CheckInstalledVisualStudio;
}

diagnostic! {
    ["VS Code is a different product, and is not sufficient."]
    pub struct InsufficientVSCodeProduct;
}

// TODO: fix debug formatter here
diagnostic! {
    ["linker {:?} not found", linker_path]
    [note: "{}", error]
    pub struct LinkerNotFound {
        pub linker_path: PathBuf,
        pub error: io::Error,
    }
}

diagnostic! {
    ["could not exec the linker"]
    [note: "{}", error]
    [note: "{}", command_formatted]
    pub struct UnableToExeLinker {
        pub linker_path: PathBuf,
        pub error: io::Error,
        pub command_formatted: String,
    }
}

diagnostic! {
    ["could not create a temp dir: {}", error]
    pub struct CreateTempDir {
        pub error: io::Error,
    }
}
