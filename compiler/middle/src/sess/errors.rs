use {
    super::{DiagnosticBuilder, Emission, Handler, IntoDiagnostic},
    crate::diagnostic,
};

diagnostic! {
    ["output file {:?} is not writeable -- check its permissions", file]
    pub struct FileIsNotWriteable<'a> {
        pub file: &'a std::path::Path,
    }
}

diagnostic! {
    ["module name must not be empty"]
    pub struct ModuleNameEmpty;
}

diagnostic! {
    ["invalid character `{}` in crate name: `{}`", character, module_name]
    pub struct InvalidCharacterInModuleName<'a> {
        pub character: char,
        pub module_name: &'a str,
    }
}

diagnostic! {
    ["module names cannot start with a `-`, but `{}` has a leading hyphen", name]
    pub struct ModuleNameInvalid<'a> {
        pub name: &'a str,
    }
}
