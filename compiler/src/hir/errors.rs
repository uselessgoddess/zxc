use crate::{
    diagnostic,
    sess::{DiagnosticBuilder, Emission, Handler, IntoDiagnostic},
};

diagnostic! {
    ["multiple `start` functions: TODO"]
    pub struct MultipleStartFunctions;
}

diagnostic! {
    [" `main` function not found in file"]
    pub struct NoMainErr;
}
