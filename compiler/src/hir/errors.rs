use {
    crate::{
        diagnostic,
        errors::Level::*,
        sess::{DiagnosticBuilder, Emission, Handler, IntoDiagnostic},
    },
    lexer::Span,
};

diagnostic! {
    ["multiple `#[start]` functions"]
    [code: E0001]
    [primary(Error, label): "multiple `start` functions"]
    [primary(Help, previous): "previous `#[start]` function here"]
    pub struct MultipleStartFunctions {
        pub label: Span,
        pub previous: Span,
    }
}

diagnostic! {
    [" `main` function not found in file"]
    pub struct NoMainErr;
}
