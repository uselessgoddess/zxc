use {
    crate::{
        diagnostic,
        errors::Level::*,
        sess::{DiagnosticBuilder, Emission, Handler, IntoDiagnostic},
        symbol::{Ident, Symbol},
    },
    errors::{Code, DiagnosticMessage, ErrorGuaranteed},
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
    ["multiple `main` functions"]
    [code: E0011]
    [primary(Error, span): "multiple `main` functions"]
    pub struct MultipleMainFunctions {
        pub span: Span,
    }
}

diagnostic! {
    [" `main` function not found in file"]
    pub struct NoMainErr;
}

pub struct TypeMismatch {
    pub expect: (DiagnosticMessage, Span),
    pub found: (DiagnosticMessage, Span),
}

impl<'a> IntoDiagnostic<'a> for TypeMismatch {
    fn into_diagnostic(self, handler: &'a Handler) -> DiagnosticBuilder<'a, ErrorGuaranteed> {
        let TypeMismatch { expect: (exp_msg, expect_span), found: (found_msg, found_span) } = self;
        let mut db = handler.struct_err("type mismatch");

        db.code(Code::E0002)
            .primary(
                Error { lint: false },
                found_span,
                Some(format!("expected {}, found {}", exp_msg, found_msg)),
            )
            .primary(Note, expect_span, Some("expected due to this"));

        db
    }
}

pub struct ExpectType {
    pub expect: (DiagnosticMessage, Span),
}

impl<'a> IntoDiagnostic<'a> for ExpectType {
    fn into_diagnostic(self, handler: &'a Handler) -> DiagnosticBuilder<'a, ErrorGuaranteed> {
        let ExpectType { expect: (expect, span) } = self;
        let mut db = handler.struct_err("type mismatch");

        db.code(Code::E0002).primary(
            Error { lint: false },
            span,
            Some(format!("expected {expect}")),
        );

        db
    }
}

diagnostic! {
    ["can't find local"]
    [code: E0003]
    [primary(Error, span): "cannot find value `{}` in this scope", local]
    pub struct NotFoundLocal {
        pub local: Symbol,
        pub span: Span,
    }
}

impl NotFoundLocal {
    pub fn ident(ident: Ident) -> Self {
        Self { local: ident.name, span: ident.span }
    }
}

diagnostic! {
    ["DUMMY ERROR!"]
    pub struct Todo;
}

diagnostic! {
    ["non primitive cast"]
    [code: E0004]
    [primary(Error, span): "{} as {}", from, cast]
    pub struct NonPrimitiveCast {
        pub span: Span,
        pub from: DiagnosticMessage,
        pub cast: DiagnosticMessage,
    }
}

diagnostic! {
    ["invalid cast"]
    [code: E0004]
    [primary(Error, span): "casting {} as {} is invalid", from, cast]
    pub struct InvalidCast {
        pub span: Span,
        pub from: DiagnosticMessage,
        pub cast: DiagnosticMessage,
    }
}

pub struct MismatchFnSig {
    pub expect: DiagnosticMessage,
    pub found: DiagnosticMessage,
    pub caller: Span,
    pub target: Option<Span>,
}

impl<'a> IntoDiagnostic<'a> for MismatchFnSig {
    fn into_diagnostic(self, handler: &'a Handler) -> DiagnosticBuilder<'a, ErrorGuaranteed> {
        let MismatchFnSig { expect, found, caller, target } = self;
        let mut db = handler.struct_err("called function has another signature");

        let location = target.unwrap_or(caller);
        db.code(Code::E0005);
        db.primary(Error { lint: false }, location, Some(format!("expected {expect}")));
        db.primary(Error { lint: false }, caller, Some(format!("found: {found}")));

        db
    }
}

diagnostic! {
    ["`#[start]` function has wrong type"]
    [code: E0005]
    [primary(Error, span): "expected signature {}", expect]
    [primary(Error, span): "   found signature {}", found]
    pub struct MismatchStartSig {
        pub expect: DiagnosticMessage,
        pub found: DiagnosticMessage,
        pub span: Span,
    }
}

diagnostic! {
    ["`main` function has wrong type"]
    [code: E0005]
    [primary(Error, span): "expected signature {}", expect]
    [primary(Error, span): "   found signature {}", found]
    pub struct MismatchMainSig {
        pub expect: DiagnosticMessage,
        pub found: DiagnosticMessage,
        pub span: Span,
    }
}

pub struct InvalidLvalue {
    pub span: Span,
}

impl<'a> IntoDiagnostic<'a> for InvalidLvalue {
    fn into_diagnostic(self, handler: &'a Handler) -> DiagnosticBuilder<'a, ErrorGuaranteed> {
        let mut db = handler.struct_err("temporary or immutable lvalue");
        db.code(Code::E0006).primary(Error { lint: false }, self.span, none());
        db
    }
}

pub struct ConstArithmetic {
    pub case: &'static str,
    pub note: Option<String>,
    pub span: Span,
}

impl<'a> IntoDiagnostic<'a> for ConstArithmetic {
    fn into_diagnostic(self, handler: &'a Handler) -> DiagnosticBuilder<'a, ErrorGuaranteed> {
        let ConstArithmetic { case, note, span } = self;
        let mut db = handler.struct_err("const evaluation error");

        db.code(Code::E0007).primary(Error { lint: false }, span, Some(case));
        if let Some(note) = note {
            db.note(note);
        }
        db
    }
}

pub struct DefinedMultiple {
    pub name: DiagnosticMessage,
    pub def: Span,
    pub redef: Span,
}

impl<'a> IntoDiagnostic<'a> for DefinedMultiple {
    fn into_diagnostic(self, handler: &'a Handler) -> DiagnosticBuilder<'a, ErrorGuaranteed> {
        let DefinedMultiple { name, def, redef } = self;
        let mut db = handler.struct_err(format!("the name: {name} is defined multiple times"));

        db.code(Code::E0008).primary(Error { lint: false }, def, none());
        db.primary(Error { lint: false }, redef, Some(format!("{name} redefined here")));
        db
    }
}

diagnostic! {
    ["cannot borrow `{}` as mutable, as it is not declared as mutable", local]
    [code: E0009]
    [primary(Error, borrow): "cannot borrow as mutable"]
    pub struct MutBorrowImmut {
        pub local: Symbol,
        pub borrow: Span,
    }
}

pub struct InvalidDeref {
    pub ty: (DiagnosticMessage, Span),
}

impl<'a> IntoDiagnostic<'a> for InvalidDeref {
    fn into_diagnostic(self, handler: &'a Handler) -> DiagnosticBuilder<'a, ErrorGuaranteed> {
        let (ty, span) = self.ty;
        let mut db = handler.struct_err(format!("type {ty} cannot be dereferenced"));
        db.code(Code::E0010).primary(Error { lint: false }, span, none());
        db
    }
}

diagnostic! {
    ["can't infer type on ambiguous numeric type `{{integer}}`"]
    [code: E0012]
    [primary(Note, span): "you must specify a concrete type for this numeric value, like `i32`"]
    pub struct InferInt {
        pub span: Span,
    }
}

fn none() -> Option<String> {
    None
}
