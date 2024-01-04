use {
    crate::{
        hir::{self, Ty},
        mir,
        pretty::DisplayCtx,
        symbol::{Ident, Symbol},
        util,
    },
    ariadne::{Color, Fmt, Label},
    lexer::Span,
    std::ops::Range,
};

pub struct ReportSettings {
    pub err_kw: Color,
    pub err: Color,
    pub kw: Color,
}

pub type Result<'tcx, T> = std::result::Result<T, Error<'tcx>>;

pub enum Error<'tcx> {
    NotFoundLocal(Ident),
    TypeMismatch { expected: Ty<'tcx>, found: Ty<'tcx> },
    TypeMismatchOnePlace { expected: Ty<'tcx>, found: mir::Ty<'tcx> },
    ConcreteType { expected: Vec<Ty<'tcx>>, found: Ty<'tcx> },
    NonPrimitiveCast { from: Ty<'tcx>, cast: mir::Ty<'tcx> },
    DefinedMultiple { name: Symbol, definition: Span },
    WrongMainSig { sig: mir::FnSig<'tcx>, span: Span },
    HasNoMain(Span),
}

type Spanned<'a> = (&'a str, Range<usize>);

impl<'a> Error<'a> {
    pub fn report(
        &self,
        hx: &hir::HirCtx<'a>,
        src_loc: &'a str,
        colors: ReportSettings,
    ) -> (&str, String, Vec<Label<Spanned<'a>>>) {
        let fmt = |hx, ty: mir::Ty<'a>| format!("`{}`", ty.to_string(hx)).fg(colors.err_kw);

        use ariadne::Fmt;

        match self {
            Error::TypeMismatch { expected, found } => (
                "E0228", // sample code
                "mismatch types".into(),
                vec![
                    Label::new((src_loc, expected.span.into_range()))
                        .with_message(format!("expected {} ", fmt(hx, expected.kind))),
                    Label::new((src_loc, found.span.into_range()))
                        .with_message(format!("found {} ", fmt(hx, found.kind))),
                ],
            ),
            Error::TypeMismatchOnePlace { expected, found } => (
                "E0228", // sample code
                "mismatch types".into(),
                vec![
                    Label::new((src_loc, expected.span.into_range()))
                        .with_message(format!("expected {} ", fmt(hx, expected.kind))),
                    Label::new((src_loc, expected.span.into_range()))
                        .with_message(format!("found {} ", fmt(hx, *found))),
                ],
            ),
            Error::ConcreteType { expected, found } => (
                "E1337",
                "mismatch types".into(),
                vec![
                    Label::new((src_loc, found.span.into_range())).with_message(format!(
                        "expected {} ",
                        match &expected[..] {
                            [expected] => format!("{}", fmt(hx, expected.kind)),
                            [a, b] =>
                                format!("expected {} or {}", fmt(hx, a.kind), fmt(hx, b.kind)),
                            types => format!(
                                "expected one of: {}",
                                util::join_fmt(types, |ty| fmt(hx, ty.kind))
                            ),
                        }
                    )),
                    Label::new((src_loc, found.span.into_range()))
                        .with_message(format!("found {} ", fmt(hx, found.kind))),
                ],
            ),
            Error::NotFoundLocal(local) => (
                "E1234",
                "cannot find local".into(),
                vec![
                    Label::new((src_loc, local.span.into_range()))
                        .with_message(format!("cannot find value `{local}` in this scope ")),
                ],
            ),
            Error::NonPrimitiveCast { from, cast } => (
                "E0xFF",
                "non primitive cast".into(),
                vec![Label::new((src_loc, from.span.into_range())).with_message(format!(
                    "{} as {}",
                    fmt(hx, from.kind),
                    fmt(hx, *cast)
                ))],
            ),
            Error::DefinedMultiple { name, definition } => (
                "ELMAO",
                format!("the name: `{name}` is defined multiple times"),
                vec![Label::new((src_loc, definition.into_range()))],
            ),
            #[rustfmt::skip]
            Error::WrongMainSig { sig, span } => (
                "#ERROR_PLACEHOLDER",
                "`main` function has wrong type".into(),
                vec![
                    Label::new((src_loc, span.into_range()))
                        .with_message(format!(
                            "expected signature {}", "`fn()`".fg(colors.kw)
                        )),
                    Label::new((src_loc, span.into_range())).with_message(format!(
                        "   found signature {}",
                        format!("`{sig:?}`").fg(colors.kw)
                    )),
                ],
            ),
            Error::HasNoMain(span) => (
                "#ERROR_PLACEHOLDER",
                "`main` function not found in ...".into(),
                vec![Label::new((src_loc, span.into_range()))],
            ),
        }
    }
}
