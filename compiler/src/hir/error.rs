use {
    crate::{hir::Ty, mir, util},
    ariadne::{Color, Label},
    lexer::Ident,
    std::ops::Range,
};

pub struct ReportSettings {
    pub err_kw: Color,
    pub err: Color,
    pub kw: Color,
}

pub type Result<'tcx, T> = std::result::Result<T, Error<'tcx>>;

pub enum Error<'tcx> {
    NotFoundLocal(Ident<'tcx>),
    TypeMismatch { expected: Ty<'tcx>, found: Ty<'tcx> },
    TypeMismatchOnePlace { expected: Ty<'tcx>, found: mir::Ty<'tcx> },
    ConcreateType { expected: Vec<Ty<'tcx>>, found: Ty<'tcx> },
    NonPrimitiveCast { from: Ty<'tcx>, cast: mir::Ty<'tcx> },
}

type Spanned<'a> = (&'a str, Range<usize>);

impl<'a> Error<'a> {
    pub fn report(
        &self,
        src: &'a str,
        colors: ReportSettings,
    ) -> (&str, String, Vec<Label<Spanned<'a>>>) {
        let fmt = |ty: mir::Ty| format!("`{}`", ty).fg(colors.err_kw);

        use ariadne::Fmt;

        match self {
            Error::TypeMismatch { expected, found } => (
                "E0228", // sample code
                "mismatch types".into(),
                vec![
                    Label::new((src, expected.span.into_range()))
                        .with_message(format!("expected {} ", fmt(expected.kind))),
                    Label::new((src, found.span.into_range()))
                        .with_message(format!("found {} ", fmt(found.kind))),
                ],
            ),
            Error::TypeMismatchOnePlace { expected, found } => (
                "E0228", // sample code
                "mismatch types".into(),
                vec![
                    Label::new((src, expected.span.into_range()))
                        .with_message(format!("expected {} ", fmt(expected.kind))),
                    Label::new((src, expected.span.into_range()))
                        .with_message(format!("found {} ", fmt(*found))),
                ],
            ),
            Error::ConcreateType { expected, found } => (
                "E1337",
                "mismatch types".into(),
                vec![
                    Label::new((src, found.span.into_range())).with_message(format!(
                        "expected {} ",
                        match &expected[..] {
                            [expected] => format!("{}", fmt(expected.kind)),
                            [a, b] => format!("expected {} or {}", fmt(a.kind), fmt(b.kind)),
                            types => format!(
                                "expected one of: {}",
                                util::join_fmt(types, |ty| fmt(ty.kind))
                            ),
                        }
                    )),
                    Label::new((src, found.span.into_range()))
                        .with_message(format!("found {} ", fmt(found.kind))),
                ],
            ),
            Error::NotFoundLocal(local) => {
                (
                    "E1234",
                    "cannot find local".into(),
                    vec![Label::new((src, local.span.into_range())).with_message(format!(
                        "cannot find value `{}` in this scope ",
                        local.ident()
                    ))],
                )
            }
            Error::NonPrimitiveCast { from, cast } => (
                "E0xFF",
                "non primitive cast".into(),
                vec![Label::new((src, from.span.into_range())).with_message(format!(
                    "{} as {}",
                    fmt(from.kind),
                    fmt(*cast)
                ))],
            ),
        }
    }
}
