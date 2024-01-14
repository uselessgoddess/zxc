use {
    crate::{
        hir::{self, Ty},
        mir,
        pretty::{self, DisplayCtx, Print, Printer},
        symbol::{Ident, Symbol},
        util,
    },
    ariadne::{Color, Fmt, Label},
    lexer::Span,
    std::{fmt, ops::Range},
};

#[derive(Clone)]
pub struct ReportSettings {
    pub err_kw: Color,
    pub err: Color,
    pub kw: Color,
}

pub type Result<'tcx, T> = std::result::Result<T, Error<'tcx>>;

#[derive(Debug, Clone)]
pub enum Error<'tcx> {
    NotFoundLocal(Ident),
    TypeMismatch {
        expected: Ty<'tcx>,
        found: Ty<'tcx>,
    },
    TypeMismatchOnePlace {
        expected: Ty<'tcx>,
        found: mir::Ty<'tcx>,
    },
    ConcreteType {
        expected: Vec<Ty<'tcx>>,
        found: Ty<'tcx>,
    },
    NonPrimitiveCast {
        from: Ty<'tcx>,
        cast: mir::Ty<'tcx>,
    },
    DefinedMultiple {
        name: Symbol,
        def: Span,
        redef: Span,
    },
    WrongMainSig {
        sig: mir::FnSig<'tcx>,
        span: Span,
    },
    WrongFnArgs {
        expect: Vec<mir::Ty<'tcx>>,
        found: Vec<Ty<'tcx>>,
        caller: Span,
        target: Option<Span>,
    },
    ConstArithmetic {
        case: &'static str,
        note: Option<String>,
        span: Span,
    },
    HasNoMain(Span),
    InvalidLvalue(Span),
}

type Spanned<'a> = (&'a str, Range<usize>);

pub const PLACEHOLDER: &str = "#ERROR_PLACEHOLDER";

impl<'a> Error<'a> {
    pub fn report(
        &self,
        hx: &hir::HirCtx<'a>,
        src_loc: &'a str,
        colors: ReportSettings,
    ) -> (&str, String, Span, Vec<Label<Spanned<'a>>>) {
        let fmt = |hx, ty: mir::Ty<'a>| format!("`{}`", ty.to_string(hx)).fg(colors.err_kw);
        let s = |span: &Span| (src_loc, span.into_range());

        use ariadne::Fmt;

        match self {
            Error::TypeMismatch { expected, found } => (
                PLACEHOLDER,
                "mismatch types".into(),
                expected.span,
                vec![
                    Label::new((src_loc, expected.span.into_range()))
                        .with_message(format!("expected {} ", fmt(hx, expected.kind))),
                    Label::new((src_loc, found.span.into_range()))
                        .with_message(format!("found {} ", fmt(hx, found.kind))),
                ],
            ),
            Error::TypeMismatchOnePlace { expected, found } => (
                PLACEHOLDER,
                "mismatch types".into(),
                expected.span,
                vec![
                    Label::new((src_loc, expected.span.into_range()))
                        .with_message(format!("expected {} ", fmt(hx, expected.kind))),
                    Label::new((src_loc, expected.span.into_range()))
                        .with_message(format!("found {} ", fmt(hx, *found))),
                ],
            ),
            Error::ConcreteType { expected, found } => (
                PLACEHOLDER,
                "mismatch types".into(),
                expected.first().map(|ty| ty.span).unwrap_or(found.span),
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
                PLACEHOLDER,
                "cannot find local".into(),
                local.span,
                vec![
                    Label::new((src_loc, local.span.into_range()))
                        .with_message(format!("cannot find value `{local}` in this scope ")),
                ],
            ),
            Error::NonPrimitiveCast { from, cast } => (
                PLACEHOLDER,
                "non primitive cast".into(),
                from.span,
                vec![Label::new((src_loc, from.span.into_range())).with_message(format!(
                    "{} as {}",
                    fmt(hx, from.kind),
                    fmt(hx, *cast)
                ))],
            ),
            Error::DefinedMultiple { name, def, redef } => (
                PLACEHOLDER,
                format!("the name: `{name}` is defined multiple times"),
                *def,
                vec![
                    Label::new((src_loc, def.into_range())),
                    Label::new((src_loc, redef.into_range()))
                        .with_message(format!("`{name}` redefined here")),
                ],
            ),
            #[rustfmt::skip]
            Error::WrongMainSig { sig, span } => (
                PLACEHOLDER,
                "`main` function has wrong type".into(),
                *span,
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
                PLACEHOLDER,
                "`main` function not found in ...".into(),
                *span,
                vec![Label::new((src_loc, span.into_range()))],
            ),
            Error::WrongFnArgs { expect, found, caller, target } => {
                let location = target.unwrap_or(*caller);
                (
                    PLACEHOLDER,
                    "function has another signature".into(),
                    location,
                    vec![
                        Label::new(s(&location)).with_message(format!(
                            "expected: {}",
                            format_sig(hx, expect.iter()).fg(colors.kw)
                        )),
                        Label::new(s(caller)).with_message(format!(
                            "{}found: {}",
                            // tabulation when has no target
                            if target.is_some() { "" } else { "   " },
                            format_sig(hx, found.iter().map(|ty| &**ty)).fg(colors.kw)
                        )),
                    ],
                )
            }
            Error::ConstArithmetic { case, note, span } => {
                let mut label = Label::new(s(span));

                if let Some(note) = note {
                    label = label.with_message(note);
                }
                (PLACEHOLDER, case.to_string(), *span, vec![label])
            }
            Error::InvalidLvalue(span) => (
                PLACEHOLDER,
                "temporary or immutable lvalue".into(),
                *span,
                vec![Label::new(s(span))],
            ),
        }
    }
}

fn format_sig<'a, 'tcx: 'a>(
    hx: &hir::HirCtx<'tcx>,
    args: impl Iterator<Item = &'a mir::Ty<'tcx>>,
) -> String {
    use fmt::Write;
    let mut fmt = pretty::FmtPrinter::new(hx);

    let buf: std::result::Result<_, fmt::Error> = try {
        write!(fmt, "(")?;
        fmt.comma_sep_with(args, |cx, arg| arg.print(cx))?;
        write!(fmt, ")")?;

        fmt.into_buf()
    };
    format!("`{}`", buf.unwrap())
}
