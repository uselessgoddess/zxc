use {
    crate::{
        codegen::{
            abi::{Abi, Align, FieldsShape, Integer, Layout, LayoutKind, Scalar, Size, TyAbi},
            ctx,
            list::List,
            CPlace, DroplessArena, Expr, IntTy, InternSet, Ty, TyKind, TypedArena,
        },
        Span,
    },
    index_vec::IndexVec,
    std::{fmt, hash::Hash, ops::Range},
};

mod private {
    #[derive(Clone, Copy, Debug)]
    pub struct PrivateZst;
}

pub struct CommonTypes<'tcx> {
    pub unit: Ty<'tcx>,
    pub i8: Ty<'tcx>,
    pub i16: Ty<'tcx>,
    pub i32: Ty<'tcx>,
    pub i64: Ty<'tcx>,
}

impl<'tcx> CommonTypes<'tcx> {
    pub fn new(intern: &ctx::Intern<'tcx>, _: &Session) -> CommonTypes<'tcx> {
        use TyKind::*;

        // Safety: compiler intrinsics
        let mk = |ty| unsafe { intern.intern_ty(ty) };

        Self {
            unit: mk(Tuple(List::empty())),
            i8: mk(Int(IntTy::I8)),
            i16: mk(Int(IntTy::I16)),
            i32: mk(Int(IntTy::I32)),
            i64: mk(Int(IntTy::I64)),
        }
    }
}

index_vec::define_index_type! {
    pub struct Local = u32;
}

pub struct Arena<'tcx> {
    pub dropless: DroplessArena,
    pub expr: TypedArena<Expr<'tcx>>,
    pub stmt: TypedArena<Stmt<'tcx>>,
}

pub struct Intern<'tcx> {
    pub types: InternSet<TyKind<'tcx>>,
    pub layouts: InternSet<LayoutKind>,
}

impl<'tcx> Intern<'tcx> {
    pub unsafe fn intern_ty(&self, kind: TyKind<'tcx>) -> Ty<'tcx> {
        self.types.intern_outlive(kind)
    }

    pub unsafe fn intern_layout(&self, kind: LayoutKind) -> Layout<'tcx> {
        self.layouts.intern_outlive(kind)
    }
}

pub struct TyCtx<'tcx> {
    pub arena: Arena<'tcx>,
    pub intern: Intern<'tcx>,

    pub types: CommonTypes<'tcx>,
    pub locals: IndexVec<Local, CPlace<'tcx>>,
}

pub struct Session {}

impl<'tcx> TyCtx<'tcx> {
    pub fn enter(sess: Session) -> Self {
        let intern = Intern { types: InternSet::new(), layouts: InternSet::new() };
        let types = CommonTypes::new(&intern, &sess);

        Self {
            arena: Arena {
                dropless: DroplessArena::default(),
                expr: TypedArena::default(),
                stmt: TypedArena::default(),
            },
            intern,
            types,
            locals: IndexVec::with_capacity(128),
        }
    }

    pub fn layout_of(&self, ty: Ty<'tcx>) -> TyAbi<'tcx> {
        // Safety: compiler intrinsics
        let scalar = |ty, sign, (size, align)| LayoutKind {
            abi: Abi::Scalar(Scalar::Int(ty, true)),
            size: Size::from_bytes(size),
            align: Align::from_bytes(align).expect("compiler query"),
            shape: FieldsShape::Primitive,
        };

        // Safety: compiler intrinsics
        let layout = unsafe {
            self.intern.intern_layout(match ty.kind() {
                TyKind::Int(int) => match int {
                    IntTy::I8 => scalar(Integer::I8, true, (1, 1)),
                    IntTy::I16 => scalar(Integer::I16, true, (2, 2)),
                    IntTy::I32 => scalar(Integer::I32, true, (4, 4)),
                    IntTy::I64 => scalar(Integer::I64, true, (8, 8)),
                },
                TyKind::Tuple(list) => {
                    if list.is_empty() {
                        LayoutKind {
                            abi: Abi::Aggregate,
                            size: Size::ZERO,
                            align: Align::from_bytes(1).expect("compiler query"),
                            shape: FieldsShape::Primitive,
                        }
                    } else {
                        todo!()
                    }
                }
            })
        };
        TyAbi { ty, layout }
    }

    pub fn fatal(&self, msg: impl Into<String>) -> ! {
        // TODO: Add diagnostic handler
        panic!("{}", msg.into())
    }
}

use {
    crate::codegen::Stmt,
    ariadne::{Color, Fmt, Label},
};

pub struct ReportSettings {
    pub err_kw: Color,
    pub err: Color,
    pub kw: Color,
}

pub type Result<'tcx, T> = std::result::Result<T, Error<'tcx>>;

pub enum Error<'tcx> {
    TypeMismatch { expected: (Ty<'tcx>, Span), found: (Ty<'tcx>, Span) },
    ConcreateType { expected: Vec<Ty<'tcx>>, found: (Ty<'tcx>, Span) },
}

type Spanned<'a> = (&'a str, Range<usize>);

fn s(str: &str, span: Span) -> Spanned {
    (str, span.into_range())
}

impl Error<'_> {
    pub fn report<'a>(
        &self,
        src: &'a str,
        colors: ReportSettings,
    ) -> (&str, String, Vec<Label<Spanned<'a>>>) {
        let t = |ty| format!("{ty}").fg(colors.err_kw);

        use ariadne::Fmt;

        match self {
            Error::TypeMismatch { expected, found } => (
                "E0228", // sample code
                "mismatch types".into(),
                vec![
                    Label::new(s(src, expected.1))
                        .with_message(format!("expected `{}` ", t(expected.0)))
                        .with_color(colors.err),
                    Label::new(s(src, found.1))
                        .with_message(format!("found `{}` ", t(found.0)))
                        .with_color(colors.err),
                ],
            ),
            Error::ConcreateType { expected, found: (ty, span) } => (
                "E1337",
                "mismatch types".into(),
                vec![
                    Label::new(s(src, *span))
                        .with_message(format!(
                            "expected {} ",
                            match &expected[..] {
                                [expected] => format!("{}", t(*ty)),
                                &[a, b] => format!("expected {} or {}", t(a), t(b)),
                                types =>
                                    format!("expected one of: {}", join_fmt(types, |&ty| t(ty))),
                            }
                        ))
                        .with_color(colors.err),
                ],
            ),
        }
    }
}

fn join_fmt<T, U: fmt::Display>(slice: &[T], mut map: impl FnMut(&T) -> U) -> String {
    let mut place = String::with_capacity(128);

    for fmt in slice {
        place.push_str(&format!("{}", map(fmt)));
    }
    place
}

pub type Tx<'tcx> = &'tcx TyCtx<'tcx>;
