use {
    crate::{
        codegen::{
            abi::{Abi, Align, FieldsShape, Integer, Layout, LayoutKind, Scalar, Size, TyAbi},
            list::List,
            CPlace, DroplessArena, Expr, IntTy, Sharded, Ty, TyKind, TypedArena,
        },
        Span,
    },
    index_vec::IndexVec,
    std::{hash::Hash, ops::Range},
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
    pub fn new(intern: &Intern<'tcx>, arena: &'tcx Arena<'tcx>, _: &Session) -> CommonTypes<'tcx> {
        use TyKind::*;

        // Safety: compiler intrinsics
        let mk = |ty| intern.intern_ty(arena, ty);

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

#[derive(Default)]
pub struct Arena<'tcx> {
    pub dropless: DroplessArena,
    pub expr: TypedArena<Expr<'tcx>>,
    pub stmt: TypedArena<Stmt<'tcx>>,
    pub type_: TypedArena<TyKind<'tcx>>,
    pub layout: TypedArena<LayoutKind>,
}

type InternSet<'tcx, T> = Sharded<Interned<'tcx, T>>;

#[derive(Default)]
pub struct Intern<'tcx> {
    pub types: InternSet<'tcx, TyKind<'tcx>>,
    pub layouts: InternSet<'tcx, LayoutKind>,
    pub type_lists: InternSet<'tcx, List<Ty<'tcx>>>,
}

impl<'tcx> Intern<'tcx> {
    pub fn intern_ty(&self, arena: &'tcx Arena<'tcx>, kind: TyKind<'tcx>) -> Ty<'tcx> {
        self.types.intern(kind, |kind| Interned::new_unchecked(arena.type_.alloc(kind)))
    }

    pub fn intern_layout(&self, arena: &'tcx Arena<'tcx>, kind: LayoutKind) -> Layout<'tcx> {
        self.layouts.intern(kind, |kind| Interned::new_unchecked(arena.layout.alloc(kind)))
    }

    pub fn intern_type_list(
        &self,
        arena: &'tcx Arena<'tcx>,
        slice: &[Ty<'tcx>],
    ) -> &'tcx List<Ty<'tcx>> {
        self.type_lists
            .intern_ref(slice, || Interned::new_unchecked(List::from_arena(arena, slice)))
            .0
    }
}

pub struct TyCtx<'tcx> {
    pub arena: &'tcx Arena<'tcx>,
    pub intern: Intern<'tcx>,

    pub types: CommonTypes<'tcx>,
    pub locals: IndexVec<Local, CPlace<'tcx>>,
}

pub struct Session {}

impl<'tcx> TyCtx<'tcx> {
    pub fn enter(arena: &'tcx Arena<'tcx>, sess: Session) -> Self {
        let intern = Intern::default();
        let types = CommonTypes::new(&intern, &arena, &sess);

        Self { arena, intern, types, locals: IndexVec::with_capacity(128) }
    }

    pub fn layout_of(&self, ty: Ty<'tcx>) -> TyAbi<'tcx> {
        // Safety: compiler intrinsics
        let scalar = |ty, sign, (size, align)| LayoutKind {
            abi: Abi::Scalar(Scalar::Int(ty, sign)),
            size: Size::from_bytes(size),
            align: Align::from_bytes(align).expect("compiler query"),
            shape: FieldsShape::Primitive,
        };

        // Safety: compiler intrinsics
        let layout = self.intern.intern_layout(
            &self.arena,
            match ty.kind() {
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
            },
        );
        TyAbi { ty, layout }
    }

    pub fn mk_type_list(&self, slice: &[Ty<'tcx>]) -> &'tcx List<Ty<'tcx>> {
        if slice.is_empty() {
            List::empty()
        } else {
            self.intern.intern_type_list(&self.arena, slice)
        }
    }

    pub fn fatal(&self, msg: impl Into<String>) -> ! {
        // TODO: Add diagnostic handler
        panic!("{}", msg.into())
    }
}

use {
    crate::codegen::{util, Interned, Stmt},
    ariadne::{Color, Label},
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
        let t = |ty| format!("`{ty}`").fg(colors.err_kw);

        use ariadne::Fmt;

        match self {
            Error::TypeMismatch { expected, found } => (
                "E0228", // sample code
                "mismatch types".into(),
                vec![
                    Label::new(s(src, expected.1))
                        .with_message(format!("expected {} ", t(expected.0)))
                        .with_color(colors.err),
                    Label::new(s(src, found.1))
                        .with_message(format!("found {} ", t(found.0)))
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
                                [expected] => format!("{}", t(*expected)),
                                &[a, b] => format!("expected {} or {}", t(a), t(b)),
                                types => format!(
                                    "expected one of: {}",
                                    util::join_fmt(types, |&ty| t(ty))
                                ),
                            }
                        ))
                        .with_color(colors.err),
                    Label::new(s(src, *span))
                        .with_message(format!("found {} ", t(*ty)))
                        .with_color(colors.err),
                ],
            ),
        }
    }
}

pub type Tx<'tcx> = &'tcx TyCtx<'tcx>;
