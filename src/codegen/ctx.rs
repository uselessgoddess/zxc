use {
    crate::codegen::{
        abi::{Abi, Align, FieldsShape, Integer, Layout, LayoutKind, Scalar, Size, TyAbi},
        ctx,
        list::List,
        CPlace, DroplessArena, Expr, IntTy, InternSet, Ty, TyKind, TypedArena,
    },
    index_vec::IndexVec,
    std::hash::Hash,
};

mod private {
    #[derive(Clone, Copy, Debug)]
    pub struct PrivateZst;
}

pub struct CommonTypes<'tcx> {
    pub unit: Ty<'tcx>,
    pub i64: Ty<'tcx>,
}

impl<'tcx> CommonTypes<'tcx> {
    pub fn new(intern: &ctx::Intern<'tcx>, _: &Session) -> CommonTypes<'tcx> {
        use TyKind::*;

        // Safety: compiler intrinsics
        let mk = |ty| unsafe { intern.intern_ty(ty) };

        Self { unit: mk(Tuple(List::empty())), i64: mk(Int(IntTy::I64)) }
    }
}

index_vec::define_index_type! {
    pub struct Local = u32;
}

pub struct Arena<'tcx> {
    pub dropless: DroplessArena,
    pub expr: TypedArena<Expr<'tcx>>,
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
            arena: Arena { dropless: DroplessArena::default(), expr: TypedArena::default() },
            intern,
            types,
            locals: IndexVec::with_capacity(128),
        }
    }

    pub fn layout_of(&self, ty: Ty<'tcx>) -> TyAbi<'tcx> {
        // Safety: compiler intrinsics
        let layout = unsafe {
            self.intern.intern_layout(match ty.kind() {
                TyKind::Int(int) => match int {
                    IntTy::I64 => LayoutKind {
                        abi: Abi::Scalar(Scalar::Int(Integer::I64, true)),
                        size: Size::from_bytes(8),
                        align: Align::from_bytes(8).expect("compiler query"),
                        shape: FieldsShape::Primitive,
                    },
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
}

pub type Tx<'tcx> = &'tcx TyCtx<'tcx>;
