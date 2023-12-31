use crate::codegen::{
    abi,
    abi::{Abi, Align, FieldsShape, Integer, Layout, LayoutKind, PassMode, Scalar, Size, TyAbi},
    hir,
    mir::{self, ty::List, IntTy, Ty, TyKind},
    DroplessArena, Interned, Sharded, TypedArena,
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

#[derive(Default)]
pub struct Arena<'tcx> {
    pub dropless: DroplessArena,
    pub expr: TypedArena<hir::Expr<'tcx>>,
    pub stmt: TypedArena<hir::Stmt<'tcx>>,
    pub scope: TypedArena<hir::Scope<'tcx>>,
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
}

pub struct Session {}

impl<'tcx> TyCtx<'tcx> {
    pub fn enter(arena: &'tcx Arena<'tcx>, sess: Session) -> Self {
        let intern = Intern::default();
        let types = CommonTypes::new(&intern, arena, &sess);

        Self { arena, intern, types }
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
            self.arena,
            match ty.kind() {
                TyKind::Int(int) => match int {
                    IntTy::I8 => scalar(Integer::I8, true, (1, 1)),
                    IntTy::I16 => scalar(Integer::I16, true, (2, 2)),
                    IntTy::I32 => scalar(Integer::I32, true, (4, 4)),
                    IntTy::I64 => scalar(Integer::I64, true, (8, 8)),
                    _ => todo!(),
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
            self.intern.intern_type_list(self.arena, slice)
        }
    }

    pub fn empty_hir_scope(&self) -> &mut hir::Scope<'tcx> {
        self.arena.scope.alloc(hir::Scope::new())
    }

    pub fn fatal(&self, msg: impl Into<String>) -> ! {
        // TODO: Add diagnostic handler
        panic!("{}", msg.into())
    }

    pub fn fn_abi_of_sig(&self, sig: mir::FnSig<'tcx>) -> abi::FnAbi<'tcx> {
        let probe_abi = |tcx, ty| abi::ArgAbi {
            ty: self.layout_of(ty),
            mode: match ty.kind() {
                TyKind::Int(_) => PassMode::Direct,
                TyKind::Tuple(types) => {
                    if types.is_empty() {
                        PassMode::Ignore
                    } else {
                        PassMode::Direct
                    }
                }
            },
        };

        // FIXME: now `sig.` doesn't affect on abi inferring
        abi::FnAbi {
            args: sig.inputs().iter().map(|&ty| probe_abi(self, ty)).collect(),
            ret: probe_abi(self, sig.output()),
        }
    }
}

pub type Tx<'tcx> = &'tcx TyCtx<'tcx>;
