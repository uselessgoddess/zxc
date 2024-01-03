mod arenas;
pub mod intern;

use std::fmt;
pub use {
    arenas::{DroplessArena, TypedArena},
    intern::Interned,
};

use crate::{
    abi, hir,
    mir::{
        self,
        ty::{self, List},
        IntTy, Ty, TyKind,
    },
    par::{ShardedHashMap, WorkerLocal},
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
    pub isize: Ty<'tcx>,
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
            isize: mk(Int(IntTy::Isize)),
        }
    }
}

pub struct CommonSigs<'tcx> {
    pub main: &'tcx [mir::FnSig<'tcx>],
}

impl<'tcx> CommonSigs<'tcx> {
    pub fn new(
        intern: &Intern<'tcx>,
        arena: &'tcx Arena<'tcx>,
        types: &CommonTypes<'tcx>,
    ) -> CommonSigs<'tcx> {
        let main = arena.dropless.alloc_from_iter([
            mir::FnSig {
                inputs_and_output: intern.intern_type_list(arena, &[types.unit]),
                abi: ty::Abi::Zxc,
            },
            mir::FnSig {
                inputs_and_output: intern
                    .intern_type_list(arena, &[types.isize, types.isize, types.isize]),
                abi: ty::Abi::C,
            },
        ]);
        Self { main }
    }
}

#[derive(Default)]
pub struct Arena<'tcx> {
    pub dropless: DroplessArena,
    pub expr: TypedArena<hir::Expr<'tcx>>,
    pub stmt: TypedArena<hir::Stmt<'tcx>>,
    pub scope: TypedArena<hir::Scope<'tcx>>,
    pub type_: TypedArena<TyKind<'tcx>>,
    pub layout: TypedArena<abi::LayoutKind>,
}

type InternSet<'tcx, T> = ShardedHashMap<Interned<'tcx, T>, ()>;

#[derive(Default)]
pub struct Intern<'tcx> {
    pub types: InternSet<'tcx, TyKind<'tcx>>,
    pub symbols: InternSet<'tcx, TyKind<'tcx>>,
    pub layouts: InternSet<'tcx, abi::LayoutKind>,
    pub type_lists: InternSet<'tcx, List<Ty<'tcx>>>,
}

impl<'tcx> Intern<'tcx> {
    pub fn intern_ty(&self, arena: &'tcx Arena<'tcx>, kind: TyKind<'tcx>) -> Ty<'tcx> {
        self.types.intern(kind, |kind| Interned::new_unchecked(arena.type_.alloc(kind)))
    }

    pub fn intern_layout(
        &self,
        arena: &'tcx Arena<'tcx>,
        kind: abi::LayoutKind,
    ) -> abi::Layout<'tcx> {
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
    pub arena: &'tcx WorkerLocal<Arena<'tcx>>,
    pub intern: Intern<'tcx>,
    pub types: CommonTypes<'tcx>,
    pub sigs: CommonSigs<'tcx>,
    pub sess: &'tcx Session,
}

impl fmt::Debug for TyCtx<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TyCtx {{ todo }}")
    }
}

pub struct Session {}

impl Session {
    pub fn fake() -> Self {
        Self {}
    }
}

impl<'tcx> TyCtx<'tcx> {
    pub fn enter(arena: &'tcx WorkerLocal<Arena<'tcx>>, sess: &'tcx Session) -> Self {
        let intern = Intern::default();
        let types = CommonTypes::new(&intern, arena, sess);
        let sigs = CommonSigs::new(&intern, arena, &types);

        Self { arena, intern, types, sigs, sess }
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

    pub fn intern_ty(&self, kind: TyKind<'tcx>) -> Ty<'tcx> {
        self.intern.intern_ty(self.arena, kind)
    }
}

pub type Tx<'tcx> = &'tcx TyCtx<'tcx>;
