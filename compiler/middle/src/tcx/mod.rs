mod arenas;
pub mod intern;

pub use {
    arenas::{DroplessArena, TypedArena},
    intern::Interned,
};
use {
    errors::{Diagnostic, Style},
    lexer::Span,
    lint::{DecorateLint, Level, Lint, LintId, LintStore},
    std::fmt,
};

use crate::{
    abi,
    hir::{self, attr},
    lints::{self, LevelSource},
    mir::{
        self,
        ty::{self, List},
        IntTy, PlaceElem, Ty, TyKind,
    },
    par::{ShardedHashMap, WorkerLocal},
    sess::{output, ModuleType, OutputFilenames},
    FxHashMap, Session,
};

mod private {
    #[derive(Clone, Copy, Debug)]
    pub struct PrivateZst;
}

pub struct CommonTypes<'tcx> {
    pub never: Ty<'tcx>,
    pub unit: Ty<'tcx>,
    pub bool: Ty<'tcx>,
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
            never: mk(Never),
            unit: mk(Tuple(List::empty())),
            bool: mk(Bool),
            i8: mk(Int(IntTy::I8)),
            i16: mk(Int(IntTy::I16)),
            i32: mk(Int(IntTy::I32)),
            i64: mk(Int(IntTy::I64)),
            isize: mk(Int(IntTy::Isize)),
        }
    }
}

pub struct CommonSigs<'tcx> {
    pub main: mir::FnSig<'tcx>,
    pub start: mir::FnSig<'tcx>,
}

impl<'tcx> CommonSigs<'tcx> {
    pub fn new(
        intern: &Intern<'tcx>,
        arena: &'tcx Arena<'tcx>,
        types: &CommonTypes<'tcx>,
    ) -> CommonSigs<'tcx> {
        let main = mir::FnSig {
            inputs_and_output: intern.intern_type_list(arena, &[types.unit]),
            abi: ty::Abi::Zxc,
        };
        let start = mir::FnSig {
            inputs_and_output: intern
                .intern_type_list(arena, &[types.isize, types.isize, types.isize]),
            abi: ty::Abi::Zxc,
        };

        Self { main, start }
    }
}

// TODO: add macro like in `rustc`
#[derive(Default)]
pub struct Arena<'tcx> {
    pub dropless: DroplessArena,
    pub expr: TypedArena<hir::Expr<'tcx>>,
    pub stmt: TypedArena<hir::Stmt<'tcx>>,
    pub scope: TypedArena<hir::Scope<'tcx>>,
    pub ffi: TypedArena<hir::ForeignItem<'tcx>>,
    pub type_: TypedArena<TyKind<'tcx>>,
    pub layout: TypedArena<abi::LayoutKind>,
    pub attrs: TypedArena<attr::MetaItem>,
    pub body: TypedArena<mir::Body<'tcx>>,
}

type InternSet<'tcx, T> = ShardedHashMap<Interned<'tcx, T>, ()>;

#[derive(Default)]
pub struct Intern<'tcx> {
    pub types: InternSet<'tcx, TyKind<'tcx>>,
    pub symbols: InternSet<'tcx, TyKind<'tcx>>,
    pub layouts: InternSet<'tcx, abi::LayoutKind>,
    pub type_lists: InternSet<'tcx, List<Ty<'tcx>>>,
    pub place_elems: InternSet<'tcx, List<PlaceElem<'tcx>>>,
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
    output: OutputFilenames,
    module_types: Vec<ModuleType>,
    pub lints: LintStore,
    levels: FxHashMap<LintId, (Level, LevelSource)>,
}

impl fmt::Debug for TyCtx<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TyCtx {{ todo }}")
    }
}

impl<'tcx> TyCtx<'tcx> {
    pub fn enter(
        arena: &'tcx WorkerLocal<Arena<'tcx>>,
        sess: &'tcx Session,
        output: OutputFilenames,
    ) -> Self {
        let intern = Intern::default();
        let types = CommonTypes::new(&intern, arena, sess);
        let sigs = CommonSigs::new(&intern, arena, &types);

        let mut lints = LintStore::new();
        lint::register_lints(&mut lints);
        let levels = lints::lint_levels_on_raw(&lints, &sess.opts.lints);

        let module_types = collect_module_types(sess);
        Self { arena, intern, types, sigs, sess, output, lints, levels, module_types }
    }

    pub fn mk_type_list(&self, slice: &[Ty<'tcx>]) -> &'tcx List<Ty<'tcx>> {
        if slice.is_empty() {
            List::empty()
        } else {
            self.intern.intern_type_list(self.arena, slice)
        }
    }

    pub fn mk_place_elems(&self, slice: &[PlaceElem<'tcx>]) -> &'tcx List<PlaceElem<'tcx>> {
        if slice.is_empty() {
            List::empty()
        } else {
            self.intern
                .place_elems
                .intern_ref(slice, || Interned::new_unchecked(List::from_arena(self.arena, slice)))
                .0
        }
    }

    pub fn empty_hir_scope(&self, sig: hir::FnSig<'tcx>) -> &mut hir::Scope<'tcx> {
        self.arena.scope.alloc(hir::Scope::new(Some(sig)))
    }

    pub fn fatal(&self, msg: impl Into<String>) -> ! {
        // TODO: Add diagnostic handler
        panic!("{}", msg.into())
    }

    pub fn intern_ty(&self, kind: TyKind<'tcx>) -> Ty<'tcx> {
        self.intern.intern_ty(self.arena, kind)
    }

    pub fn output_filenames(&self) -> &OutputFilenames {
        &self.output
    }

    pub fn module_types(&self) -> &[ModuleType] {
        &self.module_types
    }

    pub fn lint_level_at(&self, lint: LintId) -> (Level, LevelSource) {
        self.levels.get(&lint).copied().unwrap_or_else(|| todo!())
    }

    pub fn emit_spanned_lint(
        &self,
        span: Span,
        lint: &'static Lint,
        decorate: impl for<'a> DecorateLint<'a>,
    ) {
        let sess = self.sess;
        let (level, src) = self.lint_level_at(LintId::of(lint));

        let mut diag = match level {
            Level::Allow => return,
            Level::Warn => sess.diagnostic().struct_warn(""),
            Level::Deny => sess.diagnostic().struct_err_lint(""),
        };

        diag.span = Some(span);
        diag.message = (decorate.message(), Style::NoStyle);

        decorate.decorate_lint(&mut diag);
        explain_source(lint, level, src, &mut *diag);
        diag.emit();
    }
}

fn explain_source(lint: &'static Lint, level: Level, src: LevelSource, err: &mut Diagnostic) {
    let name = lint.name_lower();
    match src {
        LevelSource::Default => {
            err.note(format!("`#[{}({})]` on by default", level.as_str(), name));
        }
        LevelSource::CommandLine(lint_flag, default) => {
            let hyphen_name = name.replace('_', "-");
            let flag = default.as_cmd_flag();
            if lint_flag.as_str() == name {
                err.note(format!("requested on the command line with `{flag} {hyphen_name}`"));
            } else {
                let hyphen_flag = lint_flag.as_str().replace('_', "-");
                err.note(format!("`{flag} {hyphen_name}` implied by `{flag} {hyphen_flag}`"));
                err.help(format!("to override `{flag} {hyphen_flag}` add `#[allow({name})]`"));
            }
        }
    }
}

fn collect_module_types(sess: &Session) -> Vec<ModuleType> {
    let mut base = sess.opts.module_types.clone();

    if base.is_empty() {
        base.push(output::default_output_for_target(sess));
    }

    base
}

pub type Tx<'tcx> = &'tcx TyCtx<'tcx>;
