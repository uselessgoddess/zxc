#![feature(
    let_chains,
    try_blocks,
    vec_into_raw_parts,
    dropck_eyepatch,
    extern_types,
    new_uninit,
    maybe_uninit_slice,
    strict_provenance,
    hash_raw_entry,
    core_intrinsics,
    mem_copy_fn
)]
#![feature(iter_order_by)]
#![allow(clippy::unit_arg)]
#![allow(internal_features)]

pub mod abi;
pub mod hir;
pub mod mir;
pub mod tcx;

mod fx;

pub(crate) mod index;
pub mod par;
pub mod sess;
pub mod symbol;
pub(crate) mod util;

pub use {
    ariadne,
    mir::pretty,
    tcx::{Arena, DroplessArena, Intern, Session, Tx, TyCtx},
};
pub(crate) use {
    fx::{FxHashMap, FxHashSet, FxHasher},
    lexer::Span,
    par::Lock,
    symbol::{sym, Symbol},
};

pub use {rayon, rayon_core};
