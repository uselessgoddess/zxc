#![feature(
    let_chains,
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
#![allow(internal_features)]

pub mod abi;
pub mod hir;
pub mod mir;
pub mod tcx;

mod fx;

pub(crate) mod index;
pub mod sess;
mod sharded;
mod symbol;
mod sync;
pub(crate) mod util;

pub use tcx::{Arena, DroplessArena, Intern, Session, Tx, TyCtx};
pub(crate) use {
    fx::{FxHashMap, FxHashSet, FxHasher},
    lexer::Span,
    symbol::{sym, Symbol},
    sync::Lock,
};
