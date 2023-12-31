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

pub(crate) mod util;

pub use {
    lexer::Span,
    tcx::{Arena, DroplessArena, Intern, Session, Tx, TyCtx},
};
