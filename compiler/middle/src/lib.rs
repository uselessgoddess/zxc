#![feature(
    let_chains,
    try_blocks,
    iter_order_by,
    type_alias_impl_trait,
    vec_into_raw_parts,
    dropck_eyepatch,
    extern_types,
    new_uninit,
    maybe_uninit_slice,
    strict_provenance,
    hash_raw_entry,
    core_intrinsics,
    mem_copy_fn,
    const_option,
    box_patterns,
    never_type,
    negative_impls
)]
#![feature(assert_matches)]
#![allow(clippy::unit_arg, clippy::missing_safety_doc, clippy::should_implement_trait)]
#![allow(internal_features)]
#![deny(unused_must_use)]

pub mod abi;
pub mod hir;
pub mod mir;
pub mod tcx;

mod fx;

#[allow(clippy::all, dead_code)]
mod idx; // inspired from rustc_index

pub(crate) mod index;
mod man;
pub mod par;
pub mod sess;
pub mod spec;
pub mod symbol;
mod temp;
pub mod tls;
pub(crate) mod util;

pub use {
    errors::{self, ariadne, ErrorGuaranteed, FatalError, FatalErrorMarker},
    fx::*,
    idx::{BitSet, Idx, IndexSlice, IndexVec},
    mir::pretty,
    sess::Session,
    tcx::{Arena, DroplessArena, Intern, Tx, TyCtx},
    temp::MaybeTempDir,
};
pub(crate) use {
    lexer::Span,
    par::Lock,
    symbol::{sym, Symbol},
};

pub use {rayon, rayon_core};

pub type Result<T> = std::result::Result<T, ErrorGuaranteed>;

macro_rules! assert_size {
    ($ty:ty as $size:literal) => {
        const _: () = {
            const _: [(); $size] = [(); std::mem::size_of::<$ty>()];
            // assert!(std::mem::size_of::<$ty>() == $size)
        };
    };
}

pub(crate) use assert_size;

macro_rules! index_vec {
    ($($tokens:tt)*) => {
        $crate::IndexVec::from_raw(vec![$($tokens)*])
    }
}

pub(crate) use index_vec;

#[macro_export]
macro_rules! diagnostic {
    {
        $([$($head:tt)*])*
        pub struct $name:ident;
    } => {
        pub struct $name;

        impl<'a, E: Emission> IntoDiagnostic<'a, E> for $name  {
            $crate::diagnostic!(@impl $([$($head)*])*);
        }
    };

    {
        $([$($head:tt)*])*
        pub struct $name:ident $(<$($l:lifetime),*>)? { $($body:tt)* }
    } => {
        pub struct $name$(<$($l),*>)? { $($body)* }

        impl<'a, E: Emission> IntoDiagnostic<'a, E> for $name$(<$($l),*>)?  {
            $crate::diagnostic!(@impl $([$($head)*])*);
        }
    };

    (@impl
        [$fmt:literal $(, $arg:ident)*]
        $([code: $code:ident])?
        $([primary($plevel:expr, $pspan:ident): $fmt_p:literal $(, $arg_p:ident)*])*
        $([note: $fmt_notes:literal $(, $arg_notes:ident)*])*
    ) => {
        fn into_diagnostic(self, handler: &'a Handler) -> DiagnosticBuilder<'a, E> {
            #[allow(unused_mut)]
            let mut diag = handler.struct_diagnostic(format!($fmt $(, self.$arg )*));

            $( diag.code($crate::errors::Code::$code); )?
            $(
                diag.primary($plevel, self.$pspan, Some(format!($fmt_p $(, self.$arg_p )*)));
            )*
            $(
                diag.note(format!($fmt_notes $(, self.$arg_notes )*));
            )*

            diag
        }
    };
}
