mod arenas;
mod ctx;
mod hir;
mod intern;
mod mir;
mod numeric;
pub mod util;

use cranelift::prelude::types;
pub use {
    arenas::{DroplessArena, TypedArena},
    ctx::{Arena, Session, Tx, TyCtx},
    intern::{Interned, Sharded},
};

mod abi {
    use {
        crate::codegen::{mir, Interned},
        index_vec::IndexVec,
        std::fmt,
    };

    index_vec::define_index_type! {
        pub struct FieldIdx = u32;
    }

    #[derive(Debug, Clone, Eq, PartialEq, Hash)]
    pub enum FieldsShape {
        Primitive,
        Arbitrary { offsets: IndexVec<FieldIdx, Size>, memory_index: IndexVec<FieldIdx, u32> },
    }

    impl FieldsShape {
        pub fn count(&self) -> usize {
            match *self {
                FieldsShape::Primitive => 0,
                FieldsShape::Arbitrary { ref offsets, .. } => offsets.len(),
            }
        }

        pub fn offset(&self, i: usize) -> Size {
            match *self {
                FieldsShape::Primitive => {
                    unreachable!("FieldsShape::offset: `Primitive`s have no fields")
                }
                FieldsShape::Arbitrary { ref offsets, .. } => offsets[FieldIdx::from_usize(i)],
            }
        }
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
    pub struct Align {
        pow2: u8,
    }

    impl Align {
        pub const ONE: Align = Align { pow2: 0 };
        pub const MAX: Align = Align { pow2: 29 };

        pub fn from_bytes(align: u64) -> Option<Self> {
            if align == 0 {
                return Some(Align::ONE);
            }

            let tz = align.trailing_zeros();
            if align != (1 << tz) {
                return None; // not power of two
            }

            let pow2 = tz as u8;
            if pow2 > Self::MAX.pow2 {
                return None; // too large
            }

            Some(Align { pow2 })
        }

        pub fn bytes(self) -> u64 {
            1 << self.pow2
        }

        pub fn bits(self) -> u64 {
            self.bytes() * 8
        }
    }

    #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Size {
        raw: u64,
    }

    impl fmt::Debug for Size {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "Size({} bytes)", self.bytes())
        }
    }

    impl Size {
        pub const ZERO: Size = Size { raw: 0 };

        pub fn from_bytes(bytes: u64) -> Self {
            Self { raw: bytes }
        }

        pub fn bytes(self) -> u64 {
            self.raw
        }

        pub fn is_aligned(self, align: Align) -> bool {
            let mask = align.bytes() - 1;
            self.bytes() & mask == 0
        }

        pub fn truncate(self, value: u128) -> u128 {
            let size = self.bits();
            if size == 0 {
                // Truncated until nothing is left.
                return 0;
            }
            let shift = 128 - size;
            // Truncate (shift left to drop out leftover values, shift right to fill with zeroes).
            (value << shift) >> shift
        }

        pub fn bits(self) -> u64 {
            #[cold]
            fn overflow(bytes: u64) -> ! {
                panic!("Size::bits: {bytes} bytes in bits doesn't fit in u64")
            }

            self.bytes().checked_mul(8).unwrap_or_else(|| overflow(self.bytes()))
        }
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
    pub enum Integer {
        I8,
        I16,
        I32,
        I64,
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
    pub enum Scalar {
        Int(Integer, bool),
        F32,
        F64,
        Pointer,
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
    pub enum Abi {
        Scalar(Scalar),
        Aggregate,
    }

    #[derive(Debug, Clone, Eq, PartialEq, Hash)]
    pub struct LayoutKind {
        pub abi: Abi,
        pub size: Size,
        pub align: Align,
        pub shape: FieldsShape,
    }

    impl LayoutKind {
        pub fn is_zst(&self) -> bool {
            match self.abi {
                Abi::Scalar(_) => false,
                Abi::Aggregate => self.size.bytes() == 0,
            }
        }
    }

    pub type Layout<'cx> = Interned<'cx, LayoutKind>;

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    pub enum PassMode {
        Ignore,
        Direct, // Add ArgAttributes
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    pub struct TyAbi<'tcx> {
        pub ty: mir::Ty<'tcx>,
        pub layout: Layout<'tcx>,
    }

    #[derive(Debug, Copy, Clone)]
    pub struct ArgAbi<'tcx> {
        pub ty: TyAbi<'tcx>,
        pub mode: PassMode,
    }

    #[derive(Debug, Clone)]
    pub struct FnAbi<'tcx> {
        pub args: Box<[ArgAbi<'tcx>]>,
        pub ret: ArgAbi<'tcx>,
    }
}

use crate::{
    codegen::{
        abi::{Integer, Scalar},
        mir::Ty,
    },
    parse,
};

pub(crate) fn pointer_ty(tcx: Tx<'_>) -> types::Type {
    // match tcx.data_layout.pointer_size.bits() {
    //     16 => types::I16,
    //     32 => types::I32,
    //     64 => types::I64,
    //     bits => todo!("unknown bits: {bits}"),
    // }
    types::I64
}

pub(crate) fn scalar_to_clif(tcx: Tx<'_>, scalar: Scalar) -> types::Type {
    match scalar {
        Scalar::Int(int, _sign) => match int {
            Integer::I8 => types::I8,
            Integer::I16 => types::I16,
            Integer::I32 => types::I32,
            Integer::I64 => types::I64,
        },
        Scalar::F32 => types::F32,
        Scalar::F64 => types::F64,
        Scalar::Pointer => pointer_ty(tcx),
    }
}

#[test]
fn ty_intern() {
    use crate::{parse::ParseBuffer, util::lex_it};

    let mut input = lex_it!("(i32, (i32,))");
    let ty: parse::Type = input.parse().unwrap();

    let arena = Arena::default();
    let tcx = TyCtx::enter(&arena, Session {});

    let a = Ty::analyze(&tcx, &ty);
    let b = Ty::analyze(&tcx, &ty);
    println!("`{a}` is `{b}`");

    assert_eq!(a, b);
    assert_eq!(a.0 as *const _, b.0 as *const _);
}
