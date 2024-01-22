use {
    index_vec::IndexVec,
    std::{fmt, num::ParseIntError},
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

#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Align {
    pow2: u8,
}

impl Align {
    pub const ONE: Align = Align { pow2: 0 };
    pub const MAX: Align = Align { pow2: 29 };

    pub const unsafe fn from_pow2(pow2: u8) -> Self {
        Self { pow2 }
    }

    #[inline]
    pub fn from_bits(bits: u64) -> Result<Align, AlignFromBytesError> {
        Align::from_bytes(Size::from_bits(bits).bytes())
    }

    pub const fn from_bytes(align: u64) -> Result<Align, AlignFromBytesError> {
        if align == 0 {
            return Ok(Align::ONE);
        }

        #[cold]
        const fn not_power_of_2(align: u64) -> AlignFromBytesError {
            AlignFromBytesError::NotPowerOfTwo(align)
        }

        #[cold]
        const fn too_large(align: u64) -> AlignFromBytesError {
            AlignFromBytesError::TooLarge(align)
        }

        let tz = align.trailing_zeros();
        if align != (1 << tz) {
            return Err(not_power_of_2(align)); // not power of two
        }

        let pow2 = tz as u8;
        if pow2 > Self::MAX.pow2 {
            return Err(too_large(align)); // too large
        }

        Ok(Align { pow2 })
    }

    pub fn bytes(self) -> u64 {
        1 << self.pow2
    }

    pub fn bits(self) -> u64 {
        self.bytes() * 8
    }

    pub fn pow2(self) -> u8 {
        self.pow2
    }
}

impl fmt::Debug for Align {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Align({} bytes)", self.bytes())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum AlignFromBytesError {
    NotPowerOfTwo(u64),
    TooLarge(u64),
}

impl AlignFromBytesError {
    pub fn align(self) -> u64 {
        let (Self::NotPowerOfTwo(align) | Self::TooLarge(align)) = self;
        align
    }
}

impl fmt::Display for AlignFromBytesError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AlignFromBytesError::NotPowerOfTwo(align) => write!(f, "`{align}` is not a power of 2"),
            AlignFromBytesError::TooLarge(align) => write!(f, "`{align}` is too large"),
        }
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

    pub const fn from_bytes(bytes: u64) -> Self {
        Self { raw: bytes }
    }

    pub fn from_bits(bits: impl TryInto<u64>) -> Size {
        let bits = bits.try_into().ok().unwrap();
        // Avoid potential overflow from `bits + 7`.
        Size { raw: bits / 8 + ((bits % 8) + 7) / 8 }
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

    #[inline]
    pub fn unsigned_int_max(&self) -> u128 {
        u128::MAX >> (128 - self.bits())
    }
}

use std::ops::Mul;

impl Mul<u64> for Size {
    type Output = Size;

    #[inline]
    fn mul(self, count: u64) -> Size {
        match self.bytes().checked_mul(count) {
            Some(bytes) => Size::from_bytes(bytes),
            None => panic!("Size::mul: {} * {} doesn't fit in u64", self.bytes(), count),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Integer {
    I8,
    I16,
    I32,
    I64,
}

impl Integer {
    #[inline]
    pub fn size(self) -> Size {
        use Integer::*;
        match self {
            I8 => Size::from_bytes(1),
            I16 => Size::from_bytes(2),
            I32 => Size::from_bytes(4),
            I64 => Size::from_bytes(8),
        }
    }

    pub fn align(self, dl: &TargetDataLayout) -> AbiPrefAlign {
        use Integer::*;

        match self {
            I8 => dl.i8_align,
            I16 => dl.i16_align,
            I32 => dl.i32_align,
            I64 => dl.i64_align,
        }
    }

    pub fn approximate_align(data: &TargetDataLayout, wanted: Align) -> Integer {
        use Integer::*;

        for candidate in [I64, I32, I16] {
            if wanted >= candidate.align(data).abi && wanted.bytes() >= candidate.size().bytes() {
                return candidate;
            }
        }
        I8
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WrappingRange {
    pub start: u128,
    pub end: u128,
}

impl WrappingRange {
    pub fn full(size: Size) -> Self {
        Self { start: 0, end: size.unsigned_int_max() }
    }

    #[inline]
    pub fn is_full_for(&self, size: Size) -> bool {
        let max_value = size.unsigned_int_max();
        debug_assert!(self.start <= max_value && self.end <= max_value);
        self.start == (self.end.wrapping_add(1) & max_value)
    }

    #[inline(always)]
    pub fn contains(&self, v: u128) -> bool {
        if self.start <= self.end {
            self.start <= v && v <= self.end
        } else {
            self.start <= v || v <= self.end
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Primitive {
    Int(Integer, bool),
    F32,
    F64,
    Pointer,
}

impl Primitive {
    pub fn size(self, dl: &TargetDataLayout) -> Size {
        use Primitive::*;

        match self {
            Int(i, _) => i.size(),
            F32 => Size::from_bits(32),
            F64 => Size::from_bits(64),
            Pointer => dl.pointer_size,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Scalar {
    Initialized { value: Primitive, valid: WrappingRange },
}

impl Scalar {
    pub fn size(self, dl: &TargetDataLayout) -> Size {
        self.primitive().size(dl)
    }

    pub fn primitive(&self) -> Primitive {
        match *self {
            Scalar::Initialized { value, .. } => value,
        }
    }

    pub fn is_bool(&self) -> bool {
        matches!(
            self,
            Scalar::Initialized {
                value: Primitive::Int(Integer::I8, false),
                valid: WrappingRange { start: 0, end: 1 }
            }
        )
    }

    pub fn is_full_range(&self, dl: &TargetDataLayout) -> bool {
        match *self {
            Scalar::Initialized { valid, .. } => valid.is_full_for(self.size(dl)),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum PointerKind {
    Shared,
    Mutable,
}

pub struct PointeeInfo {
    pub size: Size,
    pub align: Align,
    pub safe: Option<PointerKind>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Abi {
    Scalar(Scalar),
    Aggregate,
    Uninhabited,
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
            Abi::Uninhabited | Abi::Aggregate => self.size.bytes() == 0,
        }
    }

    #[inline]
    pub fn is_uninhabited(&self) -> bool {
        matches!(self.abi, Abi::Uninhabited)
    }

    pub fn is_sized(&self) -> bool {
        true
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum PassMode {
    Ignore,
    Direct, // Add ArgAttributes
}

#[derive(Debug, Copy, Clone)]
pub enum Conv {
    C,
    Zxc,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Endian {
    Little,
    Big,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct AbiPrefAlign {
    pub abi: Align,
    pub pref: Align,
}

impl AbiPrefAlign {
    #[inline]
    pub fn new(align: Align) -> Self {
        Self { abi: align, pref: align }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AddressSpace(pub u32);

impl AddressSpace {
    /// The default address space, corresponding to data space.
    pub const DATA: Self = AddressSpace(0);
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TargetDataLayout {
    pub endian: Endian,
    pub i1_align: AbiPrefAlign,
    pub i8_align: AbiPrefAlign,
    pub i16_align: AbiPrefAlign,
    pub i32_align: AbiPrefAlign,
    pub i64_align: AbiPrefAlign,
    pub i128_align: AbiPrefAlign,
    pub f32_align: AbiPrefAlign,
    pub f64_align: AbiPrefAlign,
    pub pointer_size: Size,
    pub pointer_align: AbiPrefAlign,
    pub aggregate_align: AbiPrefAlign,
    pub vector_align: Vec<(Size, AbiPrefAlign)>,
    pub instruction_address_space: AddressSpace,
}

impl Default for TargetDataLayout {
    fn default() -> TargetDataLayout {
        let align = |bits| Align::from_bits(bits).unwrap();
        TargetDataLayout {
            endian: Endian::Big,
            i1_align: AbiPrefAlign::new(align(8)),
            i8_align: AbiPrefAlign::new(align(8)),
            i16_align: AbiPrefAlign::new(align(16)),
            i32_align: AbiPrefAlign::new(align(32)),
            i64_align: AbiPrefAlign { abi: align(32), pref: align(64) },
            i128_align: AbiPrefAlign { abi: align(32), pref: align(64) },
            f32_align: AbiPrefAlign::new(align(32)),
            f64_align: AbiPrefAlign::new(align(64)),
            pointer_size: Size::from_bits(64),
            pointer_align: AbiPrefAlign::new(align(64)),
            aggregate_align: AbiPrefAlign { abi: align(0), pref: align(64) },
            vector_align: vec![
                (Size::from_bits(64), AbiPrefAlign::new(align(64))),
                (Size::from_bits(128), AbiPrefAlign::new(align(128))),
            ],
            instruction_address_space: AddressSpace::DATA,
        }
    }
}

#[derive(Debug)]
pub enum TargetDataLayoutErrors<'a> {
    InvalidAddressSpace { addr_space: &'a str, cause: &'a str, err: ParseIntError },
    InvalidBits { kind: &'a str, bit: &'a str, cause: &'a str, err: ParseIntError },
    MissingAlignment { cause: &'a str },
    InvalidAlignment { cause: &'a str, err: AlignFromBytesError },
    InconsistentTargetArchitecture { dl: &'a str, target: &'a str },
    InconsistentTargetPointerWidth { pointer_size: u64, target: u32 },
    InvalidBitsSize { err: String },
}

// Inspired from:
//  https://doc.rust-lang.org/stable/nightly-rustc/src/rustc_abi/lib.rs.html#224-318
impl TargetDataLayout {
    pub fn parse_from_llvm_datalayout_string<'a>(
        input: &'a str,
    ) -> Result<TargetDataLayout, TargetDataLayoutErrors<'a>> {
        // Parse an address space index from a string.
        let parse_address_space = |s: &'a str, cause: &'a str| {
            s.parse::<u32>().map(AddressSpace).map_err(|err| {
                TargetDataLayoutErrors::InvalidAddressSpace { addr_space: s, cause, err }
            })
        };

        // Parse a bit count from a string.
        let parse_bits = |s: &'a str, kind: &'a str, cause: &'a str| {
            s.parse::<u64>().map_err(|err| TargetDataLayoutErrors::InvalidBits {
                kind,
                bit: s,
                cause,
                err,
            })
        };

        // Parse a size string.
        let parse_size =
            |s: &'a str, cause: &'a str| parse_bits(s, "size", cause).map(Size::from_bits);

        // Parse an alignment string.
        let parse_align = |s: &[&'a str], cause: &'a str| {
            if s.is_empty() {
                return Err(TargetDataLayoutErrors::MissingAlignment { cause });
            }
            let align_from_bits = |bits| {
                Align::from_bits(bits)
                    .map_err(|err| TargetDataLayoutErrors::InvalidAlignment { cause, err })
            };
            let abi = parse_bits(s[0], "alignment", cause)?;
            let pref = s.get(1).map_or(Ok(abi), |pref| parse_bits(pref, "alignment", cause))?;
            Ok(AbiPrefAlign { abi: align_from_bits(abi)?, pref: align_from_bits(pref)? })
        };

        let mut dl = TargetDataLayout::default();
        let mut i128_align_src = 64;
        for spec in input.split('-') {
            let spec_parts = spec.split(':').collect::<Vec<_>>();

            match &*spec_parts {
                ["e"] => dl.endian = Endian::Little,
                ["E"] => dl.endian = Endian::Big,
                [p] if p.starts_with('P') => {
                    dl.instruction_address_space = parse_address_space(&p[1..], "P")?
                }
                ["a", ref a @ ..] => dl.aggregate_align = parse_align(a, "a")?,
                ["f32", ref a @ ..] => dl.f32_align = parse_align(a, "f32")?,
                ["f64", ref a @ ..] => dl.f64_align = parse_align(a, "f64")?,
                // FIXME(erikdesjardins): we should be parsing nonzero address spaces
                // this will require replacing TargetDataLayout::{pointer_size,pointer_align}
                // with e.g. `fn pointer_size_in(AddressSpace)`
                [p @ "p", s, ref a @ ..] | [p @ "p0", s, ref a @ ..] => {
                    dl.pointer_size = parse_size(s, p)?;
                    dl.pointer_align = parse_align(a, p)?;
                }
                [s, ref a @ ..] if s.starts_with('i') => {
                    let Ok(bits) = s[1..].parse::<u64>() else {
                        parse_size(&s[1..], "i")?; // For the user error.
                        continue;
                    };
                    let a = parse_align(a, s)?;
                    match bits {
                        1 => dl.i1_align = a,
                        8 => dl.i8_align = a,
                        16 => dl.i16_align = a,
                        32 => dl.i32_align = a,
                        64 => dl.i64_align = a,
                        _ => {}
                    }
                    if bits >= i128_align_src && bits <= 128 {
                        // Default alignment for i128 is decided by taking the alignment of
                        // largest-sized i{64..=128}.
                        i128_align_src = bits;
                        dl.i128_align = a;
                    }
                }
                [s, ref a @ ..] if s.starts_with('v') => {
                    let v_size = parse_size(&s[1..], "v")?;
                    let a = parse_align(a, s)?;
                    if let Some(v) = dl.vector_align.iter_mut().find(|v| v.0 == v_size) {
                        v.1 = a;
                        continue;
                    }
                    // No existing entry, add a new one.
                    dl.vector_align.push((v_size, a));
                }
                _ => {} // Ignore everything else.
            }
        }
        Ok(dl)
    }
}
