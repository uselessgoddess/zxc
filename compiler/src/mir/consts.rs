use crate::{abi::Size, mir::ScalarRepr};

#[derive(Copy, Clone)]
/// A type for representing any integer. Only used for printing.
pub struct ConstInt {
    /// The "untyped" variant of `ConstInt`.
    int: ScalarRepr,
    /// Whether the value is of a signed integer type.
    signed: bool,
    /// Whether the value is a `usize` or `isize` type.
    is_ptr_sized_integral: bool,
}

impl ConstInt {
    pub fn new(int: ScalarRepr, signed: bool, is_ptr_sized_integral: bool) -> Self {
        Self { int, signed, is_ptr_sized_integral }
    }
}

#[allow(clippy::unnecessary_cast)]
impl std::fmt::Debug for ConstInt {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { int, signed, is_ptr_sized_integral } = *self;
        let size = int.size().bytes();
        let raw = int.data;
        if signed {
            let bit_size = size * 8;
            let min = 1u128 << (bit_size - 1);
            let max = min - 1;
            if raw == min {
                match (size, is_ptr_sized_integral) {
                    (_, true) => write!(fmt, "isize::MIN"),
                    (1, _) => write!(fmt, "i8::MIN"),
                    (2, _) => write!(fmt, "i16::MIN"),
                    (4, _) => write!(fmt, "i32::MIN"),
                    (8, _) => write!(fmt, "i64::MIN"),
                    (16, _) => write!(fmt, "i128::MIN"),
                    // TODO: add more human panics
                    _ => {
                        panic!("ConstInt 0x{:x} with size = {} and signed = {}", raw, size, signed)
                    }
                }
            } else if raw == max {
                match (size, is_ptr_sized_integral) {
                    (_, true) => write!(fmt, "isize::MAX"),
                    (1, _) => write!(fmt, "i8::MAX"),
                    (2, _) => write!(fmt, "i16::MAX"),
                    (4, _) => write!(fmt, "i32::MAX"),
                    (8, _) => write!(fmt, "i64::MAX"),
                    (16, _) => write!(fmt, "i128::MAX"),
                    _ => {
                        panic!("ConstInt 0x{:x} with size = {} and signed = {}", raw, size, signed)
                    }
                }
            } else {
                match size {
                    1 => write!(fmt, "{}", raw as i8)?,
                    2 => write!(fmt, "{}", raw as i16)?,
                    4 => write!(fmt, "{}", raw as i32)?,
                    8 => write!(fmt, "{}", raw as i64)?,
                    16 => write!(fmt, "{}", raw as i128)?,
                    _ => {
                        panic!("ConstInt 0x{:x} with size = {} and signed = {}", raw, size, signed)
                    }
                }
                if fmt.alternate() {
                    match (size, is_ptr_sized_integral) {
                        (_, true) => write!(fmt, "_isize")?,
                        (1, _) => write!(fmt, "_i8")?,
                        (2, _) => write!(fmt, "_i16")?,
                        (4, _) => write!(fmt, "_i32")?,
                        (8, _) => write!(fmt, "_i64")?,
                        (16, _) => write!(fmt, "_i128")?,
                        _ => panic!(),
                    }
                }
                Ok(())
            }
        } else {
            let max = Size::from_bytes(size).truncate(u128::MAX);
            if raw == max {
                match (size, is_ptr_sized_integral) {
                    (_, true) => write!(fmt, "usize::MAX"),
                    (1, _) => write!(fmt, "u8::MAX"),
                    (2, _) => write!(fmt, "u16::MAX"),
                    (4, _) => write!(fmt, "u32::MAX"),
                    (8, _) => write!(fmt, "u64::MAX"),
                    (16, _) => write!(fmt, "u128::MAX"),
                    _ => {
                        panic!("ConstInt 0x{:x} with size = {} and signed = {}", raw, size, signed)
                    }
                }
            } else {
                match size {
                    1 => write!(fmt, "{}", raw as u8)?,
                    2 => write!(fmt, "{}", raw as u16)?,
                    4 => write!(fmt, "{}", raw as u32)?,
                    8 => write!(fmt, "{}", raw as u64)?,
                    16 => write!(fmt, "{}", raw as u128)?,
                    _ => {
                        panic!("ConstInt 0x{:x} with size = {} and signed = {}", raw, size, signed)
                    }
                }
                if fmt.alternate() {
                    match (size, is_ptr_sized_integral) {
                        (_, true) => write!(fmt, "_usize")?,
                        (1, _) => write!(fmt, "_u8")?,
                        (2, _) => write!(fmt, "_u16")?,
                        (4, _) => write!(fmt, "_u32")?,
                        (8, _) => write!(fmt, "_u64")?,
                        (16, _) => write!(fmt, "_u128")?,
                        _ => panic!(),
                    }
                }
                Ok(())
            }
        }
    }
}
