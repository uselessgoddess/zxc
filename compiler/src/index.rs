// Modified macro from `index_vec` crate
macro_rules! define_index {
    (
        $v:vis struct $type:ident = $raw:ident;
    ) => {

        #[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
        #[repr(transparent)]
        $v struct $type { _raw: $raw }

        impl $type {
            /// If `Self::CHECKS_MAX_INDEX` is true, we'll assert if trying to
            /// produce a value larger than this in any of the ctors that don't
            /// have `unchecked` in their name.
            $v const MAX_INDEX: usize = $raw::max_value() as usize;

            /// Does this index type assert if asked to construct an index
            /// larger than MAX_INDEX?
            $v const CHECKS_MAX_INDEX: bool = !false;

            /// Construct this index type from a usize. Alias for `from_usize`.
            #[inline(always)]
            $v fn new(value: usize) -> Self {
                Self::from_usize(value)
            }

            /// Construct this index type from the wrapped integer type.
            #[inline(always)]
            $v fn from_raw(value: $raw) -> Self {
                Self::from_usize(value as usize)
            }

            /// Construct this index type from one in a different domain
            #[inline(always)]
            $v fn from_foreign<F: index_vec::Idx>(value: F) -> Self {
                Self::from_usize(value.index())
            }

            /// Construct from a usize without any checks.
            #[inline(always)]
            $v const fn from_usize_unchecked(value: usize) -> Self {
                Self { _raw: value as $raw }
            }

            /// Construct from the underlying type without any checks.
            #[inline(always)]
            $v const fn from_raw_unchecked(raw: $raw) -> Self {
                Self { _raw: raw }
            }

            /// Construct this index type from a usize.
            #[inline]
            $v fn from_usize(value: usize) -> Self {
                Self::check_index(value as usize);
                Self { _raw: value as $raw }
            }

            /// Get the wrapped index as a usize.
            #[inline(always)]
            $v const fn index(self) -> usize {
                self._raw as usize
            }

            /// Get the wrapped index.
            #[inline(always)]
            $v const fn raw(self) -> $raw {
                self._raw
            }

            /// Asserts `v <= Self::MAX_INDEX` unless Self::CHECKS_MAX_INDEX is false.
            #[inline]
            $v fn check_index(v: usize) {
                if Self::CHECKS_MAX_INDEX && (v > Self::MAX_INDEX) {
                    index_vec::__max_check_fail(v, Self::MAX_INDEX);
                }
            }

            const _ENSURE_RAW_IS_UNSIGNED: [(); 0] = [(); <$raw>::min_value() as usize];
        }

        impl core::cmp::PartialOrd<usize> for $type {
            #[inline]
            fn partial_cmp(&self, other: &usize) -> Option<core::cmp::Ordering> {
                self.index().partial_cmp(other)
            }
        }

        impl core::cmp::PartialOrd<$type> for usize {
            #[inline]
            fn partial_cmp(&self, other: &$type) -> Option<core::cmp::Ordering> {
                self.partial_cmp(&other.index())
            }
        }

        impl PartialEq<usize> for $type {
            #[inline]
            fn eq(&self, other: &usize) -> bool {
                self.index() == *other
            }
        }

        impl PartialEq<$type> for usize {
            #[inline]
            fn eq(&self, other: &$type) -> bool {
                *self == other.index()
            }
        }

        impl core::ops::Add<usize> for $type {
            type Output = Self;
            #[inline]
            fn add(self, other: usize) -> Self {
                // use wrapping add so that it's up to the index type whether or
                // not to check -- e.g. if checks are disabled, they're disabled
                // on both debug and release.
                Self::new(self.index().wrapping_add(other))
            }
        }

        impl core::ops::Sub<usize> for $type {
            type Output = Self;
            #[inline]
            fn sub(self, other: usize) -> Self {
                // use wrapping sub so that it's up to the index type whether or
                // not to check -- e.g. if checks are disabled, they're disabled
                // on both debug and release.
                Self::new(self.index().wrapping_sub(other))
            }
        }

        impl core::ops::AddAssign<usize> for $type {
            #[inline]
            fn add_assign(&mut self, other: usize) {
                *self = *self + other
            }
        }

        impl core::ops::SubAssign<usize> for $type {
            #[inline]
            fn sub_assign(&mut self, other: usize) {
                *self = *self - other;
            }
        }

        impl core::ops::Rem<usize> for $type {
            type Output = Self;
            #[inline]
            fn rem(self, other: usize) -> Self {
                Self::new(self.index() % other)
            }
        }

        impl core::ops::Add<$type> for usize {
            type Output = $type;
            #[inline]
            fn add(self, other: $type) -> $type {
                other + self
            }
        }

        impl core::ops::Sub<$type> for usize {
            type Output = $type;
            #[inline]
            fn sub(self, other: $type) -> $type {
                $type::new(self.wrapping_sub(other.index()))
            }
        }

        impl core::ops::Add for $type {
            type Output = $type;
            #[inline]
            fn add(self, other: $type) -> $type {
                $type::new(other.index() + self.index())
            }
        }

        impl core::ops::Sub for $type {
            type Output = $type;
            #[inline]
            fn sub(self, other: $type) -> $type {
                $type::new(self.index().wrapping_sub(other.index()))
            }
        }

        impl core::ops::AddAssign for $type {
            #[inline]
            fn add_assign(&mut self, other: $type) {
                *self = *self + other
            }
        }

        impl core::ops::SubAssign for $type {
            #[inline]
            fn sub_assign(&mut self, other: $type) {
                *self = *self - other;
            }
        }

        impl index_vec::Idx for $type {
            #[inline]
            fn from_usize(value: usize) -> Self {
                Self::from(value)
            }

            #[inline]
            fn index(self) -> usize {
                usize::from(self)
            }
        }

        impl From<$type> for usize {
            #[inline]
            fn from(v: $type) -> usize {
                v.index()
            }
        }

        impl From<usize> for $type {
            #[inline]
            fn from(value: usize) -> Self {
                $type::from_usize(value)
            }
        }
    };
}

pub(crate) use define_index;
