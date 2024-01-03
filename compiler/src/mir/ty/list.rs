use {
    crate::{Arena},
    std::{
        alloc::Layout,
        cmp::Ordering,
        fmt,
        hash::{Hash, Hasher},
        iter, mem,
        ops::Deref,
        ptr, slice,
    },
};

// From rustc implementation
#[repr(C)]
pub struct List<T> {
    len: usize,
    data: [T; 0],
    opaque: OpaqueListContents, // force unsize
}

extern "C" {
    type OpaqueListContents;
}

impl<T> List<T> {
    #[inline(always)]
    pub fn empty<'a>() -> &'a List<T> {
        #[repr(align(64))]
        struct MaxAlign;

        assert!(mem::align_of::<T>() <= mem::align_of::<MaxAlign>());

        #[repr(C)]
        struct InOrder<T, U>(T, U);

        // The empty slice is static and contains a single `0` usize (for the
        // length) that is 64-byte aligned, thus featuring the necessary
        // trailing padding for elements with up to 64-byte alignment.
        static EMPTY_SLICE: InOrder<usize, MaxAlign> = InOrder(0, MaxAlign);
        unsafe { &*(&EMPTY_SLICE as *const _ as *const List<T>) }
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn as_slice(&self) -> &[T] {
        self
    }
}

impl<T: Copy> List<T> {
    #[inline]
    pub(crate) fn from_arena<'tcx>(arena: &'tcx Arena<'tcx>, slice: &[T]) -> &'tcx List<T> {
        assert!(!mem::needs_drop::<T>());
        assert!(mem::size_of::<T>() != 0);
        assert!(!slice.is_empty());

        let (layout, _offset) =
            Layout::new::<usize>().extend(Layout::for_value::<[T]>(slice)).unwrap();
        let mem = arena.dropless.alloc_raw(layout) as *mut List<T>;
        unsafe {
            // Write the length
            ptr::addr_of_mut!((*mem).len).write(slice.len());

            // Write the elements
            ptr::addr_of_mut!((*mem).data)
                .cast::<T>()
                .copy_from_nonoverlapping(slice.as_ptr(), slice.len());

            &*mem
        }
    }

    // prevent
    #[inline(always)]
    pub fn iter(&self) -> <&'_ List<T> as IntoIterator>::IntoIter {
        self.into_iter()
    }
}

impl<T: fmt::Debug> fmt::Debug for List<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl<T: PartialEq> PartialEq for List<T> {
    fn eq(&self, other: &List<T>) -> bool {
        ptr::eq(self, other)
    }
}

impl<T: Eq> Eq for List<T> {}

impl<T> Ord for List<T>
where
    T: Ord,
{
    fn cmp(&self, other: &List<T>) -> Ordering {
        if self == other { Ordering::Equal } else { <[T] as Ord>::cmp(&**self, &**other) }
    }
}

impl<T> PartialOrd for List<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &List<T>) -> Option<Ordering> {
        if self == other {
            Some(Ordering::Equal)
        } else {
            <[T] as PartialOrd>::partial_cmp(&**self, &**other)
        }
    }
}

impl<T> Hash for List<T> {
    fn hash<H: Hasher>(&self, s: &mut H) {
        (self as *const List<T>).hash(s)
    }
}

impl<T> Deref for List<T> {
    type Target = [T];
    #[inline(always)]
    fn deref(&self) -> &[T] {
        self.as_ref()
    }
}

impl<T> AsRef<[T]> for List<T> {
    #[inline(always)]
    fn as_ref(&self) -> &[T] {
        unsafe { slice::from_raw_parts(self.data.as_ptr(), self.len) }
    }
}

impl<'a, T: Copy> IntoIterator for &'a List<T> {
    type Item = T;
    type IntoIter = iter::Copied<<&'a [T] as IntoIterator>::IntoIter>;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self[..].iter().copied()
    }
}

unsafe impl<T: Sync> Sync for List<T> {}
