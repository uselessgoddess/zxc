use {
    crate::{
        abi::LayoutKind,
        hir,
        mir::{ty::List, TyKind},
    },
    std::{
        borrow::Borrow,
        cmp::Ordering,
        fmt,
        hash::{Hash, Hasher},
        ops::Deref,
        ptr,
    },
};

mod private {
    #[derive(Debug, Copy, Clone)]
    pub struct Zst;
}

pub struct Interned<'a, T: ?Sized>(pub &'a T, pub private::Zst);

impl<T: ?Sized> Clone for Interned<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for Interned<'_, T> {}

impl<'a, T: ?Sized> Interned<'a, T> {
    pub const fn new_unchecked(t: &'a T) -> Self {
        Interned(t, private::Zst)
    }
}

impl<'a, T: ?Sized> Deref for Interned<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.0
    }
}

impl<'a, T: ?Sized> PartialEq for Interned<'a, T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl<'a, T: ?Sized> Eq for Interned<'a, T> {}

impl<'a, T: PartialOrd + ?Sized> PartialOrd for Interned<'a, T> {
    fn partial_cmp(&self, other: &Interned<'a, T>) -> Option<Ordering> {
        if ptr::eq(self.0, other.0) {
            Some(Ordering::Equal)
        } else {
            let res = self.0.partial_cmp(other.0);
            debug_assert_ne!(res, Some(Ordering::Equal)); // satisfy `new_unchecked` rules
            res
        }
    }
}

impl<'a, T: Ord + ?Sized> Ord for Interned<'a, T> {
    fn cmp(&self, other: &Interned<'a, T>) -> Ordering {
        if ptr::eq(self.0, other.0) {
            Ordering::Equal
        } else {
            let res = self.0.cmp(other.0);
            debug_assert_ne!(res, Ordering::Equal); // satisfy `new_unchecked` rules
            res
        }
    }
}

impl<'a, T: ?Sized> Hash for Interned<'a, T> {
    fn hash<H: Hasher>(&self, s: &mut H) {
        ptr::hash(self.0, s)
    }
}

impl<T: fmt::Debug + ?Sized> fmt::Debug for Interned<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<'tcx, T> Borrow<[T]> for Interned<'tcx, List<T>> {
    fn borrow(&self) -> &[T] {
        &self.0[..]
    }
}

impl<'tcx> Borrow<TyKind<'tcx>> for Interned<'tcx, TyKind<'tcx>> {
    fn borrow(&self) -> &TyKind<'tcx> {
        self.0
    }
}

impl<'tcx> Borrow<LayoutKind> for Interned<'tcx, LayoutKind> {
    fn borrow(&self) -> &LayoutKind {
        self.0
    }
}
