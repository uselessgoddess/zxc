use {
    crate::codegen::{
        abi::LayoutKind,
        mir::{ty::List, TyKind},
    },
    parking_lot::Mutex,
    std::{
        borrow::Borrow,
        cmp::Ordering,
        collections::{hash_map::RawEntryMut, HashMap},
        fmt,
        hash::{BuildHasher, Hash, Hasher},
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

pub struct HashSet<P>(HashMap<P, ()>);

impl<P: Deref + Eq + Hash> Default for HashSet<P> {
    fn default() -> Self {
        HashSet::new()
    }
}
impl<P> HashSet<P> {
    pub fn new() -> Self {
        HashSet(HashMap::new())
    }
}
impl<P: Deref + Eq + Hash> HashSet<P> {
    pub fn get<Q: ?Sized + Eq + Hash>(&self, key: &Q) -> Option<&P>
    where
        P::Target: Borrow<Q>,
    {
        let hash = self.0.hasher().hash_one(key);
        self.0
            .raw_entry()
            .from_hash(hash, |k| <P::Target as Borrow<Q>>::borrow(k) == key)
            .as_ref()
            .map(|kv| kv.0)
    }
    pub fn _take<Q: ?Sized + Hash + Eq>(&mut self, k: &Q) -> Option<P>
    where
        P: Borrow<Q>,
    {
        self.0.remove_entry(k).map(|(a, ())| a)
    }

    pub fn insert(&mut self, x: P) {
        self.0.insert(x, ());
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

// FIXME: later use sharder by `Hash` HashMap
pub struct Sharded<T> {
    data: Mutex<HashSet<T>>,
}

impl<T> Default for Sharded<T> {
    fn default() -> Self {
        Self { data: Mutex::new(HashSet::new()) }
    }
}

impl<K: Eq + Hash + Copy> Sharded<K> {
    #[inline]
    pub fn intern_ref<Q>(&self, value: &Q, make: impl FnOnce() -> K) -> K
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let mut lock = self.data.lock();
        let hash = lock.0.hasher().hash_one(value);
        let entry = lock.0.raw_entry_mut().from_key_hashed_nocheck(hash, value);

        match entry {
            RawEntryMut::Occupied(e) => *e.key(),
            RawEntryMut::Vacant(e) => {
                let v = make();
                e.insert_hashed_nocheck(hash, v, ());
                v
            }
        }
    }

    #[inline]
    pub fn intern<Q>(&self, value: Q, make: impl FnOnce(Q) -> K) -> K
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        let mut lock = self.data.lock();
        let hash = lock.0.hasher().hash_one(&value);
        let entry = lock.0.raw_entry_mut().from_key_hashed_nocheck(hash, &value);

        match entry {
            RawEntryMut::Occupied(e) => *e.key(),
            RawEntryMut::Vacant(e) => {
                let v = make(value);
                e.insert_hashed_nocheck(hash, v, ());
                v
            }
        }
    }
}
