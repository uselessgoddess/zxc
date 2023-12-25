use {
    parking_lot::Mutex,
    std::{
        borrow::Borrow,
        cmp::Ordering,
        collections::HashMap,
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

pub struct Interned<'a, T>(pub &'a T, pub private::Zst);

impl<T> Clone for Interned<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Interned<'_, T> {}

impl<'a, T> Interned<'a, T> {
    pub const fn new_unchecked(t: &'a T) -> Self {
        Interned(t, private::Zst)
    }
}

impl<'a, T> Deref for Interned<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.0
    }
}

impl<'a, T> PartialEq for Interned<'a, T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl<'a, T> Eq for Interned<'a, T> {}

impl<'a, T: PartialOrd> PartialOrd for Interned<'a, T> {
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

impl<'a, T: Ord> Ord for Interned<'a, T> {
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

impl<'a, T> Hash for Interned<'a, T> {
    fn hash<H: Hasher>(&self, s: &mut H) {
        ptr::hash(self.0, s)
    }
}

impl<T: fmt::Debug> fmt::Debug for Interned<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
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

pub struct InternSet<T: ?Sized> {
    data: Mutex<HashSet<Box<T>>>,
}

impl<T> InternSet<T> {
    pub fn new() -> Self {
        Self { data: Mutex::new(HashSet::new()) }
    }
}

impl<T: Eq + Hash> InternSet<T> {
    pub unsafe fn intern_outlive<'a>(&self, val: T) -> Interned<'a, T> {
        let mut lock = self.data.lock();
        Interned::new_unchecked({
            &*if let Some(b) = lock.get(&val) {
                b.as_ref() as *const T
            } else {
                let b = Box::new(val);
                let p = b.as_ref() as *const T;
                lock.insert(b);
                p
            }
        })
    }
}
