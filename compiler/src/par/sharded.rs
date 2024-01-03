use {
    crate::{
        par::{CachePadded, Lock, LockGuard},
        FxHashMap, FxHasher,
    },
    std::{
        borrow::Borrow,
        collections::hash_map::RawEntryMut,
        hash::{Hash, Hasher},
        mem,
    },
};

const SHARD_BITS: usize = 5;

const SHARDS: usize = 1 << SHARD_BITS;

#[inline]
fn make_hash<K: Hash + ?Sized>(val: &K) -> u64 {
    let mut state = FxHasher::default();
    val.hash(&mut state);
    state.finish()
}

// Source:
//  https://doc.rust-lang.org/stable/nightly-rustc/src/rustc_data_structures/sharded.rs.html#24
#[inline]
fn get_shard_hash(hash: u64) -> usize {
    let hash_len = mem::size_of::<usize>();
    // Ignore the top 7 bits as hashbrown uses these and get the next SHARD_BITS highest bits.
    // hashbrown also uses the lowest bits, so we can't use those
    (hash >> (hash_len * 8 - 7 - SHARD_BITS)) as usize
}

// TODO:
pub struct Place<'sh, T> {
    shards: Box<[CachePadded<LockGuard<'sh, T>>; SHARDS]>,
}

pub struct Sharded<T> {
    shards: Box<[CachePadded<Lock<T>>; SHARDS]>,
}

impl<T: Default> Default for Sharded<T> {
    fn default() -> Self {
        Self::new(Default::default)
    }
}

impl<T> Sharded<T> {
    pub fn new(mut value: impl FnMut() -> T) -> Self {
        Self { shards: Box::new([(); SHARDS].map(|()| CachePadded(Lock::new(value())))) }
    }

    #[inline]
    pub fn get_shard_by_hash(&self, hash: u64) -> &Lock<T> {
        self.get_shard_by_index(get_shard_hash(hash))
    }

    #[inline]
    pub fn get_shard_by_index(&self, i: usize) -> &Lock<T> {
        // SAFETY: The index gets ANDed with the shard mask, ensuring it is always inbounds.
        unsafe { &self.shards.get_unchecked(i & (SHARDS - 1)).0 }
    }

    #[inline]
    pub fn get_shard_by_value<K: Hash + ?Sized>(&self, val: &K) -> &Lock<T> {
        self.get_shard_by_hash(make_hash(val))
    }

    #[inline]
    #[track_caller]
    pub fn lock_shard_by_hash(&self, hash: u64) -> LockGuard<'_, T> {
        self.lock_shard_by_index(get_shard_hash(hash))
    }

    #[inline]
    #[track_caller]
    pub fn lock_shard_by_index(&self, i: usize) -> LockGuard<'_, T> {
        // SAFETY: The index gets ANDed with the shard mask, ensuring it is always inbounds.
        unsafe { self.shards.get_unchecked(i & (SHARDS - 1)).0.lock() }
    }

    #[inline]
    #[track_caller]
    pub fn lock_shard_by_value<K: Hash + ?Sized>(&self, val: &K) -> LockGuard<'_, T> {
        self.lock_shard_by_hash(make_hash(val))
    }
}

pub type ShardedHashMap<K, V> = Sharded<FxHashMap<K, V>>;

#[allow(clippy::len_without_is_empty)]
impl<K: Eq, V> ShardedHashMap<K, V> {
    pub fn len(&self) -> usize {
        todo!()
    }
}

impl<K: Eq + Hash + Copy> ShardedHashMap<K, ()> {
    #[inline]
    pub fn intern_ref<Q>(&self, value: &Q, make: impl FnOnce() -> K) -> K
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let hash = make_hash(&value);
        let mut shard = self.lock_shard_by_hash(hash);
        let entry = shard.raw_entry_mut().from_key_hashed_nocheck(hash, value);

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
        let hash = make_hash(&value);
        let mut shard = self.lock_shard_by_hash(hash);
        let entry = shard.raw_entry_mut().from_key_hashed_nocheck(hash, &value);

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
