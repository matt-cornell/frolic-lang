use hashbrown::HashTable;
use parking_lot::{RwLock, RwLockUpgradableReadGuard};
use std::borrow::{Borrow, Cow};
use std::fmt::{self, Debug, Formatter};
use std::hash::{BuildHasher, Hash};

pub struct Interner<T, S = ahash::RandomState> {
    lookup: RwLock<HashTable<*const T>>,
    storage: boxcar::Vec<T>,
    hash: S,
}

impl<T, S: BuildHasher> Interner<T, S> {
    pub fn intern_with<Q: Hash + Eq + ?Sized, F: FnOnce() -> T>(&self, key: &Q, gen: F) -> &T
    where
        T: Borrow<Q>,
    {
        let hash = self.hash.hash_one(key);
        {
            let lock = self.lookup.read();
            if let Some(out) = lock.find(hash, |val| unsafe { (**val).borrow() == key }) {
                return unsafe { &**out };
            }
        }
        {
            let lock = self.lookup.upgradable_read();
            // double-check
            if let Some(out) = lock.find(hash, |val| unsafe { (**val).borrow() == key }) {
                unsafe { &**out }
            } else {
                let mut lock = RwLockUpgradableReadGuard::upgrade(lock);
                let idx = self.storage.push(gen());
                let r = &self.storage[idx];
                lock.insert_unique(hash, r as *const T, |t| self.hash.hash_one(t));
                r
            }
        }
    }
    pub fn intern_ref<Q: Hash + Eq + ToOwned<Owned = T> + ?Sized>(&self, key: &Q) -> &T
    where
        T: Borrow<Q>,
    {
        self.intern_with(key, || key.to_owned())
    }
    pub fn intern(&self, key: T) -> &T
    where
        T: Hash + Eq,
    {
        let hash = self.hash.hash_one(&key);
        {
            let lock = self.lookup.read();
            if let Some(out) = lock.find(hash, |val| unsafe { **val == key }) {
                return unsafe { &**out };
            }
        }
        {
            let lock = self.lookup.upgradable_read();
            // double-check
            if let Some(out) = lock.find(hash, |val| unsafe { **val == key }) {
                unsafe { &**out }
            } else {
                let mut lock = RwLockUpgradableReadGuard::upgrade(lock);
                let idx = self.storage.push(key);
                let r = &self.storage[idx];
                lock.insert_unique(hash, r as *const T, |t| self.hash.hash_one(t));
                r
            }
        }
    }
    pub fn reset(&mut self) {
        self.storage = boxcar::Vec::new(); // wish there was a way to reuse storage
        self.lookup.get_mut().clear();
    }
}
impl<'a, T: ?Sized + ToOwned + Hash + Eq, S: BuildHasher> Interner<Cow<'a, T>, S> {
    pub fn intern_cow(&self, key: Cow<'a, T>) -> &Cow<'a, T> {
        match key {
            Cow::Borrowed(k) => self.intern_with(k, || Cow::Owned(k.to_owned())),
            Cow::Owned(k) => self.intern(Cow::Owned(k)),
        }
    }
}

impl<T: Debug, S> Debug for Interner<T, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(&self.storage).finish()
    }
}
