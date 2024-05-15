use smallvec::{smallvec, SmallVec};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::{BuildHasher, Hash, RandomState};

/// Convenience type with lookups for nested scopes. Inner scopes can have symbols defined that are
/// inaccessible from outer scopes.
#[derive(Debug, Clone)]
pub struct Scopes<K, V, S = RandomState> {
    pub scopes: SmallVec<[HashMap<K, V, S>; 1]>,
}
impl<K, V, S> Scopes<K, V, S> {
    /// Create a scope list that's empty.
    #[inline]
    pub const fn new_empty() -> Self {
        Self {
            scopes: SmallVec::new_const(),
        }
    }
    /// Create a scope list with a single scope.
    #[inline]
    pub fn new_single() -> Self
    where
        S: Default,
    {
        Self {
            scopes: smallvec![HashMap::default()],
        }
    }

    /// Create a scope list with a single scope, using the provided hasher.
    #[inline]
    pub fn new_single_with_hasher(hasher: S) -> Self {
        Self {
            scopes: smallvec![HashMap::with_hasher(hasher)],
        }
    }

    /// Push a scope to the stack. Same as `self.scopes.push`.
    #[inline(always)]
    pub fn push_scope(&mut self, scope: HashMap<K, V, S>) {
        self.scopes.push(scope);
    }
    /// Create an empty scope and push it to the stack. Same as
    /// `self.scopes.push(Default::default())`.
    #[inline(always)]
    pub fn push_new_scope(&mut self)
    where
        S: Default,
    {
        self.scopes.push(Default::default());
    }

    /// Pop a scope from the stack, returning it if there was one.
    /// Same as `self.scopes.pop()`.
    #[inline(always)]
    pub fn pop_scope(&mut self) -> Option<HashMap<K, V, S>> {
        self.scopes.pop()
    }
}
impl<K: Hash + Eq, V, S: BuildHasher> Scopes<K, V, S> {
    pub fn insert(&mut self, key: K, val: V) -> Option<V> {
        self.scopes
            .last_mut()
            .expect("ICE: attempt to insert symbol without scope")
            .insert(key, val)
    }
    pub fn lookup<Q: Hash + Eq + ?Sized>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
    {
        self.scopes.iter().rev().find_map(|s| s.get(key))
    }
    pub fn lookup_all<'a: 'b, 'b, Q: Hash + Eq + ?Sized>(
        &'a self,
        key: &'b Q,
    ) -> impl DoubleEndedIterator<Item = &'a V> + Clone + 'b
    where
        K: Borrow<Q>,
    {
        self.scopes.iter().rev().filter_map(move |s| s.get(key))
    }
}
impl<K, V, S: BuildHasher> Scopes<K, Vec<V>, S> {
    /// For use with multimaps: insert or append a value. Returns `true` if the key already
    /// existed.
    pub fn insert_one(&mut self, key: K, val: V) -> bool
    where
        K: Hash + Eq,
    {
        let scope = self
            .scopes
            .last_mut()
            .expect("ICE: attempt to insert symbol without scope");
        if let Some(vec) = scope.get_mut(&key) {
            vec.push(val);
            true
        } else {
            scope.insert(key, vec![val]);
            false
        }
    }
}
