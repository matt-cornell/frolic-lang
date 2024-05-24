use smallvec::{smallvec, SmallVec};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::{BuildHasher, Hash, RandomState};

/// Wrapper around a `Vec` of `HashMap`s, but makes lookup more convenient and asserts that the
/// scope list isn't empty.
#[derive(Debug, Clone)]
pub struct Scopes<K, V, S = RandomState> {
    scopes: SmallVec<[HashMap<K, V, S>; 1]>,
}
impl<K, V, S> Scopes<K, V, S> {
    pub fn with_init_scope(scope: HashMap<K, V, S>) -> Self {
        Self {
            scopes: smallvec![scope],
        }
    }
    /// Push a given scope to the scope list.
    pub fn push_scope(&mut self, scope: HashMap<K, V, S>) {
        self.scopes.push(scope);
    }
    /// Pop the innermost scope from the list. Note that the base scope cannot be popped.
    pub fn pop_scope(&mut self) -> Option<HashMap<K, V, S>> {
        (self.scopes.len() > 1).then(|| self.scopes.pop().unwrap())
    }
    /// Get the scope stack as a slice. This slice is never empty.
    pub fn scopes(&self) -> &[HashMap<K, V, S>] {
        &self.scopes
    }
    /// Get the scope stack as a mutable slice. This slice is never empty.
    pub fn scopes_mut(&mut self) -> &mut [HashMap<K, V, S>] {
        &mut self.scopes
    }
    pub fn first_scope(&self) -> &HashMap<K, V, S> {
        &self.scopes[0]
    }
    pub fn first_scope_mut(&mut self) -> &mut HashMap<K, V, S> {
        &mut self.scopes[0]
    }
    pub fn last_scope(&self) -> &HashMap<K, V, S> {
        self.scopes.last().unwrap()
    }
    pub fn last_scope_mut(&mut self) -> &mut HashMap<K, V, S> {
        self.scopes.last_mut().unwrap()
    }
}
impl<K, V, S: Default> Scopes<K, V, S> {
    pub fn new() -> Self {
        Self {
            scopes: smallvec![HashMap::default()],
        }
    }
    /// Push a new scope to the scope list.
    pub fn push_new_scope(&mut self)
    where
        S: Default,
    {
        self.scopes.push(HashMap::default());
    }
}
impl<K: Hash + Eq, V, S: BuildHasher> Scopes<K, V, S> {
    pub fn lookup<Q: Hash + Eq + ?Sized>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
    {
        self.scopes.iter().rev().find_map(|s| s.get(key))
    }
    pub fn lookup_mut<Q: Hash + Eq + ?Sized>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
    {
        self.scopes.iter_mut().rev().find_map(|s| s.get_mut(key))
    }
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.last_scope_mut().insert(key, value)
    }
}
impl<K, V, S: Default> Default for Scopes<K, V, S> {
    fn default() -> Self {
        Self::new()
    }
}
