use smallvec::{smallvec, SmallVec};
use std::borrow::Cow;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol<S, F, V> {
    pub span: S,
    pub file: F,
    pub value: V,
}

#[derive(Debug, Clone)]
pub struct LocalMap<'src, V> {
    pub scopes: SmallVec<[ahash::HashMap<Cow<'src, str>, V>; 1]>,
}
impl<'src, V> LocalMap<'src, V> {
    pub fn single() -> Self {
        Self {
            scopes: smallvec![Default::default()]
        }
    }
    pub fn insert(&mut self, name: impl Into<Cow<'src, str>>, value: V) {
        self.scopes
            .last_mut()
            .expect("Attempt to insert a local variable at global scope!")
            .insert(name.into(), value);
    }
    pub fn get(&self, name: &str) -> Option<&V> {
        self.scopes.iter().rev().find_map(|s| s.get(name))
    }
    pub fn push_scope(&mut self) {
        self.scopes.push(Default::default());
    }
    pub fn pop_scope(&mut self) -> Option<ahash::HashMap<Cow<'src, str>, V>> {
        self.scopes.pop()
    }
}
impl<V> Default for LocalMap<'_, V> {
    fn default() -> Self {
        Self {
            scopes: SmallVec::new(),
        }
    }
}
