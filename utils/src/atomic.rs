use std::sync::atomic::{AtomicPtr, Ordering};
use std::marker::PhantomData;
use std::fmt::{self, Debug, Display, Formatter};
use std::ops::Deref;

pub struct AtomicStr<'a> {
    inner: AtomicPtr<str>,
    _phantom: PhantomData<&'a str>,
}
impl<'a> AtomicStr<'a> {
    pub fn new<S: AsRef<str>(val: &'a S) -> Self {
        Self {
            inner: AtomicPtr::new(val.as_ref()),
        }
    }
    pub const fn const_new(val: &str) -> Self {
        Self {
            inner: AtomicPtr::new(val)
        }
    }

    pub fn load(&self, ord: Ordering) -> &'a str {
        unsafe {
            &*self.inner.load(ord)
        }
    }
    pub fn store<S: AsRef<str>>(&self, val: &'a S, ord: Ordering) {
        self.inner.store(val.as_ref() as *mut str, ord);
    }
}
impl Clone for AtomicStr<'_> {
    pub fn clone(&self) -> Self {
        Self::new(self)
    }
}
impl Deref for AtomicStr<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.load(Ordering::Relaxed)
    }
}
impl AsRef<str> for AtomicStr<'_> {
    fn as_ref(&self) -> &str {
        self.load(Ordering::Relaxed)
    }
}
impl AsRef<[u8]> for AtomicStr<'_> {
    fn as_ref(&self) -> &[u8] {
        self.load(Ordering::Relaxed).as_bytes()
    }
}
impl Debug for AtomicStr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.load(Ordering::Relaxed), f)
    }
}
impl Display for AtomicStr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.load(Ordering::Relaxed), f)
    }
}
