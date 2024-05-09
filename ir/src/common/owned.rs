use portable_atomic::{AtomicPtr, Ordering};
use std::mem::ManuallyDrop;
use std::ops::Deref;

/// Type used to signify that a value is stored on the heap but managed elsewhere.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Owned<T> {
    inner: ManuallyDrop<Box<T>>,
}
impl<T> Owned<T> {
    pub unsafe fn from_raw(value: *mut T) -> Self {
        Self {
            inner: ManuallyDrop::new(Box::from_raw(value)),
        }
    }
    pub unsafe fn try_from_raw(value: *mut T) -> Option<Self> {
        (!value.is_null()).then(|| Self::from_raw(value))
    }
    pub fn into_raw(this: Self) -> *mut T {
        Box::into_raw(ManuallyDrop::into_inner(this.inner))
    }
}
impl<T> Deref for Owned<T> {
    type Target = Box<T>;

    fn deref(&self) -> &Self::Target {
        &*self.inner
    }
}
impl<T> Clone for Owned<T> {
    fn clone(&self) -> Self {
        unsafe { Self::from_raw(&*self.inner as *const _ as *mut _) }
    }
}

/// Write-once owned value.
pub struct MonoOwned<T> {
    inner: AtomicPtr<T>,
}
impl<T> MonoOwned<T> {
    pub const fn empty() -> Self {
        Self {
            inner: AtomicPtr::new(std::ptr::null_mut()),
        }
    }
    pub fn new(value: Option<Owned<T>>) -> Self {
        Self {
            inner: AtomicPtr::new(value.map_or(std::ptr::null_mut(), |v| {
                Box::as_ref(&v.inner) as *const _ as *mut _
            })),
        }
    }
    pub fn store(&self, value: Owned<T>) -> Result<(), Owned<T>> {
        let ptr = Owned::into_raw(value);
        match self.inner.compare_exchange(
            std::ptr::null_mut(),
            ptr,
            Ordering::Release,
            Ordering::Relaxed,
        ) {
            Ok(_) => Ok(()),
            Err(_) => unsafe { Err(Owned::from_raw(ptr)) },
        }
    }
    pub fn load(&self) -> Option<&T> {
        unsafe { self.inner.load(Ordering::Acquire).as_ref() }
    }
}
impl<T> From<Owned<T>> for MonoOwned<T> {
    fn from(value: Owned<T>) -> Self {
        Self::new(Some(value))
    }
}
impl<T> From<Option<Owned<T>>> for MonoOwned<T> {
    fn from(value: Option<Owned<T>>) -> Self {
        Self::new(value)
    }
}
