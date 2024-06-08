use spin::mutex::spin::SpinMutex as Mutex;
use std::cell::Cell;
use std::fmt::{self, Debug, Formatter};
use std::hash::{Hash, Hasher};

/// Acts like a `Cell` but is `Sync`. Under the hood, uses a spinlock.
#[derive(Default)]
pub struct SyncCell<T> {
    lock: Mutex<()>,
    val: Cell<T>,
}
impl<T> SyncCell<T> {
    pub const fn new(value: T) -> Self {
        Self {
            lock: Mutex::new(()),
            val: Cell::new(value),
        }
    }
    pub fn replace(&self, val: T) -> T {
        let _guard = self.lock.lock();
        self.val.replace(val)
    }
    pub fn set(&self, val: T) {
        let _guard = self.lock.lock();
        self.val.set(val);
    }
    pub fn with<R, F: FnOnce(&T) -> R>(&self, f: F) -> R {
        let _guard = self.lock.lock();
        f(unsafe { &*self.val.as_ptr() })
    }
    /// Turn this into a `SyncRef`.
    pub fn as_sync_ref(&self) -> SyncRef<T> {
        SyncRef {
            lock: &self.lock,
            raw: self.val.as_ptr() as *mut (),
            map: |p| p as *mut T,
        }
    }
    /// Get a subreference of this value.
    pub fn map_ref<'a, U>(&'a self, map: fn(&'a mut T) -> &'a mut U) -> SyncRef<'a, U> {
        SyncRef {
            lock: &self.lock,
            raw: self.val.as_ptr() as *mut (),
            map: unsafe {
                std::mem::transmute::<fn(&'a mut T) -> &'a mut U, unsafe fn(*mut ()) -> *mut U>(map)
            }, // function parameters and return have same layout
        }
    }
}
impl<T: Copy> SyncCell<T> {
    pub fn get(&self) -> T {
        let _guard = self.lock.lock();
        self.val.get()
    }
}
impl<T: Default> SyncCell<T> {
    pub fn take(&self) -> T {
        let _guard = self.lock.lock();
        self.val.take()
    }
}
impl<T: Debug> Debug for SyncCell<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let _guard = self.lock.lock();
        let inner = unsafe { &*self.val.as_ptr() };
        f.debug_struct("SyncCell")
            .field("inner", &inner)
            .finish_non_exhaustive()
    }
}
impl<T: Clone> Clone for SyncCell<T> {
    fn clone(&self) -> Self {
        Self::new(self.with(|v| v.clone()))
    }
}
impl<T: PartialEq> PartialEq for SyncCell<T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&self.lock, &other.lock) || self.with(|lhs| other.with(|rhs| lhs == rhs))
    }
}
impl<T: Eq> Eq for SyncCell<T> {}
impl<T: Hash> Hash for SyncCell<T> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.with(|v| v.hash(hasher))
    }
}
unsafe impl<T: Sync> Sync for SyncCell<T> {}

/// A reference, either to a `SyncCell` or a part of it.
pub struct SyncRef<'a, T> {
    lock: &'a Mutex<()>,
    raw: *mut (),
    map: unsafe fn(*mut ()) -> *mut T,
}

impl<'a, T> SyncRef<'a, T> {
    /// Get the reference by mapping the pointer.
    ///
    /// ## Safety
    /// This is unsafe because it doesn't lock the mutex. This value must not be aliased
    /// or raced while the reference is held.
    pub unsafe fn get_ref(&self) -> &'a mut T {
        &mut *(self.map)(self.raw)
    }
    pub fn replace(&self, val: T) -> T {
        let _guard = self.lock.lock();
        std::mem::replace(unsafe { self.get_ref() }, val)
    }
    pub fn set(&self, val: T) {
        let _guard = self.lock.lock();
        unsafe {
            *self.get_ref() = val;
        }
    }
    pub fn with<R, F: FnOnce(&T) -> R>(&self, f: F) -> R {
        let _guard = self.lock.lock();
        f(unsafe { self.get_ref() })
    }
}
impl<T: Copy> SyncRef<'_, T> {
    pub fn get(&self) -> T {
        let _guard = self.lock.lock();
        unsafe { *self.get_ref() }
    }
}
impl<T: Default> SyncRef<'_, T> {
    pub fn take(&self) -> T {
        let _guard = self.lock.lock();
        std::mem::take(unsafe { self.get_ref() })
    }
}
impl<T: Debug> Debug for SyncRef<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let _guard = self.lock.lock();
        let inner = unsafe { self.get_ref() };
        f.debug_struct("SyncRef")
            .field("inner", &inner)
            .finish_non_exhaustive()
    }
}
unsafe impl<T: Send + Sync> Send for SyncRef<'_, T> {}
unsafe impl<T: Send + Sync> Sync for SyncRef<'_, T> {}
