#![allow(clippy::borrowed_box)]

use super::owned::Owned;
use portable_atomic::{AtomicPtr, Ordering, fence};
use std::marker::PhantomData;
use std::ptr::NonNull;

/// An iterator over elements in a linked list.
#[derive(Debug)]
pub struct Iter<'a, T> {
    inner: Option<NonNull<T>>,
    _phantom: PhantomData<&'a ()>,
}
impl<'a, T: LinkedListElem + 'a> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            let elem = self.inner?.as_ref();
            self.inner = NonNull::new(elem.next_elem().load(Ordering::Relaxed));
            Some(elem)
        }
    }
}
/// A mutable iterator over elements in a linked list.
#[derive(Debug)]
pub struct IterMut<'a, T> {
    inner: Option<NonNull<T>>,
    _phantom: PhantomData<&'a ()>,
}
impl<'a, T: LinkedListElem + 'a> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            let elem = self.inner?.as_mut();
            self.inner = NonNull::new(elem.next_elem().load(Ordering::Relaxed));
            Some(elem)
        }
    }
}

/// Common trait for an intrusive linked list, elements also have a pointer to this list.
/// ## Safety
/// Don't mess with the element pointers if you don't know what you're doing.
pub unsafe trait LinkedList: Sized {
    type Elem: LinkedListElem<Parent = Self>;

    /// Get the pointer to the first element
    fn first_elem(&self) -> &AtomicPtr<Self::Elem>;
    /// Get the pointer to the last element
    fn last_elem(&self) -> &AtomicPtr<Self::Elem>;

    /// Push an element to the front of the list
    fn prepend(self: &Box<Self>, elem: Box<Self::Elem>) -> Owned<Self::Elem> {
        unsafe {
            let ptr = Box::into_raw(elem);
            (*ptr).unlink();
            (*ptr)
                .parent()
                .store(&**self as *const Self as *mut Self, Ordering::Relaxed);
            let ldef = self.first_elem().swap(ptr, Ordering::AcqRel);
            (*ptr).next_elem().store(ldef, Ordering::Release);
            if let Some(ld) = ldef.as_ref() {
                ld.prev_elem().store(ptr, Ordering::Release);
            } else {
                self.last_elem().store(ptr, Ordering::Release);
            }
            Owned::from_raw(ptr)
        }
    }
    /// Push an element to the back of the list
    fn append(self: &Box<Self>, elem: Box<Self::Elem>) -> Owned<Self::Elem> {
        unsafe {
            let ptr = Box::into_raw(elem);
            (*ptr).unlink();
            (*ptr)
                .parent()
                .store(&**self as *const Self as *mut Self, Ordering::Relaxed);
            let ldef = self.last_elem().swap(ptr, Ordering::AcqRel);
            (*ptr).prev_elem().store(ldef, Ordering::Release);
            if let Some(ld) = ldef.as_ref() {
                ld.next_elem().store(ptr, Ordering::Release);
            } else {
                self.first_elem().store(ptr, Ordering::Release);
            }
            Owned::from_raw(ptr)
        }
    }
    fn iter(&self) -> Iter<Self::Elem> {
        Iter {
            inner: NonNull::new(self.first_elem().load(Ordering::Relaxed)),
            _phantom: PhantomData,
        }
    }
    fn iter_mut(&mut self) -> IterMut<Self::Elem> {
        IterMut {
            inner: NonNull::new(self.first_elem().load(Ordering::Relaxed)),
            _phantom: PhantomData,
        }
    }

    /// Clear the list. This takes mutable access because it invalidates every element pointer.
    fn clear(&mut self) {
        self.iter_mut().for_each(LinkedListElem::prep_drop);
        fence(Ordering::SeqCst);
        unsafe {
            let null = std::ptr::null_mut();
            self.last_elem().store(null, Ordering::Relaxed);
            let mut ptr = self.first_elem().swap(null, Ordering::Relaxed);
            while !ptr.is_null() {
                ptr = (*ptr).next_elem().load(Ordering::Relaxed);
            }
        }
    }
}
/// Trait for elements of an intrusive linked list with a pointer to the list.
/// ## Safety
/// Don't mess with the pointers if you don't know what you're doing.
pub unsafe trait LinkedListElem: Sized {
    type Parent: LinkedList<Elem = Self>;

    fn parent(&self) -> &AtomicPtr<Self::Parent>;
    fn prev_elem(&self) -> &AtomicPtr<Self>;
    fn next_elem(&self) -> &AtomicPtr<Self>;

    /// Call this before dropping.
    fn prep_drop(&mut self) {}

    /// Unlink self from a linked list. If we were previously in a linked list, take ownership with
    /// a `Box`
    fn unlink(&self) -> Option<Box<Self>> {
        let null = std::ptr::null_mut();
        let (prev, next) = (
            self.prev_elem().swap(null, Ordering::Acquire),
            self.next_elem().swap(null, Ordering::Acquire),
        );
        let p = self.parent().swap(std::ptr::null_mut(), Ordering::Acquire);
        unsafe {
            match (prev.as_ref(), next.as_ref()) {
                (None, None) => p.as_ref().map(|p| {
                    p.first_elem().store(null, Ordering::Release);
                    p.last_elem().store(null, Ordering::Release);
                    Box::from_raw(self as *const Self as *mut Self)
                }),
                (Some(pref), None) => {
                    pref.next_elem().store(null, Ordering::Release);
                    p.as_ref().map(|p| {
                        p.last_elem().store(prev, Ordering::Release);
                        Box::from_raw(self as *const Self as *mut Self)
                    })
                }
                (None, Some(nref)) => {
                    nref.prev_elem().store(null, Ordering::Release);
                    p.as_ref().map(|p| {
                        p.first_elem().store(next, Ordering::Release);
                        Box::from_raw(self as *const Self as *mut Self)
                    })
                }
                (Some(pref), Some(nref)) => {
                    pref.next_elem().store(next, Ordering::Release);
                    nref.prev_elem().store(prev, Ordering::Release);
                    (!p.is_null()).then(|| Box::from_raw(self as *const Self as *mut Self))
                }
            }
        }
    }
}
