use super::*;
use derivative::Derivative;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
pub struct ListIter<'b, T> {
    pub inner: Option<&'b T>,
    pub rev: bool,
}
impl<T> ListIter<'_, T> {
    /// Note that this is not the standard `DoubleEndedIterator` behavior! Calling this will change
    /// the direction but stay at the same element.
    pub fn rev_dir(&mut self) -> &mut Self {
        self.rev = !self.rev;
        self
    }
}
impl<'b, T: LinkedListElem<'b>> Iterator for ListIter<'b, T> {
    type Item = &'b T;

    fn next(&mut self) -> Option<Self::Item> {
        let r = self.inner?;
        self.inner = if self.rev {
            r.prev(Ordering::Relaxed)
        } else {
            r.next(Ordering::Relaxed)
        };
        Some(r)
    }
}

/// A linked list for elements that know their parent, meant to be the field 
#[derive(Derivative)]
#[derivative(Default(bound=""))]
pub struct LinkedList<'b, T> {
    first: AtomicRef<'b, T>,
    last: AtomicRef<'b, T>,
}
impl<'b, T> LinkedList<'b, T> {
    pub fn iter(&self) -> ListIter<'b, T> {
        ListIter {
            inner: self.first.load(Ordering::Relaxed),
            rev: false,
        }
    }
}
impl<'b, T: LinkedListElem<'b> + Debug> Debug for LinkedList<'b, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}
impl<'b, T: LinkedListElem<'b> + PartialEq> PartialEq for LinkedList<'b, T> {
    fn eq(&self, other: &Self) -> bool {
        self.iter().eq(other.iter())
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound=""), Default(bound=""))]
pub struct LinkedListLink<'b, S: LinkedListElem<'b>> {
    #[derivative(Debug(format_with="fmt_ref"))]
    parent: AtomicRef<'b, S::Parent>,
    #[derivative(Debug(format_with="fmt_ref"))]
    next: AtomicRef<'b, S>,
    #[derivative(Debug(format_with="fmt_ref"))]
    prev: AtomicRef<'b, S>,
}
impl<'b, S: LinkedListElem<'b>> LinkedListLink<'b, S> {
    /// A constant, unlinked link.
    pub const NEW: Self = Self {
        parent: AtomicRef::new(None),
        next: AtomicRef::new(None),
        prev: AtomicRef::new(None),
    };
}
impl<'b, S: LinkedListElem<'b>> PartialEq for LinkedListLink<'b, S> {
    fn eq(&self, _other: &Self) -> bool { true }
}
impl<'b, S: LinkedListElem<'b>> Eq for LinkedListLink<'b, S> {}
impl<'b, S: LinkedListElem<'b>> Hash for LinkedListLink<'b, S> {
    fn hash<H: Hasher>(&self, _hasher: &mut H) {}
}
impl<'b, S: LinkedListElem<'b>> LinkedListLink<'b, S> {
    /// Remove references to this, leaving outgoing refs.
    pub(self) fn unlink_partial(&self) {
        let (prev, next) = (self.prev.load(Ordering::Acquire), self.next.load(Ordering::Acquire));
        let parent = self.parent.load(Ordering::Acquire);
        if let Some(prev) = prev {
            prev.get_link().next.store(next, Ordering::Release);
        } else if let Some(p) = parent {
            p.get_list().first.store(None, Ordering::Release);
        }
        if let Some(next) = next {
            next.get_link().prev.store(prev, Ordering::Release);
        } else if let Some(p) = parent {
            p.get_list().first.store(None, Ordering::Release);
        }
    }
}
impl<'b, S: LinkedListElem<'b>> Clone for LinkedListLink<'b, S> {
    fn clone(&self) -> Self {
        Self::default()
    }
}

pub trait LinkedListParent<'a>: Sized {
    type Elem: LinkedListElem<'a, Parent = Self>;
    fn get_list(&'a self) -> &'a LinkedList<'a, Self::Elem>;

    fn first(&'a self, ord: Ordering) -> Option<&'a Self::Elem> {
        self.get_list().first.load(ord)
    }
    fn last(&'a self, ord: Ordering) -> Option<&'a Self::Elem> {
        self.get_list().last.load(ord)
    }
    fn iter(&'a self) -> ListIter<'a, Self::Elem> {
        ListIter {
            inner: self.first(Ordering::Relaxed),
            rev: false,
        }
    }
    fn rev_iter(&'a self) -> ListIter<'a, Self::Elem> {
        ListIter {
            inner: self.last(Ordering::Relaxed),
            rev: false,
        }
    }
    fn push_front(&'a self, elem: &'a Self::Elem) {
        let list = self.get_list();
        let _ = list.last.compare_exchange(None, Some(elem), Ordering::AcqRel, Ordering::Acquire);
        let old_first = list.first.swap(Some(elem), Ordering::AcqRel);
        if let Some(of) = old_first {
            of.get_link().prev.store(Some(elem), Ordering::Release);
        }
        let link = elem.get_link();
        link.unlink_partial();
        link.next.store(old_first, Ordering::Release);
        link.prev.store(None, Ordering::Release);
        link.parent.store(Some(self), Ordering::Release);
    }
    fn push_back(&'a self, elem: &'a Self::Elem) {
        let list = self.get_list();
        let _ = list.first.compare_exchange(None, Some(elem), Ordering::AcqRel, Ordering::Acquire);
        let old_last = list.last.swap(Some(elem), Ordering::AcqRel);
        if let Some(ol) = old_last {
            ol.get_link().next.store(Some(elem), Ordering::Release);
        }
        let link = elem.get_link();
        link.unlink_partial();
        link.prev.store(old_last, Ordering::Release);
        link.next.store(None, Ordering::Release);
        link.parent.store(Some(self), Ordering::Release);
    }
    fn pop_front(&'a self) -> Option<&'a Self::Elem> {
        let list = self.get_list();
        let old_first = list.first.load(Ordering::Acquire)?;
        let _ = list.last.compare_exchange(Some(old_first), None, Ordering::AcqRel, Ordering::Acquire);
        let link = old_first.get_link();
        let new_first = link.next.swap(None, Ordering::AcqRel);
        link.parent.store(None, Ordering::Relaxed);
        list.first.store(new_first, Ordering::Release);
        if let Some(nf) = new_first {
            nf.get_link().prev.store(None, Ordering::Release);
        }
        Some(old_first)
    }
    fn pop_back(&'a self) -> Option<&'a Self::Elem> {
        let list = self.get_list();
        let old_last = list.last.load(Ordering::Acquire)?;
        let _ = list.first.compare_exchange(Some(old_last), None, Ordering::AcqRel, Ordering::Acquire);
        let link = old_last.get_link();
        let new_last = link.prev.swap(None, Ordering::AcqRel);
        link.parent.store(None, Ordering::Relaxed);
        list.last.store(new_last, Ordering::Release);
        if let Some(nl) = new_last {
            nl.get_link().next.store(None, Ordering::Release);
        }
        Some(old_last)
    }
}
pub trait LinkedListElem<'a>: Sized {
    type Parent: LinkedListParent<'a, Elem = Self>;
    fn get_link(&'a self) -> &'a LinkedListLink<'a, Self>;
    fn next(&'a self, ord: Ordering) -> Option<&'a Self> {
        self.get_link().next.load(ord)
    }
    fn prev(&'a self, ord: Ordering) -> Option<&'a Self> {
        self.get_link().prev.load(ord)
    }
    fn unlink(&'a self) -> bool {
        let l = self.get_link();
        l.unlink_partial();
        l.prev.store(None, Ordering::Release);
        l.next.store(None, Ordering::Release);
        let op = l.parent.swap(None, Ordering::Release);
        op.is_some()
    }
}
