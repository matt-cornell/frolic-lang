use std::sync::Arc;
use arc_swap::ArcSwapOption;
use frolic_utils::prelude::*;
use portable_atomic::{AtomicPtr, Ordering};
use crate::common::*;
use std::borrow::Cow;

pub struct Module<'src, S> {
    first_def: AtomicPtr<Definition<'src, S>>,
    last_def: AtomicPtr<Definition<'src, S>>,
}
impl<'src, S> Module<'src, S> {
    /// Construct a new `Module`. Note that its functionality will be limited if it's not in a `Box`.
    pub const fn new() -> Self {
        Self {
            first_def: AtomicPtr::new(std::ptr::null_mut()),
            last_def: AtomicPtr::new(std::ptr::null_mut()),
        }
    }

    /// Convenience method to construct a new `Box` directly.
    pub fn new_box() -> Box<Self> {
        Box::new(Self::new())
    }
}
impl<S> Drop for Module<'_, S> {
    fn drop(&mut self) {
        self.clear();
    }
}
impl<'src, S> LinkedList for Module<'src, S> {
    type Elem = Definition<'src, S>;

    fn first_elem(&self) -> &AtomicPtr<Self::Elem> {
        &self.first_def
    }
    fn last_elem(&self) -> &AtomicPtr<Self::Elem> {
        &self.last_def
    }
}

pub struct Definition<'src, S> {
    name: ArcSwapOption<DottedName<'src, S>>,
    parent: AtomicPtr<Module<'src, S>>,
    prev_def: AtomicPtr<Self>,
    next_def: AtomicPtr<Self>,
    first_blk: AtomicPtr<Block<'src, S>>,
    last_blk: AtomicPtr<Block<'src, S>>,
}
impl<'src, S> Definition<'src, S> {
    pub fn new(name: Option<Arc<DottedName<'src, S>>>) -> Self {
        Self {
            name: ArcSwapOption::new(name),
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_def: AtomicPtr::new(std::ptr::null_mut()),
            next_def: AtomicPtr::new(std::ptr::null_mut()),
            first_blk: AtomicPtr::new(std::ptr::null_mut()),
            last_blk: AtomicPtr::new(std::ptr::null_mut()),
        }
    }
    pub const fn anon() -> Self {
        Self {
            name: ArcSwapOption::const_empty(),
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_def: AtomicPtr::new(std::ptr::null_mut()),
            next_def: AtomicPtr::new(std::ptr::null_mut()),
            first_blk: AtomicPtr::new(std::ptr::null_mut()),
            last_blk: AtomicPtr::new(std::ptr::null_mut()),
        }
    }

    pub fn get_name(&self) -> Option<Arc<DottedName<'src, S>>> {
        self.name.load_full()
    }
    pub fn set_name(&self, name: impl Into<Arc<DottedName<'src, S>>>) {
        self.name.store(Some(name.into()));
    }
}
impl<S> Drop for Definition<'_, S> {
    fn drop(&mut self) {
        self.clear();
    }
}
impl<'src, S> LinkedList for Definition<'src, S> {
    type Elem = Block<'src, S>;

    fn first_elem(&self) -> &AtomicPtr<Self::Elem> {
        &self.first_blk
    }
    fn last_elem(&self) -> &AtomicPtr<Self::Elem> {
        &self.last_blk
    }
}
impl<'src, S> LinkedListElem for Definition<'src, S> {
    type Parent = Module<'src, S>;

    fn parent(&self) -> &AtomicPtr<Self::Parent> {
        &self.parent
    }
    fn prev_elem(&self) -> &AtomicPtr<Self> {
        &self.prev_def
    }
    fn next_elem(&self) -> &AtomicPtr<Self> {
        &self.next_def
    }
}

pub struct Block<'src, S> {
    parent: AtomicPtr<Definition<'src, S>>,
    prev_blk: AtomicPtr<Self>,
    next_blk: AtomicPtr<Self>,
    first_val: AtomicPtr<Value<'src, S>>,
    last_val: AtomicPtr<Value<'src, S>>,
}
impl<'src, S> Block<'src, S> {
    pub fn parent(&self) -> Option<&Definition<'src, S>> {
        unsafe {
            self.parent.load(Ordering::Relaxed).as_ref()
        }
    }
    pub fn module(&self) -> Option<&Module<'src, S>> {
        unsafe {
            self.parent.load(Ordering::Relaxed).as_ref()?.parent.load(Ordering::Relaxed).as_ref()
        }
    }
}
impl<S> Drop for Block<'_, S> {
    fn drop(&mut self) {
        self.clear();
    }
}
impl<'src, S> LinkedList for Block<'src, S> {
    type Elem = Value<'src, S>;

    fn first_elem(&self) -> &AtomicPtr<Self::Elem> {
        &self.first_val
    }
    fn last_elem(&self) -> &AtomicPtr<Self::Elem> {
        &self.last_val
    }
}
impl<'src, S> LinkedListElem for Block<'src, S> {
    type Parent = Definition<'src, S>;

    fn parent(&self) -> &AtomicPtr<Self::Parent> {
        &self.parent
    }
    fn prev_elem(&self) -> &AtomicPtr<Self> {
        &self.prev_blk
    }
    fn next_elem(&self) -> &AtomicPtr<Self> {
        &self.next_blk
    }
}

pub enum ValueInner<'src, S> {
    Int(i128),
    Float(f64),
    String(Cow<'src, [u8]>),
    /// First param, second param
    Call(*const Value<'src, S>, *const Value<'src, S>),
    /// Wrapper around a global value
    Function(*const Definition<'src, S>),
    /// Argument for a function
    FunctionArg(*const Definition<'src, S>),
    CondBr {
        cond: *const Value<'src, S>,
        if_true: *const Block<'src, S>,
        if_false: *const Block<'src, S>,
    },
    UncondBr(*const Block<'src, S>),
    Phi {
        block: *const Block<'src, S>,
        value: *const Value<'src, S>,
        default: *const Value<'src, S>,
    },
}

pub struct Value<'src, S> {
    parent: AtomicPtr<Block<'src, S>>,
    prev_val: AtomicPtr<Self>,
    next_val: AtomicPtr<Self>,
    pub span: S,
    pub inner: ValueInner<'src, S>,
}
impl<'src, S> Value<'src, S> {
    pub fn parent(&self) -> Option<&Block<'src, S>> {
        unsafe {
            self.parent.load(Ordering::Relaxed).as_ref()
        }
    }
    pub fn definition(&self) -> Option<&Definition<'src, S>> {
        unsafe {
            self.parent.load(Ordering::Relaxed).as_ref()?.parent.load(Ordering::Relaxed).as_ref()
        }
    }
    pub fn module(&self) -> Option<&Module<'src, S>> {
        unsafe {
            self.parent.load(Ordering::Relaxed).as_ref()?.parent.load(Ordering::Relaxed).as_ref()?.parent.load(Ordering::Relaxed).as_ref()
        }
    }
}
impl<'src, S> LinkedListElem for Value<'src, S> {
    type Parent = Block<'src, S>;

    fn parent(&self) -> &AtomicPtr<Self::Parent> {
        &self.parent
    }
    fn prev_elem(&self) -> &AtomicPtr<Self> {
        &self.prev_val
    }
    fn next_elem(&self) -> &AtomicPtr<Self> {
        &self.next_val
    }
}
