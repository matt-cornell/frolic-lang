#![allow(clippy::borrowed_box)]

use crate::common::*;
use frolic_utils::prelude::*;
use portable_atomic::{AtomicBool, AtomicPtr, AtomicUsize, Ordering};
use std::borrow::Cow;
use std::fmt::{self, Debug, Formatter};
use std::mem::ManuallyDrop;

mod disp;
pub mod error;
pub mod lower;

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
impl<S> Default for Module<'_, S> {
    fn default() -> Self {
        Self::new()
    }
}
impl<S> Drop for Module<'_, S> {
    fn drop(&mut self) {
        self.clear();
    }
}
unsafe impl<'src, S> LinkedList for Module<'src, S> {
    type Elem = Definition<'src, S>;

    fn first_elem(&self) -> &AtomicPtr<Self::Elem> {
        &self.first_def
    }
    fn last_elem(&self) -> &AtomicPtr<Self::Elem> {
        &self.last_def
    }
}

pub struct Definition<'src, S> {
    pub name: Option<DottedName<'src, S>>,
    parent: AtomicPtr<Module<'src, S>>,
    prev_def: AtomicPtr<Self>,
    next_def: AtomicPtr<Self>,
    first_blk: AtomicPtr<Block<'src, S>>,
    last_blk: AtomicPtr<Block<'src, S>>,
    refs: AtomicUsize,
}
impl<'src, S> Definition<'src, S> {
    pub fn new(name: Option<DottedName<'src, S>>) -> Self {
        Self {
            name,
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_def: AtomicPtr::new(std::ptr::null_mut()),
            next_def: AtomicPtr::new(std::ptr::null_mut()),
            first_blk: AtomicPtr::new(std::ptr::null_mut()),
            last_blk: AtomicPtr::new(std::ptr::null_mut()),
            refs: AtomicUsize::new(0),
        }
    }
    pub const fn anon() -> Self {
        Self {
            name: None,
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_def: AtomicPtr::new(std::ptr::null_mut()),
            next_def: AtomicPtr::new(std::ptr::null_mut()),
            first_blk: AtomicPtr::new(std::ptr::null_mut()),
            last_blk: AtomicPtr::new(std::ptr::null_mut()),
            refs: AtomicUsize::new(0),
        }
    }

    pub fn append_block(self: &Box<Self>, mut blk: Box<Block<'src, S>>) -> Owned<Block<'src, S>> {
        let raw = blk.as_mut() as *mut _;
        self.append(blk);
        unsafe { ManuallyDrop::new(Box::from_raw(raw)) }
    }
    pub fn append_new_block(self: &Box<Self>) -> Owned<Block<'src, S>> {
        self.append_block(Box::new(Block::new()))
    }
}
impl<S> Drop for Definition<'_, S> {
    fn drop(&mut self) {
        let refs = *self.refs.get_mut();
        assert_eq!(refs, 0, "dropped definition ({self:p}) with refs!");
        self.clear();
    }
}
unsafe impl<'src, S> LinkedList for Definition<'src, S> {
    type Elem = Block<'src, S>;

    fn first_elem(&self) -> &AtomicPtr<Self::Elem> {
        &self.first_blk
    }
    fn last_elem(&self) -> &AtomicPtr<Self::Elem> {
        &self.last_blk
    }
}
unsafe impl<'src, S> LinkedListElem for Definition<'src, S> {
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
    fn prep_drop(&self) {
        self.iter().for_each(LinkedListElem::prep_drop);
    }
}

pub struct Block<'src, S> {
    parent: AtomicPtr<Definition<'src, S>>,
    prev_blk: AtomicPtr<Self>,
    next_blk: AtomicPtr<Self>,
    first_val: AtomicPtr<Value<'src, S>>,
    last_val: AtomicPtr<Value<'src, S>>,
    refs: AtomicUsize,
}
impl<'src, S> Block<'src, S> {
    pub const fn new() -> Self {
        Self {
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_blk: AtomicPtr::new(std::ptr::null_mut()),
            next_blk: AtomicPtr::new(std::ptr::null_mut()),
            first_val: AtomicPtr::new(std::ptr::null_mut()),
            last_val: AtomicPtr::new(std::ptr::null_mut()),
            refs: AtomicUsize::new(0),
        }
    }
    pub fn parent(&self) -> Option<Owned<Definition<'src, S>>> {
        unsafe {
            let ptr = self.parent.load(Ordering::Relaxed);
            (!ptr.is_null()).then(|| ManuallyDrop::new(Box::from_raw(ptr)))
        }
    }
    pub fn module(&self) -> Option<Owned<Module<'src, S>>> {
        unsafe {
            let ptr = self.parent
                .load(Ordering::Relaxed)
                .as_ref()?
                .parent
                .load(Ordering::Relaxed);
            (!ptr.is_null()).then(|| ManuallyDrop::new(Box::from_raw(ptr)))
        }
    }
}
impl<S> Drop for Block<'_, S> {
    fn drop(&mut self) {
        let refs = *self.refs.get_mut();
        assert_eq!(refs, 0, "dropped block ({self:p}) with refs!");
        self.clear();
    }
}
unsafe impl<'src, S> LinkedList for Block<'src, S> {
    type Elem = Value<'src, S>;

    fn first_elem(&self) -> &AtomicPtr<Self::Elem> {
        &self.first_val
    }
    fn last_elem(&self) -> &AtomicPtr<Self::Elem> {
        &self.last_val
    }
}
unsafe impl<'src, S> LinkedListElem for Block<'src, S> {
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
    fn prep_drop(&self) {
        self.iter().for_each(LinkedListElem::prep_drop);
    }
}

/// The kinds of data a value can hold.
enum ValueInner<'src, S> {
    Null,
    Error,
    Int(i128),
    Float(f64),
    String(Cow<'src, [u8]>),
    Comment(Cow<'src, [u8]>),
    NewLoc(*const Value<'src, S>),
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
        pred: *const Block<'src, S>,
        value: *const Value<'src, S>,
        default: *const Value<'src, S>,
    },
}

/// A value in the HIR. Acts as an intrusive list element, is reference-counted for safety.
pub struct Value<'src, S> {
    pub name: Box<str>,
    pub span: S,
    inner: ValueInner<'src, S>,
    parent: AtomicPtr<Block<'src, S>>,
    prev_val: AtomicPtr<Self>,
    next_val: AtomicPtr<Self>,
    refs: AtomicUsize,
    /// If this valus is dropped, its refs have been decremented, and any pointers through the
    /// inner value may be invalid.
    dropped: AtomicBool,
}
impl<'src, S> Value<'src, S> {
    pub fn null(span: S, name: impl Into<Box<str>>) -> Self {
        Self {
            name: name.into(),
            inner: ValueInner::Null,
            span,
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_val: AtomicPtr::new(std::ptr::null_mut()),
            next_val: AtomicPtr::new(std::ptr::null_mut()),
            refs: AtomicUsize::new(0),
            dropped: AtomicBool::new(false),
        }
    }
    pub fn error(span: S, name: impl Into<Box<str>>) -> Self {
        Self {
            name: name.into(),
            inner: ValueInner::Error,
            span,
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_val: AtomicPtr::new(std::ptr::null_mut()),
            next_val: AtomicPtr::new(std::ptr::null_mut()),
            refs: AtomicUsize::new(0),
            dropped: AtomicBool::new(false),
        }
    }
    pub fn int(val: i128, span: S, name: impl Into<Box<str>>) -> Self {
        Self {
            name: name.into(),
            inner: ValueInner::Int(val),
            span,
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_val: AtomicPtr::new(std::ptr::null_mut()),
            next_val: AtomicPtr::new(std::ptr::null_mut()),
            refs: AtomicUsize::new(0),
            dropped: AtomicBool::new(false),
        }
    }
    pub fn float(val: f64, span: S, name: impl Into<Box<str>>) -> Self {
        Self {
            name: name.into(),
            inner: ValueInner::Float(val),
            span,
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_val: AtomicPtr::new(std::ptr::null_mut()),
            next_val: AtomicPtr::new(std::ptr::null_mut()),
            refs: AtomicUsize::new(0),
            dropped: AtomicBool::new(false),
        }
    }
    pub fn string(val: Cow<'src, [u8]>, span: S, name: impl Into<Box<str>>) -> Self {
        Self {
            name: name.into(),
            inner: ValueInner::String(val),
            span,
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_val: AtomicPtr::new(std::ptr::null_mut()),
            next_val: AtomicPtr::new(std::ptr::null_mut()),
            refs: AtomicUsize::new(0),
            dropped: AtomicBool::new(false),
        }
    }
    pub fn comment(val: Cow<'src, [u8]>, span: S, name: impl Into<Box<str>>) -> Self {
        Self {
            name: name.into(),
            inner: ValueInner::Comment(val),
            span,
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_val: AtomicPtr::new(std::ptr::null_mut()),
            next_val: AtomicPtr::new(std::ptr::null_mut()),
            refs: AtomicUsize::new(0),
            dropped: AtomicBool::new(false),
        }
    }
    pub fn new_loc(val: &Box<Value<'src, S>>, span: S, name: impl Into<Box<str>>) -> Self {
        val.refs.fetch_add(1, Ordering::Relaxed);
        Self {
            name: name.into(),
            inner: ValueInner::NewLoc(&**val as *const _),
            span,
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_val: AtomicPtr::new(std::ptr::null_mut()),
            next_val: AtomicPtr::new(std::ptr::null_mut()),
            refs: AtomicUsize::new(0),
            dropped: AtomicBool::new(false),
        }
    }
    pub fn call(
        func: &Box<Value<'src, S>>,
        arg: &Box<Value<'src, S>>,
        span: S,
        name: impl Into<Box<str>>,
    ) -> Self {
        func.refs.fetch_add(1, Ordering::Relaxed);
        arg.refs.fetch_add(1, Ordering::Relaxed);
        Self {
            name: name.into(),
            inner: ValueInner::Call(&**func as *const _, &**arg as *const _),
            span,
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_val: AtomicPtr::new(std::ptr::null_mut()),
            next_val: AtomicPtr::new(std::ptr::null_mut()),
            refs: AtomicUsize::new(0),
            dropped: AtomicBool::new(false),
        }
    }
    pub fn function(val: &Box<Definition<'src, S>>, span: S, name: impl Into<Box<str>>) -> Self {
        val.refs.fetch_add(1, Ordering::Relaxed);
        Self {
            name: name.into(),
            inner: ValueInner::Function(&**val as *const _),
            span,
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_val: AtomicPtr::new(std::ptr::null_mut()),
            next_val: AtomicPtr::new(std::ptr::null_mut()),
            refs: AtomicUsize::new(0),
            dropped: AtomicBool::new(false),
        }
    }
    pub fn func_arg(func: &Box<Definition<'src, S>>, span: S, name: impl Into<Box<str>>) -> Self {
        func.refs.fetch_add(1, Ordering::Relaxed);
        Self {
            name: name.into(),
            inner: ValueInner::FunctionArg(&**func as *const _),
            span,
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_val: AtomicPtr::new(std::ptr::null_mut()),
            next_val: AtomicPtr::new(std::ptr::null_mut()),
            refs: AtomicUsize::new(0),
            dropped: AtomicBool::new(false),
        }
    }
    pub fn cond_br(
        cond: &Box<Value<'src, S>>,
        if_true: &Box<Block<'src, S>>,
        if_false: &Box<Block<'src, S>>,
        span: S,
        name: impl Into<Box<str>>,
    ) -> Self {
        cond.refs.fetch_add(1, Ordering::Relaxed);
        if_true.refs.fetch_add(1, Ordering::Relaxed);
        if_false.refs.fetch_add(1, Ordering::Relaxed);
        Self {
            name: name.into(),
            inner: ValueInner::CondBr {
                cond: &**cond as *const _,
                if_true: &**if_true as *const _,
                if_false: &**if_false as *const _,
            },
            span,
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_val: AtomicPtr::new(std::ptr::null_mut()),
            next_val: AtomicPtr::new(std::ptr::null_mut()),
            refs: AtomicUsize::new(0),
            dropped: AtomicBool::new(false),
        }
    }
    pub fn uncond_br(next: &Box<Block<'src, S>>, span: S, name: impl Into<Box<str>>) -> Self {
        next.refs.fetch_add(1, Ordering::Relaxed);
        Self {
            name: name.into(),
            inner: ValueInner::UncondBr(&**next as *const _),
            span,
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_val: AtomicPtr::new(std::ptr::null_mut()),
            next_val: AtomicPtr::new(std::ptr::null_mut()),
            refs: AtomicUsize::new(0),
            dropped: AtomicBool::new(false),
        }
    }
    pub fn phi(
        pred: &Box<Block<'src, S>>,
        value: &Box<Value<'src, S>>,
        default: &Box<Value<'src, S>>,
        span: S,
        name: impl Into<Box<str>>,
    ) -> Self {
        pred.refs.fetch_add(1, Ordering::Relaxed);
        value.refs.fetch_add(1, Ordering::Relaxed);
        default.refs.fetch_add(1, Ordering::Relaxed);
        Self {
            name: name.into(),
            inner: ValueInner::Phi {
                pred: &**pred as *const _,
                value: &**value as *const _,
                default: &**default as *const _,
            },
            span,
            parent: AtomicPtr::new(std::ptr::null_mut()),
            prev_val: AtomicPtr::new(std::ptr::null_mut()),
            next_val: AtomicPtr::new(std::ptr::null_mut()),
            refs: AtomicUsize::new(0),
            dropped: AtomicBool::new(false),
        }
    }

    pub fn parent(&self) -> Option<&Block<'src, S>> {
        unsafe { self.parent.load(Ordering::Relaxed).as_ref() }
    }
    pub fn definition(&self) -> Option<&Definition<'src, S>> {
        unsafe {
            self.parent
                .load(Ordering::Relaxed)
                .as_ref()?
                .parent
                .load(Ordering::Relaxed)
                .as_ref()
        }
    }
    pub fn module(&self) -> Option<&Module<'src, S>> {
        unsafe {
            self.parent
                .load(Ordering::Relaxed)
                .as_ref()?
                .parent
                .load(Ordering::Relaxed)
                .as_ref()?
                .parent
                .load(Ordering::Relaxed)
                .as_ref()
        }
    }
    /// Decrement the reference counts of all structs that this refers to. This also marks the
    /// value as poisoned, and it can't be used after this.
    pub fn prep_drop(&self) {
        use ValueInner::*;
        if self.dropped.swap(true, Ordering::AcqRel) {
            return;
        }
        unsafe {
            match self.inner {
                NewLoc(val) => {
                    (*val).refs.fetch_sub(1, Ordering::Relaxed);
                }
                Call(func, arg) => {
                    (*func).refs.fetch_sub(1, Ordering::Relaxed);
                    (*arg).refs.fetch_sub(1, Ordering::Relaxed);
                }
                Function(func) | FunctionArg(func) => {
                    (*func).refs.fetch_sub(1, Ordering::Relaxed);
                }
                CondBr {
                    cond,
                    if_true,
                    if_false,
                } => {
                    (*cond).refs.fetch_sub(1, Ordering::Relaxed);
                    (*if_true).refs.fetch_sub(1, Ordering::Relaxed);
                    (*if_false).refs.fetch_sub(1, Ordering::Relaxed);
                }
                UncondBr(next) => {
                    (*next).refs.fetch_sub(1, Ordering::Relaxed);
                }
                Phi {
                    pred,
                    value,
                    default,
                } => {
                    (*pred).refs.fetch_sub(1, Ordering::Relaxed);
                    (*value).refs.fetch_sub(1, Ordering::Relaxed);
                    (*default).refs.fetch_sub(1, Ordering::Relaxed);
                }
                _ => {}
            }
        }
    }
}
unsafe impl<'src, S> LinkedListElem for Value<'src, S> {
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
    fn prep_drop(&self) {
        self.prep_drop();
    }
}
impl<S> Drop for Value<'_, S> {
    fn drop(&mut self) {
        self.prep_drop();
        let refs = *self.refs.get_mut();
        assert_eq!(refs, 0, "dropped value ({self:p}) with refs!");
    }
}

pub struct Builder<'src, S> {
    pos: AtomicPtr<Block<'src, S>>,
}
impl<'src, S> Builder<'src, S> {
    pub const fn new() -> Self {
        Self {
            pos: AtomicPtr::new(std::ptr::null_mut()),
        }
    }
    pub fn position_at(&self, blk: &Box<Block<'src, S>>) {
        blk.refs.fetch_add(1, Ordering::AcqRel);
        let ptr = self.pos.swap(
            &**blk as *const Block<'src, S> as *mut Block<'src, S>,
            Ordering::AcqRel,
        );
        if let Some(old) = unsafe { ptr.as_ref() } {
            old.refs.fetch_sub(1, Ordering::AcqRel);
        }
    }
    pub fn clear_pos(&self) {
        let ptr = self.pos.swap(std::ptr::null_mut(), Ordering::AcqRel);
        if let Some(old) = unsafe { ptr.as_ref() } {
            old.refs.fetch_sub(1, Ordering::AcqRel);
        }
    }
    /// Get the position, use a `ManuallyDrop<Box<...>>` to show that this value is on heap.
    pub fn get_pos(&self) -> Option<Owned<Block<'src, S>>> {
        let ptr = self.pos.load(Ordering::Relaxed);
        (!ptr.is_null()).then(|| unsafe { ManuallyDrop::new(Box::from_raw(ptr)) })
    }
    pub fn get_func(&self) -> Option<Owned<Definition<'src, S>>> {
        self.get_pos().and_then(|b| b.parent())
    }
    pub fn append(&self, val: Box<Value<'src, S>>) -> Owned<Value<'src, S>> {
        self.get_pos()
            .expect("Cannot append with a builder with unset position!")
            .append(val)
    }
}
impl<S> Default for Builder<'_, S> {
    fn default() -> Self {
        Self::new()
    }
}
impl<S> Debug for Builder<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Builder")
            .field("pos", &self.pos.load(Ordering::Relaxed))
            .finish()
    }
}
impl<S> Drop for Builder<'_, S> {
    fn drop(&mut self) {
        if let Some(ptr) = unsafe { self.pos.get_mut().as_ref() } {
            ptr.refs.fetch_sub(1, Ordering::Release);
        }
    }
}
impl<S> Clone for Builder<'_, S> {
    fn clone(&self) -> Self {
        Self {
            pos: AtomicPtr::new(self.pos.load(Ordering::Relaxed)),
        }
    }
}
