use std::rc::Rc;
use std::sync::{Arc, Mutex, OnceLock};

/// Report an error, somehow.
pub trait ErrorReporter<E> {
    /// Handle the error. Return true if we should abort.
    /// If the reporter aborts, the state of the output is not guaranteed, but should be at least
    /// somewhat usable.
    #[must_use]
    fn report(&mut self, err: E) -> bool;
}

impl<E, T: ErrorReporter<E>> ErrorReporter<E> for &mut T {
    fn report(&mut self, err: E) -> bool {
        T::report(self, err)
    }
}
impl<'a, E, T> ErrorReporter<E> for &&'a T
where
    &'a T: ErrorReporter<E>,
{
    fn report(&mut self, err: E) -> bool {
        let mut this: &'a T = self;
        this.report(err)
    }
}

impl<T, E: Into<T>> ErrorReporter<E> for Vec<T> {
    fn report(&mut self, err: E) -> bool {
        self.push(err.into());
        false
    }
}

impl<E, T: ErrorReporter<E>> ErrorReporter<E> for Mutex<T> {
    fn report(&mut self, err: E) -> bool {
        self.get_mut().map_or(true, |r| r.report(err))
    }
}
impl<E, T: ErrorReporter<E>> ErrorReporter<E> for &Mutex<T> {
    #[allow(clippy::mut_mutex_lock)]
    fn report(&mut self, err: E) -> bool {
        self.lock().map_or(true, |mut r| r.report(err))
    }
}
impl<E, T> ErrorReporter<E> for Rc<T>
where
    for<'a> &'a T: ErrorReporter<E>,
{
    fn report(&mut self, err: E) -> bool {
        let mut this: &T = self;
        this.report(err)
    }
}
impl<E, T> ErrorReporter<E> for &Rc<T>
where
    for<'a> &'a T: ErrorReporter<E>,
{
    fn report(&mut self, err: E) -> bool {
        let mut this: &T = self;
        this.report(err)
    }
}
impl<E, T> ErrorReporter<E> for Arc<T>
where
    for<'a> &'a T: ErrorReporter<E>,
{
    fn report(&mut self, err: E) -> bool {
        let mut this: &T = self;
        this.report(err)
    }
}
impl<E, T> ErrorReporter<E> for &Arc<T>
where
    for<'a> &'a T: ErrorReporter<E>,
{
    fn report(&mut self, err: E) -> bool {
        let mut this: &T = self;
        this.report(err)
    }
}

/// Call a given callback on error.
#[derive(Debug, Clone, Copy)]
pub struct CallbackReporter<F>(pub F);
impl<E, F: FnMut(E) -> bool> ErrorReporter<E> for CallbackReporter<F> {
    fn report(&mut self, err: E) -> bool {
        (self.0)(err)
    }
}
impl<E, F: Fn(E) -> bool> ErrorReporter<E> for &CallbackReporter<F> {
    fn report(&mut self, err: E) -> bool {
        (self.0)(err)
    }
}

/// Get the first error, exit immediately.
#[derive(Debug, Clone)]
pub struct OnceReporter<E> {
    lock: OnceLock<E>,
}
impl<E> OnceReporter<E> {
    pub const fn new() -> Self {
        Self {
            lock: OnceLock::new(),
        }
    }
    pub fn failed(&self) -> bool {
        self.lock.get().is_some()
    }
    pub fn as_result(self) -> Result<(), E> {
        if let Some(err) = self.lock.into_inner() {
            Err(err)
        } else {
            Ok(())
        }
    }
}
impl<T, E: Into<T>> ErrorReporter<E> for OnceReporter<T> {
    fn report(&mut self, err: E) -> bool {
        let _ = self.lock.set(err.into());
        true
    }
}
impl<T, E: Into<T>> ErrorReporter<E> for &OnceReporter<T> {
    fn report(&mut self, err: E) -> bool {
        let _ = self.lock.set(err.into());
        true
    }
}

/// Always terminate on first error
#[derive(Debug, Clone, Copy)]
pub struct FusedReporter<E>(pub E);
impl<E, T: ErrorReporter<E>> ErrorReporter<E> for FusedReporter<T> {
    fn report(&mut self, err: E) -> bool {
        let _ = self.0.report(err);
        true
    }
}
impl<'a, E, T> ErrorReporter<E> for &'a FusedReporter<T> where &'a T: ErrorReporter<E> {
    fn report(&mut self, err: E) -> bool {
        let mut this: &'a T = &self.0;
        let _ = this.report(err);
        true
    }
}
