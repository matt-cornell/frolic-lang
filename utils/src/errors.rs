use miette::{Diagnostic, SourceCode};
use std::fmt::{self, Debug, Display, Formatter};
use std::rc::Rc;
use std::sync::{Arc, Mutex, OnceLock};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourcedError<F, E> {
    pub file: F,
    pub error: E,
}
impl<F, E: Display> Display for SourcedError<F, E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.error, f)
    }
}
impl<F: Debug, E: std::error::Error> std::error::Error for SourcedError<F, E> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.error.source()
    }
}
impl<F: Debug + SourceCode, E: Diagnostic> Diagnostic for SourcedError<F, E> {
    fn source_code(&self) -> Option<&dyn SourceCode> {
        Some(&self.file)
    }
    fn url<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        self.error.url()
    }
    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        self.error.code()
    }
    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        self.error.help()
    }
    fn severity(&self) -> Option<miette::Severity> {
        self.error.severity()
    }
    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        self.error.labels()
    }
    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        self.error.related()
    }
    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.error.diagnostic_source()
    }
}

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
impl<'a, E, T> ErrorReporter<E> for &'a FusedReporter<T>
where
    &'a T: ErrorReporter<E>,
{
    fn report(&mut self, err: E) -> bool {
        let mut this: &'a T = &self.0;
        let _ = this.report(err);
        true
    }
}

#[derive(Debug)]
pub struct DiagnosticPrint<W, H> {
    pub writer: W,
    pub handler: H,
    pub error: Option<std::io::Error>,
}
impl<H> DiagnosticPrint<std::io::Stderr, H> {
    pub fn stderr(handler: H) -> Self {
        Self::new(std::io::stderr(), handler)
    }
}
impl<W, H> DiagnosticPrint<W, H> {
    pub const fn new(writer: W, handler: H) -> Self {
        Self {writer, handler, error: None}
    }
    pub fn take_result(&mut self) -> Result<(), std::io::Error> {
        if let Some(err) = self.error.take() {
            Err(err)
        } else {
            Ok(())
        }
    }
    pub fn into_result(self) -> Result<(), std::io::Error> {
        if let Some(err) = self.error {
            Err(err)
        } else {
            Ok(())
        }
    }
}
impl<W: std::io::Write, H: miette::ReportHandler, E: Diagnostic> ErrorReporter<E> for DiagnosticPrint<W, H> {
    fn report(&mut self, err: E) -> bool {
        struct Helper<'a, H, E>(&'a H, &'a E);
        impl<H: miette::ReportHandler, E: Diagnostic> Display for Helper<'_, H, E> {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                self.0.debug(self.1, f)
            }
        }
        let res = write!(self.writer, "{}", Helper(&self.handler, &err));
        if let Err(e) = res {
            self.error = Some(e);
            true
        } else {
            false
        }
    }
}
