/// Report an error, somehow.
pub trait ErrorReporter<E> {
    fn report(&mut self, err: E);
}

impl<E, T: ErrorReporter<E>> ErrorReporter<E> for &mut T {
    fn report(&mut self, err: E) {
        T::report(self, err)
    }
}
impl<'a, E, T> ErrorReporter<E> for &&'a T
where
    &'a T: ErrorReporter<E>,
{
    fn report(&mut self, err: E) {
        let mut this: &'a T = self;
        this.report(err)
    }
}

impl<T, E: Into<T>> ErrorReporter<E> for Vec<T> {
    fn report(&mut self, err: E) {
        self.push(err.into());
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CallbackReporter<F>(pub F);
impl<E, F: FnMut(E)> ErrorReporter<E> for CallbackReporter<F> {
    fn report(&mut self, err: E) {
        (self.0)(err)
    }
}
impl<E, F: Fn(E)> ErrorReporter<E> for &CallbackReporter<F> {
    fn report(&mut self, err: E) {
        (self.0)(err)
    }
}
