use miette::SourceSpan;
use std::rc::Rc;
use std::sync::Arc;

pub trait Span: Copy + Into<SourceSpan> {
    fn merge(self, other: Self) -> Self;

    fn offset(self) -> usize;
    fn len(self) -> usize;
    fn is_empty(self) -> bool {
        self.len() == 0
    }
}

impl Span for miette::SourceSpan {
    fn merge(self, other: Self) -> Self {
        miette::SourceSpan::new(
            self.offset().into(),
            other.offset() + other.len() - self.offset(),
        )
    }
    fn offset(self) -> usize {
        miette::SourceSpan::offset(&self)
    }
    fn len(self) -> usize {
        miette::SourceSpan::len(&self)
    }
}

/// Something with a location
pub trait Located {
    type Span: Span;

    fn loc(&self) -> Self::Span;
}
impl<T: Located + ?Sized> Located for Box<T> {
    type Span = T::Span;

    fn loc(&self) -> Self::Span {
        T::loc(self)
    }
}
impl<T: Located + ?Sized> Located for Rc<T> {
    type Span = T::Span;

    fn loc(&self) -> Self::Span {
        T::loc(self)
    }
}
impl<T: Located + ?Sized> Located for Arc<T> {
    type Span = T::Span;

    fn loc(&self) -> Self::Span {
        T::loc(self)
    }
}
