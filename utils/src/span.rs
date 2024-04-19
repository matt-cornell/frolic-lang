use miette::SourceSpan;
use std::rc::Rc;
use std::sync::Arc;
use std::fmt::Debug;

pub trait Span: Copy + Debug + Into<SourceSpan> + 'static {
    fn merge(self, other: Self) -> Self;

    fn offset(self) -> usize;
    fn len(self) -> usize;
    fn is_empty(self) -> bool {
        self.len() == 0
    }
}

pub trait SpanConstruct: Span {
    fn new(offset: usize, len: usize) -> Self;
    fn range(start: usize, end: usize) -> Self {
        Self::new(start, end - start)
    }
    fn loc(offset: usize) -> Self {
        Self::new(offset, 0)
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
impl SpanConstruct for miette::SourceSpan {
    fn new(offset: usize, len: usize) -> Self {
        (offset, len).into()
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
