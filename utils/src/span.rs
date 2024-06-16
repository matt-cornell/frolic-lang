use miette::SourceSpan;
use std::fmt::{self, Debug, Formatter};
use std::rc::Rc;
use std::sync::Arc;

/// Span-like ZST that panics if you try to access anything in it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DummySpan;
impl From<DummySpan> for miette::SourceSpan {
    fn from(_value: DummySpan) -> Self {
        panic!("attempted to use a DummySpan")
    }
}
impl Span for DummySpan {
    fn merge(self, _other: Self) -> Self {
        self
    }
    fn offset(self) -> usize {
        panic!("attempted to use a DummySpan")
    }
    fn len(self) -> usize {
        panic!("attempted to use a DummySpan")
    }
}
impl SpanConstruct for DummySpan {
    fn new(_offset: usize, _len: usize) -> Self {
        DummySpan
    }
}

/// Like a `SourceSpan`, but with a nicer `Debug`
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct PrettySpan {
    pub offset: usize,
    pub len: usize,
}
impl Debug for PrettySpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.offset, self.offset + self.len)
    }
}
impl From<PrettySpan> for miette::SourceSpan {
    fn from(value: PrettySpan) -> Self {
        (value.offset, value.len).into()
    }
}
impl Span for PrettySpan {
    fn merge(self, other: Self) -> Self {
        let start = std::cmp::min(self.offset, other.offset);
        let end = std::cmp::max(self.offset + self.len, other.offset + other.len);
        Self {
            offset: start,
            len: end - start,
        }
    }

    fn offset(self) -> usize {
        self.offset
    }
    fn len(self) -> usize {
        self.len
    }
}
impl SpanConstruct for PrettySpan {
    fn new(offset: usize, len: usize) -> Self {
        Self { offset, len }
    }
}

/// General implementation of a span trait. Spans must be able to be merged, and they have an
/// offset and length (in bytes)
pub trait Span: Copy + Debug + Into<SourceSpan> + 'static {
    fn merge(self, other: Self) -> Self;

    fn offset(self) -> usize;
    fn len(self) -> usize;
    fn end(self) -> usize {
        self.offset() + self.len()
    }
    fn is_empty(self) -> bool {
        self.len() == 0
    }
}

/// A span that can be constructed from byte indices.
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
        let start = std::cmp::min(self.offset(), other.offset());
        let end = std::cmp::max(self.offset() + self.len(), other.offset() + other.len());
        miette::SourceSpan::new(start.into(), end - start)
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
#[impl_tools::autoimpl(for<T: trait + ?Sized> &T, Box<T>, Rc<T>, Arc<T>)]
pub trait Located {
    type Span: Span;

    fn loc(&self) -> Self::Span;
}
