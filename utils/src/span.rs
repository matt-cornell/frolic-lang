pub trait Span: Copy {
    fn merge(self, other: Self) -> Self;

    fn offset(self) -> usize;
    fn len(self) -> usize;
    fn is_empty(self) -> bool {self.len() == 0}

    fn file(&self) -> Option<&dyn miette::SourceCode> {None}
}

pub trait SpanConstruct<C>: Span {

    fn new(offset: usize, len: usize, ctx: C) -> Self;
}

impl Span for miette::SourceSpan {
    fn merge(self, other: Self) -> Self {
        miette::SourceSpan::new(self.offset().into(), other.offset() + other.len() - self.offset())
    }
    fn offset(self) -> usize {
        miette::SourceSpan::offset(&self)
    }
    fn len(self) -> usize {
        miette::SourceSpan::len(&self)
    }
}
impl SpanConstruct<()> for miette::SourceSpan {

    fn new(offset: usize, len: usize, _ctx: ()) -> Self {
        (offset, len).into()
    }
}

impl<F: FileLike, S: Span> Span for (F, S) {
    fn merge(self, other: Self) -> Self {
        (self.0, self.1.merge(other.1))
    }
    fn offset(self) -> usize {
        self.1.offset()
    }
    fn len(self) -> usize {
        self.1.len()
    }
    fn file(&self) -> Option<&dyn miette::SourceCode> {
        self.0.contents()
    }
}
impl<F: FileLike, S: SpanConstruct<()>> SpanConstruct<F> for (F, S) {
    fn new(offset: usize, len: usize, ctx: F) -> Self {
        (ctx, S::new(offset, len, ()))
    }
}

pub trait FileLike: Copy {
    fn contents(&self) -> Option<&dyn miette::SourceCode>;
}
impl FileLike for () {
    fn contents(&self) -> Option<&dyn miette::SourceCode> {
        None
    }
}
impl<F: FileLike> FileLike for Option<F> {
    fn contents(&self) -> Option<&dyn miette::SourceCode> {
        self.as_ref()?.contents()
    }
}

/// Something with a location
pub trait Located {
    type Span: Span;

    fn loc(&self) -> Self::Span;
}
