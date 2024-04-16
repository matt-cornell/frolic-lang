pub trait Span: Copy {
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

impl<F: Copy, S: Span> Span for (F, S) {
    fn merge(self, other: Self) -> Self {
        (self.0, self.1.merge(other.1))
    }
    fn offset(self) -> usize {
        self.1.offset()
    }
    fn len(self) -> usize {
        self.1.len()
    }
}

/// Something with a location
pub trait Located {
    type Span: Span;

    fn loc(&self) -> Self::Span;
}
