/// Spans can be merged, that's really the only requirement that we need to know about. Producers
/// and consumers can do their own things.
pub trait Span: Copy {
    fn merge(self, other: Self) -> Self;
}

/// Something with a location
pub trait Located {
    type Span: Span;

    fn loc(&self) -> Self::Span;
}
