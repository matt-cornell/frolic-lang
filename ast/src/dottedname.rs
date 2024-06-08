use super::*;
use std::fmt::{self, Display, Formatter, Write};
use std::ops::Deref;
use vec1::smallvec_v1::*;

/// A `DottedName` represents a specifier for a global variable.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DottedName<
    'src,
    S,
    V: Deref<Target = [(Cow<'src, str>, S)]> = SmallVec1<[(Cow<'src, str>, S); 1]>,
> {
    /// The segments, as tuples of segments and their spans. Should not be empty!
    pub segs: V,
}
impl<'src, S, const N: usize> DottedName<'src, S, SmallVec1<[(Cow<'src, str>, S); N]>> {
    /// Construct a new `DottedName`. This generic signature allows for string-span pairs
    /// to be used as iterator items for the segments rather than `Cow`s.
    pub fn new<O: Into<Cow<'src, str>>, I: IntoIterator<Item = (O, S)>>(segs: I) -> Self {
        let segs: SmallVec<[(Cow<'src, str>, S); N]> =
            segs.into_iter().map(|(n, s)| (n.into(), s)).collect();
        Self {
            segs: segs
                .try_into()
                .expect("DottedName input should not be empty!"),
        }
    }
    /// Convenience function to create a local name.
    pub fn local<O: Into<Cow<'src, str>>>(name: O, span: S) -> Self {
        Self {
            segs: smallvec1![(name.into(), span)],
        }
    }
}
impl<'src, S, V: Deref<Target = [(Cow<'src, str>, S)]>> Display for DottedName<'src, S, V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.segs
            .iter()
            .try_fold(false, |st, seg| {
                if st {
                    f.write_char('.')?;
                }
                f.write_str(&seg.0).map(|_| true)
            })
            .map(|_| ())
    }
}
impl<'src, S: Span, V: Deref<Target = [(Cow<'src, str>, S)]>> Located for DottedName<'src, S, V> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        let start = self
            .segs
            .first()
            .expect("DottedName should not be empty!")
            .1;
        self.segs.last().map_or(start, |l| start.merge(l.1))
    }
}
