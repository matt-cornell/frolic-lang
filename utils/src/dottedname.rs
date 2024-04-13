use std::borrow::Cow;
use std::fmt::{self, Display, Formatter, Write};
use smallvec::{smallvec, SmallVec};
use crate::span::{Span, Located};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DottedName<'src, S> {
    /// If this DottedName is global in scope, this span marks the `.`
    pub global: Option<S>,
    /// The segments, as tuples of segments and their spans. Should not be empty!
    pub segs: SmallVec<[(Cow<'src, str>, S); 1]>
}
impl<'src, S> DottedName<'src, S> {
    pub fn new<G: Into<Option<S>>, N: Into<Cow<'src, str>>, T: Into<S>, I: IntoIterator<Item = (N, T)>>(global: G, segs: I) -> Self {
        let this = Self {
            global: global.into(),
            segs: segs.into_iter().map(|(n, s)| (n.into(), s.into())).collect()
        };
        debug_assert!(!this.segs.is_empty());
        this
    }
    pub fn local<N: Into<Cow<'src, str>>, T: Into<S>>(name: N, span: T) -> Self {
        Self {
            global: None,
            segs: smallvec![(name.into(), span.into())]
        }
    }
}
impl<S> Display for DottedName<'_, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.global.is_some() {
            f.write_char('.')?;
        }
        self.segs.iter().try_fold(false, |st, seg| {
            if st {
                f.write_char('.')?;
            }
            f.write_str(&seg.0).map(|_| true)
        }).map(|_| ())
    }
}
impl<S: Span> Located for DottedName<'_, S> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        let start = self.global.unwrap_or_else(|| self.segs.first().expect("DottedName should not be empty!").1);
        self.segs.last().map_or(start, |l| start.merge(l.1))
    }
}
