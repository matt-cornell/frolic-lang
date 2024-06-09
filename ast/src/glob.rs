use super::*;
use vec1::Vec1;

/// The kind of a glob element.
#[derive(Debug, Clone, PartialEq)]
pub enum GlobTerm<'src, S> {
    Ident(Cow<'src, str>),
    Alias {
        old_name: Cow<'src, str>,
        old_span: S,
        new_name: Cow<'src, str>,
    },
    Star,
    Group(Vec1<GlobList<'src, S>>),
}

/// A list of glob elements, possibly ending in a terminator.
#[derive(Debug, Clone, PartialEq)]
pub struct GlobList<'src, S> {
    pub idents: Vec<(Cow<'src, str>, S)>,
    pub term: GlobTerm<'src, S>,
    pub term_span: S,
}
impl<S: Span> Located for GlobList<'_, S> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        self.idents
            .first()
            .map_or(self.term_span, |s| s.1.merge(self.term_span))
    }
}

/// A glob pattern, used in `import`s.
#[derive(Debug, Clone, PartialEq)]
pub struct GlobPattern<'src, S> {
    pub global: Option<S>,
    pub segs: GlobList<'src, S>,
}
impl<S: Span> Located for GlobPattern<'_, S> {
    type Span = S;

    fn loc(&self) -> Self::Span {
        if let Some(glb) = self.global {
            glb.merge(self.segs.term_span)
        } else {
            self.segs.term_span
        }
    }
}
