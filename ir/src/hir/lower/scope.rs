use super::*;

impl<'b, 'src, F: Clone + Sync, A: ToHir<'b, F> + Send + Sync> ToHir<'b, F>
    for asts::NamespaceAST<'b, A>
where
    A::Span: Sync,
{
    fn predef_global(
        &self,
        glb: &mut GlobalPreContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInGlobalContext<'b>,
    ) -> LowerResult {
        loc.predef_ns(&self.name, glb.intern_cow_slice(&self.doc), glb)?;
        let old_len = loc.scope_name.len();
        let _ = write!(loc.scope_name, ".{}", self.name);
        let erred = self
            .nodes
            .iter()
            .try_for_each(|n| n.predef_global(glb, loc));
        loc.scope_name.truncate(old_len);
        erred
    }
    fn global(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInGlobalContext<'b>,
    ) -> LowerResult {
        let old_len = loc.scope_name.len();
        let _ = write!(loc.scope_name, ".{}", self.name);
        let erred = self.nodes.iter().try_for_each(|n| n.global(glb, loc));
        loc.scope_name.truncate(old_len);
        erred
    }
    #[cfg(feature = "rayon")]
    fn global_sync(
        &self,
        glb: &SyncGlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInGlobalContext<'b>,
    ) -> LowerResult {
        use rayon::prelude::*;
        let old_len = loc.scope_name.len();
        let _ = write!(loc.scope_name, ".{}", self.name);
        let erred = self
            .nodes
            .par_iter()
            .try_for_each_init(|| loc.clone(), |loc, n| n.global_sync(glb, loc));
        loc.scope_name.truncate(old_len);
        erred
    }
}
impl<'b, 'src, F: Clone, S: Span> ToHir<'b, F> for asts::UsingAST<'b, S> {}
