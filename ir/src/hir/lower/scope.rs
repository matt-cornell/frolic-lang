use super::*;
use std::ops::Bound::*;

impl<'b, 'src: 'b, F: Clone + Sync, A: ToHir<'b, F> + Send + Sync> ToHir<'b, F>
    for asts::NamespaceAST<'src, A>
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

fn visit_all<'b, S: Span, F, C: FnMut(&str, S, GlobalId<'b, S>)>(
    search: &mut String,
    pat: &GlobList<S>,
    glb: &GlobalContext<'_, 'b, S, F>,
    optional: bool,
    callback: &mut C,
) -> LowerResult<bool> {
    let start_len = search.len();
    let mut first = true;
    for &(ref i, span) in &pat.idents {
        if !search.is_empty() {
            search.push('.');
        }
        search.push_str(i);
        if !glb.global_syms.contains_key(&**search) {
            if optional && first {
                search.truncate(start_len);
                return Ok(true);
            } else {
                let res = (glb.report)(HirError::UnboundVariable {
                    name: glb.alloc.alloc_str(search).into_ref(),
                    span,
                });
                search.truncate(start_len);
                return res.map(|_| true);
            }
        }
        first = false;
    }
    let res = match &pat.term {
        GlobTerm::Group(opts) => opts
            .iter()
            .try_fold(false, |_, pat| visit_all(search, pat, glb, false, callback)),
        GlobTerm::Star => {
            let end_len = search.len();
            let bind = search.clone();
            let res = glb
                .global_syms
                .range::<str, _>((Excluded(bind.as_str()), Unbounded))
                .map_while(|f| f.0.strip_prefix(&bind).map(|s| (&s[1..], f.1)))
                .try_fold(false, |_, (id, g)| {
                    search.push_str(id);
                    callback(id, pat.term_span, g.1);
                    search.truncate(end_len);
                    Ok(true)
                });
            res.and_then(|visited| {
                if visited {
                    Ok(false)
                } else {
                    (glb.report)(HirError::UnboundVariable {
                        name: "*",
                        span: pat.term_span,
                    })
                    .map(|_| false)
                }
            })
        }
        GlobTerm::Ident(id) => {
            search.push('.');
            search.push_str(id);
            if let Some(g) = glb.global_syms.get(&**search) {
                callback(id, pat.term_span, g.1);
                Ok(false)
            } else {
                (glb.report)(HirError::UnboundVariable {
                    name: glb.alloc.alloc_str(search).into_ref(),
                    span: pat.term_span,
                })
                .map(|_| false)
            }
        }
        GlobTerm::Alias {
            old_name,
            old_span,
            new_name,
        } => {
            search.push('.');
            search.push_str(old_name);
            if let Some(g) = glb.global_syms.get(&**search) {
                callback(new_name, pat.term_span, g.1);
                Ok(false)
            } else {
                (glb.report)(HirError::UnboundVariable {
                    name: glb.alloc.alloc_str(search).into_ref(),
                    span: *old_span,
                })
                .map(|_| false)
            }
        }
    };
    search.truncate(start_len);
    res
}

impl<'b, 'src, F: Clone, S: Span> ToHir<'b, F> for asts::UsingAST<'src, S> {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        let mut buf = String::new();
        if self.pat.global.is_none() {
            for pre in loc.global_prefixes.clone() {
                buf.push_str(pre);
                let res = visit_all(&mut buf, &self.pat.segs, glb, true, &mut |name, span, g| {
                    let name = glb.alloc.alloc_str(name).into_ref();
                    let inst = glb
                        .alloc
                        .alloc(Inst {
                            name,
                            span,
                            kind: InstKind::Bind(Operand::Global(g)),
                            link: LinkedListLink::NEW,
                        })
                        .into_ref();
                    loc.insert.0.push_back(inst);
                    loc.locals.insert(name, Id(inst));
                });
                match res {
                    Ok(false) | Err(_) => return (Operand::Const(Constant::Null), res.map(|_| ())),
                    Ok(true) => {}
                }
                buf.clear();
            }
        }
        let res = visit_all(
            &mut buf,
            &self.pat.segs,
            glb,
            false,
            &mut |name, span, g| {
                let name = glb.alloc.alloc_str(name).into_ref();
                let inst = glb
                    .alloc
                    .alloc(Inst {
                        name,
                        span,
                        kind: InstKind::Bind(Operand::Global(g)),
                        link: LinkedListLink::NEW,
                    })
                    .into_ref();
                loc.insert.0.push_back(inst);
                loc.locals.insert(name, Id(inst));
            },
        );
        (Operand::Const(Constant::Null), res.map(|_| ()))
    }

    fn resolve_imports(
        &self,
        glb: &mut GlobalPreContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInGlobalContext<'b>,
    ) -> LowerResult {
        let ctx = glb.as_context();
        let mut buf = String::new();
        let mut syms = Vec::new();
        'lookup: {
            if self.pat.global.is_none() {
                for pre in loc.global_prefixes.clone() {
                    buf.push_str(pre);
                    let again = visit_all(
                        &mut buf,
                        &self.pat.segs,
                        &ctx,
                        true,
                        &mut |name, span, g| {
                            syms.push((
                                glb.alloc
                                    .alloc_fmt(format_args!("{}.{name}", loc.scope_name))
                                    .into_ref(),
                                span,
                                g,
                            ))
                        },
                    )?;
                    if !again {
                        break 'lookup;
                    }
                    buf.clear();
                }
            }
            visit_all(
                &mut buf,
                &self.pat.segs,
                &ctx,
                false,
                &mut |name, span, g| {
                    syms.push((
                        glb.alloc
                            .alloc_fmt(format_args!("{}.{name}", loc.scope_name))
                            .into_ref(),
                        span,
                        g,
                    ))
                },
            )?;
        }
        for (name, span, base) in syms {
            let gid = glb
                .alloc
                .alloc(Global {
                    name,
                    span,
                    is_func: false,
                    docs: &[],
                    kind: match base.0.kind {
                        GlobalKind::Global(_) => GlobalKind::Global(AtomicRef::new(None)),
                        GlobalKind::Local { ty, captures } => GlobalKind::Local { ty, captures },
                        GlobalKind::Intrinsic(i) => GlobalKind::Intrinsic(i),
                    },
                    blocks: LinkedList::NEW,
                    link: LinkedListLink::NEW,
                })
                .into_ref();
            let ret = Operand::Global(base);
            let blk = glb.alloc.alloc(Block::returning("entry", ret)).into_ref();
            gid.push_back(blk);
            glb.module.push_back(gid);
            if let Some((old_file, Id(old))) =
                glb.global_syms.insert(name, (glb.file.clone(), Id(gid)))
            {
                if old.as_alias().map_or(true, |a| a != ret) {
                    (glb.report)(HirError::DuplicateDefinition {
                        name,
                        span,
                        prev: PrevDef {
                            file: old_file,
                            span: old.span,
                        },
                    })?;
                }
            }
        }
        Ok(())
    }
    fn global(
        &self,
        _glb: &GlobalContext<'_, 'b, Self::Span, F>,
        _loc: &mut LocalInGlobalContext<'b>,
    ) -> LowerResult {
        Ok(())
    }
}
