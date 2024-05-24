use super::*;
use std::collections::hash_map::Entry;
use smallvec::SmallVec;

impl<'b, 'src: 'b, F: PartialEq + Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::LetAST<'src, A> {
    fn predef_global(
        &self,
        glb: &mut GlobalPreContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInGlobalContext,
    ) -> LowerResult {
        if self.name.segs.is_empty() {
            return Ok(());
        };
        let full_name = glb
            .alloc
            .alloc_fmt(format_args!("{}.{}", loc.scope_name, self.name))
            .into_ref();
        self.name
            .segs
            .iter()
            .enumerate()
            .try_fold(loc.scope_name.len(), |mut len, (n, (seg, _))| {
                len += seg.len() + 1;
                let name = &full_name[..len];
                let dnloc = DottedName {
                    segs: &self.name.segs[..=n],
                }
                .loc();
                match glb.global_syms.entry(name) {
                    Entry::Occupied(e) => {
                        let (file, Id(old @ &Global { span, .. })) = e.get();
                        if n == self.name.segs.len() - 1
                            || old
                                .as_alias()
                                .map_or(true, |a| a != Operand::Const(Constant::Namespace(name)))
                        {
                            (glb.report)(
                                HirError::DuplicateDefinition {
                                    name,
                                    span: dnloc,
                                    prev: PrevDef {
                                        span,
                                        file: file.clone(),
                                    },
                                }
                                .into(),
                            )?;
                        }
                    }
                    Entry::Vacant(e) => {
                        let is_last = n == self.name.segs.len() - 1;
                        let gid = glb
                            .alloc
                            .alloc(Global {
                                name,
                                span: dnloc,
                                captures: None,
                                is_func: is_last && !self.params.is_empty(),
                                blocks: LinkedList::NEW,
                                link: LinkedListLink::NEW,
                            })
                            .into_ref();
                        glb.module.push_back(gid);
                        if !is_last {
                            let term =
                                Terminator::Return(Operand::Const(Constant::Namespace(name)));
                            let blk = glb.alloc.alloc(Block::new("entry")).into_ref();
                            gid.push_back(blk);
                            blk.term.set(term);
                        }
                        e.insert((glb.file.clone(), Id(gid)));
                    }
                }
                Ok(len)
            })
            .map(|_| ())
    }
    fn global(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInGlobalContext,
    ) -> LowerResult {
        use std::fmt::Write;
        if self.name.segs.is_empty() {
            return (glb.report)(HirIce::EmptyVarName { span: self.kw }.into());
        }
        let old_len = loc.scope_name.len();
        let _ = write!(loc.scope_name, ".{}", self.name);
        let Some(&(_, Id(outer))) = glb.global_syms.get(&*loc.scope_name) else {
            return (glb.report)(
                HirIce::CouldntFindInTable {
                    name: glb.alloc.alloc_str(&loc.scope_name).into_ref(),
                    span: self.name.loc(),
                }
                .into(),
            );
        };
        let erred = if let Some((last, without_last)) = self.params.split_last() {
            if let Some((_first, rest)) = without_last.split_first() {
                let dnloc = self.name.loc();
                let mut stack = SmallVec::<[_; 2]>::with_capacity(self.params.len());
                stack.push(outer);
                for i in 1..=rest.len() {
                    let gid = glb
                        .alloc
                        .alloc(Global {
                            name: glb.alloc.alloc_fmt(format_args!("{}.#{i}", loc.scope_name)).into_ref(),
                            span: dnloc,
                            captures: stack.last().copied(),
                            is_func: true,
                            blocks: LinkedList::NEW,
                            link: LinkedListLink::NEW,
                        })
                        .into_ref();
                    glb.module.push_back(gid);
                    stack.push(gid);
                }
                let inner = glb
                    .alloc
                    .alloc(Global {
                        name: glb.alloc.alloc_fmt(format_args!("{}.#{}", loc.scope_name, rest.len() + 1)).into_ref(),
                        span: dnloc,
                        captures: stack.last().copied(),
                        is_func: true,
                        blocks: LinkedList::NEW,
                        link: LinkedListLink::NEW,
                    })
                    .into_ref();
                glb.module.push_back(inner);
                stack.push(inner);
                let blk = glb.alloc.alloc(Block::new("entry")).into_ref();
                inner.push_back(blk);
                loc.in_local(Id(blk), |loc| {
                    for ([caller, callee], param) in stack.array_windows().zip(without_last) {
                        let blk = glb.alloc.alloc(Block::returning("entry", Operand::Global(Id(callee)))).into_ref();
                        caller.push_back(blk);
                        let name = glb.intern_cow(&param.name);
                        let inst = glb.alloc.alloc(Inst {
                            name, span: param.loc,
                            kind: InstKind::ArgOf { func: Id(caller) },
                            link: LinkedListLink::NEW,
                        }).into_ref();
                        loc.insert.0.push_back(inst);
                        loc.locals.insert(name, Id(inst));
                    }
                    {
                        let name = glb.intern_cow(&last.name);
                        let inst = glb.alloc.alloc(Inst {
                            name, span: last.loc,
                            kind: InstKind::ArgOf { func: Id(inner) },
                            link: LinkedListLink::NEW,
                        }).into_ref();
                        loc.insert.0.push_back(inst);
                        loc.locals.insert(name, Id(inst));
                    }
                    let (ret, erred) = self.body.local(glb, loc);
                    loc.insert.0.term.set(Terminator::Return(ret));
                    erred
                })
            } else {
                let blk = glb.alloc.alloc(Block::new("entry")).into_ref();
                outer.push_back(blk);
                loc.in_local(Id(blk), |loc| {
                    let name = glb.intern_cow(&last.name);
                    let inst = glb
                        .alloc
                        .alloc(Inst {
                            name,
                            span: last.loc,
                            kind: InstKind::ArgOf { func: Id(outer) },
                            link: LinkedListLink::NEW,
                        })
                        .into_ref();
                    loc.insert.0.push_back(inst);
                    loc.locals.insert(name, Id(inst));
                    let (ret, erred) = self.body.local(glb, loc);
                    loc.insert.0.term.set(Terminator::Return(ret));
                    erred
                })
            }
        } else {
            let blk = glb.alloc.alloc(Block::new("entry")).into_ref();
            outer.push_back(blk);
            loc.in_local(Id(blk), |loc| {
                let (ret, erred) = self.body.local(glb, loc);
                loc.insert.0.term.set(Terminator::Return(ret));
                erred
            })
        };
        loc.scope_name.truncate(old_len);
        erred
    }
}
