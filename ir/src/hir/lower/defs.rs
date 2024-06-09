use super::*;

impl<'b, 'src: 'b, F: PartialEq + Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::LetAST<'src, A> {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        let [(ref self_name, nloc)] = self.name.segs[..] else {
            return (
                const_err(),
                (glb.report)(
                    HirIce::InvalidLocalName {
                        name: glb
                            .alloc
                            .alloc_fmt(format_args!("{}", self.name))
                            .into_ref(),
                        span: self.name.loc(),
                    }
                    .into(),
                ),
            );
        };
        let old_len = loc.scope_name.len();
        let _ = write!(loc.scope_name, ".{self_name}");
        let (ret, erred) = if let Some((last, rest)) = self.params.split_last() {
            let mut stack = SmallVec::<[_; 2]>::with_capacity(self.params.len());
            let mut inner = None;
            for i in 0..=rest.len() {
                let gid = glb
                    .alloc
                    .alloc(Global {
                        name: if i == 0 {
                            glb.alloc.alloc_str(&loc.scope_name).into_ref()
                        } else {
                            glb.alloc
                                .alloc_fmt(format_args!("{}.#{i}", loc.scope_name))
                                .into_ref()
                        },
                        span: nloc,
                        captures: stack
                            .last()
                            .copied()
                            .or(loc.insert.0.parent(std::sync::atomic::Ordering::Relaxed)),
                        is_func: true,
                        docs: &[],
                        blocks: LinkedList::NEW,
                        link: LinkedListLink::NEW,
                    })
                    .into_ref();
                glb.module.push_back(gid);
                stack.push(gid);
                inner = Some(gid);
            }
            let inner = inner.unwrap();
            let blk = glb.alloc.alloc(Block::new("entry")).into_ref();
            inner.push_back(blk);
            let old_ins = std::mem::replace(&mut loc.insert, Id(blk));
            for ([caller, callee], param) in stack.array_windows().zip(rest) {
                let blk = glb
                    .alloc
                    .alloc(Block::returning("entry", Operand::Global(Id(callee))))
                    .into_ref();
                caller.push_back(blk);
                let name = glb.intern_cow_str(&param.name);
                let inst = glb
                    .alloc
                    .alloc(Inst {
                        name,
                        span: param.loc,
                        kind: InstKind::ArgOf { func: Id(caller) },
                        link: LinkedListLink::NEW,
                    })
                    .into_ref();
                loc.insert.0.push_back(inst);
                loc.locals.insert(name, Id(inst));
            }
            {
                let name = glb.intern_cow_str(&last.name);
                let inst = glb
                    .alloc
                    .alloc(Inst {
                        name,
                        span: last.loc,
                        kind: InstKind::ArgOf { func: Id(inner) },
                        link: LinkedListLink::NEW,
                    })
                    .into_ref();
                loc.insert.0.push_back(inst);
                loc.locals.insert(name, Id(inst));
            }
            let self_val = {
                let name = glb.intern_cow_str(self_name);
                let inst = glb
                    .alloc
                    .alloc(Inst {
                        name,
                        span: nloc,
                        kind: InstKind::Bind(Operand::Global(Id(stack[0]))),
                        link: LinkedListLink::NEW,
                    })
                    .into_ref();
                old_ins.0.push_back(inst);
                loc.locals.insert(name, Id(inst));
                inst
            };
            let (ret, erred) = self.body.local(glb, loc);
            loc.insert.0.term.set(Terminator::Return(ret));
            loc.insert = old_ins;
            (self_val, erred)
        } else {
            let (val, erred) = self.body.local(glb, loc);
            let name = glb.intern_cow_str(self_name);
            let inst = glb
                .alloc
                .alloc(Inst {
                    name,
                    span: nloc,
                    kind: InstKind::Bind(val),
                    link: LinkedListLink::NEW,
                })
                .into_ref();
            loc.insert.0.push_back(inst);
            loc.locals.insert(name, Id(inst));
            (inst, erred)
        };
        loc.scope_name.truncate(old_len);
        (Operand::Inst(Id(ret)), erred)
    }

    fn predef_global(
        &self,
        glb: &mut GlobalPreContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInGlobalContext<'b>,
    ) -> LowerResult {
        if self.name.segs.is_empty() {
            return Ok(());
        };
        let full_name = glb
            .alloc
            .alloc_fmt(format_args!("{}.{}", loc.scope_name, self.name))
            .into_ref();
        let docs = glb.intern_cow_slice(&self.doc);
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
                            (glb.report)(HirError::DuplicateDefinition {
                                name,
                                span: dnloc,
                                prev: PrevDef {
                                    span,
                                    file: file.clone(),
                                },
                            })?;
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
                                docs: if is_last { docs } else { &[] },
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
        loc: &mut LocalInGlobalContext<'b>,
    ) -> LowerResult {
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
                            name: glb
                                .alloc
                                .alloc_fmt(format_args!("{}.#{i}", loc.scope_name))
                                .into_ref(),
                            span: dnloc,
                            captures: stack.last().copied(),
                            is_func: true,
                            docs: &[],
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
                        name: glb
                            .alloc
                            .alloc_fmt(format_args!("{}.#{}", loc.scope_name, rest.len() + 1))
                            .into_ref(),
                        span: dnloc,
                        captures: stack.last().copied(),
                        is_func: true,
                        docs: &[],
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
                        let blk = glb
                            .alloc
                            .alloc(Block::returning("entry", Operand::Global(Id(callee))))
                            .into_ref();
                        caller.push_back(blk);
                        let name = glb.intern_cow_str(&param.name);
                        let inst = glb
                            .alloc
                            .alloc(Inst {
                                name,
                                span: param.loc,
                                kind: InstKind::ArgOf { func: Id(caller) },
                                link: LinkedListLink::NEW,
                            })
                            .into_ref();
                        loc.insert.0.push_back(inst);
                        loc.locals.insert(name, Id(inst));
                    }
                    {
                        let name = glb.intern_cow_str(&last.name);
                        let inst = glb
                            .alloc
                            .alloc(Inst {
                                name,
                                span: last.loc,
                                kind: InstKind::ArgOf { func: Id(inner) },
                                link: LinkedListLink::NEW,
                            })
                            .into_ref();
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
                    let name = glb.intern_cow_str(&last.name);
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

impl<'b, 'src: 'b, F: Clone, A: ToHir<'b, F>> ToHir<'b, F> for asts::LetOpAST<'src, A> {
    fn local(
        &self,
        glb: &GlobalContext<'_, 'b, Self::Span, F>,
        loc: &mut LocalInLocalContext<'b, Self::Span>,
    ) -> (Operand<'b, Self::Span>, LowerResult) {
        let old_len = loc.scope_name.len();
        let _ = write!(loc.scope_name, ".{}", self.name);
        let (val, erred) = self.body.local(glb, loc);
        if erred.is_err() {
            loc.scope_name.truncate(old_len);
            return (const_err(), erred);
        }
        let _ = write!(loc.scope_name, ".#cont");
        let cont = glb
            .alloc
            .alloc(Global {
                name: glb.alloc.alloc_str(&loc.scope_name).into_ref(),
                span: self.nloc,
                is_func: true,
                docs: &[],
                captures: loc.insert.0.parent(std::sync::atomic::Ordering::Relaxed),
                blocks: LinkedList::NEW,
                link: LinkedListLink::NEW,
            })
            .into_ref();
        glb.module.push_back(cont);
        let blk = glb.alloc.alloc(Block::new("entry")).into_ref();
        cont.push_back(blk);
        let old_blk = std::mem::replace(&mut loc.insert, Id(blk));
        let arg_name = glb.intern_cow_str(&self.name);
        let inst = glb
            .alloc
            .alloc(Inst {
                name: arg_name,
                span: self.nloc,
                kind: InstKind::ArgOf { func: Id(cont) },
                link: LinkedListLink::NEW,
            })
            .into_ref();
        blk.push_back(inst);
        loc.locals.push_new_scope();
        loc.locals.insert(arg_name, Id(inst));
        let (ret, erred) = self.cont.local(glb, loc);
        loc.insert.0.term.set(Terminator::Return(ret));
        loc.locals.pop_scope();
        loc.insert = old_blk;
        loc.scope_name.truncate(old_len);
        if erred.is_err() {
            return (const_err(), erred);
        }
        let (func, erred) = 'func: {
            if let Some(&v) = loc.locals.lookup(&*self.op) {
                break 'func (Operand::Inst(v), Ok(()));
            }
            let mut storage = String::new();
            for pre in loc.global_prefixes.iter().rev() {
                let _ = write!(storage, "{pre}.{}", self.op);
                if let Some(&(_, v)) = glb.global_syms.get(&*storage) {
                    break 'func (Operand::Global(v), Ok(()));
                }
            }
            let name = glb.intern_cow_str(&self.name);
            if let Some(&(_, v)) = glb.global_syms.get(name) {
                break 'func (Operand::Global(v), Ok(()));
            }
            let erred = (glb.report)(HirError::UnboundVariable {
                name,
                span: self.loc(),
            });
            (const_err(), erred)
        };
        let inst1 = glb
            .alloc
            .alloc(Inst {
                name: "",
                span: self.nloc,
                kind: InstKind::Call { func, arg: val },
                link: LinkedListLink::NEW,
            })
            .into_ref();
        loc.insert.0.push_back(inst1);
        let inst2 = glb
            .alloc
            .alloc(Inst {
                name: "",
                span: self.nloc,
                kind: InstKind::Call {
                    func: Operand::Inst(Id(inst1)),
                    arg: Operand::Global(Id(cont)),
                },
                link: LinkedListLink::NEW,
            })
            .into_ref();
        loc.insert.0.push_back(inst2);
        (Operand::Inst(Id(inst2)), erred)
    }
}
