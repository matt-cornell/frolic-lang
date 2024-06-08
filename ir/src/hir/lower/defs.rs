use super::*;
use std::cell::Cell;
use std::collections::hash_map::Entry;

#[inline(always)]
const fn unknown<'b, S>() -> Operand<'b, S> {
    Operand::Const(Constant::Unknown)
}

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
        use std::fmt::Write;
        let old_len = loc.scope_name.len();
        let _ = write!(loc.scope_name, ".{self_name}");
        let (ret, erred) = if let Some((last, rest)) = self.params.split_last() {
            'func: {
                let mut stack = SmallVec::<[_; 2]>::with_capacity(self.params.len());
                let mut inner = None;
                let mut type_store =
                    SmallVec::<[_; 5]>::from_elem(unknown(), self.params.len() * 2 + 1);
                let erred: LowerResult<usize> = self.params.iter().try_fold(0, |n, p| {
                    let ty = if let Some(ty) = &p.ty {
                        let (ty, erred) = ty.local(glb, loc);
                        erred?;
                        ty
                    } else {
                        unknown()
                    };
                    type_store[n] = ty;
                    Ok(n + 1)
                });
                let Ok(idx) = erred else {
                    break 'func (const_err(), Err(EarlyReturn));
                };
                *type_store.last_mut().unwrap() = {
                    if let Some(ty) = &self.ret {
                        let (ty, erred) = ty.local(glb, loc);
                        if erred.is_err() {
                            break 'func (const_err(), erred);
                        }
                        ty
                    } else {
                        unknown()
                    }
                };
                let (params, types) = type_store.split_at_mut(idx);
                for (n, ([new, ret], &mut arg)) in Cell::from_mut(types)
                    .as_slice_of_cells()
                    .array_windows()
                    .zip(params)
                    .rev()
                    .enumerate()
                {
                    let name = glb
                        .alloc
                        .alloc_fmt(format_args!(
                            "{}.#{}.#ty",
                            loc.scope_name,
                            self.params.len() - n
                        ))
                        .into_ref();
                    let inst = glb
                        .alloc
                        .alloc(Inst {
                            name,
                            span: nloc,
                            kind: InstKind::FnType {
                                arg,
                                ret: ret.get(),
                            },
                            link: LinkedListLink::NEW,
                        })
                        .into_ref();
                    loc.insert.0.push_back(inst);
                    new.set(Operand::Inst(Id(inst)));
                }
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
                            kind: GlobalKind::Local {
                                captures: Id(stack
                                    .last()
                                    .copied()
                                    .or(loc.insert.0.parent(Ordering::Relaxed))
                                    .unwrap()),
                                ty: types[i],
                            },
                            is_func: true,
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
                    let name = glb.intern_cow(&param.name);
                    let inst = glb
                        .alloc
                        .alloc(Inst {
                            name,
                            span: param.loc,
                            kind: InstKind::Arg,
                            link: LinkedListLink::NEW,
                        })
                        .into_ref();
                    blk.push_back(inst);
                    loc.locals.insert(name, Id(inst));
                }
                {
                    let name = glb.intern_cow(&last.name);
                    let inst = glb
                        .alloc
                        .alloc(Inst {
                            name,
                            span: last.loc,
                            kind: InstKind::Arg,
                            link: LinkedListLink::NEW,
                        })
                        .into_ref();
                    loc.insert.0.push_back(inst);
                    loc.locals.insert(name, Id(inst));
                }
                let self_val = {
                    let name = glb.intern_cow(self_name);
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
                (Operand::Inst(Id(self_val)), erred)
            }
        } else {
            let (ty, erred) = self.ret.as_ref().map_or((None, Ok(())), |ret| {
                let (ty, err) = ret.local(glb, loc);
                (Some(ty), err)
            });
            if erred.is_err() {
                return (const_err(), erred);
            }
            let (val, erred) = self.body.local(glb, loc);
            let name = glb.intern_cow(self_name);
            let inst = glb
                .alloc
                .alloc(Inst {
                    name,
                    span: nloc,
                    kind: if let Some(ty) = ty {
                        InstKind::Ascribe { val, ty }
                    } else {
                        InstKind::Bind(val)
                    },
                    link: LinkedListLink::NEW,
                })
                .into_ref();
            loc.insert.0.push_back(inst);
            loc.locals.insert(name, Id(inst));
            (Operand::Inst(Id(inst)), erred)
        };
        loc.scope_name.truncate(old_len);
        (ret, erred)
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
                                is_func: is_last && !self.params.is_empty(),
                                kind: if is_last {
                                    GlobalKind::Global(AtomicRef::new(None))
                                } else {
                                    GlobalKind::NAMESPACE
                                },
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
        use std::fmt::Write;
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
        let erred = 'body: {
            let mut type_store = SmallVec::<[_; 5]>::from_elem(None, self.params.len() * 2 + 1);
            let erred: LowerResult<usize> = self.params.iter().try_fold(0, |n, p| {
                let ty = if let Some(ty) = &p.ty {
                    let gid = glb
                        .alloc
                        .alloc(Global {
                            name: glb
                                .alloc
                                .alloc_fmt(format_args!("{}.#p{}.#ty", loc.scope_name, n + 1))
                                .into_ref(),
                            span: ty.loc(),
                            is_func: false,
                            kind: GlobalKind::Global(AtomicRef::new(None)),
                            blocks: LinkedList::NEW,
                            link: LinkedListLink::NEW,
                        })
                        .into_ref();
                    let blk = glb.alloc.alloc(Block::new("entry")).into_ref();
                    gid.push_back(blk);
                    glb.module.push_back(gid);
                    let erred = loc.in_local(Id(blk), |loc| {
                        let (ty, erred) = ty.local(glb, loc);
                        loc.insert.0.term.set(Terminator::Return(ty));
                        erred
                    });
                    erred?;
                    Some(gid)
                } else {
                    None
                };
                type_store[n] = ty;
                Ok(n + 1)
            });
            let Ok(idx) = erred else {
                break 'body Err(EarlyReturn);
            };
            *type_store.last_mut().unwrap() = {
                if let Some(ty) = &self.ret {
                    let gid = glb
                        .alloc
                        .alloc(Global {
                            name: glb
                                .alloc
                                .alloc_fmt(format_args!("{}.#ret.#ty", loc.scope_name))
                                .into_ref(),
                            span: ty.loc(),
                            is_func: false,
                            kind: GlobalKind::Global(AtomicRef::new(None)),
                            blocks: LinkedList::NEW,
                            link: LinkedListLink::NEW,
                        })
                        .into_ref();
                    let blk = glb.alloc.alloc(Block::new("entry")).into_ref();
                    gid.push_back(blk);
                    glb.module.push_back(gid);
                    let erred = loc.in_local(Id(blk), |loc| {
                        let (ty, erred) = ty.local(glb, loc);
                        loc.insert.0.term.set(Terminator::Return(ty));
                        erred
                    });
                    if erred.is_err() {
                        break 'body erred;
                    }
                    Some(gid)
                } else {
                    None
                }
            };
            let (params, types) = type_store.split_at_mut(idx);
            for (n, ([new, ret], &mut arg)) in Cell::from_mut(types)
                .as_slice_of_cells()
                .array_windows()
                .zip(params)
                .rev()
                .enumerate()
            {
                if ret.get().is_none() && arg.is_none() {
                    continue;
                }
                let name = glb
                    .alloc
                    .alloc_fmt(format_args!(
                        "{}.#{}.#ty",
                        loc.scope_name,
                        self.params.len() - n
                    ))
                    .into_ref();
                let span = match (arg, ret.get()) {
                    (None, Some(a)) | (Some(a), None) => a.span,
                    (Some(a), Some(b)) => a.span.merge(b.span),
                    _ => unreachable!(),
                };
                let gid = glb
                    .alloc
                    .alloc(Global {
                        name,
                        span,
                        is_func: false,
                        kind: GlobalKind::Global(AtomicRef::new(None)),
                        blocks: LinkedList::NEW,
                        link: LinkedListLink::NEW,
                    })
                    .into_ref();
                let blk = glb.alloc.alloc(Block::new("entry")).into_ref();
                gid.push_back(blk);
                glb.module.push_back(gid);
                let inst = glb
                    .alloc
                    .alloc(Inst {
                        name,
                        span,
                        kind: InstKind::FnType {
                            arg: arg.map_or(unknown(), |g| Operand::Global(Id(g))),
                            ret: ret.get().map_or(unknown(), |g| Operand::Global(Id(g))),
                        },
                        link: LinkedListLink::NEW,
                    })
                    .into_ref();
                blk.push_back(inst);
                blk.term.set(Terminator::Return(Operand::Inst(Id(inst))));
                new.set(Some(gid));
            }
            if let GlobalKind::Global(aref) = &outer.kind {
                aref.store(types[0], Ordering::Relaxed);
            }
            if let Some((last, without_last)) = self.params.split_last() {
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
                                is_func: true,
                                kind: GlobalKind::Local {
                                    captures: Id(*stack.last().unwrap()),
                                    ty: Operand::Global(Id(types[i].unwrap())),
                                },
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
                            is_func: true,
                            kind: GlobalKind::Local {
                                captures: Id(*stack.last().unwrap()),
                                ty: Operand::Global(Id(types[rest.len()].unwrap())),
                            },
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
                            let name = glb.intern_cow(&param.name);
                            let inst = glb
                                .alloc
                                .alloc(Inst {
                                    name,
                                    span: param.loc,
                                    kind: InstKind::Arg,
                                    link: LinkedListLink::NEW,
                                })
                                .into_ref();
                            blk.push_back(inst);
                            loc.locals.insert(name, Id(inst));
                        }
                        {
                            let name = glb.intern_cow(&last.name);
                            let inst = glb
                                .alloc
                                .alloc(Inst {
                                    name,
                                    span: last.loc,
                                    kind: InstKind::Arg,
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
                        let name = glb.intern_cow(&last.name);
                        let inst = glb
                            .alloc
                            .alloc(Inst {
                                name,
                                span: last.loc,
                                kind: InstKind::Arg,
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
            }
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
        use std::fmt::Write;
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
                kind: GlobalKind::Local {
                    captures: Id(loc.insert.0.parent(Ordering::Relaxed).unwrap()),
                    ty: Operand::Const(Constant::Unknown),
                },
                blocks: LinkedList::NEW,
                link: LinkedListLink::NEW,
            })
            .into_ref();
        glb.module.push_back(cont);
        let blk = glb.alloc.alloc(Block::new("entry")).into_ref();
        cont.push_back(blk);
        let old_blk = std::mem::replace(&mut loc.insert, Id(blk));
        let arg_name = glb.intern_cow(&self.name);
        let inst = glb
            .alloc
            .alloc(Inst {
                name: arg_name,
                span: self.nloc,
                kind: InstKind::Arg,
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
            let name = glb.intern_cow(&self.name);
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
