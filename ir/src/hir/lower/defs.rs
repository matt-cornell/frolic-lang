use super::*;
use smallvec::SmallVec;

impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::LetAST<'src, A> {
    fn local<'l, 'g: 'l>(
        &self,
        glb: &GlobalContext<'g, 'src, Self::Span, F>,
        loc: &mut LocalInLocalContext<'l, 'src, Self::Span>,
    ) -> (Operand<'src, Self::Span>, bool) {
        if self.name.segs.len() != 1 && self.name.global.is_some() {
            let erred = (glb.report)(HirError::GlobalDefAtLocal {
                span: self.name.loc()
            });
            if erred {
                return (const_err(), true);
            }
        }
        let &(ref name, span) = self.name.segs.last().expect("ICE: empty variable name");
        if let Some((last, rest)) = self.params.split_last() {
            loc.scope_name.push(name.clone());
            loc.locals.push_new_scope();
            let ret = if let Some((first, rest)) = rest.split_first() {
                let base = loc.glb_segs_base("");
                let fid = glb.module.push_global(Global::new(base.clone()));
                let mut stack = Vec::with_capacity(self.params.len());
                let lid = glb.module.push_global(Global::new(format!("{base}.#{}", rest.len())));
                let erred = loc.with_new_loc(lid, Block::new("entry"), glb.module, |loc| {
                    let mut add_arg = |param: &asts::FnParam<'src, A>, id| {
                        let inst = Instruction {
                            name: param.name.clone(),
                            span: param.loc,
                            kind: InstKind::ArgOf { func: id },
                        };
                        let iid = glb.module.intern_inst(inst);
                        loc.push_inst(iid);
                        loc.locals.insert(param.name.clone(), iid);
                        stack.push(id);
                    };
                    add_arg(first, fid);
                    for (n, param) in rest.iter().enumerate() {
                        let id = glb.module.push_global(Global::new(format!("{base}.#{}", n + 1)));
                        add_arg(param, id);
                    }
                    add_arg(last, lid);

                    let (ret, erred) = self.body.local(glb, loc);
                    loc.block_term().set(Terminator::Return(ret));
                    erred
                });

                for &[caller, callee] in stack.array_windows() {
                    loc.with_new_loc(caller, Block::new("entry"), glb.module, |loc| loc.block_term().set(Terminator::Return(Operand::Global(callee))));
                }

                (Operand::Global(fid), erred)
            } else {
                let gid = glb.module.push_global(Global::new(loc.glb_segs_base("")));

                let inst = Instruction {
                    name: last.name.clone(),
                    span: last.loc,
                    kind: InstKind::ArgOf { func: gid },
                };
                let i = glb.module.intern_inst(inst);
                loc.locals.insert(last.name.clone(), i);

                let erred = loc.with_new_loc(gid, Block::new("entry"), glb.module, |loc| {
                    loc.push_inst(i);
                    let (ret, erred) = self.body.local(glb, loc);
                    loc.block_term().set(Terminator::Return(ret));
                    erred
                });

                (Operand::Global(gid), erred)
            };
            loc.scope_name.pop();
            loc.locals.pop_scope();
            ret
        } else {
            let (res, erred) = self.body.local(glb, loc);
            let inst = Instruction {
                name: name.clone(),
                span,
                kind: InstKind::Bind(res),
            };
            let i = glb.module.intern_inst(inst);
            loc.push_inst(i);
            loc.locals.insert(name.clone(), i);
            (Operand::Instruction(i), erred)
        }
    }
    fn predef_global(&self, glb: &mut GlobalContext<'_, 'src, Self::Span, F>, loc: &mut LocalInGlobalContext<'src, Self::Span>) -> bool {
        let mangled = loc.glb_format(&self.name);

        if self.params.len() > 1 {
            for i in (1..=(self.params.len() - 1)).rev() {
                let mangled = format!("{mangled}.#{i}");
                let id = glb.module.push_global(Global::new(mangled.clone()));
                glb.symbols.insert(mangled, UniversalGlobalId {
                    id, module: glb.module.id(),
                    file: glb.file, span: self.name.loc(),
                });
            }
        }

        let id = glb.module.push_global(Global::new(mangled.clone()));
        glb.symbols.insert(mangled, UniversalGlobalId {
            id, module: glb.module.id(),
            file: glb.file, span: self.name.loc(),
        });

        loc.globals.insert(self.name.segs.last().expect("ICE: empty variable name").0.clone(), id);
        false
    }

    fn global(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalInGlobalContext<'src, Self::Span>,
    ) -> bool {
        let mangled = loc.glb_format(&self.name);
        let Some(fid) = glb.symbols.get(&mangled) else {
            return (glb.report)(HirError::MangledGlobalNotFound {
                name: mangled,
                span: self.loc(),
            })
        };
        debug_assert_eq!(fid.module, glb.module.id(), "ICE: inserted global from a different module");

        if let Some((last, rest)) = self.params.split_last() {
            if let Some((first, rest)) = rest.split_first() {
                // this takes the number of previously curried arguments!
                let get_curried = |n| {
                    let mangled = format!("{mangled}.#{n}");
                    if let Some(id) = glb.symbols.get(&mangled) {
                        debug_assert_eq!(id.module, glb.module.id(), "ICE: inserted global from a different module");
                        Ok(id.id)
                    } else {
                        Err((glb.report)(HirError::MangledGlobalNotFound {
                            name: mangled,
                            span: self.loc(),
                        }))
                    }
                };

                let lid = match get_curried(self.params.len() - 1) {
                    Ok(id) => id,
                    Err(erred) => return erred,
                };

                let mut loc_ = LocalInLocalContext::new(lid, Block::new("entry"), std::mem::take(loc));
                let mut stack = SmallVec::<[_; 2]>::with_capacity(self.params.len());

                let mut add_arg = |param: &asts::FnParam<'src, A>, id| {
                    let inst = Instruction {
                        name: param.name.clone(),
                        span: param.loc,
                        kind: InstKind::ArgOf { func: id },
                    };
                    let iid = glb.module.intern_inst(inst);
                    loc_.push_inst(iid);
                    loc_.locals.insert(param.name.clone(), iid);
                    stack.push(id);
                };
                add_arg(first, fid.id);

                for (n, param) in rest.iter().enumerate() {
                    match get_curried(n + 1) {
                        Ok(mid) => add_arg(param, mid),
                        Err(true) => {
                            *loc = loc_.to_global(glb.module);
                            return true;
                        },
                        Err(false) => {}
                    }
                }

                add_arg(last, lid);
                let (op, erred) = self.body.local(glb, &mut loc_);
                loc_.block_term().set(Terminator::Return(op));

                for &[caller, callee] in stack.array_windows() {
                    loc_.goto_pushed_in(caller, Block::with_term("entry", Terminator::Return(Operand::Global(callee))), glb.module);
                }

                *loc = loc_.to_global(glb.module);
                erred
            } else {
                let mut loc_ = LocalInLocalContext::new(fid.id, Block::new("entry"), std::mem::take(loc));
                
                let inst = Instruction {
                    name: last.name.clone(),
                    span: last.loc,
                    kind: InstKind::ArgOf { func: fid.id },
                };
                let iid = glb.module.intern_inst(inst);
                loc_.push_inst(iid);
                loc_.locals.insert(last.name.clone(), iid);

                let (op, erred) = self.body.local(glb, &mut loc_);
                loc_.block_term().set(Terminator::Return(op));
                *loc = loc_.to_global(glb.module);
                erred
            }
        } else {
            let mut loc_ = LocalInLocalContext::new(fid.id, Block::new("entry"), std::mem::take(loc));
            let (op, erred) = self.body.local(glb, &mut loc_);
            loc_.block_term().set(Terminator::Return(op));
            *loc = loc_.to_global(glb.module);
            erred
        }
    }
}
