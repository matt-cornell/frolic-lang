use super::*;
use smallvec::SmallVec;

impl<'src, F: Copy, A: ToHir<'src, F>> ToHir<'src, F> for asts::LetAST<'src, A> {
    fn hoist_pass(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> bool {
        if loc.builder.get_pos().is_none() {
            let name = loc.globalize_name(&self.name);
            let idx = glb.symbols.backing.push(Symbol {
                file: glb.file,
                span: self.name.loc(),
                value: MonoOwned::empty()
            });
            glb.symbols.map.write().insert(
                name.clone(),
                idx,
            );
            loc.global_scope.insert(self.name.segs[0].0.clone().into_owned(), idx);
        }
        false
    }
    fn to_hir(
        &self,
        glb: &GlobalContext<'_, 'src, Self::Span, F>,
        loc: &mut LocalContext<'src, Self::Span>,
    ) -> (Option<Owned<Value<'src, Self::Span>>>, bool) {
        if loc.builder.get_pos().is_some() {
            if let Some(span) = self
                .name
                .global
                .or_else(|| self.name.segs.get(1).map(|s| s.1))
            {
                if (glb.report)(HirError::GlobalInLocal { span }) {
                    return (None, true);
                }
            }
        }
        if self.params.is_empty() {
            if loc.builder.get_pos().is_some() {
                let (body, ret) = self.body.to_hir(glb, loc);
                let (ref name, span) = self.name.segs.get(0).expect("ICE: empty variable name!");
                loc.symbols.insert(
                    name.clone(),
                    Symbol {
                        file: (),
                        span: *span,
                        value: body.unwrap_or_else(|| {
                            loc.builder
                                .append(Box::new(Value::null(self.body.loc(), "error")))
                        }),
                    },
                );
                (None, ret)
            } else {
                let def = glb
                    .module
                    .append(Box::new(Definition::new(Some(self.name.clone()))));
                let blk = def.append_new_block();
                let res = loc.push_scope(
                    self.name.segs.iter().map(|s| s.0.clone()),
                    self.name.global.is_some(),
                );
                loc.builder.position_at(&blk);
                let (_body, ret) = self.body.to_hir(glb, loc);
                loc.restore_scope(res);
                loc.builder.clear_pos();
                let idx = 'glb: {
                    let glb_name = loc.globalize_name(&self.name);
                    {
                        let lock = glb.symbols.map.read();
                        if let Some(&idx) = lock.get(&glb_name) {
                            glb.symbols.backing[idx].value.store(def).expect("ICE: double definition of a variable");
                            break 'glb idx;
                        }
                    }
                    {
                        let lock = glb.symbols.map.upgradable_read();
                        if let Some(&idx) = lock.get(&glb_name) {
                            glb.symbols.backing[idx].value.store(def).expect("ICE: double definition of a variable");
                            break 'glb idx;
                        }
                        let mut upgrade = parking_lot::RwLockUpgradableReadGuard::upgrade(lock);
                        let sym = Symbol {
                            file: glb.file,
                            span: self.name.loc(),
                            value: def.into(),
                        };
                        let idx = glb.symbols.backing.push(sym);
                        upgrade.insert(glb_name, idx);
                        idx
                    }
                };
                if self.name.segs.len() == 1 {
                    loc.global_scope.insert(self.name.segs[0].0.clone(), idx);
                }
                (None, ret)
            }
        } else {
            let mut stack = SmallVec::<[_; 2]>::with_capacity(self.params.len());
            let base_def = Box::new(Definition::new(Some(self.name.clone())));
            let res = loc.push_scope(
                self.name.segs.iter().map(|s| s.0.clone()),
                self.name.global.is_some(),
            );
            let (iname, idef, iblk, ispan) = {
                let asts::FnParam { name, loc, .. } = self.params.last().unwrap();
                let mut def = base_def.clone();
                def.name
                    .as_mut()
                    .unwrap()
                    .segs
                    .push((format!("#{}", self.params.len()).into(), *loc));
                let def = glb.module.append(def);
                let blk = def.append_new_block();
                (name, def, blk, *loc)
            };
            let old_pos = {
                let asts::FnParam {
                    ref name,
                    loc: span,
                    ..
                } = self.params[0];
                let def = glb.module.append(base_def.clone());
                let blk = def.append_new_block();
                if loc.builder.get_pos().is_some() {
                    loc.symbols.insert(
                        name.clone(),
                        Symbol {
                            file: (),
                            span,
                            value: loc
                                .builder
                                .append(Box::new(Value::rglobal(&def, span, &**name))),
                        },
                    );
                } else {
                    let idx = 'glb: {
                        let glb_name = loc.globalize_name(&self.name);
                        {
                            let lock = glb.symbols.map.read();
                            if let Some(&idx) = lock.get(&glb_name) {
                                glb.symbols.backing[idx].value.store(def.clone()).expect("ICE: double definition of a variable");
                                break 'glb idx;
                            }
                        }
                        {
                            let lock = glb.symbols.map.upgradable_read();
                            if let Some(&idx) = lock.get(&glb_name) {
                                glb.symbols.backing[idx].value.store(def.clone()).expect("ICE: double definition of a variable");
                                break 'glb idx;
                            }
                            let mut upgrade = parking_lot::RwLockUpgradableReadGuard::upgrade(lock);
                            let sym = Symbol {
                                file: glb.file,
                                span: self.name.loc(),
                                value: def.clone().into(),
                            };
                            let idx = glb.symbols.backing.push(sym);
                            upgrade.insert(glb_name, idx);
                            idx
                        }
                    };
                    if self.name.segs.len() == 1 {
                        loc.global_scope.insert(self.name.segs[0].0.clone(), idx);
                    }
                }

                loc.symbols.push_scope();
                let old_pos = loc.builder.position_at(&iblk);
                loc.symbols.insert(
                    name.clone(),
                    Symbol {
                        file: (),
                        span,
                        value: loc
                            .builder
                            .append(Box::new(Value::func_arg(&def, span, &**name))),
                    },
                );
                stack.push((def, blk));

                old_pos
            };
            for (
                n,
                asts::FnParam {
                    name, loc: span, ..
                },
            ) in self.params[1..std::cmp::max(self.params.len() - 1, 1)]
                .iter()
                .enumerate()
            {
                let mut def = base_def.clone();
                def.name
                    .as_mut()
                    .unwrap()
                    .segs
                    .push((format!("#{n}").into(), *span));
                let def = glb.module.append(def);
                let blk = def.append_new_block();
                stack.push((def.clone(), blk));
                loc.symbols.insert(
                    name.clone(),
                    Symbol {
                        file: (),
                        span: *span,
                        value: loc
                            .builder
                            .append(Box::new(Value::func_arg(&def, *span, &**name))),
                    },
                );
            }
            loc.symbols.insert(
                iname.clone(),
                Symbol {
                    file: (),
                    span: ispan,
                    value: loc
                        .builder
                        .append(Box::new(Value::func_arg(&idef, ispan, &**iname))),
                },
            );
            stack.push((idef, iblk));

            let (_body, ret) = self.body.to_hir(glb, loc);

            for [(_, blk), (def, _)] in stack
                .windows(2)
                .rev()
                .map(|arr| <&[(Owned<_>, Owned<_>); 2]>::try_from(arr).unwrap())
            {
                loc.builder.position_at(&blk);
                loc.builder.append(Box::new(Value::rglobal(
                    &def,
                    def.name.as_ref().unwrap().segs.last().unwrap().1,
                    "",
                )));
            }

            loc.restore_scope(res);
            loc.symbols.pop_scope();
            loc.builder.set_pos(old_pos.as_ref());
            (None, ret)
        }
    }
}
