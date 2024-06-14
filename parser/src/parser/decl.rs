use super::*;
use smallvec::{smallvec, SmallVec};
use std::borrow::Cow;
use vec1::Vec1;

impl<'src, A: AstDefs<'src>, F: Copy, S: SpanConstruct> Parser<'src, '_, A, F, S>
where
    A::AstBox: Located<Span = S>,
    asts::ErrorAST<S>: Unsize<A::AstTrait>,
    asts::CommentAST<'src, S>: Unsize<A::AstTrait>,
    asts::IntLitAST<S>: Unsize<A::AstTrait>,
    asts::FloatLitAST<S>: Unsize<A::AstTrait>,
    asts::StringLitAST<'src, S>: Unsize<A::AstTrait>,
    asts::NullAST<S>: Unsize<A::AstTrait>,
    asts::VarAST<'src, S>: Unsize<A::AstTrait>,
    asts::LetAST<'src, A::AstBox>: Unsize<A::AstTrait>,
    asts::LetOpAST<'src, A::AstBox>: Unsize<A::AstTrait>,
    asts::SeqAST<A::AstBox>: Unsize<A::AstTrait>,
    asts::BraceAST<A::AstBox>: Unsize<A::AstTrait>,
    asts::ParenAST<A::AstBox>: Unsize<A::AstTrait>,
    asts::IfElseAST<A::AstBox>: Unsize<A::AstTrait>,
    asts::CallAST<A::AstBox>: Unsize<A::AstTrait>,
    asts::ShortCircuitAST<A::AstBox>: Unsize<A::AstTrait>,
    asts::FunctionTypeAST<A::AstBox>: Unsize<A::AstTrait>,
    asts::AscribeAST<A::AstBox>: Unsize<A::AstTrait>,
    asts::CastAST<A::AstBox>: Unsize<A::AstTrait>,
    asts::LambdaAST<'src, A::AstBox>: Unsize<A::AstTrait>,
    asts::UsingAST<'src, S>: Unsize<A::AstTrait>,
{
    pub fn parse_dottedname(
        &mut self,
        out: &mut Vec<A::AstBox>,
    ) -> (Option<DottedName<'src, S>>, bool) {
        if self.eat_comment(out) {
            return (None, true);
        }
        let mut segs = SmallVec::<[(_, S); 2]>::new();
        {
            match self.parse_ident(true, out) {
                (Some(seg), false) => segs.push(seg),
                (Some(seg), true) => return (Some(DottedName::new([seg])), true),
                (None, false) => return (None, false),
                (None, true) => return (None, true),
            }
        }
        loop {
            if self.eat_comment(out) {
                return (Some(DottedName::new(segs)), true);
            }
            if !matches!(
                self.input.get(self.index),
                Some(Token {
                    kind: TokenKind::Special(SpecialChar::Dot),
                    ..
                })
            ) {
                return (Some(DottedName::new(segs)), false);
            }
            self.index += 1;
            if self.eat_comment(out) {
                return (Some(DottedName::new(segs)), true);
            }
            match self.parse_ident(true, out) {
                (Some(seg), false) => segs.push(seg),
                (seg, ret) => {
                    segs.extend(seg);
                    return (Some(DottedName::new(segs)), ret);
                }
            }
        }
    }

    fn get_docs(&self, inner: bool) -> Cow<'src, [u8]> {
        let mut out = Cow::Borrowed(&[][..]);
        let mut index = self.index;
        while let Some(idx) = index.checked_sub(1) {
            index = idx;
            match self.input[idx].kind {
                TokenKind::Comment(_, CommentKind::Ignore) => {}
                TokenKind::Comment(ref comm, CommentKind::OuterDoc) => {
                    if !comm.is_empty() {
                        if out.is_empty() {
                            out.clone_from(comm);
                        } else {
                            let r = out.to_mut();
                            r.push(b'\n');
                            r.extend_from_slice(comm);
                        }
                    }
                }
                _ => break,
            }
        }
        if inner {
            loop {
                let Some(Token { kind, .. }) = self.current_token() else {
                    break;
                };
                match kind {
                    TokenKind::Comment(_, CommentKind::Ignore) => {}
                    TokenKind::Comment(ref comm, CommentKind::InnerDoc) => {
                        if !comm.is_empty() {
                            if out.is_empty() {
                                out.clone_from(comm);
                            } else {
                                let r = out.to_mut();
                                r.push(b'\n');
                                r.extend_from_slice(comm);
                            }
                        }
                    }
                    _ => break,
                }
            }
        }
        out
    }

    pub fn parse_let_decl(
        &mut self,
        global: bool,
        out: &mut Vec<A::AstBox>,
    ) -> (asts::LetAST<'src, A::AstBox>, bool) {
        let doc = self.get_docs(false);
        let kw = self.input[self.index].span;
        self.index += 1;
        let name = if global {
            match self.parse_dottedname(out) {
                (Some(n), false) => n,
                (None, false) => DottedName::local("<error>", self.curr_loc()),
                (name, true) => {
                    return (
                        asts::LetAST {
                            kw,
                            doc,
                            name: name.unwrap_or(DottedName::local("<error>", self.curr_loc())),
                            params: smallvec![],
                            ret: None,
                            body: A::make_box(asts::ErrorAST {
                                loc: self.curr_loc(),
                            }),
                        },
                        true,
                    )
                }
            }
        } else {
            match self.parse_ident(true, out) {
                (Some((name, span)), false) => DottedName::local(name, span),
                (None, false) => DottedName::local("<error>", self.curr_loc()),
                (name, true) => {
                    return (
                        asts::LetAST {
                            kw,
                            doc,
                            name: name.map_or(
                                DottedName::local("<error>", self.curr_loc()),
                                |(name, span)| DottedName::local(name, span),
                            ),
                            params: smallvec![],
                            ret: None,
                            body: A::make_box(asts::ErrorAST {
                                loc: self.curr_loc(),
                            }),
                        },
                        true,
                    )
                }
            }
        };
        let mut params = smallvec![];
        while let Some(tok) = self.input.get(self.index) {
            if self.eat_comment(out) {
                return (
                    asts::LetAST {
                        kw,
                        doc,
                        name,
                        params,
                        ret: None,
                        body: A::make_box(asts::ErrorAST {
                            loc: self.curr_loc(),
                        }),
                    },
                    true,
                );
            }
            if let (Some((name, loc)), false) = self.parse_ident(false, out) {
                params.push(asts::FnParam {
                    name: name.into(),
                    ty: None,
                    loc,
                });
                continue;
            }
            match tok.kind {
                TokenKind::Paren(_) => {
                    let erred = self.in_tree_here(|mut this| {
                        if this.eat_comment(out) {
                            return true;
                        }
                        let (n, loc) = match this.parse_ident(true, out) {
                            (_, true) => return true,
                            (Some(r), false) => r,
                            (None, false) => ("<error>", this.curr_loc()),
                        };
                        if this.eat_comment(out) {
                            return true;
                        }
                        if matches!(
                            this.current_token(),
                            Some(Token {
                                kind: TokenKind::Keyword(Keyword::Of),
                                ..
                            })
                        ) {
                            this.index += 1;
                        } else {
                            let err = this.exp_found("`of`");
                            if this.report(err) {
                                return true;
                            }
                            this.index += 1;
                            if let Some(n) = this.input[this.index..]
                                .iter()
                                .position(|t| matches!(t.kind, TokenKind::Keyword(Keyword::Of)))
                            {
                                this.index += n + 1;
                            } else {
                                this.index = this.input.len();
                            }
                        }
                        let (ty, erred) = this.parse_expr(true, false, out);
                        params.push(asts::FnParam {
                            name: n.into(),
                            ty: Some(ty),
                            loc,
                        });
                        erred
                    });
                    if erred {
                        return (
                            asts::LetAST {
                                kw,
                                doc,
                                name,
                                params,
                                ret: None,
                                body: A::make_box(asts::ErrorAST {
                                    loc: self.curr_loc(),
                                }),
                            },
                            true,
                        );
                    }
                }
                _ => break,
            }
        }
        let ret = if let Some(Token {
            kind: TokenKind::Keyword(Keyword::Of),
            ..
        }) = self.current_token()
        {
            self.index += 1;
            let (res, ret) = self.parse_expr(true, true, out);
            if ret {
                return (
                    asts::LetAST {
                        kw,
                        doc,
                        name,
                        params,
                        ret: None,
                        body: A::make_box(asts::ErrorAST {
                            loc: self.curr_loc(),
                        }),
                    },
                    true,
                );
            }
            Some(res)
        } else {
            None
        };
        if !matches!(
            self.current_token(),
            Some(Token {
                kind: TokenKind::Special(SpecialChar::Equals),
                ..
            })
        ) {
            let loc = self.curr_loc();
            let err = self.exp_found("value for let-binding");
            if self.report(err) {
                return (
                    asts::LetAST {
                        kw,
                        doc,
                        name,
                        params,
                        ret,
                        body: A::make_box(asts::ErrorAST { loc }),
                    },
                    true,
                );
            }
            if let Some(skip) = self.input[self.index..]
                .iter()
                .position(|t| matches!(t.kind, TokenKind::Special(SpecialChar::Equals)))
            {
                self.index += skip;
            } else {
                self.index = self.input.len();
            }
        } else {
            self.index += 1;
        }
        let (body, err) = self.parse_expr(true, true, out);
        (
            asts::LetAST {
                kw,
                doc,
                name,
                params,
                ret,
                body,
            },
            err,
        )
    }

    pub fn parse_namespace_def(
        &mut self,
        out: &mut Vec<A::AstBox>,
    ) -> (asts::NamespaceAST<'src, A::AstBox>, bool)
    where
        asts::NamespaceAST<'src, A::AstBox>: Unsize<A::AstTrait>,
    {
        let kw = self.current_token().unwrap().span;
        let doc = self.get_docs(true);
        self.index += 1;
        let (name, erred) = self.parse_dottedname(out);
        let name = name.unwrap_or_else(|| DottedName::local("<error>", self.curr_loc()));
        if erred {
            return (
                asts::NamespaceAST {
                    name,
                    kw,
                    doc,
                    body: self.curr_loc(),
                    nodes: vec![],
                },
                true,
            );
        }
        let mut nodes = Vec::<A::AstBox>::new();
        let (body, erred) = if let Some(&Token {
            kind: TokenKind::Brace(_),
            span,
        }) = self.current_token()
        {
            let erred = self.in_tree_here(|mut this| this.parse_top_level(&mut nodes));
            (span, erred)
        } else {
            let err = self.exp_found("namespace body");
            let body = self.curr_loc();
            (body, self.report(err))
        };
        (
            asts::NamespaceAST {
                name,
                kw,
                body,
                doc,
                nodes,
            },
            erred,
        )
    }

    pub fn parse_using_decl(
        &mut self,
        out: &mut Vec<A::AstBox>,
    ) -> (asts::UsingAST<'src, S>, bool) {
        let kw = self.current_token().unwrap().span;
        self.index += 1;
        let (pat, erred) = self.parse_glob_pattern(out);
        (asts::UsingAST { kw, pat }, erred)
    }

    fn parse_glob_term(
        &mut self,
        out: &mut Vec<A::AstBox>,
    ) -> (Option<(GlobTerm<'src, S>, S)>, bool) {
        match self.current_token() {
            Some(&Token {
                kind: TokenKind::AmbigOp(AmbigOp::Star),
                span,
            }) => {
                self.index += 1;
                (Some((GlobTerm::Star, span)), false)
            }
            Some(&Token {
                kind: TokenKind::Brace(_),
                span,
            }) => {
                let mut globs = Vec::new();
                let mut check_comma = false;
                let mut erred = self.in_tree_here(|mut this| loop {
                    match this.current_token() {
                        None => break false,
                        Some(Token {
                            kind: TokenKind::Special(SpecialChar::Comma),
                            ..
                        }) if check_comma => this.index += 1,
                        _ => {
                            if check_comma {
                                let err = this.exp_found("',' between glob options");
                                if this.report(err) {
                                    break true;
                                }
                            }
                        }
                    }
                    check_comma = true;
                    let (glob, erred) = this.parse_glob_list(false, out);
                    if let Some(g) = glob {
                        globs.push(g);
                    } else {
                        break erred;
                    }
                    if erred {
                        break true;
                    }
                });
                if let Ok(vec) = Vec1::try_from_vec(globs) {
                    (Some((GlobTerm::Group(vec), span)), erred)
                } else {
                    erred = erred || self.report(ParseASTError::EmptyGlobGroup { span });
                    (
                        Some((GlobTerm::Ident(Cow::Borrowed("<error>")), span)),
                        erred,
                    )
                }
            }
            _ => {
                let (id, erred) = self.parse_ident(false, out);
                if let Some((old_name, old_span)) = id {
                    if erred
                        || !matches!(
                            self.current_token(),
                            Some(Token {
                                kind: TokenKind::Keyword(Keyword::As),
                                ..
                            })
                        )
                    {
                        return (
                            Some((GlobTerm::Ident(Cow::Borrowed(old_name)), old_span)),
                            erred,
                        );
                    }
                    self.index += 1;
                    let (id, erred) = self.parse_ident(true, out);
                    if let Some((new_name, new_span)) = id {
                        (
                            Some((
                                GlobTerm::Alias {
                                    old_name: Cow::Borrowed(old_name),
                                    old_span,
                                    new_name: Cow::Borrowed(new_name),
                                },
                                old_span.merge(new_span),
                            )),
                            erred,
                        )
                    } else {
                        (
                            Some((GlobTerm::Ident(Cow::Borrowed(old_name)), old_span)),
                            erred,
                        )
                    }
                } else {
                    (None, erred)
                }
            }
        }
    }

    fn parse_glob_list(
        &mut self,
        non_empty: bool,
        out: &mut Vec<A::AstBox>,
    ) -> (Option<GlobList<'src, S>>, bool) {
        let mut idents = if non_empty {
            let (start, erred) = self.parse_ident(true, out);
            if erred {
                return (None, erred);
            }
            if matches!(
                self.current_token(),
                Some(Token {
                    kind: TokenKind::Special(SpecialChar::Dot),
                    ..
                })
            ) {
                self.index += 1;
            } else {
                let err = self.exp_found("'.' separator");
                if self.report(err) {
                    return (None, erred);
                }
            }
            start
                .map(|(i, s)| (Cow::Borrowed(i), s))
                .into_iter()
                .collect()
        } else {
            Vec::new()
        };
        let (mut last, mut erred) = self.parse_glob_term(out);
        if last.is_none() || erred {
            return (
                last.map(|(term, term_span)| GlobList {
                    term,
                    term_span,
                    idents: Vec::new(),
                }),
                erred,
            );
        }
        #[allow(unused_assignments)] // false positive
        let mut scratch = None;
        while matches!(
            self.current_token(),
            Some(Token {
                kind: TokenKind::Special(SpecialChar::Dot),
                ..
            })
        ) {
            self.index += 1;
            (scratch, erred) = self.parse_glob_term(out);
            if scratch.is_some() {
                let Some((GlobTerm::Ident(id), span)) = std::mem::replace(&mut last, scratch)
                else {
                    unreachable!()
                };
                idents.push((id, span));
            } else {
                break;
            }
            if erred || !matches!(last, Some((GlobTerm::Ident(_), _))) {
                break;
            }
        }
        let (term, term_span) = last.unwrap();
        (
            Some(GlobList {
                term,
                term_span,
                idents,
            }),
            erred,
        )
    }

    fn parse_glob_pattern(&mut self, out: &mut Vec<A::AstBox>) -> (GlobPattern<'src, S>, bool) {
        let global = if let Some(&Token {
            kind: TokenKind::Special(SpecialChar::Dot),
            span,
        }) = self.current_token()
        {
            self.index += 1;
            Some(span)
        } else {
            None
        };
        let (segs, erred) = self.parse_glob_list(true, out);
        let (segs, erred) = segs.map_or_else(
            || {
                (
                    GlobList {
                        term: GlobTerm::Star,
                        term_span: self.curr_loc(),
                        idents: Vec::new(),
                    },
                    erred || {
                        let err = self.exp_found("glob pattern");
                        self.report(err)
                    },
                )
            },
            |s| (s, erred),
        );
        (GlobPattern { global, segs }, erred)
    }
}
