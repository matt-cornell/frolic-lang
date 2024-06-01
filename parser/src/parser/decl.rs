use super::*;
use smallvec::{smallvec, SmallVec};
use std::borrow::Cow;

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
                TokenKind::Comment(_, CommentKind::Ignore) => {},
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
                let Some(Token { kind, ..}) = self.current_token() else { break };
                match kind {
                    TokenKind::Comment(_, CommentKind::Ignore) => {},
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
                            name: name.map_or(DottedName::local("<error>", self.curr_loc()), |(name, span)| DottedName::local(name, span)),
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
        'params: while let Some(tok) = self.input.get(self.index) {
            match tok.kind {
                TokenKind::Comment(..) => {
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
                }
                TokenKind::Ident(n) => {
                    params.push(asts::FnParam {
                        name: n.into(),
                        ty: None,
                        loc: tok.span,
                    });
                    self.index += 1;
                }
                TokenKind::Open(Delim::Paren) => 'param: {
                    self.index += 1;
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
                    let (n, loc) =
                        match self.parse_ident(true, out) {
                            (_, true) => {
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
                                )
                            }
                            (Some(r), false) => r,
                            (None, false) => {
                                let next = self.input[self.index..].iter().enumerate().find_map(
                                    |(n, t)| match t.kind {
                                        TokenKind::Keyword(Keyword::Of) => Some((n, 0)),
                                        TokenKind::Close(Delim::Paren) => Some((n, 1)),
                                        TokenKind::Special(SpecialChar::Equals) => Some((n, 2)),
                                        _ => None,
                                    },
                                );
                                match next {
                                    Some((n, 0)) => {
                                        self.index += n;
                                        ("<error>", self.curr_loc())
                                    }
                                    Some((n, 1)) => {
                                        self.index += n;
                                        break 'param;
                                    }
                                    Some((n, 2)) => {
                                        self.index += n;
                                        break 'params;
                                    }
                                    _ => break 'params,
                                }
                            }
                        };
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
                    if !matches!(
                        self.current_token(),
                        Some(Token {
                            kind: TokenKind::Keyword(Keyword::Of),
                            ..
                        })
                    ) {
                        let err = self.exp_found("parameter type");
                        if self.report(err) {
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
                        let next =
                            self.input[self.index..]
                                .iter()
                                .enumerate()
                                .find_map(|(n, t)| match t.kind {
                                    TokenKind::Keyword(Keyword::Of) => Some((n, 0)),
                                    TokenKind::Close(Delim::Paren) => Some((n, 1)),
                                    TokenKind::Special(SpecialChar::Equals) => Some((n, 2)),
                                    _ => None,
                                });
                        match next {
                            Some((n, 0)) => self.index += n,
                            Some((n, 1)) => {
                                self.index += n;
                                break 'param;
                            }
                            Some((n, 2)) => {
                                self.index += n;
                                break 'params;
                            }
                            _ => break 'params,
                        }
                    }
                    self.index += 1;
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
                    let (ty, ret) = self.parse_expr(true, true, out);
                    params.push(asts::FnParam {
                        name: n.into(),
                        loc,
                        ty: Some(ty),
                    });
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
                    if !matches!(
                        self.current_token(),
                        Some(Token {
                            kind: TokenKind::Close(Delim::Paren),
                            ..
                        })
                    ) {
                        let err = self.exp_found("')'");
                        if self.report(err) {
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
                        let next =
                            self.input[self.index..]
                                .iter()
                                .enumerate()
                                .find_map(|(n, t)| match t.kind {
                                    TokenKind::Keyword(Keyword::Of) => Some((n, 2)),
                                    TokenKind::Close(Delim::Paren) => Some((n, 1)),
                                    TokenKind::Special(SpecialChar::Equals) => Some((n, 2)),
                                    _ => None,
                                });
                        match next {
                            Some((n, 1)) => {
                                self.index += n;
                                break 'param;
                            }
                            Some((n, 2)) => {
                                self.index += n;
                                break 'params;
                            }
                            _ => break 'params,
                        }
                    }
                    self.index += 1;
                }
                _ => break,
            }
        }
        let ret = if let Some(Token {
            kind: TokenKind::Keyword(Keyword::Of),
            ..
        }) = self.input.get(self.index)
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
            if let Some(skip) = self.input[self.index..].iter().position(|t| t.kind == TokenKind::Special(SpecialChar::Equals)) {
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
    ) -> (asts::NamespaceAST<'src, A::AstBox>, bool) where 
    asts::NamespaceAST<'src, A::AstBox>: Unsize<A::AstTrait> {
        let kw = self.current_token().unwrap().span;
        let doc = self.get_docs(true);
        self.index += 1;
        let (name, erred) = self.parse_dottedname(out);
        let name = name.unwrap_or_else(|| DottedName::local("<error>", self.curr_loc()));
        if erred {
            return (asts::NamespaceAST {
                name, kw, doc,
                body: self.curr_loc(),
                nodes: vec![]
            }, true);
        }
        let start = if let Some(&Token { kind: TokenKind::Open(Delim::Brace), span }) = self.current_token() {
            self.index += 1;
            span
        } else {
            let err = self.exp_found("namespace body");
            let body = self.curr_loc();
            if self.report(err) {
                return (asts::NamespaceAST {
                    name, kw,
                    body, doc,
                    nodes: vec![]
                }, true)
            } else {
                body
            }
        };
        let mut nodes = Vec::<A::AstBox>::new();
        let erred = self.parse_top_level(true, &mut nodes);
        if erred {
            return (asts::NamespaceAST {
                name, kw, doc,
                body: nodes.last().map_or(start, |end| start.merge(end.loc())),
                nodes
            }, true);
        }
        let end = if let Some(&Token { kind: TokenKind::Close(Delim::Brace), span }) = self.current_token() {
            self.index += 1;
            span
        } else {
            let err = self.exp_found("closing '}'");
            let end = self.curr_loc();
            if self.report(err) {
                return (asts::NamespaceAST {
                    name, kw, doc,
                    body: start.merge(end),
                    nodes
                }, true)
            } else {
                end
            }
        };
        (asts::NamespaceAST {
            name, kw, body: start.merge(end),
            doc, nodes
        }, false)
    }
}
