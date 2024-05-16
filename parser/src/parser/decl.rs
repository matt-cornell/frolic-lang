use super::*;
use smallvec::smallvec;

impl<'src, A: AstDefs, F: Copy, S: SpanConstruct> Parser<'src, '_, A, F, S>
where
    A::AstBox<'src>: Located<Span = S>,
    asts::ErrorAST<S>: Unsize<A::AstTrait<'src>>,
    asts::CommentAST<'src, S>: Unsize<A::AstTrait<'src>>,
    asts::IntLitAST<S>: Unsize<A::AstTrait<'src>>,
    asts::FloatLitAST<S>: Unsize<A::AstTrait<'src>>,
    asts::StringLitAST<'src, S>: Unsize<A::AstTrait<'src>>,
    asts::NullAST<S>: Unsize<A::AstTrait<'src>>,
    asts::VarAST<'src, S>: Unsize<A::AstTrait<'src>>,
    asts::LetAST<'src, A::AstBox<'src>>: Unsize<A::AstTrait<'src>>,
    asts::ParenAST<A::AstBox<'src>>: Unsize<A::AstTrait<'src>>,
    asts::IfElseAST<A::AstBox<'src>>: Unsize<A::AstTrait<'src>>,
    asts::CallAST<A::AstBox<'src>>: Unsize<A::AstTrait<'src>>,
    asts::ShortCircuitAST<A::AstBox<'src>>: Unsize<A::AstTrait<'src>>,
    asts::FunctionTypeAST<A::AstBox<'src>>: Unsize<A::AstTrait<'src>>,
    asts::LambdaAST<'src, A::AstBox<'src>>: Unsize<A::AstTrait<'src>>,
{
    fn parse_dottedname(
        &mut self,
        out: &mut Vec<A::AstBox<'src>>,
    ) -> (Option<DottedName<'src, S>>, bool) {
        if self.eat_comment(out) {
            return (None, true);
        }
        let global: Option<S> = if let Some(Token {
            kind: TokenKind::Special(SpecialChar::Dot),
            span,
        }) = self.input.get(self.index)
        {
            self.index += 1;
            if self.eat_comment(out) {
                return (None, true);
            }
            Some(*span)
        } else {
            None
        };
        let mut segs = Vec::<(_, S)>::new();
        {
            match self.parse_ident(true, out) {
                (Some(seg), false) => segs.push(seg),
                (Some(seg), true) => return (Some(DottedName::<S>::new(global, vec![seg])), true),
                (None, false) => return (None, false),
                (None, true) => return (None, true),
            }
        }
        loop {
            if self.eat_comment(out) {
                return (Some(DottedName::<S>::new(global, segs)), true);
            }
            if !matches!(
                self.input.get(self.index),
                Some(Token {
                    kind: TokenKind::Special(SpecialChar::Dot),
                    ..
                })
            ) {
                return (Some(DottedName::<S>::new(global, segs)), false);
            }
            self.index += 1;
            if self.eat_comment(out) {
                return (Some(DottedName::<S>::new(global, segs)), true);
            }
            match self.parse_ident(true, out) {
                (Some(seg), false) => segs.push(seg),
                (seg, ret) => {
                    segs.extend(seg);
                    return (Some(DottedName::<S>::new(global, segs)), ret);
                }
            }
        }
    }

    pub fn parse_let_decl(
        &mut self,
        out: &mut Vec<A::AstBox<'src>>,
    ) -> (Option<asts::LetAST<'src, A::AstBox<'src>>>, bool) {
        let kw = self.input[self.index].span;
        self.index += 1;
        let name = {
            match self.parse_dottedname(out) {
                (Some(n), false) => n,
                (Some(name), true) => {
                    return (
                        Some(asts::LetAST {
                            kw,
                            name,
                            params: smallvec![],
                            ret: None,
                            body: A::make_box(asts::ErrorAST {
                                loc: self.curr_loc(),
                            }),
                        }),
                        true,
                    )
                }
                (None, ret) => return (None, ret),
            }
        };
        let mut params = smallvec![];
        'params: while let Some(tok) = self.input.get(self.index) {
            match tok.kind {
                TokenKind::Comment(..) => {
                    if self.eat_comment(out) {
                        return (
                            Some(asts::LetAST {
                                kw,
                                name,
                                params,
                                ret: None,
                                body: A::make_box(asts::ErrorAST {
                                    loc: self.curr_loc(),
                                }),
                            }),
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
                            Some(asts::LetAST {
                                kw,
                                name,
                                params,
                                ret: None,
                                body: A::make_box(asts::ErrorAST {
                                    loc: self.curr_loc(),
                                }),
                            }),
                            true,
                        );
                    }
                    let (n, loc) =
                        match self.parse_ident(true, out) {
                            (_, true) => {
                                return (
                                    Some(asts::LetAST {
                                        kw,
                                        name,
                                        params,
                                        ret: None,
                                        body: A::make_box(asts::ErrorAST {
                                            loc: self.curr_loc(),
                                        }),
                                    }),
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
                            Some(asts::LetAST {
                                kw,
                                name,
                                params,
                                ret: None,
                                body: A::make_box(asts::ErrorAST {
                                    loc: self.curr_loc(),
                                }),
                            }),
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
                                Some(asts::LetAST {
                                    kw,
                                    name,
                                    params,
                                    ret: None,
                                    body: A::make_box(asts::ErrorAST {
                                        loc: self.curr_loc(),
                                    }),
                                }),
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
                            Some(asts::LetAST {
                                kw,
                                name,
                                params,
                                ret: None,
                                body: A::make_box(asts::ErrorAST {
                                    loc: self.curr_loc(),
                                }),
                            }),
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
                            Some(asts::LetAST {
                                kw,
                                name,
                                params,
                                ret: None,
                                body: A::make_box(asts::ErrorAST {
                                    loc: self.curr_loc(),
                                }),
                            }),
                            true,
                        );
                    }
                    if self.eat_comment(out) {
                        return (
                            Some(asts::LetAST {
                                kw,
                                name,
                                params,
                                ret: None,
                                body: A::make_box(asts::ErrorAST {
                                    loc: self.curr_loc(),
                                }),
                            }),
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
                                Some(asts::LetAST {
                                    kw,
                                    name,
                                    params,
                                    ret: None,
                                    body: A::make_box(asts::ErrorAST {
                                        loc: self.curr_loc(),
                                    }),
                                }),
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
                    Some(asts::LetAST {
                        kw,
                        name,
                        params,
                        ret: None,
                        body: A::make_box(asts::ErrorAST {
                            loc: self.curr_loc(),
                        }),
                    }),
                    true,
                );
            }
            Some(res)
        } else {
            None
        };
        if !matches!(
            self.input.get(self.index),
            Some(Token {
                kind: TokenKind::Special(SpecialChar::Equals),
                ..
            })
        ) {
            let loc = self.curr_loc();
            let err = self.exp_found("value for let-binding");
            return (
                Some(asts::LetAST {
                    kw,
                    name,
                    params,
                    ret,
                    body: A::make_box(asts::ErrorAST { loc }),
                }),
                self.report(err),
            );
        }
        self.index += 1;
        let (body, err) = self.parse_expr(true, true, out);
        (
            Some(asts::LetAST {
                kw,
                name,
                params,
                ret,
                body,
            }),
            err,
        )
    }
}
