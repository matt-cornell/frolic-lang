use super::*;
use smallvec::SmallVec;
use std::borrow::Cow;

fn matches_prec(op: &TokenKind, lvl: u8) -> bool {
    match op {
        TokenKind::PreOp(_) => lvl == 1,
        TokenKind::AmbigOp(AmbigOp::Plus | AmbigOp::Minus) => lvl == 1 || lvl == 5,
        TokenKind::AmbigOp(AmbigOp::And) => lvl == 1 || lvl == 7,
        TokenKind::AmbigOp(AmbigOp::Star) => lvl == 1 || lvl == 4,
        TokenKind::InfOp("&&") => lvl == 9,
        TokenKind::InfOp("||") => lvl == 10,
        TokenKind::InfOp(op) => op
            .as_bytes()
            .first()
            .and_then(|ch| match ch {
                b'*' | b'/' | b'%' => Some(4),
                b'+' | b'-' => Some(5),
                b'@' | b'^' => Some(6),
                b'&' | b'|' | b'$' => Some(7),
                b'=' | b'<' | b'>' => Some(8),
                _ => None,
            })
            .map_or(false, |l| l == lvl),
        TokenKind::Special(SpecialChar::Arrow) => lvl == 11,
        _ => false,
    }
}

/// Lambdas are right-associative, but we do this so that we don't need recursion
struct LambdaStub<'src, A: Located> {
    bs: A::Span,
    arg: Cow<'src, str>,
    aloc: A::Span,
    argty: Option<A>,
    retty: Option<A>,
}
impl<'src, A: Located> LambdaStub<'src, A> {
    pub fn into_ast(self, body: A) -> asts::LambdaAST<'src, A> {
        let Self {
            bs,
            arg,
            aloc,
            argty,
            retty,
        } = self;
        asts::LambdaAST {
            bs,
            arg,
            aloc,
            argty,
            retty,
            body,
        }
    }
    /// allows for a more functional solution
    pub fn into_ast_boxed<D: AstDefs<AstBox<'src> = A>>(body: A, this: Self) -> D::AstBox<'src>
    where
        asts::LambdaAST<'src, A>: Unsize<D::AstTrait<'src>>,
        A: 'src,
    {
        D::make_box(this.into_ast(body))
    }
}

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
    fn parse_prefix_expr(
        &mut self,
        necessary: bool,
        prefixes: &mut SmallVec<[(&'src str, S); 1]>,
        out: &mut Vec<A::AstBox<'src>>,
    ) -> (A::AstBox<'src>, bool) {
        let start = prefixes.len();
        loop {
            match self.current_token() {
                Some(Token {
                    kind: TokenKind::Comment(..),
                    ..
                }) => {
                    if self.eat_comment(out) {
                        return (
                            A::make_box(asts::ErrorAST {
                                loc: self.curr_loc(),
                            }),
                            true,
                        );
                    }
                }
                Some(&Token {
                    kind: TokenKind::PreOp(op),
                    span,
                }) => {
                    prefixes.push((op, span));
                    self.index += 1;
                }
                Some(&Token {
                    kind: TokenKind::AmbigOp(op),
                    span,
                }) => {
                    prefixes.push((op.as_pre_str(), span));
                    self.index += 1;
                }
                _ => break,
            }
        }
        let (base, ret) = self.parse_atom(necessary, out);
        let ast = prefixes.drain(start..).rfold(base, |ast, (op, span)| {
            A::make_box(asts::CallAST {
                func: A::make_box(asts::VarAST {
                    name: op.into(),
                    global: None,
                    loc: span,
                }),
                arg: ast,
            })
        });
        (ast, ret)
    }

    fn parse_lhs_expr(
        &mut self,
        lvl: u8,
        necessary: bool,
        prefixes: &mut SmallVec<[(&'src str, S); 1]>,
        infixes: &mut SmallVec<[(&'src str, S, A::AstBox<'src>); 1]>,
        out: &mut Vec<A::AstBox<'src>>,
    ) -> (A::AstBox<'src>, bool) {
        let start = self.index;
        let (mut ast, ret) = self.parse_level(lvl - 1, necessary, prefixes, infixes, out);
        if ret {
            return (ast, true);
        }
        if self.index == start {
            return (ast, false);
        }
        loop {
            if self.eat_comment(out) {
                return (ast, true);
            }
            let Some(tok) = self.current_token() else {
                return (ast, false);
            };
            if !matches_prec(&tok.kind, lvl) {
                return (ast, false);
            }
            let func = A::make_box(asts::VarAST {
                name: tok.kind.inf_op_str().unwrap().into(),
                global: None,
                loc: tok.span,
            });
            self.index += 1;
            let (arg, err) = self.parse_level(lvl - 1, true, prefixes, infixes, out);
            let inter = A::make_box(asts::CallAST { func, arg: ast });
            ast = A::make_box(asts::CallAST { func: inter, arg });
            if err {
                return (ast, true);
            }
        }
    }

    fn parse_rhs_expr(
        &mut self,
        lvl: u8,
        necessary: bool,
        prefixes: &mut SmallVec<[(&'src str, S); 1]>,
        infixes: &mut SmallVec<[(&'src str, S, A::AstBox<'src>); 1]>,
        out: &mut Vec<A::AstBox<'src>>,
    ) -> (A::AstBox<'src>, bool) {
        let start_idx = infixes.len();
        let start = self.index;
        let (mut ast, ret) = self.parse_level(lvl - 1, necessary, prefixes, infixes, out);
        if ret {
            return (ast, true);
        }
        if self.index == start {
            return (ast, false);
        }
        let err = loop {
            if self.eat_comment(out) {
                break true;
            }
            let Some(tok) = self.current_token() else {
                break false;
            };
            if !matches_prec(&tok.kind, lvl) {
                break false;
            }
            let op = tok.kind.inf_op_str().unwrap();
            let span = tok.span;
            self.index += 1;
            let (rhs, err) = self.parse_level(lvl - 1, true, prefixes, infixes, out);
            infixes.push((op, span, ast));
            ast = rhs;
            if err {
                break true;
            }
        };
        ast = infixes
            .drain(start_idx..)
            .rfold(ast, |rhs, (op, loc, lhs)| match op {
                "->" => A::make_box(asts::FunctionTypeAST {
                    oploc: loc,
                    arg: lhs,
                    ret: rhs,
                }),
                "||" | "&&" => A::make_box(asts::ShortCircuitAST {
                    oploc: loc,
                    is_or: op == "||",
                    lhs,
                    rhs,
                }),
                _ => {
                    let func = A::make_box(asts::VarAST {
                        name: op.into(),
                        global: None,
                        loc,
                    });
                    let inter = A::make_box(asts::CallAST { func, arg: lhs });
                    A::make_box(asts::CallAST {
                        func: inter,
                        arg: rhs,
                    })
                }
            });
        (ast, err)
    }

    fn parse_fns_expr(
        &mut self,
        necessary: bool,
        prefixes: &mut SmallVec<[(&'src str, S); 1]>,
        infixes: &mut SmallVec<[(&'src str, S, A::AstBox<'src>); 1]>,
        out: &mut Vec<A::AstBox<'src>>,
    ) -> (A::AstBox<'src>, bool) {
        let start = self.index;
        let (mut ast, ret) = self.parse_level(1, necessary, prefixes, infixes, out);
        if ret {
            return (ast, true);
        }
        if self.index == start {
            return (ast, false);
        }
        loop {
            if self.eat_comment(out) {
                return (ast, true);
            }
            if matches!(
                self.current_token(),
                Some(Token {
                    kind: TokenKind::AmbigOp(_) | TokenKind::Keyword(Keyword::If),
                    ..
                })
            ) {
                return (ast, false);
            }
            let start = self.index;
            let (arg, err) = self.parse_level(1, false, prefixes, infixes, out);
            if start == self.index {
                return (ast, false);
            }
            ast = A::make_box(asts::CallAST { func: ast, arg });
            if err {
                return (ast, true);
            }
        }
    }

    #[inline]
    fn parse_types_expr(
        &mut self,
        necessary: bool,
        prefixes: &mut SmallVec<[(&'src str, S); 1]>,
        infixes: &mut SmallVec<[(&'src str, S, A::AstBox<'src>); 1]>,
        out: &mut Vec<A::AstBox<'src>>,
    ) -> (A::AstBox<'src>, bool) {
        self.parse_fns_expr(necessary, prefixes, infixes, out)
    }

    fn parse_cond_expr(
        &mut self,
        necessary: bool,
        prefixes: &mut SmallVec<[(&'src str, S); 1]>,
        infixes: &mut SmallVec<[(&'src str, S, A::AstBox<'src>); 1]>,
        out: &mut Vec<A::AstBox<'src>>,
    ) -> (A::AstBox<'src>, bool) {
        if let Some(&Token {
            kind: TokenKind::Keyword(Keyword::If),
            span: kw,
        }) = self.current_token()
        {
            self.index += 1;
            let (cond, ret) = self.parse_level(255, true, prefixes, infixes, out);
            if ret {
                let loc = self.curr_loc();
                return (
                    A::make_box(asts::IfElseAST {
                        kw,
                        cond,
                        if_true: A::make_box(asts::ErrorAST { loc }),
                        if_false: A::make_box(asts::ErrorAST { loc }),
                    }),
                    true,
                );
            }
            if !matches!(
                self.current_token(),
                Some(Token {
                    kind: TokenKind::Keyword(Keyword::Then),
                    ..
                })
            ) {
                let err = self.exp_found("'then' after condition");
                let loc = self.curr_loc();
                return (
                    A::make_box(asts::IfElseAST {
                        kw,
                        cond,
                        if_true: A::make_box(asts::ErrorAST { loc }),
                        if_false: A::make_box(asts::ErrorAST { loc }),
                    }),
                    self.report(err),
                );
            }
            self.index += 1;
            let (if_true, ret) = self.parse_level(255, true, prefixes, infixes, out);
            if ret {
                let loc = self.curr_loc();
                return (
                    A::make_box(asts::IfElseAST {
                        kw,
                        cond,
                        if_true,
                        if_false: A::make_box(asts::ErrorAST { loc }),
                    }),
                    true,
                );
            }
            if !matches!(
                self.current_token(),
                Some(Token {
                    kind: TokenKind::Keyword(Keyword::Else),
                    ..
                })
            ) {
                let err = self.exp_found("'else' after condition");
                let loc = self.curr_loc();
                return (
                    A::make_box(asts::IfElseAST {
                        kw,
                        cond,
                        if_true,
                        if_false: A::make_box(asts::ErrorAST { loc }),
                    }),
                    self.report(err),
                );
            }
            self.index += 1;
            let (if_false, ret) = self.parse_level(255, true, prefixes, infixes, out);
            return (
                A::make_box(asts::IfElseAST {
                    kw,
                    cond,
                    if_true,
                    if_false,
                }),
                ret,
            );
        };
        let start = self.index;
        let (lhs, ret) = self.parse_level(11, necessary, prefixes, infixes, out);
        if self.index == start || ret {
            return (lhs, ret);
        }
        let Some(&Token {
            kind: TokenKind::Keyword(Keyword::If),
            span: kw,
        }) = self.current_token()
        else {
            return (lhs, false);
        };
        self.index += 1;
        let (cond, ret) = self.parse_level(255, true, prefixes, infixes, out);
        if ret {
            return (
                A::make_box(asts::IfElseAST {
                    kw,
                    if_true: lhs,
                    if_false: A::make_box(asts::ErrorAST {
                        loc: self.curr_loc(),
                    }),
                    cond,
                }),
                true,
            );
        }
        match self.current_token() {
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Then),
                ..
            }) => {
                self.index += 1;
                let (if_true, ret) = self.parse_level(255, true, prefixes, infixes, out);
                if ret {
                    return (
                        A::make_box(asts::CallAST {
                            func: lhs,
                            arg: A::make_box(asts::IfElseAST {
                                kw,
                                if_true,
                                if_false: A::make_box(asts::ErrorAST {
                                    loc: self.curr_loc(),
                                }),
                                cond,
                            }),
                        }),
                        true,
                    );
                }
                if !matches!(
                    self.current_token(),
                    Some(Token {
                        kind: TokenKind::Keyword(Keyword::Else),
                        ..
                    })
                ) {
                    let err = self.exp_found("'else'");
                    return (
                        A::make_box(asts::CallAST {
                            func: lhs,
                            arg: A::make_box(asts::IfElseAST {
                                kw,
                                if_true,
                                if_false: A::make_box(asts::ErrorAST {
                                    loc: self.curr_loc(),
                                }),
                                cond,
                            }),
                        }),
                        self.report(err),
                    );
                }
                self.index += 1;
                let (if_false, ret) = self.parse_level(255, true, prefixes, infixes, out);
                (
                    A::make_box(asts::CallAST {
                        func: lhs,
                        arg: A::make_box(asts::IfElseAST {
                            kw,
                            if_true,
                            if_false,
                            cond,
                        }),
                    }),
                    ret,
                )
            }
            Some(Token {
                kind: TokenKind::Keyword(Keyword::Else),
                ..
            }) => {
                self.index += 1;
                let (if_false, ret) = self.parse_level(255, true, prefixes, infixes, out);
                (
                    A::make_box(asts::IfElseAST {
                        kw,
                        cond,
                        if_false,
                        if_true: lhs,
                    }),
                    ret,
                )
            }
            _ => {
                let err = self.exp_found("'else' after condition");
                return (
                    A::make_box(asts::IfElseAST {
                        kw,
                        if_true: lhs,
                        if_false: A::make_box(asts::ErrorAST {
                            loc: self.curr_loc(),
                        }),
                        cond,
                    }),
                    self.report(err),
                );
            }
        }
    }

    #[inline(always)]
    fn parse_level(
        &mut self,
        lvl: u8,
        necessary: bool,
        prefixes: &mut SmallVec<[(&'src str, S); 1]>,
        infixes: &mut SmallVec<[(&'src str, S, A::AstBox<'src>); 1]>,
        out: &mut Vec<A::AstBox<'src>>,
    ) -> (A::AstBox<'src>, bool) {
        match lvl {
            0 | 1 => self.parse_prefix_expr(necessary, prefixes, out), // 1 might be replaced with indexing
            2 => self.parse_fns_expr(necessary, prefixes, infixes, out),
            3 => self.parse_types_expr(necessary, prefixes, infixes, out),
            4 | 5 | 7 | 8 => self.parse_lhs_expr(lvl, necessary, prefixes, infixes, out),
            6 | 9 | 10 | 11 => self.parse_rhs_expr(lvl, necessary, prefixes, infixes, out),
            12 => self.parse_cond_expr(necessary, prefixes, infixes, out),
            255 => self.parse_level(12, necessary, prefixes, infixes, out),
            _ => unreachable!(),
        }
    }

    fn parse_lambda(
        &mut self,
        out: &mut Vec<A::AstBox<'src>>,
    ) -> Option<Result<LambdaStub<'src, A::AstBox<'src>>, ()>> {
        let Some(&Token {
            kind: TokenKind::Special(SpecialChar::Backslash),
            span: bs,
        }) = self.current_token()
        else {
            return None;
        };
        self.index += 1;
        if self.eat_comment(out) {
            return Some(Err(()));
        }
        let Some(tok) = self.current_token() else {
            let err = self.exp_found("lambda parameter");
            return self.report(err).then_some(Err(()));
        };
        let (arg, aloc, argty) = match tok.kind {
            TokenKind::Ident(n) => {
                let param = (n.into(), tok.span, None);
                self.index += 1;
                param
            }
            TokenKind::Open(Delim::Paren) => 'param: {
                self.index += 1;
                if self.eat_comment(out) {
                    return Some(Err(()));
                }
                let (n, loc) = match self.parse_ident(true, out) {
                    (_, true) => return Some(Err(())),
                    (Some(r), false) => r,
                    (None, false) => {
                        let next =
                            self.input[self.index..]
                                .iter()
                                .enumerate()
                                .find_map(|(n, t)| match t.kind {
                                    TokenKind::Keyword(Keyword::Of) => Some((n, 0)),
                                    TokenKind::Close(Delim::Paren) => Some((n, 1)),
                                    TokenKind::Special(SpecialChar::Arrow) => Some((n, 2)),
                                    _ => None,
                                });
                        match next {
                            Some((n, 0)) => {
                                self.index += n;
                                ("<error>", self.curr_loc())
                            }
                            Some((n, 1)) => {
                                self.index += n;
                                break 'param ("<error>".into(), self.curr_loc(), None);
                            }
                            Some((n, 2)) => {
                                self.index += n.saturating_sub(1);
                                break 'param ("<error>".into(), self.curr_loc(), None);
                            }
                            _ => return None,
                        }
                    }
                };
                if self.eat_comment(out) {
                    return Some(Err(()));
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
                        return Some(Err(()));
                    }
                    let next =
                        self.input[self.index..]
                            .iter()
                            .enumerate()
                            .find_map(|(n, t)| match t.kind {
                                TokenKind::Keyword(Keyword::Of) => Some((n, 0)),
                                TokenKind::Close(Delim::Paren) => Some((n, 1)),
                                TokenKind::Special(SpecialChar::Arrow) => Some((n, 2)),
                                _ => None,
                            });
                    match next {
                        Some((n, 0)) => self.index += n,
                        Some((n, 1)) => {
                            self.index += n;
                            break 'param ("<error>".into(), self.curr_loc(), None);
                        }
                        Some((n, 2)) => {
                            self.index += n.saturating_sub(1);
                            break 'param ("<error>".into(), self.curr_loc(), None);
                        }
                        _ => return None,
                    }
                }
                self.index += 1;
                if self.eat_comment(out) {
                    return Some(Err(()));
                }
                let (ty, ret) = self.parse_expr(true, true, out);
                if ret || self.eat_comment(out) {
                    return Some(Err(()));
                }
                let param: (Cow<'src, str>, _, _) = (n.into(), loc, Some(ty));
                if !matches!(
                    self.current_token(),
                    Some(Token {
                        kind: TokenKind::Close(Delim::Paren),
                        ..
                    })
                ) {
                    let err = self.exp_found("')'");
                    if self.report(err) {
                        return Some(Err(()));
                    }
                    let next =
                        self.input[self.index..]
                            .iter()
                            .enumerate()
                            .find_map(|(n, t)| match t.kind {
                                TokenKind::Keyword(Keyword::Of) => Some((n, 2)),
                                TokenKind::Close(Delim::Paren) => Some((n, 1)),
                                TokenKind::Special(SpecialChar::Arrow) => Some((n, 2)),
                                _ => None,
                            });
                    match next {
                        Some((n, 1)) => {
                            self.index += n;
                            break 'param param;
                        }
                        Some((n, 2)) => {
                            self.index += n.saturating_sub(1);
                            break 'param param;
                        }
                        _ => return None,
                    }
                }
                self.index += 1;
                param
            }
            _ => {
                let err = self.exp_found("lambda parameter");
                return self.report(err).then_some(Err(()));
            }
        };
        if self.eat_comment(out) {
            return Some(Err(()));
        }
        let retty = if let Some(Token {
            kind: TokenKind::Keyword(Keyword::Of),
            ..
        }) = self.input.get(self.index)
        {
            self.index += 1;
            let (res, ret) = self.parse_expr(true, true, out);
            if ret {
                return Some(Err(()));
            }
            Some(res)
        } else {
            None
        };
        if self.eat_comment(out) {
            return Some(Err(()));
        }
        match self.current_token() {
            Some(Token {
                kind: TokenKind::Special(SpecialChar::Arrow),
                ..
            }) => self.index += 1,
            Some(Token {
                kind: TokenKind::Special(SpecialChar::Backslash),
                ..
            }) => {}
            _ => {
                let err = self.exp_found("lambda arrow");
                if self.report(err) {
                    return Some(Err(()));
                }
            }
        }
        Some(Ok(LambdaStub {
            bs,
            arg,
            aloc,
            argty,
            retty,
        }))
    }

    /// Parse an expression. If not `allow_extra`, give an error with extra input.
    pub fn parse_expr(
        &mut self,
        allow_extra: bool,
        mut necessary: bool,
        out: &mut Vec<A::AstBox<'src>>,
    ) -> (A::AstBox<'src>, bool) {
        let Ok(lambdas) =
            std::iter::from_fn(|| self.parse_lambda(out)).collect::<Result<SmallVec<[_; 3]>, ()>>()
        else {
            return (
                A::make_box(asts::ErrorAST {
                    loc: self.curr_loc(),
                }),
                true,
            );
        };
        necessary |= !lambdas.is_empty();
        let mut prefixes = SmallVec::new();
        let mut infixes = SmallVec::new();
        let (ast, mut err) = self.parse_level(255, necessary, &mut prefixes, &mut infixes, out);
        if !err && !allow_extra {
            err = self.eat_comment(out);
            if !err && self.index < self.input.len() {
                let ef = self.exp_found("end of input");
                err = self.report(ef);
            }
        }
        (
            lambdas
                .into_iter()
                .rfold(ast, LambdaStub::into_ast_boxed::<A>),
            err,
        )
    }

    fn parse_atom(
        &mut self,
        necessary: bool,
        out: &mut Vec<A::AstBox<'src>>,
    ) -> (A::AstBox<'src>, bool) {
        self.index += 1;
        match self.input.get(self.index - 1) {
            Some(&Token {
                kind: TokenKind::Special(SpecialChar::Dot),
                span,
            }) => {
                let (Some((id, loc)), ret) = self.parse_ident(true, out) else {
                    unreachable!()
                };
                (
                    A::make_box(asts::VarAST {
                        name: id.into(),
                        global: Some(loc),
                        loc: span,
                    }),
                    ret,
                )
            }
            Some(&Token {
                kind: TokenKind::Ident(i),
                span,
            }) => (
                A::make_box(asts::VarAST {
                    name: i.into(),
                    global: None,
                    loc: span,
                }),
                false,
            ),
            Some(&Token {
                kind: TokenKind::Int(val),
                span,
            }) => (A::make_box(asts::IntLitAST { val, loc: span }), false),
            Some(&Token {
                kind: TokenKind::Float(val),
                span,
            }) => (A::make_box(asts::FloatLitAST { val, loc: span }), false),
            Some(&Token {
                kind: TokenKind::String(ref val),
                span,
            }) => (
                A::make_box(asts::StringLitAST {
                    val: val.clone(),
                    loc: span,
                }),
                false,
            ),
            Some(&Token {
                kind: TokenKind::Open(Delim::Paren),
                span: start,
            }) => match self.input.get(self.index) {
                Some(&Token {
                    kind: TokenKind::Close(Delim::Paren),
                    span: end,
                }) => {
                    self.index += 1;
                    (
                        A::make_box(asts::NullAST {
                            loc: start.merge(end),
                        }),
                        false,
                    )
                }
                Some(_) => {
                    self.index -= 1;
                    if let Some((i, span)) = self.parse_ident(false, out).0 {
                        return (
                            A::make_box(asts::VarAST {
                                name: i.into(),
                                global: None,
                                loc: span,
                            }),
                            false,
                        );
                    }
                    self.index += 1;
                    let (ast, err) = self.parse_expr(true, false, out);
                    if err {
                        return (
                            A::make_box(asts::ParenAST {
                                loc: start.merge(ast.loc()),
                                inner: ast,
                            }),
                            true,
                        );
                    }
                    if self.eat_comment(out) {
                        return (
                            A::make_box(asts::ParenAST {
                                loc: start.merge(ast.loc()),
                                inner: ast,
                            }),
                            true,
                        );
                    }
                    if let Some(&Token {
                        kind: TokenKind::Close(Delim::Paren),
                        span,
                    }) = self.current_token()
                    {
                        self.index += 1;
                        return (
                            A::make_box(asts::ParenAST {
                                loc: start.merge(span),
                                inner: ast,
                            }),
                            false,
                        );
                    } else {
                        let err = ParseASTError::UnmatchedDelimeter {
                            kind: Delim::Paren,
                            start,
                            close: false,
                            span: self.curr_loc(),
                        };
                        (
                            A::make_box(asts::ParenAST {
                                loc: start.merge(ast.loc()),
                                inner: ast,
                            }),
                            self.report(err),
                        )
                    }
                }
                None => {
                    let span = self.curr_loc();
                    let err = ParseASTError::UnmatchedDelimeter {
                        kind: Delim::Paren,
                        start,
                        span,
                        close: false,
                    };
                    (A::make_box(asts::ErrorAST { loc: span }), self.report(err))
                }
            },
            _ => {
                if necessary {
                    let err = self.exp_found("an expression");
                    (
                        A::make_box(asts::ErrorAST {
                            loc: self.curr_loc(),
                        }),
                        self.report(err),
                    )
                } else {
                    self.index -= 1;
                    (
                        A::make_box(asts::NullAST {
                            loc: self.curr_loc(),
                        }),
                        false,
                    )
                }
            }
        }
    }
}
