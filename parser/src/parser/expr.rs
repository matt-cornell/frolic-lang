use super::*;
use smallvec::SmallVec;
use std::borrow::Cow;

fn matches_prec<S>(op: &TokenKind<S>, lvl: u8) -> bool {
    match op {
        TokenKind::PreOp(_) => lvl == 1,
        TokenKind::AmbigOp(AmbigOp::Plus | AmbigOp::Minus) => lvl == 1 || lvl == 5,
        TokenKind::AmbigOp(AmbigOp::And) => lvl == 1 || lvl == 7,
        TokenKind::AmbigOp(AmbigOp::Star) => lvl == 1 || lvl == 4,
        TokenKind::InfOp(op) => match &**op {
            "&&" => lvl == 9,
            "||" => lvl == 10,
            _ => op
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
        }
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
    pub fn into_ast_boxed<D: AstDefs<'src, AstBox = A>>(body: A, this: Self) -> D::AstBox
    where
        asts::LambdaAST<'src, A>: Unsize<D::AstTrait>,
    {
        D::make_box(this.into_ast(body))
    }
}

/// A similar cast to `LambdaStub`: we want right-associativity without blowing up the stack.
struct LetOpStub<'src, A: Located> {
    kw: A::Span,
    op: Cow<'src, str>,
    name: Cow<'src, str>,
    nloc: A::Span,
    body: A,
}
impl<'src, A: Located> LetOpStub<'src, A> {
    pub fn into_ast(self, cont: A) -> asts::LetOpAST<'src, A> {
        let Self {
            kw,
            op,
            name,
            nloc,
            body,
        } = self;
        asts::LetOpAST {
            kw,
            op,
            name,
            nloc,
            body,
            cont,
        }
    }
}

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
    fn parse_prefix_expr(
        &mut self,
        necessary: bool,
        prefixes: &mut SmallVec<[(Cow<'src, str>, S); 1]>,
        out: &mut Vec<A::AstBox>,
    ) -> (A::AstBox, bool) {
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
                    kind: TokenKind::PreOp(ref op),
                    span,
                }) => {
                    prefixes.push((op.clone(), span));
                    self.index += 1;
                }
                Some(&Token {
                    kind: TokenKind::AmbigOp(op),
                    span,
                }) => {
                    prefixes.push((op.as_pre_str().into(), span));
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
        prefixes: &mut SmallVec<[(Cow<'src, str>, S); 1]>,
        infixes: &mut SmallVec<[(Cow<'src, str>, S, A::AstBox); 1]>,
        out: &mut Vec<A::AstBox>,
    ) -> (A::AstBox, bool) {
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
        prefixes: &mut SmallVec<[(Cow<'src, str>, S); 1]>,
        infixes: &mut SmallVec<[(Cow<'src, str>, S, A::AstBox); 1]>,
        out: &mut Vec<A::AstBox>,
    ) -> (A::AstBox, bool) {
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
            .rfold(ast, |rhs, (op, loc, lhs)| match &*op {
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
        prefixes: &mut SmallVec<[(Cow<'src, str>, S); 1]>,
        infixes: &mut SmallVec<[(Cow<'src, str>, S, A::AstBox); 1]>,
        out: &mut Vec<A::AstBox>,
    ) -> (A::AstBox, bool) {
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
        prefixes: &mut SmallVec<[(Cow<'src, str>, S); 1]>,
        infixes: &mut SmallVec<[(Cow<'src, str>, S, A::AstBox); 1]>,
        out: &mut Vec<A::AstBox>,
    ) -> (A::AstBox, bool) {
        let (val, err) = self.parse_fns_expr(necessary, prefixes, infixes, out);
        if err {
            return (val, true);
        }
        match self.current_token() {
            Some(&Token {
                kind: TokenKind::Keyword(Keyword::Of),
                span,
            }) => {
                self.index += 1;
                let (ty, err) = self.parse_fns_expr(necessary, prefixes, infixes, out);
                (A::make_box(asts::AscribeAST { kw: span, val, ty }), err)
            }
            Some(&Token {
                kind: TokenKind::Keyword(Keyword::As),
                span,
            }) => {
                self.index += 1;
                let (ty, err) = self.parse_fns_expr(necessary, prefixes, infixes, out);
                (A::make_box(asts::CastAST { kw: span, val, ty }), err)
            }
            _ => (val, false),
        }
    }

    fn parse_cond_expr(
        &mut self,
        necessary: bool,
        prefixes: &mut SmallVec<[(Cow<'src, str>, S); 1]>,
        infixes: &mut SmallVec<[(Cow<'src, str>, S, A::AstBox); 1]>,
        out: &mut Vec<A::AstBox>,
    ) -> (A::AstBox, bool) {
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
        prefixes: &mut SmallVec<[(Cow<'src, str>, S); 1]>,
        infixes: &mut SmallVec<[(Cow<'src, str>, S, A::AstBox); 1]>,
        out: &mut Vec<A::AstBox>,
    ) -> (A::AstBox, bool) {
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
        out: &mut Vec<A::AstBox>,
    ) -> Option<Result<LambdaStub<'src, A::AstBox>, ()>> {
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
            TokenKind::Ident(ref n) => {
                let param = (n.clone(), tok.span, None);
                self.index += 1;
                param
            }
            TokenKind::Paren(_) => {
                let ret = self.in_tree_here(|mut this| {
                    if this.eat_comment(out) {
                        return Err(());
                    }
                    let (n, loc) = match this.parse_ident(true, out) {
                        (_, true) => return Err(()),
                        (Some(r), false) => r,
                        (None, false) => ("<error>".into(), this.curr_loc()),
                    };
                    if this.eat_comment(out) {
                        return Err(());
                    }
                    if !matches!(
                        this.current_token(),
                        Some(Token {
                            kind: TokenKind::Keyword(Keyword::Of),
                            ..
                        })
                    ) {
                        let err = this.exp_found("parameter type");
                        if this.report(err) {
                            return Err(());
                        }
                        if let Some(next) = this.input[this.index..]
                            .iter()
                            .position(|t| matches!(t.kind, TokenKind::Keyword(Keyword::Of)))
                        {
                            this.index += next;
                        } else {
                            this.index = this.input.len();
                        }
                    } else {
                        this.index += 1;
                    }
                    if this.eat_comment(out) {
                        return Err(());
                    }
                    let (ty, ret) = this.parse_expr(true, false, out);
                    if ret || this.eat_comment(out) {
                        return Err(());
                    }
                    Ok((n, loc, Some(ty)))
                });
                match ret {
                    Ok(param) => param,
                    Err(_) => return Some(Err(())),
                }
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

    fn parse_let_op(&mut self, out: &mut Vec<A::AstBox>) -> (LetOpStub<'src, A::AstBox>, bool) {
        let Some(Token {
            kind: TokenKind::LetOp(op),
            span: kw,
        }) = self.current_token().cloned()
        else {
            unreachable!()
        };
        let op = op.into();
        if self.eat_comment(out) {
            let nloc = self.curr_loc();
            return (
                LetOpStub {
                    kw,
                    op,
                    name: "<error>".into(),
                    nloc,
                    body: A::make_box(asts::ErrorAST { loc: nloc }),
                },
                true,
            );
        }
        self.index += 1;
        let (name, nloc) = match self.parse_ident(true, out) {
            (Some((name, nloc)), false) => (name, nloc),
            (Some((name, nloc)), true) => {
                return (
                    LetOpStub {
                        kw,
                        op,
                        name: name.into(),
                        nloc,
                        body: A::make_box(asts::ErrorAST {
                            loc: self.curr_loc(),
                        }),
                    },
                    true,
                )
            }
            (None, false) => ("<error>".into(), self.curr_loc()),
            (None, true) => {
                let nloc = self.curr_loc();
                return (
                    LetOpStub {
                        kw,
                        op,
                        name: "<error>".into(),
                        nloc,
                        body: A::make_box(asts::ErrorAST { loc: nloc }),
                    },
                    true,
                );
            }
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
                    LetOpStub {
                        kw,
                        op,
                        name,
                        nloc,
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
        let (body, erred) = self.parse_expr(true, true, out);
        (
            LetOpStub {
                kw,
                op,
                name,
                nloc,
                body,
            },
            erred,
        )
    }

    fn parse_stmts(&mut self) -> (A::AstBox, bool) {
        enum AstOrStub<'src, A: Located> {
            Ast(A),
            Stub(LetOpStub<'src, A>),
        }
        use AstOrStub::*;
        let mut ast_buf = Vec::new();
        let mut stuff: SmallVec<[_; 2]> = SmallVec::new();
        let mut check_semicolon = false;
        let mut erred = false;
        loop {
            if self.eat_comment(&mut ast_buf) {
                erred = true;
                break;
            }
            let Some(tok) = self.current_token() else {
                break;
            };
            let tok = if check_semicolon {
                if matches!(tok.kind, TokenKind::Special(SpecialChar::Semicolon)) {
                    self.index += 1;
                    if self.eat_comment(&mut ast_buf) {
                        erred = true;
                        break;
                    }
                } else {
                    self.index = self.input.len()
                }
                if let Some(tok) = self.current_token() {
                    tok
                } else {
                    break;
                }
            } else {
                tok
            };
            match tok.kind {
                TokenKind::Keyword(Keyword::Let) => {
                    let (val, err) = self.parse_let_decl(false, &mut ast_buf);
                    stuff.extend(ast_buf.drain(..).map(Ast));
                    stuff.push(Ast(A::make_box(val)));
                    check_semicolon = true;
                    if err {
                        erred = true;
                        break;
                    }
                }
                TokenKind::Keyword(Keyword::Using) => {
                    let (val, err) = self.parse_using_decl(&mut ast_buf);
                    stuff.extend(ast_buf.drain(..).map(Ast));
                    stuff.push(Ast(A::make_box(val)));
                    check_semicolon = true;
                    if err {
                        erred = true;
                        break;
                    }
                }
                TokenKind::LetOp(_) => {
                    let (val, err) = self.parse_let_op(&mut ast_buf);
                    stuff.extend(ast_buf.drain(..).map(Ast));
                    stuff.push(Stub(val));
                    check_semicolon = true;
                    if err {
                        erred = true;
                        break;
                    }
                }
                _ => {
                    let (val, err) = self.parse_expr(true, true, &mut ast_buf);
                    stuff.extend(ast_buf.drain(..).map(Ast));
                    stuff.push(Ast(val));
                    check_semicolon = true;
                    if err {
                        erred = true;
                        break;
                    }
                }
            };
        }
        for elem in stuff.into_iter().rev() {
            match elem {
                Ast(a) => ast_buf.push(a),
                Stub(s) => {
                    ast_buf.reverse();
                    let cont = match ast_buf.len() {
                        0 => A::make_box(asts::NullAST {
                            loc: self.curr_loc(),
                        }),
                        1 => ast_buf.pop().unwrap(),
                        _ => A::make_box(asts::SeqAST {
                            nodes: std::mem::take(&mut ast_buf).try_into().unwrap(),
                        }),
                    };
                    ast_buf.push(A::make_box(s.into_ast(cont)));
                }
            }
        }
        ast_buf.reverse();
        let ast = match ast_buf.len() {
            0 => A::make_box(asts::NullAST {
                loc: self.curr_loc(),
            }),
            1 => ast_buf.pop().unwrap(),
            _ => A::make_box(asts::SeqAST {
                nodes: ast_buf.try_into().unwrap(),
            }),
        };
        (ast, erred)
    }

    /// Parse an expression. If not `allow_extra`, give an error with extra input.
    pub fn parse_expr(
        &mut self,
        allow_extra: bool,
        mut necessary: bool,
        out: &mut Vec<A::AstBox>,
    ) -> (A::AstBox, bool) {
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

    fn parse_atom(&mut self, necessary: bool, out: &mut Vec<A::AstBox>) -> (A::AstBox, bool) {
        match self.parse_ident(false, out) {
            (Some((name, loc)), erred) => {
                return (
                    A::make_box(asts::VarAST {
                        name,
                        loc,
                        global: None,
                    }),
                    erred,
                )
            }
            (None, true) => {
                return (
                    A::make_box(asts::ErrorAST {
                        loc: self.curr_loc(),
                    }),
                    true,
                )
            }
            _ => {}
        }
        self.index += 1;
        match self.input.get(self.index - 1) {
            Some(&Token {
                kind: TokenKind::Special(SpecialChar::Dot),
                span,
            }) => {
                let (Some((name, loc)), ret) = self.parse_ident(true, out) else {
                    unreachable!()
                };
                (
                    A::make_box(asts::VarAST {
                        name,
                        global: Some(loc),
                        loc: span,
                    }),
                    ret,
                )
            }
            Some(&Token {
                kind: TokenKind::Ident(ref i),
                span,
            }) => (
                A::make_box(asts::VarAST {
                    name: i.clone(),
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
                kind: TokenKind::Paren(ref toks),
                span,
            }) => {
                if toks.is_empty() {
                    (A::make_box(asts::NullAST { loc: span }), false)
                } else {
                    self.index -= 1;
                    let (inner, erred) =
                        self.in_tree_here(|mut this| this.parse_expr(true, false, out));
                    (A::make_box(asts::ParenAST { inner, loc: span }), erred)
                }
            }
            Some(&Token {
                kind: TokenKind::Brace(_),
                span,
            }) => {
                self.index -= 1;
                let (inner, erred) = self.in_tree_here(|mut this| this.parse_stmts());
                (A::make_box(asts::BraceAST { inner, loc: span }), erred)
            }
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
