use super::*;
use smallvec::SmallVec;

impl<'src, A: AstDefs, F: Copy, S: SpanConstruct> Parser<'src, '_, A, F, S>
where
    A::AstBox<'src>: Located<Span = S>,
    asts::ErrorAST<S>: Unsize<A::AstTrait<'src>>,
    asts::CommentAST<'src, S>: Unsize<A::AstTrait<'src>>,
    asts::IntLitAST<S>: Unsize<A::AstTrait<'src>>,
    asts::FloatLitAST<S>: Unsize<A::AstTrait<'src>>,
    asts::NullAST<S>: Unsize<A::AstTrait<'src>>,
    asts::VarAST<'src, S>: Unsize<A::AstTrait<'src>>,
    asts::LetAST<'src, A::AstBox<'src>>: Unsize<A::AstTrait<'src>>,
    asts::ParenAST<A::AstBox<'src>>: Unsize<A::AstTrait<'src>>,
    asts::CallAST<A::AstBox<'src>>: Unsize<A::AstTrait<'src>>,
{
    fn parse_prefix_expr(&mut self, prefixes: &mut SmallVec<[(&'src str, S); 1]>, out: &mut Vec<A::AstBox<'src>>) -> (A::AstBox<'src>, bool) {
        let start = prefixes.len();
        loop {
            match self.current_token() {
                Some(Token {kind: TokenKind::Comment(..), ..}) => {
                    if self.eat_comment(out) {
                        return (A::make_box(asts::ErrorAST {
                            loc: self.curr_loc(),
                        }), true);
                    }
                }
                Some(&Token {kind: TokenKind::PreOp(op), span}) => {
                    prefixes.push((op, span));
                    self.index += 1;
                }
                Some(&Token {kind: TokenKind::AmbigOp(op), span}) => {
                    prefixes.push((op.as_pre_str(), span));
                    self.index += 1;
                }
                _ => break,
            }
        }
        let (base, ret) = self.parse_atom(out);
        let ast = prefixes.drain(start..).rfold(base, |ast, (op, span)| A::make_box(asts::CallAST {
            func: A::make_box(asts::VarAST {
                name: op.into(),
                loc: span,
            }),
            arg: ast
        }));
        (ast, ret)
    }
    /// Parse an expression. If not `allow_extra`, give an error with extra input.
    pub fn parse_expr(
        &mut self,
        allow_extra: bool,
        out: &mut Vec<A::AstBox<'src>>,
    ) -> (A::AstBox<'src>, bool) {
        let mut prefixes = SmallVec::new();
        let (ast, mut err) = self.parse_prefix_expr(&mut prefixes, out);
        if !err && !allow_extra {
            err = self.eat_comment(out);
            if !err && self.index < self.input.len() {
                let ef = self.exp_found("end of input");
                err = self.report(ef);
            }
        }
        (ast, err)
    }

    fn parse_atom(&mut self, out: &mut Vec<A::AstBox<'src>>) -> (A::AstBox<'src>, bool) {
        self.index += 1;
        match self.input.get(self.index - 1) {
            Some(&Token {
                kind: TokenKind::Ident(i),
                span,
            }) => (
                A::make_box(asts::VarAST {
                    name: i.into(),
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
                kind: TokenKind::Open(Delim::Paren),
                span: start,
            }) => match self.input.get(self.index) {
                Some(&Token {
                    kind: TokenKind::Close(Delim::Paren),
                    span: end,
                }) => (
                    A::make_box(asts::NullAST {
                        loc: start.merge(end),
                    }),
                    false,
                ),
                Some(_) => {
                    self.index -= 1;
                    if let Some((i, span)) = self.parse_ident(false, out).0 {
                        return (
                            A::make_box(asts::VarAST {
                                name: i.into(),
                                loc: span,
                            }),
                            false,
                        );
                    }
                    self.index += 1;
                    let (ast, err) = self.parse_expr(true, out);
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
                let err = self.exp_found("an expression");
                (A::make_box(asts::ErrorAST {loc: self.curr_loc()}), self.report(err))
            }
        }
    }
}
