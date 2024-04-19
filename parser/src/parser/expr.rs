use super::*;

impl<'src, A: AstDefs, F: Copy> Parser<'src, '_, A, F>
where
    A::AstBox<'src>: Located<Span = SourceSpan>,
    asts::ErrorAST<SourceSpan>: Unsize<A::AstTrait<'src>>,
    asts::CommentAST<'src, SourceSpan>: Unsize<A::AstTrait<'src>>,
    asts::IntLitAST<SourceSpan>: Unsize<A::AstTrait<'src>>,
    asts::FloatLitAST<SourceSpan>: Unsize<A::AstTrait<'src>>,
    asts::NullAST<SourceSpan>: Unsize<A::AstTrait<'src>>,
    asts::VarAST<'src, SourceSpan>: Unsize<A::AstTrait<'src>>,
    asts::LetAST<'src, A::AstBox<'src>>: Unsize<A::AstTrait<'src>>,
    asts::ParenAST<A::AstBox<'src>>: Unsize<A::AstTrait<'src>>,
{
    /// Parse an expression. If not `allow_extra`, give an error with extra input.
    pub fn parse_expr(
        &mut self,
        allow_extra: bool,
        out: &mut Vec<A::AstBox<'src>>,
    ) -> (A::AstBox<'src>, bool) {
        if self.eat_comment(out) {
            return (A::make_box(asts::ErrorAST {
                loc: self.curr_loc().into(),
            }), true);
        }
        self.parse_atom(out)
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
                    let (ast, err) = self.parse_expr(false, out);
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
                            span: self.curr_loc().into(),
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
                    let span = self.curr_loc().into();
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
                (A::make_box(asts::ErrorAST {loc: self.curr_loc().into()}), self.report(err))
            }
        }
    }
}
