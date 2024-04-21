use super::*;
use error::ParseASTError;
use frolic_ast::prelude::*;
use std::marker::{PhantomData, Unsize};
use traits::*;

mod decl;
pub mod error;
mod expr;
pub mod traits;

/// Bundle of common state for parser
struct Parser<'src, 'a, A, F, S: Span> {
    input: &'a [Token<'src, S>],
    index: usize,
    file: F,
    errs: &'a mut dyn ErrorReporter<SourcedError<F, ParseASTError<'src, S>>>,
    _asts: PhantomData<A>,
}
impl<'src, 'a, A: AstDefs, F: Copy, S: SpanConstruct> Parser<'src, 'a, A, F, S>
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
    /// Create a new parser
    pub fn new(
        input: &'a [Token<'src, S>],
        file: F,
        errs: &'a mut dyn ErrorReporter<SourcedError<F, ParseASTError<'src, S>>>,
    ) -> Self {
        Self {
            input,
            file,
            errs,
            index: 0,
            _asts: PhantomData,
        }
    }

    /// Report an error to the reporter
    fn report(&mut self, err: ParseASTError<'src, S>) -> bool {
        self.errs.report(SourcedError {
            file: self.file,
            error: err,
        })
    }

    fn exp_found(&self, ex: &'static str) -> ParseASTError<'src, S> {
        let span = self.curr_loc();
        let tok = self.current_token();
        ParseASTError::ExpectedFound {
            ex,
            span,
            found: tok.map(|t| t.kind.clone()),
        }
    }

    #[inline]
    fn current_token(&self) -> Option<&Token<'src, S>> {
        self.input.get(self.index)
    }

    fn curr_span(&self) -> S {
        self.input.get(self.index).map_or_else(
            || {
                S::loc(self.input
                    .last()
                    .map_or(0, |t| t.span.offset() + t.span.len()))
            },
            |t| t.span,
        )
    }

    fn curr_loc(&self) -> S {
        S::loc(self.input.get(self.index).map_or_else(
            || {
                self.input
                    .last()
                    .map_or(0, |t| t.span.offset() + t.span.len())
            },
            |t| t.span.offset(),
        ))
    }

    fn eat_comment(&mut self, out: &mut Vec<A::AstBox<'src>>) -> bool {
        loop {
            let Some(tok) = self.input.get(self.index) else {
                return false;
            };
            match tok.kind {
                TokenKind::Comment(ref comm, CommentKind::Ignore) => {
                    self.index += 1;
                    out.push(A::make_box(asts::CommentAST {
                        comm: comm.clone(),
                        loc: tok.span,
                    }));
                }
                TokenKind::Comment(_, CommentKind::OuterDoc) => {
                    self.index += 1;
                    for Token { kind, span } in &self.input[self.index..] {
                        match kind {
                            TokenKind::Comment(comm, CommentKind::Ignore) => {
                                self.index += 1;
                                out.push(A::make_box(asts::CommentAST {
                                    comm: comm.clone(),
                                    loc: tok.span,
                                }));
                            }
                            TokenKind::Comment(_, CommentKind::OuterDoc) => self.index += 1,
                            TokenKind::Comment(_, CommentKind::InnerDoc) => {
                                self.index += 1;
                                if self.report(ParseASTError::UnboundInnerDoc { span: *span }) {
                                    return true;
                                }
                            }
                            TokenKind::Keyword(Keyword::Let) => return false,
                            _ => {
                                return self
                                    .report(ParseASTError::UnboundOuterDoc { span: tok.span })
                            }
                        }
                    }
                    return self.report(ParseASTError::UnboundOuterDoc { span: tok.span });
                }
                TokenKind::Comment(_, CommentKind::InnerDoc) => {
                    self.index += 1;
                    if self.report(ParseASTError::UnboundInnerDoc { span: tok.span }) {
                        return true;
                    }
                }
                _ => return false,
            }
        }
    }

    /// Parse an identifier.
    fn parse_ident(
        &mut self,
        necessary: bool,
        out: &mut Vec<A::AstBox<'src>>,
    ) -> (Option<(&'src str, S)>, bool) {
        let orig = self.index;
        match self.current_token() {
            Some(&Token {
                kind: TokenKind::Ident(i),
                span,
            }) => {
                self.index += 1;
                (Some((i, span)), false)
            }
            Some(&Token {
                kind: TokenKind::Open(Delim::Paren),
                span: start,
            }) => {
                self.index += 1;
                if self.eat_comment(out) {
                    return (None, true);
                }
                let (id, mspan) = match self.current_token() {
                    Some(&Token {kind: TokenKind::PreOp(op) | TokenKind::InfOp(op) | TokenKind::Ident(op), span}) => (op, span),
                    Some(&Token {kind: TokenKind::AmbigOp(op), span}) => (op.as_inf_str(), span),
                    Some(&Token {kind: TokenKind::Keyword(kw), span}) => (kw.as_str(), span),
                    _ => {
                        if necessary {
                            let tok = self.current_token().cloned();
                            let span = self.curr_span();
                            self.index += 1;
                            return (
                                None,
                                self.report(ParseASTError::ExpectedFound {
                                    ex: "an identifier",
                                    span,
                                    found: tok
                                        .map(|t| {
                                            t.kind
                                        }),
                                }),
                            )
                        } else {
                            self.index = orig;
                            return (None, false)
                        }
                    }
                };
                self.index += 1;
                if self.eat_comment(out) {
                    return (Some((id, start.merge(mspan))), true);
                }
                if let Some(&Token {kind: TokenKind::Close(Delim::Paren), span: end}) = self.current_token() {
                    self.index += 1;
                    (Some((id, start.merge(end))), false)
                } else if necessary {
                    let err = self.exp_found("')'");
                    (Some((id, start.merge(mspan))), self.report(err))
                } else {
                    self.index = orig;
                    (None, false)
                }
            }
            _ => {
                if necessary {
                    let tok = self.current_token().cloned();
                    let span = self.curr_span();
                    self.index += 1;
                    (
                        None,
                        self.report(ParseASTError::ExpectedFound {
                            ex: "an identifier",
                            span,
                            found: tok
                                .map(|t| {
                                    t.kind
                                }),
                        }),
                    )
                } else {
                    (None, false)
                }
            }
        }
    }

    /// Parse a program at the top level.
    fn parse_top_level_impl(&mut self) -> (Vec<A::AstBox<'src>>, bool) {
        let mut out = Vec::new();
        while let Some(tok) = self.input.get(self.index) {
            match tok.kind {
                TokenKind::Keyword(Keyword::Let) => {
                    let (res, ret) = self.parse_let_decl(&mut out);
                    if ret {
                        return (out, true);
                    }
                    if let Some(res) = res {
                        out.push(A::make_box(res));
                    }
                    if matches!(self.current_token(), Some(Token {kind: TokenKind::Special(SpecialChar::Semicolon), ..})) {self.index += 1} else {
                        let err = self.exp_found("';' after let-declaration");
                        if self.report(err) {
                            return (out, true);
                        }
                        let Some(next) = self.input[self.index..].iter().position(|t| t.kind == TokenKind::Special(SpecialChar::Semicolon)) else {
                            return (out, false);
                        };
                        self.index += next;
                    }
                }
                TokenKind::Comment(..) => {
                    if self.eat_comment(&mut out) {
                        return (out, true);
                    }
                }
                TokenKind::Special(SpecialChar::Semicolon) => self.index += 1,
                _ => {
                    self.index += 1;
                    let ret = self.report(ParseASTError::InvalidTlExpression { span: tok.span });
                    if ret {
                        return (out, true);
                    }
                    let Some(next) = self.input[self.index..].iter().position(|t| t.kind == TokenKind::Special(SpecialChar::Semicolon)) else {
                        return (out, false);
                    };
                    self.index += next;
                }
            }
        }
        (out, false)
    }

    pub fn parse_top_level(&mut self) -> Vec<A::AstBox<'src>> {
        self.parse_top_level_impl().0
    }
}
pub fn parse_expr<
    'src,
    A: AstDefs,
    F: Copy,
    S: SpanConstruct,
    E: ErrorReporter<SourcedError<F, ParseASTError<'src, S>>>,
>(
    input: &[Token<'src, S>],
    file: F,
    mut errs: E,
    _defs: A,
) -> A::AstBox<'src>
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
    let mut parser = Parser::<'src, '_, A, F, S>::new(input, file, &mut errs);
    parser.parse_expr(false, &mut vec![]).0
}
pub fn parse_tl<'src, A: AstDefs, F: Copy, S: SpanConstruct, E: ErrorReporter<SourcedError<F, ParseASTError<'src, S>>>>(
    input: &[Token<'src, S>],
    file: F,
    mut errs: E,
    _defs: A,
) -> asts::FrolicAST<A::AstBox<'src>, F>
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
    let mut parser = Parser::<'src, '_, A, F, S>::new(input, file, &mut errs);
    let nodes = parser.parse_top_level();
    asts::FrolicAST { file, nodes }
}
