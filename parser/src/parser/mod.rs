use super::*;
use error::ParseASTError;
use frolic_ast::prelude::*;
use std::marker::{PhantomData, Unsize};
use traits::*;

mod decl;
mod error;
mod expr;
pub mod traits;

/// Bundle of common state for parser
struct Parser<'src, 'a, A, F> {
    input: &'a [Token<'src, SourceSpan>],
    index: usize,
    file: F,
    errs: &'a mut dyn ErrorReporter<SourcedError<F, ParseASTError<'src>>>,
    _asts: PhantomData<A>,
}
impl<'src, 'a, A: AstDefs, F: Copy> Parser<'src, 'a, A, F>
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
    /// Create a new parser
    pub fn new(
        input: &'a [Token<'src, SourceSpan>],
        file: F,
        errs: &'a mut dyn ErrorReporter<SourcedError<F, ParseASTError<'src>>>,
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
    fn report(&mut self, err: ParseASTError<'src>) -> bool {
        self.errs.report(SourcedError {
            file: self.file,
            error: err,
        })
    }

    fn exp_found(&self, ex: &'static str) -> ParseASTError<'src> {
        let span = self.curr_loc().into();
        let tok = self.current_token();
        ParseASTError::ExpectedFound {
            ex,
            span,
            found_loc: tok.as_ref().map(|t| t.span),
            found: tok.map_or(TokenKind::EMPTY_COMMENT, |t| t.kind.clone()),
        }
    }

    #[inline]
    fn current_token(&self) -> Option<&Token<'src, SourceSpan>> {
        self.input.get(self.index)
    }

    fn curr_span(&self) -> SourceSpan {
        self.input.get(self.index).map_or_else(
            || {
                self.input
                    .last()
                    .map_or(0, |t| t.span.offset() + t.span.len())
                    .into()
            },
            |t| t.span,
        )
    }

    fn curr_loc(&self) -> usize {
        self.input.get(self.index).map_or_else(
            || {
                self.input
                    .last()
                    .map_or(0, |t| t.span.offset() + t.span.len())
            },
            |t| t.span.offset(),
        )
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
    ) -> (Option<(&'src str, SourceSpan)>, bool) {
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
                todo!()
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
                            found_loc: tok.as_ref().map(|t| t.span),
                            found: tok
                                .map_or(TokenKind::Comment(b"".into(), CommentKind::Ignore), |t| {
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
                }
                TokenKind::Comment(..) => {
                    if self.eat_comment(&mut out) {
                        return (out, true);
                    }
                }
                _ => {
                    self.index += 1;
                    let ret = self.report(ParseASTError::InvalidTlExpression { span: tok.span });
                    if ret {
                        return (out, true);
                    }
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
    E: ErrorReporter<SourcedError<F, ParseASTError<'src>>>,
>(
    input: &[Token<'src, SourceSpan>],
    file: F,
    mut errs: E,
    _defs: A,
) -> A::AstBox<'src>
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
    let mut parser = Parser::<'src, '_, A, F>::new(input, file, &mut errs);
    parser.parse_expr(false, &mut vec![]).0
}
pub fn parse_tl<'src, A: AstDefs, F: Copy, E: ErrorReporter<SourcedError<F, ParseASTError<'src>>>>(
    input: &[Token<'src, SourceSpan>],
    file: F,
    mut errs: E,
    _defs: A,
) -> asts::FrolicAST<A::AstBox<'src>, F>
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
    let mut parser = Parser::<'src, '_, A, F>::new(input, file, &mut errs);
    let nodes = parser.parse_top_level();
    asts::FrolicAST { file, nodes }
}
