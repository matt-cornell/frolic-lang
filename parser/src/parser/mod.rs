use std::marker::PhantomData;
use super::*;
use error::ParseASTError;
use frolic_ast::prelude::*;

mod error;

struct Parser<'src, 'a, A, F> {
    input: &'a [Token<'src, SourceSpan>],
    index: usize,
    file: F,
    errs: &'a mut dyn ErrorReporter<SourcedError<F, ParseASTError>>,
    _asts: PhantomData<A>,
}
impl<'src, 'a, A: AstDefs, F: Copy> Parser<'src, 'a, A, F> {
    pub fn new(input: &'a [Token<'src, SourceSpan>], file: F, errs: &'a mut dyn ErrorReporter<SourcedError<F, ParseASTError>>) -> Self {
        Self {
            input, file, errs,
            index: 0,
            _asts: PhantomData
        }
    }
    pub fn report(&mut self, err: ParseASTError) -> bool {
        self.errs.report(SourcedError {
            file: self.file,
            error: err
        })
    }
    pub fn parse_top_level(&mut self) -> Vec<A::Box<A::AstTrait<'src>>> where (): AllImplTrait<A::AstTrait<'src>> {
        vec![]
    }
}

pub fn parse<'src, A: AstDefs, F: Copy, E: ErrorReporter<SourcedError<F, ParseASTError>>>(input: &[Token<'src, SourceSpan>], file: F, mut errs: E, _defs: A) -> asts::FrolicAST<A::Box<A::AstTrait<'src>>, F> {
    let mut parser = Parser::<'src, '_, A, F>::new(input, file, &mut errs);
    let nodes = parser.parse_top_level();
    asts::FrolicAST { file, nodes }
}
