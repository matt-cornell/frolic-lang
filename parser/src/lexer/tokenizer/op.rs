use super::*;

impl<'src, F: Copy, S: SpanConstruct> Lexer<'src, '_, F, S> {
    pub fn parse_inf_op(&mut self) {
        let start = self.index;
        let len = self.input[(self.index + 1)..].iter().position(|c| !b"$&*%+-/=<>@^|!.:?~".contains(c)).unwrap_or(self.input.len() - self.index - 1) + 1;
        self.index += len;
        self.tokens.push(Token {
            kind: TokenKind::InfOp(unsafe {std::str::from_utf8_unchecked(&self.input[start..self.index])}),
            span: S::new(start, len),
        });
    }
    pub fn parse_pre_op(&mut self) {
        let start = self.index;
        let len = self.input[(self.index + 1)..].iter().position(|c| !b"$&*%+-/=<>@^|".contains(c)).unwrap_or(self.input.len() - self.index - 1) + 1;
        self.index += len;
        self.tokens.push(Token {
            kind: TokenKind::PreOp(unsafe {std::str::from_utf8_unchecked(&self.input[start..self.index])}),
            span: S::new(start, len),
        });
    }
}
