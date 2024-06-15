use super::*;

impl<'src, F: Copy, S: SpanConstruct> Lexer<'src, '_, F, S> {
    pub fn parse_inf_op(&mut self) {
        let start = self.index;
        let len = self.input[(self.index + 1)..]
            .iter()
            .position(|c| !b"$&*%+-/=<>@^|!.:?~".contains(c))
            .unwrap_or(self.input.len() - self.index - 1)
            + 1;
        self.index += len;
        let op = unsafe { std::str::from_utf8_unchecked(&self.input[start..self.index]) };
        self.push_token(Token {
            kind: if op == "->" {
                TokenKind::Special(SpecialChar::Arrow)
            } else {
                TokenKind::InfOp(op)
            },
            span: S::new(start, len),
        });
    }
    pub fn parse_pre_op(&mut self) {
        let start = self.index;
        let len = self.input[(self.index + 1)..]
            .iter()
            .position(|c| !b"$&*%+-/=<>@^|!.:?~".contains(c))
            .unwrap_or(self.input.len() - self.index - 1)
            + 1;
        self.index += len;
        self.push_token(Token {
            kind: TokenKind::PreOp(unsafe {
                std::str::from_utf8_unchecked(&self.input[start..self.index])
            }),
            span: S::new(start, len),
        });
    }
    // starts after the "let".
    pub fn parse_let_op(&mut self) {
        let start = self.index - 3;
        let len = self.input[self.index..]
            .iter()
            .position(|c| !b"$&*%+-/=<>@^|!.:?~".contains(c))
            .unwrap_or(self.input.len() - self.index);
        if len == 0 {
            self.push_token(Token {
                kind: TokenKind::Keyword(Keyword::Let),
                span: S::new(start, 3),
            });
        } else {
            self.index += len;
            self.push_token(Token {
                kind: TokenKind::LetOp(unsafe {
                    std::str::from_utf8_unchecked(&self.input[start..self.index])
                }),
                span: S::new(start, len + 3),
            });
        }
    }
}
