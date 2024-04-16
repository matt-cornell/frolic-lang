use super::*;
use crate::lexer::error::LitKind;

impl<'src, F: Copy> Lexer<'src, '_, F> {
    fn parse_num_impl(&mut self, start: usize, kind: LitKind, neg: bool) {
        let mut int = 0;
        let mul = if neg { -1 } else { 1 };
        loop {
            let Some(&c) = self.input.get(self.index) else {
                self.tokens.push(Token {
                    kind: TokenKind::Int(int * mul),
                    span: ((self.offset + start)..(self.offset + self.index)).into(),
                });
                return;
            };
            self.index += 1;
            match c {
                b'0'..=b'9' => {
                    let x = c - b'0';
                    if x >= kind as u8 {
                        let _ = self.report(
                            TokenizeErrorKind::InvalidCharInLit {
                                span: (self.offset + self.index, 1).into(),
                                found: c as _,
                                kind,
                            }
                        );
                        return;
                    }
                    int *= 10;
                    int += x as i128;
                }
                b'a'..=b'f' if kind == LitKind::Hex => {
                    int *= 10;
                    int += 10;
                    int += (c - b'a') as i128;
                }
                b'A'..=b'F' if kind == LitKind::Hex => {
                    int *= 10;
                    int += 10;
                    int += (c - b'A') as i128;
                }
                b'.' if kind == LitKind::Decimal => break,
                _ => {
                    self.index -= 1;
                    self.tokens.push(Token {
                        kind: TokenKind::Int(int * mul),
                        span: ((self.offset + start)..(self.offset + self.index)).into(),
                    });
                    return;
                }
            }
        }
        let mut float = int as f64;
        let mut mul = 0.1;
        while let Some(c @ b'0'..=b'9') = self.input.get(self.index) {
            float += (c - b'0') as f64 * mul;
            mul *= 0.1;
        }
        self.tokens.push(Token {
            kind: TokenKind::Float(float * if neg { -1.0 } else { 1.0 }),
            span: ((self.offset + start)..(self.offset + self.index)).into(),
        });
    }

    pub fn parse_num(&mut self) {
        let start = self.index;
        let neg = match self.input[self.index] {
            b'+' => {
                self.index += 1;
                false
            }
            b'-' => {
                self.index += 1;
                true
            }
            _ => false,
        };
        if self.input[self.index] == b'0' {
            self.index += 1;
            match self.input.get(self.index) {
                Some(&b'b') => self.parse_num_impl(
                    start,
                    LitKind::Binary,
                    neg,
                ),
                Some(&b'o') => self.parse_num_impl(
                    start,
                    LitKind::Octal,
                    neg,
                ),
                Some(&b'x') => self.parse_num_impl(
                    start,
                    LitKind::Hex,
                    neg,
                ),
                Some(b'0'..=b'9') => self.parse_num_impl(
                    start,
                    LitKind::Decimal,
                    neg,
                ),
                _ => self.tokens.push(Token {
                    kind: TokenKind::Int(0),
                    span: (self.offset + start, 1).into(),
                }),
            }
        } else {
            self.parse_num_impl(
                start,
                LitKind::Decimal,
                neg,
            )
        }
    }

    pub fn parse_char(&mut self) {
        let start = self.index;
        let Some(Ok(ch)) = self.next_char(false) else {
            return;
        };
        if ch != '\'' {
            return;
        }
        let Some(Ok(ch)) = self.next_char(false) else {
            let _ = self.report(
                TokenizeErrorKind::UnclosedCharLit {
                    span: (start + self.offset, 1).into(),
                    end: self.index + self.offset,
                }
            );
            return;
        };
        let val = match ch {
            '\'' => {
                self.tokens.push(Token {
                    kind: TokenKind::Char(0),
                    span: (start + self.offset, 2).into(),
                });
                return;
            }
            '\\' => {
                if let Some(&b) = self.input.get(self.index) {
                    if self.next_char(false) == Some(Err(true)) {
                        return;
                    }
                    match b {
                        b'0' => b'\0' as u32,
                        b'n' => b'\n' as u32,
                        b'r' => b'\r' as u32,
                        b't' => b'\t' as u32,
                        b'u' => 'unicode: {
                            match self.next_char(false) {
                                Some(Err(true)) => return,
                                Some(Err(false)) => break 'unicode 0,
                                Some(Ok('{')) => {
                                    let mut last = '}';
                                    let res = std::iter::from_fn(|| self.next_char(false))
                                    .take(6)
                                    .take_while(|c| c.map_or(true, |c| {
                                        last = c;
                                        c.is_ascii_hexdigit()
                                    }))
                                    .try_fold(0, |out, ch| {
                                        ch?.to_digit(16).map(|c| (out << 4) | c).ok_or(false)
                                    });
                                    let ch = match res {
                                        Ok(ch) => ch,
                                        Err(true) => return,
                                        Err(false) => 0,
                                    };
                                    if last != '}' {
                                        let l = last.len_utf8();
                                        if self.report(
                                            TokenizeErrorKind::ExpectedUnicodeBrace {
                                                close: true,
                                                span: (self.index - l + self.offset, l).into(),
                                                found: last,
                                            }
                                        ) {
                                            return;
                                        } else {
                                            break 'unicode 0;
                                        }
                                    }
                                    ch
                                }
                                Some(Ok(c)) => {
                                    let l = c.len_utf8();
                                    if self.report(
                                        TokenizeErrorKind::ExpectedUnicodeBrace {
                                            close: false,
                                            span: (self.index - l + self.offset, l).into(),
                                            found: c,
                                        }
                                    ) {
                                        return;
                                    } else {
                                        break 'unicode 0;
                                    }
                                }
                                None => {
                                    let _ = self.report(
                                        TokenizeErrorKind::UnclosedCharLit {
                                            span: (start + self.offset, 1).into(),
                                            end: self.index + self.offset,
                                        }
                                    );
                                    return;
                                }
                            }
                        }
                        _ => {
                            if self.report(
                                TokenizeErrorKind::UnknownEscapeCode {
                                    span: (self.index + self.offset, 1).into(),
                                    code: b,
                                }
                            ) {
                                return;
                            } else {
                                0
                            }
                        }
                    }
                } else {
                    let _ = self.report(
                        TokenizeErrorKind::UnclosedCharLit {
                            span: (start + self.offset, 1).into(),
                            end: self.index + self.offset,
                        }
                    );
                    return;
                }
            }
            _ => ch as u32,
        };
        match self.next_char(false) {
            Some(Err(true)) => return,
            Some(Ok('\'') | Err(false)) => {}
            Some(Ok(_)) | None => {
                let _ = self.report(
                    TokenizeErrorKind::UnclosedCharLit {
                        span: (start + self.offset, 1).into(),
                        end: self.index + self.offset,
                    }
                );
                return;
            }
        }
        self.tokens.push(Token {
            kind: TokenKind::Char(val),
            span: ((start + self.offset)..(self.index + self.offset)).into(),
        });
    }
}
