use super::*;
use bstr::ByteSlice;

impl<'src, F: Copy, S: SpanConstruct> Lexer<'src, '_, F, S> {
    fn parse_num_impl(&mut self, start: usize, kind: LitKind, neg: bool) -> bool {
        if kind != LitKind::Decimal {
            self.index += 1;
        }
        let mut int = 0;
        let mul = if neg { -1 } else { 1 };
        let base = kind as u8 as i128;
        loop {
            let Some(&c) = self.input.get(self.index) else {
                self.tokens.push(Token {
                    kind: TokenKind::Int(int * mul),
                    span: S::range(self.offset + start, self.offset + self.index),
                });
                return false;
            };
            self.index += 1;
            match c {
                b'0'..=b'9' => {
                    let x = c - b'0';
                    if x >= kind as u8 {
                        return self.report(TokenizeError::InvalidCharInLit {
                            span: S::new(self.offset + self.index, 1),
                            found: c as _,
                            kind,
                        });
                    }
                    int *= base;
                    int += x as i128;
                }
                b'a'..=b'f' if kind == LitKind::Hex => {
                    int *= 16;
                    int += 10;
                    int += (c - b'a') as i128;
                }
                b'A'..=b'F' if kind == LitKind::Hex => {
                    int *= 16;
                    int += 10;
                    int += (c - b'A') as i128;
                }
                b'.' if kind == LitKind::Decimal => break,
                _ => {
                    self.index -= 1;
                    self.tokens.push(Token {
                        kind: TokenKind::Int(int * mul),
                        span: S::range(self.offset + start, self.offset + self.index),
                    });
                    return false;
                }
            }
        }
        let mut float = int as f64;
        let mut mul = 0.1;
        while let Some(c @ b'0'..=b'9') = self.input.get(self.index) {
            float += (c - b'0') as f64 * mul;
            mul *= 0.1;
            self.index += 1;
        }
        self.tokens.push(Token {
            kind: TokenKind::Float(float * if neg { -1.0 } else { 1.0 }),
            span: S::range(self.offset + start, self.offset + self.index),
        });
        false
    }

    pub fn parse_num(&mut self) -> bool {
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
                Some(&b'b') => self.parse_num_impl(start, LitKind::Binary, neg),
                Some(&b'o') => self.parse_num_impl(start, LitKind::Octal, neg),
                Some(&b'x') => self.parse_num_impl(start, LitKind::Hex, neg),
                Some(b'0'..=b'9') => self.parse_num_impl(start, LitKind::Decimal, neg),
                _ => {
                    self.tokens.push(Token {
                        kind: TokenKind::Int(0),
                        span: S::new(self.offset + start, 1),
                    });
                    false
                }
            }
        } else {
            self.parse_num_impl(start, LitKind::Decimal, neg)
        }
    }

    pub fn parse_char(&mut self) -> bool {
        let start = self.index;
        let ch = match self.next_char(false) {
            Some(Ok(ch)) => ch,
            Some(Err(true)) => return true,
            _ => return false,
        };
        if ch != '\'' {
            return false;
        }
        let ch = match self.next_char(false) {
            Some(Ok(ch)) => ch,
            Some(Err(ret)) => return ret,
            None => {
                return self.report(TokenizeError::UnclosedCharLit {
                    span: S::new(start + self.offset, 1),
                    end: self.index + self.offset,
                })
            }
        };
        let val = match ch {
            '\'' => {
                self.tokens.push(Token {
                    kind: TokenKind::Char(0),
                    span: S::new(start + self.offset, 2),
                });
                return false;
            }
            '\\' => {
                if let Some(&b) = self.input.get(self.index) {
                    if self.next_char(false) == Some(Err(true)) {
                        return true;
                    }
                    match b {
                        b'0' => b'\0' as u32,
                        b'n' => b'\n' as u32,
                        b'r' => b'\r' as u32,
                        b't' => b'\t' as u32,
                        b'u' => 'unicode: {
                            match self.next_char(false) {
                                Some(Err(true)) => return true,
                                Some(Err(false)) => break 'unicode 0,
                                Some(Ok('{')) => {
                                    let mut last = '}';
                                    let res = std::iter::from_fn(|| self.next_char(false))
                                        .take(7)
                                        .take_while(|c| {
                                            c.map_or(true, |c| {
                                                last = c;
                                                c.is_ascii_hexdigit()
                                            })
                                        })
                                        .try_fold(0, |out, ch| {
                                            ch?.to_digit(16).map(|c| (out << 4) | c).ok_or(false)
                                        });
                                    let ch = match res {
                                        Ok(ch) => ch,
                                        Err(true) => return true,
                                        Err(false) => 0,
                                    };
                                    if last != '}' {
                                        let l = last.len_utf8();
                                        if self.report(TokenizeError::ExpectedUnicodeBrace {
                                            close: true,
                                            span: S::new(self.index - l + self.offset, l),
                                            found: last,
                                        }) {
                                            return true;
                                        } else {
                                            break 'unicode 0;
                                        }
                                    }
                                    ch
                                }
                                Some(Ok(c)) => {
                                    let l = c.len_utf8();
                                    if self.report(TokenizeError::ExpectedUnicodeBrace {
                                        close: false,
                                        span: S::new(self.index - l + self.offset, l),
                                        found: c,
                                    }) {
                                        return true;
                                    } else {
                                        break 'unicode 0;
                                    }
                                }
                                None => {
                                    return self.report(TokenizeError::UnclosedCharLit {
                                        span: S::new(start + self.offset, 1),
                                        end: self.index + self.offset,
                                    });
                                }
                            }
                        }
                        _ => {
                            if self.report(TokenizeError::UnknownEscapeCode {
                                span: S::new(self.index + self.offset - 1, 1),
                                code: b,
                            }) {
                                return true;
                            } else {
                                0
                            }
                        }
                    }
                } else {
                    return self.report(TokenizeError::UnclosedCharLit {
                        span: S::new(start + self.offset, 1),
                        end: self.index + self.offset,
                    });
                }
            }
            _ => ch as u32,
        };
        match self.next_char(false) {
            Some(Err(true)) => return true,
            Some(Ok('\'') | Err(false)) => {}
            Some(Ok(_)) | None => {
                return self.report(TokenizeError::UnclosedCharLit {
                    span: S::new(start + self.offset, 1),
                    end: self.index + self.offset,
                });
            }
        }
        self.tokens.push(Token {
            kind: TokenKind::Char(val),
            span: S::range(start + self.offset, self.index + self.offset),
        });
        false
    }

    pub fn parse_str(&mut self) -> bool {
        let start = self.index;
        let ch = match self.next_char(false) {
            Some(Ok(ch)) => ch,
            Some(Err(true)) => return true,
            _ => return false,
        };
        if ch != '\"' {
            return false;
        }
        let mut out = Cow::Borrowed(&[][..]);
        let mut last = self.index;
        loop {
            if let Some(idx) = self.input[self.index..].find_byteset(b"\\\"") {
                self.index += idx;
                if idx > 0 {
                    let slice = &self.input[last..self.index];
                    if out.is_empty() {
                        out = slice.into();
                    } else {
                        out.to_mut().extend_from_slice(slice);
                    }
                }
                if self.input[self.index] == b'"' {
                    self.index += 1;
                    break;
                }
                self.index += 1;
                let Some(&b) = self.input.get(self.index) else {
                    continue;
                };
                self.index += 1;
                match b {
                    b'0' => out.to_mut().push(b'\0'),
                    b'n' => out.to_mut().push(b'\n'),
                    b'r' => out.to_mut().push(b'\r'),
                    b't' => out.to_mut().push(b'\t'),
                    b'u' => match self.next_char(false) {
                        Some(Err(true)) => return true,
                        Some(Err(false)) => {}
                        Some(Ok('{')) => {
                            let mut last = '}';
                            let res = std::iter::from_fn(|| self.next_char(false))
                                .take(7)
                                .take_while(|c| {
                                    c.map_or(true, |c| {
                                        last = c;
                                        c.is_ascii_hexdigit()
                                    })
                                })
                                .try_fold(0, |out, ch| {
                                    ch?.to_digit(16).map(|c| (out << 4) | c).ok_or(false)
                                });
                            let ch = match res {
                                Ok(ch) => ch,
                                Err(true) => return true,
                                Err(false) => 0,
                            };
                            if last != '}' {
                                let l = last.len_utf8();
                                if self.report(TokenizeError::ExpectedUnicodeBrace {
                                    close: true,
                                    span: S::new(self.index - l + self.offset, l),
                                    found: last,
                                }) {
                                    return true;
                                }
                            }
                            out.to_mut()
                                .extend(CharBytesIterator::from_u32(ch).unwrap());
                        }
                        Some(Ok(c)) => {
                            let l = c.len_utf8();
                            if self.report(TokenizeError::ExpectedUnicodeBrace {
                                close: false,
                                span: S::new(self.index - l + self.offset, l),
                                found: c,
                            }) {
                                return true;
                            }
                        }
                        None => continue,
                    },
                    _ => {
                        if self.report(TokenizeError::UnknownEscapeCode {
                            span: S::new(self.index + self.offset - 1, 1),
                            code: b,
                        }) {
                            return true;
                        }
                    }
                }
                last = self.index;
            } else {
                self.tokens.push(Token {
                    kind: TokenKind::String(out),
                    span: S::range(start + self.offset, self.index + self.offset),
                });
                return self.report(TokenizeError::UnclosedStrLit {
                    span: S::new(start + self.offset, 1),
                    end: self.index + self.offset,
                });
            }
        }
        self.tokens.push(Token {
            kind: TokenKind::String(out),
            span: S::range(start + self.offset, self.index + self.offset),
        });
        false
    }
}
