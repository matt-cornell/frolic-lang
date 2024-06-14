use super::*;

mod literals;
mod misc;
mod op;

struct Lexer<'src, 'e, F, S: Span> {
    input: &'src [u8],
    index: usize,
    file: F,
    tokens: Vec<Token<'src, S>>,
    errs: &'e mut dyn ErrorReporter<SourcedError<F, TokenizeError<S>>>,
}

impl<'src, 'e, F: Copy, S: SpanConstruct> Lexer<'src, 'e, F, S> {
    #[inline]
    pub fn new(
        input: &'src [u8],
        file: F,
        errs: &'e mut dyn ErrorReporter<SourcedError<F, TokenizeError<S>>>,
    ) -> Self {
        Self {
            input,
            file,
            errs,
            index: 0,
            tokens: vec![],
        }
    }

    fn next_char(&mut self, peek: bool) -> Option<Result<char, bool>> {
        let b = *self.input.get(self.index)?;
        let (res, incr) = match b.leading_ones() {
            0 => (Ok(b as char), 1),
            2 => 'blk: {
                let mut out = ((b & 0x0F) as u32) << 6;
                if let Some(&b) = self.input.get(self.index + 1) {
                    if b & 0xB0 == 0x80 {
                        out |= (b & 0x3F) as u32;
                    } else {
                        break 'blk (Err((b, self.index + 1)), 2);
                    }
                } else {
                    break 'blk (Err((b, self.index)), 1);
                }
                (char::try_from(out).map_err(|_| (b, self.index)), 3)
            }
            3 => 'blk: {
                let mut out = ((b & 0x0F) as u32) << 12;
                if let Some(&b) = self.input.get(self.index + 1) {
                    if b & 0xB0 == 0x80 {
                        out |= ((b & 0x3F) as u32) << 6;
                    } else {
                        break 'blk (Err((b, self.index + 1)), 2);
                    }
                } else {
                    break 'blk (Err((b, self.index)), 1);
                }
                if let Some(&b) = self.input.get(self.index + 2) {
                    if b & 0xB0 == 0x80 {
                        out |= (b & 0x3F) as u32;
                    } else {
                        break 'blk (Err((b, self.index + 2)), 3);
                    }
                } else {
                    break 'blk (Err((b, self.index)), 1);
                }
                (char::try_from(out).map_err(|_| (b, self.index)), 3)
            }
            4 => 'blk: {
                let mut out = ((b & 0x0F) as u32) << 18;
                if let Some(&b) = self.input.get(self.index + 1) {
                    if b & 0xB0 == 0x80 {
                        out |= ((b & 0x3F) as u32) << 12;
                    } else {
                        break 'blk (Err((b, self.index + 1)), 2);
                    }
                } else {
                    break 'blk (Err((b, self.index)), 1);
                }
                if let Some(&b) = self.input.get(self.index + 2) {
                    if b & 0xB0 == 0x80 {
                        out |= ((b & 0x3F) as u32) << 6;
                    } else {
                        break 'blk (Err((b, self.index + 2)), 3);
                    }
                } else {
                    break 'blk (Err((b, self.index)), 1);
                }
                if let Some(&b) = self.input.get(self.index + 3) {
                    if b & 0xB0 == 0x80 {
                        out |= (b & 0x3F) as u32;
                    } else {
                        break 'blk (Err((b, self.index + 3)), 4);
                    }
                } else {
                    break 'blk (Err((b, self.index)), 1);
                }
                (char::try_from(out).map_err(|_| (b, self.index)), 4)
            }
            _ => (Err((b, self.index)), 1),
        };

        if !peek || res.is_err() {
            self.index += incr;
        }

        Some(res.map_err(|(byte, off)| {
            self.report(TokenizeError::InvalidUTF8 {
                span: S::new(off, 1),
                byte,
            })
        }))
    }

    #[inline]
    fn report(&mut self, err: TokenizeError<S>) -> bool {
        self.errs.report(SourcedError {
            error: err,
            file: self.file,
        })
    }

    fn tokenize(&mut self) {
        macro_rules! single_char {
            ($tok:expr) => {{
                self.tokens.push(Token {
                    kind: $tok,
                    span: S::new(self.index, 1),
                });
                self.index += 1;
            }};
        }
        let mut group_stack = Vec::new();
        while let Some(ch) = self.next_char(true) {
            let ch = match ch {
                Ok(ch) => ch,
                Err(ret) => {
                    if ret {
                        break;
                    } else {
                        continue;
                    }
                }
            };
            match ch {
                '\\' => single_char!(TokenKind::Special(SpecialChar::Backslash)),
                ';' => single_char!(TokenKind::Special(SpecialChar::Semicolon)),
                '.' => single_char!(TokenKind::Special(SpecialChar::Dot)),
                ',' => single_char!(TokenKind::Special(SpecialChar::Comma)),
                '(' | '{' | '[' => {
                    let tok_idx = self.tokens.len();
                    group_stack.push((Delim::try_from(ch).unwrap(), tok_idx, self.index));
                    self.index += 1;
                }
                ')' | '}' | ']' => {
                    self.index += 1;
                    let kind = Delim::try_from(ch).unwrap();
                    if let Some((delim, tok_idx, src_idx)) = group_stack.pop() {
                        if delim == kind {
                            let toks = self.tokens.split_off(tok_idx);
                            self.tokens.push(Token {
                                kind: match kind {
                                    Delim::Paren => TokenKind::Paren(toks),
                                    Delim::Brace => TokenKind::Brace(toks),
                                    Delim::Bracket => TokenKind::Bracket(toks),
                                },
                                span: S::range(src_idx, self.index),
                            });
                            continue;
                        } else {
                            if let Some(&(delim, tok_idx, src_idx)) = group_stack.last() {
                                if delim == kind {
                                    group_stack.pop();
                                    if self.report(TokenizeError::UnmatchedOpenDelim {
                                        kind: delim,
                                        span: S::loc(self.index),
                                        prev: S::new(src_idx, 1),
                                    }) {
                                        return;
                                    }
                                    let toks = self.tokens.split_off(tok_idx);
                                    self.tokens.push(Token {
                                        kind: match kind {
                                            Delim::Paren => TokenKind::Paren(toks),
                                            Delim::Brace => TokenKind::Brace(toks),
                                            Delim::Bracket => TokenKind::Bracket(toks),
                                        },
                                        span: S::range(src_idx, self.index),
                                    });
                                    continue;
                                }
                            }
                        }
                    }
                    if self.report(TokenizeError::UnmatchedCloseDelim {
                        kind,
                        span: S::new(self.index - 1, 1),
                    }) {
                        return;
                    }
                }
                '=' => {
                    if self
                        .input
                        .get(self.index + 1)
                        .map_or(false, |c| b"$&*%+-/=<>@^|!.:?~".contains(c))
                    {
                        self.parse_inf_op();
                    } else {
                        single_char!(TokenKind::Special(SpecialChar::Equals))
                    }
                }
                ':' => {
                    let (ch, len) = if self.input.get(self.index + 1) == Some(&b':') {
                        (SpecialChar::DoubleColon, 2)
                    } else {
                        (SpecialChar::Colon, 1)
                    };
                    self.tokens.push(Token {
                        kind: TokenKind::Special(ch),
                        span: S::new(self.index, len),
                    });
                    self.index += len;
                }
                '+' | '-' => match self.input.get(self.index + 1).copied() {
                    Some(b'0'..=b'9') => {
                        if self.parse_num() {
                            return;
                        }
                    }
                    Some(
                        b'$' | b'&' | b'*' | b'%' | b'+' | b'-' | b'/' | b'=' | b'<' | b'>' | b'@'
                        | b'^' | b'|',
                    ) => self.parse_inf_op(),
                    _ => single_char!(TokenKind::AmbigOp(if self.input[self.index] == b'+' {
                        AmbigOp::Plus
                    } else {
                        AmbigOp::Minus
                    })),
                },
                '*' | '&' => {
                    if self
                        .input
                        .get(self.index + 1)
                        .map_or(false, |c| b"$&*%+-/=<>@^|!.:?~".contains(c))
                    {
                        self.parse_inf_op();
                    } else {
                        single_char!(TokenKind::AmbigOp(if self.input[self.index] == b'*' {
                            AmbigOp::Star
                        } else {
                            AmbigOp::And
                        }))
                    }
                }
                '$' | '<' | '>' | '@' | '^' | '|' | '%' => self.parse_inf_op(),
                '?' | '~' | '!' => self.parse_pre_op(),
                '0'..='9' => {
                    if self.parse_num() {
                        return;
                    }
                }
                '\'' => {
                    if self.parse_char() {
                        return;
                    }
                }
                '"' => {
                    if self.parse_str() {
                        return;
                    }
                }
                '#' => {
                    if self.parse_comment() {
                        return;
                    }
                }
                ch if ch.is_whitespace() => {
                    if self.parse_comment() {
                        return;
                    }
                }
                '_' => self.parse_ident(),
                ch if unicode_ident::is_xid_start(ch) => self.parse_ident(),
                ch => {
                    if self.report(TokenizeError::UnexpectedChar {
                        span: S::new(self.index, 1),
                        found: ch,
                    }) {
                        return;
                    }
                    let _ = self.next_char(false);
                }
            }
        }
        for (kind, _, src_idx) in group_stack.into_iter().rev() {
            if self.report(TokenizeError::UnmatchedOpenDelim {
                kind,
                span: S::loc(self.index),
                prev: S::new(src_idx, 1),
            }) {
                return;
            }
        }
    }
}

pub fn tokenize<
    I: AsRef<[u8]> + ?Sized,
    F: Copy,
    S: SpanConstruct,
    E: ErrorReporter<SourcedError<F, TokenizeError<S>>>,
>(
    input: &I,
    file: F,
    mut errs: E,
) -> Vec<Token<S>> {
    let mut lex = Lexer::new(input.as_ref(), file, &mut errs);
    lex.tokenize();
    lex.tokens
}
