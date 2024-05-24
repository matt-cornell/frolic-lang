use super::*;

mod literals;
mod misc;
mod op;

struct Lexer<'src, 'e, F, S: Span> {
    input: &'src [u8],
    index: usize,
    file: F,
    offset: usize,
    tokens: Vec<Token<'src, S>>,
    errs: &'e mut dyn ErrorReporter<SourcedError<F, TokenizeError<S>>>,
}

impl<'src, 'e, F: Copy, S: SpanConstruct> Lexer<'src, 'e, F, S> {
    #[inline]
    pub fn new(
        input: &'src [u8],
        offset: usize,
        file: F,
        errs: &'e mut dyn ErrorReporter<SourcedError<F, TokenizeError<S>>>,
    ) -> Self {
        Self {
            input,
            file,
            offset,
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
                span: S::new(self.offset + off, 1),
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
                    span: S::new(self.index + self.offset, 1),
                });
                self.index += 1;
            }};
        }
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
                '(' => single_char!(TokenKind::Open(Delim::Paren)),
                ')' => single_char!(TokenKind::Close(Delim::Paren)),
                '{' => single_char!(TokenKind::Open(Delim::Brace)),
                '}' => single_char!(TokenKind::Close(Delim::Brace)),
                '[' => single_char!(TokenKind::Open(Delim::Bracket)),
                ']' => single_char!(TokenKind::Close(Delim::Bracket)),
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
                        span: S::new(self.index + self.offset, len),
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
                        span: S::new(self.offset + self.index, 1),
                        found: ch,
                    }) {
                        return;
                    }
                    let _ = self.next_char(false);
                }
            }
        }
    }
}

#[cfg(feature = "rayon")]
fn tokenize_impl<
    F: Copy + Send + Sync,
    S: SpanConstruct + Send,
    E: ErrorReporter<SourcedError<F, TokenizeError<S>>> + Copy + Send + Sync,
>(
    input: &[u8],
    file: F,
    errs: E,
) -> Vec<Token<S>> {
    const STARTS: &[u8] = b"\n\t !$%&(),.:;@~";
    dispatch_chunks(
        input,
        |offset, input| {
            input[offset..]
                .iter()
                .position(|b| STARTS.contains(b))
                .map_or(input.len(), |i| i + offset)
        },
        move |src, offset| {
            let mut errs = errs;
            let mut lex = Lexer::new(src, offset, file, &mut errs);
            lex.tokenize();
            lex.tokens
        },
        1024,
    )
}

#[cfg(feature = "rayon")]
#[inline(never)]
pub fn tokenize<
    I: AsRef<[u8]> + ?Sized,
    F: Copy + Send + Sync,
    S: SpanConstruct + Send,
    E: ErrorReporter<SourcedError<F, TokenizeError<S>>> + Copy + Send + Sync,
>(
    input: &I,
    file: F,
    errs: E,
) -> Vec<Token<S>> {
    tokenize_impl::<F, S, E>(input.as_ref(), file, errs)
}

#[cfg(not(feature = "rayon"))]
pub fn tokenize<
    I: AsRef<[u8]> + ?Sized,
    F: Copy,
    S: SpanConstruct,
    E: ErrorReporter<SourcedError<F, TokenizeError<S>>>,
>(
    input: &I,
    file: F,
    mut errs: E,
) -> Vec<Token<SourceSpan>> {
    let mut lex = Lexer::new(src, offset, file, &mut errs);
    lex.tokenize();
    lex.tokens
}
