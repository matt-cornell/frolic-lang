use super::*;

mod literals;
mod misc;

struct Lexer<'src, 'e, F> {
    input: &'src [u8],
    index: usize,
    file: F,
    offset: usize,
    tokens: Vec<Token<'src, SourceSpan>>,
    errs: &'e mut dyn ErrorReporter<SourcedError<F, TokenizeError>>,
}

impl<'src, 'e, F: Copy> Lexer<'src, 'e, F> {
    #[inline]
    pub fn new(
        input: &'src [u8],
        offset: usize,
        file: F,
        errs: &'e mut dyn ErrorReporter<SourcedError<F, TokenizeError>>,
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
                span: (self.offset + off, 1).into(),
                byte,
            })
        }))
    }

    #[inline]
    fn report(&mut self, err: TokenizeError) -> bool {
        self.errs.report(SourcedError {
            error: err,
            file: self.file,
        })
    }

    fn tokenize(&mut self) {
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
                '+' | '-'
                    if matches!(self.input.get(self.index + 1).copied(), Some(b'0'..=b'9')) =>
                {
                    self.parse_num()
                }
                '0'..='9' => self.parse_num(),
                '\'' => self.parse_char(),
                '"' => self.parse_str(),
                '#' => self.parse_comment(),
                ch if ch.is_whitespace() => self.parse_comment(),
                '_' => self.parse_ident(),
                ch if unicode_ident::is_xid_start(ch) => self.parse_ident(),
                ch => {
                    if self.report(TokenizeError::UnexpectedChar {
                        span: (self.offset + self.index, 1).into(),
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
fn tokenize_impl<'src, F: Copy + Send + Sync, E: Sync>(
    input: &'src [u8],
    file: F,
    errs: E,
) -> Vec<Token<'src, SourceSpan>>
where
    for<'a> &'a E: ErrorReporter<SourcedError<F, TokenizeError>>,
{
    const STARTS: &[u8] = b"\n\t !$%&(),.:;@~";
    dispatch_chunks(
        input,
        |offset, input| {
            input[offset..]
                .iter()
                .position(|b| STARTS.contains(b))
                .map_or(input.len(), |i| i + offset)
        },
        |src, offset| {
            let mut r = &errs;
            let mut lex = Lexer::new(src, offset, file, &mut r);
            lex.tokenize();
            lex.tokens
        },
        1024,
    )
}

#[cfg(feature = "rayon")]
#[inline(never)]
pub fn tokenize<'src, S: AsRef<[u8]> + ?Sized, F: Copy + Send + Sync, E: Sync>(
    input: &'src S,
    file: F,
    errs: E,
) -> Vec<Token<'src, SourceSpan>>
where
    for<'a> &'a E: ErrorReporter<SourcedError<F, TokenizeError>>,
{
    tokenize_impl::<F, E>(input.as_ref(), file, errs)
}

#[cfg(not(feature = "rayon"))]
pub fn tokenize<
    'src,
    S: AsRef<[u8]> + ?Sized,
    F,
    E: ErrorReporter<SourcedError<F, TokenizeError>>,
>(
    input: &'src S,
    file: F,
    errs: E,
) -> Vec<Token<'src, SourceSpan>> {
    let mut lex = Lexer::new(src, offset, file, &mut errs);
    lex.tokenize();
    lex.tokens
}
