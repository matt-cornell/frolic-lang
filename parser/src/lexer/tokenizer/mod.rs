use super::*;

mod literals;
mod misc;

fn next_char<F>(
    input: &[u8],
    index: &mut usize,
    peek: bool,
    file: F,
    offset: usize,
    errs: &mut dyn ErrorReporter<TokenizeError<F>>,
) -> Option<Result<char, bool>> {
    let b = *input.get(*index)?;
    let (res, incr) = match b.leading_ones() {
        0 => (Ok(b as char), 1),
        2 => 'blk: {
            let mut out = ((b & 0x0F) as u32) << 6;
            if let Some(&b) = input.get(*index + 1) {
                if b & 0xB0 == 0x80 {
                    out |= (b & 0x3F) as u32;
                } else {
                    break 'blk (Err((b, *index + 1)), 2);
                }
            } else {
                break 'blk (Err((b, *index)), 1);
            }
            (char::try_from(out).map_err(|_| (b, *index)), 3)
        }
        3 => 'blk: {
            let mut out = ((b & 0x0F) as u32) << 12;
            if let Some(&b) = input.get(*index + 1) {
                if b & 0xB0 == 0x80 {
                    out |= ((b & 0x3F) as u32) << 6;
                } else {
                    break 'blk (Err((b, *index + 1)), 2);
                }
            } else {
                break 'blk (Err((b, *index)), 1);
            }
            if let Some(&b) = input.get(*index + 2) {
                if b & 0xB0 == 0x80 {
                    out |= (b & 0x3F) as u32;
                } else {
                    break 'blk (Err((b, *index + 2)), 3);
                }
            } else {
                break 'blk (Err((b, *index)), 1);
            }
            (char::try_from(out).map_err(|_| (b, *index)), 3)
        }
        4 => 'blk: {
            let mut out = ((b & 0x0F) as u32) << 18;
            if let Some(&b) = input.get(*index + 1) {
                if b & 0xB0 == 0x80 {
                    out |= ((b & 0x3F) as u32) << 12;
                } else {
                    break 'blk (Err((b, *index + 1)), 2);
                }
            } else {
                break 'blk (Err((b, *index)), 1);
            }
            if let Some(&b) = input.get(*index + 2) {
                if b & 0xB0 == 0x80 {
                    out |= ((b & 0x3F) as u32) << 6;
                } else {
                    break 'blk (Err((b, *index + 2)), 3);
                }
            } else {
                break 'blk (Err((b, *index)), 1);
            }
            if let Some(&b) = input.get(*index + 3) {
                if b & 0xB0 == 0x80 {
                    out |= (b & 0x3F) as u32;
                } else {
                    break 'blk (Err((b, *index + 3)), 4);
                }
            } else {
                break 'blk (Err((b, *index)), 1);
            }
            (char::try_from(out).map_err(|_| (b, *index)), 4)
        }
        _ => (Err((b, *index)), 1),
    };

    let start = *index;

    if !peek || res.is_err() {
        *index += incr;
    }

    Some(res.map_err(|(byte, off)| {
        errs.report(TokenizeError {
            file,
            error: TokenizeErrorKind::InvalidUTF8 {
                span: (offset + start + off, 1).into(),
                byte,
            },
        })
    }))
}

fn tokenize_bytes<'src, F: Copy>(
    input: &'src [u8],
    offset: usize,
    file: F,
    errs: &mut dyn ErrorReporter<TokenizeError<F>>,
) -> Vec<Token<'src, SourceSpan>> {
    let mut index = 0;
    let mut tokens = Vec::new();
    while let Some(ch) = next_char(input, &mut index, true, file, offset, errs) {
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
            '+' | '-' if matches!(input.get(index + 1).copied(), Some(b'0'..=b'9')) => {
                literals::parse_num(input, &mut index, offset, &mut tokens, file, errs)
            }
            '0'..='9' => literals::parse_num(input, &mut index, offset, &mut tokens, file, errs),
            '\'' => literals::parse_char(input, &mut index, offset, &mut tokens, file, errs),
            '#' => misc::parse_comment(input, &mut index, offset, &mut tokens, file, errs),
            ch if ch.is_whitespace() => {
                misc::parse_comment(input, &mut index, offset, &mut tokens, file, errs)
            }
            '_' => misc::parse_ident(input, &mut index, offset, &mut tokens, file, errs),
            ch if unicode_ident::is_xid_start(ch) => {
                misc::parse_ident(input, &mut index, offset, &mut tokens, file, errs)
            }
            ch => {
                if errs.report(
                    TokenizeErrorKind::UnexpectedChar {
                        span: (offset + index, 1).into(),
                        found: ch,
                    }
                    .with_src(file),
                ) {
                    return tokens;
                }
            }
        }
    }
    tokens
}

#[cfg(feature = "rayon")]
fn tokenize_impl<'src, F: Copy + Send + Sync, E: Sync>(
    input: &'src [u8],
    file: F,
    errs: E,
) -> Vec<Token<'src, SourceSpan>>
where
    for<'a> &'a E: ErrorReporter<TokenizeError<F>>,
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
        |src, offset| tokenize_bytes(src, offset, file, &mut &errs),
        1024,
    )
}

#[cfg(feature = "rayon")]
pub fn tokenize<'src, S: AsRef<[u8]>, F: Copy + Send + Sync, E: Sync>(
    input: &'src S,
    file: F,
    errs: E,
) -> Vec<Token<'src, SourceSpan>>
where
    for<'a> &'a E: ErrorReporter<TokenizeError<F>>,
{
    tokenize_impl::<F, E>(input.as_ref(), file, errs)
}

#[cfg(not(feature = "rayon"))]
pub fn tokenize<'src, S: AsRef<[u8]>, F, E: ErrorReporter<TokenizeError<F>>>(
    input: &'src S,
    file: F,
    errs: E,
) -> Vec<Token<'src, SourceSpan>> {
    tokenize_bytes(src, offset, file, errs)
}
