use super::*;
use crate::lexer::error::LitKind;

#[allow(clippy::too_many_arguments)] // I'll do what I want
fn parse_num_impl<'src, F>(
    start: usize,
    kind: LitKind,
    neg: bool,
    input: &'src [u8],
    index: &mut usize,
    offset: usize,
    tokens: &mut Vec<Token<'src, SourceSpan>>,
    file: F,
    errs: &mut dyn ErrorReporter<TokenizeError<F>>,
) {
    let mut int = 0;
    let mul = if neg { -1 } else { 1 };
    loop {
        let Some(&c) = input.get(*index) else {
            tokens.push(Token {
                kind: TokenKind::Int(int * mul),
                span: ((offset + start)..(offset + *index)).into(),
            });
            return;
        };
        *index += 1;
        match c {
            b'0'..=b'9' => {
                let x = c - b'0';
                if x >= kind as u8 {
                    let _ = errs.report(
                        TokenizeErrorKind::InvalidCharInLit {
                            span: (offset + *index, 1).into(),
                            found: c as _,
                            kind,
                        }
                        .with_src(file),
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
                *index -= 1;
                tokens.push(Token {
                    kind: TokenKind::Int(int * mul),
                    span: ((offset + start)..(offset + *index)).into(),
                });
                return;
            }
        }
    }
    let mut float = int as f64;
    let mut mul = 0.1;
    while let Some(c @ b'0'..=b'9') = input.get(*index) {
        float += (c - b'0') as f64 * mul;
        mul *= 0.1;
    }
    tokens.push(Token {
        kind: TokenKind::Float(float * if neg { -1.0 } else { 1.0 }),
        span: ((offset + start)..(offset + *index)).into(),
    });
}

pub fn parse_num<'src, F>(
    input: &'src [u8],
    index: &mut usize,
    offset: usize,
    tokens: &mut Vec<Token<'src, SourceSpan>>,
    file: F,
    errs: &mut dyn ErrorReporter<TokenizeError<F>>,
) {
    let start = *index;
    let neg = match input[*index] {
        b'+' => {
            *index += 1;
            false
        }
        b'-' => {
            *index += 1;
            true
        }
        _ => false,
    };
    if input[*index] == b'0' {
        *index += 1;
        match input.get(*index) {
            Some(&b'b') => parse_num_impl(
                start,
                LitKind::Binary,
                neg,
                input,
                index,
                offset,
                tokens,
                file,
                errs,
            ),
            Some(&b'o') => parse_num_impl(
                start,
                LitKind::Octal,
                neg,
                input,
                index,
                offset,
                tokens,
                file,
                errs,
            ),
            Some(&b'x') => parse_num_impl(
                start,
                LitKind::Hex,
                neg,
                input,
                index,
                offset,
                tokens,
                file,
                errs,
            ),
            Some(b'0'..=b'9') => parse_num_impl(
                start,
                LitKind::Decimal,
                neg,
                input,
                index,
                offset,
                tokens,
                file,
                errs,
            ),
            _ => tokens.push(Token {
                kind: TokenKind::Int(0),
                span: (offset + start, 1).into(),
            }),
        }
    } else {
        parse_num_impl(
            start,
            LitKind::Decimal,
            neg,
            input,
            index,
            offset,
            tokens,
            file,
            errs,
        )
    }
}

pub fn parse_char<'src, F: Copy>(
    input: &'src [u8],
    index: &mut usize,
    offset: usize,
    tokens: &mut Vec<Token<'src, SourceSpan>>,
    file: F,
    errs: &mut dyn ErrorReporter<TokenizeError<F>>,
) {
    let start = *index;
    let Some(Ok(ch)) = next_char(input, index, false, file, offset, errs) else {
        return;
    };
    if ch != '\'' {
        return;
    }
    let Some(Ok(ch)) = next_char(input, index, false, file, offset, errs) else {
        let _ = errs.report(
            TokenizeErrorKind::UnclosedCharLit {
                span: (start, 1).into(),
                end: *index,
            }
            .with_src(file),
        );
        return;
    };
    let val = match ch {
        '\'' => {
            tokens.push(Token {
                kind: TokenKind::Char(0),
                span: (offset + start, 2).into(),
            });
            return;
        }
        '\\' => {
            if let Some(&b) = input.get(*index) {
                if next_char(input, index, false, file, offset, errs) == Some(Err(true)) {
                    return;
                }
                match b {
                    b'0' => b'\0' as u32,
                    b'n' => b'\n' as u32,
                    b'r' => b'\r' as u32,
                    b't' => b'\t' as u32,
                    b'u' => 'unicode: {
                        match next_char(input, index, false, file, offset, errs) {
                            Some(Err(true)) => return,
                            Some(Err(false)) => break 'unicode 0,
                            Some(Ok('{')) => {
                                let res = std::iter::from_fn(|| {
                                    next_char(input, index, false, file, offset, errs)
                                })
                                .take(6)
                                .take_while(|c| c.map_or(true, |c| c.is_ascii_hexdigit()))
                                .try_fold(0, |out, ch| ch?.to_digit(16).map(|c| (out << 4) | c).ok_or(false));
                                let ch = match res {
                                    Ok(ch) => ch,
                                    Err(true) => return,
                                    Err(false) => 0,
                                };
                                match next_char(input, index, false, file, offset, errs) {
                                    Some(Err(true)) => return,
                                    Some(Err(false)) => break 'unicode 0,
                                    Some(Ok('}')) => {}
                                    Some(Ok(c)) => {
                                        let l = c.len_utf8();
                                        if errs.report(
                                            TokenizeErrorKind::ExpectedUnicodeBrace {
                                                close: true,
                                                span: (*index - l + offset, l).into(),
                                                found: c,
                                            }
                                            .with_src(file),
                                        ) {
                                            return;
                                        } else {
                                            break 'unicode 0;
                                        }
                                    }
                                    None => {
                                        let _ = errs.report(
                                            TokenizeErrorKind::UnclosedCharLit {
                                                span: (start + offset, 1).into(),
                                                end: *index + offset,
                                            }
                                            .with_src(file),
                                        );
                                        return;
                                    }
                                }
                                ch
                            }
                            Some(Ok(c)) => {
                                let l = c.len_utf8();
                                if errs.report(
                                    TokenizeErrorKind::ExpectedUnicodeBrace {
                                        close: false,
                                        span: (*index - l + offset, l).into(),
                                        found: c,
                                    }
                                    .with_src(file),
                                ) {
                                    return;
                                } else {
                                    break 'unicode 0;
                                }
                            }
                            None => {
                                let _ = errs.report(
                                    TokenizeErrorKind::UnclosedCharLit {
                                        span: (start + offset, 1).into(),
                                        end: *index + offset,
                                    }
                                    .with_src(file),
                                );
                                return;
                            }
                        }
                    }
                    _ => {
                        if errs.report(
                            TokenizeErrorKind::UnknownEscapeCode {
                                span: (*index + offset, 1).into(),
                                code: b,
                            }
                            .with_src(file),
                        ) {
                            return;
                        } else {
                            0
                        }
                    }
                }
            } else {
                let _ = errs.report(
                    TokenizeErrorKind::UnclosedCharLit {
                        span: (start + offset, 1).into(),
                        end: *index + offset,
                    }
                    .with_src(file),
                );
                return;
            }
        }
        _ => ch as u32,
    };
    match next_char(input, index, false, file, offset, errs) {
        Some(Err(true)) => return,
        Some(Ok('\'') | Err(false)) => {}
        Some(Ok(_)) | None => {
            let _ = errs.report(
                TokenizeErrorKind::UnclosedCharLit {
                    span: (start + offset, 1).into(),
                    end: *index + offset,
                }
                .with_src(file),
            );
            return;
        }
    }
    tokens.push(Token {
        kind: TokenKind::Char(val),
        span: ((start + offset)..(*index + offset)).into(),
    });
}
