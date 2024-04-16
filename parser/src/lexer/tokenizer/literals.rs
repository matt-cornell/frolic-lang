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
                    errs.report(
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
