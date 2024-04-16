use super::*;

fn eat_comment<'src>(
    this: CommentKind,
    data: &mut Option<(usize, usize, CommentKind)>,
    comment: &mut Cow<'src, [u8]>,
    input: &'src [u8],
    index: &mut usize,
    offset: usize,
    tokens: &mut Vec<Token<'src, SourceSpan>>,
) {
    let start = *index;
    let line_end = input[start..]
        .iter()
        .position(|&i| i == b'\n')
        .unwrap_or(input.len() - start)
        + start;
    if line_end != start {
        *index = line_end + 1;
    }
    let slice = &input[start..line_end];
    if let Some((start, end, kind)) = data {
        if *kind == this {
            if comment.is_empty() {
                *comment = slice.into();
            } else if !slice.is_empty() {
                let comm = comment.to_mut();
                comm.push(b'\n');
                comm.extend_from_slice(slice);
            }
            return;
        } else {
            let comment = std::mem::replace(comment, Cow::Borrowed(&[]));
            tokens.push(Token {
                kind: TokenKind::Comment(comment, *kind),
                span: ((*start + offset - usize::from(this != CommentKind::Ignore))
                    ..(*end + offset))
                    .into(),
            });
        }
    }
    *data = Some((start, *index, this));
    *comment = slice.into();
}

pub fn parse_comment<'src, F: Copy>(
    input: &'src [u8],
    index: &mut usize,
    offset: usize,
    tokens: &mut Vec<Token<'src, SourceSpan>>,
    file: F,
    errs: &mut dyn ErrorReporter<TokenizeError<F>>,
) {
    let mut comment = Cow::Borrowed(&[][..]);
    let mut data = None;
    while let Some(ch) = next_char(input, index, true) {
        match ch {
            Ok('#') => match input.get(*index + 1) {
                Some(&b'#') => {
                    *index += 1;
                    eat_comment(
                        CommentKind::OuterDoc,
                        &mut data,
                        &mut comment,
                        input,
                        index,
                        offset,
                        tokens,
                    )
                }
                Some(&b'!') => {
                    *index += 1;
                    eat_comment(
                        CommentKind::InnerDoc,
                        &mut data,
                        &mut comment,
                        input,
                        index,
                        offset,
                        tokens,
                    )
                }
                Some(&b'=') => {
                    let start = *index;
                    let len = input[(start + 1)..]
                        .iter()
                        .position(|&c| c != b'=')
                        .unwrap_or(input.len() - start);
                    *index += len;

                    let mut remaining = &input[*index..];

                    let valid = loop {
                        let Some(found) = remaining.iter().position(|&c| c == b'=') else {
                            break false;
                        };
                        remaining = &remaining[found..];
                        *index += found;
                        let Some(count) = remaining.iter().position(|&c| c != b'=') else {
                            break false;
                        };
                        remaining = &remaining[count..];
                        *index += count;
                        if count < len {
                            continue;
                        }
                        if remaining[0] == b'#' {
                            *index += 1;
                            break true;
                        }
                    };
                    if !valid
                        && errs.report(
                            TokenizeErrorKind::UnclosedMultiline {
                                span: (start + offset, len + 1).into(),
                                end: input.len() + offset,
                            }
                            .with_src(file),
                        )
                    {
                        return;
                    }
                    let slice = &input[start..*index];
                    'blk: {
                        if let Some((start, end, kind)) = data {
                            if kind == CommentKind::Ignore {
                                if comment.is_empty() {
                                    comment = slice.into();
                                } else if !slice.is_empty() {
                                    let comm = comment.to_mut();
                                    comm.push(b'\n');
                                    comm.extend_from_slice(slice);
                                }
                                break 'blk;
                            } else {
                                let comment = std::mem::replace(&mut comment, Cow::Borrowed(&[]));
                                tokens.push(Token {
                                    kind: TokenKind::Comment(comment, kind),
                                    span: ((start + offset)..(end + offset)).into(),
                                });
                            }
                        }
                        data = Some((start, *index, CommentKind::Ignore));
                        comment = slice.into();
                    }
                }
                _ => eat_comment(
                    CommentKind::Ignore,
                    &mut data,
                    &mut comment,
                    input,
                    index,
                    offset,
                    tokens,
                ),
            },
            Ok(ch) => {
                if !ch.is_whitespace() {
                    *index -= ch.len_utf8();
                    break;
                }
            }
            Err((b, idx)) => {
                if errs.report(
                    TokenizeErrorKind::InvalidUTF8 {
                        span: (offset + idx, 1).into(),
                        byte: b,
                    }
                    .with_src(file),
                ) {
                    return;
                }
            }
        }
    }
    if let Some((start, end, kind)) = data {
        if !comment.is_empty() {
            tokens.push(Token {
                kind: TokenKind::Comment(comment, kind),
                span: (start..end).into(),
            });
        }
    }
}

pub fn parse_ident<'src, F: Copy>(
    input: &'src [u8],
    index: &mut usize,
    offset: usize,
    tokens: &mut Vec<Token<'src, SourceSpan>>,
    file: F,
    errs: &mut dyn ErrorReporter<TokenizeError<F>>,
) {
    use unicode_ident::*;
    let start = *index;
    let ch = match next_char(input, index, false) {
        None => return,
        Some(Ok(ch)) => ch,
        Some(Err((b, idx))) => {
            let _ = errs.report(
                TokenizeErrorKind::InvalidUTF8 {
                    span: (offset + idx, 1).into(),
                    byte: b,
                }
                .with_src(file),
            );
            return;
        }
    };
    if !(ch == '_' || is_xid_start(ch)) {
        return;
    }
    std::iter::from_fn(|| next_char(input, index, false))
        .map_while(|ch| ch.ok())
        .take_while(|&ch| is_xid_continue(ch))
        .count();
    let ident = unsafe { std::str::from_utf8_unchecked(&input[start..*index]) };
    tokens.push(Token {
        kind: TokenKind::Ident(ident),
        span: ((start + offset)..(*index + offset)).into(),
    });
}
