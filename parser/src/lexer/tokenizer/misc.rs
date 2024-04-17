use super::*;
impl<'src, F: Copy> Lexer<'src, '_, F> {
    fn eat_comment(
        &mut self,
        this: CommentKind,
        data: &mut Option<(usize, usize, CommentKind)>,
        comment: &mut Cow<'src, [u8]>,
    ) {
        let start = self.index;
        let line_end = self.input[start..]
            .iter()
            .position(|&i| i == b'\n')
            .unwrap_or(self.input.len() - start)
            + start;
        if line_end != start {
            self.index = line_end + 1;
        }
        let slice = &self.input[start..line_end];
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
                self.tokens.push(Token {
                    kind: TokenKind::Comment(comment, *kind),
                    span: ((*start + self.offset - usize::from(this != CommentKind::Ignore))
                        ..(*end + self.offset))
                        .into(),
                });
            }
        }
        *data = Some((start, self.index, this));
        *comment = slice.into();
    }

    pub fn parse_comment(&mut self) {
        let mut comment = Cow::Borrowed(&[][..]);
        let mut data = None;
        while let Some(ch) = self.next_char(false) {
            match ch {
                Ok('#') => match self.input.get(self.index + 1) {
                    Some(&b'#') => {
                        self.index += 1;
                        self.eat_comment(CommentKind::OuterDoc, &mut data, &mut comment);
                    }
                    Some(&b'!') => {
                        self.index += 1;
                        self.eat_comment(CommentKind::InnerDoc, &mut data, &mut comment);
                    }
                    Some(&b'=') => {
                        let start = self.index;
                        let len = self.input[(start + 1)..]
                            .iter()
                            .position(|&c| c != b'=')
                            .unwrap_or(self.input.len() - start);
                        self.index += len;

                        let mut remaining = &self.input[self.index..];

                        let valid = loop {
                            let Some(found) = remaining.iter().position(|&c| c == b'=') else {
                                break false;
                            };
                            remaining = &remaining[found..];
                            self.index += found;
                            let Some(count) = remaining.iter().position(|&c| c != b'=') else {
                                break false;
                            };
                            remaining = &remaining[count..];
                            self.index += count;
                            if count < len {
                                continue;
                            }
                            if remaining[0] == b'#' {
                                self.index += 1;
                                break true;
                            }
                        };
                        if !valid
                            && self.report(TokenizeError::UnclosedMultiline {
                                span: (start + self.offset, len + 1).into(),
                                end: self.input.len() + self.offset,
                            })
                        {
                            return;
                        }
                        let slice = &self.input[start..self.index];
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
                                    let comment =
                                        std::mem::replace(&mut comment, Cow::Borrowed(&[]));
                                    self.tokens.push(Token {
                                        kind: TokenKind::Comment(comment, kind),
                                        span: ((start + self.offset)..(end + self.offset)).into(),
                                    });
                                }
                            }
                            data = Some((start, self.index, CommentKind::Ignore));
                            comment = slice.into();
                        }
                    }
                    _ => self.eat_comment(CommentKind::Ignore, &mut data, &mut comment),
                },
                Ok(ch) => {
                    if !ch.is_whitespace() {
                        self.index -= ch.len_utf8();
                        break;
                    }
                }
                Err(ret) => {
                    if ret {
                        return;
                    }
                }
            }
        }
        if let Some((start, end, kind)) = data {
            if !comment.is_empty() {
                self.tokens.push(Token {
                    kind: TokenKind::Comment(comment, kind),
                    span: ((start + self.offset)..(end + self.offset)).into(),
                });
            }
        }
    }

    pub fn parse_ident(&mut self) {
        use unicode_ident::*;
        let start = self.index;
        let Some(Ok(ch)) = self.next_char(false) else {
            return;
        };
        if !(ch == '_' || is_xid_start(ch)) {
            return;
        }
        let last = std::iter::from_fn(|| self.next_char(false))
            .map_while(|ch| ch.ok())
            .take_while(|&ch| is_xid_continue(ch))
            .last();
        if let Some(last) = last {
            if self.index < self.input.len() {
                self.index -= last.len_utf8();
            }
        }
        let ident = unsafe { std::str::from_utf8_unchecked(&self.input[start..self.index]) };
        self.tokens.push(Token {
            kind: TokenKind::Ident(ident),
            span: ((start + self.offset)..(self.index + self.offset)).into(),
        });
    }
}
