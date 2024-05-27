use super::*;

impl<F: Write, S: Span> FormatAST<F> for asts::IntLitAST<S> {
    fn fmt(
        &self,
        cfg: FormatConfig,
        f: &mut TrackingFormatter<F>,
        source: Option<&[u8]>,
    ) -> fmt::Result {
        use asts::IntLitKind::*;
        if cfg == FormatConfig::Mini {
            write!(f, "{}", self.val)
        } else if let Some(src) = source.and_then(|src| {
            src.get(self.loc.offset()..self.loc.end())
                .and_then(|s| std::str::from_utf8(s).ok())
        }) {
            f.write_str(src)
        } else {
            let width = self.loc.len();
            match self.kind {
                Dec => write!(f, "{:0>width$}", self.val),
                Bin => write!(f, "0b{:0>1$b}", self.val, width - 2),
                Oct => write!(f, "0o{:0>1$o}", self.val, width - 2),
                Hex => write!(f, "0x{:0>1$x}", self.val, width - 2),
                Char => {
                    if let Some(ch) =
                        u32::try_from(self.val).map_or(None, |x| char::try_from(x).ok())
                    {
                        write!(f, "{ch:?}")
                    } else {
                        write!(f, "'\\u{{{:0>4x}}}'", self.val)
                    }
                }
            }
        }
    }

    fn estimate_len(
        &self,
        cfg: FormatConfig,
        _col: usize,
        source: Option<&[u8]>,
    ) -> Option<usize> {
        use std::cmp::Ordering::*;
        use asts::IntLitKind::*;
        Some(
            if cfg == FormatConfig::Mini {
                (match self.val.cmp(&0) {
                    Equal => 1,
                    Less => (-self.val).ilog10() + 2,
                    Greater => self.val.ilog10() + 1,
                }) as usize
            } else if source.is_some() {
                self.loc.len()
            } else {
                let width = self.loc.len();
                let min = match self.kind {
                    Dec => (match self.val.cmp(&0) {
                        Equal => 1,
                        Less => (-self.val).ilog10() + 2,
                        Greater => self.val.ilog10() + 1,
                    }) as usize,
                    Char => {
                        if let Some(ch) =
                            u32::try_from(self.val).map_or(None, |x| char::try_from(x).ok())
                        {
                            ch.escape_debug().count() + 2
                        } else {
                            (std::cmp::max(self.val.abs(), 1).ilog2() as usize + 31) / 4
                        }
                    }
                    _ => {
                        let bits = match self.val.cmp(&0) {
                            Equal => 1,
                            Less => (-self.val).ilog2() + 2,
                            Greater => self.val.ilog2() + 1,
                        };
                        let digits = match self.kind {
                            Oct => bits / 3,
                            Hex => bits / 4,
                            _ => bits,
                        };
                        digits as usize + 2
                    }
                };
                std::cmp::max(width, min)
            }
        )
    }
}
impl<F: Write, S: Span> FormatAST<F> for asts::FloatLitAST<S> {
    fn fmt(
        &self,
        cfg: FormatConfig,
        f: &mut TrackingFormatter<F>,
        source: Option<&[u8]>,
    ) -> fmt::Result {
        if let Some(src) = (cfg != FormatConfig::Mini)
            .then(|| {
                source.and_then(|src| {
                    src.get(self.loc.offset()..self.loc.end())
                        .and_then(|s| std::str::from_utf8(s).ok())
                })
            })
            .flatten()
        {
            f.write_str(src)
        } else {
            write!(f, "{}", self.val)
        }
    }

    fn estimate_len(
        &self,
        cfg: FormatConfig,
        _col: usize,
        source: Option<&[u8]>,
    ) -> Option<usize> {
        if source.is_some() && cfg != FormatConfig::Mini {
            Some(self.loc.len())
        } else {
            Some(GetWriteLen::new().write(self.val).chars)
        }
    }
}
impl<F: Write, S: Span> FormatAST<F> for asts::StringLitAST<'_, S> {
    fn fmt(
        &self,
        cfg: FormatConfig,
        f: &mut TrackingFormatter<F>,
        source: Option<&[u8]>,
    ) -> fmt::Result {
        if let Some(src) = (cfg != FormatConfig::Mini)
            .then(|| {
                source.and_then(|src| {
                    src.get(self.loc.offset()..self.loc.end())
                        .and_then(|s| std::str::from_utf8(s).ok())
                })
            })
            .flatten()
        {
            f.write_str(src)
        } else {
            write!(f, "{:?}", bstr::BStr::new(&self.val))
        }
    }

    fn estimate_len(
        &self,
        cfg: FormatConfig,
        _col: usize,
        source: Option<&[u8]>,
    ) -> Option<usize> {
        use bstr::ByteSlice;
        if let Some(Some(src)) = (cfg != FormatConfig::Mini).then(|| source.and_then(|src| src.get(self.loc.offset()..self.loc.end()))) {
            (!src.contains(&b'\n')).then_some(self.loc.len())
        } else {
            Some(self.val.escape_bytes().count() + 2)
        }
    }
}
