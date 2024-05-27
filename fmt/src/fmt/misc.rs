use super::*;

impl<F: Write, S: Span> FormatAST<F> for asts::CommentAST<'_, S> {
    fn fmt(
        &self,
        cfg: FormatConfig,
        f: &mut TrackingFormatter<F>,
        source: Option<&[u8]>,
    ) -> fmt::Result {
        use bstr::ByteSlice;
        if let FormatConfig::Pretty(cfg) = cfg {
            let old_len = f.indent.len();
            f.indent.push_str("# ");
            let mut idx = 0;
            let mut col = f.col();
            let indent_len = f.indent.chars().count();
            let it = self.val.char_indices().filter_map(|(start, stop, ch)| {
                if ch == '\n' {
                    let out = &self.val[idx..start];
                    idx = stop;
                    col = indent_len;
                    return Some(out);
                }
                col += 1;
                if cfg.wrap > 0 && col > cfg.wrap && ch.is_whitespace() {
                    let out = &self.val[idx..start];
                    idx = stop;
                    col = indent_len;
                    return Some(out);
                }
                None
            });
            let res = it.try_for_each(|s| f.write_str(s));
            f.indent.truncate(old_len);
            res
        } else {
            Ok(())
        }
    }
    fn estimate_len(
        &self,
        cfg: FormatConfig,
        f: &mut TrackingFormatter<F>,
        source: Option<&[u8]>,
    ) -> Option<usize> {
        None // we always want to break to a newline
    }
}
impl<F: Write, S> FormatAST<F> for ErrorAST<S> {
    fn fmt(
        &self,
        _cfg: FormatConfig,
        _f: &mut TrackingFormatter<F>,
        _source: Option<&[u8]>,
    ) -> fmt::Result {
        Ok(())
    }

    fn estimate_len(
        &self,
        _cfg: FormatConfig,
        _col: usize,
        _source: Option<&[u8]>
    ) -> Option<usize> {
        Some(0)
    }
}
impl<F: Write, S> FormatAST<F> for VarAST<S> {
    fn fmt(
        &self,
        _cfg: FormatConfig,
        f: &mut TrackingFormatter<F>,
        _source: Option<&[u8]>,
    ) -> fmt::Result {
        if self.global.is_some() {
            f.write_str(".")?;
        }
        f.write_str(&self.name)
    }

    fn estimate_len(
        &self,
        _cfg: FormatConfig,
        _col: usize,
        _source: Option<&[u8]>
    ) -> Option<usize> {
        Some(self.name.chars().count() + usize::from(self.global.is_some()))
    }
}
