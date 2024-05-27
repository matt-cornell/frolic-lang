use super::*;

/// A formatter that tracks line/column and does indentation. Meant for use in AST formatting, but
/// could easily be useful in other places.
#[derive(Debug, Clone)]
pub struct TrackingFormatter<F> {
    /// Indentation to use.
    pub indent: String,
    /// Inner formatter to write to.
    pub inner: F,
    line: usize,
    col: usize,
}
impl<F> TrackingFormatter<F> {
    pub const fn new(inner: F) -> Self {
        Self {
            inner,
            indent: String::new(),
            line: 0,
            col: 0,
        }
    }
    pub fn with_indent(inner: F, indent: impl Into<String>) -> Self {
        Self {
            inner,
            indent: indent.into(),
            line: 0,
            col: 0,
        }
    }
    pub const fn line(&self) -> usize {
        self.line
    }
    pub const fn col(&self) -> usize {
        self.col
    }
}
impl<F: Write> Write for TrackingFormatter<F> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let indent_len = self.indent.chars().count();
        let mut char_len = 0;
        let mut it = s.char_indices().enumerate().inspect(|x| char_len = x.0);
        let mut last_byte = 0;
        let mut last_char = 0;
        while let Some((ch_idx, (bt_idx, _))) = it.find(|x| x.1 .1 == '\n') {
            if self.col == 0 && bt_idx == last_byte {
                self.inner.write_str(&self.indent)?;
                self.col = indent_len;
            }
            self.inner.write_str(&s[last_byte..bt_idx])?;
            self.line += 1;
            self.col += ch_idx - last_char;
            last_byte = bt_idx + 1;
            last_char = ch_idx + 1;
        }
        if last_byte < s.len() {
            self.inner.write_str(&self.indent)?;
            self.col = indent_len;
            self.inner.write_str(&s[last_byte..])?;
            self.col += char_len - last_char;
        }
        Ok(())
    }
}

/// Simple type that implements `Write` and counts the bytes and characters in its input.
#[derive(Debug, Default, Clone, Copy)]
pub struct GetWriteLen {
    pub bytes: usize,
    pub chars: usize,
}
impl GetWriteLen {
    /// Sets with `bytes` and `chars` at 0.
    pub const fn new() -> Self {
        Self {
            bytes: 0,
            chars: 0,
        }
    }
    /// Convenience method for chaining.
    pub fn write(&mut self, f: impl std::fmt::Display) -> &mut Self {
        let _ = write!(self, "{f}");
        self
    }
}
impl Write for GetWriteLen {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.bytes += s.len();
        self.chars += s.chars().count();
        Ok(())
    }
}
