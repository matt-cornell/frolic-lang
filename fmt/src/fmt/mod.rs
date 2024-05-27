use frolic_ast::prelude::asts;
use frolic_utils::prelude::*;
use std::fmt::{self, Write};

pub mod utils;
use utils::*;

mod lits;

/// Configuration used for pretty-printed output.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PrettyConfig<'a> {
    /// Indentation. Common values could be `"  "` or `"\t"` but any single-line string could be
    /// allowed. If it contains newlines though, it'll break the formatter's line/col tracking.
    indent: &'a str,
    /// Point at which to wrap code. A value of 0 disables wrapping.
    wrap: usize,
}
impl Default for PrettyConfig<'_> {
    fn default() -> Self {
        Self {
            indent: "  ",
            wrap: 100,
        }
    }
}

/// Configuration for formatting.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FormatConfig<'a> {
    /// Print a minified output.
    Mini,
    /// Pretty-print according to the config.
    Pretty(PrettyConfig<'a>),
}

/// Trait to format an AST node. Parameterized over the type of the formatter, which shouldn't
/// matter. I wish I could put it as a parameter in the method, but that would break object safety.
#[impl_tools::autoimpl(for<T: trait> &T, Box<T>, std::rc::Rc<T>, std::sync::Arc<T>)]
pub trait FormatAST<F: Write> {
    /// Write out a formatted version of this AST. If a source input is provided, it can be used to
    /// create an output closer to the original, therefore it *must* be the same as where the AST
    /// came from!
    fn fmt(
        &self,
        cfg: FormatConfig,
        f: &mut TrackingFormatter<F>,
        source: Option<&[u8]>,
    ) -> fmt::Result;

    /// Estimate the length when printed. Return `None` if this will overflow to another line.
    fn estimate_len(
        &self,
        cfg: FormatConfig,
        col: usize,
        source: Option<&[u8]>
    ) -> Option<usize>;
}

/// A type that wraps everything necessary to call `FormatAST`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FmtDisplay<'a, T> {
    /// The AST to format.
    pub inner: T,
    /// Configuration.
    pub cfg: FormatConfig<'a>,
    /// The source input, if it's available. Must be the same as where the AST came from!
    pub source: Option<&'a [u8]>,
}

impl<'a, T: for<'f1, 'f2> FormatAST<&'f1 mut fmt::Formatter<'f2>>> fmt::Display
    for FmtDisplay<'a, T>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner
            .fmt(self.cfg, &mut TrackingFormatter::new(f), self.source)
    }
}
