use owo_colors::{AnsiColors, DynColors, OwoColorize, Style};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::fmt::{self, Display, Formatter};

/// Simple wrapper that prepends `"color="` to a `Display` impl.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CssSetColor<S>(pub S);
impl<S: Display> Display for CssSetColor<S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "color={}", self.0)
    }
}

/// Use CSS to color the output. Note that the `style` and `class` are *not* sanitized!
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HtmlColors<S> {
    pub style: Option<S>,
    pub class: Option<S>,
}
impl<S> HtmlColors<S> {
    pub const fn normal() -> Self {
        Self {
            style: None,
            class: None,
        }
    }
    pub const fn style(style: S) -> Self {
        Self {
            style: Some(style),
            class: None,
        }
    }
    pub const fn class(class: S) -> Self {
        Self {
            style: None,
            class: Some(class),
        }
    }
}
impl<S> From<S> for HtmlColors<S> {
    fn from(value: S) -> Self {
        Self {
            style: Some(value),
            class: None,
        }
    }
}
impl<S> From<S> for HtmlColors<CssSetColor<S>> {
    fn from(value: S) -> Self {
        Self {
            style: Some(CssSetColor(value)),
            class: None,
        }
    }
}

/// Replace all instances of `$` with the code to print.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct FormatString<S>(pub S);

/// A `Colorer` defines a way to color a string to the output.
pub trait Colorer {
    fn write(&self, s: &str, f: &mut Formatter<'_>) -> fmt::Result;
}
/// Optional style, defaulting to just writing the string.
impl<C: Colorer> Colorer for Option<C> {
    fn write(&self, s: &str, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(c) = self {
            c.write(s, f)
        } else {
            f.write_str(s)
        }
    }
}
impl Colorer for () {
    fn write(&self, s: &str, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(s)
    }
}
impl Colorer for Style {
    fn write(&self, s: &str, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.style(s))
    }
}
impl Colorer for AnsiColors {
    fn write(&self, s: &str, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", s.color(*self))
    }
}
impl Colorer for DynColors {
    fn write(&self, s: &str, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", s.color(*self))
    }
}
impl Colorer for [u8; 3] {
    fn write(&self, s: &str, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", s.truecolor(self[0], self[1], self[2]))
    }
}
impl<S: Display> Colorer for HtmlColors<S> {
    fn write(&self, s: &str, f: &mut Formatter<'_>) -> fmt::Result {
        match (&self.style, &self.class) {
            (None, None) => f.write_str(s),
            (Some(style), None) => write!(f, "<span style=\"{style}\">{s}</span>"),
            (None, Some(class)) => write!(f, "<span class=\"{class}\">{s}</span>"),
            (Some(style), Some(class)) => {
                write!(f, "<span style=\"{style}\" class=\"{class}\">{s}</span>")
            }
        }
    }
}
impl<S: Display> Colorer for FormatString<S> {
    fn write(&self, s: &str, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0.to_string().replace('$', s))
    }
}
impl<C: Colorer + ?Sized> Colorer for &C {
    fn write(&self, s: &str, f: &mut Formatter<'_>) -> fmt::Result {
        C::write(self, s, f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(
    feature = "serde",
    serde(
        bound(deserialize = "C: serde::de::Deserialize<'de> + Clone"),
        from = "ThemeBuilder<C>"
    )
)]
pub struct Theme<C> {
    /// Default way to print code.
    pub default: C,
    /// Identifiers
    pub ident: C,
    /// Keyword
    pub keyword: C,
    /// Numeric literals
    pub num: C,
    /// String literals
    pub string: C,
    /// Character literals
    pub char: C,
    /// Ignored comments
    pub ign: C,
    /// Doc comments
    pub doc: C,
    /// Operators
    pub ops: C,
}
impl<C> Theme<C> {
    /// Implementing `From` directly doesn't work, so we have this method.
    pub fn convert<D: From<C>>(self) -> Theme<D> {
        let Theme {
            default,
            ident,
            keyword,
            num,
            string,
            char,
            ign,
            doc,
            ops,
        } = self;
        Theme {
            default: D::from(default),
            ident: D::from(ident),
            keyword: D::from(keyword),
            num: D::from(num),
            string: D::from(string),
            char: D::from(char),
            ign: D::from(ign),
            doc: D::from(doc),
            ops: D::from(ops),
        }
    }
}
impl<C: Colorer> Theme<C> {
    /// Type-erase the colorer.
    pub fn erase(&self) -> Theme<&dyn Colorer> {
        let Theme {
            default,
            ident,
            keyword,
            num,
            string,
            char,
            ign,
            doc,
            ops,
        } = self;
        Theme {
            default: default as _,
            ident: ident as _,
            keyword: keyword as _,
            num: num as _,
            string: string as _,
            char: char as _,
            ign: ign as _,
            doc: doc as _,
            ops: ops as _,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ThemeBuilder<C> {
    default: C,
    ident: Option<C>,
    keyword: Option<C>,
    num: Option<C>,
    string: Option<C>,
    char: Option<C>,
    lit: Option<C>,
    ign: Option<C>,
    doc: Option<C>,
    comment: Option<C>,
    ops: Option<C>,
}
impl<C> ThemeBuilder<C> {
    pub const fn new(default: C) -> Self {
        Self {
            default,
            ident: None,
            keyword: None,
            num: None,
            string: None,
            char: None,
            lit: None,
            doc: None,
            ign: None,
            comment: None,
            ops: None,
        }
    }
    /// Set the color for identifiers
    pub fn ident(&mut self, val: impl Into<C>) -> &mut Self {
        self.ident = Some(val.into());
        self
    }
    /// Set the color for keywords
    pub fn keyword(&mut self, val: impl Into<C>) -> &mut Self {
        self.keyword = Some(val.into());
        self
    }
    /// Set the color for numeric literals
    pub fn num(&mut self, val: impl Into<C>) -> &mut Self {
        self.num = Some(val.into());
        self
    }
    /// Set the color for string literals
    pub fn string(&mut self, val: impl Into<C>) -> &mut Self {
        self.string = Some(val.into());
        self
    }
    /// Set the color for character literals
    pub fn char(&mut self, val: impl Into<C>) -> &mut Self {
        self.char = Some(val.into());
        self
    }
    /// Set the color for all literals.
    pub fn lit(&mut self, val: impl Into<C>) -> &mut Self {
        self.lit = Some(val.into());
        self
    }
    /// Set the color for ignored comments.
    pub fn ign(&mut self, val: impl Into<C>) -> &mut Self {
        self.ign = Some(val.into());
        self
    }
    /// Set the color for documentation comments.
    pub fn doc(&mut self, val: impl Into<C>) -> &mut Self {
        self.doc = Some(val.into());
        self
    }
    /// Set the color for all comments.
    pub fn comment(&mut self, val: impl Into<C>) -> &mut Self {
        self.comment = Some(val.into());
        self
    }
    /// Set the color for operators.
    pub fn ops(&mut self, val: impl Into<C>) -> &mut Self {
        self.ops = Some(val.into());
        self
    }
}
impl<C: Clone> ThemeBuilder<C> {
    pub fn finish(self) -> Theme<C> {
        // TODO: avoid unnecessary clones?
        let ident = self.ident.unwrap_or_else(|| self.default.clone());
        let keyword = self.keyword.unwrap_or_else(|| ident.clone());
        let lit = self.lit.unwrap_or_else(|| self.default.clone());
        let num = self.num.unwrap_or_else(|| lit.clone());
        let string = self.string.unwrap_or_else(|| lit.clone());
        let char = self.char.unwrap_or_else(|| lit.clone());
        let comm = self.comment.unwrap_or_else(|| self.default.clone());
        let ign = self.ign.unwrap_or_else(|| comm.clone());
        let doc = self.doc.unwrap_or_else(|| comm.clone());
        let ops = self.ops.unwrap_or_else(|| self.default.clone());
        Theme {
            ident,
            keyword,
            num,
            string,
            char,
            ign,
            doc,
            ops,
            default: self.default,
        }
    }
}

impl<C: Clone> From<ThemeBuilder<C>> for Theme<C> {
    fn from(value: ThemeBuilder<C>) -> Self {
        value.finish()
    }
}

#[derive(Debug, Clone)]
pub struct Colored<S, C> {
    pub code: S,
    pub theme: Theme<C>,
}
impl<S: AsRef<str>, C: Colorer> Display for Colored<S, C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fmt_impl(self.code.as_ref(), &self.theme, f)
    }
}

fn fmt_impl<C: Colorer>(code: &str, theme: &Theme<C>, f: &mut Formatter<'_>) -> fmt::Result {
    use frolic_parser::lexer::tokens::*;
    use frolic_parser::prelude::*;
    use frolic_utils::prelude::*;

    let toks: Vec<Token<PrettySpan>> = tokenize(code, "", SilentReporter);
    let mut last = 0;
    for Token { kind, span } in toks {
        theme.default.write(&code[last..span.offset], f)?;
        last = span.offset + span.len;
        let color = match kind {
            TokenKind::Int(_) | TokenKind::Float(_) => &theme.num,
            TokenKind::String(_) => &theme.string,
            TokenKind::Char(_) => &theme.char,
            TokenKind::Comment(_, CommentKind::Ignore) => &theme.ign,
            TokenKind::Comment(..) => &theme.doc,
            TokenKind::PreOp(_)
            | TokenKind::InfOp(_)
            | TokenKind::AmbigOp(_)
            | TokenKind::Special(SpecialChar::Arrow) => &theme.ops,
            TokenKind::Keyword(_) => &theme.keyword,
            TokenKind::Ident(_) => &theme.ident,
            _ => &theme.default,
        };
        color.write(&code[span.offset..last], f)?;
    }
    theme.default.write(&code[last..], f)
}

pub mod themes {
    use super::*;

    /// Use default ANSI codes
    pub const DEFAULT_ANSI: Theme<AnsiColors> = Theme {
        default: AnsiColors::Default,
        ident: AnsiColors::Red,
        keyword: AnsiColors::Magenta,
        string: AnsiColors::Green,
        char: AnsiColors::Green,
        num: AnsiColors::Yellow,
        ops: AnsiColors::Default,
        ign: AnsiColors::BrightBlack,
        doc: AnsiColors::BrightBlack,
    };

    /// Use CSS classes to style the code
    pub const CSS_CLASSES: Theme<HtmlColors<&str>> = Theme {
        default: HtmlColors::normal(),
        ident: HtmlColors::class("ident"),
        keyword: HtmlColors::class("keyword ident"),
        string: HtmlColors::class("string literal"),
        char: HtmlColors::class("char literal"),
        num: HtmlColors::class("num literal"),
        ops: HtmlColors::class("operator"),
        ign: HtmlColors::class("ignored comment"),
        doc: HtmlColors::class("docs comment"),
    };

    /// Use truecolor output
    pub const DEFAULT_TRUECOLOR: Theme<Option<[u8; 3]>> = Theme {
        default: None,
        ident: Some([224, 108, 117]),
        keyword: Some([198, 120, 221]),
        string: Some([152, 195, 121]),
        char: Some([152, 195, 121]),
        num: Some([229, 192, 123]),
        ops: None,
        ign: Some([92, 99, 112]),
        doc: Some([92, 99, 112]),
    };

    pub const COLORLESS: Theme<()> = Theme {
        default: (),
        ident: (),
        keyword: (),
        string: (),
        char: (),
        num: (),
        ops: (),
        ign: (),
        doc: (),
    };
}
