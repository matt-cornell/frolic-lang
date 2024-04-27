use super::*;
use frolic_fmt::prelude::*;

#[derive(Debug, Clone, Args)]
pub struct FrolicColor {
    input: PathBuf,
    #[command(flatten)]
    theme: FrolicTheme,
}

#[derive(Debug, Clone, Args)]
#[group(multiple = false)]
pub struct FrolicTheme {
    #[arg(short, long)]
    pub none: bool,
    #[arg(short, long)]
    pub ansi: bool,
    #[arg(short, long)]
    pub truecolor: bool,
    #[arg(short, long)]
    pub css: bool,
    #[cfg(feature = "serde")]
    #[arg(short, long)]
    pub path: Option<PathBuf>,
}

impl Runnable for FrolicColor {
    fn run<I: Read + Send + Sync, O: Write + Send + Sync, E: Write + Send + Sync>(
        self,
        _stdin: I,
        mut stdout: O,
        _stderr: E,
    ) -> eyre::Result<()> {
        let code = std::fs::read_to_string(self.input)?;
        #[cfg(feature = "serde")]
        #[allow(unused_assignments)] // this is just used to extend the lifetime
        let mut storage: Option<Theme<frolic_fmt::color::FormatString<String>>> = None;
        #[cfg(feature = "serde")]
        let theme = match self.theme {
            FrolicTheme {
                none: false,
                ansi: false,
                truecolor: false,
                css: false,
                path: None,
            } => {
                if let Some(support) = supports_color::on(supports_color::Stream::Stdout) {
                    if support.has_16m {
                        themes::DEFAULT_TRUECOLOR.erase()
                    } else if support.has_basic {
                        themes::DEFAULT_ANSI.erase()
                    } else {
                        themes::COLORLESS.erase()
                    }
                } else {
                    themes::COLORLESS.erase()
                }
            }
            FrolicTheme {
                none: true,
                ansi: false,
                truecolor: false,
                css: false,
                path: None,
            } => themes::COLORLESS.erase(),
            FrolicTheme {
                none: false,
                ansi: true,
                truecolor: false,
                css: false,
                path: None,
            } => themes::DEFAULT_ANSI.erase(),
            FrolicTheme {
                none: false,
                ansi: false,
                truecolor: true,
                css: false,
                path: None,
            } => themes::DEFAULT_TRUECOLOR.erase(),
            FrolicTheme {
                none: false,
                ansi: false,
                truecolor: false,
                css: true,
                path: None,
            } => themes::CSS_CLASSES.erase(),
            FrolicTheme {
                none: false,
                ansi: false,
                truecolor: false,
                css: false,
                path: Some(p),
            } => {
                let f = io::BufReader::new(std::fs::File::open(p)?);
                storage = Some(serde_json::from_reader(f)?);
                storage.as_ref().unwrap().erase()
            }
            _ => panic!("at most one theme option can be set!"),
        };
        #[cfg(not(feature = "serde"))]
        let theme = match self.theme {
            FrolicTheme {
                none: false,
                ansi: false,
                truecolor: false,
                css: false,
            } => {
                if let Some(support) = supports_color::on(supports_color::Stream::Stdout) {
                    if support.has_16m {
                        themes::DEFAULT_TRUECOLOR.erase()
                    } else if support.has_basic {
                        themes::DEFAULT_ANSI.erase()
                    } else {
                        themes::COLORLESS.erase()
                    }
                } else {
                    themes::COLORLESS.erase()
                }
            }
            FrolicTheme {
                none: true,
                ansi: false,
                truecolor: false,
                css: false,
            } => themes::COLORLESS.erase(),
            FrolicTheme {
                none: false,
                ansi: true,
                truecolor: false,
                css: false,
            } => themes::DEFAULT_ANSI.erase(),
            FrolicTheme {
                none: false,
                ansi: false,
                truecolor: true,
                css: false,
            } => themes::DEFAULT_TRUECOLOR.erase(),
            FrolicTheme {
                none: false,
                ansi: false,
                truecolor: false,
                css: true,
            } => themes::CSS_CLASSES.erase(),
            _ => panic!("at most one theme option can be set!"),
        };
        write!(stdout, "{}", Colored { code, theme })?;
        Ok(())
    }
}
