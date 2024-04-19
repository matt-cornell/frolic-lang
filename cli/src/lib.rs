use clap::{Args, Parser, Subcommand};
use std::io::{self, Read, Write};
use std::path::PathBuf;

pub mod debug;

pub trait Runnable: Sized {
    fn run<I: Read + Send + Sync, O: Write + Send + Sync, E: Write + Send + Sync>(
        self,
        stdin: I,
        stdout: O,
        stderr: E,
    ) -> eyre::Result<()>;
    fn run_stdio(self) -> eyre::Result<()> {
        self.run(io::stdin(), io::stdout(), io::stderr())
    }
}

/// Frolic main API
#[derive(Debug, Clone, Parser)]
pub enum FrolicCli {
    /// Debug subcommands. These are mainly meant to be used for compiler development, and are
    /// skipped during parsing in release builds.
    #[cfg_attr(debug_assertions, command(subcommand))]
    #[cfg_attr(not(debug_assertions), command(skip))]
    Debug(debug::FrolicDebug),
}
impl Runnable for FrolicCli {
    fn run<I: Read + Send + Sync, O: Write + Send + Sync, E: Write + Send + Sync>(
        self,
        stdin: I,
        stdout: O,
        stderr: E,
    ) -> eyre::Result<()> {
        match self {
            Self::Debug(cmd) => cmd.run(stdin, stdout, stderr),
        }
    }
}

pub mod prelude {
    pub use crate::FrolicCli;
    pub use crate::Runnable;
    pub use clap::Parser;
}