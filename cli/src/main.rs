use frolic_cli::prelude::*;

fn main() -> eyre::Result<()> {
    // It's this simple to run
    FrolicCli::parse().run_stdio()
}
