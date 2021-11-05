mod semver;
use crate::semver::{SemanticVersion, SemanticVersionPart};
use structopt::StructOpt;

fn main() -> std::io::Result<()> {
    let cli = Cli::from_args();

    println!("{}", cli.version.bump(cli.part));
    Ok(())
}

#[derive(Debug, StructOpt)]
#[structopt(name = "bump_version")]
struct Cli {
    part: SemanticVersionPart,
    version: SemanticVersion,
}
