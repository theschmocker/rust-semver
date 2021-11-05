mod semver;
use crate::semver::{SemanticVersion, SemanticVersionPart};
use structopt::StructOpt;

fn main() -> std::io::Result<()> {
    let cli = Cli::from_args();

    let command: Command = cli.cmd;

    match command {
        Command::Bump(BumpVersion { part, version }) => {
            println!("{}", version.bump(part));
        }
        Command::Compare(CompareVersions {
            first_version,
            second_version,
        }) => {
            let sign = match first_version.partial_cmp(&second_version).unwrap() {
                std::cmp::Ordering::Less => "<",
                std::cmp::Ordering::Equal => "==",
                std::cmp::Ordering::Greater => ">",
            };

            println!("{} {} {}", first_version, sign, second_version);
        }
        Command::Explain(ExplainVersion { version }) => {
            println!("Major: {}", version.major);
            println!("Minor: {}", version.minor);
            println!("Patch: {}", version.patch);

            if let Some(prerelease) = version.prerelease {
                println!("Prerelease: {}", prerelease);
            }

            if let Some(buildmetadata) = version.buildmetadata {
                println!("Build Metadata: {}", buildmetadata);
            }
        },
    }

    Ok(())
}

#[derive(Debug, StructOpt)]
#[structopt(name = "semver")]
/// A little CLI for doing simple operations on semantic version strings
struct Cli {
    #[structopt(subcommand)]
    cmd: Command,
}

#[derive(Debug, StructOpt)]
enum Command {
    /// bump a specified version part
    Bump(BumpVersion),

    /// compare two semantic version strings
    Compare(CompareVersions),

    /// prints out labelled parts of a semantic version string
    Explain(ExplainVersion),
}

#[derive(Debug, StructOpt)]
struct BumpVersion {
    /// version part - major, minor, or patch
    part: SemanticVersionPart,

    /// version to bump, eg. 1.2.3
    version: SemanticVersion,
}

#[derive(Debug, StructOpt)]
struct CompareVersions {
    first_version: SemanticVersion,
    second_version: SemanticVersion,
}

#[derive(Debug, StructOpt)]
struct ExplainVersion {
    version: SemanticVersion,
}
