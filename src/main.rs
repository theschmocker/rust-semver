use::std;
use std::{error::Error, num::ParseIntError, str::FromStr};

fn main() -> std::io::Result<()> {
    let mut buffer = String::new();
    std::io::stdin().read_line(&mut buffer)?;

    println!("{:?}", SemanticVersion::from_str(&buffer)?);
    Ok(())
}

#[derive(Debug)]
struct SemanticVersion {
    major: u32,
    minor: u32,
    point: u32,
}

impl FromStr for SemanticVersion {
    type Err = SemanticVersionParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let cleaned = s.trim();

        if cleaned.len() == 0 {
            return Err(Self::Err::Empty.into());
        }

        let parts: Vec<_> = cleaned.split(".").collect();

        match &parts[..] {
            [major, minor, point] => {
                let major = parse_version_part(major)?;
                let minor = parse_version_part(minor)?;
                let point = parse_version_part(point)?;
                Ok(SemanticVersion { major, minor, point })
            },
            _ => return Err(Self::Err::MissingPieces),
        }
    }
}

fn parse_version_part(part: &str) -> Result<u32, SemanticVersionParseError> {
    part.parse().map_err(|e| SemanticVersionParseError::ParseInt(e, part.to_owned()))
}

#[derive(Debug)]
enum SemanticVersionParseError {
    ParseInt(ParseIntError, String),
    MissingPieces,
    Empty,
}

impl std::fmt::Display for SemanticVersionParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemanticVersionParseError::ParseInt(e, original) => write!(f, "could not parse number from \"{}\": {} ", original, e),
            SemanticVersionParseError::MissingPieces => write!(f, "missing pieces of semantic version"),
            SemanticVersionParseError::Empty => write!(f, "empty string"),
        }
    }
}

impl Error for SemanticVersionParseError {}

impl From<SemanticVersionParseError> for std::io::Error {
    fn from(e: SemanticVersionParseError) -> Self {
        std::io::Error::new(std::io::ErrorKind::Other, format!("{}", e))
    }
}
