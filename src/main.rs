use std::{error::Error, fmt::Display, num::ParseIntError, str::FromStr};
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
    version: SemanticVersion
}

#[derive(Debug, PartialEq)]
struct SemanticVersion {
    major: u32,
    minor: u32,
    patch: u32,
}

impl SemanticVersion {
    fn bump(&self, part: SemanticVersionPart) -> Self {
        match part {
            SemanticVersionPart::Major => Self {
                major: self.major + 1,
                ..*self
            },
            SemanticVersionPart::Minor => Self {
                minor: self.minor + 1,
                ..*self
            },
            SemanticVersionPart::Patch => Self {
                patch: self.patch + 1,
                ..*self
            },
        }
    }
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
            [major, minor, patch] => {
                let major = parse_version_part_number(major)?;
                let minor = parse_version_part_number(minor)?;
                let patch = parse_version_part_number(patch)?;
                Ok(SemanticVersion { major, minor, patch })
            },
            _ => return Err(Self::Err::MissingPieces),
        }
    }
}

impl Display for SemanticVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

fn parse_version_part_number(part: &str) -> Result<u32, SemanticVersionParseError> {
    part.parse().map_err(|e| SemanticVersionParseError::ParseInt(e, part.to_owned()))
}

#[derive(Debug)]
enum SemanticVersionParseError {
    ParseInt(ParseIntError, String),
    MissingPieces,
    Empty,
}

impl Display for SemanticVersionParseError {
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

#[derive(Debug, PartialEq)]
enum SemanticVersionPart {
    Major,
    Minor,
    Patch,
}

#[derive(Debug, PartialEq)]
enum SemanticVersionPartParseError {
    Empty,
    Invalid(String),
}

impl Display for SemanticVersionPartParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemanticVersionPartParseError::Empty => write!(f, "{}", "version part is empty"),
            SemanticVersionPartParseError::Invalid(unrecognized) => write!(f, "\"{}\" is not a valid version part", unrecognized),
        }
    }
}

impl FromStr for SemanticVersionPart {
    type Err = SemanticVersionPartParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.trim().to_lowercase().as_str() {
            "major" => Ok(Self::Major),
            "minor" => Ok(Self::Minor),
            "patch" => Ok(Self::Patch),
            "point" => Ok(Self::Patch),
            "" => Err(Self::Err::Empty),
            s => Err(Self::Err::Invalid(s.to_owned()))
        }
    }
}

impl Display for SemanticVersionPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let part = match self {
            SemanticVersionPart::Major => "major",
            SemanticVersionPart::Minor => "minor",
            SemanticVersionPart::Patch => "patch",
        };

        write!(f, "{}", part)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_part_major() {
        for input in vec!["major", "MAJOR", "mAjOr"] {
            assert_eq!(input.parse::<SemanticVersionPart>().unwrap(), SemanticVersionPart::Major)
        }
    }

    #[test]
    fn parse_part_minor() {
        for input in vec!["minor", "MINOR", "mInOr"] {
            assert_eq!(input.parse::<SemanticVersionPart>().unwrap(), SemanticVersionPart::Minor)
        }
    }

    #[test]
    fn parse_part_patch() {
        for input in vec!["patch", "PATCH", "pAtCh", "point"] {
            assert_eq!(input.parse::<SemanticVersionPart>().unwrap(), SemanticVersionPart::Patch)
        }
    }

    #[test]
    fn parse_part_error_empty() {
        assert_eq!("".parse::<SemanticVersionPart>().unwrap_err(), SemanticVersionPartParseError::Empty)
    }

    #[test]
    fn parse_part_error_invalid_includes_value() {
        assert_eq!("oof".parse::<SemanticVersionPart>().unwrap_err(), SemanticVersionPartParseError::Invalid("oof".to_string()))
    }

    #[test]
    fn display_semantic_version() {
        assert_eq!("1.2.3", format!("{}", SemanticVersion { major: 1, minor: 2, patch: 3 }))
    }

    #[test]
    fn parse_semantic_version() {
        assert_eq!("1.2.3".parse::<SemanticVersion>().unwrap(), SemanticVersion { major: 1, minor: 2, patch: 3 })
    }

    #[test]
    fn bump_version_part_major() {
        test_bump("2.0.0", "1.0.0", SemanticVersionPart::Major)
    }

    #[test]
    fn bump_version_part_minor() {
        test_bump("1.1.0", "1.0.0", SemanticVersionPart::Minor)
    }

    #[test]
    fn bump_version_part_patch() {
        test_bump("1.0.1", "1.0.0", SemanticVersionPart::Patch)
    }

    fn test_bump(expected: &str, version: &str, part: SemanticVersionPart) {
        let actual = version.parse::<SemanticVersion>().unwrap().bump(part);
        assert_eq!(expected.parse::<SemanticVersion>().unwrap(), actual)
    }
}
