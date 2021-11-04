use regex::Regex;
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
    version: SemanticVersion,
}

#[derive(Debug, PartialEq)]
struct SemanticVersion {
    major: u32,
    minor: u32,
    patch: u32,
    prerelease: Option<String>,
    buildmetadata: Option<String>,
}

impl SemanticVersion {
    fn bump(&self, part: SemanticVersionPart) -> Self {
        match part {
            SemanticVersionPart::Major => Self {
                major: self.major + 1,
                prerelease: self.prerelease.as_ref().map(|s| s.as_str().to_owned()),
                buildmetadata: self.buildmetadata.as_ref().map(|s| s.as_str().to_owned()),
                ..*self
            },
            SemanticVersionPart::Minor => Self {
                minor: self.minor + 1,
                prerelease: self.prerelease.as_ref().map(|s| s.as_str().to_owned()),
                buildmetadata: self.buildmetadata.as_ref().map(|s| s.as_str().to_owned()),
                ..*self
            },
            SemanticVersionPart::Patch => Self {
                patch: self.patch + 1,
                prerelease: self.prerelease.as_ref().map(|s| s.as_str().to_owned()),
                buildmetadata: self.buildmetadata.as_ref().map(|s| s.as_str().to_owned()),
                ..*self
            },
        }
    }
}

impl FromStr for SemanticVersion {
    type Err = SemanticVersionParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // TODO: use lazy_static as recommended in the regex crate's docs
        let re = Regex::new("^(?P<major>0|[1-9]\\d*)\\.(?P<minor>0|[1-9]\\d*)\\.(?P<patch>0|[1-9]\\d*)(?:-(?P<prerelease>(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\\+(?P<buildmetadata>[0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$").unwrap();

        let trimmed = s.trim();
        if trimmed.len() == 0 {
            return Err(Self::Err::Empty);
        }

        let captures = re.captures(trimmed).ok_or(Self::Err::InvalidFormat)?;

        let major = captures
            .name("major")
            .map(|v| parse_version_part_number(v.as_str()))
            .ok_or(Self::Err::InvalidFormat)??;

        let minor = captures
            .name("minor")
            .map(|v| parse_version_part_number(v.as_str()))
            .ok_or(Self::Err::InvalidFormat)??;

        let patch = captures
            .name("patch")
            .map(|v| parse_version_part_number(v.as_str()))
            .ok_or(Self::Err::InvalidFormat)??;

        Ok(SemanticVersion {
            major,
            minor,
            patch,
            prerelease: captures.name("prerelease").map(|v| v.as_str().to_owned()),
            buildmetadata: captures
                .name("buildmetadata")
                .map(|v| v.as_str().to_owned()),
        })
    }
}

impl Display for SemanticVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut version_string = format!("{}.{}.{}", self.major, self.minor, self.patch);
        if let Some(prerelease) = &self.prerelease {
            version_string.push_str(&format!("-{}", prerelease));
        }
        if let Some(buildmetadata) = &self.buildmetadata {
            version_string.push_str(&format!("+{}", buildmetadata));
        }

        write!(f, "{}", version_string)
    }
}

fn parse_version_part_number(part: &str) -> Result<u32, SemanticVersionParseError> {
    part.parse()
        .map_err(|e| SemanticVersionParseError::ParseInt(e, part.to_owned()))
}

#[derive(Debug)]
enum SemanticVersionParseError {
    ParseInt(ParseIntError, String),
    InvalidFormat,
    Empty,
}

impl Display for SemanticVersionParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemanticVersionParseError::ParseInt(e, original) => {
                write!(f, "could not parse number from \"{}\": {} ", original, e)
            }
            SemanticVersionParseError::InvalidFormat => {
                write!(f, "version passed does not match expected format")
            }
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
            SemanticVersionPartParseError::Invalid(unrecognized) => {
                write!(f, "\"{}\" is not a valid version part", unrecognized)
            }
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
            s => Err(Self::Err::Invalid(s.to_owned())),
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
            assert_eq!(
                input.parse::<SemanticVersionPart>().unwrap(),
                SemanticVersionPart::Major
            )
        }
    }

    #[test]
    fn parse_part_minor() {
        for input in vec!["minor", "MINOR", "mInOr"] {
            assert_eq!(
                input.parse::<SemanticVersionPart>().unwrap(),
                SemanticVersionPart::Minor
            )
        }
    }

    #[test]
    fn parse_part_patch() {
        for input in vec!["patch", "PATCH", "pAtCh", "point"] {
            assert_eq!(
                input.parse::<SemanticVersionPart>().unwrap(),
                SemanticVersionPart::Patch
            )
        }
    }

    #[test]
    fn parse_part_error_empty() {
        assert_eq!(
            "".parse::<SemanticVersionPart>().unwrap_err(),
            SemanticVersionPartParseError::Empty
        )
    }

    #[test]
    fn parse_part_error_invalid_includes_value() {
        assert_eq!(
            "oof".parse::<SemanticVersionPart>().unwrap_err(),
            SemanticVersionPartParseError::Invalid("oof".to_string())
        )
    }

    #[test]
    fn display_semantic_version() {
        assert_eq!(
            "1.2.3",
            format!(
                "{}",
                SemanticVersion {
                    major: 1,
                    minor: 2,
                    patch: 3,
                    prerelease: None,
                    buildmetadata: None,
                }
            )
        )
    }

    #[test]
    fn parse_semantic_version() {
        assert_eq!(
            "1.2.3".parse::<SemanticVersion>().unwrap(),
            SemanticVersion {
                major: 1,
                minor: 2,
                patch: 3,
                prerelease: None,
                buildmetadata: None,
            }
        )
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

    #[test]
    fn bump_version_with_prerelease() {
        test_bump("2.0.0-alpha", "1.0.0-alpha", SemanticVersionPart::Major)
    }

    #[test]
    fn bump_version_with_buildmetadata() {
        test_bump(
            "2.0.0+myawesomebuild",
            "1.0.0+myawesomebuild",
            SemanticVersionPart::Major,
        )
    }

    #[test]
    fn bump_version_with_prerelease_and_buildmetadata() {
        test_bump(
            "2.0.0-alpha+myawesomebuild",
            "1.0.0-alpha+myawesomebuild",
            SemanticVersionPart::Major,
        )
    }

    fn test_bump(expected: &str, version: &str, part: SemanticVersionPart) {
        let actual = version.parse::<SemanticVersion>().unwrap().bump(part);
        assert_eq!(expected.parse::<SemanticVersion>().unwrap(), actual)
    }
}
