use regex::Regex;
use std::{cmp::Ordering, error::Error, fmt::Display, num::ParseIntError, str::FromStr};

#[derive(Debug)]
pub(crate) struct SemanticVersion {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
    pub prerelease: Option<String>,
    pub buildmetadata: Option<String>,
}

impl PartialEq for SemanticVersion {
    fn eq(&self, other: &Self) -> bool {
        self.major == other.major
            && self.minor == other.minor
            && self.patch == other.patch
            && self.prerelease == other.prerelease
        // buildmetadata has no bearing on equivalence
    }
}

impl PartialOrd for SemanticVersion {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.major.partial_cmp(&other.major) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        match self.minor.partial_cmp(&other.minor) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        match self.patch.partial_cmp(&other.patch) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }

        // regular version numbers are equal at this point, so compare prerelease
        match (&self.prerelease, &other.prerelease) {
            // no prerelease; versions are equal
            (None, None) => Some(Ordering::Equal),

            // if a version without a prerelease is otherwise equal to a version with a prerelease, the version without takes precedence
            (Some(_), None) => Some(Ordering::Less),
            (None, Some(_)) => Some(Ordering::Greater),

            (Some(self_prerelease_str), Some(other_prerelease_str)) => {
                let self_ids: Vec<_> = self_prerelease_str.split('.').collect();
                let other_ids: Vec<_> = other_prerelease_str.split('.').collect();

                for (self_id, other_id) in self_ids.iter().zip(other_ids.iter()) {
                    let self_id_numeric_result = self_id.parse::<u64>();
                    let other_id_numeric_result = other_id.parse::<u64>();

                    match (self_id_numeric_result, other_id_numeric_result) {
                        // numeric identifiers are compared
                        (Ok(self_id_numeric), Ok(other_id_numeric)) => {
                            let numeric_order = self_id_numeric.cmp(&other_id_numeric);
                            if numeric_order != Ordering::Equal {
                                return Some(numeric_order);
                            }
                        }

                        // non-numeric identifiers take precedence over numeric identifiers
                        (Ok(_), Err(_)) => {
                            return Some(Ordering::Less);
                        }
                        (Err(_), Ok(_)) => {
                            return Some(Ordering::Greater);
                        }

                        // neither numeric; move to string comparison
                        (Err(_), Err(_)) => (),
                    }

                    // compare identifiers as strings
                    let str_order = self_id.cmp(other_id);
                    if str_order != Ordering::Equal {
                        return Some(str_order);
                    }
                }

                // the identifier comparison above is limited by the min number of prerelease identifiers between the two
                // at this point, the prerelease ids are equivalent up til and including that min bound. precedence is
                // finally determined by the largest number of prerelease ids
                Some(match (self_ids.len(), other_ids.len()) {
                    (s, o) if s > o => Ordering::Greater,
                    (s, o) if s < o => Ordering::Less,
                    _ => Ordering::Equal,
                })
            }
        }
    }
}

impl SemanticVersion {
    pub fn bump(&self, part: SemanticVersionPart) -> Self {
        match part {
            SemanticVersionPart::Major => Self {
                major: self.major + 1,
                minor: 0,
                patch: 0,
                prerelease: self.prerelease.as_ref().map(|s| s.as_str().to_owned()),
                buildmetadata: self.buildmetadata.as_ref().map(|s| s.as_str().to_owned()),
                ..*self
            },
            SemanticVersionPart::Minor => Self {
                minor: self.minor + 1,
                patch: 0,
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
pub(crate) enum SemanticVersionParseError {
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
pub(crate) enum SemanticVersionPart {
    Major,
    Minor,
    Patch,
}

#[derive(Debug, PartialEq)]
pub(crate) enum SemanticVersionPartParseError {
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

    #[test]
    fn bump_version_major_resets_minor_and_patch() {
        let bumped_version = SemanticVersion::from_str("1.2.3")
            .unwrap()
            .bump(SemanticVersionPart::Major);
        assert_eq!(SemanticVersion::from_str("2.0.0").unwrap(), bumped_version)
    }

    #[test]
    fn bump_version_minor_resets_patch() {
        let bumped_version = SemanticVersion::from_str("1.2.3")
            .unwrap()
            .bump(SemanticVersionPart::Minor);
        assert_eq!(SemanticVersion::from_str("1.3.0").unwrap(), bumped_version)
    }

    #[test]
    fn precedence_of_normal_versions() {
        let first = SemanticVersion::from_str("1.0.0").unwrap();
        let second = SemanticVersion::from_str("2.0.0").unwrap();
        let third = SemanticVersion::from_str("2.1.0").unwrap();
        let forth = SemanticVersion::from_str("2.1.1").unwrap();

        assert!(first < second && second < third && third < forth);
        assert!(first == first)
    }

    #[test]
    fn precedence_prerelease_and_normal_version() {
        let normal = SemanticVersion::from_str("1.0.0").unwrap();
        let prerelease = SemanticVersion::from_str("1.0.0-alpha").unwrap();

        assert!(prerelease < normal)
    }

    #[test]
    fn precedence_between_prereleases() {
        let v1 = SemanticVersion::from_str("1.0.0-alpha").unwrap();
        let v2 = SemanticVersion::from_str("1.0.0-alpha.1").unwrap();
        let v3 = SemanticVersion::from_str("1.0.0-alpha.beta").unwrap();
        let v4 = SemanticVersion::from_str("1.0.0-beta").unwrap();
        let v5 = SemanticVersion::from_str("1.0.0-beta.2").unwrap();
        let v6 = SemanticVersion::from_str("1.0.0-beta.11").unwrap();
        let v7 = SemanticVersion::from_str("1.0.0-rc.1").unwrap();
        let v8 = SemanticVersion::from_str("1.0.0").unwrap();

        assert!(v1 < v2 && v2 < v3 && v3 < v4 && v4 < v5 && v5 < v6 && v6 < v7 && v7 < v8)
    }

    #[test]
    fn precedence_unaffected_by_buildmetadata() {
        let no_buildmeta = SemanticVersion::from_str("1.0.0").unwrap();
        let with_buildmeta = SemanticVersion::from_str("1.0.0+asdf").unwrap();

        assert_eq!(no_buildmeta, with_buildmeta);

        let no_buildmeta_greater = SemanticVersion::from_str("2.0.0").unwrap();
        assert!(no_buildmeta_greater > with_buildmeta);

        let prerelease_version = SemanticVersion::from_str("1.0.0-alpha").unwrap();
        assert!(prerelease_version < with_buildmeta);
    }

    fn test_bump(expected: &str, version: &str, part: SemanticVersionPart) {
        let actual = version.parse::<SemanticVersion>().unwrap().bump(part);
        assert_eq!(expected.parse::<SemanticVersion>().unwrap(), actual)
    }
}
