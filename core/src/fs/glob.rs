use std::{collections::BTreeSet, fmt, ops::Deref, path::Path, str::FromStr};

use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// Represents a glob pattern which can match strings or paths
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pattern(glob::Pattern);
impl fmt::Display for Pattern {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}
impl FromStr for Pattern {
    type Err = <glob::Pattern as FromStr>::Err;

    #[inline]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse().map(Self)
    }
}
impl Deref for Pattern {
    type Target = glob::Pattern;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl Serialize for Pattern {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}
impl<'de> Deserialize<'de> for Pattern {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        use serde::de::Visitor;

        struct PatternVisitor;
        impl<'de> Visitor<'de> for PatternVisitor {
            type Value = Pattern;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a valid glob pattern")
            }

            fn visit_str<E>(self, s: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                s.parse::<Pattern>().map_err(serde::de::Error::custom)
            }
        }

        deserializer.deserialize_str(PatternVisitor)
    }
}

/// Represents a set of glob patterns which are applied as a unit
#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct PatternSet(BTreeSet<Pattern>);
impl PatternSet {
    /// Returns true if there are no patterns in this set
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Pattern> + '_ {
        self.0.iter()
    }

    /// Returns true if any pattern in the set matches `s`
    pub fn matches(&self, s: &str) -> bool {
        self.0.iter().any(|pattern| pattern.matches(s))
    }

    /// Returns true if any pattern in the set matches `path`
    pub fn matches_path(&self, path: &Path) -> bool {
        self.0.iter().any(|pattern| pattern.matches_path(path))
    }
}
impl FromIterator<Pattern> for PatternSet {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Pattern>,
    {
        Self(iter.into_iter().collect())
    }
}
impl<'a> FromIterator<&'a Pattern> for PatternSet {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = &'a Pattern>,
    {
        Self(iter.into_iter().cloned().collect())
    }
}
