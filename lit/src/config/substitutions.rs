use std::{borrow::Borrow, borrow::Cow, hash::Hash};

use litcheck::{
    diagnostics::{Diagnostic, SourceSpan, Span},
    StaticCow,
};
use serde::Deserialize;

use crate::FxIndexMap;

#[derive(Diagnostic, Debug, thiserror::Error)]
#[error("invalid substitution pattern: '{pattern}'")]
#[diagnostic()]
pub struct InvalidSubstitutionPatternError {
    #[label("{error}")]
    span: SourceSpan,
    #[source]
    error: regex::Error,
    pattern: String,
}

pub struct ScopedSubstitutionSet<'a> {
    parent: &'a SubstitutionSet,
    set: SubstitutionSet,
}
impl<'scope> ScopedSubstitutionSet<'scope> {
    pub fn new(parent: &'scope SubstitutionSet) -> Self {
        Self {
            parent,
            set: SubstitutionSet::default(),
        }
    }

    #[allow(unused)]
    pub fn is_empty(&self) -> bool {
        self.set.is_empty() && self.parent.is_empty()
    }

    #[allow(unused)]
    pub fn get<Q>(&self, pattern: &Q) -> Option<&StaticCow<str>>
    where
        StaticCow<str>: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.set.get(pattern).or_else(|| self.parent.get(pattern))
    }

    pub fn contains<Q>(&self, pattern: &Q) -> bool
    where
        StaticCow<str>: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.set.contains(pattern) || self.parent.contains(pattern)
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (&StaticCow<str>, &StaticCow<str>)> + '_ {
        SubstitutionsIter {
            set: &self.set,
            iter: self.parent.iter(),
            filter: true,
        }
    }

    // Identify previously defined substitutions that may conflict with this one
    pub fn find_matching<'a>(&'a self, pattern: &'a str) -> Matches<'a> {
        let has_exact = self.contains(pattern);
        let has_fuzzy = self
            .iter()
            .any(|(k, _)| k != pattern && k.contains(pattern));
        match (has_exact, has_fuzzy) {
            (false, _) => Matches::Empty,
            (true, false) => Matches::Exact(pattern),
            (true, true) => Matches::Fuzzy {
                exact: pattern,
                keys: SubstitutionsIter {
                    set: &self.set,
                    iter: self.parent.iter(),
                    filter: true,
                },
            },
        }
    }

    pub fn extend<I, K, V>(&mut self, substitutions: I)
    where
        StaticCow<str>: From<K>,
        StaticCow<str>: From<V>,
        I: IntoIterator<Item = (K, V)>,
    {
        let substitutions = substitutions
            .into_iter()
            .map(|(k, v)| (StaticCow::from(k), StaticCow::from(v)));
        self.set.extend(substitutions);
    }

    pub fn insert<K, V>(&mut self, pattern: K, replacement: V)
    where
        StaticCow<str>: From<K>,
        StaticCow<str>: From<V>,
    {
        self.set
            .insert(StaticCow::from(pattern), StaticCow::from(replacement));
    }

    pub fn apply<'a>(
        &self,
        input: Span<&'a str>,
    ) -> Result<Cow<'a, str>, InvalidSubstitutionPatternError> {
        let escape_re = regex::Regex::new("%%").unwrap();

        let (span, input) = input.into_parts();
        let mut buffer = Cow::Borrowed(input);
        let mut needs_unescape = false;
        let mut needs_escaping = true;
        for (pattern, replacement) in self.iter() {
            if needs_escaping {
                if let Cow::Owned(escaped) =
                    escape_re.replace_all(&buffer, regex::NoExpand("#_MARKER_#"))
                {
                    buffer = Cow::Owned(escaped);
                    needs_unescape = true;
                } else {
                    needs_escaping = false;
                }
            }
            let re =
                regex::Regex::new(pattern).map_err(|error| InvalidSubstitutionPatternError {
                    span,
                    error,
                    pattern: pattern.clone().into_owned(),
                })?;
            if let Cow::Owned(replaced) = re.replace_all(&buffer, replacement) {
                buffer = Cow::Owned(replaced);
                needs_escaping = true;
            }
        }

        if needs_unescape {
            let unescape_re = regex::Regex::new("#_MARKER_#").unwrap();
            if let Cow::Owned(unescaped) = unescape_re.replace_all(&buffer, regex::NoExpand("%")) {
                buffer = Cow::Owned(unescaped);
            }
        }

        Ok(buffer)
    }
}

pub struct SubstitutionsIter<'a> {
    set: &'a SubstitutionSet,
    iter: indexmap::map::Iter<'a, StaticCow<str>, StaticCow<str>>,
    filter: bool,
}
impl<'a> Iterator for SubstitutionsIter<'a> {
    type Item = (&'a StaticCow<str>, &'a StaticCow<str>);

    fn next(&mut self) -> Option<Self::Item> {
        if self.filter {
            let result = self
                .iter
                .next()
                .and_then(|item @ (k, _)| self.set.get(k).map(|v| (k, v)).or(Some(item)));
            if result.is_none() {
                self.iter = self.set.iter();
                self.filter = false;
            } else {
                return result;
            }
        }
        self.iter.next()
    }
}

#[derive(Default, Clone, Debug, Deserialize)]
#[serde(transparent)]
pub struct SubstitutionSet {
    set: FxIndexMap<StaticCow<str>, StaticCow<str>>,
}
impl IntoIterator for SubstitutionSet {
    type Item = (StaticCow<str>, StaticCow<str>);
    type IntoIter = indexmap::map::IntoIter<StaticCow<str>, StaticCow<str>>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.set.into_iter()
    }
}
impl SubstitutionSet {
    #[allow(unused)]
    pub fn new<I, K, V>(substitutions: I) -> Self
    where
        StaticCow<str>: From<K>,
        StaticCow<str>: From<V>,
        I: IntoIterator<Item = (K, V)>,
    {
        let set = substitutions
            .into_iter()
            .map(|(k, v)| (StaticCow::from(k), StaticCow::from(v)))
            .collect();
        Self { set }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.set.is_empty()
    }

    pub fn contains<Q>(&self, pattern: &Q) -> bool
    where
        StaticCow<str>: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.set.contains_key(pattern)
    }

    pub fn get<Q>(&self, pattern: &Q) -> Option<&StaticCow<str>>
    where
        StaticCow<str>: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.set.get(pattern)
    }

    #[inline]
    pub fn iter(&self) -> indexmap::map::Iter<'_, StaticCow<str>, StaticCow<str>> {
        self.set.iter()
    }

    #[inline]
    pub fn keys(&self) -> indexmap::map::Keys<'_, StaticCow<str>, StaticCow<str>> {
        self.set.keys()
    }

    pub fn extend<I, K, V>(&mut self, substitutions: I)
    where
        StaticCow<str>: From<K>,
        StaticCow<str>: From<V>,
        I: IntoIterator<Item = (K, V)>,
    {
        let substitutions = substitutions
            .into_iter()
            .map(|(k, v)| (StaticCow::from(k), StaticCow::from(v)));
        self.set.extend(substitutions);
    }

    pub fn insert<K, V>(&mut self, pattern: K, replacement: V)
    where
        StaticCow<str>: From<K>,
        StaticCow<str>: From<V>,
    {
        self.set
            .insert(StaticCow::from(pattern), StaticCow::from(replacement));
    }
}

pub enum Matches<'a> {
    Empty,
    Exact(&'a str),
    Fuzzy {
        exact: &'a str,
        keys: SubstitutionsIter<'a>,
    },
}
impl<'a> Iterator for Matches<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Empty => None,
            Self::Exact(s) => {
                let s = *s;
                *self = Self::Empty;
                Some(s)
            }
            Self::Fuzzy { exact, keys } => loop {
                if let Some((key, _)) = keys.next() {
                    if key.contains(*exact) {
                        break Some(key.as_ref());
                    }
                } else {
                    *self = Self::Empty;
                    return None;
                }
            },
        }
    }
}
