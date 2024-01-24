use std::{borrow::Borrow, collections::BTreeSet};

use litcheck::StaticCow;
use serde::Deserialize;

use super::BooleanExpr;

#[derive(Default, Clone, Deserialize)]
#[serde(transparent)]
pub struct FeatureSet {
    pub features: BTreeSet<StaticCow<str>>,
}
impl IntoIterator for FeatureSet {
    type Item = StaticCow<str>;
    type IntoIter = std::collections::btree_set::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.features.into_iter()
    }
}
impl FeatureSet {
    #[allow(unused)]
    pub fn new<I, S>(features: I) -> Self
    where
        StaticCow<str>: From<S>,
        I: IntoIterator<Item = S>,
    {
        let features = features.into_iter().map(StaticCow::from).collect();
        Self { features }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.features.is_empty()
    }

    pub fn contains<Q>(&self, feature: &Q) -> bool
    where
        Q: Ord + ?Sized,
        StaticCow<str>: Borrow<Q>,
    {
        self.features.contains(feature)
    }

    pub fn iter(&self) -> std::collections::btree_set::Iter<'_, StaticCow<str>> {
        self.features.iter()
    }

    pub fn insert<S>(&mut self, feature: S)
    where
        StaticCow<str>: From<S>,
    {
        self.features.insert(StaticCow::from(feature));
    }

    pub fn extend<I, S>(&mut self, features: I)
    where
        StaticCow<str>: From<S>,
        I: IntoIterator<Item = S>,
    {
        self.features
            .extend(features.into_iter().map(StaticCow::from));
    }

    pub fn missing_features<F: AsRef<BooleanExpr>>(&self, required: &[F]) -> Option<String> {
        use std::fmt::Write;

        let mut buf = String::new();
        for expr in required {
            let expr = expr.as_ref();
            if !expr.evaluate(&self.features) {
                if buf.is_empty() {
                    buf.push_str("Test requires the following unavailable features: ");
                } else {
                    buf.push_str(", ");
                }
                write!(&mut buf, "{}", expr).unwrap();
            }
        }
        if buf.is_empty() {
            None
        } else {
            Some(buf)
        }
    }

    pub fn unsupported_features<F: AsRef<BooleanExpr>>(&self, unsupported: &[F]) -> Option<String> {
        use std::fmt::Write;

        let mut buf = String::new();
        for expr in unsupported {
            let expr = expr.as_ref();
            if expr.evaluate(&self.features) {
                if buf.is_empty() {
                    buf.push_str("Test does not support the following features: ");
                } else {
                    buf.push_str(", ");
                }
                write!(&mut buf, "{}", expr).unwrap();
            }
        }
        if buf.is_empty() {
            None
        } else {
            Some(buf)
        }
    }
}
