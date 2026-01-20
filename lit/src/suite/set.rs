use std::{borrow::Borrow, fmt, path::Path, sync::Arc};

use intrusive_collections::{intrusive_adapter, RBTreeAtomicLink};
use serde::Deserialize;

use super::TestSuite;

pub type TestSuiteCursor<'a> = intrusive_collections::rbtree::Cursor<'a, TestSuiteAdapter>;

intrusive_adapter!(pub TestSuiteAdapter = Arc<TestSuite>: TestSuite { link => RBTreeAtomicLink });
impl<'a> intrusive_collections::KeyAdapter<'a> for TestSuiteAdapter {
    type Key = TestSuiteKey;

    #[inline]
    fn get_key(&self, suite: &'a TestSuite) -> Self::Key {
        suite.id()
    }
}

#[derive(Default)]
pub struct TestSuiteSet {
    len: usize,
    suites: intrusive_collections::RBTree<TestSuiteAdapter>,
}
impl TestSuiteSet {
    pub fn is_empty(&self) -> bool {
        self.suites.is_empty()
    }

    #[allow(unused)]
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn iter(&self) -> Iter<'_> {
        Iter::new(self.suites.front())
    }

    pub fn insert(&mut self, suite: Arc<TestSuite>) -> bool {
        use intrusive_collections::rbtree::Entry;

        match self.suites.entry(&suite.id()) {
            Entry::Occupied(_) => {
                log::debug!(
                    "attempted to register multiple test suites with the same key: {:?}",
                    suite.id()
                );
                false
            }
            Entry::Vacant(entry) => {
                entry.insert(suite);
                self.len += 1;
                true
            }
        }
    }

    pub fn get<Q>(&self, key: &Q) -> Option<Arc<TestSuite>>
    where
        Q: Ord + ?Sized,
        TestSuiteKey: Borrow<Q>,
    {
        let cursor = self.suites.find(key);
        cursor.clone_pointer()
    }

    pub fn clear(&mut self) {
        self.suites.clear();
        self.len = 0;
    }
}

pub struct Iter<'a> {
    cursor: TestSuiteCursor<'a>,
}
impl<'a> Iter<'a> {
    pub fn new(cursor: TestSuiteCursor<'a>) -> Self {
        Self { cursor }
    }
}
impl<'a> core::iter::FusedIterator for Iter<'a> {}
impl<'a> Iterator for Iter<'a> {
    type Item = Arc<TestSuite>;

    fn next(&mut self) -> Option<Self::Item> {
        let suite = self.cursor.clone_pointer();
        if suite.is_some() {
            self.cursor.move_next();
        }
        suite
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize)]
pub struct TestSuiteKey {
    /// The name of the test suite, may not be empty.
    name: toml::Spanned<Arc<str>>,
    /// The source path from which the test suite configuration was loaded
    #[serde(skip, default)]
    pub path: Option<Arc<Path>>,
}
impl TestSuiteKey {
    pub fn new(name: toml::Spanned<Arc<str>>, path: Option<Arc<Path>>) -> Self {
        Self { name, path }
    }

    /// The name of the suite to which this key is associated
    #[inline]
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    /// The path to the configuration file for the suite to which this key is associated
    pub fn path(&self) -> &Path {
        self.path.as_deref().unwrap_or(Path::new(""))
    }

    pub fn span(&self) -> core::ops::Range<usize> {
        self.name.span()
    }
}
impl fmt::Display for TestSuiteKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name = self.name();
        let path = self.path();
        match std::env::current_dir() {
            Ok(cwd) => {
                let path = path.strip_prefix(cwd).unwrap_or(path);
                write!(f, "{name} @ {}", path.display())
            }
            Err(_) => {
                write!(f, "{name} @ {}", path.display())
            }
        }
    }
}
