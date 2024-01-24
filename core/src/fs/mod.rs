mod filesearch;
mod glob;
mod path_tree;

pub use self::filesearch::*;
pub use self::glob::*;
pub use self::path_tree::PathPrefixTree;

use std::{borrow::Cow, path::Path};

/// Returns an empty placeholder path value for use with [serde::Deserialize]
#[inline]
pub fn empty_path() -> Cow<'static, Path> {
    Cow::Borrowed(Path::new(""))
}

/// Returns a sane default value for the `source_dir` field of [TestSuite]
#[inline]
pub fn default_source_dir() -> &'static Path {
    Path::new(".")
}

/// Returns a new temporary directory that can be used for transient artifacts
#[inline]
pub fn default_temp_dir() -> Box<Path> {
    std::env::temp_dir().into_boxed_path()
}
