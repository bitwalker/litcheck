use std::path::PathBuf;

use litcheck::diagnostics::{Diagnostic, SourceSpan};

use crate::test::TestConfigError;

#[derive(Debug, Diagnostic, thiserror::Error)]
pub enum TestSuiteError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Config(#[from] TestConfigError),
    #[error("failed to parse test suite configuration")]
    #[diagnostic()]
    Syntax {
        #[label("{error}")]
        span: Option<SourceSpan>,
        #[source]
        error: toml::de::Error,
    },
    #[error("invalid test suite configuration")]
    #[diagnostic()]
    EmptyName {
        #[label("name cannot be empty")]
        span: SourceSpan,
    },
    #[error("invalid test suite configuration")]
    #[diagnostic()]
    InvalidSourceDir {
        #[label("source directory '{}' is not a directory", .source_dir.display())]
        span: SourceSpan,
        source_dir: PathBuf,
    },
    #[error("invalid test suite configuration")]
    #[diagnostic()]
    InvalidWorkingDir {
        #[label("working directory '{}' is not a directory", .working_dir.display())]
        span: SourceSpan,
        working_dir: PathBuf,
    },
}
