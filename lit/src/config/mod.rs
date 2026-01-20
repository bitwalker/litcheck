mod expr;
mod features;
pub(crate) mod substitutions;

pub use self::expr::*;
pub use self::features::FeatureSet;
pub use self::substitutions::{
    InvalidSubstitutionPatternError, ScopedSubstitutionSet, SubstitutionSet,
};

use std::{fmt, path::PathBuf, sync::Arc};

use clap::Args;
use litcheck::diagnostics::{DefaultSourceManager, SourceManagerSync};
use regex::Regex;

pub type Variable = litcheck::variables::Variable<String>;

pub struct Config {
    pub options: Options,
    pub source_manager: Arc<dyn SourceManagerSync>,
}

impl Config {
    pub fn new(tests: Vec<String>) -> Self {
        Self {
            options: Options {
                tests,
                workers: None,
                params: vec![],
                quiet: false,
                verbose: false,
                all: false,
                no_execute: false,
                search_paths: vec![],
                timeout: None,
                filter: None,
                filter_out: None,
                target: None,
            },
            source_manager: Arc::from(DefaultSourceManager::default()),
        }
    }

    #[inline(always)]
    pub fn source_manager(&self) -> &dyn SourceManagerSync {
        &self.source_manager
    }

    /// Returns true if a test with `name` should be selected for execution
    pub fn is_selected(&self, name: &str) -> bool {
        let is_selected = self
            .options
            .filter
            .as_ref()
            .map(|filter| filter.is_match(name))
            .unwrap_or(true);
        let is_excluded = self
            .options
            .filter_out
            .as_ref()
            .map(|filter_out| filter_out.is_match(name))
            .unwrap_or(false);
        is_selected && !is_excluded
    }

    pub fn host(&self) -> target_lexicon::Triple {
        target_lexicon::Triple::host()
    }

    pub fn target(&self) -> target_lexicon::Triple {
        self.options
            .target
            .clone()
            .unwrap_or_else(target_lexicon::Triple::host)
    }
}

impl Default for Config {
    #[inline]
    fn default() -> Self {
        Self::new(vec![])
    }
}

impl fmt::Debug for Config {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.options, f)
    }
}
#[derive(Debug, Args)]
pub struct Options {
    /// The test files or directories to search for tests
    #[arg(value_name = "TESTS", required(true), trailing_var_arg(true))]
    pub tests: Vec<String>,
    /// Run `N` tests in parallel
    ///
    /// By default, this is automatically chosen to match the number of available CPUs
    #[arg(long, short = 'j', value_name = "N")]
    pub workers: Option<usize>,
    /// Add a user-defined parameter NAME with the given optional VALUE.
    #[arg(long = "param", short = 'D', value_name = "NAME=[VALUE]")]
    pub params: Vec<Variable>,
    /// Suppress any output except for test failures.
    #[arg(long, short = 'q', default_value_t = false, help_heading = "Output")]
    pub quiet: bool,
    /// Show more information on test failures, for example, the entire test output instead
    /// of just the result.
    ///
    /// Each command is printed before it is executed, along with where the command was derived from.
    #[arg(long, short = 'v', default_value_t = false, help_heading = "Output")]
    pub verbose: bool,
    /// Enable `-v`, but for all tests, not just failed tests
    #[arg(
        long = "show-all",
        short = 'a',
        default_value_t = false,
        help_heading = "Output"
    )]
    pub all: bool,
    /// Don't execute any tests (assume PASS).
    ///
    /// For example, this will still parse test cases for RUN commands, but will not
    /// actually execute those commands to run the tests.
    #[arg(
        long = "no-execute",
        default_value_t = false,
        help_heading = "Execution Options"
    )]
    pub no_execute: bool,
    /// Specify an additional PATH to use when searching for executables in tests.
    #[arg(
        long = "path",
        value_name = "PATH",
        value_parser(canonical_path_parser()),
        help_heading = "Execution Options"
    )]
    pub search_paths: Vec<PathBuf>,
    /// Spend at most N seconds running each individual test.
    ///
    /// 0 means no time limit, and is the default value.
    #[arg(
        long,
        value_name = "N",
        default_value = "0",
        help_heading = "Execution Options"
    )]
    pub timeout: Option<u64>,
    /// Run only those tests whose name matches the given regular expression.
    #[arg(long, env = "LIT_FILTER", help_heading = "Selection Options")]
    pub filter: Option<Regex>,
    /// Exclude those tests whose name matches the given regular expression.
    #[arg(long, env = "LIT_FILTER_OUT", help_heading = "Selection Options")]
    pub filter_out: Option<Regex>,
    /// If specified, will set available features based on the provided target triple.
    ///
    /// Expects a string like 'x86_64-apple-darwin'.
    ///
    /// The default target is the host
    #[arg(
        long,
        value_parser(target_triple_parser()),
        value_name = "TRIPLE",
        help_heading = "Selection Options"
    )]
    pub target: Option<target_lexicon::Triple>,
}

fn target_triple_parser() -> clap::builder::ValueParser {
    use clap::{builder::ValueParser, error::ErrorKind, Error};
    ValueParser::from(|s: &str| -> Result<target_lexicon::Triple, clap::Error> {
        s.parse::<target_lexicon::Triple>().map_err(|err| {
            Error::raw(
                ErrorKind::ValueValidation,
                format!("invalid target triple '{s}': {err}"),
            )
        })
    })
}

fn canonical_path_parser() -> clap::builder::ValueParser {
    use clap::{builder::ValueParser, error::ErrorKind, Error};
    ValueParser::from(|s: &str| -> Result<PathBuf, clap::Error> {
        let path = std::path::Path::new(s);
        path.canonicalize().map_err(|err| {
            Error::raw(
                ErrorKind::ValueValidation,
                format!("invalid path '{s}': {err}"),
            )
        })
    })
}
