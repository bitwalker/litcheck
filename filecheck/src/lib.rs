pub mod ast;
pub mod check;
mod context;
mod cursor;
mod env;
pub mod errors;
pub mod expr;
mod input;
pub mod parse;
pub mod pattern;
pub mod rules;
mod test;

pub use self::errors::{CheckFailedError, RelatedCheckError, RelatedError, TestFailed};
#[cfg(test)]
pub use self::test::TestContext;
pub use self::test::{Test, TestResult};

use clap::{builder::ValueParser, ArgAction, Args, ColorChoice, ValueEnum};
use litcheck::diagnostics::{DefaultSourceManager, DiagResult, Report, SourceManager};
use std::sync::Arc;

#[doc(hidden)]
pub use litcheck;

pub(crate) mod common {
    pub use std::{
        borrow::Cow,
        fmt,
        ops::{ControlFlow, RangeBounds},
        sync::Arc,
    };

    pub use either::Either::{self, Left, Right};
    #[cfg(test)]
    pub use litcheck::reporting;
    pub use litcheck::{
        diagnostics::{
            Diag, DiagResult, Diagnostic, FileName, Label, Report, SourceFile, SourceId,
            SourceLanguage, SourceManager, SourceManagerError, SourceManagerExt, SourceSpan, Span,
            Spanned,
        },
        range::{self, Range},
        text::{self, Newline},
        StringInterner, Symbol,
    };
    pub use regex_automata::{meta::Regex, util::look::LookMatcher};
    pub use smallvec::{smallvec, SmallVec};

    pub use crate::ast::{Check, Constraint};
    pub use crate::context::{Context, ContextExt, ContextGuard, MatchContext};
    pub use crate::cursor::{Cursor, CursorGuard, CursorPosition};
    pub use crate::env::{Env, LexicalScope, LexicalScopeMut, ScopeGuard};
    pub use crate::errors::{
        CheckFailedError, RelatedCheckError, RelatedError, RelatedLabel, TestFailed,
    };
    pub use crate::expr::{BinaryOp, Expr, Number, NumberFormat, Value, VariableName};
    pub use crate::input::Input;
    pub use crate::pattern::{
        AnyMatcher, CaptureInfo, MatchInfo, MatchResult, MatchType, Matcher, MatcherMut, Matches,
        Pattern, PatternIdentifier, PatternSearcher, Searcher,
    };
    pub use crate::rules::{DynRule, Rule};
    #[cfg(test)]
    pub(crate) use crate::test::TestContext;
    pub use crate::test::TestResult;
    pub use crate::Config;
}

pub const DEFAULT_CHECK_PREFIXES: &[&str] = &["CHECK"];
pub const DEFAULT_COMMENT_PREFIXES: &[&str] = &["COM", "RUN"];

/// FileCheck reads two files, one from standard input, and one specified on
/// the command line; and uses one to verify the other.
pub struct Config {
    pub source_manager: Arc<dyn SourceManager>,
    pub options: Options,
}

impl Config {
    #[inline(always)]
    pub fn source_manager(&self) -> &dyn SourceManager {
        &self.source_manager
    }

    /// Returns true if the user has passed -v, requesting diagnostic remarks be output for matches
    pub const fn remarks_enabled(&self) -> bool {
        self.options.verbose > 0
    }

    /// Returns true if the user has passed -vv, requesting verbose diagnostics be output
    ///
    /// NOTE: This is different than the tracing enabled via LITCHECK_TRACE which is tailored for
    /// diagnosing litcheck internals. Instead, the tracing referred to here is end user-oriented,
    /// and meant to provide helpful information to understand why a test is failing
    pub const fn tracing_enabled(&self) -> bool {
        self.options.verbose > 1
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            source_manager: Arc::from(DefaultSourceManager::default()),
            options: Options::default(),
        }
    }
}

impl core::fmt::Debug for Config {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        core::fmt::Debug::fmt(&self.options, f)
    }
}

/// FileCheck reads two files, one from standard input, and one specified on
/// the command line; and uses one to verify the other.
#[derive(Debug, Args)]
pub struct Options {
    /// Allow checking empty input. By default, empty input is rejected.
    #[arg(
        long,
        default_value_t = false,
        action(clap::ArgAction::SetTrue),
        help_heading = "Input"
    )]
    pub allow_empty: bool,
    /// Which prefixes to treat as directives.
    ///
    /// For example, in the directive `CHECK-SAME`, `CHECK` is the prefix.
    #[arg(
        long,
        alias = "check-prefix",
        value_name = "PREFIX",
        default_value = "CHECK",
        action(clap::ArgAction::Append),
        value_parser(prefix_value_parser()),
        value_delimiter(','),
        help_heading = "Syntax"
    )]
    pub check_prefixes: Vec<Arc<str>>,
    /// Which prefixes to treat as comments.
    ///
    /// All content on a line following a comment directive is ignored,
    /// up to the next newline.
    #[arg(
        long,
        alias = "comment-prefix",
        value_name = "PREFIX",
        default_value = "COM,RUN",
        action(clap::ArgAction::Append),
        value_parser(prefix_value_parser()),
        value_delimiter(','),
        help_heading = "Syntax"
    )]
    pub comment_prefixes: Vec<Arc<str>>,
    /// If specifying multiple check prefixes, this controls whether or not
    /// to raise an error if one of the prefixes is missing in the test file.
    #[arg(long, default_value_t = false, help_heading = "Syntax")]
    pub allow_unused_prefixes: bool,
    /// Disable default canonicalization of whitespace.
    ///
    /// By default, FileCheck canonicalizes horizontal whitespace (spaces and tabs)
    /// which causes it to ignore these differences (a space will match a tab).
    ///
    /// This flag disables horizontal whitespace canonicalization.
    ///
    /// Newlines are always canonicalized to LF regardless of this setting.
    #[arg(
        long = "strict-whitespace",
        default_value_t = false,
        help_heading = "Matching"
    )]
    pub strict_whitespace: bool,
    /// This flag changes the default matching behavior to require all positive
    /// matches to cover an entire line. Leading/trailing whitespace is ignored
    /// unless `--strict-whitespace` is also specified.
    ///
    /// By default, FileCheck allows matches of anywhere on a line, so by setting
    /// this, you effectively insert `{{^.*}}` or `{{^}}` before, and `{{[*$]}}`
    /// or `{{$}}` after every positive check pattern.
    ///
    /// NOTE Negative matches, i.e `CHECK-NOT` are not affected by this option.
    #[arg(long, default_value_t = false, help_heading = "Matching")]
    pub match_full_lines: bool,
    /// Disable case-sensitive matching
    #[arg(long, default_value_t = false, help_heading = "Matching")]
    pub ignore_case: bool,
    /// Adds implicit negative checks for the specified patterns between positive checks.
    ///
    /// This option allows writing stricter tests without polluting them with CHECK-NOTs.
    ///
    /// For example, `--implicit-check-not warning:` can be useful when testing diagnostic
    /// messages from tools that don’t have an option similar to `clang -verify`. With this
    /// option FileCheck will verify that input does not contain warnings not covered by any
    /// `CHECK:` patterns.
    #[arg(long, value_name = "CHECK", help_heading = "Matching")]
    pub implicit_check_not: Vec<Arc<str>>,
    /// Dump input to stderr, adding annotations representing currently enabled diagnostics.
    #[arg(long, value_enum, value_name = "TYPE", default_value_t = Dump::Fail, help_heading = "Output")]
    pub dump_input: Dump,
    /// Specify the parts of the input to dump when `--dump-input` is set.
    ///
    /// When specified, print only input lines of KIND, plus any context specified by `--dump-input-context`.
    ///
    /// Defaults to `error` when `--dump-input=fail`, and `all` when `--dump-input=always`
    #[arg(
        long,
        value_enum,
        value_name = "KIND",
        default_value_t = DumpFilter::Error,
        default_value_ifs([("dump-input", "fail", Some("error")), ("dump-input", "always", Some("all"))]),
        help_heading = "Output"
    )]
    pub dump_input_filter: DumpFilter,
    /// Enables scope for regex variables
    ///
    /// Variables with names that start with `$` are considered global, and remain set throughout the file
    ///
    /// All other variables get undefined after each encountered `CHECK-LABEL`
    #[arg(long, default_value_t = false, help_heading = "Variables")]
    pub enable_var_scope: bool,
    /// Set a pattern variable VAR with value VALUE that can be used in `CHECK:` lines
    ///
    /// You must specify each one in `key=value` format
    #[arg(
        long = "define",
        short = 'D',
        value_name = "NAME=VALUE",
        help_heading = "Variables"
    )]
    pub variables: Vec<expr::CliVariable>,
    /// Set the verbosity level.
    ///
    /// If specified a single time, it causes filecheck to print good directive pattern matches
    ///
    /// If specified multiple times, filecheck will emit internal diagnostics to aid in troubleshooting.
    ///
    /// If `--dump-input=fail` or `--dump-input=always`, add information as input annotations instead.
    #[arg(long, short = 'v', action = ArgAction::Count, help_heading = "Output")]
    pub verbose: u8,
    /// Whether, and how, to color terminal output
    #[arg(
        global(true),
        value_enum,
        long,
        default_value_t = ColorChoice::Auto,
        default_missing_value = "auto",
        help_heading = "Output"
    )]
    pub color: ColorChoice,
}

/// This is implemented for [Options] so that we can use [clap::Parser::update_from] on it.
impl clap::CommandFactory for Options {
    fn command() -> clap::Command {
        let cmd = clap::Command::new("filecheck")
            .no_binary_name(true)
            .disable_help_flag(true)
            .disable_version_flag(true);
        <Self as clap::Args>::augment_args(cmd)
    }

    fn command_for_update() -> clap::Command {
        let cmd = clap::Command::new("filecheck")
            .no_binary_name(true)
            .disable_help_flag(true)
            .disable_version_flag(true);
        <Self as clap::Args>::augment_args_for_update(cmd)
    }
}

/// This is implemented for [Options] in order to use [clap::Parser::update_from].
impl clap::Parser for Options {}

impl Default for Options {
    fn default() -> Self {
        Self {
            allow_empty: false,
            check_prefixes: vec![Arc::from("CHECK".to_string().into_boxed_str())],
            comment_prefixes: vec![
                Arc::from("COM".to_string().into_boxed_str()),
                Arc::from("RUN".to_string().into_boxed_str()),
            ],
            allow_unused_prefixes: false,
            strict_whitespace: false,
            match_full_lines: false,
            ignore_case: false,
            implicit_check_not: vec![],
            dump_input: Default::default(),
            dump_input_filter: Default::default(),
            enable_var_scope: false,
            variables: vec![],
            verbose: 0,
            color: Default::default(),
        }
    }
}

impl Options {
    pub fn validate(&self) -> DiagResult<()> {
        // Validate that we do not have overlapping check and comment prefixes
        if self
            .check_prefixes
            .iter()
            .any(|prefix| prefix.as_ref() != "CHECK")
        {
            for check_prefix in self.check_prefixes.iter() {
                if self.comment_prefixes.contains(check_prefix) {
                    return Err(Report::msg(format!("supplied check prefix must be unique among check and comment prefixes: '{check_prefix}'")));
                }
            }
        } else if self
            .comment_prefixes
            .iter()
            .any(|prefix| !matches!(prefix.as_ref(), "COM" | "RUN"))
        {
            for comment_prefix in self.comment_prefixes.iter() {
                if self.check_prefixes.contains(comment_prefix) {
                    return Err(Report::msg(format!("supplied comment prefix must be unique among check and comment prefixes: '{comment_prefix}'")));
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, ValueEnum)]
pub enum Dump {
    /// Explain input dump and quit
    Help,
    /// Always dump input
    Always,
    /// Dump input on failure
    #[default]
    Fail,
    /// Never dump input
    Never,
}

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, ValueEnum)]
pub enum DumpFilter {
    /// All input lines
    All,
    /// Input lines with annotations
    AnnotationFull,
    /// Input lines with starting points of annotations
    Annotation,
    /// Input lines with starting points of error annotations
    #[default]
    Error,
}

fn prefix_value_parser() -> ValueParser {
    use clap::{error::ErrorKind, Error};

    ValueParser::from(move |s: &str| -> Result<Arc<str>, clap::Error> {
        if s.is_empty() {
            return Err(Error::raw(
                ErrorKind::ValueValidation,
                "supplied prefix must not be an empty string",
            ));
        }
        if !s.starts_with(|c: char| c.is_ascii_alphabetic()) {
            return Err(Error::raw(
                ErrorKind::ValueValidation,
                "supplied prefix must start with an ASCII alphabetic character",
            ));
        }
        if s.contains(|c: char| !c.is_alphanumeric() && c != '_' && c != '-') {
            return Err(Error::raw(
                ErrorKind::ValueValidation,
                "supplied prefix may only contain ASCII alphanumerics, hyphens, or underscores",
            ));
        }
        Ok(Arc::from(s.to_owned().into_boxed_str()))
    })
}

/// Use `filecheck` in a Rust test directly against an input value that implements `Display`.
///
/// ## Example
///
/// ```rust
/// #![expect(unstable_name_collisions)]
/// use litcheck_filecheck::filecheck;
/// use itertools::Itertools;
///
/// let original = "abbc";
/// let modified = original.chars().intersperse('\n').collect::<String>();
///
/// filecheck!(modified, "
/// ; CHECK: a
/// ; CHECK-NEXT: b
/// ; CHECK-NEXT: b
/// ; CHECK-NEXT: c
/// ");
/// ```
///
/// If custom configuration is desired, you may instantiate the `filecheck` configuration (see
/// [Config]) and pass it as an additional parameter:
///
/// ```rust
/// #![expect(unstable_name_collisions)]
/// use litcheck_filecheck::{filecheck, Config, Options};
/// use itertools::Itertools;
///
/// let original = "abbc";
/// let modified = original.chars().intersperse('\n').collect::<String>();
/// let config = Config {
///     options: Options {
///         match_full_lines: true,
///         ..Options::default()
///     },
///     ..Config::default()
/// };
///
/// filecheck!(modified, "
/// ; CHECK: a
/// ; CHECK-NEXT: b
/// ; CHECK-NEXT: b
/// ; CHECK-NEXT: c
/// ");
/// ```
///
/// If successful, the `filecheck!` macro returns the pattern matches produced by verifying the
/// checks, allowing you to examine them in more detail.
#[macro_export]
macro_rules! filecheck {
    ($input:expr, $checks:expr) => {
        $crate::filecheck!($input, $checks, $crate::Config::default())
    };

    ($input:expr, $checks:expr, $config:expr) => {{
        let config = $config;
        let input = $crate::source_file!(config, $input.to_string());
        let checks = $crate::source_file!(config, $checks.to_string());
        let mut test = $crate::Test::new(checks, &config);
        match test.verify(input) {
            Err(err) => {
                let printer = $crate::litcheck::reporting::PrintDiagnostic::new(err);
                panic!("{printer}");
            }
            Ok(matches) => matches,
        }
    }};
}
