#![cfg_attr(test, feature(assert_matches))]
pub mod check;
pub mod expr;
pub mod parse;
#[cfg(test)]
mod testing;
#[cfg(test)]
mod tests;

pub use self::check::FileCheckTest;

use clap::{builder::ValueParser, ArgAction, Args, ColorChoice, ValueEnum};

/// FileCheck reads two files, one from standard input, and one specified on
/// the command line; and uses one to verify the other.
#[derive(Debug, Args)]
pub struct Config {
    /// Which prefixes to treat as directives.
    ///
    /// For example, in the directive `CHECK-SAME`, `CHECK` is the prefix.
    #[arg(
        long = "check-prefix",
        value_name = "PREFIX",
        default_value = "CHECK",
        value_parser(re_value_parser("^[A-Za-z][A-Za-z0-9_]*")),
        help_heading = "Syntax"
    )]
    pub check_prefixes: Vec<Box<str>>,
    /// Which prefixes to treat as comments.
    ///
    /// All content on a line following a comment directive is ignored,
    /// up to the next newline.
    #[arg(
        long = "comment-prefix",
        value_name = "PREFIX",
        default_value = "COM,RUN",
        value_parser(re_value_parser("^[A-Za-z][A-Za-z0-9_]*")),
        help_heading = "Syntax"
    )]
    pub comment_prefixes: Vec<Box<str>>,
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
    pub implicit_check_not: Vec<String>,
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
impl Default for Config {
    fn default() -> Self {
        Self {
            check_prefixes: vec!["CHECK".to_string().into_boxed_str()],
            comment_prefixes: vec![
                "COM".to_string().into_boxed_str(),
                "RUN".to_string().into_boxed_str(),
            ],
            allow_unused_prefixes: true,
            strict_whitespace: false,
            match_full_lines: false,
            ignore_case: false,
            implicit_check_not: vec![],
            dump_input: Dump::Fail,
            dump_input_filter: DumpFilter::Error,
            enable_var_scope: false,
            variables: vec![],
            verbose: 0,
            color: Default::default(),
        }
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

fn re_value_parser(r: &'static str) -> ValueParser {
    use clap::{error::ErrorKind, Error};

    ValueParser::from(move |s: &str| -> Result<Box<str>, clap::Error> {
        let re = regex::Regex::new(r).unwrap();
        if re.is_match(s) {
            Ok(s.to_owned().into_boxed_str())
        } else {
            Err(Error::raw(
                ErrorKind::ValueValidation,
                "'{s}' does not match expected pattern `{r}`",
            ))
        }
    })
}
