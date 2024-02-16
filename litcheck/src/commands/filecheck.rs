use std::process::ExitCode;

use clap::Parser;
use filecheck::Test;
use litcheck::{
    diagnostics::{DiagResult, IntoDiagnostic},
    Input,
};

use super::Command;

#[derive(Debug, Parser)]
#[command(name = "filecheck", arg_required_else_help(true))]
pub struct FileCheck {
    /// The path to the file containing patterns, e.g. `CHECK`, to
    /// match against the `input_file`.
    ///
    /// Typically, this file is also the source code for the test, i.e.
    /// the test file is fed into a command which produces some output
    /// derived from the test file, and FileCheck validates that output
    /// using the CHECK directives found in the test file.
    #[arg(value_name = "CHECK")]
    pub match_file: Input,
    /// The path to the file to verify.
    ///
    /// By default this reads from standard input
    #[arg(value_name = "VERIFY", default_value = "-")]
    pub input_file: Input,
    #[command(flatten)]
    pub config: filecheck::Config,
}
impl Command for FileCheck {
    fn is_help_requested(&self) -> bool {
        self.match_file.path().as_os_str() == "help"
    }

    fn run(self) -> DiagResult<ExitCode> {
        let mut config = self.config;
        config.comment_prefixes.sort();
        config.comment_prefixes.dedup();
        config.check_prefixes.sort();
        config.check_prefixes.dedup();

        let match_file = self.match_file.into_source(true).into_diagnostic()?;
        let input_file = self.input_file.into_source(true).into_diagnostic()?;
        let mut test = Test::new(match_file, &config);
        test.verify(input_file).map(|_| ExitCode::SUCCESS)
    }
}
