use std::process::ExitCode;

use clap::Parser;
use filecheck::{Config, Test};
use litcheck::{
    Input,
    diagnostics::{DiagResult, IntoDiagnostic},
};

use super::Command;

#[derive(Debug, Parser)]
#[command(name = "filecheck")]
pub struct FileCheck {
    /// The path to the file containing patterns, e.g. `CHECK`, to
    /// match against the `input_file`.
    ///
    /// Typically, this file is also the source code for the test, i.e.
    /// the test file is fed into a command which produces some output
    /// derived from the test file, and FileCheck validates that output
    /// using the CHECK directives found in the test file.
    #[arg(value_name = "CHECK_FILE")]
    pub match_file: Input,
    /// The path to the file to verify.
    ///
    /// By default this reads from standard input
    #[arg(long, value_name = "PATH", default_value = "-")]
    pub input_file: Input,
    #[command(flatten)]
    pub options: filecheck::Options,
}
impl Command for FileCheck {
    fn is_help_requested(&self) -> bool {
        self.match_file.path().as_os_str() == "help"
    }

    fn run(mut self) -> DiagResult<ExitCode> {
        if let Some(filecheck_opts) = std::env::var("FILECHECK_OPTS").ok()
            && let Some(extra_args) = shlex::split(&filecheck_opts)
        {
            <filecheck::Options as clap::Parser>::update_from(&mut self.options, extra_args);
        }

        let mut options = self.options;
        options.comment_prefixes.sort();
        options.comment_prefixes.dedup();
        options.check_prefixes.sort();
        options.check_prefixes.dedup();

        options.validate()?;

        let config = Config {
            options,
            ..Default::default()
        };

        let match_file = self
            .match_file
            .into_source(true, config.source_manager())
            .into_diagnostic()?;
        let input_file = self
            .input_file
            .into_source(true, config.source_manager())
            .into_diagnostic()?;
        let mut test = Test::new(match_file, &config);
        test.verify(input_file).map(|_| ExitCode::SUCCESS)
    }
}
