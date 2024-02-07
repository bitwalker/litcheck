use std::{
    ffi::{OsStr, OsString},
    process::ExitCode,
};

use clap::Parser;
use litcheck::diagnostics::DiagResult;

use super::Command;

/// `not` runs a command and exits with status code 0 if the command
/// doesn't crash, but exits with a non-zero status code.
///
/// If the `--crash` flag is given before any other argument, then
/// status 0 is returned only if the command crashes.
///
/// In all other scenarios, `not` exits with status code 1, with
/// the exception of errors with the command given to `not` itself.
/// If the program failed to execute for some reason, `not` will
/// exit with code 127 (not found) or 126 (some other error).
#[derive(Debug, Parser)]
#[command(name = "not", arg_required_else_help(true))]
pub struct Not {
    /// If set, returns true if the given command crashes, otherwise false
    #[arg(long("crash"), required(false), default_value_t = false)]
    pub expect_crash: bool,
    /// The command to run (and it's arguments)
    #[arg(value_name = "COMMAND", num_args(..), trailing_var_arg(true), allow_hyphen_values(true))]
    pub command: Vec<OsString>,
}
impl Command for Not {
    fn is_help_requested(&self) -> bool {
        self.command.first().map(OsString::as_os_str) == Some(OsStr::new("help"))
    }

    fn run(self) -> DiagResult<ExitCode> {
        let mut cmd = match self.command.split_first() {
            None => std::process::Command::new(&self.command[0]),
            Some((prog, argv)) => {
                let mut cmd = std::process::Command::new(prog);
                cmd.args(argv);
                cmd
            }
        };
        cmd.env("LLVM_DISABLE_CRASH_REPORT", "1");
        cmd.env("LLVM_DISABLE_SYMBOLIZATION", "1");

        let exit_status = match cmd.status() {
            Ok(exit_status) => exit_status,
            Err(err) => {
                eprintln!("{}", err);
                return match err.kind() {
                    std::io::ErrorKind::NotFound => Ok(ExitCode::from(127)),
                    _kind => Ok(ExitCode::from(126)),
                };
            }
        };

        match exit_status.code() {
            // Command crashed, but crash was expected
            None if self.expect_crash => Ok(ExitCode::SUCCESS),
            // Command crashed, and crash was unexpected
            None => Ok(ExitCode::from(1)),
            // Command did not crash, but crash was expected
            Some(_) if self.expect_crash => Ok(ExitCode::from(1)),
            // Command did not crash, and exited with status 0
            Some(0) => Ok(ExitCode::from(1)),
            // Command did not crash, and exited non-zero
            Some(_) => Ok(ExitCode::SUCCESS),
        }
    }
}
