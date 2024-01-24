mod filecheck;
mod lit;

pub use self::filecheck::FileCheck;
pub use self::lit::Lit;

use litcheck::diagnostics::DiagResult;

pub trait Command: clap::FromArgMatches {
    /// Return true if help/usage information should be printed
    fn is_help_requested(&self) -> bool;

    /// The main entrypoint for the command
    fn run(self) -> DiagResult<std::process::ExitCode>;
}

/// Return a collection containing all of the enabled commands
pub fn all() -> impl IntoIterator<Item = clap::Command> {
    use clap::CommandFactory;

    [Lit::command(), FileCheck::command()]
}
