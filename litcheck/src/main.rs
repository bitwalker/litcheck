mod commands;
use self::commands::*;

use std::process::ExitCode;

use litcheck::diagnostics::{self, DiagResult};

pub fn main() -> DiagResult<ExitCode> {
    init_logger();

    let mut cmd = build_cli();
    let matches = cmd.get_matches_mut();
    let mut subcommand = matches.subcommand();
    if let Some(("litcheck", cmd)) = subcommand {
        subcommand = cmd.subcommand();
    }

    diagnostics::reporting::set_hook(Box::new(|_| {
        Box::new(diagnostics::reporting::ReportHandlerOpts::new().build())
    }))?;
    diagnostics::set_panic_hook();

    match subcommand.unwrap() {
        (name @ ("filecheck" | "FileCheck" | "check"), matches) => {
            run::<FileCheck>(&mut cmd, name, matches)
        }
        (name @ "lit", matches) => run::<Lit>(&mut cmd, name, matches),
        (name, _) => cmd
            .error(
                clap::error::ErrorKind::InvalidSubcommand,
                format!("unexpected subcommand '{name}'"),
            )
            .exit(),
    }
}

fn run<C: Command>(
    cmd: &mut clap::Command,
    name: &str,
    matches: &clap::ArgMatches,
) -> DiagResult<ExitCode> {
    match C::from_arg_matches(matches) {
        Ok(command) if command.is_help_requested() => {
            cmd.find_subcommand_mut(name)
                .unwrap()
                .print_long_help()
                .unwrap();
            Ok(ExitCode::from(2))
        }
        Ok(command) => command.run(),
        Err(err) => err.exit(),
    }
}

fn init_logger() {
    let mut builder = env_logger::Builder::from_env("LITCHECK_TRACE");
    builder.format_indent(Some(2));
    if let Ok(precision) = std::env::var("LITCHECK_TRACE_TIMING") {
        match precision.as_str() {
            "s" => builder.format_timestamp_secs(),
            "ms" => builder.format_timestamp_millis(),
            "us" => builder.format_timestamp_micros(),
            "ns" => builder.format_timestamp_nanos(),
            other => {
                eprintln!(
                    "invalid LITCHECK_TRACE_TIMING precision, expected one of [s, ms, us, ns], got '{}'",
                    other
                );
                std::process::exit(2);
            }
        };
    } else {
        builder.format_timestamp(None);
    }
    builder.init();
}

/// This builds a [clap::Command] representing the `litcheck` multicall binary,
/// i.e. an executable that can be called the following ways:
///
/// * Explicit: `litcheck lit ARGS..`
/// * Implicit: `lit ARGS..`
///
/// The implicit form uses the name of the exectuable it was invoked with to
/// determine which command to run. Our default installation method installs
/// two symlinks to the `litcheck` executable called `lit` and `FileCheck`
/// to allow calling those tools in the usual way.
#[allow(non_upper_case_globals)]
fn build_cli() -> clap::Command {
    clap::Command::new("litcheck")
        .author(clap::crate_authors!(", "))
        .version(clap::crate_version!())
        .about("An implementation of LLVM's filecheck and lit utilities in Rust")
        .multicall(true)
        .subcommand(
            clap::Command::new("litcheck")
                .arg_required_else_help(true)
                .subcommand_value_name("COMMAND")
                .subcommands(commands::all()),
        )
        .subcommands(commands::all())
}
