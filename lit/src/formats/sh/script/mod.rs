#![expect(unused_assignments)]

pub mod directives;
mod parser;
#[cfg(test)]
mod tests;

use std::{borrow::Cow, sync::Arc};

use litcheck::diagnostics::{
    DiagResult, Diagnostic, FileName, SourceFile, SourceSpan, Span, Spanned,
};

use self::directives::*;
use self::parser::TestScriptParser;

use crate::config::{BooleanExpr, InvalidBooleanExprError, ScopedSubstitutionSet};

#[derive(Diagnostic, Debug, thiserror::Error)]
#[error("parsing test script at '{file}' failed")]
#[diagnostic()]
pub struct InvalidTestScriptError {
    file: FileName,
    #[related]
    errors: Vec<TestScriptError>,
}
impl InvalidTestScriptError {
    pub fn new(file: FileName, errors: Vec<TestScriptError>) -> Self {
        Self { file, errors }
    }
}

#[derive(Diagnostic, Debug, thiserror::Error)]
pub enum TestScriptError {
    #[error("unable to read file")]
    #[diagnostic()]
    Io(#[from] std::io::Error),
    #[error("invalid line substitution syntax")]
    #[diagnostic()]
    InvalidLineSubstitution {
        #[label("{error}")]
        span: SourceSpan,
        #[source]
        error: core::num::ParseIntError,
    },
    #[error(transparent)]
    #[diagnostic(transparent)]
    InvalidBooleanExpr(
        #[from]
        #[diagnostic_source]
        InvalidBooleanExprError,
    ),
    #[error(transparent)]
    #[diagnostic(transparent)]
    InvalidSubstitution(
        #[from]
        #[diagnostic_source]
        InvalidSubstitutionError,
    ),
    #[error("unexpected directive keyword")]
    #[diagnostic()]
    InvalidDirectiveContinuation {
        #[label("expected {expected} here")]
        span: SourceSpan,
        #[label("because this line ended with a continuation")]
        prev_line: SourceSpan,
        expected: DirectiveKind,
    },
    #[error("missing directive keyword")]
    #[diagnostic()]
    MissingDirectiveContinuation {
        #[label("expected to find the {expected} keyword on this line")]
        span: SourceSpan,
        #[label("because this line ended with a continuation")]
        prev_line: SourceSpan,
        expected: DirectiveKind,
    },
    #[error("test has no 'RUN:' directive")]
    #[diagnostic(help("lit requires at least one command to run per test"))]
    MissingRunDirective,
    #[error("'{kind}' directive conflict")]
    #[diagnostic()]
    DirectiveConflict {
        #[label("this directive conflicts with a previous occurrence")]
        span: SourceSpan,
        #[label("previously occurred here")]
        prev_span: SourceSpan,
        kind: DirectiveKind,
    },
    #[error("unable to parse value of '{kind}' directive")]
    #[diagnostic()]
    InvalidIntegerDirective {
        #[label("{error}")]
        span: SourceSpan,
        kind: DirectiveKind,
        #[source]
        error: core::num::ParseIntError,
    },
}

#[derive(Debug, Default)]
pub struct TestScript {
    /// A list of commands to be evaluated in-order during test execution.
    ///
    /// Commands are either [Run] or [Substitution] directives.
    pub commands: Vec<ScriptCommand>,
    /// A list of conditions under which this test is expected to fail.
    /// Each condition is a boolean expression of features, or '*'.
    /// These can optionally be provided by test format handlers,
    /// and will be honored when the test result is supplied.
    pub xfails: Vec<Span<BooleanExpr>>,
    /// A list of conditions that must be satisfied before running the test.
    /// Each condition is a boolean expression of features. All of them
    /// must be True for the test to run.
    pub requires: Vec<Span<BooleanExpr>>,
    /// A list of conditions that prevent execution of the test.
    /// Each condition is a boolean expression of features. All of them
    /// must be False for the test to run.
    pub unsupported: Vec<Span<BooleanExpr>>,
    /// An optional number of retries allowed before the test finally succeeds.
    /// The test is run at most once plus the number of retries specified here.
    pub allowed_retries: Option<Span<usize>>,
}
impl TestScript {
    pub fn parse(source: Arc<SourceFile>) -> DiagResult<Self> {
        let parser = TestScriptParser::new(&source);
        parser
            .parse()
            .map_err(|err| litcheck::diagnostics::Report::from(err).with_source_code(source))
    }

    pub fn apply_substitutions(
        &mut self,
        substitutions: &mut ScopedSubstitutionSet<'_>,
    ) -> DiagResult<()> {
        for script_command in self.commands.iter_mut() {
            match script_command {
                ScriptCommand::Run(ref mut run) => {
                    let span = run.span();
                    let command = substitutions.apply(Span::new(span, run.command.as_str()))?;
                    if let Cow::Owned(command) = command {
                        run.command = command;
                    }
                }
                ScriptCommand::Substitution(ref subst) => {
                    subst.apply(substitutions)?;
                }
            }
        }

        Ok(())
    }
}
