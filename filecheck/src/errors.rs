#![expect(unused_assignments)]

use crate::common::*;
use crate::test::TestInputType;

#[derive(Diagnostic, Debug, thiserror::Error)]
#[error("{test_from} failed")]
#[diagnostic(help("see below for details"))]
pub struct TestFailed {
    pub test_from: TestInputType,
    #[related]
    pub errors: Vec<CheckFailedError>,
}
impl TestFailed {
    pub fn new<'input, 'context: 'input>(
        errors: Vec<CheckFailedError>,
        context: &MatchContext<'input, 'context>,
    ) -> Self {
        Self {
            test_from: TestInputType(context.match_file().uri().clone()),
            errors,
        }
    }

    pub fn errors(&self) -> &[CheckFailedError] {
        self.errors.as_slice()
    }
}

#[derive(Diagnostic, Debug, thiserror::Error)]
pub enum InvalidCheckFileError {
    #[error("check file did not contain any rules")]
    #[diagnostic()]
    Empty,
    #[error("invalid CHECK-LABEL pattern")]
    #[diagnostic()]
    CheckLabelVariable {
        #[label("in this pattern")]
        line: SourceSpan,
        #[label("variables/substitutions are not allowed on CHECK-LABEL lines")]
        var: SourceSpan,
    },
    #[error("{kind} directives are not permitted to be the first directive in a file")]
    #[diagnostic()]
    InvalidFirstCheck {
        #[label]
        line: SourceSpan,
        kind: Check,
    },
    #[error("invalid CHECK pattern")]
    #[diagnostic()]
    EmptyPattern(#[label("expected a non-empty pattern here")] SourceSpan),
}

#[derive(Debug, Diagnostic, thiserror::Error)]
#[diagnostic()]
#[error("invalid cast to numeric value: {kind:?}")]
pub struct InvalidNumericCastError {
    #[label("occurs due to cast implied by this pattern")]
    pub span: Option<SourceSpan>,
    pub kind: std::num::IntErrorKind,
    #[label("specifically, the value captured by this pattern is not of the correct format")]
    pub specific_span: Option<SourceSpan>,
    #[source_code]
    pub match_file: Arc<SourceFile>,
}

#[derive(Debug, Diagnostic, thiserror::Error)]
#[error("reference to undefined variable '{name}'")]
pub struct UndefinedVariableError {
    #[label("occurs here")]
    pub span: SourceSpan,
    #[source_code]
    pub match_file: Arc<SourceFile>,
    pub name: Symbol,
}

#[derive(Diagnostic, Debug, thiserror::Error)]
pub enum CheckFailedError {
    #[error("the input file was rejected because it is empty, and --allow-empty was not set")]
    #[diagnostic(
        help = "if your input was the piped output of a command, it may have succeeded with no output when you expected it to fail"
    )]
    EmptyInput,
    /// Indicates an error while processing a potential match
    #[error("an error occurred while processing a potential match")]
    #[diagnostic()]
    MatchError {
        #[label(primary, "when matching against this input")]
        span: SourceSpan,
        #[source_code]
        input_file: Arc<SourceFile>,
        #[related]
        labels: Vec<RelatedLabel>,
        #[help]
        help: Option<String>,
    },
    /// Indicates a match for an excluded pattern.
    #[error("match found, but was excluded")]
    #[diagnostic()]
    MatchFoundButExcluded {
        #[label(primary, "match found here")]
        span: SourceSpan,
        #[source_code]
        input_file: Arc<SourceFile>,
        #[related]
        labels: Vec<RelatedLabel>,
    },
    /// Indicates a match for an expected pattern, but the match is on the
    /// wrong line.
    #[error("match found for expected pattern, but on the wrong line")]
    #[diagnostic()]
    MatchFoundButWrongLine {
        #[label(primary, "match found here")]
        span: SourceSpan,
        #[source_code]
        input_file: Arc<SourceFile>,
        #[related]
        pattern: Option<RelatedCheckError>,
    },
    /// Indicates a discarded match for an expected pattern.
    #[error("match found, but was discarded")]
    #[diagnostic()]
    MatchFoundButDiscarded {
        #[label(primary, "match found here")]
        span: SourceSpan,
        #[source_code]
        input_file: Arc<SourceFile>,
        #[related]
        labels: Vec<RelatedLabel>,
        #[help]
        note: Option<String>,
    },
    /// Indicates an error while processing a match after the match was found
    /// for an expected or excluded pattern.
    #[error("match found, but there was an error processing it")]
    #[diagnostic()]
    MatchFoundErrorNote {
        #[label(primary, "match found here")]
        span: SourceSpan,
        #[source_code]
        input_file: Arc<SourceFile>,
        #[related]
        pattern: Option<RelatedCheckError>,
        #[help]
        help: Option<String>,
    },
    /// Indicates an error while processing a match after the match was found
    /// for an expected or excluded pattern.
    #[error("match found, but there was an error when evaluating a constraint")]
    #[diagnostic()]
    MatchFoundConstraintFailed {
        #[label(primary, "match found here")]
        span: SourceSpan,
        #[source_code]
        input_file: Arc<SourceFile>,
        #[related]
        pattern: Option<RelatedCheckError>,
        #[related]
        error: Option<RelatedError>,
        #[help]
        help: Option<String>,
    },
    /// Indicates no match for an expected pattern, but this might follow good
    /// matches when multiple matches are expected for the pattern, or it might
    /// follow discarded matches for the pattern.
    #[error("no matches were found for expected pattern")]
    #[diagnostic()]
    MatchNoneButExpected {
        #[label(primary, "pattern at this location was not matched")]
        span: SourceSpan,
        #[source_code]
        match_file: Arc<SourceFile>,
        #[help]
        note: Option<String>,
    },
    /// Indicates no match due to an expected or excluded pattern that has
    /// proven to be invalid at match time.  The exact problems are usually
    /// reported in subsequent diagnostics of the same match type but with
    /// `Note` set.
    #[error("unable to match invalid pattern")]
    #[diagnostic()]
    MatchNoneForInvalidPattern {
        #[label(primary, "pattern at this location was invalid")]
        span: SourceSpan,
        #[source_code]
        match_file: Arc<SourceFile>,
        #[related]
        error: Option<RelatedError>,
    },
    /// Indicates a match attempt failed for unknown reasons
    #[error("error occurred while matching pattern")]
    #[diagnostic()]
    MatchNoneErrorNote {
        #[label(primary, "when matching this pattern")]
        span: SourceSpan,
        #[source_code]
        match_file: Arc<SourceFile>,
        #[related]
        error: Option<RelatedError>,
    },
    /// Indicates a fuzzy match that serves as a suggestion for the next
    /// intended match for an expected pattern with too few or no good matches.
    #[error("an exact match was not found, but some similar matches were found, see notes")]
    #[diagnostic()]
    MatchFuzzy {
        #[label(primary, "pattern at this location was invalid")]
        span: SourceSpan,
        #[source_code]
        match_file: Arc<SourceFile>,
        #[help]
        notes: Option<String>,
    },
    /// Indicates that matching all patterns in a set of patterns failed due
    /// to at least one pattern not being matched.
    ///
    /// This occurs with CHECK-DAG/CHECK-NOT which are evaluated in groups
    #[error("one or more matches were not found for a set of expected patterns")]
    #[diagnostic(help("see diagnostics for details about each failed pattern"))]
    MatchAllFailed {
        #[related]
        failed: Vec<CheckFailedError>,
    },
    #[error("unable to match all instances of repeat pattern (matched {n} of {count} times)")]
    #[diagnostic(help("see related errors below for additional details"))]
    MatchRepeatedError {
        #[label(primary, "when matching this pattern for the {}th time", n + 1)]
        span: SourceSpan,
        #[source_code]
        match_file: Arc<SourceFile>,
        n: usize,
        count: usize,
        #[label(collection)]
        related: Vec<LabeledSpan>,
    },
    #[error("one or more matches were not found for a set of expected patterns")]
    #[diagnostic(help("see related error for more information"))]
    MatchGroupFailed {
        #[label(primary, "this check failed")]
        span: SourceSpan,
        #[source_code]
        match_file: Arc<SourceFile>,
        #[related]
        cause: Vec<CheckFailedError>,
        #[label("these checks were skipped because they were dependent on the check that failed")]
        skipped: Option<SourceSpan>,
    },
}
impl CheckFailedError {
    /// Returns true if this error was produced in the context of a possibly-valid match
    pub fn match_was_found(&self) -> bool {
        matches!(
            self,
            Self::MatchFoundButExcluded { .. }
                | Self::MatchFoundButWrongLine { .. }
                | Self::MatchFoundButDiscarded { .. }
                | Self::MatchFoundErrorNote { .. }
                | Self::MatchFoundConstraintFailed { .. }
        )
    }

    pub fn related_labels_for(&self, related_span: SourceSpan) -> Vec<LabeledSpan> {
        use CheckFailedError::*;
        let mut related = vec![];
        let related_source_id = related_span.source_id();
        match self {
            EmptyInput => (),
            err @ (MatchError { span, labels, .. }
            | MatchFoundButExcluded { span, labels, .. }
            | MatchFoundButDiscarded { span, labels, .. }) => {
                if span.source_id() == related_source_id {
                    related.push(LabeledSpan::new_with_span(Some(err.to_string()), *span));
                }
                for label in labels {
                    if label.file.id() == related_source_id {
                        for label in label.labels.iter() {
                            related.push(LabeledSpan::new_with_span(
                                label.label().map(|s| s.to_string()),
                                label.span(),
                            ))
                        }
                    }
                }
            }
            err @ (MatchFoundButWrongLine { span, pattern, .. }
            | MatchFoundErrorNote { span, pattern, .. }) => {
                if span.source_id() == related_source_id {
                    related.push(LabeledSpan::new_with_span(Some(err.to_string()), *span));
                }
                if let Some(pattern) = pattern.as_ref()
                    && pattern.span.source_id() == related_source_id
                {
                    related.push(LabeledSpan::new_with_span(
                        Some("due to pattern at this location".to_string()),
                        pattern.span,
                    ));
                }
            }
            err @ MatchFoundConstraintFailed {
                span,
                pattern,
                error,
                ..
            } => {
                if span.source_id() == related_source_id {
                    related.push(LabeledSpan::new_with_span(Some(err.to_string()), *span));
                    if let Some(error) = error.as_ref() {
                        related.push(LabeledSpan::new_with_span(Some(error.to_string()), *span));
                    }
                }
                if let Some(pattern) = pattern.as_ref()
                    && pattern.span.source_id() == related_source_id
                {
                    related.push(LabeledSpan::new_with_span(
                        Some("due to pattern at this location".to_string()),
                        pattern.span,
                    ));
                }
            }
            err @ MatchNoneButExpected { span, .. } => {
                let message = err.to_string();
                related.push(LabeledSpan::new_with_span(Some(message), *span));
            }
            err @ MatchNoneForInvalidPattern { span, error, .. } => {
                if span.source_id() == related_source_id {
                    let message = err.to_string();
                    related.push(LabeledSpan::new_with_span(Some(message), *span));
                    if let Some(error) = error.as_ref() {
                        related.push(LabeledSpan::new_with_span(Some(error.to_string()), *span));
                    }
                }
            }
            MatchNoneErrorNote { span, error, .. } => {
                if span.source_id() == related_source_id
                    && let Some(error) = error.as_ref()
                {
                    related.push(LabeledSpan::new_with_span(Some(error.to_string()), *span));
                }
            }
            err @ MatchFuzzy { span, notes, .. } => {
                if span.source_id() == related_source_id {
                    let message = err.to_string();
                    related.push(LabeledSpan::new_with_span(Some(message), *span));
                    if let Some(notes) = notes.clone() {
                        related.push(LabeledSpan::new_with_span(Some(notes), *span));
                    }
                }
            }
            MatchAllFailed { .. } | MatchRepeatedError { .. } | MatchGroupFailed { .. } => (),
        }

        related
    }
}

/// This is used to associated source spans from the match file
/// with those from the input file.
#[derive(Diagnostic, Debug, thiserror::Error)]
#[error("check failed")]
#[diagnostic()]
pub struct RelatedCheckError {
    #[label("due to pattern at this location")]
    pub span: SourceSpan,
    #[source_code]
    pub match_file: Arc<SourceFile>,
}

#[derive(Debug, thiserror::Error)]
#[error("see also")]
pub struct RelatedLabel {
    pub severity: litcheck::diagnostics::Severity,
    pub labels: SmallVec<[Label; 1]>,
    pub file: Arc<SourceFile>,
}
impl RelatedLabel {
    pub fn error(label: Label, file: Arc<SourceFile>) -> Self {
        Self {
            severity: litcheck::diagnostics::Severity::Error,
            labels: smallvec![label],
            file,
        }
    }

    pub fn warn(label: Label, file: Arc<SourceFile>) -> Self {
        Self {
            severity: litcheck::diagnostics::Severity::Warning,
            labels: smallvec![label],
            file,
        }
    }

    pub fn note(label: Label, file: Arc<SourceFile>) -> Self {
        Self {
            severity: litcheck::diagnostics::Severity::Advice,
            labels: smallvec![label],
            file,
        }
    }

    pub fn notes(label: impl IntoIterator<Item = Label>, file: Arc<SourceFile>) -> Self {
        Self {
            severity: litcheck::diagnostics::Severity::Advice,
            labels: label.into_iter().collect(),
            file,
        }
    }
}
impl Diagnostic for RelatedLabel {
    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        None
    }
    fn severity(&self) -> Option<litcheck::diagnostics::Severity> {
        Some(self.severity)
    }
    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        None
    }
    fn url<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        None
    }
    fn source_code(&self) -> Option<&dyn litcheck::diagnostics::SourceCode> {
        Some(&self.file)
    }
    fn labels(&self) -> Option<Box<dyn Iterator<Item = litcheck::diagnostics::LabeledSpan> + '_>> {
        if self.labels.is_empty() {
            None
        } else {
            Some(Box::new(self.labels.iter().cloned().map(|l| l.into())))
        }
    }
    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        None
    }
    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        None
    }
}

/// This type wraps related diagnostics for use with [CheckFailedError]
#[derive(Debug)]
pub struct RelatedError(Report);
impl RelatedError {
    pub fn into_report(self) -> Report {
        self.0
    }

    #[inline(always)]
    pub fn as_diagnostic(&self) -> &dyn Diagnostic {
        self.0.as_ref()
    }
}
impl Diagnostic for RelatedError {
    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.as_diagnostic().code()
    }
    fn severity(&self) -> Option<litcheck::diagnostics::Severity> {
        self.as_diagnostic().severity()
    }
    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.as_diagnostic().help()
    }
    fn url<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.as_diagnostic().url()
    }
    fn source_code(&self) -> Option<&dyn litcheck::diagnostics::SourceCode> {
        self.as_diagnostic().source_code()
    }
    fn labels(&self) -> Option<Box<dyn Iterator<Item = litcheck::diagnostics::LabeledSpan> + '_>> {
        self.as_diagnostic().labels()
    }
    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        self.as_diagnostic().related()
    }
    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.as_diagnostic().diagnostic_source()
    }
}
impl fmt::Display for RelatedError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}
impl std::error::Error for RelatedError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        AsRef::<dyn std::error::Error>::as_ref(&self.0).source()
    }
}
impl From<Report> for RelatedError {
    fn from(report: Report) -> Self {
        Self(report)
    }
}
impl RelatedError {
    pub const fn new(report: Report) -> Self {
        Self(report)
    }

    pub fn wrap<E>(error: E) -> Self
    where
        E: Diagnostic + Send + Sync + 'static,
    {
        Self(Report::new_boxed(Box::new(error)))
    }
}
