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

    /// Suppress repeated renderings of the same searched region.
    ///
    /// Several checks commonly fail against one region -- a group of CHECK-DAG patterns, for
    /// instance -- and rendering it once per failure buries the differences between them.
    /// The first failure to reach a region keeps it; later ones get a back-reference.
    ///
    /// This must run after all errors have been collected, and walks them in the order they
    /// will be reported.
    pub fn dedup_searched_regions(&mut self) {
        fn visit(errors: &mut [CheckFailedError], seen: &mut Vec<SmallVec<[SourceSpan; 2]>>) {
            for error in errors {
                match error {
                    CheckFailedError::MatchNoneButExpected { searched, .. } => {
                        let key = searched
                            .iter()
                            .filter_map(SearchedRegion::region)
                            .collect::<SmallVec<[_; 2]>>();
                        // A region-less entry is a bare note, which is cheap and always kept
                        if key.is_empty() {
                            continue;
                        }
                        if seen.contains(&key) {
                            *searched = smallvec![SearchedRegion::Note(
                                "searched the same region of the input as an earlier failure"
                            )];
                        } else {
                            seen.push(key);
                        }
                    }
                    CheckFailedError::MatchGroupFailed { cause, .. } => visit(cause, seen),
                    CheckFailedError::MatchAllFailed { failed } => visit(failed, seen),
                    _ => (),
                }
            }
        }

        visit(&mut self.errors, &mut Vec::new());
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
        /// The region of the input which was searched for this pattern, if it is being
        /// reported. See [SearchedRegion] and [CheckFailedError::match_none].
        #[related]
        searched: SmallVec<[SearchedRegion; 2]>,
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
    /// Construct a [Self::MatchNoneButExpected] for `pattern_span`, annotated with the
    /// region of the input which was searched.
    ///
    /// Whether the region is reported is governed by `--dump-input`; see [crate::Dump].
    pub fn match_none<'input, 'context, C>(
        pattern_span: SourceSpan,
        input: &Input<'input>,
        context: &C,
    ) -> Self
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let config = context.config();
        let searched = if config.options.dump_input.is_enabled() {
            SearchedRegion::describe(
                input.bounds(),
                context.input_file(),
                config.tracing_enabled(),
            )
        } else {
            SmallVec::new()
        };
        Self::MatchNoneButExpected {
            span: pattern_span,
            match_file: context.source_file(pattern_span.source_id()).unwrap(),
            searched,
            note: None,
        }
    }

    /// Attach an explanatory note to this error.
    ///
    /// Has no effect on variants which do not carry a note.
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        if let Self::MatchNoneButExpected { note: slot, .. } = &mut self {
            *slot = Some(note.into());
        }
        self
    }

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

/// The maximum number of lines of input rendered inline when reporting the region of the
/// input which was searched for a pattern.
///
/// Larger regions are reported by marking their endpoints instead. miette renders every
/// line between two labels in the same source, so without this a failed match against a
/// large input would dump the entire input to the terminal.
///
/// Passing `-vv` overrides this, on the basis that if you asked for the verbose output you
/// would rather have the whole region than a summary of it.
const MAX_INLINE_SEARCHED_LINES: u32 = 10;

/// Describes the region of the input which was searched, unsuccessfully, for a pattern.
///
/// This is attached to [CheckFailedError::MatchNoneButExpected] as a related diagnostic, so
/// that a failed check reports not just _what_ failed to match, but _where_ we looked for it.
#[derive(Debug)]
pub enum SearchedRegion {
    /// Render `span` of `input_file`, annotated with `label`.
    ///
    /// A region small enough to display in full produces a single marker covering the whole
    /// region. A larger one produces a marker for each endpoint, so that the lines in
    /// between are not printed.
    Marker {
        span: SourceSpan,
        input_file: Arc<SourceFile>,
        /// The headline for this marker. The trailing marker of a pair leaves this empty,
        /// since the leading marker already introduced the region.
        message: Option<String>,
        label: &'static str,
    },
    /// A bare note, with no associated source to render.
    Note(&'static str),
}

impl SearchedRegion {
    /// Describe `range` of `input_file` as the region which was searched for a pattern.
    ///
    /// Set `always_inline` to render the region in full however large it is, rather than
    /// collapsing it to its endpoints past [MAX_INLINE_SEARCHED_LINES].
    pub fn describe(
        range: Range<usize>,
        input_file: Arc<SourceFile>,
        always_inline: bool,
    ) -> SmallVec<[Self; 2]> {
        let id = input_file.id();
        let eof = input_file.len();
        let start = core::cmp::min(range.start, eof);
        let end = core::cmp::min(core::cmp::max(range.end, start), eof);

        // An empty region is worth reporting in its own right: it means the pattern never
        // had a chance to match, which is usually a symptom of an earlier failed check
        // having left the cursor at the end of the block.
        if start == end {
            return smallvec![Self::Note(if start >= eof {
                "the search began at the end of the input, so no input remained to be searched"
            } else {
                "the region of input available to be searched was empty"
            })];
        }

        let first_line = input_file.location(SourceSpan::at(id, start as u32)).line;
        // `end` is exclusive, so the last byte actually searched is the one before it.
        let last_line = input_file
            .location(SourceSpan::at(id, (end - 1) as u32))
            .line;
        let num_lines = last_line.to_u32().saturating_sub(first_line.to_u32()) + 1;

        if always_inline || num_lines <= MAX_INLINE_SEARCHED_LINES {
            let message = if num_lines == 1 {
                format!("searched this region of the input (line {first_line})")
            } else {
                format!("searched this region of the input (lines {first_line}-{last_line})")
            };
            return smallvec![Self::Marker {
                span: SourceSpan::from_range_unchecked(id, start..end),
                input_file,
                message: Some(message),
                label: "no match anywhere in this region",
            }];
        }

        // Too large to render in full, so mark the endpoints. The trailing marker points at
        // the start of the last line searched rather than at `end` itself, since a point
        // span at EOF has no line to attach to.
        let last_line_start = input_file
            .content()
            .line_start(input_file.content().line_index((end as u32 - 1).into()))
            .map(|idx| idx.to_u32())
            .unwrap_or(end as u32 - 1);
        let reached_eof = end == eof;
        smallvec![
            Self::Marker {
                span: SourceSpan::at(id, start as u32),
                input_file: input_file.clone(),
                message: Some(format!(
                    "searched {num_lines} lines of input without a match (lines {first_line}-{last_line})"
                )),
                label: "search began here",
            },
            Self::Marker {
                span: SourceSpan::at(id, last_line_start),
                input_file,
                message: Some(
                    if reached_eof {
                        "...and ran to the end of the input"
                    } else {
                        "...and ran to here"
                    }
                    .to_string(),
                ),
                label: "no match was found in any of the intervening lines",
            },
        ]
    }

    /// Returns the region of input this describes, for de-duplication purposes.
    ///
    /// [Self::Note] has no region, and is never suppressed.
    pub fn region(&self) -> Option<SourceSpan> {
        match self {
            Self::Marker { span, .. } => Some(*span),
            Self::Note(_) => None,
        }
    }
}

impl fmt::Display for SearchedRegion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Marker { message, .. } => f.write_str(message.as_deref().unwrap_or("")),
            Self::Note(note) => f.write_str(note),
        }
    }
}

impl std::error::Error for SearchedRegion {}

impl Diagnostic for SearchedRegion {
    fn severity(&self) -> Option<litcheck::diagnostics::Severity> {
        Some(litcheck::diagnostics::Severity::Advice)
    }
    fn source_code(&self) -> Option<&dyn litcheck::diagnostics::SourceCode> {
        match self {
            Self::Marker { input_file, .. } => Some(input_file),
            Self::Note(_) => None,
        }
    }
    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        match self {
            Self::Marker { span, label, .. } => Some(Box::new(core::iter::once(
                LabeledSpan::new_with_span(Some(label.to_string()), *span),
            ))),
            Self::Note(_) => None,
        }
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
