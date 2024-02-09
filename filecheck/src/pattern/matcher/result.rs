use crate::{ast::Capture, common::*};

#[derive(Debug)]
pub struct MatchResult<'input> {
    pub ty: MatchType,
    pub info: Option<MatchInfo<'input>>,
}
impl<'input> MatchResult<'input> {
    pub fn new(ty: MatchType, info: Option<MatchInfo<'input>>) -> Self {
        Self { ty, info }
    }

    /// An expected pattern was succesfully found in the input
    pub fn ok(info: MatchInfo<'input>) -> Self {
        Self {
            ty: MatchType::MatchFoundAndExpected,
            info: Some(info),
        }
    }

    /// A negative match assertion, i.e. CHECK-NOT, successfully
    /// matched by not finding the pattern in the input. Such a
    /// "match" is empty.
    pub fn empty() -> Self {
        Self {
            ty: MatchType::MatchNoneAndExcluded,
            info: None,
        }
    }

    /// A match failed with the given error
    pub fn failed(error: CheckFailedError) -> Self {
        Self {
            ty: MatchType::Failed(error),
            info: None,
        }
    }

    /// A match was found, but failed with the given error,
    pub fn match_found_but_failed(error: CheckFailedError, info: MatchInfo<'input>) -> Self {
        Self {
            ty: MatchType::Failed(error),
            info: Some(info),
        }
    }

    /// Returns true if the match succeeded.
    ///
    /// This does not necessarily mean there is a corresponding [MatchInfo],
    /// such as in the case of negative matches like CHECK-NOT.
    pub fn is_ok(&self) -> bool {
        self.ty.is_ok()
    }

    /// Returns true if this result represents a successful CHECK-NOT match.
    pub fn is_empty(&self) -> bool {
        matches!(self.ty, MatchType::MatchNoneAndExcluded)
    }

    #[inline]
    pub fn pattern_id(&self) -> Option<usize> {
        self.info.as_ref().map(|info| info.pattern_id)
    }

    #[inline]
    pub fn matched_range(&self) -> Option<Range<usize>> {
        self.info.as_ref().map(|info| info.matched_range())
    }

    pub fn unwrap_err(self) -> CheckFailedError {
        match self.ty {
            MatchType::Failed(err) => err,
            ty => panic!("attempted to unwrap error from {ty}"),
        }
    }

    pub fn into_result(self) -> Result<Option<MatchInfo<'input>>, CheckFailedError> {
        match self {
            Self {
                ty: MatchType::Failed(err),
                ..
            } => Err(err),
            Self {
                info: Some(info), ..
            } => Ok(Some(info)),
            Self { info: None, .. } => Ok(None),
        }
    }
}

#[derive(Debug)]
pub enum MatchType {
    /// Indicates a good match for an expected pattern.
    MatchFoundAndExpected,
    /// Indicates no match for an excluded pattern.
    MatchNoneAndExcluded,
    /// The match failed for some reason
    Failed(CheckFailedError),
}
impl MatchType {
    pub fn is_ok(&self) -> bool {
        matches!(
            self,
            Self::MatchFoundAndExpected | Self::MatchNoneAndExcluded
        )
    }

    pub fn match_was_found(&self) -> bool {
        match self {
            Self::MatchFoundAndExpected => true,
            Self::Failed(err) => err.match_was_found(),
            Self::MatchNoneAndExcluded => false,
        }
    }
}
impl fmt::Display for MatchType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::MatchFoundAndExpected => f.write_str("match found for expected pattern"),
            Self::MatchNoneAndExcluded => f.write_str("excluded pattern was never matched"),
            Self::Failed(err) => write!(f, "{err}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct MatchInfo<'input> {
    /// The span of the matched input
    pub span: SourceSpan,
    /// The span of the pattern that was matched
    pub pattern_span: SourceSpan,
    /// The index of the pattern that was matched, if applicable
    ///
    /// For single pattern matchers, this is always 0
    pub pattern_id: usize,
    /// For matchers which capture parts of the input, this
    /// contains the captured values and their information
    pub captures: Vec<CaptureInfo<'input>>,
}
impl<'input> MatchInfo<'input> {
    pub fn new(span: impl Into<SourceSpan>, pattern_span: SourceSpan) -> Self {
        Self::new_with_pattern(span, pattern_span, 0)
    }

    pub fn new_with_pattern(
        span: impl Into<SourceSpan>,
        pattern_span: SourceSpan,
        pattern_id: usize,
    ) -> Self {
        Self {
            span: span.into(),
            pattern_span,
            pattern_id,
            captures: Vec::new(),
        }
    }

    pub fn with_pattern(mut self, pattern_id: usize) -> Self {
        self.pattern_id = pattern_id;
        self
    }

    pub fn with_captures(mut self, captures: Vec<CaptureInfo<'input>>) -> Self {
        self.captures = captures;
        self
    }

    pub fn matched_range(&self) -> Range<usize> {
        let start = self.span.offset();
        Range::new(start, start + self.span.len())
    }

    /// Extract the value of a variable binding that was captured by this match
    pub fn extract(&self, name: Symbol) -> Option<&Value<'input>> {
        self.captures.iter().find_map(|cap| {
            if cap.capture.name().filter(|v| *v == name).is_some() {
                Some(&cap.value)
            } else {
                None
            }
        })
    }

    pub fn into_static(self) -> MatchInfo<'static> {
        MatchInfo {
            captures: self
                .captures
                .into_iter()
                .map(CaptureInfo::into_static)
                .collect(),
            ..self
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CaptureInfo<'input> {
    /// The span of the capture in the input
    pub span: SourceSpan,
    /// The span of the pattern from which this capture originated
    pub pattern_span: SourceSpan,
    /// The index of the capture
    pub index: usize,
    /// The captured value
    pub value: Value<'input>,
    /// The original capture metadata, or the default "ignore"
    pub capture: Capture,
}
impl<'input> Spanned for CaptureInfo<'input> {
    fn span(&self) -> SourceSpan {
        self.span
    }
}
impl CaptureInfo<'_> {
    pub fn into_static(self) -> CaptureInfo<'static> {
        CaptureInfo {
            value: match self.value {
                Value::Undef => Value::Undef,
                Value::Str(s) => Value::Str(s.into_owned().into()),
                Value::Num(expr) => Value::Num(expr),
            },
            ..self
        }
    }
}
