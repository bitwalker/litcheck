mod context;
mod hybrid;
mod regex;
mod substring;
mod whitespace;

pub use self::context::MatchContext;
pub use self::hybrid::HybridMatcher;
pub use self::regex::{RegexMatcher, RegexSetMatcher};
pub use self::substring::{MultiSubstringMatcher, SubstringMatcher};
pub use self::whitespace::AsciiWhitespaceMatcher;

pub use regex_automata::util::look::LookMatcher;

use crate::{check::Input, expr::Value};
use litcheck::{diagnostics::SourceSpan, range::Range};

pub trait Matcher {
    /// Search for a match in the given input buffer, using the provided context
    ///
    /// The first match found is returned.
    fn try_match<'input>(&self, input: Input<'input>) -> Option<MatchInfo<'input>>;
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
    pub fn new(span: SourceSpan, pattern_span: SourceSpan) -> Self {
        Self {
            span,
            pattern_span,
            pattern_id: 0,
            captures: Vec::new(),
        }
    }

    pub fn new_with_pattern(span: SourceSpan, pattern_span: SourceSpan, pattern_id: usize) -> Self {
        Self {
            span,
            pattern_span,
            pattern_id,
            captures: Vec::new(),
        }
    }

    pub fn with_pattern(&mut self, pattern_id: usize) -> &mut Self {
        self.pattern_id = pattern_id;
        self
    }

    pub fn with_captures(&mut self, captures: Vec<CaptureInfo<'input>>) -> &mut Self {
        self.captures = captures;
        self
    }

    pub fn matched_range(&self) -> Range<usize> {
        let start = self.span.offset();
        Range::new(start, start + self.span.len())
    }
}

#[derive(Debug, PartialEq)]
pub struct CaptureInfo<'input> {
    /// The span of the capture in the input
    pub span: SourceSpan,
    /// The index of the capture
    pub index: usize,
    /// The captured value
    pub value: Value<'input>,
}
