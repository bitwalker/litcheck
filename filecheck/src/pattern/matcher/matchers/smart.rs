mod builder;
mod instruction;
mod operand;
mod searcher;

pub use self::builder::SmartMatcherBuilder;
use self::instruction::{CaptureGroup, MatchOp};
use self::operand::Operand;
use self::searcher::SmartSearcher;

use crate::common::*;

use regex_automata::util::captures::{Captures, GroupInfo};

/// This is a combination, jack-of-all trades matcher, which is
/// executes a set of smaller matches to form a large one. Some
/// parts of the "pattern" are known statically, while others
/// rely on runtime variable bindings and expression evaluation,
/// hence "smart". This is the default matcher that is used any
/// time match blocks or substitutions are present in a pattern.
pub struct SmartMatcher<'a> {
    span: SourceSpan,
    /// The underlying "smart" components of this matcher.
    ///
    /// These parts are matched and evaluated left-to-right,
    /// each one is fully processed before moving to the next
    /// part. The matcher maintains its own workspace for
    /// locally-bound variables and the like, so that if the
    /// pattern fails, none of the state changes are applied
    /// globally
    parts: Vec<MatchOp<'a>>,
    /// Capture group metadata for this regex
    group_info: GroupInfo,
}
impl<'a> fmt::Debug for SmartMatcher<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("SmartMatcher")
            .field("parts", &self.parts)
            .field(
                "group_info",
                &self
                    .group_info
                    .all_names()
                    .collect::<smallvec::SmallVec<[_; 4]>>(),
            )
            .finish()
    }
}
impl<'a> Spanned for SmartMatcher<'a> {
    fn span(&self) -> SourceSpan {
        self.span
    }
}
impl<'a> SmartMatcher<'a> {
    fn new(
        span: SourceSpan,
        parts: Vec<MatchOp<'a>>,
        groups: Vec<Vec<Option<Cow<'a, str>>>>,
    ) -> Self {
        let group_info =
            GroupInfo::new(groups).expect("expected capturing groups to meet all requirements");
        Self {
            span,
            parts,
            group_info,
        }
    }

    pub fn build<'config>(
        span: SourceSpan,
        interner: &'config mut StringInterner,
    ) -> SmartMatcherBuilder<'a, 'config> {
        SmartMatcherBuilder::new(span, interner)
    }

    pub fn group_info(&self) -> &GroupInfo {
        &self.group_info
    }
}
impl<'a> MatcherMut for SmartMatcher<'a> {
    fn try_match_mut<'input, 'context, C>(
        &self,
        input: Input<'input>,
        context: &mut C,
    ) -> DiagResult<MatchResult<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let mut captures = Captures::matches(self.group_info().clone());
        let searcher = SmartSearcher::new(self.span, input, &mut captures);
        searcher.search_captures(&self.parts, context)?;
        if let Some(matched) = captures.get_match() {
            // TODO: Add captures to result
            Ok(MatchResult::ok(MatchInfo::new(matched.range(), self.span)))
        } else {
            Ok(MatchResult::failed(
                CheckFailedError::MatchNoneButExpected {
                    span: self.span,
                    match_file: context.match_file(),
                    note: None,
                },
            ))
        }
    }
}
