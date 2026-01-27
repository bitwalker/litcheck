mod builder;
mod instruction;
mod operand;
mod searcher;

pub use self::builder::SmartMatcherBuilder;
use self::instruction::{CaptureGroup, MatchOp};
use self::operand::Operand;
use self::searcher::SmartSearcher;

use crate::common::*;

use regex_automata::util::{
    captures::{Captures, GroupInfo},
    primitives::NonMaxUsize,
};

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
        group_info: Vec<Vec<Option<Symbol>>>,
    ) -> Self {
        let group_info =
            GroupInfo::new(group_info).expect("expected capturing groups to meet all requirements");
        Self {
            span,
            parts,
            group_info,
        }
    }

    pub fn build<'config>(
        span: SourceSpan,
        config: &'config Config,
    ) -> SmartMatcherBuilder<'a, 'config> {
        SmartMatcherBuilder::new(span, config)
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
        let mut captures = Captures::all(self.group_info().clone());
        let searcher = SmartSearcher::new(self.span, input, &mut captures);
        searcher.search_captures(&self.parts, context)?;
        if let Some(matched) = captures.get_match() {
            let matched =
                extract_captures_from_match(self.span, matched, &captures, &self.parts, context)?;
            Ok(matched)
        } else {
            Ok(MatchResult::failed(
                CheckFailedError::MatchNoneButExpected {
                    span: self.span,
                    match_file: context.source_file(self.span.source_id()).unwrap(),
                    note: None,
                },
            ))
        }
    }
}

fn extract_captures_from_match<'a, 'input, 'context, C>(
    pattern_span: SourceSpan,
    matched: regex_automata::Match,
    captures: &Captures,
    patterns: &[MatchOp<'a>],
    context: &C,
) -> DiagResult<MatchResult<'input>>
where
    C: Context<'input, 'context> + ?Sized,
{
    use crate::ast::Capture;

    let input = context.search();
    let overall_span = SourceSpan::from_range_unchecked(input.source_id(), matched.range());
    let mut capture_infos = Vec::with_capacity(captures.group_len());
    let slots = captures.slots();
    for op in patterns {
        match op {
            MatchOp::Regex {
                source,
                captures: group_captures,
                ..
            } => {
                let pattern_span = source.span();
                for group in group_captures.iter() {
                    if matches!(group.info, Capture::Ignore(_)) {
                        continue;
                    }
                    match capture_group_to_capture_info(
                        overall_span,
                        pattern_span,
                        group,
                        captures,
                        slots,
                        context,
                        &input,
                    ) {
                        Ok(None) => continue,
                        Ok(Some(capture_info)) => {
                            capture_infos.push(capture_info);
                        }
                        Err(error) => return Ok(MatchResult::failed(error)),
                    }
                }
            }
            MatchOp::Numeric {
                span,
                capture: Some(group),
                ..
            } => {
                match capture_group_to_capture_info(
                    overall_span,
                    *span,
                    group,
                    captures,
                    slots,
                    context,
                    &input,
                ) {
                    Ok(None) => continue,
                    Ok(Some(capture_info)) => {
                        capture_infos.push(capture_info);
                    }
                    Err(error) => return Ok(MatchResult::failed(error)),
                }
            }
            MatchOp::Substitution { .. }
            | MatchOp::Bind { .. }
            | MatchOp::Drop
            | MatchOp::Constraint { .. }
            | MatchOp::Literal(_)
            | MatchOp::Numeric { .. } => (),
        }
    }
    Ok(MatchResult::ok(MatchInfo {
        span: overall_span,
        pattern_span,
        pattern_id: 0,
        captures: capture_infos,
    }))
}

fn capture_group_to_capture_info<'input, 'context, C>(
    overall_span: SourceSpan,
    pattern_span: SourceSpan,
    group: &CaptureGroup,
    captures: &Captures,
    slots: &[Option<NonMaxUsize>],
    context: &C,
    input: &Input<'input>,
) -> Result<Option<CaptureInfo<'input>>, CheckFailedError>
where
    C: Context<'input, 'context> + ?Sized,
{
    let group_info = captures.group_info();

    let (a, b) = group_info
        .slots(group.pattern_id, group.group_id)
        .expect("invalid capture group");
    if let Some(start) = slots[a].map(|offset| offset.get()) {
        let end = slots[b].expect("expected end offset to be present").get();
        let captured = input.as_str(start..end);
        let capture_span = SourceSpan::from_range_unchecked(input.source_id(), start..end);

        super::regex::try_convert_capture_to_type(
            group.pattern_id,
            group.group_id,
            pattern_span,
            overall_span,
            Span::new(capture_span, captured),
            group.info,
            captures,
            context,
        )
        .map(Some)
    } else {
        Ok(None)
    }
}
