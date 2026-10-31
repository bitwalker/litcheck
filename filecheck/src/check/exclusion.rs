use crate::{ast::CheckLine, common::*};

/// Negative patterns evaluated independently against the same input region.
///
/// Keeping the patterns separate preserves their order and diagnostics, including when their
/// matches overlap. A negative match must never publish captures to another pattern.
#[derive(Debug)]
pub struct ExclusionGroup<'a> {
    patterns: Vec<Pattern<'a>>,
}
impl<'a> ExclusionGroup<'a> {
    pub fn compile(lines: Vec<CheckLine<'a>>, config: &Config) -> DiagResult<Self> {
        let patterns = lines
            .into_iter()
            .map(|line| {
                if line.ty.is_literal_match() {
                    Pattern::compile_literal(line.pattern, config)
                } else {
                    Pattern::compile(line.pattern, config)
                }
            })
            .collect::<DiagResult<Vec<_>>>()?;
        assert!(
            !patterns.is_empty(),
            "expected at least one negative pattern"
        );
        Ok(Self { patterns })
    }

    pub fn first_pattern_span(&self) -> SourceSpan {
        self.patterns[0].span()
    }

    pub fn pattern_len(&self) -> usize {
        self.patterns.len()
    }

    pub fn apply<'input, 'context: 'input>(
        &self,
        input: Input<'input>,
        context: &mut MatchContext<'input, 'context>,
    ) -> Matches<'input> {
        let mut matches = Matches::with_capacity(self.patterns.len());
        for pattern in &self.patterns {
            let mut guard = context.protect();
            let result = match pattern.try_match_mut(input, &mut guard) {
                Ok(MatchResult {
                    info: Some(info),
                    ty,
                }) if ty.is_ok() => MatchResult::failed(CheckFailedError::MatchFoundButExcluded {
                    span: info.span,
                    input_file: guard.input_file(),
                    labels: vec![RelatedLabel::error(
                        Label::new(info.pattern_span, "by this pattern"),
                        guard.source_file(info.pattern_span.source_id()).unwrap(),
                    )],
                }),
                Ok(MatchResult {
                    ty: MatchType::Failed(CheckFailedError::MatchNoneButExpected { .. }),
                    ..
                }) => MatchResult::empty(),
                Ok(result) => result,
                Err(error) => MatchResult::failed(CheckFailedError::MatchNoneErrorNote {
                    span: pattern.span(),
                    match_file: guard.source_file(pattern.span().source_id()).unwrap(),
                    error: Some(RelatedError::new(error)),
                }),
            };
            matches.push(result);
        }
        matches
    }
}

impl Spanned for ExclusionGroup<'_> {
    fn span(&self) -> SourceSpan {
        let first = self.first_pattern_span();
        // Implicit patterns may originate in separate command-line arguments.
        if self
            .patterns
            .iter()
            .all(|pattern| pattern.span().source_id() == first.source_id())
        {
            SourceSpan::new(
                first.source_id(),
                Range::new(first.start(), self.patterns.last().unwrap().span().end()),
            )
        } else {
            first
        }
    }
}
