use crate::common::*;

#[derive(Debug)]
pub struct CheckNext<M> {
    pattern: M,
}
impl<M> CheckNext<M>
where
    M: MatcherMut,
{
    pub fn new(pattern: M) -> Self {
        Self { pattern }
    }
}
impl<M> Spanned for CheckNext<M>
where
    M: MatcherMut,
{
    fn span(&self) -> SourceSpan {
        self.pattern.span()
    }
}
impl<M> Rule for CheckNext<M>
where
    M: MatcherMut,
{
    fn kind(&self) -> Check {
        Check::Next
    }

    fn apply<'input, 'context, C>(&self, context: &mut C) -> DiagResult<Matches<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let cursor = context.cursor();
        let start = cursor.start_of_next_line();
        let block_end = cursor.end();
        let next_eol = cursor.next_newline_from(start).unwrap_or(block_end);
        if start >= block_end {
            // Cannot match, since the search range would
            // overflow the current block.
            return Ok(Matches::from(MatchResult {
                ty: MatchType::Failed(CheckFailedError::MatchNoneButExpected {
                    span: self.pattern.span(),
                    match_file: context.match_file(),
                    note: if cursor.end_of_file() == block_end {
                        None
                    } else {
                        Some(
                            "search was stopped at end of the preceding CHECK-LABEL scope"
                                .to_string(),
                        )
                    },
                }),
                info: None,
            }));
        }
        let input = context.search_range(start..next_eol);
        let result = self.pattern.try_match_mut(input, context)?;
        match &result {
            MatchResult {
                ty,
                info: Some(ref info),
            } if ty.is_ok() => {
                context.cursor_mut().set_start(info.span.end());
            }
            _ => (),
        }
        Ok(result.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pattern::matcher::*;
    use crate::rules::CheckPlain;

    #[test]
    fn check_next_test() -> DiagResult<()> {
        let mut context = TestContext::new();
        context
            .with_checks(
                "
CHECK: @inc4
CHECK-NEXT: entry:
",
            )
            .with_input(
                "
define void @inc4(i64* %p) {
entry:
        %0 = tail call i64 @llvm.atomic.load.add.i64.p0i64(i64* %p, i64 1)
        ret void
}
",
            );
        let mut mctx = context.match_context();
        let pattern =
            SubstringMatcher::new(Span::new(SourceSpan::from(8..12), Cow::Borrowed("@inc4")))
                .expect("expected pattern to be valid");
        let rule = CheckPlain::new(pattern);
        let matches = rule
            .apply(&mut mctx)
            .expect("expected non-fatal application of rule");
        TestResult::from_matches(matches, &mctx).into_result()?;

        let pattern =
            SubstringMatcher::new(Span::new(SourceSpan::from(22..30), Cow::Borrowed("entry:")))
                .expect("expected pattern to be valid");
        let rule = CheckNext::new(pattern);
        let matches = rule
            .apply(&mut mctx)
            .expect("expected non-fatal application of rule");
        let matched = TestResult::from_matches(matches, &mctx).into_result()?;
        assert_eq!(matched.len(), 1);
        assert_eq!(matched[0].span.offset(), 30);
        assert_eq!(matched[0].span.len(), 6);
        let input = mctx.search();
        assert_eq!(input.as_str(matched[0].matched_range()), "entry:");
        assert_eq!(input.buffer()[mctx.cursor().start()], b'\n');

        Ok(())
    }
}
