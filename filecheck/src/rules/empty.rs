use crate::common::*;

#[derive(Debug)]
pub struct CheckEmpty {
    span: SourceSpan,
}
impl CheckEmpty {
    pub fn new(span: SourceSpan) -> Self {
        Self { span }
    }
}
impl Spanned for CheckEmpty {
    fn span(&self) -> SourceSpan {
        self.span
    }
}
impl Rule for CheckEmpty {
    fn kind(&self) -> Check {
        Check::Empty
    }

    fn apply<'input, 'context, C>(&self, context: &mut C) -> DiagResult<Matches<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        // Next line must be empty, meaning the start of the line is the end of the line
        let cursor = context.cursor_mut();
        let next_line_start = cursor.start_of_next_line();
        let next_eol = cursor
            .next_newline_from(next_line_start)
            .unwrap_or_else(|| cursor.end_of_file());
        let next_line = next_line_start..next_eol;
        if cursor.at_end_of_line() && next_line.is_empty() {
            let span = SourceSpan::from(next_line_start..next_eol);
            // Move to the end of the empty line
            cursor.set_start(next_eol);
            Ok(MatchResult {
                ty: MatchType::MatchFoundAndExpected,
                info: Some(MatchInfo::new(span, self.span)),
            }
            .into())
        } else {
            Ok(MatchResult {
                ty: MatchType::Failed(CheckFailedError::MatchNoneButExpected {
                    span: self.span,
                    match_file: context.match_file(),
                    note: None,
                }),
                info: None,
            }
            .into())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_empty_test() -> DiagResult<()> {
        let mut context = TestContext::new();
        context.with_checks("CHECK-EMPTY:").with_input(
            "
abc


def",
        );
        let mut mctx = context.match_context();
        let rule = CheckEmpty::new(SourceSpan::from(0..12));
        let matches = rule
            .apply(&mut mctx)
            .expect("expected non-fatal application of rule");
        let test_result = TestResult::from_matches(matches, &mctx);
        assert!(test_result.is_failed());
        match test_result.errors() {
            [CheckFailedError::MatchNoneButExpected { .. }] => (),
            _ => return test_result.into_result().map(|_| ()).map_err(Report::new),
        }

        // Move to the 'abc' line
        mctx.cursor_mut().set_start(5);

        let matches = rule
            .apply(&mut mctx)
            .expect("expected non-fatal application of rule");
        let test_result = TestResult::from_matches(matches, &mctx);
        assert!(test_result.is_ok());
        assert_eq!(test_result.num_matched(), 1);

        let matched = test_result.into_result()?;
        assert_eq!(matched[0].span.offset(), 6);
        let buffer = mctx.cursor().buffer();
        assert_eq!(buffer[5], b'\n');
        assert_eq!(buffer[6], b'\n');
        assert_eq!(buffer[7], b'd');
        assert_eq!(matched[0].span.len(), 0);

        Ok(())
    }
}
