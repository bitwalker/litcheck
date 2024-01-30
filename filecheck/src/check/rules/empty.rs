use litcheck::diagnostics::{DiagResult, SourceSpan};

use crate::check::{Check, CheckFailedError, Context, MatchInfo, MatchResult, MatchType};

use super::*;

#[derive(Debug)]
pub struct CheckEmpty {
    span: SourceSpan,
}
impl CheckEmpty {
    pub fn new(span: SourceSpan) -> Self {
        Self { span }
    }
}
impl Rule for CheckEmpty {
    fn kind(&self) -> Check {
        Check::Empty
    }

    fn span(&self) -> SourceSpan {
        self.span
    }

    fn apply<'input, 'context, C>(&self, context: &mut C) -> DiagResult<MatchResult<'input>>
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
            })
        } else {
            Ok(MatchResult {
                ty: MatchType::Failed(CheckFailedError::MatchNoneButExpected {
                    span: self.span,
                    match_file: context.match_file(),
                    note: None,
                }),
                info: None,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::Context;
    use crate::testing::TestContext;
    use std::assert_matches::assert_matches;

    #[test]
    fn check_empty_test() {
        let mut context = TestContext::new();
        context.with_checks("CHECK-EMPTY:").with_input(
            "
abc


def",
        );
        let mut mctx = context.match_context();
        let rule = CheckEmpty::new(SourceSpan::from(0..12));
        let result = rule
            .apply(&mut mctx)
            .expect("expected non-fatal application of rule");
        assert!(!result.is_ok());
        assert_matches!(
            result.ty,
            MatchType::Failed(CheckFailedError::MatchNoneButExpected { .. })
        );

        // Move to the 'abc' line
        mctx.cursor_mut().set_start(5);

        let result = rule
            .apply(&mut mctx)
            .expect("expected non-fatal application of rule");
        assert!(result.is_ok());
        assert_matches!(result.ty, MatchType::MatchFoundAndExpected);
        assert_eq!(result.info.as_ref().unwrap().span.offset(), 6);
        let buffer = mctx.cursor().buffer();
        assert_eq!(buffer[5], b'\n');
        assert_eq!(buffer[6], b'\n');
        assert_eq!(buffer[7], b'd');
        assert_eq!(result.info.as_ref().unwrap().span.len(), 0);
    }
}
