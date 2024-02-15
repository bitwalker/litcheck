use crate::{
    common::*,
    pattern::{matcher::MatchAll, search::PatternSetSearcher, visitors::*},
};

#[derive(Debug)]
pub struct CheckDag<'a> {
    patterns: MatchAll<'a>,
}
impl<'a> CheckDag<'a> {
    pub fn new(patterns: MatchAll<'a>) -> Self {
        Self { patterns }
    }

    #[inline]
    pub fn first_pattern_span(&self) -> SourceSpan {
        self.patterns.first_pattern_span()
    }
}
impl<'check> Spanned for CheckDag<'check> {
    fn span(&self) -> SourceSpan {
        self.patterns.span()
    }
}
impl<'check> Rule for CheckDag<'check> {
    fn kind(&self) -> Check {
        Check::Dag
    }

    fn apply<'input, 'context, C>(&self, context: &mut C) -> DiagResult<Matches<'input>>
    where
        C: Context<'input, 'context> + ?Sized,
    {
        let mut guard = context.protect();
        let input = guard.search_block();
        let result = match self.patterns {
            MatchAll::Literal(ref matcher) => {
                let mut searcher = matcher.search(input)?;
                let mut visitor = StaticPatternSetVisitor::new(&mut searcher);
                visitor.try_match_all(&mut guard)
            }
            MatchAll::Regex(ref matcher) => {
                let mut searcher = matcher.search(input);
                let mut visitor = StaticPatternSetVisitor::new(&mut searcher);
                visitor.try_match_all(&mut guard)
            }
            MatchAll::RegexPrefix {
                ref prefixes,
                ref suffixes,
                ..
            } => {
                let mut searcher = prefixes.search(input);
                let mut visitor = DynamicPatternSetVisitor::new(&mut searcher, suffixes);
                visitor.try_match_all(&mut guard)
            }
            MatchAll::SubstringPrefix {
                ref prefixes,
                ref suffixes,
            } => {
                let mut searcher = prefixes.search(input)?;
                let mut visitor = DynamicPatternSetVisitor::new(&mut searcher, suffixes);
                visitor.try_match_all(&mut guard)
            }
            MatchAll::AnyPrefix {
                ref prefixes,
                ref suffixes,
            } => {
                let mut searcher = PatternSetSearcher::new(input, prefixes)?;
                let mut visitor = DynamicPatternSetVisitor::new(&mut searcher, suffixes);
                visitor.try_match_all(&mut guard)
            }
        };
        match result {
            Ok(result) if result.is_ok() => {
                guard.save();
                Ok(result)
            }
            result => result,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::CheckSection;
    use std::assert_matches::assert_matches;
    use std::collections::VecDeque;

    /// This test ensures that the ordering of CHECK-DAG patterns is not
    /// significant even in the presence of a common prefix. The test can
    /// only pass if this is true, as line matched by the first directive
    /// appears after the line matched by the second.
    #[test]
    fn check_dag_common_prefix_non_overlapping_patterns_test() -> DiagResult<()> {
        let mut context = TestContext::new();
        context
            .with_checks(
                "
CHECK-DAG: v[[#]] = add v9, v0
CHECK-DAG: v[[#]] = add v6, v7
",
            )
            .with_input(
                "
function foo(i32) -> i32 {
block0(v0: i32):
  v2 = const.i32 0
  v1 = const.i32 1
  br block2(v1, v2)

block1(v6: i32, v7: i32):
  v8 = add v6, v7
  v9 = const.i32 0
  v10 = add v9, v0
  br block3(v8, v10)

block2(v3: i32, v4: i32):
  v5 = mul v3, v4
  br block1

block3(v11):
  ret v11
}
",
            );
        let match_file = context.match_file();
        let match_file_source = match_file.as_ref();
        let check_file = context.parse(match_file_source)?;

        let lines = check_file.into_lines();

        let mut mctx = context.match_context();
        let match_all = MatchAll::compile(lines, mctx.env_mut().interner())?;
        let rule = CheckDag::new(match_all);
        let result = rule
            .apply(&mut mctx)
            .expect("expected non-fatal application of rule");

        let test_result = TestResult::from_matches(result, &mctx);
        test_result.into_result()?;

        Ok(())
    }

    #[test]
    fn check_dag_common_prefix_regex_non_overlapping_patterns_test() -> DiagResult<()> {
        let mut context = TestContext::new();
        context
            .with_checks(
                r"
CHECK-DAG: {{v[\d]+}} = add v9, v0
CHECK-DAG: {{v[\d]+}} = add v6, v7
",
            )
            .with_input(
                "
function foo(i32) -> i32 {
block0(v0: i32):
  v2 = const.i32 0
  v1 = const.i32 1
  br block2(v1, v2)

block1(v6: i32, v7: i32):
  v8 = add v6, v7
  v9 = const.i32 0
  v10 = add v9, v0
  br block3(v8, v10)

block2(v3: i32, v4: i32):
  v5 = mul v3, v4
  br block1

block3(v11):
  ret v11
}
",
            );
        let match_file = context.match_file();
        let match_file_source = match_file.as_ref();
        let check_file = context.parse(match_file_source)?;

        let lines = check_file.into_lines();

        let mut mctx = context.match_context();
        let match_all = MatchAll::compile(lines, mctx.env_mut().interner())?;
        let rule = CheckDag::new(match_all);
        let result = rule
            .apply(&mut mctx)
            .expect("expected non-fatal application of rule");

        let test_result = TestResult::from_matches(result, &mctx);
        test_result.into_result()?;

        Ok(())
    }

    /// In this test, the first of the two CHECK-DAG directives can match the
    /// same content as the second, while the second only has one possible match.
    /// This test can only succeed if the ordering of the directives is not significant,
    /// at least in this specific case. However, the catch here is that the reason the
    /// test does not fail is because we always take the longest match first, so even
    /// though the first directive would match the only line that the second can match,
    /// the second directive wins because it matches a longer span of input.
    #[test]
    fn check_dag_overlapping_test() -> DiagResult<()> {
        let mut context = TestContext::new();
        context
            .with_checks(
                "
CHECK-LABEL: block0:
CHECK-DAG: v[[#]] = {{[a-z]+[.][a-z]+}}
CHECK-DAG: v[[#]] = const.i32 0
CHECK-LABEL: block1(
CHECK: br block2(v[[#]], v[[#]])
",
            )
            .with_input(
                "
function foo(i32) -> i32 {
block0(v0: i32):
  v2 = const.i32 0
  v1 = const.i32 1
  br block2(v1, v2)

block1(v6: i32, v7: i32):
  v8 = add v6, v7
  v9 = const.i32 0
  v10 = add v9, v0
  br block3(v8, v10)

block2(v3: i32, v4: i32):
  v5 = mul v3, v4
  br block1

block3(v11):
  ret v11
}
",
            );
        let match_file = context.match_file();
        let match_file_source = match_file.as_ref();
        let check_file = context.parse(match_file_source)?;

        let mut lines = VecDeque::from(check_file.into_lines());
        lines.pop_back();
        lines.pop_back();
        lines.pop_front();
        let lines = Vec::from(lines);

        let mut mctx = context.match_context();
        let match_all = MatchAll::compile(lines, mctx.env_mut().interner())?;
        let rule = CheckDag::new(match_all);
        let result = rule
            .apply(&mut mctx)
            .expect("expected non-fatal application of rule");
        let test_result = TestResult::from_matches(result, &mctx);
        test_result.into_result()?;

        Ok(())
    }

    /// This test ensures that a CHECK-DAG does not consider matches successful
    /// outside the current block
    #[test]
    fn check_dag_does_not_search_beyond_check_label_boundaries_test() -> DiagResult<()> {
        let mut context = TestContext::new();
        context
            .with_checks(
                "
CHECK-LABEL: block0(
CHECK-DAG: v1 = const.i32 1
CHECK-DAG: v8 = add v6, v7
CHECK-LABEL: block1(
CHECK-DAG: v8 = add v6, v7
",
            )
            .with_input(
                "
function foo(i32) -> i32 {
block0(v0: i32):
  v2 = const.i32 0
  v1 = const.i32 1
  br block2(v1, v2)

block1(v6: i32, v7: i32):
  v8 = add v6, v7
  v9 = const.i32 0
  v10 = add v9, v0
  br block3(v8, v10)

block2(v3: i32, v4: i32):
  v5 = mul v3, v4
  br block1

block3(v11):
  ret v11
}
",
            );
        let match_file = context.match_file();
        let match_file_source = match_file.as_ref();
        let check_file = context.parse(match_file_source)?;
        let mut program = context.compile(check_file)?;

        let mut mctx = context.match_context();
        let mut blocks = crate::check::discover_blocks(&program, &mut mctx)?;
        assert_eq!(blocks.len(), 2);

        blocks.pop().unwrap();
        let block = blocks.pop().unwrap();
        assert!(block.start.is_some());
        assert_eq!(block.sections, Some(Range::new(0, 1)));
        assert!(!block.range().is_empty());

        mctx.enter_block(block.range());
        assert_matches!(&program.sections[0], CheckSection::Block { .. });
        program.sections.pop();

        let test_result = crate::check::check_blocks([block], &program, &mut mctx);
        assert!(test_result.is_failed());
        if let [CheckFailedError::MatchNoneButExpected { .. }] = test_result.errors() {
            Ok(())
        } else {
            Ok(test_result.into_result().map(|_| ())?)
        }
    }

    /// This test ensures that all of the successful matches are contained within
    /// their respective blocks.
    #[test]
    fn check_dag_check_label_boundaries_test() -> DiagResult<()> {
        let mut context = TestContext::new();
        context
            .with_checks(
                "
CHECK-LABEL: block0(
CHECK-DAG: v1 = const.i32 1
CHECK-DAG: v2 = const.i32 0
CHECK-LABEL: block1(
CHECK-DAG: v8 = add v6, v7
",
            )
            .with_input(
                "
function foo(i32) -> i32 {
block0(v0: i32):
  v2 = const.i32 0
  v1 = const.i32 1
  br block2(v1, v2)

block1(v6: i32, v7: i32):
  v8 = add v6, v7
  v9 = const.i32 0
  v10 = add v9, v0
  br block3(v8, v10)

block2(v3: i32, v4: i32):
  v5 = mul v3, v4
  br block1

block3(v11):
  ret v11
}
",
            );
        let match_file = context.match_file();
        let match_file_source = match_file.as_ref();
        let check_file = context.parse(match_file_source)?;
        let program = context.compile(check_file)?;

        let mut mctx = context.match_context();
        let blocks = crate::check::discover_blocks(&program, &mut mctx)?;
        assert_eq!(blocks.len(), 2);

        let block = &blocks[0];
        assert!(block.start.is_some());
        assert_eq!(block.sections, Some(Range::new(0, 1)));
        assert!(!block.range().is_empty());

        mctx.enter_block(block.range());
        let block_ranges = blocks
            .iter()
            .map(|blk| blk.range())
            .collect::<SmallVec<[_; 2]>>();
        let matches = crate::check::check_blocks(blocks, &program, &mut mctx).into_result()?;

        assert_eq!(matches.len(), 5);
        assert!(matches[0].span.end() <= block_ranges[0].start); // CHECK-LABEL
        assert!(matches[1].span.end() <= block_ranges[0].end);
        assert!(matches[2].span.end() <= block_ranges[0].end);
        let match3_span = matches[3].span; // CHECK-LABEL
        assert!(
            match3_span.start() >= block_ranges[0].end
                && match3_span.end() <= block_ranges[1].start
        );
        let match4_span = matches[4].span;
        assert!(
            match4_span.start() >= block_ranges[1].start
                && match4_span.end() <= block_ranges[1].end
        );
        let match5_span = matches[4].span;
        assert!(
            match5_span.start() >= block_ranges[1].start
                && match5_span.end() <= block_ranges[1].end
        );

        Ok(())
    }
}
