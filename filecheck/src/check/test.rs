use litcheck::{
    diagnostics::{ArcSource, DiagResult, Report},
    StringInterner,
};

use super::{Checker, MatchInfo};
use crate::{parse, Config};

pub struct FileCheckTest<'a> {
    config: &'a Config,
    match_file: ArcSource,
    strings: StringInterner,
}
impl<'a> FileCheckTest<'a> {
    pub fn new<S>(match_file: S, config: &'a Config) -> Self
    where
        ArcSource: From<S>,
    {
        Self {
            config,
            match_file: ArcSource::from(match_file),
            strings: StringInterner::new(),
        }
    }

    /// Checks the file this test was created against.
    ///
    /// First, it parses the check file for rules, post-processes them, and then
    /// applies the parsed rules to the input file, to determine if the file matches
    /// or fails to match.
    pub fn check<'input, S>(&mut self, input_file: S) -> DiagResult<Vec<MatchInfo<'static>>>
    where
        ArcSource: From<S> + 'input,
    {
        // Parse the check file
        let mut parser = parse::CheckFileParser::new(
            &self.config.check_prefixes,
            &self.config.comment_prefixes,
            &mut self.strings,
        );

        let match_file = parser
            .parse(&self.match_file)
            .map_err(|err| Report::new(err).with_source_code(self.match_file.clone()))?;

        // Compile the check rules, and raise an error if any are invalid
        let program = match_file.compile(self.config, &mut self.strings)?;

        // Apply the checks
        let mut checker = Checker::new(
            self.config,
            &mut self.strings,
            program,
            self.match_file.clone(),
        );
        checker
            .check_input(ArcSource::from(input_file))
            .into_result()
            .map_err(Report::new)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::CheckFailedError;
    use crate::testing::TestContext;

    #[test]
    fn filecheck_sanity_test() -> DiagResult<()> {
        const MATCH_FILE: &str = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../tests/filecheck/sanity/check.txt"
        ));

        const INPUT_FILE: &str = r"
Some random


content to show output
and some rules
";

        let mut context = TestContext::new();
        context.with_checks(MATCH_FILE).with_input(INPUT_FILE);
        context.check()?;

        Ok(())
    }

    #[test]
    fn filecheck_sanity_check_same_violation_test() -> DiagResult<()> {
        const MATCH_FILE: &str = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../tests/filecheck/sanity/check.txt"
        ));

        const INPUT_FILE: &str = r"
Some random


content to
and some rules
";

        let mut context = TestContext::new();
        context.with_checks(MATCH_FILE).with_input(INPUT_FILE);
        let error = context.check_failed();
        if let [CheckFailedError::MatchNoneButExpected { .. }] = error.errors() {
            Ok(())
        } else {
            Err(error.into())
        }
    }
}
