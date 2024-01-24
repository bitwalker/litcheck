use litcheck::{
    diagnostics::{ArcSource, DiagResult, Report},
    StringInterner,
};

use super::Checker;
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
    pub fn check<'input, S>(&mut self, input_file: S) -> DiagResult<()>
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
        let program = match_file.compile(self.config)?;

        // Apply the checks
        Checker::new(
            self.config,
            &mut self.strings,
            program,
            self.match_file.clone(),
        )
        .check_all(ArcSource::from(input_file))
        .map_err(Report::new)
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use super::*;
    use crate::check::{CheckFailedError, TestFailed};

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

        let config = Config::default();
        let mut test = FileCheckTest::new(MATCH_FILE, &config);
        test.check(INPUT_FILE)
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

        let config = Config::default();
        let mut test = FileCheckTest::new(MATCH_FILE, &config);
        let error = test
            .check(INPUT_FILE)
            .unwrap_err()
            .downcast::<TestFailed>()
            .unwrap();
        assert_matches!(
            error.errors(),
            [CheckFailedError::MatchNoneButExpected { .. }]
        );

        Ok(())
    }
}
