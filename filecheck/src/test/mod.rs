mod result;
#[cfg(test)]
pub(crate) mod testing;

pub(crate) use self::result::TestInputType;
pub use self::result::TestResult;
#[cfg(test)]
pub use self::testing::TestContext;

use crate::{check::Checker, common::*, parse};

/// This struct represents a single FileCheck test which
/// can be run against one or more input files for verification.
///
/// This is the primary entrypoint for running FileCheck tests.
pub struct Test<'a> {
    config: &'a Config,
    strings: StringInterner,
    match_file: ArcSource,
}
impl<'a> Test<'a> {
    /// Create a new test from the given match file (containing CHECKs) and configuration
    ///
    /// This function does not compile the actual test file until verification is requested.
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

    /// Verify the given input file passes this test.
    ///
    /// First, it parses the check file for rules, post-processes them, and then
    /// applies the parsed rules to the input file, to determine if the file matches
    /// or fails to match.
    pub fn verify<'input, S>(&mut self, input_file: S) -> DiagResult<Vec<MatchInfo<'static>>>
    where
        ArcSource: From<S> + 'input,
    {
        // Parse the check file
        let mut parser = parse::CheckFileParser::new(&self.config, &mut self.strings);

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
