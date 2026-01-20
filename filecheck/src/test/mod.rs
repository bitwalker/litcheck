mod result;
#[cfg(test)]
pub(crate) mod testing;

pub(crate) use self::result::TestInputType;
pub use self::result::TestResult;
#[cfg(test)]
pub use self::testing::TestContext;

use std::path::Path;

use litcheck::diagnostics::SourceManagerExt;

use crate::{check::Checker, common::*, parse};

#[macro_export]
macro_rules! source_file {
    ($config:ident, $content:expr) => {
        $config.source_manager().load(
            litcheck::diagnostics::SourceLanguage::Unknown,
            litcheck::diagnostics::FileName::from(format!("{}{}", file!(), line!())),
            $content.into(),
        )
    };

    ($config:expr, $content:expr) => {
        $config.source_manager().load(
            litcheck::diagnostics::SourceLanguage::Unknown,
            litcheck::diagnostics::FileName::from(format!("{}{}", file!(), line!())),
            $content.into(),
        )
    };

    ($config:ident, $name:expr, $content:expr) => {
        $config.source_manager().load(
            litcheck::diagnostics::SourceLanguage::Unknown,
            litcheck::diagnostics::FileName::from($name),
            $content.into(),
        )
    };

    ($config:expr, $name:expr, $content:expr) => {
        $config.source_manager().load(
            litcheck::diagnostics::SourceLanguage::Unknown,
            litcheck::diagnostics::FileName::from($name),
            $content.into(),
        )
    };
}

/// This struct represents a single FileCheck test which
/// can be run against one or more input files for verification.
///
/// This is the primary entrypoint for running FileCheck tests.
pub struct Test<'a> {
    config: &'a Config,
    strings: StringInterner,
    match_file: Arc<SourceFile>,
}
impl<'a> Test<'a> {
    /// Create a new test from the given match file (containing CHECKs) and configuration
    ///
    /// This function does not compile the actual test file until verification is requested.
    pub fn new(match_file: Arc<SourceFile>, config: &'a Config) -> Self {
        Self {
            config,
            match_file,
            strings: StringInterner::new(),
        }
    }

    /// Create a test from a file path containing CHECKs
    pub fn from_file(match_file: impl AsRef<Path>, config: &'a Config) -> Self {
        let match_file = config
            .source_manager
            .load_file(match_file.as_ref())
            .expect("could not load match file");
        Self::new(match_file, config)
    }

    /// Verify the file at the given path passes this test.
    ///
    /// See [Self::verify] for details.
    pub fn verify_file(
        &mut self,
        input_file: impl AsRef<Path>,
    ) -> DiagResult<Vec<MatchInfo<'static>>> {
        let input_file = self
            .config
            .source_manager
            .load_file(input_file.as_ref())
            .map_err(Report::from_err)?;
        self.verify(input_file)
    }

    /// Verify the given input file passes this test.
    ///
    /// First, it parses the check file for rules, post-processes them, and then
    /// applies the parsed rules to the input file, to determine if the file matches
    /// or fails to match.
    pub fn verify(&mut self, input_file: Arc<SourceFile>) -> DiagResult<Vec<MatchInfo<'static>>> {
        // Parse the check file
        let mut parser = parse::CheckFileParser::new(self.config, &mut self.strings);

        let match_file = parser
            .parse(self.match_file.id(), self.match_file.as_str())
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
            .check_input(input_file)
            .into_result()
            .map_err(Report::new)
    }
}
