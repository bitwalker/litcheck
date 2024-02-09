use litcheck::diagnostics::FileName;

use crate::common::*;

/// Information about a successful test run
#[derive(Debug)]
pub struct TestResult {
    /// The info for each positive match (i.e. does not include CHECK-NOT)
    matches: Vec<MatchInfo<'static>>,
    /// The number of checks that passed (includes both positive and negative assertions)
    passed: usize,
    /// If the test failed, this field contains the errors associated with that failure
    error: TestFailed,
}
impl TestResult {
    pub fn new<'input, 'context: 'input>(context: &MatchContext<'input, 'context>) -> Self {
        let error = TestFailed::new(vec![], context);
        Self {
            matches: vec![],
            passed: 0,
            error,
        }
    }

    pub fn from_matches<'input, 'context: 'input>(
        matches: Matches<'_>,
        context: &MatchContext<'input, 'context>,
    ) -> Self {
        let mut test_result = Self::new(context);
        test_result.append(matches);
        test_result
    }

    pub fn from_error(error: TestFailed) -> Self {
        Self {
            matches: vec![],
            passed: 0,
            error,
        }
    }

    pub fn append(&mut self, matches: Matches<'_>) {
        for result in matches.into_iter() {
            match result {
                Ok(Some(info)) => self.matched(info),
                Ok(None) => self.passed(),
                Err(err) => self.failed(err),
            }
        }
    }

    pub fn matched(&mut self, matched: MatchInfo<'_>) {
        self.matches.push(matched.into_static());
        self.passed += 1;
    }

    pub fn passed(&mut self) {
        self.passed += 1;
    }

    pub fn failed(&mut self, error: CheckFailedError) {
        self.error.errors.push(error);
    }

    pub fn is_ok(&self) -> bool {
        self.error.errors.is_empty()
    }

    pub fn is_failed(&self) -> bool {
        !self.error.errors.is_empty()
    }

    pub fn num_matched(&self) -> usize {
        self.matches.len()
    }

    pub fn num_errors(&self) -> usize {
        self.error.errors.len()
    }

    pub fn errors(&self) -> &[CheckFailedError] {
        self.error.errors.as_slice()
    }

    pub fn last_match_end(&self) -> Option<usize> {
        self.matches.iter().map(|mi| mi.span.end()).max()
    }

    pub fn into_result(mut self) -> Result<Vec<MatchInfo<'static>>, TestFailed> {
        if self.is_ok() {
            self.matches
                .sort_by(|a, b| a.pattern_span.start().cmp(&b.pattern_span.start()));
            Ok(self.matches)
        } else {
            Err(self.error)
        }
    }

    pub fn unwrap_err(self) -> TestFailed {
        if self.is_ok() {
            self.error
        } else {
            panic!(
                "attempted to unwrap error when test was successful: {:#?}",
                self
            );
        }
    }
}

#[derive(Debug)]
pub struct TestInputType(pub FileName);
impl fmt::Display for TestInputType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            FileName::Stdin => f.write_str("test from standard input"),
            FileName::Path(path) => write!(f, "test at {}", path.display()),
            FileName::Virtual(name) => write!(f, "test '{name}'"),
        }
    }
}
