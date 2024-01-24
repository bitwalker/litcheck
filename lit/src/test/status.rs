use std::fmt;

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TestStatus {
    #[default]
    Unresolved,
    Excluded,
    Skipped,
    Pass,
    FlakyPass,
    Fail,
    Xfail,
    Xpass,
    Timeout,
    Unsupported,
}
impl TestStatus {
    pub fn tag(&self) -> &'static str {
        match self {
            Self::Excluded => "EXCLUDED",
            Self::Skipped => "SKIPPED",
            Self::Unsupported => "UNSUPPORTED",
            Self::Pass => "PASS",
            Self::FlakyPass => "FLAKYPASS",
            Self::Xfail => "XFAIL",
            Self::Unresolved => "UNRESOLVED",
            Self::Timeout => "TIMEOUT",
            Self::Fail => "FAIL",
            Self::Xpass => "XPASS",
        }
    }

    pub fn label(&self) -> &'static str {
        match self {
            Self::Excluded => "Excluded",
            Self::Skipped => "Skipped",
            Self::Unsupported => "Unsupported",
            Self::Pass => "Passed",
            Self::FlakyPass => "Passed With Retry",
            Self::Xfail => "Failed As Expected",
            Self::Unresolved => "Unresolved",
            Self::Timeout => "Timed Out",
            Self::Fail => "Failed",
            Self::Xpass => "Unexpectedly Passed",
        }
    }

    pub fn is_failure(&self) -> bool {
        matches!(
            self,
            Self::Unresolved | Self::Timeout | Self::Fail | Self::Xpass
        )
    }
}
impl From<std::process::ExitStatus> for TestStatus {
    fn from(status: std::process::ExitStatus) -> Self {
        match status.code() {
            None => TestStatus::Unresolved,
            Some(0) => TestStatus::Pass,
            Some(_) => TestStatus::Fail,
        }
    }
}
impl fmt::Display for TestStatus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.label())
    }
}
