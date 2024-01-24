use std::{
    fmt,
    time::{Duration, Instant},
};

use super::TestStatus;

/// Wrapper for the results of executing an individual test
#[derive(Default)]
pub struct TestResult {
    pub status: TestStatus,
    exit_status: Option<std::process::ExitStatus>,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
    elapsed: Option<Duration>,
    start: Option<Instant>,
}
unsafe impl Send for TestResult {}
impl From<std::process::Output> for TestResult {
    fn from(output: std::process::Output) -> Self {
        let mut result = Self::new(output.status.into());
        result.exit_status = Some(output.status);
        result.stdout = output.stdout;
        result.stderr = output.stderr;
        result
    }
}
impl TestResult {
    pub fn new(status: TestStatus) -> Self {
        Self {
            status,
            ..Default::default()
        }
    }

    #[inline(always)]
    pub fn with_stdout(mut self, stdout: Vec<u8>) -> Self {
        self.stdout = stdout;
        self
    }

    #[inline(always)]
    pub fn with_stderr(mut self, stderr: Vec<u8>) -> Self {
        self.stderr = stderr;
        self
    }

    #[inline(always)]
    pub fn with_elapsed(mut self, elapsed: Duration) -> Self {
        self.elapsed = Some(elapsed);
        self
    }

    #[inline(always)]
    pub fn with_start(mut self, start: Instant) -> Self {
        self.start = Some(start);
        self
    }

    #[inline(always)]
    pub fn with_exit_status(mut self, exit_status: std::process::ExitStatus) -> Self {
        self.exit_status = Some(exit_status);
        self
    }

    #[inline(always)]
    pub fn status(&self) -> TestStatus {
        self.status
    }

    pub fn stdout(&self) -> std::borrow::Cow<'_, str> {
        String::from_utf8_lossy(&self.stdout)
    }

    pub fn stderr(&self) -> std::borrow::Cow<'_, str> {
        String::from_utf8_lossy(&self.stderr)
    }

    #[inline(always)]
    pub fn is_failure(&self) -> bool {
        self.status.is_failure()
    }

    #[inline(always)]
    pub fn is_timeout(&self) -> bool {
        matches!(self.status, TestStatus::Timeout)
    }

    #[inline]
    pub fn exit_code(&self) -> Option<i32> {
        self.exit_status.as_ref().and_then(|status| status.code())
    }
}
impl fmt::Display for TestResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(exit_code) = self.exit_code() {
            if exit_code != 0 {
                write!(f, "{}", console::style("Exit Code: ").dim().red())?;
                writeln!(f, "{}", console::style(exit_code).dim().red())?;
            }
        } else {
            write!(f, "{}", console::style("Exit Code: ").dim().yellow())?;
            writeln!(f, "{}", console::style("N/A").dim().yellow())?;
        }

        if self.is_timeout() {
            let elapsed = self
                .elapsed
                .expect("expected elapsed duration to be set for timeout results");
            writeln!(
                f,
                "{}: Reached timeout after {}s",
                console::style("Timeout").dim().yellow(),
                elapsed.as_secs()
            )?;
        } else if let Some(elapsed) = self.elapsed {
            let secs = elapsed.as_secs();
            write!(f, "{}: ", console::style("Elapsed").dim())?;
            if secs == 0 {
                writeln!(f, "{}ms", elapsed.subsec_millis())?;
            } else {
                writeln!(
                    f,
                    "{}",
                    console::style(format!("{:.2}s", elapsed.as_secs_f64()))
                        .dim()
                        .yellow()
                )?;
            }
        } else {
            writeln!(f)?;
        }

        if !self.stdout.is_empty() {
            writeln!(
                f,
                "{}",
                console::style("Command Output (stdout) ----").dim()
            )?;
            writeln!(f, "{}", console::style(&self.stdout()).dim().cyan())?;
            writeln!(
                f,
                "{}",
                console::style("----------------------------").dim()
            )?;
        }

        if !self.stderr.is_empty() {
            writeln!(
                f,
                "{}",
                console::style("Command Output (stderr) ----").dim()
            )?;
            writeln!(f, "{}", console::style(&self.stderr()).dim().cyan())?;
            writeln!(
                f,
                "{}",
                console::style("----------------------------").dim()
            )?;
        }

        Ok(())
    }
}
