use std::borrow::Cow;
use std::ffi::OsStr;
use std::path::Path;
use std::time::{Duration, Instant};

use litcheck::diagnostics::{DiagResult, IntoDiagnostic};

use super::{TestResult, TestStatus};

pub struct ShellCommand {
    command: std::process::Command,
    pipe_stdin: Option<Vec<u8>>,
}
impl ShellCommand {
    pub fn new<S: AsRef<OsStr>>(prog: S) -> Self {
        Self {
            command: std::process::Command::new(prog),
            pipe_stdin: None,
        }
    }

    pub fn paths<S: AsRef<Path>>(&mut self, extra_paths: &[S]) -> DiagResult<&mut Self> {
        let env_paths = std::env::var_os("PATH").unwrap_or_default();
        let path = std::env::join_paths(
            extra_paths
                .iter()
                .map(|p| Cow::Borrowed(p.as_ref().as_os_str()))
                .chain(std::env::split_paths(&env_paths).map(|p| Cow::Owned(p.into_os_string()))),
        )
        .into_diagnostic()?;
        self.command.env("PATH", path);
        Ok(self)
    }

    pub fn current_dir<S: AsRef<Path>>(&mut self, cwd: S) -> &mut Self {
        self.command.current_dir(cwd);
        self
    }

    pub fn args<I, S>(&mut self, args: I) -> &mut Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        self.command.args(args);
        self
    }

    pub fn envs<I, K, V>(&mut self, vars: I) -> &mut Self
    where
        I: IntoIterator<Item = (K, V)>,
        K: AsRef<OsStr>,
        V: AsRef<OsStr>,
    {
        self.command.envs(vars);
        self
    }

    pub fn pipe_stdin(&mut self, bytes: Vec<u8>) -> &mut Self {
        self.pipe_stdin = Some(bytes);
        self.command.stdin(std::process::Stdio::piped());
        self
    }

    /// Capture the command's output, allowing at most `timeout` for both the
    /// command and its pipes. A zero timeout waits without a deadline.
    ///
    /// Finite deadlines require Unix process groups and nonblocking pipes;
    /// other platforms report an unresolved result instead of ignoring a limit.
    pub fn wait_with_timeout(self, timeout: Duration) -> TestResult {
        let started_at = Instant::now();
        let result = match process::run(self.command, self.pipe_stdin, timeout, started_at) {
            Ok((output, timed_out)) => {
                if timed_out {
                    TestResult::new(TestStatus::Timeout)
                        .with_exit_status(output.status)
                        .with_stdout(output.stdout)
                        .with_stderr(output.stderr)
                } else {
                    TestResult::from(output)
                }
            }
            Err(error) => TestResult::new(TestStatus::Unresolved)
                .with_stderr(format!("Failed to run process: {error}").into_bytes()),
        };
        result
            .with_start(started_at)
            .with_elapsed(started_at.elapsed())
    }

    #[allow(unused)]
    pub fn wait(self) -> TestResult {
        self.wait_with_timeout(Duration::ZERO)
    }
}

#[cfg(unix)]
mod process;

#[cfg(not(unix))]
mod process {
    use std::io::{self, Write};
    use std::process::{Command, Output, Stdio};
    use std::time::{Duration, Instant};

    pub(super) fn run(
        mut command: Command,
        input: Option<Vec<u8>>,
        timeout: Duration,
        _started_at: Instant,
    ) -> io::Result<(Output, bool)> {
        if !timeout.is_zero() {
            return Err(io::Error::new(
                io::ErrorKind::Unsupported,
                "command timeouts are only supported on Unix",
            ));
        }
        let mut child = command
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;
        // There is deliberately no deadline on this platform. Feed stdin while
        // wait_with_output drains both output streams to avoid pipe deadlocks.
        std::thread::scope(|scope| {
            if let (Some(mut stdin), Some(input)) = (child.stdin.take(), input) {
                scope.spawn(move || {
                    let _ = stdin.write_all(&input);
                });
            }
            child.wait_with_output().map(|output| (output, false))
        })
    }
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;

    const TIMEOUT: Duration = Duration::from_millis(150);

    fn shell(script: &str) -> ShellCommand {
        let mut command = ShellCommand::new("/bin/sh");
        command.args(["-c", script]);
        command
    }

    fn assert_timeout(command: ShellCommand) -> TestResult {
        let started = Instant::now();
        let result = command.wait_with_timeout(TIMEOUT);
        assert!(
            started.elapsed() < Duration::from_secs(2),
            "timeout took {:?}",
            started.elapsed()
        );
        assert_eq!(result.status(), TestStatus::Timeout);
        result
    }

    #[test]
    fn timeout_terminates_sleeping_command() {
        assert_timeout(shell("sleep 3"));
    }

    #[test]
    fn timeout_preserves_captured_output() {
        let result = assert_timeout(shell("printf stdout; printf stderr >&2; sleep 3"));
        assert_eq!(result.stdout(), "stdout");
        assert_eq!(result.stderr(), "stderr");
    }

    #[test]
    fn timeout_includes_blocked_stdin() {
        let mut command = shell("sleep 3");
        command.pipe_stdin(vec![b'x'; 2 * 1024 * 1024]);
        assert_timeout(command);
    }

    #[test]
    fn timeout_includes_pipes_held_by_descendants_after_child_exit() {
        assert_timeout(shell("sleep 3 &"));
    }

    #[test]
    fn timeout_reaps_child_and_terminates_descendants() {
        let directory = tempdir::TempDir::new("litcheck-timeout").unwrap();
        let marker = directory.path().join("survived");
        let mut command = shell("echo $$; (sleep 1; touch \"$1\") & wait");
        command.args([OsStr::new("litcheck-test"), marker.as_os_str()]);
        let result = assert_timeout(command);
        let pid: i32 = result.stdout().trim().parse().unwrap();
        let mut status = 0;
        // SAFETY: status points to writable storage and pid names the command
        // started by this test. WNOHANG prevents an accidental blocking wait.
        assert_eq!(
            unsafe { libc::waitpid(pid, &mut status, libc::WNOHANG) },
            -1
        );
        assert_eq!(
            std::io::Error::last_os_error().raw_os_error(),
            Some(libc::ECHILD)
        );
        std::thread::sleep(Duration::from_millis(1100));
        assert!(!marker.exists(), "a descendant survived the timeout");
    }

    #[test]
    fn large_input_and_output_make_progress_together() {
        let mut command = shell("head -c 1048576 /dev/zero; head -c 1048576 /dev/zero >&2; cat");
        command.pipe_stdin(vec![b'x'; 1024 * 1024]);
        let started = Instant::now();
        let result = command.wait_with_timeout(Duration::from_secs(2));
        assert!(started.elapsed() < Duration::from_secs(3));
        assert_eq!(result.status(), TestStatus::Pass);
        assert_eq!(result.stdout().len(), 2 * 1024 * 1024);
        assert!(result.stdout().as_bytes()[..1024 * 1024]
            .iter()
            .all(|byte| *byte == 0));
        assert!(result.stdout().as_bytes()[1024 * 1024..]
            .iter()
            .all(|byte| *byte == b'x'));
        assert_eq!(result.stderr().len(), 1024 * 1024);
        assert!(result.stderr().as_bytes().iter().all(|byte| *byte == 0));
    }
}
