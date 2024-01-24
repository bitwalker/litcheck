use std::borrow::Cow;
use std::ffi::OsStr;
use std::io::Write;
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

    pub fn wait_with_timeout(mut self, timeout: Duration) -> TestResult {
        self.command
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped());
        let started_at = Instant::now();
        match self.command.spawn() {
            Ok(mut child) => {
                let pipe_stdin = self.pipe_stdin.take();
                let mut stdin = child.stdin.take().unwrap();
                let _ = std::thread::scope(move |scope| {
                    scope
                        .spawn(move || {
                            if let Some(bytes) = pipe_stdin {
                                stdin.write_all(&bytes)
                            } else {
                                Ok(())
                            }
                        })
                        .join()
                });
                let stdout_handle = child.stdout.take().map(read);
                let stderr_handle = child.stderr.take().map(read);

                loop {
                    match child.try_wait() {
                        Ok(None) => {
                            let elapsed = started_at.elapsed();
                            if timeout.is_zero() || elapsed < timeout {
                                continue;
                            }
                            let stdout = stdout_handle
                                .and_then(|t| t.join().ok())
                                .unwrap_or_default();
                            let stderr = stderr_handle
                                .and_then(|t| t.join().ok())
                                .unwrap_or_default();
                            let _ = child.kill();
                            let status = child.try_wait().transpose();
                            let result = TestResult::new(TestStatus::Timeout)
                                .with_stdout(stdout)
                                .with_stderr(stderr)
                                .with_start(started_at)
                                .with_elapsed(elapsed);
                            if let Some(Ok(status)) = status {
                                break result.with_exit_status(status);
                            } else {
                                break result;
                            }
                        }
                        Ok(Some(status)) => {
                            let elapsed = started_at.elapsed();
                            let stdout = stdout_handle
                                .and_then(|t| t.join().ok())
                                .unwrap_or_default();
                            let stderr = stderr_handle
                                .and_then(|t| t.join().ok())
                                .unwrap_or_default();
                            break TestResult::from(std::process::Output {
                                status,
                                stdout,
                                stderr,
                            })
                            .with_start(started_at)
                            .with_elapsed(elapsed);
                        }
                        Err(wait_err) => {
                            let elapsed = started_at.elapsed();
                            let stdout = stdout_handle
                                .and_then(|t| t.join().ok())
                                .unwrap_or_default();
                            let mut stderr = stderr_handle
                                .and_then(|t| t.join().ok())
                                .unwrap_or_default();
                            if stderr.is_empty() {
                                write!(&mut stderr, "Unexpected error occurred waiting on child process: {wait_err}").unwrap();
                            } else {
                                write!(&mut stderr, "\n\nUnexpected error occurred waiting on child process: {wait_err}").unwrap();
                            }
                            break TestResult::new(TestStatus::Unresolved)
                                .with_start(started_at)
                                .with_elapsed(elapsed)
                                .with_stdout(stdout)
                                .with_stderr(stderr);
                        }
                    };
                }
            }
            Err(spawn_err) => TestResult::new(TestStatus::Unresolved)
                .with_stderr(format!("Failed to spawn process: {spawn_err}").into_bytes()),
        }
    }

    #[allow(unused)]
    pub fn wait(self) -> TestResult {
        self.wait_with_timeout(Duration::ZERO)
    }
}

fn read<R: std::io::Read + Send + 'static>(mut input: R) -> std::thread::JoinHandle<Vec<u8>> {
    std::thread::spawn(move || {
        let mut buffer = vec![];
        match input.read_to_end(&mut buffer) {
            Ok(_) => buffer,
            Err(_) => buffer,
        }
    })
}
