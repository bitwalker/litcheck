//! Unix command capture. One readiness loop owns all pipes and the deadline,
//! so a blocked writer or a descendant holding a pipe cannot delay termination.

use std::io::{self, Read, Write};
use std::os::fd::{AsRawFd, RawFd};
use std::os::unix::process::CommandExt;
use std::process::{Child, Command, Output, Stdio};
use std::time::{Duration, Instant};

pub(super) fn run(
    mut command: Command,
    input: Option<Vec<u8>>,
    timeout: Duration,
    started_at: Instant,
) -> io::Result<(Output, bool)> {
    let mut child = command
        .process_group(0)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;
    let result = capture(&mut child, input, timeout, started_at);
    if result.is_err() {
        // Setup and I/O errors must release the process just like a timeout.
        terminate(&mut child);
        let _ = child.wait();
    }
    result
}

fn capture(
    child: &mut Child,
    input: Option<Vec<u8>>,
    timeout: Duration,
    started_at: Instant,
) -> io::Result<(Output, bool)> {
    let mut stdin = child.stdin.take();
    let input = input.unwrap_or_default();
    let mut written = 0;
    let mut stdout = Reader::new(child.stdout.take());
    let mut stderr = Reader::new(child.stderr.take());
    for fd in [
        stdin.as_ref().map(AsRawFd::as_raw_fd),
        stdout.fd(),
        stderr.fd(),
    ]
    .into_iter()
    .flatten()
    {
        nonblocking(fd)?;
    }
    let mut status = None;
    loop {
        if status.is_none() {
            status = child.try_wait()?;
        }
        if status.is_some() || written == input.len() {
            stdin = None;
        }
        if let Some(status) = status {
            if stdout.fd().is_none() && stderr.fd().is_none() {
                return Ok((
                    Output {
                        status,
                        stdout: stdout.bytes,
                        stderr: stderr.bytes,
                    },
                    false,
                ));
            }
        }
        let remaining = (!timeout.is_zero()).then(|| timeout.saturating_sub(started_at.elapsed()));
        if remaining.is_some_and(|remaining| remaining.is_zero()) {
            // Kill the whole group before waiting or draining. The direct child
            // may already have exited while a descendant still owns its pipes.
            terminate(child);
            let status = child.wait()?;
            // A single nonblocking read per stream captures queued data without
            // waiting for EOF from descendants that escaped the process group.
            stdout.read_available()?;
            stderr.read_available()?;
            return Ok((
                Output {
                    status,
                    stdout: stdout.bytes,
                    stderr: stderr.bytes,
                },
                true,
            ));
        }

        // Bound work per iteration so continuously readable/writable pipes
        // cannot starve the deadline or the other stream.
        stdout.read_available()?;
        stderr.read_available()?;
        if let Some(pipe) = stdin.as_mut() {
            let end = input.len().min(written.saturating_add(64 * 1024));
            match pipe.write(&input[written..end]) {
                Ok(0) => stdin = None,
                Ok(count) => written += count,
                Err(error) if error.kind() == io::ErrorKind::BrokenPipe => stdin = None,
                Err(error)
                    if matches!(
                        error.kind(),
                        io::ErrorKind::WouldBlock | io::ErrorKind::Interrupted
                    ) => {}
                Err(error) => return Err(error),
            }
        }

        let mut fds = [
            poll_fd(stdin.as_ref().map(AsRawFd::as_raw_fd), libc::POLLOUT),
            poll_fd(stdout.fd(), libc::POLLIN),
            poll_fd(stderr.fd(), libc::POLLIN),
        ];
        // Pipes do not notify us when a process exits. Sleep in poll for at
        // most 10ms between status checks, including when every pipe is closed.
        let delay = remaining
            .unwrap_or(Duration::from_millis(10))
            .min(Duration::from_millis(10));
        let millis = delay.as_millis().max(1) as libc::c_int;
        // SAFETY: fds contains initialized pollfd entries and remains writable
        // for the entire call. Negative descriptors are ignored by poll.
        let ready = unsafe { libc::poll(fds.as_mut_ptr(), fds.len() as libc::nfds_t, millis) };
        if ready < 0 {
            let error = io::Error::last_os_error();
            if error.kind() != io::ErrorKind::Interrupted {
                return Err(error);
            }
        }
    }
}

struct Reader<R> {
    pipe: Option<R>,
    bytes: Vec<u8>,
}

impl<R: Read + AsRawFd> Reader<R> {
    fn new(pipe: Option<R>) -> Self {
        Self {
            pipe,
            bytes: Vec::new(),
        }
    }

    fn fd(&self) -> Option<RawFd> {
        self.pipe.as_ref().map(AsRawFd::as_raw_fd)
    }

    fn read_available(&mut self) -> io::Result<()> {
        if let Some(pipe) = self.pipe.as_mut() {
            let mut buffer = [0; 64 * 1024];
            match pipe.read(&mut buffer) {
                Ok(0) => self.pipe = None,
                Ok(count) => self.bytes.extend_from_slice(&buffer[..count]),
                Err(error)
                    if matches!(
                        error.kind(),
                        io::ErrorKind::WouldBlock | io::ErrorKind::Interrupted
                    ) => {}
                Err(error) => return Err(error),
            }
        }
        Ok(())
    }
}

fn nonblocking(fd: RawFd) -> io::Result<()> {
    // SAFETY: fd belongs to a live child pipe, and F_GETFL/F_SETFL accept the
    // integer arguments supplied here. Existing descriptor flags are preserved.
    let flags = unsafe { libc::fcntl(fd, libc::F_GETFL) };
    if flags < 0 || unsafe { libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK) } < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
    }
}

fn poll_fd(fd: Option<RawFd>, events: libc::c_short) -> libc::pollfd {
    libc::pollfd {
        fd: fd.unwrap_or(-1),
        events,
        revents: 0,
    }
}

fn terminate(child: &mut Child) {
    // SAFETY: process_group(0) placed this child in a group whose ID is its PID.
    // The negative ID targets that group, including children holding pipes.
    unsafe {
        libc::kill(-(child.id() as libc::pid_t), libc::SIGKILL);
    }
    // Also target the direct child if it changed its group after spawning.
    let _ = child.kill();
}
