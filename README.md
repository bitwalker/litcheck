# litcheck

This project is a re-implementation of the `FileCheck` and `lit` utilities, used
in the LLVM project and elsewhere, as a single multi-call binary containing the
functionality of both while keeping them largely compatible with the existing
tools via symlinks, if you so require.

> [!WARNING]
> This is a work-in-progress/experiment, it doesn't necessarily support
> the full lit/FileCheck feature set with the exact semantics of LLVM lit/FileCheck.

> [!NOTE] 
> This project is a playground for experimenting with various abstractions
> in the face of zero-copy (or minimal copy) processing of input. As a result, the code 
> (namely in the FileCheck impl) has some duplication and some oddities in its structure 
> that will be eventually factored out as I make changes. Don't look at the code as a 
> model architecture for this kind of tooling, at least at this point in time.

See [Writing tests with litcheck](docs/user-guide.md) for a guide that starts with
simple output checks and builds up to more involved test suites.

## Rationale

Why?

* I wanted a small, fast, single-binary tool that I could use across projects
to solve the problems that `FileCheck` and `lit` solve.
* Other implementations of one or the other of these things are typically
very limited in scope, many features are missing. This implementation aims
to be full fidelity in terms of the kinds of tests you can write.
* I wanted the ability to use these tools as a library within a larger
project, for example, to run tests via the Rust test runner using this
as the backend.
* I wanted the ability to extend the feature set myself

## Current Status

The initial implementation is done, albeit with a few missing items I will be
checking off in the near future, or which you should be aware of:

* The imported LLVM FileCheck fixtures run through Cargo tests and CI. Some
unsupported diagnostic features remain explicitly marked as expected failures.
* (lit) I added support for a variety of target features and things used by LLVM,
but what I chose to support and didn't overall isn't super consistent, mostly
whether or not I would find it useful. If you are hoping for 100% compatibility,
this is not that.
* `lit` uses Python scripts for configuration, `litcheck` does not, it uses simple
TOML configuration files for test suites, and you can accomplish most of what you'd
want this way. Until I hit a compelling reason for something more complicated, that's
what exists for now.
* The [user guide](docs/user-guide.md) covers writing tests and configuring suites
for this implementation, including its differences from LLVM lit/FileCheck.
* The code is a bit of a mess, and I haven't yet gotten around to cleaning it up,
so don't judge me too harshly.

## Installation

First, `cargo install` it:

    $ cargo install litcheck
    
Then, you can use the tools using two different approaches:

1. As subcommands of `litcheck`

```
# lit
$ litcheck lit run path/to/test/suite
    
# filecheck
$ litcheck filecheck path/to/checks < path/to/verify
```

    
2. As independent commands via symlinks, for example on Unix:

```sh
mkdir -p "$HOME/.local/bin"
ln -s "$(command -v litcheck)" "$HOME/.local/bin/lit"
ln -s "$(command -v litcheck)" "$HOME/.local/bin/filecheck"
```

Ensure `$HOME/.local/bin` is on `PATH`, then use `lit` and `filecheck` directly.
Use the lowercase `filecheck` symlink name. Tests run through `litcheck lit` can
use bare `FileCheck` and `not` commands without creating symlinks.

That's all there is to it!

## Quickstart

Create a test suite and one test:

```sh
mkdir -p tests
cat > tests/lit.suite.toml <<'EOF'
name = "hello-world"
patterns = ["*.txt"]

[format.shtest]
EOF

cat > tests/greeting.txt <<'EOF'
# RUN: printf 'Hello, world!\n' | FileCheck "%s"
# CHECK: Hello, world!
EOF

litcheck lit run tests
```

This runs the command in `RUN:` and checks its output against `CHECK:`. The
runner replaces `%s` with the test file's path and invokes its own checker for
`FileCheck`.

Read [Writing tests with litcheck](docs/user-guide.md) for line ordering, regular
expressions, captures, numeric relationships, negative tests, suite configuration,
and troubleshooting.
