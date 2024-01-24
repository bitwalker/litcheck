# litcheck

This project is a re-implementation of the `FileCheck` and `lit` utilities, used
in the LLVM project and elsewhere, as a single multi-call binary containing the
functionality of both while keeping them largely compatible with the exiting
tools via symlinks, if you so require.

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

* Support for `CHECK-DAG`/`CHECK-NOT` is all but done, however the rules are not
finished, so `filecheck` will panic if it encounters these directives
* Very little testing of various parts of the system, so bugs are numerous,
but I'm slowly adding more tests, and will eventually try to replicate the main
original FileCheck/lit test suite(s), to the degree it makes sense.
* I added support for a variety of target features and things used by LLVM,
but what I chose to support and didn't overall isn't super consistent, mostly
whether or not I would find it useful. If you are hoping for 100% compatiblity,
this is not that.
* `lit` uses Python scripts for configuration, `litcheck` does not, it uses simple
TOML configuration files for test suites, and you can accomplish most of what you'd
want this way. Until I hit a compelling reason for something more complicated, that's
what exists for now.
* You should use the upstream FileCheck/lit documentation for now, but I plan to
write some of my own that provides a better on-ramp for using this with non-LLVM
projects and infrastructure.
* The code is a bit of a mess, and I haven't yet gotten around to cleaning it up,
so don't judge me too harshly.

## Installation

First, `cargo install` it:

    $ cargo install --git https://github.com/bitwalker/litcheck
    
Then, you can use the tools using two different approaches:

1. As subcommands of `litcheck`

```
# lit
$ litcheck lit run path/to/test/suite
    
# filecheck
$ litcheck filecheck path/to/checks < path/to/verify
```

    
2. As independent commands via symlinks

```
# lit
$ ln -s -T /usr/local/bin/lit $(which litcheck)
$ lit --help

# filecheck
$ ln -s -T /usr/local/bin/FileCheck $(which litcheck)
$ filecheck --help
```
    

That's all there is to it!

## Quickstart

NOTE: See upstream [FileCheck](https://llvm.org/docs/CommandGuide/FileCheck.html) and [lit](https://www.llvm.org/docs/CommandGuide/lit.html)
documentation for details on how to write tests with, and configure, these tools. My own documentation is forthcoming, but this
will get you by in the near term.


1. Create a `lit` test suite:

```
$ mkdir -p tests
$ cat <<EOF > tests/lit.suite.toml
name = "hello-world"
patterns = ["*.txt"]
    
[format.shtest]
EOF
```
    
This will find tests in the `tests` directory matching `*.txt`, and run them using the `shtest` format.

2. Add a test:

```
$ cat <<EOF > tests/example.txt
# Assumes you've symlinked `filecheck`
# RUN: filecheck %s %s.input
# CHECK: Hello
# CHECK-SAME: {{\w+}}!
EOF
```
    
To keep things simple here, we're going to use two files, one with the patterns to match, and
one with the input text to verify/check. 

The `RUN:` directive is used by `lit` (specifically the `shtest` format) to determine how to
run this test. In this case, it is going to run the command `filecheck %s %s.input` in the
system shell. The "variable" `%s` is a substitution that `lit` replaces with the path to
the test file itself (see the `lit` docs). We use this to reference both the check file
and a file to verify (with an extra extension for simplicity). The other directives in
the file are for `FileCheck`, and we'll get to those in a second.


3. Add a file to verify (as referenced in the `RUN` directive above), containing our input:

    $ cat <<EOF > tests/example.txt.input
    Hello Paul!
    
The test file contains directives, e.g. `CHECK: Hello` consisting of patterns that can be of
a few different varieties (which can be mixed together on the same line): literals, regular
expressions, and match/substitution blocks. For example, `CHECK-SAME: {{\w+}}!` contains the
first two: a regular expression, followed by a literal `!`. The meaning of the various directives
is covered in the upstream FileCheck docs for now, but this pair of rules we're using is going
to essentially match a single line in the input with the equivalent of `^(.*)Hello(.*)\w+!(.*)$`,
i.e. both rules must match the same line, but it is not required that the `CHECK-SAME` pattern
start immediately after the `CHECK`, just that somewhere after the appearance of `Hello`, the
`{{\w+}}!` pattern matches.

Ok, so we've written our test suite config, a test file, and have some inputs to test, how
do we run the suite?

```
# Assuming you've symlinked `lit`
$ lit run tests
    
# If you want to dump all the output
$ lit run tests --show-all
```
    
That's the gist of it. There's a lot more to these tools, but that is the rundown on how to
get up and running with them!
