# Writing tests with litcheck

litcheck runs commands and checks their output against patterns you write. It is
useful for testing compilers, command-line tools, generated code, diagnostics, and
other text output without keeping a complete snapshot of everything printed.

There are two parts:

- `litcheck filecheck` checks text against expectations in a file.
- `litcheck lit run` discovers tests and executes their `RUN:` commands.

Start with a standalone check, then add a suite when you want to run several
tests together. The examples use a Unix-like shell and the `litcheck` binary on
`PATH`:

~~~sh
cargo install litcheck
litcheck --help
~~~

When working from this repository, you can instead build the binary with
`cargo build -p litcheck --locked` and add `target/debug` to `PATH`.

## Contents

1. [Check a line of output](#check-a-line-of-output)
2. [Create a test suite](#create-a-test-suite)
3. [Describe the layout of output](#describe-the-layout-of-output)
4. [Use regular expressions and captures](#use-regular-expressions-and-captures)
5. [Check numeric relationships](#check-numeric-relationships)
6. [Group checks and share expectations](#group-checks-and-share-expectations)
7. [Test failures and empty output](#test-failures-and-empty-output)
8. [Use input files and temporary output](#use-input-files-and-temporary-output)
9. [Configure tools and subdirectories](#configure-tools-and-subdirectories)
10. [Select tests with features](#select-tests-with-features)
11. [Run tests and diagnose failures](#run-tests-and-diagnose-failures)
12. [Use other test entry points](#use-other-test-entry-points)

## Check a line of output

Save this as `hello.check`:

~~~text
CHECK: Hello, world!
~~~

Run a command and pipe its output into the checker:

~~~sh
printf 'Hello, world!\n' | litcheck filecheck hello.check
~~~

A successful standalone check prints nothing and exits with status zero. Change
the output to `Goodbye, world!` to see a failure and its source locations.

The check file and the input are separate things. To check an existing output
file, use `--input-file`:

~~~sh
printf 'Hello, world!\n' > hello.out
litcheck filecheck hello.check --input-file hello.out
~~~

There is one positional argument: the check file. An input file is supplied with
`--input-file` or standard input.

`CHECK:` searches forward for the literal text following the colon. It can skip
unrelated text, and the match does not have to occupy the whole line. This lets
you assert what matters without recording every detail of the output.

## Create a test suite

Create a `tests` directory. Save this as `tests/lit.suite.toml`:

~~~toml
name = "hello-world"
patterns = ["*.txt"]

[format.shtest]
~~~

The configuration names the suite, selects its test files, and chooses the shell
test format. Specify `patterns` explicitly. Discovery descends into subdirectories.

Save this as `tests/greeting.txt`:

~~~text
# RUN: printf 'Hello, world!\n' | FileCheck "%s"
# CHECK: Hello, world!
~~~

Run it from the directory containing `tests`:

~~~sh
litcheck lit run tests
~~~

The result includes `PASS: hello-world::greeting.txt`. A suite run exits nonzero
when it has failing tests.

`RUN:` is a shell command to execute. `%s` expands to the path of this test file,
so the same file holds both the command and its expectations. The `#` makes the
directives look like comments; in a source file you can use its comment syntax,
such as `// RUN:`.

Inside `RUN:` commands, bare `FileCheck`, `filecheck`, and `not` are automatically
rewritten to invoke this installation of litcheck. No symlinks are needed.
Outside a suite, use `litcheck filecheck` and `litcheck not` explicitly.

The remaining `tests/*.txt` examples can be added to this suite. Replace
`printf` with the tool you want to test when adapting them to a real project.

## Describe the layout of output

Save this as `tests/layout.txt`:

~~~text
# RUN: printf 'begin\nresult: 42 units\n\nend\n' | FileCheck "%s"
# CHECK: begin
# CHECK-NEXT: result:
# CHECK-SAME: 42 units
# CHECK-EMPTY:
# CHECK-NEXT: end
~~~

Each positive match establishes the position from which the next check starts:

| Directive | Expectation |
| --- | --- |
| `CHECK: text` | Find `text` later in the input. |
| `CHECK-NEXT: text` | Find `text` on the next line. |
| `CHECK-SAME: text` | Find `text` later on the current line. |
| `CHECK-EMPTY:` | The next line must be empty. It has no pattern. |
| `CHECK-NOT: text` | `text` must not occur in the region between surrounding positive checks. |

`CHECK-SAME` does not require adjacent text; it can skip characters on the same
line. `NEXT`, `SAME`, and `EMPTY` need a preceding positive check.

For example, this test allows harmless text between its first and last checks,
but rejects a warning in that interval:

Save this as `tests/no-warning.txt`:

~~~text
# RUN: printf 'begin\nwork complete\nend\n' | FileCheck "%s"
# CHECK: begin
# CHECK-NOT: warning:
# CHECK: end
~~~

A leading `CHECK-NOT` checks before the first positive match; a trailing one
checks after the last match to the end of its region. One negative check in the
middle does not forbid that text everywhere in the file. To apply a policy
between all positive checks, use:

~~~sh
litcheck filecheck tests/no-warning.txt --implicit-check-not='warning:' \
  --input-file output.txt
~~~

Here `output.txt` is your tool's saved output. Implicit exclusions use the same
literal and `{{regex}}` syntax as explicit negative checks; text consumed by a
positive match is allowed.

## Use regular expressions and captures

Text outside `{{...}}` and `[[...]]` is literal. Put only the variable part of
an expectation inside a regular expression.

Save this as `tests/captures.txt`:

~~~text
# RUN: printf 'created item.17\nusing item.17\n' | FileCheck "%s"
# CHECK: created [[ITEM:item\.[0-9]+]]
# CHECK-NEXT: using [[ITEM]]
~~~

`[[ITEM:regex]]` captures the matching text. `[[ITEM]]` reuses it literally, so
the second line must name the same item, including the dot. String captures
can also be reused later in the same directive.

An unnamed regular expression is useful when the exact value is irrelevant:

~~~text
# CHECK: elapsed: {{[0-9]+}} ms
~~~

Use regex character classes such as `{{[ \t]+}}` when a separator may contain
spaces or tabs. Patterns use Rust-style regular expressions; do not assume
look-around or regex backreferences are available. Use named captures and
substitutions to relate separate matches.

To treat the pattern markers themselves as literal text, use the literal
modifier.

Save this as `tests/literal.txt`:

~~~text
# RUN: printf '[[raw]] {{raw}}\n' | FileCheck "%s"
# CHECK{LITERAL}: [[raw]] {{raw}}
~~~

To require complete lines, enable `--match-full-lines`. Leading and trailing
horizontal whitespace is permitted unless you also enable
`--strict-whitespace`:

~~~sh
litcheck filecheck hello.check --match-full-lines --strict-whitespace \
  --input-file hello.out
~~~

This requires exactly `Hello, world!` on that line. Negative checks still match
substrings with `--match-full-lines`. Use `--ignore-case` when letter case should
not affect matching.

## Check numeric relationships

Numeric captures let you check relationships without hard-coding every value.

Save this as `tests/numbers.txt`:

~~~text
# RUN: printf 'count=7\nnext=8\nhex=00AF\n' | FileCheck "%s"
# CHECK: count=[[#COUNT:]]
# CHECK-NEXT: next=[[#NEXT:COUNT+1]]
# CHECK-NEXT: hex=[[#%.4X,HEX:]]
~~~

`[[#COUNT:]]` captures a number. `[[#COUNT+1]]` would match its successor without
capturing it; `[[#NEXT:COUNT+1]]` also saves the matched successor as `NEXT` and
requires equality to the expression. The last check captures an uppercase
hexadecimal value with a minimum precision of four digits.

| Format | Example |
| --- | --- |
| `%u` | Unsigned decimal, the default for a new numeric capture. |
| `%d` | Signed decimal, such as `[[#%d,DELTA:]]`. |
| `%x` / `%X` | Hexadecimal / uppercase hexadecimal. |
| `%#x` | Hexadecimal with a `0x` prefix. |
| `%.4X` | Uppercase hexadecimal with at least four digits. |

Numeric expressions support `+` and `-`, parentheses, and the two-argument
functions `add`, `sub`, `mul`, `div`, `min`, and `max`. Multiplication and division
use function syntax, such as `mul(COUNT,2)`. Keep values bounded; arithmetic and
numeric formatting are not arbitrary-precision operations.

Use a numeric variable on a later check line after capturing it. Unlike string
substitution, a numeric variable defined earlier in the same directive cannot
be referenced by another expression in that directive. A name cannot switch
between string and numeric types, and numeric redefinitions must retain their
format.

You can provide initial values with `-D`. Save this as `version.check`:

~~~text
CHECK: version=[[VERSION]], next=[[#START+1]]
~~~

Then check output using those definitions:

~~~sh
printf 'version=demo, next=8\n' | \
  litcheck filecheck version.check -DVERSION=demo '-D#START=7'
~~~

Quote numeric definitions when shell syntax would otherwise interpret their
contents.

When testing diagnostics, `[[#@LINE]]` refers to the line number of its check
directive. The shell runner's `%(line+N)` substitution can supply a related
source line to a command.

Save this as `tests/line-number.txt`:

~~~text
# RUN: printf 'error at line %(line+1)\n' | FileCheck "%s"
# CHECK: error at line [[#@LINE]]
~~~

## Group checks and share expectations

### Unordered output

Use consecutive `CHECK-DAG` directives for output whose order may vary.

Save this as `tests/workers.txt`:

~~~text
# RUN: printf 'worker beta ready\nworker alpha ready\nall ready\n' | FileCheck "%s"
# CHECK-DAG: worker alpha ready
# CHECK-DAG: worker beta ready
# CHECK: all ready
~~~

The DAG patterns must find distinct, non-overlapping matches. An intervening
ordinary or negative check separates groups; it is not part of the unordered
set. Use this only where ordering is intentionally irrelevant.

### Repeated output

Save this as `tests/count.txt`:

~~~text
# RUN: printf 'item\nitem\ndone\n' | FileCheck "%s"
# CHECK-COUNT-2: item
# CHECK-NOT: item
# CHECK: done
# CHECK-NOT: item
~~~

`CHECK-COUNT-2` requires two successive matches in the check sequence. It does
not by itself prohibit additional occurrences; the negative checks do that
before and after `done` in this example.

### Separate functions or sections

Labels partition the input so one section's checks cannot accidentally match
text belonging to the next.

Save this as `tests/sections.txt`:

~~~text
# RUN: printf 'function first\nreg=r1\nuse r1\nfunction second\nreg=r2\nuse r2\n' | FileCheck --enable-var-scope "%s"
# CHECK-LABEL: function first
# CHECK: reg=[[REG:r[0-9]+]]
# CHECK-NEXT: use [[REG]]
# CHECK-LABEL: function second
# CHECK: reg=[[REG:r[0-9]+]]
# CHECK-NEXT: use [[REG]]
~~~

Label patterns must be literal text or regular expressions, without variables.
`--enable-var-scope` clears local captures at each label. Names beginning with
`$` are global; command-line definitions also survive label boundaries.

### Several modes in one file

Select custom check prefixes to share common expectations while keeping
mode-specific checks separate.

Save this as `tests/modes.txt`:

~~~text
# RUN: printf 'mode=fast\nresult=42\n' | FileCheck --check-prefixes=FAST,COMMON "%s"
# RUN: printf 'mode=debug\nresult=42\n' | FileCheck --check-prefixes=DEBUG,COMMON "%s"
# FAST: mode=fast
# DEBUG: mode=debug
# COMMON: result=42
~~~

The checker processes selected directives in file order. Selecting custom
prefixes replaces the default `CHECK` prefix; include it explicitly if needed.
Each explicitly selected prefix must occur unless `--allow-unused-prefixes` is
set. Use that option deliberately so a misspelled prefix does not hide a test.

## Test failures and empty output

To test a command that should fail, use `not` to check its exit status and capture
its diagnostics.

Save this as `tests/failure.txt`:

~~~text
# RUN: not sh -c 'printf "error: invalid input\n" >&2; exit 1' > "%t.out" 2>&1
# RUN: FileCheck --check-prefix=ERROR "%s" --input-file "%t.out"
# ERROR: error: invalid input
~~~

`not` succeeds when the command exits nonzero without crashing. An unexpected
success or crash fails the test. `not --crash` is available when a crash is
specifically the expected outcome.

Separate the command from the output check this way when both exit status and
text matter. The shell format defaults to `sh` without `pipefail`, so a pipeline
usually reports the last command's status. If you prefer pipelines, configure
`shell = "bash"` and `pipefail = true` under `[format.shtest]`, with Bash installed.

For successful commands expected to produce no output, permit empty checker
input and forbid every character, including newlines.

Save this as `tests/empty.txt`:

~~~text
# RUN: true > "%t.out" 2>&1
# RUN: FileCheck --allow-empty "%s" --input-file "%t.out"
# CHECK-NOT: {{[\s\S]}}
~~~

Without `--allow-empty`, empty input is rejected even if it contains no
forbidden text.

## Use input files and temporary output

Keep helper inputs out of test discovery. In `tests/lit.suite.toml`, add
`exclude` alongside `patterns`, before any table headers:

~~~toml
name = "hello-world"
patterns = ["*.txt"]
exclude = ["Inputs/*.txt"]

[format.shtest]
~~~

Create `tests/Inputs/message.txt` containing `Hello from a file!`. Save this
test as `tests/files.txt`:

~~~text
# RUN: cat "%S/Inputs/message.txt" > "%t.out"
# RUN: FileCheck "%s" --input-file "%t.out"
# CHECK: Hello from a file!
~~~

The two `RUN:` directives execute sequentially in one shell script. A failed
command prevents later `RUN:` groups from executing. Shell state, such as
variables and the current directory, also carries between groups.

| Substitution | Meaning |
| --- | --- |
| `%s` | The test file's path. |
| `%S` | Its containing source directory. |
| `%t` | A unique per-test temporary filename; append suffixes such as `%t.out`. |
| `%basename_t` | The temporary filename's stem. |
| `%%` | A literal percent sign. |
| `%(line)`, `%(line+N)`, `%(line-N)` | The RUN directive's line number, with an optional offset. |

Quote paths after substitution. Tests normally execute in a temporary suite
working directory, not beside the source file, so use `%s` and `%S` rather than
assuming relative paths resolve under `tests`.

Substitution happens before the shell interprets quoting. A shell format such
as `printf '%s\n'` must therefore be written `printf '%%s\n'` inside a `RUN:` line.
Long `RUN:` commands can continue onto the next `RUN:` line with a trailing
backslash.

If a source payload contains text that looks like shell-test directives, put
`# END.` after the command header to stop the runner from parsing more
directives. FileCheck still reads the check patterns throughout the file.

## Configure tools and subdirectories

### Environment and tool paths

For a real tool, set environment variables and suite-wide command substitutions
in TOML. This is a separate example configuration; replace the path with your
tool's absolute path:

~~~toml
name = "compiler"
patterns = ["*.txt"]

[env]
LC_ALL = "C"

[substitutions]
'%\{tool\}' = '/absolute/path/to/my-tool'

[format.shtest]
~~~

Use `"%{tool}"` in a `RUN:` command, for example:

~~~text
# RUN: "%{tool}" "%s" > "%t.out" 2>&1
# RUN: FileCheck "%s" --input-file "%t.out"
~~~

Configuration substitution keys are regular expressions, so escape braces when
matching a name such as `%{tool}` literally. TOML's single-quoted strings
preserve the backslashes. Replacement values support regex captures such as
`$1`; use `$$` for a literal dollar sign. Environment values are ordinary
strings, and commands inherit the invoking process's environment in addition to
the configured entries.

For test-specific text substitutions, `DEFINE` and `REDEFINE` use literal keys
and values instead:

Save this as `tests/definitions.txt`:

~~~text
# DEFINE: %{word} = first
# RUN: printf '%%s\n' '%{word}' | FileCheck --check-prefix=FIRST "%s"
# REDEFINE: %{word} = second
# RUN: printf '%%s\n' '%{word}' | FileCheck --check-prefix=SECOND "%s"
# FIRST: first
# SECOND: second
~~~

These definitions affect subsequent `RUN:` lines. `DEFINE` requires a new name;
`REDEFINE` requires an existing one. Their values are not regex replacement
strings, so dollar signs do not need regex escaping.

### Directory-specific configuration

Place a `lit.local.toml` in a test subdirectory when its tests need extra
features, environment variables, substitutions, or another format. For example,
`tests/vector/lit.local.toml` can contain:

~~~toml
available_features = ["vector-tests"]

[env]
VECTOR_MODE = "1"
~~~

Local configuration is inherited from the suite root down to the test's
directory. Environment and substitution maps merge, with child values
overriding the same keys; feature sets accumulate. Nonempty `patterns` and
`exclude` lists replace inherited lists. Omitted or empty lists inherit them.
A nested `lit.suite.toml` starts a separate suite instead.

Suite configurations can also set `source_dir` and `working_dir` relative to the
configuration file. An explicitly configured working directory must exist
already. Use a separate build directory for generated files.

## Select tests with features

List project capabilities in the suite configuration, before table headers:

~~~toml
available_features = ["json-output", "threads"]
~~~

Then select the tests that need them. For example, with those features available:

Save this as `tests/feature.txt`:

~~~text
# REQUIRES: json-output && threads
# UNSUPPORTED: broken-json
# RUN: printf '{"ok":true}\n' | FileCheck "%s"
# CHECK: {"ok":true}
~~~

`REQUIRES` needs every comma-separated expression to be true.
`UNSUPPORTED` skips the test when any expression is true. Expressions support
`!`, `&&`, `||`, parentheses, and regex fragments in `{{...}}`.
The runner also provides host and target features; inspect them with
`litcheck lit show-suites tests`. `--target` changes feature selection, not how
commands are compiled or where they execute.

Mark a known failing test with `XFAIL: feature-expression`, or `XFAIL: *` to
expect failure on every configuration. An ordinary failure becomes `XFAIL`; a
success becomes `XPASS` and fails the suite so an obsolete expectation is noticed.
Use feature conditions to make the expectation as narrow as possible.

`ALLOWED_RETRIES: 1` permits one additional attempt after an ordinary failure.
A later success is `FLAKYPASS`. Temporary paths are shared between attempts;
timeouts and unresolved tests are not retried. Prefer deterministic tests and
use retries only for a specific transient condition.

## Run tests and diagnose failures

Put options before the test paths:

~~~sh
litcheck lit show-tests tests
litcheck lit show-suites tests
litcheck lit run tests/greeting.txt
litcheck lit run --filter 'greeting|numbers' tests
litcheck lit run --filter-out 'slow' tests
litcheck lit run -j 1 --verbose tests
litcheck lit run --show-all --timeout 10 tests
~~~

Use `--verbose` to see commands and output from failing tests, or `--show-all`
for passing tests too. `--path /absolute/tool/directory` adds a command search
directory. `--timeout` is in seconds; zero means unlimited. Finite command
timeouts currently require Unix.

| Result | Meaning |
| --- | --- |
| `PASS` | The test succeeded. |
| `FAIL` | Its command or output checks failed. |
| `XFAIL` / `XPASS` | Expected failure / unexpected success. |
| `UNSUPPORTED` | Feature conditions excluded the test. |
| `UNRESOLVED` | The test could not be run or interpreted, such as a missing RUN directive. |
| `TIMEOUT` | A command exceeded its time limit. |
| `FLAKYPASS` | It passed on a retry. |

A missing `RUN:` is an error even for a feature-gated shell test.

For a matching failure, first reproduce the producer command and save its exact
output. Then invoke the checker directly:

~~~sh
litcheck filecheck tests/greeting.txt --input-file output.txt -vv
~~~

The report identifies the check location and input region. `-v` also reports
successful directive matches; `-vv` expands large searched regions in failure
reports. `--dump-input=never` suppresses input context. Verbosity and dump
formatting follow litcheck's own behavior rather than LLVM's exact trace format.

`FILECHECK_OPTS` supplies additional checker options from the environment.
Unset it when an otherwise identical reproduction behaves differently. Likewise,
`LIT_FILTER` and `LIT_FILTER_OUT` can affect which tests run.

When command substitution itself is unclear, inspect the generated script:

~~~sh
LITCHECK_TRACE=lit:shtest=trace litcheck lit run -j 1 --show-all tests/greeting.txt
~~~

Keep checks specific enough to catch regressions, but avoid unstable details
such as temporary paths, timings, or unrelated diagnostic wording. Use
`CHECK-NEXT` only when line adjacency is part of the behavior, and use negative
checks wherever the absence of output matters.

## Use other test entry points

### Executable tests

For tests that already decide their own result, create a separate suite:

~~~toml
name = "scripts"
patterns = ["*.sh"]

[format.executable]
~~~

For example, save that as `script-tests/lit.suite.toml` and make
`script-tests/smoke.sh` executable:

~~~sh
#!/bin/sh
test 2 -eq 2
~~~

~~~sh
chmod +x script-tests/smoke.sh
litcheck lit run script-tests
~~~

The executable format runs the file directly and uses its exit status. It does
not parse `RUN`, `CHECK`, or other shell-test directives.

### Rust tests

The `litcheck-filecheck` crate also exposes the checker as a library. Add it as a
development dependency:

~~~toml
[dev-dependencies]
litcheck-filecheck = "0.5"
~~~

The current macro also needs the `litcheck` re-export in scope, so import both
names when using it in a normal Rust test:

~~~rust
use litcheck_filecheck::{filecheck, litcheck};

#[test]
fn renders_a_summary() {
    let output = "count: 2\nstatus: ready\n";
    filecheck!(output, "
        ; CHECK: count: 2
        ; CHECK-NEXT: status: ready
    ");
}
~~~

The macro panics with diagnostics on a mismatch, so Cargo reports a failed test.
It also accepts a `Config` as a third argument when you need checker options.

For the full current CLI option lists, use `litcheck filecheck --help` and
`litcheck lit run --help`. Suite configuration is TOML, not Python lit
configuration; the examples here use the implementation's supported behavior.
