use std::{
    collections::BTreeMap,
    process::ExitCode,
    sync::{
        Arc,
        mpsc::{self, Sender},
    },
};

use clap::Parser;
use indicatif::{ParallelProgressIterator, ProgressBar, ProgressStyle};
use lit::{
    Config, DefaultTestSuiteRegistry, LitError, Test, TestFormat, TestResult, TestStatus,
    TestSuiteRegistry, TestSuiteRegistryExt,
};
use litcheck::diagnostics::{DiagResult, Report};
use rayon::prelude::*;
use regex::Regex;

use super::Command;

/// lit is a tool for executing LLVM-style test suites, summarizing their results, and
/// providing indication of failures. It is designed to be lightweight and user-friendly.
///
/// lit should be run with one or more TESTS, which are paths to individual test files,
/// or directories to search for tests.
///
/// Each lit test must exist inside some test suite. lit resolves the inputs specified on
/// the command line to test suites by searching upwards from the input path until it finds
/// a `lit.suite.toml` file. These files serve both as a marker of test suites, and as
/// configuration files which lit loads in order to understand how to find and run the tests
/// which are part of that suite.
///
/// Once lit has mapped the inputs to test suites, it traverses the list of inputs, adding
/// tests for individual files, and recursively searching for tests in directories.
///
/// Each specified test will be executed and once all tests have been run, lit will print
/// summary information on the number of tests which passed or failed. lit will exit with
/// a non-zero status code if any tests fail.
#[derive(Debug, Parser)]
#[command(name = "lit")]
pub enum Lit {
    /// Run all discovered test suites
    ///
    /// If no command is given, this is the default
    Run(lit::Options),

    /// List the discovered test suites and exit
    #[command(name = "show-suites")]
    Suites {
        /// The test files or directories to search for tests
        #[arg(value_name = "PATH", required(true), trailing_var_arg(true))]
        tests: Vec<String>,
        /// Run only those tests whose name matches the given regular expression.
        #[arg(long, env = "LIT_FILTER", help_heading = "Selection Options")]
        filter: Option<Regex>,
        /// Exclude those tests whose name matches the given regular expression.
        #[arg(long, env = "LIT_FILTER_OUT", help_heading = "Selection Options")]
        filter_out: Option<Regex>,
    },

    /// List the discovered tests and exit
    #[command(name = "show-tests")]
    Tests {
        /// The test files or directories to search for tests
        #[arg(value_name = "PATH", required(true), trailing_var_arg(true))]
        tests: Vec<String>,
        /// Run only those tests whose name matches the given regular expression.
        #[arg(long, env = "LIT_FILTER", help_heading = "Test Selection")]
        filter: Option<Regex>,
        /// Exclude those tests whose name matches the given regular expression.
        #[arg(long, env = "LIT_FILTER_OUT", help_heading = "Test Selection")]
        filter_out: Option<Regex>,
    },
}
impl Command for Lit {
    fn is_help_requested(&self) -> bool {
        false
    }

    /// The main entrypoint for the `lit` command
    fn run(self) -> DiagResult<ExitCode> {
        let config;
        let show_suites;
        let show_tests;
        match self {
            Self::Run(options) => {
                config = Arc::new(lit::Config {
                    options,
                    ..Default::default()
                });
                show_suites = false;
                show_tests = false;
            }
            Self::Suites {
                tests,
                filter,
                filter_out,
            } => {
                let mut cfg = Box::new(lit::Config::new(tests));
                cfg.options.filter = filter;
                cfg.options.filter_out = filter_out;
                config = Arc::from(cfg);
                show_suites = true;
                show_tests = false;
            }
            Self::Tests {
                tests,
                filter,
                filter_out,
            } => {
                let mut cfg = Box::new(lit::Config::new(tests));
                cfg.options.filter = filter;
                cfg.options.filter_out = filter_out;
                config = Arc::from(cfg);
                show_suites = false;
                show_tests = true;
            }
        }

        // Initialize the test suite manager by loading test suites
        // for the specified input paths.
        let mut registry = DefaultTestSuiteRegistry::default();
        registry.load(&config)?;

        if registry.is_empty() {
            eprintln!("did not discover any test suites for provided path(s)");
            return Ok(ExitCode::from(100u8));
        }

        if show_suites {
            self::show_suites(&registry);
            return Ok(ExitCode::SUCCESS);
        }
        // If we have no tests, raise an error
        if registry.is_empty() {
            return Err(Report::new(LitError::NoTests));
        }

        if show_tests {
            self::show_tests(&registry);
            return Ok(ExitCode::SUCCESS);
        }

        start(registry, config)
    }
}

/// Show the available test suites, and then exit
pub fn show_suites<R>(registry: &R)
where
    R: TestSuiteRegistryExt,
{
    println!("-- Test Suites --");
    for suite in registry.suites() {
        println!(
            "  * {} - {} tests",
            suite.name(),
            registry.size_of_suite(&suite.id())
        );
        println!("    Source Root  : {}", suite.source_dir().display());
        println!("    Exec Root    : {}", suite.working_dir().display());
        println!("    Test Format  : {}", suite.config.format.name());
        println!("    Available Features:");
        for feature in suite.config.available_features.iter() {
            println!("        - {feature}");
        }
        println!("    Available Substitutions:");
        let max_width = suite
            .config
            .substitutions
            .keys()
            .map(|k| k.len())
            .max()
            .unwrap_or(0);
        for (k, v) in suite.config.substitutions.iter() {
            let padding = max_width - k.len();
            println!("        - {k}{1:0$}: {v}", padding, "");
        }
        let max_width = suite.config.env.keys().map(|k| k.len()).max().unwrap_or(0);
        println!("    Environment Variables:");
        for (k, v) in suite.config.env.iter() {
            let padding = max_width - k.len();
            println!("        - {k}{1:0$}: {v}", padding, "");
        }
    }
}

/// Show the available tests, and then exit
pub fn show_tests<R>(registry: &R)
where
    R: TestSuiteRegistryExt,
{
    println!("-- Available Tests --");
    for test in registry.tests() {
        println!(
            "  {}::{} (format={})",
            &test.suite.name(),
            test.path.display(),
            test.config.format.name()
        );
    }
}

/// Start the `lit` test interface, and run to completion.
pub fn start<R>(registry: R, config: Arc<Config>) -> DiagResult<ExitCode>
where
    R: TestSuiteRegistryExt,
    <R as IntoIterator>::IntoIter: Send,
{
    let num_tests = registry.num_tests();
    let num_workers = core::cmp::max(
        core::cmp::min(config.options.workers.unwrap_or(usize::MAX), num_tests),
        0,
    );

    rayon::ThreadPoolBuilder::new()
        .num_threads(num_workers)
        .build_global()
        .unwrap();

    let progress = ProgressBar::new(num_tests as u64)
        .with_prefix("Testing")
        .with_style(
            ProgressStyle::with_template(
                "{spinner:.green} [{bar:40.cyan/blue}] {pos:>4}/{len:4} {prefix}: {msg}",
            )
            .unwrap()
            .progress_chars("#>-"),
        );

    let (sender, receiver) = mpsc::channel();
    let runner = TestRunner {
        sender,
        progress,
        show_all: config.options.all,
        show_non_error: !config.options.quiet,
        show_output: config.options.all || config.options.verbose,
    };

    let results = Arc::new(TestResultManager::new(
        receiver,
        config.clone(),
        registry.into_iter(),
    ));

    let result = TestResultManagerIter(results.clone())
        .par_bridge()
        .progress_with(runner.progress.clone())
        .try_for_each_with(runner, |runner, selection| match selection {
            SelectResult::Selected(test) => runner.run(test, &config),
            SelectResult::Excluded(test) => runner.mark_excluded(test),
        });

    if let Err(ref err) = result {
        println!("{err}: skipping remaining tests");
        println!();
    } else if !config.options.quiet {
        println!();
    }

    let results_by_status = Arc::into_inner(results).unwrap().into_results()?;

    print_results(&results_by_status, &config);

    Ok(ExitCode::SUCCESS)
}

enum SelectResult {
    Selected(Arc<Test>),
    Excluded(Arc<Test>),
}

struct TestReport {
    pub test: Arc<Test>,
    pub result: TestResult,
}

struct TestResultManager<I: Iterator<Item = Arc<Test>> + Send> {
    receiver: mpsc::Receiver<TestReport>,
    config: Arc<Config>,
    tests: parking_lot::Mutex<I>,
}
unsafe impl<I: Iterator<Item = Arc<Test>> + Send> Send for TestResultManager<I> {}
impl<I: Iterator<Item = Arc<Test>> + Send> TestResultManager<I> {
    fn new(receiver: mpsc::Receiver<TestReport>, config: Arc<Config>, tests: I) -> Self {
        Self {
            receiver,
            config,
            tests: parking_lot::Mutex::new(tests),
        }
    }

    fn into_results(self) -> DiagResult<BTreeMap<TestStatus, Vec<TestReport>>> {
        let mut all_excluded = true;
        let mut results = BTreeMap::<TestStatus, Vec<TestReport>>::default();
        for report in self.receiver {
            if report.result.status != TestStatus::Excluded {
                all_excluded = false;
            }
            results
                .entry(report.result.status)
                .or_insert_with(Vec::new)
                .push(report);
        }
        let mut skipped = vec![];
        let excluded = results.entry(TestStatus::Excluded).or_insert(vec![]);
        for test in self.tests.into_inner() {
            if self.config.is_selected(test.name()) {
                all_excluded = false;
                skipped.push(TestReport {
                    test,
                    result: TestResult::new(TestStatus::Skipped),
                });
            } else {
                excluded.push(TestReport {
                    test,
                    result: TestResult::new(TestStatus::Excluded),
                });
            }
        }
        if excluded.is_empty() {
            results.remove(&TestStatus::Excluded);
        }
        if !skipped.is_empty() {
            results.insert(TestStatus::Skipped, skipped);
        }
        if all_excluded {
            Err(LitError::NoTestsSelected {
                available: results.values().map(|reports| reports.len()).sum(),
            }
            .into())
        } else {
            Ok(results)
        }
    }
}

struct TestResultManagerIter<I: Iterator<Item = Arc<Test>> + Send>(Arc<TestResultManager<I>>);
unsafe impl<I: Iterator<Item = Arc<Test>> + Send> Send for TestResultManagerIter<I> {}
impl<I: Iterator<Item = Arc<Test>> + Send> Iterator for TestResultManagerIter<I> {
    type Item = SelectResult;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        let test = self.0.tests.lock().next()?;
        if self.0.config.is_selected(test.name()) {
            Some(SelectResult::Selected(test))
        } else {
            Some(SelectResult::Excluded(test))
        }
    }
}

#[derive(Clone)]
struct TestRunner {
    sender: Sender<TestReport>,
    progress: ProgressBar,
    show_all: bool,
    show_non_error: bool,
    show_output: bool,
}
impl TestRunner {
    pub fn mark_excluded(&mut self, test: Arc<Test>) -> DiagResult<()> {
        self.progress.set_message(test.name().to_string());

        self.report(test, TestResult::new(TestStatus::Excluded))
    }

    pub fn run(&mut self, test: Arc<Test>, config: &Config) -> DiagResult<()> {
        self.progress.set_message(test.name().to_string());

        let result = test.suite.config.format.execute(&test, config)?;
        if result.is_failure() {
            self.progress.suspend(|| {
                println!(
                    "{}: {}",
                    console::style(result.status().tag()).red().bold(),
                    test.name(),
                );
                if self.show_output {
                    println!("{result:#}");
                }
            });
        } else if self.show_non_error || self.show_all {
            self.progress.suspend(|| {
                println!(
                    "{}: {}",
                    console::style(result.status().tag()).green().bold(),
                    test.name()
                );
                if self.show_all {
                    println!("{result:#}");
                }
            });
        }
        self.report(test, result)
    }

    fn report(&mut self, test: Arc<Test>, result: TestResult) -> DiagResult<()> {
        let _ = self.sender.send(TestReport { test, result });

        Ok(())
    }
}

fn print_results(results_by_status: &BTreeMap<TestStatus, Vec<TestReport>>, config: &Config) {
    let show_non_error = !config.options.quiet;

    // Print tests by status (grouped)
    let prefix = "*".repeat(20);
    for (status, reports) in results_by_status.iter() {
        if !status.is_failure() && !show_non_error {
            continue;
        }
        let count = reports.len();
        println!("{prefix} {status} Tests ({count}):");
        for report in reports.iter() {
            println!("  {}", report.test.name());
        }
        println!();
    }

    // Print summary
    let num_tests = results_by_status
        .values()
        .map(|reports| reports.len())
        .sum::<usize>();
    println!("\nTotal Discovered Tests: {num_tests}");
    let count_by_status = results_by_status
        .iter()
        .map(|(k, v)| (k, v.len(), v.len().to_string()))
        .collect::<Vec<_>>();
    let max_label_len = count_by_status
        .iter()
        .map(|(k, _, _)| k.label().len())
        .max()
        .unwrap_or(0);
    let max_count_len = count_by_status
        .iter()
        .map(|(_, _, count_string)| count_string.len())
        .max()
        .unwrap_or(0);
    for (status, count, count_string) in count_by_status.into_iter() {
        let label = console::pad_str(
            status.label(),
            max_label_len,
            console::Alignment::Left,
            None,
        );
        let count_string = console::pad_str(
            &count_string,
            max_count_len,
            console::Alignment::Right,
            None,
        );
        let percentage = (f64::from(count as u32) / f64::from(num_tests as u32)) * 100.0;
        println!("  {label}: {count_string} ({percentage:.2}%)");
    }
}
