//! Run the imported FileCheck fixtures through the binary built by Cargo.

#![cfg(unix)]

use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::Path,
    process::Command,
};

// These fixtures already declare XFAIL for unsupported diagnostic options.
// New expected failures require an explicit decision here as well as in the fixture.
const EXPECTED_FAILURES: &[&str] = &[
    "comment/unused-comment-prefixes.txt",
    "dump-input/annotations.txt",
    "dump-input/context.txt",
    "dump-input/enable.txt",
    "dump-input/filter.txt",
];

fn collect_fixtures(root: &Path, dir: &Path, fixtures: &mut BTreeSet<String>) {
    for entry in fs::read_dir(dir).expect("read fixture directory") {
        let path = entry.expect("read fixture entry").path();
        if path.is_dir() {
            if path.file_name().unwrap() != "Inputs" {
                collect_fixtures(root, &path, fixtures);
            }
        } else if path.extension().is_some_and(|extension| extension == "txt") {
            fixtures.insert(path.strip_prefix(root).unwrap().to_str().unwrap().into());
        }
    }
}

fn validate_results(fixtures: &BTreeSet<String>, stdout: &str) -> Result<(), String> {
    if fixtures.is_empty() {
        return Err("no FileCheck fixtures found".into());
    }
    let mut reported = BTreeMap::new();
    for line in stdout.lines() {
        let Some((status, name)) = line.split_once(": filecheck-llvm::") else {
            continue;
        };
        if !fixtures.contains(name) {
            return Err(format!("reported unknown fixture {name}"));
        }
        if reported.insert(name, status).is_some() {
            return Err(format!("reported fixture {name} more than once"));
        }
        let expected = if EXPECTED_FAILURES.contains(&name) {
            "XFAIL"
        } else {
            "PASS"
        };
        if status != expected {
            return Err(format!("{name}: expected {expected}, got {status}"));
        }
    }
    for name in fixtures {
        if !reported.contains_key(name.as_str()) {
            return Err(format!("fixture {name} was not executed"));
        }
    }
    Ok(())
}

#[test]
fn imported_llvm_filecheck_suite() {
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let root = workspace.join("tests/filecheck/llvm");
    let mut fixtures = BTreeSet::new();
    collect_fixtures(&root, &root, &mut fixtures);

    let output = Command::new(env!("CARGO_BIN_EXE_litcheck"))
        .current_dir(workspace)
        .args([
            "lit",
            "run",
            "--verbose",
            "--timeout",
            "5",
            "tests/filecheck/llvm",
        ])
        .env_remove("FILECHECK_OPTS")
        .env_remove("LITCHECK_TRACE")
        .env_remove("LITCHECK_TRACE_TIMING")
        .env_remove("LIT_FILTER")
        .env_remove("LIT_FILTER_OUT")
        .env("NO_COLOR", "1")
        .output()
        .expect("run imported FileCheck suite");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let validation = validate_results(&fixtures, &stdout);
    assert!(
        output.status.success() && validation.is_ok(),
        "FileCheck fixtures failed (exit {:?}): {validation:?}\nstdout:\n{stdout}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stderr),
    );
}

#[test]
fn suite_results_require_complete_successful_execution() {
    let fixtures = BTreeSet::from(["a.txt".into(), "b.txt".into()]);
    let pass = "PASS: filecheck-llvm::a.txt\nPASS: filecheck-llvm::b.txt\n";
    assert!(validate_results(&fixtures, pass).is_ok());
    for invalid in [
        "",
        "PASS: filecheck-llvm::a.txt\n",
        "PASS: filecheck-llvm::a.txt\nPASS: filecheck-llvm::a.txt\n",
        "PASS: filecheck-llvm::a.txt\nPASS: filecheck-llvm::unknown.txt\n",
        "PASS: filecheck-llvm::a.txt\nXFAIL: filecheck-llvm::b.txt\n",
        "PASS: filecheck-llvm::a.txt\nUNSUPPORTED: filecheck-llvm::b.txt\n",
        "PASS: filecheck-llvm::a.txt\nFAIL: filecheck-llvm::b.txt\n",
    ] {
        assert!(validate_results(&fixtures, invalid).is_err(), "{invalid}");
    }
    assert!(validate_results(&BTreeSet::new(), "").is_err());
}
