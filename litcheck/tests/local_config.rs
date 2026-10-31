use std::{
    path::Path,
    process::{Command, Output},
};

fn run_selections(selections: &[&str]) -> Output {
    let suite = Path::new(env!("CARGO_MANIFEST_DIR")).join("../tests/lit/local-config");
    Command::new(env!("CARGO_BIN_EXE_litcheck"))
        .current_dir(suite)
        .args(["lit", "run", "--show-all"])
        .args(selections)
        .output()
        .expect("failed to run litcheck")
}

fn output_text(output: &Output) -> String {
    format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

#[test]
fn locally_enabled_failures_fail_for_suite_directory_and_file_selections() {
    for selection in [".", "sub", "sub/a.txt"] {
        let output = run_selections(&[selection]);
        let text = output_text(&output);
        assert_eq!(output.status.code(), Some(1), "{selection}: {text}");
        assert!(text.contains("FAIL: local-config::sub/a.txt"), "{text}");
    }
}

#[test]
fn nested_local_configs_inherit_features_environment_and_substitutions() {
    for selection in [".", "sub/deep", "sub/deep/pass.txt"] {
        let output = run_selections(&[selection]);
        let text = output_text(&output);
        assert!(
            text.contains("PASS: local-config::sub/deep/pass.txt"),
            "{selection}: {text}"
        );
        if selection != "." {
            assert!(output.status.success(), "{text}");
        }
    }
}

#[test]
fn suite_root_local_config_applies_and_child_features_do_not_leak_to_siblings() {
    let output = run_selections(&["."]);
    let text = output_text(&output);
    assert!(text.contains("PASS: local-config::root.txt"), "{text}");
    assert!(
        text.contains("UNSUPPORTED: local-config::sibling/unsupported.txt"),
        "{text}"
    );
}

#[test]
fn local_patterns_and_exclusions_apply_for_every_selection_kind() {
    for selection in [".", "selection", "selection/chosen.case"] {
        let output = run_selections(&[selection]);
        let text = output_text(&output);
        assert!(
            text.contains("PASS: local-config::selection/chosen.case"),
            "{selection}: {text}"
        );
        assert!(
            !text.contains("local-config::selection/skip.case"),
            "{text}"
        );
        assert!(
            !text.contains("local-config::selection/hidden.txt"),
            "{text}"
        );
    }
    for selection in ["selection/skip.case", "selection/hidden.txt"] {
        let output = run_selections(&[selection]);
        let text = output_text(&output);
        assert!(!text.contains("FAIL:"), "{selection}: {text}");
    }
}

#[test]
fn local_test_formats_apply_for_every_selection_kind() {
    for selection in [".", "format", "format/only.txt"] {
        let output = run_selections(&[selection]);
        let text = output_text(&output);
        assert!(
            text.contains("UNSUPPORTED: local-config::format/only.txt"),
            "{selection}: {text}"
        );
    }
}

#[test]
fn explicit_selections_descend_beyond_an_already_cached_local_config() {
    let output = run_selections(&["sub/a.txt", "sub/deep/pass.txt"]);
    let text = output_text(&output);
    assert_eq!(output.status.code(), Some(1), "{text}");
    assert!(text.contains("FAIL: local-config::sub/a.txt"), "{text}");
    assert!(
        text.contains("PASS: local-config::sub/deep/pass.txt"),
        "{text}"
    );
}

#[test]
fn nested_suites_start_their_own_local_config_inheritance() {
    for selection in [".", "sub/nested", "sub/nested/own.txt"] {
        let output = run_selections(&[selection]);
        let text = output_text(&output);
        assert!(
            text.contains("PASS: local-nested::own.txt"),
            "{selection}: {text}"
        );
        if selection != "sub/nested/own.txt" {
            assert!(
                text.contains("UNSUPPORTED: local-nested::parent-feature.txt"),
                "{text}"
            );
        }
    }
}

#[test]
fn explicit_nested_suite_selections_respect_boundaries_after_parent_is_cached() {
    let output = run_selections(&["sub/a.txt", "sub/nested/own.txt"]);
    let text = output_text(&output);
    assert!(text.contains("FAIL: local-config::sub/a.txt"), "{text}");
    assert!(text.contains("PASS: local-nested::own.txt"), "{text}");
    assert!(!text.contains("local-config::sub/nested/own.txt"), "{text}");
}

#[test]
fn recursive_discovery_preserves_the_effective_config_supplied_by_its_caller() {
    use std::sync::Arc;

    use lit::{
        Config, DefaultTestSuiteRegistry, TestRegistry, TestSuite, test::DefaultTestRegistry,
    };

    let config = Config::new(vec![]);
    let source_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("../tests/lit/local-config");
    let suite = TestSuite::parse(source_dir.join("lit.suite.toml"), &config).unwrap();
    let mut effective = (*suite.config).clone();
    effective.available_features.insert("provided-by-caller");
    let tests = DefaultTestRegistry
        .find(
            Path::new("sub/deep"),
            suite,
            &config,
            Arc::new(effective),
            &mut DefaultTestSuiteRegistry::default(),
        )
        .unwrap();
    assert_eq!(tests.len(), 2);
    for test in tests.iter() {
        assert!(
            test.config
                .available_features
                .contains("provided-by-caller"),
            "{}: {:?}",
            test.name(),
            test.config.available_features.iter().collect::<Vec<_>>()
        );
    }
}
