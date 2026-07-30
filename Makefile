.DEFAULT_GOAL := help

# -- help -----------------------------------------------------------------------------------------
.PHONY: help
help:
	@printf "\nTargets:\n\n"
	@awk 'BEGIN {FS = ":.*##"; OFS = ""} /^[a-zA-Z0-9_.-]+:.*?##/ { printf "  \033[36m%-24s\033[0m %s\n", $$1, $$2 }' $(MAKEFILE_LIST)
	@printf "\nTesting:\n"
	@printf "  make test                      # Run all tests\n"
	@printf "  make test-X                   # Test X crate\n"
	@printf "\nQuality:\n"
	@printf "  make format                   # Format code\n"
	@printf "  make format-check             # Check formatting\n"
	@printf "  make clippy                  # Run clippy linter\n"
	@printf "  make lint                    # Run all quality checks\n"
	@printf "\nDocumentation:\n"
	@printf "  make doc                      # Generate documentation\n"
	@printf "  make test-docs                # Test documentation\n"
	@printf "\nBuilding:\n"
	@printf "  make build                    # Build project\n"
	@printf "  make check                   # Check without building\n"
	@printf "\nCI:\n"
	@printf "  make ci                      # Run CI checks locally\n"
	@printf "\n"

# -- environment toggles --------------------------------------------------------------------------
BACKTRACE := RUST_BACKTRACE=1
WARNINGS  := RUSTDOCFLAGS="-D warnings"

# -- feature configuration ------------------------------------------------------------------------
WORKSPACE_FEATURES :=
DEFAULT_TEST_FEATURES :=

# -- linting --------------------------------------------------------------------------------------

.PHONY: clippy
clippy: ## Runs Clippy with configs
	cargo clippy --workspace --all-targets -- -D warnings

.PHONY: fix
fix: ## Runs Fix with configs
	cargo fix --allow-dirty --all-targets

.PHONY: format
format: ## Runs Format
	cargo +nightly fmt --all

.PHONY: format-check
format-check: ## Runs Format in check mode
	cargo +nightly fmt --all --check

.PHONY: lint
lint: format fix clippy ## Runs all linting tasks at once

# -- testing --------------------------------------------------------------------------------------

# Core knobs (overridable from CLI)
CRATE   ?=
FEATURES ?=
EXPR    ?=
EXTRA   ?=

define _CARGO_TEST
	$(BACKTRACE) cargo test \
		$(if $(FEATURES),--features $(FEATURES),) \
		$(if $(CRATE),-p $(CRATE),) \
		$(EXTRA) $(EXPR)
endef

.PHONY: core-test core-test-build
## Core: run tests with overridable parameters
core-test:
	$(_CARGO_TEST)

## Core: build test binaries only (no run)
core-test-build:
	$(BACKTRACE) cargo test --no-run \
		$(if $(FEATURES),--features $(FEATURES),) \
		$(if $(CRATE),-p $(CRATE),) \
		$(EXTRA) $(EXPR)

# Pattern rule for testing individual crates
.PHONY: test-%
test-%: ## Tests a specific crate; accepts 'test=' to pass a selector
	$(MAKE) core-test \
		CRATE=miden-$* \
		FEATURES=$(FEATURES_$*) \
		EXPR=$(if $(test),$(test),)

# Workspace-wide tests
.PHONY: test-build
test-build: ## Build test binaries for workspace
	$(MAKE) core-test-build

.PHONY: test
test: ## Run all tests for workspace
	$(MAKE) core-test

.PHONY: test-docs
test-docs: ## Run documentation tests
	cargo test --doc

# Filtered test runs
.PHONY: test-fast
test-fast: ## Runs fast tests (excludes slow tests)
	$(MAKE) core-test \
		EXPR="-- --skip slow_test"  # Adjust skip pattern as needed

# --- checking ------------------------------------------------------------------------------------

.PHONY: check
check: ## Checks all targets for errors
	cargo check --all-targets

# --- building ------------------------------------------------------------------------------------

.PHONY: build
build: ## Builds project with release profile
	cargo build --release

# --- documentation ------------------------------------------------------------------------------

.PHONY: doc
doc: ## Generates & checks documentation
	$(WARNINGS) cargo doc --workspace --keep-going --release

# --- CI ------------------------------------------------------------------------------------------

.PHONY: ci
ci: ## Run CI checks locally
	@echo "Running CI checks locally..."
	@echo "1. Checking formatting..."
	@cargo fmt --all -- --check
	@echo "2. Running clippy..."
	@cargo clippy --workspace --all-targets -- -D warnings
	@echo "3. Running tests..."
	@cargo nextest run --profile ci
	@echo "4. Checking MSRV..."
	@chmod +x scripts/check-msrv.sh && ./scripts/check-msrv.sh
	@echo "CI checks completed successfully!"

# Crate-specific features (adjust as needed)
FEATURES_core :=
FEATURES_filecheck :=
FEATURES_litcheck :=
FEATURES_lit :=
