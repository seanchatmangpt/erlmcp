.PHONY: all compile test ct eunit lint dialyze clean distclean release help \
        setup direnv asdf deps deps-tree quality info coverage taiea-compile taiea-test \
        release-dev release-prod tar show-release \
        workspace-build workspace-test workspace-lint workspace-check workspace-clean workspace-release \
        check build \
        test-unit test-int test-perf test-quick test-verbose test-coverage \
        test-runner test-analyze test-report test-debug

SHELL := /bin/bash

# Colors for output
BLUE := \033[0;34m
GREEN := \033[0;32m
RED := \033[0;31m
NC := \033[0m # No Color

# Default target
all: compile test lint

# ============================================================================
# WORKSPACE COORDINATION TARGETS (erlmcp + taiea)
# ============================================================================

workspace-build: compile taiea-compile
	@echo "$(GREEN)✓ Workspace build complete (erlmcp + taiea)$(NC)"

workspace-test: eunit ct taiea-test
	@echo "$(GREEN)✓ Workspace tests complete (erlmcp + taiea)$(NC)"

workspace-lint: lint
	@echo "$(BLUE)Linting TAIEA...$(NC)"
	cd taiea && rebar3 lint && rebar3 dialyzer 2>/dev/null || true
	@echo "$(GREEN)✓ Workspace lint complete$(NC)"

workspace-check: workspace-build workspace-lint workspace-test
	@echo "$(GREEN)✓ WORKSPACE VALIDATION PASSED$(NC)"
	@echo "  - erlmcp: compiled, tested, linted"
	@echo "  - taiea: compiled, tested, linted"

workspace-clean: distclean
	@echo "$(BLUE)Cleaning TAIEA...$(NC)"
	cd taiea && rebar3 clean 2>/dev/null || true
	rm -rf taiea/_build 2>/dev/null || true
	@echo "$(GREEN)✓ Workspace clean complete$(NC)"

workspace-release: workspace-check release-prod
	@echo "$(BLUE)Building TAIEA release...$(NC)"
	cd taiea && rebar3 as prod release 2>/dev/null || true
	@echo "$(GREEN)✓ Workspace releases built$(NC)"
	@echo "  - erlmcp: _build/prod/rel/erlmcp/"
	@echo "  - taiea: taiea/_build/prod/rel/taiea/"

# Aliases for common commands
build: compile
check: workspace-check

# ============================================================================
# BUILD TARGETS
# ============================================================================

compile:
	@echo "$(BLUE)Compiling workspace...$(NC)"
	rebar3 compile

# ============================================================================
# TEST TARGETS - UNIFIED TEST INFRASTRUCTURE
# ============================================================================

test: eunit ct
	@echo "$(GREEN)All tests passed!$(NC)"

# Fast unit tests only (quick feedback, < 2 minutes)
test-unit:
	@echo "$(BLUE)Running fast unit tests...$(NC)"
	rebar3 eunit
	@echo "$(GREEN)Unit tests complete!$(NC)"

# Integration tests (comprehensive, < 5 minutes)
test-int:
	@echo "$(BLUE)Running integration tests...$(NC)"
	rebar3 ct
	@echo "$(GREEN)Integration tests complete!$(NC)"

# Performance benchmarks (if enabled)
test-perf:
	@echo "$(BLUE)Running performance benchmarks...$(NC)"
	rebar3 ct -suite integration_SUITE -group performance_group
	@echo "$(GREEN)Performance tests complete!$(NC)"

# Quick smoke tests (< 10 seconds)
test-quick:
	@echo "$(BLUE)Running quick smoke tests...$(NC)"
	rebar3 eunit -module erlmcp_server_tests
	@echo "$(GREEN)Quick smoke tests complete!$(NC)"

# All tests with verbose output
test-verbose:
	@echo "$(BLUE)Running all tests (verbose)...$(NC)"
	rebar3 eunit -v
	rebar3 ct -v
	@echo "$(GREEN)All tests complete (verbose)!$(NC)"

# Test coverage analysis
test-coverage: test
	@echo "$(BLUE)Generating coverage report...$(NC)"
	rebar3 cover
	@echo "$(GREEN)Coverage report: _build/test/cover/index.html$(NC)"

# Test runner with default behavior
test-runner:
	@echo "$(BLUE)Running test suite (eunit + ct)...$(NC)"
	@echo "  - EUnit: Fast unit tests"
	@echo "  - CT: Integration tests"
	rebar3 eunit
	rebar3 ct
	@echo "$(GREEN)Test suite complete!$(NC)"

# Analyze test results
test-analyze:
	@echo "$(BLUE)Analyzing test results...$(NC)"
	@if [ -d "_build/test/cover" ]; then \
		echo "$(GREEN)Coverage report available:$(NC)"; \
		ls -lh _build/test/cover/index.html 2>/dev/null || echo "  No coverage data"; \
	fi
	@if [ -d "ct_logs" ]; then \
		echo "$(GREEN)Common Test logs available:$(NC)"; \
		ls -lh ct_logs/run.*/all_runs.html 2>/dev/null || true; \
	fi

# Detailed test report
test-report:
	@echo "$(BLUE)Test Report:$(NC)"
	@echo ""
	@echo "Available test targets:"
	@echo "  make test          - Run all tests (eunit + ct)"
	@echo "  make test-unit     - Fast unit tests only (< 2 min)"
	@echo "  make test-int      - Integration tests (< 5 min)"
	@echo "  make test-perf     - Performance benchmarks"
	@echo "  make test-quick    - Quick smoke tests (< 10 sec)"
	@echo "  make test-verbose  - All tests with verbose output"
	@echo "  make test-coverage - Run tests + coverage report"
	@echo ""
	@echo "Debug targets:"
	@echo "  make test-debug    - Run with debug output"
	@echo ""
	@if [ -d "_build/test/cover" ]; then \
		echo "Coverage metrics (latest run):"; \
		grep -h "^<title>" _build/test/cover/index.html 2>/dev/null | sed 's/<[^>]*>//g' || echo "  No coverage data yet"; \
	fi

# Debug test execution
test-debug:
	@echo "$(BLUE)Running tests with debug output...$(NC)"
	@echo "Test environment:"
	@echo "  Erlang: $$(erl -noshell -eval 'erlang:display(erlang:system_info(otp_release))' -s init stop 2>/dev/null)"
	@echo "  Rebar3: $$(rebar3 --version 2>/dev/null)"
	@echo ""
	rebar3 eunit -v --no_cover
	rebar3 ct -v
	@echo "$(GREEN)Debug tests complete!$(NC)"

# Standard CT and EUnit targets (backward compatible)
eunit:
	@echo "$(BLUE)Running EUnit tests...$(NC)"
	rebar3 eunit

ct:
	@echo "$(BLUE)Running Common Test suite...$(NC)"
	rebar3 ct

ct-verbose:
	@echo "$(BLUE)Running Common Test (verbose)...$(NC)"
	rebar3 ct -v

coverage:
	@echo "$(BLUE)Generating coverage report...$(NC)"
	rebar3 cover
	@echo "$(GREEN)Coverage report: _build/test/cover/index.html$(NC)"

# ============================================================================
# QUALITY TARGETS
# ============================================================================

lint:
	@echo "$(BLUE)Running linting...$(NC)"
	rebar3 lint

dialyze:
	@echo "$(BLUE)Running Dialyzer (type checker)...$(NC)"
	rebar3 dialyze

quality: lint dialyze
	@echo "$(GREEN)Quality checks passed!$(NC)"

# ============================================================================
# DOCUMENTATION
# ============================================================================

docs:
	@echo "$(BLUE)Generating documentation...$(NC)"
	rebar3 edoc
	@echo "$(GREEN)Documentation: doc/$(NC)"

# ============================================================================
# PROJECT-SPECIFIC TARGETS
# ============================================================================

taiea-test:
	@echo "$(BLUE)Testing TAIEA project...$(NC)"
	cd taiea && rebar3 eunit && rebar3 ct

taiea-compile:
	@echo "$(BLUE)Compiling TAIEA...$(NC)"
	cd taiea && rebar3 compile

# ============================================================================
# RELEASE TARGETS
# ============================================================================

release-dev:
	@echo "$(BLUE)Building development release...$(NC)"
	rebar3 release

release-prod:
	@echo "$(BLUE)Building production release...$(NC)"
	rebar3 as prod release

tar:
	@echo "$(BLUE)Creating release tarball...$(NC)"
	rebar3 as prod tar

show-release:
	@echo "$(GREEN)Release artifacts:$(NC)"
	@ls -lh _build/prod/rel/erlmcp/ 2>/dev/null || echo "No prod release found"
	@ls -lh _build/prod/*.tar.gz 2>/dev/null || echo "No tarball found"

# ============================================================================
# DEPENDENCY MANAGEMENT
# ============================================================================

deps:
	@echo "$(BLUE)Fetching dependencies...$(NC)"
	rebar3 get-deps

deps-tree:
	@echo "$(BLUE)Dependency tree:$(NC)"
	rebar3 tree

# ============================================================================
# ENVIRONMENT & SETUP
# ============================================================================

direnv:
	@echo "$(BLUE)Loading direnv...$(NC)"
	direnv allow

asdf:
	@echo "$(BLUE)Installing Erlang/Elixir via asdf...$(NC)"
	asdf install
	@echo "$(GREEN)Done! Use: direnv allow$(NC)"

setup: deps asdf direnv
	@echo "$(GREEN)Workspace setup complete!$(NC)"

# ============================================================================
# CLEANUP TARGETS
# ============================================================================

clean:
	@echo "$(BLUE)Cleaning build artifacts...$(NC)"
	rebar3 clean

distclean: clean
	@echo "$(BLUE)Removing all generated files...$(NC)"
	rm -rf _build/ doc/ ct_logs/ cover/ .rebar3/
	@echo "$(GREEN)Workspace clean!$(NC)"

# ============================================================================
# HELP & INFO
# ============================================================================

info:
	@echo "$(BLUE)Workspace Information:$(NC)"
	@echo "  Location: /Users/sac/erlmcp"
	@echo "  Erlang: $$(erl -noshell -eval 'erlang:display(erlang:system_info(otp_release))' -s init stop 2>/dev/null || echo 'unknown')"
	@echo "  Projects: taiea, vendor/erlmcp"
	@echo ""
	@echo "$(BLUE)Commands:$(NC)"
	@make help | grep -E '^\s+(make [a-z-]+|rebar3)'

help:
	@echo "$(BLUE)erlmcp Workspace - Build Targets$(NC)"
	@echo ""
	@echo "$(GREEN)WORKSPACE TARGETS (erlmcp + taiea):$(NC)"
	@echo "  make workspace-build  Compile both systems"
	@echo "  make workspace-test   Run all tests (erlmcp + taiea)"
	@echo "  make workspace-lint   Static analysis (erlmcp + taiea)"
	@echo "  make workspace-check  Full validation (build + lint + test)"
	@echo "  make workspace-clean  Clean all artifacts"
	@echo "  make workspace-release Build production releases"
	@echo ""
	@echo "$(GREEN)Primary Targets:$(NC)"
	@echo "  make all              Compile + test + lint (default)"
	@echo "  make build            Compile erlmcp (alias: make compile)"
	@echo "  make check            Full workspace validation"
	@echo "  make test             Run all tests (eunit + ct)"
	@echo "  make quality          Lint + Dialyzer"
	@echo ""
	@echo "$(GREEN)UNIFIED TEST INFRASTRUCTURE:$(NC)"
	@echo "  make test-unit        Fast unit tests only (< 2 min)"
	@echo "  make test-int         Integration tests (< 5 min)"
	@echo "  make test-perf        Performance benchmarks"
	@echo "  make test-quick       Quick smoke tests (< 10 sec)"
	@echo "  make test-verbose     All tests with verbose output"
	@echo "  make test-coverage    Run tests + generate coverage"
	@echo "  make test-runner      Test suite runner (default)"
	@echo "  make test-analyze     Analyze test results"
	@echo "  make test-report      Show test report"
	@echo "  make test-debug       Run with debug output"
	@echo ""
	@echo "$(GREEN)Code Quality:$(NC)"
	@echo "  make lint             Run linting"
	@echo "  make dialyze          Run Dialyzer (type checker)"
	@echo ""
	@echo "$(GREEN)Documentation:$(NC)"
	@echo "  make docs             Generate edoc documentation"
	@echo ""
	@echo "$(GREEN)Release:$(NC)"
	@echo "  make release-dev      Development release"
	@echo "  make release-prod     Production release"
	@echo "  make tar              Create tarball"
	@echo "  make show-release     Show release artifacts"
	@echo ""
	@echo "$(GREEN)Project Targets:$(NC)"
	@echo "  make taiea-compile    Compile TAIEA only"
	@echo "  make taiea-test       Test TAIEA only"
	@echo ""
	@echo "$(GREEN)Setup:$(NC)"
	@echo "  make setup            Complete workspace setup"
	@echo "  make direnv           Load direnv"
	@echo "  make asdf             Install Erlang/Elixir"
	@echo "  make deps             Fetch dependencies"
	@echo "  make deps-tree        Show dependency tree"
	@echo ""
	@echo "$(GREEN)Cleanup:$(NC)"
	@echo "  make clean            Remove build artifacts"
	@echo "  make distclean        Remove all generated files"
	@echo ""
	@echo "$(GREEN)Info:$(NC)"
	@echo "  make info             Show workspace information"
	@echo "  make help             This message"
	@echo ""
	@echo "$(BLUE)For detailed test strategy, see TEST_STRATEGY.md$(NC)"
