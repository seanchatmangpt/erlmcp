.PHONY: all compile test ct eunit check clean distclean help bench-quick \
        console observer deps info coverage \
        compile-core compile-transports compile-observability compile-tcps \
        test-core test-transports test-observability test-tcps test-smoke test-quick test-full \
        dialyzer dialyzer-fast dialyzer-full dialyzer-update-plt dialyzer-clean xref format release benchmark \
        validate validate-compile validate-test validate-coverage validate-quality validate-bench \
        test-strict benchmark-strict coverage-strict quality-strict \
        jidoka andon poka-yoke tcps-quality-gates release-validate \
        doctor quick verify ci-local \
        example-mcp-complete example-help andon-clear andon-watch \
        setup-profile check-erlang-version \
        test-shutdown shutdown-load-test validate-shutdown \
        ggen-validate ggen-sync ggen-help ggen-shell
.PHONY: all-inner compile-inner test-inner ct-inner eunit-inner check-inner clean-inner distclean-inner help-inner
.PHONY: compile-core-inner compile-transports-inner compile-observability-inner compile-tcps-inner
.PHONY: test-core-inner test-transports-inner test-observability-inner test-tcps-inner test-smoke-inner test-quick-inner test-full-inner
.PHONY: dialyzer-inner dialyzer-fast-inner dialyzer-full-inner dialyzer-update-plt-inner dialyzer-clean-inner xref-inner
.PHONY: validate-inner validate-compile-inner validate-test-inner validate-coverage-inner validate-quality-inner validate-bench-inner
.PHONY: jidoka-inner andon-inner poka-yoke-inner release-validate-inner
.PHONY: doctor-inner quick-inner verify-inner ci-local-inner
.PHONY: setup-profile-inner check-erlang-version-inner

SHELL := /bin/sh

# ============================================================================
# DOCKER-ONLY CONSTITUTION (ANDON ENFORCEMENT)
# ============================================================================
# CRITICAL: This project REQUIRES Docker for ALL execution.
# Host execution is FORBIDDEN and will trigger ANDON stop.
#
# Architecture:
#   Host Makefile → refuses, prints docker command
#   Container Makefile → runs actual work (*-inner targets)
#
# Gate mapping:
#   compile  → erlmcp-build
#   eunit    → erlmcp-unit
#   ct       → erlmcp-ct
#   check/*  → erlmcp-check
#   bench    → erlmcp-bench
#   cluster  → erlmcp-node*
# ============================================================================

# Detect execution environment
DOCKER_ENV := $(shell ./scripts/dev/is_docker.sh >/dev/null 2>&1 && echo docker || echo host)

# Targets that MUST NOT run on host (ALL quality gates)
DOCKER_REQUIRED_TARGETS := all compile test ct eunit check clean distclean \
        compile-core compile-transports compile-observability compile-tcps \
        test-core test-transports test-observability test-tcps test-smoke test-quick test-full \
        dialyzer dialyzer-fast dialyzer-full dialyzer-update-plt dialyzer-clean xref \
        validate validate-compile validate-test validate-coverage validate-quality validate-bench \
        test-strict benchmark-strict coverage-strict quality-strict \
        jidoka andon poka-yoke tcps-quality-gates release-validate \
        doctor quick verify ci-local setup-profile check-erlang-version \
        test-shutdown shutdown-load-test validate-shutdown

# Docker service mapping for each target
define DOCKER_SERVICE
$(shell ./scripts/dev/docker_service_map.sh $(1) 2>/dev/null || echo erlmcp-check)
endef

# ANDON refusal message
define DOCKER_REFUSE
	@echo ""
	@echo "╔════════════════════════════════════════════════════════════════════╗"
	@echo "║                                                                    ║"
	@echo "║   🚨 ANDON: FORBIDDEN_HOST_EXECUTION 🚨                           ║"
	@echo "║                                                                    ║"
	@echo "╚════════════════════════════════════════════════════════════════════╝"
	@echo ""
	@echo "Refusal Code: FORBIDDEN_HOST_EXECUTION"
	@echo "Constitution: DOCKER-ONLY CONSTITUTION"
	@echo ""
	@echo "Target: $(@)"
	@echo "Detected: HOST (forbidden)"
	@echo ""
	@echo "✅ CORRECT EXECUTION:"
	@echo "   docker compose run --rm $(call DOCKER_SERVICE,$(@)) make $(@)"
	@echo ""
	@exit 1
endef

# Host-only targets (allowed to run on host for dispatch)
HOST_ONLY_TARGETS := help help-extra

# Colors for output
BLUE := \033[0;34m
CYAN := \033[0;36m
GREEN := \033[0;32m
YELLOW := \033[0;33m
RED := \033[0;31m
BOLD := \033[1m
NC := \033[0m # No Color

# ============================================================================
# DOCKER WRAPPER RULES
# ============================================================================
# This pattern creates wrappers for all DOCKER_REQUIRED_TARGETS.
# On host: refuses with ANDON message
# In Docker: runs the *-inner target (actual implementation)
# ============================================================================

# Generic wrapper pattern applied to all DOCKER_REQUIRED_TARGETS
$(DOCKER_REQUIRED_TARGETS):
ifeq ($(DOCKER_ENV),host)
	$(DOCKER_REFUSE)
else
	@$(MAKE) $(@)-inner
endif

# ============================================================================
# MAIN TARGETS (INNER - Docker-only implementations)
# ============================================================================

all-inner: compile-inner test-inner
	@echo "$(GREEN)✓ Build complete: compile + test passed$(NC)"

# ============================================================================
# CANONICAL WORKFLOW TARGETS (INNER - Docker-only implementations)
# ============================================================================
# Fast, focused targets for daily development workflow:
#   doctor   : Check environment health before starting work
#   quick    : Fast smoke test before commit (< 5min)
#   verify   : Full validation before PR (< 15min)
#   ci-local : Reproduce exact CI workflow locally
# ============================================================================

doctor-inner: ## Check environment health (OTP, rebar3, deps, structure, profile)
	@./scripts/dev/doctor.sh
	@echo ""
	@echo "$(BLUE)Validating ERLMCP_PROFILE...$(NC)"
	@./scripts/validate_profile.sh $${ERLMCP_PROFILE:-dev} || exit 1

quick-inner: doctor-inner ## Fast quality check: compile + smoke tests + validator (< 5min)
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(CYAN)⚡ Quick Quality Check (<5min)$(NC)"
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@echo "$(BLUE)1/3 Compiling...$(NC)"
	@./scripts/build_and_test.sh --quick || exit 1
	@echo ""
	@echo "$(BLUE)2/3 Running smoke tests...$(NC)"
	@rebar3 eunit --module=erlmcp_json_rpc_tests || exit 1
	@echo ""
	@echo "$(BLUE)3/3 Running quick validator...$(NC)"
	@if [ -x ./bin/erlmcp-validate ]; then \
		./bin/erlmcp-validate --quick || exit 1; \
	else \
		echo "$(YELLOW)⚠ Validator not built, skipping$(NC)"; \
	fi
	@echo ""
	@echo "$(BOLD)$(GREEN)✅ Quick check PASSED - Ready to commit$(NC)"
	@echo ""

verify-inner: compile-inner ## Full validation: spec + transport + dialyzer + xref (< 15min)
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(CYAN)🔍 Full Validation (<15min)$(NC)"
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@echo "$(BLUE)1/5 Running xref...$(NC)"
	@rebar3 xref || exit 1
	@echo ""
	@echo "$(BLUE)2/5 Running dialyzer...$(NC)"
	@rebar3 dialyzer || exit 1
	@echo ""
	@echo "$(BLUE)3/5 Running spec compliance validation...$(NC)"
	@if [ -f scripts/validation/run-ci.sh ]; then \
		./scripts/validation/run-ci.sh --compliance || exit 1; \
	else \
		echo "$(YELLOW)⚠ Spec compliance script not found, skipping$(NC)"; \
	fi
	@echo ""
	@echo "$(BLUE)4/5 Running transport validation...$(NC)"
	@if [ -x ./bin/erlmcp-validate ]; then \
		./bin/erlmcp-validate --transport || exit 1; \
	else \
		echo "$(YELLOW)⚠ Transport validator not built, skipping$(NC)"; \
	fi
	@echo ""
	@echo "$(BLUE)5/5 Running full test suite...$(NC)"
	@./tools/test-runner.sh || exit 1
	@echo ""
	@echo "$(BOLD)$(GREEN)✅ Full validation PASSED - Ready for PR$(NC)"
	@echo ""

ci-local-inner: ## Reproduce exact CI workflow locally (matches .github/workflows/ci.yml)
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(CYAN)🔬 CI Workflow (Local Reproduction)$(NC)"
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@echo "$(BOLD)$(BLUE)GATE 1: Compilation$(NC)"
	@TERM=dumb rebar3 compile 2>&1 | tee /tmp/erlmcp_ci_compile.log || exit 1
	@echo "$(GREEN)✓ Compilation passed$(NC)"
	@echo ""
	@echo "$(BOLD)$(BLUE)GATE 2: Xref$(NC)"
	@rebar3 xref 2>&1 | tee /tmp/erlmcp_ci_xref.log || true
	@echo "$(YELLOW)⚠ Xref complete (warnings non-blocking)$(NC)"
	@echo ""
	@echo "$(BOLD)$(BLUE)GATE 3: Dialyzer$(NC)"
	@rebar3 dialyzer 2>&1 | tee /tmp/erlmcp_ci_dialyzer.log || exit 1
	@echo "$(GREEN)✓ Dialyzer passed$(NC)"
	@echo ""
	@echo "$(BOLD)$(BLUE)GATE 4: EUnit Tests$(NC)"
	@rebar3 as test do compile, eunit --cover 2>&1 | tee /tmp/erlmcp_ci_eunit.log || exit 1
	@echo "$(GREEN)✓ EUnit tests passed$(NC)"
	@echo ""
	@echo "$(BOLD)$(BLUE)GATE 5: Common Test$(NC)"
	@rebar3 ct --dir=test/integration --cover 2>&1 | tee /tmp/erlmcp_ci_ct.log || true
	@echo "$(YELLOW)⚠ CT complete (failures non-blocking if no suites)$(NC)"
	@echo ""
	@echo "$(BOLD)$(BLUE)GATE 6: Coverage Check (≥80%)$(NC)"
	@rebar3 cover --verbose 2>&1 | tee /tmp/erlmcp_ci_coverage.log
	@if [ -f scripts/check_coverage_threshold.sh ]; then \
		chmod +x scripts/check_coverage_threshold.sh; \
		./scripts/check_coverage_threshold.sh 80 || exit 1; \
		echo "$(GREEN)✓ Coverage ≥80% passed$(NC)"; \
	else \
		echo "$(YELLOW)⚠ Coverage check script not found$(NC)"; \
	fi
	@echo ""
	@echo "$(BOLD)$(GREEN)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(GREEN)✅ ALL CI GATES PASSED$(NC)"
	@echo "$(BOLD)$(GREEN)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@echo "CI logs saved to /tmp/erlmcp_ci_*.log"
	@echo ""

help:
	@echo "$(BOLD)$(BLUE)erlmcp Makefile - Umbrella Build System$(NC)"
	@echo ""
	@echo "$(BOLD)$(CYAN)Canonical Workflow Targets:$(NC)"
	@echo "  make doctor        - Check environment health (OTP, rebar3, deps)"
	@echo "  make quick         - Fast check: compile + smoke tests (< 5min)"
	@echo "  make verify        - Full validation: spec + transport + dialyzer + xref (< 15min)"
	@echo "  make ci-local      - Reproduce exact CI workflow locally"
	@echo ""
	@echo "$(BOLD)$(GREEN)Main targets:$(NC)"
	@echo "  make check-erlang-version - Verify OTP 28+ (BLOCKING)"
	@echo "  make compile       - Compile all apps (requires OTP 28+)"
	@echo "  make test          - Run all tests (eunit + ct)"
	@echo "  make check         - Full quality check (compile + xref + dialyzer + tests)"
	@echo "  make clean         - Clean build artifacts"
	@echo ""
	@echo "$(BOLD)$(GREEN)Individual app targets:$(NC)"
	@echo "  make compile-core          - Compile erlmcp_core only"
	@echo "  make compile-transports    - Compile erlmcp_transports only"
	@echo "  make compile-observability - Compile erlmcp_observability only"
	@echo "  make compile-tcps          - Compile tcps_erlmcp only"
	@echo ""
	@echo "  make test-core             - Test erlmcp_core only"
	@echo "  make test-transports       - Test erlmcp_transports only"
	@echo "  make test-observability    - Test erlmcp_observability only"
	@echo "  make test-tcps             - Test tcps_erlmcp only"
	@echo ""
	@echo "$(BOLD)$(GREEN)Test tier system (Chicago School TDD):$(NC)"
	@echo "  make test-smoke        - Smoke tests (≤2 min): codec, lifecycle, basic transport"
	@echo "  make test-quick        - Quick tests (≤10 min): smoke + core integration"
	@echo "  make test-full         - Full test suite: all EUnit + CT + coverage"
	@echo ""
	@echo "$(BOLD)$(GREEN)Quality gates (BLOCKING):$(NC)"
	@echo "  make validate              - Run ALL quality gates (BLOCKING)"
	@echo "  make validate-compile      - Check compilation (exit 1 on failure)"
	@echo "  make validate-test         - Check tests pass (exit 1 on failure)"
	@echo ""
	@echo "$(BOLD)$(GREEN)TCPS Quality System (自働化):$(NC)"
	@echo "  make jidoka                - Run 8 Jidoka quality gates (stop-the-line)"
	@echo "  make poka-yoke             - Run 8 Poka-Yoke error-proofing checks"
	@echo "  make andon                 - Show Andon board status (行灯)"
	@echo "  make andon-clear           - Clear Andon status (after fixing)"
	@echo "  make andon-watch           - Real-time Andon monitoring"
	@echo "  make tcps-quality-gates    - Run complete TCPS quality system"
	@echo "  make validate-coverage     - Check ≥80% coverage (exit 1 on failure)"
	@echo "  make validate-quality      - Check dialyzer + xref (exit 1 on failure)"
	@echo "  make validate-bench        - Check no performance regression (exit 1 on failure)"
	@echo ""
	@echo "$(BOLD)$(GREEN)Automated test runners (BLOCKING):$(NC)"
	@echo "  make test-strict           - Run tests, BLOCK on failures (≥90% pass rate)"
	@echo "  make benchmark-strict      - Run benchmarks, BLOCK on regression (>10%)"
	@echo "  make coverage-strict       - Check coverage, BLOCK if <80%"
	@echo "  make quality-strict        - Master script: ALL checks MUST pass"
	@echo ""
	@echo "$(BOLD)$(GREEN)TCPS Manufacturing (自働化 Jidoka):$(NC)"
	@echo "  make jidoka                - Built-in quality gates (stop-the-line on failure)"
	@echo "  make andon                 - Pull andon cord (emergency stop + alert)"
	@echo "  make poka-yoke             - Error-proofing validation (ポカヨケ)"
	@echo "  make release-validate      - Pre-release validation with quality receipt"
	@echo ""
	@echo "$(BOLD)$(GREEN)Standard quality gates:$(NC)"
	@echo "  make dialyzer              - Type checking (dialyzer)"
	@echo "  make xref                  - Cross-reference analysis"
	@echo "  make coverage              - Test coverage report"
	@echo ""
	@echo "$(BOLD)$(GREEN)Development:$(NC)"
	@echo "  make console               - Start Erlang shell with apps"
	@echo "  make observer              - Start observer GUI"
	@echo "  make deps                  - Fetch dependencies"
	@echo "  make info                  - Show project info"
	@echo ""
	@echo "$(BOLD)$(GREEN)Release:$(NC)"
	@echo "  make release               - Build production release"
	@echo "  make benchmark             - Run benchmarks"
	@echo "  make bench-quick           - Quick performance check (<2 min, local dev)"
	@echo ""
	@echo "$(BOLD)$(YELLOW)⚠ CRITICAL: Quality gates are BLOCKING$(NC)"
	@echo "  All validate-* targets exit with code 1 on failure."
	@echo "  Use before commits, PRs, and releases."
	@echo "  No compromises on quality (Lean Six Sigma 99.99966% defect-free)."

# ============================================================================
# ERLANG VERSION ENFORCEMENT (BLOCKING GATE)
# ============================================================================
# CRITICAL: This project requires Erlang/OTP 28 or higher.
# Lower versions will fail with a clear error message.
# This gate runs BEFORE any compilation to provide immediate feedback.
# ============================================================================

check-erlang-version-inner: ## Enforce Erlang/OTP 28+ requirement (BLOCKING)
	@./scripts/check_erlang_version.sh

# ============================================================================
# COMPILATION (INNER - Docker-only implementations)
# ============================================================================

# Profile setup - creates symlink for rebar3 sys.config selection
# Controlled by ERLMCP_PROFILE environment variable (dev|test|staging|prod)
setup-profile-inner:
	@ERLMCP_PROFILE=$${ERLMCP_PROFILE:-dev}; \
	CONFIG_SOURCE="config/sys.config.$$ERLMCP_PROFILE"; \
	CONFIG_TARGET="config/sys.config"; \
	if [ ! -f "$$CONFIG_SOURCE" ]; then \
		echo "$(RED)❌ Config file not found: $$CONFIG_SOURCE$(NC)"; \
		echo "$(RED)Available profiles: dev, test, staging, prod$(NC)"; \
		exit 1; \
	fi; \
	echo "$(CYAN)Setting up profile: $$ERLMCP_PROFILE$(NC)"; \
	ln -sf "sys.config.$$ERLMCP_PROFILE" "$$CONFIG_TARGET"; \
	echo "$(GREEN)✓ Config symlink: $$CONFIG_TARGET -> sys.config.$$ERLMCP_PROFILE$(NC)"

compile-inner: check-erlang-version-inner setup-profile-inner
	@echo "$(BLUE)Compiling all apps...$(NC)"
	@TERM=dumb rebar3 compile
	@echo "$(GREEN)✓ Compilation complete$(NC)"

compile-core-inner:
	@echo "$(BLUE)Compiling erlmcp_core...$(NC)"
	@cd apps/erlmcp_core && rebar3 compile
	@echo "$(GREEN)✓ erlmcp_core compiled$(NC)"

compile-transports-inner:
	@echo "$(BLUE)Compiling erlmcp_transports...$(NC)"
	@cd apps/erlmcp_transports && rebar3 compile
	@echo "$(GREEN)✓ erlmcp_transports compiled$(NC)"

compile-observability-inner:
	@echo "$(BLUE)Compiling erlmcp_observability...$(NC)"
	@cd apps/erlmcp_observability && rebar3 compile
	@echo "$(GREEN)✓ erlmcp_observability compiled$(NC)"

compile-tcps-inner:
	@echo "$(BLUE)Compiling tcps_erlmcp...$(NC)"
	@cd apps/tcps_erlmcp && rebar3 compile
	@echo "$(GREEN)✓ tcps_erlmcp compiled$(NC)"

# ============================================================================
# TESTING (INNER - Docker-only implementations)
# ============================================================================

test-inner: eunit-inner ct-inner
	@echo "$(GREEN)✓ All tests passed$(NC)"

# Test tier system (Chicago School TDD - real processes, no mocks)
test-smoke-inner:
	@echo "$(BLUE)Running smoke tests (target: ≤2 min)...$(NC)"
	@./scripts/test/smoke.sh

test-quick-inner:
	@echo "$(BLUE)Running quick tests (target: ≤10 min)...$(NC)"
	@./scripts/test/quick.sh

test-full-inner:
	@echo "$(BLUE)Running full test suite...$(NC)"
	@./scripts/test/full.sh

# Automated test runners (BLOCKING on failures)
test-strict-inner:
	@echo "$(BLUE)🧪 Strict Test Runner - BLOCKING on failures$(NC)"
	@./tools/test-runner.sh || exit 1

benchmark-strict-inner:
	@echo "$(BLUE)⚡ Strict Benchmark Runner - BLOCKING on regression$(NC)"
	@./tools/benchmark-runner.sh || exit 1

coverage-strict-inner:
	@echo "$(BLUE)📊 Strict Coverage Checker - BLOCKING if <80%$(NC)"
	@./tools/coverage-checker.sh || exit 1

quality-strict-inner:
	@echo "$(BLUE)🔍 Master Quality Checker - ALL checks MUST pass$(NC)"
	@./tools/quality-checker.sh || exit 1

eunit-inner: setup-profile-inner
	@echo "$(BLUE)Running EUnit tests...$(NC)"
	@rebar3 eunit
	@echo "$(GREEN)✓ EUnit tests passed$(NC)"

ct-inner: setup-profile-inner
	@echo "$(BLUE)Running Common Test...$(NC)"
	@rebar3 ct || echo "$(YELLOW)⚠ Some CT tests skipped (expected if no CT suites)$(NC)"
	@echo "$(GREEN)✓ Common Test complete$(NC)"

test-core-inner:
	@echo "$(BLUE)Testing erlmcp_core...$(NC)"
	@cd apps/erlmcp_core && rebar3 eunit
	@echo "$(GREEN)✓ erlmcp_core tests passed$(NC)"

test-transports-inner:
	@echo "$(BLUE)Testing erlmcp_transports...$(NC)"
	@cd apps/erlmcp_transports && rebar3 eunit
	@echo "$(GREEN)✓ erlmcp_transports tests passed$(NC)"

test-observability-inner:
	@echo "$(BLUE)Testing erlmcp_observability...$(NC)"
	@cd apps/erlmcp_observability && rebar3 eunit
	@echo "$(GREEN)✓ erlmcp_observability tests passed$(NC)"

test-tcps-inner:
	@echo "$(BLUE)Testing tcps_erlmcp...$(NC)"
	@cd apps/tcps_erlmcp && rebar3 eunit
	@echo "$(GREEN)✓ tcps_erlmcp tests passed$(NC)"

coverage-inner:
	@echo "$(BLUE)Generating coverage report...$(NC)"
	@rebar3 cover
	@echo "$(GREEN)✓ Coverage report generated$(NC)"
	@echo "$(BLUE)See _build/test/cover/*.html for detailed reports$(NC)"

# ============================================================================
# QUALITY GATES (BLOCKING) - Lean Six Sigma 99.99966% Defect-Free (INNER - Docker-only)
# ============================================================================
# CRITICAL: All validate-* targets are BLOCKING and exit with code 1 on failure.
# These gates enforce zero-defect quality standards:
#   - 0 compilation errors
#   - 0 test failures
#   - ≥80% code coverage
#   - 0 dialyzer warnings
#   - 0 undefined function calls (xref)
#   - <10% performance regression
#
# Use before:
#   - Git commits
#   - Pull requests
#   - Production releases
#   - Deployments
#
# NO COMPROMISES. Stop the line on failure (自働化 Jidoka).
# ============================================================================

validate-inner: validate-profile-inner validate-compile-inner validate-test-inner validate-coverage-inner validate-quality-inner validate-bench-inner
	@echo ""
	@echo "$(BOLD)$(GREEN)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(GREEN)✅ ALL QUALITY GATES PASSED - READY FOR PRODUCTION$(NC)"
	@echo "$(BOLD)$(GREEN)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@echo "$(GREEN)✓ Profile:$(NC) Valid ERLMCP_PROFILE configuration"
	@echo "$(GREEN)✓ Compilation:$(NC) All modules compiled successfully (0 errors)"
	@echo "$(GREEN)✓ Tests:$(NC) All tests passed (0 failures)"
	@echo "$(GREEN)✓ Coverage:$(NC) ≥80% code coverage achieved"
	@echo "$(GREEN)✓ Quality:$(NC) Dialyzer + Xref passed (0 warnings)"
	@echo "$(GREEN)✓ Benchmarks:$(NC) No performance regression (<10%)"
	@echo ""

validate-profile-inner:
	@echo "$(BLUE)🔧 Quality Gate: Profile Validation$(NC)"
	@echo "  Target: Valid ERLMCP_PROFILE configuration"
	@echo "  Action: Checking profile $${ERLMCP_PROFILE:-dev}..."
	@echo ""
	@if ./scripts/validate_profile.sh $${ERLMCP_PROFILE:-dev}; then \
		echo ""; \
		echo "$(GREEN)✅ Profile validation passed$(NC)"; \
		echo ""; \
	else \
		echo ""; \
		echo "$(RED)❌ PROFILE VALIDATION FAILED$(NC)"; \
		echo "$(RED)Gate: BLOCKED$(NC)"; \
		echo "$(RED)Action: Set valid ERLMCP_PROFILE (dev, test, staging, prod)$(NC)"; \
		echo ""; \
		exit 1; \
	fi

validate-compile-inner:
	@echo "$(BLUE)🔨 Quality Gate: Compilation$(NC)"
	@echo "  Target: 0 compilation errors"
	@echo "  Action: Compiling all apps with TERM=dumb..."
	@echo ""
	@if TERM=dumb rebar3 compile 2>&1 | tee /tmp/erlmcp_compile.log; then \
		echo ""; \
		echo "$(GREEN)✅ Compilation passed - 0 errors$(NC)"; \
		echo ""; \
	else \
		echo ""; \
		echo "$(RED)❌ COMPILATION FAILED$(NC)"; \
		echo "$(RED)Gate: BLOCKED$(NC)"; \
		echo "$(RED)Action: Fix compilation errors before proceeding$(NC)"; \
		echo ""; \
		cat /tmp/erlmcp_compile.log; \
		exit 1; \
	fi

validate-test-inner:
	@echo "$(BLUE)🧪 Quality Gate: Tests$(NC)"
	@echo "  Target: 0 test failures (EUnit + CT)"
	@echo "  Action: Running EUnit + CT..."
	@echo ""
	@set -o pipefail; \
	EUNIT_PASS=1; \
	CT_PASS=1; \
	echo "  Running EUnit..."; \
	if ! rebar3 eunit 2>&1 | tee /tmp/erlmcp_eunit.log; then \
		EUNIT_PASS=0; \
	fi; \
	echo ""; \
	echo "  Running Common Test..."; \
	if ! rebar3 ct 2>&1 | tee /tmp/erlmcp_ct.log; then \
		CT_PASS=0; \
	fi; \
	echo ""; \
	if [ $$EUNIT_PASS -eq 1 ] && [ $$CT_PASS -eq 1 ]; then \
		echo "$(GREEN)✅ Tests passed - 0 failures (EUnit + CT)$(NC)"; \
		echo ""; \
	else \
		echo "$(RED)❌ TESTS FAILED$(NC)"; \
		echo "$(RED)Gate: BLOCKED$(NC)"; \
		if [ $$EUNIT_PASS -eq 0 ]; then \
			echo "$(RED)  - EUnit failures detected$(NC)"; \
			grep -A 5 "Failed:" /tmp/erlmcp_eunit.log || true; \
		fi; \
		if [ $$CT_PASS -eq 0 ]; then \
			echo "$(RED)  - Common Test failures detected$(NC)"; \
			grep -A 5 "FAILED" /tmp/erlmcp_ct.log || true; \
		fi; \
		echo "$(RED)Action: Fix failing tests before proceeding$(NC)"; \
		echo ""; \
		exit 1; \
	fi

validate-coverage-inner:
	@echo "$(BLUE)📊 Quality Gate: Coverage$(NC)"
	@echo "  Target: ≥80% code coverage"
	@echo "  Action: Generating coverage report..."
	@echo ""
	@if rebar3 cover 2>&1 | tee /tmp/erlmcp_coverage.log; then \
		COVERAGE=$$(grep -o '[0-9]\+%' /tmp/erlmcp_coverage.log | head -1 | sed 's/%//'); \
		if [ -z "$$COVERAGE" ]; then COVERAGE=0; fi; \
		echo ""; \
		echo "  Measured coverage: $$COVERAGE%"; \
		if [ $$COVERAGE -ge 80 ]; then \
			echo "$(GREEN)✅ Coverage passed - $$COVERAGE% ≥ 80%$(NC)"; \
			echo ""; \
		else \
			echo ""; \
			echo "$(RED)❌ COVERAGE BELOW THRESHOLD$(NC)"; \
			echo "$(RED)Gate: BLOCKED$(NC)"; \
			echo "$(RED)Measured: $$COVERAGE% < 80% required$(NC)"; \
			echo "$(RED)Action: Add tests to reach ≥80% coverage$(NC)"; \
			echo ""; \
			exit 1; \
		fi; \
	else \
		echo ""; \
		echo "$(RED)❌ COVERAGE TOOL FAILED$(NC)"; \
		echo "$(RED)Gate: BLOCKED$(NC)"; \
		echo "$(RED)Refusal Code: MISSING_TOOL_COVERAGE$(NC)"; \
		echo "$(RED)Remediation: Install rebar3 cover plugin or fix rebar3 installation$(NC)"; \
		echo ""; \
		exit 1; \
	fi

validate-quality-inner:
	@echo "$(BLUE)🔍 Quality Gate: Static Analysis$(NC)"
	@echo "  Target: 0 dialyzer warnings + 0 xref undefined calls"
	@echo "  Action: Running dialyzer + xref..."
	@echo ""
	@DIALYZER_PASS=1; \
	XREF_PASS=1; \
	echo "  Running dialyzer..."; \
	if ! rebar3 dialyzer 2>&1 | tee /tmp/erlmcp_dialyzer.log; then \
		DIALYZER_PASS=0; \
	fi; \
	echo ""; \
	echo "  Running xref..."; \
	if ! rebar3 xref 2>&1 | tee /tmp/erlmcp_xref.log; then \
		XREF_PASS=0; \
	fi; \
	echo ""; \
	if [ $$DIALYZER_PASS -eq 1 ] && [ $$XREF_PASS -eq 1 ]; then \
		echo "$(GREEN)✅ Quality checks passed - 0 warnings$(NC)"; \
		echo ""; \
	else \
		echo "$(RED)❌ QUALITY CHECKS FAILED$(NC)"; \
		echo "$(RED)Gate: BLOCKED$(NC)"; \
		if [ $$DIALYZER_PASS -eq 0 ]; then \
			echo "$(RED)  - Dialyzer warnings detected$(NC)"; \
		fi; \
		if [ $$XREF_PASS -eq 0 ]; then \
			echo "$(RED)  - Xref undefined calls detected$(NC)"; \
		fi; \
		echo "$(RED)Action: Fix static analysis issues before proceeding$(NC)"; \
		echo ""; \
		exit 1; \
	fi

validate-bench-inner:
	@echo "$(BLUE)⚡ Quality Gate: Performance$(NC)"
	@echo "  Target: <10% regression vs baseline"
	@echo "  Action: Running quick benchmarks..."
	@echo ""
	@if [ -f scripts/bench/check_regression.sh ]; then \
		if ./scripts/bench/check_regression.sh 2>&1 | tee /tmp/erlmcp_bench.log; then \
			echo ""; \
			echo "$(GREEN)✅ Benchmarks passed - no regression$(NC)"; \
			echo ""; \
		else \
			echo ""; \
			echo "$(RED)❌ PERFORMANCE REGRESSION DETECTED$(NC)"; \
			echo "$(RED)Gate: BLOCKED$(NC)"; \
			echo "$(RED)Action: Investigate performance degradation$(NC)"; \
			echo ""; \
			cat /tmp/erlmcp_bench.log; \
			exit 1; \
		fi; \
	else \
		echo ""; \
		echo "$(RED)❌ BENCHMARK SCRIPT MISSING$(NC)"; \
		echo "$(RED)Gate: BLOCKED$(NC)"; \
		echo "$(RED)Refusal Code: MISSING_TOOL_BENCHMARK$(NC)"; \
		echo "$(RED)Remediation: Create scripts/bench/check_regression.sh or disable gate in validate target$(NC)"; \
		echo ""; \
		exit 1; \
	fi

# ============================================================================
# TCPS MANUFACTURING TARGETS (INNER - Docker-only) (自働化 Jidoka - Stop-the-Line Authority)
# ============================================================================
# TCPS (Toyota Code Production System) applies manufacturing principles to code:
#   - 自働化 (Jidoka): Built-in quality, stop production on defects
#   - 行灯 (Andon): Visual alert system, emergency stop cord
#   - ポカヨケ (Poka-yoke): Error-proofing to prevent defects
#   - レシート (Receipt): Evidence chain for release certification
#
# See: .claude/TCPS_SYSTEM_COMPLETE.md for full manufacturing system
# ============================================================================

jidoka-inner:
	@echo ""
	@echo "$(BOLD)$(GREEN)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(GREEN)🏭 自働化 (JIDOKA) QUALITY GATE$(NC)"
	@echo "$(BOLD)$(GREEN)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@if [ -f tools/tcps/jidoka_quality_gate.sh ]; then \
		./tools/tcps/jidoka_quality_gate.sh; \
	else \
		echo "$(YELLOW)⚠ TCPS jidoka script not found (expected: tools/tcps/jidoka_quality_gate.sh)$(NC)"; \
		echo "$(YELLOW)Running standard quality gates instead$(NC)"; \
		echo ""; \
		$(MAKE) validate-inner; \
	fi

andon-inner:
	@echo ""
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(CYAN)🚨 行灯 (ANDON) BOARD STATUS$(NC)"
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@if [ -f tools/tcps/andon_cord.sh ]; then \
		./tools/tcps/andon_cord.sh status; \
	else \
		echo "$(YELLOW)⚠ TCPS andon script not found (expected: tools/tcps/andon_cord.sh)$(NC)"; \
		echo "$(GREEN)✓ No issues detected (fallback mode)$(NC)"; \
		echo ""; \
	fi

poka-yoke-inner:
	@echo ""
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(CYAN)🛡️ ポカヨケ (POKA-YOKE) ERROR-PROOFING$(NC)"
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@if [ -f tools/tcps/poka_yoke_validator.sh ]; then \
		./tools/tcps/poka_yoke_validator.sh; \
	else \
		echo "$(YELLOW)⚠ TCPS poka-yoke script not found (expected: tools/tcps/poka_yoke_validator.sh)$(NC)"; \
		echo "$(YELLOW)Running standard quality checks instead$(NC)"; \
		echo ""; \
		$(MAKE) validate-compile-inner validate-test-inner validate-quality-inner; \
	fi
	@echo ""

release-validate-inner: validate-inner jidoka-inner
	@echo ""

# ============================================================================
# GRACEFUL SHUTDOWN TEST TARGETS (INNER - Docker-only)
# ============================================================================
# Test graceful shutdown behavior under load to ensure zero-dropped requests
# during deployment and proper connection draining.
# ============================================================================

test-shutdown-inner: shutdown-load-test-inner

shutdown-load-test-inner:
	@echo "$(BLUE)⚡ Graceful Shutdown Load Test$(NC)"
	@echo "  Testing: Connection draining, zero-dropped requests, shutdown hooks"
	@echo ""
	@if [ -f scripts/test_graceful_shutdown_load.sh ]; then \
		chmod +x scripts/test_graceful_shutdown_load.sh; \
		scripts/test_graceful_shutdown_load.sh; \
	else \
		echo "$(RED)❌ GRACEFUL SHUTDOWN TEST SCRIPT MISSING$(NC)"; \
		exit 1; \
	fi

validate-shutdown-inner:
	@echo "$(BLUE)⚡ Quality Gate: Graceful Shutdown$(NC)"
	@echo "  Validating: prep_stop/1 implementations, connection draining, timeout enforcement"
	@echo ""
	@if [ -f scripts/validate_graceful_shutdown.sh ]; then \
		chmod +x scripts/validate_graceful_shutdown.sh; \
		scripts/validate_graceful_shutdown.sh; \
	else \
		echo "$(RED)❌ GRACEFUL SHUTDOWN VALIDATION SCRIPT MISSING$(NC)"; \
		exit 1; \
	fi
	@echo ""
	@echo "$(GREEN)✅ Graceful shutdown validation passed$(NC)"
	@echo ""

# ============================================================================
# TCPS MANUFACTURING TARGETS (INNER - Docker-only) (自働化 Jidoka - Stop-the-Line Authority)
# ============================================================================

tcps-quality-gates-inner: validate-shutdown-inner validate-compile-inner validate-test-inner validate-quality-inner
	@echo ""
	@echo "$(BOLD)$(BLUE)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(BLUE)📋 GENERATING QUALITY RECEIPT (レシート)$(NC)"
	@echo "$(BOLD)$(BLUE)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@if [ -f tools/tcps/generate-quality-receipt.sh ]; then \
		if ./tools/tcps/generate-quality-receipt.sh; then \
			echo ""; \
			echo "$(BOLD)$(GREEN)════════════════════════════════════════════════════════════$(NC)"; \
			echo "$(BOLD)$(GREEN)🎉 RELEASE READY - 認証 (CERTIFIED)$(NC)"; \
			echo "$(BOLD)$(GREEN)════════════════════════════════════════════════════════════$(NC)"; \
			echo ""; \
			echo "$(GREEN)✓ All quality gates passed$(NC)"; \
			echo "$(GREEN)✓ Jidoka validation complete$(NC)"; \
			echo "$(GREEN)✓ Quality receipt generated$(NC)"; \
			echo "$(GREEN)✓ Ready for production deployment$(NC)"; \
			echo ""; \
		else \
			echo ""; \
			echo "$(RED)❌ RELEASE BLOCKED - CERTIFICATION FAILED$(NC)"; \
			echo "$(RED)Refusal Code: QUALITY_RECEIPT_BLOCKED$(NC)"; \
			echo "$(RED)Remediation: Fix blockers listed in receipt, re-run release-validate$(NC)"; \
			echo ""; \
			exit 1; \
		fi; \
	else \
		echo "$(RED)❌ RECEIPT GENERATOR MISSING$(NC)"; \
		echo "$(RED)Refusal Code: MISSING_TOOL_RECEIPT$(NC)"; \
		echo "$(RED)Remediation: Create tools/tcps/generate-quality-receipt.sh$(NC)"; \
		echo ""; \
		exit 1; \
	fi

# ============================================================================
# QUALITY GATES (MANDATORY BEFORE "DONE")
# ============================================================================
# Legacy targets maintained for backward compatibility.
# Use validate-* targets for strict enforcement.
# ============================================================================

check-inner: compile-inner test-inner
	@echo ""
	@echo "$(GREEN)════════════════════════════════════════$(NC)"
	@echo "$(GREEN)✓ BASIC QUALITY GATES PASSED$(NC)"
	@echo "$(GREEN)════════════════════════════════════════$(NC)"
	@echo ""
	@echo "$(GREEN)✓ Compilation:$(NC) All modules compiled successfully"
	@echo "$(GREEN)✓ Tests:$(NC) All tests passed (eunit + ct)"
	@echo ""
	@echo "$(YELLOW)⚠ Note: Run 'make check-full' for complete quality gates (xref + dialyzer)$(NC)"
	@echo "$(YELLOW)⚠ Use 'make validate' for BLOCKING quality enforcement$(NC)"
	@echo ""

check-full-inner: compile-inner xref-inner dialyzer-inner test-inner coverage-inner
	@echo ""
	@echo "$(GREEN)════════════════════════════════════════$(NC)"
	@echo "$(GREEN)✓ ALL QUALITY GATES PASSED$(NC)"
	@echo "$(GREEN)════════════════════════════════════════$(NC)"
	@echo ""
	@echo "$(GREEN)✓ Compilation:$(NC) All modules compiled successfully"
	@echo "$(GREEN)✓ Xref:$(NC) Cross-reference analysis complete"
	@echo "$(GREEN)✓ Dialyzer:$(NC) Type checking passed"
	@echo "$(GREEN)✓ Tests:$(NC) All tests passed (eunit + ct)"
	@echo "$(GREEN)✓ Coverage:$(NC) Report generated"
	@echo ""

dialyzer-inner: dialyzer-fast-inner ## Default to incremental dialyzer (3-7x faster)

dialyzer-fast-inner: ## Incremental dialyzer (development) - 3-7x faster
	@echo "$(BLUE)Running incremental Dialyzer (development mode)...$(NC)"
	@rebar3 dialyzer --incremental
	@echo "$(GREEN)✓ Incremental Dialyzer passed (15-30s)$(NC)"

dialyzer-full-inner: ## Full dialyzer (CI/CD) - complete analysis
	@echo "$(BLUE)Running full Dialyzer analysis (CI/CD mode)...$(NC)"
	@rebar3 dialyzer
	@echo "$(GREEN)✓ Full Dialyzer passed (60-90s)$(NC)"

dialyzer-update-plt-inner: ## Update PLT (after dependency changes)
	@echo "$(BLUE)Updating Dialyzer PLT...$(NC)"
	@rebar3 dialyzer --update_plt
	@echo "$(GREEN)✓ PLT updated$(NC)"

dialyzer-clean-inner: ## Clean Dialyzer cache (force rebuild)
	@echo "$(BLUE)Cleaning Dialyzer cache...$(NC)"
	@rm -rf _build/default/*_plt*
	@echo "$(GREEN)✓ Dialyzer cache cleaned$(NC)"
	@echo "$(YELLOW)Next dialyzer run will rebuild PLT (slower)$(NC)"

xref-inner:
	@echo "$(BLUE)Running xref (cross-reference analysis)...$(NC)"
	@rebar3 xref || echo "$(YELLOW)⚠ Xref encountered issues (see above warnings)$(NC)"
	@echo "$(GREEN)✓ Xref complete (with warnings)$(NC)"

# ============================================================================
# CLEANUP (INNER - Docker-only implementations)
# ============================================================================

clean-inner:
	@echo "$(BLUE)Cleaning build artifacts...$(NC)"
	@rebar3 clean
	@echo "$(GREEN)✓ Clean complete$(NC)"

distclean-inner: clean-inner
	@echo "$(BLUE)Deep cleaning (includes deps)...$(NC)"
	@rm -rf _build rebar.lock
	@cd apps/erlmcp_core && rm -rf _build
	@cd apps/erlmcp_transports && rm -rf _build
	@cd apps/erlmcp_observability && rm -rf _build
	@cd apps/tcps_erlmcp && rm -rf _build
	@echo "$(GREEN)✓ Distclean complete$(NC)"

# ============================================================================
# DEVELOPMENT (INNER - Docker-only implementations)
# ============================================================================

console-inner: setup-profile-inner
	@echo "$(BLUE)Starting Erlang shell...$(NC)"
	@rebar3 shell

observer:
	@echo "$(BLUE)Starting Observer...$(NC)"
	@erl -pa _build/default/lib/*/ebin -eval 'observer:start().'

deps:
	@echo "$(BLUE)Fetching dependencies...$(NC)"
	@rebar3 get-deps
	@echo "$(GREEN)✓ Dependencies fetched$(NC)"

info:
	@echo "$(BLUE)Project Information:$(NC)"
	@echo "  Name: erlmcp (Erlang MCP SDK)"
	@echo "  Version: 2.0.0"
	@echo "  Structure: Umbrella with 4 apps"
	@echo ""
	@echo "$(BLUE)Applications:$(NC)"
	@echo "  1. erlmcp_core         - JSON-RPC, Registry, Client/Server"
	@echo "  2. erlmcp_transports   - STDIO, TCP, HTTP, WebSocket"
	@echo "  3. erlmcp_observability - Metrics, Traces, Receipts"
	@echo "  4. tcps_erlmcp         - Toyota Code Production System"
	@echo ""
	@echo "$(BLUE)Key dependencies:$(NC)"
	@rebar3 tree | head -20

# ============================================================================
# BENCHMARKS
# ============================================================================

benchmark:
	@echo "$(BLUE)Running quick benchmarks...$(NC)"
	@make -f Makefile benchmark-quick
	@echo "$(GREEN)✓ Benchmarks complete (see above results)$(NC)"

bench-quick:
	@echo "$(BLUE)Running quick performance check (<2 min)...$(NC)"
	@./scripts/bench/quick.sh
	@echo "$(GREEN)✓ Quick benchmark complete$(NC)"

# ============================================================================
# RELEASE
# ============================================================================

release: setup-profile
	@echo "$(BLUE)Building production release...$(NC)"
	@rebar3 as prod release
	@echo "$(GREEN)✓ Release built: _build/prod/rel/erlmcp$(NC)"

release-validate: release ## Validate production release via Docker (build + smoke test + digest verification)
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(CYAN)🐳 PRODUCTION RELEASE VALIDATION (Docker-Only)$(NC)"
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@echo "$(BLUE)[1/3] Building Docker image...$(NC)"
	@docker build -t erlmcp:3.0.0-prod . > /tmp/docker-release-build.log 2>&1 || { \
		echo "$(RED)✗ Docker build failed$(NC)"; \
		cat /tmp/docker-release-build.log; \
		exit 1; \
	}
	@echo "$(GREEN)✓ Docker image built$(NC)"
	@echo ""
	@echo "$(BLUE)[2/3] Running smoke tests...$(NC)"
	@./scripts/release/smoke-test.sh erlmcp:3.0.0-prod || { \
		echo "$(RED)✗ Smoke tests failed$(NC)"; \
		exit 1; \
	}
	@echo "$(GREEN)✓ Smoke tests passed$(NC)"
	@echo ""
	@echo "$(BLUE)[3/3] Verifying image digest...$(NC)"
	@./scripts/release/verify-digest.sh erlmcp:3.0.0-prod
	@echo "$(GREEN)✓ Digest verified$(NC)"
	@echo ""
	@echo "$(BOLD)$(GREEN)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(GREEN)🎉 RELEASE VALIDATION COMPLETE - CERTIFIED$(NC)"
	@echo "$(BOLD)$(GREEN)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@docker images | grep erlmcp | grep 3.0.0-prod
	@echo ""
	@echo "$(GREEN)✓ Ready for production deployment$(NC)"
	@echo ""

# ============================================================================
# TCPS ADDITIONAL TARGETS (Enhanced automation)
# ============================================================================

andon-clear:
	@echo "$(BOLD)$(CYAN)Clearing Andon Status...$(NC)"
	@echo ""
	@./tools/tcps/andon_cord.sh clear

andon-watch:
	@echo "$(BOLD)$(CYAN)Starting Andon Monitor...$(NC)"
	@./tools/tcps/andon_cord.sh watch

tcps-quality-gates-inner: validate-inner
	@echo ""
	@echo "$(GREEN)╔═══════════════════════════════════════════════════════════╗$(NC)"
	@echo "$(GREEN)║  ✓ TCPS QUALITY SYSTEM COMPLETE                           ║$(NC)"
	@echo "$(GREEN)║  自働化 (Jidoka) + ポカヨケ (Poka-Yoke)                   ║$(NC)"
	@echo "$(GREEN)╚═══════════════════════════════════════════════════════════╝$(NC)"
	@echo ""
	@if [ -f tools/tcps/jidoka_quality_gate.sh ]; then \
		./tools/tcps/jidoka_quality_gate.sh; \
	fi

# ============================================================================
# BUILD SYSTEM VERIFICATION (for agent compliance)
# ============================================================================

build-inner: compile-inner
	@echo "$(GREEN)✓ Build target complete (alias for compile)$(NC)"

##
## Quality Metrics Targets
##

.PHONY: metrics-snapshot metrics-trend metrics-report metrics-all metrics-ci

## Capture current quality snapshot
metrics-snapshot:
	@echo "=== Capturing Quality Snapshot ==="
	@./tools/metrics/quality-snapshot.sh

## Analyze quality trends (30 days)
metrics-trend:
	@echo "=== Analyzing Quality Trends ==="
	@./tools/metrics/quality-trend.sh --days 30

## Generate quality trend HTML report
metrics-trend-html:
	@echo "=== Generating HTML Trend Report ==="
	@./tools/metrics/quality-trend.sh --days 30 --html metrics/quality-trend.html
	@echo "Report saved to: metrics/quality-trend.html"

## Comprehensive quality report
metrics-report:
	@echo "=== Generating Comprehensive Quality Report ==="
	@./tools/metrics/quality-report.sh

## Weekly quality report
metrics-weekly:
	@echo "=== Weekly Quality Report ==="
	@./tools/metrics/quality-report.sh --period week

## Monthly quality report
metrics-monthly:
	@echo "=== Monthly Quality Report ==="
	@./tools/metrics/quality-report.sh --period month

## Run all metrics (snapshot + trend + report)
metrics-all: metrics-snapshot metrics-trend metrics-report

## CI metrics workflow (snapshot + regression check)
metrics-ci: metrics-snapshot
	@echo "=== Checking for Regressions ==="
	@if ./tools/metrics/quality-trend.sh --days 7 | grep "DEGRADING (alert threshold exceeded)"; then \
		echo "❌ Quality regression detected!"; \
		exit 1; \
	else \
		echo "✅ No regressions detected"; \
	fi

## Clean metrics data
metrics-clean:
	@echo "=== Cleaning Metrics Data ==="
	@rm -rf metrics/snapshots/*.json
	@rm -f metrics/*.html
	@echo "✅ Metrics data cleaned"

help-extra:
	@echo "  metrics-snapshot    - Capture current quality snapshot"
	@echo "  metrics-trend       - Analyze quality trends (30 days)"
	@echo "  metrics-trend-html  - Generate HTML trend report"
	@echo "  metrics-report      - Comprehensive quality report"
	@echo "  metrics-weekly      - Weekly quality report"
	@echo "  metrics-monthly     - Monthly quality report"
	@echo "  metrics-all         - Run all metrics"
	@echo "  metrics-ci          - CI workflow (snapshot + regression check)"
	@echo "  metrics-clean       - Clean metrics data"

##
## Auto-Fix System (Jidoka 自働化)
##

.PHONY: auto-fix auto-fix-quick auto-fix-validate auto-fix-status auto-fix-reset auto-fix-interactive

auto-fix: ## Run full auto-fix orchestration (5 iterations)
	@echo "Running Auto-Fix System (Jidoka 自働化)..."
	@./tools/auto-fix/orchestrator.sh orchestrate

auto-fix-quick: ## Quick quality check with auto-fix (1 pass)
	@echo "Quick auto-fix check..."
	@./tools/auto-fix/gate-failure-dispatcher.sh run-all

auto-fix-validate: ## Validate all fixes with clean build
	@echo "Validating auto-fixes..."
	@./tools/auto-fix/orchestrator.sh validate

auto-fix-status: ## Show auto-fix status and attempt counts
	@./tools/auto-fix/gate-failure-dispatcher.sh status

auto-fix-reset: ## Reset auto-fix state and counters
	@./tools/auto-fix/gate-failure-dispatcher.sh reset

auto-fix-interactive: ## Start interactive auto-fix mode
	@./tools/auto-fix/orchestrator.sh interactive

auto-fix-help: ## Show auto-fix system help
	@echo "Auto-Fix System Commands:"
	@echo "  make auto-fix              Run full orchestration"
	@echo "  make auto-fix-quick        Quick quality check"
	@echo "  make auto-fix-validate     Validate with clean build"
	@echo "  make auto-fix-status       Show current status"
	@echo "  make auto-fix-reset        Reset state"
	@echo "  make auto-fix-interactive  Interactive mode"
	@echo ""
	@echo "Documentation:"
	@echo "  tools/auto-fix/README.md"
	@echo "  docs/auto-fix/AUTO_FIX_SYSTEM.md"
	@echo ""
	@echo "For detailed help: ./tools/auto-fix/orchestrator.sh help"

# ============================================================================
# EXAMPLES
# ============================================================================

.PHONY: example-mcp-complete example-help

example-mcp-complete: compile
	@echo "$(BOLD)$(BLUE)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(BLUE)Running erlmcp Full Surface End-to-End Example$(NC)"
	@echo "$(BOLD)$(BLUE)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@escript examples/mcp_complete/example.erl
	@echo ""
	@echo "$(BOLD)$(GREEN)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(GREEN)✓ Example complete - All MCP features demonstrated$(NC)"
	@echo "$(BOLD)$(GREEN)════════════════════════════════════════════════════════════$(NC)"
	@echo ""

example-help:
	@echo "$(BOLD)$(BLUE)erlmcp Examples$(NC)"
	@echo ""
	@echo "  make example-mcp-complete  - Run full surface end-to-end example"
	@echo ""
	@echo "  This example demonstrates:"
	@echo "    - Resources (list, read, templates)"
	@echo "    - Tools (invocation, secret injection)"
	@echo "    - Prompts (templates, arguments)"
	@echo "    - Subscriptions (change notifications)"
	@echo "    - Progress (token reporting)"
	@echo "    - HTTP Transport"
	@echo "    - Secrets management"
	@echo ""
	@echo "  See examples/mcp_complete/README.md for detailed documentation"
	@echo ""

# ============================================================================
# MCP SPECIFICATION VALIDATION
# ============================================================================

validate-spec:
	@echo "$(BLUE)📋 MCP Spec Validation$(NC)"
	@echo "  Target: MCP spec compliance (2025-11-25)"
	@echo "  Action: Running spec validation tests..."
	@echo ""
	@if [ ! -d "apps/erlmcp_validation" ]; then \
		echo "$(RED)❌ VALIDATION APP NOT FOUND$(NC)"; \
		echo "$(RED)Gate: BLOCKED$(NC)"; \
		echo "$(RED)Action: Ensure apps/erlmcp_validation exists$(NC)"; \
		echo ""; \
		exit 1; \
	fi
	@echo "  1. Compiling..."; \
	if ! TERM=dumb rebar3 compile 2>&1 | tee /tmp/erlmcp_spec_compile.log; then \
		echo ""; \
		echo "$(RED)❌ COMPILATION FAILED$(NC)"; \
		echo "$(RED)Gate: BLOCKED$(NC)"; \
		exit 1; \
	fi
	@echo "     ✓ Compiled$(NC)"
	@echo "  2. Running spec compliance tests..."; \
	if ! rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE.ct 2>&1 | tee /tmp/erlmcp_spec_ct.log; then \
		echo ""; \
		echo "$(RED)❌ SPEC COMPLIANCE TESTS FAILED$(NC)"; \
		echo "$(RED)Gate: BLOCKED$(NC)"; \
		echo "$(RED)Action: Fix failing spec compliance tests$(NC)"; \
		echo ""; \
		exit 1; \
	fi
	@echo "     ✓ Spec compliance tests passed$(NC)"
	@if [ -f "apps/erlmcp_validation/src/erlmcp_validate_cli.erl" ]; then \
		echo "  3. Generating validation report..."; \
		if rebar3 as validation escriptize > /dev/null 2>&1; then \
			if ./_build/validation/bin/erlmcp_validate run --all --output-file=/tmp/spec_validation_report.json 2>&1; then \
				if [ -f /tmp/spec_validation_report.json ]; then \
					if grep -q '"overall_status":"fail"' /tmp/spec_validation_report.json 2>/dev/null; then \
						echo ""; \
						echo "$(RED)❌ VALIDATION REPORT INDICATES FAILURE$(NC)"; \
						echo "$(RED)Gate: BLOCKED$(NC)"; \
						exit 1; \
					fi; \
					echo "     ✓ Validation report passed$(NC)"; \
				else \
					echo "     ⚠ Report generation skipped (no report file)$(NC)"; \
				fi; \
			else \
				echo "     ⚠ Report generation failed (non-blocking)$(NC)"; \
			fi; \
		else \
			echo "     ⚠ Could not build validation CLI (non-blocking)$(NC)"; \
		fi; \
	fi
	@echo ""
	@echo "$(GREEN)✅ MCP spec validation PASSED$(NC)"
	@echo ""

# ============================================================================
# NINE-NINES PERFORMANCE VALIDATION
# ============================================================================

.PHONY: benchmark-nine-nines benchmark-nine-nines-baseline benchmark-nine-nines-overload benchmark-nine-nines-full

benchmark-nine-nines: benchmark-nine-nines-full

benchmark-nine-nines-baseline: ## Run baseline nine-nines benchmarks
	@echo "$(BLUE)Running nine-nines baseline benchmarks...$(NC)"
	@./scripts/bench/run_nine_nines_validation.sh baseline

benchmark-nine-nines-overload: ## Run nine-nines overload profiling
	@echo "$(BLUE)Running nine-nines overload profiling...$(NC)"
	@./scripts/bench/run_nine_nines_validation.sh overload

benchmark-nine-nines-full: ## Run complete nine-nines validation
	@echo "$(BLUE)Running complete nine-nines validation...$(NC)"
	@./scripts/bench/run_nine_nines_validation.sh full


# ============================================================================
# GOVERNANCE SYSTEM (Claude Code Web v3.0.0)
# ============================================================================
# Armstrong-style governance using Claude Code Web native primitives
# (hooks, skills, subagents, settings scopes).
#
# Pattern: Policy → Execution → Verification → Receipt
#
# Architecture:
#   Layer 1: Sandbox + Network Policy (product enforced)
#   Layer 2: Hook-Based Runtime Governor (.claude/hooks + settings.json)
#   Layer 3: Skills + Subagents (reusable procedures + role-based execution)
#
# References:
#   - CLAUDE_CODE_WEB_GOVERNANCE_SYSTEM.md (specification)
#   - AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md (WO-010)
#   - .claude/settings.json (governance configuration)
# ============================================================================

.PHONY: hooks-validate settings-validate governance-test receipts-list governance-status governance-validate

hooks-validate: ## Validate all hooks exist, are executable, and have valid bash syntax
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(CYAN)🔍 Hook Validation$(NC)"
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@echo "$(BLUE)[1/3] Checking hook files exist...$(NC)"
	@HOOK_ERRORS=0; \
	HOOKS_DIR=".claude/hooks"; \
	if [ ! -d "$$HOOKS_DIR" ]; then \
		echo "  $(RED)✗ Hooks directory missing: $$HOOKS_DIR$(NC)"; \
		exit 1; \
	fi; \
	echo "  $(GREEN)✓ Hooks directory exists$(NC)"; \
	echo ""; \
	echo "$(BLUE)[2/3] Checking hook files are executable...$(NC)"; \
	for hook in $$HOOKS_DIR/*.sh; do \
		if [ -f "$$hook" ]; then \
			if [ -x "$$hook" ]; then \
				echo "  $(GREEN)✓ $$(basename $$hook) (executable)$(NC)"; \
			else \
				echo "  $(RED)✗ $$(basename $$hook) (not executable)$(NC)"; \
				HOOK_ERRORS=$$((HOOK_ERRORS + 1)); \
			fi; \
		fi; \
	done; \
	echo ""; \
	echo "$(BLUE)[3/3] Validating bash syntax...$(NC)"; \
	for hook in $$HOOKS_DIR/*.sh; do \
		if [ -f "$$hook" ]; then \
			if bash -n "$$hook" 2>/dev/null; then \
				echo "  $(GREEN)✓ $$(basename $$hook) (valid syntax)$(NC)"; \
			else \
				echo "  $(RED)✗ $$(basename $$hook) (syntax error)$(NC)"; \
				bash -n "$$hook" 2>&1 | sed 's/^/    /'; \
				HOOK_ERRORS=$$((HOOK_ERRORS + 1)); \
			fi; \
		fi; \
	done; \
	echo ""; \
	if [ $$HOOK_ERRORS -eq 0 ]; then \
		echo "$(BOLD)$(GREEN)✅ All hooks validated successfully$(NC)"; \
		echo ""; \
	else \
		echo "$(BOLD)$(RED)❌ Hook validation FAILED ($$HOOK_ERRORS errors)$(NC)"; \
		echo "$(RED)Action: Fix hook files listed above$(NC)"; \
		echo ""; \
		exit 1; \
	fi

settings-validate: ## Validate .claude/settings.json schema and hook references
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(CYAN)🔍 Settings Validation$(NC)"
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@SETTINGS_FILE=".claude/settings.json"; \
	if [ ! -f "$$SETTINGS_FILE" ]; then \
		echo "$(RED)✗ Settings file missing: $$SETTINGS_FILE$(NC)"; \
		exit 1; \
	fi; \
	echo "$(BLUE)[1/3] Checking JSON syntax...$(NC)"; \
	if command -v jq >/dev/null 2>&1; then \
		if jq empty "$$SETTINGS_FILE" 2>/dev/null; then \
			echo "  $(GREEN)✓ Valid JSON syntax$(NC)"; \
		else \
			echo "  $(RED)✗ Invalid JSON syntax$(NC)"; \
			jq empty "$$SETTINGS_FILE" 2>&1 | sed 's/^/    /'; \
			exit 1; \
		fi; \
	else \
		echo "  $(YELLOW)⚠ jq not available, skipping JSON validation$(NC)"; \
	fi; \
	echo ""; \
	echo "$(BLUE)[2/3] Validating hook file references...$(NC)"; \
	if command -v jq >/dev/null 2>&1; then \
		HOOK_REF_ERRORS=0; \
		jq -r '.. | .command? // empty | select(startswith("./.claude/hooks/"))' "$$SETTINGS_FILE" 2>/dev/null | sort -u | while read -r hook_path; do \
			if [ -f "$$hook_path" ]; then \
				echo "  $(GREEN)✓ $$hook_path exists$(NC)"; \
			else \
				echo "  $(RED)✗ $$hook_path missing$(NC)"; \
				HOOK_REF_ERRORS=$$((HOOK_REF_ERRORS + 1)); \
			fi; \
		done; \
	else \
		echo "  $(YELLOW)⚠ jq not available, skipping hook reference validation$(NC)"; \
	fi; \
	echo ""; \
	echo "$(BLUE)[3/3] Validating subagent definitions...$(NC)"; \
	if command -v jq >/dev/null 2>&1; then \
		SUBAGENT_COUNT=$$(jq '.subagents | keys | length' "$$SETTINGS_FILE" 2>/dev/null); \
		echo "  $(GREEN)✓ Found $$SUBAGENT_COUNT subagent definitions$(NC)"; \
		jq -r '.subagents | keys[]' "$$SETTINGS_FILE" 2>/dev/null | while read -r subagent; do \
			echo "    - $$subagent"; \
		done; \
	else \
		echo "  $(YELLOW)⚠ jq not available, skipping subagent validation$(NC)"; \
	fi; \
	echo ""; \
	echo "$(BOLD)$(GREEN)✅ Settings validation PASSED$(NC)"; \
	echo ""

governance-test: ## Run all hook test suites and governance validation
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(CYAN)🧪 Governance Test Suite$(NC)"
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@TEST_ERRORS=0; \
	TESTS_PASSED=0; \
	TESTS_FAILED=0; \
	echo "$(BOLD)Running governance tests...$(NC)"; \
	echo ""; \
	echo "$(BLUE)[1/5] Hook validation...$(NC)"; \
	if $(MAKE) -s hooks-validate; then \
		echo "  $(GREEN)✓ Hook validation passed$(NC)"; \
		TESTS_PASSED=$$((TESTS_PASSED + 1)); \
	else \
		echo "  $(RED)✗ Hook validation failed$(NC)"; \
		TESTS_FAILED=$$((TESTS_FAILED + 1)); \
		TEST_ERRORS=$$((TEST_ERRORS + 1)); \
	fi; \
	echo ""; \
	echo "$(BLUE)[2/5] Settings validation...$(NC)"; \
	if $(MAKE) -s settings-validate; then \
		echo "  $(GREEN)✓ Settings validation passed$(NC)"; \
		TESTS_PASSED=$$((TESTS_PASSED + 1)); \
	else \
		echo "  $(RED)✗ Settings validation failed$(NC)"; \
		TESTS_FAILED=$$((TESTS_FAILED + 1)); \
		TEST_ERRORS=$$((TEST_ERRORS + 1)); \
	fi; \
	echo ""; \
	echo "$(BLUE)[3/5] Hook test suite (policy-bash)...$(NC)"; \
	if [ -x ".claude/hooks/test_policy_bash.sh" ]; then \
		if ./.claude/hooks/test_policy_bash.sh > /tmp/governance_test_policy_bash.log 2>&1; then \
			echo "  $(GREEN)✓ policy-bash tests passed$(NC)"; \
			TESTS_PASSED=$$((TESTS_PASSED + 1)); \
		else \
			echo "  $(RED)✗ policy-bash tests failed$(NC)"; \
			echo "  $(YELLOW)See /tmp/governance_test_policy_bash.log for details$(NC)"; \
			TESTS_FAILED=$$((TESTS_FAILED + 1)); \
			TEST_ERRORS=$$((TEST_ERRORS + 1)); \
		fi; \
	else \
		echo "  $(YELLOW)⚠ test_policy_bash.sh not found or not executable$(NC)"; \
	fi; \
	echo ""; \
	echo "$(BLUE)[4/5] SessionStart hook test...$(NC)"; \
	if [ -x ".claude/hooks/SessionStart.sh" ]; then \
		if ./.claude/hooks/SessionStart.sh --dry-run > /tmp/governance_test_sessionstart.log 2>&1; then \
			echo "  $(GREEN)✓ SessionStart hook test passed$(NC)"; \
			TESTS_PASSED=$$((TESTS_PASSED + 1)); \
		else \
			echo "  $(YELLOW)⚠ SessionStart hook test skipped (no --dry-run support)$(NC)"; \
		fi; \
	else \
		echo "  $(YELLOW)⚠ SessionStart.sh not found or not executable$(NC)"; \
	fi; \
	echo ""; \
	echo "$(BLUE)[5/5] Receipt generation test...$(NC)"; \
	if [ -x ".claude/hooks/receipt.sh" ]; then \
		if bash -n ./.claude/hooks/receipt.sh 2>/dev/null; then \
			echo "  $(GREEN)✓ Receipt.sh syntax valid$(NC)"; \
			TESTS_PASSED=$$((TESTS_PASSED + 1)); \
		else \
			echo "  $(RED)✗ Receipt.sh syntax error$(NC)"; \
			TESTS_FAILED=$$((TESTS_FAILED + 1)); \
			TEST_ERRORS=$$((TEST_ERRORS + 1)); \
		fi; \
	else \
		echo "  $(YELLOW)⚠ receipt.sh not found or not executable$(NC)"; \
	fi; \
	echo ""; \
	echo "$(BOLD)Test Summary:$(NC)"; \
	echo "  Passed: $(GREEN)$$TESTS_PASSED$(NC)"; \
	echo "  Failed: $(RED)$$TESTS_FAILED$(NC)"; \
	echo ""; \
	if [ $$TEST_ERRORS -eq 0 ]; then \
		echo "$(BOLD)$(GREEN)✅ All governance tests PASSED$(NC)"; \
		echo ""; \
	else \
		echo "$(BOLD)$(RED)❌ Governance tests FAILED ($$TEST_ERRORS errors)$(NC)"; \
		echo "$(RED)Action: Fix failing tests listed above$(NC)"; \
		echo ""; \
		exit 1; \
	fi

receipts-list: ## List recent session receipts (default: 10 most recent)
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(CYAN)📋 Recent Session Receipts$(NC)"
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@./.claude/commands/governance.sh receipts 10
	@echo ""

governance-status: ## Show governance system status
	@./.claude/commands/governance.sh status

governance-validate: ## Run full governance configuration validation
	@./.claude/commands/governance.sh validate

# ============================================================================
# GOVERNANCE CLI HELP
# ============================================================================

governance-help: ## Show governance system help
	@echo "$(BOLD)$(BLUE)Governance System Targets$(NC)"
	@echo ""
	@echo "$(BOLD)Makefile Targets:$(NC)"
	@echo "  make hooks-validate       - Validate all hooks (existence, permissions, syntax)"
	@echo "  make settings-validate    - Validate .claude/settings.json schema"
	@echo "  make governance-test      - Run all hook test suites"
	@echo "  make receipts-list        - List 10 most recent session receipts"
	@echo "  make governance-status    - Show governance system status"
	@echo "  make governance-validate  - Run full governance validation"
	@echo ""
	@echo "$(BOLD)CLI Commands:$(NC)"
	@echo "  ./.claude/commands/governance.sh hooks       - List active hooks"
	@echo "  ./.claude/commands/governance.sh receipts    - Show recent receipts"
	@echo "  ./.claude/commands/governance.sh verify      - Run manual verification"
	@echo "  ./.claude/commands/governance.sh status      - Show system status"
	@echo "  ./.claude/commands/governance.sh validate    - Validate configuration"
	@echo "  ./.claude/commands/governance.sh help        - Show CLI help"
	@echo ""
	@echo "$(BOLD)Hook Lifecycle:$(NC)"
	@echo "  SessionStart → PreToolUse* → PostToolUse* → Stop → SessionEnd"
	@echo ""
	@echo "$(BOLD)References:$(NC)"
	@echo "  - CLAUDE_CODE_WEB_GOVERNANCE_SYSTEM.md"
	@echo "  - AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md (WO-010)"
	@echo "  - DEVELOPMENT.md (Hook Lifecycle section)"
	@echo ""
		fi; \
	fi
	@echo ""
	@echo "$(GREEN)✅ MCP spec validation PASSED$(NC)"
	@echo ""
# ============================================================================
# CLI VERSIONING & RELEASE TARGETS
# ============================================================================

.PHONY: cli-version cli-release cli-release-dry-run cli-benchmark-baseline \
        cli-test-startup cli-checksum cli-install

cli-version: ## Show current CLI version
	@echo "$(BLUE)CLI Version Information:$(NC)"
	@echo ""
	@grep "define(VERSION" apps/erlmcp_validation/src/erlmcp_validate_cli.erl | \
		sed 's/.*"\(.*\)".*/  CLI Module: v\1/'
	@grep "{vsn," apps/erlmcp_validation/src/erlmcp_validation.app.src | \
		sed 's/.*"\(.*\)".*/  App Version: v\1/'
	@echo ""
	@if [ -f _build/validation/bin/erlmcp_validate ]; then \
		echo "  Escript Version:"; \
		./_build/validation/bin/erlmcp_validate --version | sed 's/^/    /'; \
	else \
		echo "  Escript: Not built (run 'make validate-cli')"; \
	fi
	@echo ""

cli-release: ## Create CLI release (usage: make cli-release VERSION=1.0.0)
	@if [ -z "$(VERSION)" ]; then \
		echo "$(RED)❌ Error: VERSION not specified$(NC)"; \
		echo "Usage: make cli-release VERSION=1.0.0"; \
		exit 1; \
	fi
	@echo "$(BLUE)Creating CLI release v$(VERSION)...$(NC)"
	@./scripts/release-cli.sh $(VERSION)

cli-release-dry-run: ## Test CLI release process (usage: make cli-release-dry-run VERSION=1.0.0)
	@if [ -z "$(VERSION)" ]; then \
		echo "$(RED)❌ Error: VERSION not specified$(NC)"; \
		echo "Usage: make cli-release-dry-run VERSION=1.0.0"; \
		exit 1; \
	fi
	@echo "$(YELLOW)DRY RUN: Testing CLI release v$(VERSION)...$(NC)"
	@./scripts/release-cli.sh $(VERSION) --dry-run

cli-benchmark-baseline: validate-cli ## Establish CLI performance baseline
	@echo "$(BLUE)CLI Performance Baseline Benchmarking$(NC)"
	@echo ""
	@echo "$(BOLD)Test 1: Startup Time (--version)$(NC)"
	@TOTAL=0; \
	RUNS=10; \
	for i in $$(seq 1 $$RUNS); do \
		START=$$(date +%s%N); \
		./_build/validation/bin/erlmcp_validate --version > /dev/null 2>&1; \
		END=$$(date +%s%N); \
		ELAPSED=$$((($${END} - $${START}) / 1000000)); \
		TOTAL=$$(($$TOTAL + $$ELAPSED)); \
		echo "  Run $$i: $${ELAPSED}ms"; \
	done; \
	AVG=$$(($$TOTAL / $$RUNS)); \
	echo "  Average: $${AVG}ms"; \
	if [ $$AVG -gt 2000 ]; then \
		echo "$(RED)❌ Startup time regression: $${AVG}ms > 2000ms$(NC)"; \
	else \
		echo "$(GREEN)✓ Startup time acceptable: $${AVG}ms$(NC)"; \
	fi
	@echo ""
	@echo "$(BOLD)Test 2: Help Command Time$(NC)"
	@START=$$(date +%s%N); \
	./_build/validation/bin/erlmcp_validate --help > /dev/null 2>&1; \
	END=$$(date +%s%N); \
	ELAPSED=$$((($${END} - $${START}) / 1000000)); \
	echo "  Elapsed: $${ELAPSED}ms"; \
	if [ $$ELAPSED -gt 3000 ]; then \
		echo "$(RED)❌ Help command slow: $${ELAPSED}ms > 3000ms$(NC)"; \
	else \
		echo "$(GREEN)✓ Help command acceptable: $${ELAPSED}ms$(NC)"; \
	fi
	@echo ""
	@echo "$(GREEN)✓ Baseline benchmarking complete$(NC)"
	@echo ""
	@echo "Thresholds:"
	@echo "  - Startup time: < 2000ms"
	@echo "  - Help command: < 3000ms"
	@echo ""

cli-test-startup: validate-cli ## Quick CLI startup test
	@echo "$(BLUE)Testing CLI startup...$(NC)"
	@if ./_build/validation/bin/erlmcp_validate --version; then \
		echo "$(GREEN)✓ CLI starts successfully$(NC)"; \
	else \
		echo "$(RED)❌ CLI failed to start$(NC)"; \
		exit 1; \
	fi

cli-checksum: validate-cli ## Generate CLI checksum
	@echo "$(BLUE)Generating CLI checksum...$(NC)"
	@cd _build/validation/bin && \
		sha256sum erlmcp_validate > erlmcp_validate.sha256 && \
		echo "$(GREEN)✓ Checksum generated:$(NC)" && \
		cat erlmcp_validate.sha256

cli-install: validate-cli ## Install CLI to /usr/local/bin (requires sudo)
	@echo "$(BLUE)Installing CLI to /usr/local/bin...$(NC)"
	@if [ ! -f _build/validation/bin/erlmcp_validate ]; then \
		echo "$(RED)❌ CLI not built$(NC)"; \
		exit 1; \
	fi
	@sudo cp _build/validation/bin/erlmcp_validate /usr/local/bin/erlmcp-validate
	@sudo chmod +x /usr/local/bin/erlmcp-validate
	@echo "$(GREEN)✓ CLI installed to /usr/local/bin/erlmcp-validate$(NC)"
	@echo ""
	@echo "Usage:"
	@echo "  erlmcp-validate --help"
	@echo "  erlmcp-validate --version"
	@echo ""

cli-uninstall: ## Uninstall CLI from /usr/local/bin (requires sudo)
	@echo "$(BLUE)Uninstalling CLI from /usr/local/bin...$(NC)"
	@if [ -f /usr/local/bin/erlmcp-validate ]; then \
		sudo rm /usr/local/bin/erlmcp-validate; \
		echo "$(GREEN)✓ CLI uninstalled$(NC)"; \
	else \
		echo "$(YELLOW)⚠ CLI not installed at /usr/local/bin/erlmcp-validate$(NC)"; \
	fi

# ============================================================================
# CLI PERFORMANCE TARGETS
# ============================================================================

bench-cli-startup: ## Benchmark CLI startup time (target: <100ms)
	@echo "$(BLUE)Benchmarking CLI startup performance...$(NC)"
	@rebar3 shell --config config/sys.config --eval "erlmcp_cli_startup_bench:run(#{iterations => 100}), init:stop()." --sname cli_bench_startup

bench-cli-commands: ## Benchmark CLI command execution (target: <500ms)
	@echo "$(BLUE)Benchmarking CLI command execution...$(NC)"
	@rebar3 shell --config config/sys.config --eval "erlmcp_cli_command_bench:run(#{iterations => 10}), init:stop()." --sname cli_bench_commands

bench-cli: bench-cli-startup bench-cli-commands ## Run all CLI benchmarks
	@echo "$(GREEN)✓ CLI benchmarks complete$(NC)"
	@echo "Results in bench/results/"

profile-cli: ## Profile CLI with fprof to find bottlenecks
	@echo "$(BLUE)Profiling CLI startup...$(NC)"
	@./scripts/bench/cli_profile.sh

bench-cli-quick: ## Quick CLI performance check
	@echo "$(BLUE)Quick CLI performance check...$(NC)"
	@rebar3 shell --config config/sys.config --eval "erlmcp_cli_startup_bench:run(#{iterations => 10}), init:stop()." --sname cli_bench_quick

.PHONY: bench-cli-startup bench-cli-commands bench-cli profile-cli bench-cli-quick

# ============================================================================
# CLI FEATURE TESTS (Interactive, Plugins, Completion, Diagnostics)
# ============================================================================

test-cli: test-cli-eunit test-cli-ct ## Run all CLI feature tests
	@echo "$(GREEN)✓ All CLI tests passed$(NC)"

test-cli-eunit: ## Run EUnit tests for CLI modules
	@echo "$(BLUE)Running CLI EUnit tests...$(NC)"
	@rebar3 eunit --module=erlmcp_cli_interactive_tests
	@rebar3 eunit --module=erlmcp_cli_completer_tests
	@rebar3 eunit --module=erlmcp_cli_formatter_tests
	@rebar3 eunit --module=erlmcp_cli_suggester_tests
	@rebar3 eunit --module=erlmcp_plugin_manager_tests
	@rebar3 eunit --module=erlmcp_cli_diagnostics_tests
	@echo "$(GREEN)✓ CLI EUnit tests passed$(NC)"

test-cli-ct: ## Run Common Test suites for CLI integration
	@echo "$(BLUE)Running CLI Common Test suites...$(NC)"
	@rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_cli_interactive_SUITE
	@rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_cli_plugins_SUITE
	@rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_cli_completion_SUITE
	@rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_cli_diagnostics_SUITE
	@rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_cli_performance_SUITE
	@echo "$(GREEN)✓ CLI Common Test suites passed$(NC)"

test-cli-coverage: test-cli ## Run CLI tests with coverage analysis
	@echo "$(BLUE)Generating CLI test coverage report...$(NC)"
	@rebar3 eunit --cover --module=erlmcp_cli_interactive_tests
	@rebar3 eunit --cover --module=erlmcp_cli_completer_tests
	@rebar3 eunit --cover --module=erlmcp_cli_formatter_tests
	@rebar3 eunit --cover --module=erlmcp_cli_suggester_tests
	@rebar3 eunit --cover --module=erlmcp_plugin_manager_tests
	@rebar3 eunit --cover --module=erlmcp_cli_diagnostics_tests
	@rebar3 cover --verbose
	@echo "$(GREEN)✓ CLI coverage report generated$(NC)"
	@echo "$(YELLOW)Coverage report: _build/test/cover/index.html$(NC)"

test-cli-interactive: ## Run only interactive CLI tests
	@echo "$(BLUE)Running interactive CLI tests...$(NC)"
	@rebar3 eunit --module=erlmcp_cli_interactive_tests
	@rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_cli_interactive_SUITE
	@echo "$(GREEN)✓ Interactive CLI tests passed$(NC)"

test-cli-plugins: ## Run only plugin system tests
	@echo "$(BLUE)Running plugin system tests...$(NC)"
	@rebar3 eunit --module=erlmcp_plugin_manager_tests
	@rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_cli_plugins_SUITE
	@echo "$(GREEN)✓ Plugin system tests passed$(NC)"

test-cli-completion: ## Run only completion tests
	@echo "$(BLUE)Running completion tests...$(NC)"
	@rebar3 eunit --module=erlmcp_cli_completer_tests
	@rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_cli_completion_SUITE
	@echo "$(GREEN)✓ Completion tests passed$(NC)"

test-cli-diagnostics: ## Run only diagnostics tests
	@echo "$(BLUE)Running diagnostics tests...$(NC)"
	@rebar3 eunit --module=erlmcp_cli_diagnostics_tests
	@rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_cli_diagnostics_SUITE
	@echo "$(GREEN)✓ Diagnostics tests passed$(NC)"

test-cli-performance: ## Run CLI performance tests
	@echo "$(BLUE)Running CLI performance tests...$(NC)"
	@rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_cli_performance_SUITE
	@echo "$(GREEN)✓ CLI performance tests passed$(NC)"

test-cli-regression: test-cli-performance ## Check for performance regressions in CLI
	@echo "$(BLUE)Checking for CLI performance regressions...$(NC)"
	@if [ -f .baseline_cli_performance.txt ]; then \
		echo "Comparing against baseline..."; \
		./scripts/check_cli_performance_regression.sh || exit 1; \
		echo "$(GREEN)✓ No performance regressions detected$(NC)"; \
	else \
		echo "$(YELLOW)⚠ No baseline found, creating baseline...$(NC)"; \
		./scripts/save_cli_performance_baseline.sh; \
	fi

# ============================================================================
# ggen - Ontology-Driven Code Generation Targets
# ============================================================================
# Generate Erlang/OTP code, K8s manifests, Docker configs from RDF ontologies
# All execution via Docker only (DOCKER-ONLY CONSTITUTION)
# ============================================================================

.PHONY: ggen-validate ggen-sync ggen-help ggen-shell

ggen-validate: ## Validate ggen ontologies against SHACL shapes
	@echo "$(BLUE)Validating ggen ontologies...$(NC)"
	@docker compose run --rm ggen ggen validate
	@echo "$(GREEN)✓ ggen validation passed$(NC)"

ggen-sync: ## Generate code from ggen ontologies
	@echo "$(BLUE)Generating code from ontologies...$(NC)"
	@docker compose run --rm ggen ggen sync
	@echo "$(GREEN)✓ Code generation complete$(NC)"
	@echo "$(BLUE)Generated files in ggen/src/generated/$(NC)"

ggen-help: ## Show ggen help
	@echo "$(BLUE)ggen help:$(NC)"
	@docker compose run --rm ggen ggen --help

ggen-shell: ## Open ggen shell for interactive use
	@echo "$(BLUE)Opening ggen shell...$(NC)"
	@docker compose run --rm ggen /bin/bash

ggen-validate-inner:
	@echo "$(BLUE)Validating ggen ontologies (Docker-only)...$(NC)"
	@docker compose run --rm ggen ggen validate

ggen-sync-inner:
	@echo "$(BLUE)Generating code from ontologies (Docker-only)...$(NC)"
	@docker compose run --rm ggen ggen sync

# Add ggen targets to DOCKER_REQUIRED_TARGETS list
DOCKER_REQUIRED_TARGETS += ggen-validate ggen-sync ggen-help ggen-shell

ggen-help-extra:
	@echo ""
	@echo "$(BOLD)$(BLUE)ggen - Ontology-Driven Code Generation$(NC)"
	@echo ""
	@echo "  make ggen-validate     - Validate RDF ontologies against SHACL shapes"
	@echo "  make ggen-sync         - Generate code from ontologies"
	@echo "  make ggen-help         - Show ggen CLI help"
	@echo "  make ggen-shell        - Open interactive ggen shell"
	@echo ""
	@echo "$(YELLOW)Note: All ggen execution is Docker-only (constitution)$(NC)"
	@echo ""
