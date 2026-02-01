# SPARC Makefile Refactoring Specification v1.0.0

**Date**: 2026-02-01
**Project**: erlmcp Makefile Modularization
**Methodology**: SPARC (Specification → Pseudocode → Architecture → Refinement → Completion)
**Armstrong Compliance**: Let-It-Crash, Isolation, Supervision, Black-Box Testing

---

## PHASE 1: SPECIFICATION

### 1.1 Problem Statement

**Current State**:
- **Makefile Size**: 1499 lines, 110+ targets
- **Complexity**: Deep nesting, inline shell scripts (50+ lines), mixed concerns
- **Maintainability**: Difficult to modify, test, or extend
- **Duplication**: Per-app targets (compile-core, test-core, compile-transports, test-transports, etc.)
- **Dependencies**: Implicit, unclear ordering (setup-profile, check-erlang-version scattered)
- **Testing**: No unit tests for Makefile logic
- **Cloud Safety**: Some targets not idempotent (andon-clear, distclean)

**Observed Issues**:
1. **Cognitive Load**: 110 targets with unclear relationships
2. **Code Duplication**: 4 apps × (compile, test, validate) = 12 redundant targets
3. **Error Handling**: Inconsistent (some use `|| exit 1`, others use `|| true`)
4. **Parallelization**: Explicit in `make check-parallel`, but not leveraged elsewhere
5. **State Management**: Implicit state in temp files (`/tmp/erlmcp_*.log`)
6. **Cloud Determinism**: Not all targets verified for cloud execution

### 1.2 Success Criteria

**Functional Requirements**:
- ✅ **Backward Compatibility**: All existing targets work identically
- ✅ **Modularity**: ≤300 lines per file, single responsibility
- ✅ **Testability**: Each module independently testable
- ✅ **Parallelizability**: Safe concurrent execution (file locks, atomic operations)
- ✅ **Idempotency**: All targets repeatable without side effects
- ✅ **Observability**: Progress reporting, failure tracing, receipt generation

**Non-Functional Requirements**:
- ✅ **Performance**: No regression (±5% tolerance)
- ✅ **Clarity**: Self-documenting (comments, naming conventions)
- ✅ **Armstrong Compliance**: Supervision (dependency chains), isolation (modules), determinism
- ✅ **Cloud Safety**: All targets cloud-executable (deterministic, environment-agnostic)

### 1.3 Constraints

**Immutable Constraints**:
1. **GNU Make**: Must use standard GNU Make 3.81+ (no custom tools)
2. **POSIX Shell**: Scripts must be POSIX-compliant (bash as fallback)
3. **OTP 28+**: All targets enforce Erlang/OTP 28+ requirement
4. **Rebar3**: Build tool remains rebar3 (no switch to mix/rebar)
5. **Git Workflow**: Preserve pre-commit hooks, CI/CD integration
6. **TPS System**: TCPS quality gates unchanged (jidoka, andon, poka-yoke)

**Flexible Constraints**:
- File structure (can introduce `makefiles/` directory)
- Target naming (can introduce namespaces like `gate:compile`, `tcps:jidoka`)
- Logging strategy (can centralize to `.erlmcp/logs/`)
- State management (can use lock files instead of temp files)

### 1.4 Invariants

**Armstrong Invariants** (MUST preserve):
1. **Process-per-App**: ∀app ∈ Apps. ∃!compile(app) ∧ ∃!test(app) (isolation)
2. **Gate Ordering**: compile ≺ test ≺ coverage ≺ dialyzer (dependency chain)
3. **Let-It-Crash**: ∀gate. failure(gate) → exit(1) ∧ ¬continue (stop-the-line)
4. **Black-Box Validation**: ∀target. test(target) ⊨ Observable(Behavior) ⊬ Implementation
5. **Idempotency**: ∀target. result(run₁) ≡ result(run₂) (deterministic)
6. **Supervision**: ∀child-target. ∃parent-target. supervises(parent, child) (dependency tree)
7. **Cloud Determinism**: ∀target. result(cloud) ≡ result(local) (environment-agnostic)

**Quality Invariants**:
- Coverage ≥ 80% (blocking gate)
- Compilation errors = 0 (blocking gate)
- Test failures = 0 (blocking gate)
- Performance regression < 10% (blocking gate)

---

## PHASE 2: PSEUDOCODE

### 2.1 Doctor Target Execution Flow

```pseudocode
FUNCTION doctor()
    LOG("Starting environment health check")

    // Gate 1: OTP Version
    IF NOT check_otp_version(28) THEN
        ERROR("OTP 28+ required")
        EXIT(1)
    END IF
    LOG("✓ OTP version: OK")

    // Gate 2: Rebar3 Availability
    IF NOT command_exists("rebar3") THEN
        ERROR("rebar3 not found")
        EXIT(1)
    END IF
    LOG("✓ rebar3: OK")

    // Gate 3: Dependencies
    IF NOT deps_fetched() THEN
        WARN("Dependencies not fetched, running rebar3 get-deps")
        RUN("rebar3 get-deps")
    END IF
    LOG("✓ Dependencies: OK")

    // Gate 4: Directory Structure
    FOR EACH app IN [core, transports, observability, tcps] DO
        IF NOT exists("apps/erlmcp_" + app) THEN
            ERROR("Missing app: erlmcp_" + app)
            EXIT(1)
        END IF
    END FOR
    LOG("✓ Directory structure: OK")

    // Gate 5: Profile Validation
    profile = ENV["ERLMCP_PROFILE"] OR "dev"
    IF NOT validate_profile(profile) THEN
        ERROR("Invalid profile: " + profile)
        EXIT(1)
    END IF
    LOG("✓ Profile: " + profile)

    LOG("✅ Environment healthy")
    RETURN 0
END FUNCTION
```

### 2.2 Quick → Verify → CI-Local Chain

```pseudocode
// Quick: Fast feedback loop (<5 min)
FUNCTION quick()
    start_time = NOW()

    RUN_DEPENDENCY(doctor)  // Ensures healthy environment

    // Phase 1: Compile (parallel across apps)
    LOG("1/3 Compiling...")
    PARALLEL_RUN([
        compile_app("core"),
        compile_app("transports"),
        compile_app("observability"),
        compile_app("tcps")
    ])

    // Phase 2: Smoke tests (critical path only)
    LOG("2/3 Running smoke tests...")
    RUN("rebar3 eunit --module=erlmcp_json_rpc_tests")

    // Phase 3: Quick validator
    LOG("3/3 Running quick validator...")
    IF exists("./bin/erlmcp-validate") THEN
        RUN("./bin/erlmcp-validate --quick")
    END IF

    elapsed = NOW() - start_time
    IF elapsed > 300 THEN  // 5 minutes
        WARN("Quick check exceeded 5min: " + elapsed + "s")
    END IF

    LOG("✅ Quick check PASSED")
    RETURN 0
END FUNCTION

// Verify: Full validation (<15 min)
FUNCTION verify()
    RUN_DEPENDENCY(compile)  // Reuses cached compilation

    // Phase 1: Static analysis (parallel)
    LOG("Running static analysis...")
    PARALLEL_RUN([
        run_xref(),
        run_dialyzer()
    ])

    // Phase 2: Compliance validation
    LOG("Running spec compliance...")
    RUN("./scripts/validation/run-ci.sh --compliance")

    // Phase 3: Transport validation
    LOG("Running transport validation...")
    RUN("./bin/erlmcp-validate --transport")

    // Phase 4: Full test suite
    LOG("Running full test suite...")
    RUN("./tools/test-runner.sh")

    LOG("✅ Full validation PASSED")
    RETURN 0
END FUNCTION

// CI-Local: Reproduce CI workflow exactly
FUNCTION ci_local()
    // Phase 1: Compilation (TERM=dumb for CI compatibility)
    LOG("GATE 1: Compilation")
    ENV["TERM"] = "dumb"
    RUN_LOGGED("rebar3 compile", "/tmp/erlmcp_ci_compile.log")

    // Phase 2: Xref (non-blocking)
    LOG("GATE 2: Xref")
    TRY
        RUN_LOGGED("rebar3 xref", "/tmp/erlmcp_ci_xref.log")
    CATCH
        WARN("Xref warnings (non-blocking)")
    END TRY

    // Phase 3: Dialyzer (blocking)
    LOG("GATE 3: Dialyzer")
    RUN_LOGGED("rebar3 dialyzer", "/tmp/erlmcp_ci_dialyzer.log")

    // Phase 4: EUnit (blocking)
    LOG("GATE 4: EUnit Tests")
    RUN_LOGGED("rebar3 as test do compile, eunit --cover", "/tmp/erlmcp_ci_eunit.log")

    // Phase 5: Common Test (non-blocking if no suites)
    LOG("GATE 5: Common Test")
    TRY
        RUN_LOGGED("rebar3 ct --dir=test/integration --cover", "/tmp/erlmcp_ci_ct.log")
    CATCH
        WARN("CT complete (failures non-blocking if no suites)")
    END TRY

    // Phase 6: Coverage check (blocking)
    LOG("GATE 6: Coverage Check (≥80%)")
    RUN_LOGGED("rebar3 cover --verbose", "/tmp/erlmcp_ci_coverage.log")
    IF NOT check_coverage_threshold(80) THEN
        ERROR("Coverage below 80%")
        EXIT(1)
    END IF

    LOG("✅ ALL CI GATES PASSED")
    RETURN 0
END FUNCTION
```

### 2.3 Failure Recovery Logic

```pseudocode
FUNCTION run_gate_with_recovery(gate_name, gate_function)
    attempt = 0
    max_attempts = 3

    WHILE attempt < max_attempts DO
        attempt = attempt + 1
        LOG("Running gate: " + gate_name + " (attempt " + attempt + ")")

        TRY
            result = gate_function()
            IF result == 0 THEN
                LOG("✓ Gate passed: " + gate_name)
                RETURN 0
            END IF
        CATCH error
            LOG("✗ Gate failed: " + gate_name)
            LOG("Error: " + error)

            // Auto-recovery strategies
            IF gate_name == "compile" THEN
                IF error.contains("dependency_not_found") THEN
                    LOG("Attempting auto-recovery: rebar3 get-deps")
                    RUN("rebar3 get-deps")
                    CONTINUE
                END IF
            ELSE IF gate_name == "network_timeout" THEN
                backoff = 2^attempt  // Exponential backoff
                LOG("Retrying after " + backoff + "s...")
                SLEEP(backoff)
                CONTINUE
            END IF

            // Manual intervention required
            IF attempt >= max_attempts THEN
                ERROR("Gate failed after " + max_attempts + " attempts")
                andon_pull(gate_name, error)
                EXIT(1)
            END IF
        END TRY
    END WHILE
END FUNCTION

FUNCTION andon_pull(gate_name, error)
    LOG("🚨 ANDON CORD PULLED: " + gate_name)
    WRITE_FILE(".erlmcp/andon.status", {
        gate: gate_name,
        error: error,
        timestamp: NOW(),
        context: get_git_context()
    })
    NOTIFY_DASHBOARD(gate_name, error)
END FUNCTION
```

### 2.4 Parallel Execution Design

```pseudocode
FUNCTION parallel_compile(apps)
    jobs = []
    lock_dir = ".erlmcp/locks/"

    FOR EACH app IN apps DO
        // File-level locking for isolation
        lock_file = lock_dir + app + ".lock"

        job = SPAWN(FUNCTION()
            ACQUIRE_LOCK(lock_file)
            TRY
                LOG("Compiling: " + app)
                RUN("cd apps/erlmcp_" + app + " && rebar3 compile")
                LOG("✓ Compiled: " + app)
            FINALLY
                RELEASE_LOCK(lock_file)
            END TRY
        END FUNCTION)

        jobs.APPEND(job)
    END FOR

    // Wait for all jobs to complete
    results = WAIT_ALL(jobs)

    // Check for failures
    FOR EACH result IN results DO
        IF result.exit_code != 0 THEN
            ERROR("Compilation failed: " + result.app)
            EXIT(1)
        END IF
    END FOR

    RETURN 0
END FUNCTION
```

---

## PHASE 3: ARCHITECTURE

### 3.1 Modular Makefile Structure

```
erlmcp/
├── Makefile                          # Main entry point (150 lines)
│   ├── include makefiles/*.mk
│   └── define canonical targets (doctor, quick, verify, ci-local)
│
├── makefiles/                        # Modular includes
│   ├── 00-config.mk                  # Constants, colors, env vars (50 lines)
│   ├── 01-dependencies.mk            # OTP check, rebar3, deps (80 lines)
│   ├── 02-compile.mk                 # Compilation targets (120 lines)
│   ├── 03-test.mk                    # Test targets (150 lines)
│   ├── 04-quality-gates.mk           # Validate-* targets (200 lines)
│   ├── 05-tcps.mk                    # TCPS manufacturing (120 lines)
│   ├── 06-governance.mk              # Hooks, settings validation (150 lines)
│   ├── 07-cli.mk                     # CLI versioning, release (180 lines)
│   ├── 08-benchmarks.mk              # Benchmarking targets (100 lines)
│   ├── 09-metrics.mk                 # Quality metrics (80 lines)
│   ├── 10-examples.mk                # Example runners (50 lines)
│   ├── 11-release.mk                 # Release targets (60 lines)
│   ├── 12-cleanup.mk                 # Clean, distclean (40 lines)
│   └── 99-help.mk                    # Help documentation (100 lines)
│
└── .erlmcp/
    ├── logs/                         # Centralized logging
    │   ├── compile.log
    │   ├── test.log
    │   └── gates/
    │       ├── gate-compile.log
    │       ├── gate-test.log
    │       └── gate-coverage.log
    │
    ├── locks/                        # File-level locks for parallelism
    │   ├── core.lock
    │   ├── transports.lock
    │   └── observability.lock
    │
    ├── state/                        # Persistent state
    │   ├── last-run.json
    │   └── baseline-performance.json
    │
    └── receipts/                     # Quality receipts (TCPS)
        └── release-2.1.0.receipt
```

**Rationale**:
- **Main Makefile**: Thin orchestrator (150 lines), delegates to modules
- **Modules**: Single responsibility, ≤200 lines each, independently testable
- **Logging**: Centralized in `.erlmcp/logs/` (not `/tmp/`, survives reboots)
- **Locks**: Explicit file locks for parallel safety
- **State**: Persistent state for incremental builds, baseline tracking

### 3.2 State Machine: Quality Gate Lifecycle

```
┌────────────────────────────────────────────────────────────────┐
│                     QUALITY GATE STATE MACHINE                 │
└────────────────────────────────────────────────────────────────┘

States: PENDING → RUNNING → PASSED | FAILED | SKIPPED

Transitions:
  PENDING    → RUNNING     (gate starts)
  RUNNING    → PASSED      (exit 0)
  RUNNING    → FAILED      (exit 1)
  RUNNING    → SKIPPED     (gate disabled or pre-condition failed)
  FAILED     → PENDING     (manual intervention: make andon-clear)

State Persistence:
  .erlmcp/state/gates.json:
  {
    "compile": {"status": "PASSED", "timestamp": 1704139200, "duration": 30},
    "test": {"status": "RUNNING", "timestamp": 1704139230, "duration": null},
    "coverage": {"status": "PENDING", "timestamp": null, "duration": null}
  }

Invariants:
  - ∀gate. status(gate) ∈ {PENDING, RUNNING, PASSED, FAILED, SKIPPED}
  - ∀gate₁, gate₂. depends(gate₂, gate₁) → (status(gate₁) = PASSED) ⊢ start(gate₂)
  - ∀gate. status(gate) = FAILED → status(downstream_gates) = SKIPPED
```

### 3.3 Coordination Mechanism: Dependency DAG

```
┌────────────────────────────────────────────────────────────────┐
│                     DEPENDENCY DAG (Directed Acyclic Graph)    │
└────────────────────────────────────────────────────────────────┘

Nodes: Make targets
Edges: Dependencies (A → B means "B depends on A")

Example DAG for `make validate`:

                   check-erlang-version
                            │
                      setup-profile
                            │
                        compile
                       ╱    │    ╲
                      ╱     │     ╲
              compile-core  │  compile-transports
                            │
                         test
                       ╱    │    ╲
                      ╱     │     ╲
                  eunit     ct   coverage
                            │
                      validate-test
                            │
                  ┌─────────┼─────────┐
                  │         │         │
            dialyzer      xref   coverage-check
                  │         │         │
                  └─────────┼─────────┘
                            │
                      validate-quality
                            │
                      validate-bench
                            │
                        validate

Parallelization Opportunities:
  - compile-{core, transports, observability, tcps} (4-way parallel)
  - {dialyzer, xref, coverage-check} (3-way parallel)

Implementation:
  - Use Make's built-in dependency resolution
  - Add `.NOTPARALLEL:` to targets requiring serial execution
  - Use file locks for shared resources (e.g., _build/ directory)
```

### 3.4 Test Infrastructure

**Test Strategy**:
1. **Unit Tests**: Each `.mk` module independently testable
2. **Integration Tests**: Canonical workflows (doctor, quick, verify, ci-local)
3. **Regression Tests**: Performance baselines, output format validation

**Test Harness** (`tests/makefile-tests/`):

```bash
#!/usr/bin/env bash
# tests/makefile-tests/test-compile-module.sh

set -euo pipefail

source tests/makefile-tests/test-framework.sh

test_compile_core_success() {
    # Setup
    setup_clean_environment

    # Execute
    make compile-core > /tmp/test-output.log 2>&1
    exit_code=$?

    # Assert
    assert_equals "Exit code" 0 "$exit_code"
    assert_file_exists "_build/default/lib/erlmcp_core/ebin/erlmcp_core.app"
    assert_log_contains "/tmp/test-output.log" "✓ erlmcp_core compiled"
}

test_compile_core_failure_missing_deps() {
    # Setup
    rm -rf _build/default/lib/jsx  # Simulate missing dependency

    # Execute
    make compile-core > /tmp/test-output.log 2>&1
    exit_code=$?

    # Assert
    assert_not_equals "Exit code" 0 "$exit_code"
    assert_log_contains "/tmp/test-output.log" "ERROR"
}

# Run all tests
run_tests \
    test_compile_core_success \
    test_compile_core_failure_missing_deps
```

**Test Framework** (`tests/makefile-tests/test-framework.sh`):

```bash
#!/usr/bin/env bash
# tests/makefile-tests/test-framework.sh

assert_equals() {
    local name="$1"
    local expected="$2"
    local actual="$3"

    if [ "$expected" != "$actual" ]; then
        echo "❌ FAIL: $name"
        echo "   Expected: $expected"
        echo "   Actual:   $actual"
        exit 1
    fi
    echo "✓ PASS: $name"
}

assert_file_exists() {
    local file="$1"
    if [ ! -f "$file" ]; then
        echo "❌ FAIL: File not found: $file"
        exit 1
    fi
    echo "✓ PASS: File exists: $file"
}

assert_log_contains() {
    local log_file="$1"
    local pattern="$2"
    if ! grep -q "$pattern" "$log_file"; then
        echo "❌ FAIL: Log does not contain: $pattern"
        exit 1
    fi
    echo "✓ PASS: Log contains: $pattern"
}

run_tests() {
    local total=0
    local passed=0

    for test_func in "$@"; do
        total=$((total + 1))
        echo "Running: $test_func"
        if $test_func; then
            passed=$((passed + 1))
        fi
    done

    echo ""
    echo "Results: $passed / $total passed"

    if [ "$passed" -ne "$total" ]; then
        exit 1
    fi
}

setup_clean_environment() {
    rm -rf _build/
    rebar3 get-deps > /dev/null 2>&1
}
```

### 3.5 Module Design: Example (`makefiles/02-compile.mk`)

```makefile
# makefiles/02-compile.mk
# Purpose: Compilation targets (all apps)
# Dependencies: 01-dependencies.mk (check-erlang-version, setup-profile)
# Exports: compile, compile-{core,transports,observability,tcps}

.PHONY: compile compile-core compile-transports compile-observability compile-tcps

# Master compile target (umbrella, all apps)
compile: check-erlang-version setup-profile
	@echo "$(BLUE)Compiling all apps...$(NC)"
	@TERM=dumb rebar3 compile 2>&1 | tee $(LOG_DIR)/compile.log
	@echo "$(GREEN)✓ Compilation complete$(NC)"

# Parallel compile helper (used by `make compile-parallel`)
compile-parallel: check-erlang-version setup-profile
	@echo "$(BLUE)Compiling all apps (parallel)...$(NC)"
	@$(MAKE) -j4 compile-core compile-transports compile-observability compile-tcps
	@echo "$(GREEN)✓ Parallel compilation complete$(NC)"

# Individual app compilation (isolated, lockable)
compile-core:
	@$(call compile_app,erlmcp_core)

compile-transports:
	@$(call compile_app,erlmcp_transports)

compile-observability:
	@$(call compile_app,erlmcp_observability)

compile-tcps:
	@$(call compile_app,tcps_erlmcp)

# Helper function: compile single app with locking
define compile_app
	@echo "$(BLUE)Compiling $(1)...$(NC)"
	@mkdir -p $(LOCK_DIR)
	@(
		flock -n 200 || (echo "$(YELLOW)⚠ Waiting for lock: $(1)$(NC)" && flock 200); \
		cd apps/$(1) && rebar3 compile 2>&1 | tee $(LOG_DIR)/compile-$(1).log; \
		exit_code=$$?; \
		if [ $$exit_code -eq 0 ]; then \
			echo "$(GREEN)✓ $(1) compiled$(NC)"; \
		else \
			echo "$(RED)✗ $(1) compilation failed$(NC)"; \
			exit 1; \
		fi
	) 200>$(LOCK_DIR)/$(1).lock
endef

# Incremental compilation (only changed files)
compile-incremental:
	@echo "$(BLUE)Incremental compilation (changed files only)...$(NC)"
	@./scripts/build/incremental-compile.sh

# Clean compiled artifacts
compile-clean:
	@echo "$(BLUE)Cleaning compiled artifacts...$(NC)"
	@rebar3 clean
	@rm -f $(LOG_DIR)/compile*.log
	@echo "$(GREEN)✓ Compile artifacts cleaned$(NC)"
```

---

## PHASE 4: REFINEMENT

### 4.1 Task Breakdown

**Task Hierarchy** (23 tasks, 6 milestones):

```
MILESTONE 1: Foundation (Days 1-2, 3 tasks)
├── TASK-001: Create makefiles/ directory structure
├── TASK-002: Extract configuration to 00-config.mk
└── TASK-003: Create logging infrastructure (.erlmcp/logs/)

MILESTONE 2: Core Modules (Days 3-5, 5 tasks)
├── TASK-004: Refactor dependencies to 01-dependencies.mk
├── TASK-005: Refactor compilation to 02-compile.mk
├── TASK-006: Refactor testing to 03-test.mk
├── TASK-007: Refactor quality gates to 04-quality-gates.mk
└── TASK-008: Update main Makefile to include modules

MILESTONE 3: Specialized Modules (Days 6-8, 5 tasks)
├── TASK-009: Refactor TCPS to 05-tcps.mk
├── TASK-010: Refactor governance to 06-governance.mk
├── TASK-011: Refactor CLI to 07-cli.mk
├── TASK-012: Refactor benchmarks to 08-benchmarks.mk
└── TASK-013: Refactor metrics to 09-metrics.mk

MILESTONE 4: Support Modules (Days 9-10, 3 tasks)
├── TASK-014: Refactor examples to 10-examples.mk
├── TASK-015: Refactor release to 11-release.mk
└── TASK-016: Refactor cleanup to 12-cleanup.mk

MILESTONE 5: Testing & Validation (Days 11-13, 4 tasks)
├── TASK-017: Create test framework (tests/makefile-tests/)
├── TASK-018: Write unit tests for each module
├── TASK-019: Write integration tests (doctor, quick, verify, ci-local)
└── TASK-020: Validate backward compatibility (all 110 targets)

MILESTONE 6: Documentation & Rollout (Days 14-15, 3 tasks)
├── TASK-021: Update help system (99-help.mk)
├── TASK-022: Generate migration guide (MAKEFILE_MIGRATION.md)
└── TASK-023: Staged rollout (branch → PR → merge → monitor)
```

### 4.2 Task Dependencies

```
Dependency Graph (topological sort):

TASK-001 (directory structure)
    ↓
TASK-002 (00-config.mk)
    ↓
TASK-003 (logging infrastructure)
    ↓
┌───┴───┬───────┬───────┬───────┐
│       │       │       │       │
004     005     006     007     008  (core modules)
│       │       │       │       │
└───┬───┴───────┴───────┴───────┘
    ↓
┌───┴───┬───────┬───────┬───────┐
│       │       │       │       │
009     010     011     012     013  (specialized modules)
│       │       │       │       │
└───┬───┴───────┴───────┴───────┘
    ↓
┌───┴───┬───────┐
│       │       │
014     015     016  (support modules)
│       │       │
└───┬───┴───────┘
    ↓
017 (test framework)
    ↓
┌───┴───┬───────┐
│       │       │
018     019     020  (testing)
│       │       │
└───┬───┴───────┘
    ↓
┌───┴───┬───────┐
│       │       │
021     022     023  (documentation & rollout)

Critical Path: 001 → 002 → 003 → 004 → 008 → 017 → 019 → 023 (8 tasks)
```

### 4.3 Detailed Task Requirements

#### TASK-001: Create makefiles/ directory structure
- **Agent**: erlang-architect
- **Priority**: HIGH
- **Effort**: 1 hour
- **Risk**: LOW
- **Requirements**:
  - Create `makefiles/` directory
  - Create placeholder `.mk` files (00-12, 99)
  - Add `.gitignore` for generated files
  - Validate directory structure with `ls -la makefiles/`
- **Acceptance**:
  - Directory exists: `[ -d makefiles/ ]`
  - 14 `.mk` files created
  - Git tracks structure

#### TASK-002: Extract configuration to 00-config.mk
- **Agent**: erlang-otp-developer
- **Priority**: HIGH
- **Effort**: 2 hours
- **Risk**: LOW
- **Requirements**:
  - Extract colors (BLUE, CYAN, GREEN, YELLOW, RED, BOLD, NC)
  - Extract paths (LOG_DIR, LOCK_DIR, STATE_DIR, RECEIPTS_DIR)
  - Extract constants (APPS, OTP_VERSION_MIN, COVERAGE_THRESHOLD)
  - Make all paths configurable via environment variables
- **Acceptance**:
  - `makefiles/00-config.mk` exists
  - No hardcoded paths in other modules
  - `make info` shows all config values

#### TASK-004: Refactor dependencies to 01-dependencies.mk
- **Agent**: erlang-otp-developer
- **Priority**: HIGH
- **Effort**: 3 hours
- **Risk**: MEDIUM
- **Requirements**:
  - Move `check-erlang-version` target
  - Move `setup-profile` target
  - Move `deps` target
  - Add dependency validation (check if rebar3 exists)
  - Add caching logic (skip `get-deps` if already fetched)
- **Acceptance**:
  - `make check-erlang-version` works identically
  - `make setup-profile` works identically
  - `make deps` idempotent (no re-fetch if exists)
  - 100% backward compatible

#### TASK-005: Refactor compilation to 02-compile.mk
- **Agent**: erlang-otp-developer
- **Priority**: HIGH
- **Effort**: 4 hours
- **Risk**: HIGH
- **Requirements**:
  - Move all `compile*` targets
  - Implement `compile_app` function with locking
  - Add parallel compilation support (`compile-parallel`)
  - Add incremental compilation (`compile-incremental`)
  - Preserve TERM=dumb for cloud compatibility
- **Acceptance**:
  - `make compile` works identically
  - `make compile-core` works identically
  - `make compile-parallel` 2x faster than `make compile`
  - No race conditions (verified with 100 parallel runs)
  - Logs written to `.erlmcp/logs/compile*.log`

#### TASK-008: Update main Makefile to include modules
- **Agent**: erlang-architect
- **Priority**: CRITICAL
- **Effort**: 2 hours
- **Risk**: MEDIUM
- **Requirements**:
  - Replace inline code with `include makefiles/*.mk`
  - Preserve canonical targets (doctor, quick, verify, ci-local)
  - Ensure correct include order (00 → 01 → 02 → ... → 99)
  - Test all 110 targets for backward compatibility
- **Acceptance**:
  - Main Makefile ≤150 lines
  - All 110 targets work identically
  - `make help` shows all targets
  - No duplicate target definitions

#### TASK-017: Create test framework
- **Agent**: erlang-test-engineer
- **Priority**: HIGH
- **Effort**: 4 hours
- **Risk**: MEDIUM
- **Requirements**:
  - Create `tests/makefile-tests/` directory
  - Implement `test-framework.sh` with assertion library
  - Create test harness for each module
  - Add `make test-makefile` target to run all tests
- **Acceptance**:
  - Test framework supports: `assert_equals`, `assert_file_exists`, `assert_log_contains`
  - `make test-makefile` runs all tests
  - Exit code 0 if all pass, 1 if any fail
  - Coverage: 100% of modules tested

#### TASK-019: Write integration tests
- **Agent**: erlang-test-engineer
- **Priority**: HIGH
- **Effort**: 6 hours
- **Risk**: HIGH
- **Requirements**:
  - Test `make doctor` (healthy env, missing OTP, missing deps)
  - Test `make quick` (success, compile failure, test failure)
  - Test `make verify` (success, dialyzer failure)
  - Test `make ci-local` (success, coverage failure)
  - Measure performance (ensure <5% regression)
- **Acceptance**:
  - 12+ integration test cases
  - All canonical workflows tested
  - Performance baselines established
  - CI/CD integration verified

#### TASK-023: Staged rollout
- **Agent**: erlang-github-ops
- **Priority**: CRITICAL
- **Effort**: 2 hours
- **Risk**: HIGH
- **Requirements**:
  - Create feature branch: `refactor/makefile-modularization`
  - Run full test suite (unit + integration + regression)
  - Create PR with SPARC spec as description
  - Merge with squash commit
  - Monitor CI/CD for 24 hours post-merge
- **Acceptance**:
  - PR approved by 2+ reviewers
  - All CI/CD checks pass
  - No rollback required within 24 hours
  - Post-merge metrics: compile time, test time, CI/CD duration

### 4.4 Effort Estimates & Risk Assessment

| Task | Agent | Effort (hrs) | Risk | Blocker? | Dependencies |
|------|-------|--------------|------|----------|--------------|
| 001 | architect | 1 | LOW | NO | - |
| 002 | developer | 2 | LOW | NO | 001 |
| 003 | developer | 2 | LOW | NO | 001 |
| 004 | developer | 3 | MED | NO | 002 |
| 005 | developer | 4 | HIGH | YES | 002, 003, 004 |
| 006 | developer | 4 | HIGH | YES | 002, 003, 004 |
| 007 | developer | 5 | HIGH | YES | 002, 003, 004, 005, 006 |
| 008 | architect | 2 | MED | YES | 004-007 |
| 009 | developer | 3 | MED | NO | 008 |
| 010 | developer | 3 | MED | NO | 008 |
| 011 | developer | 3 | MED | NO | 008 |
| 012 | developer | 2 | LOW | NO | 008 |
| 013 | developer | 2 | LOW | NO | 008 |
| 014 | developer | 1 | LOW | NO | 008 |
| 015 | developer | 2 | LOW | NO | 008 |
| 016 | developer | 1 | LOW | NO | 008 |
| 017 | test-engineer | 4 | MED | YES | 008 |
| 018 | test-engineer | 6 | MED | YES | 017 |
| 019 | test-engineer | 6 | HIGH | YES | 017, 018 |
| 020 | test-engineer | 4 | HIGH | YES | 017, 018, 019 |
| 021 | developer | 2 | LOW | NO | 020 |
| 022 | developer | 2 | LOW | NO | 020 |
| 023 | github-ops | 2 | HIGH | YES | 020, 021, 022 |
| **TOTAL** | - | **68 hrs** | - | **8 blockers** | - |

**Timeline**:
- 1 developer @ 8 hrs/day = 8.5 days
- 2 developers @ 8 hrs/day = 4.25 days (parallelizable after TASK-008)
- **Recommended**: 2 developers, 5 days (with buffer)

**Risk Mitigation**:
- **HIGH RISK (5 tasks)**: 005, 006, 007, 019, 023
  - Mitigation: Pair programming, extra code review, rollback plan
- **MEDIUM RISK (7 tasks)**: 004, 008, 009, 010, 011, 017, 018
  - Mitigation: Unit tests, integration tests, staged rollout
- **LOW RISK (11 tasks)**: All others
  - Mitigation: Standard development process

---

## PHASE 5: COMPLETION

### 5.1 Acceptance Criteria (Per Task)

**TASK-001**: ✅ Directory structure created
- [ ] `makefiles/` directory exists
- [ ] 14 `.mk` files created (placeholders OK)
- [ ] Git tracks structure (`git status` shows new files)

**TASK-005**: ✅ Compilation refactored
- [ ] `make compile` produces identical output
- [ ] `make compile-core` works
- [ ] `make compile-parallel` completes in <60% of serial time
- [ ] No race conditions (verified with `seq 100 | xargs -P 10 -I {} make compile-parallel`)
- [ ] Logs written to `.erlmcp/logs/compile*.log`
- [ ] File locks acquired/released correctly (`ls -la .erlmcp/locks/`)

**TASK-019**: ✅ Integration tests passed
- [ ] 12+ test cases implemented
- [ ] `make test-makefile` exits 0 (all tests pass)
- [ ] Performance regression <5% (compare to baseline)
- [ ] CI/CD integration verified (locally reproduce CI workflow)

**TASK-023**: ✅ Staged rollout complete
- [ ] Feature branch merged to main
- [ ] All CI/CD checks passed post-merge
- [ ] No rollback required within 24 hours
- [ ] Metrics collected: compile time, test time, CI/CD duration

### 5.2 Validation Approach

**Validation Levels**:
1. **Unit Validation**: Each `.mk` module independently validated
2. **Integration Validation**: Canonical workflows (doctor, quick, verify, ci-local) validated
3. **Regression Validation**: All 110 original targets validated
4. **Performance Validation**: Baseline comparison (compile time, test time, gate time)

**Validation Harness** (`tests/makefile-tests/validate-refactoring.sh`):

```bash
#!/usr/bin/env bash
# tests/makefile-tests/validate-refactoring.sh

set -euo pipefail

echo "════════════════════════════════════════════════════════════"
echo "Makefile Refactoring Validation Suite"
echo "════════════════════════════════════════════════════════════"
echo ""

# Phase 1: Unit validation (each module)
echo "[1/4] Unit Validation..."
for module in makefiles/*.mk; do
    echo "  Validating: $(basename $module)"
    make -f $module --dry-run > /dev/null 2>&1 || {
        echo "  ✗ FAIL: $module has syntax errors"
        exit 1
    }
    echo "  ✓ PASS: $(basename $module)"
done
echo ""

# Phase 2: Integration validation (canonical workflows)
echo "[2/4] Integration Validation..."
for target in doctor quick verify ci-local; do
    echo "  Testing: make $target (dry-run)"
    make $target --dry-run > /dev/null 2>&1 || {
        echo "  ✗ FAIL: make $target failed"
        exit 1
    }
    echo "  ✓ PASS: make $target"
done
echo ""

# Phase 3: Regression validation (all 110 targets)
echo "[3/4] Regression Validation..."
targets=$(make -qp | awk -F':' '/^[a-zA-Z0-9][^$#\/\t=]*:([^=]|$)/ {split($1,A,/ /);for(i in A)print A[i]}' | sort -u)
total=0
passed=0
for target in $targets; do
    total=$((total + 1))
    if make $target --dry-run > /dev/null 2>&1; then
        passed=$((passed + 1))
    else
        echo "  ✗ FAIL: make $target"
    fi
done
echo "  Results: $passed / $total targets passed"
if [ "$passed" -ne "$total" ]; then
    echo "  ✗ FAIL: Some targets broken"
    exit 1
fi
echo ""

# Phase 4: Performance validation
echo "[4/4] Performance Validation..."
if [ ! -f .baseline_performance.txt ]; then
    echo "  ⚠ No baseline found, skipping performance validation"
else
    echo "  Comparing against baseline..."
    ./tests/makefile-tests/compare-performance.sh || {
        echo "  ✗ FAIL: Performance regression detected"
        exit 1
    }
    echo "  ✓ PASS: No performance regression"
fi
echo ""

echo "════════════════════════════════════════════════════════════"
echo "✅ All validation passed"
echo "════════════════════════════════════════════════════════════"
```

### 5.3 Staged Rollout Plan

**Phase 1: Development Branch** (Days 1-13)
- Work on feature branch: `refactor/makefile-modularization`
- Commit after each task (atomic commits)
- Run validation suite after each commit
- Push daily to remote for backup

**Phase 2: Local Testing** (Day 14)
- Run full test suite: `make test-makefile`
- Run integration tests: `tests/makefile-tests/validate-refactoring.sh`
- Run regression tests: compare all 110 targets
- Measure performance: establish baseline, compare to old Makefile
- Fix any issues discovered

**Phase 3: PR Creation** (Day 14)
- Create PR with SPARC spec as description
- Request reviews from: erlang-architect, erlang-otp-developer, code-reviewer
- Address review comments
- Ensure CI/CD checks pass

**Phase 4: Merge** (Day 15)
- Merge with squash commit (single commit to main)
- Tag commit: `v2.2.0-makefile-refactoring`
- Monitor CI/CD for 1 hour post-merge (ensure no breakage)

**Phase 5: Post-Merge Monitoring** (Days 15-16)
- Monitor CI/CD for 24 hours
- Watch for:
  - Build failures
  - Test failures
  - Performance regressions
  - Developer complaints (ergonomics)
- If issues: hot-fix or rollback

**Rollback Plan**:
```bash
# If critical issue discovered post-merge
git revert <merge-commit-sha>
git push origin main
make clean
make compile test  # Verify rollback works
```

### 5.4 Migration Strategy

**For Developers**:
- **Backward Compatibility**: All existing `make` commands work identically
- **New Features**: Access via `make help` (shows new targets like `compile-parallel`)
- **Migration Guide**: `MAKEFILE_MIGRATION.md` documents changes

**For CI/CD**:
- **No Changes Required**: All CI/CD workflows use same commands
- **Performance Gains**: CI/CD may run faster due to parallel compilation
- **Monitoring**: Track CI/CD duration metrics for 7 days post-merge

**For Scripts**:
- **Audit**: Search codebase for `make` invocations
- **Test**: Run all scripts that invoke `make` targets
- **Update**: If any scripts break, update to use new targets

**Migration Checklist**:
- [ ] All 110 original targets preserved
- [ ] CI/CD workflows unchanged
- [ ] Pre-commit hooks unchanged
- [ ] Scripts invoking `make` tested
- [ ] Documentation updated (README.md, DEVELOPMENT.md)
- [ ] CHANGELOG.md entry added

### 5.5 Success Metrics

**Quantitative**:
- **Lines of Code**: 1499 → <600 (main Makefile), 900 split across modules
- **Targets**: 110 preserved, 0 broken
- **Compilation Time**: ±5% tolerance (no regression)
- **Test Time**: ±5% tolerance (no regression)
- **Parallel Speedup**: `compile-parallel` 2x faster than `compile`
- **Maintainability**: Average module size <200 lines

**Qualitative**:
- **Readability**: Code reviewers rate 4+/5 on clarity
- **Testability**: 100% of modules covered by unit tests
- **Extensibility**: New target addition <30 minutes (vs 2+ hours previously)
- **Developer Satisfaction**: Survey shows 80%+ approval

### 5.6 Post-Completion Tasks

**Documentation**:
- [ ] Update `DEVELOPMENT.md` with new Makefile structure
- [ ] Add `MAKEFILE_ARCHITECTURE.md` (reference for future maintainers)
- [ ] Update `CONTRIBUTING.md` with Makefile contribution guidelines

**Knowledge Transfer**:
- [ ] Demo session with team (30 min)
- [ ] Q&A session (30 min)
- [ ] Office hours (2 hours) for questions

**Future Enhancements**:
- [ ] Add `make watch` target (auto-recompile on file changes)
- [ ] Add `make profile` target (fprof/eflame integration)
- [ ] Add `make deps-update` target (automated dependency updates)
- [ ] Add `make security-scan` target (vulnerability scanning)

---

## APPENDIX A: Make Include Order

```makefile
# Makefile (main entry point)

# Phase 1: Configuration
include makefiles/00-config.mk         # Constants, colors, paths

# Phase 2: Dependencies
include makefiles/01-dependencies.mk   # OTP check, rebar3, deps

# Phase 3: Core Build
include makefiles/02-compile.mk        # Compilation targets
include makefiles/03-test.mk           # Test targets

# Phase 4: Quality
include makefiles/04-quality-gates.mk  # Validate-* targets

# Phase 5: Specialized
include makefiles/05-tcps.mk           # TCPS manufacturing
include makefiles/06-governance.mk     # Hooks, settings validation
include makefiles/07-cli.mk            # CLI versioning, release
include makefiles/08-benchmarks.mk     # Benchmarking targets
include makefiles/09-metrics.mk        # Quality metrics

# Phase 6: Support
include makefiles/10-examples.mk       # Example runners
include makefiles/11-release.mk        # Release targets
include makefiles/12-cleanup.mk        # Clean, distclean

# Phase 7: Help
include makefiles/99-help.mk           # Help documentation
```

**Rationale**: Numeric prefixes enforce load order, preventing forward references.

---

## APPENDIX B: Armstrong Compliance Matrix

| Principle | Current Makefile | Refactored Makefile | Evidence |
|-----------|------------------|---------------------|----------|
| **Isolation** | ❌ Mixed concerns (compile + test + quality in one file) | ✅ Isolated modules (single responsibility) | 14 `.mk` files, each <200 lines |
| **Supervision** | ⚠️ Implicit dependencies (hard to trace) | ✅ Explicit dependency DAG | Dependency graph documented, validated with `make --dry-run` |
| **Let-It-Crash** | ✅ `exit 1` on failure (preserved) | ✅ Unchanged | All gates still exit 1 on failure |
| **Black-Box Testing** | ❌ No tests for Makefile logic | ✅ Test harness validates observable behavior | `tests/makefile-tests/` suite |
| **Determinism** | ⚠️ Temp files (`/tmp/erlmcp_*.log`) not persistent | ✅ Persistent logs (`.erlmcp/logs/`) | Logs survive reboots, reproducible |
| **Idempotency** | ⚠️ Some targets not idempotent (`andon-clear`) | ✅ All targets idempotent | `andon-clear` checks state before clearing |
| **Cloud Safety** | ⚠️ Not all targets cloud-verified | ✅ All targets cloud-safe | `TERM=dumb` preserved, no interactive prompts |

---

## APPENDIX C: File Locks for Parallelism

**Lock Protocol**:
```bash
# Acquire lock (blocking)
flock 200

# Acquire lock (non-blocking, wait if locked)
flock -n 200 || (echo "Waiting..." && flock 200)

# Critical section
do_work

# Release lock (implicit on fd close)
```

**Lock File Locations**:
- `.erlmcp/locks/core.lock` → `compile-core`
- `.erlmcp/locks/transports.lock` → `compile-transports`
- `.erlmcp/locks/observability.lock` → `compile-observability`
- `.erlmcp/locks/tcps.lock` → `compile-tcps`

**Why File Locks?**:
- **POSIX Compliance**: `flock` available on all Unix-like systems
- **Process Isolation**: Kernel-enforced, no race conditions
- **Cleanup**: Auto-released on process exit (no leaked locks)

---

## APPENDIX D: Pseudocode for Incremental Compilation

```pseudocode
FUNCTION compile_incremental()
    // Phase 1: Detect changed files
    last_compile_time = READ(".erlmcp/state/last-compile-time.txt")
    changed_files = GIT_DIFF(last_compile_time, NOW())

    IF changed_files.EMPTY() THEN
        LOG("No changes detected, skipping compilation")
        RETURN 0
    END IF

    // Phase 2: Determine affected apps
    affected_apps = SET()
    FOR EACH file IN changed_files DO
        IF file.STARTS_WITH("apps/erlmcp_core") THEN
            affected_apps.ADD("core")
        ELSE IF file.STARTS_WITH("apps/erlmcp_transports") THEN
            affected_apps.ADD("transports")
        // ... (similar for observability, tcps)
        END IF
    END FOR

    // Phase 3: Compile only affected apps
    LOG("Compiling affected apps: " + affected_apps)
    FOR EACH app IN affected_apps DO
        RUN("make compile-" + app)
    END FOR

    // Phase 4: Update last compile time
    WRITE(".erlmcp/state/last-compile-time.txt", NOW())

    LOG("✅ Incremental compilation complete")
    RETURN 0
END FUNCTION
```

---

## APPENDIX E: TCPS Integration

**Requirement**: Preserve all TCPS quality gates (jidoka, andon, poka-yoke)

**Implementation** (`makefiles/05-tcps.mk`):
```makefile
# makefiles/05-tcps.mk
# Purpose: TCPS (Toyota Code Production System) manufacturing targets
# Dependencies: 04-quality-gates.mk (validate)

.PHONY: jidoka andon poka-yoke tcps-quality-gates andon-clear andon-watch release-validate

jidoka:
	@echo ""
	@echo "$(BOLD)$(GREEN)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(GREEN)🏭 自働化 (JIDOKA) QUALITY GATE$(NC)"
	@echo "$(BOLD)$(GREEN)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@if [ -f tools/tcps/jidoka_quality_gate.sh ]; then \
		./tools/tcps/jidoka_quality_gate.sh; \
	else \
		$(MAKE) validate; \
	fi

andon:
	@echo ""
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo "$(BOLD)$(CYAN)🚨 行灯 (ANDON) BOARD STATUS$(NC)"
	@echo "$(BOLD)$(CYAN)════════════════════════════════════════════════════════════$(NC)"
	@echo ""
	@if [ -f .erlmcp/state/andon.status ]; then \
		cat .erlmcp/state/andon.status; \
	else \
		echo "$(GREEN)✓ No issues detected$(NC)"; \
	fi

andon-clear:
	@echo "$(BOLD)$(CYAN)Clearing Andon Status...$(NC)"
	@rm -f .erlmcp/state/andon.status
	@echo "$(GREEN)✓ Andon cleared$(NC)"

# ... (similar for poka-yoke, tcps-quality-gates, release-validate)
```

**Key Preservation**:
- ✅ All TCPS targets preserved
- ✅ `tools/tcps/` scripts unchanged
- ✅ Receipt generation unchanged
- ✅ Andon state moved to `.erlmcp/state/` (persistent)

---

## SUMMARY

**SPARC Methodology Applied**:
1. ✅ **Specification**: Problem, success criteria, constraints, invariants defined
2. ✅ **Pseudocode**: Algorithms for doctor, quick/verify/ci-local, failure recovery, parallelism designed
3. ✅ **Architecture**: Modular structure (14 `.mk` files), state machine (quality gates), dependency DAG, test infrastructure designed
4. ✅ **Refinement**: 23 tasks, 6 milestones, dependencies, effort estimates, risk assessment detailed
5. ✅ **Completion**: Acceptance criteria, validation approach, staged rollout, migration strategy, success metrics defined

**Deliverables**:
- 📄 This SPARC specification document
- 📦 Modular Makefile architecture (14 `.mk` files)
- 🧪 Test framework (`tests/makefile-tests/`)
- 📊 Validation harness (`validate-refactoring.sh`)
- 📘 Migration guide (`MAKEFILE_MIGRATION.md`)
- 📈 Success metrics dashboard

**Next Steps**:
1. Review SPARC spec with team
2. Approve/modify specification
3. Assign tasks to agents (erlang-architect, erlang-otp-developer, erlang-test-engineer, erlang-github-ops)
4. Execute tasks (Days 1-15)
5. Merge feature branch
6. Monitor post-merge (24 hours)
7. Celebrate 🎉

---

**Document Version**: 1.0.0
**Status**: READY FOR REVIEW
**Estimated Completion**: 5 days (2 developers, parallel execution)
**Risk Level**: MEDIUM-HIGH (8 blocking tasks, comprehensive testing mitigates)
**Armstrong Compliance**: ✅ ALL PRINCIPLES PRESERVED

---
