# Quality Gate Integration Tests

**Status**: Production-Ready Test Suite
**Version**: v0.6.0
**Test Framework**: Common Test (Chicago School TDD)
**Coverage Target**: 85%+

## Overview

Comprehensive integration test suite for TCPS Quality Gate Enforcement System. All tests follow Chicago School TDD principles:

- **Real processes**: No mocks, spawn actual gen_servers and processes
- **State-based verification**: Assert on observable state, not interactions
- **Real I/O**: Use real files, git repos, compilers
- **Integration focus**: Test components together

## Test Suites

### 1. Quality Gates Test Suite (`quality_gates_SUITE.erl`)

**Purpose**: Tests core quality gate enforcement with real compilation, testing, and validation.

**Test Cases** (10 total):

| Test Case | Description | Verification |
|-----------|-------------|--------------|
| `gate_detects_compilation_failure_test` | Real compiler detects syntax errors | Fail with violation details |
| `gate_detects_test_failure_test` | Real EUnit detects failing tests | Pass rate < 95% triggers fail |
| `gate_detects_coverage_drop_test` | Real coverage analysis detects drops | <80% coverage triggers fail |
| `gate_blocks_on_failure_test` | Gate blocks stage transition | `can_proceed/2` returns `false` |
| `gate_allows_on_pass_test` | Gate allows transition on pass | `can_proceed/2` returns `true` |
| `gate_generates_receipt_test` | Receipt with SHA-256 hash generated | Immutable audit trail created |
| `gate_integrates_with_tcps_test` | TCPS workflow integration | Stage transitions tracked |
| `multiple_gates_cascade_failure_test` | Multiple gate failures cascade | All gate results captured |
| `gate_timeout_handling_test` | Gate respects timeout limits | Timeout handled gracefully |
| `gate_recovery_after_fix_test` | Gate passes after code fix | Fail → Fix → Pass cycle |

**Quality Standards Enforced**:
- Compilation: 0 errors
- Test pass rate: ≥95%
- Code coverage: ≥80%
- Receipt generation: SHA-256 hash required

**Run Tests**:
```bash
rebar3 ct --suite=test/quality_gates_SUITE
```

**Expected Output**:
```
=== Quality Gates Test Suite ===
✅ All 10 tests passed
✅ Zero compilation warnings
✅ Chicago School TDD compliant
✅ Real processes: gen_servers, compilers, file I/O
```

---

### 2. Hooks Integration Test Suite (`hooks_integration_SUITE.erl`)

**Purpose**: Tests git hooks and Claude Code post-task hooks with real git operations.

**Test Cases** (8 total):

| Test Case | Description | Verification |
|-----------|-------------|--------------|
| `pre_commit_hook_blocks_bad_code_test` | Real git pre-commit hook blocks | Commit rejected by hook |
| `post_task_hook_validates_completion_test` | Post-task validates all checks | Compilation, tests, coverage verified |
| `session_end_hook_generates_report_test` | Session report with metrics | JSON report file created |
| `hooks_work_with_git_test` | Real git commit with hooks | Hook execution in git workflow |
| `hook_failure_stops_pipeline_test` | Failed hook stops pipeline | Later stages not executed |
| `hook_success_allows_continuation_test` | Successful hooks allow pipeline | All stages complete |
| `hook_execution_order_test` | Hooks execute sequentially | Order: A → B → C verified |
| `hook_environment_variables_test` | Hooks read environment vars | TCPS_* variables captured |

**Hook Types Tested**:
- **Pre-commit**: Blocks bad code before commit
- **Post-task**: Validates Claude Code task completion
- **Session-end**: Generates session reports

**Run Tests**:
```bash
rebar3 ct --suite=test/hooks_integration_SUITE
```

**Expected Output**:
```
=== Hooks Integration Test Suite ===
✅ All 8 tests passed
✅ Real git operations: init, add, commit, log
✅ Hook scripts executed via bash
✅ Environment variables validated
```

---

### 3. Regression Detection Test Suite (`regression_detection_SUITE.erl`)

**Purpose**: Tests regression detection with historical baseline metrics.

**Test Cases** (9 total):

| Test Case | Description | Verification |
|-----------|-------------|--------------|
| `detects_test_pass_rate_drop_test` | Detects >5% pass rate drop | Critical regression flagged |
| `detects_coverage_decrease_test` | Detects >10% coverage drop | Critical severity |
| `detects_performance_regression_test` | Detects >10% throughput drop | Performance regression detected |
| `blocks_on_critical_regression_test` | Blocks on critical regressions | Stage transition blocked |
| `allows_with_justification_test` | Allows with valid justification | Approved override |
| `tracks_regression_history_test` | Tracks multiple regressions | Trend analysis (degrading) |
| `detects_multiple_regressions_test` | Detects 3+ regressions | All metrics analyzed |
| `regression_severity_classification_test` | Classifies severity correctly | Warning/Moderate/Critical/Blocker |
| `regression_recovery_tracking_test` | Tracks recovery time | Recovery duration measured |

**Regression Severity Thresholds**:
- **Warning**: 2-5% drop
- **Moderate**: 5-10% drop
- **Critical**: 10-20% drop
- **Blocker**: >20% drop

**Run Tests**:
```bash
rebar3 ct --suite=test/regression_detection_SUITE
```

**Expected Output**:
```
=== Regression Detection Test Suite ===
✅ All 9 tests passed
✅ Historical baseline comparison
✅ Severity classification accurate
✅ Recovery tracking functional
```

---

### 4. Auto-Fix System Test Suite (`auto_fix_SUITE.erl`)

**Purpose**: Tests automatic fix agent spawning and validation.

**Test Cases** (8 total):

| Test Case | Description | Verification |
|-----------|-------------|--------------|
| `dispatcher_routes_correctly_test` | Routes failures to correct agents | Syntax/Test/Coverage fixers |
| `fix_agents_spawn_correctly_test` | Real agent processes spawned | `is_process_alive/1` = true |
| `fixes_are_validated_test` | Fixes validated before acceptance | Real compilation check |
| `system_retries_on_failure_test` | Retries up to 3 times | Retry loop tracked |
| `escalation_works_test` | Escalates to human review | Escalation ticket created |
| `multiple_fixes_coordinated_test` | 3+ fixes coordinated | Parallel fix execution |
| `fix_timeout_handling_test` | Fix respects timeout | Timeout handled gracefully |
| `fix_rollback_on_validation_failure_test` | Rolls back invalid fixes | Original file restored |

**Fix Agent Types**:
- **syntax_fixer**: Compilation errors
- **test_fixer**: Test failures
- **coverage_fixer**: Coverage drops
- **generic_fixer**: Other issues

**Run Tests**:
```bash
rebar3 ct --suite=test/auto_fix_SUITE
```

**Expected Output**:
```
=== Auto-Fix System Test Suite ===
✅ All 8 tests passed
✅ Real agent processes spawned
✅ Fix validation with real compiler
✅ Retry and escalation logic verified
```

---

## Running All Integration Tests

### Full Suite Execution

```bash
# Run all 4 integration test suites (35 total tests)
rebar3 ct --suite=test/quality_gates_SUITE,test/hooks_integration_SUITE,test/regression_detection_SUITE,test/auto_fix_SUITE

# Expected output:
# ✅ 35/35 tests passed (0 failures)
# ✅ Chicago School TDD: Real processes, no mocks
# ✅ Quality gates: Compilation, tests, coverage
# ✅ Hooks: Git integration, post-task validation
# ✅ Regression: Detection, blocking, escalation
# ✅ Auto-fix: Routing, spawning, validation, retry
```

### Individual Suite Execution

```bash
# Quality gates only
rebar3 ct --suite=test/quality_gates_SUITE

# Hooks only
rebar3 ct --suite=test/hooks_integration_SUITE

# Regression detection only
rebar3 ct --suite=test/regression_detection_SUITE

# Auto-fix only
rebar3 ct --suite=test/auto_fix_SUITE
```

### Verbose Mode (Debug)

```bash
# Run with detailed logging
rebar3 ct --suite=test/quality_gates_SUITE --verbose

# View CT logs
cat _build/test/logs/ct_run.*/quality_gates_SUITE.logs/run.*/suite.log
```

---

## Expected Test Outcomes

### Success Criteria

**All tests MUST pass with:**
- ✅ 0 failures
- ✅ 0 errors
- ✅ 0 warnings
- ✅ Real process verification (no mock objects)
- ✅ State-based assertions (observable behavior)

**Quality Metrics**:
- Test pass rate: 100%
- Code coverage: 85%+ for quality gate modules
- Execution time: <2 minutes for full suite
- Memory: <500MB for all tests

### Failure Scenarios

If tests fail, check:

1. **Compilation errors**: Ensure `rebar3 compile` succeeds
2. **Missing dependencies**: `rebar3 tree` shows all deps
3. **File permissions**: Test directories writable
4. **Git configuration**: Git user.name and user.email set
5. **Application start**: `application:ensure_all_started(tcps_erlmcp)` succeeds

---

## Test Coverage Report

### Generate Coverage

```bash
# Run tests with coverage
rebar3 ct --cover --suite=test/quality_gates_SUITE,test/hooks_integration_SUITE,test/regression_detection_SUITE,test/auto_fix_SUITE

# View coverage report
rebar3 cover --verbose

# HTML report
open _build/test/cover/index.html
```

### Coverage Targets

| Module | Target | Actual | Status |
|--------|--------|--------|--------|
| `tcps_quality_gates.erl` | 85% | TBD | ⏳ Pending |
| `tcps_quality_receipt_verifier.erl` | 85% | TBD | ⏳ Pending |
| `tcps_rebar3_quality.erl` | 80% | TBD | ⏳ Pending |
| Hook modules | 80% | TBD | ⏳ Pending |

---

## CI/CD Integration

### GitHub Actions Workflow

Add to `.github/workflows/quality-gate.yml`:

```yaml
name: Quality Gate Integration Tests

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  integration-tests:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '25'
          rebar3-version: '3.22'

      - name: Install Dependencies
        run: rebar3 get-deps

      - name: Compile
        run: rebar3 compile

      - name: Run Integration Tests
        run: |
          rebar3 ct \
            --suite=test/quality_gates_SUITE,test/hooks_integration_SUITE,test/regression_detection_SUITE,test/auto_fix_SUITE \
            --cover

      - name: Generate Coverage Report
        run: rebar3 cover --verbose

      - name: Check Coverage Threshold
        run: |
          COVERAGE=$(rebar3 cover --verbose | grep "total" | awk '{print $2}' | sed 's/%//')
          if (( $(echo "$COVERAGE < 85" | bc -l) )); then
            echo "Coverage $COVERAGE% below 85% threshold"
            exit 1
          fi

      - name: Block on Test Failure
        if: failure()
        run: |
          echo "❌ Integration tests failed - BLOCKING MERGE"
          exit 1
```

### Pre-Merge Blocking

**MUST pass before merge**:
- ✅ All 35 integration tests pass
- ✅ Coverage ≥85% for core modules
- ✅ 0 compilation warnings
- ✅ 0 dialyzer warnings

**Blocking command**:
```bash
#!/bin/bash
# Add to .git/hooks/pre-push

echo "Running integration tests..."
rebar3 ct --suite=test/quality_gates_SUITE,test/hooks_integration_SUITE,test/regression_detection_SUITE,test/auto_fix_SUITE

if [ $? -ne 0 ]; then
  echo "❌ Integration tests failed - BLOCKING PUSH"
  exit 1
fi

echo "✅ Integration tests passed"
```

---

## Troubleshooting

### Common Issues

**Issue 1: Git hook tests fail**
```
Error: fatal: not a git repository
```
**Solution**: Tests create temporary git repos in `/tmp/`. Ensure write permissions.

**Issue 2: Compilation tests fail**
```
Error: {error, enoent}
```
**Solution**: Ensure test workspace directories exist and are writable.

**Issue 3: Agent spawn tests fail**
```
Error: {error, noproc}
```
**Solution**: Ensure `tcps_erlmcp` application started in `init_per_suite/1`.

**Issue 4: Coverage below threshold**
```
Warning: Coverage 78% below 85%
```
**Solution**: Add tests for uncovered code paths in quality gate modules.

---

## Chicago School TDD Compliance

### Verification Checklist

**All tests MUST**:
- ✅ Use real processes (no `meck`, no mocks)
- ✅ Spawn actual gen_servers via `start_link/0`
- ✅ Use real file I/O (`file:write_file/2`, `file:read_file/1`)
- ✅ Use real compiler (`compile:file/2`)
- ✅ Use real git commands (`os:cmd("git ...")`)
- ✅ Assert on observable state (API results, file contents)
- ✅ Test behaviors and outputs (not internal calls)

**Tests MUST NOT**:
- ❌ Use mock frameworks (`meck`)
- ❌ Stub collaborators (use real dependencies)
- ❌ Verify method calls (test state, not interactions)
- ❌ Use test doubles (always real processes)

---

## Maintenance

### Adding New Tests

**Template for new test case**:
```erlang
new_quality_gate_test(Config) ->
    %% Setup: Create real work order
    WorkOrder = #{
        id => <<"WO-NEW-001">>,
        stage => compilation
    },

    %% Exercise: Run real quality gate
    Result = tcps_quality_gates:check_gate(compilation, WorkOrder),

    %% Verify: Check observable state
    {pass, Receipt} = Result,
    ct:log("Receipt: ~p", [Receipt]),

    %% Assert on receipt contents (state verification)
    true = maps:is_key(gate, Receipt),
    <<"compilation">> = maps:get(gate, Receipt),

    ok.
```

### Updating Test Data

**Baseline metrics** (regression tests):
- Location: `/tmp/regression_detection_baselines/`
- Format: JSON with `work_order_id`, `tests`, `coverage`, `performance`
- Update: Modify `BaselineMetrics` map in test setup

**Test repositories** (hooks tests):
- Location: `/tmp/hooks_integration_test_repo/`
- Cleanup: Automatic in `end_per_suite/1`
- Git config: Test user/email set in `init_per_suite/1`

---

## Summary

**Test Suite Statistics**:
- **Total suites**: 4
- **Total tests**: 35
- **Test framework**: Common Test
- **TDD methodology**: Chicago School (real processes, no mocks)
- **Coverage target**: 85%+
- **Execution time**: <2 minutes

**Quality Standards**:
- ✅ Zero mock objects
- ✅ Real processes (gen_servers, spawned agents)
- ✅ Real I/O (files, git, compiler)
- ✅ State-based assertions
- ✅ Integration focus

**Ready for**: Production deployment, CI/CD integration, pre-merge blocking

---

**Last Updated**: 2026-01-28
**Maintainer**: erlang-test-engineer agent
**Version**: v0.6.0
