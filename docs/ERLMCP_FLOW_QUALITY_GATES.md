# erlmcp-flow Quality Gates

**Version:** 1.0.0
**Date:** 2026-02-02
**Status:** MANDATORY
**Enforcement:** BLOCKING (Cannot merge without passing ALL gates)

---

## Executive Summary

This document defines the **comprehensive quality gates** for erlmcp-flow that MUST pass before any code is merged. These gates enforce:

1. **Zero errors** - Compilation, type checking, cross-references
2. **Zero failures** - Unit tests, integration tests, chaos tests
3. **Performance targets** - 500K msg/s, <50ms p99 latency, zero task loss
4. **Coverage requirements** - ≥85% for core modules, ≥80% overall
5. **Anti-pattern detection** - Chicago TDD violations, OTP violations

**Enforcement:** All gates run automatically in CI/CD and pre-commit hooks. **ANY failure blocks merge.**

---

## Table of Contents

1. [Gate 1: Compilation](#gate-1-compilation)
2. [Gate 2: Cross-Reference Analysis (Xref)](#gate-2-cross-reference-analysis-xref)
3. [Gate 3: Type Checking (Dialyzer)](#gate-3-type-checking-dialyzer)
4. [Gate 4: Unit Tests (EUnit)](#gate-4-unit-tests-eunit)
5. [Gate 5: Integration Tests (Common Test)](#gate-5-integration-tests-common-test)
6. [Gate 6: Coverage](#gate-6-coverage)
7. [Gate 7: Performance Benchmarks](#gate-7-performance-benchmarks)
8. [Gate 8: Chaos Engineering](#gate-8-chaos-engineering)
9. [Gate 9: Chicago TDD Compliance](#gate-9-chicago-tdd-compliance)
10. [Gate 10: OTP Compliance](#gate-10-otp-compliance)
11. [Gate 11: Style & Format](#gate-11-style--format)
12. [Gate 12: Documentation](#gate-12-documentation)
13. [CI/CD Integration](#cicd-integration)
14. [Pre-Commit Hook Integration](#pre-commit-hook-integration)

---

## Gate 1: Compilation

**Rule:** Zero compilation errors.

### Command

```bash
cd apps/erlmcp_flow
TERM=dumb rebar3 compile
```

### Success Criteria

```
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling erlmcp_flow
===> Compiled: 12 modules
===> Errors: 0
===> Warnings: 0 (acceptable)
```

### Failure Criteria

```
===> Compiling erlmcp_flow
===> Error: src/erlmcp_flow_router.erl:42: syntax error before: '}'
===> Errors: 1
❌ GATE FAILED: Compilation errors detected
```

### Enforcement

- **CI/CD:** BLOCKING (fails build)
- **Pre-commit:** BLOCKING (prevents commit)
- **Manual:** `make compile-flow`

### Checklist

- [ ] Exit code: 0
- [ ] Errors: 0
- [ ] Warnings: Acceptable (non-blocking, but review required)
- [ ] All modules in `apps/erlmcp_flow/src/*.erl` compile
- [ ] All tests in `apps/erlmcp_flow/test/*.erl` compile

---

## Gate 2: Cross-Reference Analysis (Xref)

**Rule:** Zero undefined function calls.

### Command

```bash
cd apps/erlmcp_flow
rebar3 xref
```

### Success Criteria

```
===> Running cross reference analysis...
===> Xref: No undefined function calls
===> Xref: No unused exports
✅ GATE PASSED: Xref analysis clean
```

### Failure Criteria

```
===> Xref warnings:
Warning: erlmcp_flow_router:42: call to undefined function
         erlmcp_flow_registry:lookup_agent_bad/1
❌ GATE FAILED: Undefined function call detected
```

### Enforcement

- **CI/CD:** BLOCKING
- **Pre-commit:** NON-BLOCKING (warning only)
- **Manual:** `make xref-flow`

### Checklist

- [ ] No undefined function calls
- [ ] No calls to non-existent modules
- [ ] No typos in function names
- [ ] All dependencies properly declared in `rebar.config`

---

## Gate 3: Type Checking (Dialyzer)

**Rule:** Zero type errors or warnings.

### Command

```bash
cd apps/erlmcp_flow
rebar3 dialyzer
```

### Success Criteria

```
===> Compiling erlmcp_flow
===> Building PLT (if needed)...
===> Analyzing erlmcp_flow
  Checking whether the PLT is up-to-date... yes
  Proceeding with analysis...
done in 0m12.34s
done (passed successfully)
✅ GATE PASSED: Dialyzer clean (0 warnings)
```

### Failure Criteria

```
===> Dialyzer warnings:
src/erlmcp_flow_router.erl:42: Function route_task/2 has no local return
src/erlmcp_flow_router.erl:45: The call erlmcp_flow_registry:lookup_agent(atom())
                                 breaks the contract (binary()) -> {ok, pid()}
❌ GATE FAILED: 2 Dialyzer warnings
```

### Enforcement

- **CI/CD:** BLOCKING
- **Pre-push:** BLOCKING
- **Manual:** `make dialyzer-flow`

### Checklist

- [ ] All exported functions have `-spec` annotations
- [ ] Type specs match actual return types
- [ ] No type contradictions
- [ ] Guards match type specs
- [ ] Custom types exported with `-export_type`

---

## Gate 4: Unit Tests (EUnit)

**Rule:** All unit tests pass (0 failures).

### Command

```bash
cd apps/erlmcp_flow
rebar3 eunit --app erlmcp_flow
```

### Success Criteria

```
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling erlmcp_flow
===> Performing EUnit tests...

erlmcp_flow_registry_tests: ........................................ (37 tests, 0 failures)
erlmcp_flow_router_tests: ........................................... (43 tests, 0 failures)
erlmcp_flow_q_learning_tests: ....................................... (32 tests, 0 failures)
erlmcp_flow_load_balancer_tests: .................................... (28 tests, 0 failures)
erlmcp_flow_backpressure_tests: ..................................... (24 tests, 0 failures)

Finished in 8.234 seconds
164 tests, 0 failures

✅ GATE PASSED: All EUnit tests passed
```

### Failure Criteria

```
erlmcp_flow_router_tests: test_routing_with_q_learning...FAILED
in function erlmcp_flow_router_tests:'-test_routing_with_q_learning/0-fun-0-'/0 (test/erlmcp_flow_router_tests.erl, line 142)
**error:{assertEqual,
         [{module,erlmcp_flow_router_tests},
          {line,145},
          {expression,"AgentId"},
          {expected,<<"agent-1">>},
          {value,<<"agent-2">>}]}

164 tests, 1 failure
❌ GATE FAILED: 1 test failure
```

### Enforcement

- **CI/CD:** BLOCKING
- **Pre-commit:** BLOCKING (smoke tests only)
- **Pre-push:** BLOCKING (full suite)
- **Manual:** `make test-flow`

### Checklist

- [ ] All tests pass (0 failures)
- [ ] Pass rate: 100%
- [ ] Test execution time: < 10 seconds for unit tests
- [ ] No flaky tests (pass consistently)
- [ ] Real processes used (no mocks)

---

## Gate 5: Integration Tests (Common Test)

**Rule:** All integration tests pass (0 failures).

### Command

```bash
cd apps/erlmcp_flow
rebar3 ct --dir test
```

### Success Criteria

```
===> Running Common Test suites...

Testing erlmcp_flow_integration_SUITE:
  test_end_to_end_routing............... PASSED (1.234s)
  test_multi_agent_coordination......... PASSED (2.456s)
  test_transport_bridge_stdio........... PASSED (1.123s)
  test_transport_bridge_tcp............. PASSED (1.789s)
  test_backpressure_handling............ PASSED (3.012s)

All 5 test cases passed.

✅ GATE PASSED: Integration tests passed
```

### Failure Criteria

```
Testing erlmcp_flow_integration_SUITE:
  test_end_to_end_routing............... FAILED
    Reason: Timeout waiting for agent response (expected < 5000ms, got timeout)

1 of 5 test cases failed.
❌ GATE FAILED: Integration test failure
```

### Enforcement

- **CI/CD:** BLOCKING
- **Pre-push:** BLOCKING
- **Manual:** `make ct-flow`

### Checklist

- [ ] All integration tests pass
- [ ] Real transports used (stdio, TCP, HTTP)
- [ ] End-to-end workflows validated
- [ ] Multi-agent coordination verified
- [ ] Failure scenarios tested

---

## Gate 6: Coverage

**Rule:** ≥85% coverage for core modules, ≥80% overall.

### Command

```bash
cd apps/erlmcp_flow
rebar3 cover --verbose
```

### Success Criteria

```
===> Cover analysis:
  |----------------------------|------------|
  | module                     | coverage % |
  |----------------------------|------------|
  | erlmcp_flow_registry       |      92%   |
  | erlmcp_flow_router         |      89%   |
  | erlmcp_flow_q_learning     |      87%   |
  | erlmcp_flow_load_balancer  |      86%   |
  | erlmcp_flow_backpressure   |      88%   |
  | erlmcp_flow_agent          |      85%   |
  |----------------------------|------------|
  | total                      |      88%   |
  |----------------------------|------------|

✅ GATE PASSED: Coverage 88% (target: ≥80%, core: ≥85%)
```

### Failure Criteria

```
===> Cover analysis:
  | erlmcp_flow_router         |      78%   |  ❌ Core module below 85%
  | total                      |      79%   |  ❌ Overall below 80%

❌ GATE FAILED: Coverage below threshold
```

### Enforcement

- **CI/CD:** BLOCKING
- **Pre-push:** BLOCKING
- **Manual:** `make coverage-flow`

### Coverage Requirements

| Module Category | Minimum Coverage | Enforcement |
|----------------|------------------|-------------|
| Core routing (registry, router, q_learning) | 85% | BLOCKING |
| Load balancing, backpressure | 85% | BLOCKING |
| Agent lifecycle | 82% | BLOCKING |
| Transport bridges | 80% | BLOCKING |
| Utilities, helpers | 75% | NON-BLOCKING |
| Overall | 80% | BLOCKING |

### Checklist

- [ ] Core modules ≥ 85% coverage
- [ ] Overall coverage ≥ 80%
- [ ] All public functions covered
- [ ] Error paths covered
- [ ] Happy paths covered
- [ ] Edge cases covered

---

## Gate 7: Performance Benchmarks

**Rule:** All performance targets met.

### Command

```bash
cd apps/erlmcp_flow
rebar3 eunit --module erlmcp_flow_bench
```

### Success Criteria

```
=== erlmcp-flow Performance Benchmark Suite ===

--- Throughput Benchmarks ---
Message Passing Throughput: 523K msg/s (PASS)
Consensus Throughput: 105K ops/s (PASS)
Agent Scheduling Throughput: 112K tasks/s (PASS)
Pattern Search Throughput: 1.2K searches/s, 8.3ms avg (PASS)

--- Latency Benchmarks ---
Message Passing Latency: p50=0.12ms, p95=0.45ms, p99=0.89ms (PASS)
Consensus Finality Latency: p50=42ms, p95=89ms, p99=156ms (FAIL - target: <100ms p99)
Agent Assignment Latency: p50=0.05ms, p95=0.21ms, p99=0.48ms (PASS)
Pattern Search Latency: p50=8.2ms, p95=34ms, p99=78ms (PASS)

--- Memory Benchmarks ---
HNSW Index Memory: 87 MB (PASS)
Agent Pool Memory: 245 MB (PASS)
Message Queue Memory: 32 MB (PASS)
Total System Memory: 498 MB (PASS)

--- Reliability Benchmarks ---
Zero Task Loss: 10000/10000 completed, 0.00% loss (PASS)
Consensus Safety: 1000/1000 committed (PASS)
Agent Failover: p50=45ms, p95=128ms, p99=189ms (FAIL - target: <100ms p99)

=== Benchmark Complete ===
Report: bench/results/flow_benchmark_20260202_120000.json

Overall: 10/12 targets met (83%)
❌ GATE FAILED: 2 performance targets not met
```

### Performance Targets

| Metric | Target | Measurement | Gate |
|--------|--------|-------------|------|
| Registry lookup (p99) | < 100μs | Benchmark | BLOCKING |
| Routing decision (p99) | < 50ms | Benchmark | BLOCKING |
| Message throughput | > 500K msg/s | Benchmark | BLOCKING |
| Consensus latency (p99) | < 100ms | Benchmark | BLOCKING |
| Agent failover (p99) | < 100ms | Benchmark | BLOCKING |
| Total memory (1000 agents) | < 512MB | Benchmark | BLOCKING |
| Zero task loss | 100% | Chaos test | BLOCKING |

### Enforcement

- **CI/CD:** BLOCKING for releases, NON-BLOCKING for feature branches
- **Pre-push:** NON-BLOCKING (warning only)
- **Manual:** `make bench-flow`

### Checklist

- [ ] All latency targets met (p50, p95, p99)
- [ ] Throughput targets met
- [ ] Memory usage within bounds
- [ ] Zero task loss verified
- [ ] Benchmark report generated
- [ ] Performance regression < 10%

---

## Gate 8: Chaos Engineering

**Rule:** System survives agent crashes, network partitions, Byzantine faults.

### Command

```bash
cd apps/erlmcp_flow
rebar3 ct --suite erlmcp_flow_chaos_SUITE
```

### Success Criteria

```
===> Running Chaos Engineering tests...

erlmcp_flow_chaos_SUITE:
  test_agent_crash_recovery............. PASSED
    - Killed 20 random agents during execution
    - All 10000 tasks completed successfully
    - Average recovery time: 78ms (target: <2s)
    - Zero task loss

  test_network_partition................ PASSED
    - Simulated 50% network partition
    - Consensus maintained via quorum
    - 0 safety violations

  test_byzantine_node................... PASSED
    - 1 Byzantine node (sending incorrect responses)
    - 3f+1 quorum maintained
    - Correct consensus achieved

  test_overload_backpressure............ PASSED
    - 100K tasks submitted in 1 second
    - Backpressure engaged at 10K queue depth
    - No tasks dropped
    - Circuit breaker activated correctly

All 4 chaos tests passed.

✅ GATE PASSED: Chaos engineering tests passed
```

### Failure Criteria

```
erlmcp_flow_chaos_SUITE:
  test_agent_crash_recovery............. FAILED
    - Killed 20 random agents
    - 9897/10000 tasks completed
    - 103 tasks lost (1.03% loss rate)
    - ❌ Target: 0% task loss

❌ GATE FAILED: Task loss detected in chaos test
```

### Enforcement

- **CI/CD:** BLOCKING for releases
- **Pre-merge:** BLOCKING for main/release branches
- **Manual:** `make chaos-flow`

### Chaos Scenarios

| Scenario | Test | Target | Gate |
|----------|------|--------|------|
| Agent crashes | 20 random crashes | 0% task loss | BLOCKING |
| Network partition | 50% partition | Consensus maintained | BLOCKING |
| Byzantine node | 1 malicious node | Correct consensus | BLOCKING |
| Overload | 100K tasks/sec | Backpressure engaged | BLOCKING |
| Leader failure | Kill leader | Failover < 2s | BLOCKING |

### Checklist

- [ ] Zero task loss in agent crash scenario
- [ ] Consensus safety maintained in partition
- [ ] Byzantine tolerance verified (3f+1)
- [ ] Backpressure prevents overload
- [ ] Leader failover < 2 seconds
- [ ] All chaos tests pass

---

## Gate 9: Chicago TDD Compliance

**Rule:** No mocks, no state inspection, real processes only.

### Command

```bash
cd apps/erlmcp_flow
./.github/scripts/chicago-tdd-scan-flow.sh
```

### Success Criteria

```
===> Scanning for Chicago TDD violations...

Checking for mock usage (meck, mocking frameworks)...
  ✅ No mock frameworks found

Checking for state inspection (sys:get_status, sys:get_state)...
  ✅ No state inspection in tests

Checking for dummy processes (spawn without supervision)...
  ✅ No dummy process patterns found

Checking for test-specific state records...
  ✅ No test state records (using production records)

Checking for real process usage...
  ✅ All tests use real erlmcp_flow processes

✅ GATE PASSED: Chicago TDD compliance verified
```

### Failure Criteria

```
===> Scanning for Chicago TDD violations...

Checking for mock usage...
  ❌ Found meck usage in test/erlmcp_flow_router_tests.erl:42
    Line 42: meck:new(erlmcp_flow_registry)
    Line 43: meck:expect(erlmcp_flow_registry, lookup_agent, ...)

❌ GATE FAILED: Chicago TDD violations detected
```

### Enforcement

- **CI/CD:** BLOCKING
- **Pre-commit:** BLOCKING
- **Manual:** `make chicago-tdd-check-flow`

### Anti-Patterns (Violations)

```erlang
% ❌ Mock usage
meck:new(erlmcp_flow_registry)

% ❌ State inspection
sys:get_status(RouterPid)

% ❌ Dummy process
spawn(fun() -> receive stop -> ok end end)

% ❌ Test-specific record
-record(test_state, {field}).  % In test file
```

### Checklist

- [ ] No meck, mock, stub frameworks
- [ ] No sys:get_status or sys:get_state
- [ ] No dummy spawn patterns
- [ ] All tests use real processes
- [ ] Production records used in tests
- [ ] Observable behavior tested only

---

## Gate 10: OTP Compliance

**Rule:** All gen_server callbacks, supervision, let-it-crash patterns.

### Command

```bash
cd apps/erlmcp_flow
./.github/scripts/otp-compliance-scan-flow.sh
```

### Success Criteria

```
===> Scanning for OTP compliance...

Checking gen_server implementations...
  ✅ erlmcp_flow_registry: All 6 callbacks present
  ✅ erlmcp_flow_router: All 6 callbacks present
  ✅ erlmcp_flow_q_learning: All 6 callbacks present

Checking init/1 blocking...
  ✅ No blocking operations in init/1
  ✅ All use {continue, ...} or self() ! msg

Checking supervision trees...
  ✅ erlmcp_flow_sup: one_for_all strategy
  ✅ erlmcp_flow_agent_sup: simple_one_for_one
  ✅ No unsupervised spawn calls

Checking timeout configurations...
  ✅ All timeouts ≥ 5000ms or infinity

✅ GATE PASSED: OTP compliance verified
```

### Failure Criteria

```
===> Scanning for OTP compliance...

Checking gen_server implementations...
  ❌ erlmcp_flow_router: Missing code_change/3

Checking init/1 blocking...
  ❌ erlmcp_flow_registry:init/1 blocks on database connection (line 82)

Checking supervision trees...
  ❌ Found unsupervised spawn in erlmcp_flow_router.erl:156

❌ GATE FAILED: 3 OTP compliance violations
```

### Enforcement

- **CI/CD:** BLOCKING
- **Pre-commit:** BLOCKING
- **Manual:** `make otp-check-flow`

### OTP Requirements

- [ ] All 6 gen_server callbacks exported
- [ ] init/1 never blocks (use async initialization)
- [ ] All processes supervised
- [ ] Timeouts ≥ 5000ms
- [ ] Monitors used for cleanup (not links)
- [ ] Let-it-crash error handling
- [ ] Proper restart strategies

---

## Gate 11: Style & Format

**Rule:** Code formatted according to rebar3 format rules.

### Command

```bash
cd apps/erlmcp_flow
rebar3 format --verify
```

### Success Criteria

```
===> Verifying code formatting...
  ✅ src/erlmcp_flow_registry.erl: formatted
  ✅ src/erlmcp_flow_router.erl: formatted
  ✅ src/erlmcp_flow_q_learning.erl: formatted
  ✅ test/erlmcp_flow_registry_tests.erl: formatted

All files formatted correctly.

✅ GATE PASSED: Code formatting verified
```

### Failure Criteria

```
===> Verifying code formatting...
  ❌ src/erlmcp_flow_router.erl: not formatted
    - Line 42: Expected 4 spaces, found 2
    - Line 89: Line too long (120 chars, max 100)

❌ GATE FAILED: Code formatting violations
```

### Enforcement

- **CI/CD:** NON-BLOCKING (warning)
- **Pre-commit:** BLOCKING (auto-fix enabled)
- **Manual:** `make format-flow`

### Style Requirements

- [ ] 4-space indentation
- [ ] Max line length: 100 characters
- [ ] Unix line endings (LF)
- [ ] No trailing whitespace
- [ ] Consistent comma placement
- [ ] Module naming: `erlmcp_flow_*`
- [ ] Function naming: `verb_noun`

---

## Gate 12: Documentation

**Rule:** All exported functions have type specs and @doc comments.

### Command

```bash
cd apps/erlmcp_flow
./.github/scripts/doc-coverage-scan-flow.sh
```

### Success Criteria

```
===> Checking documentation coverage...

Checking module headers...
  ✅ All modules have @doc

Checking function specs...
  ✅ All exported functions have -spec

Checking type exports...
  ✅ All custom types exported

Checking examples...
  ✅ Public API functions have @example

Documentation coverage: 100%

✅ GATE PASSED: Documentation complete
```

### Failure Criteria

```
===> Checking documentation coverage...

Missing specs:
  ❌ erlmcp_flow_router:route_task/2 (line 42): No -spec
  ❌ erlmcp_flow_registry:lookup_agent/1 (line 89): No -spec

Missing @doc:
  ❌ erlmcp_flow_q_learning (module): No module @doc

Documentation coverage: 78% (target: 100%)

❌ GATE FAILED: Incomplete documentation
```

### Enforcement

- **CI/CD:** NON-BLOCKING (warning for main branch, blocking for releases)
- **Pre-push:** NON-BLOCKING
- **Manual:** `make doc-check-flow`

### Documentation Requirements

- [ ] All modules have @doc header
- [ ] All exported functions have -spec
- [ ] All public functions have @doc
- [ ] Custom types exported with -export_type
- [ ] Examples provided for public API
- [ ] README.md updated (if API changes)

---

## CI/CD Integration

### GitHub Actions Workflow

```yaml
# .github/workflows/erlmcp-flow-quality-gates.yml
name: erlmcp-flow Quality Gates

on:
  push:
    branches: [main, 'release/**', 'feature/erlmcp-flow-**']
    paths:
      - 'apps/erlmcp_flow/**'
      - '.github/workflows/erlmcp-flow-quality-gates.yml'
  pull_request:
    paths:
      - 'apps/erlmcp_flow/**'

jobs:
  quality-gates:
    runs-on: ubuntu-22.04
    timeout-minutes: 45

    steps:
      - uses: actions/checkout@v4

      - name: Setup Erlang/OTP 28+
        uses: erlef/setup-beam@v1
        with:
          otp-version: 28
          rebar3-version: '3.25'

      - name: Cache Dependencies
        uses: actions/cache@v3
        with:
          path: |
            _build
            deps
          key: ${{ runner.os }}-erlmcp-flow-${{ hashFiles('**/rebar.lock') }}

      # GATE 1: Compilation (BLOCKING)
      - name: Gate 1 - Compilation
        run: |
          cd apps/erlmcp_flow
          TERM=dumb rebar3 compile
          if [ $? -ne 0 ]; then
            echo "::error::Compilation FAILED"
            exit 1
          fi

      # GATE 2: Xref (BLOCKING)
      - name: Gate 2 - Xref
        run: |
          cd apps/erlmcp_flow
          rebar3 xref
          if [ $? -ne 0 ]; then
            echo "::error::Xref FAILED"
            exit 1
          fi

      # GATE 3: Dialyzer (BLOCKING)
      - name: Gate 3 - Dialyzer
        run: |
          cd apps/erlmcp_flow
          rebar3 dialyzer
          if [ $? -ne 0 ]; then
            echo "::error::Dialyzer FAILED"
            exit 1
          fi

      # GATE 4: Unit Tests (BLOCKING)
      - name: Gate 4 - Unit Tests
        run: |
          cd apps/erlmcp_flow
          rebar3 eunit --app erlmcp_flow
          if [ $? -ne 0 ]; then
            echo "::error::EUnit tests FAILED"
            exit 1
          fi

      # GATE 5: Integration Tests (BLOCKING)
      - name: Gate 5 - Integration Tests
        run: |
          cd apps/erlmcp_flow
          rebar3 ct --dir test
          if [ $? -ne 0 ]; then
            echo "::error::Common Test FAILED"
            exit 1
          fi

      # GATE 6: Coverage (BLOCKING)
      - name: Gate 6 - Coverage
        run: |
          cd apps/erlmcp_flow
          rebar3 cover --verbose
          ./../../scripts/check_coverage_threshold_flow.sh 80
          if [ $? -ne 0 ]; then
            echo "::error::Coverage below 80%"
            exit 1
          fi

      # GATE 7: Performance Benchmarks (BLOCKING for releases)
      - name: Gate 7 - Performance Benchmarks
        if: startsWith(github.ref, 'refs/heads/release') || startsWith(github.ref, 'refs/tags/')
        run: |
          cd apps/erlmcp_flow
          rebar3 eunit --module erlmcp_flow_bench
          if [ $? -ne 0 ]; then
            echo "::error::Performance benchmarks FAILED"
            exit 1
          fi

      # GATE 8: Chaos Engineering (BLOCKING for releases)
      - name: Gate 8 - Chaos Engineering
        if: startsWith(github.ref, 'refs/heads/release') || startsWith(github.ref, 'refs/tags/')
        run: |
          cd apps/erlmcp_flow
          rebar3 ct --suite erlmcp_flow_chaos_SUITE
          if [ $? -ne 0 ]; then
            echo "::error::Chaos tests FAILED"
            exit 1
          fi

      # GATE 9: Chicago TDD Compliance (BLOCKING)
      - name: Gate 9 - Chicago TDD Compliance
        run: |
          ./.github/scripts/chicago-tdd-scan-flow.sh
          if [ $? -ne 0 ]; then
            echo "::error::Chicago TDD violations"
            exit 1
          fi

      # GATE 10: OTP Compliance (BLOCKING)
      - name: Gate 10 - OTP Compliance
        run: |
          ./.github/scripts/otp-compliance-scan-flow.sh
          if [ $? -ne 0 ]; then
            echo "::error::OTP compliance violations"
            exit 1
          fi

      # GATE 11: Format (NON-BLOCKING, warning only)
      - name: Gate 11 - Format Check
        continue-on-error: true
        run: |
          cd apps/erlmcp_flow
          rebar3 format --verify

      # GATE 12: Documentation (NON-BLOCKING, warning only)
      - name: Gate 12 - Documentation
        continue-on-error: true
        run: |
          ./.github/scripts/doc-coverage-scan-flow.sh

      # Generate Quality Report
      - name: Generate Quality Report
        if: always()
        run: |
          make quality-report-flow

      - name: Upload Quality Report
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: erlmcp-flow-quality-report
          path: generated/reports/erlmcp-flow-quality-*.md
```

---

## Pre-Commit Hook Integration

### Pre-Commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit
# erlmcp-flow quality gates (fast version)

set -euo pipefail

# Only run if erlmcp-flow files changed
if ! git diff --cached --name-only | grep -q "^apps/erlmcp_flow/"; then
    exit 0
fi

echo ""
echo "🚦 Running erlmcp-flow pre-commit gates..."
echo ""

FAILURES=0

# GATE 1: Compilation (FAST)
echo "[1/4] Compilation..."
if ! cd apps/erlmcp_flow && TERM=dumb rebar3 compile > /dev/null 2>&1; then
    echo "  ❌ Compilation failed"
    FAILURES=$((FAILURES + 1))
else
    echo "  ✅ Compilation passed"
fi
cd ../..

# GATE 2: Format (AUTO-FIX)
echo "[2/4] Format check..."
cd apps/erlmcp_flow
if ! rebar3 format --verify > /dev/null 2>&1; then
    echo "  ⚠️  Auto-formatting..."
    rebar3 format
    git add -u src/*.erl test/*.erl
    echo "  ✅ Formatted and staged"
else
    echo "  ✅ Format check passed"
fi
cd ../..

# GATE 3: Chicago TDD (FAST)
echo "[3/4] Chicago TDD compliance..."
if grep -r "meck:new\|sys:get_status" apps/erlmcp_flow/test/ > /dev/null 2>&1; then
    echo "  ❌ Chicago TDD violations"
    FAILURES=$((FAILURES + 1))
else
    echo "  ✅ Chicago TDD compliance"
fi

# GATE 4: Smoke Tests (FAST)
echo "[4/4] Smoke tests..."
if ! cd apps/erlmcp_flow && rebar3 eunit --module=erlmcp_flow_registry_tests > /dev/null 2>&1; then
    echo "  ❌ Smoke tests failed"
    FAILURES=$((FAILURES + 1))
else
    echo "  ✅ Smoke tests passed"
fi
cd ../..

echo ""
if [ $FAILURES -eq 0 ]; then
    echo "✅ All pre-commit gates passed"
    exit 0
else
    echo "❌ $FAILURES gate(s) failed - commit blocked"
    echo ""
    echo "Run for details: cd apps/erlmcp_flow && make check-flow"
    exit 1
fi
```

---

## Summary

### All Quality Gates

| Gate | Target | Enforcement | Timing |
|------|--------|-------------|--------|
| 1. Compilation | 0 errors | BLOCKING | Pre-commit, CI |
| 2. Xref | 0 undefined | BLOCKING | CI |
| 3. Dialyzer | 0 warnings | BLOCKING | Pre-push, CI |
| 4. Unit Tests | 0 failures | BLOCKING | Pre-commit, CI |
| 5. Integration Tests | 0 failures | BLOCKING | Pre-push, CI |
| 6. Coverage | ≥80% (≥85% core) | BLOCKING | CI |
| 7. Performance | All targets | BLOCKING (releases) | CI |
| 8. Chaos | 0% task loss | BLOCKING (releases) | CI |
| 9. Chicago TDD | No violations | BLOCKING | Pre-commit, CI |
| 10. OTP Compliance | No violations | BLOCKING | CI |
| 11. Format | Formatted | BLOCKING (pre-commit) | Pre-commit |
| 12. Documentation | 100% | NON-BLOCKING | CI |

### Fast vs Full Validation

**Fast (Pre-Commit):** 30-60 seconds
- Compilation
- Format (auto-fix)
- Chicago TDD scan
- Smoke tests

**Full (Pre-Push/CI):** 5-10 minutes
- All gates 1-12
- Performance benchmarks
- Chaos engineering tests
- Full test suite

---

**Document Version:** 1.0.0
**Last Updated:** 2026-02-02
**Maintained By:** erlmcp-flow Core Team
**Review Cycle:** Quarterly

**GATE ENFORCEMENT = QUALITY ASSURANCE**
