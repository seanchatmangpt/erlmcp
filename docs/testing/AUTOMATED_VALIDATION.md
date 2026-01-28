# Automated Validation System

**erlmcp** implements a comprehensive automated validation system that enforces Lean Six Sigma quality standards with **zero-defect tolerance**. All validation scripts are **BLOCKING** and exit with code 1 on failure.

## Philosophy

**Stop the line on defects (自働化 Jidoka)** - Manufacturing principle applied to code quality:
- NO partial deliveries
- NO compromises on quality
- MANDATORY gates before commits/PRs/releases
- 99.99966% defect-free delivery target

## Quick Reference

```bash
# Individual checks (BLOCKING)
make test-strict           # Tests: ≥90% pass rate required
make benchmark-strict      # Benchmarks: <10% regression allowed
make coverage-strict       # Coverage: ≥80% required
make quality-strict        # Master: ALL checks MUST pass

# Legacy validate targets (also BLOCKING)
make validate              # All quality gates
make validate-compile      # Compilation only
make validate-test         # Tests only
make validate-coverage     # Coverage only
make validate-quality      # Dialyzer + xref
make validate-bench        # Benchmarks only

# TCPS Manufacturing (Japanese terminology)
make jidoka                # 自働化 - Quality gates
make poka-yoke             # ポカヨケ - Error-proofing
make andon                 # 行灯 - Emergency stop
make release-validate      # レシート - Certified release
```

## Validation Scripts

### 1. Test Runner (`tools/test-runner.sh`)

**Purpose:** Run all tests and block if pass rate < 90%

**What it does:**
- Runs `rebar3 eunit` for all apps
- Runs `rebar3 ct` for integration tests
- Collects results in JSON format
- Calculates pass rate
- Generates detailed failure report
- **EXITS 1** if pass rate < 90% or any test fails

**Usage:**
```bash
./tools/test-runner.sh

# Or via Makefile
make test-strict
```

**Exit codes:**
- `0` - All tests passed (≥90% pass rate)
- `1` - Tests failed OR pass rate below threshold

**Output locations:**
- `_build/test/results/test_results.json` - JSON summary
- `_build/test/results/eunit_output.txt` - EUnit detailed output
- `_build/test/results/ct_output.txt` - CT detailed output

**Example output:**
```
========================================
TEST SUMMARY
========================================
Total Tests:  245
Passed:       245
Failed:       0
Skipped:      0
Pass Rate:    100%
Threshold:    90%

✅ SUCCESS: All tests passed (100%)

Results saved to: _build/test/results/test_results.json
```

### 2. Benchmark Runner (`tools/benchmark-runner.sh`)

**Purpose:** Run benchmarks and detect performance regressions

**What it does:**
- Runs 10 benchmark workloads across 5 suites
- Compares results to baseline (`bench/baselines/`)
- Detects regressions >10%
- Creates new baselines if none exist
- **EXITS 1** if regression detected

**Usage:**
```bash
./tools/benchmark-runner.sh

# Or via Makefile
make benchmark-strict
```

**Exit codes:**
- `0` - No regressions detected
- `1` - Regression >10% detected

**Baselines:**
Stored in `bench/baselines/` as `{module}_{workload}.baseline`:
```
bench/baselines/
├── erlmcp_bench_core_ops_core_ops_1k.baseline
├── erlmcp_bench_core_ops_core_ops_10k.baseline
├── erlmcp_bench_core_ops_core_ops_100k.baseline
├── erlmcp_bench_network_real_tcp_sustained_1k.baseline
└── ...
```

**Benchmarks run:**
1. **Core Operations**: 1K, 10K, 100K ops
2. **Network**: TCP/HTTP 1K, 10K sustained
3. **Stress**: 30s sustained load
4. **Chaos**: Network partition test
5. **Integration**: MCP tool sequence

**Example output:**
```
========================================
BENCHMARK SUMMARY
========================================
Total Benchmarks: 10
Regressions:      0
Threshold:        10%

✅ SUCCESS: No regressions detected

Results saved to: _build/test/benchmarks/benchmark_results.json
```

**Regression detected:**
```
========================================
REGRESSION DETAILS
========================================

  ❌ erlmcp_bench_core_ops:core_ops_100k: -15% (2287000 vs 2690000 msg/s)

❌ FAILURE: 1 regression(s) detected (>10%)

Report saved to: _build/test/benchmarks/regression_report.txt
```

### 3. Coverage Checker (`tools/coverage-checker.sh`)

**Purpose:** Check code coverage and block if any module < 80%

**What it does:**
- Runs `rebar3 do eunit, ct --cover`
- Generates coverage report with `rebar3 cover`
- Parses HTML index for per-module coverage
- Checks each module ≥80%
- **EXITS 1** if any module below threshold

**Usage:**
```bash
./tools/coverage-checker.sh

# Or via Makefile
make coverage-strict
```

**Exit codes:**
- `0` - All modules ≥80% coverage
- `1` - One or more modules below threshold

**Output locations:**
- `_build/test/coverage/coverage_report.txt` - Text summary
- `_build/test/coverage/coverage_results.json` - JSON data
- `_build/test/cover/index.html` - HTML report

**Example output:**
```
Analyzing module coverage...

  erlmcp_client                           92% ✅
  erlmcp_server                           88% ✅
  erlmcp_registry                         85% ✅
  erlmcp_json_rpc                         94% ✅
  erlmcp_transport_stdio                  82% ✅

========================================
COVERAGE SUMMARY
========================================
Overall Coverage: 89%
Threshold:        80%
Total Modules:    45
Below Threshold:  0

✅ SUCCESS: All modules meet 80% coverage threshold

Detailed HTML: file:///Users/sac/erlmcp/_build/test/cover/index.html
```

**Low coverage detected:**
```
  erlmcp_new_feature                      67% ❌

========================================
LOW COVERAGE MODULES
========================================

  ❌ erlmcp_new_feature: 67% (threshold: 80%)

❌ FAILURE: 1 module(s) below 80% coverage
```

### 4. Quality Checker (`tools/quality-checker.sh`)

**Purpose:** Master script that runs ALL validation checks

**What it does:**
- CHECK 1: Compilation (`rebar3 compile`)
- CHECK 2: Unit Tests (`./tools/test-runner.sh`)
- CHECK 3: Coverage (`./tools/coverage-checker.sh`)
- CHECK 4: Dialyzer (`rebar3 dialyzer`)
- CHECK 5: Xref (`rebar3 xref`)
- **EXITS 1** if ANY check fails

**Usage:**
```bash
./tools/quality-checker.sh

# Or via Makefile
make quality-strict
```

**Exit codes:**
- `0` - All checks passed
- `1` - One or more checks failed

**Output locations:**
- `_build/test/quality/quality_report.txt` - Comprehensive report

**Example output:**
```
========================================
ErlMCP Quality Checker
========================================
Comprehensive validation suite

========================================
CHECK 1: Compilation
========================================

Compiling all apps...
✅ PASSED: Compilation

========================================
CHECK 2: Unit Tests (EUnit + CT)
========================================

Running tests...
✅ PASSED: Unit Tests (EUnit + CT)

========================================
CHECK 3: Code Coverage
========================================

Generating coverage...
✅ PASSED: Code Coverage

========================================
CHECK 4: Dialyzer (Type Checking)
========================================

Running dialyzer...
✅ PASSED: Dialyzer (Type Checking)

========================================
CHECK 5: Xref (Cross-Reference)
========================================

Running xref...
✅ PASSED: Xref (Cross-Reference)

========================================
QUALITY CHECK SUMMARY
========================================
Duration:      185 seconds
Total Checks:  5
Passed:        5
Failed:        0

========================================
PASSED CHECKS
========================================

  ✅ Compilation
  ✅ Unit Tests (EUnit + CT)
  ✅ Code Coverage
  ✅ Dialyzer (Type Checking)
  ✅ Xref (Cross-Reference)

✅ QUALITY CHECK PASSED: All checks successful
```

## Integration with Makefile

All scripts are integrated into the Makefile with BLOCKING semantics:

```makefile
# Individual strict checks
test-strict:
	@./tools/test-runner.sh || exit 1

benchmark-strict:
	@./tools/benchmark-runner.sh || exit 1

coverage-strict:
	@./tools/coverage-checker.sh || exit 1

quality-strict:
	@./tools/quality-checker.sh || exit 1
```

## Pre-Commit Workflow

**Recommended workflow before commits:**

```bash
# 1. Quick check (compile + tests)
make test-strict

# 2. Full validation (all gates)
make quality-strict

# 3. Benchmark validation (optional, for performance code)
make benchmark-strict

# 4. TCPS certification (for releases)
make jidoka
```

## CI/CD Integration

Use in CI pipelines to enforce quality gates:

```yaml
# GitHub Actions example
- name: Quality Gates
  run: |
    make quality-strict || exit 1

- name: Benchmark Validation
  run: |
    make benchmark-strict || exit 1
```

## Quality Thresholds

### Tests
- **Threshold:** ≥90% pass rate
- **Target:** 100% pass rate (0 failures)
- **Rationale:** Some flaky tests allowed, but must investigate

### Coverage
- **Threshold:** ≥80% per module
- **Target:** 85-90% for core modules
- **Rationale:** Minimum for production code confidence

### Benchmarks
- **Threshold:** <10% regression
- **Target:** 0% regression or improvement
- **Rationale:** Small variations acceptable, major regressions blocked

### Type Checking (Dialyzer)
- **Threshold:** 0 warnings
- **Target:** 0 warnings
- **Rationale:** Type safety is binary (safe or unsafe)

### Cross-Reference (Xref)
- **Threshold:** 0 undefined function calls
- **Target:** 0 undefined calls
- **Rationale:** Runtime errors are unacceptable

## Failure Handling

When a validation fails:

1. **Read the detailed report** - Scripts provide context
2. **Fix the root cause** - No workarounds
3. **Re-run validation** - Ensure fix works
4. **Document if needed** - Update baselines for benchmarks

### Examples

**Test failure:**
```bash
make test-strict
# ❌ FAILURE: 2 test(s) failed
# Check: _build/test/results/test_results.json
# Fix: Update test or code
# Re-run: make test-strict
```

**Coverage failure:**
```bash
make coverage-strict
# ❌ FAILURE: erlmcp_new_feature: 67% (threshold: 80%)
# Fix: Add 13% more test coverage
# Re-run: make coverage-strict
```

**Benchmark regression:**
```bash
make benchmark-strict
# ❌ FAILURE: -15% regression detected
# Fix: Investigate performance issue
# Alternative: Update baseline if intentional change
# Re-run: make benchmark-strict
```

## Baseline Management

Benchmark baselines are stored in `bench/baselines/`:

### Creating new baselines
```bash
# Delete old baselines
rm bench/baselines/*

# Run benchmarks (creates new baselines)
make benchmark-strict
```

### Updating specific baseline
```bash
# Remove specific baseline
rm bench/baselines/erlmcp_bench_core_ops_core_ops_100k.baseline

# Re-run to create new baseline
make benchmark-strict
```

### When to update baselines
- After intentional performance optimizations
- After algorithm changes expected to affect performance
- After infrastructure changes (new hardware, OS updates)
- **NEVER** update to hide regressions

## TCPS Manufacturing Integration

The validation system integrates with TCPS (Toyota Code Production System):

### 自働化 (Jidoka) - Built-in Quality
```bash
make jidoka
# Runs full validate + TCPS quality gates
```

### ポカヨケ (Poka-yoke) - Error Proofing
```bash
make poka-yoke
# Validates safeguards: compile + test + quality
```

### 行灯 (Andon) - Stop the Line
```bash
make andon
# Emergency stop: halts production, requires supervisor
```

### レシート (Receipt) - Quality Evidence
```bash
make release-validate
# Generates quality receipt for release certification
```

## Troubleshooting

### Scripts not executable
```bash
chmod +x tools/*.sh
```

### Missing baseline files
```bash
# First run creates baselines
make benchmark-strict
```

### HTML coverage not found
```bash
# Ensure tests with coverage ran first
rebar3 do eunit, ct --cover
rebar3 cover
```

### JSON parsing errors
```bash
# Check if jq is installed (optional, not required)
# Scripts use grep/sed for portability
```

## Summary

**Zero-defect quality enforcement:**
- ✅ **test-strict**: ≥90% pass rate, 0 failures target
- ✅ **benchmark-strict**: <10% regression threshold
- ✅ **coverage-strict**: ≥80% per module minimum
- ✅ **quality-strict**: ALL gates MUST pass

**All scripts are BLOCKING** - exit 1 on failure, no compromises.

**Philosophy**: Stop the line on defects (自働化 Jidoka). Fix issues before they reach production.

**Integration**: Use in pre-commit hooks, CI/CD pipelines, and release validation.

**Reports**: Comprehensive JSON/text reports for debugging and auditing.

---

**Last Updated:** 2026-01-28
**Version:** 2.0.0
**Status:** Production-ready
