# Quick Start - Automated Validation

## Installation Complete ✅

The automated validation system is ready to use immediately.

## Usage

### 1. Run Tests (BLOCKING)
```bash
make test-strict
# Or:
./tools/test-runner.sh
```

**What it does:**
- Runs EUnit + Common Test
- Calculates pass rate
- **EXITS 1** if pass rate < 90%
- Generates JSON results

**Output:**
```
========================================
TEST SUMMARY
========================================
Total Tests:  245
Passed:       245
Failed:       0
Pass Rate:    100%
Threshold:    90%

✅ SUCCESS: All tests passed (100%)
```

---

### 2. Run Benchmarks (BLOCKING)
```bash
make benchmark-strict
# Or:
./tools/benchmark-runner.sh
```

**What it does:**
- Runs 10 benchmark workloads
- Compares to baselines
- **EXITS 1** if regression > 10%
- Creates baselines on first run

**First run:**
```
Running: erlmcp_bench_core_ops:core_ops_100k
  ⚠️  No baseline - creating new baseline
```

**Subsequent runs:**
```
Running: erlmcp_bench_core_ops:core_ops_100k
  ✅ IMPROVEMENT: +3%
```

---

### 3. Check Coverage (BLOCKING)
```bash
make coverage-strict
# Or:
./tools/coverage-checker.sh
```

**What it does:**
- Runs tests with coverage
- Checks EVERY module ≥80%
- **EXITS 1** if any module below threshold
- Generates HTML report

**Output:**
```
Analyzing module coverage...

  erlmcp_client                           92% ✅
  erlmcp_server                           88% ✅
  erlmcp_registry                         85% ✅

========================================
COVERAGE SUMMARY
========================================
Overall Coverage: 89%
Total Modules:    45
Below Threshold:  0

✅ SUCCESS: All modules meet 80% coverage
```

---

### 4. Master Quality Check (BLOCKING)
```bash
make quality-strict
# Or:
./tools/quality-checker.sh
```

**What it does:**
- CHECK 1: Compilation
- CHECK 2: Unit Tests
- CHECK 3: Coverage
- CHECK 4: Dialyzer
- CHECK 5: Xref
- **EXITS 1** if ANY fail

**Output:**
```
========================================
QUALITY CHECK SUMMARY
========================================
Duration:      185 seconds
Total Checks:  5
Passed:        5
Failed:        0

✅ QUALITY CHECK PASSED: All checks successful
```

---

## Pre-Commit Workflow

**Recommended before every commit:**

```bash
# Quick check (30 seconds)
make test-strict

# Full validation (3-5 minutes)
make quality-strict
```

**Before releases:**

```bash
# Full validation + benchmarks (5-10 minutes)
make quality-strict && make benchmark-strict

# TCPS certification
make jidoka
```

---

## CI/CD Integration

### GitHub Actions
```yaml
name: Quality Gates

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Erlang
        uses: erlef/setup-beam@v1
        with:
          otp-version: '25'
          
      - name: Quality Gates
        run: make quality-strict || exit 1
        
      - name: Benchmark Validation
        run: make benchmark-strict || exit 1
```

---

## Failure Examples

### Test Failure
```bash
$ make test-strict

❌ FAILURE DETAILS
========================================

EUnit Failures:
  erlmcp_client_tests:subscribe_test
    Expected: {ok, subscription_id}
    Got:      {error, timeout}

❌ FAILURE: 1 test(s) failed

# Exit code: 1
```

**Action:** Fix the failing test, then re-run.

---

### Coverage Failure
```bash
$ make coverage-strict

❌ LOW COVERAGE MODULES
========================================

  ❌ erlmcp_new_feature: 67% (threshold: 80%)

❌ FAILURE: 1 module(s) below 80% coverage

# Exit code: 1
```

**Action:** Add tests to reach 80% coverage.

---

### Benchmark Regression
```bash
$ make benchmark-strict

❌ REGRESSION DETAILS
========================================

  ❌ erlmcp_bench_core_ops:core_ops_100k: -15%
     (2287000 vs 2690000 msg/s)

❌ FAILURE: 1 regression(s) detected (>10%)

# Exit code: 1
```

**Action:** Investigate performance issue or update baseline if intentional.

---

## Baseline Management

### Create New Baselines
```bash
# Delete all baselines
rm bench/baselines/*

# Run benchmarks (creates new baselines)
make benchmark-strict
```

### Update Specific Baseline
```bash
# Remove one baseline
rm bench/baselines/erlmcp_bench_core_ops_core_ops_100k.baseline

# Re-run (creates new baseline for that workload)
make benchmark-strict
```

---

## Output Locations

### Test Results
- `_build/test/results/test_results.json`
- `_build/test/results/eunit_output.txt`

### Benchmark Results
- `_build/test/benchmarks/benchmark_results.json`
- `bench/baselines/*.baseline`

### Coverage Results
- `_build/test/coverage/coverage_report.txt`
- `_build/test/cover/index.html` (open in browser)

### Quality Results
- `_build/test/quality/quality_report.txt`

---

## Philosophy

**Zero-defect quality enforcement**

All validation is **BLOCKING**:
- Scripts exit with code 1 on failure
- Must fix issues before proceeding
- No compromises on quality
- Stop the line on defects (自働化 Jidoka)

---

## Next Steps

1. **Run initial validation:**
   ```bash
   make quality-strict
   ```

2. **Set up pre-commit hook:**
   ```bash
   echo "make test-strict" > .git/hooks/pre-commit
   chmod +x .git/hooks/pre-commit
   ```

3. **Add to CI/CD pipeline** (see examples above)

4. **Create benchmark baselines:**
   ```bash
   make benchmark-strict
   ```

---

## Documentation

- **Comprehensive guide:** `docs/testing/AUTOMATED_VALIDATION.md`
- **Tools reference:** `tools/README.md`
- **Makefile help:** `make help`

---

**Status:** Production-ready ✅
**Created:** 2026-01-28
**Version:** 1.0.0
