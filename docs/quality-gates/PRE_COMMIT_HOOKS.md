# ERLMCP Pre-Commit Hooks - Quality Gate System

## Overview

erlmcp enforces **Lean Six Sigma quality standards** through automated git hooks that run before commits and pushes. These hooks ensure zero-defect code delivery by validating:

- 100% compilation success (0 errors)
- Type coverage via Dialyzer
- Cross-reference analysis via Xref
- 100% test pass rate
- ≥80% test coverage
- <10% performance regression (pre-push only)

## Hook Architecture

```
Git Commit/Push Flow:
┌──────────────┐
│ Developer    │
│ commits code │
└──────┬───────┘
       │
       ▼
┌──────────────────────────────────┐
│ .git/hooks/pre-commit            │
│ ─────────────────────────────    │
│ 1. Check for merge/rebase        │
│ 2. Detect WIP commits            │
│ 3. Run quality-gate.sh (full)    │
│    - Compile                     │
│    - Xref                        │
│    - Dialyzer                    │
│    - EUnit                       │
│    - Common Test                 │
│    - Coverage                    │
└──────┬───────────────────────────┘
       │
       │ PASS ✓
       ▼
┌──────────────┐
│ Commit       │
│ succeeds     │
└──────┬───────┘
       │
       │ git push
       ▼
┌──────────────────────────────────┐
│ .git/hooks/pre-push              │
│ ─────────────────────────────    │
│ 1. Run quality-gate.sh (full)    │
│ 2. Run benchmarks                │
│ 3. Check for regressions         │
│ 4. Update baseline if needed     │
└──────┬───────────────────────────┘
       │
       │ PASS ✓
       ▼
┌──────────────┐
│ Push         │
│ succeeds     │
└──────────────┘
```

## Installation

### Automatic Installation

Hooks are already installed in `.git/hooks/` and are executable. If you need to reinstall:

```bash
# Install hooks
cp tools/install-hooks.sh .git/hooks/
./tools/install-hooks.sh

# Verify installation
ls -lah .git/hooks/pre-commit
ls -lah .git/hooks/pre-push
```

### Manual Installation

```bash
# Make scripts executable
chmod +x tools/quality-gate.sh
chmod +x .git/hooks/pre-commit
chmod +x .git/hooks/pre-push

# Test quality gate
./tools/quality-gate.sh
```

## Quality Gates

### Pre-Commit Hook

Runs **before each commit** (typically 1-3 minutes):

#### Gate 1: Compilation
```bash
✓ Compiles all modules with TERM=dumb rebar3 compile
✓ Reports module count
⚠ Reports warnings (non-blocking)
✗ Fails on compilation errors
```

#### Gate 2: Cross-Reference Analysis (Xref)
```bash
✓ Checks for undefined function calls
✓ Checks for unused exports
✓ Checks for deprecated functions
✗ Fails if warnings exceed threshold (default: 0)
```

#### Gate 3: Type Checking (Dialyzer)
```bash
✓ Validates type specifications
✓ Checks for type inconsistencies
✓ Builds PLT on first run (cached)
✗ Fails if warnings exceed threshold (default: 0)
```

#### Gate 4: Unit Tests (EUnit)
```bash
✓ Runs all EUnit tests
✓ Reports pass/fail count
✗ Fails if any test fails (100% pass required)
```

#### Gate 5: Integration Tests (Common Test)
```bash
✓ Runs Common Test suites (if present)
⚠ Skips if no suites found
✗ Fails if any suite fails
```

#### Gate 6: Test Coverage
```bash
✓ Measures code coverage via rebar3 cover
✓ Extracts coverage percentage
✗ Fails if coverage < 80% (configurable)
```

#### Gate 7: Security Scan
```bash
⚠ Not yet implemented (TODO)
```

### Pre-Push Hook

Runs **before each push** (typically 5-10 minutes):

#### Step 1: Full Quality Gate
```bash
✓ Runs all pre-commit gates
✗ Blocks push if any gate fails
```

#### Step 2: Performance Benchmarks
```bash
✓ Runs quick benchmark suite
✓ Measures throughput (ops/sec)
✓ Measures latency (p50, p95, p99)
⚠ Proceeds with warning if benchmarks fail
```

#### Step 3: Regression Analysis
```bash
✓ Compares against baseline performance
✓ Calculates regression percentage
✓ Creates baseline if missing
✗ Fails if regression > 10% (configurable)
```

## Usage

### Normal Workflow

```bash
# Make changes
vim src/my_module.erl

# Commit (hooks run automatically)
git commit -m "Add new feature"

# Output:
# ╔════════════════════════════════════════╗
# ║   ERLMCP PRE-COMMIT QUALITY GATE       ║
# ╚════════════════════════════════════════╝
#
# [✓] Compilation passed: 42 modules compiled
# [✓] Xref passed: 0 warnings
# [✓] Dialyzer passed: 0 warnings
# [✓] EUnit passed: 157/157 tests passed
# [✓] Common Test passed
# [✓] Coverage passed: 87%
#
# ✓ ALL GATES PASSED

# Push (hooks run automatically)
git push origin main

# Output:
# ╔════════════════════════════════════════╗
# ║    ERLMCP PRE-PUSH QUALITY GATE        ║
# ╚════════════════════════════════════════╝
#
# [1/3] Running full quality gate...
# ✓ Quality gate passed
#
# [2/3] Running quick performance benchmarks...
# ✓ Benchmarks completed
#
# [3/3] Checking for performance regressions...
# ✓ Performance within acceptable range (2.3% regression)
#
# ✓ PRE-PUSH QUALITY GATE PASSED
```

### WIP Commits (Fast Mode)

For work-in-progress commits, use `WIP:` prefix to run fast checks only:

```bash
git commit -m "WIP: experimenting with optimization"

# Output:
# WIP commit detected - running FAST checks only
# [✓] Compilation passed
# [✓] EUnit passed
# ✓ FAST CHECKS PASSED
```

Supported prefixes for fast mode:
- `WIP:` - Work in progress
- `FIXUP:` - Fixup commit
- `TODO:` - Incomplete implementation

### Manual Quality Gate

Run quality gate manually without committing:

```bash
# Full quality gate
./tools/quality-gate.sh

# Fast mode (compile + tests only)
./tools/quality-gate.sh --fast

# Compile only
./tools/quality-gate.sh --compile-only

# Help
./tools/quality-gate.sh --help
```

### Bypassing Hooks (Emergency Only)

**WARNING:** Bypassing hooks violates quality standards. Only use in emergencies.

```bash
# Bypass pre-commit hook
git commit --no-verify -m "Emergency fix"

# Bypass pre-push hook
git push --no-verify
```

**When to bypass:**
- Critical production hotfix (must be fixed immediately)
- Build system issues (hooks themselves are broken)
- Time-sensitive security patches

**After bypassing:**
1. Create follow-up ticket to fix quality issues
2. Run quality gate manually: `./tools/quality-gate.sh`
3. Fix all reported issues
4. Commit fixes with hooks enabled

## Configuration

### Quality Gate Thresholds

Edit `tools/quality-gate.sh`:

```bash
# Test coverage requirement
REQUIRED_TEST_COVERAGE=80  # Default: 80%

# Dialyzer warnings tolerance
MAX_DIALYZER_WARNINGS=0    # Default: 0 (strict)

# Xref warnings tolerance
MAX_XREF_WARNINGS=0        # Default: 0 (strict)
```

### Performance Regression

Edit `.git/hooks/pre-push`:

```bash
# Maximum allowed performance regression
MAX_REGRESSION_PERCENT=10  # Default: 10%

# Enable/disable benchmarks
BENCHMARK_ENABLED=true     # Default: true
```

### Baseline Performance

Update baseline after intentional performance changes:

```bash
# Run benchmarks and save as new baseline
./scripts/bench/run_quick_benchmarks.sh > bench/performance_baseline.txt

# Commit updated baseline
git add bench/performance_baseline.txt
git commit -m "Update performance baseline after optimization"
```

## Troubleshooting

### Hook Not Running

```bash
# Check if hook is executable
ls -lah .git/hooks/pre-commit

# Make executable if needed
chmod +x .git/hooks/pre-commit
chmod +x .git/hooks/pre-push
```

### Quality Gate Fails

```bash
# Run quality gate manually for detailed output
./tools/quality-gate.sh

# Check specific gates
TERM=dumb rebar3 compile  # Compilation
rebar3 xref               # Cross-reference
rebar3 dialyzer           # Type checking
rebar3 eunit              # Unit tests
rebar3 ct                 # Integration tests
rebar3 cover              # Coverage
```

### Dialyzer Takes Too Long

First run builds PLT (5-10 minutes). Subsequent runs are fast (1-2 minutes).

```bash
# Build PLT manually
rebar3 dialyzer --plt

# Check PLT status
ls -lh _build/default/rebar3_*_plt
```

### Coverage Below Threshold

```bash
# View detailed coverage report
rebar3 cover
open _build/test/cover/index.html

# Identify uncovered modules
grep -E "^[^|]*\|[ ]*[0-7][0-9]%" _build/test/cover/*.txt
```

### Performance Regression

```bash
# View benchmark results
cat /tmp/erlmcp_benchmark_prepush.log

# Compare with baseline
diff bench/performance_baseline.txt /tmp/erlmcp_benchmark_prepush.log

# Run full benchmark suite
./scripts/bench/run_all_benchmarks.sh

# Profile specific module
erl -pa _build/default/lib/*/ebin -eval 'eprof:start(), ...'
```

## Integration with CI/CD

### GitHub Actions

```yaml
# .github/workflows/quality-gate.yml
name: Quality Gate

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  quality-gate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26.0'
          rebar3-version: '3.22.0'

      - name: Cache PLT
        uses: actions/cache@v3
        with:
          path: _build/default/rebar3_*_plt
          key: plt-${{ runner.os }}-${{ hashFiles('rebar.config') }}

      - name: Run Quality Gate
        run: ./tools/quality-gate.sh

      - name: Upload Coverage
        uses: codecov/codecov-action@v3
        with:
          files: _build/test/cover/coverage.xml
```

### GitLab CI

```yaml
# .gitlab-ci.yml
quality-gate:
  stage: test
  image: erlang:26
  cache:
    paths:
      - _build/default/rebar3_*_plt
  script:
    - ./tools/quality-gate.sh
  artifacts:
    reports:
      coverage_report:
        coverage_format: cobertura
        path: _build/test/cover/coverage.xml
```

## Best Practices

### 1. Run Quality Gate Locally

Before committing, run quality gate manually to catch issues early:

```bash
./tools/quality-gate.sh
```

### 2. Keep Commits Small

Smaller commits = faster quality gates:
- Focus on single feature/fix
- Avoid mixing refactoring with features
- Split large changes into logical commits

### 3. Write Tests First (TDD)

Chicago School TDD approach:
1. Write failing test
2. Implement minimal code to pass
3. Refactor with confidence (tests catch regressions)
4. Commit with hooks enabled

### 4. Monitor Coverage Trends

Track coverage over time:

```bash
# After each quality gate run
echo "$(date): $(grep 'total' _build/test/cover/*.txt)" >> coverage_history.log
```

### 5. Update Baselines After Major Changes

After architectural changes or intentional performance tradeoffs:

```bash
# Update performance baseline
./scripts/bench/run_quick_benchmarks.sh > bench/performance_baseline.txt
git add bench/performance_baseline.txt
git commit -m "Update performance baseline after architecture refactor"
```

## Metrics and Reporting

### Quality Gate Statistics

Track quality gate execution time:

```bash
# Add to tools/quality-gate.sh
echo "$(date),$(duration_seconds),$(failures),$(warnings)" >> metrics/quality_gate.csv
```

### Coverage Trends

```bash
# Extract coverage percentage
coverage=$(grep 'total' _build/test/cover/*.txt | grep -oE '[0-9]+%')
echo "$(date),$coverage" >> metrics/coverage_trends.csv
```

### Performance Baselines

```bash
# Track performance over time
grep "ops/sec" bench/performance_baseline.txt >> metrics/perf_history.csv
```

## Support

For issues or questions:

1. Check logs: `/tmp/erlmcp_*.log`
2. Run manual quality gate: `./tools/quality-gate.sh --help`
3. Review documentation: `docs/quality-gates/`
4. Open issue: https://github.com/erlmcp/erlmcp/issues

## References

- **Quality Standards**: `CLAUDE.md` - Project quality requirements
- **Testing Guide**: `docs/testing/TESTING.md`
- **Benchmark Guide**: `docs/benchmarking/BENCHMARKING.md`
- **CI/CD Setup**: `docs/ci-cd/GITHUB_ACTIONS.md`
