# ERLMCP Quality Gate System

## Quick Start

```bash
# Install git hooks
./tools/install-hooks.sh

# Test quality gate manually
./tools/quality-gate.sh --compile-only

# Make changes and commit (hooks run automatically)
git commit -m "Your commit message"

# Push (benchmarks run automatically)
git push origin main
```

## What is This?

The ERLMCP Quality Gate System is a comprehensive pre-commit and pre-push hook infrastructure that enforces **Lean Six Sigma quality standards** with zero tolerance for defects.

### Quality Standards Enforced

1. **Compilation:** 0 errors (via `rebar3 compile`)
2. **Cross-reference:** 0 undefined functions (via `rebar3 xref`)
3. **Type checking:** 0 dialyzer warnings (via `rebar3 dialyzer`)
4. **Unit tests:** 100% pass rate (via `rebar3 eunit`)
5. **Integration tests:** 100% pass rate (via `rebar3 ct`)
6. **Test coverage:** ≥80% (via `rebar3 cover`)
7. **Performance:** <10% regression (via benchmarks)

## Components

### 1. Core Quality Gate Script
**Path:** `/Users/sac/erlmcp/tools/quality-gate.sh`

Main quality checker that runs all 7 gates.

```bash
# Run all gates (default)
./tools/quality-gate.sh

# Run fast checks (compile + tests)
./tools/quality-gate.sh --fast

# Run compile only
./tools/quality-gate.sh --compile-only

# Show help
./tools/quality-gate.sh --help
```

### 2. Pre-Commit Hook
**Path:** `/Users/sac/erlmcp/.git/hooks/pre-commit`

Runs automatically before each commit.

**Features:**
- Executes full quality gate (1-3 minutes)
- Detects WIP commits (runs fast checks only)
- Skips during merge/rebase
- Blocks commit if quality gates fail

**Bypass (NOT recommended):**
```bash
git commit --no-verify -m "Emergency fix"
```

### 3. Pre-Push Hook
**Path:** `/Users/sac/erlmcp/.git/hooks/pre-push`

Runs automatically before each push.

**Features:**
- Executes full quality gate
- Runs quick benchmark suite (2-5 minutes)
- Detects performance regressions (>10%)
- Blocks push if regression detected

**Bypass (NOT recommended):**
```bash
git push --no-verify
```

### 4. Quick Benchmark Script
**Path:** `/Users/sac/erlmcp/scripts/bench/run_quick_benchmarks.sh`

Fast benchmark suite for pre-push validation.

```bash
# Run benchmarks manually
./scripts/bench/run_quick_benchmarks.sh

# Update baseline after optimization
./scripts/bench/run_quick_benchmarks.sh > bench/performance_baseline.txt
git add bench/performance_baseline.txt
git commit -m "Update performance baseline"
```

### 5. Hook Installer
**Path:** `/Users/sac/erlmcp/tools/install-hooks.sh`

One-command installation of all hooks.

```bash
./tools/install-hooks.sh
```

## Workflow Examples

### Normal Development

```bash
# 1. Make changes
vim src/my_module.erl

# 2. Run quality gate manually (optional but recommended)
./tools/quality-gate.sh

# 3. Commit (hooks run automatically)
git commit -m "Add new feature"
# Output:
# [✓] Compilation passed: 6 applications compiled
# [✓] Xref passed: 0 warnings
# [✓] Dialyzer passed: 0 warnings
# [✓] EUnit passed: 157/157 tests passed
# [✓] Common Test passed
# [✓] Coverage passed: 87%
# ✓ ALL GATES PASSED

# 4. Push (benchmarks run automatically)
git push origin main
# Output:
# [1/3] Running full quality gate...
# ✓ Quality gate passed
# [2/3] Running quick performance benchmarks...
# ✓ Benchmarks completed
# [3/3] Checking for performance regressions...
# ✓ Performance within acceptable range (2.3% regression)
# ✓ PRE-PUSH QUALITY GATE PASSED
```

### WIP Development (Fast Mode)

```bash
# During iterative development, use WIP commits
git commit -m "WIP: experimenting with optimization"
# Output:
# WIP commit detected - running FAST checks only
# [✓] Compilation passed
# [✓] EUnit passed
# ✓ FAST CHECKS PASSED (30 seconds instead of 3 minutes)

# Final commit runs full checks
git commit -m "Optimize routing algorithm"
# Full quality gate runs automatically
```

### Emergency Bypass

**Only use in genuine emergencies** (critical hotfix, time-sensitive security patch):

```bash
# Bypass pre-commit
git commit --no-verify -m "HOTFIX: Critical security patch"

# Bypass pre-push
git push --no-verify

# IMPORTANT: Create follow-up ticket to fix quality issues
# Run quality gate manually and fix all reported issues
./tools/quality-gate.sh
```

## Configuration

### Quality Thresholds

Edit `/Users/sac/erlmcp/tools/quality-gate.sh`:

```bash
# Line 27-29
REQUIRED_TEST_COVERAGE=80  # Minimum test coverage (80%)
MAX_DIALYZER_WARNINGS=0    # Zero tolerance for type warnings
MAX_XREF_WARNINGS=0        # Zero tolerance for undefined functions
```

### Performance Regression

Edit `/Users/sac/erlmcp/.git/hooks/pre-push`:

```bash
# Line 20-21
MAX_REGRESSION_PERCENT=10  # Maximum allowed regression (10%)
BENCHMARK_ENABLED=true     # Enable/disable benchmarks
```

## Troubleshooting

### Hook Not Running

```bash
# Check if hook is executable
ls -lah .git/hooks/pre-commit .git/hooks/pre-push

# Make executable if needed
chmod +x .git/hooks/pre-commit
chmod +x .git/hooks/pre-push
chmod +x tools/quality-gate.sh
```

### Quality Gate Fails

```bash
# Run quality gate manually for detailed output
./tools/quality-gate.sh

# Check specific gates individually
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
# Build PLT manually (one-time)
rebar3 dialyzer --plt

# Verify PLT exists
ls -lh _build/default/rebar3_*_plt
```

### Coverage Below Threshold

```bash
# View detailed coverage report
rebar3 cover
open _build/test/cover/index.html

# Find uncovered modules
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

# Update baseline if regression is intentional
./scripts/bench/run_quick_benchmarks.sh > bench/performance_baseline.txt
git add bench/performance_baseline.txt
git commit -m "Update baseline after architecture refactor"
```

## Documentation

- **[PRE_COMMIT_HOOKS.md](PRE_COMMIT_HOOKS.md)** - Comprehensive guide (28KB)
  - Hook architecture and workflow diagrams
  - Detailed gate descriptions
  - Installation and configuration
  - Usage examples and best practices
  - CI/CD integration (GitHub Actions, GitLab CI)
  - Troubleshooting and metrics

- **[IMPLEMENTATION_SUMMARY.md](IMPLEMENTATION_SUMMARY.md)** - Technical summary (16KB)
  - Files created and their purposes
  - Quality standards reference
  - Configuration options
  - Integration with existing tools
  - Future enhancements

- **[DASHBOARD.md](DASHBOARD.md)** - Quality metrics dashboard (13KB)
  - Real-time quality gate status
  - Historical trends and metrics
  - Performance baselines

## CI/CD Integration

### GitHub Actions

Create `.github/workflows/quality-gate.yml`:

```yaml
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
      - name: Cache Dialyzer PLT
        uses: actions/cache@v3
        with:
          path: _build/default/rebar3_*_plt
          key: plt-${{ runner.os }}-${{ hashFiles('rebar.config') }}
      - name: Run Quality Gate
        run: ./tools/quality-gate.sh
```

### GitLab CI

Add to `.gitlab-ci.yml`:

```yaml
quality-gate:
  stage: test
  image: erlang:26
  cache:
    paths:
      - _build/default/rebar3_*_plt
  script:
    - ./tools/quality-gate.sh
```

## Best Practices

1. **Run quality gate locally before committing**
   ```bash
   ./tools/quality-gate.sh  # Catch issues early
   ```

2. **Keep commits small**
   - Faster quality gates
   - Easier to debug failures
   - Better git history

3. **Use WIP commits during development**
   ```bash
   git commit -m "WIP: exploring variants"  # Fast checks (30s)
   git commit -m "Optimize algorithm"       # Full checks (3min)
   ```

4. **Update baselines after major changes**
   ```bash
   ./scripts/bench/run_quick_benchmarks.sh > bench/performance_baseline.txt
   git add bench/performance_baseline.txt
   git commit -m "Update baseline after refactor"
   ```

5. **Monitor coverage trends**
   ```bash
   rebar3 cover
   grep 'total' _build/test/cover/*.txt | tee -a coverage_history.log
   ```

## Support

For issues or questions:

1. Check logs: `/tmp/erlmcp_*.log`
2. Run manual quality gate: `./tools/quality-gate.sh`
3. Review documentation: `docs/quality-gates/`
4. Open issue: https://github.com/erlmcp/erlmcp/issues

## Status

- **Implementation:** ✅ Complete
- **Testing:** ✅ Verified (compile-only mode tested successfully)
- **Documentation:** ✅ Comprehensive (28KB guide + 16KB summary)
- **Integration:** ✅ Compatible with existing erlmcp tools
- **CI/CD:** ✅ GitHub Actions and GitLab CI examples provided

---

**Quality Gate System: OPERATIONAL**

All scripts are executable, hooks are installed, and the system is ready for use.
