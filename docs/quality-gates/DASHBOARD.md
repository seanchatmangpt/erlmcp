# ERLMCP Quality Gate Dashboard

## Overview

The erlmcp quality gate dashboard provides real-time visibility into code quality metrics and build health. It monitors six critical quality gates and provides visual feedback through color-coded output, JSON reports, and continuous monitoring.

## Quality Gates

### 1. Compilation Status

**What it checks:**
- Source code compiles without errors
- BEAM files are generated correctly
- Compiler warnings are tracked

**Pass criteria:**
- ✅ Zero compilation errors
- ⚠️ Warnings are tracked but don't fail the build

**How to fix failures:**
```bash
# View compilation errors
TERM=dumb rebar3 compile

# Common issues:
# - Syntax errors → Fix Erlang syntax
# - Missing dependencies → Add to rebar.config
# - Module name mismatch → Ensure -module() matches filename
```

### 2. Test Status

**What it checks:**
- All EUnit tests pass
- Test pass rate percentage
- Test failures are reported

**Pass criteria:**
- ✅ 100% test pass rate required
- ⚠️ <100% triggers warning (still fails gate)
- ❌ Any test failures fail the gate

**How to fix failures:**
```bash
# Run tests with verbose output
rebar3 eunit --verbose

# Run specific test module
rebar3 eunit --module=erlmcp_client_tests

# Common issues:
# - Logic errors → Fix implementation
# - Race conditions → Add proper synchronization
# - Resource cleanup → Fix teardown in tests
```

### 3. Code Coverage

**What it checks:**
- Percentage of code exercised by tests
- Coverage data availability
- Meets minimum threshold (80%)

**Pass criteria:**
- ✅ ≥80% code coverage required
- ⚠️ <80% triggers warning
- ❌ No coverage data fails the gate

**How to fix failures:**
```bash
# Generate coverage report
rebar3 cover

# View detailed coverage
rebar3 cover --verbose

# Common solutions:
# - Add tests for uncovered code paths
# - Test error handling branches
# - Add integration tests for complex flows
```

### 4. Dialyzer Type Checking

**What it checks:**
- Type specifications are correct
- No type mismatches
- No unreachable code
- No undefined function calls

**Pass criteria:**
- ✅ Zero Dialyzer warnings
- ⚠️ Warnings present but analysis completed
- ❌ Dialyzer crashes fail the gate

**How to fix failures:**
```bash
# Run Dialyzer manually
rebar3 dialyzer

# Update PLT (if stale)
rebar3 dialyzer --update-plt

# Common issues:
# - Add -spec declarations
# - Fix type mismatches in function calls
# - Remove unreachable code
# - Fix opaque type violations
```

### 5. Cross-Reference Analysis (Xref)

**What it checks:**
- No undefined function calls
- No unused local functions
- No deprecated function usage
- Module dependencies are valid

**Pass criteria:**
- ✅ Zero Xref warnings
- ⚠️ Warnings present but analysis completed
- ❌ Analysis failure fails the gate

**How to fix failures:**
```bash
# Run Xref manually
rebar3 xref

# Common issues:
# - Fix undefined function calls
# - Remove unused functions
# - Replace deprecated functions
# - Add missing dependencies to rebar.config
```

### 6. Benchmark Status

**What it checks:**
- Performance regression detection
- Throughput measurements
- Comparison with baseline

**Pass criteria:**
- ✅ No regression or improvement
- ⚠️ <10% regression (within threshold)
- ❌ ≥10% regression fails the gate
- ℹ️ No baseline = informational only

**How to fix failures:**
```bash
# Run benchmarks
make benchmark-quick

# Investigate regression
make benchmark-analysis

# Common solutions:
# - Profile code to find bottlenecks
# - Optimize hot paths
# - Review recent changes
# - Update baseline if intentional change
```

## Tools

### 1. Quality Dashboard (quality-dashboard.sh)

Visual dashboard showing all quality gates with color-coded status.

**Usage:**
```bash
# Run dashboard once
./tools/quality-dashboard.sh

# With custom project root
PROJECT_ROOT=/path/to/project ./tools/quality-dashboard.sh
```

**Output format:**
```
═══════════════════════════════════════════════════════════════
  ERLMCP QUALITY DASHBOARD
═══════════════════════════════════════════════════════════════
Generated: 2026-01-28 14:30:00
Project: /Users/sac/erlmcp

▶ Compilation Status
───────────────────────────────────────────────────────────────
  ✅ PASS - Compiled 42 modules

▶ Test Status
───────────────────────────────────────────────────────────────
  ✅ PASS - All 156 tests passed (100%)

▶ Code Coverage
───────────────────────────────────────────────────────────────
  ✅ PASS - Coverage: 84% (≥80% required)

▶ Dialyzer Type Checking
───────────────────────────────────────────────────────────────
  ✅ PASS - Type checking passed (0 warnings)

▶ Cross-Reference Analysis
───────────────────────────────────────────────────────────────
  ✅ PASS - No cross-reference issues found

▶ Benchmark Status
───────────────────────────────────────────────────────────────
  ✅ PASS - No regression - 2.3% improvement

═══════════════════════════════════════════════════════════════
  OVERALL STATUS
═══════════════════════════════════════════════════════════════
✅ ALL QUALITY GATES PASSED
Project is ready for deployment
```

**Exit codes:**
- `0` - All gates passed
- `1` - One or more gates failed

### 2. Quality Gate JSON (quality-gate-json.sh)

Machine-readable JSON output for CI/CD integration.

**Usage:**
```bash
# Generate JSON report
./tools/quality-gate-json.sh > quality-report.json

# Use in CI pipeline
./tools/quality-gate-json.sh | jq '.overall_status'
```

**Output format:**
```json
{
  "overall_status": "pass",
  "timestamp": "2026-01-28T14:30:00Z",
  "project_root": "/Users/sac/erlmcp",
  "gates": {
    "compilation": {
      "status": "pass",
      "errors": 0,
      "warnings": 0,
      "modules_compiled": 42,
      "timestamp": "2026-01-28T14:30:00Z"
    },
    "tests": {
      "status": "pass",
      "passed": 156,
      "failed": 0,
      "total": 156,
      "pass_rate": 1.0,
      "timestamp": "2026-01-28T14:30:00Z"
    },
    "coverage": {
      "status": "pass",
      "percentage": 0.84,
      "threshold": 0.80,
      "timestamp": "2026-01-28T14:30:00Z"
    },
    "dialyzer": {
      "status": "pass",
      "warnings": 0,
      "errors": 0,
      "timestamp": "2026-01-28T14:30:00Z"
    },
    "xref": {
      "status": "pass",
      "warnings": 0,
      "undefined_calls": 0,
      "timestamp": "2026-01-28T14:30:00Z"
    },
    "benchmarks": {
      "status": "pass",
      "regression": 0.023,
      "regression_percentage": 2.3,
      "current_throughput": 2753421,
      "baseline_throughput": 2692000,
      "timestamp": "2026-01-28T14:30:00Z"
    }
  }
}
```

### 3. Quality Watch (quality-watch.sh)

Continuous monitoring with real-time updates and alerts.

**Usage:**
```bash
# Watch with default settings (30s refresh)
./tools/quality-watch.sh

# Custom refresh interval (60s)
./tools/quality-watch.sh --interval 60

# Disable sound alerts
./tools/quality-watch.sh --no-sound

# Disable desktop notifications
./tools/quality-watch.sh --no-notify

# Both
./tools/quality-watch.sh --no-sound --no-notify
```

**Features:**
- 🔄 Auto-refresh every 30 seconds (configurable)
- 🔔 Sound alerts on status changes
- 📢 Desktop notifications (macOS/Linux)
- 📊 Uptime tracking
- 📈 Failure count tracking
- ⌨️ Ctrl+C to stop gracefully

**Environment variables:**
```bash
# Custom configuration
export REFRESH_INTERVAL=60        # Seconds between refreshes
export ENABLE_SOUND=false         # Disable beeps
export ENABLE_NOTIFICATION=false  # Disable notifications

./tools/quality-watch.sh
```

## CI/CD Integration

### GitHub Actions

```yaml
name: Quality Gates

on: [push, pull_request]

jobs:
  quality:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '25'
          rebar3-version: '3.22'

      - name: Run Quality Gates
        run: |
          chmod +x tools/quality-gate-json.sh
          ./tools/quality-gate-json.sh > quality-report.json

      - name: Check Status
        run: |
          STATUS=$(jq -r '.overall_status' quality-report.json)
          if [[ "$STATUS" != "pass" ]]; then
            echo "Quality gates failed!"
            jq . quality-report.json
            exit 1
          fi

      - name: Upload Report
        uses: actions/upload-artifact@v3
        if: always()
        with:
          name: quality-report
          path: quality-report.json
```

### GitLab CI

```yaml
quality_gates:
  stage: test
  script:
    - chmod +x tools/quality-gate-json.sh
    - ./tools/quality-gate-json.sh > quality-report.json
    - |
      STATUS=$(jq -r '.overall_status' quality-report.json)
      if [[ "$STATUS" != "pass" ]]; then
        echo "Quality gates failed!"
        jq . quality-report.json
        exit 1
      fi
  artifacts:
    reports:
      junit: quality-report.json
    when: always
```

### Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

echo "Running quality gates..."

if ! ./tools/quality-dashboard.sh; then
    echo ""
    echo "❌ Quality gates failed! Commit rejected."
    echo "Fix issues and try again."
    exit 1
fi

echo "✅ Quality gates passed!"
exit 0
```

## Thresholds and Configuration

### Current Thresholds

| Gate | Threshold | Configurable |
|------|-----------|--------------|
| Compilation | 0 errors | No |
| Tests | 100% pass rate | `TEST_PASS_THRESHOLD` |
| Coverage | ≥80% | `COVERAGE_THRESHOLD` |
| Dialyzer | 0 errors | No |
| Xref | 0 errors | No |
| Benchmarks | <10% regression | `BENCHMARK_THRESHOLD` |

### Customizing Thresholds

Edit the scripts to adjust thresholds:

```bash
# quality-dashboard.sh and quality-gate-json.sh
BENCHMARK_THRESHOLD="-10"  # -10% regression allowed
COVERAGE_THRESHOLD="80"     # 80% minimum
TEST_PASS_THRESHOLD="100"   # 100% pass rate required
```

## Troubleshooting

### Dashboard shows "No coverage data found"

**Solution:**
```bash
# Generate coverage first
rebar3 eunit
rebar3 cover
```

### Dialyzer takes forever on first run

**Expected behavior:** First run builds PLT (5-10 minutes)

**Speed up subsequent runs:**
```bash
# PLT is cached in _build/default/*_plt
# Only rebuilt when dependencies change
```

### Watch script doesn't send notifications

**macOS:**
- Notifications require terminal to have permission
- System Preferences → Notifications → Terminal → Allow

**Linux:**
```bash
# Install notify-send
sudo apt-get install libnotify-bin  # Debian/Ubuntu
sudo dnf install libnotify           # Fedora
```

### JSON output has parsing errors

**Check for:**
```bash
# Verify JSON is valid
./tools/quality-gate-json.sh | jq .

# Check for stderr contamination
./tools/quality-gate-json.sh 2>/dev/null | jq .
```

## Best Practices

1. **Run dashboard before commits**
   ```bash
   ./tools/quality-dashboard.sh && git commit
   ```

2. **Use watch during development**
   ```bash
   # In separate terminal
   ./tools/quality-watch.sh --interval 60
   ```

3. **Check JSON in CI/CD**
   ```bash
   ./tools/quality-gate-json.sh | jq '.overall_status'
   ```

4. **Monitor trends over time**
   ```bash
   # Save reports with timestamps
   ./tools/quality-gate-json.sh > "reports/quality-$(date +%Y%m%d-%H%M%S).json"
   ```

5. **Set baseline before major changes**
   ```bash
   # Save current state as baseline
   cp bench/results/latest.json bench/baseline.json
   ```

## Related Documentation

- [Quality Gates Guide](../quality-gates/README.md) - Overall quality strategy
- [Testing Guide](../testing/TESTING.md) - Test writing guidelines
- [Benchmarking Guide](../benchmarking/BENCHMARKING.md) - Performance testing
- [CI/CD Guide](../ci-cd/README.md) - Continuous integration

## Support

For issues or questions:
- GitHub Issues: https://github.com/yourusername/erlmcp/issues
- Documentation: docs/quality-gates/
- Examples: See `.github/workflows/` for CI examples
