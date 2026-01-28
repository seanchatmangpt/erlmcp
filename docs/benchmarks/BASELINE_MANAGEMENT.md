# Baseline Management - erlmcp Performance System

## Overview

The baseline management system provides **automated regression detection** for erlmcp's performance. It captures stable reference measurements, compares new results against baselines, and blocks merges that introduce regressions.

## System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                   Baseline Management System                  │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  [1] Capture Baseline                                         │
│      └─> Run all benchmarks                                   │
│          └─> Save to bench/baselines/YYYY-MM-DD_vX.Y.Z.json  │
│              └─> Commit to git                                │
│                                                               │
│  [2] Compare to Baseline (CI/CD)                              │
│      └─> Run benchmarks                                       │
│          └─> Compare to latest baseline                       │
│              └─> Flag regressions (>10% slower)               │
│                  └─> EXIT 1 → Block merge                     │
│                                                               │
│  [3] Update Baseline (Post-Optimization)                      │
│      └─> Show diff (old vs new)                               │
│          └─> Require justification                            │
│              └─> Create git commit                            │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

## Tools

### 1. baseline-capture.sh

**Purpose**: Capture comprehensive performance baseline

**Usage**:
```bash
./tools/baseline-capture.sh [OPTIONS]

Options:
  --no-commit       Don't commit baseline to git
  --version VER     Override version (default: from erlmcp.app.src)
  -h, --help        Show help
```

**Output**: `bench/baselines/YYYY-MM-DD_vX.Y.Z.json`

**When to run**:
- Before releases (tag with version)
- After major optimizations
- After infrastructure changes (OTP upgrade)
- When establishing new benchmark

**Process**:
1. Compile project (`rebar3 compile`)
2. Run benchmarks:
   - `core_ops_100k` - 100K in-memory operations
   - `tcp_quick_1k` - 1K TCP connections
   - `stress_30s_100k_ops` - 30 second sustained load
3. Consolidate results into JSON
4. Commit to git with metadata

**Example**:
```bash
$ ./tools/baseline-capture.sh

========================================
erlmcp Baseline Capture
========================================
Version: 0.7.0
Output: bench/baselines/2026-01-28_v0.7.0.json

[1/4] Compiling project...
✓ Compiled

[2/4] Running benchmarks...
  Running core_ops (100K operations)...
  Running network_real (TCP 1K connections)...
  Running stress (30 seconds sustained)...
✓ Benchmarks complete

[3/4] Consolidating results...
✓ Baseline saved: bench/baselines/2026-01-28_v0.7.0.json
✓ Baseline JSON valid

========================================
Baseline Summary
========================================
core_ops_100k:
  Throughput: 2690088.37 msg/s
  Latency p95: 83 µs
  Memory: 19.1 MiB

[4/4] Committing baseline to git...
✓ Git commit: 3a8f2c1

✓ Baseline capture complete
```

### 2. baseline-compare.sh

**Purpose**: Compare current performance to baseline, detect regressions

**Usage**:
```bash
./tools/baseline-compare.sh [OPTIONS]

Options:
  --baseline FILE   Compare to specific baseline (default: latest)
  --threshold PCT   Regression threshold (default: 10)
  --html-only       Skip benchmark run, use latest results
  -h, --help        Show help
```

**Exit codes**:
- `0` - No regressions detected
- `1` - Regressions detected (blocks CI)
- `2` - Error (no baseline, benchmark failure)

**Regression thresholds**:
- **Throughput**: -10% (e.g., 2.69M → 2.42M msg/s)
- **Latency p95**: +10% (e.g., 83µs → 91µs)
- **Memory**: +20% (e.g., 19.1 MiB → 22.9 MiB)

**Output**:
- Terminal: Comparison table with status
- HTML: `bench/baselines/comparison_TIMESTAMP.html`

**Example (no regressions)**:
```bash
$ ./tools/baseline-compare.sh

========================================
erlmcp Performance Comparison
========================================
Baseline: 2026-01-28_v0.7.0.json

[1/3] Running benchmarks...
[2/3] Comparing results...

Performance Comparison (vs 2026-01-28_v0.7.0)

core_ops_100k:
  Throughput:
    Baseline: 2690088.37 msg/s
    Current:  2745123.92 msg/s
    Delta:    +2.1% ✓

  Latency p95:
    Baseline: 83 µs
    Current:  79 µs
    Delta:    -4.8% ✓

  Memory:
    Baseline: 19.1 MiB
    Current:  19.8 MiB
    Delta:    +3.7% ✓

[3/3] Generating HTML report...
✓ HTML report: bench/baselines/comparison_1769567400.html

✓ PASS: No regressions detected
```

**Example (regression detected)**:
```bash
$ ./tools/baseline-compare.sh

Performance Comparison (vs 2026-01-28_v0.7.0)

tcp_quick_1k:
  Throughput:
    Baseline: 372000 msg/s
    Current:  311000 msg/s
    Delta:    -16.4% ❌ REGRESSION

✗ FAIL: 1 regression(s) detected
See: bench/baselines/comparison_1769567400.html
```

### 3. baseline-update.sh

**Purpose**: Update baseline after intentional performance changes

**Usage**:
```bash
./tools/baseline-update.sh [OPTIONS]

Options:
  --auto-yes       Skip confirmation prompts
  --reason TEXT    Justification for update
  -h, --help       Show help
```

**Interactive workflow**:
1. Show diff (old vs new metrics)
2. Check if changes are significant (>5%)
3. Require justification
4. Confirm update
5. Create new baseline
6. Commit to git with explanation

**When to run**:
- After optimization (improved performance)
- After intentional tradeoff (e.g., more memory for speed)
- After infrastructure change (OTP upgrade)
- After adding new workload

**Example**:
```bash
$ ./tools/baseline-update.sh

========================================
erlmcp Baseline Update
========================================
Old baseline: 2026-01-28_v0.7.0.json
New baseline: 2026-02-15_v0.8.0.json

[1/4] Comparing old vs new...

Benchmark                 Old             New             Change
------------------------- --------------- --------------- ----------
core_ops_100k (msg/s)     2690088         3100000         +15.2%
tcp_quick_1k (msg/s)      372000          353000          -5.1%
stress_30s (msg/s)        242000          251000          +3.7%

[2/4] Justification required

Why is this baseline update necessary?

Reason: gproc migration improved registry performance

[3/4] Confirmation

This will:
  1. Create new baseline: 2026-02-15_v0.8.0.json
  2. Commit to git with justification
  3. Old baseline remains in git history

Proceed? (y/n): y

[4/4] Committing to git...
✓ Git commit: b4e7d9a

✓ Baseline update complete

New baseline: bench/baselines/2026-02-15_v0.8.0.json
Git commit: b4e7d9a
```

## Baseline File Format

### Structure

```json
{
  "version": "0.7.0",
  "timestamp": 1769567361,
  "date": "2026-01-28T14:22:41Z",
  "environment": {
    "os": "darwin",
    "os_version": "25.2.0",
    "arch": "x86_64",
    "hostname": "build-server",
    "otp_version": "27",
    "erts_version": "15.2.7.1",
    "cores": 8
  },
  "benchmarks": {
    "core_ops_100k": {
      "throughput_msg_per_s": 2690088.37,
      "latency_p50_us": 0.0,
      "latency_p95_us": 83.0,
      "latency_p99_us": 98.0,
      "memory_delta_mib": 19.1
    },
    "tcp_quick_1k": {
      "throughput_msg_per_s": 372000,
      "latency_p50_us": 12.0,
      "latency_p95_us": 245.0,
      "latency_p99_us": 512.0,
      "memory_delta_mib": 48.5
    },
    "stress_30s_100k_ops": {
      "throughput_msg_per_s": 242000,
      "latency_p50_us": 8.0,
      "latency_p95_us": 156.0,
      "latency_p99_us": 312.0,
      "memory_delta_mib": 75.2
    }
  }
}
```

### Metrology Compliance

All metrics follow erlmcp metrology standards:

| Metric | Unit | Scope | Precision |
|--------|------|-------|-----------|
| `throughput_msg_per_s` | msg/s | per_node | N/A |
| `latency_pXX_us` | µs | per_request | microsecond |
| `memory_delta_mib` | MiB | per_node | N/A |

See `docs/metrology/METRICS_GLOSSARY.md` for full specifications.

## CI/CD Integration

### GitHub Actions Workflow

```yaml
# .github/workflows/performance.yml
name: Performance Regression Check

on:
  pull_request:
    branches: [main]
  push:
    branches: [main]

jobs:
  performance:
    runs-on: ubuntu-latest
    
    steps:
      - name: Checkout code
        uses: actions/checkout@v4
        with:
          fetch-depth: 0  # Full history for baseline access
      
      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '27'
          rebar3-version: '3.23'
      
      - name: Compile
        run: rebar3 compile
      
      - name: Fetch latest baseline
        run: |
          git fetch origin main:main
          ls -la bench/baselines/
      
      - name: Run performance comparison
        id: perf_check
        run: |
          ./tools/baseline-compare.sh
        continue-on-error: true
      
      - name: Upload comparison report
        if: always()
        uses: actions/upload-artifact@v4
        with:
          name: performance-comparison
          path: bench/baselines/comparison_*.html
      
      - name: Comment on PR
        if: failure() && github.event_name == 'pull_request'
        uses: actions/github-script@v7
        with:
          script: |
            const fs = require('fs');
            const report = fs.readFileSync('bench/baselines/comparison_*.html', 'utf8');
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: '## ⚠️ Performance Regression Detected\n\nSee artifacts for detailed comparison report.'
            });
      
      - name: Fail on regression
        if: steps.perf_check.outcome == 'failure'
        run: exit 1
```

### Jenkins Pipeline

```groovy
pipeline {
    agent any
    
    stages {
        stage('Performance Check') {
            steps {
                sh 'rebar3 compile'
                sh './tools/baseline-compare.sh'
            }
        }
    }
    
    post {
        always {
            archiveArtifacts artifacts: 'bench/baselines/comparison_*.html'
        }
        failure {
            emailext (
                subject: "Performance Regression Detected - ${env.JOB_NAME} #${env.BUILD_NUMBER}",
                body: "See attached report",
                attachmentsPattern: 'bench/baselines/comparison_*.html'
            )
        }
    }
}
```

### GitLab CI

```yaml
# .gitlab-ci.yml
performance:
  stage: test
  script:
    - rebar3 compile
    - ./tools/baseline-compare.sh
  artifacts:
    when: always
    paths:
      - bench/baselines/comparison_*.html
    reports:
      junit: bench/baselines/comparison_*.html
  allow_failure: false
```

## Workflow Examples

### Example 1: Release Preparation

```bash
# Before v0.8.0 release

# 1. Ensure clean state
git checkout main
git pull

# 2. Capture baseline
./tools/baseline-capture.sh

# Output:
# ✓ Baseline saved: bench/baselines/2026-02-15_v0.8.0.json
# ✓ Git commit: 7d3a9f2

# 3. Tag release
git tag v0.8.0
git push origin v0.8.0
```

### Example 2: Development Workflow

```bash
# Feature branch: optimize-registry

# 1. Make changes
vim src/erlmcp_registry.erl

# 2. Compile
rebar3 compile

# 3. Check against baseline
./tools/baseline-compare.sh

# Output:
# core_ops_100k:
#   Throughput: 2.69M → 3.10M msg/s (+15.2%) ✓
# ✓ PASS: No regressions (improvement detected!)

# 4. Commit and push
git add src/erlmcp_registry.erl
git commit -m "Optimize registry lookup with gproc"
git push origin optimize-registry

# 5. CI runs baseline-compare.sh automatically
# 6. If passes, update baseline in main
```

### Example 3: Handling Regression

```bash
# PR introduces regression

# CI Output:
# ❌ REGRESSION: tcp_quick_1k throughput -16.4%

# Options:

# A. Fix regression
git revert HEAD
./tools/baseline-compare.sh  # Should pass now

# B. Accept regression with justification (rare)
./tools/baseline-update.sh --reason "Tradeoff: reduced throughput for better memory safety"
```

### Example 4: Infrastructure Change

```bash
# Upgrade OTP 27 → 28

# 1. Update environment
asdf install erlang 28.0

# 2. Run benchmarks
./tools/baseline-compare.sh

# Output:
# core_ops_100k: -3.2% throughput (OTP differences)
# Memory: +8.1% (expected with OTP 28)

# 3. Update baseline
./tools/baseline-update.sh --reason "OTP 28 upgrade - expected memory increase"

# 4. Document in release notes
echo "- Upgraded to OTP 28 (minor memory increase expected)" >> CHANGELOG.md
```

## Best Practices

### 1. Stable Environment

Capture baselines on consistent hardware:
- Same CPU/RAM configuration
- Minimal background processes
- Consistent network conditions (for network benchmarks)
- Same OS and OTP version

### 2. Multiple Runs

Verify stability across 3+ runs:

```bash
for i in {1..3}; do
  ./tools/baseline-compare.sh --html-only
  sleep 60
done

# Check variance - should be <5%
```

### 3. Version Alignment

Tag baselines with release versions:

```bash
# Release v0.8.0
./tools/baseline-capture.sh --version 0.8.0
git tag v0.8.0
git push --tags
```

### 4. Document Changes

Always explain baseline updates:

```bash
# Good justifications:
./tools/baseline-update.sh --reason "gproc migration improved registry perf by 15%"
./tools/baseline-update.sh --reason "OTP 28 upgrade - expected 8% memory increase"
./tools/baseline-update.sh --reason "Optimized binary handling - reduced allocations"

# Bad justifications:
./tools/baseline-update.sh --reason "updated baseline"  # Too vague
```

### 5. Review History

Track baseline evolution:

```bash
# View all baselines
ls -lh bench/baselines/

# View baseline history
git log --oneline -- bench/baselines/

# Compare two baselines
git diff bench/baselines/2026-01-28_v0.7.0.json \
         bench/baselines/2026-02-15_v0.8.0.json

# Show changes in specific baseline
git show 7d3a9f2
```

## Troubleshooting

### Issue: "No baseline found"

**Cause**: No baseline has been captured yet

**Fix**:
```bash
./tools/baseline-capture.sh
```

### Issue: "Regression detected but expected"

**Cause**: Intentional change (optimization, tradeoff)

**Fix**:
```bash
./tools/baseline-update.sh --reason "Detailed justification here"
```

### Issue: "Baseline too old (>6 months)"

**Cause**: Environment changes invalidate old baseline

**Fix**:
```bash
# Re-capture on current environment
./tools/baseline-capture.sh

# Compare old vs new environments
git diff bench/baselines/2025-07-01_v0.5.0.json \
         bench/baselines/2026-01-28_v0.7.0.json
```

### Issue: "Unstable benchmarks (variance >10%)"

**Cause**: System load, background processes

**Fix**:
```bash
# Close background applications
# Disable Spotlight indexing temporarily
sudo mdutil -a -i off

# Run multiple times to verify stability
for i in {1..5}; do
  ./tools/baseline-compare.sh --html-only
  sleep 120
done

# Re-enable Spotlight
sudo mdutil -a -i on
```

### Issue: "Different hardware"

**Cause**: Baselines are hardware-specific

**Fix**:
```bash
# Capture new baseline on new hardware
./tools/baseline-capture.sh

# Note hardware change in commit
git commit --amend -m "Baseline v0.7.0 (new hardware: M4 Pro)"
```

## Advanced Usage

### Custom Thresholds

```bash
# Stricter threshold (5% instead of 10%)
./tools/baseline-compare.sh --threshold 5

# More lenient (20% instead of 10%)
./tools/baseline-compare.sh --threshold 20
```

### Specific Baseline

```bash
# Compare to specific baseline (not latest)
./tools/baseline-compare.sh --baseline bench/baselines/2025-12-01_v0.6.0.json
```

### Non-Interactive Update

```bash
# Automated update (for scripts)
./tools/baseline-update.sh \
  --auto-yes \
  --reason "CI: Automated baseline update after optimization"
```

### HTML-Only Mode

```bash
# Generate report without re-running benchmarks
./tools/baseline-compare.sh --html-only
```

## Metrics Reference

### Throughput

- **Unit**: `msg/s` (messages per second)
- **Scope**: `per_node`
- **Regression**: `-10%` (e.g., 1M → 900K msg/s)
- **Target**: Higher is better

### Latency

- **Unit**: `µs` (microseconds)
- **Percentiles**: p50, p95, p99
- **Scope**: `per_request`
- **Regression**: `+10%` (e.g., 100µs → 110µs)
- **Target**: Lower is better

### Memory

- **Unit**: `MiB` (mebibytes)
- **Scope**: `per_node` or `per_connection`
- **Regression**: `+20%` (e.g., 50 MiB → 60 MiB)
- **Target**: Lower is better (with exceptions)

## See Also

- `bench/baselines/README.md` - Quick reference
- `docs/metrology/METRICS_GLOSSARY.md` - Metric definitions
- `bench/README.md` - Benchmark suite overview
- `.github/workflows/performance.yml` - CI integration
