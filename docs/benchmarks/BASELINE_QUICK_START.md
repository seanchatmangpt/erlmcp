# Baseline Management - Quick Start Guide

## TL;DR

```bash
# Capture baseline (before release)
./tools/baseline-capture.sh

# Compare to baseline (development)
./tools/baseline-compare.sh

# Update baseline (after optimization)
./tools/baseline-update.sh --reason "Your justification"
```

## Common Scenarios

### Scenario 1: New Release

```bash
# Step 1: Ensure clean state
git checkout main && git pull

# Step 2: Capture baseline
./tools/baseline-capture.sh
# Output: bench/baselines/2026-01-28_v0.7.0.json (auto-committed)

# Step 3: Tag release
git tag v0.7.0 && git push --tags
```

### Scenario 2: Check Performance During Development

```bash
# Step 1: Make changes
vim src/erlmcp_registry.erl

# Step 2: Compile
rebar3 compile

# Step 3: Check against baseline
./tools/baseline-compare.sh

# Possible outcomes:

# A. ✓ PASS - No regressions
#    Continue with PR

# B. ❌ REGRESSION - Performance degraded
#    Fix code or justify tradeoff

# C. ✓ IMPROVEMENT - Performance improved
#    Document in commit message
```

### Scenario 3: Optimization Landed

```bash
# After merging optimization PR

# Step 1: Verify improvement
./tools/baseline-compare.sh
# Output: core_ops_100k: +15.2% throughput ✓

# Step 2: Update baseline
./tools/baseline-update.sh --reason "gproc migration improved registry by 15%"
# Output: New baseline committed to git
```

### Scenario 4: Handling CI Regression

```bash
# CI reports regression in PR

# Option A: Fix the regression
git revert HEAD
./tools/baseline-compare.sh  # Should pass

# Option B: Accept regression (rare, requires justification)
./tools/baseline-update.sh --reason "Tradeoff: -5% throughput for better safety"
```

## Tools at a Glance

| Tool | When | Output | Exit Code |
|------|------|--------|-----------|
| `baseline-capture.sh` | Before release, after optimization | JSON baseline, git commit | Always 0 |
| `baseline-compare.sh` | Development, CI/CD | HTML report, terminal output | 0=pass, 1=regression |
| `baseline-update.sh` | After intentional change | New baseline, git commit | Always 0 |

## Regression Thresholds

| Metric | Threshold | Example |
|--------|-----------|---------|
| **Throughput** | -10% | 2.69M → 2.42M msg/s = ❌ |
| **Latency p95** | +10% | 83µs → 91µs = ❌ |
| **Memory** | +20% | 19.1 MiB → 22.9 MiB = ❌ |

## File Structure

```
bench/baselines/
├── README.md                    # Overview and concepts
├── 2026-01-28_v0.7.0.json       # Baseline for v0.7.0
├── 2026-02-15_v0.8.0.json       # Baseline for v0.8.0
├── comparison_1769567400.html   # Latest comparison report
└── BASELINE_EXAMPLE.json        # Sample format

tools/
├── baseline-capture.sh          # Capture tool
├── baseline-compare.sh          # Compare tool
└── baseline-update.sh           # Update tool

docs/benchmarks/
├── BASELINE_MANAGEMENT.md       # Full documentation
└── BASELINE_QUICK_START.md      # This file

.github/workflows/
└── performance-regression.yml   # CI integration
```

## Baseline File Format

```json
{
  "version": "0.7.0",
  "timestamp": 1769567361,
  "environment": { "os": "darwin", "otp_version": "27", ... },
  "benchmarks": {
    "core_ops_100k": {
      "throughput_msg_per_s": 2690088.37,
      "latency_p95_us": 83.0,
      "memory_delta_mib": 19.1
    },
    ...
  }
}
```

## CI/CD Integration

The system automatically runs on PRs:

1. **PR opened** → `baseline-compare.sh` runs
2. **Regression detected** → PR comment added, merge blocked
3. **No regression** → PR can merge
4. **Main updated** → Baseline captured (if benchmark files changed)

## Best Practices

1. **Capture baselines on stable hardware** - Same machine, minimal load
2. **Run multiple times** - Verify stability (variance <5%)
3. **Document changes** - Always explain baseline updates
4. **Review history** - Track baseline evolution with `git log -- bench/baselines/`
5. **Version alignment** - Tag baselines with release versions

## Troubleshooting

| Problem | Solution |
|---------|----------|
| No baseline found | Run `./tools/baseline-capture.sh` |
| Regression but expected | Run `./tools/baseline-update.sh --reason "..."` |
| Unstable benchmarks | Close background apps, run multiple times |
| Different hardware | Capture new baseline on new machine |

## Examples

### Check Performance

```bash
$ ./tools/baseline-compare.sh

========================================
erlmcp Performance Comparison
========================================
Baseline: 2026-01-28_v0.7.0.json

core_ops_100k:
  Throughput: 2.69M → 2.75M msg/s (+2.1%) ✓
  Latency p95: 83µs → 79µs (-4.8%) ✓
  Memory: 19.1 → 19.8 MiB (+3.7%) ✓

✓ PASS: No regressions detected
```

### Update Baseline

```bash
$ ./tools/baseline-update.sh

Old baseline: 2026-01-28_v0.7.0.json
New baseline: 2026-02-15_v0.8.0.json

Benchmark                 Old         New         Change
core_ops_100k (msg/s)     2690088     3100000     +15.2%

Reason: gproc migration improved registry performance

✓ Baseline update complete
✓ Git commit: b4e7d9a
```

## See Also

- `bench/baselines/README.md` - Concepts and workflow
- `docs/benchmarks/BASELINE_MANAGEMENT.md` - Full documentation
- `.github/workflows/performance-regression.yml` - CI workflow
