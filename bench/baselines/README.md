# Performance Baselines - erlmcp

## Overview

Performance baselines are **stable reference measurements** used to detect regressions in erlmcp's performance characteristics. Each baseline captures:

- **Throughput**: Operations per second (msg/s)
- **Latency**: Percentiles (p50/p95/p99) in microseconds
- **Memory**: Heap usage (MiB) per connection/node
- **Workload metadata**: Environment, OTP version, architecture

## Purpose

Baselines enable:

1. **Regression detection**: Automatically flag performance degradation >10%
2. **Trend analysis**: Track performance over releases
3. **CI/CD gates**: Block merges that introduce regressions
4. **Capacity planning**: Understand baseline capacity for deployment

## Baseline Structure

Each baseline is a JSON file: `YYYY-MM-DD_vX.Y.Z.json`

```json
{
  "version": "0.7.0",
  "timestamp": 1769567361,
  "environment": {
    "os": "darwin",
    "otp_version": "27",
    "erts_version": "15.2.7.1",
    "hostname": "...",
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
    "tcp_sustained_10k": { ... },
    "stress_30s_100k_ops": { ... }
  }
}
```

## Workflow

### 1. Capture Baseline (After Major Changes)

```bash
# Run all benchmarks and save baseline
./tools/baseline-capture.sh

# Output: bench/baselines/2026-01-28_v0.7.0.json
# Automatically committed to git
```

**When to capture:**
- Before releases (tag baseline with version)
- After major optimizations
- After infrastructure changes (OTP upgrade, hardware)
- When establishing new benchmark

### 2. Compare Against Baseline (Development)

```bash
# Run benchmarks and compare to latest baseline
./tools/baseline-compare.sh

# Output: bench/baselines/comparison_TIMESTAMP.html
# Exits 1 if regression detected (>10% slower, >20% more memory)
```

**Regression thresholds:**
- Throughput: -10% (e.g., 2.69M → 2.42M msg/s)
- Latency p95: +10% (e.g., 83µs → 91µs)
- Memory: +20% (e.g., 19.1 MiB → 22.9 MiB)

### 3. Update Baseline (Intentional Changes)

```bash
# Interactive update with justification
./tools/baseline-update.sh

# Prompts:
# - Show diff (old vs new)
# - Require justification (release notes)
# - Create git commit with explanation
```

**When to update:**
- Optimization improved performance (document gains)
- Intentional tradeoff (e.g., more memory for speed)
- Infrastructure change (OTP upgrade)
- New workload added

## CI/CD Integration

Baselines are automatically checked in CI:

```bash
# .github/workflows/ci.yml
- name: Performance Regression Check
  run: |
    git fetch origin main:main
    ./tools/baseline-compare.sh
    # Exits 1 if regression detected → blocks merge
```

## File Naming Convention

- **Format**: `YYYY-MM-DD_vX.Y.Z.json`
- **Examples**:
  - `2026-01-28_v0.7.0.json` - Release 0.7.0 baseline
  - `2026-02-15_v0.8.0.json` - Release 0.8.0 baseline

**Latest baseline** = most recent file (sorted by date)

## Baseline History

Track baselines in git:

```bash
# View baseline history
git log -- bench/baselines/

# Compare two baselines
git diff bench/baselines/2026-01-28_v0.7.0.json \
         bench/baselines/2026-02-15_v0.8.0.json
```

## Metrology Compliance

All baselines conform to erlmcp metrology standards:

- **Units**: `msg/s`, `us` (microseconds), `MiB`
- **Scope**: `per_node`, `per_connection`
- **Precision**: `microsecond` (raw µs values)
- **Required fields**: workload_id, transport, duration_s

See `docs/metrology/METRICS_GLOSSARY.md` for details.

## Best Practices

1. **Stable environment**: Capture baselines on consistent hardware
2. **Minimal load**: Close other applications during capture
3. **Multiple runs**: Verify stability across 3+ runs
4. **Document changes**: Always explain baseline updates
5. **Version alignment**: Tag baselines with release versions

## Troubleshooting

### "No baseline found"

```bash
# Capture initial baseline
./tools/baseline-capture.sh
```

### "Regression detected but expected"

```bash
# Update baseline with justification
./tools/baseline-update.sh
# Enter reason: "OTP 28 upgrade - expected memory increase"
```

### "Baseline too old (>6 months)"

```bash
# Re-capture baseline on current environment
./tools/baseline-capture.sh
```

## Tools Reference

| Tool | Purpose |
|------|---------|
| `baseline-capture.sh` | Run all benchmarks, save baseline, commit to git |
| `baseline-compare.sh` | Compare current run to latest baseline, flag regressions |
| `baseline-update.sh` | Update baseline with justification, create git commit |

## Example Output

### Capture
```
✓ Running core_ops benchmarks...
✓ Running network_real benchmarks...
✓ Running stress benchmarks...
✓ Baseline saved: bench/baselines/2026-01-28_v0.7.0.json
✓ Git commit: 3a8f2c1 "Performance baseline for v0.7.0"
```

### Compare
```
Performance Comparison (vs 2026-01-28_v0.7.0)

core_ops_100k:
  Throughput: 2.69M → 2.45M msg/s (-8.9%) ✓
  Latency p95: 83µs → 91µs (+9.6%) ✓
  Memory: 19.1 → 20.3 MiB (+6.3%) ✓

tcp_sustained_10k:
  Throughput: 372K → 311K msg/s (-16.4%) ❌ REGRESSION
  
✗ 1 regression detected. See: bench/baselines/comparison_1769567400.html
```

### Update
```
Baseline Update

Old: 2026-01-28_v0.7.0
New: 2026-02-15_v0.8.0

Changes:
  core_ops_100k: +15.2% throughput (optimization)
  tcp_sustained_10k: -5.1% throughput (acceptable)

Justification: "gproc migration improved registry performance"
✓ Baseline updated
✓ Git commit: b4e7d9a "Update baseline: gproc migration improvements"
```

## See Also

- `docs/benchmarks/BASELINE_MANAGEMENT.md` - Detailed management guide
- `docs/metrology/METRICS_GLOSSARY.md` - Metric definitions
- `bench/README.md` - Benchmark suite overview
