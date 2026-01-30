# Performance Regression Framework Guide

## Overview

The `erlmcp_performance_regression_SUITE` provides automated performance regression detection for the erlmcp project. It integrates with existing benchmarks to detect performance degradation before it reaches production.

## Features

### 1. Baseline Tracking
- Load baseline metrics from JSON files
- Save baseline metrics for version control
- Baseline version management
- Support for multiple baseline files

### 2. Regression Detection
- **Throughput**: Alert if >10% degradation
- **Latency (P95, P99)**: Alert if >10% increase
- **Memory**: Alert if >20% increase
- Configurable thresholds per metric

### 3. Historical Comparison
- Compare to previous N runs
- Trend analysis (improving/stable/degrading)
- Performance history tracking
- Average calculation across runs

### 4. Performance Gates
- Pass/fail based on thresholds
- Suitable for CI/CD pipelines
- Detailed failure reporting
- Exit codes for automation

### 5. CI/CD Integration
- JSON report generation
- Human-readable text reports
- Exit codes for automation
- GitHub Actions compatible

## Quick Start

### Run All Tests
```bash
rebar3 ct --suite=erlmcp_performance_regression_SUITE
```

### Run Specific Test Group
```bash
rebar3 ct --suite=erlmcp_performance_regression_SUITE --group=core_ops
rebar3 ct --suite=erlmcp_performance_regression_SUITE --group=ci
```

### Run with Custom Baseline
```bash
rebar3 ct --suite=erlmcp_performance_regression_SUITE \
  --var=baseline_path=/path/to/baseline.json
```

## Metrics Tracked

All metrics are metrology-compliant with canonical units:

| Metric | Unit | Scope |
|--------|------|-------|
| throughput_msg_per_s | msg/s | per_node |
| latency_p50_us | microseconds | per_operation |
| latency_p95_us | microseconds | per_operation |
| latency_p99_us | microseconds | per_operation |
| memory_delta_mib | MiB | per_run |

## Baseline File Format

```json
{
  "version": "2.0.0",
  "timestamp": 1769631041,
  "date": "2026-01-28T12:10:41Z",
  "git_sha": "abc123",
  "environment": {
    "os": "darwin",
    "os_version": "25.2.0",
    "arch": "x86_64",
    "hostname": "build-server",
    "otp_version": "OTP-27",
    "erts_version": "15.2.7.1",
    "cores": 8
  },
  "benchmarks": {
    "core_ops_100k": {
      "throughput_msg_per_s": 2748290.91,
      "latency_p50_us": 0.0,
      "latency_p95_us": 83.0,
      "latency_p99_us": 98.0,
      "memory_delta_mib": 21.9
    }
  },
  "regression_thresholds": {
    "throughput": -10,
    "latency_p95": 10,
    "memory": 20
  }
}
```

## Test Cases

### test_baseline_loading
Tests loading baseline from JSON file and verifying structure.

### test_baseline_saving
Tests saving baseline to JSON file with validation.

### test_regression_detection_throughput
Tests throughput regression detection with various scenarios:
- Within threshold (5% degradation) → PASS
- Exceeds threshold (15% degradation) → FAIL
- Improvement (15% improvement) → PASS

### test_regression_detection_latency
Tests latency regression detection:
- P95 within threshold → PASS
- P95 exceeds threshold → FAIL
- P99 exceeds threshold → FAIL

### test_regression_detection_memory
Tests memory regression detection:
- Within 20% threshold → PASS
- Exceeds 20% threshold → FAIL

### test_historical_comparison
Tests comparison to previous runs with trend analysis.

### test_trend_analysis
Tests trend detection across multiple data points.

### test_performance_gates
Tests pass/fail evaluation based on all metrics.

### test_ci_report_generation
Tests CI/CD report generation in text format.

### test_full_regression_suite
End-to-end test running actual benchmarks and comparing to baseline.

## Benchmark Integration

The SUITE integrates with existing benchmark modules:

- `erlmcp_bench_core_ops` - Core operations (registry, queue, pool, session)
- `erlmcp_bench_network_real` - Network benchmarks (TCP, HTTP)
- `erlmcp_bench_stress` - Sustained load benchmarks

### Running Benchmarks Standalone

```bash
# Core operations
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).

# Network benchmarks
erlmcp_bench_network_real:run(<<"tcp_quick_1k">>).

# Stress benchmarks
erlmcp_bench_stress:run(<<"stress_30s_100k_ops">>).
```

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Performance Regression Tests

on: [push, pull_request]

jobs:
  performance:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: erlang-solutions/elixir-otp-rebar3-actions@v1
      - name: Run performance tests
        run: rebar3 ct --suite=erlmcp_performance_regression_SUITE
```

### Exit Codes

- `0` - All tests passed, no regressions
- `1` - Tests failed or regressions detected

## Updating Baselines

When performance legitimately changes (e.g., optimization):

1. Run benchmarks to generate new results
2. Create new baseline file
3. Update `baseline_path` in SUITE config
4. Commit to version control

```bash
# Generate new baseline
rebar3 ct --suite=erlmcp_performance_regression_SUITE \
  --var=baseline_path=./bench/baselines/new_baseline.json

# Update config
cp new_baseline.json ./bench/baselines/2026-01-30_v2.1.0.json
```

## Thresholds

Default thresholds (configurable in baseline file):

| Metric | Threshold | Direction |
|--------|-----------|-----------|
| Throughput | -10% | Lower is worse |
| Latency P95 | +10% | Higher is worse |
| Memory | +20% | Higher is worse |

## Troubleshooting

### Tests Skip with "Benchmark module not available"
- Ensure benchmark modules are compiled
- Check that `bench/` directory is in code path

### Baseline File Not Found
- Provide correct path via `--var=baseline_path=/path/to/baseline.json`
- Ensure baseline file exists in `bench/baselines/`

### False Positives
- Increase thresholds in baseline file
- Consider test environment variability
- Run multiple iterations for stability

## Best Practices

1. **Run on every commit**: Catch regressions early
2. **Use stable hardware**: Performance varies by hardware
3. **Control environment**: Disable background processes
4. **Version baselines**: Track performance over releases
5. **Investigate regressions**: Don't just accept degradation
6. **Document improvements**: Keep notes on optimizations

## Files

- `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_performance_regression_SUITE.erl` - Main SUITE
- `/Users/sac/erlmcp/bench/baselines/` - Baseline storage
- `/Users/sac/erlmcp/bench/erlmcp_bench_core_ops.erl` - Core benchmarks
- `/Users/sac/erlmcp/bench/erlmcp_bench_network_real.erl` - Network benchmarks
- `/Users/sac/erlmcp/bench/erlmcp_bench_stress.erl` - Stress benchmarks

## References

- [Benchmark Documentation](/Users/sac/erlmcp/bench/README.md)
- [Metrology Compliance](/Users/sac/erlmcp/docs/metrology/METRICS_GLOSSARY.md)
- [Performance Baselines](/Users/sac/erlmcp/bench/baselines/README.md)
