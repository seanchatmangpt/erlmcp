# ERLMCP Bench and Chaos Commands

**Version**: v1.4.0
**Purpose**: One-command reproducible benchmarking and chaos testing
**Status**: Stable - Ready for production use

## Overview

The `erlmcp bench` and `erlmcp chaos` commands provide reproducible performance and resilience testing with:

- **Bench**: 4+ suites (latency, throughput, registry, 100k)
- **Chaos**: 3 scenarios (loss, latency, slow) with 3 intensity levels
- **Output Formats**: Text, JSON, CSV for integration
- **Deterministic Results**: Seed control for reproducibility
- **No Manual Setup**: One-command execution

## Quick Start

### Benchmarking

```bash
# Run latency benchmark
erlmcp bench run --suite latency

# Run 100K concurrent benchmark
erlmcp bench run --suite 100k --duration 60

# Export to JSON
erlmcp bench run --suite throughput --output json > bench.json

# Export to CSV
erlmcp bench run --suite registry --scale 100K --output csv > bench.csv
```

### Chaos Testing

```bash
# Run packet loss chaos
erlmcp chaos run --scenario loss --intensity high

# Run latency injection with 2-minute duration
erlmcp chaos run --scenario latency --duration 120 --output json

# Reproducible chaos test (with seed)
erlmcp chaos run --scenario all --seed 12345 --output csv > chaos.csv
```

## Benchmark Suites

### Latency Suite
Measures request latency with percentile distributions.

```bash
erlmcp bench run --suite latency --duration 60
```

**Output**:
```
========================================
BENCHMARK RESULTS: latency
========================================
Duration:           60s
Total Operations:   1000
Throughput:         16.67 ops/sec
Latency (avg):      5000.00 µs
Latency (p50):      4500.00 µs
Latency (p95):      7500.00 µs
Latency (p99):      9000.00 µs
Memory Delta:       512 KB
Timestamp:          1234567890
========================================
```

### Throughput Suite
Measures raw operations per second at various scales.

```bash
erlmcp bench run --suite throughput --scale 100K --duration 60
```

**Supported Scales**:
- `10K`: 10,000 operations
- `100K`: 100,000 operations (default)
- `1M`: 1,000,000 operations

### Registry Suite
Measures registry/lookup operation performance.

```bash
erlmcp bench run --suite registry --scale 100K
```

### 100K Concurrent Suite
Simulates 100,000 concurrent connections.

```bash
erlmcp bench run --suite 100k --duration 30
```

**Note**: This is a stress test - use cautiously in production.

### Combined Suite
Runs all suites sequentially.

```bash
erlmcp bench run --suite combined --duration 30 --output json
```

## Chaos Scenarios

### Packet Loss Scenario
Simulates network packet loss with configurable rate.

```bash
# Low intensity (1% loss)
erlmcp chaos run --scenario loss --intensity low

# High intensity (20% loss)
erlmcp chaos run --scenario loss --intensity high
```

**Rates by Intensity**:
- `low`: 1% loss
- `medium`: 5% loss
- `high`: 20% loss

### Latency Injection Scenario
Injects random latency into operations.

```bash
# Medium intensity (50ms random delays)
erlmcp chaos run --scenario latency --intensity medium
```

**Delays by Intensity**:
- `low`: 10ms
- `medium`: 50ms
- `high`: 200ms

### Slow Node Degradation Scenario
Simulates gradual node performance degradation.

```bash
# High intensity degradation
erlmcp chaos run --scenario slow --intensity high --duration 120
```

### All Scenarios
Runs all three scenarios sequentially.

```bash
erlmcp chaos run --scenario all --intensity medium --output json > full-chaos.json
```

## Output Formats

### Text (Default)
Human-readable output on console.

```bash
erlmcp bench run --suite latency
erlmcp chaos run --scenario loss --output text
```

### JSON
Machine-readable JSON output.

```bash
erlmcp bench run --suite throughput --output json
erlmcp chaos run --scenario latency --output json
```

**Bench JSON Structure**:
```json
{
  "suite": "throughput",
  "duration_sec": 60,
  "operations": 100000,
  "throughput": 1666.67,
  "latency_avg_us": 0.00,
  "latency_p50_us": 0.00,
  "latency_p95_us": 0.00,
  "latency_p99_us": 0.00,
  "memory_delta_kb": 1024,
  "timestamp": 1234567890
}
```

**Chaos JSON Structure**:
```json
{
  "scenario": "loss",
  "duration_sec": 60,
  "intensity": "high",
  "chaos_events": 100,
  "operations": 5000,
  "failures": 100,
  "recovery_time_ms": 2500,
  "resilience_score_percent": 98.00,
  "timestamp": 1234567890
}
```

### CSV
Comma-separated values for spreadsheet import.

```bash
erlmcp bench run --suite registry --output csv > registry.csv
erlmcp chaos run --scenario all --output csv > chaos.csv
```

**Bench CSV Format**:
```
suite,duration_sec,operations,throughput,latency_avg_us,...
registry,60,100000,1666.67,0.00,...
```

**Chaos CSV Format**:
```
scenario,duration_sec,intensity,chaos_events,operations,failures,...
loss,60,high,100,5000,100,...
```

## Command Options

### Common Options

```bash
--verbose              # Show detailed progress
--output <format>      # json | csv | text (default: text)
```

### Bench Options

```bash
--suite <name>         # latency | throughput | registry | 100k | combined
--duration <seconds>   # Test duration in seconds (default: 60)
--scale <size>         # 10K | 100K | 1M (default: 100K)
```

### Chaos Options

```bash
--scenario <name>      # loss | latency | slow | all
--duration <seconds>   # Test duration in seconds (default: 60)
--intensity <level>    # low | medium | high (default: medium)
--seed <number>        # Random seed for reproducibility
```

## Examples

### Example 1: Performance Regression Detection

```bash
# Baseline run
erlmcp bench run --suite latency --duration 60 --output json > baseline.json

# Current run
erlmcp bench run --suite latency --duration 60 --output json > current.json

# Compare JSON files (manually or via script)
```

### Example 2: Reproducible Chaos Test

```bash
# Run 1 with seed 12345
erlmcp chaos run --scenario all --seed 12345 --output json > chaos-run1.json

# Run 2 with same seed
erlmcp chaos run --scenario all --seed 12345 --output json > chaos-run2.json

# Results should be identical structure, validating determinism
```

### Example 3: Continuous Monitoring

```bash
#!/bin/bash

# Run benchmarks every hour
while true; do
  erlmcp bench run --suite combined --output json >> bench-history.jsonl
  sleep 3600
done
```

### Example 4: Stress Testing with Different Scales

```bash
# Test at 10K scale
erlmcp bench run --suite 100k --scale 10K --duration 30

# Test at 100K scale
erlmcp bench run --suite 100k --scale 100K --duration 60

# Test at 1M scale
erlmcp bench run --suite 100k --scale 1M --duration 120
```

### Example 5: Chaos Progression

```bash
# Start with low intensity
erlmcp chaos run --scenario loss --intensity low

# Increase intensity
erlmcp chaos run --scenario loss --intensity medium

# Full intensity test
erlmcp chaos run --scenario loss --intensity high
```

## Metrics Explained

### Benchmark Metrics

- **Duration**: Total test duration in seconds
- **Operations**: Total number of operations completed
- **Throughput**: Operations per second (ops/sec)
- **Latency (avg)**: Average latency in microseconds
- **Latency (p50)**: Median latency
- **Latency (p95)**: 95th percentile latency (5% slower than this)
- **Latency (p99)**: 99th percentile latency (1% slower than this)
- **Memory Delta**: Memory allocations during test in KB

### Chaos Metrics

- **Chaos Events**: Number of failures/anomalies injected
- **Operations**: Total operations attempted
- **Failures**: Operations that failed due to chaos
- **Resilience Score**: Percentage of successful operations (0-100%)
- **Recovery Time**: Time to stabilize after chaos event (ms)

## Best Practices

### 1. Establish Baseline
```bash
# Run 5 times and average for consistent baseline
for i in {1..5}; do
  erlmcp bench run --suite latency --output json >> latency-baseline.jsonl
done
```

### 2. Use Seeds for Reproducibility
```bash
# Store seed in results
erlmcp chaos run --scenario all --seed 42 --output json > chaos-seed-42.json
```

### 3. Monitor Memory
```bash
# Check memory delta for memory leaks
erlmcp bench run --suite 100k --duration 300 --output json | grep memory_delta_kb
```

### 4. Verify Determinism
```bash
# Run same benchmark 5 times
erlmcp bench run --suite latency > run1.json
erlmcp bench run --suite latency > run2.json
erlmcp bench run --suite latency > run3.json
# All should have similar values (within margin of timing variance)
```

### 5. Load Test Before Production
```bash
# Run chaos tests before deploying
erlmcp chaos run --scenario all --intensity high --duration 300
```

## Troubleshooting

### Benchmark Too Slow
- Reduce `--duration`
- Lower `--scale` from 100K to 10K
- Check system resource availability

### High Memory Delta
- Indicates memory allocation during test
- Monitor with multiple runs to detect leaks
- Check GC logs with `make observer`

### Chaos Test Timing Out
- Check system load with `top`
- Reduce `--intensity` level
- Increase `--duration` to allow recovery

### Results Vary Between Runs
- Specify `--seed` for reproducibility
- Check system load (background processes)
- Use `--verbose` to see detailed progress

## Integration with CI/CD

### GitHub Actions Example

```yaml
name: Performance Tests

on: [push]

jobs:
  bench:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Run benchmarks
        run: |
          erlmcp bench run --suite latency --output json > bench.json
      - name: Upload results
        uses: actions/upload-artifact@v2
        with:
          name: bench-results
          path: bench.json
```

### Jenkins Example

```groovy
pipeline {
    stages {
        stage('Benchmark') {
            steps {
                sh 'erlmcp bench run --suite combined --output json > bench.json'
                archiveArtifacts artifacts: 'bench.json'
            }
        }
        stage('Chaos') {
            steps {
                sh 'erlmcp chaos run --scenario all --output json > chaos.json'
                archiveArtifacts artifacts: 'chaos.json'
            }
        }
    }
}
```

## Known Limitations

1. **Single Node Only**: Benchmarks run on local node only
2. **Simulated Load**: Chaos scenarios simulate failures, not real network conditions
3. **Memory Measurements**: Memory delta reflects GC bucket, not exact allocations
4. **Latency Precision**: Microsecond precision, subject to system timer precision
5. **No Distributed Mode**: Cannot benchmark across multiple nodes
6. **Reproducibility**: Results vary by system load; use seed for consistent patterns

## Performance Expectations

### Typical Results (on modern hardware)

**Latency Benchmark**:
- Avg latency: 1-10ms
- p99 latency: 10-50ms
- Throughput: 100-1000 ops/sec

**Throughput Benchmark**:
- 10K: <1 second
- 100K: 1-3 seconds
- 1M: 10-30 seconds

**100K Concurrent**:
- Spawn time: 5-15 seconds
- Throughput: 10K-50K ops/sec

**Chaos Test**:
- Recovery time: 100ms-5s (depends on intensity)
- Resilience: >95% under chaos

## See Also

- `docs/architecture.md` - System design
- `docs/otp-patterns.md` - OTP patterns used
- `rebar.config` - Build configuration
- Examples: `examples/simple/`, `examples/calculator/`

## Support

For issues or questions:
1. Check `make observer` for live monitoring
2. Run with `--verbose` for detailed logs
3. Report in project issues with output from `erlmcp bench run`
