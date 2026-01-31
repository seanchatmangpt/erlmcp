# ERLMCP Benchmark Scripts

Performance benchmarking and validation scripts for erlmcp.

## Overview

These scripts automate performance regression testing by comparing current benchmark results against established baselines. They enforce quality gates to prevent performance degradation.

## Scripts

### run_validation_benchmarks.sh

**Purpose:** Validates current performance against established baselines and enforces regression thresholds.

**Features:**
- Runs erlmcp_bench_core_ops:run(<<"core_ops_10k">>) by default
- Compares against baseline in bench/baselines/2026-01-28_v2.0.0.json
- Calculates regression for throughput, latency (p50/p95/p99), and memory
- Exits with status 1 if regression exceeds threshold (default 10%)
- Clear pass/fail report per metric with color-coded output

## Usage

### Basic Usage

```bash
# Run with default settings
./scripts/bench/run_validation_benchmarks.sh

# Run with verbose output
./scripts/bench/run_validation_benchmarks.sh --verbose

# Show help
./scripts/bench/run_validation_benchmarks.sh --help
```

### Advanced Usage

```bash
# Run specific workload
./scripts/bench/run_validation_benchmarks.sh --workload core_ops_100k

# Use custom baseline file
./scripts/bench/run_validation_benchmarks.sh --baseline bench/baselines/custom.json

# Set stricter regression threshold (5%)
./scripts/bench/run_validation_benchmarks.sh --threshold 5

# Combine options
./scripts/bench/run_validation_benchmarks.sh --workload core_ops_100k --threshold 5 --verbose
```

## Command-Line Options

| Option | Description | Default |
|--------|-------------|---------|
| -w, --workload ID | Workload ID to run | core_ops_10k |
| -b, --baseline FILE | Baseline JSON file | bench/baselines/2026-01-28_v2.0.0.json |
| -t, --threshold N | Maximum regression % | 10 |
| -v, --verbose | Enable verbose output | false |
| -h, --help | Show help message | - |

## Output Format

### Success Example

```
╔═══════════════════════════════════════════════════════════════╗
║         ERLMCP Performance Validation Benchmark              ║
╔═══════════════════════════════════════════════════════════════╝

Configuration:
  Workload: core_ops_10k
  Baseline: /Users/sac/erlmcp/bench/baselines/2026-01-28_v2.0.0.json
  Threshold: 10%

╔═══════════════════════════════════════════════════════════════╗
║              Performance Validation Results                   ║
╔═══════════════════════════════════════════════════════════════╝

Throughput (msg/s):
  Throughput:         | Current: 2741603 msg/s | Baseline: 2830000 msg/s | Delta: -3.12%

Latency P50 (us):
  Latency P50:        | Current: 0 us | Baseline: 0 us | Delta: 0.00%

Latency P95 (us):
  Latency P95:        | Current: 82 us | Baseline: 81 us | Delta: 1.23%

Latency P99 (us):
  Latency P99:        | Current: 98 us | Baseline: 97 us | Delta: 1.03%

Memory (MiB):
  Memory Delta:       | Current: 2 MiB | Baseline: 2 MiB | Delta: 0.00%

╔═══════════════════════════════════════════════════════════════╗
║         Overall Status: ALL TESTS PASSED                     ║
╔═══════════════════════════════════════════════════════════════╝

[SUCCESS] No performance regression detected
```

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | All tests passed (no regression) |
| 1 | Performance regression detected |
| 2 | Benchmark execution failed |
| 3 | Missing dependencies or configuration |

## Baseline Metrics

### v2.0.0 Baseline (2026-01-28)

| Metric | Value |
|--------|-------|
| Throughput | 2.83M msg/s |
| Latency P50 | 0 us |
| Latency P95 | 81 us |
| Latency P99 | 97 us |
| Memory Delta | 2.0 MiB |

## Creating a New Baseline

```bash
# Run benchmark and capture output
erl -noshell -eval "Result = erlmcp_bench_core_ops:run(<<\"core_ops_10k\">>), io:format(\"~s~n\", [jsx:encode(Result)]), halt(0)." -s init stop > /tmp/bench.json

# Create baseline file with proper structure
cat > bench/baselines/$(date +%Y-%m-%d)_v2.1.0.json << ENDJSON
{
  "baseline_name": "v2.1.0",
  "baseline_date": "$(date +%Y-%m-%d)",
  "workload_id": "core_ops_10k",
  "metrics": {
    "throughput_msg_per_s": $(jq -r '.throughput_msg_per_s' /tmp/bench.json),
    "latency_p50_us": $(jq -r '.latency_p50_us' /tmp/bench.json),
    "latency_p95_us": $(jq -r '.latency_p95_us' /tmp/bench.json),
    "latency_p99_us": $(jq -r '.latency_p99_us' /tmp/bench.json),
    "memory_delta_mib": $(jq -r '.memory_delta_mib' /tmp/bench.json)
  },
  "thresholds": {
    "max_regression_percent": 10
  },
  "environment": $(jq '.environment' /tmp/bench.json)
}
ENDJSON
```

## Integration with CI/CD

### GitHub Actions Example

```yaml
name: Performance Validation

on:
  pull_request:
    branches: [main]

jobs:
  performance:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Install Dependencies
        run: sudo apt-get install -y jq
      - name: Run Performance Validation
        run: ./scripts/bench/run_validation_benchmarks.sh --threshold 10
```

## Dependencies

Required tools:
- rebar3 - Build tool
- jq - JSON processor
- Erlang/OTP 25-28 - Runtime environment

## Performance Targets

| Metric | Target | Threshold |
|--------|--------|-----------|
| Throughput | >=2.5M msg/s | <10% regression |
| Latency P95 | <=100 us | <10% increase |
| Latency P99 | <=120 us | <10% increase |
| Memory Delta | <=3 MiB | <10% increase |

## Troubleshooting

### Benchmark fails to run
- Ensure code is compiled: TERM=dumb rebar3 compile
- Check benchmark module exists in bench/ directory
- Verify Erlang node can start

### jq not found
```bash
# macOS
brew install jq

# Linux
sudo apt-get install jq
```

### Baseline file not found
- Create baseline file (see "Creating a New Baseline" above)
- Verify path with --baseline flag
- Check file exists: ls -la bench/baselines/

## Related Documentation

- Benchmark Results: /Users/sac/erlmcp/bench/results/
- Metrology Glossary: /Users/sac/erlmcp/docs/metrology/METRICS_GLOSSARY.md
- Performance Guide: /Users/sac/erlmcp/docs/PERFORMANCE.md
