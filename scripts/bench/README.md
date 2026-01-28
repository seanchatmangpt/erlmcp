# Benchmark Automation Scripts

Automated benchmark runner with metrology validation, regression detection, and CI/CD integration.

## Quick Start

### Run Complete Benchmark Suite

```bash
# Quick mode (5 minutes - 1 workload per category)
./scripts/bench/run_all_benchmarks.sh quick

# Standard mode (30 minutes - 3 workloads per category)
./scripts/bench/run_all_benchmarks.sh standard

# Full mode (2 hours - all workloads)
./scripts/bench/run_all_benchmarks.sh full

# CI mode (quick + strict validation)
./scripts/bench/run_all_benchmarks.sh ci
```

### Quick Development Testing

```bash
# Minimal smoke test (~2 minutes)
./scripts/bench/quick_bench.sh
```

## Scripts Overview

### `run_all_benchmarks.sh` - Main Benchmark Runner

**Purpose**: Executes all benchmark categories with metrology validation and reporting.

**Features**:
- ✅ Runs 5 benchmark categories: core, integration, network, stress, chaos
- ✅ Validates metrology compliance on all results
- ✅ Generates JSON + human-readable summary reports
- ✅ Checks for regressions against baseline
- ✅ CI/CD ready with appropriate exit codes

**Execution Modes**:

| Mode | Duration | Workloads | Use Case |
|------|----------|-----------|----------|
| `quick` | ~5 min | 1 per category | Local development, smoke testing |
| `standard` | ~30 min | 3 per category | Pre-release validation |
| `full` | ~2 hours | All workloads | Release candidate testing |
| `ci` | ~5 min | Quick + strict validation | Automated CI/CD pipeline |

**Exit Codes**:
- `0` - All benchmarks passed
- `1` - Compilation failed
- `2` - Benchmark execution failed
- `3` - Metrology violations detected
- `4` - Regression detected (>10%)

**Output Structure**:
```
bench/results/<timestamp>/
├── core_registry_contention_10k.json
├── core_registry_contention_100k.json
├── integration_tool_sequence.json
├── network_tcp_burst_100.json
├── network_tcp_sustained_25k.json
├── stress_sustained_30s.json
├── chaos_memory_exhaustion.json
├── summary.json
├── summary.txt
├── execution.log
└── regression_report.txt (if baseline exists)
```

### `set_baseline.sh` - Set Regression Baseline

**Purpose**: Designates a set of benchmark results as the baseline for regression detection.

**Usage**:
```bash
./scripts/bench/set_baseline.sh bench/results/20260127_183000
```

**Validation**:
- ✅ Verifies metrology compliance on all results
- ✅ Checks for failed benchmarks
- ✅ Backs up existing baseline before overwriting

**Output**: Copies results to `bench/results/baseline/`

### `compare_to_baseline.sh` - Regression Detection

**Purpose**: Compares current benchmark results against baseline to detect performance regressions.

**Usage**:
```bash
./scripts/bench/compare_to_baseline.sh bench/results/20260127_190000 [threshold]
```

**Arguments**:
- `results_dir` - Directory with current results
- `threshold` - Max allowed regression % (default: 10)

**Metrics Compared**:
- **Throughput** (msg/s) - Higher is better
- **Latency P99** (µs) - Lower is better
- **Memory** (MiB/node) - Lower is better

**Exit Codes**:
- `0` - No regressions detected
- `1` - Invalid input or baseline not found
- `2` - Regressions detected above threshold

**Example Output**:
```
Benchmark               Metric          Baseline      Current       Change
--------------------------------------------------------------------------------
tcp_sustained_25k       throughput          900.00       850.00       -5.6%
tcp_sustained_25k       latency_p99        8000.00      8500.00       +6.3%
tcp_sustained_25k       memory             1536.00      1600.00       +4.2%
```

### `quick_bench.sh` - Development Smoke Test

**Purpose**: Minimal benchmark run for rapid iteration during development.

**Features**:
- ⚡ Runs in ~2 minutes
- ⚡ 1 workload per category (core + integration only)
- ⚡ Relaxed metrology validation (warnings only)
- ⚡ Higher regression threshold (20%)

**Usage**:
```bash
./scripts/bench/quick_bench.sh
```

Perfect for pre-commit testing and local verification.

## Benchmark Categories

### 1. Core Operations (`core`)

**Module**: `erlmcp_registry_contention`

**Workloads**:
- `registry_contention_10k` - 10K connections
- `registry_contention_25k` - 25K connections
- `registry_contention_50k` - 50K connections
- `registry_contention_100k` - 100K connections (full capacity)

**Duration**: ~5 minutes (100K workload)

**Metrics**:
- Registration latency (p50/p95/p99)
- Lookup latency (p50/p95/p99)
- Routing latency (p50/p95/p99)
- Ops/sec, lock contention ratio, memory usage

### 2. Integration Tests (`integration`)

**Module**: `erlmcp_bench_integration`

**Workloads**:
- `tool_sequence` - Tool call sequence validation
- `resource_subscribe` - Resource subscription flow
- `prompt_template` - Prompt template rendering
- `notification_flow` - Notification handling

**Duration**: ~10 minutes total

**Metrics**:
- End-to-end latency
- Message ordering
- Protocol compliance
- Resource cleanup

### 3. Network Real (`network`)

**Modules**: `tcp_real_bench`, `http_real_bench`

**Workloads**:
- `tcp_burst_100` - 100 connections, burst load
- `tcp_sustained_25k` - 25K connections, sustained (Team tier)
- `tcp_sustained_100k` - 100K connections, sustained (Enterprise tier)
- `http_sse_1k` - 1K SSE connections
- `http_sse_50k` - 50K SSE connections

**Duration**: ~30 minutes (100K workload can take 30+ minutes)

**Metrics**:
- Real socket throughput (msg/s)
- Network latency (µs)
- Bandwidth (MiB/s)
- Connection overhead
- Resource usage per connection

### 4. Stress Tests (`stress`)

**Module**: `erlmcp_bench_stress`

**Workloads**:
- `sustained_30s` - 30 second sustained load
- `sustained_300s` - 5 minute sustained load
- `high_conn_100k` - 100K concurrent connections
- `memory_pressure` - Memory pressure testing

**Duration**: ~10 minutes (300s workload takes 5+ minutes)

**Metrics**:
- Stability under sustained load
- Memory growth
- CPU utilization
- Process count stability

### 5. Chaos Engineering (`chaos`)

**Module**: `erlmcp_bench_chaos`

**Workloads**:
- `memory_exhaustion` - Memory exhaustion scenario
- `process_kill` - Random process termination
- `network_partition` - Network partition simulation
- `message_flood` - Message queue flooding
- `clock_drift` - Clock drift simulation

**Duration**: ~15 minutes total

**Metrics**:
- Recovery time
- Message loss during failure
- State consistency after recovery
- Supervisor restart effectiveness

## Metrology Validation

All benchmark results are validated using `erlmcp_metrology_validator`.

**Requirements**:
- ✅ Unit strings present and canonical
- ✅ Scope specified for composite metrics (e.g., "MiB/conn")
- ✅ Precision fields for time metrics (µs precision)
- ✅ Required fields: `workload_id`, `transport`, `duration_seconds`
- ✅ No "0.00 ms" without raw microsecond values

**Example Valid Metric**:
```json
{
  "latency_p99": {
    "value": 8.5,
    "unit": "ms",
    "precision_us": 8500
  },
  "memory_per_conn": {
    "value": 0.048,
    "unit": "MiB/conn"
  }
}
```

**Validation Modes**:
- **Strict** (CI mode): Fail on any violation
- **Lenient** (dev mode): Warn on violations, continue

## CI/CD Integration

### GitHub Actions Workflow

**File**: `.github/workflows/benchmarks.yml`

**Triggers**:
- ✅ Push to `main` branch
- ✅ Release tags (`v*`)
- ✅ Pull requests (quick mode)
- ✅ Manual dispatch

**Jobs**:
1. **Benchmark execution** - Runs benchmark suite
2. **Metrology validation** - Validates all results
3. **Regression detection** - Compares to baseline
4. **PR comment** - Posts results to PR
5. **Baseline update** - Updates baseline on main branch merge

**Artifacts**:
- Benchmark results (90 day retention)
- Baseline results (365 day retention)

**Exit Behavior**:
- PR: Comment with results, don't block merge on regression
- Main branch: Block on metrology violations or regressions
- Release: Run standard mode, upload artifacts

### Manual GitHub Actions Run

```bash
# Via gh CLI
gh workflow run benchmarks.yml -f mode=standard -f upload_artifacts=true

# Via web UI
# Navigate to Actions -> Benchmark Suite -> Run workflow
```

## Regression Detection

### Setting a New Baseline

After validating a clean benchmark run:

```bash
./scripts/bench/set_baseline.sh bench/results/20260127_183000
```

This:
1. Validates all results with metrology
2. Backs up existing baseline (if present)
3. Copies results to `bench/results/baseline/`
4. Creates metadata file

### Comparing to Baseline

```bash
./scripts/bench/compare_to_baseline.sh bench/results/20260127_190000 10
```

**Regression Criteria**:
- Throughput decreased by >10%
- Latency P99 increased by >10%
- Memory usage increased by >10%

**Output**:
- Green: No regression or improvement
- Yellow: Minor regression (5-10%)
- Red: Regression above threshold (>10%)

## Environment Variables

### `run_all_benchmarks.sh`

| Variable | Description | Default |
|----------|-------------|---------|
| `BENCHMARK_MODE` | Execution mode | `ci` |
| `BASELINE_DIR` | Baseline results location | `bench/results/baseline` |
| `METROLOGY_STRICT` | Fail on metrology violations | `true` |
| `REGRESSION_THRESHOLD` | Max allowed regression % | `10` |

### `quick_bench.sh`

Uses relaxed settings for development:
- `METROLOGY_STRICT=false`
- `REGRESSION_THRESHOLD=20`

## Troubleshooting

### Compilation Fails

```bash
# Clean build
rebar3 clean
rebar3 compile
```

### Benchmark Hangs

- Check system load (`top`, `htop`)
- Verify Erlang VM isn't overcommitted
- Reduce scale (e.g., run 10K instead of 100K)

### Metrology Validation Fails

Check error details in execution log:
```bash
cat bench/results/<timestamp>/execution.log | grep "Metrology"
```

Common issues:
- Missing `unit` field
- Non-canonical units (e.g., "MB" instead of "MiB")
- Missing `precision_us` for time metrics

### Regressions Detected

1. Verify baseline is still valid (not too old)
2. Check for environmental differences (hardware, OS version)
3. Review code changes that may impact performance
4. Consider updating baseline if intentional change

## Best Practices

### For Development

1. Use `quick_bench.sh` for rapid iteration
2. Set lenient regression threshold (20%)
3. Focus on metrology compliance early

### For CI/CD

1. Use `ci` mode (quick + strict)
2. Update baseline on main branch merge
3. Comment PR results but don't block merge
4. Block main branch on regressions

### For Releases

1. Run `standard` or `full` mode
2. Strict metrology validation
3. 10% regression threshold
4. Manual review of results

## Related Documentation

- **Metrology Validator**: `src/erlmcp_metrology_validator.erl`
- **Registry Benchmarks**: `bench/README_BENCHMARKS.md`
- **Transport Real Benchmarks**: `bench/transport_real/INTEGRATION.md`
- **Workload Definitions**: `bench/workloads/*.json`

## Support

For issues or questions:
1. Check execution log: `bench/results/<timestamp>/execution.log`
2. Review metrology violations in summary
3. Compare to baseline manually
4. Open GitHub issue with full results

---

**Last Updated**: 2026-01-27
**Version**: erlmcp v1.3.0
**Status**: Production-Ready Automation
