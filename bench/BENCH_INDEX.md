# ERLMCP Benchmark Suite Index

## Overview

The erlmcp benchmark suite provides comprehensive performance measurement across all system layers:

- **Protocol Layer**: Integration workflows (end-to-end MCP)
- **Transport Layer**: stdio/TCP/HTTP overhead
- **Registry Layer**: Message routing and contention
- **Throughput Layer**: Sustained high-load performance
- **Latency Layer**: Distribution analysis and tail latencies

## Quick Reference

| Benchmark | Purpose | Runtime | When to Use |
|-----------|---------|---------|-------------|
| **Integration** | End-to-end MCP workflows | 2-3 min | Protocol compliance, workflow latency |
| **Latency** | Latency distribution (p50/p95/p99) | 5-10 min | SLA validation, regression detection |
| **Throughput** | Sustained high-throughput | 10-15 min | Capacity planning, stress testing |
| **Registry** | Message routing contention | 10-15 min | Registry performance at 100K connections |
| **Transport** | Transport-specific overhead | Varies | Transport selection, optimization |

## Benchmark Details

### 1. Integration Benchmark

**File**: `bench/erlmcp_bench_integration.erl`
**Runner**: `bash bench/run_integration_bench.sh`
**Docs**: [INTEGRATION_BENCHMARK.md](./INTEGRATION_BENCHMARK.md)

**What it measures**:
- Complete MCP workflows (initialize → tools/prompts/resources → shutdown)
- Protocol encoding/decoding overhead
- Step-by-step latency breakdown
- Workflow success rates
- JSON-RPC efficiency

**Workflows**:
- Basic initialize: 100 iterations, ~3 steps
- Tool sequence: 100 iterations, ~6 steps
- Prompts workflow: 100 iterations, ~4 steps
- Resources workflow: 100 iterations, ~4 steps
- Complete workflow: 50 iterations, ~8 steps

**Output**: `bench/results/integration_<timestamp>.json`

**Key metrics**:
```json
{
  "latency_e2e_p50_ms": 4.8,
  "latency_e2e_p99_ms": 12.5,
  "throughput_workflows_per_s": 810.3,
  "success_rate_percent": 100.0,
  "protocol_overhead_percent": 8.5
}
```

### 2. Latency Suite

**File**: `bench/latency_SUITE.erl`
**Runner**: `rebar3 ct --suite=latency_SUITE`
**Docs**: See inline documentation

**What it measures**:
- Latency stability over time (60s windows)
- Latency under load (1, 10, 50, 100, 500 concurrent)
- Tail latency analysis (p50, p95, p99, p99.9)
- Latency variance (coefficient of variation)
- Memory per request

**Test cases**:
- `latency_stability_test`: Variance across 10s windows
- `latency_under_load_test`: Scaling with concurrency
- `latency_tail_analysis`: Tail latency percentiles
- `latency_variance_test`: Statistical stability
- `memory_per_request`: Memory efficiency

**Output**: CT logs in `_build/test/logs/`

### 3. Throughput Suite

**File**: `bench/throughput_SUITE.erl`
**Runner**: `rebar3 ct --suite=throughput_SUITE`
**Docs**: See inline documentation

**What it measures**:
- Maximum sustained throughput
- Throughput scaling with concurrency
- Throughput stability over time
- Throughput degradation under load
- Resource efficiency (memory, CPU)

**Test cases**:
- `max_throughput_test`: Peak operations/second
- `throughput_scaling_test`: Linear scaling validation
- `sustained_throughput_test`: 60s sustained load
- `resource_efficiency_test`: Memory/CPU per operation

**Output**: CT logs in `_build/test/logs/`

### 4. Registry Contention Benchmark

**File**: `bench/erlmcp_registry_contention.erl`
**Runner**: `bash bench/run_registry_benchmarks.sh`
**Docs**: [README_BENCHMARKS.md](./README_BENCHMARKS.md)

**What it measures**:
- Registry performance at 10K, 25K, 50K, 100K connections
- Registration latency (p50, p95, p99)
- Lookup latency (p50, p95, p99)
- Message routing latency
- Lock contention ratio
- Memory usage scaling

**Scales tested**: 10000, 25000, 50000, 100000 connections

**Output**: `bench/results/registry_contention_<scale>_<timestamp>.csv`

**Key metrics**:
```
Scale  | Reg P99 (ms) | Lookup P99 (ms) | Routing P99 (ms) | Ops/Sec | Contention | Mem (MB)
-------|--------------|-----------------|------------------|---------|------------|----------
100000 | 6.89         | 0.98            | 1.42             | 1650000 | 1.65       | 435.3
```

### 5. Transport Benchmarks

**Directory**: `bench/transport_real/`
**Files**:
- `bench_stdio.erl`: stdio transport
- `bench_tcp.erl`: TCP transport
- `bench_http.erl`: HTTP transport

**What it measures**:
- Transport-specific overhead
- Connection setup/teardown time
- Message framing overhead
- Protocol-specific features (HTTP SSE, TCP keep-alive)

**Output**: Varies by transport

## Running Benchmarks

### Run All Benchmarks (Full Suite)

```bash
# Integration workflows
bash bench/run_integration_bench.sh

# Latency distribution
rebar3 ct --suite=latency_SUITE

# Throughput testing
rebar3 ct --suite=throughput_SUITE

# Registry contention
bash bench/run_registry_benchmarks.sh
```

**Total time**: ~30-40 minutes

### Quick Smoke Test

```bash
# Fast integration check
erl -noshell -pa _build/default/lib/*/ebin \
  -eval 'Workflow = #{id => <<"quick">>, steps => [initialize, list_tools, shutdown], iterations => 10}, erlmcp_bench_integration:benchmark_workflow(Workflow)' \
  -s init stop

# Quick latency check
rebar3 ct --suite=latency_SUITE --case=latency_stability_test
```

**Total time**: ~2-3 minutes

### CI/CD Integration

```yaml
# .github/workflows/benchmarks.yml
name: Benchmarks

on:
  push:
    branches: [main]
  pull_request:

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '25'
          rebar3-version: '3.22'

      - name: Compile
        run: rebar3 compile

      - name: Integration Benchmarks
        run: bash bench/run_integration_bench.sh

      - name: Upload Results
        uses: actions/upload-artifact@v3
        with:
          name: benchmark-results
          path: bench/results/*.json
```

## Interpreting Results

### Performance Targets

| Metric | Target | Threshold |
|--------|--------|-----------|
| **Integration**: E2E P99 | < 30ms | > 50ms warning |
| **Latency**: Operation P99 | < 10ms | > 20ms warning |
| **Throughput**: Ops/sec | > 50K | < 10K warning |
| **Registry**: Lookup P99 @ 100K | < 2ms | > 5ms warning |
| **Success Rate** | 100% | < 99% warning |

### Regression Detection

Compare against baseline:

```bash
# Integration benchmark comparison
jq -s '
  .[0] as $baseline |
  .[1] as $current |
  ($current.latency_e2e_p50_ms - $baseline.latency_e2e_p50_ms) / $baseline.latency_e2e_p50_ms * 100 as $regression |
  if $regression > 10 then
    "REGRESSION: P50 latency increased by \($regression)%"
  else
    "OK: P50 latency change: \($regression)%"
  end
' bench/baseline/integration_baseline.json bench/results/integration_latest.json
```

### Bottleneck Identification

**High integration latency**:
1. Check step-level metrics in integration results
2. Run latency suite to isolate operation
3. Profile with `eprof` or `fprof`

**Low throughput**:
1. Check registry contention metrics
2. Run throughput suite with profiling
3. Check memory usage and GC pauses

**High tail latency**:
1. Run latency tail analysis
2. Check for lock contention
3. Analyze scheduler utilization

## Benchmark Development

### Adding New Benchmark

1. **Create module**: `bench/erlmcp_bench_<name>.erl`
2. **Follow patterns**: See `erlmcp_bench_integration.erl` as template
3. **Add documentation**: Create `bench/<NAME>_BENCHMARK.md`
4. **Add runner script**: `bench/run_<name>_bench.sh`
5. **Update index**: Add entry to this file

### Benchmark Guidelines

- **Deterministic**: Same input → same output
- **Reproducible**: Multiple runs → consistent results
- **Isolated**: No external dependencies
- **Documented**: Clear purpose and interpretation
- **Fast**: < 10 minutes for full run
- **Metrology compliant**: JSON output with standard fields

### Required Output Fields

All benchmarks must export JSON with:

```json
{
  "workload_id": "<unique_id>",
  "benchmark": "<benchmark_name>",
  "iterations": 100,
  "success_rate_percent": 100.0,
  "failures": 0,
  "timeouts": 0,
  "total_duration_s": 1.234,
  "throughput_<unit>_per_s": 810.3,
  "latency_<metric>_p50_ms": 4.8,
  "latency_<metric>_p95_ms": 8.2,
  "latency_<metric>_p99_ms": 12.5,
  "scope": "per_node",
  "precision": "microsecond"
}
```

## Environment Setup

### System Requirements

- **CPU**: 4+ cores recommended
- **Memory**: 8GB+ recommended
- **OS**: Linux, macOS, or Windows (WSL)
- **Erlang/OTP**: 25+

### Tuning for Benchmarks

```bash
# Increase process limit
ulimit -n 200000

# Erlang VM tuning
export ERL_FLAGS="+P 200000 +Q 200000 +t 1000000"

# Disable energy-saving features (Linux)
echo performance | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor
```

### Benchmark-Specific Setup

**Registry benchmarks**:
```bash
# Increase ETS table limit
export ERL_FLAGS="$ERL_FLAGS +e 200000"
```

**Transport benchmarks**:
```bash
# Increase socket buffer sizes
sudo sysctl -w net.core.rmem_max=16777216
sudo sysctl -w net.core.wmem_max=16777216
```

## Troubleshooting

### Benchmarks Fail to Run

**Symptom**: Compilation errors, missing dependencies

**Solution**:
```bash
rebar3 clean
rebar3 compile
rebar3 dialyzer
```

### Inconsistent Results

**Symptom**: High variance between runs

**Solution**:
- Ensure system is idle (no other workloads)
- Disable frequency scaling
- Increase warmup iterations
- Use longer measurement windows

### Out of Memory

**Symptom**: Benchmark crashes with memory exhaustion

**Solution**:
```bash
# Increase heap size
export ERL_FLAGS="$ERL_FLAGS +MHsms 256"

# Enable incremental GC
export ERL_FLAGS="$ERL_FLAGS +hms 32 +hmaxT 128"
```

### Port/Process Limit Errors

**Symptom**: `{system_limit, ...}` errors

**Solution**:
```bash
# Increase limits
ulimit -n 200000
export ERL_FLAGS="+P 200000 +Q 200000"
```

## References

- [Integration Benchmark Documentation](./INTEGRATION_BENCHMARK.md)
- [Registry Benchmark Documentation](./README_BENCHMARKS.md)
- [erlmcp Architecture](../docs/architecture.md)
- [MCP Protocol Specification](https://modelcontextprotocol.io/specification/)
