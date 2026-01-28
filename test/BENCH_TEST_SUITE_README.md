# Benchmark Test Suite - erlmcp_bench_consolidated_SUITE

## Overview

Comprehensive test suite for all 5 erlmcp benchmark modules following **Chicago School TDD** principles (no mocks, real benchmarks, observable behavior verification).

## Test Coverage

### Total Tests: 36

| Category | Tests | Description |
|----------|-------|-------------|
| **Core Operations** | 6 | Message processing, encoding/decoding, latency targets |
| **Network Real** | 8 | TCP/HTTP with real sockets, bandwidth, connection handling |
| **Stress** | 5 | Sustained load, memory stability, trend analysis |
| **Chaos** | 11 | Failure injection, bounded refusal, recovery validation |
| **Integration** | 6 | End-to-end MCP protocol flows, concurrent clients |

## Quality Gates (MANDATORY)

All tests **MUST** pass these gates before PR merge:

- ✅ **All tests pass** (0 failures, 0 errors)
- ✅ **Metrology compliance** - Every benchmark result validated
- ✅ **Determinism** - ±2% variance across 3 runs
- ✅ **Regression detection** - >10% degradation fails test
- ✅ **Memory leak detection** - Stable memory over time
- ✅ **Performance targets** - Meet or exceed plan envelope bounds

## Running Tests

### Full Suite
```bash
rebar3 ct --suite=test/erlmcp_bench_consolidated_SUITE
```

### Individual Test Category
```bash
# Core operations only
rebar3 ct --suite=test/erlmcp_bench_consolidated_SUITE \
  --case=test_core_ops_1k_workload,test_core_ops_100k_workload

# Network real only
rebar3 ct --suite=test/erlmcp_bench_consolidated_SUITE \
  --case=test_tcp_burst_100_workload,test_tcp_sustained_10k_workload

# Chaos scenarios only
rebar3 ct --suite=test/erlmcp_bench_consolidated_SUITE \
  --case=test_chaos_process_crash_recovery,test_chaos_memory_exhaustion_refusal
```

### With Coverage
```bash
rebar3 ct --suite=test/erlmcp_bench_consolidated_SUITE --cover
rebar3 cover --verbose
```

## Test Categories Detail

### 1. Core Operations Tests (6 tests)

Tests message processing fundamentals:

- **test_core_ops_1k_workload** - Small workload baseline (1K messages)
- **test_core_ops_100k_workload** - Production workload (100K messages, 95K msg/s target)
- **test_core_ops_metrology_compliance** - Metrology field validation
- **test_core_ops_deterministic_results** - 3 runs with ±2% variance
- **test_core_ops_memory_no_leaks** - Memory growth <5% over 10 iterations
- **test_core_ops_latency_targets** - P50 <2ms, P95 <8ms, P99 <15ms

**Performance Targets (from plans):**
- Throughput: >95,000 msg/s
- Latency P99: <15ms
- Memory: <100 MB

### 2. Network Real Tests (8 tests)

Tests real TCP/HTTP network operations:

- **test_tcp_burst_100_workload** - Burst traffic (100 messages)
- **test_tcp_sustained_10k_workload** - Sustained load (10K messages, >5K msg/s)
- **test_http_burst_100_workload** - HTTP with real connections (skip if unavailable)
- **test_network_metrology_compliance** - Network-specific metrology validation
- **test_network_connection_failures_tracked** - Failure tracking and error rates
- **test_network_bandwidth_calculated** - Accurate bandwidth calculation (Mbps)
- **test_network_tls_overhead** - TLS vs plain TCP overhead (<50%)
- **test_network_socket_cleanup** - Port leak detection (<5 ports)

**Chicago School Approach:**
- Real TCP sockets (no mocking)
- Actual network I/O
- Observable connection counts and bandwidth

### 3. Stress Tests (5 tests)

Tests sustained load and stability:

- **test_stress_30s_no_degradation** - 30s load with <5% throughput degradation
- **test_stress_5min_memory_stable** - 5min load with <10% memory growth
- **test_stress_time_series_captured** - Time-series data points (≥60 per minute)
- **test_stress_trend_analysis** - Trend detection (throughput, latency, memory)
- **test_stress_early_termination_safety** - No orphaned processes/ports on shutdown

**Stability Targets:**
- Throughput degradation: <5% over time
- Memory growth: <10% over 5 minutes
- No process/port leaks

### 4. Chaos Tests (11 tests, one per scenario)

Tests failure injection and bounded refusal:

- **test_chaos_process_crash_recovery** - Server crash with ≥80% recovery in <2s
- **test_chaos_memory_exhaustion_refusal** - Bounded refusal under memory pressure
- **test_chaos_rate_limit_enforcement** - Rate limiting (1000 msg/s ±10%)
- **test_chaos_invalid_payload_handling** - 100% graceful error handling
- **test_chaos_connection_leak_prevention** - <1% connection leak with churn
- **test_chaos_slow_consumer_timeout** - Timeout enforcement for slow consumers
- **test_chaos_supervisor_restart** - ≥66% child recovery, no data loss
- **test_chaos_disk_full_graceful** - Graceful degradation on disk full
- **test_chaos_cpu_saturation_backpressure** - Backpressure at >90% CPU
- **test_chaos_large_payload_rejection** - 100% rejection of oversized payloads
- **test_chaos_bounded_refusal_validation** - Refusal rate within bounds (10%-90%)

**Chaos Philosophy (Chicago School):**
- Real failure injection (kill processes, exhaust memory)
- Observable recovery behavior
- No mocking of failures

### 5. Integration Tests (6 tests)

Tests end-to-end MCP protocol flows:

- **test_integration_basic_initialize** - Initialize handshake (<100ms)
- **test_integration_tool_sequence** - 300 tool calls with ≥98% success
- **test_integration_concurrent_clients** - 10 clients, 1000 requests, ≥99% success
- **test_integration_protocol_compliance** - 100% MCP protocol conformance
- **test_integration_error_handling** - 100% correct error codes
- **test_integration_e2e_latency_targets** - P50 <5ms, P95 <15ms, P99 <30ms

**Integration Coverage:**
- Initialize + server capabilities
- Tool listing and invocation
- Resource listing and reading
- Error handling (invalid_request, method_not_found, etc.)
- Concurrent client coordination

## Metrology Validation

Every benchmark result is validated against strict metrology standards:

### Required Fields (Performance Claims)
- `workload_id` - Unique workload identifier
- `transport` - Transport type (stdio, tcp, http)
- `duration_seconds` - Actual benchmark duration

### Metric Requirements
- **Unit strings** - Must be from allowed set (µs, ms, s, MiB, req/s, etc.)
- **Scope fields** - Required for memory/rate metrics (e.g., MiB/conn, req/s)
- **Precision fields** - Required for time metrics (raw µs values)
- **Canonical units** - MiB not MB, µs not ms for precision

### Validation Example
```erlang
%% Every test validates metrology compliance:
Result = run_core_ops_benchmark(<<"core_ops_100k">>, 100000),

%% Metrology validation (Chicago School: assert on structure)
ok = erlmcp_metrology_validator:validate_report(Result),

%% Required fields present
#{workload_id := <<"core_ops_100k">>,
  throughput_msg_per_s := Throughput,
  latency_p99_us := P99,
  precision := <<"microsecond">>,
  scope := <<"per_node">>} = Result.
```

## Determinism Testing

Ensures benchmark results are reproducible:

```erlang
%% Run same workload 3 times
Results = [run_core_ops_benchmark(WorkloadId, 1000) || _ <- lists:seq(1, 3)],

%% Extract metric to check
Throughputs = [maps:get(throughput_msg_per_s, R) || R <- Results],

%% Validate variance within ±2%
{ok, VariancePercent} = check_determinism(Throughputs, 2.0).
```

**Pass Criteria:** Variance ≤2% across 3 runs

## Regression Detection

Compares current results to baseline:

```erlang
%% Load baseline from previous run
{ok, Baseline} = load_baseline(WorkloadId, BaselineDir),

%% Run current benchmark
Current = run_core_ops_benchmark(WorkloadId, 100000),

%% Check for regression (>10% degradation)
ok = check_regression(Current, Baseline).
```

**Fail Criteria:** >10% throughput degradation vs baseline

## Memory Leak Detection

```erlang
%% Baseline memory
BaselineMemory = erlang:memory(total),

%% Run multiple iterations
[run_core_ops_benchmark(<<"mem_test">>, 1000) || _ <- lists:seq(1, 10)],

%% Force GC and check growth
erlang:garbage_collect(),
timer:sleep(100),
FinalMemory = erlang:memory(total),
MemoryGrowth = (FinalMemory - BaselineMemory) / BaselineMemory * 100,

%% Pass if growth <5%
?assert(MemoryGrowth < 5.0).
```

## Helper Module: erlmcp_bench_test_helpers

Provides reusable test utilities:

```erlang
%% Determinism checking
{ok, Variance} = erlmcp_bench_test_helpers:check_determinism(Values, 2.0),

%% Regression detection
ok = erlmcp_bench_test_helpers:check_regression(Current, Baseline),

%% Variance calculation
{Mean, Variance, StdDev} = erlmcp_bench_test_helpers:calculate_variance(Values),

%% Baseline management
{ok, Baseline} = erlmcp_bench_test_helpers:load_baseline(WorkloadId, BaselineDir),
ok = erlmcp_bench_test_helpers:save_baseline(WorkloadId, Result, BaselineDir),

%% Cleanup
ok = erlmcp_bench_test_helpers:cleanup_test_data().
```

## CI Integration

### Pre-Merge Quality Gates
```bash
#!/bin/bash
# Run in CI pipeline

# 1. Run all benchmark tests
rebar3 ct --suite=test/erlmcp_bench_consolidated_SUITE

# 2. Check for test failures
if [ $? -ne 0 ]; then
    echo "❌ Benchmark tests failed"
    exit 1
fi

# 3. Validate coverage
rebar3 cover --verbose
COVERAGE=$(rebar3 cover --verbose | grep "total" | awk '{print $2}')
if [ "${COVERAGE%\%}" -lt 80 ]; then
    echo "❌ Coverage below 80% (${COVERAGE})"
    exit 1
fi

echo "✅ All benchmark tests passed"
```

### Regression Detection in CI
```bash
# Load baseline from artifacts
BASELINE_DIR="${CI_PROJECT_DIR}/bench/results/baseline"

# Run benchmarks
rebar3 ct --suite=test/erlmcp_bench_consolidated_SUITE

# Compare to baseline
REGRESSION=$(rebar3 ct --suite=test/erlmcp_bench_consolidated_SUITE | grep "regression_detected")
if [ -n "$REGRESSION" ]; then
    echo "❌ Performance regression detected"
    exit 1
fi
```

## Example Test Output

```
Starting test case: test_core_ops_100k_workload
Testing core ops with 100K message workload
Core ops 100K workload PASSED: 98543 msg/s, P99 12345 µs, Mem 48 MB
Test case test_core_ops_100k_workload completed in 1024 ms

Starting test case: test_chaos_process_crash_recovery
Testing chaos: process crash recovery
Process crash recovery PASSED: 5/5 recovered in 723 ms
Test case test_chaos_process_crash_recovery completed in 5234 ms

Starting test case: test_integration_e2e_latency_targets
Testing integration: end-to-end latency targets
E2E latency PASSED: P50 3.2 ms, P95 12.5 ms, P99 25.8 ms
Test case test_integration_e2e_latency_targets completed in 10123 ms
```

## Future Enhancements

### Planned Improvements
1. **Distributed Testing** - Multi-node benchmarks for clustering scenarios
2. **Historical Trends** - Track performance over time with database storage
3. **Automated Baselines** - Auto-update baselines on successful PRs
4. **Profiling Integration** - Flamegraphs and bottleneck identification
5. **Custom Workloads** - JSON-driven workload definitions

## References

- **Metrology Validator**: `src/erlmcp_metrology_validator.erl`
- **Benchmark Plans**: `bench/workloads/*.json`
- **Test Helpers**: `test/erlmcp_bench_test_helpers.erl`
- **Chicago School TDD**: Focus on behavior, not implementation
- **OTP Patterns**: `docs/otp-patterns.md`

## Support

For questions or issues:
1. Check test output for detailed error messages
2. Review metrology violations with `erlmcp_metrology_validator:format_violation/1`
3. Run individual tests for faster debugging
4. Inspect baseline files in `bench/results/baseline/`

---

**Last Updated:** 2026-01-27
**Test Suite Version:** 1.0.0
**Coverage Target:** ≥80% (≥85% for core modules)
