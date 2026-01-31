# Benchmark Methodology - erlmcp v1.5.0

**Version**: 1.5.0
**Last Updated**: 2026-01-30
**Status**: Canonical

---

## Overview

This document defines the official benchmark methodology for erlmcp performance measurements. All benchmarks must follow these procedures to ensure metrology compliance and reproducible results.

---

## Core Principles

### 1. Metrology Compliance

All benchmarks MUST comply with **erlmcp Metrology v1.5.0**:

- Canonical units only (no `req/s`, use `msg_per_s`)
- Workload references for all performance claims
- Scope specification (`per_node`, `per_cluster`, `per_connection`)
- Precision specification (`microsecond`, `millisecond`)
- Transport specification (`tcp`, `http`, `stdio`, `websocket`)
- Environment specification (hardware profile)

### 2. Chicago School TDD

**NO MOCKS, FAKE, OR PLACEHOLDER IMPLEMENTATIONS**:
- Use real erlmcp processes
- Use real sockets (gen_tcp, gun, cowboy)
- Test all observable behavior
- Measure actual protocol overhead

### 3. Reproducibility

All benchmarks MUST be reproducible:
- Deterministic workloads
- Documented environment specs
- Version-controlled dependencies
- Automated execution

---

## Benchmark Suite Architecture

### Consolidated Modules (v1.5.0)

The benchmark suite consolidates 15+ legacy modules into 5 focused modules:

| Module | Purpose | Source Files | Workloads |
|--------|---------|--------------|-----------|
| **core_ops** | In-memory operations | `erlmcp_bench_core_ops.erl` | 4 |
| **network_real** | Real transport sockets | `erlmcp_bench_network_real.erl` | 7 |
| **stress** | Sustained load | `erlmcp_bench_stress.erl` | 4 |
| **chaos** | Failure injection | `erlmcp_bench_chaos.erl` | 11 |
| **integration** | MCP workflows | `erlmcp_bench_integration.erl` | 5 |

**Total**: 31 benchmark workloads across 5 modules

---

## Measurement Standards

### Throughput Measurement

**Canonical Unit**: `msg_per_s` (messages per second)

**Definition**: Number of complete JSON-RPC messages processed per second.

```erlang
%% Formula
Throughput = TotalMessages / DurationSeconds

%% Where:
%% TotalMessages = Requests + Responses (counted separately)
%% DurationSeconds = Actual measured duration (wall clock)
```

**Requirements**:
- Measure wall-clock time (monotonic)
- Count requests and responses separately
- Include protocol overhead (encoding, framing)
- Exclude warmup period

### Latency Measurement

**Canonical Units**:
- `latency_p50_us` - Median latency (microseconds)
- `latency_p95_us` - 95th percentile (microseconds)
- `latency_p99_us` - 99th percentile (microseconds)

**Measurement Points**:
- `client_to_client` - Full round-trip (application layer)
- `server_processing` - Server-side only
- `network_only` - Transport layer

**Requirements**:
- Use monotonic time (high resolution)
- Sample first 10K operations (or use reservoir sampling)
- Exclude warmup period
- Report P50, P95, P99

### Memory Measurement

**Canonical Units**:
- `per_connection_heap_mib` - Heap memory per connection
- `per_connection_state_mib` - Application state per connection
- `per_node_base_overhead_mib` - Fixed overhead before connections
- `per_node_total_rss_mib` - Total RSS under load

**Measurement Method**:
```erlang
%% Heap per connection
HeapPerConn = (MemoryAfter - MemoryBefore) / (1024 * 1024 * Connections)

%% Total RSS
TotalRSS = erlang:memory(total) / (1024 * 1024)
```

**Formula Validation**:
```
per_node_total_rss_mib ≈
  per_node_base_overhead_mib +
  (connections × (per_connection_heap_mib + per_connection_state_mib)) +
  overhead_margin_mib
```

---

## Benchmark Execution Procedure

### 1. Environment Preparation

```bash
# 1. Clean environment
erl -noshell -eval "erlang:halt()." -sname cleanup

# 2. Start with clean VM
rebar3 shell --apps erlmcp

# 3. Verify system state
erlang:system_info(process_count).
erlang:memory(total).
```

### 2. Warmup Phase

**Purpose**: Allow JIT compilation and cache warming

**Duration**: 10% of benchmark duration (minimum 5 seconds)

### 3. Benchmark Execution

**Standard Pattern**:
```erlang
%% 1. Capture baseline
StartMemory = erlang:memory(total),
StartTime = erlang:monotonic_time(microsecond),

%% 2. Execute workload
Results = execute_workload(WorkloadSpec),

%% 3. Capture final metrics
EndTime = erlang:monotonic_time(microsecond),
EndMemory = erlang:memory(total),

%% 4. Calculate metrics
DurationUs = EndTime - StartTime,
DurationS = DurationUs / 1_000_000,
MemoryDelta = EndMemory - StartMemory.
```

### 4. Result Validation

**Metrology Validation**:
```erlang
%% Validate required fields
validate_metrics(Result) ->
    Required = [
        workload_id, benchmark, timestamp, environment,
        operations, duration_s, throughput_msg_per_s,
        latency_p50_us, latency_p95_us, latency_p99_us,
        precision, scope
    ],
    lists:all(fun(F) -> maps:is_key(F, Result) end, Required).
```

---

## Workload Definition Standards

### Workload ID Format

**Pattern**: `{transport}_{pattern}_{scale}_{payload}`

**Examples**:
- `tcp_sustained_10k_1kib` - TCP, sustained, 10K connections, 1 KiB
- `http_burst_50k_mixed` - HTTP, burst, 50K connections, mixed sizes
- `core_ops_100k` - In-memory, 100K operations

### Workload Specification

Every workload MUST have a specification in `bench/workloads/{id}.json`:

```json
{
  "workload_id": "tcp_sustained_10k_1kib",
  "transport": "tcp",
  "pattern": "sustained",
  "duration_s": 300,
  "connections": 10000,
  "message_size_bytes": 1024,
  "expected_performance": {
    "throughput_msg_per_s": 43000,
    "latency_p99_us": 15800,
    "memory_rss_mib_per_node": 512
  },
  "environment": "prod_hw_spec_01"
}
```

---

## Statistical Analysis

### Percentile Calculation

**Method**: Nearest-rank method

```erlang
percentile(SortedList, P) when P >= 0, P =< 1 ->
    N = length(SortedList),
    Index = max(1, min(N, round(N * P))),
    lists:nth(Index, SortedList).
```

### Sampling Strategy

**For high-volume operations** (>1M ops):
- Use reservoir sampling (10K samples max)
- Random start position to avoid bias
- Strided sampling to cover duration

### Degradation Detection

**Linear Regression Method**:
```erlang
%% Detect throughput degradation
detect_degradation(Samples) ->
    Throughputs = [maps:get(<<"tput">>, S) || S <- Samples],
    Slope = linear_regression_slope(Throughputs),
    FirstThroughput = hd(Throughputs),
    SlopePerMinute = Slope * 12,  %% Samples every 5s
    PercentDecline = (SlopePerMinute / FirstThroughput) * 100,
    PercentDecline < -5.0.  %% Threshold: 5% decline per minute
```

---

## Transport-Specific Methodology

### TCP Transport

**Implementation**: `ranch` (gen_tcp acceptor pool)

**Connection Pattern**:
```erlang
%% Client connection
{ok, Socket} = gen_tcp:connect(Host, Port,
    [binary, {active, false}, {packet, line}, {nodelay, true}],
    5000).

%% Message send
gen_tcp:send(Socket, [Payload, <<"\n">>]).

%% Message receive
gen_tcp:recv(Socket, 0, 5000).
```

### HTTP/2 Transport

**Implementation**: `gun` (client) + `cowboy` (server)

**Connection Pattern**:
```erlang
%% Open HTTP/2 connection
{ok, GunPid} = gun:open(Host, Port, #{
    protocols => [http2],
    transport => tcp
}),
{ok, http2} = gun:await_up(GunPid, 5000).

%% Send POST request
StreamRef = gun:post(GunPid, Path, [
    {<<"content-type">>, <<"application/json">>}
], Payload).
```

---

## Chaos Engineering Methodology

### Bounded Refusal Validation

**Principle**: Refusal MUST occur BEFORE resource exhaustion

**Validation Criteria**:
```erlang
validate_bounded_refusal(Result, Scenario) ->
    ExpectedCode = maps:get(expected_code, Scenario),
    ActualCode = maps:get(refusal_code, Result),
    DetectionTime = maps:get(detection_time_ms, Result),
    RecoveryTime = maps:get(recovery_time_ms, Result),

    CodeMatches = ActualCode =:= ExpectedCode,
    FastDetection = DetectionTime < 1000,  %% <1s
    AutoRecovery = RecoveryTime < 5000,    %% <5s
    NoDataLoss = maps:get(data_loss, Result) =:= false,
    NoCascade = maps:get(cascading_failures, Result) =:= 0,

    CodeMatches andalso FastDetection andalso AutoRecovery
        andalso NoDataLoss andalso NoCascade.
```

---

## Baseline Establishment

### Baseline Criteria

A measurement becomes a **baseline** when:
1. Executed on reference hardware (`prod_hw_spec_01`)
2. Metrology-compliant (v1.5.0)
3. Reproducible (<5% variance across 3 runs)
4. Documented in `V2.1.0_PERFORMANCE_BASELINES.md`
5. Validated against regression tests

### Baseline Update Procedure

**Weekly Review** (every Monday):
1. Run full benchmark suite
2. Compare to existing baseline
3. If variance >5%, investigate
4. If variance >10%, mark as **CRITICAL REGRESSION**
5. Update baseline only if justified improvement

---

## Continuous Integration

### Pre-Commit Checks

```bash
#!/bin/bash
# scripts/bench/pre_commit_bench.sh

# Quick validation (< 2 min)
rebar3 shell --eval "
    io:format(\"Running quick benchmark...~n\"),
    erlmcp_bench_core_ops:run(<<"core_ops_10k">>),
    erlmcp_bench_stress:quick_stress(),
    erlang:halt().
"
```

### CI/CD Integration

**Workflow**: `.github/workflows/benchmark.yml`

```yaml
name: Benchmark Validation
on: [push, pull_request]
jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Run benchmarks
        run: ./scripts/bench/run_all_benchmarks.sh
      - name: Check regression
        run: ./tools/regression/detect-regression.sh
```

---

## Reporting Standards

### Benchmark Result Report

**Required Sections**:
1. Executive Summary
2. Workload Description
3. Environment Specification
4. Results (throughput, latency, memory)
5. Comparison to Baseline
6. Metrology Compliance Badge

### Metrology Compliance Badge

```
METROLOGY COMPLIANT v1.5.0
- Canonical units: msg_per_s, MiB, us
- Workload reference: tcp_sustained_10k_1kib
- Scope: per_node
- Precision: microsecond
```

---

## References

- **Metrics Glossary**: `METRICS_GLOSSARY.md`
- **Performance Baselines**: `V2.1.0_PERFORMANCE_BASELINES.md`
- **Validation Report**: `V1.5.0_VALIDATION_REPORT.md`
- **Benchmark Suite**: `apps/erlmcp_core/test/erlmcp_bench_*.erl`

---

**Document Status**: CANONICAL
**Last Updated**: 2026-01-30
**Maintainer**: erlmcp Performance Team
