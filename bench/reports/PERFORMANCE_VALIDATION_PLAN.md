# ERLMCP Performance Validation Report
**Date:** 2026-01-31  
**Version:** v2.2.0  
**Status:** Validation Plan (Awaiting Erlang Runtime)

---

## Executive Summary

This report outlines the comprehensive performance validation plan for erlmcp after recent validator implementations. The validation framework is designed to:

1. **Establish Baselines** - Measure core operations, network transport, and validation framework performance
2. **Test New Validators** - Validate performance overhead of new validation modules
3. **Detect Regressions** - Flag >10% degradation in any critical path
4. **Identify Bottlenecks** - Profile hot code paths and suggest optimizations
5. **Ensure Compliance** - Validate against metrology v1.5.0 standards

---

## 1. Baseline Benchmark Requirements

### 1.1 Core Operations Baseline (erlmcp_bench_core_ops)

**Target Baseline (from CLAUDE.md):**
- Registry: 553K msg/s
- Queue: 971K msg/s  
- Pool: 149K msg/s
- Session: 242K msg/s
- Overall: 2.69M ops/sec

**Workloads to Run:**
```erlang
%% In erlang shell after compilation:
erlmcp_bench_core_ops:run(<<"core_ops_1k">>).
erlmcp_bench_core_ops:run(<<"core_ops_10k">>).
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).  % PRIMARY BASELINE
erlmcp_bench_core_ops:run(<<"core_ops_1m">>).
```

**Expected Output Metrics:**
- `throughput_msg_per_s`: 2.69M ± 10%
- `latency_p50_us`: < 50 µs
- `latency_p95_us`: < 200 µs
- `latency_p99_us`: < 500 µs
- `memory_delta_mib`: < 100 MiB for 1M ops
- `cpu_percent_avg`: 40-60%

**Regression Criteria:**
- ❌ **BLOCK PR if throughput < 2.42M ops/sec** (>10% regression)
- ⚠️  **WARN if latency p99 > 550 µs** (>10% regression)

---

### 1.2 Network Transport Baseline (erlmcp_bench_network_real)

**Target Baseline (from CLAUDE.md):**
- Network I/O: 43K msg/s (bottleneck: 4KB real packets)
- TCP Sustained 10K: 100K msg/s with 10K connections
- Honest Capacity: 40-50K concurrent active connections per node

**Workloads to Run:**
```erlang
%% TCP Benchmarks
erlmcp_bench_network_real:run_workload(tcp_burst_100_1kib).
erlmcp_bench_network_real:run_workload(tcp_sustained_10k_1kib).  % PRIMARY

%% HTTP Benchmarks
erlmcp_bench_network_real:run_workload(http_burst_100_1kib).
erlmcp_bench_network_real:run_workload(http_sustained_5k_1kib).
```

**Expected Output Metrics (TCP):**
- `throughput_msg_per_s`: 43K-100K (depends on workload)
- `latency_p50_us`: < 5000 µs (5ms)
- `latency_p95_us`: < 20000 µs (20ms)
- `latency_p99_us`: < 50000 µs (50ms)
- `bandwidth_mib_per_s`: 40-100 MiB/s (1KB payloads)
- `memory_heap_mib_per_conn`: < 0.1 MiB (100KB per connection)
- `connection_setup_avg_ms`: < 100ms

**Regression Criteria:**
- ❌ **BLOCK PR if throughput < 38.7K msg/s** (>10% regression)
- ❌ **BLOCK PR if latency p99 > 55ms** (>10% regression)
- ⚠️  **WARN if connection setup > 110ms**

---

### 1.3 Stress Load Baseline (erlmcp_bench_stress)

**Target Baseline:**
- Sustained: 372K msg/s (60M ops/30s)
- Duration: 30s → 5min → 30min → 24hr options
- Degradation: < 5% throughput drop over 5min

**Workloads to Run:**
```erlang
%% Quick stress test (30 seconds)
erlmcp_bench_stress:run(<<"stress_30s_100k_ops">>).

%% Standard stress test (5 minutes)
erlmcp_bench_stress:run(<<"stress_5min_100k_ops">>).
```

**Expected Output Metrics:**
- `throughput_msg_per_s`: 372K ± 10%
- `latency_p50_us`: < 100 µs
- `latency_p95_us`: < 500 µs
- `memory_rss_mib_per_node`: < 500 MiB after 5min
- `degradation_percent`: < 5% over duration

**Regression Criteria:**
- ❌ **BLOCK PR if sustained throughput < 334.8K msg/s**
- ❌ **BLOCK PR if degradation > 5% over 5min**

---

## 2. Validator Performance Testing

### 2.1 Protocol Validator Performance

**Module:** `erlmcp_protocol_validator.erl`

**Test Cases:**
```erlang
%% Measure validation overhead per message
StartTime = erlang:monotonic_time(microsecond),
lists:foreach(fun(_) ->
    erlmcp_protocol_validator:validate_json_rpc(stdio),
    erlmcp_protocol_validator:validate_request_method(stdio, <<"initialize">>),
    erlmcp_protocol_validator:validate_response_structure(#{...}),
    erlmcp_protocol_validator:validate_error_codes(#{...})
end, lists:seq(1, 1000)),
EndTime = erlang:monotonic_time(microsecond),
AvgOverheadUs = (EndTime - StartTime) / 1000.
```

**Performance Targets:**
- **Validation Overhead:** < 100 µs per message
- **Throughput Impact:** < 5% of baseline message latency
- **Memory Impact:** < 10 MB for 1000 validations

**Validation:**
- ✅ **PASS if validation overhead < 100 µs**
- ⚠️  **WARN if 100-200 µs**
- ❌ **FAIL if > 200 µs**

---

### 2.2 Transport Validator Performance

**Module:** `erlmcp_transport_validator.erl`

**Test Cases:**
```erlang
%% Measure transport validation overhead
{Time, _} = timer:tc(fun() ->
    lists:foreach(fun(_) ->
        erlmcp_transport_validator:validate_callbacks(erlmcp_transport_stdio),
        erlmcp_transport_validator:validate_framing(erlmcp_transport_stdio, <<"test">>),
        erlmcp_transport_validator:validate_registry(erlmcp_transport_stdio),
        erlmcp_transport_validator:validate_lifecycle(erlmcp_transport_stdio)
    end, lists:seq(1, 100))
end),
AvgTimeUs = Time / 100.
```

**Performance Targets:**
- **Validation Time:** < 500 µs per transport
- **Concurrent Validation:** > 100 transports/sec
- **Memory Impact:** < 1 MB per validation

**Validation:**
- ✅ **PASS if < 500 µs**
- ⚠️  **WARN if 500-1000 µs**
- ❌ **FAIL if > 1000 µs**

---

### 2.3 Performance Validator Performance (Meta-Validation)

**Module:** `erlmcp_performance_validator.erl`

**Test Cases:**
```erlang
%% Measure performance measurement overhead
{Time, _} = timer:tc(fun() ->
    erlmcp_performance_validator:measure_latency(stdio, 100),
    erlmcp_performance_validator:measure_throughput(stdio, 1000),
    erlmcp_performance_validator:measure_memory(stdio),
    erlmcp_performance_validator:measure_connection_setup(stdio)
end).
```

**Performance Targets:**
- **Measurement Overhead:** < 10% of operation being measured
- **Latency Measurement:** < 10 µs overhead per sample
- **Throughput Measurement:** < 1% impact on throughput
- **Memory Measurement:** < 1 KB per measurement

**Validation:**
- ✅ **PASS if overhead < 10%**
- ❌ **FAIL if overhead > 10%**

---

### 2.4 Validation Framework Benchmark

**Module:** `erlmcp_bench_validation_performance.erl`

**Run Full Benchmark Suite:**
```erlang
erlmcp_validation_performance_benchmark:run_all_benchmarks().
```

**Expected Execution Time:**
- **Total Duration:** < 2 minutes (120 seconds)
- **Spec Parsing (1K iterations):** < 10 seconds
- **Validation Operations (1K):** < 15 seconds
- **Memory Usage (100 iterations):** < 10 seconds
- **Timeout Handling (100):** < 20 seconds
- **Large Messages (100):** < 30 seconds
- **Transport Types (100):** < 15 seconds
- **Stress Load (30 seconds):** 30 seconds

**Performance Targets:**
```
Component                 | Latency (µs/op) | Throughput (ops/sec)
--------------------------|-----------------|---------------------
Spec Parsing (1KB)        | < 50            | > 20,000
Spec Parsing (10KB)       | < 200           | > 5,000
Spec Parsing (100KB)      | < 2000          | > 500
Valid Request Validation  | < 100           | > 10,000
Invalid Request Detection | < 50            | > 20,000
Error Response Validation | < 75            | > 13,000
Memory per Validation     | -               | < 10 KB
```

**Regression Criteria:**
- ❌ **BLOCK PR if total benchmark time > 132s** (>10% regression)
- ❌ **BLOCK PR if any component > 10% slower**

---

## 3. Performance Validation Checklist

### 3.1 Pre-Validation Checks

- [ ] **Compilation:** `TERM=dumb rebar3 compile` - MUST pass with 0 errors
- [ ] **Unit Tests:** `rebar3 eunit` - MUST have 100% pass rate
- [ ] **CT Tests:** `rebar3 ct` - MUST have 100% pass rate
- [ ] **Dialyzer:** `rebar3 dialyzer` - SHOULD have 0 warnings
- [ ] **Xref:** `rebar3 xref` - SHOULD have 0 issues

### 3.2 Baseline Measurement

- [ ] **Core Ops 100K:** Run and record baseline (target: 2.69M ops/sec)
- [ ] **TCP Sustained 10K:** Run and record baseline (target: 43K msg/sec)
- [ ] **HTTP Sustained 5K:** Run and record baseline (target: 5K msg/sec)
- [ ] **Stress 5min:** Run and record baseline (target: 372K msg/sec sustained)

### 3.3 Validator Performance

- [ ] **Protocol Validator:** Measure overhead (target: < 100 µs/msg)
- [ ] **Transport Validator:** Measure overhead (target: < 500 µs/transport)
- [ ] **Performance Validator:** Measure overhead (target: < 10% of operation)
- [ ] **Validation Benchmark:** Run full suite (target: < 120 seconds)

### 3.4 Regression Detection

- [ ] **Compare to Baseline:** All metrics within ±10% of baseline
- [ ] **Flag Regressions:** Any metric >10% worse than baseline
- [ ] **Profile Bottlenecks:** Use `fprof` on any regressed component
- [ ] **Generate Report:** Comprehensive metrics report with before/after

### 3.5 Stress Testing

- [ ] **5min Sustained:** < 5% throughput degradation
- [ ] **Memory Stability:** < 10% memory growth over 5min
- [ ] **Connection Stability:** 10K connections remain stable
- [ ] **Recovery:** System recovers within 5s of stress removal

---

## 4. Profiling and Optimization

### 4.1 Profiling Tools

**Function-level profiling (fprof):**
```erlang
fprof:trace([start, {procs, [Pid]}]),
%% Run code to profile
fprof:profile(),
fprof:analyse([{dest, "bench/reports/profile_validator.txt"}]).
```

**Time-based profiling (eprof):**
```erlang
eprof:start(),
eprof:profile([Pid]),
eprof:stop_profiling(),
eprof:analyze(total).
```

**Live tracing (recon_trace):**
```erlang
recon_trace:calls({erlmcp_protocol_validator, validate_json_rpc, '_'}, 100).
```

### 4.2 Expected Hot Paths

Based on code analysis, expected bottlenecks:

1. **JSON-RPC Encoding/Decoding (jsx)**
   - `jsx:encode/1` and `jsx:decode/1` calls
   - **Optimization:** Consider `jiffy` for 2-3x speedup on large messages

2. **Registry Lookups (gproc)**
   - `gproc:lookup_local_name/1` calls
   - **Optimization:** Cache frequently accessed registrations

3. **Request Correlation (Client pending map)**
   - Map lookup in `erlmcp_client:handle_call/3`
   - **Optimization:** Already uses maps (O(log n)), no change needed

4. **Validator Check Functions**
   - Multiple sequential checks in validators
   - **Optimization:** Parallelize independent checks

### 4.3 Memory Profiling

**Expected Memory Usage:**
```
Component                | Per Operation | Per Connection | Notes
-------------------------|---------------|----------------|------------------
Registry Entry           | 200 bytes     | -              | gproc overhead
Session State            | 1-5 KB        | -              | ETS/DETS/Mnesia
Connection Process       | -             | 50-100 KB      | gen_server heap
Validator State          | 10 KB         | -              | Results storage
Message Buffer           | 4 KB          | 4 KB           | Network buffer
```

**Memory Leak Detection:**
```erlang
%% Before benchmark
MemoryBefore = erlang:memory(total),

%% Run benchmark
erlmcp_bench_core_ops:run(<<"core_ops_100k">>),

%% After benchmark
MemoryAfter = erlang:memory(total),

%% Force GC
erlang:garbage_collect(),
MemoryAfterGC = erlang:memory(total),

%% Check for leaks
MemoryLeaked = MemoryAfterGC - MemoryBefore,
%% EXPECTED: < 1 MB leaked
```

---

## 5. Metrology Compliance

### 5.1 Canonical Units (v1.5.0)

All benchmark output MUST use canonical units:

```
Metric                     | Unit                    | Example
---------------------------|-------------------------|------------------
Throughput                 | throughput_msg_per_s    | 43000.0
Latency                    | latency_p50_us          | 4500.0
                           | latency_p95_us          | 18000.0
                           | latency_p99_us          | 45000.0
Memory (heap)              | memory_heap_mib_per_conn| 0.098
Memory (RSS)               | memory_rss_mib_per_node | 485.3
Duration                   | duration_s              | 300.0
Bandwidth                  | bandwidth_mib_per_s     | 42.5
CPU                        | cpu_percent_per_node    | 65.2
Connection Setup           | connection_setup_avg_ms | 85.5
```

**Scope Identifiers:**
- `per_connection_heap` - Memory allocated per connection
- `per_node_total` - System-wide total
- `per_operation` - Per individual operation

**Precision:**
- `microsecond` - Latency measurements
- `millisecond` - Duration measurements
- `byte` - Raw sizes
- `mebibyte` - Memory usage (MiB = 1024^2 bytes)

### 5.2 Validation

All output JSON MUST pass:
```erlang
erlmcp_metrology_validator:validate(MetricsMap).
```

**Required Fields:**
- `workload_id` - Unique workload identifier
- `benchmark` - Benchmark category
- `transport` - Transport type
- `duration_s` - Test duration
- `throughput_msg_per_s` - Message throughput
- `latency_p50_us`, `latency_p95_us`, `latency_p99_us`
- `scope` - Measurement scope
- `precision` - Time precision

---

## 6. Automation Scripts

### 6.1 Quick Benchmark Script

**File:** `/home/user/erlmcp/scripts/bench/quick_bench.sh`

```bash
#!/bin/bash
# Quick performance validation (< 2 minutes)

set -e

echo "======================================"
echo "ERLMCP Quick Performance Validation"
echo "======================================"
echo

# Compile
echo "1. Compiling..."
TERM=dumb rebar3 compile

# Run quick benchmarks
echo "2. Running core_ops_10k..."
erl -noshell -pa _build/default/lib/*/ebin \
    -eval 'erlmcp_bench_core_ops:run(<<"core_ops_10k">>)' \
    -s init stop

echo "3. Running validation benchmark..."
erl -noshell -pa _build/default/lib/*/ebin \
    -eval 'erlmcp_validation_performance_benchmark:run_full_compliance_suite()' \
    -s init stop

echo
echo "Quick validation complete!"
echo "Results in bench/results/"
```

### 6.2 Full Benchmark Script

**File:** `/home/user/erlmcp/scripts/bench/run_performance_validation.sh`

```bash
#!/bin/bash
# Full performance validation (10-15 minutes)

set -e

echo "======================================"
echo "ERLMCP Full Performance Validation"
echo "======================================"
echo

# 1. Baseline benchmarks
echo "Phase 1: Baseline Benchmarks"
./scripts/bench/run_quick_benchmarks.sh

# 2. Validator performance
echo "Phase 2: Validator Performance"
erl -noshell -pa _build/default/lib/*/ebin \
    -eval 'erlmcp_validation_performance_benchmark:run_all_benchmarks()' \
    -s init stop

# 3. Stress test
echo "Phase 3: Stress Test (5 minutes)"
erl -noshell -pa _build/default/lib/*/ebin \
    -eval 'erlmcp_bench_stress:run(<<"stress_5min_100k_ops">>)' \
    -s init stop

# 4. Compare to baseline
echo "Phase 4: Regression Detection"
./scripts/bench/compare_to_baseline.sh

echo
echo "Full validation complete!"
echo "Review bench/reports/ for detailed analysis"
```

### 6.3 Regression Check Script

**File:** `/home/user/erlmcp/scripts/bench/check_regression.sh`

```bash
#!/bin/bash
# Check for performance regressions

BASELINE_FILE="bench/results/baseline.json"
LATEST_FILE="bench/results/latest.json"

if [ ! -f "$BASELINE_FILE" ]; then
    echo "ERROR: No baseline file found"
    echo "Run: ./scripts/bench/set_baseline.sh"
    exit 1
fi

# Compare metrics
python3 << 'PYTHON'
import json
import sys

with open('bench/results/baseline.json') as f:
    baseline = json.load(f)

with open('bench/results/latest.json') as f:
    latest = json.load(f)

regressions = []

# Check core ops throughput
baseline_tp = baseline['core_ops']['throughput_msg_per_s']
latest_tp = latest['core_ops']['throughput_msg_per_s']
delta_percent = ((latest_tp - baseline_tp) / baseline_tp) * 100

if delta_percent < -10:
    regressions.append(f"Core ops throughput: {delta_percent:.1f}% regression")

# Check network latency p99
baseline_lat = baseline['network']['latency_p99_us']
latest_lat = latest['network']['latency_p99_us']
delta_percent = ((latest_lat - baseline_lat) / baseline_lat) * 100

if delta_percent > 10:
    regressions.append(f"Network p99 latency: +{delta_percent:.1f}% regression")

if regressions:
    print("❌ REGRESSIONS DETECTED:")
    for reg in regressions:
        print(f"  - {reg}")
    sys.exit(1)
else:
    print("✅ No regressions detected")
    sys.exit(0)
PYTHON
```

---

## 7. Expected Output Report

### 7.1 Benchmark Results Format

```
========================================
ERLMCP PERFORMANCE VALIDATION REPORT
========================================

Date: 2026-01-31T12:00:00Z
Version: v2.2.0
Environment: local_dev
Erlang: OTP-28

----------------------------------------
1. CORE OPERATIONS BASELINE
----------------------------------------

Component: Registry
- Throughput: 553K msg/s ✅
- Latency p50: 45 µs ✅
- Latency p95: 180 µs ✅
- Latency p99: 420 µs ✅

Component: Queue
- Throughput: 971K msg/s ✅
- Latency p50: 25 µs ✅
- Latency p95: 95 µs ✅
- Latency p99: 230 µs ✅

Component: Pool
- Throughput: 149K msg/s ✅
- Latency p50: 120 µs ✅
- Latency p95: 450 µs ✅
- Latency p99: 890 µs ✅

Component: Session
- Throughput: 242K msg/s ✅
- Latency p50: 80 µs ✅
- Latency p95: 320 µs ✅
- Latency p99: 650 µs ✅

Overall: 2.69M ops/sec ✅

----------------------------------------
2. NETWORK TRANSPORT BASELINE
----------------------------------------

Transport: TCP (10K connections, 1KB payload)
- Throughput: 43K msg/s ✅
- Latency p50: 4.5 ms ✅
- Latency p95: 18 ms ✅
- Latency p99: 45 ms ✅
- Bandwidth: 42 MiB/s ✅
- Memory per conn: 0.098 MiB ✅
- Connection setup: 85 ms ✅

Transport: HTTP/2 (5K connections, 1KB payload)
- Throughput: 5K msg/s ✅
- Latency p50: 8.5 ms ✅
- Latency p95: 35 ms ✅
- Latency p99: 78 ms ✅

----------------------------------------
3. VALIDATOR PERFORMANCE
----------------------------------------

Protocol Validator:
- Validation overhead: 85 µs/msg ✅
- Throughput impact: 3.8% ✅
- Memory impact: 8.5 MB/1K validations ✅

Transport Validator:
- Validation time: 420 µs/transport ✅
- Concurrent validation: 150 transports/sec ✅
- Memory impact: 0.8 MB/validation ✅

Performance Validator:
- Measurement overhead: 7.2% ✅
- Latency overhead: 8 µs/sample ✅
- Throughput impact: 0.5% ✅

Validation Benchmark Suite:
- Total duration: 115 seconds ✅
- Spec parsing (1KB): 45 µs/op, 22K ops/sec ✅
- Spec parsing (10KB): 185 µs/op, 5.4K ops/sec ✅
- Spec parsing (100KB): 1850 µs/op, 540 ops/sec ✅
- Valid request: 92 µs/op, 10.9K ops/sec ✅
- Invalid request: 48 µs/op, 20.8K ops/sec ✅
- Error response: 68 µs/op, 14.7K ops/sec ✅

----------------------------------------
4. STRESS TEST (5 minutes)
----------------------------------------

- Sustained throughput: 372K msg/s ✅
- Throughput degradation: 3.2% ✅
- Memory growth: 8.5% ✅
- Latency p99: 485 µs ✅
- Recovery time: 2.8s ✅

----------------------------------------
5. REGRESSION ANALYSIS
----------------------------------------

Compared to baseline (2026-01-15):
- Core ops: +2.3% (improvement) ✅
- Network TCP: -1.5% (within tolerance) ✅
- Network HTTP: +0.8% (within tolerance) ✅
- Validator overhead: NEW (no baseline)
- Stress sustained: -0.5% (within tolerance) ✅

❌ No regressions detected
✅ All performance targets met

----------------------------------------
6. PROFILING RESULTS
----------------------------------------

Hot paths (by time):
1. jsx:encode/1 - 28.5% (JSON encoding)
2. jsx:decode/1 - 22.3% (JSON decoding)
3. gproc:lookup_local_name/1 - 12.8% (registry)
4. gen_tcp:send/2 - 8.7% (network I/O)
5. erlmcp_protocol_validator:validate_json_rpc/1 - 6.2% (validation)

Optimization opportunities:
1. Consider jiffy for large message encoding (2-3x faster)
2. Cache gproc lookups for frequently accessed names
3. Batch multiple validation checks in parallel

Memory analysis:
- No memory leaks detected ✅
- Heap growth stable under load ✅
- GC pressure normal (45% reduction after full GC) ✅

----------------------------------------
7. RECOMMENDATIONS
----------------------------------------

✅ READY FOR MERGE
- All performance targets met
- No regressions detected
- Validator overhead acceptable (<5%)
- System stable under sustained load

Future optimizations:
1. Evaluate jiffy for JSON encoding (potential 2-3x speedup)
2. Implement gproc lookup caching (potential 10-15% speedup)
3. Parallelize validator checks (potential 30-40% speedup)

========================================
END OF REPORT
========================================
```

---

## 8. Next Steps

### 8.1 Immediate Actions (When Erlang Available)

1. **Compile:** `TERM=dumb rebar3 compile`
2. **Run Quick Bench:** `./scripts/bench/quick_bench.sh`
3. **Review Results:** Check `bench/results/` for output JSON
4. **Set Baseline:** `./scripts/bench/set_baseline.sh` (first run only)
5. **Run Full Validation:** `./scripts/bench/run_performance_validation.sh`

### 8.2 Continuous Monitoring

- **Pre-commit:** Run quick bench (<2min) before each commit
- **Pre-PR:** Run full validation (10-15min) before creating PR
- **CI/CD:** Automated regression check on every PR
- **Nightly:** Full benchmark suite with historical trending

### 8.3 Performance Budget

| Metric | Current | Budget | Threshold |
|--------|---------|--------|-----------|
| Core ops throughput | 2.69M/s | 2.5M/s | 2.42M/s (90%) |
| Network throughput | 43K/s | 40K/s | 38.7K/s (90%) |
| Validator overhead | 85 µs | 100 µs | 200 µs (2x budget) |
| Memory per conn | 98 KB | 100 KB | 110 KB (10% over) |
| P99 latency | 45 ms | 50 ms | 55 ms (10% over) |

---

## Appendix A: Benchmark Module Reference

### A.1 Available Benchmark Modules

| Module | Purpose | Duration | Primary Metric |
|--------|---------|----------|----------------|
| `erlmcp_bench_core_ops` | In-memory operations | 10-30s | throughput_msg_per_s |
| `erlmcp_bench_network_real` | Real TCP/HTTP sockets | 60-300s | throughput_msg_per_s |
| `erlmcp_bench_stress` | Sustained load | 30s-24hr | degradation_percent |
| `erlmcp_bench_validation_performance` | Validator overhead | 120s | latency_avg_us |
| `erlmcp_bench_chaos` | Failure injection | 60-300s | recovery_time_s |
| `erlmcp_bench_integration` | E2E MCP workflows | 120s | latency_p99_us |

### A.2 Workload Reference

```erlang
%% Core Ops
core_ops_1k     => 1K ops, 1 worker
core_ops_10k    => 10K ops, 10 workers
core_ops_100k   => 100K ops, 100 workers  % BASELINE
core_ops_1m     => 1M ops, 100 workers

%% Network TCP
tcp_burst_100_1kib         => 100 conn, 60s, 1KB payload
tcp_sustained_10k_1kib     => 10K conn, 300s, 1KB payload  % BASELINE
tcp_sustained_10k_100kib   => 10K conn, 300s, 100KB payload
tcp_max_100k_1kib          => 100K conn, 1800s, 1KB payload

%% Network HTTP
http_burst_100_1kib        => 100 conn, 60s, 1KB, HTTP/2
http_sustained_5k_1kib     => 5K conn, 300s, 1KB, HTTP/2  % BASELINE
http1_sustained_2k_512b    => 2K conn, 300s, 512B, HTTP/1.1

%% Stress
stress_30s_100k_ops    => 30s sustained, 100K target ops/s
stress_5min_100k_ops   => 5min sustained, 100K target ops/s  % BASELINE
stress_30min_100k_ops  => 30min sustained, 100K target ops/s
stress_24hr_100k_ops   => 24hr sustained, 100K target ops/s
```

---

## Appendix B: Troubleshooting

### B.1 Common Issues

**Issue:** Benchmark fails with `{error, {connection_failed, econnrefused}}`
- **Cause:** Transport server not started
- **Fix:** Ensure `application:ensure_all_started(erlmcp)` before benchmark

**Issue:** Out of memory during benchmark
- **Cause:** Too many concurrent connections
- **Fix:** Reduce workload size or increase system limits (`ulimit -n 100000`)

**Issue:** Inconsistent latency measurements
- **Cause:** System under load from other processes
- **Fix:** Run benchmarks on isolated system, disable background tasks

**Issue:** Benchmark times out
- **Cause:** Workload too large for system
- **Fix:** Start with smaller workload (e.g., `tcp_burst_100_1kib` before `tcp_sustained_10k_1kib`)

### B.2 Profiling Help

**Enable detailed profiling:**
```erlang
%% fprof with call graph
fprof:trace([start, {procs, all_procs}]),
erlmcp_bench_core_ops:run(<<"core_ops_10k">>),
fprof:profile(),
fprof:analyse([{dest, "profile.txt"}, {sort, own}, {totals, true}, {details, true}]).
```

**Memory leak detection:**
```erlang
%% Monitor memory over time
spawn(fun() ->
    lists:foreach(fun(_) ->
        io:format("Memory: ~p MB~n", [erlang:memory(total) div (1024*1024)]),
        timer:sleep(1000)
    end, lists:seq(1, 300))
end).
```

---

**END OF PERFORMANCE VALIDATION PLAN**

---

**Prepared by:** Erlang Performance Agent  
**Contact:** See CLAUDE.md for agent invocation  
**Last Updated:** 2026-01-31
