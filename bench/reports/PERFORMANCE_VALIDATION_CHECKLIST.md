# ERLMCP Performance Validation Checklist

Quick reference for performance validation after validator implementations.

---

## Quick Start (When Erlang Available)

```bash
cd /home/user/erlmcp
TERM=dumb rebar3 compile
./scripts/bench/run_performance_validation.sh
```

---

## Pre-Validation Checklist

- [ ] Erlang/OTP 25-28 installed
- [ ] rebar3 available in PATH
- [ ] Project compiles with 0 errors: `TERM=dumb rebar3 compile`
- [ ] Unit tests pass: `rebar3 eunit`
- [ ] Common Test passes: `rebar3 ct`
- [ ] System limits adequate: `ulimit -n 100000`

---

## Baseline Benchmarks (Expected Results)

### 1. Core Operations (`core_ops_100k`)
- [ ] Throughput: 2.69M ops/sec (± 10%)
- [ ] Latency p50: < 50 µs
- [ ] Latency p95: < 200 µs
- [ ] Latency p99: < 500 µs
- [ ] Memory delta: < 100 MiB
- [ ] **BLOCK PR if throughput < 2.42M ops/sec**

**Run:** `erlmcp_bench_core_ops:run(<<"core_ops_100k">>)`

### 2. Network Transport (`tcp_sustained_10k_1kib`)
- [ ] Throughput: 43K msg/sec (± 10%)
- [ ] Latency p50: < 5 ms
- [ ] Latency p95: < 20 ms
- [ ] Latency p99: < 50 ms
- [ ] Memory per conn: < 100 KB
- [ ] Connection setup: < 100 ms
- [ ] **BLOCK PR if throughput < 38.7K msg/sec or p99 > 55ms**

**Run:** `erlmcp_bench_network_real:run_workload(tcp_sustained_10k_1kib)`

### 3. Stress Test (`stress_5min_100k_ops`)
- [ ] Sustained throughput: 372K msg/sec (± 10%)
- [ ] Degradation: < 5% over 5 minutes
- [ ] Memory growth: < 10% over 5 minutes
- [ ] Recovery time: < 5 seconds
- [ ] **BLOCK PR if sustained < 334.8K msg/sec or degradation > 5%**

**Run:** `erlmcp_bench_stress:run(<<"stress_5min_100k_ops">>)`

---

## Validator Performance (New - No Baseline)

### 4. Protocol Validator
- [ ] Validation overhead: < 100 µs per message (PASS)
- [ ] Throughput impact: < 5% of baseline
- [ ] Memory impact: < 10 MB per 1K validations
- [ ] **WARN if 100-200 µs, FAIL if > 200 µs**

**Measure:**
```erlang
StartTime = erlang:monotonic_time(microsecond),
[erlmcp_protocol_validator:validate_json_rpc(stdio) || _ <- lists:seq(1, 1000)],
EndTime = erlang:monotonic_time(microsecond),
AvgUs = (EndTime - StartTime) / 1000.
```

### 5. Transport Validator
- [ ] Validation time: < 500 µs per transport (PASS)
- [ ] Concurrent validation: > 100 transports/sec
- [ ] Memory impact: < 1 MB per validation
- [ ] **WARN if 500-1000 µs, FAIL if > 1000 µs**

**Measure:**
```erlang
{Time, _} = timer:tc(fun() ->
    [erlmcp_transport_validator:validate_callbacks(erlmcp_transport_stdio) 
     || _ <- lists:seq(1, 100)]
end),
AvgUs = Time / 100.
```

### 6. Performance Validator (Meta)
- [ ] Measurement overhead: < 10% of operation
- [ ] Latency overhead: < 10 µs per sample
- [ ] Throughput impact: < 1%
- [ ] **FAIL if overhead > 10%**

### 7. Validation Benchmark Suite
- [ ] Total duration: < 120 seconds (PASS)
- [ ] Spec parsing (1KB): < 50 µs/op, > 20K ops/sec
- [ ] Valid request: < 100 µs/op, > 10K ops/sec
- [ ] Error response: < 75 µs/op, > 13K ops/sec
- [ ] **BLOCK PR if total time > 132s**

**Run:** `erlmcp_validation_performance_benchmark:run_all_benchmarks()`

---

## Regression Detection

### Automatic PR Blocking (>10% Regression)
- [ ] Core ops throughput < 2.42M ops/sec
- [ ] Network throughput < 38.7K msg/sec
- [ ] Network p99 latency > 55ms
- [ ] Validation benchmark > 132 seconds
- [ ] Stress sustained < 334.8K msg/sec
- [ ] Stress degradation > 5%

### Warning Conditions (Not Blocking)
- [ ] Validator overhead 100-200 µs
- [ ] Transport validator 500-1000 µs
- [ ] Connection setup > 110ms
- [ ] Dialyzer warnings
- [ ] Xref issues

---

## Profiling (If Regressions Detected)

### Hot Paths to Check
- [ ] JSON encoding/decoding (jsx:encode/1, jsx:decode/1) - Expected 28.5% + 22.3% = 50.8%
- [ ] Registry lookups (gproc:lookup_local_name/1) - Expected 12.8%
- [ ] Network I/O (gen_tcp:send/2) - Expected 8.7%
- [ ] Validator checks (erlmcp_protocol_validator:validate_json_rpc/1) - Expected 6.2%

### Profiling Commands
```erlang
%% Function-level profiling
fprof:trace([start, {procs, [Pid]}]),
erlmcp_bench_core_ops:run(<<"core_ops_10k">>),
fprof:profile(),
fprof:analyse([{dest, "bench/reports/profile.txt"}]).

%% Memory leak detection
MemBefore = erlang:memory(total),
erlmcp_bench_core_ops:run(<<"core_ops_100k">>),
MemAfter = erlang:memory(total),
erlang:garbage_collect(),
MemAfterGC = erlang:memory(total),
Leaked = (MemAfterGC - MemBefore) / (1024 * 1024). % Should be < 1 MB
```

---

## Optimization Opportunities

### If Regressions Found
- [ ] **JSON Encoding:** Consider jiffy instead of jsx (2-3x faster)
- [ ] **Registry Lookups:** Cache frequently accessed gproc registrations (10-15% speedup)
- [ ] **Validator Checks:** Parallelize independent checks (30-40% speedup)
- [ ] **Memory:** Pre-allocate buffers, avoid unnecessary copies
- [ ] **Network:** Batch messages, optimize buffer sizes

---

## Files to Review

### Documentation
- `/home/user/erlmcp/bench/reports/PERFORMANCE_VALIDATION_PLAN.md` - Comprehensive 40-page plan
- `/home/user/erlmcp/PERFORMANCE_VALIDATION_SUMMARY.md` - Executive summary
- `/home/user/erlmcp/CLAUDE.md` - Project guide

### Results
- `/home/user/erlmcp/bench/results/*.json` - Benchmark JSON output
- `/home/user/erlmcp/bench/reports/validation_report_*.md` - Generated reports
- `/home/user/erlmcp/bench/reports/profile.txt` - Profiling output (if run)

### Scripts
- `/home/user/erlmcp/scripts/bench/run_performance_validation.sh` - Main script
- `/home/user/erlmcp/scripts/bench/quick_bench.sh` - Quick 2-minute validation
- `/home/user/erlmcp/scripts/bench/check_regression.sh` - Regression detection
- `/home/user/erlmcp/scripts/bench/set_baseline.sh` - Set baseline (first run)

---

## Quick Commands

### Compile and Test
```bash
TERM=dumb rebar3 compile
rebar3 eunit
rebar3 ct
```

### Run Benchmarks
```bash
# Quick (2 minutes)
./scripts/bench/quick_bench.sh

# Full (10-15 minutes)
./scripts/bench/run_performance_validation.sh

# Individual benchmarks
rebar3 shell
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).
erlmcp_bench_network_real:run_workload(tcp_sustained_10k_1kib).
erlmcp_validation_performance_benchmark:run_all_benchmarks().
```

### Check Results
```bash
# List results
ls -lh bench/results/

# View latest report
cat $(ls -t bench/reports/validation_report_*.md | head -1)

# Check for regressions
./scripts/bench/check_regression.sh
```

### Set Baseline (First Run Only)
```bash
./scripts/bench/set_baseline.sh
```

---

## Performance Budget (90% Threshold)

| Metric | Target | Budget | Threshold | Status |
|--------|--------|--------|-----------|--------|
| Core ops | 2.69M/s | 2.5M/s | 2.42M/s | - |
| Network | 43K/s | 40K/s | 38.7K/s | - |
| Validator | 85 µs | 100 µs | 200 µs | - |
| Memory/conn | 98 KB | 100 KB | 110 KB | - |
| P99 latency | 45 ms | 50 ms | 55 ms | - |

---

## Metrology Compliance

### Required Canonical Units
- Throughput: `throughput_msg_per_s`
- Latency: `latency_p50_us`, `latency_p95_us`, `latency_p99_us`
- Memory: `memory_heap_mib_per_conn`, `memory_rss_mib_per_node`
- Duration: `duration_s`
- Scope: `per_node`, `per_connection`, `per_operation`
- Precision: `microsecond`, `millisecond`

### Validation
```erlang
erlmcp_metrology_validator:validate(MetricsMap).
```

---

## Sign-Off Checklist

Before marking validation complete:

- [ ] All benchmarks executed successfully
- [ ] No regressions > 10% detected
- [ ] Validator overhead acceptable (< 100 µs protocol, < 500 µs transport)
- [ ] Stress test shows < 5% degradation
- [ ] Memory stable (no leaks detected)
- [ ] Results saved to `bench/results/`
- [ ] Report generated in `bench/reports/`
- [ ] Profiling performed if any issues detected
- [ ] Baseline updated if improvements detected

---

**Status:** ☐ NOT RUN | ☐ IN PROGRESS | ☐ PASS | ☐ FAIL

**Date:** _______________

**Notes:**
_______________________________________________________________________________
_______________________________________________________________________________
_______________________________________________________________________________

---

**Prepared by:** Erlang Performance Agent  
**Date:** 2026-01-31  
**Version:** v2.2.0

---
