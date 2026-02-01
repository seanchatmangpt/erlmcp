# OTP 28.3.1 Benchmark Execution Guide

## Overview

This guide explains how to execute the comprehensive OTP 28.3.1 performance baselines for erlmcp and compare them against OTP 27 baselines.

## Prerequisites

### 1. Install OTP 28.3.1

The project currently uses OTP 27.3.4.2 (see `.tool-versions`). To run OTP 28 benchmarks:

```bash
# Using asdf
asdf install erlang 28.3.1
cd /home/user/erlmcp
asdf local erlang 28.3.1

# Verify installation
erl -noshell -eval "io:format('OTP: ~s~n', [erlang:system_info(otp_release)]), halt()."
# Should output: OTP: 28
```

### 2. Compile erlmcp on OTP 28

```bash
cd /home/user/erlmcp
TERM=dumb rebar3 clean
TERM=dumb rebar3 compile
```

## New Benchmarks Created

Three new benchmark modules were created specifically for OTP 28 features:

### 1. erlmcp_bench_json_otp28.erl

**Location:** `/home/user/erlmcp/bench/erlmcp_bench_json_otp28.erl`  
**Purpose:** Compare OTP 28 native `json` module vs JSX for MCP protocol messages  
**Target:** 2-3x improvement for native JSON  
**Workloads:** 1K, 10K, 100K JSON objects (small, medium, large messages)

**Run:**
```bash
cd /home/user/erlmcp/bench
erl -pa ../_build/default/lib/*/ebin -noshell -eval "erlmcp_bench_json_otp28:run_all(), halt()."

# Run single workload
erl -pa ../_build/default/lib/*/ebin -noshell -eval "erlmcp_bench_json_otp28:run(<<\"json_10k\">>), halt()."
```

**Expected Results:**
- Small messages: 2-3x speedup for native JSON
- Medium messages: 2-3x speedup for native JSON
- Large messages: 2-3x speedup for native JSON
- Memory: Lower memory allocation per operation

### 2. erlmcp_bench_process_iteration.erl

**Location:** `/home/user/erlmcp/bench/erlmcp_bench_process_iteration.erl`  
**Purpose:** Measure process iteration scalability with `processes_iterator/0`  
**Target:** O(1) memory on OTP 28 vs O(N) on OTP 27  
**Workloads:** 1K, 10K, 100K, 1M processes

**Run:**
```bash
cd /home/user/erlmcp/bench
erl -pa ../_build/default/lib/*/ebin -noshell -eval "erlmcp_bench_process_iteration:run_all(), halt()."

# Run single workload
erl -pa ../_build/default/lib/*/ebin -noshell -eval "erlmcp_bench_process_iteration:run(<<\"proc_iter_100k\">>), halt()."
```

**Expected Results:**
- Memory per process: O(1) constant for iterator vs O(N) linear for list
- >100x memory reduction for large process counts
- Iterator should show minimal memory delta

### 3. erlmcp_bench_priority_messages.erl

**Location:** `/home/user/erlmcp/bench/erlmcp_bench_priority_messages.erl`  
**Purpose:** Measure health check latency with priority messages under load  
**Target:** <1ms p99 latency for health checks  
**Workloads:** 100, 1,000, 10,000 background messages

**Run:**
```bash
cd /home/user/erlmcp/bench
erl -pa ../_build/default/lib/*/ebin -noshell -eval "erlmcp_bench_priority_messages:run_all(), halt()."

# Run single workload
erl -pa ../_build/default/lib/*/ebin -noshell -eval "erlmcp_bench_priority_messages:run(<<\"priority_msg_1000\">>), halt()."
```

**Expected Results:**
- Health check p99 latency: <1ms with priority messages
- Significant improvement over normal FIFO queue processing
- Higher background load should show greater benefit

## Existing Benchmarks to Run

### 1. Core Operations

**Module:** `erlmcp_bench_core_ops`  
**Purpose:** Registry, queue, pool, session operations  
**Run:**
```bash
cd /home/user/erlmcp/bench
erl -pa ../_build/default/lib/*/ebin -noshell -eval "erlmcp_bench_core_ops:run_all(), halt()."
```

**OTP 27 Baselines to Compare:**
- Registry: 553K msg/s → Target OTP 28: >600K msg/s
- Queue: 971K msg/s → Target OTP 28: >1M msg/s
- Pool: 149K msg/s → Target OTP 28: >160K msg/s
- Session: 242K msg/s → Target OTP 28: >260K msg/s

### 2. Network Real

**Module:** `erlmcp_bench_network_real`  
**Purpose:** TCP/HTTP real socket throughput  
**Note:** Uses separate bench modules in bench/ directory

**Run:**
```bash
cd /home/user/erlmcp/bench
# TCP benchmarks
erl -pa ../_build/default/lib/*/ebin -noshell -eval "tcp_real_bench:run_workload(sustained_25k), halt()."

# HTTP benchmarks
erl -pa ../_build/default/lib/*/ebin -noshell -eval "http_real_bench:run_workload(sse_1k), halt()."
```

### 3. Stress

**Module:** `erlmcp_bench_stress`  
**Purpose:** Sustained load testing  
**Run:**
```bash
cd /home/user/erlmcp/bench
erl -pa ../_build/default/lib/*/ebin -noshell -eval "erlmcp_bench_stress:run(<<\"sustained_30s\">>), halt()."
```

**OTP 27 Baseline:** 372K msg/s sustained → Target OTP 28: >400K msg/s

### 4. Chaos

**Module:** `erlmcp_bench_chaos`  
**Purpose:** Failure injection and recovery  
**Run:**
```bash
cd /home/user/erlmcp/bench
erl -pa ../_build/default/lib/*/ebin -noshell -eval "erlmcp_bench_chaos:run(<<\"message_flood\">>), halt()."
```

**Target:** <5s recovery for all failure scenarios

### 5. Integration

**Module:** `erlmcp_bench_integration`  
**Purpose:** End-to-end MCP protocol workflows  
**Run:**
```bash
cd /home/user/erlmcp/bench
erl -pa ../_build/default/lib/*/ebin -noshell -eval "erlmcp_bench_integration:benchmark_all(), halt()."
```

## Full Benchmark Suite

Run all benchmarks using the automated script:

```bash
cd /home/user/erlmcp
./scripts/bench/run_all_benchmarks.sh standard
```

**Modes:**
- `quick`: 1 workload per benchmark (~5 min)
- `standard`: 3 workloads per benchmark (~30 min)
- `full`: All workloads (~2 hours)
- `ci`: Quick mode + strict validation

**Results Location:**
```
/home/user/erlmcp/bench/results/[timestamp]/
```

## Filling in the Baseline Document

After running all benchmarks, update `/home/user/erlmcp/docs/benchmarks/OTP_28_BASELINES.md`:

1. **Environment Section:** Fill in actual system details (cores, memory, hostname)
2. **Results Tables:** Replace all [TBD] with actual measurements from JSON files
3. **Regression Analysis:** Calculate deltas vs OTP 27 baselines
4. **Status Fields:** Mark as PASS/FAIL based on targets
5. **Recommendations:** Update based on actual findings

### Example: Updating a Result

From JSON result file:
```json
{
  "workload_id": "core_ops_10k",
  "throughput_msg_per_s": 650000,
  "latency_p50_us": 1.5,
  "latency_p95_us": 3.2,
  "latency_p99_us": 5.1
}
```

Update table:
```markdown
| core_ops_10k | 40,000 | 650,000 | 1.5 | 3.2 | 5.1 | 0.5 |
```

## Regression Detection

Check for regressions (>2% performance drop):

```bash
cd /home/user/erlmcp/bench/results

# Compare OTP 28 results against OTP 27 baseline
# Example: Registry operations
# OTP 27: 553K msg/s
# OTP 28: [your result]
# Delta: ((OTP28 - OTP27) / OTP27) * 100

# If delta < -2%, flag as regression
```

## Memory Efficiency Test

Measure idle connection memory:

```bash
# Start erlmcp server with 1K idle connections
# Measure memory with hibernation
# Target: <45KB per idle connection (OTP 27: ~50KB)
```

## Validation

Ensure all results are metrology-compliant:

```bash
cd /home/user/erlmcp
erl -pa _build/default/lib/*/ebin -noshell -eval "
    erlmcp_metrology_validator:validate_file(\"bench/results/[timestamp]/core_ops_10k.json\"),
    halt()."
```

## Expected Outcomes

### Performance Improvements

1. **JSON Encoding:** 2-3x faster with native `json` module
2. **Process Iteration:** O(1) memory vs O(N) with `processes_iterator/0`
3. **Priority Messages:** <1ms p99 latency for health checks
4. **Core Operations:** 5-10% general improvement across the board

### No Regressions

All OTP 27 baseline metrics should be maintained or improved. If any regression >2% is detected:
1. Document in "Regressions Detected" section
2. Investigate root cause
3. Propose mitigation strategy

## Troubleshooting

### OTP 28 Not Available

If OTP 28.3.1 is not yet released or not available via asdf:
- The benchmarks will gracefully handle missing features
- Native JSON and priority message benchmarks will skip OTP 28 comparisons
- Process iterator will fall back to baseline measurements only

### Compilation Errors

```bash
# Clean build
TERM=dumb rebar3 clean
TERM=dumb rebar3 compile

# Check for syntax errors in new benchmark modules
TERM=dumb rebar3 eunit --module=erlmcp_bench_json_otp28
```

### Missing Dependencies

Ensure all dependencies are compiled:
```bash
TERM=dumb rebar3 compile
```

Required deps: jsx, jesse, gproc, gun, ranch, poolboy, cowboy

## Next Steps

1. Install OTP 28.3.1
2. Compile erlmcp
3. Run NEW benchmarks (json, process_iteration, priority_messages)
4. Run EXISTING benchmarks (core_ops, network, stress, chaos, integration)
5. Fill in OTP_28_BASELINES.md with actual results
6. Analyze regressions
7. Update recommendations
8. Commit results and documentation

## Files Created

1. `/home/user/erlmcp/bench/erlmcp_bench_json_otp28.erl` (500 lines)
2. `/home/user/erlmcp/bench/erlmcp_bench_process_iteration.erl` (449 lines)
3. `/home/user/erlmcp/bench/erlmcp_bench_priority_messages.erl` (470 lines)
4. `/home/user/erlmcp/docs/benchmarks/OTP_28_BASELINES.md` (490 lines)
5. `/home/user/erlmcp/BENCHMARK_EXECUTION_GUIDE.md` (this file)

**Total:** 1,909 lines of new benchmark code + 490 lines of documentation

---

**Status:** Ready for execution once OTP 28.3.1 is installed
