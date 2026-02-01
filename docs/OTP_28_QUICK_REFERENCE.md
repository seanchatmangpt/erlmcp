# OTP 28.3.1 Benchmark Quick Reference

## Installation

```bash
# Install OTP 28.3.1
asdf install erlang 28.3.1
cd /home/user/erlmcp
asdf local erlang 28.3.1

# Compile
TERM=dumb rebar3 clean
TERM=dumb rebar3 compile
```

## NEW Benchmarks (OTP 28 Features)

### JSON Performance (Native JSON vs JSX)
```bash
cd /home/user/erlmcp/bench
erl -pa ../_build/default/lib/*/ebin -noshell -eval "erlmcp_bench_json_otp28:run_all(), halt()."
```
**Target:** 2-3x improvement  
**Workloads:** json_1k, json_10k, json_100k

### Process Iteration (O(1) Memory)
```bash
erl -pa ../_build/default/lib/*/ebin -noshell -eval "erlmcp_bench_process_iteration:run_all(), halt()."
```
**Target:** O(1) memory vs O(N)  
**Workloads:** proc_iter_1k, proc_iter_10k, proc_iter_100k, proc_iter_1m

### Priority Messages (<1ms p99)
```bash
erl -pa ../_build/default/lib/*/ebin -noshell -eval "erlmcp_bench_priority_messages:run_all(), halt()."
```
**Target:** <1ms p99 latency  
**Workloads:** priority_msg_100, priority_msg_1000, priority_msg_10000

## Existing Benchmarks

### Core Operations
```bash
erl -pa ../_build/default/lib/*/ebin -noshell -eval "erlmcp_bench_core_ops:run_all(), halt()."
```
**Targets:** Registry >600K, Queue >1M, Pool >160K, Session >260K msg/s

### Full Suite
```bash
cd /home/user/erlmcp
./scripts/bench/run_all_benchmarks.sh standard
```
**Duration:** ~30 minutes

## Results

**Location:** `/home/user/erlmcp/bench/results/[timestamp]/`  
**Format:** JSON with metrology-compliant fields

## Documentation

**Baseline Template:** `/home/user/erlmcp/docs/benchmarks/OTP_28_BASELINES.md`  
**Execution Guide:** `/home/user/erlmcp/BENCHMARK_EXECUTION_GUIDE.md`  
**Summary:** `/home/user/erlmcp/OTP_28_BENCHMARK_SUMMARY.md`

## OTP 27 Baselines (CLAUDE.md)

- Registry: 553K msg/s
- Queue: 971K msg/s
- Pool: 149K msg/s
- Session: 242K msg/s
- Network I/O: 43K msg/s
- Sustained: 372K msg/s

## Regression Threshold

**±2%** - Flag any performance drop >2%
