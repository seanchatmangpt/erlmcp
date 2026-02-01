# erlmcp_core Benchmarks

This directory contains the 5 canonical benchmark modules for erlmcp, consolidated from v1.5.0.

## Benchmark Modules

### 1. Core Operations (`erlmcp_bench_core_ops.erl`)
**Purpose:** In-memory operation benchmarks (registry, queue, pool, session)  
**Baseline:** 2.69M ops/sec on M1 Pro (2026-01)  
**Workloads:**
- `core_ops_1k` - 1K operations (quick validation)
- `core_ops_10k` - 10K operations
- `core_ops_100k` - 100K operations (standard baseline)
- `core_ops_1m` - 1M operations (stress test)

**Run:**
```erlang
rebar3 shell
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).
erlmcp_bench_core_ops:run_all().
```

**Expected Metrics:**
- Throughput: 2-3M msg/sec (in-memory ops)
- Latency p99: <100 us
- Memory: <10 MiB delta per 100K ops

### 2. Network Real (`erlmcp_bench_network_real.erl`)
**Purpose:** TCP/HTTP transport with real sockets (ranch, gun, cowboy)  
**Baseline:** TCP 100 conn @ 50K msg/sec, HTTP 5K conn @ 50K msg/sec  
**Workloads:**
- TCP: `tcp_burst_100_1kib`, `tcp_sustained_10k_1kib`, `tcp_max_100k_1kib`
- HTTP: `http_burst_100_1kib`, `http_sustained_5k_1kib`, `http1_sustained_2k_512b`

**Run:**
```erlang
erlmcp_bench_network_real:run_workload(tcp_burst_100_1kib).
erlmcp_bench_network_real:run_all().
```

**Expected Metrics:**
- TCP throughput: 40-50K msg/sec (real sockets, 1KB payloads)
- HTTP throughput: 5-10K req/sec (HTTP/2)
- Latency p99: 100-500 us (TCP), 1-5 ms (HTTP)

### 3. Stress/Sustained Load (`erlmcp_bench_stress.erl`)
**Purpose:** Time-series performance under sustained load (30s to 24hr)  
**Baseline:** 372K msg/sec sustained over 30s  
**Workloads:**
- `stress_30s_100k_ops` - Quick validation (30 seconds)
- `stress_5min_100k_ops` - Standard test (5 minutes)
- `stress_1hr_50k_ops` - Endurance (1 hour)
- `stress_24hr_10k_ops` - Production simulation (24 hours)

**Run:**
```erlang
erlmcp_bench_stress:run_workload(#{id => <<"stress_5min_100k_ops">>, duration_s => 300, target_ops_per_s => 100000, workers => 100}).
```

**Expected Metrics:**
- Throughput degradation: <5% per minute
- Memory leak detection: <1 MiB/minute growth
- Recovery time: <5s after failure injection

### 4. Chaos Engineering (`erlmcp_bench_chaos.erl`)
**Purpose:** Adversarial testing with failure injection and bounded refusal validation  
**Baseline:** 11 scenarios, 100% pass rate, <1s detection, <5s recovery  
**Scenarios:**
- Process crashes, network partitions, memory exhaustion
- Message floods, invalid payloads, connection leaks
- Slow consumers, supervisor cascades, disk full, CPU saturation, large payloads

**Run:**
```erlang
erlmcp_bench_chaos:run_scenario(<<"chaos_memory_exhaustion">>).
erlmcp_bench_chaos:run_all_scenarios().
```

**Expected Metrics:**
- Detection time: <1s (bounded refusal)
- Recovery time: <5s (automatic)
- Refusal codes: Correct code from plans/*.json
- Data loss: 0 events
- Cascading failures: 0

### 5. Integration/E2E (`erlmcp_bench_integration.erl`)
**Purpose:** End-to-end MCP protocol workflows (initialize, tools, prompts, resources)  
**Baseline:** 100-200 workflows/sec, p99 <50ms  
**Workloads:**
- `mcp_basic_initialize` - Init + shutdown
- `mcp_tool_sequence` - Multiple tool calls
- `mcp_prompts_workflow` - List/get prompts
- `mcp_resources_workflow` - List/read resources
- `mcp_complete_workflow` - All capabilities

**Run:**
```erlang
erlmcp_bench_integration:benchmark_all().
```

**Expected Metrics:**
- E2E latency p99: 30-50 ms
- Throughput: 100-200 workflows/sec
- Protocol overhead: 5-10%
- Success rate: 100%

## Metrology Compliance

All benchmarks output canonical units per v1.5.0 metrology standards:
- `throughput_msg_per_s` (NOT "req/s" - ambiguous)
- `latency_p50_us`, `latency_p95_us`, `latency_p99_us` (raw microseconds)
- `memory_heap_mib_per_conn` (scope: per_connection_heap)
- `memory_rss_mib_per_node` (scope: per_node_total)
- `scope` field: `per_node`, `per_connection`, `per_cluster`
- `precision` field: `microsecond`, `millisecond`

See `docs/metrology/METRICS_GLOSSARY.md` for full glossary.

## Results Storage

Benchmark results are written to `bench/results/` in JSON format:
```
bench/results/
  core_ops_core_ops_100k_1738012345.json
  network_real_tcp_burst_100_1kib_1738012346.json
  stress_stress_5min_100k_ops_1738012347.json
  chaos_chaos_memory_exhaustion_1738012348.json
  integration_1738012349.json
```

## Regression Detection

Regression thresholds (fail if exceeded):
- Throughput: >10% decrease from baseline
- Latency p99: >20% increase from baseline
- Memory: >50% increase from baseline
- Recovery time: >2x increase from baseline

**Baseline values (v1.5.0, M1 Pro, 2026-01):**
- Core ops: 2.69M msg/sec
- Network TCP: 43K msg/sec (1KB payloads, real sockets)
- Sustained: 372K msg/sec (60M ops / 30s)
- Chaos recovery: <5s average
- Integration e2e: 30ms p99

## Quick Start

```bash
# From project root
cd apps/erlmcp_core
rebar3 shell

# Run quick baseline (< 2 minutes)
erlmcp_bench_core_ops:run(<<"core_ops_1k">>).

# Run standard baseline (2-5 minutes)
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).
erlmcp_bench_stress:run_workload(#{id => <<"stress_30s_100k_ops">>, duration_s => 30, target_ops_per_s => 100000, workers => 100}).

# Run full suite (15-30 minutes)
make benchmark-quick  # From project root
```

## Dependencies

Benchmarks require:
- `jsx` - JSON encoding/decoding
- `ranch` - TCP transport (network_real)
- `gun` - HTTP client (network_real)
- `cowboy` - HTTP server (network_real)

All dependencies are listed in `rebar.config`.

## Architecture Notes

**Process-per-benchmark:** Each benchmark runs in isolated process with timeout and cleanup.  
**No mocking:** Chicago School TDD - uses real processes, real sockets, real supervision.  
**Metrology first:** All metrics validated by `erlmcp_metrology_validator` before write.  
**Bounded refusal:** Chaos scenarios validate preventive refusal codes (not reactive crashes).

## Migration from v1.5.0

These 5 modules consolidate 15+ legacy benchmark modules:
- `erlmcp_bench_core_ops.erl` - Replaces 4 registry/queue/pool/session modules
- `erlmcp_bench_network_real.erl` - Replaces tcp_real_bench + http_real_bench
- `erlmcp_bench_stress.erl` - Replaces sustained_load_bench
- `erlmcp_bench_chaos.erl` - Replaces chaos/adversarial modules
- `erlmcp_bench_integration.erl` - Replaces mcp_e2e_bench

All legacy modules have been deprecated and removed.
