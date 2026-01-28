# erlmcp Benchmarks - Canonical Reference

## Overview

The erlmcp benchmark suite measures performance, reliability, and scalability of the Erlang/OTP MCP client and server under real-world conditions. Version 1.5.0 consolidates 15+ legacy benchmarks into 5 production-grade modules.

### What's Measured

- **Core Operations**: Registry, message queues, connection pools, session management (in-memory)
- **Network I/O**: TCP and HTTP transport with real sockets (not mocked)
- **Sustained Load**: Time-series degradation detection, memory leaks, throughput stability
- **Failure Scenarios**: 11 chaos engineering patterns (connection failures, resource exhaustion, timeouts)
- **MCP Protocol**: End-to-end tool invocation, streaming, error handling

### Why Consolidated?

**Before v1.5.0**: 15+ modules spread across `bench/`, `test/`, `swarm/` with inconsistent output formats and partial metrology compliance.

**After v1.5.0**: 5 unified modules with:
- Canonical metric units (msg/sec, microseconds, MiB)
- Consistent JSON output format
- Metrology validation (no ambiguous units)
- Single entry point: `erlmcp_bench_*.erl:run_all()` or `run/1`

---

## The 5 Consolidated Benchmarks

### 1. **Core Operations** (`erlmcp_bench_core_ops.erl`)

In-memory micro-benchmarks for message handling primitives.

**What**: Registry lookups, queue operations, pool allocation, session management
**Workloads**: 1K, 10K, 100K, 1M operations with 1-100 concurrent workers
**Metrics**: Throughput (msg/sec), latency percentiles (P50, P95, P99, max)
**Output**: `bench/results/core_ops_*.json`

**Run**:
```bash
rebar3 shell
erlmcp_bench_core_ops:run_all().           # All workloads
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).  # Single workload
```

**Use Case**: Detect regressions in message routing, session tracking, or pool management.

---

### 2. **Network Real** (`erlmcp_bench_network_real.erl`)

Real-socket transport benchmarks for TCP and HTTP/SSE.

**What**: Actual TCP connections (gen_tcp), HTTP/2 with gun client, SSE with cowboy server
**Workloads**:
- TCP: 100→10K connections, 60s→300s duration, 1 KiB→100 KiB payloads
- HTTP/2: 100→5K connections, 60s duration
- HTTP/1.1: 2K connections, 60s duration

**Metrics**:
- Throughput: messages/second (NOT ambiguous "requests/sec")
- Latency: P50, P95, P99 in microseconds
- Memory per socket: MiB (heap only, not RSS)
- Packet efficiency: real bytes sent vs. logical messages

**Output**: `bench/results/network_real_*.json`

**Run**:
```bash
erlmcp_bench_network_real:run_all().                        # All transports
erlmcp_bench_network_real:run_workload(tcp_burst_100_1kib). # TCP only
erlmcp_bench_network_real:run_workload(http_sustained_5k_1kib).  # HTTP/2
```

**Use Case**: Validate transport layer performance, detect socket leaks, confirm payload encoding efficiency.

---

### 3. **Stress/Sustained Load** (`erlmcp_bench_stress.erl`)

Time-series performance under continuous operation for hours.

**What**: Sustained message load with degradation detection
**Workloads**:
- `stress_30s_100k_ops` - Quick validation (30 seconds)
- `stress_5min_100k_ops` - Standard (5 minutes)
- `stress_1hr_50k_ops` - Endurance (1 hour)
- `stress_24hr_10k_ops` - Production simulation (24 hours)

**Metrics**:
- Throughput trend (5s samples): ops/sec, standard deviation
- Latency trend: P99 average and peak over time
- Memory trend: heap growth detection, leak analysis via linear regression
- Degradation flags: throughput decline >5%/min, memory growth >1 MiB/min, latency spike >10%/min

**Output**: `bench/results/stress_*.json` with embedded time-series array

**Run**:
```bash
erlmcp_bench_stress:run_all().                         # All durations
erlmcp_bench_stress:run_workload(<<"stress_30s_100k_ops">>). # 30-second quick test
erlmcp_bench_stress:run_workload(<<"stress_5min_100k_ops">>).  # 5-minute standard
```

**Use Case**: Detect memory leaks, GC pressure, or scheduler starvation under production load.

---

### 4. **Chaos Engineering** (`erlmcp_bench_chaos.erl`)

11 failure scenarios with bounded refusal validation.

**What**: Simulated failures to verify recovery
**Scenarios**:
1. Connection reset while active
2. Sudden socket closure (no FIN)
3. Timeout during message exchange
4. Buffer exhaustion (backpressure)
5. Process crash with monitor cleanup
6. Message loss detection
7. Out-of-memory (resource limit)
8. Rapid connect/disconnect cycles
9. Partial packet delivery
10. DNS resolution failure (HTTP only)
11. TLS handshake timeout (if enabled)

**Metrics**:
- Recovery time (seconds)
- Message loss count
- Resource cleanup verification (no orphaned processes)
- Bounded refusal: system remains operational after failure

**Output**: `bench/results/chaos_*.json`

**Run**:
```bash
erlmcp_bench_chaos:run_all_scenarios().                    # All 11 scenarios
erlmcp_bench_chaos:run_scenario(<<"chaos_connection_reset">>).  # Single scenario
```

**Use Case**: Validate supervisor recovery, connection pooling resilience, and graceful degradation.

---

### 5. **MCP Integration** (`erlmcp_bench_integration.erl`)

End-to-end protocol workflows with real client-server communication.

**What**: MCP protocol compliance and latency
**Workflows**:
1. Tool invocation (simple request-response)
2. Streaming response (large payload, chunked)
3. Error propagation (malformed input)
4. Resource management (concurrent tools)
5. Prompt system usage

**Metrics**:
- End-to-end latency (client call → server response)
- Protocol compliance (JSON-RPC structure, required fields)
- Throughput under concurrent client load
- Protocol violation detection

**Output**: `bench/results/integration_*.json`

**Run**:
```bash
erlmcp_bench_integration:benchmark_all().                    # All 5 workflows
erlmcp_bench_integration:benchmark_workflow(tool_invocation).  # Single workflow
```

**Use Case**: Validate MCP protocol implementation, measure real e2e latency, confirm error handling.

---

## How to Run

### Quick Benchmark (< 2 minutes)

```bash
cd /Users/sac/erlmcp
make benchmark-quick
```

This runs small workloads from each module:
- Core ops: 1K and 10K operations
- Network: 10 connections, 10s duration
- Stress: 30s run
- Chaos: connection reset only
- Integration: single workflow

**Output**: Console + JSON files in `bench/results/`

### Full Benchmark Suite (10-15 minutes)

```bash
make benchmark-full
```

This runs all workloads with real durations:
- Core ops: all scales (1K→1M)
- Network: all transports (TCP burst, HTTP sustained)
- Stress: 5-minute sustained run
- Chaos: all 11 scenarios
- Integration: all 5 workflows

**Output**: Same as quick + comprehensive JSON archive

### Manual Invocation

```bash
# Compile first
rebar3 compile

# Start Erlang shell
rebar3 shell

# Run individual benchmarks
erlmcp_bench_core_ops:run_all().
erlmcp_bench_network_real:run_workload(tcp_burst_100_1kib).
erlmcp_bench_stress:run_workload(<<"stress_5min_100k_ops">>).
erlmcp_bench_chaos:run_all_scenarios().
erlmcp_bench_integration:benchmark_all().
```

---

## Results Format

All benchmarks output metrology-compliant JSON to `bench/results/<benchmark>_<workload_id>_<timestamp>.json`.

### Example Core Ops Result

```json
{
  "workload_id": "core_ops_100k",
  "benchmark": "core_ops",
  "transport": "memory",
  "duration_s": 2.35,
  "operations": 100000,
  "workers": 100,
  "throughput_msg_per_s": 42553,
  "latency_p50_us": 847,
  "latency_p95_us": 3542,
  "latency_p99_us": 6281,
  "latency_max_us": 28340,
  "memory_heap_mib_per_conn": 0.12,
  "memory_rss_mib_per_node": 85.4,
  "precision": "microsecond",
  "scope": "per_connection_heap",
  "timestamp": "2026-01-27T18:29:35Z",
  "erlang_version": "25.2",
  "hostname": "macbook.local"
}
```

### Example Network Real Result

```json
{
  "workload_id": "tcp_burst_100_1kib",
  "benchmark": "network_real",
  "transport": "tcp",
  "connections": 100,
  "duration_s": 60,
  "sockets_open": 100,
  "messages_sent": 287342,
  "messages_received": 287342,
  "throughput_msg_per_s": 4789,
  "latency_p50_us": 1243,
  "latency_p95_us": 4856,
  "latency_p99_us": 8234,
  "memory_heap_mib_per_conn": 0.42,
  "packet_efficiency": 0.95,
  "precision": "microsecond",
  "scope": "per_connection_heap",
  "timestamp": "2026-01-27T18:33:15Z"
}
```

### Metric Definitions

| Metric | Unit | Scope | Definition |
|--------|------|-------|-----------|
| `throughput_msg_per_s` | messages/sec | workload | Logical messages processed per second |
| `latency_p50_us` | microseconds | request | 50th percentile latency (median) |
| `latency_p95_us` | microseconds | request | 95th percentile latency (SLA critical) |
| `latency_p99_us` | microseconds | request | 99th percentile latency (tail behavior) |
| `latency_max_us` | microseconds | request | Worst-case observed latency |
| `memory_heap_mib_per_conn` | MiB | per connection | Erlang heap memory per active connection |
| `memory_rss_mib_per_node` | MiB | per node | Total RSS (resident set size) for whole node |
| `sockets_open` | count | workload | Number of open TCP/HTTP sockets |
| `packet_efficiency` | ratio | transport | (logical_bytes_sent / real_bytes_sent) |

---

## Interpreting Results

### Throughput Analysis

**Good**: Consistent ±5% across runs
**Warning**: Variance >10%, may indicate GC pressure or scheduler contention
**Action**: Compare against baseline, profile with `observer`, check for lock contention

### Latency Analysis

**P50**: Typical performance (50% of requests faster)
**P95**: SLA critical (95% must meet this threshold)
**P99**: Tail behavior (detects outliers)
**Max**: Worst-case (usually GC pause or scheduling hiccup)

**Good**: P95 < 10ms for core ops, < 50ms for network
**Warning**: P99 > 5x P95 (high variance), investigate GC frequency
**Action**: Profile CPU, check heap size, monitor GC logs

### Memory Analysis

**Memory per connection**: Should remain constant as load increases
**Linear growth over time**: Indicates memory leak (linear regression detector)
**Spike at start**: Normal (pool initialization), should plateau
**Growth >1 MiB/min**: Degradation flag, investigate

**Action**: Compare before/after change, run stress test to 1+ hour

---

## Regression Detection

### Manual Comparison

```bash
# Establish baseline
git checkout main
make benchmark-quick > baseline.txt 2>&1

# Make changes
# ... edit code ...

# Compare
make benchmark-quick > after.txt 2>&1
diff baseline.txt after.txt
```

### Automated (CI/CD)

The project CI pipeline (GitHub Actions) runs benchmarks on:
- Every commit to `main` branch
- Manual trigger option
- Pull requests (optional)

Regressions flagged if:
- Throughput decreases >10%
- P95 latency increases >10%
- Memory leak detected (linear growth >1 MiB/min)

---

## Best Practices

### Running Benchmarks

1. **Consistent environment**: Close other apps, no competing workloads
2. **Multiple runs**: Run 2-3 times, ignore first run (warmup), average results
3. **Isolated changes**: One code change per benchmark run
4. **Document baseline**: Note Erlang version, hardware, OS state
5. **Track results**: Save JSON outputs for trending

### Interpreting Numbers

- **Always compare to baseline**, not absolute values (varies by hardware)
- **Variance >10%**: Not a regression, just normal variance
- **Consistent decline >10%**: Investigate code changes
- **Spike in P99**: Check for GC pauses (CPU profile) or scheduling (trace logs)
- **Memory growth >1 MiB/min**: High priority, likely leak

### Performance Tuning

1. Profile with `rebar3 prof` (CPU)
2. Check heap size: `erlang:system_info(max_heap_size)`
3. Monitor GC: `erlang:statistics(garbage_collection)`
4. Trace scheduling: `erlang:trace_pattern({erlang, system_info, 1}, [])`
5. Use `observer` to visualize process tree and memory

---

## References

### Documentation

- [Workload Definitions](./workloads.md) - Complete workload catalog with parameters
- [Metrology Glossary](./metrology.md) - Canonical metric definitions and units
- [Quick Start](./quickstart.md) - Copy-paste commands to get started

### Related Docs

- [erlmcp Architecture](../architecture.md) - System design and OTP patterns
- [OTP Patterns](../otp-patterns.md) - gen_server, supervisor best practices
- [Erlang Performance Guide](https://erlang.org/doc/efficiency_guide/) - Official tuning guide

---

## Troubleshooting

### "benchmark-quick" target not found

```bash
# Ensure you're in the project root
cd /Users/sac/erlmcp

# Check Makefile exists
ls -la Makefile
```

### Results not appearing in bench/results/

```bash
# Verify directory exists
mkdir -p bench/results

# Check permissions
ls -ld bench/results/

# Run benchmark with verbose output
rebar3 shell
erlmcp_bench_core_ops:run(<<"core_ops_1k">>).
```

### "Module not found" error

```bash
# Ensure code is compiled
rebar3 compile

# Verify beam files exist
ls -la ebin/erlmcp_bench_*.beam

# Start fresh shell
rebar3 clean
rebar3 compile
rebar3 shell
```

---

## Support & Contributing

For issues, regressions, or feature requests:
1. Check [GitHub Issues](https://github.com/erlang-mcp/erlmcp/issues)
2. Run relevant benchmark to confirm regression
3. Provide JSON output from `bench/results/`
4. Note Erlang/OTP version and hardware details

---

**Version**: 1.5.0
**Last Updated**: 2026-01-27
**Status**: Production-Ready
