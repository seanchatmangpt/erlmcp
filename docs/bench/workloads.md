# Benchmark Workloads Registry

Canonical reference of all `workload_id` strings, parameters, and expected outputs.

---

## Core Operations (erlmcp_bench_core_ops)

### Registry Workloads

| workload_id | ops_count | pool_size | Expected Throughput | Expected Latency p99 |
|-------------|-----------|-----------|--------------------|--------------------|
| `core_ops_1k` | 1,000 | 1 | 450K msg/s | 2.2 us |
| `core_ops_10k` | 10,000 | 1 | 500K msg/s | 2.0 us |
| `core_ops_100k` | 100,000 | 1 | 553K msg/s | 1.8 us |
| `core_ops_1m` | 1,000,000 | 1 | 520K msg/s | 1.9 us |

### Queue Workloads

| workload_id | queue_depth | msg_size | Expected Throughput | Notes |
|-------------|------------|----------|--------------------|----|
| `queue_10k` | 10,000 | 512 bytes | 850K msg/s | FIFO queue push/pop |
| `queue_100k` | 100,000 | 512 bytes | 971K msg/s | Peak performance |
| `queue_1m` | 1,000,000 | 512 bytes | 920K msg/s | Slightly slower at scale |

### Pool Workloads

| workload_id | pool_size | workers | ops_count | Expected Throughput | Notes |
|-------------|-----------|---------|-----------|--------------------|----|
| `pool_10` | 10 | 10 | 10,000 | 85K msg/s | Small pool |
| `pool_50` | 50 | 50 | 50,000 | 149K msg/s | Optimal size |
| `pool_100` | 100 | 100 | 100,000 | 145K msg/s | Contention starts |

### Session Workloads

| workload_id | session_count | msg_per_session | Expected Throughput | Notes |
|-------------|---------------|-----------------|--------------------|----|
| `session_100` | 100 | 1,000 | 185K msg/s | Per-session state tracking |
| `session_1k` | 1,000 | 100 | 242K msg/s | Peak session throughput |
| `session_10k` | 10,000 | 10 | 210K msg/s | Map lookup slowdown |

---

## Network I/O (erlmcp_bench_network_real)

### TCP Workloads

| workload_id | connections | msg_per_conn | transport | Expected Throughput | Notes |
|-------------|------------|-------------|-----------|--------------------|----|
| `tcp_100_connections` | 100 | 100 | ranch TCP | 35K msg/s | Baseline |
| `tcp_1k_connections` | 1,000 | 10 | ranch TCP | 42K msg/s | Good scaling |
| `tcp_10k_connections` | 10,000 | 1 | ranch TCP | 38K msg/s | Bottleneck: per-msg overhead |
| `tcp_sustained_10k` | 10,000 (pool) | 1,000 | ranch TCP | 40K msg/s | Sustained 60s |

### HTTP Workloads

| workload_id | connections | msg_per_conn | transport | Expected Throughput | Notes |
|-------------|------------|-------------|-----------|--------------------|----|
| `http_100_connections` | 100 | 100 | gun HTTP/1.1 | 22K msg/s | TLS + headers |
| `http_1k_connections` | 1,000 | 10 | gun HTTP/1.1 | 28K msg/s | Better batching |
| `http_5k_connections` | 5,000 | 1 | gun HTTP/1.1 | 35K msg/s | HTTP/2 multiplexing |

### Stdio Workloads

| workload_id | msg_count | msg_size | line_format | Expected Throughput | Notes |
|-------------|-----------|----------|-------------|--------------------|----|
| `stdio_json_100` | 100 | 512 bytes | JSON-RPC 2.0 | 8.5K msg/s | JSON parsing overhead |
| `stdio_binary_1k` | 1,000 | 256 bytes | binary msgpack | 12K msg/s | Faster parsing |

---

## Stress Testing (erlmcp_bench_stress)

| workload_id | duration | ops_target | monitoring | Expected Result |
|-------------|----------|-----------|-----------|-----------------|
| `stress_30s` | 30 seconds | 10M ops | Memory baseline | 372K msg/s, flat memory |
| `stress_5min` | 5 minutes | 100M ops | Degradation % | <5% degradation |
| `stress_1h` | 1 hour | 1.3B ops | Memory leaks | No memory growth >1% |
| `stress_24h` | 24 hours | 31B ops | Crash recovery | Supervised via observer |

### Stress Monitoring Output

```
Throughput:
  Current: 372K msg/s
  Avg 5min: 371K msg/s
  Avg 10min: 370K msg/s
  Degradation: -0.5% (PASS)

Memory:
  RSS baseline: 123 MiB
  RSS current: 124 MiB
  Heap/conn: 2.34 KiB
  Growth rate: +0.8% per minute (PASS)

Recovery:
  Crash at 5min 23s → Restart
  Recovery time: 2.1s
  Lost messages: 0
```

---

## Chaos (erlmcp_bench_chaos)

| workload_id | failure_type | duration | expected_recovery | result_check |
|-------------|--------------|----------|-------------------|--------------|
| `chaos_memory_exhaustion` | Force GC, reduce heap | 30s | Recovery in <5s | No supervisor crash |
| `chaos_connection_refused` | Refuse all TCP connects | 10s | Reconnect success | Queue backoff |
| `chaos_packet_loss` | Drop 20% of packets | 20s | Timeout recovery | Retry mechanism |
| `chaos_latency_spike` | Add >1s latency | 30s | No timeouts | Client resilience |
| `chaos_supervisor_crash` | Kill supervisor | 10s | Auto-restart | Tree recovery <2s |
| `chaos_worker_cascade` | Crash 5 workers sequentially | 20s | No cascading failure | Isolation works |

### Chaos Validation

Each scenario tracks:
- ✅ **Bounded refusal**: Failures must be finite (not infinite)
- ✅ **Recovery**: System recovers within timeout
- ✅ **No cascades**: Crash doesn't propagate
- ✅ **Message loss**: <1% acceptable under stress

---

## Integration (erlmcp_bench_integration)

| workload_id | workflow | tools_called | expected_latency_p99 | Notes |
|-------------|----------|-------------|----------------------|-------|
| `mcp_init` | Client init + server ready | 0 | 45ms | Protocol overhead |
| `mcp_simple_tool` | Initialize → call echo | 1 | 150ms | JSON-RPC roundtrip |
| `mcp_complex_tool` | Initialize → call calculator | 1 | 180ms | Computation overhead |
| `mcp_resource_list` | Initialize → list resources | 1 | 120ms | Generator iteration |
| `mcp_tool_sequence` | Full init → 3 tools → cleanup | 3 | 500ms | Complete MCP workflow |

---

## Metrology Standards

All benchmarks output metrics in canonical format:

```erlang
#{
  workload_id => <<"core_ops_100k">>,
  throughput_msg_per_s => 2690000,
  latency_p50_us => 0.37,
  latency_p95_us => 0.89,
  latency_p99_us => 1.20,
  memory_heap_mib_per_conn => 2.34,
  memory_rss_mib_per_node => 456,
  transport => tcp,
  duration_s => 5,
  scope => per_connection_heap,
  precision => microseconds,
  timestamp => <<"2026-01-27T18:45:30Z">>,
  status => pass
}
```

See [metrology.md](metrology.md) for full standards.

---

## Running Specific Workloads

```bash
cd /Users/sac/erlmcp && rebar3 shell
```

```erlang
%% Run single workload
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).

%% Run multiple
erlmcp_bench_network_real:run(<<"tcp_100_connections">>).
erlmcp_bench_network_real:run(<<"http_1k_connections">>).

%% Run all stress tests
erlmcp_bench_stress:run(<<"stress_30s">>).
erlmcp_bench_stress:run(<<"stress_5min">>).
erlmcp_bench_stress:run(<<"stress_1h">>).
```

---

**Navigation**: [Back to Benchmark Index](INDEX.md) | [README](README.md) | [Metrology](metrology.md)
