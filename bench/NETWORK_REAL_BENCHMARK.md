# erlmcp_bench_network_real - Unified Network Transport Benchmark

## Overview

Consolidated real-socket benchmark for TCP and HTTP/SSE transports. Replaces separate `tcp_real_bench.erl` and `http_real_bench.erl` modules with a single unified implementation.

## Features

- **Real Sockets**: Uses actual gen_tcp, ranch (TCP), gun (HTTP client), cowboy (HTTP server)
- **Dual Transport**: Supports both TCP and HTTP/SSE protocols
- **Full Metrology**: Compliant with erlmcp metrology standards
- **Unified Output**: Consistent JSON format across all transports
- **Transport-Specific Metrics**: Protocol version, TLS handshake, HTTP overhead

## Workloads

### TCP Workloads (High Performance, Low Overhead)

| Workload ID | Connections | Duration | Payload | Target |
|-------------|-------------|----------|---------|--------|
| `tcp_burst_100_1kib` | 100 | 60s | 1 KiB | 50K msg/s |
| `tcp_sustained_10k_1kib` | 10,000 | 300s | 1 KiB | 100K msg/s |
| `tcp_sustained_10k_100kib` | 10,000 | 300s | 100 KiB | 10K msg/s |
| `tcp_max_100k_1kib` | 100,000 | 1800s | 1 KiB | 200K msg/s |

### HTTP Workloads (Higher Overhead, Request/Response)

| Workload ID | Connections | Duration | Payload | Protocol | Target |
|-------------|-------------|----------|---------|----------|--------|
| `http_burst_100_1kib` | 100 | 60s | 1 KiB | HTTP/2 | 5K msg/s |
| `http_sustained_5k_1kib` | 5,000 | 300s | 1 KiB | HTTP/2 | 50K msg/s |
| `http1_sustained_2k_512b` | 2,000 | 300s | 512 B | HTTP/1.1 | 20K msg/s |

## Usage

### List Available Workloads

```erlang
erlmcp_bench_network_real:list_workloads().
% Returns: [{WorkloadName, WorkloadId, Transport}, ...]
```

### Run Specific Workload

```erlang
%% Run with default parameters
erlmcp_bench_network_real:run_workload(tcp_burst_100_1kib).

%% Override parameters
erlmcp_bench_network_real:run_workload(tcp_burst_100_1kib, #{
    connections => 50,
    duration_s => 30,
    payload_size_bytes => 512
}).
```

### Run All Workloads

```erlang
erlmcp_bench_network_real:run_all().
```

### From Command Line

```bash
# Compile
rebar3 compile

# Run all benchmarks
erl -pa _build/default/lib/*/ebin \
    -s erlmcp_bench_network_real run_all \
    -s init stop

# Run specific workload
erl -pa _build/default/lib/*/ebin \
    -eval "erlmcp_bench_network_real:run_workload(tcp_burst_100_1kib)" \
    -s init stop
```

## Output Format

### TCP Metrics

```json
{
  "workload_id": "tcp_sustained_10k_1kib",
  "benchmark": "network_real",
  "transport": "tcp",
  "connections": 10000,
  "duration_s": 300,
  "messages_sent": 5000000,
  "throughput_msg_per_s": 16666.67,
  "bandwidth_mib_per_s": 16.3,
  "latency_p50_us": 450.0,
  "latency_p95_us": 1200.0,
  "latency_p99_us": 2500.0,
  "connection_setup_avg_ms": 2.5,
  "connection_failures": 0,
  "timeout_count": 0,
  "memory_rss_mib_per_node": 2048.0,
  "memory_heap_mib_per_conn": 0.05,
  "cpu_percent_per_node": 45.2,
  "scope": "per_node",
  "tls_enabled": false,
  "precision": "microsecond",
  "environment": "local_dev",
  "timestamp": 1738012800,
  "actual_duration_s": 300.12
}
```

### HTTP Metrics

```json
{
  "workload_id": "http_sustained_5k_1kib",
  "benchmark": "network_real",
  "transport": "http_2.0",
  "connections": 5000,
  "duration_s": 300,
  "messages_sent": 2500000,
  "throughput_msg_per_s": 8333.33,
  "bandwidth_mib_per_s": 8.2,
  "latency_p50_us": 1200.0,
  "latency_p95_us": 3000.0,
  "latency_p99_us": 5000.0,
  "connection_setup_avg_ms": 5.5,
  "connection_failures": 0,
  "timeout_count": 0,
  "memory_rss_mib_per_node": 1024.0,
  "memory_heap_mib_per_conn": 0.1,
  "cpu_percent_per_node": 38.5,
  "scope": "per_node",
  "tls_enabled": false,
  "precision": "microsecond",
  "environment": "local_dev",
  "timestamp": 1738012800,
  "actual_duration_s": 300.08,
  "protocol": "http/2",
  "tls_handshake_avg_us": 500.0,
  "request_overhead_bytes": 234,
  "connection_reuse_percent": 100.0
}
```

## Metrology Compliance

### Required Fields (All Transports)

- `workload_id` - Unique identifier
- `benchmark` - Always "network_real"
- `transport` - "tcp", "http_1.1", or "http_2.0"
- `connections` - Number of concurrent connections
- `duration_s` - Target duration in seconds
- `messages_sent` - Total messages successfully sent
- `throughput_msg_per_s` - Messages per second
- `bandwidth_mib_per_s` - Bandwidth in MiB/sec
- `latency_p50_us`, `latency_p95_us`, `latency_p99_us` - Latency percentiles in microseconds
- `connection_setup_avg_ms` - Average connection setup time
- `connection_failures` - Number of failed connections
- `timeout_count` - Number of timeouts
- `memory_rss_mib_per_node` - Memory usage in MiB
- `memory_heap_mib_per_conn` - Heap per connection in MiB
- `cpu_percent_per_node` - Estimated CPU usage
- `scope` - Always "per_node"
- `tls_enabled` - Boolean
- `precision` - Always "microsecond"
- `environment` - "local_dev", "kubernetes", "aws_unknown"
- `timestamp` - Unix timestamp
- `actual_duration_s` - Actual duration (may differ from target)

### HTTP-Specific Fields

- `protocol` - "http/1.1" or "http/2"
- `tls_handshake_avg_us` - TLS handshake time (if enabled)
- `request_overhead_bytes` - HTTP header overhead (typically 234 bytes)
- `connection_reuse_percent` - Percentage of connection reuse

## Validation

All metrics are validated before being written to JSON:

```erlang
Result = erlmcp_bench_network_real:validate_metrics(JsonMap).
% Returns: ok | {error, {missing_fields, [Field]}}
```

## Transport Availability

The benchmark automatically detects available transports:

```erlang
%% TCP requires ranch
%% HTTP requires gun + cowboy

%% Check availability
TcpAvailable = check_tcp_transport().
HttpAvailable = check_http_transport().
```

If a transport is not available, its workloads are skipped with a clear message.

## Architecture

### TCP Benchmark Flow

1. Start ranch-based TCP server
2. Spawn N client connections (gen_tcp)
3. Measure connection setup time
4. Send messages for D seconds
5. Collect latency samples (first 10K per client)
6. Aggregate metrics
7. Cleanup and report

### HTTP Benchmark Flow

1. Start cowboy HTTP server
2. Spawn N gun HTTP clients
3. Measure TLS handshake time
4. Send M messages per connection
5. Collect latency samples
6. Calculate HTTP-specific metrics
7. Cleanup and report

## Testing

### Unit Tests

```bash
rebar3 eunit --module=erlmcp_bench_network_real_tests
```

### Integration Test

```bash
erl -pa _build/default/lib/*/ebin \
    -s test_network_real_bench run
```

## Comparison: TCP vs HTTP

| Metric | TCP | HTTP/2 | HTTP/1.1 |
|--------|-----|--------|----------|
| **Overhead** | Minimal (framing only) | 234+ bytes/req | 234+ bytes/req |
| **Connections** | 100K+ practical | 10K practical | 5K practical |
| **Throughput** | Very High | Moderate | Lower |
| **Latency** | Low (< 1ms p99) | Medium (1-5ms p99) | Medium-High |
| **Use Case** | Sustained streaming | Request/response | Simple req/resp |

## Performance Targets

### TCP

- **Burst (100 conn)**: ≥ 50K msg/s
- **Sustained (10K conn)**: ≥ 100K msg/s
- **Max (100K conn)**: ≥ 200K msg/s

### HTTP/2

- **Burst (100 conn)**: ≥ 5K msg/s
- **Sustained (5K conn)**: ≥ 50K msg/s

### HTTP/1.1

- **Sustained (2K conn)**: ≥ 20K msg/s

## Migration from Legacy Modules

### tcp_real_bench.erl

```erlang
%% OLD
tcp_real_bench:run_workload(sustained_10k).

%% NEW
erlmcp_bench_network_real:run_workload(tcp_sustained_10k_1kib).
```

### http_real_bench.erl

```erlang
%% OLD
http_real_bench:run_workload(?WORKLOAD_HTTP_SUSTAINED_5K).

%% NEW
erlmcp_bench_network_real:run_workload(http_sustained_5k_1kib).
```

## Dependencies

- **jsx** - JSON encoding/decoding
- **ranch** - TCP server (for TCP benchmarks)
- **gun** - HTTP client (for HTTP benchmarks)
- **cowboy** - HTTP server (for HTTP benchmarks)

## Files

- `bench/erlmcp_bench_network_real.erl` - Main benchmark module
- `bench/http_bench_handler.erl` - Cowboy HTTP handler (reused)
- `test/erlmcp_bench_network_real_tests.erl` - EUnit tests
- `bench/test_network_real_bench.erl` - Integration test
- `bench/NETWORK_REAL_BENCHMARK.md` - This documentation

## Consolidation Benefits

1. **Single Interface**: One module for all network transports
2. **Consistent Metrics**: Unified output format
3. **Code Reuse**: Shared validation, JSON serialization, helpers
4. **Easier Maintenance**: Update once, affects all transports
5. **Reduced LOC**: ~2000 LOC → ~1200 LOC (40% reduction)

## Future Enhancements

- WebSocket transport support
- TLS/SSL benchmarks
- Multi-node distributed benchmarks
- Real-time metrics streaming (via OpenTelemetry)
- Automated performance regression detection

## See Also

- `bench/README_BENCHMARKS.md` - General benchmark documentation
- `docs/v0.6.0-FINAL-PLAN.md` - Library integration plan
- `bench/transport_real/VERIFICATION.md` - Original verification docs
