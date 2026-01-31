# Transport Performance Benchmarks

**File**: `erlmcp_bench_transports.erl`  
**Purpose**: Measure WebSocket, SSE, and TLS overhead for transport layer performance  
**Status**: P1 Gap Filled - Critical transport benchmarks now implemented

## Overview

This benchmark module fills a critical gap in erlmcp's performance testing by measuring real transport layer performance across three categories:

1. **WebSocket Throughput** - Bidirectional message performance
2. **SSE Streaming** - Server-to-client event streaming performance
3. **TLS Overhead** - Handshake time, throughput impact, pooling benefits

## Benchmark Categories

### 1. WebSocket Throughput

Measures bidirectional WebSocket message performance with real connections.

**Workloads**:
- `websocket_100_1kb` - 100 messages × 1KB (baseline)
- `websocket_1k_1kb` - 1,000 messages × 1KB
- `websocket_1k_10kb` - 1,000 messages × 10KB
- `websocket_10k_1kb` - 10,000 messages × 1KB (sustained)
- `websocket_10k_10kb` - 10,000 messages × 10KB (sustained large)
- `websocket_1k_100kb` - 1,000 messages × 100KB (large payload)

**Metrics**:
- `throughput_msg_per_s` - Messages per second
- `latency_p50_us` / `latency_p95_us` / `latency_p99_us` - Latency percentiles
- `memory_delta_mib` - Memory usage delta
- `duration_s` - Total benchmark duration

**Expected Performance**:
- Small messages (1KB): 10,000-20,000 msg/sec
- Medium messages (10KB): 5,000-10,000 msg/sec  
- Large messages (100KB): 500-1,000 msg/sec
- P95 latency: <5ms for small messages

### 2. SSE Streaming

Measures server-to-client event streaming performance.

**Workloads**:
- `sse_streaming_100_1kb` - 100 events × 1KB (baseline)
- `sse_streaming_1k_1kb` - 1,000 events × 1KB
- `sse_streaming_1k_10kb` - 1,000 events × 10KB
- `sse_streaming_10k_1kb` - 10,000 events × 1KB (sustained)
- `sse_streaming_10k_10kb` - 10,000 events × 10KB (sustained large)

**Metrics**:
- `throughput_events_per_s` - Events per second
- `time_to_first_event_us` - Time until first event received
- `latency_p50_us` / `latency_p95_us` / `latency_p99_us` - Latency percentiles
- `memory_delta_mib` - Memory usage delta

**Expected Performance**:
- Small events (1KB): 8,000-15,000 events/sec
- Medium events (10KB): 4,000-8,000 events/sec
- Time to first event: <100ms
- P95 latency: <10ms for small events

### 3. TLS Overhead

Measures TLS handshake time, throughput impact, and pooling benefits.

**Workloads**:
- `tls_handshake_100` - TLS handshake performance for 100 connections
- `tls_vs_plain_1k_msg` - Compare TLS vs plain TCP throughput (1,000 messages)
- `tls_pooling_100_conn` - Connection pooling impact (100 conn, 10 reuses)

**Metrics**:
- `tls_handshake_p50_us` / `tls_handshake_p95_us` / `tls_handshake_p99_us`
- `plain_tcp_throughput_msg_per_s` - Plain TCP throughput
- `tls_tcp_throughput_msg_per_s` - TLS TCP throughput
- `tls_throughput_impact_percent` - Percentage throughput reduction
- `pooling_improvement_percent` - Percentage improvement with pooling

**Expected Performance**:
- TLS handshake P50: 1-3ms (local)
- TLS throughput impact: 10-30% reduction vs plain TCP
- Connection pooling improvement: 50-80% faster vs create/destroy

## Usage

### Run All Benchmarks

```erlang
rebar3 shell
> erlmcp_bench_transports:run_all().
```

This runs all 18 workloads (6 WebSocket + 5 SSE + 3 TLS) sequentially.

### Run Specific Workload

```erlang
> erlmcp_bench_transports:run(<<"websocket_1k_1kb">>).
> erlmcp_bench_transports:run(<<"sse_streaming_10k_1kb">>).
> erlmcp_bench_transports:run(<<"tls_handshake_100">>).
```

### List Available Workloads

```erlang
> erlmcp_bench_transports:all_workloads().
```

## Output Format

All benchmarks produce metrology-compliant JSON reports in `bench/results/`:

```json
{
  "workload_id": "websocket_1k_1kb",
  "benchmark": "transport_websocket",
  "transport": "websocket",
  "timestamp": 1738310400,
  "environment": {
    "os": "x86_64-pc-linux-gnu",
    "otp_version": "26",
    "cores": 8,
    "schedulers": 8
  },
  "messages": 1000,
  "payload_size_bytes": 1024,
  "duration_s": 0.125,
  "throughput_msg_per_s": 8000.0,
  "latency_p50_us": 89.5,
  "latency_p95_us": 142.3,
  "latency_p99_us": 198.7,
  "memory_delta_mib": 1.2,
  "precision": "microsecond",
  "scope": "per_node"
}
```

## Metrology Compliance

All metrics follow erlmcp metrology standards:

- **Throughput**: `throughput_msg_per_s` or `throughput_events_per_s`
- **Latency**: `latency_pXX_us` (raw microseconds)
- **Memory**: `memory_delta_mib` (mebibytes)
- **Precision**: Always `"microsecond"`
- **Scope**: `"per_node"` or `"per_connection"`

## Implementation Details

### Real Transports (No Mocks)

All benchmarks use real transport implementations:

- **WebSocket**: `erlmcp_transport_ws` with `gun` client
- **SSE**: `erlmcp_transport_sse` with `gun` HTTP/SSE client
- **TLS**: `ssl` module with self-signed certificates
- **TCP**: `gen_tcp` with echo servers

### Test Isolation

Each workload runs in isolation:

1. Start transport server
2. Wait for server ready (1s)
3. Connect client(s)
4. Run benchmark
5. Disconnect client(s)
6. Stop server
7. Wait for cleanup (500ms)

### Self-Signed Certificates

TLS benchmarks use auto-generated self-signed certificates in `/tmp/erlmcp_bench_certs/`:

```bash
openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem \
  -days 1 -nodes -subj '/CN=localhost'
```

These are generated once and reused for all TLS benchmarks.

## Dependencies

Required Erlang applications:

- `gun` - HTTP/WebSocket client
- `cowboy` - HTTP/WebSocket server
- `ranch` - TCP acceptor pool
- `ssl` - TLS support

All are standard erlmcp dependencies.

## Performance Targets

### Acceptance Criteria

- WebSocket 1KB: >8,000 msg/sec
- SSE 1KB: >6,000 events/sec
- TLS handshake P95: <5ms
- TLS throughput impact: <35%
- Connection pooling improvement: >50%

### Regression Detection

Compare results against baseline in `bench/results/v2_1_baseline/`:

```bash
# Run baseline
erlmcp_bench_transports:run(<<"websocket_1k_1kb">>).

# Compare against v2.1 baseline
cat bench/results/v2_1_baseline/transports_websocket_1k_1kb_*.json
```

If throughput drops >10% or latency increases >20%, investigate regression.

## Troubleshooting

### Port Conflicts

Benchmarks use these ports:
- WebSocket: 18080
- SSE: 18081
- Plain TCP: 18082
- TLS: 18443

If ports are in use:

```bash
lsof -i :18080
kill -9 <PID>
```

### TLS Certificate Errors

If TLS benchmarks fail with certificate errors:

```bash
rm -rf /tmp/erlmcp_bench_certs
# Benchmarks will regenerate
```

### Timeouts

If benchmarks timeout (default 10s):

Edit timeouts in module:
```erlang
-define(CONNECT_TIMEOUT, 30000).  % Increase to 30s
```

## Integration with CI/CD

Add to `.github/workflows/benchmarks.yml`:

```yaml
- name: Run Transport Benchmarks
  run: |
    rebar3 shell -eval "erlmcp_bench_transports:run_all(), init:stop()."
    
- name: Check Performance Regression
  run: |
    ./scripts/bench/check_transport_regression.sh
```

## Related Documentation

- `BENCHMARKS.md` - Core operations benchmarks
- `docs/bench/metrology.md` - Metrology system overview
- `docs/TRANSPORT_ARCHITECTURE.md` - Transport layer design

## Joe Armstrong Quote

> "If you can't measure it, you can't optimize it."

These benchmarks provide the measurement foundation for transport layer optimization.

