# Real Transport Benchmarks

This directory contains benchmarks for erlmcp's real transport implementations using production-grade libraries:

- **TCP**: Uses `ranch` 2.1.0 for connection handling
- **HTTP**: Uses `gun` 2.0.1 for HTTP/1.1 and HTTP/2 client

## Benchmark Modules

### tcp_real_bench.erl
Measures TCP transport performance with ranch integration.

**Target scenarios:**
- 100K concurrent connections
- Sustained throughput measurements
- Latency profiling (p50/p95/p99)
- Connection reuse efficiency

### http_real_bench.erl
Measures HTTP/SSE transport performance with gun integration.

**IMPORTANT LIMITATIONS:**

#### Connection Limits
- **HTTP/1.1**: ~5K concurrent connections (practical limit)
- **HTTP/2**: ~10K concurrent connections (practical limit)
- **NOT suitable for 100K+ connections** (use TCP or WebSocket instead)

#### Protocol Overhead
- Each HTTP request has **234+ bytes of overhead**:
  - Request line: ~30 bytes
  - Headers: ~200 bytes (Host, Content-Type, Content-Length, User-Agent)
  - Body separator: 4 bytes
- HTTP/2 reduces per-request overhead via header compression (HPACK)
- Still significantly higher than raw TCP (which has ~20 bytes TCP/IP header)

#### Performance Characteristics
- **TLS handshake**: Measured separately (~2-10ms, not included in message latency)
- **Connection reuse**: 100% in benchmark (persistent connections)
- **Request overhead**: Documented in metrics
- **Stream multiplexing**: HTTP/2 allows multiple streams per connection

## Running Benchmarks

### Run All Benchmarks
```bash
./scripts/bench/run_transport_real.sh all
```

### Run TCP Only
```bash
./scripts/bench/run_transport_real.sh tcp
```

### Run HTTP Only
```bash
./scripts/bench/run_transport_real.sh http
```

## Output Format

Results are saved to `bench/results/transport_real_[tcp|http]_<timestamp>.json`

### JSON Structure
```json
[
  {
    "workload_id": "http_sustained_5k_1kib",
    "transport": "http_2.0",
    "metrology": {
      "duration_ms": 30000.0,
      "total_messages": 5000,
      "successful_messages": 4998,
      "failed_messages": 2,
      "throughput_msg_sec": 166.6,
      "throughput_bytes_sec": 170598.4,
      "latency_p50_us": 1250.0,
      "latency_p95_us": 3500.0,
      "latency_p99_us": 5200.0,
      "latency_max_us": 8900.0,
      "cpu_percent": 35.2,
      "memory_mb": 12.5
    },
    "http_specific": {
      "protocol": "http/2",
      "connections": 100,
      "tls_handshake_avg_us": 2500.0,
      "request_overhead_bytes": 234,
      "connection_reuse_percent": 100.0
    }
  }
]
```

## HTTP Transport Use Cases

### ✅ Good Use Cases
- Request/response patterns (RPC-style)
- Low to moderate concurrency (< 10K connections)
- Standard HTTP infrastructure compatibility
- Load balancer integration
- Web browser clients

### ❌ Bad Use Cases
- 100K+ concurrent connections
- Sustained high-throughput streaming
- Low-latency requirements (< 1ms)
- Minimal overhead requirements

### 🔄 Alternatives for 100K+ Connections
- **TCP with custom framing**: Lowest overhead, highest scalability
- **WebSocket**: Persistent, bidirectional, lower overhead than HTTP
- **QUIC/HTTP/3**: Future consideration (not yet in gun 2.0.1)

## Benchmark Workloads

### HTTP/2 Workloads
1. **http_sustained_5k_1kib**: 100 connections, 50 messages each, 1KB payload
2. **http_burst_1k_256b**: 50 connections, 20 messages each, 256B payload

### HTTP/1.1 Workloads
1. **http1_sustained_2k_512b**: 200 connections, 10 messages each, 512B payload

## Performance Expectations

Based on gun 2.0.1 and cowboy 2.10.0:

| Metric | HTTP/1.1 | HTTP/2 |
|--------|----------|--------|
| **Throughput** | ~2K-5K msg/sec | ~5K-10K msg/sec |
| **Latency P95** | 2-5ms | 1-3ms |
| **Connection limit** | ~5K | ~10K |
| **Overhead per request** | 234+ bytes | 150+ bytes (HPACK) |
| **CPU usage** | 30-50% | 35-60% |

## Comparison with TCP

| Feature | TCP (ranch) | HTTP (gun) |
|---------|-------------|------------|
| **Max connections** | 100K+ | ~10K |
| **Overhead per message** | 20 bytes (IP/TCP) | 234+ bytes (headers) |
| **Latency P95** | 50-200µs | 1-3ms |
| **Throughput** | 100K+ msg/sec | 5K-10K msg/sec |
| **Protocol complexity** | Custom framing | Standard HTTP |
| **Infrastructure compatibility** | Direct TCP | Load balancers, CDNs |

## Notes on SSE (Server-Sent Events)

If MCP uses SSE for server→client streaming:
- SSE uses chunked transfer encoding (additional overhead)
- Each event has `data: ` prefix and `\n\n` suffix (~8 bytes)
- SSE only supports server→client (client→server uses regular POST)
- Connection must remain open for streaming

**Current benchmark**: Uses standard HTTP POST (request/response), not SSE streaming.

## Implementation Status

### ✅ Implemented
- HTTP/1.1 POST benchmarks
- HTTP/2 POST benchmarks
- Connection reuse measurement
- TLS handshake timing
- Request overhead calculation
- gun integration
- cowboy server for testing

### ⚠️ Known Limitations
- No SSE streaming benchmark (would require separate implementation)
- No 100K connection test (not realistic for HTTP)
- No TLS benchmarking (using localhost TCP for performance)
- No load balancer testing

### 📝 Future Enhancements
- WebSocket transport benchmark (for comparison)
- QUIC/HTTP/3 when available in gun
- SSE streaming workload
- Connection pooling metrics
