# erlmcp v2 Architecture Glossary

**Canonical Terms and Metrics for v2.0.0 Design**

Version: v2.0.0-draft
Status: CANONICAL REFERENCE
Date: 2026-01-27

---

## Purpose

This glossary defines authoritative terminology for the erlmcp v2 architecture redesign. All v2 design documents, diagrams, and code MUST use these canonical terms.

---

## Architecture Terms

### Core Entities

| Term | Definition | Usage Context |
|------|------------|---------------|
| **MCP Node** | Single Erlang VM instance running erlmcp application | Deployment, clustering |
| **MCP Server** | gen_server process implementing MCP protocol server role | Protocol layer |
| **MCP Client** | gen_server process implementing MCP protocol client role | Protocol layer |
| **Transport** | Communication channel (stdio, tcp, http, websocket) | I/O layer |
| **Session** | Stateful MCP connection (post-initialize handshake) | Protocol lifecycle |
| **Connection** | Low-level transport connection (socket, pipe) | Transport layer |

### Protocol Entities

| Term | Definition | Canonical Source |
|------|------------|------------------|
| **JSON-RPC Message** | Complete JSON-RPC 2.0 envelope (request/response/notification) | `erlmcp_json_rpc.erl` |
| **Request** | Client→Server JSON-RPC call expecting response | MCP spec 2024-11-05 |
| **Response** | Server→Client JSON-RPC result or error | MCP spec 2024-11-05 |
| **Notification** | Bidirectional JSON-RPC message (no response) | MCP spec 2024-11-05 |
| **Resource** | Server-exposed data endpoint (static or template) | `erlmcp_server.erl` |
| **Tool** | Server-exposed executable function with JSON Schema | `erlmcp_server.erl` |
| **Prompt** | Server-exposed prompt template with arguments | `erlmcp_server.erl` |

### Supervision Terms

| Term | Definition | OTP Pattern |
|------|------------|-------------|
| **Supervision Tree** | Hierarchical supervisor structure for fault isolation | `erlmcp_sup.erl` |
| **Subsystem** | Isolated supervisor branch (registry, servers, transports, monitoring) | Bulkhead pattern |
| **Worker** | Leaf process (gen_server, gen_statem) | OTP behavior |
| **Simple One-for-One** | Dynamic worker pool supervisor (clients, servers, transports) | OTP supervisor |
| **Rest-for-One** | Dependency-aware supervisor (restart dependents only) | `erlmcp_sup.erl:L123` |

---

## Metrics Terminology

### Throughput Metrics

| Term | Unit | Definition | Scope |
|------|------|------------|-------|
| **throughput_msg_per_s** | msg/s | JSON-RPC messages processed per second (req + resp counted separately) | per_node |
| **throughput_jsonrpc_requests** | req/s | JSON-RPC requests only (excludes responses/notifications) | per_node |
| **throughput_jsonrpc_messages** | msg/s | All JSON-RPC messages (requests + responses + notifications) | per_node |

**CRITICAL**: `msg_per_s` is canonical. `req/s` is DEPRECATED (ambiguous).

### Connection Metrics

| Term | Unit | Definition | Scope |
|------|------|------------|-------|
| **sockets_open** | count | Active TCP/WebSocket connections | per_node |
| **sessions_active** | count | Post-initialize MCP sessions (capability negotiation complete) | per_node |
| **connections_total** | count | All transport connections (includes pre-handshake) | per_node |
| **connection_rate_per_s** | conn/s | New connections established per second | per_node |

**Relationship**: `sessions_active ≤ sockets_open ≤ connections_total`

### Latency Metrics

| Term | Unit | Definition | Measurement Point |
|------|------|------------|-------------------|
| **latency_p50_us** | μs | 50th percentile request→response time | client_to_client |
| **latency_p95_us** | μs | 95th percentile request→response time | client_to_client |
| **latency_p99_us** | μs | 99th percentile request→response time | client_to_client |
| **latency_p50_ms** | ms | 50th percentile (use for >1ms latencies) | client_to_client |

**Measurement Points**:
- `client_to_client` - Full round-trip (application layer, includes transport)
- `server_processing` - Server-side only (gen_server:call duration)
- `network_only` - Transport layer (socket send→recv, excludes application logic)

### Memory Metrics

| Term | Unit | Definition | Scope |
|------|------|------------|-------|
| **per_connection_heap_mib** | MiB | Erlang process heap per connection (erlang:process_info(Pid, memory)) | per_connection |
| **per_connection_state_mib** | MiB | Application state size (transport buffers + pending requests) | per_connection |
| **per_node_base_overhead_mib** | MiB | Fixed memory before first connection (VM + OTP apps + supervision) | per_node |
| **per_node_total_rss_mib** | MiB | Total Resident Set Size (ps -o rss= or recon_alloc:memory(allocated)) | per_node |

**Formula Validation**:
```
per_node_total_rss_mib ≈
  per_node_base_overhead_mib +
  (sockets_open × (per_connection_heap_mib + per_connection_state_mib)) +
  overhead_margin_mib
```

**DEPRECATED**: `MiB/conn`, `memory_mb`, `memory` (ambiguous component).

---

## Workload References

All performance metrics MUST cite a workload_id for reproducibility.

### Workload ID Format

`{transport}_{pattern}_{scale}_{payload}`

Examples:
- `tcp_sustained_10k_1kib` - TCP, sustained load, 10K connections, 1 KiB messages, 30s duration
- `http_sse_burst_50k_mixed` - HTTP SSE, burst pattern, 50K connections, mixed payload sizes
- `stdio_sequential_1k_small` - stdio, sequential, 1K messages, small payloads

**Canonical Source**: `bench/workloads/{workload_id}.json`

### Common Workloads

| Workload ID | Transport | Pattern | Scale | Payload | Duration |
|-------------|-----------|---------|-------|---------|----------|
| `core_ops_100k` | in-memory | sequential | 400K ops | N/A | 0.15s |
| `tcp_sustained_10k_1kib` | tcp | sustained | 10K conn | 1 KiB | 30s |
| `http_sse_burst_50k` | http_sse | burst | 50K conn | mixed | 60s |
| `stdio_sequential_1k` | stdio | sequential | 1K msg | small | 5s |

---

## Transport Types

| Term | Protocol | Use Case | Module |
|------|----------|----------|--------|
| **stdio** | Standard I/O pipes | Local LLM integration | `erlmcp_transport_stdio.erl` |
| **tcp** | Raw TCP sockets | High-throughput server | `erlmcp_transport_tcp.erl` |
| **http_sse** | HTTP Server-Sent Events | Web clients, long-polling | `erlmcp_transport_http.erl` |
| **websocket** | WebSocket (RFC 6455) | Bidirectional web clients | `erlmcp_transport_ws.erl` |

---

## Scope Definitions

| Scope | Definition | Example Metric |
|-------|------------|----------------|
| **per_node** | Single Erlang VM instance | `per_node_total_rss_mib` |
| **per_cluster** | Distributed Erlang cluster (multiple nodes) | `per_cluster_total_sessions` |
| **per_connection** | Individual TCP/HTTP/WebSocket connection | `per_connection_heap_mib` |
| **per_request** | Single JSON-RPC request→response cycle | `latency_p99_us` |

---

## Component Classification

### Canonical vs. Legacy

| Classification | Definition | Disposition in v2 |
|----------------|------------|-------------------|
| **Canonical** | Core MCP protocol implementation (servers, transports, JSON-RPC) | KEEP, refactor if needed |
| **Legacy** | Deprecated experimental features (e.g., early TCPS prototypes) | DELETE or isolate |
| **Library** | Production libraries (gproc, gun, ranch, poolboy) | KEEP, upgrade |
| **TCPS** | Toyota Code Production System (manufacturing layer) | SEPARATE package |

### Module Categories

| Category | Modules | v2 Status |
|----------|---------|-----------|
| **Core Protocol** | `erlmcp_client.erl`, `erlmcp_server.erl`, `erlmcp_json_rpc.erl` | CANONICAL |
| **Transports** | `erlmcp_transport_{stdio,tcp,http,ws}.erl` | CANONICAL |
| **Registry** | `erlmcp_registry.erl`, `erlmcp_registry_sup.erl` | CANONICAL (migrate to gproc) |
| **Supervision** | `erlmcp_sup.erl`, `erlmcp_*_sup.erl` | CANONICAL |
| **TCPS** | `tcps_*.erl` (85+ modules) | SEPARATE (move to tcps_erlmcp package) |
| **Benchmarks** | `bench/*.erl` | CANONICAL |
| **Examples** | `examples/*/` | CANONICAL |

---

## Precision Standards

### Timestamp Precision

| Field | Unit | Precision | Example |
|-------|------|-----------|---------|
| `timestamp` | Unix seconds | second | `1769567361` |
| `timestamp_us` | Unix microseconds | microsecond | `1769567361123456` |
| `duration_s` | Seconds (decimal) | second | `30.5` |
| `duration_ms` | Milliseconds (decimal) | millisecond | `5248.3` |

### Numeric Precision

| Metric Type | Decimal Places | Example |
|-------------|----------------|---------|
| Throughput (msg/s) | 2 | `2690088.37` |
| Latency (μs) | 1 | `83.0` |
| Memory (MiB) | 3 | `19.142` |
| CPU (%) | 1 | `42.0` |

---

## Prohibited Terms (v1.x Deprecated)

| DEPRECATED (v1.x) | CANONICAL (v2.0) | Reason |
|-------------------|------------------|--------|
| `req/s` | `throughput_msg_per_s` | Ambiguous (requests only or req+resp?) |
| `MiB/conn` | `per_connection_heap_mib` | Unclear component (heap vs. state vs. total) |
| `memory_mb` | `per_node_total_rss_mib` or component-specific | Missing scope |
| `max_connections` | `sockets_open` + `workload_id` | Missing context |
| `latency_ms` | `latency_p99_ms` + `workload_id` + `transport` | Missing percentile/context |

---

## Environment Specifications

### Standard Hardware Profiles

| Profile | CPU | RAM | Network | Disk | OS |
|---------|-----|-----|---------|------|-----|
| `dev_laptop` | 8 vCPU | 16 GB | 1 Gbps | SSD | macOS 14+ / Ubuntu 22.04 |
| `prod_hw_spec_01` | 16 vCPU AMD EPYC 7002 | 64 GB | 10 Gbps | NVMe SSD | Ubuntu 22.04 LTS |
| `prod_hw_spec_02` | 32 vCPU AMD EPYC 7002 | 128 GB | 10 Gbps | NVMe SSD | Ubuntu 22.04 LTS |
| `gov_fips_hardware` | 16 vCPU FIPS-certified | 64 GB ECC | 10 Gbps | Encrypted NVMe | RHEL 8 FIPS |

**Canonical Source**: `bench/environments/{profile}.json`

---

## Validation

Every v2 design document MUST:

1. ✅ Use ONLY canonical terms from this glossary
2. ✅ Cite workload_id for ALL performance claims
3. ✅ Specify transport for ALL throughput/latency metrics
4. ✅ Include scope (per_node, per_cluster, per_connection)
5. ✅ Reference environment profile for hardware specs
6. ✅ Use correct units (msg/s, μs, MiB) with precision standards

---

## References

- **Metrics Glossary (v1.5.0)**: `docs/metrology/METRICS_GLOSSARY.md`
- **Workload Definitions**: `bench/workloads/*.json`
- **Environment Specs**: `bench/environments/*.json`
- **Benchmark Results**: `bench/results/*.json`
- **MCP Protocol Spec**: `docs/protocol.md` (2024-11-05)
- **OTP Patterns**: `docs/otp-patterns.md`

---

**Document Status**: CANONICAL (v2.0.0-draft)
**Last Updated**: 2026-01-27
**Approver**: erlmcp Architecture Team
