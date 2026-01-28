# Metrics Glossary v1.5.0

**Canonical Metric Definitions for erlmcp Performance Specifications**

This document defines the authoritative metric units and measurement standards for erlmcp v1.5.0+. All plan specifications, benchmark reports, and performance claims MUST use these canonical definitions.

## Version History

- **v1.5.0** (2026-01-27): Initial canonical metrology specification
- **Supersedes**: v1.4.0 ambiguous metrics (req/s, MiB/conn)

## Core Principles

1. **Explicit Units**: Every numeric value MUST have a unit field
2. **Workload References**: Every performance claim MUST reference a workload_id
3. **Transport Specification**: All metrics MUST specify transport (tcp, http_sse, stdio)
4. **Scope Clarity**: Distinguish per_node, per_cluster, per_connection metrics
5. **Reproducibility**: All measurements must include environment details

---

## Throughput Metrics

### msg_per_s (Messages Per Second)

**Definition**: Number of complete MCP protocol messages processed per second.

**What counts as a message**:
- JSON-RPC request (client → server)
- JSON-RPC response (server → client)
- JSON-RPC notification (bidirectional)
- Each counted independently (1 req + 1 resp = 2 messages)

**Required Fields**:
```json
{
  "throughput": {
    "value": 50000,
    "unit": "msg_per_s",
    "definition": "JSON-RPC messages (requests + responses)",
    "workload_id": "tcp_sustained_10k_1kib",
    "transport": "tcp",
    "scope": "per_node",
    "duration_s": 30
  }
}
```

**Common Workload IDs**:
- `tcp_sustained_10k_1kib` - 10K connections, 1 KiB messages, 30s sustained
- `http_sse_burst_50k` - 50K connections, HTTP SSE transport, burst pattern
- `stdio_sequential_1k` - 1K messages, stdio transport, sequential
- `tcp_mixed_workload` - Mixed message sizes (100B-10KiB), realistic traffic

**Deprecated**: `req/s`, `requests_per_second`, `throughput_req_s` (ambiguous: requests only or req+resp?)

---

## Memory Metrics

### Per-Connection Memory

erlmcp memory usage has multiple components. Use specific metrics:

#### per_connection_heap_mib

**Definition**: Erlang process heap memory per active connection (gen_server state).

**Measurement**: `erlang:process_info(Pid, memory)` / (1024*1024)

```json
{
  "per_connection_heap_mib": 0.048,
  "workload_id": "tcp_sustained_10k_1kib",
  "connections": 10000,
  "scope": "per_connection"
}
```

#### per_connection_state_mib

**Definition**: Application state size per connection (transport buffers, pending requests).

**Measurement**: Size of `#state{}` record + transport buffers.

```json
{
  "per_connection_state_mib": 0.012,
  "includes": ["transport_buffers", "pending_requests", "subscription_state"]
}
```

#### per_node_base_overhead_mib

**Definition**: Fixed memory overhead before first connection (VM, OTP apps, supervision tree).

**Measurement**: RSS after application start, zero connections.

```json
{
  "per_node_base_overhead_mib": 150,
  "includes": ["beam_vm", "kernel", "stdlib", "ssl", "erlmcp_app"],
  "scope": "per_node"
}
```

#### per_node_total_rss_mib

**Definition**: Total Resident Set Size (RSS) for entire Erlang node under load.

**Measurement**: `ps -o rss=` or `recon_alloc:memory(allocated)`.

```json
{
  "per_node_total_rss_mib": 2048,
  "workload_id": "tcp_sustained_10k_1kib",
  "connections": 10000,
  "scope": "per_node",
  "environment": "prod_hw_spec_01"
}
```

**Formula Validation**:
```
per_node_total_rss_mib ≈
  per_node_base_overhead_mib +
  (connections × (per_connection_heap_mib + per_connection_state_mib)) +
  overhead_margin_mib
```

**Deprecated**: `memory_limit_mb`, `MiB/conn`, `memory` (ambiguous component)

---

## Latency Metrics

### Percentile Latency

**Units**: Milliseconds (ms) or microseconds (us) depending on magnitude.

**Definition**: Time from request sent to response received, measured at percentiles.

```json
{
  "latency": {
    "p50_ms": 5.2,
    "p95_ms": 12.8,
    "p99_ms": 28.4,
    "workload_id": "tcp_sustained_10k_1kib",
    "transport": "tcp",
    "measurement_point": "client_to_client"
  }
}
```

**Measurement Points**:
- `client_to_client` - Full round-trip (application layer)
- `server_processing` - Server-side only (gen_server:call duration)
- `network_only` - Transport layer (socket send → recv)

---

## Connection Metrics

### concurrent_connections

**Definition**: Number of simultaneously active TCP/HTTP connections.

```json
{
  "max_concurrent_connections": 100000,
  "workload_id": "tcp_sustained_100k_1kib",
  "transport": "tcp",
  "scope": "per_node"
}
```

### connection_rate_per_s

**Definition**: New connections established per second.

```json
{
  "connection_rate_per_s": 5000,
  "duration_s": 10,
  "workload_id": "tcp_connection_burst"
}
```

---

## Queue Metrics

### queue_depth_messages

**Definition**: Number of messages in process mailbox or application queue.

```json
{
  "max_queue_depth_messages": 500000,
  "scope": "per_node",
  "component": "erlmcp_server_mailbox"
}
```

---

## Failover & SLA Metrics

### failover_sla_seconds

**Definition**: Maximum time for cluster to detect node failure and reroute traffic.

```json
{
  "failover_sla_seconds": 2,
  "topology": "3_node_cluster",
  "detection_mechanism": "net_kernel_monitor",
  "workload_id": "ha_failover_test"
}
```

---

## Transport Types

All metrics MUST specify transport:

- `tcp` - Raw TCP sockets (gen_tcp)
- `http_sse` - HTTP Server-Sent Events
- `http_polling` - HTTP long-polling
- `websocket` - WebSocket protocol
- `stdio` - Standard I/O (pipes)

---

## Scope Definitions

- `per_node` - Single Erlang node (VM instance)
- `per_cluster` - Distributed Erlang cluster
- `per_connection` - Individual client connection
- `per_request` - Single request/response cycle

---

## Workload ID Format

Format: `{transport}_{pattern}_{scale}_{payload}`

Examples:
- `tcp_sustained_10k_1kib` - TCP, sustained load, 10K connections, 1 KiB messages
- `http_sse_burst_50k_mixed` - HTTP SSE, burst pattern, 50K connections, mixed sizes
- `stdio_sequential_1k_small` - stdio, sequential, 1K messages, small payload

Workload definitions MUST exist in `bench/workloads/{workload_id}.json`.

---

## Environment Specifications

### Hardware Specifications

Reference standard hardware profiles:

```json
{
  "environment": "prod_hw_spec_01",
  "cpu": "16 vCPU (AMD EPYC 7002)",
  "ram_gb": 64,
  "network": "10 Gbps",
  "disk": "NVMe SSD",
  "os": "Ubuntu 22.04 LTS"
}
```

Standard profiles:
- `dev_laptop` - Development machine (8 vCPU, 16 GB RAM)
- `prod_hw_spec_01` - Production tier 1 (16 vCPU, 64 GB RAM)
- `prod_hw_spec_02` - Production tier 2 (32 vCPU, 128 GB RAM)
- `gov_fips_hardware` - FIPS-140-2 certified hardware

---

## Migration from v1.4.0

### Ambiguous Metrics → Canonical Replacements

| v1.4.0 (Ambiguous) | v1.5.0 (Canonical) | Notes |
|--------------------|-------------------|-------|
| `throughput_req_s: 450` | `throughput.msg_per_s: 900` | Clarify req+resp |
| `memory: "5 MB/conn"` | `per_connection_heap_mib: 0.048` | Specify component |
| `max_connections: 10000` | `concurrent_connections: 10000` + workload_id | Add reference |
| `p99_latency_ms: 150` | `latency.p99_ms: 150` + workload_id + transport | Add context |

### Required Transformations

1. **Add workload_id** to every performance claim
2. **Specify transport** for all throughput/latency metrics
3. **Break down memory** into heap, state, base overhead, total RSS
4. **Add duration_s** for sustained load claims
5. **Add scope** (per_node, per_cluster, per_connection)
6. **Add environment** reference for reproducibility

---

## Validation Rules

### Mandatory Fields

Every performance metric MUST include:
- ✅ `value` (numeric)
- ✅ `unit` (from glossary)
- ✅ `workload_id` (must exist in bench/workloads/)
- ✅ `transport` (tcp, http_sse, stdio, websocket)
- ✅ `scope` (per_node, per_cluster, per_connection)

### Prohibited Patterns

- ❌ `req/s` (use `msg_per_s`)
- ❌ `MiB/conn` (use `per_connection_heap_mib` or `per_connection_state_mib`)
- ❌ `memory_mb` (specify component: heap, state, RSS)
- ❌ Numeric values without units
- ❌ Performance claims without workload references

---

## Compliance Badge

Plans compliant with v1.5.0 metrology MUST include:

```json
{
  "metrology_compliance": {
    "version": "v1.5.0",
    "validated": "2026-01-27",
    "validator": "scripts/validate_plan_metrology.sh",
    "violations": 0
  }
}
```

---

## References

- **Workload Definitions**: `bench/workloads/*.json`
- **Environment Specs**: `bench/environments/*.json`
- **Validation Tool**: `scripts/validate_plan_metrology.sh`
- **Plan Files**: `plans/*.plan.json`

---

## Contact

For questions about metrology standards or exceptions:
- **Issue Tracker**: github.com/erlmcp/erlmcp/issues
- **Tag**: `metrology`, `v1.5.0`

---

**Document Status**: CANONICAL (v1.5.0)
**Last Updated**: 2026-01-27
**Approver**: erlmcp Core Team
