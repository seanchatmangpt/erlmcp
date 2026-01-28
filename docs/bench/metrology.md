# Benchmark Metrology Standards v1.5.0

**Version**: v1.5.0 (2026-01-27)
**Status**: CANONICAL (Production)
**Source of Truth**: This document defines canonical metrics for all erlmcp benchmarks
**Validator Module**: `erlmcp_metrology_validator.erl`

---

## Executive Summary

This document establishes canonical metrics, units, scopes, and validation rules for erlmcp performance benchmarks. All benchmark reports, plans, and performance claims MUST conform to these standards to ensure:

- Reproducibility and traceability
- Automated validation and quality gates
- Elimination of ambiguous metrics
- SLA and compliance documentation

**Scope**: Performance metrics for erlmcp v0.6.0+ (client, server, all transports)

---

## Section 1: Canonical Metrics

### 1.1 Throughput: msg_per_s

**Definition**: Number of complete JSON-RPC protocol messages processed per second.

**What Counts**:
- 1x JSON-RPC request (client → server) = 1 message
- 1x JSON-RPC response (server → client) = 1 message
- 1x JSON-RPC notification (bidirectional) = 1 message
- Request-response cycle = 2 messages (not 1)

**Formula**:
```
msg_per_s = (requests_sent + responses_received + notifications) / duration_seconds
```

**JSON Structure**:
```json
{
  "throughput": {
    "value": 50000,
    "unit": "msg_per_s",
    "workload_id": "tcp_sustained_10k_1kib",
    "transport": "tcp",
    "scope": "per_node",
    "duration_s": 30
  }
}
```

**Deprecated**: `req/s`, `requests_per_second`, `throughput_req_s` (ambiguous)

---

### 1.2 Memory: Component-Specific Breakdown

Do NOT use aggregate terms like `memory_mb` or `MiB/conn`. Use specific component metrics.

#### per_connection_heap_mib

**Definition**: Erlang process heap memory per active connection.

**Measurement**:
```erlang
{memory, Heap} = erlang:process_info(Pid, memory),
MemMiB = Heap / (1024 * 1024)
```

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

**Includes**: Transport I/O buffers, request correlation map, session data

```json
{
  "per_connection_state_mib": 0.012,
  "workload_id": "tcp_sustained_10k_1kib",
  "scope": "per_connection"
}
```

#### per_node_base_overhead_mib

**Definition**: Fixed memory overhead before first connection.

**Includes**: VM, OTP apps, supervision tree, registry initialization

```json
{
  "per_node_base_overhead_mib": 150,
  "scope": "per_node"
}
```

#### per_node_total_rss_mib

**Definition**: Total Resident Set Size for entire node under load.

**Validation Formula**:
```
per_node_total_rss_mib ≈
  per_node_base_overhead_mib +
  (connections × per_connection_heap_mib) +
  (connections × per_connection_state_mib) +
  overhead_margin_mib
```

```json
{
  "per_node_total_rss_mib": 2048,
  "workload_id": "tcp_sustained_10k_1kib",
  "connections": 10000,
  "scope": "per_node",
  "environment": "prod_hw_spec_01"
}
```

**Deprecated**: `memory_limit_mb`, `MiB/conn`, `memory` (unspecified), `MB`

---

### 1.3 Latency: Percentile Metrics

**Definition**: Time from request sent to response received (full round-trip, application layer).

**Units**: Milliseconds for display; microseconds for raw precision

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
- `client_to_client`: Full round-trip (application layer)
- `server_processing`: Server-side only (gen_server:call)
- `network_only`: Transport layer (socket send → recv)

**Raw Precision Example**:
```json
{
  "latency": {
    "p99_ms": 28.4,
    "p99_us": 28400,
    "precision_us": 28400,
    "workload_id": "tcp_sustained_10k_1kib"
  }
}
```

---

### 1.4 Connection Metrics

#### concurrent_connections

**Definition**: Number of simultaneously active connections.

```json
{
  "concurrent_connections": 100000,
  "workload_id": "tcp_sustained_100k_1kib",
  "transport": "tcp",
  "scope": "per_node"
}
```

#### connection_rate_per_s

**Definition**: New connections established per second.

```json
{
  "connection_rate_per_s": 5000,
  "duration_s": 10,
  "workload_id": "tcp_connection_burst",
  "transport": "tcp"
}
```

---

### 1.5 Process & Queue Metrics

#### queue_depth_messages

**Definition**: Number of messages in process mailbox.

```json
{
  "max_queue_depth_messages": 500000,
  "scope": "per_node",
  "component": "erlmcp_server_mailbox"
}
```

#### sessions

**Definition**: Count of active MCP session objects.

Note: Sessions ≠ connections (one session may span multiple transports)

```json
{
  "sessions": 10000,
  "workload_id": "tcp_sustained_10k_1kib",
  "scope": "per_node"
}
```

---

## Section 2: Allowed Units & Canonicalization

### 2.1 Canonical Unit Lists

#### Time Units (Prefer µs/us for raw measurements)
- `ns` - Nanoseconds (raw precision)
- `µs` or `us` - Microseconds (raw precision)
- `ms` - Milliseconds (display)
- `s` - Seconds (display)

#### Memory Units (Binary Prefixes ONLY)
- `B` - Bytes
- `KiB` - Kibibytes (1024 B)
- `MiB` - Mebibytes (1024² B)
- `GiB` - Gibibytes (1024³ B)

**FORBIDDEN**: `MB`, `KB`, `GB` (decimal prefixes)

#### Rate Units
- `msg/s` - Messages per second
- `ops/s` - Operations per second
- `req/s` - Requests per second
- `MiB/s` - Mebibytes per second

### 2.2 Automatic Canonicalization

The validator applies these transformations:

```
milliseconds → µs
microseconds → µs
nanoseconds → ns
req/sec → req/s
ops/sec → ops/s
msg/sec → msg/s
MB → MiB
KB → KiB
GB → GiB
```

---

## Section 3: Scope Definitions

Every metric MUST specify its scope:

| Scope | Definition | Example |
|-------|-----------|---------|
| `per_connection` | Individual client | `per_connection_heap_mib: 0.048` |
| `per_request` | Single RPC cycle | Latency of one call |
| `per_node` | Single Erlang VM | `per_node_total_rss_mib: 2048` |
| `per_cluster` | Distributed cluster | Failover SLA for 3-node |
| `per_process` | Specific Erlang process | Queue depth per router |

---

## Section 4: Required Fields

### 4.1 Mandatory for All Performance Metrics

| Field | Type | Example | Purpose |
|-------|------|---------|---------|
| `value` | number | `50000` | Numeric measurement |
| `unit` | string | `"msg/s"` | Canonical unit |
| `workload_id` | string | `"tcp_sustained_10k_1kib"` | Reproducibility reference |
| `transport` | string | `"tcp"` | tcp, http_sse, stdio, websocket |
| `scope` | string | `"per_node"` | per_node, per_connection, per_cluster |

### 4.2 Conditional Fields

**For time metrics**:
- `precision_us` (required): Raw microsecond value for consistency checking

**For sustained load metrics**:
- `duration_s` (required): Length of test run in seconds

**For memory metrics**:
- `workload_id` (required): Which workload produced this profile

---

## Section 5: Forbidden & Anti-Patterns

### 5.1 ABSOLUTE PROHIBITIONS (Validation Failures)

| Pattern | Why Forbidden | Replacement |
|---------|---------------|-----------  |
| `req/s` | Ambiguous: requests only? | `msg_per_s` |
| `MiB/conn` | Composite without clarity | `per_connection_heap_mib` or `per_connection_state_mib` |
| `memory_mb` | Unspecified component | Explicit: heap, state, or RSS |
| `memory_limit_mb` | Vague, no workload context | `per_node_total_rss_mib` with `workload_id` |
| `MB`, `KB`, `GB` | Decimal ambiguity at scale | `MiB`, `KiB`, `GiB` |
| Numeric without unit | Meaningless | Add `unit` field |
| Performance claim without workload_id | Not reproducible | Reference workload in `bench/workloads/` |

### 5.2 Anti-Patterns (Discouraged but Not Fatal)

| Anti-Pattern | Issue | Better |
|--------------|-------|--------|
| `0.00 ms` without precision | Sub-ms measurement ambiguous | Include `precision_us` |
| `throughput_req_s` | Mixes terminology | Use `throughput.msg_per_s` |
| Memory without scope | Unclear context | Add explicit `scope` field |
| Latency without measurement_point | Unclear what measured | Specify `"client_to_client"` etc. |

---

## Section 6: Validation Rules (Linter)

### 6.1 Rule: Unit Canonicalization
**Check**: All units from canonical list
**Violation Type**: `invalid_unit`
**Auto-Fix**: Conversion table applies

### 6.2 Rule: Scope Requirement
**Check**: Memory metrics MUST have scope
**Violation Type**: `missing_scope`
**Example**: `per_connection_heap_mib` implicitly scoped

### 6.3 Rule: Workload Reference Validation
**Check**: Every `workload_id` exists in `bench/workloads/`
**Violation Type**: `missing_workload_definition`
**Auto-Fail**: Cannot proceed without valid workload file

### 6.4 Rule: Transport Specification
**Check**: Throughput/latency MUST specify transport
**Violation Type**: `missing_transport`
**Applies To**: msg_per_s, latency percentiles, connection_rate_per_s

### 6.5 Rule: Time Metric Precision
**Check**: Time metrics (ms, µs, ns) MUST include raw precision
**Violation Type**: `missing_precision`
**Required**: `precision_us` field

### 6.6 Rule: Value-Precision Consistency
**Check**: Displayed value matches raw precision (within 1% tolerance)
**Violation Type**: `inconsistent_precision`
**Formula**: `|value_in_µs - precision_µs| ≤ 0.01 × precision_µs`

### 6.7 Rule: Memory Component Breakdown
**Check**: Prevent ambiguous "memory" field
**Violation Type**: `ambiguous_memory_metric`
**Requires**: Specific metrics (heap, state, RSS)

### 6.8 Rule: Prohibited Unit Patterns
**Check**: No `req/s`, `MiB/conn`, unqualified memory
**Violation Type**: `forbidden_pattern`
**Auto-Fix**: None (manual correction required)

---

## Section 7: Workload IDs

### 7.1 Naming Convention

Format: `{transport}_{pattern}_{scale}_{payload}`

Examples:
- `tcp_sustained_10k_1kib` → TCP, sustained, 10K conn, 1 KiB msg
- `http_sse_burst_50k_mixed` → HTTP SSE, burst, 50K conn, mixed sizes
- `stdio_sequential_1k_small` → stdio, sequential, 1K msg, small

### 7.2 Workload Definition Files

Location: `bench/workloads/{workload_id}.json`

```json
{
  "workload_id": "tcp_sustained_10k_1kib",
  "description": "10K TCP connections, 1 KiB messages, 30s sustained",
  "transport": "tcp",
  "pattern": "sustained",
  "duration_s": 30,
  "connections": 10000,
  "message_size_bytes": 1024,
  "expected_performance": {
    "throughput_msg_per_s": 50000,
    "p99_latency_ms": 150
  }
}
```

**Requirement**: Every performance claim MUST reference an existing workload_id.

---

## Section 8: Environment Specifications

Standard hardware profiles for reproducibility:

### prod_hw_spec_01 (Team Tier)
```json
{
  "cpu": "16 vCPU (AMD EPYC 7002)",
  "ram_gb": 64,
  "network": "10 Gbps",
  "disk": "NVMe SSD",
  "os": "Ubuntu 22.04 LTS"
}
```

### prod_hw_spec_02 (Enterprise Tier)
```json
{
  "cpu": "32 vCPU (AMD EPYC 7002)",
  "ram_gb": 128,
  "network": "40 Gbps",
  "disk": "NVMe SSD",
  "os": "Ubuntu 22.04 LTS"
}
```

### gov_fips_hardware (Government Tier)
```json
{
  "cpu": "16 vCPU (FIPS-certified)",
  "ram_gb": 64,
  "certification": "FIPS-140-2 Level 3",
  "disk": "Encrypted NVMe",
  "os": "RHEL 8 with FIPS mode"
}
```

---

## Section 9: Compliance & Certification

### 9.1 Metrology Compliance Marker

All plan files MUST include:

```json
{
  "metrology_compliance": {
    "version": "v1.5.0",
    "validated": "2026-01-27",
    "validator": "erlmcp_metrology_validator",
    "violations": 0
  }
}
```

---

## Section 10: Complete Example

### Full Benchmark Report (v1.5.0 Compliant)

```json
{
  "workload_id": "tcp_sustained_10k_1kib",
  "timestamp": "2026-01-27T18:30:00Z",
  "transport": "tcp",
  "duration_s": 30,

  "throughput": {
    "value": 50000,
    "unit": "msg_per_s",
    "workload_id": "tcp_sustained_10k_1kib",
    "transport": "tcp",
    "scope": "per_node"
  },

  "latency": {
    "p50_ms": 5.2,
    "p95_ms": 12.8,
    "p99_ms": 28.4,
    "workload_id": "tcp_sustained_10k_1kib",
    "transport": "tcp",
    "measurement_point": "client_to_client"
  },

  "memory": {
    "per_connection_heap_mib": 0.048,
    "per_connection_state_mib": 0.012,
    "per_node_base_overhead_mib": 150,
    "per_node_total_rss_mib": 1536,
    "workload_id": "tcp_sustained_10k_1kib",
    "connections": 10000,
    "scope": "per_node",
    "environment": "prod_hw_spec_01"
  },

  "metrology_compliance": {
    "version": "v1.5.0",
    "validated": "2026-01-27",
    "validator": "erlmcp_metrology_validator",
    "violations": 0
  }
}
```

---

## Section 11: Migration (v1.4.0 → v1.5.0)

Quick reference for upgrading:

| Old (v1.4.0) | New (v1.5.0) | Action |
|--------------|--------------|--------|
| `throughput_req_s: 450` | `throughput.msg_per_s: 900` | Double value; add context |
| `memory_limit_mb: 512` | `per_node_total_rss_mib: 512` | Rename; add scope |
| `p99_latency_ms: 150` | `latency.p99_ms: 150` | Nest; add transport |
| (missing) | `workload_id` | Create workload file |
| (missing) | `transport` | Specify tcp/http_sse/stdio |

**Checklist**:
- [ ] All `req/s` → `msg_per_s`
- [ ] All `MiB/conn` → component-specific
- [ ] Every metric has `workload_id`
- [ ] All workload_ids exist in `bench/workloads/`
- [ ] Every metric has `transport`
- [ ] Every metric has `scope`
- [ ] Compliance section added
- [ ] Validator passes (0 violations)

---

## Section 12: References

| Resource | Location |
|----------|----------|
| **Metrics Glossary** | `docs/metrology/METRICS_GLOSSARY.md` |
| **Migration Guide** | `docs/metrology/V1.5.0_MIGRATION_GUIDE.md` |
| **Validation Report** | `docs/metrology/V1.5.0_VALIDATION_REPORT.md` |
| **Validator Source** | `src/erlmcp_metrology_validator.erl` |
| **Workload Definitions** | `bench/workloads/*.json` |
| **Environment Specs** | `bench/environments/*.json` |

---

## Appendix A: Canonicalization Map

```erlang
#{
    <<"MB">> => <<"MiB">>,
    <<"KB">> => <<"KiB">>,
    <<"GB">> => <<"GiB">>,
    <<"milliseconds">> => <<"µs">>,
    <<"microseconds">> => <<"µs">>,
    <<"nanoseconds">> => <<"ns">>,
    <<"req/sec">> => <<"req/s">>,
    <<"requests/s">> => <<"req/s">>,
    <<"ops/sec">> => <<"ops/s">>,
    <<"msg/sec">> => <<"msg/s">>
}
```

---

## Appendix B: Violation Types

| Type | Severity | Auto-Fixable |
|------|----------|--------------|
| `invalid_unit` | ERROR | Yes |
| `missing_unit` | ERROR | No |
| `missing_workload_definition` | ERROR | No |
| `missing_transport` | ERROR | No |
| `missing_scope` | WARNING | Often implicit |
| `missing_precision` | WARNING | No |
| `inconsistent_precision` | WARNING | No |
| `ambiguous_memory_metric` | ERROR | No |
| `forbidden_pattern` | ERROR | No |
| `missing_required_field` | ERROR | No |

---

**Document Status**: CANONICAL (v1.5.0)
**Last Updated**: 2026-01-27
**Approver**: erlmcp Core Team
**Compliance Standard**: Zero-Defect Manufacturing
