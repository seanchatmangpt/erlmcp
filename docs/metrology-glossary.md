# erlmcp Metrology Glossary

**Version:** 1.5.0
**Purpose:** Canonical definitions for all performance measurement terms
**Status:** Reference Document

---

## Introduction

This glossary defines **unambiguous terms** for performance measurement in erlmcp. Every term has:
1. **Canonical definition** (what it means)
2. **Scope constraint** (where it applies)
3. **Unit requirement** (how to measure it)
4. **Examples** (correct vs incorrect usage)

**Use this when:**
- Writing benchmark specifications
- Analyzing performance results
- Reporting to stakeholders
- Comparing measurements across versions

---

## Core Concepts

### Artifact Type

**Definition:** Classification of metrology document.

**Types:**
- **plan**: Test specification (before execution)
- **evidence**: Test results (after execution)
- **baseline**: Reference measurement for comparison
- **comparison**: Regression analysis (baseline vs current)

**Schema field:** `artifact_type`

---

### Connection

**Definition:** Transport-level link between client and server.

**Disambiguations:**
- **TCP connection**: Socket-level link (one per `gen_tcp:accept/1`)
- **WebSocket connection**: WS handshake + upgrade (one per ws:connect)
- **HTTP connection**: Persistent or non-persistent (connection pooling)
- **stdio connection**: One per process lifetime (erlmcp_transport_stdio)

**NOT the same as:**
- Session (MCP protocol-level)
- Request (single operation)

**Scope:** `per_connection`

**Example:**
```json
{
  "metric_name": "memory_process",
  "value": 24,
  "unit": {"dimension": "bytes", "symbol": "MiB"},
  "scope": "per_connection",
  "transport": "tcp",
  "notes": "Memory for one TCP connection handler process"
}
```

---

### Duration

**Definition:** Time window over which measurement was taken.

**Required for:**
- All rate metrics (throughput, bandwidth, ops/s)
- Sustained load tests (vs burst)

**Unit:** Always seconds (`s` or fractional)

**Schema field:** `duration_seconds`

**Example:**
```json
{
  "metric_name": "throughput",
  "value": 150000,
  "unit": {"dimension": "rate", "symbol": "msg/s"},
  "duration_seconds": 60.0,
  "notes": "Sustained load, excludes 5s warmup"
}
```

**Anti-pattern:**
```json
// ❌ WRONG: No duration_seconds
{
  "metric_name": "throughput",
  "value": 150000,
  "unit": {"dimension": "rate", "symbol": "msg/s"}
}
```

---

### JSON-RPC Operation

**Definition:** MCP protocol method invoked by client.

**Standard Operations:**
- `initialize` - Handshake
- `tools/list` - List available tools
- `tools/call` - Invoke tool
- `resources/list` - List resources
- `resources/read` - Read resource
- `resources/subscribe` - Subscribe to updates
- `prompts/list` - List prompts
- `prompts/get` - Get prompt template
- `ping` - Health check
- `notifications/cancelled` - Cancellation
- `logging/setLevel` - Set log level

**Used in:** `workload_details.json_rpc_operations`

**Example:**
```json
{
  "metric_name": "throughput",
  "value": 150000,
  "unit": {"dimension": "rate", "symbol": "req/s"},
  "workload_details": {
    "json_rpc_operations": ["tools/list", "tools/call"],
    "request_pattern": "constant"
  },
  "notes": "Only client requests, excludes server responses"
}
```

---

### Latency

**Definition:** Time between request sent and response received.

**Disambiguations:**
- **Transport latency**: Socket RTT (send → recv, transport layer)
- **Protocol latency**: JSON-RPC request → response (application layer)
- **Application latency**: Tool invocation time (business logic)

**Measurement points:**
- **Client-side**: Measures full round-trip (includes network + server processing)
- **Server-side**: Measures processing only (excludes network)

**Percentiles:** p50, p95, p99, p99.9 (MUST include sample_size)

**Units:** ns, µs, ms, s (prefer milliseconds for network operations)

**Schema fields:**
- `metric_name`: latency_p50, latency_p95, latency_p99, latency_p99_9
- `percentile`: 50.0, 95.0, 99.0, 99.9
- `sample_size`: Number of requests measured

**Example:**
```json
{
  "metric_name": "latency_p99",
  "value": 2.1,
  "unit": {"dimension": "time", "symbol": "ms"},
  "percentile": 99.0,
  "sample_size": 10000,
  "scope": "per_connection",
  "transport": "tcp",
  "duration_seconds": 60.0,
  "notes": "Client-side measurement, includes network RTT"
}
```

---

### Memory

**Definition:** RAM allocated to Erlang processes, ETS tables, or OS.

**Components (MUST specify):**
- **memory_heap**: Erlang process heap only (per_process)
- **memory_process**: Heap + stack + mailbox + internal (per_process)
- **memory_ets**: ETS table memory (per_table or per_node)
- **memory_total**: OS-reported RSS (per_node, includes VM overhead)

**Units:** B, KiB, MiB, GiB (prefer MiB for process-level, GiB for node-level)

**Scope:**
- `per_process`: Single gen_server or worker
- `per_connection`: All processes for one connection
- `per_node`: Entire Erlang VM

**Example (process):**
```json
{
  "metric_name": "memory_heap",
  "value": 24,
  "unit": {"dimension": "bytes", "symbol": "MiB"},
  "scope": "per_process",
  "notes": "Heap for one gen_server worker"
}
```

**Example (node):**
```json
{
  "metric_name": "memory_total",
  "value": 320,
  "unit": {"dimension": "bytes", "symbol": "MiB"},
  "scope": "per_node",
  "notes": "OS-reported RSS, includes Erlang VM + all processes"
}
```

**Anti-pattern:**
```json
// ❌ WRONG: Generic "memory" without component or scope
{
  "metric_name": "memory",
  "value": 320
}
```

---

### Message

**Definition:** Single JSON-RPC frame on the wire.

**Types:**
- **Request**: Client → Server (has `id` field)
- **Response**: Server → Client (has `result` or `error`)
- **Notification**: Either direction (no `id` field)

**Frame count:**
- One request-response pair = 2 messages
- One notification = 1 message

**Throughput unit:** `msg/s` (all frames) or `req/s` (client requests only)

**NOT the same as:**
- Request (subset of messages)
- Payload (message body, excludes framing)

**Example (all frames):**
```json
{
  "metric_name": "throughput",
  "value": 300000,
  "unit": {"dimension": "rate", "symbol": "msg/s"},
  "notes": "Includes requests + responses + notifications"
}
```

**Example (requests only):**
```json
{
  "metric_name": "throughput",
  "value": 150000,
  "unit": {"dimension": "rate", "symbol": "req/s"},
  "workload_details": {
    "json_rpc_operations": ["tools/call"]
  },
  "notes": "Client requests only, excludes responses"
}
```

---

### Payload Size

**Definition:** Message body size in bytes (wire format).

**Measurement point:** After JSON encoding, before TLS encryption

**Includes:**
- JSON structure overhead (`{`, `}`, `[`, `]`, `:`, `,`)
- Field names and values
- Whitespace (if any)

**Excludes:**
- HTTP headers (if HTTP transport)
- TLS overhead
- TCP/IP headers

**Units:** B, KiB, MiB (prefer bytes for individual messages)

**Schema field:** `workload_details.message_size_bytes`

**Example:**
```json
{
  "metric_name": "payload_size",
  "value": 4096,
  "unit": {"dimension": "bytes", "symbol": "B"},
  "notes": "JSON-RPC tools/call request body, pre-TLS"
}
```

---

### Request

**Definition:** Client-initiated JSON-RPC message with `id` field.

**Characteristics:**
- Always has `method` field
- Always has `id` field (for correlation)
- Expects response (unless cancelled)

**NOT the same as:**
- Message (requests are a subset)
- Notification (no `id` field)

**Throughput unit:** `req/s` (requests per second)

**Example:**
```json
{
  "metric_name": "throughput",
  "value": 150000,
  "unit": {"dimension": "rate", "symbol": "req/s"},
  "workload_details": {
    "json_rpc_operations": ["tools/list", "tools/call"]
  },
  "notes": "Client requests only, excludes responses/notifications"
}
```

---

### Sample Size

**Definition:** Number of measurements in statistical aggregate.

**Required for:**
- Percentile metrics (p50, p95, p99, p99.9)
- Average/mean calculations
- Variance/standard deviation

**Schema field:** `sample_size`

**Minimum:** 1 (but recommend ≥30 for statistical significance)

**Example:**
```json
{
  "metric_name": "latency_p99",
  "value": 2.1,
  "unit": {"dimension": "time", "symbol": "ms"},
  "percentile": 99.0,
  "sample_size": 10000,
  "notes": "10K requests over 60s (sustained load)"
}
```

**Anti-pattern:**
```json
// ❌ WRONG: Percentile without sample_size
{
  "metric_name": "latency_p99",
  "value": 2.1,
  "unit": {"dimension": "time", "symbol": "ms"}
}
```

---

### Scope

**Definition:** Level of aggregation for measurement.

**Values:**
- **per_process**: Single Erlang process (one gen_server, one worker)
- **per_connection**: All resources for one client connection
- **per_node**: Entire Erlang VM (all processes)
- **per_cluster**: Distributed Erlang cluster (all nodes)

**Schema field:** `scope`

**Required for:** ALL measurements (no default)

**Example decision tree:**
```
Is it one gen_server's heap?          → per_process
Is it all processes for one client?   → per_connection
Is it the node's total RSS?           → per_node
Is it aggregated across N nodes?      → per_cluster
```

**Examples:**
```json
// Process-level memory
{
  "metric_name": "memory_heap",
  "value": 24,
  "unit": {"dimension": "bytes", "symbol": "MiB"},
  "scope": "per_process"
}

// Node-level throughput
{
  "metric_name": "throughput",
  "value": 150000,
  "unit": {"dimension": "rate", "symbol": "req/s"},
  "scope": "per_node"
}
```

---

### Session

**Definition:** MCP protocol-level conversation (from `initialize` to close).

**Disambiguations:**
- **1:1 with connection** (stdio, TCP, WebSocket)
- **N:1 with connection** (HTTP with connection pooling)

**Lifecycle:**
1. Client sends `initialize`
2. Server responds with capabilities
3. Multiple request-response exchanges
4. Either side closes (explicit or timeout)

**NOT the same as:**
- Connection (transport-level)
- Request (single operation)

**Scope:** `per_session` (if distinct from per_connection)

**Example:**
```json
{
  "metric_name": "session_duration",
  "value": 300,
  "unit": {"dimension": "time", "symbol": "s"},
  "scope": "per_session",
  "notes": "From initialize to close, HTTP transport with pooling"
}
```

---

### Throughput

**Definition:** Rate of operations per unit time.

**Disambiguations:**
- **throughput (msg/s)**: All JSON-RPC frames (requests + responses + notifications)
- **throughput (req/s)**: Client requests only (subset of msg/s)
- **throughput (ops/s)**: Generic operations (use when neither msg nor req applies)

**Rule:** MUST specify unit.symbol as "msg/s", "req/s", or "ops/s"

**Schema field:** `metric_name: "throughput"`

**Context required:**
- `workload_details.json_rpc_operations` (which operations?)
- `duration_seconds` (sustained or burst?)
- `workload_details.request_pattern` (constant, burst, ramp?)

**Example (all frames):**
```json
{
  "metric_name": "throughput",
  "value": 300000,
  "unit": {"dimension": "rate", "symbol": "msg/s"},
  "duration_seconds": 60.0,
  "notes": "All JSON-RPC frames: requests + responses + notifications"
}
```

**Example (requests only):**
```json
{
  "metric_name": "throughput",
  "value": 150000,
  "unit": {"dimension": "rate", "symbol": "req/s"},
  "duration_seconds": 60.0,
  "workload_details": {
    "json_rpc_operations": ["tools/list", "tools/call"],
    "request_pattern": "constant"
  },
  "notes": "Client requests only, excludes responses"
}
```

---

### Transport

**Definition:** Underlying communication mechanism.

**Values:**
- **stdio**: Standard I/O (pipes, no network)
- **tcp**: TCP sockets (raw or with framing)
- **http**: HTTP/1.1 or HTTP/2
- **websocket**: WebSocket over HTTP
- **sse**: Server-Sent Events (HTTP streaming)

**Schema field:** `transport`

**Required for:** ALL measurements (provides context)

**Example:**
```json
{
  "metric_name": "latency_p99",
  "value": 2.1,
  "unit": {"dimension": "time", "symbol": "ms"},
  "transport": "tcp",
  "notes": "TCP transport adds ~0.5ms vs stdio"
}
```

---

### Unit

**Definition:** Measurement scale and dimension.

**Structure:**
```json
{
  "dimension": "time" | "rate" | "bytes" | "bandwidth" | "percentage" | "count" | "dimensionless",
  "symbol": "ns" | "µs" | "ms" | "s" | "msg/s" | "req/s" | "ops/s" | "B" | "KiB" | "MiB" | "GiB" | "Mbps" | "MiB/s" | "%" | "count" | "ratio"
}
```

**Schema field:** `unit`

**Required for:** ALL measurements (no naked numbers)

**Time units:**
- ns (nanoseconds) - 10^-9 s
- µs (microseconds) - 10^-6 s
- ms (milliseconds) - 10^-3 s
- s (seconds)

**Rate units:**
- msg/s (messages per second)
- req/s (requests per second)
- ops/s (operations per second)

**Byte units (IEC binary):**
- B (bytes)
- KiB (kibibytes) - 1024 B
- MiB (mebibytes) - 1024^2 B
- GiB (gibibytes) - 1024^3 B

**Bandwidth units:**
- Mbps (megabits per second, decimal)
- MiB/s (mebibytes per second, binary)

**Example:**
```json
{
  "metric_name": "latency_p99",
  "value": 2.1,
  "unit": {"dimension": "time", "symbol": "ms"}
}
```

---

### Workload

**Definition:** Pattern of operations applied to system under test.

**Patterns:**
- **constant**: Steady request rate (e.g., 1000 req/s)
- **burst**: Short spike (e.g., 10K req in 1s, then idle)
- **ramp**: Gradual increase (e.g., 0 → 10K over 60s)
- **sawtooth**: Ramp up + drop (e.g., 0 → 10K → 0, repeat)
- **random**: Poisson arrival (e.g., λ=1000 req/s)

**Schema field:** `workload_details.request_pattern`

**Example:**
```json
{
  "workload_details": {
    "request_pattern": "constant",
    "concurrent_connections": 1000,
    "message_size_bytes": 4096,
    "json_rpc_operations": ["tools/list", "tools/call"]
  }
}
```

---

## Disambiguation Rules

### Rule 1: Always Specify Units

**Violation:** Naked numbers
**Fix:** Add `unit` object with `dimension` and `symbol`

**Before:**
```json
{"metric_name": "throughput", "value": 150000}
```

**After:**
```json
{
  "metric_name": "throughput",
  "value": 150000,
  "unit": {"dimension": "rate", "symbol": "msg/s"}
}
```

---

### Rule 2: Disambiguate msg/s vs req/s

**Violation:** Ambiguous throughput
**Fix:** Specify `unit.symbol` and add `workload_details.json_rpc_operations`

**Before:**
```json
{
  "metric_name": "throughput",
  "value": 150000,
  "unit": {"dimension": "rate", "symbol": "msg/s"}
}
```

**After:**
```json
{
  "metric_name": "throughput",
  "value": 150000,
  "unit": {"dimension": "rate", "symbol": "req/s"},
  "workload_details": {
    "json_rpc_operations": ["tools/call"]
  },
  "notes": "Client requests only, excludes responses"
}
```

---

### Rule 3: Always Specify Scope

**Violation:** Undefined scope
**Fix:** Add `scope` field (per_process, per_connection, per_node, per_cluster)

**Before:**
```json
{"metric_name": "memory_heap", "value": 24}
```

**After:**
```json
{
  "metric_name": "memory_heap",
  "value": 24,
  "unit": {"dimension": "bytes", "symbol": "MiB"},
  "scope": "per_process"
}
```

---

### Rule 4: Anchor Rates with Duration

**Violation:** Unanchored duration
**Fix:** Add `duration_seconds` for all rate metrics

**Before:**
```json
{
  "metric_name": "throughput",
  "value": 150000,
  "unit": {"dimension": "rate", "symbol": "msg/s"}
}
```

**After:**
```json
{
  "metric_name": "throughput",
  "value": 150000,
  "unit": {"dimension": "rate", "symbol": "msg/s"},
  "duration_seconds": 60.0,
  "notes": "Sustained load, excludes warmup"
}
```

---

### Rule 5: Disambiguate Connection vs Session

**Violation:** Mixed context
**Fix:** Specify `scope` + `transport` + clarify in `notes`

**Before:**
```json
{"metric_name": "latency_p99", "value": 2.1}
```

**After:**
```json
{
  "metric_name": "latency_p99",
  "value": 2.1,
  "unit": {"dimension": "time", "symbol": "ms"},
  "scope": "per_connection",
  "transport": "tcp",
  "notes": "Transport-level RTT, single TCP connection"
}
```

---

### Rule 6: Label Memory Components

**Violation:** Unlabeled memory
**Fix:** Use specific metric_name (memory_heap, memory_process, memory_ets, memory_total)

**Before:**
```json
{"metric_name": "memory", "value": 320}
```

**After:**
```json
{
  "metric_name": "memory_total",
  "value": 320,
  "unit": {"dimension": "bytes", "symbol": "MiB"},
  "scope": "per_node",
  "notes": "OS-reported RSS, includes VM + all processes"
}
```

---

### Rule 7: Include Sample Size for Percentiles

**Violation:** Zero sample size
**Fix:** Add `sample_size` and `percentile` fields

**Before:**
```json
{"metric_name": "latency_p99", "value": 2.1}
```

**After:**
```json
{
  "metric_name": "latency_p99",
  "value": 2.1,
  "unit": {"dimension": "time", "symbol": "ms"},
  "percentile": 99.0,
  "sample_size": 10000,
  "duration_seconds": 60.0
}
```

---

## Quick Reference Table

| Metric | Unit | Scope | Required Fields | Example Value |
|--------|------|-------|-----------------|---------------|
| throughput | msg/s, req/s, ops/s | per_node, per_cluster | duration_seconds, workload_details | 150000 msg/s |
| latency_p99 | ms | per_connection, per_node | sample_size, percentile, duration_seconds | 2.1 ms |
| memory_heap | MiB | per_process | - | 24 MiB |
| memory_total | MiB, GiB | per_node | - | 320 MiB |
| cpu_utilization | % | per_node | duration_seconds | 69% |
| gc_pause_avg | ms | per_process, per_node | sample_size | 4.8 ms |
| connection_count | count | per_node | - | 1000 |
| error_rate | % | per_node, per_connection | sample_size, duration_seconds | 0.005% |
| payload_size | B, KiB | per_message | - | 4096 B |

---

## References

- **erlmcp v1.5.0 Schema:** shapes/metrology.schema.json
- **Architecture:** docs/v1.5.0-metrology-validation-architecture.md
- **MCP Spec:** https://spec.modelcontextprotocol.io/
- **Erlang/OTP Docs:** https://www.erlang.org/doc/
- **IEEE 1541-2021:** Binary prefixes (KiB, MiB, GiB)

---

**Version:** 1.5.0
**Last Updated:** 2026-01-27
**Maintained By:** erlang-architect

Use this glossary when in doubt. When adding new metrics, update both this file and shapes/metrology.schema.json.
