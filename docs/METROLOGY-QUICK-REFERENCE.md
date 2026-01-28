# erlmcp Metrology Quick Reference Card

**Version:** 1.5.0 | **Agent:** erlang-architect | **Date:** 2026-01-27

---

## The 7 Stop-Line Rules (CHECK BEFORE COMMITTING)

```
❌ #1: Naked numbers        → ✅ Add unit: {dimension, symbol}
❌ #2: Ambiguous metrics    → ✅ Clarify msg/s vs req/s + add operations
❌ #3: Undefined scope      → ✅ Add scope: per_process/per_connection/per_node/per_cluster
❌ #4: Unanchored duration  → ✅ Add duration_seconds for all rates
❌ #5: Mixed context        → ✅ Add scope + transport + clarify in notes
❌ #6: Unlabeled memory     → ✅ Use memory_heap/memory_process/memory_ets/memory_total
❌ #7: Zero sample size     → ✅ Add sample_size + percentile for all percentiles
```

---

## Required Fields (ALL MEASUREMENTS)

```erlang
#{
    metric_name => throughput,              % From canonical registry
    value => 150000,                        % Numeric value
    unit => #{                              % REQUIRED (no naked numbers)
        dimension => rate,
        symbol => 'req/s'
    },
    scope => per_node,                      % REQUIRED (per_process/per_connection/per_node/per_cluster)
    transport => tcp,                       % REQUIRED (stdio/tcp/http/websocket/sse)
    duration_seconds => 60.0,               % REQUIRED for rates
    sample_size => 1000,                    % REQUIRED for percentiles
    workload_details => #{...},             % Recommended for context
    notes => <<"...">>                      % Optional clarifications
}
```

---

## Canonical Metric Names (18 Total)

### Throughput & Latency
- `throughput` → msg/s, req/s, ops/s (MUST specify in unit.symbol)
- `latency_p50`, `latency_p95`, `latency_p99`, `latency_p99_9` → ms (MUST add sample_size)

### Memory (MUST specify component)
- `memory_heap` → MiB (per_process) - Process heap only
- `memory_process` → MiB (per_process) - Heap + stack + mailbox
- `memory_ets` → MiB (per_node) - ETS tables
- `memory_total` → MiB/GiB (per_node) - OS RSS

### System
- `cpu_utilization` → % (per_node)
- `gc_pause_avg`, `gc_pause_max` → ms (MUST add sample_size)
- `gc_count` → count (per_process/per_node)
- `connection_count` → count (per_node)
- `message_queue_len` → count (per_process)
- `error_rate`, `success_rate` → % (MUST add sample_size)
- `bandwidth` → Mbps, MiB/s (per_connection/per_node)
- `payload_size` → B, KiB (wire bytes, pre-TLS)

---

## Unit Symbols (Standard Only)

### Time
- `ns` (nanoseconds), `µs` (microseconds), `ms` (milliseconds), `s` (seconds)

### Rate
- `msg/s` (all frames), `req/s` (requests only), `ops/s` (generic)

### Bytes (IEC Binary)
- `B`, `KiB` (1024 B), `MiB` (1024² B), `GiB` (1024³ B)

### Bandwidth
- `Mbps` (megabits/s, decimal), `MiB/s` (mebibytes/s, binary)

### Other
- `%` (percentage), `count`, `ratio`

---

## Scope Values (ALWAYS REQUIRED)

- `per_process` → Single Erlang process (one gen_server)
- `per_connection` → All resources for one client connection
- `per_node` → Entire Erlang VM (all processes)
- `per_cluster` → Distributed Erlang cluster (all nodes)

**Decision tree:**
```
Is it one gen_server's heap?          → per_process
Is it all processes for one client?   → per_connection
Is it the node's total RSS?           → per_node
Is it aggregated across N nodes?      → per_cluster
```

---

## Transport Values (ALWAYS REQUIRED)

- `stdio` → Standard I/O (pipes, no network)
- `tcp` → TCP sockets
- `http` → HTTP/1.1 or HTTP/2
- `websocket` → WebSocket over HTTP
- `sse` → Server-Sent Events
- `all` → Aggregated across transports (rare)

---

## Common Patterns

### Throughput (msg/s)
```erlang
#{
    metric_name => throughput,
    value => 150000,
    unit => #{dimension => rate, symbol => 'msg/s'},
    scope => per_node,
    transport => tcp,
    duration_seconds => 60.0,
    notes => <<"All JSON-RPC frames: requests + responses + notifications">>
}
```

### Throughput (req/s)
```erlang
#{
    metric_name => throughput,
    value => 150000,
    unit => #{dimension => rate, symbol => 'req/s'},
    scope => per_node,
    transport => tcp,
    duration_seconds => 60.0,
    sample_size => 1000,
    workload_details => #{
        json_rpc_operations => [<<"tools/list">>, <<"tools/call">>],
        request_pattern => constant
    },
    notes => <<"Client requests only, excludes responses">>
}
```

### Latency (p99)
```erlang
#{
    metric_name => latency_p99,
    value => 2.1,
    unit => #{dimension => time, symbol => ms},
    scope => per_connection,
    transport => tcp,
    percentile => 99.0,
    sample_size => 10000,
    duration_seconds => 60.0,
    notes => <<"Client-side measurement, includes network RTT">>
}
```

### Memory (process heap)
```erlang
#{
    metric_name => memory_heap,
    value => 24,
    unit => #{dimension => bytes, symbol => 'MiB'},
    scope => per_process,
    notes => <<"Heap for one gen_server worker">>
}
```

### Memory (node total)
```erlang
#{
    metric_name => memory_total,
    value => 320,
    unit => #{dimension => bytes, symbol => 'MiB'},
    scope => per_node,
    notes => <<"OS-reported RSS, includes Erlang VM + all processes">>
}
```

---

## Helper API (Test Usage)

### Emit Measurement in Test
```erlang
-module(my_benchmark_SUITE).
-include_lib("common_test/include/ct.hrl").

test_throughput(_Config) ->
    {ok, ThroughputReqSec} = run_benchmark(),

    % Emit v1.5.0-compliant measurement
    metrology_helpers:emit_measurement(throughput, ThroughputReqSec, #{
        unit => #{dimension => rate, symbol => 'req/s'},
        scope => per_node,
        transport => tcp,
        duration_seconds => 60.0,
        sample_size => 1000,
        workload_details => #{
            concurrent_connections => 1000,
            message_size_bytes => 4096,
            request_pattern => constant,
            json_rpc_operations => [<<"tools/list">>, <<"tools/call">>]
        },
        notes => <<"Sustained load, excludes 5s warmup">>
    }),

    ok.
```

---

## Validation Commands

### Validate Single Artifact
```bash
rebar3 tcps metrology_validate --file=dist/evidence/v1.5.0/benchmarks/throughput.json
```

### Validate Directory
```bash
rebar3 tcps metrology_validate --dir=dist/evidence/v1.5.0/
```

### Generate Report
```bash
rebar3 tcps metrology_validate --report=dist/evidence/v1.5.0/metrology_compliance_report.md
```

### CI Integration
```bash
# In .github/workflows/ci.yml
rebar3 tcps metrology_validate || exit 1
```

---

## Disambiguation Rules

### Rule 1: msg/s vs req/s
- **msg/s**: All JSON-RPC frames (requests + responses + notifications)
- **req/s**: Client requests only (subset of msg/s)
- **Fix**: Specify in `unit.symbol` + add `workload_details.json_rpc_operations`

### Rule 2: Connection vs Session
- **Connection**: Transport-level link (TCP socket, WebSocket)
- **Session**: MCP protocol conversation (from initialize to close)
- **Fix**: Use `scope: per_connection` + `transport: tcp` + clarify in notes

### Rule 3: Memory Components
- **memory_heap**: Process heap only
- **memory_process**: Heap + stack + mailbox
- **memory_ets**: ETS tables
- **memory_total**: OS RSS (includes VM overhead)
- **Fix**: NEVER use generic "memory" - always specify component

### Rule 4: Latency Types
- **Transport latency**: Socket RTT (network)
- **Protocol latency**: JSON-RPC request → response
- **Application latency**: Tool invocation time
- **Fix**: Clarify in `notes` which layer is measured

---

## Quality Gates (Default)

```erlang
#{
    throughput_min_req_s => 95000,
    latency_p99_max_ms => 5.0,
    memory_max_mb => 512,
    error_rate_max_percent => 0.01,
    regression_tolerance_percent => 5.0
}
```

**Override in artifact:**
```json
{
  "quality_gates": {
    "throughput_min_req_s": 100000,
    "latency_p99_max_ms": 3.0
  }
}
```

---

## Violation Examples

### ❌ Missing Unit
```erlang
#{metric_name => throughput, value => 150000}
% ERROR: missing_unit
```

### ✅ Fixed
```erlang
#{
    metric_name => throughput,
    value => 150000,
    unit => #{dimension => rate, symbol => 'req/s'}
}
```

---

### ❌ Undefined Scope
```erlang
#{metric_name => memory_heap, value => 24}
% ERROR: undefined_scope
```

### ✅ Fixed
```erlang
#{
    metric_name => memory_heap,
    value => 24,
    unit => #{dimension => bytes, symbol => 'MiB'},
    scope => per_process
}
```

---

### ❌ Unanchored Duration
```erlang
#{
    metric_name => throughput,
    value => 150000,
    unit => #{dimension => rate, symbol => 'req/s'}
}
% ERROR: unanchored_duration (rate without time window)
```

### ✅ Fixed
```erlang
#{
    metric_name => throughput,
    value => 150000,
    unit => #{dimension => rate, symbol => 'req/s'},
    duration_seconds => 60.0
}
```

---

### ❌ Zero Sample Size
```erlang
#{
    metric_name => latency_p99,
    value => 2.1,
    unit => #{dimension => time, symbol => ms}
}
% ERROR: zero_sample_size (percentile without N)
```

### ✅ Fixed
```erlang
#{
    metric_name => latency_p99,
    value => 2.1,
    unit => #{dimension => time, symbol => ms},
    percentile => 99.0,
    sample_size => 10000
}
```

---

## Artifact Template

```json
{
  "schema_version": "1.5.0",
  "artifact_type": "evidence",
  "workload_id": "throughput_baseline_tcp",
  "metadata": {
    "timestamp": "2026-01-27T20:51:45Z",
    "environment": "ci",
    "erlang_version": "25.3",
    "hardware": {
      "cpu_cores": 4,
      "memory_gb": 16,
      "architecture": "x86_64"
    },
    "description": "Baseline throughput test with 1K concurrent TCP connections"
  },
  "measurements": [
    {
      "metric_name": "throughput",
      "value": 150000,
      "unit": {"dimension": "rate", "symbol": "req/s"},
      "scope": "per_node",
      "transport": "tcp",
      "duration_seconds": 60.0,
      "sample_size": 1000,
      "workload_details": {
        "concurrent_connections": 1000,
        "message_size_bytes": 4096,
        "request_pattern": "constant",
        "json_rpc_operations": ["tools/list", "tools/call"]
      },
      "notes": "Sustained load, 4KB payload, excludes 5s warmup"
    }
  ],
  "quality_gates": {
    "throughput_min_req_s": 95000,
    "latency_p99_max_ms": 5.0,
    "memory_max_mb": 512,
    "error_rate_max_percent": 0.01
  },
  "status": "PASS",
  "violations": []
}
```

---

## Resources

- **Schema:** `/Users/sac/erlmcp/shapes/metrology.schema.json`
- **Glossary:** `/Users/sac/erlmcp/docs/metrology-glossary.md`
- **Architecture:** `/Users/sac/erlmcp/docs/v1.5.0-metrology-validation-architecture.md`
- **Index:** `/Users/sac/erlmcp/docs/v1.5.0-METROLOGY-INDEX.md`

---

## Quick Lookup Table

| Need to measure... | Use metric_name | Unit | Scope | Required fields |
|--------------------|----------------|------|-------|-----------------|
| Requests/second | throughput | req/s | per_node | duration_seconds, sample_size, workload_details |
| All messages/second | throughput | msg/s | per_node | duration_seconds |
| 99th percentile latency | latency_p99 | ms | per_connection | sample_size, percentile, duration_seconds |
| Process heap memory | memory_heap | MiB | per_process | - |
| Node total memory | memory_total | MiB | per_node | - |
| CPU usage | cpu_utilization | % | per_node | duration_seconds |
| GC pause time | gc_pause_avg | ms | per_node | sample_size |
| Error rate | error_rate | % | per_node | sample_size, duration_seconds |

---

**Version:** 1.5.0 | **Status:** Reference Card | **Print:** 2-sided, keep at desk

For detailed explanations, see [Glossary](metrology-glossary.md) or [Architecture](v1.5.0-metrology-validation-architecture.md).
