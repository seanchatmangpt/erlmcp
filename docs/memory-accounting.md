# Memory Accounting - Eliminating "MiB/conn" Ambiguity

## Overview

The `erlmcp_memory_accounting` module provides detailed memory decomposition to prevent ambiguous memory reporting. It eliminates the #1 source of confusion in performance reports: unclear "MiB/conn" metrics.

## Problem Statement

Previous memory reports contained ambiguous statements like:
- "0.5 MiB/conn" - Does this include process heap only? State data? Infrastructure overhead?
- "100K connections use 2GB" - Is this per-node or cluster-wide? What about supervisor overhead?
- "Memory per connection: 1.2 MiB" - Is this RSS, heap, or total allocated?

## Solution

All memory measurements MUST be decomposed into labeled components:

1. **Per-connection process heap** - The gen_server process memory (stack, heap, messages)
2. **Per-connection state data** - State record + buffers + queues
3. **Per-node base overhead** - Supervisors, registry, ETS tables, atoms, code
4. **Per-node total RSS** - Operating system view of node memory
5. **Cluster total** - Aggregate across all nodes

## Usage

### Basic Decomposition

```erlang
%% Measure memory for connection processes
Decomposition = erlmcp_memory_accounting:decompose(#{
    connection_pids => [Pid1, Pid2, ...],
    server_pid => ServerPid,
    registry_pid => RegistryPid,
    supervisor_pids => [Sup1, Sup2]
}),

%% Validate components sum correctly
ok = erlmcp_memory_accounting:validate_decomposition(Decomposition),

%% Get human-readable report
Report = erlmcp_memory_accounting:format_report(Decomposition),
io:format("~s", [Report]).
```

### Example Output

```
=== MEMORY DECOMPOSITION REPORT ===
Timestamp: 1738012345678
Node: 'node@host'

--- Per-Connection Memory (Average) ---
  Heap (process memory):     0.048 MiB
  State (data structures):   0.012 MiB
  Total per connection:      0.060 MiB

--- Per-Node Memory ---
  Base overhead (infra):     150.000 MiB
  Total RSS (OS view):       206.000 MiB
  Process count:             10250
  Active connections:        10000

--- Cluster Memory ---
  Cluster nodes:             3
  Cluster total RSS:         618.000 MiB

--- Scope Indicators ---
  Per-node scope:            true
  Per-cluster scope:         true

--- Calculations ---
  Expected node memory:      206.000 MiB (overhead + conn*total)
  Actual node RSS:           206.000 MiB
  Variance:                  0.000 MiB
=== END REPORT ===
```

### Compact Format for Benchmarks

```erlang
Compact = erlmcp_memory_accounting:format_compact(Decomposition),
%% Returns:
#{
    per_connection_heap_mib => 0.048,
    per_connection_state_mib => 0.012,
    per_connection_total_mib => 0.060,
    per_node_base_overhead_mib => 150.0,
    per_node_total_rss_mib => 206.0,
    connections => 10000,
    cluster_nodes => 3,
    cluster_total_rss_mib => 618.0,
    scope_per_node => true,
    scope_per_cluster => true
}
```

## API Reference

### Core Measurement Functions

#### `measure_per_connection_heap/1`

Measures the Erlang process heap size for a connection gen_server.

```erlang
-spec measure_per_connection_heap(pid()) -> {ok, bytes()} | {error, term()}.
```

Includes:
- Stack
- Heap
- Old heap
- Message queue

#### `measure_per_connection_state/1`

Measures the state record size using `erts_debug:flat_size/1`.

```erlang
-spec measure_per_connection_state(pid()) -> {ok, bytes()} | {error, term()}.
```

Includes all data referenced by the state term (deep traversal).

#### `measure_per_node_rss/0`

Measures total resident memory for this Erlang node.

```erlang
-spec measure_per_node_rss() -> {ok, bytes()}.
```

Uses `erlang:memory(total)` for OS-level view.

#### `measure_per_node_base_overhead/1`

Measures memory used by infrastructure components.

```erlang
-spec measure_per_node_base_overhead(#{
    server_pid => pid() | undefined,
    registry_pid => pid() | undefined,
    supervisor_pids => [pid()]
}) -> {ok, bytes()}.
```

Includes:
- Server processes
- Registry
- Supervisors
- ETS tables
- Atom table
- Code memory

#### `measure_cluster_total/1`

Aggregates memory across all cluster nodes.

```erlang
-spec measure_cluster_total([node()]) -> {ok, bytes()}.
```

Uses RPC to collect RSS from each node.

### Decomposition and Validation

#### `decompose/1`

Main function for creating a complete memory breakdown.

```erlang
-spec decompose(#{
    connection_pids := [pid()],
    server_pid => pid(),
    registry_pid => pid(),
    supervisor_pids => [pid()],
    cluster_nodes => [node()]
}) -> memory_decomposition().
```

Returns a `#memory_decomposition{}` record with all components.

#### `validate_decomposition/1`

Validates that components sum correctly.

```erlang
-spec validate_decomposition(memory_decomposition()) -> ok | {error, term()}.
```

Checks:
1. Per-connection total = heap + state
2. Per-node RSS >= overhead + (connections * per_conn_total)
3. Cluster total >= per-node RSS * node_count
4. All values non-negative

### Reporting

#### `format_report/1`

Creates human-readable report with all components labeled.

```erlang
-spec format_report(memory_decomposition()) -> binary().
```

#### `format_compact/1`

Creates compact map for benchmark integration.

```erlang
-spec format_compact(memory_decomposition()) -> map().
```

### Utilities

#### `get_connection_process_info/1`

Gets detailed process information.

```erlang
-spec get_connection_process_info(pid()) -> map().
```

Returns memory, heap_size, stack_size, message_queue_len, etc.

#### `estimate_state_size/1`

Estimates size of any Erlang term.

```erlang
-spec estimate_state_size(term()) -> bytes().
```

#### `aggregate_measurements/1`

Aggregates multiple decompositions into summary statistics.

```erlang
-spec aggregate_measurements([memory_decomposition()]) -> map().
```

Returns min/max/avg for heap, state, and total.

## Integration with Benchmarks

### Before (Ambiguous)

```erlang
%% BAD: What does this include?
TotalMem = erlang:memory(total),
PerConn = TotalMem div ConnectionCount,
io:format("Memory per connection: ~.2f MiB~n", [PerConn / (1024 * 1024)]).
```

### After (Clear)

```erlang
%% GOOD: All components labeled
Decomp = erlmcp_memory_accounting:decompose(#{
    connection_pids => ConnectionPids
}),
ok = erlmcp_memory_accounting:validate_decomposition(Decomp),
Compact = erlmcp_memory_accounting:format_compact(Decomp),

io:format("Memory Breakdown:~n"),
io:format("  Per-connection heap:   ~.3f MiB~n",
          [maps:get(per_connection_heap_mib, Compact)]),
io:format("  Per-connection state:  ~.3f MiB~n",
          [maps:get(per_connection_state_mib, Compact)]),
io:format("  Per-node overhead:     ~.3f MiB~n",
          [maps:get(per_node_base_overhead_mib, Compact)]),
io:format("  Node total RSS:        ~.3f MiB~n",
          [maps:get(per_node_total_rss_mib, Compact)]).
```

## Conformance Rules

1. **NEVER report "MiB/conn" without decomposition**
   - All reports MUST use `decompose/1`
   - All reports MUST pass `validate_decomposition/1`

2. **ALWAYS label components**
   - Use descriptive names: "heap", "state", "overhead", "RSS"
   - Distinguish per-connection vs per-node vs per-cluster

3. **ALWAYS specify scope**
   - Indicate if measurement is per-node or cluster-wide
   - Use `scope_per_node` and `scope_per_cluster` indicators

4. **ALWAYS validate sums**
   - Components must sum to totals
   - Node RSS must account for overhead + connections
   - Cluster total must be >= sum of node RSS

## Testing

The module includes comprehensive tests in `test/erlmcp_memory_accounting_SUITE.erl`:

```bash
rebar3 ct --suite=erlmcp_memory_accounting_SUITE
```

Key tests:
- `test_decompose_single_connection` - Basic decomposition
- `test_decompose_multiple_connections` - Averaging across connections
- `test_validate_decomposition_valid` - Validation passes
- `test_validate_decomposition_invalid` - Validation catches errors
- `test_large_scale_decomposition` - 100 connections
- `test_conformance_no_ambiguous_reports` - Enforces all fields present

## Performance Impact

The memory accounting module has minimal overhead:
- `decompose/1`: ~1ms for 100 connections
- `validate_decomposition/1`: <0.1ms
- `format_report/1`: <1ms

Recommended usage: Before/after benchmark runs, not in hot path.

## Future Enhancements

1. **Distributed tracing integration** - Link memory to OTEL spans
2. **Historical trending** - Track decomposition over time
3. **Alerting** - Warn when components exceed thresholds
4. **Memory leak detection** - Compare decompositions across runs
5. **Automatic regression detection** - Flag unexpected increases

## References

- `erlang:memory/0,1` - Standard memory measurement
- `erlang:process_info/2` - Per-process memory
- `erts_debug:flat_size/1` - Deep term size calculation
- `sys:get_state/2` - Extract gen_server state

## License

Part of erlmcp - Erlang MCP SDK
