# Profiling and Debugging Guide

## Overview

erlmcp provides comprehensive profiling and debugging tools for performance analysis and troubleshooting:

- **erlmcp_profiler** - CPU & memory profiling, flame graphs
- **erlmcp_debugger** - Live process debugging, message tracing
- **erlmcp_memory_analyzer** - Memory leak detection, heap analysis

## CPU Profiling

### Profile a Module

```erlang
%% Profile for 60 seconds
erlmcp_profiler:profile(erlmcp_server, 60000, #{
    mode => fprof,
    output => "server_profile.out",
    sorting => acc
}).
```

### Profile a Specific Function (MFA)

```erlang
%% Profile erlmcp_json_rpc:encode/1
erlmcp_profiler:profile_mfa(
    {erlmcp_json_rpc, encode, 1},
    30000,
    #{mode => fprof, output => "encode_profile.out"}
).
```

### Profile by PID

```erlang
Pid = whereis(erlmcp_server),
erlmcp_profiler:profile_pid(Pid, #{
    duration => 60000,
    mode => fprof,
    output => "pid_profile.out"
}).
```

### Profiling Modes

- **fprof** - Function-level profiling with call counts and time
- **eprof** - Time-based profiling (faster, less detail)
- **cprof** - Call count profiling only (minimal overhead)

## Memory Profiling

### Memory Snapshot

```erlang
%% Get top 20 processes by memory
{ok, Snapshot} = erlmcp_profiler:memory_snapshot(#{
    top => 20,
    sort => memory
}).

%% Snapshot structure:
[
    #{
        pid => <0.123.0>,
        memory_bytes => 12345678,
        memory_mb => 11.77,
        message_queue_len => 0,
        reductions => 4567890,
        registered_name => erlmcp_server
    },
    ...
]
```

### Process Memory Details

```erlang
{ok, Info} = erlmcp_profiler:process_memory(Pid).

%% Info structure:
#{
    pid => <0.123.0>,
    memory_bytes => 12345678,
    memory_mb => 11.77,
    heap_size_words => 6000,
    stack_size_words => 24,
    total_heap_size_words => 10946,
    heap_fragmentation_pct => 45.2,
    message_queue_len => 0
}
```

### Binary Leak Detection

```erlang
{ok, Suspects} = erlmcp_profiler:binary_leaks().

%% Returns processes with binary memory > 50% of total memory
[
    #{
        pid => <0.456.0>,
        binary_size_bytes => 10485760,
        binary_size_mb => 10.0,
        total_memory_bytes => 12000000,
        binary_ratio => 0.87,
        binary_count => 42,
        registered_name => data_processor
    },
    ...
]
```

### Heap Fragmentation Analysis

```erlang
%% Single process
{ok, Fragmentation} = erlmcp_profiler:heap_fragmentation(Pid).
%% Returns: 45.2 (percent)

%% All processes with fragmentation > 30%
{ok, Analysis} = erlmcp_memory_analyzer:heap_analysis(#{
    threshold => 30.0
}).
```

## Flame Graphs

### Generate Flame Graph Data

```erlang
%% 1. Profile your application
erlmcp_profiler:profile(erlmcp_server, 60000, #{
    mode => fprof,
    output => "server_profile.out"
}).

%% 2. Export to folded stack format
erlmcp_profiler:flame_graph(
    "server_profile.out",
    "server_stacks.txt"
).

%% 3. Generate SVG (requires flamegraph.pl)
%% $ flamegraph.pl server_stacks.txt > server_flame.svg
```

### Install FlameGraph Tool

```bash
git clone https://github.com/brendangregg/FlameGraph
export PATH=$PATH:/path/to/FlameGraph
flamegraph.pl server_stacks.txt > server_flame.svg
```

## Live Debugging

### Attach to Process

```erlang
%% Attach to registered process
{ok, Pid} = erlmcp_debugger:attach(erlmcp_server).

%% Or by PID
{ok, Pid} = erlmcp_debugger:attach(<0.123.0>).
```

### Inspect State

```erlang
{ok, Snapshot} = erlmcp_debugger:inspect_state(Pid).

%% Snapshot structure:
#{
    timestamp => 1706389200000,
    process_info => [...],
    state => #state{...}  % gen_server state
}
```

### Trace Function Calls

```erlang
%% Trace all calls to erlmcp_json_rpc:encode/1
{ok, Ref} = erlmcp_debugger:trace_calls(
    erlmcp_json_rpc, encode, 1
).

%% ... let it run ...

%% Stop and get results
{ok, Traces} = erlmcp_debugger:stop_trace(Ref).
```

### Trace Messages

```erlang
%% Trace messages to a process for 30 seconds
{ok, Ref} = erlmcp_debugger:trace_messages(Pid, 30000).

%% ... wait for completion ...

%% Get captured messages
{ok, Messages} = erlmcp_debugger:stop_trace(Ref).
```

### Detach from Process

```erlang
ok = erlmcp_debugger:detach(Pid).
```

## Memory Analysis

### Full Analysis

```erlang
{ok, Analysis} = erlmcp_memory_analyzer:analyze(#{
    top => 20,
    include_ets => true,
    include_binaries => true
}).

%% Analysis structure:
#{
    timestamp => 1706389200000,
    system_memory => #{
        total => 123456789,
        processes => 45678900,
        system => 67890123,
        binary => 12345678,
        ets => 3456789
    },
    top_processes => [...],
    total_processes => 342,
    ets_tables => [...],
    binary_leaks => #{...}
}
```

### Top Processes

```erlang
TopProcs = erlmcp_memory_analyzer:top_processes(10).

%% Returns 10 processes with highest memory usage
```

### ETS Table Analysis

```erlang
%% All tables
AllTables = erlmcp_memory_analyzer:ets_tables(all).

%% Top 10 by size
Top10 = erlmcp_memory_analyzer:ets_tables(10).

%% Table info structure:
#{
    table => #Ref<0.1234.0>,
    name => session_cache,
    size => 15000,
    memory_bytes => 2400000,
    type => set,
    owner => <0.123.0>
}
```

### Leak Detection

```erlang
LeakInfo = erlmcp_memory_analyzer:detect_leaks().

%% Returns comprehensive leak analysis:
#{
    binary_leaks => [...],           % Processes with excessive binaries
    long_message_queues => [...],    % Processes with long queues
    large_ets_tables => [...],       % ETS tables > 10K entries
    leak_score => 42.5               % Overall leak severity (0-100)
}
```

## Profiling Workflows

### Performance Investigation

```erlang
%% 1. Take baseline memory snapshot
{ok, Baseline} = erlmcp_profiler:memory_snapshot(#{top => 20}).

%% 2. Profile CPU for 60 seconds
erlmcp_profiler:profile(erlmcp_server, 60000, #{
    mode => fprof,
    output => "investigation.out"
}).

%% 3. Take another snapshot
{ok, After} = erlmcp_profiler:memory_snapshot(#{top => 20}).

%% 4. Check for leaks
LeakInfo = erlmcp_memory_analyzer:detect_leaks().

%% 5. Generate flame graph
erlmcp_profiler:flame_graph("investigation.out", "flame.txt").
```

### Production Debugging

```erlang
%% 1. Attach to suspected process
{ok, Pid} = erlmcp_debugger:attach(suspected_process).

%% 2. Inspect current state
{ok, State} = erlmcp_debugger:inspect_state(Pid).

%% 3. Trace messages for 30s
{ok, Ref} = erlmcp_debugger:trace_messages(Pid, 30000).

%% 4. Get memory details
{ok, MemInfo} = erlmcp_profiler:process_memory(Pid).

%% 5. Check heap fragmentation
{ok, Frag} = erlmcp_profiler:heap_fragmentation(Pid).

%% 6. Detach when done
ok = erlmcp_debugger:detach(Pid).
```

## Performance Best Practices

### Profiling Overhead

- **fprof**: High overhead (10-50x slower), use for detailed analysis
- **eprof**: Medium overhead (2-10x slower), good balance
- **cprof**: Low overhead (<2x slower), use in production

### When to Profile

1. **Development**: Use fprof for detailed optimization
2. **Staging**: Use eprof for realistic load testing
3. **Production**: Use cprof or short fprof bursts

### Memory Analysis Tips

1. Check binary leaks first (often the culprit)
2. Monitor heap fragmentation (>50% is concerning)
3. Watch message queue lengths (>1000 is suspicious)
4. Review ETS table growth regularly

### Flame Graph Interpretation

- **Wide bars**: Frequently called functions (hot paths)
- **Tall stacks**: Deep call chains (potential optimization)
- **Color**: Usually by module (configurable)

## Integration with Dashboard

```erlang
%% Start profiling from dashboard
%% HTTP POST /api/debug/profile
curl -X POST http://localhost:8080/api/debug/profile \
  -H "Content-Type: application/json" \
  -d '{
    "target": "erlmcp_server",
    "duration": 60000,
    "mode": "fprof"
  }'

%% Get memory analysis
%% HTTP GET /api/debug/memory
curl http://localhost:8080/api/debug/memory?top=20
```

## Troubleshooting

### fprof Output File Too Large

```erlang
%% Use shorter duration or eprof
erlmcp_profiler:profile(Module, 10000, #{mode => eprof}).
```

### Process Not Found

```erlang
%% Check if process is registered
whereis(ProcessName).

%% List all registered processes
registered().
```

### Trace Not Capturing Data

```erlang
%% Verify trace is active
erlang:trace_info(Pid, flags).

%% Stop all traces
erlmcp_profiler:stop_profiling().
```

## See Also

- [Architecture Guide](../architecture.md)
- [Metrics Guide](../metrics.md)
- [Dashboard Guide](./dashboard.md)
