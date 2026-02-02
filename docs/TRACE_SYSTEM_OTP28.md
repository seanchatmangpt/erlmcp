# OTP 28 Trace System for erlmcp

## Overview

This document describes erlmcp's integration with **OTP 28's `trace:system/3`** API for distributed tracing of MCP tool invocation chains and session lifecycle events.

## What is OTP 28 `trace:system/3`?

**Introduced in OTP 28.0**, the `trace:system/3` function provides a modern, modular alternative to the legacy global `dbg` tracer.

### Key Improvements Over `dbg`

| Feature | Legacy (`dbg`) | OTP 28 (`trace`) |
|---------|----------------|------------------|
| **Sessions** | Single global session | Multiple isolated sessions |
| **Cleanup** | Manual, error-prone | Automatic on session destroy |
| **System Monitoring** | `erlang:system_monitor/2` (separate) | Integrated via `trace:system/3` |
| **Interference** | High - tools conflict | None - sessions isolated |
| **API** | Complex | Streamlined |

## erlmcp Tracing Architecture

```
┌─────────────────────────────────────────────────────────┐
│                   erlmcp_tracer                          │
│  - Manages trace sessions                                │
│  - Creates tracer processes                              │
│  - Enables system monitoring (long_gc, busy_port, etc.)  │
└─────────────────────────────────────────────────────────┘
                          │
                          ├── trace:session_create/3
                          ├── trace:system/3 (system monitoring)
                          ├── trace:function/4 (tool calls)
                          ├── trace:send/3 (messages)
                          └── trace:recv/3 (messages)
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│              erlmcp_trace_visualizer                     │
│  - Formats trace events as timeline                      │
│  - Generates call graphs                                 │
│  - Visualizes message flow                               │
│  - Analyzes performance                                  │
└─────────────────────────────────────────────────────────┘
```

## Trace Point Categories

### 1. Tool Invocation Chains

**What**: Trace tool execution from request to response
**Modules**: `erlmcp_server`, `erlmcp_session_backend`
**Trace Points**:
- `erlmcp_server:handle_call/3` - Tool requests received
- `erlmcp_server:execute_tool_call/3` - Tool execution
- Tool handler function calls

**Example**:
```erlang
%% Start tracing tool calls
{ok, SessionId, TracerPid} = erlmcp_tracer:trace_tool_calls(),

%% Call a tool
{ok, Result} = erlmcp_server:call_tool(Server, <<"my_tool">>, #{}),

%% Collect traces
{ok, Events} = erlmcp_tracer:collect_trace(SessionId),
%% Returns: [#{timestamp => 123456, event_type => call, data => ...}, ...]
```

### 2. Session Lifecycle Events

**What**: Trace MCP session state changes
**Modules**: `erlmcp_session_backend`, `erlmcp_session`
**Trace Points**:
- Session creation
- Initialization phase transitions
- Session closure
- Session timeout

**Example**:
```erlang
%% Trace session lifecycle
erlmcp_tracer:trace_session(SessionId),

%% Session events captured:
%% - {trace, Pid, call, {erlmcp_session_backend, store, 2}}
%% - {trace, Pid, 'receive', {session_created, SessionId}}
```

### 3. Message Passing

**What**: Trace inter-process communication
**Modules**: All MCP components
**Trace Points**:
- Send events (`trace:send/3`)
- Receive events (`trace:recv/3`)
- JSON-RPC messages
- Notification messages

**Example**:
```erlang
%% Enable message tracing
{ok, SessionId, _} = erlmcp_tracer:trace_messages(),

%% Send a message
erlmcp_client:call_tool(Client, <<"tool">>, #{}),

%% Collect traces
{ok, Events} = erlmcp_tracer:collect_trace(SessionId),
%% Contains send/receive events with timestamps
```

### 4. System Events

**What**: Monitor system health via `trace:system/3`
**Events**:
- `long_gc` - Garbage collection > threshold
- `long_schedule` - Process/port running too long
- `long_message_queue` - Message queue buildup
- `large_heap` - Heap size exceeded
- `busy_port` - Port congestion
- `busy_dist_port` - Distributed port busy

**Example**:
```erlang
%% Enable system monitoring
erlmcp_tracer:enable_system_monitor([{long_gc, 10}, {long_schedule, 100}]),

%% Monitor events
{ok, Events} = erlmcp_tracer:collect_trace(),
%% Contains: #{timestamp => ..., event_type => system_monitor,
%%             data => #{system_event => long_gc, info => ...}}
```

## Usage Patterns

### Pattern 1: Trace Tool Execution

```erlang
%% Start tracing
{ok, SessionId, _} = erlmcp_tracer:trace_tool_calls(),

%% Execute tools
{ok, R1} = erlmcp_server:call_tool(Server, <<"tool1">>, #{}),
{ok, R2} = erlmcp_server:call_tool(Server, <<"tool2">>, #{}),

%% Analyze trace
{ok, Events} = erlmcp_tracer:collect_trace(SessionId),
Report = erlmcp_trace_visualizer:generate_report(Events),

%% View call graph
io:format("~s~n", [maps:get(call_graph, Report)]),

%% Cleanup
ok = erlmcp_tracer:stop_trace_session(SessionId).
```

### Pattern 2: Distributed Tracing

```erlang
%% Start trace session on multiple nodes
Nodes = [node(), 'node2@host', 'node3@host'],
{ok, SessionId, _} = erlmcp_tracer:start_trace_session(Nodes),

%% All nodes now trace into same session
%% Correlate by request ID across nodes

%% Collect from all nodes
{ok, Events} = erlmcp_tracer:collect_trace(SessionId),
%% Events from all nodes merged by timestamp
```

### Pattern 3: Performance Analysis

```erlang
%% Enable comprehensive tracing
{ok, SessionId, _} =
    erlmcp_tracer:start_trace_session([node()],
                                      #{trace_tools => true,
                                        trace_messages => true,
                                        system_monitor => true}),

%% Run workload
run_benchmark(),

%% Analyze performance
{ok, Events} = erlmcp_tracer:collect_trace(SessionId),
Perf = erlmcp_trace_visualizer:analyze_performance(Events),

%% View hotspots
Hotspots = maps:get(hotspots, Perf),
%% [{erlmcp_server, call_tool, 3} = 1500, ...]
```

### Pattern 4: Real-Time Monitoring

```erlang
%% Start system monitoring
erlmcp_tracer:enable_system_monitor([{long_gc, 50},
                                      {long_schedule, 200}]),

%% Receive system monitor events
receive
    {monitor, Pid, long_gc, Info} ->
        logger:warning("Long GC on ~p: ~p", [Pid, Info]);
    {monitor, Pid, long_schedule, Info} ->
        logger:warning("Long schedule on ~p: ~p", [Pid, Info])
end.
```

## Trace Visualization

### Text Timeline

```erlang
{ok, Events} = erlmcp_tracer:collect_trace(),
Timeline = erlmcp_trace_visualizer:format_timeline(Events),
io:format("~s~n", [Timeline]),

%% Output:
%% ========================================
%% MCP Trace Timeline
%% ========================================
%% Total Events: 42
%% ========================================
%%
%% [2026-02-01 12:34:56.789] call erlmcp_server:call_tool/3 Args: [<<"my_tool">>, #{}]
%% [2026-02-01 12:34:56.790] send To: <0.123.0> Msg: {json_rpc, ...}
%% [2026-02-01 12:34:56.791] receive Msg: {json_rpc_response, ...}
```

### JSON Export

```erlang
{ok, Events} = erlmcp_tracer:collect_trace(),
ok = erlmcp_trace_visualizer:export_trace(Events, "trace.json"),
%% Generates JSON with all events for dashboards
```

### HTML Timeline

```erlang
{ok, Events} = erlmcp_tracer:collect_trace(),
ok = erlmcp_trace_visualizer:export_trace(Events, "trace.html"),
%% Interactive HTML timeline with syntax highlighting
```

## Performance Metrics

The visualizer provides automatic performance analysis:

```erlang
Perf = erlmcp_trace_visualizer:analyze_performance(Events),

%% Metrics:
%% - total_events: Total trace events
%% - call_counts: Call frequency by function
%% - avg_call_duration_us: Average call duration
%% - total_gc_time_ms: Total GC time
%% - hotspots: Top 10 most-called functions
```

## Integration with erlmcp Components

### Adding Trace Points

To add tracing to your code:

```erlang
%% In your module
-include_lib("kernel/include/logger.hrl").

handle_call({execute_tool, ToolName, Args}, _From, State) ->
    %% Trace tool call
    erlmcp_trace_integration:trace_tool_call(self(), ToolName, Args),

    %% Execute
    Result = do_execute_tool(ToolName, Args, State),

    %% Trace result
    erlmcp_trace_integration:trace_tool_result(self(), ToolName, Result),

    {reply, Result, State}.
```

### Runtime Tracing Control

```erlang
%% Enable tracing at runtime
erlmcp_trace_integration:enable_tracing(),

%% Disable tracing
erlmcp_trace_integration:disable_tracing(),

%% Check if enabled
true = erlmcp_trace_integration:is_tracing_enabled().
```

## Best Practices

### 1. Use Isolated Sessions

**DO**:
```erlang
%% Each debug session gets its own tracer
{ok, SessionId1, _} = erlmcp_tracer:start_trace_session([node()]),
{ok, SessionId2, _} = erlmcp_tracer:start_trace_session([node()]),
%% Sessions don't interfere
```

**DON'T**:
```erlang
%% Don't use global dbg (conflicts with other tools)
dbg:tracer(),
dbg:p(all, call),
%% Breaks other tracing tools
```

### 2. Clean Up Sessions

**DO**:
```erlang
{ok, SessionId, _} = erlmcp_tracer:start_trace_session([node()]),
try
    do_work(),
    {ok, Events} = erlmcp_tracer:collect_trace(SessionId)
after
    %% Always cleanup
    erlmcp_tracer:stop_trace_session(SessionId)
end
```

**DON'T**:
```erlang
%% Don't leak sessions
{ok, SessionId, _} = erlmcp_tracer:start_trace_session([node()]),
do_work(),
%% Forgot to stop - memory leak!
```

### 3. Set Appropriate Thresholds

**DO**:
```erlang
%% Production-safe thresholds
erlmcp_tracer:enable_system_monitor([{long_gc, 100},      % 100ms
                                      {long_schedule, 500}]), % 500ms
```

**DON'T**:
```erlang
%% Too sensitive - flood of events
erlmcp_tracer:enable_system_monitor([{long_gc, 1},  % 1ms
                                      {long_schedule, 1}]),
```

### 4. Filter Trace Data

**DO**:
```erlang
%% Filter to specific events
Filter = fun(#{event_type := Type}) ->
             Type =:= call orelse Type =:= send
         end,
Timeline = erlmcp_trace_visualizer:format_timeline(
             Events,
             #{filter => Filter}),
```

**DON'T**:
```erlang
%% Don't analyze all events - too noisy
%% Unfiltered output is overwhelming
```

## Troubleshooting

### No Trace Events Collected

**Problem**: `{ok, []}` returned from `collect_trace/1`

**Causes**:
1. Tracer process died
2. Trace session destroyed prematurely
3. No activity in traced components

**Solutions**:
```erlang
%% Check if tracer alive
erlang:is_process_alive(TracerPid),

%% Check session status
trace:session_info(SessionId),

%% Increase timeout
timer:sleep(500),  % Allow events to propagate
```

### Performance Impact

**Problem**: Tracing slows down system

**Solutions**:
```erlang
%% Use selective tracing
erlmcp_tracer:trace_tool_calls(),    % Only tools
%% vs
erlmcp_tracer:trace_messages(),      % Everything (slower)

%% Increase thresholds
erlmcp_tracer:enable_system_monitor([{long_gc, 1000}]),  % 1 second
```

### Memory Leaks

**Problem**: Memory usage grows with tracing

**Solutions**:
```erlang
%% Always stop sessions
ok = erlmcp_tracer:stop_trace_session(SessionId),

%% Collect and clear events periodically
{ok, Events} = erlmcp_tracer:collect_trace(),
process_events(Events),
%% Events cleared from tracer buffer
```

## API Reference

### erlmcp_tracer

| Function | Purpose |
|----------|---------|
| `start_link/0` | Start tracer gen_server |
| `start_trace_session/1` | Create trace session |
| `trace_session/1` | Trace specific MCP session |
| `trace_tool_calls/0` | Enable tool call tracing |
| `trace_messages/0` | Enable message tracing |
| `collect_trace/0` | Collect all trace events |
| `stop_trace_session/0` | Stop all sessions |
| `enable_system_monitor/0` | Enable system monitoring |

### erlmcp_trace_visualizer

| Function | Purpose |
|----------|---------|
| `format_timeline/1` | Format as text timeline |
| `format_call_graph/1` | Generate call graph |
| `format_message_flow/1` | Visualize message flow |
| `format_system_events/1` | Format system events |
| `export_trace/2` | Export to file |
| `generate_report/1` | Generate full report |
| `analyze_performance/1` | Analyze performance |

### erlmcp_trace_integration

| Function | Purpose |
|----------|---------|
| `trace_tool_call/3` | Trace tool invocation |
| `trace_tool_result/3` | Trace tool result |
| `trace_session_event/2` | Trace session event |
| `trace_message/3` | Trace message |
| `trace_error/2` | Trace error |
| `is_tracing_enabled/0` | Check if tracing enabled |
| `enable_tracing/0` | Enable tracing |
| `disable_tracing/0` | Disable tracing |

## OTP Version Requirements

- **Minimum**: OTP 28.0 (for `trace:system/3`)
- **Recommended**: OTP 28.3.1 (latest stable)

To check your OTP version:
```bash
erl -noshell -eval "io:format('~p~n', [erlang:system_info(otp_release)]), init:stop()."
```

## References

- [OTP 28 trace:system/3 Documentation](https://www.erlang.org/doc/apps/kernel/trace.html#system-3)
- [OTP 28 Release Notes](https://www.erlang.org/news/180)
- [erlmcp OTP Patterns](otp-patterns.md)
- [erlmcp Observability Guide](docs/architecture/observability.md)

## Summary

**OTP 28's `trace:system/3`** provides:
- ✅ Modular, isolated trace sessions
- ✅ Integrated system monitoring
- ✅ Automatic cleanup
- ✅ Better performance than `dbg`

**erlmcp integration** provides:
- ✅ Tool invocation chain tracing
- ✅ Session lifecycle tracking
- ✅ Message flow visualization
- ✅ Performance analysis
- ✅ Distributed tracing support

Use tracing for debugging complex tool interactions, performance optimization, and production monitoring.
