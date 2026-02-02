# Memory Guard and Process Limits - OTP 28 Feature

## Overview

ErlMCP implements comprehensive memory guard functionality using OTP 28's advanced process flag support to prevent memory leaks in long-running model contexts. This document describes the memory guard architecture, configuration, and best practices.

## Table of Contents

1. [Features](#features)
2. [OTP 28 Process Flags](#otp-28-process-flags)
3. [API Reference](#api-reference)
4. [Configuration](#configuration)
5. [Integration Patterns](#integration-patterns)
6. [Monitoring and Observability](#monitoring-and-observability)
7. [Performance Considerations](#performance-considerations)
8. [Migration Guide from OTP 27](#migration-guide-from-otp-27)
9. [Troubleshooting](#troubleshooting)
10. [Examples](#examples)

## Features

### Per-Process Memory Limits

ErlMCP provides granular memory limits for different process types:

- **Context Processes**: 100MB heap, 50MB binary heap
- **Tool Processes**: 50MB heap, 25MB binary heap
- **Transport Processes**: 30MB heap, 15MB binary heap
- **Generic Processes**: 20MB heap, 10MB binary heap

### Automatic Memory Guard Integration

Memory guards are automatically enabled for:

- Tool processes spawned via `erlmcp_session_backend:spawn_tool/2`
- Context processes created by session managers
- Transport processes via behavior callbacks

### Memory Leak Detection

The memory monitor (`erlmcp_memory_monitor`) continuously tracks process memory usage and detects leaks through:

- Periodic memory checks (default: 5 seconds)
- Growth pattern analysis (linear regression)
- Automatic hibernation recommendations
- OTEL integration for observability

## OTP 28 Process Flags

### Heap Size Flags

```erlang
%% Set minimum heap size (prevents excessive GC)
erlang:process_flag(min_heap_size, MinHeapWords),

%% Set maximum heap size (triggers GC or kills process)
erlang:process_flag(max_heap_size, MaxHeapWords).
```

### Binary Heap Flags

```erlang
%% Set minimum binary virtual heap
erlang:process_flag(min_bin_vheap_size, MinBinHeapWords),

%% Set maximum binary virtual heap
erlang:process_flag(max_bin_vheap_size, MaxBinHeapWords).
```

### Behavior

When `max_heap_size` is exceeded:

1. Garbage collection is triggered
2. If still exceeded, process is killed with reason: `heap_size`
3. `spawn_opt` option `{max_heap_size, Size}` can configure behavior

## API Reference

### erlmcp_memory_guard

#### `configure_limits/2`

```erlang
-spec configure_limits(MaxHeap :: pos_integer(),
                      MaxBinHeap :: pos_integer()) -> ok.
```

Configure memory limits for the current process.

**Parameters:**
- `MaxHeap`: Maximum heap size in words
- `MaxBinHeap`: Maximum binary virtual heap size in words

**Example:**

```erlang
%% Set 50MB heap, 25MB binary heap
ok = erlmcp_memory_guard:configure_limits(50_000_000, 25_000_000).
```

#### `enable_context_guard/0`

```erlang
-spec enable_context_guard() -> ok.
```

Enable memory guard for LLM context processes (100MB heap, 50MB binary heap).

**Example:**

```erlang
ok = erlmcp_memory_guard:enable_context_guard().
```

#### `enable_tool_guard/0`

```erlang
-spec enable_tool_guard() -> ok.
```

Enable memory guard for tool execution processes (50MB heap, 25MB binary heap).

**Example:**

```erlang
ok = erlmcp_memory_guard:enable_tool_guard().
```

#### `enable_transport_guard/0`

```erlang
-spec enable_transport_guard() -> ok.
```

Enable memory guard for transport processes (30MB heap, 15MB binary heap).

**Example:**

```erlang
ok = erlmcp_memory_guard:enable_transport_guard().
```

#### `get_memory_usage/0`

```erlang
-spec get_memory_usage() -> {Heap :: non_neg_integer(),
                             BinHeap :: non_neg_integer()}.
```

Get current memory usage for the calling process.

**Returns:** Tuple of `{HeapSize, BinaryHeapSize}` in bytes.

**Example:**

```erlang
{Heap, BinHeap} = erlmcp_memory_guard:get_memory_usage(),
logger:info("Process memory: heap=~p, binary=~p", [Heap, BinHeap]).
```

#### `force_hibernate/0`

```erlang
-spec force_hibernate() -> ok.
```

Force the current process to hibernate, discarding the call stack to minimize memory footprint.

**Example:**

```erlang
ok = erlmcp_memory_guard:force_hibernate().
```

#### `validate_memory/1`

```erlang
-spec validate_memory(ProcessType :: process_type()) ->
    {ok, float()} | {warning, float()} | {error, float()}.
```

Validate memory usage is within configured limits.

**Returns:**
- `{ok, Percent}`: Within limits
- `{warning, Percent}`: Approaching threshold (90%+)
- `{error, Percent}`: Exceeded threshold

**Example:**

```erlang
case erlmcp_memory_guard:validate_memory(context) of
    {ok, Percent} ->
        logger:debug("Memory usage: ~.2f%", [Percent]);
    {warning, Percent} ->
        logger:warning("Memory usage high: ~.2f%", [Percent]),
        erlmcp_memory_guard:force_hibernate();
    {error, Percent} ->
        logger:error("Memory usage critical: ~.2f%", [Percent])
end.
```

#### `get_limits/1`

```erlang
-spec get_limits(ProcessType :: process_type()) -> memory_limit().
```

Get memory limits for a process type.

**Returns:** Map with `max_heap`, `max_bin_heap`, and `hibernate_threshold`.

**Example:**

```erlang
Limits = erlmcp_memory_guard:get_limits(context),
#{max_heap := MaxHeap,
  max_bin_heap := MaxBinHeap,
  hibernate_threshold := Threshold} = Limits.
```

#### `is_otp_28_or_later/0`

```erlang
-spec is_otp_28_or_later() -> boolean().
```

Check if running on OTP 28 or later.

**Example:**

```erlang
case erlmcp_memory_guard:is_otp_28_or_later() of
    true -> logger:info("Memory guard fully supported");
    false -> logger:warning("Memory guard limited (requires OTP 28+)")
end.
```

### erlmcp_memory_monitor

#### `start_link/1`

```erlang
-spec start_link(Config :: monitor_config()) -> {ok, pid()} | {error, term()}.
```

Start the memory monitor gen_server.

**Configuration:**

```erlang
#{check_interval => 5000,           % Check every 5 seconds
  alert_threshold => 0.85,          % Alert at 85% usage
  enable_hibernation => false,      % Auto-hibernate on critical
  max_history => 10}.               % Keep 10 samples for leak detection
```

**Example:**

```erlang
{ok, Pid} = erlmcp_memory_monitor:start_link(#{
    check_interval => 5000,
    alert_threshold => 0.85,
    enable_hibernation => true
}).
```

#### `register_process/2`

```erlang
-spec register_process(Pid :: pid(), Type :: process_type()) -> ok.
```

Register a process for monitoring.

**Example:**

```erlang
ok = erlmcp_memory_monitor:register_process(self(), context).
```

#### `check_now/0`

```erlang
-spec check_now() -> {ok, monitor_report()}.
```

Trigger an immediate memory check.

**Example:**

```erlang
{ok, Report} = erlmcp_memory_monitor:check_now(),
#{high_memory := HighMem,
  memory_leaks := Leaks,
  total_memory := Total} = Report.
```

## Configuration

### Application Environment

Add to `erlmcp.config` or `sys.config`:

```erlang
{erlmcp, [
    {memory_limits, [
        {context, #{max_heap => 100_000_000,
                   max_bin_heap => 50_000_000,
                   hibernate_threshold => 0.9}},
        {tool, #{max_heap => 50_000_000,
                max_bin_heap => 25_000_000,
                hibernate_threshold => 0.85}},
        {transport, #{max_heap => 30_000_000,
                     max_bin_heap => 15_000_000,
                     hibernate_threshold => 0.80}}
    ]},
    {memory_monitor, [
        {check_interval, 5000},
        {alert_threshold, 0.85},
        {enable_hibernation, false},
        {max_history, 10}
    ]}
]}.
```

### Runtime Configuration

Update limits at runtime:

```erlang
%% Update via erlmcp_config
ok = erlmcp_config:set(memory_limits, NewLimits).

%% Or via memory monitor directly
ok = erlmcp_memory_monitor:set_config(#{
    check_interval => 10000,
    alert_threshold => 0.90
}).
```

## Integration Patterns

### Custom Process Guards

```erlang
-module(my_custom_process).
-behaviour(gen_server).

init([]) ->
    %% Enable memory guard for this process type
    ok = erlmcp_memory_guard:enable_generic_guard(),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    %% Check memory before expensive operations
    case erlmcp_memory_guard:validate_memory(generic) of
        {ok, _} ->
            do_expensive_work(Request);
        {warning, _} ->
            logger:warning("Memory high, hibernating after work"),
            Result = do_expensive_work(Request),
            erlmcp_memory_guard:force_hibernate(),
            {reply, Result, State, hibernate};
        {error, _} ->
            logger:error("Memory critical, rejecting request"),
            {reply, {error, memory_limit_exceeded}, State}
    end.
```

### Session Backend Integration

Memory guards are automatically enabled for tool processes:

```erlang
%% In erlmcp_session_backend:do_spawn_tool/2
{Pid, Ref} = spawn_monitor(fun() ->
    erlmcp_memory_guard:enable_tool_guard(),
    execute_tool(ToolName, Params)
end).
```

### Monitoring Integration

```erlang
%% Register process with memory monitor
ok = erlmcp_memory_monitor:register_process(ContextPid, context),

%% Memory monitor will:
%% 1. Check memory every 5 seconds
%% 2. Alert if usage exceeds 85%
%% 3. Detect memory leaks (growing trend)
%% 4. Force hibernation if configured
%% 5. Emit OTEL spans and events
```

## Monitoring and Observability

### OpenTelemetry Integration

Memory monitor emits OTEL spans:

```erlang
%% Automatic OTEL span for each check
SpanCtx = erlmcp_otel:start_span(<<"erlmcp.memory.check">>,
    #{<<"monitor.checked_processes">> => Count,
      <<"monitor.high_memory_count">> => HighCount,
      <<"monitor.total_memory">> => TotalMemory}),

%% Events for issues
erlmcp_otel:add_event(SpanCtx, <<"high_memory">>,
    #{<<"pid">> => PidBin,
      <<"usage">> => Percent}),
```

### Dashboard Integration

Memory reports sent to `erlmcp_dashboard_server`:

```erlang
%% Report format
#{timestamp => Timestamp,
  checked_processes => Count,
  high_memory => [Pid1, Pid2, ...],
  memory_leaks => [Pid3, Pid4, ...],
  hibernated => [Pid5],
  total_memory => TotalBytes}
```

### Log Messages

Memory guard logs important events:

```
[info] Context memory guard enabled: 400000000 bytes
[warning] Process memory usage high: 87.50% (35000000 / 40000000 bytes)
[warning] Process memory usage critical: 92.00% (36800000 / 40000000 bytes)
[info] Process hibernated, saved 2500000 bytes (7.14%)
[warning] Process ~p (~p) suspected memory leak: [100000, 150000, 200000, ...]
```

## Performance Considerations

### Memory Overhead

- **Process flags**: Negligible (stored in process structure)
- **Memory monitoring**: ~1KB per monitored process (history tracking)
- **Check interval**: Default 5 seconds balances responsiveness vs overhead

### GC Impact

- **Min heap size**: Reduces GC frequency (10% of max)
- **Max heap size**: Triggers GC before hard limit
- **Hibernation**: Reduces memory footprint by discarding stack

### Recommendations

1. **Context processes**: Use 100MB limit for long-lived contexts
2. **Tool processes**: Use 50MB limit for short-lived tools
3. **Transport processes**: Use 30MB limit for high connection counts
4. **Generic processes**: Use 20MB limit for utility processes

## Migration Guide from OTP 27

### Graceful Degradation

Memory guards work on OTP 27 but with limited functionality:

```erlang
%% On OTP 27: Process flags are ignored (no crash)
ok = erlmcp_memory_guard:enable_context_guard(),

%% Still works: Memory tracking
{Heap, BinHeap} = erlmcp_memory_guard:get_memory_usage(),

%% Still works: Hibernation
ok = erlmcp_memory_guard:force_hibernate(),

%% Still works: Validation
{ok, Percent} = erlmcp_memory_guard:validate_memory(context).
```

### Version Detection

Check OTP version at runtime:

```erlang
case erlmcp_memory_guard:is_otp_28_or_later() of
    true ->
        %% Full memory guard support
        ok = erlmcp_memory_guard:enable_context_guard();
    false ->
        %% Fallback: Monitor memory but can't set flags
        logger:info("Running on OTP ~s, memory guard limited",
                    [erlang:system_info(otp_release)])
end.
```

### Configuration

Same configuration works on both OTP 27 and 28:

```erlang
%% Works on both versions
{erlmcp, [
    {memory_limits, [
        {context, #{max_heap => 100_000_000,
                   max_bin_heap => 50_000_000}}
    ]}
]}.
```

On OTP 27: Limits used for validation only.
On OTP 28: Limits enforced via process flags.

## Troubleshooting

### High Memory Usage

**Symptom:** Process memory exceeds limits

**Diagnosis:**

```erlang
%% Check current usage
{Heap, BinHeap} = erlmcp_memory_guard:get_memory_usage(),

%% Validate against limits
case erlmcp_memory_guard:validate_memory(context) of
    {error, Percent} ->
        logger:error("Memory critical: ~.2f%", [Percent]);
    {warning, Percent} ->
        logger:warning("Memory high: ~.2f%", [Percent])
end.
```

**Solutions:**

1. Force hibernation: `erlmcp_memory_guard:force_hibernate()`
2. Reduce context size: Limit message history
3. Increase limits: Update configuration
4. Check for leaks: Review memory monitor reports

### Memory Leak Detection

**Symptom:** Memory grows consistently over time

**Diagnosis:**

```erlang
%% Get memory monitor report
{ok, Report} = erlmcp_memory_monitor:get_report(),

%% Check for leaks
#{memory_leaks := LeakedPids} = Report,
lists:foreach(fun(Pid) ->
    logger:warning("Suspected memory leak in ~p", [Pid])
end, LeakedPids).
```

**Solutions:**

1. Review process code for unbounded growth
2. Check for accumulator patterns without cleanup
3. Verify ETS tables are properly sized
4. Consider process restart strategy

### Process Crashes

**Symptom:** Process exits with `heap_size` reason

**Diagnosis:**

```erlang
%% Check crash logs
logger:error("Process ~p crashed: heap_size exceeded", [Pid]),

%% Review limits
Limits = erlmcp_memory_guard:get_limits(context),
logger:info("Context limit: ~p bytes", [maps:get(max_heap, Limits)]).
```

**Solutions:**

1. Increase limits: Update configuration
2. Optimize memory usage: Reduce context size
3. Implement hibernation: Force periodic cleanup
4. Split work: Divide into smaller processes

## Examples

### Example 1: Context Process with Memory Guard

```erlang
-module(my_context_process).
-behaviour(gen_server).

init([SessionId]) ->
    %% Enable memory guard for context
    ok = erlmcp_memory_guard:enable_context_guard(),

    %% Register with memory monitor
    ok = erlmcp_memory_monitor:register_process(self(), context),

    {ok, #state{session_id = SessionId, messages = []}}.

handle_call({add_message, Message}, _From, State) ->
    %% Check memory before adding
    case erlmcp_memory_guard:validate_memory(context) of
        {ok, _} ->
            NewMessages = [Message | State#state.messages],
            {reply, ok, State#state{messages = NewMessages}};
        {warning, _} ->
            logger:warning("Context memory high, truncating history"),
            %% Keep last 100 messages
            Truncated = lists:sublist([Message | State#state.messages], 100),
            {reply, ok, State#state{messages = Truncated}, hibernate};
        {error, _} ->
            logger:error("Context memory critical, rejecting message"),
            {reply, {error, memory_limit_exceeded}, State}
    end.

handle_info({hibernate_now, _Monitor}, State) ->
    %% Request from memory monitor to hibernate
    logger:info("Context hibernating due to memory pressure"),
    {noreply, State, hibernate}.
```

### Example 2: Custom Memory Limits

```erlang
%% Define custom process type with specific limits
-spec enable_custom_guard() -> ok.
enable_custom_guard() ->
    %% 75MB heap, 30MB binary heap
    ok = erlmcp_memory_guard:configure_limits(75_000_000, 30_000_000),

    %% Set normal priority
    erlang:process_flag(priority, normal),

    ok.
```

### Example 3: Memory Leak Detection

```erlang
%% Simulate memory leak and detection
leaky_process() ->
    spawn(fun() ->
        erlmcp_memory_guard:enable_context_guard(),
        erlmcp_memory_monitor:register_process(self(), context),

        %% Simulate growing memory
        leak_loop(1000)
    end).

leak_loop(Size) ->
    %% Allocate memory (leak!)
    _Data = lists:duplicate(Size, leaking_data),

    %% Wait for next check
    timer:sleep(6000),

    %% Grow more
    leak_loop(Size * 2).

%% Memory monitor will detect leak and alert
%% Output: "Process ~p (context) suspected memory leak: [X, Y, Z, ...]"
```

## Further Reading

- [OTP 28 Release Notes](https://www.erlang.org/doc/system_principals/system_principles.html)
- [Process Flags Documentation](https://www.erlang.org/doc/man/erlang.html#process_flag-2)
- [Memory Guard Design](https://github.com/erlang/otp/blob/master/erts/emulator/beam/erl_gc.c)
- [ErlMCP OTP Patterns](/docs/otp-patterns.md)
- [Observability Guide](/docs/OBSERVABILITY.md)

## Summary

Memory guards provide essential protection against memory leaks in long-running MCP contexts:

- **Automatic**: Enabled by default for tool and context processes
- **Configurable**: Per-process-type limits via application config
- **Observable**: Integrated with OTEL and dashboard
- **Safe**: Graceful degradation on OTP < 28
- **Effective**: Prevents unbounded memory growth

Use memory guards proactively to ensure system stability and prevent memory-related outages.
