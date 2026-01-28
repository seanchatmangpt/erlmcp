# Profiling Quick Reference Card

## One-Liners

### Memory Analysis
```erlang
%% Quick memory check
{ok, Top} = erlmcp_profiler:memory_snapshot(#{top => 10}).

%% Check for leaks
{ok, Leaks} = erlmcp_profiler:binary_leaks().

%% Full analysis
{ok, Analysis} = erlmcp_memory_analyzer:analyze(#{top => 20, include_ets => true}).
```

### CPU Profiling
```erlang
%% Profile for 60s (low overhead)
erlmcp_profiler:profile(MyModule, 60000, #{mode => cprof}).

%% Profile for detailed analysis
erlmcp_profiler:profile(MyModule, 30000, #{mode => fprof, output => "prof.out"}).

%% Generate flame graph
erlmcp_profiler:flame_graph("prof.out", "flame.txt").
```

### Live Debugging
```erlang
%% Attach and inspect
{ok, Pid} = erlmcp_debugger:attach(process_name),
{ok, State} = erlmcp_debugger:inspect_state(Pid).

%% Trace messages
{ok, Ref} = erlmcp_debugger:trace_messages(Pid, 30000),
{ok, Messages} = erlmcp_debugger:stop_trace(Ref).
```

## Common Workflows

### Performance Investigation
```erlang
%% 1. Baseline
{ok, Before} = erlmcp_profiler:memory_snapshot(#{top => 20}).

%% 2. Profile
erlmcp_profiler:profile(erlmcp_server, 60000, #{mode => fprof, output => "inv.out"}).

%% 3. Compare
{ok, After} = erlmcp_profiler:memory_snapshot(#{top => 20}).

%% 4. Check leaks
LeakInfo = erlmcp_memory_analyzer:detect_leaks().

%% 5. Flame graph
erlmcp_profiler:flame_graph("inv.out", "inv.txt").
```

### Memory Leak Hunt
```erlang
%% Binary leaks
{ok, BinLeaks} = erlmcp_profiler:binary_leaks().

%% Message queue buildup
LeakInfo = erlmcp_memory_analyzer:detect_leaks(),
LongQueues = maps:get(long_message_queues, LeakInfo).

%% ETS growth
LargeTables = maps:get(large_ets_tables, LeakInfo).

%% Heap fragmentation
{ok, Fragmented} = erlmcp_memory_analyzer:heap_analysis(#{threshold => 50.0}).
```

## Cheat Sheet

| Task | Command |
|------|---------|
| Top 10 memory users | `erlmcp_profiler:memory_snapshot(#{top => 10})` |
| Binary leaks | `erlmcp_profiler:binary_leaks()` |
| Process details | `erlmcp_profiler:process_memory(Pid)` |
| Profile 60s | `erlmcp_profiler:profile(Mod, 60000, #{mode => fprof})` |
| Attach debugger | `erlmcp_debugger:attach(Name)` |
| Trace calls | `erlmcp_debugger:trace_calls(Mod, Fun, Arity)` |
| Full analysis | `erlmcp_memory_analyzer:analyze(#{})` |
| ETS tables | `erlmcp_memory_analyzer:ets_tables(10)` |
| Leak detection | `erlmcp_memory_analyzer:detect_leaks()` |
| Heap fragmentation | `erlmcp_memory_analyzer:heap_analysis(#{})` |

## Profiling Mode Guide

| Mode | Overhead | Output | Use When |
|------|----------|--------|----------|
| cprof | <2x | Call counts | Production monitoring |
| eprof | 2-10x | Time per function | Staging tests |
| fprof | 10-50x | Full call graph | Deep analysis |

## Leak Severity

| Score | Severity | Action |
|-------|----------|--------|
| 0-25 | Normal | No action needed |
| 26-50 | Low | Monitor trends |
| 51-75 | Medium | Investigate |
| 76-100 | High | Immediate action |

## Fragmentation Thresholds

| Percent | Status | Impact |
|---------|--------|--------|
| <20% | Good | Normal operation |
| 20-40% | Fair | Monitor |
| 40-60% | Poor | GC more frequent |
| >60% | Critical | Performance impact |
