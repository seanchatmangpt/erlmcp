# Timeline Profiler for OTP 26-27

## Overview

The Timeline Profiler leverages OTP 26's `system_profile/2` and OTP 27's enhanced `fprof` capabilities to provide visual timeline-based performance analysis for MCP operations. This tool enables developers to understand process execution patterns, context switching behavior, and scheduler interactions at microsecond granularity.

## Key Features

### 1. Timeline-Based Profiling
- **Microsecond precision**: Track execution with μs accuracy
- **Process lifecycle**: Visualize spawn, execute, sleep, and exit states
- **Scheduler tracking**: Monitor scheduler migrations and load balancing
- **Context switching**: Identify expensive process switches

### 2. MCP-Specific Analysis
- **Tool invocation profiling**: Complete lifecycle from request to response
- **Session flow tracing**: Track request-response patterns
- **Transport layer analysis**: Encode/decode and send/receive performance
- **Resource operations**: File I/O, subprocess spawn, and system call latency

### 3. Visualization
- **SVG timelines**: Static vector graphics for reports
- **HTML interactive**: Zoomable, filterable timeline with tooltips
- **Flame graphs**: Stack-based call visualization
- **Interaction graphs**: Process communication diagrams

## Installation

The timeline profiler is included in `erlmcp_observability` application:

```erlang
%% In rebar.config
{deps, [
    {erlmcp_observability, {path, "apps/erlmcp_observability"}}
]}.
```

Ensure OTP 26+ for full `system_profile` support:

```bash
erl -version
%% Erlang/OTP 26[+]
```

## Quick Start

### Basic Profiling

```erlang
%% Start the profiler
{ok, Pid} = erlmcp_profiler:start_link().

%% Profile a function
{ok, Result, Timeline} = erlmcp_profiler:profile_timeline(
    fun() -> my_expensive_function() end,
    <<"my_function">>
).

%% Generate SVG visualization
{ok, SVG} = erlmcp_timeline_viz:generate_svg(Timeline),
file:write_file("timeline.svg", SVG).
```

### CLI Usage

```bash
# Profile a tool call
erlmcp profile:tool fs read_file path=/tmp/test.txt

# Profile a session
erlmcp profile:session abc123def456

# Profile transport performance (1KB message)
erlmcp profile:transport stdio 1024

# Generate visualization
erlmcp profile:visualize profile.json timeline.html

# Compare profiles
erlmcp profile:compare baseline.json current.json

# List saved profiles
erlmcp profile:list
```

## API Reference

### erlmcp_profiler

#### `profile_timeline/2`

```erlang
profile_timeline(fun(() -> term()), binary()) ->
    {ok, term(), timeline_profile()} | {error, term()}.
```

Profile a function execution with default options.

**Parameters:**
- `Fun`: Function to profile (arity 0)
- `Label`: Descriptive label for the timeline

**Returns:**
- `{ok, Result, Timeline}`: Result and timeline data
- `{error, Reason}`: Profiling failed

**Example:**
```erlang
{ok, Result, Timeline} = erlmcp_profiler:profile_timeline(
    fun() -> lists:seq(1, 100000) end,
    <<"generate_sequence">>
).
```

#### `profile_function/3`

```erlang
profile_function(fun(() -> term()), binary(), profile_opts()) ->
    {ok, term(), timeline_profile()} | {error, term()}.

-type profile_opts() ::
    #{include_scheduler => boolean(),
      include_gc => boolean(),
      include_ports => boolean(),
      sample_rate => float(),
      max_events => pos_integer()}.
```

Profile with custom options.

**Options:**
- `include_scheduler`: Track scheduler events (default: `true`)
- `include_gc`: Track garbage collection (default: `true`)
- `include_ports`: Track port operations (default: `true`)
- `sample_rate`: Sampling rate 0.0-1.0 (default: `1.0`)
- `max_events`: Maximum events to collect (default: `10000`)

#### `profile_tool_call/4`

```erlang
profile_tool_call(Server, Tool, Arguments, Opts) ->
    {ok, term(), timeline_profile()} | {error, term()}.
```

Profile a complete tool invocation.

**Example:**
```erlang
{ok, Result, Timeline} = erlmcp_profiler:profile_tool_call(
    <<"fs">>,
    <<"read_file">>,
    #{path => <<"/tmp/test.txt">>},
    #{}
).
```

#### `aggregate_profiles/1`

```erlang
aggregate_profiles([timeline_profile()]) ->
    {ok, timeline_profile()} | {error, term()}.
```

Combine multiple profiles into aggregate statistics.

**Example:**
```erlang
%% Run multiple iterations
Profiles = [begin
    {ok, _, T} = erlmcp_profiler:profile_timeline(Fun, Label),
    T
end || _ <- lists:seq(1, 10)],

{ok, Aggregated} = erlmcp_profiler:aggregate_profiles(Profiles).
```

#### `compare_profiles/2`

```erlang
compare_profiles(timeline_profile(), timeline_profile()) ->
    #{baseline := timeline_profile(),
      comparison := timeline_profile(),
      diff := map()}.
```

Compare two profiles and calculate differences.

**Example:**
```erlang
Diff = erlmcp_profiler:compare_profiles(BaselineProfile, CurrentProfile),
#{duration_us_diff := DiffUs,
  duration_percent_change := PercentChange,
  faster := IsFaster} = maps:get(diff, Diff).
```

### erlmcp_timeline_viz

#### `generate_svg/2`

```erlang
generate_svg(timeline_profile(), viz_opts()) ->
    {ok, binary()} | {error, term()}.

-type viz_opts() ::
    #{width => pos_integer(),
      height => pos_integer(),
      show_grid => boolean(),
      show_legend => boolean(),
      color_scheme => default | warm | cool | monochrome}.
```

Generate SVG visualization.

**Example:**
```erlang
{ok, SVG} = erlmcp_timeline_viz:generate_svg(
    Timeline,
    #{width => 1600,
      height => 800,
      show_grid => true,
      show_legend => true}
).
```

#### `generate_html/2`

```erlang
generate_html(timeline_profile(), viz_opts()) ->
    {ok, binary()} | {error, term()}.
```

Generate interactive HTML visualization with zoom/pan.

#### `generate_flamegraph/1`

```erlang
generate_flamegraph(timeline_profile()) ->
    {ok, binary()} | {error, term()}.
```

Generate flamegraph-style visualization.

#### `export_json/1` and `export_csv/1`

```erlang
export_json(timeline_profile()) -> {ok, binary()} | {error, term()}.
export_csv(timeline_profile()) -> {ok, binary()} | {error, term()}.
```

Export timeline data for external analysis.

## Timeline Data Structure

```erlang
-type timeline_profile() ::
    #{profile_id := binary(),          %% Unique profile identifier
      label := binary(),               %% Descriptive label
      start_time := integer(),         %% Start timestamp (μs)
      end_time := integer(),           %% End timestamp (μs)
      total_duration_us := integer(),  %% Total duration (μs)
      events := [timeline_event()],    %% Timeline events
      statistics := map()}.            %% Calculated statistics

-type timeline_event() ::
    #{type := scheduler | process | port | gc,
      timestamp := integer(),          %% Event timestamp (μs)
      duration_us := integer(),        %% Event duration (μs)
      details := map()}.               %% Event-specific details
```

## Use Cases

### 1. Tool Invocation Performance

Profile complete tool lifecycle:

```erlang
{ok, Result, Timeline} = erlmcp_profiler:profile_tool_call(
    <<"fs">>,
    <<"write_file">>,
    #{path => <<"/tmp/large.txt">>,
      content => crypto:strong_rand_bytes(1024 * 1024)},  %% 1MB
    #{max_events => 50000}
).

%% Analyze bottlenecks
Stats = maps:get(statistics, Timeline),
EventCounts = maps:get(event_counts, Stats, #{}),
SchedulerCount = maps:get(scheduler, EventCounts, 0),
GCCount = maps:get(gc, EventCounts, 0),

io:format("Scheduler events: ~p~n", [SchedulerCount]),
io:format("GC events: ~p~n", [GCCount]).
```

### 2. Session Request Latency

Track session processing:

```erlang
{ok, Result, Timeline} = erlmcp_profiler:profile_session_request(
    SessionId,
    #{method => <<"tools/call">>,
      params => #{name => <<"my_tool">>}}
).

%% Generate visualization
{ok, HTML} = erlmcp_timeline_viz:generate_html(Timeline),
file:write_file("session_trace.html", HTML).
```

### 3. Transport Comparison

Compare transport implementations:

```bash
# Profile stdio with 1KB message
erlmcp profile:transport stdio 1024

# Profile tcp with 1KB message
erlmcp profile:transport tcp 1024

# Compare
erlmcp profile:compare profile_stdio_*.json profile_tcp_*.json
```

### 4. Regression Detection

Establish performance baseline:

```erlang
%% Establish baseline
{ok, _, Baseline} = erlmcp_profiler:profile_timeline(
    fun critical_operation/0,
    <<"baseline_v1.0">>
),
ok = file:write_file("baseline.json", jsx:encode(Baseline)).

%% Test current version
{ok, _, Current} = erlmcp_profiler:profile_timeline(
    fun critical_operation/0,
    <<"current_v1.1">>
).

%% Compare
Diff = erlmcp_profiler:compare_profiles(Baseline, Current),
#{duration_percent_change := Change} = maps:get(diff, Diff),

%% Fail if slower than 10%
case Change > 10.0 of
    true -> error({performance_regression, Change});
    false -> ok
end.
```

## Performance Considerations

### Overhead

Timeline profiling adds overhead:
- **System profiling**: ~5-10% CPU overhead
- **Event collection**: ~1-2μs per event
- **Memory**: ~100 bytes per event

**Recommendations:**
1. Use `max_events` to limit collection
2. Disable unused event types (`include_gc`, `include_ports`)
3. Sample selectively with `sample_rate` for production

### Best Practices

1. **Short-lived operations**: Profile specific functions, not entire system
2. **Multiple iterations**: Aggregate 5-10 runs for statistical significance
3. **Baseline comparison**: Always compare against known-good baseline
4. **Export for analysis**: Use JSON/CSV export for external tools

## Troubleshooting

### No Events Collected

```erlang
%% Verify OTP version
erlang:system_info(otp_release).
%% Expected: "26" or higher

%% Check system_profile availability
erlang:system_profile(self(), [scheduler]).
%% Expected: {ok, ProfilerPid}
```

### Memory Growth

```erlang
%% Limit event collection
Opts = #{max_events => 1000},  %% Reduce from default 10000

%% Sample instead of full collection
Opts = #{sample_rate => 0.1},  %% Collect 10% of events
```

### Missing Event Types

```erlang
%% Explicitly enable event types
Opts = #{include_scheduler => true,
         include_gc => true,
         include_ports => true}.
```

## Integration with CI/CD

### Regression Tests

```erlang
%% test/performance_profiling_SUITE.erl
profile_tool_write_file_test(_Config) ->
    %% Load baseline
    {ok, BaselineJSON} = file:read_file("test/baselines/write_file.json"),
    {ok, Baseline} = jsx:decode(BaselineJSON, [return_maps]),

    %% Profile current implementation
    {ok, _, Current} = erlmcp_profiler:profile_tool_call(
        <<"fs">>, <<"write_file">>,
        #{path => <<"/tmp/test.txt">>, content => <<"test">>},
        #{}
    ),

    %% Compare
    Diff = erlmcp_profiler:compare_profiles(Baseline, Current),
    #{duration_percent_change := Change} = maps:get(diff, Diff),

    %% Assert no regression (>10% slower)
    ?assert(Change =< 10.0).
```

### Automated Benchmarking

```bash
#!/bin/bash
# scripts/benchmark.sh

echo "Running performance benchmarks..."

# Profile tool calls
erlmcp profile:tool fs read_file path=/tmp/test.txt
erlmcp profile:tool fs write_file path=/tmp/test.txt content=test

# Profile transport
erlmcp profile:transport stdio 1024
erlmcp profile:transport tcp 1024

# Generate report
echo "Generating benchmark report..."
python scripts/generate_report.py profile_*.json > benchmark_report.html
```

## References

- [OTP 26 Release Notes](https://www.erlang.org/doc/system_principles/system_profiling.html)
- [fprof Manual](https://www.erlang.org/doc/man/fprof.html)
- [MCP Specification](https://modelcontextprotocol.io/)
- [erlmcp Performance Guide](../docs/PERFORMANCE.md)

## License

MIT License - See LICENSE file for details.
