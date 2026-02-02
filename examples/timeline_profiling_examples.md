# Timeline Profiling Examples

This directory contains practical examples of using the OTP 26-27 Timeline Profiler for MCP performance analysis.

## Examples

### 1. Basic Profiling

**File**: `basic_profiling.erl`

```erlang
#!/usr/bin/env escript
%%! -pa _build/default/lib/erlmcp_observability/ebin

main(_) ->
    io:format("Starting basic timeline profiling example~n"),

    %% Start profiler
    {ok, _Pid} = erlmcp_profiler:start_link(),

    %% Profile a simple function
    Fun = fun() ->
                  %% Simulate work
                  lists:sum(lists:seq(1, 100000))
          end,

    {ok, Result, Timeline} = erlmcp_profiler:profile_timeline(
        Fun,
        <<"basic_example">>
    ),

    io:format("Result: ~p~n", [Result]),
    io:format("Duration: ~p μs~n", [maps:get(total_duration_us, Timeline)]),

    %% Generate visualization
    {ok, SVG} = erlmcp_timeline_viz:generate_svg(Timeline),
    file:write_file("basic_timeline.svg", SVG),

    io:format("Timeline saved to: basic_timeline.svg~n"),
    ok.
```

**Run**:
```bash
escript basic_profiling.erl
```

### 2. Tool Invocation Profiling

**File**: `tool_profiling.erl`

```erlang
#!/usr/bin/env escript

main(_) ->
    io:format("Profiling tool invocation~n"),

    %% Profile fs:read_file tool
    {ok, Result, Timeline} = erlmcp_profiler:profile_tool_call(
        <<"fs">>,
        <<"read_file">>,
        #{path => <<"/etc/hostname">>},
        #{max_events => 10000}
    ),

    io:format("Tool result: ~p~n", [Result]),
    print_statistics(Timeline),

    %% Save profile
    ProfileId = maps:get(profile_id, Timeline),
    Filename = <<"tool_read_file_", ProfileId/binary, ".json">>,
    JSON = jsx:encode(Timeline, [space, {indent, 2}]),
    file:write_file(Filename, JSON),

    io:format("Profile saved to: ~s~n", [Filename]),
    ok.

print_statistics(Timeline) ->
    Stats = maps:get(statistics, Timeline),
    DurationUs = maps:get(total_duration_us, Timeline),
    EventCount = maps:get(event_count, Stats),

    io:format("~nStatistics:~n"),
    io:format("  Duration: ~p μs~n", [DurationUs]),
    io:format("  Events: ~p~n", [EventCount]).
```

**Run**:
```bash
escript tool_profiling.erl
```

### 3. Transport Comparison

**File**: `transport_comparison.erl`

```erlang
#!/usr/bin/env escript

main(_) ->
    io:format("Comparing transport performance~n"),

    MessageSizes = [1024, 4096, 16384, 65536],  %% 1KB, 4KB, 16KB, 64KB

    Results = lists:map(fun(Size) ->
        %% Profile stdio
        {ok, _, StdioTimeline} = erlmcp_profiler:profile_transport(<<"stdio">>, Size),
        StdioDuration = maps:get(total_duration_us, StdioTimeline),

        %% Profile tcp
        {ok, _, TcpTimeline} = erlmcp_profiler:profile_transport(<<"tcp">>, Size),
        TcpDuration = maps:get(total_duration_us, TcpTimeline),

        #{size => Size,
          stdio_us => StdioDuration,
          tcp_us => TcpDuration,
          ratio => StdioDuration / TcpDuration}
    end, MessageSizes),

    %% Print comparison table
    io:format("~n=== Transport Performance Comparison ===~n"),
    io:format("Size (B) | stdio (μs) | tcp (μs) | Ratio~n"),
    io:format("---------|------------|----------|-------~n"),

    lists:foreach(fun(R) ->
        Size = maps:get(size, R),
        Stdio = maps:get(stdio_us, R),
        Tcp = maps:get(tcp_us, R),
        Ratio = maps:get(ratio, R),

        io:format("~8b | ~10b | ~8b | ~.2fx~n",
                  [Size, Stdio, Tcp, Ratio])
    end, Results).

    ok.
```

**Run**:
```bash
escript transport_comparison.erl
```

### 4. Regression Detection

**File**: `regression_detection.erl`

```erlang
#!/usr/bin/env escript

main([BaselineFile]) ->
    io:format("Running regression detection~n"),

    %% Load baseline
    {ok, BaselineJSON} = file:read_file(BaselineFile),
    {ok, Baseline} = jsx:decode(BaselineJSON, [return_maps]),

    %% Profile current implementation
    {ok, _, Current} = erlmcp_profiler:profile_timeline(
        fun critical_operation/0,
        <<"current">>
    ),

    %% Compare
    Diff = erlmcp_profiler:compare_profiles(Baseline, Current),
    #{diff := DiffMap} = Diff,

    DurationPercentChange = maps:get(duration_percent_change, DiffMap),
    IsFaster = maps:get(faster, DiffMap),

    io:format("~nRegression Analysis:~n"),
    io:format("  Duration change: ~.1f%~n", [DurationPercentChange]),
    io:format("  Status: "),
    case IsFaster of
        true -> io:format("IMPROVED ✓~n");
        false when DurationPercentChange < 10.0 -> io:format("STABLE ✓~n");
        false -> io:format("REGRESSION ✗ (~.1f% slower)~n", [DurationPercentChange])
    end,

    %% Exit with error on regression
    case (not IsFaster) andalso (DurationPercentChange > 10.0) of
        true -> halt(1);
        false -> ok
    end.

critical_operation() ->
    %% Replace with actual operation to test
    lists:sort(lists:seq(1, 10000)).
```

**Run**:
```bash
# Create baseline
escript -erlc regression_detection.erl
escript regression_detection.erl baseline.json
```

### 5. Interactive HTML Visualization

**File**: `interactive_viz.erl`

```erlang
#!/usr/bin/env escript

main(_) ->
    io:format("Creating interactive visualization~n"),

    %% Profile operation
    Fun = fun() ->
        %% Simulate complex workflow
        lists:map(fun(X) -> X * 2 end, lists:seq(1, 1000))
    end,

    {ok, _, Timeline} = erlmcp_profiler:profile_timeline(Fun, <<"workflow">>),

    %% Generate interactive HTML
    {ok, HTML} = erlmcp_timeline_viz:generate_html(
        Timeline,
        #{width => 1600, height => 800}
    ),

    file:write_file("interactive_timeline.html", HTML),

    io:format("Interactive timeline saved to: interactive_timeline.html~n"),
    io:format("Open in a web browser to explore the timeline.~n"),
    ok.
```

**Run**:
```bash
escript interactive_viz.erl
open interactive_timeline.html
```

### 6. Batch Profiling

**File**: `batch_profiling.erl`

```erlang
#!/usr/bin/env escript

main([IterationsStr]) ->
    Iterations = list_to_integer(IterationsStr),
    io:format("Running batch profiling: ~p iterations~n", [Iterations]),

    %% Run multiple iterations
    Profiles = lists:map(fun(N) ->
        Label = iolist_to_binary(["iteration_", integer_to_binary(N)]),
        {ok, _, Timeline} = erlmcp_profiler:profile_timeline(
            fun test_operation/0,
            Label
        ),
        Timeline
    end, lists:seq(1, Iterations)),

    %% Aggregate profiles
    {ok, Aggregated} = erlmcp_profiler:aggregate_profiles(Profiles),

    %% Print statistics
    Stats = maps:get(statistics, Aggregated),
    AvgDuration = maps:get(avg_duration_us, Stats),
    TotalEvents = maps:get(total_events, Stats),

    io:format("~n=== Batch Results ===~n"),
    io:format("Iterations: ~p~n", [Iterations]),
    io:format("Avg duration: ~p μs~n", [AvgDuration]),
    io:format("Total events: ~p~n", [TotalEvents]),
    io:format("Events/iteration: ~p~n", [TotalEvents div Iterations]),

    %% Save aggregated profile
    JSON = jsx:encode(Aggregated, [space, {indent, 2}]),
    file:write_file("batch_profile.json", JSON),

    io:format("~nAggregated profile saved to: batch_profile.json~n"),
    ok.

test_operation() ->
    %% Simulate test operation
    timer:sleep(1),
    lists:sum(lists:seq(1, 100)).
```

**Run**:
```bash
escript batch_profiling.erl 10
```

### 7. Flamegraph Generation

**File**: `flamegraph.erl`

```erlang
#!/usr/bin/env escript

main(_) ->
    io:format("Generating flamegraph~n"),

    %% Profile with detailed call tracking
    Fun = fun() ->
        %% Simulate nested calls
        outer_function()
    end,

    {ok, _, Timeline} = erlmcp_profiler:profile_timeline(Fun, <<"flamegraph_test">>),

    %% Generate flamegraph
    {ok, FlameSVG} = erlmcp_timeline_viz:generate_flamegraph(Timeline),

    file:write_file("flamegraph.svg", FlameSVG),

    io:format("Flamegraph saved to: flamegraph.svg~n"),
    ok.

outer_function() ->
    inner_function1(),
    inner_function2().

inner_function1() ->
    lists:seq(1, 1000).

inner_function2() ->
    lists:map(fun(X) -> X * 2 end, lists:seq(1, 1000)).
```

**Run**:
```bash
escript flamegraph.erl
open flamegraph.svg
```

## Quick Reference

### Common Operations

**Profile function**:
```erlang
{ok, Result, Timeline} = erlmcp_profiler:profile_timeline(Fun, Label).
```

**Generate SVG**:
```erlang
{ok, SVG} = erlmcp_timeline_viz:generate_svg(Timeline).
```

**Generate HTML**:
```erlang
{ok, HTML} = erlmcp_timeline_viz:generate_html(Timeline).
```

**Export JSON**:
```erlang
{ok, JSON} = erlmcp_timeline_viz:export_json(Timeline).
```

**Compare profiles**:
```erlang
Diff = erlmcp_profiler:compare_profiles(Baseline, Current).
```

### CLI Commands

```bash
# Profile tool
erlmcp profile:tool fs read_file path=/etc/hosts

# Profile session
erlmcp profile:session abc123

# Profile transport
erlmcp profile:transport stdio 1024

# Visualize
erlmcp profile:visualize profile.json output.svg

# Compare
erlmcp profile:compare baseline.json current.json
```

## Tips

1. **Start simple**: Begin with basic profiling, then add options
2. **Use HTML for exploration**: Interactive HTML is best for understanding timelines
3. **SVG for reports**: Use SVG for static reports and documentation
4. **Compare against baselines**: Always maintain performance baselines
5. **Profile in isolation**: Profile specific operations, not entire systems
6. **Multiple iterations**: Aggregate 5-10 runs for statistical significance

## See Also

- [Timeline Profiler Documentation](../../docs/TIMELINE_PROFILER_OTP26.md)
- [OTP System Profiling](https://www.erlang.org/doc/system_principles/system_profiling.html)
- [MCP Specification](https://modelcontextprotocol.io/)
