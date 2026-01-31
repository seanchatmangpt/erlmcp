# erlmcp_supervisor_utils - Supervisor Introspection Utilities

## Overview

`erlmcp_supervisor_utils` provides comprehensive runtime analysis and monitoring of OTP supervision trees. This module enables deep introspection of supervisor hierarchies for debugging, health monitoring, and system observability.

## Joe Armstrong Philosophy

> "The supervision tree is the architecture of your system.
>  Make it visible. Make it inspectable. Make it understandable."

This module embodies the philosophy that supervision trees should be first-class citizens in system observability, not hidden implementation details.

## Features

### 1. **Children Status Retrieval**
- Get detailed status of all direct children of a supervisor
- Includes PID, type (worker/supervisor), modules, and process state
- Handles dead, restarting, and not-started processes gracefully

### 2. **Supervision Tree Traversal**
- Recursively traverse entire supervision tree
- Supports both nested and flat representations
- Preserves hierarchical structure for visualization

### 3. **Health Scoring**
- Calculates 0.0-1.0 health score based on:
  - Dead/restarting processes (-0.5 penalty)
  - High message queues >1000 (-0.3 penalty)
  - High memory usage >100MB (-0.2 penalty)
- Recursive scoring across entire tree
- Useful for automated alerting and dashboards

### 4. **JSON Export**
- Export entire supervision tree to JSON
- Pretty-printing support for human readability
- Includes comprehensive metrics in export
- Integration with external monitoring tools

### 5. **Performance Metrics**
- Per-process metrics: memory, queue length, reductions
- Tree-wide aggregates: total memory, process count, max depth
- Compatible with Prometheus, Grafana, and other monitoring systems

### 6. **Problem Detection**
- Automatically find unhealthy processes
- Detect high message queues (>1000)
- Detect high memory usage (>100MB)
- Identify dead/restarting processes

### 7. **Validation**
- Validate supervision tree health
- Configurable health checks
- Returns violations with detailed reasons

## API Reference

### Basic Introspection

```erlang
%% Get status of all direct children
-spec get_children_status(sup_ref()) -> [child_status()].
Children = erlmcp_supervisor_utils:get_children_status(erlmcp_sup).

%% Get complete supervision tree
-spec get_supervision_tree(sup_ref()) -> supervision_tree().
Tree = erlmcp_supervisor_utils:get_supervision_tree(erlmcp_sup).

%% Get flattened tree (all processes in list)
-spec get_supervision_tree_flat(sup_ref()) -> [#{pid := pid(), supervisor := sup_ref()}].
Flat = erlmcp_supervisor_utils:get_supervision_tree_flat(erlmcp_sup).

%% Count total processes in tree
-spec count_processes(sup_ref()) -> non_neg_integer().
Count = erlmcp_supervisor_utils:count_processes(erlmcp_sup).
```

### Health Monitoring

```erlang
%% Calculate health score (0.0 = critical, 1.0 = healthy)
-spec calculate_health_score(sup_ref()) -> health_score().
Score = erlmcp_supervisor_utils:calculate_health_score(erlmcp_sup).
%% Score: 0.95 (healthy)

%% Find unhealthy processes
-spec find_unhealthy_processes(sup_ref()) -> [#{pid := pid(), reason := term()}].
Problems = erlmcp_supervisor_utils:find_unhealthy_processes(erlmcp_sup).
%% [#{pid => <0.123.0>, reason => high_queue, queue_len => 1500}]

%% Validate supervision tree
-spec validate_supervision_tree(sup_ref()) ->
    {ok, #{valid := true}} | {error, #{violations := [term()]}}.
erlmcp_supervisor_utils:validate_supervision_tree(erlmcp_sup).
```

### Metrics & Export

```erlang
%% Get process metrics
-spec get_process_metrics(pid()) -> map() | {error, term()}.
Metrics = erlmcp_supervisor_utils:get_process_metrics(Pid).
%% #{memory_bytes => 2048, message_queue_len => 0, ...}

%% Get tree-wide metrics
-spec get_tree_metrics(sup_ref()) -> tree_metrics().
TreeMetrics = erlmcp_supervisor_utils:get_tree_metrics(erlmcp_sup).
%% #{total_supervisors => 5, total_workers => 12, total_memory_bytes => 1048576, ...}

%% Export to JSON
-spec export_to_json(sup_ref()) -> binary().
Json = erlmcp_supervisor_utils:export_to_json(erlmcp_sup).

%% Export to pretty-printed JSON
-spec export_to_json_pretty(sup_ref()) -> binary().
PrettyJson = erlmcp_supervisor_utils:export_to_json_pretty(erlmcp_sup).
file:write_file("/tmp/tree.json", PrettyJson).

%% Get restart statistics
-spec get_restart_statistics(sup_ref()) -> map().
Stats = erlmcp_supervisor_utils:get_restart_statistics(erlmcp_sup).
```

## Health Scoring Criteria

| Condition | Penalty | Threshold |
|-----------|---------|-----------|
| Dead process | -0.5 | Process is dead |
| Restarting | -0.3 | Process is restarting |
| Not started | -0.4 | Process never started |
| High queue | -0.3 | Queue length > 1000 |
| High queue warning | -0.1 | Queue length > 100 |
| High memory | -0.2 | Memory > 100MB |
| High memory warning | -0.1 | Memory > 50MB |

**Score Interpretation:**
- **1.0 - 0.9**: Healthy (green)
- **0.9 - 0.7**: Degraded (yellow)
- **0.7 - 0.5**: Warning (orange)
- **< 0.5**: Critical (red)

## Usage Examples

### Example 1: Basic Status Check

```erlang
%% In Erlang shell
1> Children = erlmcp_supervisor_utils:get_children_status(erlmcp_sup).
[#{id => erlmcp_core_sup,
   pid => <0.156.0>,
   type => supervisor,
   modules => [erlmcp_core_sup],
   status => #{child_count => 15}},
 #{id => erlmcp_server_sup,
   pid => <0.178.0>,
   type => supervisor,
   modules => [erlmcp_server_sup],
   status => #{child_count => 3}}]

2> length(Children).
2
```

### Example 2: Health Monitoring Dashboard

```erlang
%% Create a simple monitoring dashboard
monitor_dashboard() ->
    Metrics = erlmcp_supervisor_utils:get_tree_metrics(erlmcp_sup),
    Score = erlmcp_supervisor_utils:calculate_health_score(erlmcp_sup),
    Problems = erlmcp_supervisor_utils:find_unhealthy_processes(erlmcp_sup),

    io:format("=== erlmcp Health Dashboard ===~n"),
    io:format("Health Score: ~.2f~n", [Score]),
    io:format("Total Processes: ~p~n", [maps:get(total_processes, Metrics)]),
    io:format("Memory (MB): ~.2f~n", [maps:get(total_memory_bytes, Metrics) / 1048576]),
    io:format("Unhealthy: ~p~n", [length(Problems)]),

    %% Alert if critical
    if
        Score < 0.5 ->
            io:format("⚠️  ALERT: System health critical!~n");
        true ->
            ok
    end.
```

### Example 3: JSON Export for External Tools

```erlang
%% Export supervision tree for Grafana/Prometheus
export_for_monitoring() ->
    %% Export to JSON file
    Json = erlmcp_supervisor_utils:export_to_json_pretty(erlmcp_sup),
    file:write_file("/var/lib/erlmcp/supervision_tree.json", Json),

    %% Also extract metrics for Prometheus scraping
    Metrics = erlmcp_supervisor_utils:get_tree_metrics(erlmcp_sup),

    PrometheusFormat = [
        io_lib:format("erlmcp_supervisors ~p~n", [maps:get(total_supervisors, Metrics)]),
        io_lib:format("erlmcp_workers ~p~n", [maps:get(total_workers, Metrics)]),
        io_lib:format("erlmcp_health_score ~.2f~n", [maps:get(health_score, Metrics)])
    ],

    file:write_file("/var/lib/erlmcp/metrics.prom", PrometheusFormat).
```

### Example 4: Integration with Health Monitor

```erlang
%% Register supervision tree health check
start_health_monitoring() ->
    HealthCheckFun = fun() ->
        Score = erlmcp_supervisor_utils:calculate_health_score(erlmcp_sup),
        Problems = erlmcp_supervisor_utils:find_unhealthy_processes(erlmcp_sup),

        case {Score, length(Problems)} of
            {S, 0} when S >= 0.9 -> healthy;
            {S, _} when S >= 0.7 -> degraded;
            {S, _} when S >= 0.5 -> {degraded, {score, S}};
            {S, _} -> {unhealthy, {score, S}}
        end
    end,

    erlmcp_health_monitor:register_component(
        erlmcp_supervision_tree,
        self(),
        HealthCheckFun
    ).
```

### Example 5: Continuous Monitoring Loop

```erlang
%% Monitor every 10 seconds
monitoring_loop() ->
    Score = erlmcp_supervisor_utils:calculate_health_score(erlmcp_sup),
    Problems = erlmcp_supervisor_utils:find_unhealthy_processes(erlmcp_sup),

    logger:info("Health: ~.2f, Problems: ~p", [Score, length(Problems)]),

    %% Alert if degraded
    if
        Score < 0.7 ->
            erlmcp_health_monitor:report_degradation(erlmcp_supervision_tree);
        true ->
            ok
    end,

    timer:sleep(10000),
    monitoring_loop().
```

## JSON Export Format

### Example Output

```json
{
  "tree": {
    "supervisor": "erlmcp_sup",
    "pid": "<0.123.0>",
    "health_score": 0.95,
    "children": [
      {
        "id": "erlmcp_core_sup",
        "type": "supervisor",
        "pid": "<0.124.0>",
        "modules": ["erlmcp_core_sup"],
        "tree": {
          "supervisor": "<0.124.0>",
          "pid": "<0.124.0>",
          "health_score": 0.98,
          "children": [...]
        }
      },
      {
        "id": "erlmcp_server_sup",
        "type": "supervisor",
        "pid": "<0.125.0>",
        "modules": ["erlmcp_server_sup"]
      }
    ]
  },
  "metrics": {
    "total_supervisors": 5,
    "total_workers": 12,
    "total_processes": 17,
    "total_memory_bytes": 8388608,
    "total_memory_mb": 8.0,
    "max_depth": 3,
    "health_score": 0.95,
    "unhealthy_count": 0
  },
  "timestamp": 1706745600
}
```

## Testing

### Run Tests

```bash
# Run all tests
./scripts/test_supervisor_utils.sh

# Or manually:
rebar3 eunit --module=erlmcp_supervisor_utils_tests
rebar3 cover
```

### Test Coverage

The test suite includes:
- ✓ 20+ EUnit tests with real OTP processes
- ✓ Chicago School TDD (NO mocks, fakes, or placeholders)
- ✓ Integration tests with erlmcp_sup and erlmcp_core_sup
- ✓ Stress tests with nested supervision trees
- ✓ Error handling tests

**Target Coverage**: ≥80%

## Integration Points

### erlmcp_health_monitor

```erlang
%% Register supervision tree health check
erlmcp_health_monitor:register_component(
    erlmcp_supervision_tree,
    self(),
    fun() ->
        Score = erlmcp_supervisor_utils:calculate_health_score(erlmcp_sup),
        if Score >= 0.9 -> healthy;
           Score >= 0.7 -> degraded;
           true -> unhealthy
        end
    end
).
```

### erlmcp_dashboard_server

```erlang
%% Add supervision tree endpoint to dashboard
handle_request("/supervision_tree", _Req) ->
    Json = erlmcp_supervisor_utils:export_to_json_pretty(erlmcp_sup),
    {200, [{<<"content-type">>, <<"application/json">>}], Json}.
```

### erlmcp_metrics

```erlang
%% Export metrics for Prometheus
collect_metrics() ->
    Metrics = erlmcp_supervisor_utils:get_tree_metrics(erlmcp_sup),
    [
        {erlmcp_supervisors, maps:get(total_supervisors, Metrics)},
        {erlmcp_workers, maps:get(total_workers, Metrics)},
        {erlmcp_health_score, maps:get(health_score, Metrics)}
    ].
```

## Performance Considerations

### Benchmarks (Expected)

- **get_children_status**: ~100μs for 10 children
- **get_supervision_tree**: ~1ms for 50-process tree
- **calculate_health_score**: ~500μs for 50-process tree
- **export_to_json**: ~2ms for 50-process tree

### Best Practices

1. **Avoid Frequent Deep Scans**: Cache tree results when possible
2. **Use Flattened Trees**: For simple process counting
3. **Batch Operations**: Collect metrics once, use multiple times
4. **Set Timeouts**: sys:get_state uses 1000ms timeout by default

## Error Handling

The module is designed to be fault-tolerant:
- Returns empty lists on non-existent supervisors
- Gracefully handles dead processes
- Logs warnings instead of crashing
- Uses try/catch extensively

```erlang
%% Example error handling
case erlmcp_supervisor_utils:get_children_status(bad_supervisor) of
    [] -> logger:warning("Supervisor not found");
    Children -> process_children(Children)
end.
```

## Changelog

### v1.0.0 (2026-01-31)

**Initial Release**

- ✓ Core introspection API (11 functions)
- ✓ Health scoring algorithm
- ✓ JSON export with jsx integration
- ✓ Comprehensive metrics collection
- ✓ Problem detection and validation
- ✓ 20+ EUnit tests (Chicago School TDD)
- ✓ Full type specifications
- ✓ Integration with erlmcp supervision architecture

## Future Enhancements

- [ ] PropertyBased tests with PropEr
- [ ] WebSocket streaming for live updates
- [ ] Historical health trending
- [ ] Automatic anomaly detection
- [ ] Integration with erlmcp_chaos for failure testing
- [ ] GraphQL API for tree queries

## See Also

- **erlmcp_health_monitor**: Health monitoring and alerting
- **erlmcp_dashboard_server**: Web-based visualization
- **erlmcp_metrics**: Metrics collection and export
- **supervisor**: OTP supervisor behavior
- **sys**: System messages and debugging

## License

Copyright (c) 2026 erlmcp

Licensed under the Apache License, Version 2.0.
