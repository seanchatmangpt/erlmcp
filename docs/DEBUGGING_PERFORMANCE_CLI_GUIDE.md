# Claude Code CLI Debugging & Performance Optimization Guide

## Overview

This guide documents how to leverage Claude Code CLI for debugging and performance optimization in the erlmcp project, based on proven patterns from extensive benchmarking and testing workflows.

## 1. Debugging Workflows and CLI Tools

### 1.1 Core Debugging Commands

```bash
# Compile validation (always first)
rebar3 compile
if [ $? -ne 0 ]; then echo "❌ Compilation failed"; exit 1; fi

# Interactive debugging
make console
```

### 1.2 Module-Specific Debugging

```erlang
% In erlang console
observer:start().                    % GUI process monitor
recon_trace:calls({module, function}, 100).  % Live trace
recon_trace:calls({erlmcp_json_rpc, encode, '_'}, 100).
```

### 1.3 Error Analysis Patterns

```bash
# Check test failures
rebar3 ct --suite=test/failure_suite --verbose

# Analyze crash dumps
erl -name debug@127.0.0.1 -pa ebin -eval "error_logger:logfile({filename, \"crash.log\"})" -s init stop -noshell
```

## 2. Performance Profiling Integration

### 2.1 Function-Level Profiling

```erlang
% fprof workflow
fprof:trace([start, {procs, [self()]}]),
% Run your code
fprof:profile(),
fprof:analyse([{dest, "profile.txt"}, {sort, self}]),
fprof:stop().
```

### 2.2 Time-Based Profiling

```erlang
% eprof for total time
eprof:start(),
eprof:profile([self()]),
% Execute test code
eprof:stop_profiling(),
eprof:analyse(total).
```

### 2.3 Hot Path Identification

```bash
# Identify functions with most calls
grep "calls" profile.txt | head -10

# Find functions with most time
grep "time" profile.txt | head -10
```

## 3. Memory Leak Detection and Prevention

### 3.1 Memory Monitoring

```erlang
% Monitor process memory
{memory, Bytes} = process_info(self(), memory),
io:format("Memory usage: ~p bytes~n", [Bytes]).

% Monitor all processes
[begin {P, M} = Pid, {P, process_info(P, memory)} end || Pid <- processes()].
```

### 3.2 Memory Leak Detection Scripts

```bash
# Memory tracking over time
#!/bin/bash
echo "Time,PID,Memory(bytes)" > memory.csv
while true; do
    ps -p $(pgrep -f erlang) -o pid,rss --no-headers | \
    awk '{print strftime("%H:%M:%S"), $1, $2*1024}' >> memory.csv
    sleep 5
done
```

### 3.3 Prevention Patterns

```erlang
% Bad: Accumulating in process state
State#state{items = [Item | State#state.items]}.

% Good: Using ETS or process dictionary
ets:new(my_cache, [set, public, named_table]).
ets:insert(my_cache, {Key, Value}).
```

## 4. Bottleneck Identification Strategies

### 4.1 Throughput Analysis

```erlang
% Registry bottleneck test
erlmcp_bench_core_ops:run(<<"registry_test_100k">>).

% Expected: Registry 553K msg/s
% If <400K: investigate registry implementation
% If <200K: critical bottleneck
```

### 4.2 Network Bottleneck Detection

```erlang
% TCP vs HTTP comparison
erlmcp_bench_network_real:run(<<"tcp_sustained_10k">>).
erlmcp_bench_network_real:run(<<"http_sustained_5k">>).

% Network I/O typically 43K msg/s (4KB packets)
% If <30K: network stack issue
% If >60K: measurement error
```

### 4.3 Database Bottlenecks

```bash
# Mnesia query analysis
mnesia:system_info(tables).
mnesia:table_info(table, size).

% Slow queries
mnesia:transaction(fun() ->
    mnesia:read({table, key})
end).
```

## 5. Performance Benchmarking Workflows

### 5.1 Benchmark Categories

```bash
# Core operations (in-memory)
./scripts/bench/run_core_ops_benchmarks.sh

# Network benchmarks (real sockets)
./scripts/bench/run_network_benchmarks.sh

# Stress testing (sustained load)
./scripts/bench/run_stress_tests.sh

# Chaos engineering (failure injection)
./scripts/bench/run_chaos_tests.sh

# Integration tests (e2e MCP)
./scripts/bench/run_integration_tests.sh
```

### 5.2 Benchmark Analysis

```erlang
% Parse results
Results = erlmcp_bench_results:parse("benchmark.json"),
Throughput = proplists:get_value(throughput_msg_per_s, Results),
io:format("Throughput: ~p msg/s~n", [Throughput]).

% Compare against baseline
Baseline = 2690000,  % 2.69M ops/sec
Regression = Throughput / Baseline,
if Regression < 0.9 ->
    io:format("❌ Performance regression: ~p%~n", [(1-Regression)*100]);
   true ->
    io:format("✅ Performance OK~n")
end.
```

## 6. Error Analysis and Debugging Patterns

### 6.1 Common Error Patterns

```erlang
% Pattern 1: Timeout issues
handle_call({request, Data}, From, State) ->
    case catch my_operation(Data) of
        {'EXIT', {timeout, _}} ->
            {reply, {error, timeout}, State};
        Result ->
            {reply, Result, State}
    end.

% Pattern 2: Memory limits
ensure_memory_limit(State) ->
    case process_info(self(), memory) of
        {memory, Mem} when Mem > 100000000 ->  % 100MB
            error(memory_limit);
        _ ->
            State
    end.
```

### 6.2 Error Recovery Patterns

```erlang
% Supervisor recovery
child_spec(_) ->
    {id, erlmcp_session_sup},
    {start, {erlmcp_session_sup, start_link, []}},
    {restart, transient},        % Restart after crashes
    {shutdown, 5000},
    {type, supervisor},
    {modules, [erlmcp_session_sup]}.
```

## 7. Monitoring and Observability Tools

### 7.1 Built-in Monitoring

```erlang
% Process monitoring
process_info(self(), messages).  % Message queue length
process_info(self(), status).   % Process status

% System monitoring
system_info(process_count).
system_info(memory).
```

### 7.2 Recon Integration

```erlang
% Top memory consumers
recon:memory(processes, 10).

% CPU usage
recon_cpuinfo:top(5).

% Long-running processes
recon:proc_window(10).
```

### 7.3 Custom Metrics

```erlang
% Define metrics
define_metric(request_duration, histogram).
define_metric(active_connections, gauge).

% Record metrics
record_request_duration(Duration),
record_active_connections(Count).
```

## 8. Performance Optimization Techniques

### 8.1 Code Optimizations

```erlang
% Bad: String concatenation
lists:foldl(fun(A, Acc) -> Acc ++ A end, [], Lists).

% Good: Binary concatenation
lists:foldl(fun(A, Acc) <<Acc/binary, A/binary>>, <<>>, Binaries).

% Bad: Pattern matching in hot path
case Data of
    {tag, value} -> do_something();
    _ -> do_other()
end.

% Good: Tag test
erlang:element(1, Data) =:= tag.
```

### 8.2 Data Structure Choices

```erlang
% For small datasets (<1000 elements)
gb_sets:set()        % Fast membership testing
gb_trees:empty()    % Ordered access

% For large datasets
ets:new(table, [set, public, ordered_set]).  % Fast, persistent
mnesia:create_table(...).  % Distributed, ACID
```

### 8.3 Concurrency Patterns

```erlang
% Pool pattern
{ok, Pid} = poolboy:start_link([{worker_module, my_worker},
                               {size, 10},
                               {max_overflow, 5}]),
poolboy:transaction(Pool, fun(Worker) -> gen_server:call(Worker, request) end).

% Queue pattern
{ok, Q} = queue:start([{capacity, 1000}]),
queue:enqueue(Q, task_data).
```

## 9. Load Testing and Stress Testing

### 9.1 Load Test Patterns

```bash
# Connection flood test
./bench/run_port_exhaustion_test.sh

# Message rate test
./bench/erlmcp_bench_stress.erl

# Memory exhaustion test
./bench/run_memory_exhaustion_test.sh
```

### 9.2 Test Scenarios

```erlang
% Sustained load test
erlmcp_bench_stress:run(<<"sustained_5min_100k_ops">>).

% Burst load test
erlmcp_bench_stress:run(<<"burst_10k_ops_1min">>).

% Degradation test
erlmcp_bench_stress:run(<<"degradation_1hr_10k_ops">>).
```

### 9.3 Chaos Engineering

```erlang
% Kill random processes
erlmcp_bench_chaos:run(<<"process_kill_5percent">>).

% Network partition
erlmcp_bench_chaos:run(<<"network_partition_10s">>).

% Memory pressure
erlmcp_bench_chaos:run(<<"memory_pressure_90percent">>).
```

## 10. Performance Reporting and Metrics

### 10.1 Standard Metrics Format

```json
{
    "workload_id": "tcp_sustained_10k",
    "throughput_msg_per_s": 43210,
    "latency_p50_us": 1500,
    "latency_p95_us": 4500,
    "latency_p99_us": 12000,
    "memory_heap_mib_per_conn": 0.5,
    "memory_rss_mib_per_node": 128,
    "transport": "tcp",
    "duration_s": 30,
    "scope": "per_connection",
    "precision": "microseconds"
}
```

### 10.2 Performance Reports

```bash
# Generate report
./scripts/performance/generate_report.sh

# Compare baselines
./scripts/performance/compare_baseline.sh new_results.json baseline.json

# Trend analysis
./scripts/performance/analyze_trends.sh
```

### 10.3 Quality Gates

```bash
# Performance regression check
./scripts/performance/check_regression.sh --threshold 10

# Minimum throughput
./scripts/performance/check_throughput.sh --minimum 100000

# Memory limits
./scripts/performance/check_memory.sh --maximum 256
```

## CLI Command Reference

```bash
# Quick commands
make benchmark-quick          # Fast benchmark (< 2min)
make check                    # Full validation
make observer                 # Process visualization
make console                  # Interactive shell

# Testing
rebar3 eunit --verbose        # Detailed unit tests
rebar3 ct --suite=test/*      # All integration tests
rebar3 cover                 # Coverage report

# Profiling
./scripts/profile/fprof.sh    # Function profiling
./scripts/profile/eprof.sh   # Time profiling
./scripts/profile/memory.sh   # Memory profiling
```

## Best Practices

1. **Measure First**: Always establish baselines before optimization
2. **Test Continuously**: Run benchmarks after every change
3. **Document Everything**: Keep track of all measurements
4. **Version Control**: Store benchmark results in git
5. **Automate**: Integrate quality gates into CI/CD
6. **Monitor Production**: Use the same tools in production
7. **Iterate**: Optimization is an ongoing process
8. **Balance**: Don't optimize prematurely; measure first

## Troubleshooting Checklist

- [ ] Compilation successful (0 errors)
- [ ] All tests passing (100% pass rate)
- [ ] Benchmarks within 10% of baseline
- [ ] No memory leaks over time
- [ ] Error handling robust
- [ ] Supervision trees intact
- [ ] Network I/O not bottleneck
- [ ] Database queries efficient

## Real Examples from erlmcp

### Memory Leak Detection

```bash
# Run memory test
./bench/erlmcp_bench_memory_leak.erl

# Output example:
# Before test: 24.5 MiB
# After 10min: 24.5 MiB  ✅ No leak
# After 10min: 125.3 MiB ❌ Leak detected
```

### Profiling Integration

```bash
# Profile JSON encoding
./scripts/profile/profile_json_encoding.sh

# Results:
# jsx:encode/1: 72% of time
# jiffy:encode/1: 3% of time
# Recommendation: Switch to jiffy for large messages
```

### Performance Regression

```bash
# Check for regressions
./scripts/performance/check_regression.sh

# Output:
# WARNING: Throughput decreased by 15% (2.69M → 2.28M ops/sec)
# Investigate: Recent changes to registry.erl
```

### Load Testing Results

```json
{
    "workload_id": "stress_5min_100k_ops",
    "throughput_msg_per_s": 372000,
    "latency_p50_us": 1200,
    "latency_p95_us": 3200,
    "memory_rss_mib_per_node": 96,
    "status": "degraded",
    "recommendation": "Scale to 3 nodes"
}
```

## Advanced Debugging Examples

### Memory Leak Detection with Linear Regression

```erlang
% From erlmcp_bench_stress.erl - Memory leak detection
linear_regression_timestamps(Timestamps, Values) ->
    LinearRegression = linear_regression:fit(Timestamps, Values),
    Slope = linear_regression:slope(LinearRegression),
    
    % Memory growth > 1 MiB/minute indicates leak
    if Slope > (1024*1024/60) ->  % 1 MiB per minute
        io:format("❌ Memory leak detected: ~p bytes/sec~n", [Slope]),
        leak_detected;
       true ->
        io:format("✅ No significant memory growth~n"),
        clean
    end.
```

### Live Performance Monitoring

```erlang
% Real-time performance dashboard
start_performance_monitor() ->
    spawn(fun() ->
        monitor_loop(#{timestamp => [], throughput => [], 
                      memory => [], latency_p95 => []})
    end).

monitor_loop(State) ->
    Timestamp = erlang:system_time(millisecond),
    Metrics = collect_metrics(),
    
    % Update state
    NewState = State#{timestamp => [Timestamp | State#timestamp],
                     throughput => [Metrics#throughput | State#throughput]},
    
    % Check for degradation
    case detect_degradation(NewState) of
        degraded ->
            send_alert("Performance degradation detected"),
            degraded;
        _ ->
            monitor_loop(NewState)
    end after 5000.  % Sample every 5 seconds
```

### Profile-Driven Optimization

```bash
# Generate optimization report
./scripts/optimize/generate_report.sh

# Example output:
# === OPTIMIZATION OPPORTUNITIES ===
# 1. erlmcp_json_rpc:encode/1 - 72% CPU time
#    - Current: jsx:encode/1 (JSON library)
#    - Alternative: jiffy:encode/1 (2-3x faster)
#    - Potential improvement: 65% faster encoding
#
# 2. Registry lookup - 15% CPU time
#    - Current: gproc:lookup_local_name/1
#    - Alternative: Direct ETS access
#    - Potential improvement: 30% faster lookups
#
# 3. String concatenation - 8% CPU time
#    - Current: list_to_binary/1 in loop
#    - Alternative: binary:concat/2
#    - Potential improvement: 40% faster
```

## Integration with CI/CD

### GitHub Actions Quality Gates

```yaml
# .github/workflows/quality-check.yml
name: Quality Check
on: [push, pull_request]

jobs:
  performance:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - run: rebar3 compile
      - run: make benchmark-quick
      - name: Check performance
        run: ./scripts/performance/check_regression.sh --threshold 5
      - name: Upload results
        uses: actions/upload-artifact@v3
        with:
          name: performance-results
          path: bench/results/
```

### Pre-commit Hooks

```bash
# .git/hooks/pre-commit
#!/bin/bash
# Run quality checks before commit
rebar3 compile || exit 1
rebar3 eunit || exit 1
rebar3 ct --suite=test/ --skip_case=failing_test || exit 1
make benchmark-quick || exit 1
```

## Production Monitoring Integration

### Telemetry Integration

```erlang
% OpenTelemetry integration
telemetry:start(),
telemetry:attach(
    "request-duration",
    [{[:erlmcp, :request, :start], []}],
    fn event, measurements, metadata ->
        Duration = proplists:get_value(duration, measurements),
        telemetry:execute(
            [:erlmcp, :request, :stop],
            #{duration => Duration},
            metadata
        )
    end,
    []
).

% Export metrics
telemetry_exporter:start_link(exporter, otlp,
    #{endpoint => "http://otel-collector:4317"})
```

### Alerting Rules

```yaml
# monitoring/alerts.yml
groups:
- name: erlmcp_alerts
  rules:
  - alert: HighLatency
    expr: histogram_quantile(0.99, erlmcp_request_duration_seconds) > 5
    for: 5m
    labels:
      severity: warning
    annotations:
      summary: "High P99 latency detected"
      
  - alert: MemoryLeak
    expr: rate(erlmcp_memory_bytes[5m]) > 1048576  # 1 MiB/min
    for: 10m
    labels:
      severity: critical
    annotations:
      summary: "Potential memory leak detected"
```

## Performance Debugging Toolkit

### Quick Diagnostic Commands

```bash
# System health check
./scripts/health/check.sh

# Process tree analysis
./scripts/analyze/process_tree.sh

# Message queue analysis
./scripts/analyze/queue_depth.sh

# Memory usage by type
./scripts/analyze/memory_breakdown.sh

# Network socket analysis
./scripts/analyze/network_sockets.sh
```

### Interactive Debug Mode

```erlang
% Start interactive debugger
start_debug_session() ->
    spawn_link(fun() ->
        register(debug_session, self()),
        debug_loop(#{metrics => [], errors => []})
    end).

debug_loop(State) ->
    receive
        {get_metrics, Pid} ->
            Pid ! {metrics, State#metrics},
            debug_loop(State);
        {record_error, Error} ->
            NewState = State#{errors => [Error | State#errors]},
            debug_loop(NewState);
        {analyze, Pid} ->
            Analysis = analyze_problems(State),
            Pid ! {analysis, Analysis},
            debug_loop(State)
    end.
```

## Performance Optimization Roadmap

### Phase 1: Quick Wins (1-2 days)
- [ ] Replace jsx with jiffy for large messages
- [ ] Optimize registry lookups with ETS
- [ ] Fix string concatenation in hot paths
- [ ] Cache frequently accessed data

### Phase 2: Architecture Improvements (1 week)
- [ ] Implement connection pooling
- [ ] Add request queuing for overload protection
- [ ] Optimize supervision tree structure
- [ ] Implement circuit breakers

### Phase 3: Advanced Optimizations (2 weeks)
- [ ] Implement worker stealing
- [ ] Add adaptive load balancing
- [ ] Optimize memory allocation patterns
- [ ] Implement zero-copy optimizations

### Phase 4: Monitoring & Observability (ongoing)
- [ ] Implement comprehensive metrics
- [ ] Add performance dashboards
- [ ] Create alerting system
- [ ] Establish performance baselines
```

