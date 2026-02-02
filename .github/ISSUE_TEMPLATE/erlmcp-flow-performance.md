---
name: erlmcp-flow Performance Issue
about: Report a performance regression or bottleneck in erlmcp-flow
title: '[erlmcp-flow] Performance: '
labels: 'performance, erlmcp-flow'
assignees: ''
---

# erlmcp-flow Performance Issue

## Performance Issue Summary
<!-- One-sentence description of the performance problem -->



## Component
<!-- Which erlmcp-flow component has the performance issue? -->

- [ ] Registry (`erlmcp_flow_registry`) - Agent lookup
- [ ] Router (`erlmcp_flow_router`) - Message routing
- [ ] Transport (`erlmcp_flow_transport`) - Transport layer
- [ ] Agent (`erlmcp_flow_agent`) - Agent processing
- [ ] Bridge (stdio/TCP/HTTP) - Transport integration
- [ ] Flow Control (`erlmcp_flow_backpressure`) - Backpressure
- [ ] Serializer (`erlmcp_flow_serializer`) - Message serialization
- [ ] Overall system performance

## Performance Metrics

### Current Performance
<!-- What are the current metrics? -->

| Metric | Target | Current | Regression |
|--------|--------|---------|------------|
| Agent lookup (p50) | <10μs | _____μs | _____% |
| Agent lookup (p95) | <50μs | _____μs | _____% |
| Agent lookup (p99) | <100μs | _____μs | _____% |
| Direct messaging | >100K msg/sec | _____K msg/sec | _____% |
| Broadcast (60 agents) | <10ms avg | _____ms | _____% |
| Registry throughput | >500K lookups/sec | _____K lookups/sec | _____% |

### Performance Targets
<!-- What should the performance be? -->

| Metric | Target Value | Rationale |
|--------|--------------|-----------|
| _____ | _____ | _____ |

## Environment

**erlmcp Version**: _____ (e.g., 2.1.0)
**erlmcp_flow Version**: _____ (e.g., 1.0.0)
**Erlang/OTP Version**: _____ (run `erl -eval '{ok, V} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:fwrite(V), halt().' -noshell`)
**Operating System**: _____ (e.g., Ubuntu 22.04)
**Architecture**: _____ (e.g., x86_64)

**Hardware**:
- CPU: _____ cores, _____ GHz
- RAM: _____ GB
- Disk: SSD/HDD

**Load**:
- Number of agents: _____
- Messages per second: _____
- Concurrent connections: _____

## Benchmark Results

### Reproduction Script

```erlang
%% Run this script to reproduce the performance issue

-module(perf_issue_repro).
-export([run/0]).

run() ->
    % Setup
    application:start(erlmcp_flow),

    % Register agents
    lists:foreach(fun(N) ->
        AgentId = iolist_to_binary([<<"agent-">>, integer_to_binary(N)]),
        erlmcp_flow_registry:register_agent(AgentId, spawn(fun() -> receive _ -> ok end end), #{})
    end, lists:seq(1, 60)),

    % Benchmark
    Start = erlang:monotonic_time(microsecond),
    lists:foreach(fun(N) ->
        AgentId = iolist_to_binary([<<"agent-">>, integer_to_binary(N rem 60 + 1)]),
        erlmcp_flow_registry:lookup_agent(AgentId)
    end, lists:seq(1, 10000)),
    End = erlang:monotonic_time(microsecond),

    AvgLatency = (End - Start) / 10000,
    io:format("Average lookup latency: ~.2f μs~n", [AvgLatency]).
```

### Benchmark Output

```
% Copy benchmark output here
```

### Flamegraph / Profiling Results

<!-- Attach flamegraph image or profiling results -->

**fprof Results**:
```
% Copy fprof output
```

**eprof Results**:
```
% Copy eprof output
```

## Root Cause Analysis

### Suspected Bottleneck
<!-- What do you think is causing the performance issue? -->



### Evidence
<!-- What evidence supports this hypothesis? -->

1. **Profiling Data**:
   - Hot function: _________
   - CPU time: _____% of total
   - Call count: _____

2. **Memory Analysis**:
   - Memory usage: _____ MB (normal: _____ MB)
   - Garbage collection: _____ collections/sec
   - Process count: _____ (normal: _____)

3. **Message Queue Analysis**:
   - Queue length: _____ (normal: _____)
   - Backlogged process: _________

### Reproduction Steps

1. Setup environment:
   ```bash
   # Commands to setup
   ```

2. Run benchmark:
   ```bash
   # Commands to reproduce
   ```

3. Observe metrics:
   ```bash
   # Commands to measure
   ```

## Performance Regression

### Was this a regression?

- [ ] Yes (performance was better before)
- [ ] No (always been this slow)

**If regression**:

**Previous Good Version**: _____ (e.g., v1.0.0)
**First Bad Version**: _____ (e.g., v1.1.0)

**Commit Range**:
```bash
git log --oneline v1.0.0..v1.1.0
```

**Suspected Commit**: _____ (commit hash)

**Change Summary**: _________

### Benchmark Comparison

| Metric | v1.0.0 (good) | v1.1.0 (bad) | Regression |
|--------|---------------|--------------|------------|
| Latency | _____μs | _____μs | _____% |
| Throughput | _____K/sec | _____K/sec | _____% |

## Impact

**Severity**:
- [ ] Critical (>50% regression, production impact)
- [ ] High (>25% regression, noticeable slowdown)
- [ ] Medium (>10% regression, minor impact)
- [ ] Low (<10% regression, acceptable)

**Affected Operations**:
- [ ] Agent registration
- [ ] Agent lookup
- [ ] Message routing (direct)
- [ ] Message routing (broadcast)
- [ ] Message routing (gossip)
- [ ] Flow control
- [ ] All operations

**User Impact**:
- Number of affected users: _____
- Severity of impact: _____

## Proposed Optimization

### Optimization Approach
<!-- How can we fix this? -->



### Code Changes

**Before** (slow):
```erlang
% Current slow implementation
```

**After** (optimized):
```erlang
% Proposed optimized implementation
```

### Expected Improvement

| Metric | Current | After Optimization | Improvement |
|--------|---------|-------------------|-------------|
| Latency | _____μs | _____μs | _____% faster |
| Throughput | _____K/sec | _____K/sec | _____% higher |

### Trade-offs
<!-- Are there any trade-offs? -->

**Pros**:
-
-

**Cons**:
-
-

## Workarounds

### Temporary Workaround
<!-- Is there a workaround users can apply now? -->

```erlang
% Workaround code or configuration
```

**Limitations**:
-
-

## Benchmarking Plan

### Test Scenarios

1. **Baseline Test**:
   - Setup: _________
   - Workload: _________
   - Expected: _________

2. **Stress Test**:
   - Setup: _________
   - Workload: _________
   - Expected: _________

3. **Scalability Test**:
   - Setup: _________
   - Workload: _________
   - Expected: _________

### Success Criteria

- [ ] Latency improved by _____% (or meets target)
- [ ] Throughput improved by _____% (or meets target)
- [ ] No regression in other metrics
- [ ] Memory usage acceptable (< _____ MB)
- [ ] CPU usage acceptable (< _____%)

## Related Issues

- Related to #
- Regression from #
- Blocks #

## Checklist

- [ ] I have provided benchmark results
- [ ] I have profiled the code (fprof/eprof/flamegraph)
- [ ] I have identified the suspected bottleneck
- [ ] I have checked if this is a regression
- [ ] I have proposed an optimization approach
- [ ] I have specified the impact severity

---

**Reporter**: @_________
**Date**: 2026-02-02
**Urgency**: High/Medium/Low
