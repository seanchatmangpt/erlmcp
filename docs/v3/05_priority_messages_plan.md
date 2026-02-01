# Priority Messages Implementation Plan - erlmcp v3

**Version:** 3.0.0
**Status:** Design Specification
**Author:** erlmcp OTP Development Team
**Date:** 2026-01-31

---

## Executive Summary

This document details the implementation of **OTP 28 Priority Messages** for erlmcp v3, focusing on circuit breaker state transitions, interference handling, starvation prevention, and priority monitoring.

### Current State Analysis

**Implemented:**
- `erlmcp_health_monitor` - Priority messages enabled (line 140-141)
- `erlmcp_graceful_drain` - Priority shutdown via `initiate_shutdown/1` (line 67-71)
- `erlmcp_circuit_breaker` - Basic priority state change notifications (line 343, 412, 468)
- Priority metrics tracking: `priority_messages_delivered`, `priority_latency_sum_us`

**Missing:**
- Circuit breaker priority state transitions NOT using priority for gen_statem calls
- NO interference detection/prevention mechanisms
- NO starvation prevention for normal messages
- NO priority message queue monitoring
- Incomplete test coverage for edge cases

### Problem Statement

The circuit breaker uses `erlang:send/3` with `[nosuspend]` for state change notifications, but:
1. **gen_statem calls are NOT prioritized** - `handle_call` messages process in FIFO order
2. **No interference detection** - Priority messages can be delayed by normal messages during high load
3. **No starvation protection** - Normal messages could be starved by priority floods
4. **No queue monitoring** - Cannot detect priority queue saturation or latency spikes

---

## Architecture Design

### 1. Circuit Breaker Priority State Transitions

#### Current Implementation Issues

**Problem:** `notify_state_change_priority/2` uses `erlang:send/3` but gen_statem `handle_call` is NOT prioritized.

```erlang
% Current (line 674) - NOT effective for gen_statem calls
notify_state_change_priority(Name, NewState) ->
    case whereis(erlmcp_health_monitor) of
        undefined -> ok;
        Pid ->
            % This is NOT a priority message for gen_statem calls!
            erlang:send(Pid, {circuit_breaker_state_change, Name, NewState}, [nosuspend]),
            ok
    end.
```

**Issue:** State transitions triggered by `gen_statem:call/3` are NOT priority messages.

#### Solution: Dual-Channel State Transitions

**Design Pattern:** Separate priority channel for critical state transitions.

```erlang
% New module: erlmcp_priority_state_transition
-behaviour(gen_server).

-record(state, {
    target_pid :: pid(),
    transition_queue :: queue:queue({atom(), atom()}),
    pending_acks :: #{reference() => {atom(), atom(), integer()}}
}).

% Priority state transition API
transition_state(TargetPid, FromState, ToState) ->
    % Use alias-based priority message
    Alias = erlang:monitor(process, TargetPid, [{alias, explicit_unalias}]),
    Alias ! {priority_state_transition, FromState, ToState, self()},
    receive
        {state_transition_ack, FromState, ToState} -> ok
    after 1000 ->
        {error, timeout}
    end.
```

#### Integration with Circuit Breaker

**Modified Circuit Breaker (erlmcp_circuit_breaker):**

```erlang
% Replace direct state change calls with priority transitions
closed(enter, _OldState, Data) ->
    StartTime = erlang:monotonic_time(microsecond),

    % NEW: Use priority state transition
    ok = erlmcp_priority_state_transition:transition_state(
        whereis(erlmcp_health_monitor),
        closed,
        Data#data.name
    ),

    EndTime = erlang:monotonic_time(microsecond),
    LatencyUs = EndTime - StartTime,

    NewData = Data#data{
        priority_messages_delivered = Data#data.priority_messages_delivered + 1,
        priority_latency_sum_us = Data#data.priority_latency_sum_us + LatencyUs
    },
    {keep_state, NewData};
```

**Advantages:**
1. **Priority preemption** - State changes jump normal message queue
2. **Observable latency** - Track transition delivery time
3. **No blocking** - Asynchronous with timeout protection
4. **Graceful degradation** - Falls back to normal send if alias unavailable

---

### 2. Interference Detection and Prevention

#### Problem: Priority Message Interference

**Scenario:** Under high load (1000+ normal messages), priority messages can be delayed by:
1. Mailbox scanning overhead
2. GC pauses during message processing
3. Process heap allocation contention

#### Solution: Interference Detection System

**New Module: `erlmcp_priority_interference`**

```erlang
-module(erlmcp_priority_interference).
-behaviour(gen_server).

-record(interference_state, {
    priority_targets = #{} :: #{pid() => priority_stats()},
    interference_threshold_ms = 10 :: pos_integer(),
    last_check_time :: integer()
}).

-record(priority_stats, {
    pid :: pid(),
    normal_message_count = 0 :: non_neg_integer(),
    priority_message_count = 0 :: non_neg_integer(),
    avg_priority_latency_us = 0 :: non_neg_integer(),
    interference_detected = false :: boolean(),
    last_interference_time :: undefined | integer()
}).

% API
detect_interference(TargetPid) ->
    gen_server:call(?MODULE, {detect_interference, TargetPid}).

get_interference_stats(TargetPid) ->
    gen_server:call(?MODULE, {get_stats, TargetPid}).
```

**Detection Algorithm:**

```erlang
% Periodic interference check (every 100ms)
handle_info(check_interference, State) ->
    Now = erlang:monotonic_time(millisecond),

    NewStats = maps:map(fun(Pid, Stats) ->
        % Get process info
        {message_queue_len, QLen} = process_info(Pid, message_queue_len),
        {memory, Memory} = process_info(Pid, memory),

        % Check for interference indicators
        InterferenceDetected = detect_interference_indicators(Pid, Stats),

        Stats#priority_stats{
            interference_detected = InterferenceDetected,
            last_interference_time = case InterferenceDetected of
                true -> Now;
                false -> Stats#priority_stats.last_interference_time
            end
        }
    end, State#interference_state.priority_targets),

    erlang:send_after(100, self(), check_interference),
    {noreply, State#interference_state{priority_targets = NewStats}}.

detect_interference_indicators(Pid, Stats) ->
    % Indicator 1: Queue length > 100 with priority latency > threshold
    {message_queue_len, QLen} = process_info(Pid, message_queue_len),
    AvgLatency = Stats#priority_stats.avg_priority_latency_us,

    QueueInterference = QLen > 100 andalso AvgLatency > ?PRIORITY_LATENCY_THRESHOLD_US,

    % Indicator 2: Priority message starvation (> 50 normal messages per priority)
    PriorityRatio = case Stats#priority_stats.normal_message_count of
        0 -> 0;
        NormalCount -> Stats#priority_stats.priority_message_count / NormalCount
    end,
    StarvationInterference = PriorityRatio < 0.02, % < 2% priority ratio

    % Indicator 3: GC pauses during priority processing
    GcInfo = process_info(Pid, garbage_collection),
    GcInterference = detect_gc_interference(GcInfo),

    QueueInterference orelse StarvationInterference orelse GcInterference.
```

**Prevention Mechanisms:**

```erlang
% When interference detected
handle_cast({interference_detected, Pid, Reason}, State) ->
    ?LOG_WARNING("Interference detected for ~p: ~p", [Pid, Reason]),

    % Prevention: Increase process priority to max
    process_flag(Pid, priority, high),

    % Prevention: Enable off-heap message queue data
    process_flag(Pid, message_queue_data, off_heap),

    % Prevention: Enable fullsweep after GC
    process_flag(Pid, fullsweep_after, 0),

    % Notify health monitor for escalation
    erlmcp_health_monitor:report_degradation(priority_interference),

    {noreply, State}.
```

---

### 3. Starvation Prevention for Normal Messages

#### Problem: Priority Flood

**Scenario:** If 10K priority messages flood the queue, normal messages starve.

**Solution: Token Bucket Rate Limiting**

**New Module: `erlmcp_priority_token_bucket`**

```erlang
-module(erlmcp_priority_token_bucket).

-record(bucket, {
    capacity :: pos_integer(),      % Max tokens (e.g., 100)
    tokens :: pos_integer(),         % Current tokens
    refill_rate :: number(),         % Tokens per ms (e.g., 0.1 = 10/sec)
    last_refill :: integer()         % Last refill timestamp (ms)
}).

% API
new_bucket(Capacity, RefillRate) ->
    Now = erlang:monotonic_time(millisecond),
    #bucket{
        capacity = Capacity,
        tokens = Capacity,
        refill_rate = RefillRate,
        last_refill = Now
    }.

consume_token(Bucket) ->
    Now = erlang:monotonic_time(millisecond),
    TimeDelta = Now - Bucket#bucket.last_refill,

    % Refill tokens based on time
    TokensToAdd = min(Bucket#bucket.capacity,
        Bucket#bucket.tokens + trunc(TimeDelta * Bucket#bucket.refill_rate)),

    NewBucket = Bucket#bucket{
        tokens = TokensToAdd,
        last_refill = Now
    },

    % Try to consume
    case NewBucket#bucket.tokens > 0 of
        true ->
            {ok, NewBucket#bucket{tokens = NewBucket#bucket.tokens - 1}};
        false ->
            {error, rate_limited}
    end.
```

**Integration with Circuit Breaker:**

```erlang
% Modified erlmcp_circuit_breaker init
init({Name, UserConfig}) ->
    % Initialize token bucket for priority state changes
    TokenBucket = erlmcp_priority_token_bucket:new_bucket(
        100,  % 100 priority messages capacity
        0.1   % 10 priority messages per second refill rate
    ),

    % Setup periodic bucket check
    erlang:send_after(100, self(), refill_token_bucket),

    {ok, closed, Data#data{token_bucket = TokenBucket}}.

handle_info(refill_token_bucket, Data) ->
    % Check if we can send priority notification
    case erlmcp_priority_token_bucket:consume_token(Data#data.token_bucket) of
        {ok, NewBucket} ->
            % Send priority state change
            notify_state_change_priority(Data#data.name, closed),
            {noreply, Data#data{token_bucket = NewBucket}};
        {error, rate_limited} ->
            % Fallback to normal send (rate limited)
            notify_state_change_normal(Data#data.name, closed),
            {noreply, Data}
    end.
```

**Configuration:**

```erlang
% sys.config
{erlmcp_circuit_breaker, [
    {priority_rate_limit, [
        {capacity, 100},           % Max 100 priority messages burst
        {refill_rate, 10},         % 10 priority messages per second
        {enable_starvation_protection, true}
    ]}
]}.
```

---

### 4. Priority Message Queue Monitoring

#### Problem: No Visibility into Priority Queue Health

**Missing Metrics:**
- Priority message delivery latency (p50, p95, p99)
- Priority queue depth
- Priority vs normal message ratio
- Interference events per minute
- Starvation detection alerts

#### Solution: Priority Queue Monitor

**New Module: `erlmcp_priority_monitor`**

```erlang
-module(erlmcp_priority_monitor).
-behaviour(gen_server).

-record(monitor_state, {
    target_pids = [] :: [pid()],
    metrics = #{} :: #{pid() => priority_metrics()},
    alert_thresholds :: #{
        priority_latency_p99_us => pos_integer(),
        priority_queue_depth => pos_integer(),
        starvation_ratio => float()
    }
}).

-record(priority_metrics, {
    pid :: pid(),
    priority_messages_sent = 0 :: non_neg_integer(),
    priority_latency_samples = [] :: [integer()],  % Last 100 samples
    normal_messages_sent = 0 :: non_neg_integer(),
    last_priority_latency_us = 0 :: non_neg_integer(),
    starvation_detected = false :: boolean(),
    last_starvation_time :: undefined | integer()
}).

% API
start_monitor(TargetPid) ->
    gen_server:call(?MODULE, {start_monitor, TargetPid}).

get_priority_metrics(TargetPid) ->
    gen_server:call(?MODULE, {get_metrics, TargetPid}).

get_priority_report() ->
    gen_server:call(?MODULE, get_report).
```

**Metrics Collection:**

```erlang
% Track priority message latency
track_priority_latency(Pid, LatencyUs) ->
    gen_server:cast(?MODULE, {track_priority, Pid, LatencyUs}).

% Calculate percentiles
calculate_percentiles(Samples) ->
    Sorted = lists:sort(Samples),
    Len = length(Sorted),
    #{
        p50 = nth(Len div 2, Sorted),
        p95 = nth(trunc(Len * 0.95), Sorted),
        p99 = nth(trunc(Len * 0.99), Sorted),
        avg = lists:sum(Samples) div Len
    }.

% Detect starvation
detect_starvation(Pid, Metrics) ->
    % Starvation: < 1% priority ratio over last 10 seconds
    PriorityRatio = case Metrics#priority_metrics.normal_messages_sent of
        0 -> 0;
        NormalCount -> Metrics#priority_metrics.priority_messages_sent / NormalCount
    end,

    StarvationDetected = PriorityRatio < 0.01,

    case StarvationDetected of
        true ->
            ?LOG_ALERT("Starvation detected for ~p: ratio=~p",
                      [Pid, PriorityRatio]),
            erlmcp_health_monitor:report_degradation({starvation, Pid});
        false ->
            ok
    end,

    StarvationDetected.
```

**Health Dashboard Integration:**

```erlang
% Priority health endpoint
get_priority_health() ->
    {ok, Report} = get_priority_report(),

    #{
        total_monitored_processes => maps:size(Report),
        avg_priority_latency_p99_us => calculate_avg_p99(Report),
        starvation_count => count_starvations(Report),
        interference_events => count_interferences(Report),
        status => determine_priority_health(Report)
    }.

determine_priority_health(Report) ->
    AvgP99 = calculate_avg_p99(Report),
    StarvationCount = count_starvations(Report),

    case {AvgP99, StarvationCount} of
        {Lat, _} when Lat > 10000 -> degraded;  % p99 > 10ms
        {_, Count} when Count > 0 -> degraded;
        _ -> healthy
    end.
```

---

## Implementation Roadmap

### Phase 1: Circuit Breaker Priority Transitions (Week 1)

**Tasks:**
1. Create `erlmcp_priority_state_transition` module
2. Implement alias-based priority state transitions
3. Modify `erlmcp_circuit_breaker` to use priority transitions
4. Add latency tracking for state transitions
5. Update tests for priority state transitions

**Deliverables:**
- `apps/erlmcp_core/src/erlmcp_priority_state_transition.erl`
- Modified `apps/erlmcp_core/src/erlmcp_circuit_breaker.erl`
- `apps/erlmcp_core/test/erlmcp_priority_state_transition_tests.erl`

**Acceptance Criteria:**
- All state transitions use priority messages
- p50 latency < 100us under normal load
- p99 latency < 1ms under 10K message queue load
- Tests pass for OTP 28+

---

### Phase 2: Interference Detection (Week 2)

**Tasks:**
1. Create `erlmcp_priority_interference` module
2. Implement interference detection algorithm
3. Add automatic interference prevention (priority flags)
4. Integrate with health monitor for escalation
5. Add interference metrics

**Deliverables:**
- `apps/erlmcp_observability/src/erlmcp_priority_interference.erl`
- Modified `apps/erlmcp_observability/src/erlmcp_health_monitor.erl`
- `apps/erlmcp_observability/test/erlmcp_priority_interference_tests.erl`

**Acceptance Criteria:**
- Detect interference within 100ms
- Auto-prevent interference via process flags
- Escalate to health monitor on persistent interference
- Interference events < 1 per hour (production target)

---

### Phase 3: Starvation Prevention (Week 2-3)

**Tasks:**
1. Create `erlmcp_priority_token_bucket` module
2. Implement token bucket rate limiting
3. Integrate with circuit breaker state transitions
4. Add starvation detection alerts
5. Test under flood conditions (10K priority messages)

**Deliverables:**
- `apps/erlmcp_core/src/erlmcp_priority_token_bucket.erl`
- Modified `erlmcp_circuit_breaker.erl` (token bucket integration)
- `apps/erlmcp_core/test/erlmcp_priority_starvation_tests.erl`

**Acceptance Criteria:**
- Prevent priority floods from starving normal messages
- Token bucket capacity: 100 messages, 10/sec refill
- Starvation detection < 1 second
- Normal message latency < 5ms during priority flood

---

### Phase 4: Priority Queue Monitoring (Week 3)

**Tasks:**
1. Create `erlmcp_priority_monitor` module
2. Implement metrics collection (p50, p95, p99 latency)
3. Add starvation detection
4. Create priority health dashboard
5. Integrate with observability (OTEL traces)

**Deliverables:**
- `apps/erlmcp_observability/src/erlmcp_priority_monitor.erl`
- `apps/erlmcp_observability/src/erlmcp_priority_dashboard.erl`
- `apps/erlmcp_observability/test/erlmcp_priority_monitor_tests.erl`

**Acceptance Criteria:**
- Track latency percentiles for all priority messages
- Detect starvation within 1 second
- Dashboard shows priority health in real-time
- OTEL traces for priority messages

---

### Phase 5: Testing & Documentation (Week 4)

**Tasks:**
1. Complete test suite for all priority features
2. Load testing (10K msg/s sustained)
3. Chaos testing (priority floods, interference)
4. Update documentation (api.md, otp-patterns.md)
5. Create migration guide for v2 -> v3

**Deliverables:**
- Complete test coverage (≥80% for all priority modules)
- `docs/v3/priority-messages-guide.md`
- `docs/v3/migration-v2-v3.md`
- Benchmark results (priority latency, interference detection)

**Acceptance Criteria:**
- All tests passing (EUnit + CT)
- Coverage ≥80% for priority modules
- Documentation complete
- Production ready

---

## Test Specifications

### Priority State Transition Tests

**File:** `apps/erlmcp_core/test/erlmcp_priority_state_transition_tests.erl`

```erlang
-module(erlmcp_priority_state_transition_tests).

% Test cases:
% 1. Priority state transition under normal load
% 2. Priority state transition under high load (10K messages)
% 3. Priority state transition latency percentiles (p50, p95, p99)
% 4. Priority state transition timeout handling
% 5. Graceful degradation when alias unavailable
% 6. Concurrent state transitions (race conditions)
% 7. Priority state transition vs normal send latency comparison

% Acceptance:
% - p50 < 100us, p95 < 500us, p99 < 1ms
% - 0% timeout under normal load
% - <1% timeout under high load
```

### Interference Detection Tests

**File:** `apps/erlmcp_observability/test/erlmcp_priority_interference_tests.erl`

```erlang
-module(erlmcp_priority_interference_tests).

% Test cases:
% 1. Detect queue interference (queue > 100, latency > threshold)
% 2. Detect starvation interference (priority ratio < 2%)
% 3. Detect GC interference (GC pauses during priority)
% 4. Auto-prevention via process flags
% 5. Escalation to health monitor
% 6. Interference recovery (clear after resolved)
% 7. False positive prevention (avoid alerting on normal load)

% Acceptance:
% - Detection latency < 100ms
% - False positive rate < 5%
% - Auto-prevention success rate > 95%
% - Recovery time < 1s
```

### Starvation Prevention Tests

**File:** `apps/erlmcp_core/test/erlmcp_priority_starvation_tests.erl`

```erlang
-module(erlmcp_priority_starvation_tests).

% Test cases:
% 1. Token bucket rate limiting (capacity enforcement)
% 2. Token refill over time (10/sec rate)
% 3. Priority flood handling (10K priority messages)
% 4. Normal message processing during flood
% 5. Starvation detection (< 1% priority ratio)
% 6. Starvation recovery (after flood ends)
% 7. Token bucket overflow protection

% Acceptance:
% - Token bucket capacity enforced (max 100)
% - Refill rate accurate (10/sec ±10%)
% - Normal message latency < 5ms during flood
% - Starvation detected < 1s
```

### Priority Monitor Tests

**File:** `apps/erlmcp_observability/test/erlmcp_priority_monitor_tests.erl`

```erlang
-module(erlmcp_priority_monitor_tests).

% Test cases:
% 1. Track priority message latency (accurate timestamps)
% 2. Calculate percentiles (p50, p95, p99)
% 3. Detect starvation (priority ratio < 1%)
% 4. Generate priority health report
% 5. Alert on latency degradation (p99 > 10ms)
% 6. Alert on starvation events
% 7. Integration with OTEL traces

% Acceptance:
% - Latency tracking accuracy ±10us
% - Percentile calculation error < 5%
% - Starvation detection < 1s
% - Health dashboard updates in real-time
% - OTEL traces include priority metadata
```

---

## Configuration

### Application Configuration

```erlang
% sys.config
[
  {erlmcp_circuit_breaker, [
    {priority_level, high},
    {priority_rate_limit, [
      {capacity, 100},
      {refill_rate, 10},
      {enable_starvation_protection, true}
    ]},
    {priority_state_transitions, [
      {use_alias_priority, true},
      {timeout_ms, 1000},
      {fallback_to_normal, true}
    ]}
  ]},

  {erlmcp_priority_interference, [
    {detection_interval_ms, 100},
    {interference_threshold_ms, 10},
    {auto_prevention, [
      {enable_priority_flag, true},
      {enable_off_heap, true},
      {enable_fullsweep, true}
    ]},
    {escalate_to_health_monitor, true}
  ]},

  {erlmcp_priority_monitor, [
    {monitored_processes, [
      erlmcp_health_monitor,
      erlmcp_graceful_drain
    ]},
    {alert_thresholds, [
      {priority_latency_p99_us, 10000},
      {priority_queue_depth, 1000},
      {starvation_ratio, 0.01}
    ]},
    {metrics_retention_sec, 3600}
  ]}
].
```

### Runtime Configuration API

```erlang
% Update priority rate limit at runtime
erlmcp_circuit_breaker:set_priority_rate_limit(BreakerName, #{
    capacity => 200,
    refill_rate => 20
}).

% Enable/disable interference detection
erlmcp_priority_interference:set_detection_enabled(true).

% Update monitoring alert thresholds
erlmcp_priority_monitor:set_alert_thresholds(#{
    priority_latency_p99_us => 5000
}).
```

---

## Migration Guide (v2 → v3)

### Breaking Changes

1. **Circuit breaker state transitions now use priority by default**
   - Old: Normal `erlang:send/2` for state changes
   - New: Priority alias-based messages
   - Migration: Update config to opt-out if needed

2. **Priority rate limiting enabled by default**
   - Old: No rate limiting
   - New: Token bucket (100 capacity, 10/sec refill)
   - Migration: Adjust config for high-throughput scenarios

3. **Interference detection auto-enables process flags**
   - Old: Manual process flag management
   - New: Auto-detection and prevention
   - Migration: Monitor logs for interference events

### Migration Steps

1. **Update configuration**
   ```erlang
   % Add to sys.config
   {erlmcp_circuit_breaker, [
     {priority_level, high},
     {priority_rate_limit, [
       {capacity, 100},
       {refill_rate, 10}
     ]}
   ]}.
   ```

2. **Enable monitoring**
   ```erlang
   % Start priority monitor
   erlmcp_priority_monitor:start_monitor(erlmcp_health_monitor).
   erlmcp_priority_monitor:start_monitor(erlmcp_graceful_drain).
   ```

3. **Test priority features**
   ```bash
   # Run priority test suite
   rebar3 ct --suite=erlmcp_priority_messages_SUITE

   # Verify priority latency
   erlmcp_priority_monitor:get_priority_metrics(Pid).
   ```

4. **Monitor for interference**
   ```erlang
   % Check interference stats
   erlmcp_priority_interference:get_interference_stats(Pid).
   ```

5. **Adjust thresholds based on load**
   ```erlang
   % Tune rate limits for high-throughput
   erlmcp_circuit_breaker:set_priority_rate_limit(my_breaker, #{
       capacity => 500,
       refill_rate => 50
   }).
   ```

---

## Performance Targets

### Latency Targets

| Metric | Target | Notes |
|--------|--------|-------|
| Priority state transition p50 | < 100us | Under normal load |
| Priority state transition p95 | < 500us | Under normal load |
| Priority state transition p99 | < 1ms | Under normal load |
| Priority state transition p99 | < 10ms | Under 10K message queue |
| Interference detection | < 100ms | From interference start |
| Starvation detection | < 1s | From starvation start |
| Normal message latency (during flood) | < 5ms | With token bucket |

### Throughput Targets

| Scenario | Target | Notes |
|----------|--------|-------|
| Priority messages (normal) | 10/sec | Token bucket refill rate |
| Priority messages (burst) | 100 | Token bucket capacity |
| Circuit breaker state transitions | 1000/sec | With priority |
| Interference detection overhead | < 1% CPU | Monitoring cost |

---

## Open Questions

1. **gen_statem priority integration:** Should we use OTP 28's `gen_statem:call` with priority flag?
   - **Decision:** Use alias-based priority for now (more portable)
   - **Future:** Investigate native gen_statem priority support

2. **Token bucket refill granularity:** Should we use timer-based or event-based refill?
   - **Decision:** Event-based (check on every send)
   - **Future:** Optimize with timer-based if overhead > 5%

3. **Interference false positives:** How to minimize alerts during normal load spikes?
   - **Decision:** Adaptive thresholds (baseline + 3σ)
   - **Future:** Machine learning-based anomaly detection

4. **Cross-node priority messages:** How to handle priority in distributed systems?
   - **Decision:** Local priority only (v3)
   - **Future:** Distributed priority with Erlang distribution

---

## References

- [OTP 28 Priority Messages](https://www.erlang.org/doc/system/principles/instructions#priority-messages)
- `erlmcp_circuit_breaker.erl` - Current circuit breaker implementation
- `erlmcp_health_monitor.erl` - Health monitor with priority support
- `erlmcp_graceful_drain.erl` - Graceful drain with priority shutdown
- `apps/erlmcp_core/test/erlmcp_priority_messages_SUITE.erl` - Priority test suite

---

**Document Status:** Draft - Ready for review
**Next Steps:**
1. Review and approve design
2. Assign developers to phases
3. Create implementation tickets
4. Begin Phase 1 development
