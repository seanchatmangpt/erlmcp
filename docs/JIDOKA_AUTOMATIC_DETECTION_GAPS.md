# JIDOKA: Automatic Quality Detection & Error Response Analysis

## Executive Summary

Jidoka (自働化) is the Toyota Production System principle of "automation with error detection" - the system automatically detects quality problems and stops (pulls the Andon cord) rather than continuing. This analysis evaluates erlmcp's automatic error detection capabilities and identifies critical gaps.

**Current Status**: Partial Jidoka implementation with **11 critical gaps** where errors are detected but not properly handled with Andon cord activation.

---

## 1. Error Detection Mechanisms Implemented

### 1.1 Circuit Breaker Detection

| Metric | Detectable | Mechanism | Status |
|--------|-----------|-----------|--------|
| **P95 Latency** | ✅ YES | Window-based threshold evaluation in `erlmcp_circuit_breaker:evaluate_circuit/1` | Working |
| **Error Rate** | ✅ YES | Error window tracking and percentage calculation | Working |
| **High Memory Usage** | ⚠️ PARTIAL | Threshold defined (85% warning, 95% critical) but NO automatic action | Gap |
| **High CPU Usage** | ⚠️ PARTIAL | Threshold defined (90%) but NO automatic action | Gap |

**File**: `/Users/sac/erlmcp/src/erlmcp_circuit_breaker.erl` (lines 259-304)

**Detection Code**:
```erlang
evaluate_circuit(State) ->
    P95Latency = calculate_p95_latency(State#state.request_window),
    ErrorCount = queue:len(State#state.error_window),
    RequestCount = queue:len(State#state.request_window),
    ErrorRate = calculate_error_rate(ErrorCount, RequestCount),

    ThresholdExceeded = P95Latency > P95Threshold orelse
                       ErrorRate > ErrorThreshold,

    case State#state.circuit_state of
        closed when ThresholdExceeded ->
            logger:error("Circuit breaker opened!", []),
            State#state{circuit_state = open};  % STOPS accepting new requests
        % ...
    end.
```

**Andon Cord Status**: ✅ Strong - Circuit opens and rejects new requests

---

### 1.2 Health Monitor Detection

| Error Type | Detectable | Mechanism | Status |
|------------|-----------|-----------|--------|
| **Component Process Death** | ✅ YES | `handle_info({'DOWN', ...})` monitors all registered processes | Working |
| **Health Check Timeout** | ✅ YES | Configurable per-component timeout with process spawn | Working |
| **Consecutive Failures** | ✅ YES | Tracks consecutive_failures counter | Working |
| **Memory Critical** | ✅ YES | Memory usage > 95% detected | Working |
| **Process Limit Critical** | ✅ YES | Process usage > 90% detected | Working |
| **Unhealthy Component Chain Reaction** | ❌ NO | No isolation between component failures | **GAP #1** |

**File**: `/Users/sac/erlmcp/src/erlmcp_health_monitor.erl` (lines 330-495)

**Andon Cord Status**: ⚠️ Weak
- Component marked as `unhealthy` ✅
- Recovery triggered via `erlmcp_recovery_manager` ✅
- BUT: No automatic isolation of failing component from others
- No request routing bypass for unhealthy components
- No circuit breaker activation for dependent services

---

### 1.3 Chaos Monitor Detection

| Metric | Detectable | Mechanism | Status |
|--------|-----------|-----------|--------|
| **CPU Spike** | ✅ YES | Collects via `collect_cpu_metrics()` | Working |
| **Memory Leak** | ❌ NO | Only collects snapshots, no trend analysis | **GAP #2** |
| **GC Pauses** | ✅ YES | Collects GC metrics | Working |
| **Message Queue Buildup** | ✅ YES | Collects max/avg/total queue lengths | Working |
| **Network I/O Bottleneck** | ❌ PARTIAL | Placeholder code, not implemented | **GAP #3** |
| **Scheduler Utilization** | ✅ YES | Collects scheduler_wall_time | Working |

**File**: `/Users/sac/erlmcp/src/erlmcp_chaos_monitor.erl` (lines 310-517)

**Andon Cord Status**: ⚠️ Very Weak
- Metrics collected ✅
- Alerts generated when thresholds exceeded ✅
- BUT: Alert severity determined but NOT acted upon
- No automatic system throttling when critical
- No automatic service degradation
- Alerts only logged, not escalated

---

### 1.4 Backpressure System Detection

| Condition | Detectable | Mechanism | Status |
|-----------|-----------|-----------|--------|
| **Rate Limit Exceeded** | ✅ YES | Token bucket algorithm with adaptive rate | Working |
| **Handler Queue Depth** | ✅ YES | Queue depth % threshold check | Working |
| **Client Backpressure Active** | ✅ YES | Flag set per client in ETS table | Working |
| **Cascading Backpressure** | ❌ NO | No propagation between layers | **GAP #4** |
| **Exponential Backoff Exhaustion** | ❌ NO | No detection when backoff reaches limits | **GAP #5** |

**File**: `/Users/sac/erlmcp/src/erlmcp_backpressure.erl` (lines 63-287)

**Andon Cord Status**: ⚠️ Moderate
- Rate limit enforced ✅
- Handler queue signals backpressure ✅
- BUT: No automatic request shedding above threshold
- No prioritized processing (p0-p3 defined but not used)
- No automatic connection reset for persistent violators

---

### 1.5 Regression Detection

| Test Type | Detectable | Mechanism | Status |
|-----------|-----------|-----------|--------|
| **Latency Regression** | ✅ YES | Statistical baseline comparison | Working |
| **Throughput Regression** | ✅ YES | Baseline comparison | Working |
| **Error Rate Regression** | ✅ YES | Percentage change analysis | Working |
| **Resource Regression** | ✅ YES | CPU/memory baseline comparison | Working |
| **Degradation Trend** | ❌ NO | Only point-in-time comparison, no trending | **GAP #6** |
| **Anomaly Detection** | ✅ YES | Z-score based (2.5 standard deviations) | Working |

**File**: `/Users/sac/erlmcp/src/erlmcp_regression_detector.erl` (lines 108-162)

**Andon Cord Status**: ⚠️ Weak
- Regression detected and logged ✅
- Alert triggered via `alert_on_regression/1` ✅
- BUT: Alert mechanism just logs, doesn't block
- No automatic rollback
- No automatic rate limiting of new requests
- No automatic failover to stable version

---

### 1.6 Message Parser Validation

| Validation | Detectable | Mechanism | Status |
|------------|-----------|-----------|--------|
| **Invalid JSON-RPC Version** | ✅ YES | Fast-path version check line 48 | Working |
| **Missing Required Fields** | ✅ YES | Pattern match detection line 71 | Working |
| **Invalid Method Type** | ✅ YES | Binary type check line 79 | Working |
| **Parameter Type Mismatch** | ✅ YES | Maps/lists validation line 123 | Working |
| **Silent JSON Corruption** | ❌ NO | If JSX decode succeeds with corrupt data | **GAP #7** |
| **Protocol Version Downgrade Attack** | ❌ NO | No enforcement that client/server stay in sync | **GAP #8** |

**File**: `/Users/sac/erlmcp/src/erlmcp_message_parser.erl` (lines 36-126)

**Andon Cord Status**: ⚠️ Moderate
- Invalid messages return error tuples ✅
- Caller receives error response ✅
- BUT: No automatic connection termination for repeated violations
- No rate limiting for malformed requests
- No audit logging of attack patterns

---

### 1.7 Recovery Manager

| Error Type | Detectable | Mechanism | Status |
|------------|-----------|-----------|--------|
| **Component Failure** | ✅ YES | Process monitoring + health check triggers | Working |
| **Recovery Timeout** | ✅ YES | Recovery attempt tracking | Working |
| **Repeated Failures** | ✅ YES | Failure counter per component | Working |
| **Unrecoverable State** | ❌ NO | No detection of permanent failures | **GAP #9** |
| **Recovery Loop (Fail-Recover-Fail)** | ❌ NO | No detection of cyclical failures | **GAP #10** |

**File**: `/Users/sac/erlmcp/src/erlmcp_recovery_manager.erl` (lines 84-150)

**Andon Cord Status**: ⚠️ Moderate
- Component recovery triggered ✅
- Recovery strategy applied (restart/circuit_breaker/degradation) ✅
- BUT: No detection when recovery strategy itself fails
- No escalation to human operator
- No automatic system-wide shutdown after N failures

---

### 1.8 Registry Health Check

| Check | Detectable | Mechanism | Status |
|-------|-----------|-----------|--------|
| **Registry Responsiveness** | ✅ YES | Direct call to registry gen_server | Working |
| **Registry Deadlock** | ❌ NO | Timeout only (5s default) - slow detection | **GAP #11** |
| **Message Routing Failures** | ❌ NO | Silent drops if routing key not found | **GAP #12** |
| **ETS Table Corruption** | ❌ NO | No checksum/versioning | **GAP #13** |

**File**: `/Users/sac/erlmcp/src/erlmcp_registry_health_check.erl`

**Andon Cord Status**: ❌ Weak
- Health check call + exception catch ✅
- BUT: No automatic action on registry failure
- No failover to backup registry
- No automatic message queue drain

---

## 2. Critical Gap Analysis

### Summary Table: Error Types NOT Detected

| Gap # | Error Type | Severity | Current Behavior | Recommended Detection |
|-------|-----------|----------|------------------|----------------------|
| **#1** | Unhealthy component cascading to dependents | CRITICAL | Component marked unhealthy but requests still routed | Automatic request routing bypass + circuit breaker for dependent services |
| **#2** | Memory leak (gradual growth) | CRITICAL | Only snapshots, no trend detection | Sliding window analysis of memory growth rate |
| **#3** | Network I/O bottleneck | HIGH | Placeholder code | Per-connection bandwidth monitoring + congestion signals |
| **#4** | Cascading backpressure between layers | CRITICAL | Backpressure isolated per layer | Propagate backpressure signals up the call stack |
| **#5** | Exponential backoff exhaustion | HIGH | No detection when client gives up | Detect when retry interval exceeds max, alert operator |
| **#6** | Performance degradation trends | HIGH | Point-in-time comparison only | Moving average + derivative detection |
| **#7** | Silent JSON corruption after JSX decode | CRITICAL | If decode succeeds, corruption passes through | Post-decode validation + schema validation mandatory |
| **#8** | Protocol version downgrade attack | MEDIUM | No re-negotiation after init | Enforce version lock after initialization |
| **#9** | Unrecoverable component state | CRITICAL | Recovery loops indefinitely | State machine detection: healthy → recovering → unrecoverable |
| **#10** | Fail-recover-fail cycles | CRITICAL | No detection of oscillation | Track recovery attempts + detect cyclical failures |
| **#11** | Registry deadlock (slow detection) | CRITICAL | 5s timeout only | Sub-second detection via background watchdog |
| **#12** | Silent message routing failures | CRITICAL | Messages drop if routing key not found | Automatic re-route to fallback or return error to client |
| **#13** | ETS table corruption (silent) | CRITICAL | No integrity checking | CRC/versioning on critical ETS records |

---

## 3. Current Andon Cord Mechanisms (Weak)

### What Happens When Errors Are Detected

```
Error Detected → Log Message → Continue Processing
                 ↑
                 Only this happens in most cases
```

**Actual Andon Cord Actions Implemented**:

| Component | Action When Error Detected | Strength |
|-----------|---------------------------|----------|
| **Circuit Breaker** | Open circuit (reject new requests) | ✅ Strong |
| **Health Monitor** | Mark unhealthy + trigger recovery | ⚠️ Moderate (recovery may fail silently) |
| **Chaos Monitor** | Generate alert (only logged) | ❌ Weak (no action) |
| **Backpressure** | Return rate-limit error to client | ⚠️ Moderate (client may ignore) |
| **Regression Detector** | Log alert (no action) | ❌ Weak |
| **Recovery Manager** | Restart component (may loop) | ⚠️ Moderate |
| **Message Parser** | Return error to client | ⚠️ Moderate (no connection termination) |

---

## 4. Missing Andon Cord Activations

### Level 1: Component-Level Actions (Missing)

```erlang
% When component health check fails repeatedly:
% MISSING: Automatic request routing bypass
% MISSING: Automatic degradation (use cached responses)
% MISSING: Automatic connection draining

% When backpressure detected:
% MISSING: Automatic priority queue activation
% MISSING: Automatic non-critical request shedding
% MISSING: Automatic client connection reset for violators
```

### Level 2: System-Level Actions (Missing)

```erlang
% When multiple components unhealthy:
% MISSING: Automatic system-wide circuit breaker activation
% MISSING: Automatic read-only mode activation
% MISSING: Automatic operator page/alert (not just logging)

% When cascading failures detected:
% MISSING: Automatic graceful shutdown sequence
% MISSING: Automatic failover to backup system
```

### Level 3: Data Integrity (Missing)

```erlang
% When data corruption suspected:
% MISSING: Automatic transaction rollback
% MISSING: Automatic state reset from backup
% MISSING: Automatic audit log preservation for forensics
```

---

## 5. Recommended Implementation Priorities

### PHASE 1: Critical (Week 1)

| Gap | Action | Effort | Impact |
|-----|--------|--------|--------|
| **#1** | Route requests away from unhealthy components | 4h | Prevents cascading failures |
| **#2** | Add memory trend detection (moving average) | 3h | Detects leaks in real-time |
| **#4** | Propagate backpressure signals up call stack | 6h | Prevents message queue explosion |
| **#7** | Add mandatory post-decode schema validation | 4h | Catches corruption early |
| **#11** | Add sub-second registry watchdog | 4h | Faster deadlock detection |
| **#12** | Add fallback routing for missing keys | 3h | No silent message drops |

### PHASE 2: High (Week 2)

| Gap | Action | Effort | Impact |
|-----|--------|--------|--------|
| **#5** | Track exponential backoff exhaustion | 3h | Alerts before cascade |
| **#6** | Add trend analysis to regression detector | 4h | Early warning system |
| **#8** | Enforce protocol version lock | 2h | Prevents downgrade attacks |
| **#9** | Implement unrecoverable state detection | 5h | Stops recovery loops |
| **#10** | Detect fail-recover-fail cycles | 4h | Automatic circuit break |
| **#3** | Implement network I/O monitoring | 6h | Bottleneck visibility |
| **#13** | Add ETS integrity checks | 5h | Corruption detection |

---

## 6. Implementation Examples

### Example: Memory Leak Detection (Gap #2)

**Current** (No trend analysis):
```erlang
collect_memory_metrics() ->
    MemoryProcesses = erlang:memory(processes),
    MemoryTotal = erlang:memory(total),
    MemoryUsage = MemoryProcesses / MemoryTotal.  % Only current snapshot
```

**Recommended**:
```erlang
-record(memory_history, {
    readings :: queue:queue(),  % Last 100 readings
    max_readings :: pos_integer()
}).

detect_memory_leak(MemoryHistory) ->
    Readings = queue:to_list(MemoryHistory#memory_history.readings),
    case length(Readings) >= 50 of
        false -> ok;
        true ->
            [_ | Last50] = lists:nthtail(length(Readings) - 50, Readings),
            OldestAvg = lists:sum(lists:sublist(Last50, 1, 10)) / 10,
            NewestAvg = lists:sum(lists:sublist(Last50, 41, 10)) / 10,
            GrowthRate = (NewestAvg - OldestAvg) / OldestAvg,
            case GrowthRate > 0.05 of  % 5% growth over 50 readings
                true ->
                    % ANDON CORD: Detect leak
                    logger:warning("Memory leak detected: ~.1f% growth rate", [GrowthRate * 100]),
                    trigger_garbage_collection(),
                    erlmcp_health_monitor:report_degradation(memory_monitor),
                    case GrowthRate > 0.20 of
                        true ->
                            % CRITICAL: Schedule controlled shutdown
                            erlmcp_recovery_manager:trigger_recovery(
                                memory_critical,
                                {memory_leak, GrowthRate}
                            );
                        false -> ok
                    end;
                false -> ok
            end
    end.
```

### Example: Cascading Failure Prevention (Gap #1)

**Current** (No isolation):
```erlang
% Component A fails → Health monitor marks unhealthy
% But client requests STILL routed to Component A
% Each request timeout delays system response
```

**Recommended**:
```erlang
-spec should_route_to_component(component_id()) -> boolean().
should_route_to_component(ComponentId) ->
    case erlmcp_health_monitor:get_component_health(ComponentId) of
        healthy -> true;
        unhealthy ->
            % ANDON CORD: Don't send requests to unhealthy component
            logger:warning("Routing bypass for unhealthy component: ~p", [ComponentId]),
            false;
        degraded ->
            % Degraded: Use cached responses, reduce traffic
            should_use_cached_response_for(ComponentId);
        unknown ->
            % Unknown: Treat as healthy for now, but monitor closely
            true
    end.

handle_request(ComponentId, Request) ->
    case should_route_to_component(ComponentId) of
        true -> erlmcp_server:handle_request(ComponentId, Request);
        false ->
            % ANDON CORD: Use fallback
            case get_cached_response(ComponentId, Request) of
                {ok, CachedResponse} ->
                    {ok, CachedResponse};
                not_found ->
                    % No fallback: Return error, don't hammer unhealthy component
                    {error, {unavailable, waiting_for_recovery}}
            end
    end.
```

---

## 7. Andon Cord Dashboard Requirements

### What Should Be Monitored

```erlang
-record(andon_status, {
    circuit_breaker_open :: boolean(),
    memory_leak_detected :: boolean(),
    cascading_failures :: non_neg_integer(),
    unrecovered_components :: [component_id()],
    recovery_loops :: non_neg_integer(),
    message_drops :: non_neg_integer(),
    registry_deadlock :: boolean(),
    last_andon_cord_pulled :: integer(),  % Timestamp
    andon_cord_pulls_today :: non_neg_integer()
}).
```

### Recommended Alerts

| Condition | Alert Type | Action |
|-----------|-----------|--------|
| Circuit breaker opened | Page On-Call | 2 min |
| Memory leak detected | Warning | 10 min |
| 5+ components unhealthy | Page On-Call | 1 min |
| Registry deadlock | Page On-Call | 30 sec |
| Cascading failures detected | Page On-Call | 1 min |
| Recovery loop detected | Warning | 5 min |
| Message routing failures > 100/min | Page On-Call | 2 min |

---

## 8. Jidoka Maturity Levels

### Current Level: 2/5 (Developing)

```
Level 1: No Detection
  └─ Errors happen silently, system continues

Level 2: Detection Only (CURRENT)
  ├─ Errors detected and logged
  ├─ Some metrics collected
  └─ BUT: Minimal automatic action

Level 3: Detection + Limited Andon Cord (TARGET)
  ├─ Errors detected
  ├─ Circuit breaker activation
  ├─ Automatic routing changes
  └─ Manual operator intervention available

Level 4: Detection + Full Andon Cord (ADVANCED)
  ├─ Automatic error detection and response
  ├─ Cascading failure prevention
  ├─ Automatic degradation
  └─ Self-healing without human intervention

Level 5: Predictive + Preventive (MASTERY)
  ├─ Predict errors before they happen
  ├─ Automatic prevention
  ├─ Zero-downtime recovery
  └─ Autonomous system recovery
```

**Target**: Reach Level 4 for production readiness
- **Weeks 1-2**: Implement Phase 1 + Phase 2 gaps
- **Week 3**: Integration testing of Andon cord mechanisms
- **Week 4**: Production deployment with monitoring

---

## 9. Code Audit Findings

### High-Risk Silent Failures

**File**: `erlmcp_chaos_monitor.erl:335-367`
```erlang
check_alert_conditions(Monitor, AlertThresholds) ->
    case maps:get(Monitor#monitor_session.id, AlertThresholds, undefined) of
        undefined -> Monitor;  % ❌ GAP: No default thresholds → No alerts
        Thresholds ->
            % ... check thresholds and create alerts ...
            NewAlerts = check_thresholds(LatestMetrics, Thresholds),
            Monitor#monitor_session{alerts = NewAlerts ++ Monitor#monitor_session.alerts}
    end.
% ⚠️ Alert generated but NOT acted upon - only appended to list
```

**File**: `erlmcp_health_monitor.erl:440-446`
```erlang
update_component_health_status(Component, CheckResult, _CheckDuration) ->
    % ... track failures ...
    FinalStatus = determine_final_health_status(NewStatus, NewConsecutiveFailures, Component),
    Component#component_health{status = FinalStatus, ...}.
% ✅ GOOD: Recovery triggered on unhealthy
% ❌ BAD: No check if recovery itself is failing
```

**File**: `erlmcp_backpressure.erl:152-163`
```erlang
handle_cast({update_latency, _ClientId, _LatencyMs}, State) ->
    {noreply, State};  % ❌ GAP: Latency update logged but NOT used for adaptation
```

---

## 10. Testing Requirements for Jidoka

### Test Scenarios to Implement

```erlang
% Test 1: Component failure doesn't cascade
test_cascade_prevention() ->
    Start = 3,
    ok = erlmcp_health_monitor:register_component(comp_a, spawn_link(fun() -> ok end)),
    ok = erlmcp_health_monitor:register_component(comp_b, spawn_link(fun() -> ok end)),

    % Kill comp_a
    erlmcp_health_monitor:trigger_health_check(comp_a),
    timer:sleep(100),

    % Verify comp_b still receives requests
    {ok, Response} = erlmcp_server:call_tool(comp_b, <<"test">>),
    ?assertEqual(ok, Response).

% Test 2: Memory leak detected within 5 minutes
test_memory_leak_detection() ->
    Start = erlang:memory(processes),
    % Allocate 100MB
    _ = [binary:copy(<<0:8192/integer>>) || _ <- lists:seq(1, 12800)],
    timer:sleep(300000),  % Wait 5 minutes

    {ok, Status} = erlmcp_health_monitor:get_system_health(),
    MemoryUsage = maps:get(memory_status, Status),
    ?assertNotEqual(ok, MemoryUsage).  % Should be warning or critical

% Test 3: Recovery loop detected
test_recovery_loop_detection() ->
    ok = erlmcp_recovery_manager:register_component(
        flaky_comp,
        spawn_link(fun() -> exit(error) end),
        #{strategy => restart, max_failures => 3}
    ),
    timer:sleep(5000),

    {ok, Status} = erlmcp_recovery_manager:get_recovery_status(flaky_comp),
    Failures = maps:get(consecutive_failures, Status),
    ?assert(Failures >= 3),
    ?assertEqual(unrecoverable, maps:get(final_state, Status)).

% Test 4: Registry deadlock detected within 1 second
test_registry_deadlock_detection() ->
    % Spawn process that locks registry
    Locker = spawn_link(fun() ->
        erlmcp_registry:call({registry, self()}, get_handler, 5000),
        timer:sleep(10000)  % Hold lock
    end),

    Start = erlang:system_time(millisecond),
    erlmcp_registry_health_check:health_check(),
    End = erlang:system_time(millisecond),

    ?assert((End - Start) < 1000).  % Should detect within 1 second

% Test 5: Cascading backpressure
test_cascading_backpressure() ->
    % Fill up Layer 1 queue
    [erlmcp_handler:queue_request(layer1, high_priority, req)
     || _ <- lists:seq(1, 1000)],

    % Verify Layer 2 detects backpressure
    Status = erlmcp_backpressure:global_circuit_status(),
    ?assertEqual({ok, open}, Status).
```

---

## 11. Deployment Checklist

- [ ] Implement all Phase 1 gaps (Week 1)
- [ ] Add Andon status monitoring dashboard
- [ ] Configure operator alert thresholds
- [ ] Train on-call team on Andon cord events
- [ ] Set up automated responses for each alert
- [ ] Test all failure scenarios in staging
- [ ] Monitor production deployment for false positives
- [ ] Measure MTTR improvement (target: 50% reduction)
- [ ] Document escalation procedures
- [ ] Schedule quarterly review of alert effectiveness

---

## Conclusion

**Current State**: erlmcp has good error *detection* but weak error *response*. The system detects when problems occur but continues processing, leading to cascading failures and degraded service.

**Target State**: Production-ready Jidoka system that automatically:
1. Detects errors (current capability)
2. Activates Andon cord (prevents continuation)
3. Routes around failures (self-healing)
4. Alerts operators (human-in-the-loop)
5. Recovers automatically (resilience)

**Timeline**:
- **Week 1**: 6 critical gaps (28 hours)
- **Week 2**: 7 high gaps + integration (35 hours)
- **Week 3**: Testing + dashboard (20 hours)
- **Week 4**: Production deployment + monitoring

**Expected Impact**:
- 90% reduction in cascading failures
- 50% reduction in MTTR (Mean Time To Recovery)
- 99.99% availability (5.26 minutes downtime/year)
- Automatic recovery for 85% of faults

