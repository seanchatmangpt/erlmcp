# Andon Cord (Stop-the-Line) System Analysis: Automatic Shutdown Gaps

**Date:** January 27, 2026
**System:** erlmcp (Erlang/OTP Model Context Protocol)
**Status:** Production Quality Assessment
**Severity:** CRITICAL - Core reliability feature

---

## Executive Summary

The erlmcp codebase implements **sophisticated Andon cord mechanisms** equivalent to Toyota Production System's stop-the-line philosophy. However, there are **critical gaps in the automatic shutdown pipeline** that prevent true "pull the cord and stop" semantics.

**Current State:** 7/10 - Good detection, weak enforcement
**Production Readiness:** Conditional - requires gap closure
**Risk Level:** HIGH - Cascading failures possible

---

## 1. Current Andon Cord Mechanisms (What Works)

### 1.1 Health Monitoring System
**File:** `src/erlmcp_health_monitor.erl`

```erlang
% Detects unhealthy components continuously
Health Status Detection:
  - Component-level checks (individual service health)
  - System-level checks (memory, process limits, overall health)
  - Degradation detection (degraded state != unhealthy)
  - Circuit breaker reporting (component failures)

Automatic Triggering:
  - 30-second check interval (configurable)
  - 3 consecutive failures trigger unhealthy state
  - 10-second recovery check interval for unhealthy components
```

**Strengths:**
- Monitors 85% memory usage (warning), 95% (critical)
- Monitors process limit usage (0.8 warning, 0.9 critical)
- Automatic detection of cascading failures (>3 unhealthy components = system unhealthy)
- Status tracking: `healthy`, `degraded`, `unhealthy`, `unknown`

**Gap:** Detection only - no automatic action to stop new work.

### 1.2 Recovery Manager
**File:** `src/erlmcp_recovery_manager.erl`

```erlang
% Automatic recovery orchestration when component fails
Recovery Actions:
  - restart    → Attempt component restart with backoff
  - circuit_breaker → Open circuit to prevent cascading failures
  - graceful_degradation → Reduce service scope

Failure Thresholds:
  - max_failures: 5 (default)
  - recovery_timeout: 30 seconds (default)
  - backoff_strategy: exponential (1s → 2s → 4s → 8s → 30s max)
```

**Strengths:**
- Exponential backoff prevents thundering herd
- Circuit breaker transitions: `closed` → `open` → `half_open` → `closed`
- Failure tracking with recovery metrics
- Automatic process monitoring

**Gap:** Recovery is optimistic - assumes restart will fix the problem.

### 1.3 Circuit Breaker
**File:** `src/erlmcp_circuit_breaker.erl`

```erlang
% Global system overload protection
Triggers:
  - P95 latency > 200ms (default)
  - Error rate > 1% (default)
  - CPU > 90%
  - Memory > 85%

States:
  - closed   → Normal operation, all requests allowed
  - open     → System overloaded, reject new requests
  - half_open → Testing recovery, allow limited requests
```

**Strengths:**
- Comprehensive metrics collection
- Prevents cascading failures by rejecting requests
- Recovery timeout (60 seconds default)
- Metrics-driven decision making

**Gap:** Circuit breaker is reactive - only opens AFTER thresholds exceeded. No graceful degradation strategy.

### 1.4 Rate Limiting & Backpressure
**File:** `src/erlmcp_rate_limiter.erl` & `src/erlmcp_backpressure.erl`

```erlang
% Prevent overload before it starts
Rate Limiting:
  - Per-client message rate: 100 msg/sec
  - Per-client connection rate: 10 conn/sec
  - Global rate: 10,000 msg/sec
  - Tool call rate: 50 calls/sec
  - Subscription rate: 20 sub/sec
  - DDoS detection: 100 violations/minute → 5-min block

Token Bucket Algorithm:
  - Burst tolerance (2x capacity)
  - Adaptive rate reduction
  - Violation tracking

Backpressure:
  - Handler queue depth monitoring (80% threshold)
  - Message shedding for overloaded handlers
  - Latency-based adaptive rate reduction
```

**Strengths:**
- Prevents problem before cascading
- Per-client isolation (one bad actor doesn't affect all)
- DDoS detection with automatic blocking
- Graceful degradation via message shedding

**Gap:** Backpressure cascades up - no central "stop accepting" mechanism.

### 1.5 Supervision Tree
**File:** `src/erlmcp_sup.erl`

```erlang
% OTP-style failure recovery
Supervision Strategy: one_for_all
  - If health monitor crashes → restart health monitor, recovery manager, servers, transports
  - Intensity: 5 restarts within 60 seconds
  - Prevents infinite crash loop

Critical Components:
  - erlmcp_health_monitor    (permanent restart)
  - erlmcp_recovery_manager  (permanent restart)
  - erlmcp_session_manager   (permanent restart)
  - erlmcp_server_sup        (permanent restart)
  - erlmcp_transport_sup     (permanent restart)
```

**Strengths:**
- Let-it-crash philosophy with supervisor recovery
- Intensity limits prevent infinite loops
- Process monitoring and automatic restart

**Gap:** Restart doesn't investigate root cause. Same failure may recur immediately.

### 1.6 TCPS Andon System
**File:** `src/tcps/tcps_andon.erl`

```erlang
% Production quality Andon system (Toyota Production System)
Event Triggering:
  - Failure types: shacl_violation, test_failure, non_determinism, missing_receipt, compilation_failure
  - Stops-the-line: blocks further processing of same SKU until resolved
  - Receipt generation: JSON receipts with ontology links
  - Resolution workflow: root cause → fix → prevention

Stop-the-Line Enforcement:
  - is_blocked(SkuId) → returns true if any open Andon events exist
  - can_proceed_to_stage(SkuId, Stage) → checks if blocked before proceeding
  - Resolved events must document root cause + prevention
```

**Strengths:**
- Implements true "pull the cord" semantics at SKU level
- Receipt-based audit trail with JSON persistence
- Resolution workflow enforces root cause analysis
- Dashboard integration via SSE

**Gap:** Limited to quality control workflow - not integrated with system shutdown mechanisms.

---

## 2. Gap Analysis: What's Missing

### Gap #1: No Persistent "DO NOT RESTART" Signal
**Severity:** CRITICAL
**Current Behavior:**
```erlang
% Recovery manager restarts failed component
execute_recovery_action({restart, Options}, Component, Reason) ->
    % Attempts restart after backoff delay
    % Restarts forever until max_failures exceeded
    attempt_component_restart(Component, Reason, Options)

% Circuit breaker opens, but recovery still attempted
case State#state.circuit_state of
    open when not ThresholdExceeded ->
        % Automatically transitions to half_open after 60 seconds
        % NO persistent "operator manual intervention required" state
        State#state{circuit_state = half_open}
end.
```

**Problem:**
- Circuit breaker **automatically recovers** after 60 seconds
- No mechanism to say "STOP - manual intervention required"
- Prevents infinite restart loops (good), but doesn't preserve shutdown signal across restarts
- After VM restart, circuit breaker resets to `closed` state

**Risk:**
- Same failure recurs → circuit reopens → automatic recovery → infinite cycle
- No permanent halt capability
- Operators cannot force a "do not auto-recover" state

**Solution Needed:**
```erlang
% Add persistent shutdown state
-type circuit_state() :: closed | open | half_open | HALTED.

% HALTED state = Do not restart without manual operator intervention
% Persisted in ETS with restart-resilient storage
```

---

### Gap #2: No Graceful Shutdown with Connection Draining
**Severity:** CRITICAL
**Current Behavior:**
```erlang
% When circuit opens
case Component#component.circuit_state of
    open ->
        % Logs warning and blocks new requests
        % BUT existing connections continue processing
        % NO drain period or connection tracking
end.

% Supervisor termination
supervisor:terminate_child(erlmcp_server_sup, ServerPid)
    % Immediate termination, no graceful shutdown
    % Clients receive connection reset
```

**Problem:**
- Circuit breaker opens = new requests rejected
- Existing clients continue using failed service
- No graceful connection drain (wait for in-flight requests to complete)
- No "stop accepting but finish current work" phase

**Risk:**
- Clients see mid-request failures
- Data loss in partially-processed requests
- Inconsistent state if component crashes mid-operation
- Operators cannot control shutdown sequence

**Solution Needed:**
```erlang
% Multi-phase shutdown:
% 1. DRAINING - Accept no new connections, finish existing
% 2. HALTING - Kill active connections, save state
% 3. STOPPED - Cleanly shut down

-type shutdown_phase() :: normal | draining | halting | stopped.
```

---

### Gap #3: No State Persistence Before Shutdown
**Severity:** CRITICAL
**Current Behavior:**
```erlang
% Component crashes or gets terminated
terminate(Reason, State) ->
    ?LOG_INFO("Component terminating: ~p", [Reason]),
    % Logs the crash
    % BUT does not persist state to disk
    % NO checkpoint mechanism
    % NO recovery state to restore from
    ok.
```

**Problem:**
- Component crashes → all in-memory state lost
- Recovery mechanism cannot determine what was in-flight
- Clients don't know if their request succeeded or failed
- No persistent transaction log

**Risk:**
- Data loss on unplanned shutdown
- Duplicate processing if client retries
- No way to replay failed operations
- Inconsistent distributed state

**Solution Needed:**
```erlang
% Add persistent state management
terminate(Reason, State) ->
    % 1. Flush critical state to persistent storage
    erlmcp_persistence:save_component_state(ComponentId, State),
    % 2. Save in-flight transactions
    erlmcp_persistence:save_pending_requests(State#state.pending),
    % 3. Close connections cleanly
    close_connections(State),
    ok.
```

---

### Gap #4: No Operator Notification on Critical Errors
**Severity:** HIGH
**Current Behavior:**
```erlang
% Health monitor detects critical condition
case MemoryStatus of
    critical ->
        % Logs warning to error_logger
        ?LOG_WARNING("Memory critical: ~p", [MemoryUsage]);
        % BUT no integration with:
        % - Email alerts
        % - PagerDuty
        % - SMS alerts
        % - Slack webhooks
end.
```

**Problem:**
- System silently degrades without external notification
- Operators may not notice until severe
- No guaranteed communication channel
- No escalation for critical errors

**Risk:**
- Silent failures without human awareness
- 30+ minute response time to detect problems
- SLA violations
- Cascading failures while operators unaware

**Solution Needed:**
```erlang
% Add multi-channel alerting
report_critical_error(Error) ->
    % 1. Log to system
    ?LOG_ERROR("CRITICAL: ~p", [Error]),
    % 2. Send email
    erlmcp_alert_manager:send_email(alert_recipients(), Error),
    % 3. Page on-call engineer
    erlmcp_alert_manager:page_oncall(Error),
    % 4. Send Slack message
    erlmcp_alert_manager:notify_slack(Error),
    % 5. Increment metrics
    telemetry:increment(erlmcp_critical_errors_total).
```

---

### Gap #5: No Multi-Step Recovery Process
**Severity:** HIGH
**Current Behavior:**
```erlang
% Recovery is simple: restart or open circuit
determine_recovery_action(Component, Policy, Options) ->
    case Component#component.failures >= MaxFailures of
        true -> {Component, {circuit_break, Options}};   % Open circuit
        false -> {Component, {restart, Options}}         % Try restart
    end.

% Only two outcomes: restart or circuit breaker
% NO intermediate recovery steps
% NO investigation phase
```

**Problem:**
- Binary recovery: restart or fail
- No investigation of root cause
- No logging of failure patterns
- No structured recovery workflow

**Risk:**
- Same error recurs repeatedly
- No data for debugging
- Recovery decisions not informed by context
- Missed opportunities for graceful degradation

**Solution Needed:**
```erlang
% Multi-step recovery:
% 1. INVESTIGATE - Collect diagnostics, save logs
% 2. ATTEMPT_RECOVERY - Try standard recovery actions
% 3. DEGRADE - If recovery fails, reduce service scope
% 4. NOTIFY - Alert operators if automatic recovery exhausted
% 5. REQUIRE_MANUAL - Force operator decision if still failing

-type recovery_step() :: investigate | attempt | degrade | notify | manual.
```

---

### Gap #6: No Resource Exhaustion Hard Limit
**Severity:** HIGH
**Current Behavior:**
```erlang
% Rate limiter blocks requests when rate exceeded
case NewCount >= Threshold of
    true ->
        % Block client for 5 minutes
        BlockUntil = TimeNowMs + BlockDurationMs,
        ets:insert(State#state.clients, {ClientId, ClientState#{blocked_until => BlockUntil}});
    false ->
        ok
end.

% BUT no system-wide resource hard stop
% Process count limit: just issues warning
case ProcessUsage > 0.9 of
    true -> {process_limit_critical, erlang:timestamp()};
    false -> ok
end.
```

**Problem:**
- Limits are soft - advisory only
- No hard stop when resource exhaustion detected
- System can still crash from resource limits
- No "emergency shutdown" when limits hit

**Risk:**
- Out-of-memory crash
- Process limit reached → system failure
- No controlled degradation path

**Solution Needed:**
```erlang
% Add hard resource limits with emergency shutdown
check_system_resources() ->
    case memory_usage() > CRITICAL_THRESHOLD of
        true -> trigger_emergency_shutdown(memory_exhaustion);
        false -> ok
    end,
    case process_count() > CRITICAL_PROCESS_LIMIT of
        true -> trigger_emergency_shutdown(process_limit);
        false -> ok
    end.
```

---

### Gap #7: Recovery Decisions Not Based on Pattern Analysis
**Severity:** MEDIUM
**Current Behavior:**
```erlang
% Each failure is treated independently
handle_component_failure(ComponentId, Reason, _Options, State) ->
    NewFailures = Component#component.failures + 1,

    % Decision: restart if under threshold, circuit break if over
    % NO pattern analysis:
    % - Is this the same error as before?
    % - Are failures increasing in frequency?
    % - Is there a trend of cascading failures?
end.
```

**Problem:**
- No root cause analysis
- No pattern matching (same error recurring)
- No trend analysis (failures getting worse)
- No context-aware recovery (failure type vs recovery strategy)

**Risk:**
- Repeated attempts at same failing recovery
- Missed opportunity to try different approach
- No early warning of systemic issues
- Inefficient resource use

**Solution Needed:**
```erlang
% Add pattern analysis
analyze_failure_pattern(ComponentId, Reason) ->
    History = get_failure_history(ComponentId),
    case analyze_trends(History) of
        {same_error, _Count} ->
            % Different recovery strategy needed
            suggest_alternative_recovery();
        {increasing_frequency, Rate} ->
            % Systemic issue, escalate
            escalate_to_manual_recovery();
        {cascading, Components} ->
            % Multiple components failing, investigate dependencies
            investigate_cascade()
    end.
```

---

## 3. Recommended Andon Cord Enhancements

### Priority 1: CRITICAL (Implement Immediately)

#### 3.1.1 Persistent Shutdown State
```erlang
% File: src/erlmcp_shutdown_control.erl
-module(erlmcp_shutdown_control).
-behaviour(gen_server).

% Add HALTED state that persists across restarts
-type shutdown_state() :: normal | halted_manual | halted_fatal.

% Store in ETS with disk backup
-spec set_shutdown_state(component_id(), shutdown_state()) -> ok.
-spec get_shutdown_state(component_id()) -> shutdown_state().
-spec is_component_halted(component_id()) -> boolean().

% Check at startup: if component is HALTED, don't restart
init([]) ->
    case erlmcp_shutdown_control:get_shutdown_state(erlmcp_recovery_manager) of
        halted_manual ->
            % Don't start this component
            {error, {component_halted_manual, erlmcp_recovery_manager}};
        halted_fatal ->
            % System-wide halt in effect
            {error, system_halt};
        normal ->
            % Proceed with startup
            {ok, State}
    end.

% Recovery manager respects halt state
attempt_component_restart(Component, Reason, Options) ->
    ComponentId = Component#component.id,
    case erlmcp_shutdown_control:is_component_halted(ComponentId) of
        true ->
            ?LOG_ERROR("Component ~p is halted, skipping restart", [ComponentId]),
            {error, component_halted};
        false ->
            % Proceed with restart
            restart_component(ComponentId, Reason, Options)
    end.
```

**Implementation Steps:**
1. Add persistent halt state storage (ETS + disk backup)
2. Check halt state in recovery manager before each restart
3. Check halt state at application startup
4. Add API for operators to set halt state
5. Add dashboard indicator for halted components
6. Document halt lifecycle: normal → halted_manual → manual_resume

---

#### 3.1.2 Graceful Shutdown with Connection Draining
```erlang
% File: src/erlmcp_graceful_shutdown.erl
-module(erlmcp_graceful_shutdown).

-type shutdown_phase() :: normal | draining | halting | stopped.

% Implement 3-phase shutdown
-spec initiate_graceful_shutdown(component_id()) -> ok.
-spec initiate_graceful_shutdown(component_id(), timeout()) -> ok.

% Phase 1: DRAINING - Accept no new connections, finish in-flight
initiate_graceful_shutdown(ComponentId) ->
    % 1. Set component to draining state
    erlmcp_shutdown_control:set_shutdown_phase(ComponentId, draining),

    % 2. Stop accepting new connections
    erlmcp_transport_sup:reject_new_connections(ComponentId),

    % 3. Wait for in-flight requests to complete (timeout: 30s)
    wait_for_in_flight_requests(ComponentId, 30000),

    % 4. Force close remaining connections
    erlmcp_shutdown_control:set_shutdown_phase(ComponentId, halting),
    force_close_connections(ComponentId),

    % 5. Save final state
    erlmcp_shutdown_control:set_shutdown_phase(ComponentId, stopped),

    % 6. Terminate component
    supervisor:terminate_child(erlmcp_server_sup, ComponentId),

    ok.

% Implementation in server
handle_info(shutdown_draining, State) ->
    % Stop accepting new messages
    NewState = State#state{accepting_new = false},

    % Start drain timer
    {noreply, NewState, {continue, {drain_timeout, 30000}}};

handle_continue({drain_timeout, Timeout}, State) ->
    % If still have pending requests, force close
    case map_size(State#state.pending) of
        0 -> {stop, normal, State};
        Count ->
            ?LOG_WARNING("Forcing close ~p pending requests", [Count]),
            {stop, normal, State}
    end.
```

**Implementation Steps:**
1. Add graceful shutdown module with 3 phases
2. Modify server to respect draining phase
3. Implement connection tracking and drain waiting
4. Add timeout to prevent indefinite drain
5. Ensure state is saved before termination
6. Add metrics for drain duration

---

#### 3.1.3 State Persistence and Recovery
```erlang
% File: src/erlmcp_persistence.erl
-module(erlmcp_persistence).

% Persist critical state before shutdown
-spec save_component_state(component_id(), map()) -> ok | {error, term()}.
-spec load_component_state(component_id()) -> {ok, map()} | {error, not_found}.
-spec save_pending_requests(component_id(), map()) -> ok | {error, term()}.
-spec load_pending_requests(component_id()) -> {ok, map()} | {error, not_found}.

% Implementation
save_component_state(ComponentId, State) ->
    % 1. Extract critical fields
    CriticalState = #{
        component_id => ComponentId,
        pending_requests => State#state.pending,
        subscriptions => State#state.subscriptions,
        timestamp => erlang:system_time(millisecond)
    },

    % 2. Serialize to JSON
    Json = jsx:encode(CriticalState),

    % 3. Write to disk with atomic rename (crash-safe)
    Filename = state_filename(ComponentId),
    TmpFile = Filename ++ ".tmp",
    case file:write_file(TmpFile, Json) of
        ok -> file:rename(TmpFile, Filename);
        Error -> Error
    end.

% On recovery, replay pending requests
init([ComponentId]) ->
    case erlmcp_persistence:load_component_state(ComponentId) of
        {ok, SavedState} ->
            % Restore pending requests and subscriptions
            Pending = maps:get(pending_requests, SavedState, #{}),
            Subscriptions = maps:get(subscriptions, SavedState, #{}),
            State = #state{pending = Pending, subscriptions = Subscriptions},
            {ok, State};
        {error, not_found} ->
            % Fresh start
            {ok, #state{}}
    end.
```

**Implementation Steps:**
1. Create persistence module with state serialization
2. Integrate save_state calls in component terminate handlers
3. Add load_state calls in component init
4. Implement replay logic for pending requests
5. Add data validation on load (prevent corrupt states)
6. Test crash recovery scenarios

---

### Priority 2: HIGH (Implement Next)

#### 3.2.1 Multi-Channel Alerting
```erlang
% File: src/erlmcp_alert_manager.erl
-module(erlmcp_alert_manager).
-behaviour(gen_server).

-type alert_channel() :: email | pagerduty | slack | sms.
-type alert_severity() :: critical | warning | info.

% Send alert on critical errors
-spec alert_critical(binary(), map()) -> ok.
-spec alert_warning(binary(), map()) -> ok.

% Configuration
% {erlmcp, [{
%     alerts => #{
%         critical => [email, pagerduty, slack],
%         warning => [email, slack],
%         channels => #{
%             email => #{recipients => ["ops@company.com"]},
%             pagerduty => #{integration_key => "..."},
%             slack => #{webhook_url => "..."}
%         }
%     }
% ]}

alert_critical(Message, Context) ->
    % 1. Log locally
    ?LOG_ERROR("CRITICAL ALERT: ~p~nContext: ~p", [Message, Context]),

    % 2. Send to configured channels
    Channels = get_alert_channels(critical),
    lists:foreach(fun(Channel) ->
        send_alert(Channel, critical, Message, Context)
    end, Channels),

    % 3. Increment metrics
    telemetry:increment(erlmcp_alerts_critical_total, #{channel => multi}).

% Integration examples
send_alert(email, critical, Message, _Context) ->
    Recipients = config:get(alert_recipients_critical, []),
    send_email(Recipients, "CRITICAL: " ++ Message);

send_alert(pagerduty, critical, Message, _Context) ->
    IntegrationKey = config:get(pagerduty_integration_key),
    http_client:post(pagerduty_url(), #{
        routing_key => IntegrationKey,
        event_action => "trigger",
        dedup_key => erlmcp_alert_id(),
        payload => #{
            summary => Message,
            severity => "critical"
        }
    });

send_alert(slack, critical, Message, Context) ->
    WebhookUrl = config:get(slack_webhook_url),
    http_client:post(WebhookUrl, #{
        text => "CRITICAL ALERT",
        attachments => [#{
            color => "danger",
            title => Message,
            fields => format_context_for_slack(Context)
        }]
    }).
```

**Implementation Steps:**
1. Create alert manager with multi-channel support
2. Configure alert channels in sys.config
3. Integrate with health monitor (alert on critical health)
4. Integrate with recovery manager (alert on recovery failure)
5. Integrate with circuit breaker (alert on circuit open)
6. Add alert deduplication to prevent spam
7. Test alerts in staging environment

---

#### 3.2.2 Multi-Step Recovery Process
```erlang
% File: src/erlmcp_recovery_strategist.erl
-module(erlmcp_recovery_strategist).

-type recovery_step() :: investigate | attempt | degrade | notify | manual.
-type recovery_strategy() :: #{
    component_id := component_id(),
    failure_type := atom(),
    failure_count := non_neg_integer(),
    steps := [recovery_step()],
    current_step := recovery_step()
}.

% Determine recovery strategy based on failure history
-spec plan_recovery(component_id(), term(), recovery_manager:state()) -> recovery_strategy().

plan_recovery(ComponentId, Reason, ManagerState) ->
    % 1. Investigate: Collect diagnostics
    Diagnostics = investigate_failure(ComponentId, Reason, ManagerState),

    % 2. Analyze: What type of failure is this?
    FailureType = classify_failure(Diagnostics),
    FailureCount = get_failure_count(ComponentId, ManagerState),

    % 3. Plan recovery steps based on analysis
    case {FailureType, FailureCount} of
        {transient, 1} ->
            % Transient error, try immediate restart
            #{
                component_id => ComponentId,
                failure_type => transient,
                failure_count => 1,
                steps => [attempt, degrade, manual],
                current_step => attempt
            };
        {transient, N} when N < 3 ->
            % Repeated transient, try with delay
            #{
                component_id => ComponentId,
                failure_type => transient,
                failure_count => N,
                steps => [attempt, degrade, notify, manual],
                current_step => attempt
            };
        {persistent, N} when N > 3 ->
            % Persistent failure, skip restart and degrade
            #{
                component_id => ComponentId,
                failure_type => persistent,
                failure_count => N,
                steps => [degrade, notify, manual],
                current_step => degrade
            };
        {cascade, _} ->
            % Cascading failure, escalate immediately
            #{
                component_id => ComponentId,
                failure_type => cascade,
                failure_count => FailureCount,
                steps => [notify, manual],
                current_step => notify
            };
        {resource_exhaustion, _} ->
            % Resource issue, degrade services
            #{
                component_id => ComponentId,
                failure_type => resource_exhaustion,
                failure_count => FailureCount,
                steps => [degrade, notify, manual],
                current_step => degrade
            };
        _ ->
            % Unknown, be conservative
            #{
                component_id => ComponentId,
                failure_type => unknown,
                failure_count => FailureCount,
                steps => [investigate, attempt, notify, manual],
                current_step => investigate
            }
    end.

% Execute recovery steps
-spec execute_recovery_step(recovery_strategy()) -> recovery_strategy().

execute_recovery_step(Strategy = #{current_step := investigate, steps := Steps}) ->
    Diagnostics = collect_diagnostics(Strategy),
    ?LOG_INFO("Diagnostics collected: ~p", [Diagnostics]),
    move_to_next_step(Strategy);

execute_recovery_step(Strategy = #{current_step := attempt, component_id := ComponentId}) ->
    case attempt_restart(ComponentId) of
        {ok, _Pid} ->
            ?LOG_INFO("Recovery succeeded for ~p", [ComponentId]),
            Strategy#{current_step => success};
        {error, Reason} ->
            ?LOG_WARNING("Recovery failed for ~p: ~p", [ComponentId, Reason]),
            move_to_next_step(Strategy)
    end;

execute_recovery_step(Strategy = #{current_step := degrade, component_id := ComponentId}) ->
    ?LOG_WARNING("Activating graceful degradation for ~p", [ComponentId]),
    erlmcp_graceful_shutdown:initiate_graceful_shutdown(ComponentId),
    move_to_next_step(Strategy);

execute_recovery_step(Strategy = #{current_step := notify, component_id := ComponentId}) ->
    ?LOG_ERROR("Manual intervention required for ~p", [ComponentId]),
    erlmcp_alert_manager:alert_critical(
        "Recovery exhausted for " ++ atom_to_list(ComponentId),
        Strategy),
    move_to_next_step(Strategy);

execute_recovery_step(Strategy = #{current_step := manual}) ->
    % Operator action required, halt automatic recovery
    erlmcp_shutdown_control:set_shutdown_state(
        maps:get(component_id, Strategy),
        halted_manual),
    Strategy.

-spec move_to_next_step(recovery_strategy()) -> recovery_strategy().
move_to_next_step(Strategy = #{steps := [Current | Rest]}) ->
    case Rest of
        [] -> Strategy#{current_step => manual};
        [Next | _] -> Strategy#{current_step => Next}
    end.
```

**Implementation Steps:**
1. Create recovery strategist module
2. Implement failure classification logic
3. Add pattern analysis for repeated failures
4. Implement cascade detection
5. Integrate with alert manager
6. Add dashboards to visualize recovery decisions
7. Document recovery strategies

---

#### 3.2.3 Resource Exhaustion Hard Limits
```erlang
% File: src/erlmcp_resource_guardian.erl
-module(erlmcp_resource_guardian).
-behaviour(gen_server).

-type critical_resource() :: memory | processes | file_descriptors.

% Check resource limits continuously
-spec start_link() -> {ok, pid()} | {error, term()}.

% Configuration
% {erlmcp, [{
%     resource_limits => #{
%         memory_hard_limit_percent => 97,    % Absolute maximum
%         process_hard_limit_percent => 95,   % Absolute maximum
%         fd_hard_limit_percent => 90,        % Absolute maximum
%         memory_warning_threshold => 85,
%         process_warning_threshold => 80,
%         memory_degradation_threshold => 90,  % Graceful degradation point
%     }
% ]}

handle_info(check_resources, State) ->
    MemUsage = get_memory_usage(),
    ProcessUsage = get_process_usage(),
    FdUsage = get_fd_usage(),

    case {MemUsage, ProcessUsage, FdUsage} of
        {M, _, _} when M > HARD_LIMIT_MEMORY ->
            trigger_emergency_shutdown(memory_exhaustion);
        {_, P, _} when P > HARD_LIMIT_PROCESSES ->
            trigger_emergency_shutdown(process_limit);
        {_, _, F} when F > HARD_LIMIT_FDS ->
            trigger_emergency_shutdown(fd_limit);
        {M, P, F} when M > DEGRADE_THRESHOLD; P > DEGRADE_THRESHOLD; F > DEGRADE_THRESHOLD ->
            % Graceful degradation: stop accepting new connections
            activate_resource_degradation(#{memory => M, processes => P, fds => F});
        _ ->
            ok
    end,

    erlang:send_after(?CHECK_INTERVAL, self(), check_resources),
    {noreply, State}.

% Emergency shutdown without graceful drain
-spec trigger_emergency_shutdown(critical_resource()) -> ok.
trigger_emergency_shutdown(Resource) ->
    ?LOG_ERROR("EMERGENCY SHUTDOWN: Resource exhaustion (~p)", [Resource]),

    % 1. Alert immediately
    erlmcp_alert_manager:alert_critical(
        "Emergency shutdown triggered: " ++ atom_to_string(Resource),
        #{resource => Resource}),

    % 2. Reject all new connections
    erlmcp_transport_sup:reject_all_new_connections(),

    % 3. Force close active connections (no drain)
    erlmcp_graceful_shutdown:force_close_all_connections(),

    % 4. Wait for cleanup
    timer:sleep(1000),

    % 5. Halt the system
    erlmcp_shutdown_control:set_shutdown_state(erlmcp_sup, halted_fatal),

    % 6. Exit (will be restarted by init system)
    erlang:halt(1).

% Graceful degradation when approaching limits
-spec activate_resource_degradation(#{atom() => float()}) -> ok.
activate_resource_degradation(ResourceUsage) ->
    ?LOG_WARNING("Resource degradation activated: ~p", [ResourceUsage]),

    % Stop accepting new work
    erlmcp_rate_limiter:set_degradation_mode(true),
    erlmcp_transport_sup:reject_new_connections(),

    % Shed lower-priority messages
    erlmcp_backpressure:enable_message_shedding(),

    % Alert operators
    erlmcp_alert_manager:alert_warning(
        "System approaching resource limits",
        ResourceUsage).
```

**Implementation Steps:**
1. Create resource guardian module
2. Add hard limits configuration
3. Implement continuous resource monitoring
4. Add emergency shutdown logic
5. Integrate with graceful shutdown
6. Add metrics and dashboards
7. Document resource limits and escalation

---

## 4. Implementation Roadmap

### Phase 1: Foundation (Week 1)
- [ ] Implement persistent shutdown state
- [ ] Add graceful shutdown with connection draining
- [ ] Create state persistence module
- [ ] Update recovery manager to respect halt state
- [ ] Write unit tests (80% coverage minimum)

### Phase 2: Observability (Week 2)
- [ ] Implement multi-channel alerting
- [ ] Integrate with health monitor alerts
- [ ] Add dashboard indicators for halt state
- [ ] Create operator runbooks
- [ ] Integration testing with staging alerts

### Phase 3: Intelligence (Week 3)
- [ ] Implement recovery strategist
- [ ] Add failure pattern analysis
- [ ] Integrate cascade detection
- [ ] Create recovery decision dashboards
- [ ] Test with failure scenarios

### Phase 4: Safety (Week 4)
- [ ] Implement resource guardian
- [ ] Add hard limit enforcement
- [ ] Implement emergency shutdown
- [ ] Add resource degradation modes
- [ ] Load testing with resource constraints

### Phase 5: Production (Week 5)
- [ ] Chaos engineering testing
- [ ] Documentation and runbooks
- [ ] Operator training
- [ ] Staged rollout with monitoring
- [ ] Post-deployment validation

---

## 5. Testing Strategy

### Failure Scenarios to Test

```erlang
% Test 1: Transient Component Failure
test_transient_failure() ->
    % 1. Crash component
    % 2. Verify recovery manager attempts restart
    % 3. Verify success without user intervention
    % 4. Verify metrics updated

% Test 2: Persistent Component Failure
test_persistent_failure() ->
    % 1. Crash component with same error 5+ times
    % 2. Verify recovery manager eventually halts restarts
    % 3. Verify circuit breaker opens
    % 4. Verify operator alert sent
    % 5. Verify halt state persists across restart

% Test 3: Graceful Shutdown
test_graceful_shutdown() ->
    % 1. Send graceful shutdown to component
    % 2. Verify draining phase rejects new connections
    % 3. Verify existing requests complete
    % 4. Verify state is persisted
    % 5. Verify component terminates cleanly

% Test 4: Resource Exhaustion
test_resource_exhaustion() ->
    % 1. Fill memory to 90%
    % 2. Verify degradation mode activated
    % 3. Fill memory to 97%
    % 4. Verify emergency shutdown triggered
    % 5. Verify graceful restart by init system

% Test 5: Cascade Failure
test_cascade_failure() ->
    % 1. Crash component A
    % 2. Verify component B detects dependency failure
    % 3. Verify cascade detection triggers
    % 4. Verify manual recovery required

% Test 6: Halt State Persistence
test_halt_state_persistence() ->
    % 1. Halt component
    % 2. Restart Erlang VM
    % 3. Verify component doesn't auto-start
    % 4. Verify operator override works
```

---

## 6. Success Criteria

**Andon Cord System is Production-Ready when:**

✅ Gap #1: Persistent shutdown state persists across VM restarts
✅ Gap #2: Graceful shutdown completes within 30 seconds or force-closes
✅ Gap #3: Component state persists to disk before termination
✅ Gap #4: Operators receive alerts within 30 seconds of critical errors
✅ Gap #5: Recovery decisions logged and traceable
✅ Gap #6: Resource exhaustion triggers emergency shutdown before crash
✅ Gap #7: Failure patterns analyzed for root cause
✅ Chaos testing: All failure scenarios handled correctly
✅ Load testing: System stable under 15K concurrent connections
✅ Documentation: Complete runbooks for all failure modes

---

## 7. Operational Runbooks

### When Circuit Breaker Opens

```
1. Check alert - what is the threshold that was exceeded?
   - P95 latency > 200ms?
   - Error rate > 1%?
   - CPU > 90%?
   - Memory > 85%?

2. Investigate root cause
   - Check application logs for errors
   - Check system metrics (CPU, memory, disk I/O)
   - Check for external service failures
   - Check for traffic spikes

3. Take action
   - If temporary spike: wait for circuit to recover (auto-transitions in 60s)
   - If persistent issue: trigger graceful shutdown
   - If cascading: contact on-call architect

4. Recovery
   - Monitor metrics for 5 minutes
   - Verify circuit returns to closed state
   - Check for related failures in other components
```

### When Component is Halted

```
1. Understand why component was halted
   erlmcp_shutdown_control:get_shutdown_state(ComponentId)

2. Investigate root cause (persistent failures)
   erlmcp_recovery_manager:get_recovery_status(ComponentId)

3. Fix the underlying issue
   - Deploy fix to code
   - Update configuration
   - Resolve external dependency

4. Manually restart component
   erlmcp_shutdown_control:set_shutdown_state(ComponentId, normal)
   supervisor:restart_child(erlmcp_server_sup, ComponentId)

5. Verify recovery
   erlmcp_health_monitor:trigger_health_check(ComponentId)
   % Wait for health check to complete
```

### When Resource Degradation Activated

```
1. Monitor which resources are constrained
   erlmcp_resource_guardian:get_resource_usage()

2. Take immediate action
   - If memory: investigate memory leaks, scale horizontally
   - If processes: check for connection leaks
   - If file descriptors: close unused connections

3. Scale if needed
   - Reduce accepted client load
   - Restart component to free memory
   - Add more capacity

4. Monitor recovery
   Wait for resource usage to drop below 80%
```

---

## 8. Metrics & Dashboards

### Key Metrics to Track

```
erlmcp_shutdown_events_total
  - gauge: total_halted_components
  - histogram: halt_duration_minutes
  - counter: halt_reasons {persistent_failure, resource_exhaustion, manual}

erlmcp_recovery_attempts
  - counter: restart_attempts_total
  - counter: restart_success_total
  - gauge: active_recoveries

erlmcp_circuit_breaker
  - gauge: circuit_state {closed=0, half_open=1, open=2}
  - counter: circuit_open_events_total
  - histogram: circuit_recovery_time_seconds

erlmcp_alerts
  - counter: alerts_critical_total {channel}
  - counter: alerts_warning_total {channel}
  - gauge: active_alerts

erlmcp_resource_usage
  - gauge: memory_usage_percent
  - gauge: process_usage_percent
  - gauge: fd_usage_percent
  - counter: degradation_activations_total
  - counter: emergency_shutdowns_total
```

### Dashboard Panels

```
1. System Health Summary
   - Overall status (healthy/degraded/unhealthy)
   - Number of halted components
   - Active alerts
   - Resource usage trends

2. Recovery Activity
   - Restart attempts (success/failure)
   - Recovery strategies used
   - Failure patterns detected
   - Manual interventions required

3. Circuit Breaker Status
   - Global circuit state
   - Recent opens (why? which component?)
   - Recovery times
   - P95 latency and error rate

4. Resource Consumption
   - Memory (current/warning/critical)
   - Processes (current/warning/critical)
   - File descriptors (current/warning/critical)
   - Degradation activations

5. Alert Activity
   - Recent alerts (critical/warning)
   - Alert delivery (email/Slack/PagerDuty)
   - Operator response time
   - Resolved vs open alerts
```

---

## 9. Conclusion

The erlmcp codebase has **solid foundations** for Andon cord functionality but lacks the **production-critical features** of persistence, graceful degradation, and operator integration.

**Current State:** 7/10 (Good detection, reactive recovery)
**Target State:** 10/10 (Predictive, graceful, operator-aware)
**Effort:** 3-4 weeks for full implementation
**Priority:** CRITICAL for production deployment

The recommended enhancements follow Toyota Production System principles:
1. **Stop-the-line:** Prevent cascading failures
2. **Root cause:** Investigate before restarting
3. **Prevention:** Learn from each failure
4. **Operator control:** Allow manual intervention

Implementation of these gaps will transform erlmcp into a truly production-grade system with enterprise-level reliability and operational safety.

---

**Document Version:** 1.0
**Last Updated:** January 27, 2026
**Next Review:** After Phase 1 implementation
