# Andon Cord Implementation Checklist

**Status:** Ready for Implementation Phase
**Complexity:** High (5-phase rollout)
**Estimated Effort:** 3-4 weeks
**Risk Level:** Medium (well-defined with mitigation strategies)

---

## Phase 1: Foundation (Week 1) - CRITICAL FUNCTIONALITY

### 1.1 Persistent Shutdown State Module
- [ ] Create `src/erlmcp_shutdown_control.erl` gen_server
- [ ] Implement ETS table for halt state storage (`shutdown_states`)
- [ ] Add disk backup to `/priv/shutdown_state.json`
- [ ] Implement atomic read/write with crash safety
  - [ ] Write to temp file first
  - [ ] Atomic rename (no partial writes)
  - [ ] Validate on load (checksum or schema)
- [ ] API functions:
  - [ ] `set_shutdown_state(component_id, shutdown_state) -> ok`
  - [ ] `get_shutdown_state(component_id) -> shutdown_state`
  - [ ] `is_component_halted(component_id) -> boolean()`
  - [ ] `list_halted_components() -> [component_id()]`
  - [ ] `clear_shutdown_state(component_id) -> ok`
- [ ] Types:
  - [ ] `normal` - Component running, auto-recovery enabled
  - [ ] `halted_manual` - Operator halted, requires manual resume
  - [ ] `halted_fatal` - System halt in effect
- [ ] Integration with recovery manager:
  - [ ] Check halt state before each restart attempt
  - [ ] Log denial when component is halted
  - [ ] Return error to prevent infinite restart loops
- [ ] Integration with app startup:
  - [ ] Check halt state in `erlmcp_app:start/2`
  - [ ] Skip starting halted components
  - [ ] Log which components skipped
- [ ] Unit tests (80%+ coverage):
  - [ ] Test persistence across VM restart (mock file I/O)
  - [ ] Test concurrent access to halt state
  - [ ] Test invalid state values
  - [ ] Test crash during write (validate temp cleanup)

### 1.2 Graceful Shutdown Module
- [ ] Create `src/erlmcp_graceful_shutdown.erl` module
- [ ] Implement 3-phase shutdown:
  - [ ] **DRAINING**: Accept no new connections, finish in-flight
  - [ ] **HALTING**: Force close remaining connections, save state
  - [ ] **STOPPED**: Component terminated
- [ ] Main API:
  - [ ] `initiate_graceful_shutdown(component_id) -> ok`
  - [ ] `initiate_graceful_shutdown(component_id, timeout_ms) -> ok`
  - [ ] `force_shutdown(component_id) -> ok`
  - [ ] `get_shutdown_phase(component_id) -> shutdown_phase()`
- [ ] Phase implementation:
  - [ ] DRAINING (30s timeout):
    - [ ] Send 'shutdown_draining' message to component
    - [ ] Stop accepting new messages in gen_server
    - [ ] Wait for pending requests to complete
    - [ ] Use monitored timer for timeout
  - [ ] HALTING (10s timeout):
    - [ ] Send 'shutdown_halting' message to component
    - [ ] Force terminate any remaining connections
    - [ ] Call `terminate/2` handler
    - [ ] Supervisor terminates worker
  - [ ] STOPPED:
    - [ ] Component officially shut down
    - [ ] Halt state set to `halted_manual`
- [ ] Connection tracking (add to each server):
  - [ ] Maintain map of active connections: `#{ClientId => RequestRef}`
  - [ ] Track in-flight requests: `#{RequestId => {StartTime, Handler}}`
  - [ ] Await in-flight during draining phase
  - [ ] Force close during halting phase
- [ ] Metrics:
  - [ ] `erlmcp_graceful_shutdown_duration_seconds` (histogram)
  - [ ] `erlmcp_graceful_shutdown_phase` (gauge, phase value)
  - [ ] `erlmcp_graceful_shutdown_drained_requests` (counter)
  - [ ] `erlmcp_graceful_shutdown_forced_closures` (counter)
- [ ] Unit tests:
  - [ ] Test draining with no in-flight requests
  - [ ] Test draining with multiple in-flight requests
  - [ ] Test timeout during draining (force halt)
  - [ ] Test halting phase closes remaining connections
  - [ ] Test metrics are recorded

### 1.3 State Persistence Module
- [ ] Create `src/erlmcp_persistence.erl` module
- [ ] Design state structure:
  ```erlang
  -record(component_state, {
      component_id :: atom(),
      timestamp :: integer(),
      pending_requests :: #{request_id() => request()},
      subscriptions :: #{uri() => subscription()},
      sessions :: #{session_id() => session()},
      checksum :: binary()
  }).
  ```
- [ ] Implement serialization:
  - [ ] `save_component_state(component_id, state_map) -> ok | {error, term()}`
  - [ ] `load_component_state(component_id) -> {ok, map()} | {error, not_found}`
  - [ ] `delete_component_state(component_id) -> ok`
  - [ ] `list_saved_states() -> [component_id()]`
- [ ] Persistence strategy (crash-safe):
  - [ ] Write to temporary file: `priv/state/{component_id}.json.tmp`
  - [ ] Calculate SHA256 checksum of content
  - [ ] Include checksum in file
  - [ ] Atomic rename: `.tmp` → `.json`
  - [ ] On load, verify checksum (reject corrupt files)
- [ ] Request replay:
  - [ ] `list_pending_requests(component_id) -> [request()]`
  - [ ] `load_pending_requests(component_id) -> {ok, map()} | {error, not_found}`
  - [ ] `save_pending_request(component_id, request_id, request) -> ok`
  - [ ] `delete_pending_request(component_id, request_id) -> ok`
- [ ] Cleanup:
  - [ ] `cleanup_stale_states(max_age_seconds) -> {deleted, count()}`
  - [ ] Auto-cleanup on load (remove files older than 7 days)
- [ ] Integration with server init:
  - [ ] `init([ComponentId])` calls `load_component_state(ComponentId)`
  - [ ] Restore `pending_requests` and `subscriptions`
  - [ ] Set state fields from loaded data
  - [ ] Log recovery details
- [ ] Integration with server terminate:
  - [ ] `terminate(Reason, State)` calls `save_component_state(ComponentId, State)`
  - [ ] Capture critical fields only (not full state)
  - [ ] Handle write errors gracefully (log but don't fail shutdown)
- [ ] Unit tests:
  - [ ] Test save and load round-trip
  - [ ] Test crash during write (verify temp file cleanup)
  - [ ] Test corrupt file detection (bad checksum)
  - [ ] Test automatic cleanup of old files
  - [ ] Test concurrent save operations

### 1.4 Recovery Manager Integration
- [ ] Update `src/erlmcp_recovery_manager.erl`:
  - [ ] In `attempt_component_restart`:
    ```erlang
    case erlmcp_shutdown_control:is_component_halted(ComponentId) of
        true ->
            ?LOG_ERROR("Component halted, skipping restart", [ComponentId]),
            {error, component_halted};
        false ->
            restart_component(ComponentId, Reason, Options)
    end.
    ```
  - [ ] Log all restart denials
  - [ ] Update metrics to reflect halted components
- [ ] Add halt state check in `init/1`:
  - [ ] If component halted at startup, don't attempt recovery
  - [ ] Log skip with reason

### 1.5 Application Startup Integration
- [ ] Update `src/erlmcp_app.erl`:
  - [ ] In `start/2`, check for halted components
  - [ ] Skip starting halted components (don't fail startup)
  - [ ] Log which components are halted
  - [ ] Return success even with halted components
- [ ] Update `src/erlmcp_sup.erl`:
  - [ ] Modify child specs to use dynamic startup
  - [ ] Implement selective child startup based on halt state

### 1.6 Phase 1 Testing
- [ ] Unit tests for all three modules (80%+ coverage)
- [ ] Integration tests:
  - [ ] Test component crash → halt state → prevent restart
  - [ ] Test graceful shutdown → state saved → recovery on restart
  - [ ] Test startup with halted components (skip gracefully)
  - [ ] Test concurrent halt state updates
- [ ] Crash scenario tests:
  - [ ] VM crash during state write (verify recovery)
  - [ ] VM crash during shutdown draining (verify cleanup)
  - [ ] Rapid component restart cycles (verify halt prevents)

### 1.7 Documentation
- [ ] Update `/docs/architecture.md` with halt state section
- [ ] Document grace shutdown phases with diagram
- [ ] Add shutdown integration guide
- [ ] Create operator runbook: "Component Halted - What Do I Do?"

---

## Phase 2: Observability (Week 2) - ALERTING & DASHBOARDS

### 2.1 Alert Manager
- [ ] Create `src/erlmcp_alert_manager.erl` gen_server
- [ ] Configuration in `sys.config`:
  ```erlang
  {erlmcp, [{
      alerts => #{
          critical => [email, pagerduty, slack],
          warning => [email, slack],
          channels => #{
              email => #{
                  enabled => true,
                  recipients => ["ops@company.com"],
                  smtp_host => "smtp.gmail.com",
                  smtp_port => 587
              },
              pagerduty => #{
                  enabled => true,
                  integration_key => "SECRET_KEY"
              },
              slack => #{
                  enabled => true,
                  webhook_url => "https://hooks.slack.com/services/..."
              },
              sms => #{
                  enabled => false,
                  twilio_api_key => "SECRET_KEY",
                  phone_number => "+1-555-0123"
              }
          }
      }
  }]}
  ```
- [ ] Core API:
  - [ ] `alert_critical(title, context) -> ok`
  - [ ] `alert_warning(title, context) -> ok`
  - [ ] `alert_info(title, context) -> ok`
  - [ ] `alert_custom(severity, title, context, channels) -> ok`
  - [ ] `get_alert_history(limit) -> [alert()]`
- [ ] Multi-channel implementation:
  - [ ] **Email**: Use `gen_smtp` library
    - [ ] Queue emails (don't block caller)
    - [ ] Retry on failure (3 attempts)
    - [ ] HTML and plain text templates
  - [ ] **PagerDuty**: Use `gun` HTTP client
    - [ ] Create incident on critical
    - [ ] Resolve incident on recovery
    - [ ] Dedup by incident key
  - [ ] **Slack**: Use `gun` HTTP client
    - [ ] Format messages with context
    - [ ] Use message threads for related alerts
    - [ ] Add action buttons for manual recovery
  - [ ] **SMS** (optional, enable separately):
    - [ ] Use Twilio API
    - [ ] Only for critical + P1 on-call
- [ ] Alert deduplication:
  - [ ] Track recent alerts: `{title, severity, hash(context)}`
  - [ ] Suppress duplicate alerts within 5 minutes
  - [ ] Allow manual force-send of suppressed alerts
- [ ] Metrics:
  - [ ] `erlmcp_alerts_total{severity, channel}` (counter)
  - [ ] `erlmcp_alerts_pending{channel}` (gauge)
  - [ ] `erlmcp_alerts_failed{channel}` (counter)
  - [ ] `erlmcp_alert_delivery_time_seconds{channel}` (histogram)
- [ ] Tests:
  - [ ] Test each channel separately
  - [ ] Test deduplication logic
  - [ ] Test failure handling and retry
  - [ ] Test context formatting

### 2.2 Health Monitor Integration
- [ ] Update `src/erlmcp_health_monitor.erl`:
  - [ ] On critical system health: call alert manager
    ```erlang
    case SystemHealth of
        unhealthy ->
            erlmcp_alert_manager:alert_critical(
                "System health critical",
                #{status => unhealthy, metrics => SystemMetrics}
            );
        degraded ->
            erlmcp_alert_manager:alert_warning(
                "System health degraded",
                #{status => degraded, metrics => SystemMetrics}
            )
    end.
    ```
  - [ ] On component unhealthy: call alert manager
  - [ ] On memory critical: call alert manager
  - [ ] On process limit critical: call alert manager

### 2.3 Recovery Manager Integration
- [ ] Update `src/erlmcp_recovery_manager.erl`:
  - [ ] On max failures exceeded:
    ```erlang
    case Component#component.failures >= MaxFailures of
        true ->
            erlmcp_alert_manager:alert_warning(
                "Component recovery exhausted",
                #{component => ComponentId, failures => MaxFailures}
            )
    end.
    ```
  - [ ] On circuit breaker open
  - [ ] On circuit breaker half-open recovery

### 2.4 Circuit Breaker Integration
- [ ] Update `src/erlmcp_circuit_breaker.erl`:
  - [ ] On circuit open: alert
    ```erlang
    case State#state.circuit_state of
        closed when ThresholdExceeded ->
            erlmcp_alert_manager:alert_critical(
                "Circuit breaker opened",
                #{p95_latency => P95, error_rate => ErrorRate}
            )
    end.
    ```
  - [ ] On circuit half-open
  - [ ] On circuit close (recovery)

### 2.5 Dashboard Updates
- [ ] Add Grafana dashboards (or equivalent):
  - [ ] **System Health Panel**:
    - [ ] Current health status
    - [ ] Component health distribution (pie chart)
    - [ ] Alert history (last 24 hours)
  - [ ] **Alert Activity Panel**:
    - [ ] Recent alerts (timestamp, severity, title)
    - [ ] Alert delivery status (which channels failed?)
    - [ ] Operator response times
  - [ ] **Circuit Breaker Status Panel**:
    - [ ] Circuit state (closed/open/half-open)
    - [ ] Metrics that triggered open (P95, error rate)
    - [ ] Recovery countdown (if half-open)
  - [ ] **Component Halt State Panel**:
    - [ ] List of halted components
    - [ ] Reason for halt (manual vs fatal)
    - [ ] Time since halt
    - [ ] Manual resume button
- [ ] Add SSE/WebSocket updates for real-time dashboard
  - [ ] Alert pushed immediately to connected clients
  - [ ] Status changes streamed live

### 2.6 Phase 2 Testing
- [ ] Unit tests for alert manager (80%+ coverage)
- [ ] Integration tests:
  - [ ] Test email delivery (mock SMTP)
  - [ ] Test PagerDuty webhook (mock HTTP)
  - [ ] Test Slack webhook (mock HTTP)
  - [ ] Test deduplication logic
  - [ ] Test health monitor integration
- [ ] End-to-end tests:
  - [ ] Trigger critical condition → verify alert sent
  - [ ] Check all channels receive alert
  - [ ] Verify operator response tracking

### 2.7 Documentation
- [ ] Create alert runbook for each alert type
- [ ] Document channel configuration
- [ ] Add dashboard screenshots to docs
- [ ] Write operator guide: "Understanding Erlmcp Alerts"

---

## Phase 3: Intelligence (Week 3) - RECOVERY STRATEGIES

### 3.1 Recovery Strategist Module
- [ ] Create `src/erlmcp_recovery_strategist.erl` module
- [ ] Failure classification:
  - [ ] `transient` - Single error, likely temporary
  - [ ] `persistent` - Repeated same error
  - [ ] `resource_exhaustion` - Memory/process limit
  - [ ] `cascade` - Multiple dependent components failing
  - [ ] `unknown` - Can't determine failure type
- [ ] Failure history tracking:
  - [ ] Store last 100 failures per component
  - [ ] Track time between failures
  - [ ] Detect patterns (same error recurring)
  - [ ] Calculate failure frequency
- [ ] Recovery strategy selection:
  ```erlang
  plan_recovery(ComponentId, Reason, ManagerState) ->
      History = get_failure_history(ComponentId),
      Pattern = analyze_failure_pattern(History, Reason),

      case Pattern of
          {transient, 1} ->
              #{steps => [attempt, degrade, manual]};
          {transient, N} when N < 3 ->
              #{steps => [attempt, degrade, notify, manual]};
          {persistent, N} when N > 3 ->
              #{steps => [degrade, notify, manual]};
          {cascade, _} ->
              #{steps => [notify, manual]};
          {resource_exhaustion, _} ->
              #{steps => [degrade, notify, manual]};
          _ ->
              #{steps => [investigate, attempt, notify, manual]}
      end
  ```
- [ ] Recovery step execution:
  - [ ] **investigate**: Collect logs, metrics, diagnostics
  - [ ] **attempt**: Try standard recovery (restart with backoff)
  - [ ] **degrade**: Gracefully shutdown and reduce service scope
  - [ ] **notify**: Alert operators with context
  - [ ] **manual**: Force manual operator decision
- [ ] Integration with recovery manager:
  - [ ] Use strategist in `determine_recovery_action`
  - [ ] Execute recovery steps in sequence
  - [ ] Skip steps if earlier step succeeds
  - [ ] Log each step decision
- [ ] Metrics:
  - [ ] `erlmcp_recovery_strategy_selected{type}` (counter)
  - [ ] `erlmcp_recovery_step_executed{step, result}` (counter)
  - [ ] `erlmcp_failure_pattern_detected{pattern}` (counter)
  - [ ] `erlmcp_failure_history_size{component}` (gauge)
- [ ] Tests:
  - [ ] Test pattern detection (repeated errors)
  - [ ] Test cascade detection (dependent failures)
  - [ ] Test strategy selection for each pattern
  - [ ] Test step execution ordering

### 3.2 Failure Pattern Analysis
- [ ] Implement pattern analysis functions:
  - [ ] `analyze_trend(history) -> {increasing | decreasing | stable | unknown}`
  - [ ] `is_same_error(error1, error2) -> boolean()`
  - [ ] `detect_cascade(failed_components) -> {cascading, [root_causes]}`
  - [ ] `predict_next_failure(history) -> {error_type, confidence_percent}`
- [ ] Error grouping:
  - [ ] Group by error type (not just stacktrace)
  - [ ] Normalize error messages (remove timestamps)
  - [ ] Compare root causes (not symptoms)
- [ ] Trend analysis:
  - [ ] Calculate failures/minute over last 5 minutes
  - [ ] Detect increasing trends (sign of systemic issue)
  - [ ] Detect stable patterns (intermittent issue)
- [ ] Cascade detection:
  - [ ] Track component dependencies
  - [ ] If component A fails → detect if B, C also fail
  - [ ] Identify root cause (likely the first to fail)

### 3.3 Integration with Recovery Manager
- [ ] Update `src/erlmcp_recovery_manager.erl`:
  - [ ] Call strategist in failure handling:
    ```erlang
    handle_component_failure(ComponentId, Reason, Options, State) ->
        Strategy = erlmcp_recovery_strategist:plan_recovery(
            ComponentId, Reason, State),
        execute_recovery_strategy(Strategy, State)
    ```
  - [ ] Execute recovery steps from strategy
  - [ ] Log strategy decisions to persistent log
  - [ ] Track strategy effectiveness

### 3.4 Phase 3 Testing
- [ ] Unit tests for pattern analysis (80%+ coverage)
- [ ] Test data: Prepare failure scenarios
  - [ ] Repeated transient errors
  - [ ] Persistent errors (same every time)
  - [ ] Cascading failures (multiple components)
  - [ ] Resource exhaustion
- [ ] Integration tests:
  - [ ] Simulate failures and verify strategy selection
  - [ ] Verify recovery steps executed in order
  - [ ] Verify pattern detection accuracy

### 3.5 Documentation
- [ ] Create recovery strategy guide
- [ ] Document pattern analysis logic
- [ ] Add examples of each failure type
- [ ] Write troubleshooting guide

---

## Phase 4: Safety (Week 4) - RESOURCE LIMITS

### 4.1 Resource Guardian Module
- [ ] Create `src/erlmcp_resource_guardian.erl` gen_server
- [ ] Configuration in `sys.config`:
  ```erlang
  {erlmcp, [{
      resource_limits => #{
          % Hard limits - trigger emergency shutdown
          memory_hard_limit_percent => 97,
          process_hard_limit_percent => 95,
          fd_hard_limit_percent => 90,
          % Degradation thresholds - reduce service
          memory_degradation_threshold => 90,
          process_degradation_threshold => 80,
          % Warning thresholds - alert operator
          memory_warning_threshold => 85,
          process_warning_threshold => 75,
          % Check interval
          check_interval_ms => 5000
      }
  }]}
  ```
- [ ] Resource measurement:
  - [ ] Memory: `erlang:memory(processes) / erlang:memory(total)`
  - [ ] Processes: `erlang:system_info(process_count) / erlang:system_info(process_limit)`
  - [ ] File descriptors: Platform-specific (Linux: `/proc/self/fd`)
- [ ] Continuous monitoring:
  - [ ] Check every 5 seconds
  - [ ] Track trends (is usage increasing?)
  - [ ] Predict when hard limit will be hit
- [ ] Warning threshold (85% memory):
  - [ ] Alert operator
  - [ ] Recommend action (restart, scale)
  - [ ] No service degradation yet
- [ ] Degradation threshold (90% memory):
  - [ ] Activate graceful degradation
  - [ ] Stop accepting new connections
  - [ ] Stop accepting new tool calls
  - [ ] Shed low-priority messages
  - [ ] Continue handling existing work
- [ ] Hard limit (97% memory):
  - [ ] Trigger emergency shutdown
  - [ ] NO graceful drain (would use more memory)
  - [ ] Force close all connections
  - [ ] Save critical state
  - [ ] Halt the system
  - [ ] Let init system restart
- [ ] API:
  - [ ] `get_resource_usage() -> #{atom() => float()}`
  - [ ] `is_degradation_active() -> boolean()`
  - [ ] `trigger_degradation(resource_type) -> ok`
  - [ ] `trigger_emergency_shutdown(resource_type) -> noreturn()`
- [ ] Metrics:
  - [ ] `erlmcp_memory_usage_percent` (gauge)
  - [ ] `erlmcp_process_usage_percent` (gauge)
  - [ ] `erlmcp_fd_usage_percent` (gauge)
  - [ ] `erlmcp_degradation_active{resource}` (gauge)
  - [ ] `erlmcp_emergency_shutdowns_total{resource}` (counter)
- [ ] Tests:
  - [ ] Test resource measurement accuracy
  - [ ] Test threshold triggering
  - [ ] Test degradation activation
  - [ ] Test emergency shutdown
  - [ ] Test prediction accuracy

### 4.2 Degradation Mode
- [ ] Create `src/erlmcp_degradation_mode.erl` module
- [ ] Degradation actions:
  - [ ] **Connection limiting**: Reject new connections above baseline
  - [ ] **Rate limiting**: Reduce per-client rate limits by 50%
  - [ ] **Message shedding**: Drop low-priority messages
  - [ ] **Subscription pause**: Pause non-critical subscriptions
  - [ ] **Tool call limiting**: Reduce tool call rate
- [ ] Integration points:
  - [ ] Transport layer: reject new connections
  - [ ] Rate limiter: check degradation mode, reduce limits
  - [ ] Backpressure: increase shedding threshold
  - [ ] Server: pause subscriptions
- [ ] Recovery from degradation:
  - [ ] Monitor resource usage
  - [ ] When usage drops below 80%, exit degradation
  - [ ] Gradually increase limits back to normal
  - [ ] Alert operator of recovery
- [ ] Metrics:
  - [ ] `erlmcp_degradation_connections_rejected` (counter)
  - [ ] `erlmcp_degradation_messages_shed` (counter)
  - [ ] `erlmcp_degradation_rate_limit_reduction{percent}` (gauge)
  - [ ] `erlmcp_degradation_duration_seconds` (histogram)

### 4.3 Emergency Shutdown
- [ ] Implement in `erlmcp_resource_guardian.erl`:
  ```erlang
  trigger_emergency_shutdown(Resource) ->
      ?LOG_ERROR("EMERGENCY SHUTDOWN: ~p", [Resource]),

      % 1. Alert
      erlmcp_alert_manager:alert_critical(
          "Emergency shutdown", #{resource => Resource}),

      % 2. Halt system
      erlmcp_shutdown_control:set_shutdown_state(erlmcp_sup, halted_fatal),

      % 3. Exit (init system will restart)
      erlang:halt(1).
  ```
- [ ] Graceful vs emergency shutdown:
  - [ ] Graceful: 30s drain, finish work, save state
  - [ ] Emergency: immediate halt, no drain, minimal cleanup
- [ ] Recovery from emergency shutdown:
  - [ ] Init system detects exit(1)
  - [ ] Restarts erlmcp app
  - [ ] Loads component state from disk
  - [ ] Replays pending requests
  - [ ] Sets halt state to `halted_fatal` → operators must investigate

### 4.4 Phase 4 Testing
- [ ] Unit tests for resource guardian (80%+ coverage)
- [ ] Resource simulation tests:
  - [ ] Simulate memory growth → verify thresholds trigger
  - [ ] Simulate process creation → verify limits
  - [ ] Verify degradation prevents hard limits
- [ ] Emergency shutdown tests:
  - [ ] Force memory to 97% → verify shutdown
  - [ ] Verify process restarts via init system
  - [ ] Verify state recovered from disk
- [ ] Load testing:
  - [ ] 15K concurrent connections with gradual load increase
  - [ ] Verify degradation prevents OOM
  - [ ] Monitor resource recovery

### 4.5 Documentation
- [ ] Document resource limits configuration
- [ ] Create operator guide: "System Running Out of Resources"
- [ ] Explain degradation modes
- [ ] Emergency shutdown procedures

---

## Phase 5: Production (Week 5) - VALIDATION & DEPLOYMENT

### 5.1 Chaos Engineering Tests
- [ ] Test scenario: Component crash cycle
  - [ ] Crash component 100 times
  - [ ] Verify halt after 5 failures (configurable)
  - [ ] Verify alert sent
  - [ ] Verify operator intervention required
- [ ] Test scenario: Cascading failures
  - [ ] Kill component A
  - [ ] Verify dependent components (B, C) also fail
  - [ ] Verify cascade detection
  - [ ] Verify recovery attempts are strategic (not restart all)
- [ ] Test scenario: Resource exhaustion
  - [ ] Gradually increase memory usage
  - [ ] Verify degradation activates at 90%
  - [ ] Verify emergency shutdown at 97%
  - [ ] Verify recovery after restart
- [ ] Test scenario: Recovery failure
  - [ ] Trigger transient failure (passes recovery)
  - [ ] Trigger persistent failure (fails recovery, escalates)
  - [ ] Verify strategist selects different approach
- [ ] Test scenario: Circuit breaker + recovery interaction
  - [ ] Open circuit due to high latency
  - [ ] Verify recovery manager respects circuit state
  - [ ] Verify graceful recovery sequence

### 5.2 Documentation & Runbooks
- [ ] Create operator runbook directory: `docs/runbooks/`
- [ ] Runbook: `COMPONENT_HALTED.md`
  - [ ] Why is component halted?
  - [ ] How to investigate?
  - [ ] How to resume?
- [ ] Runbook: `RECOVERY_EXHAUSTED.md`
  - [ ] Component keeps failing, recovery unable to fix
  - [ ] Diagnostic steps
  - [ ] Escalation path
- [ ] Runbook: `CIRCUIT_BREAKER_OPEN.md`
  - [ ] Why is circuit open?
  - [ ] How long until recovery attempt?
  - [ ] What actions to take
- [ ] Runbook: `SYSTEM_DEGRADED.md`
  - [ ] Why is degradation active?
  - [ ] Impact on users
  - [ ] How to resolve
- [ ] Runbook: `EMERGENCY_SHUTDOWN.md`
  - [ ] What is emergency shutdown?
  - [ ] How to recover?
  - [ ] Root cause analysis steps

### 5.3 Operator Training
- [ ] Document: "Andon Cord System Overview"
  - [ ] Philosophy (stop-the-line)
  - [ ] Key concepts (halt, graceful shutdown, degradation)
  - [ ] How to interpret alerts
- [ ] Video tutorials:
  - [ ] Navigating the dashboard
  - [ ] Understanding alert types
  - [ ] Triggering manual shutdown
  - [ ] Resuming halted components
  - [ ] Checking system health
- [ ] Interactive demos:
  - [ ] Simulate component failure in staging
  - [ ] Show alert notifications
  - [ ] Walk through recovery strategy selection
  - [ ] Practice manual intervention

### 5.4 Staged Rollout
- [ ] Phase 5a: Staging environment
  - [ ] Deploy all Andon cord components
  - [ ] Run chaos tests
  - [ ] Verify all alerts in Slack/email
  - [ ] Train operators on dashboard
  - [ ] Duration: 1 week
- [ ] Phase 5b: Canary deployment (10% production)
  - [ ] Deploy to 1-2 production instances
  - [ ] Monitor for 3 days
  - [ ] Verify no performance impact
  - [ ] Verify all metrics correct
  - [ ] Duration: 3 days
- [ ] Phase 5c: 50% production rollout
  - [ ] Deploy to 50% of production instances
  - [ ] Monitor for 5 days
  - [ ] Verify recovery behavior in production
  - [ ] Duration: 5 days
- [ ] Phase 5d: Full production rollout
  - [ ] Deploy to remaining 50%
  - [ ] Full monitoring and operations
  - [ ] On-call engineer available for 48 hours
  - [ ] Duration: ongoing

### 5.5 Post-Deployment Validation
- [ ] Confirm all metrics are correctly recorded:
  - [ ] `erlmcp_shutdown_events_total` > 0 (simulated halt)
  - [ ] `erlmcp_graceful_shutdown_duration_seconds` collected
  - [ ] `erlmcp_alerts_total` > 0 (triggered test alert)
  - [ ] `erlmcp_recovery_strategy_selected` > 0
  - [ ] `erlmcp_resource_usage_percent` tracked
- [ ] Confirm all alerts delivered:
  - [ ] Test critical alert → verify email, Slack, PagerDuty
  - [ ] Verify deduplication (no spam)
  - [ ] Verify channel fallback (if one fails, others work)
- [ ] Confirm dashboards operational:
  - [ ] All panels displaying data
  - [ ] Real-time updates working
  - [ ] Manual controls functioning
- [ ] Documentation review:
  - [ ] All runbooks present and accurate
  - [ ] Screenshots match actual UI
  - [ ] No broken links or references

### 5.6 Success Criteria
- [ ] All gaps implemented and tested
- [ ] 80%+ code coverage on new modules
- [ ] Zero known bugs in production
- [ ] All chaos tests passing
- [ ] Operators confident using system
- [ ] Zero security vulnerabilities

---

## Testing Checklist

### Unit Tests (80%+ Coverage Required)
- [ ] `erlmcp_shutdown_control_tests.erl`
- [ ] `erlmcp_graceful_shutdown_tests.erl`
- [ ] `erlmcp_persistence_tests.erl`
- [ ] `erlmcp_alert_manager_tests.erl`
- [ ] `erlmcp_recovery_strategist_tests.erl`
- [ ] `erlmcp_resource_guardian_tests.erl`
- [ ] `erlmcp_degradation_mode_tests.erl`

### Integration Tests
- [ ] `integration/andon_cord_integration_SUITE.erl`
  - [ ] Component halt + recovery interaction
  - [ ] Graceful shutdown with state persistence
  - [ ] Alert delivery across channels
  - [ ] Recovery strategy selection accuracy
  - [ ] Resource limits enforcement

### Chaos Tests
- [ ] `chaos/component_crash_SUITE.erl`
- [ ] `chaos/cascade_failure_SUITE.erl`
- [ ] `chaos/resource_exhaustion_SUITE.erl`
- [ ] `chaos/recovery_failure_SUITE.erl`

### Load Tests
- [ ] 15K concurrent connections
- [ ] Gradual resource increase → verify degradation
- [ ] Sustained load with component failures
- [ ] Recovery performance impact

---

## Risk Mitigation

### Risk: Performance Impact of Monitoring
**Mitigation:**
- [ ] Profile each new module (target < 1% overhead)
- [ ] Async alert delivery (don't block request path)
- [ ] Batch ETS operations where possible

### Risk: Incomplete Recovery
**Mitigation:**
- [ ] Persist state before every shutdown
- [ ] Test recovery with various crash scenarios
- [ ] Implement request deduplication (prevent double-processing)

### Risk: Alert Fatigue
**Mitigation:**
- [ ] Aggressive deduplication (5-minute window)
- [ ] Distinguish critical from warning alerts
- [ ] Test alert tuning in staging

### Risk: Emergency Shutdown Too Aggressive
**Mitigation:**
- [ ] Start with high thresholds (97% memory)
- [ ] Test extensively before production
- [ ] Allow tuning per environment
- [ ] Monitor false positive rate

---

## Rollback Plan

If critical issues found in production:

1. **Immediate**: Disable alert channels (prevent spam)
   - Stop email, PagerDuty, Slack temporarily
   - Keep logging for debugging

2. **Short-term**: Disable recovery strategist
   - Revert to simple restart/circuit-break logic
   - Keeps halt state and graceful shutdown working

3. **Medium-term**: Disable graceful shutdown
   - Revert to immediate termination (old behavior)
   - Keep halt state for manual control

4. **Full rollback**: Deploy previous version
   - Requires 15 minutes to restart infrastructure
   - Have previous stable version ready

---

## Success Metrics (Post-Deployment)

**Week 1:**
- No escalations from Andon cord issues
- All alerts delivered successfully
- Zero false positive emergency shutdowns

**Week 4:**
- Operators report improved system visibility
- MTTR reduced by 30% (mean time to recovery)
- Fewer cascading failures due to early halt

**Month 1:**
- System downtime reduced by 50%
- Operator confidence increased significantly
- Zero security incidents from degradation mode

---

This checklist provides a complete implementation roadmap for the Andon Cord system. Start with Phase 1 (foundation), then proceed to subsequent phases as resources allow.

**Questions?** Contact the architecture team before starting implementation.
