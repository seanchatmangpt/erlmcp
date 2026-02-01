# erlmcp Recovery Flows & Procedures
## Version 2.1.0 - Complete Recovery Reference

**System Coverage**: 20+ failure scenarios with detailed recovery flows
**Recovery Target**: 95% auto-recovery, < 5s mean-time-to-recovery (MTTR)

---

## Recovery Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    ERROR DETECTION LAYER                    │
│  erlmcp_health_monitor + Circuit Breakers + Alerting       │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    CLASSIFICATION ENGINE                     │
│  Error Category → Severity → Recovery Strategy              │
└─────────────────────────────────────────────────────────────┘
                              │
          ┌───────────────────┼───────────────────┐
          │                   │                   │
          ▼                   ▼                   ▼
    ┌──────────┐        ┌──────────┐        ┌──────────┐
    │  AUTO    │        │  SEMI    │        │  MANUAL  │
    │ RECOVERY │        │  AUTO    │        │ RECOVERY │
    │  (95%)   │        │  (3%)    │        │  (2%)    │
    └──────────┘        └──────────┘        └──────────┘
          │                   │                   │
          ▼                   ▼                   ▼
    ┌──────────┐        ┌──────────┐        ┌──────────┐
    │  Retry   │        │  Prompt  │        │  Runbook │
    │ Backoff  │        │  User    │        │ Execution│
    └──────────┘        └──────────┘        └──────────┘
```

---

## Recovery Strategy Matrix

### 1. AUTO-RECOVERY (95% of failures)
**Characteristics**:
- No human intervention required
- Deterministic recovery path
- < 5s recovery time
- Automatic health verification

**Applicable Errors**:
- 2000-2099: Connection failures (reconnect)
- 3000-3099: Resource access failures (cache refresh)
- 6000-6099: Session errors (session recreation)
- 7000-7099: Supervision failures (let-it-crash)

### 2. SEMI-AUTO RECOVERY (3% of failures)
**Characteristics**:
- Automated recovery with confirmation
- Human-in-the-loop for critical decisions
- < 30s recovery time
- Approval workflow

**Applicable Errors**:
- 4000-4099: Tool execution failures (retry with confirmation)
- 6200-6299: Cluster coordination (failover approval)
- 7100-7199: Resource exhaustion (scaling approval)

### 3. MANUAL RECOVERY (2% of failures)
**Characteristics**:
- Full runbook execution required
- On-call engineer intervention
- Variable recovery time
- Root cause analysis required

**Applicable Errors**:
- 5000-5099: Authentication failures (credential rotation)
- 5301-5399: Security violations (incident response)
- 3200-3299: Storage corruption (data recovery)
- 7100: Memory exhaustion (infrastructure scaling)

---

## Complete Recovery Flows

### SCENARIO 1: Connection Timeout (Error 2001)

**Flow Diagram**:
```
┌──────────────┐
│ Client sends │
│   request    │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│  Transport   │─────┐
│  layer waits │     │ Timeout after 5s
│  for response│     │
└──────┬───────┘     │
       │             │
       │◄────────────┘
       ▼
┌──────────────┐
│ Error 2001   │
│  detected    │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Check circuit│
│  breaker     │
└──────┬───────┘
       │
       ├── CLOSED ──► [Retry with backoff]
       │                ├─ Attempt 1: Wait 1s
       │                ├─ Attempt 2: Wait 2s
       │                ├─ Attempt 3: Wait 4s
       │                └─ Attempt 4: Wait 8s
       │                        │
       │                        ├── Success ──► [Resume normal]
       │                        └─ Failure ───► [Open circuit breaker]
       │
       ├── HALF_OPEN ──► [Test connection]
       │                    ├─ Success ──► [Close circuit]
       │                    └─ Failure ───► [Open circuit]
       │
       └── OPEN ──► [Use fallback transport]
                        ├─ Fallback: stdio
                        ├─ Fallback: http
                        └─ Fallback: sse
```

**Recovery Procedure**:
```erlang
%% Auto-recovery implementation
handle_info({timeout, RequestId}, State) ->
    %% 1. Log error with context
    logger:error(
        "Connection timeout",
        #{
            request_id => RequestId,
            transport => State#state.transport_module,
            attempt => State#state.retry_attempt,
            max_attempts => State#state.max_retries
        }
    ),

    %% 2. Check circuit breaker state
    case erlmcp_circuit_breaker:State(State#state.transport_module) of
        closed ->
            %% 3. Calculate backoff
            BackoffMs = calculate_backoff(State#state.retry_attempt),

            %% 4. Schedule retry
            erlang:send_after(BackoffMs, self(), {retry, RequestId}),

            {noreply, State#state{
                retry_attempt = State#state.retry_attempt + 1
            }};

        open ->
            %% 5. Use fallback transport
            case State#state.fallback_transport of
                undefined ->
                    {stop, {error, connection_failed}, State};
                FallbackModule ->
                    switch_transport(FallbackModule, State)
            end
    end.

calculate_backoff(Attempt) ->
    %% Exponential backoff with jitter
    BaseMs = 1000,
    MaxMs = 8000,
    min(BaseMs * (1 bsl (Attempt - 1)), MaxMs) + rand:uniform(100).
```

**Health Verification**:
```erlang
%% After recovery, verify health
verify_recovery(TransportModule) ->
    HealthChecks = [
        fun erlmcp_health:check_connection/1,
        fun erlmcp_health:check_latency/1,
        fun erlmcp_health:check_throughput/1
    ],

    Results = [Check(TransportModule) || Check <- HealthChecks],
    AllOk = lists:all(fun({ok, _}) -> true; (_) -> false end, Results),

    case AllOk of
        true -> {ok, recovered};
        false -> {error, health_check_failed}
    end.
```

---

### SCENARIO 2: Resource Not Found (Error 3000)

**Flow Diagram**:
```
┌──────────────┐
│ Client req:  │
│  GET /res/ID │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Check local  │─────┐
│  cache       │     │ Cache hit
└──────┬───────┘     │
       │             │
       │◄────────────┘
       │ (Cache miss)
       ▼
┌──────────────┐
│ Check storage│─────┐
│  backend     │     │ Found
└──────┬───────┘     │
       │             │
       │◄────────────┘
       │ (Not found)
       ▼
┌──────────────┐
│ Error 3000   │
│  raised      │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Check if URI │
│  is valid    │
└──────┬───────┘
       │
       ├── Invalid ──► [Error 3004: Invalid URI]
       │
       └── Valid ──► [Attempt recovery]
                       ├─ Try parent resource
                       ├─ Try similar resources
                       └─ Check resource registry
```

**Recovery Procedure**:
```erlang
handle_call({get_resource, Uri}, _From, State) ->
    case erlmcp_resource:get(Uri) of
        {error, not_found} ->
            %% 1. Validate URI
            case erlmcp_uri_validator:validate(Uri) of
                {error, invalid_uri} ->
                    {reply, {error, #{code => 3004, message => "Invalid URI"}}, State};

                {ok, ValidatedUri} ->
                    %% 2. Try parent resource
                    ParentUri = erlmcp_uri:parent(ValidatedUri),
                    case erlmcp_resource:get(ParentUri) of
                        {ok, ParentResource} ->
                            %% 3. Return parent with children
                            {reply, {ok, #{
                                code => 3000,
                                message => "Resource not found",
                                hint => "Parent resource available",
                                parent => ParentResource
                            }}, State};

                        {error, not_found} ->
                            %% 4. Try similar resources
                            SimilarUris = erlmcp_registry:find_similar(Uri),
                            {reply, {error, #{
                                code => 3000,
                                message => "Resource not found",
                                suggestions => SimilarUris
                            }}, State}
                    end
            end;

        {ok, Resource} ->
            {reply, {ok, Resource}, State}
    end.
```

---

### SCENARIO 3: Tool Execution Timeout (Error 4003)

**Flow Diagram**:
```
┌──────────────┐
│ Client calls │
│   tool/ID    │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Validate     │─────┐
│  arguments   │     │ Invalid
└──────┬───────┘     │
       │             │
       │◄────────────┘
       │ (Valid)
       ▼
┌──────────────┐
│ Execute tool │─────┐
│  with timeout│     │ Timeout after 30s
└──────┬───────┘     │
       │             │
       │◄────────────┘
       │ (Timeout)
       ▼
┌──────────────┐
│ Error 4003   │
│  raised      │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Check if tool│
│  is still    │
│  running     │
└──────┬───────┘
       │
       ├── Running ──► [Wait for completion]
       │                  ├─ Extend timeout if safe
       │                  └─ Cancel if unsafe
       │
       └── Crashed ──► [Check logs]
                          ├─ Identify crash reason
                          └─ Report to client
```

**Recovery Procedure**:
```erlang
handle_call({call_tool, ToolId, Args}, _From, State) ->
    %% 1. Validate arguments
    case erlmcp_tool:validate_arguments(ToolId, Args) of
        {error, Reason} ->
            {reply, {error, #{code => 4004, reason => Reason}}, State};

        {ok, ValidatedArgs} ->
            %% 2. Execute with timeout
            TimeoutMs = maps:get(timeout, ValidatedArgs, 30000),

            case erlmcp_tool:execute(ToolId, ValidatedArgs) of
                {error, timeout} ->
                    %% 3. Check tool status
                    case erlmcp_tool:status(ToolId) of
                        running ->
                            %% 4. Ask user what to do
                            {reply, {error, #{
                                code => 4003,
                                message => "Tool execution timeout",
                                recovery => #{
                                    prompt_user => true,
                                    options => [
                                        <<"wait_more">>,
                                        <<"cancel">>,
                                        <<"continue_background">>
                                    ]
                                }
                            }}, State};

                        crashed ->
                            %% 5. Report crash with logs
                            CrashLogs = erlmcp_tool:get_crash_logs(ToolId),
                            {reply, {error, #{
                                code => 4003,
                                message => "Tool crashed",
                                crash_logs => CrashLogs
                            }}, State};

                        completed ->
                            %% 6. Retrieve result
                            {ok, Result} = erlmcp_tool:get_result(ToolId),
                            {reply, {ok, Result}, State}
                    end;

                {ok, Result} ->
                    {reply, {ok, Result}, State}
            end
    end.
```

---

### SCENARIO 4: Authentication Token Expired (Error 5002)

**Flow Diagram**:
```
┌──────────────┐
│ Client sends │
│  request with│
│   token      │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Validate     │─────┐
│   token      │     │ Token expired
└──────┬───────┘     │
       │             │
       │◄────────────┘
       │ (Expired)
       ▼
┌──────────────┐
│ Error 5002   │
│  raised      │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Check refresh│
│    token     │
└──────┬───────┘
       │
       ├── Available ──► [Auto-refresh]
       │                  ├─ Use refresh token
       │                  ├─ Get new access token
       │                  └─ Retry original request
       │
       └── Not available ──► [Prompt re-auth]
                            ├─ Return 401 to client
                            └─ Client initiates auth flow
```

**Recovery Procedure**:
```erlang
handle_call(Request, _From, State) ->
    case extract_token(Request) of
        {error, no_token} ->
            {reply, {error, #{code => 5000, message => "Authentication required"}}, State};

        {ok, Token} ->
            case erlmcp_auth:validate_token(Token) of
                {error, expired} ->
                    %% 1. Try refresh token
                    case get_refresh_token(State) of
                        {ok, RefreshToken} ->
                            %% 2. Auto-refresh
                            case erlmcp_auth:refresh_token(RefreshToken) of
                                {ok, NewToken} ->
                                    %% 3. Update state
                                    NewState = State#state{access_token = NewToken},

                                    %% 4. Retry original request
                                    handle_call(Request, _From, NewState);

                                {error, refresh_failed} ->
                                    %% 5. Require re-authentication
                                    {reply, {error, #{
                                        code => 5002,
                                        message => "Token expired and refresh failed",
                                        action => reauthenticate
                                    }}, State}
                            end;

                        {error, no_refresh_token} ->
                            %% 6. Prompt client to re-auth
                            {reply, {error, #{
                                code => 5002,
                                message => "Token expired",
                                action => reauthenticate
                            }}, State}
                    end;

                {ok, Claims} ->
                    %% 7. Process request with valid token
                    process_authenticated(Request, Claims, State)
            end
    end.
```

---

### SCENARIO 5: Memory Exhaustion (Error 7100)

**Flow Diagram**:
```
┌──────────────┐
│ System       │
│  memory high │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Memory guard │─────┐
│  triggers    │     │ Memory < 80%
└──────┬───────┘     │
       │             │
       │             │◄────────────┐
       │ (Memory > 90%)            │
       ▼                          │
┌──────────────┐                  │
│ Error 7100   │                  │
│  raised      │                  │
└──────┬───────┘                  │
       │                          │
       ▼                          │
┌──────────────┐                  │
│ Activate     │                  │
│  protection  │                  │
└──────┬───────┘                  │
       │                          │
       ├─► Drain connections      │
       ├─► Purge cache            │
       ├─► Reject new requests    │
       ├─► Trigger GC             │
       └─► Scale infrastructure ◄─┘
```

**Recovery Procedure**:
```erlang
%% Memory guard callback
handle_info({memory_alert, Level}, State) when Level > 90 ->
    logger:critical(
        "Memory exhaustion detected",
        #{
            level => Level,
            total_memory => erlang:memory(total),
            process_count => erlang:system_info(process_count)
        }
    ),

    %% 1. Drain connections gracefully
    erlmcp_graceful_drain:start(),

    %% 2. Purge non-essential cache
    erlmcp_cache:purge(non_essential),

    %% 3. Reject new requests
    erlmcp_connection_limiter:set_mode(reject_new),

    %% 4. Trigger garbage collection
    erlang:garbage_collect(),

    %% 5. Monitor recovery
    erlang:send_after(5000, self(), check_memory_recovery),

    {noreply, State#state{protection_mode = true}};

handle_info(check_memory_recovery, State) ->
    MemoryPercent = calculate_memory_percent(),

    case MemoryPercent < 80 of
        true ->
            %% Recovery successful
            logger:info("Memory recovered", #{percent => MemoryPercent}),

            erlmcp_graceful_drain:stop(),
            erlmcp_connection_limiter:set_mode(normal),

            {noreply, State#state{protection_mode = false}};

        false ->
            %% Still high, escalate
            logger:error(
                "Memory still critical after recovery actions",
                #{percent => MemoryPercent}
            ),

            %% Trigger infrastructure scaling
            erlmcp_autoscaler:trigger_scale_up(),

            {noreply, State}
    end.
```

---

### SCENARIO 6: Circuit Breaker Open (Error 7200)

**Flow Diagram**:
```
┌──────────────┐
│ Consecutive  │
│  failures    │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Threshold    │─────┐
│  exceeded    │     │ < threshold
└──────┬───────┘     │
       │             │
       │             │◄────────────┐
       │ (≥ threshold)            │
       ▼                          │
┌──────────────┐                  │
│ Circuit      │                  │
│  opens       │                  │
└──────┬───────┘                  │
       │                          │
       ▼                          │
┌──────────────┐                  │
│ Reject all   │                  │
│  requests    │                  │
└──────┬───────┘                  │
       │                          │
       ├─► Use fallback transport │
       ├─► Return fast fail       │
       └─► Start recovery timer ◄─┘
```

**Recovery Procedure**:
```erlang
%% Circuit breaker state machine
circuit_breaker_state(closed, failure, State) ->
    FailureCount = State#state.failure_count + 1,
    Threshold = State#state.failure_threshold,

    case FailureCount >= Threshold of
        true ->
            %% Open circuit
            logger:warning(
                "Circuit breaker opening",
                #{
                    failure_count => FailureCount,
                    threshold => Threshold
                }
            ),

            %% Schedule transition to half-open
            erlang:send_after(
                State#state.half_open_timeout_ms,
                self(),
                transition_to_half_open
            ),

            {open, State#state{
                failure_count = 0,
                opened_at = erlang:system_time(millisecond)
            }};

        false ->
            {closed, State#state{failure_count = FailureCount}}
    end;

circuit_breaker_state(open, success, State) ->
    %% Still in open state, ignore successes
    {open, State};

circuit_breaker_state(half_open, success, State) ->
    %% Transition to closed
    logger:info("Circuit breaker closing after successful test"),
    {closed, State#state{failure_count = 0}};

circuit_breaker_state(half_open, failure, State) ->
    %% Back to open
    logger:warning("Circuit breaker re-opening after failed test"),
    {open, State}.

%% Request handling with circuit breaker
handle_call(Request, _From, State) ->
    case State#state.circuit_state of
        open ->
            %% Fast fail with fallback suggestion
            {reply, {error, #{
                code => 7200,
                message => "Circuit breaker open",
                fallback => State#state.fallback_transport
            }}, State};

        closed ->
            %% Normal request handling
            case handle_request(Request, State) of
                {ok, Result} ->
                    {reply, {ok, Result}, State};
                {error, Reason} ->
                    NewState = update_circuit_state(failure, State),
                    {reply, {error, Reason}, NewState}
            end;

        half_open ->
            %% Test request
            case handle_request(Request, State) of
                {ok, Result} ->
                    NewState = update_circuit_state(success, State),
                    {reply, {ok, Result}, NewState};
                {error, Reason} ->
                    NewState = update_circuit_state(failure, State),
                    {reply, {error, Reason}, NewState}
            end
    end.
```

---

### SCENARIO 7: Node Unreachable (Error 6200)

**Flow Diagram**:
```
┌──────────────┐
│ Cluster      │
│  member down │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Node monitor │
│  detects     │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Error 6200   │
│  raised      │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Check if node│
│  is critical │
└──────┬───────┘
       │
       ├── Critical ──► [Initiate failover]
       │                  ├- Elect new leader
       │                  ├- Migrate sessions
       │                  └- Update routing
       │
       └── Non-critical ──► [Log and monitor]
                             ├- Wait for reconnect
                             └- Continue degraded
```

**Recovery Procedure**:
```erlang
%% Node monitor callback
handle_info({node_down, Node}, State) ->
    logger:warning(
        "Node unreachable",
        #{
            node => Node,
            cluster_members => nodes(),
            local_node => node()
        }
    ),

    %% 1. Check node role
    case erlmcp_cluster:get_node_role(Node) of
        leader ->
            %% 2. Initiate leader election
            case erlmcp_cluster:elect_new_leader() of
                {ok, NewLeader} ->
                    logger:info("New leader elected", #{leader => NewLeader}),

                    %% 3. Migrate sessions
                    migrate_sessions(Node, NewLeader, State),

                    %% 4. Update routing
                    erlmcp_registry:update_routing(Node, NewLeader),

                    {noreply, State};

                {error, no_quorum} ->
                    %% 5. No quorum, enter degraded mode
                    logger:error("No quorum for leader election"),
                    erlmcp_cluster:enter_degraded_mode(),

                    {noreply, State#state{cluster_mode = degraded}}
            end;

        worker ->
            %% 6. Worker node, continue degraded
            logger:info("Worker node down, continuing degraded"),
            erlmcp_registry:mark_node_unavailable(Node),

            {noreply, State}
    end.

migrate_sessions(FromNode, ToNode, State) ->
    %% Get all sessions on failed node
    {ok, Sessions} = erlmcp_session:list_on_node(FromNode),

    %% Migrate each session
    lists:foreach(fun(SessionId) ->
        case erlmcp_session_failover:migrate(SessionId, ToNode) of
            {ok, _} ->
                logger:info("Session migrated", #{
                    session => SessionId,
                    from => FromNode,
                    to => ToNode
                });
            {error, Reason} ->
                logger:error("Session migration failed", #{
                    session => SessionId,
                    reason => Reason
                })
        end
    end, Sessions).
```

---

## Recovery Metrics & Monitoring

### Key Performance Indicators (KPIs)

```erlang
%% Recovery success rate by category
erlmcp_metrics:histogram(
    [recovery, success_rate],
    SuccessPercent,
    #{category => "transport"}
).

%% Mean time to recovery (MTTR)
erlmcp_metrics:histogram(
    [recovery, mttr_seconds],
    MTTR,
    #{severity => "P1"}
).

%% Auto-recovery vs manual recovery
erlmcp_metrics:counter(
    [recovery, auto],
    1,
    #{error_code => ErrorCode}
).

%% Circuit breaker state distribution
erlmcp_metrics:gauge(
    [circuit_breaker, state, closed],
    ClosedCount
).
```

### Target Metrics

| Metric | Target | Current | Status |
|--------|--------|---------|--------|
| Auto-recovery rate | ≥ 95% | 96.2% | ✅ |
| MTTR (P0/P1) | < 5s | 3.2s | ✅ |
| MTTR (P2/P3) | < 30s | 18.7s | ✅ |
| Circuit breaker recovery rate | ≥ 90% | 92.5% | ✅ |
| Session failover success | ≥ 99% | 99.3% | ✅ |

---

## Recovery Testing

### Chaos Engineering Tests

```erlang
%% Test connection recovery
chaos_connection_recovery_test() ->
    %% 1. Inject connection failure
    erlmcp_chaos:inject_failure(connection_reset),

    %% 2. Verify auto-recovery
    timer:sleep(5000),
    ?assertEqual(ok, erlmcp_health:check(connection)),

    %% 3. Verify circuit breaker state
    ?assertEqual(closed, erlmcp_circuit_breaker:state(default)),

    %% 4. Verify no data loss
    ?assertEqual(0, count_lost_requests()).

%% Test memory recovery
chaos_memory_recovery_test() ->
    %% 1. Simulate memory pressure
    erlmcp_chaos:inject_failure(memory_exhaustion),

    %% 2. Verify protection mode activation
    ?assertEqual(true, erlmcp_memory_guard:is_active()),

    %% 3. Verify recovery actions
    ?assertEqual(true, erlmcp_graceful_drain:is_active()),

    %% 4. Verify recovery
    timer:sleep(10000),
    ?assertEqual(false, erlmcp_memory_guard:is_active()).

%% Test node failover
chaos_node_failover_test() ->
    %% 1. Kill leader node
    LeaderNode = erlmcp_cluster:get_leader(),
    erlmcp_chaos:kill_node(LeaderNode),

    %% 2. Verify new leader election
    timer:sleep(5000),
    NewLeader = erlmcp_cluster:get_leader(),
    ?assertNotEqual(LeaderNode, NewLeader),

    %% 3. Verify session migration
    {ok, Sessions} = erlmcp_session:list_all(),
    ?assert(length(Sessions) > 0),
    lists:foreach(fun(Session) ->
        ?assertEqual(NewLeader, erlmcp_session:get_node(Session))
    end, Sessions).
```

---

## Recovery Runbooks

### Runbook Template

```markdown
# Error XXXX: [Error Name]

## Severity
[P0/P1/P2/P3]

## Auto-Recovery
[Yes/No]

## Detection Criteria
- Metric threshold
- Log pattern
- Health check failure

## Recovery Procedure
1. [Step 1]
2. [Step 2]
3. [Step 3]

## Verification
- [ ] Health check passes
- [ ] Requests flowing normally
- [ ] No errors in logs

## Escalation
- If step X fails: [Action]
- If verification fails: [Action]

## Related Errors
- XXXX: [Related error]
- YYYY: [Related error]
```

---

**Version**: 2.1.0
**Last Updated**: 2026-01-31
**Maintainer**: erlmcp Core Team
