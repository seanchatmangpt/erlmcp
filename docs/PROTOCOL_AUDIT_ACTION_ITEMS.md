# Protocol Core Compliance Audit - Action Items

**Date:** 2026-01-27
**Status:** 94% Compliant (1 Blocking Issue, 2 Medium Issues)

---

## 🔴 BLOCKING ISSUE - Must Fix Before Production

### Issue #1: Initialization Timeout Not Enforced
**Gap:** Gap #4 - Initialization Phase State Machine
**Module:** `/Users/sac/erlmcp/src/erlmcp_server.erl`
**Severity:** HIGH
**Compliance Requirement:** MCP 2025-11-25 § 6.1 - 30-second initialization timeout

#### Current State
The timeout infrastructure exists but is not activated:
```erlang
-record(state, {
    init_timeout_ref :: reference() | undefined,  % DECLARED but not used
    init_timeout_ms = ?MCP_DEFAULT_INIT_TIMEOUT_MS :: pos_integer(),
    % ...
})
```

The timeout **is not triggered** anywhere in the codebase:
- ❌ `erlang:send_after/3` NOT called in init/1
- ❌ No `handle_info({init_timeout}, State)` message handler
- ❌ No timeout cancellation on successful initialization

#### Impact
- Clients can remain in `initialization` phase indefinitely
- Violates MCP protocol requirement
- Prevents server from detecting hung clients
- Could cause resource exhaustion in long-running servers

#### Required Implementation

**File:** `/Users/sac/erlmcp/src/erlmcp_server.erl`

**Change 1: Start timeout in init/1 (after line 186)**
```erlang
init([ServerId, Capabilities]) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.init">>, ServerId),
    try
        process_flag(trap_exit, true),

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"server_id">> => ServerId
        }),

        ok = erlmcp_task_manager:register_server(ServerId, self()),

        NotifierPid = case erlmcp_change_notifier:start_link() of
            {ok, Pid} -> Pid;
            {error, {already_started, Pid}} -> Pid
        end,

        % START TIMEOUT HERE
        InitTimeoutRef = erlang:send_after(
            ?MCP_DEFAULT_INIT_TIMEOUT_MS,
            self(),
            {init_timeout}
        ),

        State = #state{
            server_id = ServerId,
            capabilities = Capabilities,
            notifier_pid = NotifierPid,
            init_timeout_ref = InitTimeoutRef  % Store reference
        },

        logger:info("Starting MCP server ~p (refactored)", [ServerId]),
        erlmcp_tracing:set_status(SpanCtx, ok),
        {ok, State}
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            erlang:raise(Class, Reason, Stacktrace)
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.
```

**Change 2: Cancel timeout on successful initialization (in handle_request initialize handler, after line 461)**
```erlang
handle_request(Id, ?MCP_METHOD_INITIALIZE, Params, TransportId,
               #state{server_id = ServerId, initialized = false} = State) ->
    % ... existing initialization logic ...

    % After successful initialization:
    case State#state.init_timeout_ref of
        undefined -> ok;
        TimerRef ->
            erlang:cancel_timer(TimerRef),
            logger:debug("Initialization timeout cancelled for ~p", [ServerId])
    end,

    Response = build_initialize_response(State#state.capabilities),
    send_response_via_registry(State, TransportId, Id, Response),
    NewState = State#state{
        initialized = true,
        init_timeout_ref = undefined,  % Clear reference
        phase = ?MCP_PHASE_INITIALIZED
    },
    {noreply, NewState}.
```

**Change 3: Add timeout message handler (add new function after handle_info)**
```erlang
% Handle initialization timeout
handle_info({init_timeout}, #state{server_id = ServerId, initialized = false} = State) ->
    ErrorMsg = <<"Initialization timeout exceeded (30 seconds)">>,
    logger:error("Server ~p initialization timeout - shutting down", [ServerId]),
    erlmcp_tracing:record_event(<<"init_timeout">>, #{
        <<"server_id">> => ServerId,
        <<"message">> => ErrorMsg
    }),
    {stop, {shutdown, init_timeout}, State};

% Cancel timeout if initialization completes (optional safety handler)
handle_info({init_timeout}, #state{initialized = true} = State) ->
    {noreply, State};
```

#### Testing Requirements

**File:** `/Users/sac/erlmcp/test/erlmcp_phase_machine_tests.erl`

Add test (after line 200):
```erlang
server_init_timeout_fired_after_30_seconds_test() ->
    ServerId = setup(),
    try
        {ok, TransportId} = create_mock_transport(),

        % Start server and wait for timeout
        % (Don't send initialize request)

        % Wait 30+ seconds
        timer:sleep(31000),

        % Check server is shut down
        case erlang:is_process_alive(ServerId) of
            true ->
                ?assert(false);  % Server should be dead
            false ->
                ok  % Expected - timeout occurred
        end
    after
        catch erlmcp_server:stop(ServerId)
    end.

server_init_timeout_cancelled_on_initialize_test() ->
    ServerId = setup(),
    try
        {ok, TransportId} = create_mock_transport(),

        % Initialize before timeout fires
        InitRequest = erlmcp_json_rpc:encode_request(
            1,
            <<"initialize">>,
            #{<<"protocolVersion">> => <<"2025-11-25">>,
              <<"capabilities">> => #{}}
        ),
        erlmcp_registry:route_message(TransportId, ServerId, InitRequest),
        receive_message(TransportId, 100),

        % Wait for timeout period
        timer:sleep(31000),

        % Server should still be alive (timeout was cancelled)
        ?assert(erlang:is_process_alive(ServerId)),

        % Verify in initialized phase
        ?assertEqual(?MCP_PHASE_INITIALIZED, state_phase(ServerId))
    after
        teardown(ServerId)
    end.
```

#### Estimated Effort
- Implementation: 2-4 hours (including testing)
- Testing: 1-2 hours
- Code review: 1 hour
- **Total:** 4-7 hours

#### Success Criteria
- ✅ erlang:send_after called in init/1 with 30-second timeout
- ✅ handle_info handler defined for {init_timeout}
- ✅ Timer cancelled on successful initialization
- ✅ New tests pass (all 40+ existing tests still pass)
- ✅ Server logs timeout events
- ✅ No memory leaks from uncancelled timers

---

## 🟡 MEDIUM ISSUES - Should Fix Before Production

### Issue #2: List Changed Notifications Not Subscription-Based
**Gaps:** Gaps #25, #26, #27
**Modules:**
- `/Users/sac/erlmcp/src/erlmcp_change_notifier.erl`
- `/Users/sac/erlmcp/src/erlmcp_server.erl`
**Severity:** MEDIUM (architectural issue)

#### Current State
All clients receive all list_changed notifications regardless of subscription:

```erlang
notify_list_changed(Feature, State) ->
    try
        case erlmcp_change_notifier:is_feature_enabled(Feature) of
            true ->
                erlmcp_change_notifier:notify_list_changed(Feature)  % Global broadcast
        end
    catch _:_ -> ok
    end.
```

#### Impact
- **Efficiency:** All clients get all notifications even if not interested
- **Scalability:** Broadcast becomes bottleneck with 100+ clients
- **Protocol Intent:** MCP spec implies targeted notifications to subscribers

#### Recommended Solution

**Phase 1: Add subscription tracking**

New module: `/Users/sac/erlmcp/src/erlmcp_notification_subscriptions.erl`

```erlang
-module(erlmcp_notification_subscriptions).
-behavior(gen_server).

-export([start_link/0, subscribe/2, unsubscribe/2, get_subscribers/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {
    subscriptions = #{} :: #{binary() => sets:set(pid())}  % Method -> {Client PIDs}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

subscribe(Method, ClientPid) when is_binary(Method), is_pid(ClientPid) ->
    gen_server:call(?MODULE, {subscribe, Method, ClientPid}).

unsubscribe(Method, ClientPid) when is_binary(Method), is_pid(ClientPid) ->
    gen_server:call(?MODULE, {unsubscribe, Method, ClientPid}).

get_subscribers(Method) when is_binary(Method) ->
    gen_server:call(?MODULE, {get_subscribers, Method}).

init([]) ->
    {ok, #state{}}.

handle_call({subscribe, Method, ClientPid}, _From, State) ->
    OldSet = maps:get(Method, State#state.subscriptions, sets:new()),
    NewSet = sets:add_element(ClientPid, OldSet),
    NewSubs = maps:put(Method, NewSet, State#state.subscriptions),
    erlang:monitor(process, ClientPid),
    {reply, ok, State#state{subscriptions = NewSubs}};

handle_call({get_subscribers, Method}, _From, State) ->
    Subs = maps:get(Method, State#state.subscriptions, sets:new()),
    {reply, sets:to_list(Subs), State}.
```

**Phase 2: Update notification sending**

Modify `/Users/sac/erlmcp/src/erlmcp_server.erl`:

```erlang
-spec send_notification_to_subscribers(binary(), map()) -> ok.
send_notification_to_subscribers(Method, Params) ->
    Notification = erlmcp_json_rpc:encode_notification(Method, Params),
    case erlmcp_notification_subscriptions:get_subscribers(Method) of
        [] ->
            logger:debug("No subscribers for ~p", [Method]);
        Subscribers ->
            lists:foreach(fun(ClientPid) ->
                erlmcp_registry:send_to_client(ClientPid, Notification)
            end, Subscribers)
    end.
```

#### Estimated Effort
- Design: 1-2 hours
- Implementation: 4-6 hours
- Testing: 2-3 hours
- **Total:** 7-11 hours

#### Success Criteria
- ✅ Subscription tracking module created
- ✅ Clients can subscribe to specific notifications
- ✅ Only subscribed clients receive notifications
- ✅ Tests verify subscription filtering works
- ✅ Performance improved with 100+ clients
- ✅ Memory usage reduced for large deployments

---

### Issue #3: No Per-Client Notification Delivery Guarantee
**Gaps:** Gaps #25, #26, #27
**Module:** `/Users/sac/erlmcp/src/erlmcp_server.erl`
**Severity:** MEDIUM (operational concern)

#### Current State
Notifications use fire-and-forget semantics:
- No acknowledgment required
- No retry on failure
- No persistence
- No delivery guarantee

#### Impact
- Client may miss notifications due to network hiccup
- No recovery mechanism
- Client list becomes stale

#### Recommended Solution

**Add optional delivery guarantee option:**

1. **Persistent Notification Queue**
   - Store notifications in ETS table
   - Clients pull recent notifications on reconnect

2. **Acknowledgment Mechanism**
   - Clients send ACK after receiving notification
   - Server tracks last ACK point

3. **Catch-up Logic**
   - On reconnect, send all missed notifications since last ACK

#### Estimated Effort
- Implementation: 3-4 hours
- Testing: 2-3 hours
- **Total:** 5-7 hours

---

## ✅ LOW PRIORITY ITEMS - Consider for Future Versions

### Enhancement #1: Add Notification Metrics
- Track notifications sent per type
- Monitor subscriber count per notification
- Measure notification latency
- Effort: 2-3 hours

### Enhancement #2: Add Notification Logging
- Log all notifications sent/received
- Debug notification flow issues
- Audit trail for compliance
- Effort: 1-2 hours

### Enhancement #3: Documentation
- Add notification architecture diagram
- Document subscription flow
- Create operator runbook
- Effort: 2-3 hours

---

## Implementation Priority

### Week 1 (MUST DO)
1. **Implement initialization timeout** (4-7 hours)
   - Add erlang:send_after in init/1
   - Add timeout handler
   - Add tests
   - Verify all tests pass

### Week 2 (SHOULD DO)
1. **Implement subscription-based notifications** (7-11 hours)
   - Create erlmcp_notification_subscriptions module
   - Update notification sending
   - Add subscription API
   - Add tests

### Week 3 (NICE TO HAVE)
1. **Add delivery guarantee** (5-7 hours)
2. **Add metrics/logging** (3-5 hours)
3. **Improve documentation** (2-3 hours)

---

## Testing Checklist

Before declaring production-ready:

- [ ] All 40+ phase machine tests pass
- [ ] All 52+ error response tests pass
- [ ] All 13+ batch request tests pass
- [ ] All 350+ list change notification tests pass
- [ ] All 25+ capability tests pass
- [ ] **NEW:** Initialization timeout test passes
- [ ] Load test: 100+ concurrent clients
- [ ] Chaos test: Network failures during initialization
- [ ] Timeout test: Server correctly timeout hung clients
- [ ] Performance test: Notification latency < 100ms (p99)

---

## Deployment Checklist

Before production deployment:

- [ ] All compliance issues fixed and tested
- [ ] Code review completed
- [ ] Load testing passed (100+ clients)
- [ ] Chaos testing passed
- [ ] Performance benchmarks acceptable
- [ ] Documentation updated
- [ ] Runbook created
- [ ] Monitoring/alerting configured
- [ ] Rollback plan documented
- [ ] Stakeholder sign-off obtained

---

## Contact & Escalation

**For questions on this audit:**
- Audit report: `/Users/sac/erlmcp/docs/PROTOCOL_CORE_COMPLIANCE_AUDIT.md`
- Action items: This file
- Questions: Escalate to Agent 1 or MCP compliance team

**Critical Issues:** Escalate immediately to DevOps/SRE

---

**Report Generated:** 2026-01-27
**Next Review:** Upon timeout implementation completion
