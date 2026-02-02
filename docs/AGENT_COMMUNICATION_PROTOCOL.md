# Agent Communication Protocol Specification
**Version:** 1.0.0
**Status:** Protocol Specification
**Date:** 2026-02-02
**Based On:** Erlang/OTP Message Passing + GPROC + ETS

---

## Table of Contents

1. [Protocol Overview](#protocol-overview)
2. [Message Format Specification](#message-format-specification)
3. [Communication Patterns](#communication-patterns)
4. [Agent Discovery and Registration](#agent-discovery-and-registration)
5. [Work Order Messaging](#work-order-messaging)
6. [Quality Gate Coordination](#quality-gate-coordination)
7. [Event Broadcasting](#event-broadcasting)
8. [Error Propagation](#error-propagation)
9. [State Synchronization](#state-synchronization)
10. [Performance Characteristics](#performance-characteristics)

---

## Protocol Overview

### Design Principles

1. **Asynchronous by Default**: Non-blocking communication using casts
2. **Synchronous When Necessary**: Timeouts for critical operations
3. **Location Transparent**: Works across distributed Erlang nodes
4. **Fault Tolerant**: Monitors and automatic cleanup
5. **Low Latency**: Target <10ms message delivery
6. **High Throughput**: Support 10K+ messages/second

### Transport Layer

```erlang
%% Built on Erlang distribution protocol
%% Message passing: PID ! Message
%% RPC: gen_server:call/cast
%% Pub/Sub: gproc
%% Shared State: ETS
```

### Protocol Stack

```
┌─────────────────────────────────────┐
│   Application Layer                  │
│   (Work Orders, Quality Gates)      │
├─────────────────────────────────────┤
│   Coordination Layer                 │
│   (Events, Commands, Queries)       │
├─────────────────────────────────────┤
│   Communication Layer                │
│   (Message Passing, Pub/Sub)        │
├─────────────────────────────────────┤
│   Transport Layer                    │
│   (Erlang Distribution)              │
└─────────────────────────────────────┘
```

---

## Message Format Specification

### Base Message Format

```erlang
-type agent_message() :: #{
    message_id := message_id(),
    message_type := message_type(),
    sender := agent_address(),
    receiver := agent_address(),
    timestamp := erlang:timestamp(),
    payload := term(),
    metadata := map()
}.

-type message_id() :: binary().  % UUID
-type message_type() ::
    command | query | event | response | error.

-type agent_address() :: #{
    role := agent_role(),
    pid := pid(),
    node := node()
}.
```

### Command Messages

```erlang
%% Commands are requests to perform an action
-type command_message() :: #{
    message_type := command,
    command := command_type(),
    params := map(),
    reply_to := pid() | undefined,
    timeout := timeout()
}.

-type command_type() ::
    start_work_order |
    complete_work_order |
    cancel_work_order |
    run_quality_gate |
    handoff_work_order |
    trigger_andon |
    escalate_issue.

%% Example: Start work order command
#{
    message_id => <<"550e8400-e29b-41d4-a716-446655440000">>,
    message_type => command,
    command => start_work_order,
    sender => #{
        role => sparc_orchestrator,
        pid => "<0.123.0>",
        node => 'agent@localhost'
    },
    receiver => #{
        role => erlang_otp_developer,
        pid => "<0.456.0>",
        node => 'agent@localhost'
    },
    timestamp => {1738, 482234, 123456},
    params => #{
        work_order_id => <<"WO-001">>
    },
    reply_to => self(),
    timeout => 5000,
    metadata => #{
        priority => 7
    }
}
```

### Query Messages

```erlang
%% Queries are requests for information
-type query_message() :: #{
    message_type := query,
    query := query_type(),
    params := map(),
    reply_to := pid(),
    timeout := timeout()
}.

-type query_type() ::
    get_work_order |
    get_agent_status |
    get_queue |
    get_dependencies |
    check_quality_gate |
    get_metrics.

%% Example: Get work order query
#{
    message_id => <<"550e8400-e29b-41d4-a716-446655440001">>,
    message_type => query,
    query => get_work_order,
    sender => #{role => plan_designer, pid => self(), node => node()},
    receiver => #{role => work_order_manager, pid => undefined, node => node()},
    timestamp => {1738, 482234, 234567},
    params => #{
        work_order_id => <<"WO-001">>
    },
    reply_to => self(),
    timeout => 5000,
    metadata => #{}
}
```

### Event Messages

```erlang
%% Events are notifications of state changes
-type event_message() :: #{
    message_type := event,
    event := event_type(),
    data := map()
}.

-type event_type() ::
    work_order_created |
    work_order_started |
    work_order_completed |
    work_order_failed |
    quality_gate_passed |
    quality_gate_failed |
    andon_triggered |
    agent_status_changed.

%% Example: Work order completed event
#{
    message_id => <<"550e8400-e29b-41d4-a716-446655440002">>,
    message_type => event,
    event => work_order_completed,
    sender => #{role => erlang_otp_developer, pid => self(), node => node()},
    receiver => #{role => broadcast, pid => undefined, node => undefined},
    timestamp => {1738, 482234, 345678},
    data => #{
        work_order_id => <<"WO-001">>,
        sku_id => <<"sku-feature-123">>,
        lead_time_hours => 8.5,
        quality_gates_passed => true
    },
    metadata => #{
        bucket => features
    }
}
```

### Response Messages

```erlang
%% Responses to commands or queries
-type response_message() :: #{
    message_type := response,
    request_id := message_id(),
    status := ok | error,
    result := term() | undefined,
    error_reason := binary() | undefined
}.

%% Example: Successful response
#{
    message_id => <<"550e8400-e29b-41d4-a716-446655440003">>,
    message_type => response,
    request_id => <<"550e8400-e29b-41d4-a716-446655440001">>,
    sender => #{role => work_order_manager, pid => self(), node => node()},
    receiver => #{role => plan_designer, pid => "<0.789.0>", node => node()},
    timestamp => {1738, 482234, 456789},
    status => ok,
    result => #{
        id => <<"WO-001">>,
        title => <<"Implement subscription protocol">>,
        status => in_progress,
        assigned_agents => [erlang_otp_developer]
    },
    error_reason => undefined,
    metadata => #{}
}

%% Example: Error response
#{
    message_id => <<"550e8400-e29b-41d4-a716-446655440004">>,
    message_type => response,
    request_id => <<"550e8400-e29b-41d4-a716-446655440000">>,
    sender => #{role => erlang_otp_developer, pid => self(), node => node()},
    receiver => #{role => sparc_orchestrator, pid => "<0.123.0>", node => node()},
    timestamp => {1738, 482234, 567890},
    status => error,
    result => undefined,
    error_reason => <<"Work order blocked by dependencies">>,
    metadata => #{
        blocking_work_orders => [<<"WO-000">>]
    }
}
```

### Error Messages

```erlang
%% Error notifications
-type error_message() :: #{
    message_type := error,
    error_type := error_type(),
    error_details := map(),
    severity := critical | high | medium | low
}.

-type error_type() ::
    compilation_error |
    test_failure |
    quality_gate_failure |
    timeout |
    agent_crash |
    deadlock_detected.

%% Example: Compilation error
#{
    message_id => <<"550e8400-e29b-41d4-a716-446655440005">>,
    message_type => error,
    error_type => compilation_error,
    sender => #{role => agent_01_compile_gate, pid => self(), node => node()},
    receiver => #{role => broadcast, pid => undefined, node => undefined},
    timestamp => {1738, 482234, 678901},
    error_details => #{
        work_order_id => <<"WO-001">>,
        module => <<"erlmcp_subscription">>,
        line => 45,
        error => <<"undefined function subscribe/2">>
    },
    severity => high,
    metadata => #{
        trigger_andon => true
    }
}
```

---

## Communication Patterns

### 1. Fire-and-Forget (Cast)

```erlang
%% Asynchronous, no response expected
%% Use for: events, notifications, low-priority commands

-spec notify_event(Event :: event_message()) -> ok.
notify_event(Event) ->
    Recipients = get_event_subscribers(maps:get(event, Event)),
    lists:foreach(fun(Recipient) ->
        Recipient ! Event
    end, Recipients),
    ok.

%% Agent implementation
handle_info(Event = #{message_type := event}, State) ->
    case maps:get(event, Event) of
        work_order_completed ->
            handle_work_order_completed(Event, State);
        _ ->
            log_event(Event),
            {noreply, State}
    end.
```

### 2. Request-Response (Call)

```erlang
%% Synchronous with timeout
%% Use for: queries, critical commands

-spec request_work_order(WorkOrderId :: binary(), Timeout :: timeout()) ->
    {ok, work_order()} | {error, term()}.
request_work_order(WorkOrderId, Timeout) ->
    Request = #{
        message_id => generate_message_id(),
        message_type => query,
        query => get_work_order,
        sender => make_agent_address(self()),
        receiver => make_agent_address(work_order_manager),
        timestamp => erlang:timestamp(),
        params => #{work_order_id => WorkOrderId},
        reply_to => self(),
        timeout => Timeout,
        metadata => #{}
    },

    %% Send request
    {ok, ManagerPid} = find_agent(work_order_manager),
    ManagerPid ! Request,

    %% Wait for response
    receive
        Response = #{
            message_type := response,
            request_id := RequestId,
            status := ok
        } when RequestId =:= maps:get(message_id, Request) ->
            {ok, maps:get(result, Response)};
        Response = #{
            message_type := response,
            request_id := RequestId,
            status := error
        } when RequestId =:= maps:get(message_id, Request) ->
            {error, maps:get(error_reason, Response)}
    after Timeout ->
        {error, timeout}
    end.
```

### 3. Publish-Subscribe

```erlang
%% Topic-based broadcast
%% Use for: events, status updates, coordination

-spec subscribe_topic(Topic :: atom()) -> ok.
subscribe_topic(Topic) ->
    gproc:reg({p, l, {topic, Topic}}).

-spec publish_topic(Topic :: atom(), Message :: agent_message()) -> ok.
publish_topic(Topic, Message) ->
    gproc:send({p, l, {topic, Topic}}, Message).

%% Topics
-define(TOPICS, [
    work_order_events,
    quality_gate_events,
    agent_status_events,
    andon_alerts,
    system_events
]).

%% Example: Subscribe to work order events
subscribe_topic(work_order_events),

%% Agent receives all work order events
handle_info(Event = #{message_type := event}, State) ->
    process_work_order_event(Event, State).
```

### 4. Handoff Protocol

```erlang
%% Two-phase handoff between agents
%% Use for: SPARC phase transitions

-spec handoff_work_order(WorkOrder :: work_order(),
                        FromAgent :: agent_role(),
                        ToAgent :: agent_role()) ->
    {ok, accepted} | {error, rejected}.
handoff_work_order(WorkOrder, FromAgent, ToAgent) ->
    %% Phase 1: Prepare handoff
    PrepareMsg = #{
        message_id => generate_message_id(),
        message_type => command,
        command => prepare_handoff,
        sender => make_agent_address(FromAgent),
        receiver => make_agent_address(ToAgent),
        timestamp => erlang:timestamp(),
        params => #{
            work_order => WorkOrder,
            artifacts => get_artifacts(WorkOrder)
        },
        reply_to => self(),
        timeout => 5000,
        metadata => #{}
    },

    %% Send prepare
    {ok, ToAgentPid} = find_agent(ToAgent),
    ToAgentPid ! PrepareMsg,

    %% Wait for acknowledgment
    receive
        #{
            message_type := response,
            request_id := RequestId,
            status := ok
        } when RequestId =:= maps:get(message_id, PrepareMsg) ->
            %% Phase 2: Commit handoff
            commit_handoff(WorkOrder, FromAgent, ToAgent);
        #{
            message_type := response,
            request_id := RequestId,
            status := error,
            error_reason := Reason
        } when RequestId =:= maps:get(message_id, PrepareMsg) ->
            {error, {rejected, Reason}}
    after 5000 ->
        {error, timeout}
    end.

commit_handoff(WorkOrder, FromAgent, ToAgent) ->
    CommitMsg = #{
        message_id => generate_message_id(),
        message_type => command,
        command => commit_handoff,
        sender => make_agent_address(FromAgent),
        receiver => make_agent_address(ToAgent),
        timestamp => erlang:timestamp(),
        params => #{
            work_order_id => get_id(WorkOrder)
        },
        reply_to => self(),
        timeout => 5000,
        metadata => #{}
    },

    {ok, ToAgentPid} = find_agent(ToAgent),
    ToAgentPid ! CommitMsg,

    receive
        #{
            message_type := response,
            request_id := RequestId,
            status := ok
        } when RequestId =:= maps:get(message_id, CommitMsg) ->
            {ok, accepted}
    after 5000 ->
        {error, commit_timeout}
    end.
```

### 5. Consensus Protocol

```erlang
%% Multi-agent voting
%% Use for: design decisions, conflict resolution

-spec request_consensus(Decision :: design_decision(),
                       Voters :: [agent_role()],
                       Timeout :: timeout()) ->
    {consensus, Result} | {split, Votes}.
request_consensus(Decision, Voters, Timeout) ->
    RequestId = generate_message_id(),

    %% Send vote requests to all voters
    VoteRequests = lists:map(fun(Voter) ->
        #{
            message_id => RequestId,
            message_type => command,
            command => vote_on_decision,
            sender => make_agent_address(self()),
            receiver => make_agent_address(Voter),
            timestamp => erlang:timestamp(),
            params => #{
                decision => Decision,
                options => get_options(Decision)
            },
            reply_to => self(),
            timeout => Timeout,
            metadata => #{}
        }
    end, Voters),

    %% Send all requests
    lists:foreach(fun(Request) ->
        VoterRole = maps:get(role, maps:get(receiver, Request)),
        {ok, VoterPid} = find_agent(VoterRole),
        VoterPid ! Request
    end, VoteRequests),

    %% Collect votes
    Votes = collect_votes(RequestId, Voters, Timeout),

    %% Tally and decide
    tally_votes(Votes).

collect_votes(RequestId, Voters, Timeout) ->
    collect_votes_loop(RequestId, Voters, [], Timeout).

collect_votes_loop(_RequestId, [], Votes, _Timeout) ->
    Votes;
collect_votes_loop(RequestId, RemainingVoters, Votes, Timeout) ->
    receive
        #{
            message_type := response,
            request_id := Rid,
            status := ok,
            result := Vote
        } when Rid =:= RequestId ->
            VoterRole = maps:get(voter, Vote),
            collect_votes_loop(
                RequestId,
                lists:delete(VoterRole, RemainingVoters),
                [Vote | Votes],
                Timeout
            )
    after Timeout ->
        %% Timeout: missing votes count as abstain
        Abstained = [{V, abstain} || V <- RemainingVoters],
        Votes ++ Abstained
    end.
```

---

## Agent Discovery and Registration

### GPROC-Based Registry

```erlang
%% Register agent with role
-spec register_agent(Role :: agent_role()) -> ok.
register_agent(Role) ->
    %% Register with unique name
    gproc:reg({n, l, {agent, Role}}, self()),

    %% Also register for monitoring
    gproc:reg({p, l, {agent_monitor, Role}}),

    ok.

%% Find agent by role
-spec find_agent(Role :: agent_role()) ->
    {ok, pid()} | {error, not_found}.
find_agent(Role) ->
    case gproc:lookup_pid({n, l, {agent, Role}}) of
        undefined -> {error, not_found};
        Pid when is_pid(Pid) -> {ok, Pid}
    end.

%% List all agents
-spec list_agents() -> [{agent_role(), pid()}].
list_agents() ->
    Matches = gproc:select({l, n}, [{
        {{n, l, {agent, '$1'}}, '$2', '_'},
        [],
        [{{'$1', '$2'}}]
    }]),
    Matches.

%% Monitor agent
-spec monitor_agent(Role :: agent_role()) -> reference().
monitor_agent(Role) ->
    {ok, Pid} = find_agent(Role),
    erlang:monitor(process, Pid).
```

### Health Monitoring

```erlang
%% Agent heartbeat protocol
-spec send_heartbeat() -> ok.
send_heartbeat() ->
    Heartbeat = #{
        message_type => event,
        event => agent_heartbeat,
        sender => make_agent_address(self()),
        timestamp => erlang:timestamp(),
        data => #{
            status => get_agent_status(),
            current_work_order => get_current_work_order(),
            memory_mb => erlang:memory(total) div (1024*1024),
            message_queue_len => erlang:process_info(self(), message_queue_len)
        }
    },
    publish_topic(agent_status_events, Heartbeat).

%% Monitor heartbeats
-spec monitor_agent_health() -> ok.
monitor_agent_health() ->
    subscribe_topic(agent_status_events),
    monitor_loop().

monitor_loop() ->
    receive
        Heartbeat = #{event := agent_heartbeat} ->
            update_agent_health(Heartbeat),
            monitor_loop();
        {check_timeouts} ->
            check_agent_timeouts(),
            erlang:send_after(5000, self(), {check_timeouts}),
            monitor_loop()
    end.
```

---

## Work Order Messaging

### Work Order Lifecycle Events

```erlang
%% Event: Work order created
-spec notify_work_order_created(WorkOrder :: work_order()) -> ok.
notify_work_order_created(WorkOrder) ->
    Event = #{
        message_id => generate_message_id(),
        message_type => event,
        event => work_order_created,
        sender => make_agent_address(work_order_manager),
        timestamp => erlang:timestamp(),
        data => #{
            work_order_id => get_id(WorkOrder),
            bucket => get_bucket(WorkOrder),
            priority => get_priority(WorkOrder),
            assigned_agents => get_assigned_agents(WorkOrder)
        },
        metadata => #{}
    },
    publish_topic(work_order_events, Event).

%% Event: Work order started
-spec notify_work_order_started(WorkOrderId :: binary(),
                               Agent :: agent_role()) -> ok.
notify_work_order_started(WorkOrderId, Agent) ->
    Event = #{
        message_id => generate_message_id(),
        message_type => event,
        event => work_order_started,
        sender => make_agent_address(Agent),
        timestamp => erlang:timestamp(),
        data => #{
            work_order_id => WorkOrderId,
            started_by => Agent,
            started_at => calendar:universal_time()
        },
        metadata => #{}
    },
    publish_topic(work_order_events, Event).

%% Event: Work order progressed
-spec notify_work_order_progressed(WorkOrderId :: binary(),
                                  Stage :: stage()) -> ok.
notify_work_order_progressed(WorkOrderId, Stage) ->
    Event = #{
        message_id => generate_message_id(),
        message_type => event,
        event => work_order_progressed,
        sender => make_agent_address(self()),
        timestamp => erlang:timestamp(),
        data => #{
            work_order_id => WorkOrderId,
            new_stage => Stage,
            progressed_at => calendar:universal_time()
        },
        metadata => #{}
    },
    publish_topic(work_order_events, Event).

%% Event: Work order completed
-spec notify_work_order_completed(WorkOrderId :: binary(),
                                 SkuId :: binary()) -> ok.
notify_work_order_completed(WorkOrderId, SkuId) ->
    Event = #{
        message_id => generate_message_id(),
        message_type => event,
        event => work_order_completed,
        sender => make_agent_address(self()),
        timestamp => erlang:timestamp(),
        data => #{
            work_order_id => WorkOrderId,
            sku_id => SkuId,
            completed_at => calendar:universal_time(),
            lead_time_hours => calculate_lead_time(WorkOrderId)
        },
        metadata => #{}
    },
    publish_topic(work_order_events, Event).

%% Event: Work order blocked
-spec notify_work_order_blocked(WorkOrderId :: binary(),
                               BlockedBy :: [binary()]) -> ok.
notify_work_order_blocked(WorkOrderId, BlockedBy) ->
    Event = #{
        message_id => generate_message_id(),
        message_type => event,
        event => work_order_blocked,
        sender => make_agent_address(work_order_manager),
        timestamp => erlang:timestamp(),
        data => #{
            work_order_id => WorkOrderId,
            blocked_by => BlockedBy,
            blocked_at => calendar:universal_time()
        },
        metadata => #{}
    },
    publish_topic(work_order_events, Event).

%% Event: Work order failed
-spec notify_work_order_failed(WorkOrderId :: binary(),
                              Reason :: binary()) -> ok.
notify_work_order_failed(WorkOrderId, Reason) ->
    Event = #{
        message_id => generate_message_id(),
        message_type => event,
        event => work_order_failed,
        sender => make_agent_address(self()),
        timestamp => erlang:timestamp(),
        data => #{
            work_order_id => WorkOrderId,
            failure_reason => Reason,
            failed_at => calendar:universal_time()
        },
        metadata => #{
            trigger_andon => true,
            severity => high
        }
    },
    publish_topic(work_order_events, Event),
    publish_topic(andon_alerts, Event).
```

---

## Quality Gate Coordination

### Quality Gate Messages

```erlang
%% Command: Run quality gate
-spec run_quality_gate(WorkOrderId :: binary(),
                       Gate :: gate_name()) ->
    {ok, gate_result()} | {error, term()}.
run_quality_gate(WorkOrderId, Gate) ->
    Command = #{
        message_id => generate_message_id(),
        message_type => command,
        command => run_quality_gate,
        sender => make_agent_address(self()),
        receiver => make_agent_address(get_gate_agent(Gate)),
        timestamp => erlang:timestamp(),
        params => #{
            work_order_id => WorkOrderId,
            gate => Gate
        },
        reply_to => self(),
        timeout => get_gate_timeout(Gate),
        metadata => #{}
    },

    {ok, AgentPid} = find_agent(get_gate_agent(Gate)),
    AgentPid ! Command,

    receive
        #{
            message_type := response,
            request_id := Rid,
            status := ok,
            result := Result
        } when Rid =:= maps:get(message_id, Command) ->
            {ok, Result};
        #{
            message_type := response,
            request_id := Rid,
            status := error,
            error_reason := Reason
        } when Rid =:= maps:get(message_id, Command) ->
            {error, Reason}
    after get_gate_timeout(Gate) ->
        {error, timeout}
    end.

%% Event: Quality gate passed
-spec notify_quality_gate_passed(WorkOrderId :: binary(),
                                Gate :: gate_name(),
                                Details :: map()) -> ok.
notify_quality_gate_passed(WorkOrderId, Gate, Details) ->
    Event = #{
        message_id => generate_message_id(),
        message_type => event,
        event => quality_gate_passed,
        sender => make_agent_address(self()),
        timestamp => erlang:timestamp(),
        data => #{
            work_order_id => WorkOrderId,
            gate => Gate,
            details => Details
        },
        metadata => #{}
    },
    publish_topic(quality_gate_events, Event).

%% Event: Quality gate failed
-spec notify_quality_gate_failed(WorkOrderId :: binary(),
                                Gate :: gate_name(),
                                Reason :: binary(),
                                Details :: map()) -> ok.
notify_quality_gate_failed(WorkOrderId, Gate, Reason, Details) ->
    Event = #{
        message_id => generate_message_id(),
        message_type => event,
        event => quality_gate_failed,
        sender => make_agent_address(self()),
        timestamp => erlang:timestamp(),
        data => #{
            work_order_id => WorkOrderId,
            gate => Gate,
            failure_reason => Reason,
            details => Details
        },
        metadata => #{
            trigger_andon => is_blocking_gate(Gate),
            severity => gate_severity(Gate)
        }
    },
    publish_topic(quality_gate_events, Event),

    %% Trigger Andon if blocking gate
    case is_blocking_gate(Gate) of
        true -> publish_topic(andon_alerts, Event);
        false -> ok
    end.
```

---

## Event Broadcasting

### System Events

```erlang
%% System-wide events
-type system_event() ::
    agent_started |
    agent_stopped |
    agent_crashed |
    system_paused |
    system_resumed |
    andon_triggered |
    andon_resolved.

%% Broadcast system event
-spec broadcast_system_event(EventType :: system_event(),
                            Data :: map()) -> ok.
broadcast_system_event(EventType, Data) ->
    Event = #{
        message_id => generate_message_id(),
        message_type => event,
        event => EventType,
        sender => make_agent_address(system),
        timestamp => erlang:timestamp(),
        data => Data,
        metadata => #{}
    },
    publish_topic(system_events, Event).
```

---

## Error Propagation

### Error Notification Chain

```erlang
%% Propagate error up the chain
-spec propagate_error(Error :: error_message()) -> ok.
propagate_error(Error) ->
    %% Step 1: Log error
    log_error(Error),

    %% Step 2: Notify orchestrator
    notify_orchestrator(Error),

    %% Step 3: Broadcast to all agents (if severe)
    case maps:get(severity, Error) of
        critical ->
            publish_topic(system_events, Error),
            trigger_andon_stop_line(Error);
        high ->
            publish_topic(system_events, Error),
            trigger_andon_warning(Error);
        _ ->
            ok
    end,

    %% Step 4: Notify affected work orders
    WorkOrderId = maps:get(work_order_id, maps:get(error_details, Error), undefined),
    case WorkOrderId of
        undefined -> ok;
        _ -> notify_work_order_error(WorkOrderId, Error)
    end.
```

---

## State Synchronization

### ETS-Based Shared State

```erlang
%% Read work order (fast, concurrent)
-spec read_work_order(WorkOrderId :: binary()) ->
    {ok, work_order()} | {error, not_found}.
read_work_order(WorkOrderId) ->
    case ets:lookup(work_orders, WorkOrderId) of
        [{WorkOrderId, WorkOrder}] -> {ok, WorkOrder};
        [] -> {error, not_found}
    end.

%% Write work order (coordinated via gen_server)
-spec write_work_order(WorkOrder :: work_order()) -> ok.
write_work_order(WorkOrder) ->
    Command = #{
        message_id => generate_message_id(),
        message_type => command,
        command => update_work_order,
        sender => make_agent_address(self()),
        receiver => make_agent_address(work_order_manager),
        timestamp => erlang:timestamp(),
        params => #{work_order => WorkOrder},
        reply_to => self(),
        timeout => 5000,
        metadata => #{}
    },

    {ok, ManagerPid} = find_agent(work_order_manager),
    ManagerPid ! Command,

    receive
        #{message_type := response, status := ok} -> ok
    after 5000 ->
        {error, timeout}
    end.
```

---

## Performance Characteristics

### Latency Targets

```erlang
-define(LATENCY_TARGETS, #{
    message_delivery_ms => 10,
    ets_read_us => 10,
    gproc_lookup_us => 50,
    gen_server_call_ms => 100,
    event_broadcast_ms => 50
}).
```

### Throughput Targets

```erlang
-define(THROUGHPUT_TARGETS, #{
    messages_per_second => 10000,
    events_per_second => 5000,
    queries_per_second => 2000,
    commands_per_second => 1000
}).
```

### Message Size Limits

```erlang
-define(MESSAGE_SIZE_LIMITS, #{
    max_payload_kb => 100,
    max_metadata_kb => 10,
    max_event_data_kb => 50
}).
```

---

## Protocol Implementation Example

```erlang
-module(agent_protocol).
-behaviour(gen_server).

-export([
    start_link/1,
    send_command/4,
    send_query/4,
    publish_event/2,
    subscribe_topic/2
]).

-export([init/1, handle_call/3, handle_cast/3, handle_info/2]).

-record(state, {
    role :: agent_role(),
    subscriptions = [] :: [atom()],
    pending_requests = #{} :: #{message_id() => {pid(), reference()}}
}).

start_link(Role) ->
    gen_server:start_link(?MODULE, [Role], []).

init([Role]) ->
    %% Register agent
    register_agent(Role),

    {ok, #state{role = Role}}.

%% Send command (synchronous)
send_command(ToRole, Command, Params, Timeout) ->
    gen_server:call(?MODULE, {send_command, ToRole, Command, Params, Timeout}).

%% Send query (synchronous)
send_query(ToRole, Query, Params, Timeout) ->
    gen_server:call(?MODULE, {send_query, ToRole, Query, Params, Timeout}).

%% Publish event (asynchronous)
publish_event(Event, Data) ->
    gen_server:cast(?MODULE, {publish_event, Event, Data}).

%% Subscribe to topic
subscribe_topic(Topic, Pid) ->
    gen_server:call(?MODULE, {subscribe_topic, Topic, Pid}).

%% gen_server callbacks
handle_call({send_command, ToRole, Command, Params, Timeout}, From, State) ->
    Message = build_command_message(Command, ToRole, Params, From, Timeout, State),
    {ok, ToPid} = find_agent(ToRole),
    ToPid ! Message,

    %% Store pending request
    RequestId = maps:get(message_id, Message),
    MonitorRef = erlang:monitor(process, ToPid),
    NewPending = maps:put(RequestId, {From, MonitorRef}, State#state.pending_requests),

    {noreply, State#state{pending_requests = NewPending}}.

handle_info(Message = #{message_type := response, request_id := RequestId}, State) ->
    case maps:get(RequestId, State#state.pending_requests, undefined) of
        {From, MonitorRef} ->
            erlang:demonitor(MonitorRef, [flush]),
            gen_server:reply(From, extract_response(Message)),
            NewPending = maps:remove(RequestId, State#state.pending_requests),
            {noreply, State#state{pending_requests = NewPending}};
        undefined ->
            {noreply, State}
    end;

handle_info(Message = #{message_type := event}, State) ->
    handle_event(Message, State),
    {noreply, State}.

%% Helper functions
build_command_message(Command, ToRole, Params, From, Timeout, State) ->
    #{
        message_id => generate_message_id(),
        message_type => command,
        command => Command,
        sender => make_agent_address(State#state.role),
        receiver => make_agent_address(ToRole),
        timestamp => erlang:timestamp(),
        params => Params,
        reply_to => From,
        timeout => Timeout,
        metadata => #{}
    }.

extract_response(#{status := ok, result := Result}) ->
    {ok, Result};
extract_response(#{status := error, error_reason := Reason}) ->
    {error, Reason}.
```

---

## Conclusion

This agent communication protocol provides a robust, performant, and fault-tolerant foundation for agent coordination in the erlmcp MCP implementation. Built on Erlang/OTP primitives, it leverages proven distributed systems patterns while maintaining low latency and high throughput.

**Key Features**:
- Message-based coordination
- Multiple communication patterns (cast, call, pub/sub, handoff, consensus)
- Agent discovery via GPROC
- Event broadcasting for state synchronization
- Error propagation chains
- Performance targets: <10ms latency, 10K msg/sec throughput

**Status**: Ready for Implementation
