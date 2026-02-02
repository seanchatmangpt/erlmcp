# OTP Patterns in erlmcp v2.1.0

## Visual Documentation

This document is enhanced with comprehensive Mermaid diagrams showing OTP patterns and supervision architecture.

**Key Diagrams:**
| Diagram | Location | Description |
|---------|----------|-------------|
| **Supervision Tree** | [`diagrams/supervision-tree.mmd`](diagrams/supervision-tree.mmd) | Complete 3-tier supervision hierarchy |
| **Session Lifecycle** | [`diagrams/protocol/session-lifecycle.mmd`](diagrams/protocol/session-lifecycle.mmd) | State machine for sessions |
| **Data Flow** | [`diagrams/data-flow.mmd`](diagrams/data-flow.mmd) | Message flow through system |

## Supervision Trees

### Complete Supervision Hierarchy (Visual)

```mermaid
graph TB
    subgraph "erlmcp Supervision Hierarchy - 3-Tier Invariant"
        direction TB

        subgraph "TIER 1: Application Supervisors (one_for_all)"
            direction TB

            ERLMCP_SUP[erlmcp_sup<br/>Root Supervisor<br/>one_for_all]

            subgraph "Core Application"
                CORE_APP[erlmcp_app<br/>Application Callback]
                CORE_SUP[erlmcp_core_sup<br/>Core Supervisor<br/>one_for_all]
            end

            subgraph "Transports Application"
                TRANS_APP[erlmcp_transports_app<br/>Application Callback]
                TRANS_SUP[erlmcp_transport_sup<br/>Transports Supervisor<br/>one_for_one]
            end

            subgraph "Observability Application"
                OBS_APP[erlmcp_observability_app<br/>Application Callback]
                OBS_SUP[erlmcp_observability_sup<br/>Observability Supervisor<br/>one_for_one]
            end

            subgraph "Validation Application"
                VAL_APP[erlmcp_validation_app<br/>Application Callback]
                VAL_SUP[erlmcp_validation_sup<br/>Validation Supervisor<br/>one_for_one]
            end
        end

        subgraph "TIER 2: Service Supervisors (simple_one_for_one)"
            direction TB

            %% Core Supervisors
            SERVER_SUP[erlmcp_server_sup<br/>Server Supervisor<br/>simple_one_for_one<br/>Process-per-Connection]
            CLIENT_SUP[erlmcp_client_sup<br/>Client Supervisor<br/>simple_one_for_one<br/>Process-per-Connection]
            SESSION_MGR[erlmcp_session_manager<br/>Session Manager<br/>simple_one_for_one<br/>Per-Session Isolation]

            %% Observability Supervisors
            CHAOS_SUP[erlmcp_chaos_worker_sup<br/>Chaos Worker Supervisor<br/>simple_one_for_one<br/>Failure Injection]

            %% Support Supervisors
            CACHE_WARMER_SUP[erlmcp_cache_warmer_sup<br/>Cache Warmer Supervisor<br/>one_for_one]
            NOTIFICATION_HANDLER_SUP[erlmcp_notification_handler_sup<br/>Notification Handler Supervisor<br/>one_for_one]
            FAILOVER_SUP[erlmcp_failover_worker_sup<br/>Failover Worker Supervisor<br/>simple_one_for_one]
            RELOAD_SUP[erlmcp_reload_sup<br/>Code Reload Supervisor<br/>one_for_one]
            CLUSTER_SUP[erlmcp_cluster_sup<br/>Cluster Supervisor<br/>one_for_one]
        end

        subgraph "TIER 3: Isolated Workers (temporary supervision)"
            direction TB

            %% Server Workers (isolated per connection)
            SERVER_WORKER1[erlmcp_server<br/>Server Process #1<br/>Isolated State]
            SERVER_WORKER2[erlmcp_server<br/>Server Process #N<br/>Isolated State]
            SERVER_WORKER_DOT[..]

            %% Client Workers (isolated per connection)
            CLIENT_WORKER1[erlmcp_client<br/>Client Process #1<br/>Request Correlation]
            CLIENT_WORKER2[erlmcp_client<br/>Client Process #N<br/>Request Correlation]
            CLIENT_WORKER_DOT[..]

            %% Session Workers
            SESSION_WORKER1[erlmcp_session<br/>Session Process #1<br/>State Management]
            SESSION_WORKER2[erlmcp_session<br/>Session Process #N<br/>State Management]
            SESSION_WORKER_DOT[..]

            %% Chaos Workers
            CHAOS_WORKER1[erlmcp_chaos_worker<br/>Chaos Worker #1<br/>Failure Injection]
            CHAOS_WORKER2[erlmcp_chaos_worker<br/>Chaos Worker #N<br/>Failure Injection]
            CHAOS_WORKER_DOT[..]

            %% Failover Workers
            FAILOVER_WORKER1[erlmcp_failover_worker<br/>Failover Worker #1<br/>Recovery Coordination]
            FAILOVER_WORKER2[erlmcp_failover_worker<br/>Failover Worker #N<br/>Recovery Coordination]
            FAILOVER_WORKER_DOT[..]
        end

        subgraph "Standalone Processes (monitored, not linked)"
            direction TB

            REGISTRY[erlmcp_registry<br/>gproc Registry<br/>Standalone gen_server]
            REGISTRY_DIST[erlmcp_registry_dist<br/>Distributed Registry<br/>Standalone gen_server]

            HEALTH_MONITOR[erlmcp_health_monitor<br/>Health Monitor<br/>Standalone gen_server]
            METRICS_SERVER[erlmcp_metrics_server<br/>Metrics HTTP Server<br/>Standalone gen_server]
            DASHBOARD_SERVER[erlmcp_dashboard_server<br/>Dashboard Web Server<br/>Standalone gen_server]

            CIRCUIT_BREAKER[erlmcp_circuit_breaker<br/>Circuit Breaker<br/>Standalone gen_server]
            RATE_LIMITER[erlmcp_rate_limiter<br/>Rate Limiter<br/>Standalone gen_server]
            CONNECTION_LIMITER[erlmcp_connection_limiter<br/>Connection Limiter<br/>Standalone gen_server]

            AUTH[erlmcp_auth<br/>Auth Service<br/>Standalone gen_server]
            SECRETS[erlmcp_secrets<br/>Secrets Manager<br/>Standalone gen_server]

            OTEL[erlmcp_otel<br/>OpenTelemetry<br/>Standalone gen_server]
            CHAOS_COORD[erlmcp_chaos<br/>Chaos Coordinator<br/>Standalone gen_server]
            RECOVERY_MGR[erlmcp_recovery_manager<br/>Recovery Manager<br/>Standalone gen_server]

            CHANGE_NOTIFIER[erlmcp_change_notifier<br/>Change Notifier<br/>Standalone gen_server]
            PROMPT_LIST_NOTIFIER[erlmcp_prompt_list_change_notifier<br/>Prompt List Notifier<br/>Standalone gen_server]

            MEMORY_GUARD[erlmcp_memory_guard<br/>Memory Guard<br/>Standalone gen_server]
            CPU_GUARD[erlmcp_cpu_guard<br/>CPU Guard<br/>Standalone gen_server]

            NODE_MONITOR[erlmcp_node_monitor<br/>Node Monitor<br/>Standalone gen_server]
            SPLIT_BRAIN[erlmcp_split_brain_detector<br/>Split-Brain Detector<br/>Standalone gen_server]

            DEBUGGER[erlmcp_debugger<br/>Debugger<br/>Standalone gen_server]
            PROFILER[erlmcp_profiler<br/>Profiler<br/>Standalone gen_server]
        end

        %% TIER 1 Relationships
        ERLMCP_SUP --> CORE_SUP
        ERLMCP_SUP --> TRANS_SUP
        ERLMCP_SUP --> OBS_SUP
        ERLMCP_SUP --> VAL_SUP

        CORE_APP -.-> CORE_SUP
        TRANS_APP -.-> TRANS_SUP
        OBS_APP -.-> OBS_SUP
        VAL_APP -.-> VAL_SUP

        %% TIER 1 to TIER 2 Relationships
        CORE_SUP --> SERVER_SUP
        CORE_SUP --> CLIENT_SUP
        CORE_SUP --> SESSION_MGR
        CORE_SUP --> CACHE_WARMER_SUP
        CORE_SUP --> NOTIFICATION_HANDLER_SUP
        CORE_SUP --> FAILOVER_SUP
        CORE_SUP --> RELOAD_SUP
        CORE_SUP --> CLUSTER_SUP

        OBS_SUP --> CHAOS_SUP

        %% TIER 2 to TIER 3 Relationships
        SERVER_SUP --> SERVER_WORKER1
        SERVER_SUP --> SERVER_WORKER2
        SERVER_SUP --> SERVER_WORKER_DOT

        CLIENT_SUP --> CLIENT_WORKER1
        CLIENT_SUP --> CLIENT_WORKER2
        CLIENT_SUP --> CLIENT_WORKER_DOT

        SESSION_MGR --> SESSION_WORKER1
        SESSION_MGR --> SESSION_WORKER2
        SESSION_MGR --> SESSION_WORKER_DOT

        CHAOS_SUP --> CHAOS_WORKER1
        CHAOS_SUP --> CHAOS_WORKER2
        CHAOS_SUP --> CHAOS_WORKER_DOT

        FAILOVER_SUP --> FAILOVER_WORKER1
        FAILOVER_SUP --> FAILOVER_WORKER2
        FAILOVER_SUP --> FAILOVER_WORKER_DOT

        %% Monitoring Relationships (monitored, not linked)
        CORE_SUP -.monitor. REGISTRY
        CORE_SUP -.monitor. REGISTRY_DIST
        CORE_SUP -.monitor. HEALTH_MONITOR
        CORE_SUP -.monitor. AUTH
        CORE_SUP -.monitor. SECRETS

        OBS_SUP -.monitor. METRICS_SERVER
        OBS_SUP -.monitor. DASHBOARD_SERVER
        OBS_SUP -.monitor. OTEL
        OBS_SUP -.monitor. CHAOS_COORD
        OBS_SUP -.monitor. RECOVERY_MGR

        CORE_SUP -.monitor. CIRCUIT_BREAKER
        CORE_SUP -.monitor. RATE_LIMITER
        CORE_SUP -.monitor. CONNECTION_LIMITER
        CORE_SUP -.monitor. CHANGE_NOTIFIER
        CORE_SUP -.monitor. PROMPT_LIST_NOTIFIER
        CORE_SUP -.monitor. MEMORY_GUARD
        CORE_SUP -.monitor. CPU_GUARD
        CORE_SUP -.monitor. NODE_MONITOR
        CORE_SUP -.monitor. SPLIT_BRAIN
        CORE_SUP -.monitor. DEBUGGER
        CORE_SUP -.monitor. PROFILER
    end

    classDef tier1Style fill:#ffebee,stroke:#c62828,stroke-width:3px
    classDef tier2Style fill:#fff9c4,stroke:#f57f17,stroke-width:2px
    classDef tier3Style fill:#e1f5fe,stroke:#0277bd,stroke-width:2px
    classDef standaloneStyle fill:#f3e5f5,stroke:#6a1b9a,stroke-width:2px,stroke-dasharray: 5 5

    class ERLMCP_SUP,CORE_APP,CORE_SUP,TRANS_APP,TRANS_SUP,OBS_APP,OBS_SUP,VAL_APP,VAL_SUP tier1Style
    class SERVER_SUP,CLIENT_SUP,SESSION_MGR,CHAOS_SUP,CACHE_WARMER_SUP,NOTIFICATION_HANDLER_SUP,FAILOVER_SUP,RELOAD_SUP,CLUSTER_SUP tier2Style
    class SERVER_WORKER1,SERVER_WORKER2,SERVER_WORKER_DOT,CLIENT_WORKER1,CLIENT_WORKER2,CLIENT_WORKER_DOT,SESSION_WORKER1,SESSION_WORKER2,SESSION_WORKER_DOT,CHAOS_WORKER1,CHAOS_WORKER2,CHAOS_WORKER_DOT,FAILOVER_WORKER1,FAILOVER_WORKER2,FAILOVER_WORKER_DOT tier3Style
    class REGISTRY,REGISTRY_DIST,HEALTH_MONITOR,METRICS_SERVER,DASHBOARD_SERVER,CIRCUIT_BREAKER,RATE_LIMITER,CONNECTION_LIMITER,AUTH,SECRETS,OTEL,CHAOS_COORD,RECOVERY_MGR,CHANGE_NOTIFIER,PROMPT_LIST_NOTIFIER,MEMORY_GUARD,CPU_GUARD,NODE_MONITOR,SPLIT_BRAIN,DEBUGGER,PROFILER standaloneStyle

    %% Legend
        subgraph "Legend"
            LEGEND_TIER1[TIER 1: Application Supervisors<br/>one_for_all strategy]
            LEGEND_TIER2[TIER 2: Service Supervisors<br/>simple_one_for_one strategy]
            LEGEND_TIER3[TIER 3: Isolated Workers<br/>Process-per-Connection]
            LEGEND_STANDALONE[Standalone Processes<br/>Monitored, not linked]
        end

        class LEGEND_TIER1 tier1Style
        class LEGEND_TIER2 tier2Style
        class LEGEND_TIER3 tier3Style
        class LEGEND_STANDALONE standaloneStyle
```

**See also:** [`supervision-tree.mmd`](diagrams/supervision-tree.mmd) for the complete diagram with all supervisors and workers.

### Application Structure

```
erlmcp_app
└── erlmcp_sup (one_for_one)
    ├── erlmcp_client_sup (simple_one_for_one)
    │   └── erlmcp_client workers
    └── erlmcp_server_sup (simple_one_for_one)
        └── erlmcp_server workers
```

### Restart Strategies

**Main Supervisor** - `one_for_one`
- Isolated failures
- Independent services

**Client/Server Supervisors** - `simple_one_for_one`
- Dynamic children
- Uniform worker type

### 3-Tier Supervision Invariant

**TIER 1: Application Supervisors** (`one_for_all`)
- Root supervision for each application
- Registry failure restarts entire application
- Ensures core dependencies are available

**TIER 2: Service Supervisors** (`simple_one_for_one`)
- Dynamic worker pools
- Process-per-connection isolation
- Independent restarts

**TIER 3: Isolated Workers** (temporary supervision)
- Per-connection state
- Failures isolated to single connection
- No cascading restarts

## gen_server Pattern

### Client Implementation
```erlang
%% Synchronous call with timeout
handle_call({calculate, Expression}, From, State) ->
    %% Send async request
    RequestId = send_request(Expression, State),
    %% Store pending request
    NewState = store_pending(RequestId, From, State),
    %% Don't reply yet
    {noreply, NewState}.

%% Handle async response
handle_info({response, RequestId, Result}, State) ->
    case take_pending(RequestId, State) of
        {ok, From, NewState} ->
            gen_server:reply(From, Result),
            {noreply, NewState};
        error ->
            {noreply, State}
    end.
```

### Server Implementation
```erlang
%% Resource management
handle_call({add_resource, Uri, Handler}, _From, State) ->
    NewState = State#state{
        resources = maps:put(Uri, Handler, State#state.resources)
    },
    {reply, ok, NewState}.

%% Async notifications
handle_cast({notify_subscribers, Event}, State) ->
    spawn(fun() -> 
        notify_all(State#state.subscribers, Event) 
    end),
    {noreply, State}.
```

## gen_statem Pattern

### Connection State Machine
```erlang
%% State definitions
-type state() :: disconnected | connecting | connected | ready.

%% State callback
disconnected({call, From}, connect, Data) ->
    {next_state, connecting, Data, 
     [{reply, From, ok}, {state_timeout, 5000, connect}]}.

connecting(state_timeout, connect, Data) ->
    case try_connect(Data) of
        {ok, Socket} ->
            {next_state, connected, Data#data{socket = Socket}};
        {error, _} ->
            {next_state, disconnected, Data,
             [{state_timeout, backoff(Data), retry}]}
    end.
```

## Process Design Patterns

### 1. Request-Response Correlation
```erlang
-record(state, {
    request_id = 1 :: integer(),
    pending = #{} :: #{integer() => {pid(), reference()}}
}).

send_request(Method, Params, #state{request_id = Id} = State) ->
    %% Send with correlation ID
    send_message(#{id => Id, method => Method, params => Params}),
    %% Track pending request
    Ref = make_ref(),
    NewPending = maps:put(Id, {self(), Ref}, State#state.pending),
    State#state{request_id = Id + 1, pending = NewPending}.
```

### 2. Subscription Management
```erlang
%% Using sets for efficient lookups
-record(state, {
    subscriptions = #{} :: #{binary() => sets:set(pid())}
}).

subscribe(Uri, Pid, State) ->
    Subs = maps:get(Uri, State#state.subscriptions, sets:new()),
    NewSubs = sets:add_element(Pid, Subs),
    State#state{
        subscriptions = maps:put(Uri, NewSubs, State#state.subscriptions)
    }.
```

### 3. Process Monitoring
```erlang
init(Args) ->
    process_flag(trap_exit, true),
    {ok, initialize_state(Args)}.

handle_info({'EXIT', Pid, Reason}, State) ->
    %% Clean up after crashed process
    NewState = cleanup_process(Pid, State),
    {noreply, NewState}.
```

## Error Handling Patterns

### 1. Let It Crash
```erlang
%% Don't defend against impossible cases
handle_call({get_resource, Uri}, _From, State) ->
    %% Let it crash if resource doesn't exist
    #{Uri := Handler} = State#state.resources,
    Result = Handler(Uri),
    {reply, Result, State}.
```

### 2. Graceful Degradation
```erlang
%% Client continues working despite errors
handle_info({tcp_closed, Socket}, State) ->
    %% Log and reconnect
    logger:warning("Connection lost, attempting reconnect"),
    {next_state, connecting, State#state{socket = undefined}}.
```

### 3. Circuit Breaker
```erlang
-record(breaker, {
    state = closed :: closed | open | half_open,
    failures = 0 :: integer(),
    threshold = 5 :: integer(),
    timeout :: reference()
}).

call_with_breaker(Fun, Breaker) ->
    case Breaker#breaker.state of
        open ->
            {error, circuit_open};
        _ ->
            try Fun() of
                Result ->
                    {Result, reset_breaker(Breaker)}
            catch
                _:_ ->
                    {error, trip_breaker(Breaker)}
            end
    end.
```

## Performance Patterns

### 1. Selective Receive
```erlang
%% Prioritize important messages
handle_info(Info, State) ->
    receive
        {priority, Msg} ->
            handle_priority(Msg, State)
    after 0 ->
        handle_normal(Info, State)
    end.
```

### 2. Batch Processing
```erlang
%% Accumulate requests
handle_cast({request, Req}, #state{batch = Batch} = State) ->
    NewBatch = [Req | Batch],
    case length(NewBatch) >= 100 of
        true ->
            process_batch(NewBatch),
            {noreply, State#state{batch = []}};
        false ->
            {noreply, State#state{batch = NewBatch}}
    end.
```

### 3. ETS for Shared State
```erlang
%% Read-heavy workload optimization
init(Name) ->
    Tab = ets:new(Name, [named_table, public, {read_concurrency, true}]),
    {ok, #state{table = Tab}}.

%% Readers access ETS directly
lookup(Name, Key) ->
    case ets:lookup(Name, Key) of
        [{_, Value}] -> {ok, Value};
        [] -> {error, not_found}
    end.
```

## Memory Guard Patterns (OTP 28)

### 1. Process Memory Limiting

```erlang
%% Enable memory guard for long-lived processes
init([]) ->
    erlmcp_memory_guard:enable_context_guard(),
    {ok, #state{}}.
```

**Purpose**: Prevent unbounded memory growth in context processes.

**OTP 28 Process Flags**:
- `min_heap_size/1`: Prevents excessive GC cycles
- `max_heap_size/1`: Triggers GC or kills process
- `min_bin_vheap_size/1`: Binary heap minimum
- `max_bin_vheap_size/1`: Binary heap maximum

### 2. Per-Process-Type Limits

```erlang
%% Context processes (100MB heap, 50MB binary)
erlmcp_memory_guard:enable_context_guard().

%% Tool processes (50MB heap, 25MB binary)
erlmcp_memory_guard:enable_tool_guard().

%% Transport processes (30MB heap, 15MB binary)
erlmcp_memory_guard:enable_transport_guard().
```

**Default Limits**:
| Type | Max Heap | Max Bin Heap | Hibernate Threshold |
|------|----------|--------------|---------------------|
| context | 100MB | 50MB | 90% |
| tool | 50MB | 25MB | 85% |
| transport | 30MB | 15MB | 80% |
| generic | 20MB | 10MB | 80% |

### 3. Memory Validation and Hibernation

```erlang
handle_call(Request, _From, State) ->
    %% Check memory before expensive operations
    case erlmcp_memory_guard:validate_memory(context) of
        {ok, _} ->
            %% Process normally
            Result = do_expensive_work(Request),
            {reply, Result, State};
        {warning, _} ->
            %% Approaching limit, hibernate after work
            Result = do_expensive_work(Request),
            {reply, Result, State, hibernate};
        {error, _} ->
            %% Over limit, reject request
            {reply, {error, memory_limit_exceeded}, State}
    end.
```

### 4. Memory Monitoring Integration

```erlang
%% Register process for monitoring
init([SessionId]) ->
    erlmcp_memory_guard:enable_context_guard(),
    ok = erlmcp_memory_monitor:register_process(self(), context),
    {ok, #state{session_id = SessionId}}.

%% Handle hibernation requests
handle_info({hibernate_now, _Monitor}, State) ->
    logger:info("Hibernating due to memory pressure"),
    {noreply, State, hibernate}.
```

**Memory Monitor Features**:
- Periodic memory checks (default: 5 seconds)
- Memory leak detection (growth pattern analysis)
- Automatic hibernation recommendations
- OTEL integration for observability

### 5. Custom Memory Limits

```erlang
%% Define custom process type with specific limits
-spec enable_custom_guard() -> ok.
enable_custom_guard() ->
    %% 75MB heap, 30MB binary heap
    ok = erlmcp_memory_guard:configure_limits(75_000_000, 30_000_000),
    ok.
```

### 6. Memory Usage Tracking

```erlang
%% Track memory usage over time
track_memory_usage() ->
    {Heap, BinHeap} = erlmcp_memory_guard:get_memory_usage(),
    logger:info("Memory: heap=~p, binary=~p", [Heap, BinHeap]),
    
    %% Emit OTEL metric
    erlmcp_otel:add_event(current_span(), <<"memory.usage">>,
        #{<<"heap">> => Heap, <<"binary">> => BinHeap}).
```

**Best Practices**:
1. **Enable early**: Call `enable_*_guard/0` in `init/1`
2. **Validate periodically**: Check memory before expensive operations
3. **Hibernate proactively**: Force hibernation at 80-90% threshold
4. **Monitor continuously**: Register with `erlmcp_memory_monitor`
5. **Set appropriate limits**: Match limits to process role and lifetime

**See Also**: [MEMORY_GUARD_LIMITS_OTP28.md](MEMORY_GUARD_LIMITS_OTP28.md)

## Library Integration Patterns (v0.6.0)

### 1. gproc Registry Pattern
```erlang
%% Register with gproc (automatic monitoring)
register_server(ServerId, ServerPid, Config) ->
    %% Name registration
    gproc:add_local_name({mcp, server, ServerId}),
    %% Store config as property
    gproc:reg({p, l, {mcp_server_config, ServerId}}, Config).

%% Lookup with gproc
find_server(ServerId) ->
    case gproc:lookup_local_name({mcp, server, ServerId}) of
        undefined -> {error, not_found};
        Pid -> {ok, Pid}
    end.

%% No manual monitoring needed - gproc handles it
```

**Benefits:**
- Automatic cleanup on process death
- No manual `monitor`/`demonitor` code
- Distributed registry support
- O(1) lookups via ETS

### 2. gun HTTP Client Pattern
```erlang
%% Initialize connection
init([TransportId, #{url := Url} = Config]) ->
    {ok, {Scheme, _Auth, Host, Port, _Path, _Query}} = http_uri:parse(Url),

    GunOpts = #{
        protocols => [http2, http],
        retry => maps:get(retry, Config, 5),
        retry_timeout => maps:get(retry_timeout, Config, 1000)
    },

    {ok, GunPid} = gun:open(Host, Port, GunOpts),
    MonitorRef = monitor(process, GunPid),

    State = #state{
        gun_pid = GunPid,
        gun_monitor = MonitorRef,
        transport_id = TransportId
    },
    {ok, State}.

%% Send request
handle_call({send, Data}, _From, #state{gun_pid = GunPid} = State) ->
    StreamRef = gun:post(GunPid, "/mcp", Headers, Data),
    %% Track pending request
    NewState = store_pending(StreamRef, State),
    {reply, ok, NewState}.

%% Handle HTTP/2 response
handle_info({gun_response, GunPid, StreamRef, fin, Status, Headers}, State) ->
    %% Response complete (no body)
    process_response(StreamRef, Status, Headers, <<>>, State);

handle_info({gun_response, GunPid, StreamRef, nofin, Status, Headers}, State) ->
    %% Response has body coming
    {noreply, State#state{current_stream = StreamRef}};

handle_info({gun_data, GunPid, StreamRef, IsFin, Data}, State) ->
    %% Route to server via registry
    erlmcp_registry:route_to_server(State#state.server_id,
                                     State#state.transport_id,
                                     Data),
    {noreply, State}.
```

**Features:**
- HTTP/2 multiplexing (multiple streams per connection)
- Automatic protocol negotiation
- Better connection reuse
- Built-in retry logic

### 3. ranch TCP Protocol Pattern
```erlang
-behaviour(ranch_protocol).

%% Server mode - ranch handles accept
start_link(Ref, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, Opts) ->
    {ok, Socket} = ranch:handshake(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    loop(Socket, Transport, Opts).

loop(Socket, Transport, Opts) ->
    receive
        {tcp, Socket, Data} ->
            %% Process data
            handle_data(Data, Opts),
            ok = Transport:setopts(Socket, [{active, once}]),
            loop(Socket, Transport, Opts);
        {tcp_closed, Socket} ->
            ok = Transport:close(Socket);
        {tcp_error, Socket, Reason} ->
            logger:error("TCP error: ~p", [Reason]),
            ok = Transport:close(Socket)
    end.

%% Client mode - simple gen_tcp
init_client(Host, Port, Opts) ->
    ConnectOpts = [
        binary,
        {active, true},
        {packet, 0},
        {nodelay, true},
        {keepalive, true}
    ],
    gen_tcp:connect(Host, Port, ConnectOpts, 5000).
```

**Benefits:**
- ranch manages accept pool
- Supervisor integration
- Built-in connection limits
- Used in production by EMQX, Cowboy

### 4. poolboy Connection Pooling Pattern
```erlang
%% Start pool in supervisor
init([]) ->
    PoolArgs = [
        {name, {local, http_pool}},
        {worker_module, erlmcp_http_worker},
        {size, 10},
        {max_overflow, 5}
    ],
    PoolSpec = poolboy:child_spec(http_pool, PoolArgs, []),
    {ok, {{one_for_one, 10, 10}, [PoolSpec]}}.

%% Use pool for requests
call_with_pool(Request) ->
    poolboy:transaction(http_pool, fun(Worker) ->
        erlmcp_http_worker:handle_request(Worker, Request)
    end, 5000).

%% Worker implementation
-module(erlmcp_http_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

handle_request(Worker, Request) ->
    gen_server:call(Worker, {request, Request}).
```

**Features:**
- Limit concurrent connections
- Queue management
- Resource reuse
- Backpressure handling

### 5. Library-Aware Supervision Pattern
```erlang
%% Transport supervisor with library support
start_child(TransportId, Type, Config) ->
    Module = transport_module(Type),

    ChildSpec = #{
        id => TransportId,
        start => {Module, start_link, [TransportId, Config]},
        restart => permanent,        % Libraries handle recovery
        shutdown => 5000,            % Time for cleanup
        type => worker,
        modules => [Module]
    },

    supervisor:start_child(?MODULE, ChildSpec).

transport_module(stdio) -> erlmcp_transport_stdio_new;
transport_module(tcp) -> erlmcp_transport_tcp;     % Uses ranch
transport_module(http) -> erlmcp_transport_http.   % Uses gun
```

## Testing Patterns

### 1. Mocking with Processes
```erlang
%% Test helper
mock_server(Responses) ->
    spawn(fun() -> mock_loop(Responses) end).

mock_loop([{Request, Response} | Rest]) ->
    receive
        Request ->
            sender() ! Response,
            mock_loop(Rest)
    end.
```

### 2. Property-Based Testing
```erlang
prop_resource_handler() ->
    ?FORALL(Uri, binary(),
        begin
            {ok, Server} = start_server(),
            Handler = fun(_) -> <<"test">> end,
            ok = erlmcp_server:add_resource(Server, Uri, Handler),
            {ok, <<"test">>} =:= get_resource(Server, Uri)
        end).
```

### 3. Testing with Libraries (v0.6.0)
```erlang
%% Mock gproc in tests
setup_gproc_test() ->
    application:ensure_all_started(gproc),
    ok.

%% Mock gun responses
setup_gun_mock() ->
    meck:new(gun, [passthrough]),
    meck:expect(gun, open, fun(_, _, _) -> {ok, self()} end),
    meck:expect(gun, post, fun(_, _, _, _) -> make_ref() end).

%% Test ranch protocol
test_ranch_handler() ->
    {ok, _} = ranch:start_listener(test_tcp,
                                   ranch_tcp,
                                   #{port => 0},
                                   erlmcp_transport_tcp,
                                   [test_id, #{}]),
    {ok, Port} = ranch:get_port(test_tcp),
    {ok, Socket} = gen_tcp:connect("localhost", Port, [binary]),
    ok.
```

## Common Pitfalls

1. **Don't block in init/1** - Do async initialization
2. **Avoid large messages** - Use references to shared data
3. **Monitor critical processes** - Clean up when they die
4. **Set proper timeouts** - Prevent hanging calls
5. **Use supervisors** - Don't spawn unsupervised processes
6. **Library cleanup** - Let libraries handle resource cleanup (gun, ranch)
7. **Pool exhaustion** - Monitor poolboy queue sizes
8. **gproc conflicts** - Use unique names across application