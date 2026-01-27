# OTP Patterns in erlmcp

## Supervision Trees

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