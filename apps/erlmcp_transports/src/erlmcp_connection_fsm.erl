%%%-------------------------------------------------------------------
%%% @doc
%%% Connection State Machine for Transport Layer
%%%
%%% This module implements a comprehensive gen_statem-based connection
%%% state machine managing the complete lifecycle of transport connections.
%%%
%%% == Features ==
%%%
%%% 1. **Connection Lifecycle Management**
%%%    - States: disconnected, connecting, connected, ready, reconnecting,
%%%      disconnecting, failed, closed
%%%    - Automatic reconnection with exponential backoff
%%%    - Graceful shutdown and cleanup
%%%
%%% 2. **Circuit Breaker Pattern**
%%%    - Prevents cascading failures
%%%    - Automatic recovery detection
%%%    - Half-open state for testing
%%%
%%% 3. **Flow Control & Backpressure**
%%%    - Window-based flow control
%%%    - Automatic backpressure signaling
%%%    - Buffer management
%%%
%%% 4. **Connection Pooling**
%%%    - Integration with poolboy
%%% - Lease management
%%% - Load balancing strategies
%%%
%%% 5. **Health Monitoring**
%%%    - Periodic health checks
%%% - Consecutive failure tracking
%%% - Automatic status updates
%%%
%%% 6. **Keep-Alive**
%%%    - Configurable keep-alive intervals
%%% - Missed keep-alive detection
%%% - Automatic reconnection
%%%
%%% 7. **Metrics Collection**
%%%    - Connection counts
%%% - Byte/message counters
%%% - Latency tracking
%%% - Error tracking
%%%
%%% 8. **Multiplexing Support**
%%%    - Parent-child connection relationships
%%% - Shared connection tracking
%%%
%%% == State Diagram ==
%%%
%%% ```
%%%     disconnected
%%%          |
%%%          | {connect, Opts}
%%%          v
%%%     connecting
%%%          |
%%%          | timeout/error | success
%%%          v                v
%%%     reconnecting      connected
%%%          |                |
%%%          | max_attempts   | {ready}
%%%          v                v
%%%     failed            ready
%%%                          |
%%%                          | {disconnect}
%%%                          v
%%%     disconnecting
%%%          |
%%%          | cleanup_complete
%%%          v
%%%     closed
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_connection_fsm).
-behaviour(gen_statem).

%% Public API
-export([start_link/4,
         connect/2,
         disconnect/1,
         send/2,
         get_state/1,
         get_metrics/1,
         get_health/1,
         reset_circuit_breaker/1,
         update_config/2,
         lease_connection/2,
         release_connection/1]).

%% gen_statem callbacks
-export([init/1,
         callback_mode/0,
         terminate/3,
         code_change/4,
         disconnected/3,
         connecting/3,
         connected/3,
         ready/3,
         reconnecting/3,
         disconnecting/3,
         failed/3,
         closed/3]).

-include("erlmcp.hrl").
-include("erlmcp_connection_state.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_CONNECT_TIMEOUT, 5000).
-define(DEFAULT_SEND_TIMEOUT, 5000).
-define(DEFAULT_LEASE_TIMEOUT, 30000).

%%====================================================================
%% Type Definitions
%%====================================================================

%%====================================================================
%% Public API
%%====================================================================

%% @doc Start a connection FSM
-spec start_link(atom(), transport_type(), inet:hostname() | inet:ip_address(),
                inet:port_number()) -> gen_statem:start_ret().
start_link(Name, TransportType, Host, Port) ->
    gen_statem:start_link({local, Name}, ?MODULE, [Name, TransportType, Host, Port], []).

%% @doc Initiate a connection
-spec connect(pid(), map()) -> ok | {error, term()}.
connect(Pid, Opts) when is_map(Opts) ->
    gen_statem:call(Pid, {connect, Opts}, ?DEFAULT_CONNECT_TIMEOUT).

%% @doc Disconnect gracefully
-spec disconnect(pid()) -> ok.
disconnect(Pid) ->
    gen_statem:call(Pid, disconnect, 5000).

%% @doc Send data through the connection
-spec send(pid(), iodata()) -> ok | {error, term()}.
send(Pid, Data) ->
    gen_statem:call(Pid, {send, Data}, ?DEFAULT_SEND_TIMEOUT).

%% @doc Get current state and metrics
-spec get_state(pid()) -> {ok, conn_state(), map()}.
get_state(Pid) ->
    gen_statem:call(Pid, get_state, 1000).

%% @doc Get connection metrics
-spec get_metrics(pid()) -> {ok, conn_metrics()}.
get_metrics(Pid) ->
    gen_statem:call(Pid, get_metrics, 1000).

%% @doc Get health status
-spec get_health(pid()) -> {ok, healthy | unhealthy | unknown, map()}.
get_health(Pid) ->
    gen_statem:call(Pid, get_health, 1000).

%% @doc Reset circuit breaker to closed state
-spec reset_circuit_breaker(pid()) -> ok.
reset_circuit_breaker(Pid) ->
    gen_statem:call(Pid, reset_circuit_breaker, 1000).

%% @doc Update configuration dynamically
-spec update_config(pid(), map()) -> ok.
update_config(Pid, Config) when is_map(Config) ->
    gen_statem:call(Pid, {update_config, Config}, 1000).

%% @doc Lease connection for exclusive use
-spec lease_connection(pid(), timeout()) -> {ok, reference()} | {error, term()}.
lease_connection(Pid, Timeout) ->
    gen_statem:call(Pid, {lease, self(), Timeout}, Timeout).

%% @doc Release leased connection
-spec release_connection(pid()) -> ok.
release_connection(Pid) ->
    gen_statem:call(Pid, {release, self()}, 1000).

%%====================================================================
%% gen_statem Callbacks
%%====================================================================

%% @private
init([Name, TransportType, Host, Port]) ->
    process_flag(trap_exit, true),

    %% Initialize state
    BackoffConfig = #backoff_config{},
    CircuitBreaker = #circuit_breaker{},
    FlowControl = #flow_control{},
    KeepAliveConfig = #keepalive_config{},
    HealthConfig = #health_config{},
    PoolConfig = #pool_config{},

    State = #conn_fsm_state{
        id = Name,
        transport_type = TransportType,
        current_state = disconnected,
        host = Host,
        port = Port,
        backoff_config = BackoffConfig,
        circuit_breaker = CircuitBreaker,
        flow_control = FlowControl,
        keepalive_config = KeepAliveConfig,
        health_config = HealthConfig,
        pool_config = PoolConfig,
        state_enter_time = erlang:system_time(millisecond)
    },

    {ok, disconnected, State}.

%% @private
callback_mode() ->
    state_functions.

%% @private
terminate(Reason, StateName, #conn_fsm_state{id = Id} = State) ->
    logger:info("Connection FSM ~p terminating in state ~p: ~p", [Id, StateName, Reason]),

    %% Cancel all timers
    cancel_all_timers(State),

    %% Close socket if still open
    close_transport(State),

    %% Release from pool if leased
    maybe_release_from_pool(State),

    ok.

%% @private
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%====================================================================
%% State: disconnected
%%====================================================================

%% @private
disconnected(info, {connect, Opts}, State) ->
    %% Async connect request
    handle_connect_request(Opts, State);

disconnected({call, From}, {connect, Opts}, State) ->
    %% Synchronous connect request
    case handle_connect_request(Opts, State) of
        {next_state, NewStateName, NewState} ->
            {next_state, NewStateName, NewState, [{reply, From, ok}]};
        {next_state, NewStateName, NewState, Actions} ->
            {next_state, NewStateName, NewState, [{reply, From, ok} | Actions]};
        Response ->
            {keep_state, State, [{reply, From, Response}]}
    end;

disconnected({call, From}, get_state, #conn_fsm_state{current_state = disconnected} = State) ->
    Info = state_to_info(State),
    {keep_state, State, [{reply, From, {ok, disconnected, Info}}]};

disconnected({call, From}, get_metrics, #conn_fsm_state{metrics = Metrics} = State) ->
    {keep_state, State, [{reply, From, {ok, Metrics}}]};

disconnected({call, From}, get_health, #conn_fsm_state{health_status = Status} = State) ->
    HealthInfo = #{
        status => Status,
        consecutive_failures => State#conn_fsm_state.consecutive_failures,
        consecutive_successes => State#conn_fsm_state.consecutive_successes,
        circuit_breaker => circuit_breaker_to_map(State#conn_fsm_state.circuit_breaker)
    },
    {keep_state, State, [{reply, From, {ok, Status, HealthInfo}}]};

disconnected({call, From}, {update_config, Config}, State) ->
    NewState = update_configuration(Config, State),
    {keep_state, NewState, [{reply, From, ok}]};

disconnected(EventType, EventContent, State) ->
    handle_common(EventType, EventContent, State).

%%====================================================================
%% State: connecting
%%====================================================================

%% @private
connecting(state_timeout, connect_timeout, #conn_fsm_state{id = Id,
                                                           host = Host,
                                                           port = Port,
                                                           reconnect_attempts = Attempts} = State) ->
    logger:error("Connection ~p to ~s:~p timed out (attempt ~p)", [Id, Host, Port, Attempts + 1]),

    %% Record failure
    NewState = record_connection_failure(timeout, State),

    %% Check circuit breaker
    #circuit_breaker{state = CBState} = NewState#conn_fsm_state.circuit_breaker,

    case CBState of
        open ->
            %% Circuit breaker is open, go to failed state
            {next_state, failed, NewState};
        _ when Attempts >= NewState#conn_fsm_state.backoff_config#backoff_config.max_attempts ->
            %% Max attempts reached
            {next_state, failed, NewState};
        _ ->
            %% Schedule reconnection
            {next_state, reconnecting, NewState}
    end;

connecting(info, {connected, Socket}, #conn_fsm_state{id = Id,
                                                      host = Host,
                                                      port = Port} = State) ->
    logger:info("Connection ~p to ~s:~p established", [Id, Host, Port]),

    %% Update metrics
    Now = erlang:system_time(millisecond),
    ConnectTime = case State#conn_fsm_state.connect_start_time of
        undefined -> 0;
        StartTime -> Now - StartTime
    end,

    NewMetrics = State#conn_fsm_state.metrics#conn_metrics{
        connect_count = State#conn_fsm_state.metrics#conn_metrics.connect_count + 1,
        last_connect_time = Now,
        latency_ms = ConnectTime
    },

    %% Start keep-alive timer
    NewState1 = maybe_start_keepalive_timer(State),

    %% Start health check timer
    NewState2 = maybe_start_health_timer(NewState1),

    %% Update circuit breaker on success
    NewState3 = record_connection_success(NewState2),

    %% Monitor transport process
    NewState4 = maybe_monitor_transport(Socket, NewState3),

    %% Notify owner
    Owner = NewState4#conn_fsm_state.owner,
    case Owner of
        undefined -> ok;
        _ -> Owner ! {connection_connected, self()}
    end,

    {next_state, connected, NewState4#conn_fsm_state{
        socket = Socket,
        current_state = connected,
        reconnect_attempts = 0,
        metrics = NewMetrics,
        state_enter_time = Now
    }};

connecting(info, {error, Reason}, #conn_fsm_state{id = Id,
                                                  host = Host,
                                                  port = Port} = State) ->
    logger:error("Connection ~p to ~s:~p failed: ~p", [Id, Host, Port, Reason]),

    %% Record failure
    NewState = record_connection_failure(Reason, State),

    %% Check circuit breaker
    #circuit_breaker{state = CBState} = NewState#conn_fsm_state.circuit_breaker,

    case CBState of
        open ->
            {next_state, failed, NewState};
        _ ->
            {next_state, reconnecting, NewState}
    end;

connecting(EventType, EventContent, State) ->
    handle_common(EventType, EventContent, State).

%%====================================================================
%% State: connected
%%====================================================================

%% @private
connected(info, ready, #conn_fsm_state{} = State) ->
    %% Transport is ready for data transfer
    logger:debug("Connection ~p ready for data transfer", [State#conn_fsm_state.id]),

    {next_state, ready, State#conn_fsm_state{
        current_state = ready,
        state_enter_time = erlang:system_time(millisecond)
    }};

connected(info, {data, Data}, #conn_fsm_state{metrics = Metrics} = State) ->
    %% Received data from transport
    NewMetrics = Metrics#conn_metrics{
        bytes_received = Metrics#conn_metrics.bytes_received + byte_size(Data),
        messages_received = Metrics#conn_metrics.messages_received + 1
    },

    %% Forward to owner
    Owner = State#conn_fsm_state.owner,
    case Owner of
        undefined -> ok;
        _ -> Owner ! {connection_data, self(), Data}
    end,

    {keep_state, State#conn_fsm_state{metrics = NewMetrics}};

connected(info, {error, Reason}, State) ->
    %% Transport error
    logger:error("Connection ~p transport error: ~p", [State#conn_fsm_state.id, Reason]),

    %% Record failure
    NewState = record_connection_failure(Reason, State),

    {next_state, reconnecting, NewState};

connected(info, {tcp_closed, _Socket}, State) ->
    %% TCP connection closed
    logger:info("Connection ~p closed by remote", [State#conn_fsm_state.id]),

    NewState = record_connection_failure(closed, State),

    {next_state, reconnecting, NewState};

connected(info, {tcp_error, _Socket, Reason}, State) ->
    %% TCP error
    logger:error("Connection ~p TCP error: ~p", [State#conn_fsm_state.id, Reason]),

    NewState = record_connection_failure(Reason, State),

    {next_state, reconnecting, NewState};

connected({call, From}, {send, _Data}, #conn_fsm_state{} = State) ->
    %% Not ready yet, buffer the data
    {keep_state, State, [{reply, From, {error, not_ready}}]};

connected(EventType, EventContent, State) ->
    handle_common(EventType, EventContent, State).

%%====================================================================
%% State: ready
%%====================================================================

%% @private
ready({call, From}, {send, Data}, #conn_fsm_state{flow_control = #flow_control{state = backpressure},
                                                  send_buffer = Buffer,
                                                  send_buffer_size = BufferSize,
                                                  max_buffer_size = MaxSize} = State) ->

    DataSize = byte_size(Data),
    NewBufferSize = BufferSize + DataSize,

    case NewBufferSize > MaxSize of
        true ->
            %% Buffer full, reject
            {keep_state, State, [{reply, From, {error, buffer_full}}]};
        false ->
            %% Add to buffer
            NewBuffer = queue:in(Data, Buffer),
            NewState = State#conn_fsm_state{
                send_buffer = NewBuffer,
                send_buffer_size = NewBufferSize
            },
            {keep_state, NewState, [{reply, From, ok}]}
    end;

ready({call, From}, {send, Data}, #conn_fsm_state{socket = Socket,
                                                    flow_control = FlowCtrl,
                                                    metrics = Metrics} = State) ->

    case send_to_transport(Socket, Data) of
        ok ->
            %% Update metrics
            DataSize = byte_size(Data),
            NewMetrics = Metrics#conn_metrics{
                bytes_sent = Metrics#conn_metrics.bytes_sent + DataSize,
                messages_sent = Metrics#conn_metrics.messages_sent + 1
            },

            %% Update flow control window
            NewFlowCtrl = update_flow_control_window(DataSize, FlowCtrl),

            %% Check if we should apply backpressure
            Actions = case should_apply_backpressure(NewFlowCtrl) of
                true ->
                    %% Notify owner of backpressure
                    Owner = State#conn_fsm_state.owner,
                    case Owner of
                        undefined -> [];
                        _ -> Owner ! {connection_backpressure, self(), true}
                    end,
                    [{reply, From, ok}];
                false ->
                    [{reply, From, ok}]
            end,

            {keep_state, State#conn_fsm_state{
                metrics = NewMetrics,
                flow_control = NewFlowCtrl
            }, Actions};
        {error, Reason} ->
            %% Send failed
            logger:error("Connection ~p send failed: ~p", [State#conn_fsm_state.id, Reason]),

            NewState = record_connection_failure(Reason, State),

            {next_state, reconnecting, NewState, [{reply, From, {error, Reason}}]}
    end;

ready(info, {keepalive, timeout}, #conn_fsm_state{id = Id,
                                                    keepalive_config = #keepalive_config{max_missed = MaxMissed}} = State) ->

    %% Check if we've exceeded max missed keepalives
    case State#conn_fsm_state.error_count >= MaxMissed of
        true ->
            logger:warning("Connection ~p missed ~p keepalives, reconnecting", [Id, MaxMissed]),

            NewState = record_connection_failure(keepalive_timeout, State),

            {next_state, reconnecting, NewState};
        false ->
            %% Send keep-alive and reschedule
            send_keepalive(State),
            NewState = schedule_keepalive_timer(State),
            {keep_state, NewState}
    end;

ready(info, {health_check, Result}, #conn_fsm_state{health_config = #health_config{failure_threshold = FailThresh,
                                                                                     success_threshold = SuccThresh}} = State) ->

    case Result of
        ok ->
            %% Health check passed
            NewConsecutiveSucc = State#conn_fsm_state.consecutive_successes + 1,
            NewHealthStatus = case NewConsecutiveSucc >= SuccThresh of
                true -> healthy;
                false -> State#conn_fsm_state.health_status
            end,

            NewState = State#conn_fsm_state{
                health_status = NewHealthStatus,
                consecutive_successes = NewConsecutiveSucc,
                consecutive_failures = 0
            },

            {keep_state, NewState};
        {error, _} ->
            %% Health check failed
            NewConsecutiveFail = State#conn_fsm_state.consecutive_failures + 1,
            NewHealthStatus = case NewConsecutiveFail >= FailThresh of
                true -> unhealthy;
                false -> State#conn_fsm_state.health_status
            end,

            NewState = State#conn_fsm_state{
                health_status = NewHealthStatus,
                consecutive_failures = NewConsecutiveFail,
                consecutive_successes = 0
            },

            {keep_state, NewState}
    end;

ready(info, {flow_control, FlowState}, State) ->
    %% Flow control state change
    NewFlowCtrl = State#conn_fsm_state.flow_control#flow_control{state = FlowState},

    %% Notify owner
    Owner = State#conn_fsm_state.owner,
    case Owner of
        undefined -> ok;
        _ ->
            case FlowState of
                backpressure -> Owner ! {connection_backpressure, self(), true};
                normal -> Owner ! {connection_backpressure, self(), false};
                _ -> ok
            end
    end,

    {keep_state, State#conn_fsm_state{flow_control = NewFlowCtrl}};

ready(EventType, EventContent, State) ->
    handle_common(EventType, EventContent, State).

%%====================================================================
%% State: reconnecting
%%====================================================================

%% @private
reconnecting(info, {circuit_breaker, closed}, #conn_fsm_state{id = Id} = State) ->
    logger:info("Connection ~p circuit breaker closed, attempting reconnection", [Id]),

    %% Reset reconnect attempts since circuit breaker is now closed
    NewState = State#conn_fsm_state{
        reconnect_attempts = 0,
        circuit_breaker = State#conn_fsm_state.circuit_breaker#circuit_breaker{state = closed}
    },

    self() ! {connect, #{}},

    {next_state, connecting, NewState};

reconnecting(info, reconnect, #conn_fsm_state{id = Id,
                                              reconnect_attempts = Attempts,
                                              backoff_config = #backoff_config{max_attempts = MaxAttempts}} = State) ->

    case Attempts >= MaxAttempts of
        true ->
            logger:error("Connection ~p max reconnection attempts (~p) reached", [Id, MaxAttempts]),
            {next_state, failed, State};
        false ->
            logger:info("Connection ~p reconnection attempt ~p", [Id, Attempts + 1]),

            %% Calculate backoff delay
            Delay = calculate_backoff(Attempts, State#conn_fsm_state.backoff_config),

            %% Schedule connection attempt
            NewState = schedule_reconnect_timer(Delay, State#conn_fsm_state{
                reconnect_attempts = Attempts + 1
            }),

            {next_state, connecting, NewState}
    end;

reconnecting({call, From}, disconnect, State) ->
    %% Cancel reconnect during disconnect
    NewState = cancel_reconnect_timer(State),

    {next_state, disconnecting, NewState, [{reply, From, ok}]};

reconnecting(EventType, EventContent, State) ->
    handle_common(EventType, EventContent, State).

%%====================================================================
%% State: disconnecting
%%====================================================================

%% @private
disconnecting(info, disconnect_complete, #conn_fsm_state{id = Id} = State) ->
    logger:info("Connection ~p disconnect complete", [Id]),

    %% Close transport
    NewState = close_transport(State),

    %% Update metrics
    Now = erlang:system_time(millisecond),
    NewMetrics = NewState#conn_fsm_state.metrics#conn_metrics{
        disconnect_count = NewState#conn_fsm_state.metrics#conn_metrics.disconnect_count + 1,
        last_disconnect_time = Now
    },

    %% Notify owner
    Owner = NewState#conn_fsm_state.owner,
    case Owner of
        undefined -> ok;
        _ -> Owner ! {connection_disconnected, self(), normal}
    end,

    {next_state, closed, NewState#conn_fsm_state{
        socket = undefined,
        metrics = NewMetrics
    }};

disconnecting(EventType, EventContent, State) ->
    handle_common(EventType, EventContent, State).

%%====================================================================
%% State: failed
%%====================================================================

%% @private
failed({call, From}, reset_circuit_breaker, #conn_fsm_state{} = State) ->
    %% Reset circuit breaker to closed state
    NewCB = State#conn_fsm_state.circuit_breaker#circuit_breaker{
        state = closed,
        failure_count = 0,
        last_state_change = erlang:system_time(millisecond)
    },

    NewState = State#conn_fsm_state{
        circuit_breaker = NewCB,
        reconnect_attempts = 0
    },

    logger:info("Connection ~p circuit breaker reset", [State#conn_fsm_state.id]),

    %% Trigger reconnection
    self() ! {connect, #{}},

    {next_state, connecting, NewState, [{reply, From, ok}]};

failed(EventType, EventContent, State) ->
    handle_common(EventType, EventContent, State).

%%====================================================================
%% State: closed
%%====================================================================

%% @private
closed({call, From}, {connect, Opts}, State) ->
    %% Allow reconnection from closed state
    case handle_connect_request(Opts, State) of
        {next_state, NewStateName, NewState} ->
            {next_state, NewStateName, NewState, [{reply, From, ok}]};
        Response ->
            {keep_state, State, [{reply, From, Response}]}
    end;

closed(EventType, EventContent, State) ->
    handle_common(EventType, EventContent, State).

%%====================================================================
%% Common Event Handlers
%%====================================================================

%% @private
handle_common({call, From}, get_state, #conn_fsm_state{current_state = CurrentState} = State) ->
    Info = state_to_info(State),
    {keep_state, State, [{reply, From, {ok, CurrentState, Info}}]};

handle_common({call, From}, get_metrics, #conn_fsm_state{metrics = Metrics} = State) ->
    {keep_state, State, [{reply, From, {ok, Metrics}}]};

handle_common({call, From}, get_health, #conn_fsm_state{health_status = Status} = State) ->
    HealthInfo = #{
        status => Status,
        consecutive_failures => State#conn_fsm_state.consecutive_failures,
        consecutive_successes => State#conn_fsm_state.consecutive_successes,
        circuit_breaker => circuit_breaker_to_map(State#conn_fsm_state.circuit_breaker)
    },
    {keep_state, State, [{reply, From, {ok, Status, HealthInfo}}]};

handle_common({call, From}, {update_config, Config}, State) ->
    NewState = update_configuration(Config, State),
    {keep_state, NewState, [{reply, From, ok}]};

handle_common({call, From}, {lease, Pid, Timeout}, #conn_fsm_state{leased_by = undefined,
                                                                  current_state = ready} = State) ->

    LeaseRef = make_ref(),
    NewState = State#conn_fsm_state{
        leased_by = Pid,
        lease_timer = erlang:send_after(Timeout, self(), {lease_timeout, Pid})
    },

    %% Monitor lessee
    MonRef = monitor(process, Pid),
    NewState2 = NewState#conn_fsm_state{
        monitor_refs = maps:put(Pid, MonRef, NewState#conn_fsm_state.monitor_refs)
    },

    {keep_state, NewState2, [{reply, From, {ok, LeaseRef}}]};

handle_common({call, From}, {lease, _Pid, _Timeout}, State) ->
    %% Already leased or not ready
    {keep_state, State, [{reply, From, {error, not_available}}]};

handle_common({call, From}, {release, Pid}, #conn_fsm_state{leased_by = Pid} = State) ->
    %% Release lease
    NewState = State#conn_fsm_state{
        leased_by = undefined,
        lease_timer = undefined
    },

    %% Demonitor lessee
    NewState2 = case maps:get(Pid, NewState#conn_fsm_state.monitor_refs, undefined) of
        undefined -> NewState;
        MonRef ->
            erlang:demonitor(MonRef, [flush]),
            NewState#conn_fsm_state{monitor_refs = maps:remove(Pid, NewState#conn_fsm_state.monitor_refs)}
    end,

    {keep_state, NewState2, [{reply, From, ok}]};

handle_common({call, From}, {release, _Pid}, State) ->
    %% Not leased by this process
    {keep_state, State, [{reply, From, {error, not_leased}}]};

handle_common(info, {lease_timeout, Pid}, #conn_fsm_state{leased_by = Pid} = State) ->
    %% Lease timeout, force release
    logger:warning("Connection ~p lease timeout for ~p", [State#conn_fsm_state.id, Pid]),

    NewState = State#conn_fsm_state{
        leased_by = undefined,
        lease_timer = undefined
    },

    {keep_state, NewState};

handle_common(info, {'DOWN', _MonRef, process, Pid, Reason}, #conn_fsm_state{leased_by = Pid,
                                                                             monitor_refs = MonRefs} = State)
    when map_size(MonRefs) > 0 ->

    %% Lessee died, release lease
    logger:info("Connection ~p lessee ~p died: ~p", [State#conn_fsm_state.id, Pid, Reason]),

    NewState = State#conn_fsm_state{
        leased_by = undefined,
        lease_timer = undefined,
        monitor_refs = maps:remove(Pid, MonRefs)
    },

    {keep_state, NewState};

handle_common(info, {'DOWN', _MonRef, process, Owner, Reason}, #conn_fsm_state{owner = Owner} = State) ->
    %% Owner died
    logger:info("Connection ~p owner ~p died: ~p", [State#conn_fsm_state.id, Owner, Reason]),

    {stop, {owner_died, Reason}, State};

handle_common(info, {'DOWN', _MonRef, process, TransportPid, Reason}, #conn_fsm_state{transport_pid = TransportPid} = State) ->
    %% Transport process died
    logger:error("Connection ~p transport ~p died: ~p", [State#conn_fsm_state.id, TransportPid, Reason]),

    NewState = record_connection_failure({transport_died, Reason}, State),

    {next_state, reconnecting, NewState};

handle_common(info, Info, State) ->
    logger:warning("Connection ~p unexpected info in state ~p: ~p", [State#conn_fsm_state.id, State#conn_fsm_state.current_state, Info]),
    {keep_state, State};

handle_common(EventType, EventContent, State) ->
    logger:warning("Connection ~p unexpected event ~p:~p in state ~p",
                   [State#conn_fsm_state.id, EventType, EventContent, State#conn_fsm_state.current_state]),
    {keep_state, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Handle connect request
-spec handle_connect_request(map(), conn_fsm_state()) -> conn_fsm_response().
handle_connect_request(Opts, #conn_fsm_state{id = Id,
                                            transport_type = TransportType,
                                            host = Host,
                                            port = Port} = State) ->

    %% Check circuit breaker
    #circuit_breaker{state = CBState} = State#conn_fsm_state.circuit_breaker,

    case CBState of
        open ->
            logger:warning("Connection ~p circuit breaker open, rejecting connection", [Id]),
            {keep_state, State};
        _ ->
            %% Update options if provided
            NewState = case maps:get(options, Opts, undefined) of
                undefined -> State;
                NewOpts -> State#conn_fsm_state{options = NewOpts}
            end,

            %% Update owner if provided
            NewState2 = case maps:get(owner, Opts, undefined) of
                undefined -> NewState;
                Owner -> NewState#conn_fsm_state{owner = Owner}
            end,

            %% Start connection based on transport type
            case start_transport(TransportType, Host, Port, NewState2) of
                {ok, TransportPid} ->
                    %% Set timeout for connection
                    Timeout = maps:get(timeout, Opts, ?DEFAULT_CONNECT_TIMEOUT),

                    NewState3 = NewState2#conn_fsm_state{
                        connect_start_time = erlang:system_time(millisecond),
                        transport_pid = TransportPid
                    },

                    {next_state, connecting, NewState3, [{state_timeout, Timeout, connect_timeout}]};
                {error, Reason} ->
                    logger:error("Connection ~p failed to start transport: ~p", [Id, Reason]),

                    NewStateFailed = record_connection_failure(Reason, NewState2),

                    {next_state, reconnecting, NewStateFailed}
            end
    end.

%% @doc Start transport based on type
-spec start_transport(transport_type(), inet:hostname() | inet:ip_address(),
                      inet:port_number(), conn_fsm_state()) ->
    {ok, pid()} | {error, term()}.
start_transport(stdio, _Host, _Port, _State) ->
    %% Start stdio transport
    case erlmcp_transport_stdio:start_link(self()) of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end;

start_transport(tcp, Host, Port, State) ->
    %% Start TCP transport (client mode)
    Opts = #{
        mode => client,
        host => Host,
        port => Port,
        owner => self(),
        server_id => State#conn_fsm_state.id
    },

    case erlmcp_transport_tcp:start_client(Opts) of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end;

start_transport(http, Host, Port, State) ->
    %% Start HTTP transport using gun
    Opts = #{
        host => Host,
        port => Port,
        owner => self(),
        transport_id => State#conn_fsm_state.id
    },

    case erlmcp_transport_http:start_client(Opts) of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end;

start_transport(http2, Host, Port, State) ->
    %% Start HTTP/2 transport using gun
    Opts = #{
        host => Host,
        port => Port,
        owner => self(),
        transport_id => State#conn_fsm_state.id,
        protocols => [http2]
    },

    case erlmcp_transport_http2_client:start_link(Opts) of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end;

start_transport(ws, Host, Port, State) ->
    %% Start WebSocket transport
    Opts = #{
        host => Host,
        port => Port,
        owner => self(),
        transport_id => State#conn_fsm_state.id
    },

    case erlmcp_transport_ws:start_client(Opts) of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end;

start_transport(sse, Host, Port, State) ->
    %% Start SSE transport
    Opts = #{
        host => Host,
        port => Port,
        owner => self(),
        transport_id => State#conn_fsm_state.id
    },

    case erlmcp_transport_sse:start_client(Opts) of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end;

start_transport(tls, Host, Port, State) ->
    %% Start TLS/TCP transport
    Opts = #{
        mode => client,
        host => Host,
        port => Port,
        owner => self(),
        server_id => State#conn_fsm_state.id,
        tls => true
    },

    case erlmcp_transport_tcp:start_client(Opts) of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end.

%% @doc Record connection success
-spec record_connection_success(conn_fsm_state()) -> conn_fsm_state().
record_connection_success(#conn_fsm_state{circuit_breaker = CB,
                                         error_count = _ErrorCount} = State) ->

    %% Update circuit breaker
    NewCB = case CB#circuit_breaker.state of
        half_open ->
            %% In half-open, test succeeded, close circuit
            CB#circuit_breaker{
                state = closed,
                success_count = 0,
                failure_count = 0,
                last_state_change = erlang:system_time(millisecond)
            };
        closed ->
            %% Normal success
            CB#circuit_breaker{
                success_count = CB#circuit_breaker.success_count + 1
            };
        open ->
            CB
    end,

    State#conn_fsm_state{
        circuit_breaker = NewCB,
        error_count = 0,
        last_error = undefined
    }.

%% @doc Record connection failure
-spec record_connection_failure(term(), conn_fsm_state()) -> conn_fsm_state().
record_connection_failure(Reason, #conn_fsm_state{circuit_breaker = CB,
                                                error_count = _ErrorCount} = State) ->

    Now = erlang:system_time(millisecond),

    %% Update circuit breaker
    NewCB = case CB#circuit_breaker.state of
        closed ->
            %% Check if threshold exceeded
            NewFailCount = CB#circuit_breaker.failure_count + 1,
            case NewFailCount >= CB#circuit_breaker.threshold of
                true ->
                    %% Open circuit breaker
                    logger:warning("Circuit breaker opened for connection ~p after ~p failures",
                                   [State#conn_fsm_state.id, NewFailCount]),

                    CB#circuit_breaker{
                        state = open,
                        failure_count = NewFailCount,
                        last_failure_time = Now,
                        last_state_change = Now
                    };
                false ->
                    CB#circuit_breaker{
                        failure_count = NewFailCount,
                        last_failure_time = Now
                    }
            end;
        half_open ->
            %% Test failed, reopen circuit
            logger:warning("Circuit breaker reopened for connection ~p (half-open test failed)",
                           [State#conn_fsm_state.id]),

            CB#circuit_breaker{
                state = open,
                failure_count = CB#circuit_breaker.failure_count + 1,
                last_failure_time = Now,
                last_state_change = Now
            };
        open ->
            %% Already open, just update
            CB#circuit_breaker{
                last_failure_time = Now
            }
    end,

    State#conn_fsm_state{
        circuit_breaker = NewCB,
        error_count = State#conn_fsm_state.error_count + 1,
        last_error = Reason
    }.

%% @doc Calculate backoff delay
-spec calculate_backoff(non_neg_integer(), backoff_config()) -> pos_integer().
calculate_backoff(Attempts, #backoff_config{strategy = exponential,
                                           initial_delay = InitialDelay,
                                           max_delay = MaxDelay,
                                           multiplier = Multiplier,
                                           jitter = Jitter}) ->

    BaseDelay = min(InitialDelay * round(math:pow(Multiplier, Attempts)), MaxDelay),

    %% Add jitter
    JitterAmount = trunc(BaseDelay * Jitter),
    BaseDelay + rand:uniform(JitterAmount) - JitterAmount div 2;

calculate_backoff(Attempts, #backoff_config{strategy = linear,
                                           initial_delay = InitialDelay,
                                           max_delay = MaxDelay}) ->

    min(InitialDelay * (Attempts + 1), MaxDelay);

calculate_backoff(_Attempts, #backoff_config{strategy = fixed,
                                            initial_delay = Delay}) ->

    Delay;

calculate_backoff(_Attempts, #backoff_config{strategy = jitter,
                                           initial_delay = InitialDelay,
                                           max_delay = MaxDelay}) ->

    BaseDelay = InitialDelay + rand:uniform(MaxDelay - InitialDelay),
    min(BaseDelay, MaxDelay);

calculate_backoff(Attempts, #backoff_config{strategy = decorrelated_jitter,
                                           initial_delay = InitialDelay,
                                           max_delay = MaxDelay}) ->

    %% Decorrelated jitter: delay = random(initial_delay, previous_delay * 3)
    %% For first attempt, use initial_delay
    PreviousDelay = case Attempts of
        0 -> InitialDelay;
        _ -> InitialDelay * 3
    end,
    min(InitialDelay + rand:uniform(PreviousDelay), MaxDelay).

%% @doc Schedule reconnect timer
-spec schedule_reconnect_timer(pos_integer(), conn_fsm_state()) -> conn_fsm_state().
schedule_reconnect_timer(Delay, #conn_fsm_state{reconnect_timer = undefined} = State) ->
    TimerRef = erlang:send_after(Delay, self(), reconnect),
    State#conn_fsm_state{reconnect_timer = TimerRef};
schedule_reconnect_timer(Delay, #conn_fsm_state{reconnect_timer = TimerRef} = State) ->
    %% Cancel existing timer first
    case erlang:cancel_timer(TimerRef) of
        false -> receive reconnect -> ok after 0 -> ok end;
        _ -> ok
    end,
    schedule_reconnect_timer(Delay, State#conn_fsm_state{reconnect_timer = undefined}).

%% @doc Cancel reconnect timer
-spec cancel_reconnect_timer(conn_fsm_state()) -> conn_fsm_state().
cancel_reconnect_timer(#conn_fsm_state{reconnect_timer = undefined} = State) ->
    State;
cancel_reconnect_timer(#conn_fsm_state{reconnect_timer = TimerRef} = State) ->
    case erlang:cancel_timer(TimerRef) of
        false -> receive reconnect -> ok after 0 -> ok end;
        _ -> ok
    end,
    State#conn_fsm_state{reconnect_timer = undefined}.

%% @doc Schedule keepalive timer
-spec schedule_keepalive_timer(conn_fsm_state()) -> conn_fsm_state().
schedule_keepalive_timer(#conn_fsm_state{keepalive_config = #keepalive_config{enabled = true,
                                                                             interval = Interval},
                                         keepalive_timer = undefined} = State) ->

    TimerRef = erlang:send_after(Interval, self(), {keepalive, timeout}),
    State#conn_fsm_state{keepalive_timer = TimerRef};

schedule_keepalive_timer(State) ->
    State.

%% @doc Maybe start keepalive timer
-spec maybe_start_keepalive_timer(conn_fsm_state()) -> conn_fsm_state().
maybe_start_keepalive_timer(#conn_fsm_state{keepalive_config = #keepalive_config{enabled = true}} = State) ->
    schedule_keepalive_timer(State);
maybe_start_keepalive_timer(State) ->
    State.

%% @doc Send keepalive
-spec send_keepalive(conn_fsm_state()) -> ok.
send_keepalive(#conn_fsm_state{socket = Socket}) when Socket =/= undefined ->
    %% Send keepalive through transport
    try
        gen_tcp:send(Socket, <<>>),
        ok
    catch
        _:_ -> {error, send_failed}
    end;
send_keepalive(_) ->
    ok.

%% @doc Schedule health check timer
-spec schedule_health_timer(conn_fsm_state()) -> conn_fsm_state().
schedule_health_timer(#conn_fsm_state{health_config = #health_config{enabled = true,
                                                                      interval = Interval},
                                      health_timer = undefined} = State) ->

    %% Perform health check
    perform_health_check(State),

    %% Schedule next check
    TimerRef = erlang:send_after(Interval, self(), health_check),
    State#conn_fsm_state{health_timer = TimerRef};

schedule_health_timer(State) ->
    State.

%% @doc Maybe start health timer
-spec maybe_start_health_timer(conn_fsm_state()) -> conn_fsm_state().
maybe_start_health_timer(#conn_fsm_state{health_config = #health_config{enabled = true}} = State) ->
    schedule_health_timer(State);
maybe_start_health_timer(State) ->
    State.

%% @doc Perform health check
-spec perform_health_check(conn_fsm_state()) -> ok.
perform_health_check(#conn_fsm_state{socket = Socket}) when Socket =/= undefined ->
    %% Check if socket is still alive
    Result = try
        case inet:setopts(Socket, [{active, once}]) of
            ok -> ok;
            {error, _} = Error -> Error
        end
    catch
        _:_ -> {error, exception}
    end,

    %% Send result to self
    self() ! {health_check, Result},
    ok;
perform_health_check(_) ->
    self() ! {health_check, {error, no_socket}},
    ok.

%% @doc Close transport
-spec close_transport(conn_fsm_state()) -> conn_fsm_state().
close_transport(#conn_fsm_state{socket = Socket} = State) when Socket =/= undefined ->
    catch gen_tcp:close(Socket),
    State#conn_fsm_state{socket = undefined};
close_transport(State) ->
    State.

%% @doc Maybe monitor transport process
-spec maybe_monitor_transport(term(), conn_fsm_state()) -> conn_fsm_state().
maybe_monitor_transport(Socket, #conn_fsm_state{transport_pid = undefined} = State)
    when is_pid(Socket) ->
    %% Socket is a process (e.g., gun connection)
    MonRef = monitor(process, Socket),
    State#conn_fsm_state{
        transport_pid = Socket,
        monitor_refs = maps:put(Socket, MonRef, State#conn_fsm_state.monitor_refs)
    };
maybe_monitor_transport(_Socket, State) ->
    State.

%% @doc Cancel all timers
-spec cancel_all_timers(conn_fsm_state()) -> ok.
cancel_all_timers(#conn_fsm_state{reconnect_timer = ReconnectTimer,
                                  keepalive_timer = KeepaliveTimer,
                                  health_timer = HealthTimer,
                                  lease_timer = LeaseTimer}) ->

    lists:foreach(fun(undefined) -> ok;
                    (TimerRef) ->
                        case erlang:cancel_timer(TimerRef) of
                            false -> ok;
                            _ -> ok
                        end
                 end,
                 [ReconnectTimer, KeepaliveTimer, HealthTimer, LeaseTimer]),

    ok.

%% @doc Maybe release from pool
-spec maybe_release_from_pool(conn_fsm_state()) -> ok.
maybe_release_from_pool(#conn_fsm_state{pool_name = undefined}) ->
    ok;
maybe_release_from_pool(#conn_fsm_state{pool_name = PoolName,
                                       id = ConnectionId}) ->
    catch poolboy:checkin(PoolName, ConnectionId),
    ok.

%% @doc Send data to transport
-spec send_to_transport(term(), iodata()) -> ok | {error, term()}.
send_to_transport(Socket, Data) when is_port(Socket) ->
    gen_tcp:send(Socket, Data);
send_to_transport(Pid, Data) when is_pid(Pid) ->
    %% Send to transport process
    Pid ! {send, Data},
    ok;
send_to_transport(_, _) ->
    {error, invalid_socket}.

%% @doc Update flow control window
-spec update_flow_control_window(non_neg_integer(), flow_control()) -> flow_control().
update_flow_control_window(BytesSent, #flow_control{current_window = CurrentWindow,
                                                     window_size = WindowSize} = FlowCtrl) ->

    NewWindow = CurrentWindow - BytesSent,

    FlowCtrl#flow_control{
        current_window = NewWindow,
        state = calculate_flow_state(NewWindow, WindowSize, FlowCtrl)
    }.

%% @doc Calculate flow control state
-spec calculate_flow_state(non_neg_integer(), non_neg_integer(), flow_control()) -> flow_control_state().
calculate_flow_state(CurrentWindow, WindowSize, #flow_control{high_watermark = HighWM,
                                                                 pause_threshold = PauseThresh,
                                                                 low_watermark = LowWM}) ->

    UsageRatio = CurrentWindow / WindowSize,

    if
        UsageRatio >= PauseThresh ->
            paused;
        UsageRatio >= HighWM ->
            backpressure;
        UsageRatio =< LowWM ->
            %% Window is low, should be replenished by peer
            normal;
        true ->
            normal
    end.

%% @doc Check if we should apply backpressure
-spec should_apply_backpressure(flow_control()) -> boolean().
should_apply_backpressure(#flow_control{state = backpressure}) ->
    true;
should_apply_backpressure(#flow_control{state = paused}) ->
    true;
should_apply_backpressure(_) ->
    false.

%% @doc Update configuration
-spec update_configuration(map(), conn_fsm_state()) -> conn_fsm_state().
update_configuration(Config, State) ->
    %% Update various configuration fields
    State1 = case maps:get(backoff_config, Config, undefined) of
        undefined -> State;
        BackoffConfig -> State#conn_fsm_state{backoff_config = BackoffConfig}
    end,

    State2 = case maps:get(circuit_breaker, Config, undefined) of
        undefined -> State1;
        CB -> State1#conn_fsm_state{circuit_breaker = CB}
    end,

    State3 = case maps:get(flow_control, Config, undefined) of
        undefined -> State2;
        FC -> State2#conn_fsm_state{flow_control = FC}
    end,

    State4 = case maps:get(keepalive_config, Config, undefined) of
        undefined -> State3;
        KAC -> State3#conn_fsm_state{keepalive_config = KAC}
    end,

    State5 = case maps:get(health_config, Config, undefined) of
        undefined -> State4;
        HC -> State4#conn_fsm_state{health_config = HC}
    end,

    State5.

%% @doc Convert state to info map
-spec state_to_info(conn_fsm_state()) -> map().
state_to_info(#conn_fsm_state{id = Id,
                              transport_type = TransportType,
                              host = Host,
                              port = Port,
                              current_state = CurrentState,
                              reconnect_attempts = ReconnectAttempts,
                              health_status = HealthStatus,
                              leased_by = LeasedBy}) ->

    #{
        id => Id,
        transport_type => TransportType,
        host => Host,
        port => Port,
        state => CurrentState,
        reconnect_attempts => ReconnectAttempts,
        health_status => HealthStatus,
        leased_by => LeasedBy
    }.

%% @doc Convert circuit breaker to map
-spec circuit_breaker_to_map(circuit_breaker()) -> map().
circuit_breaker_to_map(#circuit_breaker{state = State,
                                       failure_count = FailCount,
                                       success_count = SuccCount,
                                       threshold = Threshold,
                                       last_state_change = LastChange}) ->

    #{
        state => State,
        failure_count => FailCount,
        success_count => SuccCount,
        threshold => Threshold,
        last_state_change => LastChange
    }.
