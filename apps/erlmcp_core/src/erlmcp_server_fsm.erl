%%%-------------------------------------------------------------------
%%% @doc
%%% Server lifecycle FSM for erlmcp
%%% Implements the server connection state machine following Armstrong principles.
%%% States: initialization -> accepting -> drain -> shutdown
%%% Uses gen_statem with handle_event_function for priority message handling.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_server_fsm).
-behaviour(gen_statem).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/1,
    start_link/2,
    accept_connections/1,
    drain/1,
    shutdown/1,
    state_name/1,
    stop/1
]).

%% gen_statem callbacks
-export([
    init/1,
    callback_mode/0,
    handle_event/4,
    terminate/3,
    code_change/4,
    format_status/2
]).

%% State names for type specs
-type server_state() :: initialization | accepting | drain | shutdown.
-type server_id() :: term().

-export_type([server_state/0, server_id/0]).

%% State data record
-record(data, {
    server_id :: server_id(),
    capabilities :: #mcp_server_capabilities{} | undefined,
    protocol_version :: binary() | undefined,
    transport :: module() | undefined,
    transport_state :: term() | undefined,
    active_connections = [] :: [pid()],
    drain_timeout_ms = 30000 :: pos_integer(),
    drain_timeout_ref :: reference() | undefined,
    init_timeout_ref :: reference() | undefined,
    options :: map()
}).

-type state_data() :: #data{}.

%% Timeouts
-define(INIT_TIMEOUT_MS, 10000).
-define(DEFAULT_DRAIN_TIMEOUT_MS, 30000).
-define(SHUTDOWN_TIMEOUT_MS, 5000).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(server_id()) -> {ok, pid()} | {error, term()}.
start_link(ServerId) ->
    start_link(ServerId, #{}).

-spec start_link(server_id(), map()) -> {ok, pid()} | {error, term()}.
start_link(ServerId, Options) ->
    gen_statem:start_link(?MODULE, [ServerId, Options], []).

-spec accept_connections(pid()) -> ok | {error, term()}.
accept_connections(Pid) ->
    gen_statem:call(Pid, accept_connections).

-spec drain(pid()) -> ok.
drain(Pid) ->
    gen_statem:call(Pid, drain, ?DEFAULT_DRAIN_TIMEOUT_MS + 1000).

-spec shutdown(pid()) -> ok.
shutdown(Pid) ->
    gen_statem:call(Pid, shutdown).

-spec state_name(pid()) -> server_state().
state_name(Pid) ->
    gen_statem:call(Pid, state_name).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_statem:stop(Pid).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

-spec init([server_id() | map()]) -> {ok, server_state(), state_data()}.
init([ServerId, Options]) ->
    %% No blocking operations in init/1 - Armstrong principle
    logger:info("Server FSM ~p initializing", [ServerId]),

    Data = #data{
        server_id = ServerId,
        options = Options,
        drain_timeout_ms = maps:get(drain_timeout_ms, Options, ?DEFAULT_DRAIN_TIMEOUT_MS),
        capabilities = maps:get(capabilities, Options, #mcp_server_capabilities{}),
        protocol_version = maps:get(protocol_version, Options, ?MCP_VERSION)
    },

    %% Emit state transition event for debugging
    emit_state_transition(undefined, initialization, Data),

    {ok, initialization, Data}.

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    %% Use handle_event_function for priority message handling
    %% state_enter_calls for state entry actions
    [handle_event_function, state_enter_calls].

%%====================================================================
%% State callback - handle_event function
%%====================================================================

%% State enter calls - executed when entering a state
handle_event(enter, OldState, State, Data) ->
    emit_state_transition(OldState, State, Data),
    logger:debug("Server ~p: ~p -> ~p", [Data#data.server_id, OldState, State]),

    %% State-specific entry actions
    case State of
        initialization ->
            %% Set initialization timeout
            TimeoutRef = erlang:send_after(?INIT_TIMEOUT_MS, self(), init_timeout),
            {keep_state, Data#data{init_timeout_ref = TimeoutRef}};
        accepting ->
            %% Cancel init timeout
            cancel_init_timeout(Data),
            {keep_state, Data#data{init_timeout_ref = undefined}};
        drain ->
            %% Stop accepting new connections, wait for existing to finish
            %% Set drain timeout
            TimeoutRef = erlang:send_after(Data#data.drain_timeout_ms, self(), drain_timeout),
            logger:info("Server ~p draining ~p active connections",
                       [Data#data.server_id, length(Data#data.active_connections)]),
            {keep_state, Data#data{drain_timeout_ref = TimeoutRef}};
        shutdown ->
            %% Cleanup all resources
            cancel_drain_timeout(Data),
            close_all_connections(Data),
            cleanup_transport(Data),
            {keep_state, Data#data{active_connections = []}};
        _ ->
            {keep_state_and_data, []}
    end;

%% Priority messages (OTP 28) - bypass normal queue for control signals
%% Health check signal
handle_event({call, From}, {health_check}, State, Data) ->
    Health = #{
        state => State,
        active_connections => length(Data#data.active_connections)
    },
    {keep_state_and_data, [{reply, From, {ok, Health}}]};

%% Drain signal (graceful shutdown) - Priority message
handle_event({call, From}, drain, accepting, Data) ->
    logger:info("Server ~p received drain signal", [Data#data.server_id]),
    emit_state_transition(accepting, drain, Data),
    {next_state, drain, Data, [{reply, From, ok}]};

handle_event({call, From}, drain, State, _Data) when State =:= drain; State =:= shutdown ->
    {keep_state_and_data, [{reply, From, ok}]};

handle_event({call, From}, drain, State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, {invalid_state, State}}}]};

%% Cancel signal (immediate abort) - Priority message
handle_event({call, From}, cancel, _State, Data) ->
    logger:warning("Server ~p received cancel signal", [Data#data.server_id]),
    emit_state_transition(_State, shutdown, Data),
    {next_state, shutdown, Data, [{reply, From, ok}]};

%% Shutdown signal
handle_event({call, From}, shutdown, _State, Data) ->
    logger:info("Server ~p shutting down", [Data#data.server_id]),
    emit_state_transition(_State, shutdown, Data),
    {next_state, shutdown, Data, [{reply, From, ok}]};

%% State name query
handle_event({call, From}, state_name, State, _Data) ->
    {keep_state_and_data, [{reply, From, State}]};

%% initialization state events
handle_event({call, From}, accept_connections, initialization, Data) ->
    logger:info("Server ~p ready to accept connections", [Data#data.server_id]),
    emit_state_transition(initialization, accepting, Data),
    {next_state, accepting, Data, [{reply, From, ok}]};

%% Initialization timeout
handle_event(info, init_timeout, initialization, Data) ->
    logger:error("Server ~p initialization timeout", [Data#data.server_id]),
    emit_state_transition(initialization, shutdown, Data),
    {next_state, shutdown, Data};

%% accepting state events
handle_event(info, {connection_established, ConnPid}, accepting, Data) ->
    logger:debug("Server ~p new connection: ~p", [Data#data.server_id, ConnPid]),
    %% Monitor connection
    erlang:monitor(process, ConnPid),
    NewConnections = [ConnPid | Data#data.active_connections],
    {keep_state, Data#data{active_connections = NewConnections}};

handle_event(info, {'DOWN', _Ref, process, ConnPid, _Reason}, State, Data)
  when State =:= accepting; State =:= drain ->
    logger:debug("Server ~p connection terminated: ~p", [Data#data.server_id, ConnPid]),
    NewConnections = lists:delete(ConnPid, Data#data.active_connections),
    NewData = Data#data{active_connections = NewConnections},

    %% If draining and no more connections, transition to shutdown
    case State of
        drain when NewConnections =:= [] ->
            logger:info("Server ~p drain complete, shutting down", [Data#data.server_id]),
            emit_state_transition(drain, shutdown, NewData),
            {next_state, shutdown, NewData};
        _ ->
            {keep_state, NewData}
    end;

%% drain state events
handle_event(info, {connection_established, _ConnPid}, drain, _Data) ->
    %% Reject new connections during drain
    logger:warning("Server ~p rejecting connection during drain", [_Data#data.server_id]),
    {keep_state_and_data, []};

handle_event(info, drain_timeout, drain, Data) ->
    logger:warning("Server ~p drain timeout, forcing shutdown (~p connections remain)",
                  [Data#data.server_id, length(Data#data.active_connections)]),
    emit_state_transition(drain, shutdown, Data),
    {next_state, shutdown, Data};

%% Catch-all for unhandled events
handle_event(EventType, Event, State, Data) ->
    logger:warning("Server ~p unhandled event ~p in state ~p: ~p",
                  [Data#data.server_id, EventType, State, Event]),
    {keep_state_and_data, []}.

-spec terminate(term(), server_state(), state_data()) -> ok.
terminate(Reason, State, Data) ->
    logger:info("Server ~p terminating in state ~p: ~p",
               [Data#data.server_id, State, Reason]),
    close_all_connections(Data),
    cleanup_transport(Data),
    cancel_init_timeout(Data),
    cancel_drain_timeout(Data),
    ok.

-spec code_change(term(), server_state(), state_data(), term()) ->
    {ok, server_state(), state_data()}.
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

-spec format_status(Opt, Status) -> term() when
    Opt :: normal | terminate,
    Status :: list().
format_status(_Opt, [_PDict, State, Data]) ->
    #{
        state => State,
        server_id => Data#data.server_id,
        active_connections => length(Data#data.active_connections),
        protocol_version => Data#data.protocol_version
    }.

%%====================================================================
%% Internal functions
%%====================================================================

-spec emit_state_transition(server_state() | undefined, server_state(), state_data()) -> ok.
emit_state_transition(OldState, NewState, Data) ->
    %% Emit state transition event for debugging/observability
    Event = #{
        type => state_transition,
        module => ?MODULE,
        server_id => Data#data.server_id,
        from_state => OldState,
        to_state => NewState,
        active_connections => length(Data#data.active_connections),
        timestamp => erlang:system_time(millisecond)
    },
    logger:debug("State transition event: ~p", [Event]),
    %% Could also send to telemetry/OTEL here
    ok.

-spec cleanup_transport(state_data()) -> ok.
cleanup_transport(#data{transport = undefined}) ->
    ok;
cleanup_transport(#data{transport = Transport, transport_state = State}) when State =/= undefined ->
    %% Call transport cleanup if available
    try
        case erlang:function_exported(Transport, close, 1) of
            true -> Transport:close(State);
            false -> ok
        end
    catch
        _:_ -> ok
    end,
    ok;
cleanup_transport(_) ->
    ok.

-spec close_all_connections(state_data()) -> ok.
close_all_connections(#data{active_connections = []}) ->
    ok;
close_all_connections(#data{active_connections = Connections}) ->
    logger:info("Closing ~p active connections", [length(Connections)]),
    lists:foreach(
        fun(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    try
                        gen_statem:stop(Pid, normal, ?SHUTDOWN_TIMEOUT_MS)
                    catch
                        _:_ -> ok
                    end;
                false ->
                    ok
            end
        end,
        Connections
    ),
    ok.

-spec cancel_init_timeout(state_data()) -> ok.
cancel_init_timeout(#data{init_timeout_ref = undefined}) ->
    ok;
cancel_init_timeout(#data{init_timeout_ref = Ref}) when is_reference(Ref) ->
    erlang:cancel_timer(Ref),
    ok;
cancel_init_timeout(_) ->
    ok.

-spec cancel_drain_timeout(state_data()) -> ok.
cancel_drain_timeout(#data{drain_timeout_ref = undefined}) ->
    ok;
cancel_drain_timeout(#data{drain_timeout_ref = Ref}) when is_reference(Ref) ->
    erlang:cancel_timer(Ref),
    ok;
cancel_drain_timeout(_) ->
    ok.
