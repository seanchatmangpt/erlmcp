%%%-------------------------------------------------------------------
%%% @doc
%%% Client lifecycle FSM for erlmcp
%%% Implements the client connection state machine following Armstrong principles.
%%% States: pre_initialization -> initializing -> initialized -> disconnected
%%% Uses gen_statem with handle_event_function for priority message handling.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_client_fsm).

-behaviour(gen_statem).

-include("erlmcp.hrl").

%% API exports
-export([start_link/1, start_link/2, connect/1, initialize/2, disconnect/1, reconnect/1,
         state_name/1, stop/1]).
%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4, format_status/1]).

%% State names for type specs
-type client_state() :: pre_initialization | initializing | initialized | error | disconnected.
-type client_id() :: term().

-export_type([client_state/0, client_id/0]).

%% State data record
-record(data,
        {client_id :: client_id(),
         transport :: module() | undefined,
         transport_state :: term() | undefined,
         capabilities :: #mcp_server_capabilities{} | undefined,
         protocol_version :: binary() | undefined,
         init_params :: map() | undefined,
         error_reason :: term() | undefined,
         reconnect_attempts = 0 :: non_neg_integer(),
         max_reconnect_attempts = 5 :: pos_integer(),
         reconnect_delay_ms = 1000 :: pos_integer(),
         auto_reconnect = true :: boolean(),
         timeout_ref :: reference() | undefined,
         options :: map()}).

-type state_data() :: #data{}.

%% Timeout for initialization phase (OTP 28 priority message support)
-define(INIT_TIMEOUT_MS, 30000).
-define(CONNECT_TIMEOUT_MS, 5000).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(client_id()) -> {ok, pid()} | {error, term()}.
start_link(ClientId) ->
    start_link(ClientId, #{}).

-spec start_link(client_id(), map()) -> {ok, pid()} | {error, term()}.
start_link(ClientId, Options) ->
    gen_statem:start_link(?MODULE, [ClientId, Options], []).

-spec connect(pid()) -> ok | {error, term()}.
connect(Pid) ->
    gen_statem:call(Pid, connect, ?CONNECT_TIMEOUT_MS).

-spec initialize(pid(), map()) -> ok | {error, term()}.
initialize(Pid, Params) ->
    gen_statem:call(Pid, {initialize, Params}, ?INIT_TIMEOUT_MS).

-spec disconnect(pid()) -> ok.
disconnect(Pid) ->
    gen_statem:call(Pid, disconnect).

-spec reconnect(pid()) -> ok | {error, term()}.
reconnect(Pid) ->
    gen_statem:call(Pid, reconnect).

-spec state_name(pid()) -> client_state().
state_name(Pid) ->
    gen_statem:call(Pid, state_name).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_statem:stop(Pid).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

-spec init([client_id() | map()]) -> {ok, client_state(), state_data()}.
init([ClientId, Options]) ->
    %% No blocking operations in init/1 - Armstrong principle
    logger:info("Client FSM ~p initializing", [ClientId]),

    Data =
        #data{client_id = ClientId,
              options = Options,
              max_reconnect_attempts = maps:get(max_reconnect_attempts, Options, 5),
              reconnect_delay_ms = maps:get(reconnect_delay_ms, Options, 1000),
              auto_reconnect = maps:get(auto_reconnect, Options, true)},

    %% Emit state transition event for debugging
    emit_state_transition(undefined, pre_initialization, Data),

    {ok, pre_initialization, Data}.

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    %% Use handle_event_function for unified event handling
    %% state_enter for state entry actions
    [handle_event_function, state_enter].

%%====================================================================
%% State callback - handle_event function
%%====================================================================

%% State enter calls - executed when entering a state
handle_event(enter, OldState, State, Data) ->
    emit_state_transition(OldState, State, Data),
    logger:debug("Client ~p: ~p -> ~p", [Data#data.client_id, OldState, State]),

    %% State-specific entry actions
    case State of
        pre_initialization ->
            {keep_state_and_data, []};
        initializing ->
            %% Set initialization timeout
            TimeoutRef = erlang:send_after(?INIT_TIMEOUT_MS, self(), init_timeout),
            {keep_state, Data#data{timeout_ref = TimeoutRef}};
        initialized ->
            %% Cancel any pending timeout
            cancel_timeout(Data),
            {keep_state, Data#data{timeout_ref = undefined, reconnect_attempts = 0}};
        error ->
            %% Schedule reconnect if auto_reconnect enabled
            case Data#data.auto_reconnect of
                true ->
                    schedule_reconnect(Data);
                false ->
                    {keep_state_and_data, []}
            end;
        disconnected ->
            %% Clean up transport
            cleanup_transport(Data),
            {keep_state, Data#data{transport_state = undefined}};
        _ ->
            {keep_state_and_data, []}
    end;
%% Priority messages (OTP 28) - bypass normal queue for control signals
%% Health check signal
handle_event({call, From}, {health_check}, _State, _Data) ->
    {keep_state_and_data, [{reply, From, ok}]};
%% Drain signal (graceful shutdown)
handle_event({call, From}, drain, State, Data) ->
    logger:info("Client ~p draining from state ~p", [Data#data.client_id, State]),
    cleanup_transport(Data),
    emit_state_transition(State, disconnected, Data),
    {next_state, disconnected, Data, [{reply, From, ok}]};
%% Cancel signal (immediate abort)
handle_event({call, From}, cancel, State, Data) ->
    logger:warning("Client ~p cancelled from state ~p", [Data#data.client_id, State]),
    cleanup_transport(Data),
    emit_state_transition(State, disconnected, Data),
    {next_state, disconnected, Data, [{reply, From, ok}]};
%% State name query
handle_event({call, From}, state_name, State, _Data) ->
    {keep_state_and_data, [{reply, From, State}]};
%% pre_initialization state events
handle_event({call, From}, connect, pre_initialization, Data) ->
    %% Transition to initializing and perform connection
    logger:info("Client ~p connecting", [Data#data.client_id]),
    emit_state_transition(pre_initialization, initializing, Data),
    {next_state, initializing, Data, [{reply, From, ok}]};
handle_event({call, From}, connect, State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, {invalid_state, State}}}]};
%% initializing state events
handle_event({call, From}, {initialize, Params}, initializing, Data) ->
    %% Perform initialization (should be delegated to async process in real implementation)
    case perform_initialization(Params, Data) of
        {ok, NewData} ->
            logger:info("Client ~p initialized successfully", [NewData#data.client_id]),
            emit_state_transition(initializing, initialized, NewData),
            {next_state, initialized, NewData, [{reply, From, ok}]};
        {error, Reason} = Error ->
            logger:error("Client ~p initialization failed: ~p", [Data#data.client_id, Reason]),
            ErrorData = Data#data{error_reason = Reason},
            emit_state_transition(initializing, error, ErrorData),
            {next_state, error, ErrorData, [{reply, From, Error}]}
    end;
handle_event({call, From}, {initialize, _Params}, State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, {invalid_state, State}}}]};
%% Initialization timeout
handle_event(info, init_timeout, initializing, Data) ->
    logger:error("Client ~p initialization timeout", [Data#data.client_id]),
    ErrorData = Data#data{error_reason = init_timeout},
    emit_state_transition(initializing, error, ErrorData),
    {next_state, error, ErrorData};
%% initialized state events
handle_event({call, From}, disconnect, initialized, Data) ->
    logger:info("Client ~p disconnecting", [Data#data.client_id]),
    cleanup_transport(Data),
    emit_state_transition(initialized, disconnected, Data),
    {next_state, disconnected, Data, [{reply, From, ok}]};
%% disconnected state events
handle_event({call, From}, reconnect, disconnected, Data) ->
    logger:info("Client ~p reconnecting", [Data#data.client_id]),
    emit_state_transition(disconnected, initializing, Data),
    {next_state, initializing, Data, [{reply, From, ok}]};
%% error state events
handle_event(info, retry_connect, error, Data) ->
    case Data#data.reconnect_attempts < Data#data.max_reconnect_attempts of
        true ->
            logger:info("Client ~p retrying connection (attempt ~p/~p)",
                        [Data#data.client_id,
                         Data#data.reconnect_attempts + 1,
                         Data#data.max_reconnect_attempts]),
            NewData = Data#data{reconnect_attempts = Data#data.reconnect_attempts + 1},
            emit_state_transition(error, initializing, NewData),
            {next_state, initializing, NewData};
        false ->
            logger:error("Client ~p max reconnect attempts exceeded", [Data#data.client_id]),
            {keep_state_and_data, []}
    end;
%% Catch-all for unhandled events
handle_event(EventType, Event, State, Data) ->
    logger:warning("Client ~p unhandled event ~p in state ~p: ~p",
                   [Data#data.client_id, EventType, State, Event]),
    {keep_state_and_data, []}.

-spec terminate(term(), client_state(), state_data()) -> ok.
terminate(Reason, State, Data) ->
    logger:info("Client ~p terminating in state ~p: ~p", [Data#data.client_id, State, Reason]),
    cleanup_transport(Data),
    cancel_timeout(Data),
    ok.

-spec code_change(term(), client_state(), state_data(), term()) ->
                     {ok, client_state(), state_data()}.
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

-spec format_status(term()) -> map().
format_status([_PDict, State, Data]) ->
    #{state => State,
      client_id => Data#data.client_id,
      reconnect_attempts => Data#data.reconnect_attempts,
      error_reason => Data#data.error_reason}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec emit_state_transition(client_state() | undefined, client_state(), state_data()) -> ok.
emit_state_transition(OldState, NewState, Data) ->
    %% Emit state transition event for debugging/observability
    Event =
        #{type => state_transition,
          module => ?MODULE,
          client_id => Data#data.client_id,
          from_state => OldState,
          to_state => NewState,
          timestamp => erlang:system_time(millisecond)},
    logger:debug("State transition event: ~p", [Event]),
    %% Could also send to telemetry/OTEL here
    ok.

-spec perform_initialization(map(), state_data()) -> {ok, state_data()} | {error, term()}.
perform_initialization(Params, Data) ->
    %% This is a simplified version - real implementation would:
    %% 1. Establish transport connection
    %% 2. Negotiate protocol version
    %% 3. Exchange capabilities
    %% 4. Validate initialization parameters
    %% For now, just store the params
    try
        NewData =
            Data#data{init_params = Params,
                      capabilities = maps:get(capabilities, Params, #mcp_server_capabilities{}),
                      protocol_version = maps:get(protocol_version, Params, ?MCP_VERSION)},
        {ok, NewData}
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec cleanup_transport(state_data()) -> ok.
cleanup_transport(#data{transport = undefined}) ->
    ok;
cleanup_transport(#data{transport = Transport, transport_state = State}) when State =/= undefined ->
    %% Call transport cleanup if available
    try
        case erlang:function_exported(Transport, close, 1) of
            true ->
                Transport:close(State);
            false ->
                ok
        end
    catch
        _:_ ->
            ok
    end,
    ok;
cleanup_transport(_) ->
    ok.

-spec cancel_timeout(state_data()) -> ok.
cancel_timeout(#data{timeout_ref = undefined}) ->
    ok;
cancel_timeout(#data{timeout_ref = Ref}) when is_reference(Ref) ->
    erlang:cancel_timer(Ref),
    ok;
cancel_timeout(_) ->
    ok.

-spec schedule_reconnect(state_data()) -> gen_statem:event_handler_result(client_state()).
schedule_reconnect(Data) ->
    case Data#data.reconnect_attempts < Data#data.max_reconnect_attempts of
        true ->
            %% Exponential backoff
            Delay = Data#data.reconnect_delay_ms * (1 bsl Data#data.reconnect_attempts),
            logger:info("Client ~p scheduling reconnect in ~pms", [Data#data.client_id, Delay]),
            {keep_state_and_data, [{state_timeout, Delay, retry_connect}]};
        false ->
            logger:error("Client ~p max reconnect attempts reached", [Data#data.client_id]),
            {keep_state_and_data, []}
    end.
