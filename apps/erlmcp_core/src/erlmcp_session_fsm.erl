%%%-------------------------------------------------------------------
%%% @doc
%%% Session lifecycle FSM for erlmcp
%%% Implements per-session state machine following Armstrong principles.
%%% States: negotiation -> active -> suspended -> closed
%%% Uses gen_statem with handle_event_function for priority message handling.
%%% Acts as supervisor for per-connection processes.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_session_fsm).

-behaviour(gen_statem).

-include("erlmcp.hrl").

%% API exports
-export([start_link/1, start_link/2, negotiate/2, activate/1, suspend/1, resume/1, close/1,
         state_name/1, add_connection/2, remove_connection/2, get_connections/1, stop/1]).
%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4, format_status/2]).

%% State names for type specs
-type session_state() :: negotiation | active | suspended | closed.
-type session_id() :: binary().
-type connection_id() :: term().

-export_type([session_state/0, session_id/0]).

%% State data record
-record(data,
        {session_id :: session_id(),
         capabilities :: #mcp_server_capabilities{} | undefined,
         client_capabilities :: #mcp_client_capabilities{} | undefined,
         protocol_version :: binary() | undefined,
         connections = #{} :: #{connection_id() => pid()},
         monitors = #{} :: #{pid() => reference()},
         metadata = #{} :: map(),
         created_at :: integer(),
         last_activity :: integer(),
         timeout_ms :: pos_integer() | infinity,
         suspend_reason :: term() | undefined,
         negotiation_timeout_ref :: reference() | undefined,
         idle_timeout_ref :: reference() | undefined,
         options :: map()}).

-type state_data() :: #data{}.

%% Timeouts
-define(NEGOTIATION_TIMEOUT_MS, 30000).
-define(DEFAULT_IDLE_TIMEOUT_MS, 300000).  % 5 minutes
-define(SUSPEND_TIMEOUT_MS, 600000).  % 10 minutes

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(session_id()) -> {ok, pid()} | {error, term()}.
start_link(SessionId) ->
    start_link(SessionId, #{}).

-spec start_link(session_id(), map()) -> {ok, pid()} | {error, term()}.
start_link(SessionId, Options) ->
    gen_statem:start_link(?MODULE, [SessionId, Options], []).

-spec negotiate(pid(), map()) -> ok | {error, term()}.
negotiate(Pid, Params) ->
    gen_statem:call(Pid, {negotiate, Params}, ?NEGOTIATION_TIMEOUT_MS).

-spec activate(pid()) -> ok | {error, term()}.
activate(Pid) ->
    gen_statem:call(Pid, activate).

-spec suspend(pid()) -> ok.
suspend(Pid) ->
    gen_statem:call(Pid, suspend).

-spec resume(pid()) -> ok | {error, term()}.
resume(Pid) ->
    gen_statem:call(Pid, resume).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_statem:call(Pid, close).

-spec state_name(pid()) -> session_state().
state_name(Pid) ->
    gen_statem:call(Pid, state_name).

-spec add_connection(pid(), {connection_id(), pid()}) -> ok.
add_connection(Pid, {ConnId, ConnPid}) ->
    gen_statem:call(Pid, {add_connection, ConnId, ConnPid}).

-spec remove_connection(pid(), connection_id()) -> ok.
remove_connection(Pid, ConnId) ->
    gen_statem:call(Pid, {remove_connection, ConnId}).

-spec get_connections(pid()) -> #{connection_id() => pid()}.
get_connections(Pid) ->
    gen_statem:call(Pid, get_connections).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_statem:stop(Pid).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

-spec init([session_id() | map()]) -> {ok, session_state(), state_data()}.
init([SessionId, Options]) ->
    %% No blocking operations in init/1 - Armstrong principle
    logger:info("Session FSM ~p initializing", [SessionId]),

    Now = erlang:system_time(millisecond),
    Data =
        #data{session_id = SessionId,
              created_at = Now,
              last_activity = Now,
              timeout_ms = maps:get(timeout_ms, Options, infinity),
              metadata = maps:get(metadata, Options, #{}),
              options = Options},

    %% Emit state transition event for debugging
    emit_state_transition(undefined, negotiation, Data),

    {ok, negotiation, Data}.

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
    logger:debug("Session ~p: ~p -> ~p", [Data#data.session_id, OldState, State]),

    %% State-specific entry actions
    case State of
        negotiation ->
            %% Set negotiation timeout
            TimeoutRef = erlang:send_after(?NEGOTIATION_TIMEOUT_MS, self(), negotiation_timeout),
            {keep_state, Data#data{negotiation_timeout_ref = TimeoutRef}};
        active ->
            %% Cancel negotiation timeout, set idle timeout if configured
            cancel_negotiation_timeout(Data),
            NewData = Data#data{negotiation_timeout_ref = undefined},
            case Data#data.timeout_ms of
                infinity ->
                    {keep_state, NewData};
                TimeoutMs when is_integer(TimeoutMs) ->
                    IdleRef = erlang:send_after(TimeoutMs, self(), idle_timeout),
                    {keep_state, NewData#data{idle_timeout_ref = IdleRef}}
            end;
        suspended ->
            %% Cancel idle timeout
            cancel_idle_timeout(Data),
            {keep_state, Data#data{idle_timeout_ref = undefined}};
        closed ->
            %% Cleanup all resources
            cancel_negotiation_timeout(Data),
            cancel_idle_timeout(Data),
            close_all_connections(Data),
            {keep_state, Data#data{connections = #{}, monitors = #{}}};
        _ ->
            {keep_state_and_data, []}
    end;
%% Priority messages (OTP 28) - bypass normal queue for control signals
%% Health check signal
handle_event({call, From}, {health_check}, State, Data) ->
    Health =
        #{state => State,
          session_id => Data#data.session_id,
          connections => maps:size(Data#data.connections),
          last_activity => Data#data.last_activity},
    {keep_state_and_data, [{reply, From, {ok, Health}}]};
%% Cancel signal (immediate abort) - Priority message
handle_event({call, From}, cancel, _State, Data) ->
    logger:warning("Session ~p received cancel signal", [Data#data.session_id]),
    emit_state_transition(_State, closed, Data),
    {next_state, closed, Data, [{reply, From, ok}]};
%% Close signal
handle_event({call, From}, close, _State, Data) ->
    logger:info("Session ~p closing", [Data#data.session_id]),
    emit_state_transition(_State, closed, Data),
    {next_state, closed, Data, [{reply, From, ok}]};
%% State name query
handle_event({call, From}, state_name, State, _Data) ->
    {keep_state_and_data, [{reply, From, State}]};
%% Get connections query
handle_event({call, From}, get_connections, _State, Data) ->
    {keep_state_and_data, [{reply, From, Data#data.connections}]};
%% negotiation state events
handle_event({call, From}, {negotiate, Params}, negotiation, Data) ->
    case perform_negotiation(Params, Data) of
        {ok, NewData} ->
            logger:info("Session ~p negotiation complete", [NewData#data.session_id]),
            emit_state_transition(negotiation, active, NewData),
            {next_state, active, NewData, [{reply, From, ok}]};
        {error, Reason} = Error ->
            logger:error("Session ~p negotiation failed: ~p", [Data#data.session_id, Reason]),
            emit_state_transition(negotiation, closed, Data),
            {next_state, closed, Data, [{reply, From, Error}]}
    end;
handle_event({call, From}, {negotiate, _Params}, State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, {invalid_state, State}}}]};
%% Negotiation timeout
handle_event(info, negotiation_timeout, negotiation, Data) ->
    logger:error("Session ~p negotiation timeout", [Data#data.session_id]),
    emit_state_transition(negotiation, closed, Data),
    {next_state, closed, Data};
%% active state events
handle_event({call, From}, activate, negotiation, _Data) ->
    %% Can activate after successful negotiation
    {keep_state_and_data, [{reply, From, ok}]};
handle_event({call, From}, activate, active, _Data) ->
    %% Already active
    {keep_state_and_data, [{reply, From, ok}]};
handle_event({call, From}, suspend, active, Data) ->
    logger:info("Session ~p suspending", [Data#data.session_id]),
    emit_state_transition(active, suspended, Data),
    {next_state, suspended, Data, [{reply, From, ok}]};
%% Connection management (works in any state except closed)
handle_event({call, From}, {add_connection, ConnId, ConnPid}, State, Data) when State =/= closed ->
    %% Monitor the connection process
    MonRef = erlang:monitor(process, ConnPid),
    NewConnections = maps:put(ConnId, ConnPid, Data#data.connections),
    NewMonitors = maps:put(ConnPid, MonRef, Data#data.monitors),
    NewData =
        Data#data{connections = NewConnections,
                  monitors = NewMonitors,
                  last_activity = erlang:system_time(millisecond)},
    logger:debug("Session ~p added connection ~p", [Data#data.session_id, ConnId]),
    reset_idle_timeout(NewData),
    {keep_state, NewData, [{reply, From, ok}]};
handle_event({call, From}, {remove_connection, ConnId}, State, Data) when State =/= closed ->
    case maps:get(ConnId, Data#data.connections, undefined) of
        undefined ->
            {keep_state_and_data, [{reply, From, {error, not_found}}]};
        ConnPid ->
            %% Demonitor and remove
            case maps:get(ConnPid, Data#data.monitors, undefined) of
                undefined ->
                    ok;
                MonRef ->
                    erlang:demonitor(MonRef, [flush])
            end,
            NewConnections = maps:remove(ConnId, Data#data.connections),
            NewMonitors = maps:remove(ConnPid, Data#data.monitors),
            NewData =
                Data#data{connections = NewConnections,
                          monitors = NewMonitors,
                          last_activity = erlang:system_time(millisecond)},
            logger:debug("Session ~p removed connection ~p", [Data#data.session_id, ConnId]),
            {keep_state, NewData, [{reply, From, ok}]}
    end;
%% Handle connection process death
handle_event(info, {'DOWN', MonRef, process, ConnPid, Reason}, State, Data) when State =/= closed ->
    %% Find and remove the dead connection
    ConnId = find_connection_id(ConnPid, Data#data.connections),
    logger:info("Session ~p connection ~p terminated: ~p", [Data#data.session_id, ConnId, Reason]),

    NewConnections =
        case ConnId of
            undefined ->
                Data#data.connections;
            _ ->
                maps:remove(ConnId, Data#data.connections)
        end,
    NewMonitors = maps:remove(ConnPid, Data#data.monitors),
    NewData = Data#data{connections = NewConnections, monitors = NewMonitors},

    %% If no more connections, might want to close session
    case {State, maps:size(NewConnections)} of
        {active, 0} ->
            %% Optionally suspend instead of close
            logger:info("Session ~p has no connections, suspending", [Data#data.session_id]),
            emit_state_transition(active, suspended, NewData),
            {next_state, suspended, NewData};
        _ ->
            {keep_state, NewData}
    end;
%% suspended state events
handle_event({call, From}, resume, suspended, Data) ->
    logger:info("Session ~p resuming", [Data#data.session_id]),
    emit_state_transition(suspended, active, Data),
    {next_state, active, Data, [{reply, From, ok}]};
%% Idle timeout
handle_event(info, idle_timeout, active, Data) ->
    logger:info("Session ~p idle timeout, suspending", [Data#data.session_id]),
    emit_state_transition(active, suspended, Data),
    {next_state, suspended, Data};
%% Catch-all for unhandled events
handle_event(EventType, Event, State, Data) ->
    logger:warning("Session ~p unhandled event ~p in state ~p: ~p",
                   [Data#data.session_id, EventType, State, Event]),
    {keep_state_and_data, []}.

-spec terminate(term(), session_state(), state_data()) -> ok.
terminate(Reason, State, Data) ->
    logger:info("Session ~p terminating in state ~p: ~p", [Data#data.session_id, State, Reason]),
    close_all_connections(Data),
    cancel_negotiation_timeout(Data),
    cancel_idle_timeout(Data),
    ok.

-spec code_change(term(), session_state(), state_data(), term()) ->
                     {ok, session_state(), state_data()}.
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

-spec format_status(Opt, Status) -> term()
    when Opt :: normal | terminate,
         Status :: list().
format_status(_Opt, [_PDict, State, Data]) ->
    #{state => State,
      session_id => Data#data.session_id,
      connections => maps:size(Data#data.connections),
      created_at => Data#data.created_at,
      last_activity => Data#data.last_activity}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec emit_state_transition(session_state() | undefined, session_state(), state_data()) -> ok.
emit_state_transition(OldState, NewState, Data) ->
    %% Emit state transition event for debugging/observability
    Event =
        #{type => state_transition,
          module => ?MODULE,
          session_id => Data#data.session_id,
          from_state => OldState,
          to_state => NewState,
          connections => maps:size(Data#data.connections),
          timestamp => erlang:system_time(millisecond)},
    logger:debug("State transition event: ~p", [Event]),
    %% Could also send to telemetry/OTEL here
    ok.

-spec perform_negotiation(map(), state_data()) -> {ok, state_data()} | {error, term()}.
perform_negotiation(Params, Data) ->
    %% Perform capability negotiation
    try
        NewData =
            Data#data{capabilities =
                          maps:get(server_capabilities, Params, #mcp_server_capabilities{}),
                      client_capabilities =
                          maps:get(client_capabilities, Params, #mcp_client_capabilities{}),
                      protocol_version = maps:get(protocol_version, Params, ?MCP_VERSION),
                      last_activity = erlang:system_time(millisecond)},
        {ok, NewData}
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec close_all_connections(state_data()) -> ok.
close_all_connections(#data{connections = Connections, monitors = Monitors}) ->
    %% Demonitor all
    maps:foreach(fun(_Pid, MonRef) -> erlang:demonitor(MonRef, [flush]) end, Monitors),

    %% Close all connections gracefully
    maps:foreach(fun(_ConnId, ConnPid) ->
                    case is_process_alive(ConnPid) of
                        true ->
                            try
                                gen_statem:stop(ConnPid, normal, 5000)
                            catch
                                _:_ ->
                                    ok
                            end;
                        false ->
                            ok
                    end
                 end,
                 Connections),
    ok.

-spec find_connection_id(pid(), #{connection_id() => pid()}) -> connection_id() | undefined.
find_connection_id(Pid, Connections) ->
    case maps:fold(fun(ConnId, ConnPid, Acc) ->
                      case ConnPid of
                          Pid ->
                              ConnId;
                          _ ->
                              Acc
                      end
                   end,
                   undefined,
                   Connections)
    of
        undefined ->
            undefined;
        ConnId ->
            ConnId
    end.

-spec cancel_negotiation_timeout(state_data()) -> ok.
cancel_negotiation_timeout(#data{negotiation_timeout_ref = undefined}) ->
    ok;
cancel_negotiation_timeout(#data{negotiation_timeout_ref = Ref}) when is_reference(Ref) ->
    erlang:cancel_timer(Ref),
    ok;
cancel_negotiation_timeout(_) ->
    ok.

-spec cancel_idle_timeout(state_data()) -> ok.
cancel_idle_timeout(#data{idle_timeout_ref = undefined}) ->
    ok;
cancel_idle_timeout(#data{idle_timeout_ref = Ref}) when is_reference(Ref) ->
    erlang:cancel_timer(Ref),
    ok;
cancel_idle_timeout(_) ->
    ok.

-spec reset_idle_timeout(state_data()) -> ok.
reset_idle_timeout(Data) ->
    %% Cancel existing timeout and set new one
    cancel_idle_timeout(Data),
    case Data#data.timeout_ms of
        infinity ->
            ok;
        TimeoutMs when is_integer(TimeoutMs) ->
            _IdleRef = erlang:send_after(TimeoutMs, self(), idle_timeout),
            ok
    end.
