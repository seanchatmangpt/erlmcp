-module(erlmcp_graceful_drain).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    register_connection/2,
    unregister_connection/1,
    request_drain/2,
    drain_started/0,
    drain_in_progress/0,
    get_active_connections/0,
    graceful_shutdown/2,
    force_close_all/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

% Logger macro compatibility
-ifndef(LOG_notice).
-define(LOG_notice(Msg, Meta), logger:notice(Msg, Meta)).
-endif.
-ifndef(LOG_warning).
-define(LOG_warning(Msg, Meta), logger:warning(Msg, Meta)).
-endif.
-ifndef(LOG_info).
-define(LOG_info(Msg, Meta), logger:info(Msg, Meta)).
-endif.

%% Types
-type connection_id() :: term().
-type drain_info() :: #{
    connection_id := connection_id(),
    pid := pid(),
    started_at := erlang:timestamp(),
    timeout_ref := reference() | undefined
}.

%% State record
-record(state, {
    connections = #{} :: #{connection_id() => drain_info()},
    drain_active = false :: boolean(),
    drain_start :: erlang:timestamp() | undefined,
    drain_timeout_ms = 30000 :: pos_integer(),
    drain_timeout_ref :: reference() | undefined,
    notify_pid :: pid() | undefined,
    shutdown_signal = false :: boolean()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Register a new connection that should participate in graceful drain
-spec register_connection(connection_id(), pid()) -> ok | {error, term()}.
register_connection(ConnId, Pid) ->
    gen_server:call(?MODULE, {register_connection, ConnId, Pid}, 5000).

%% Unregister a connection (typically on normal close)
-spec unregister_connection(connection_id()) -> ok.
unregister_connection(ConnId) ->
    gen_server:cast(?MODULE, {unregister_connection, ConnId}).

%% Initiate graceful drain - ask all connections to finish work
-spec request_drain(pos_integer(), pid()) -> {ok, reference()} | {error, term()}.
request_drain(TimeoutMs, NotifyPid) ->
    gen_server:call(?MODULE, {request_drain, TimeoutMs, NotifyPid}, TimeoutMs + 5000).

%% Check if drain has been started
-spec drain_started() -> boolean().
drain_started() ->
    gen_server:call(?MODULE, drain_started, 5000).

%% Check if drain is currently in progress
-spec drain_in_progress() -> boolean().
drain_in_progress() ->
    gen_server:call(?MODULE, drain_in_progress, 5000).

%% Get list of active connections
-spec get_active_connections() -> [connection_id()].
get_active_connections() ->
    gen_server:call(?MODULE, get_active_connections, 5000).

%% Graceful shutdown: drain then close all connections
-spec graceful_shutdown(pos_integer(), pid()) -> {ok, map()} | {error, term()}.
graceful_shutdown(TimeoutMs, NotifyPid) ->
    gen_server:call(?MODULE, {graceful_shutdown, TimeoutMs, NotifyPid}, TimeoutMs + 5000).

%% Force close all connections immediately
-spec force_close_all() -> ok.
force_close_all() ->
    gen_server:call(?MODULE, force_close_all, 10000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    ?LOG_info("Graceful drain coordinator starting", #{}),
    {ok, #state{}}.

handle_call({register_connection, ConnId, Pid}, _From, State) ->
    case maps:get(ConnId, State#state.connections, undefined) of
        undefined ->
            % Monitor this connection
            MonRef = erlang:monitor(process, Pid),
            DrainInfo = #{
                connection_id => ConnId,
                pid => Pid,
                started_at => erlang:now(),
                timeout_ref => undefined,
                monitor_ref => MonRef
            },
            NewConnections = (State#state.connections)#{ConnId => DrainInfo},
            {reply, ok, State#state{connections = NewConnections}};
        _Exists ->
            {reply, {error, already_registered}, State}
    end;

handle_call({request_drain, TimeoutMs, NotifyPid}, _From, State) ->
    case State#state.drain_active of
        true ->
            {reply, {error, drain_already_active}, State};
        false ->
            % Start drain: notify all connections to complete current work
            DrainRef = make_ref(),
            DrainTimeoutRef = erlang:send_after(TimeoutMs, self(), drain_timeout),
            notify_all_connections(drain_requested, State#state.connections),
            NewState = State#state{
                drain_active = true,
                drain_start = erlang:now(),
                drain_timeout_ms = TimeoutMs,
                drain_timeout_ref = DrainTimeoutRef,
                notify_pid = NotifyPid
            },
            ?LOG_notice("Graceful drain started", #{
                active_connections => maps:size(State#state.connections),
                timeout_ms => TimeoutMs
            }),
            {reply, {ok, DrainRef}, NewState}
    end;

handle_call(drain_started, _From, State) ->
    {reply, State#state.shutdown_signal, State};

handle_call(drain_in_progress, _From, State) ->
    {reply, State#state.drain_active, State};

handle_call(get_active_connections, _From, State) ->
    ConnIds = maps:keys(State#state.connections),
    {reply, ConnIds, State};

handle_call({graceful_shutdown, TimeoutMs, NotifyPid}, _From, State) ->
    case State#state.drain_active of
        true ->
            {reply, {error, drain_already_active}, State};
        false ->
            % Start drain
            DrainTimeoutRef = erlang:send_after(TimeoutMs, self(), drain_timeout),
            notify_all_connections(shutdown_requested, State#state.connections),
            NewState = State#state{
                drain_active = true,
                drain_start = erlang:now(),
                drain_timeout_ms = TimeoutMs,
                drain_timeout_ref = DrainTimeoutRef,
                notify_pid = NotifyPid,
                shutdown_signal = true
            },
            ActiveCount = maps:size(State#state.connections),
            ?LOG_notice("Graceful shutdown started", #{
                active_connections => ActiveCount,
                timeout_ms => TimeoutMs
            }),
            {reply, {ok, #{
                drain_ref => make_ref(),
                active_connections => ActiveCount,
                timeout_ms => TimeoutMs
            }}, NewState}
    end;

handle_call(force_close_all, _From, State) ->
    % Force close all connections
    maps:foreach(fun(_ConnId, #{pid := Pid}) ->
        catch erlang:exit(Pid, shutdown)
    end, State#state.connections),
    NewState = State#state{
        connections = #{},
        drain_active = false,
        drain_start = undefined
    },
    ?LOG_warning("Forced close all connections", #{}),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({unregister_connection, ConnId}, State) ->
    case maps:get(ConnId, State#state.connections, undefined) of
        undefined ->
            {noreply, State};
        #{monitor_ref := MonRef} ->
            % Unmonitor the connection
            erlang:demonitor(MonRef, [flush]),
            NewConnections = maps:remove(ConnId, State#state.connections),
            NewState = State#state{connections = NewConnections},

            % If drain is active and no more connections, signal completion
            case State#state.drain_active andalso maps:size(NewConnections) =:= 0 of
                true ->
                    notify_drain_complete(NewState);
                false ->
                    ok
            end,

            {noreply, NewState}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MonRef, process, _Pid, _Reason}, State) ->
    % A monitored connection died - remove it
    NewConnections = maps:filter(fun(_ConnId, #{monitor_ref := MRef}) ->
        MRef =/= MonRef
    end, State#state.connections),
    NewState = State#state{connections = NewConnections},

    % If drain is active and no more connections, signal completion
    case State#state.drain_active andalso maps:size(NewConnections) =:= 0 of
        true ->
            notify_drain_complete(NewState);
        false ->
            ok
    end,

    {noreply, NewState};

handle_info(drain_timeout, State) ->
    % Drain timeout reached - force close remaining connections
    case State#state.drain_active of
        true ->
            ElapsedMs = round(timer:now_diff(erlang:now(), State#state.drain_start) / 1000),
            RemainingCount = maps:size(State#state.connections),
            ?LOG_warning("Drain timeout reached", #{
                elapsed_ms => ElapsedMs,
                remaining_connections => RemainingCount
            }),

            % Force close remaining connections
            maps:foreach(fun(_ConnId, #{pid := Pid}) ->
                catch erlang:exit(Pid, shutdown)
            end, State#state.connections),

            NewState = State#state{
                connections = #{},
                drain_active = false,
                drain_start = undefined,
                drain_timeout_ref = undefined
            },

            notify_drain_timeout(NewState);
        false ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?LOG_info("Graceful drain coordinator stopping", #{}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

notify_all_connections(Message, Connections) ->
    maps:foreach(fun(_ConnId, #{pid := Pid}) ->
        catch Pid ! Message
    end, Connections).

notify_drain_complete(State) ->
    case State#state.notify_pid of
        undefined ->
            ok;
        NotifyPid ->
            ElapsedMs = round(timer:now_diff(erlang:now(), State#state.drain_start) / 1000),
            NotifyPid ! {drain_complete, #{
                elapsed_ms => ElapsedMs,
                connections_preserved => 0
            }}
    end.

notify_drain_timeout(State) ->
    case State#state.notify_pid of
        undefined ->
            ok;
        NotifyPid ->
            ElapsedMs = round(timer:now_diff(erlang:now(), State#state.drain_start) / 1000),
            NotifyPid ! {drain_timeout, #{
                elapsed_ms => ElapsedMs
            }}
    end.
