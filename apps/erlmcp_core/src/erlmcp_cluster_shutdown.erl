%%%-------------------------------------------------------------------
%%% @doc erlmcp_cluster_shutdown - Graceful Node Shutdown
%%%
%%% Coordinates graceful shutdown with other cluster nodes:
%%% - Session migration before shutdown
%%% - Connection draining
%%% - Lock release
%%% - Cluster departure announcement
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cluster_shutdown).

-behaviour(gen_server).

%% API
-export([start_link/0,
         initiate_shutdown/0, initiate_shutdown/1,
         register_shutdown_hook/1,
         unregister_shutdown_hook/1,
         migrate_sessions/0,
         drain_connections/0,
         announce_departure/0,
         get_shutdown_status/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_SHUTDOWN_TIMEOUT, 30000).
-define(DRAIN_TIMEOUT, 10000).
-define(MIGRATION_TIMEOUT, 15000).

%%====================================================================
%% Types
%%====================================================================

-type shutdown_reason() :: normal | maintenance | upgrade | emergency.
-type shutdown_phase() :: initiating | draining | migrating | announcing | complete.
-type shutdown_hook() :: {module(), atom(), [term()]}.

-record(shutdown_state, {
    phase :: shutdown_phase() | undefined,
    reason :: shutdown_reason() | undefined,
    start_time :: integer() | undefined,
    timeout :: pos_integer(),
    hooks = [] :: [shutdown_hook()],
    sessions_migrated = false :: boolean(),
    connections_drained = false :: boolean(),
    departure_announced = false :: boolean()}).

-record(state, {
    shutdown_state :: shutdown_phase() | undefined,
    current_shutdown :: #shutdown_state{} | undefined,
    hooks = [] :: [shutdown_hook()]}).

-type state() :: #state{}.

-export_type([shutdown_reason/0, shutdown_phase/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start shutdown coordinator
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Initiate graceful shutdown with reason 'normal'
-spec initiate_shutdown() -> ok | {error, term()}.
initiate_shutdown() ->
    initiate_shutdown(normal).

%% @doc Initiate graceful shutdown with specific reason
-spec initiate_shutdown(shutdown_reason()) -> ok | {error, term()}.
initiate_shutdown(Reason) when Reason =:= normal; Reason =:= maintenance;
                             Reason =:= upgrade; Reason =:= emergency ->
    gen_server:call(?SERVER, {initiate_shutdown, Reason}, ?DEFAULT_SHUTDOWN_TIMEOUT).

%% @doc Register a shutdown hook (called during shutdown)
-spec register_shutdown_hook(shutdown_hook()) -> ok.
register_shutdown_hook(Hook) when is_tuple(Hook), tuple_size(Hook) >= 2 ->
    gen_server:call(?SERVER, {register_hook, Hook}).

%% @doc Unregister a shutdown hook
-spec unregister_shutdown_hook(shutdown_hook()) -> ok.
unregister_shutdown_hook(Hook) ->
    gen_server:call(?SERVER, {unregister_hook, Hook}).

%% @doc Migrate sessions to other nodes
-spec migrate_sessions() -> {ok, non_neg_integer()} | {error, term()}.
migrate_sessions() ->
    gen_server:call(?SERVER, migrate_sessions, ?MIGRATION_TIMEOUT).

%% @doc Drain connections (stop accepting new ones)
-spec drain_connections() -> ok.
drain_connections() ->
    gen_server:call(?SERVER, drain_connections, ?DRAIN_TIMEOUT).

%% @doc Announce departure to cluster
-spec announce_departure() -> ok.
announce_departure() ->
    gen_server:call(?SERVER, announce_departure, 5000).

%% @doc Get current shutdown status
-spec get_shutdown_status() -> map().
get_shutdown_status() ->
    gen_server:call(?SERVER, get_shutdown_status).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    ?LOG_INFO("Starting cluster shutdown coordinator"),
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {reply, term(), state(), hibernate}.
handle_call({initiate_shutdown, Reason}, _From, State) ->
    case State#state.current_shutdown of
        undefined ->
            ShutdownState = #shutdown_state{
                phase = initiating,
                reason = Reason,
                start_time = erlang:system_time(millisecond),
                timeout = ?DEFAULT_SHUTDOWN_TIMEOUT
            },
            {ok, NewState} = do_shutdown(ShutdownState, State),
            {reply, ok, NewState, hibernate};
        _ ->
            {reply, {error, shutdown_in_progress}, State, hibernate}
    end;

handle_call({register_hook, Hook}, _From, State) ->
    Hooks = lists:usort([Hook | State#state.hooks]),
    {reply, ok, State#state{hooks = Hooks}, hibernate};

handle_call({unregister_hook, Hook}, _From, State) ->
    Hooks = lists:delete(Hook, State#state.hooks),
    {reply, ok, State#state{hooks = Hooks}, hibernate};

handle_call(migrate_sessions, _From, State) ->
    case migrate_sessions_to_backup() of
        {ok, Count} ->
            ?LOG_INFO("Migrated ~p sessions", [Count]),
            {reply, {ok, Count}, State, hibernate};
        {error, Reason} ->
            {reply, {error, Reason}, State, hibernate}
    end;

handle_call(drain_connections, _From, State) ->
    Result = do_drain_connections(),
    {reply, Result, State, hibernate};

handle_call(announce_departure, _From, State) ->
    Result = do_announce_departure(),
    {reply, Result, State, hibernate};

handle_call(get_shutdown_status, _From, State) ->
    Status = case State#state.current_shutdown of
        undefined ->
            #{status => not_shutting_down};
        ShutdownState ->
            #{
                phase => ShutdownState#shutdown_state.phase,
                reason => ShutdownState#shutdown_state.reason,
                start_time => ShutdownState#shutdown_state.start_time,
                sessions_migrated => ShutdownState#shutdown_state.sessions_migrated,
                connections_drained => ShutdownState#shutdown_state.connections_drained,
                departure_announced => ShutdownState#shutdown_state.departure_announced
            }
    end,
    {reply, Status, State, hibernate};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ?LOG_INFO("Cluster shutdown coordinator terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Execute shutdown sequence
-spec do_shutdown(#shutdown_state{}, state()) -> {ok, state()}.
do_shutdown(ShutdownState, State) ->
    % Execute shutdown in phases
    {ok, State1} = phase_initiating(ShutdownState, State),
    {ok, State2} = phase_draining(State1),
    {ok, State3} = phase_migrating(State2),
    {ok, State4} = phase_announcing(State3),
    {ok, State5} = phase_complete(State4),

    % Run shutdown hooks
    run_shutdown_hooks(State5#state.hooks),

    {ok, State5}.

%% @doc Phase 1: Initiating
-spec phase_initiating(#shutdown_state{}, state()) -> {ok, state()}.
phase_initiating(ShutdownState, State) ->
    ?LOG_INFO("Initiating shutdown: reason=~p", [ShutdownState#shutdown_state.reason]),

    % Notify local processes
    notify_shutdown_initiated(ShutdownState#shutdown_state.reason),

    NewShutdownState = ShutdownState#shutdown_state{phase = initiating},
    {ok, State#state{current_shutdown = NewShutdownState}}.

%% @doc Phase 2: Draining connections
-spec phase_draining(state()) -> {ok, state()}.
phase_draining(State) ->
    ?LOG_INFO("Draining connections..."),

    _ = do_drain_connections(),

    CurrentShutdown = State#state.current_shutdown,
    NewShutdownState = CurrentShutdown#shutdown_state{
        phase = draining,
        connections_drained = true
    },
    {ok, State#state{current_shutdown = NewShutdownState}}.

%% @doc Phase 3: Migrating sessions
-spec phase_migrating(state()) -> {ok, state()}.
phase_migrating(State) ->
    ?LOG_INFO("Migrating sessions..."),

    case migrate_sessions_to_backup() of
        {ok, Count} ->
            ?LOG_INFO("Migrated ~p sessions", [Count]);
        {error, Reason} ->
            ?LOG_WARNING("Session migration failed: ~p", [Reason])
    end,

    CurrentShutdown = State#state.current_shutdown,
    NewShutdownState = CurrentShutdown#shutdown_state{
        phase = migrating,
        sessions_migrated = true
    },
    {ok, State#state{current_shutdown = NewShutdownState}}.

%% @doc Phase 4: Announcing departure
-spec phase_announcing(state()) -> {ok, state()}.
phase_announcing(State) ->
    ?LOG_INFO("Announcing departure to cluster..."),

    _ = do_announce_departure(),

    CurrentShutdown = State#state.current_shutdown,
    NewShutdownState = CurrentShutdown#shutdown_state{
        phase = announcing,
        departure_announced = true
    },
    {ok, State#state{current_shutdown = NewShutdownState}}.

%% @doc Phase 5: Complete
-spec phase_complete(state()) -> {ok, state()}.
phase_complete(State) ->
    ?LOG_INFO("Shutdown sequence complete"),

    CurrentShutdown = State#state.current_shutdown,
    NewShutdownState = CurrentShutdown#shutdown_state{phase = complete},
    {ok, State#state{current_shutdown = NewShutdownState}}.

%% @doc Migrate sessions to backup nodes
-spec migrate_sessions_to_backup() -> {ok, non_neg_integer()} | {error, term()}.
migrate_sessions_to_backup() ->
    case whereis(erlmcp_session_failover) of
        undefined ->
            {error, failover_manager_not_running};
        Pid ->
            % Get all sessions hosted on this node
            Sessions = get_local_sessions(),
            Migrated = lists:foldl(fun(SessionId, Count) ->
                case erlmcp_session_failover:get_backup_nodes(SessionId) of
                    {ok, []} ->
                        % No backups, session will be lost
                        ?LOG_WARNING("Session ~s has no backups, will be lost", [SessionId]),
                        Count;
                    {ok, BackupNodes} ->
                        % Find an available backup
                        case find_available_backup(BackupNodes) of
                            {ok, BackupNode} ->
                                case migrate_session_to_node(SessionId, BackupNode) of
                                    ok ->
                                        ?LOG_DEBUG("Migrated session ~s to ~p", [SessionId, BackupNode]),
                                        Count + 1;
                                    {error, Reason} ->
                                        ?LOG_WARNING("Failed to migrate session ~s: ~p", [SessionId, Reason]),
                                        Count
                                end;
                            {error, none} ->
                                Count
                        end;
                    {error, _} ->
                        Count
                end
            end, 0, Sessions),
            {ok, Migrated}
    end.

%% @doc Get all local sessions
-spec get_local_sessions() -> [binary()].
get_local_sessions() ->
    case whereis(erlmcp_session_manager) of
        undefined -> [];
        _Pid ->
            try erlmcp_session_manager:list_sessions() of
                Sessions when is_list(Sessions) -> Sessions;
                _ -> []
            catch
                _:_ -> []
            end
    end.

%% @doc Find an available backup node
-spec find_available_backup([node()]) -> {ok, node()} | {error, none}.
find_available_backup([]) ->
    {error, none};
find_available_backup([Node | Rest]) ->
    case net_adm:ping(Node) of
        pong -> {ok, Node};
        pang -> find_available_backup(Rest)
    end.

%% @doc Migrate session to specific node
-spec migrate_session_to_node(binary(), node()) -> ok | {error, term()}.
migrate_session_to_node(SessionId, TargetNode) ->
    % Get session data
    case erlmcp_session:retrieve(SessionId) of
        {ok, Session} ->
            % Replicate to target node
            try
                rpc:call(TargetNode, erlmcp_session, create, [Session], 5000),
                % Delete from local
                erlmcp_session:delete(SessionId),
                ok
            catch
                _:_ -> {error, migration_failed}
            end;
        {error, Reason} ->
            {error, {session_not_found, Reason}}
    end.

%% @doc Drain connections
-spec do_drain_connections() -> ok.
do_drain_connections() ->
    % Stop accepting new connections
    case whereis(erlmcp_transport_sup) of
        undefined ->
            ok;
        _SupPid ->
            % Send drain signal to all transports
            Children = supervisor:which_children(erlmcp_transport_sup),
            lists:foreach(fun({_Id, Pid, _Type, _Modules}) when is_pid(Pid) ->
                Pid ! {drain, self()}
            end, Children)
    end,
    ok.

%% @doc Announce departure to cluster
-spec do_announce_departure() -> ok.
do_announce_departure() ->
    % Use cluster membership to leave gracefully
    case whereis(erlmcp_cluster_membership) of
        undefined ->
            % Fallback to direct node notification
            lists:foreach(fun(Node) ->
                erlang:send({?SERVER, Node}, {node_leaving, node()})
            end, nodes());
        Pid when is_pid(Pid) ->
            erlmcp_cluster_membership:leave_cluster()
    end,
    ok.

%% @doc Notify local processes of shutdown
-spec notify_shutdown_initiated(shutdown_reason()) -> ok.
notify_shutdown_initiated(Reason) ->
    % Send shutdown signal to all registered processes
    gproc:send({p, l, mcp_shutdown}, {shutdown_initiated, Reason}),
    ok.

%% @doc Run shutdown hooks
-spec run_shutdown_hooks([shutdown_hook()]) -> ok.
run_shutdown_hooks([]) ->
    ok;
run_shutdown_hooks([{Module, Function, Args} | Rest]) ->
    try
        apply(Module, Function, Args)
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Shutdown hook ~p:~p failed: ~p:~p",
                      [Module, Function, Class, Reason]),
            ?LOG_DEBUG("Stacktrace: ~p", [Stacktrace])
    end,
    run_shutdown_hooks(Rest);
run_shutdown_hooks([_Malformed | Rest]) ->
    ?LOG_WARNING("Malformed shutdown hook, skipping"),
    run_shutdown_hooks(Rest).
