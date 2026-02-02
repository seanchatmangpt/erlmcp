%%%-------------------------------------------------------------------
%%% @doc erlmcp_session_affinity - Session Affinity for Distributed Routing
%%%
%%% Implements session affinity for MCP clustering, ensuring requests
%%% are routed to the node hosting the session, with support for
%%% migration and failover.
%%%
%%% == Architecture ==
%%% - Session routing: Find node hosting session via gproc
%%% - Session migration: Move sessions between nodes for load balancing
%%% - Session backup: Replicate sessions for failover (via erlmcp_session_replicator)
%%% - Failover handling: Restore session from backup on node failure
%%%
%%% == Distribution Strategy ==
%%% - Use gproc distributed registry for session location
%%% - Session affinity ensures requests go to session-hosting node
%%% - On node failure, restore from backup on new node
%%% - Vector clocks for conflict resolution (via erlmcp_session_replicator)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_session_affinity).

-behaviour(gen_server).

%% API
-export([start_link/0,
         route_request/2,
         migrate_session/3,
         backup_session/2,
         get_session_node/1,
         handle_failover/2,
         get_affinity_map/0,
         invalidate_session/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(SESSION_LOCATION_KEY, erlmcp_session_location).
-define(MIGRATION_TIMEOUT, 10000).

%%====================================================================
%% Types
%%====================================================================

-type session_id() :: binary().
-type session_node() :: node().
-type migration_result() :: {ok, session_node()} | {error, term()}.

-record(session_location,
        {session_id :: session_id(),
         node :: session_node(),
         backup_nodes :: [session_node()],
         last_updated :: integer()}).

-record(state,
        {affinity_map :: #{session_id() => #session_location{}},
         node_monitors :: #{session_node() => reference()}}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Route a request to the node hosting the session
%% Returns the node where the session is located
-spec route_request(session_id(), term()) -> {ok, session_node()} | {error, not_found | node_down}.
route_request(SessionId, _Request) ->
    gen_server:call(?SERVER, {route_request, SessionId}, 5000).

%% @doc Migrate a session to another node
%% Useful for load balancing or node maintenance
-spec migrate_session(session_id(), session_node(), [session_node()]) -> migration_result().
migrate_session(SessionId, TargetNode, BackupNodes) ->
    gen_server:call(?SERVER, {migrate_session, SessionId, TargetNode, BackupNodes}, ?MIGRATION_TIMEOUT).

%% @doc Create a backup of a session on replica nodes
%% Delegates to erlmcp_session_replicator
-spec backup_session(session_id(), term()) -> ok | {error, term()}.
backup_session(SessionId, Session) ->
    gen_server:call(?SERVER, {backup_session, SessionId, Session}, 5000).

%% @doc Get the node hosting a session
-spec get_session_node(session_id()) -> {ok, session_node()} | {error, not_found}.
get_session_node(SessionId) ->
    gen_server:call(?SERVER, {get_session_node, SessionId}, 5000).

%% @doc Handle session failover when a node goes down
%% Finds backup session and activates it on current node
-spec handle_failover(session_id(), session_node()) -> {ok, session_node()} | {error, term()}.
handle_failover(SessionId, FailedNode) ->
    gen_server:call(?SERVER, {handle_failover, SessionId, FailedNode}, 10000).

%% @doc Get the complete affinity map (for debugging)
-spec get_affinity_map() -> #{session_id() => #{node => session_node(), backup_nodes => [session_node()]}}.
get_affinity_map() ->
    gen_server:call(?SERVER, get_affinity_map, 5000).

%% @doc Invalidate a session from affinity map
-spec invalidate_session(session_id()) -> ok.
invalidate_session(SessionId) ->
    gen_server:cast(?SERVER, {invalidate_session, SessionId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    %% Start monitoring nodes
    ok = net_kernel:monitor_nodes(true),

    %% Initialize state
    State = #state{affinity_map = #{},
                   node_monitors = #{}},

    ?LOG_INFO("Starting erlmcp_session_affinity"),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {reply, term(), state(), hibernate}.
handle_call({route_request, SessionId}, _From, State) ->
    %% Check gproc for session location (distributed registry)
    Key = {n, g, {?SESSION_LOCATION_KEY, SessionId}},
    case gproc:where(Key) of
        undefined ->
            %% Session not found in distributed registry
            {reply, {error, not_found}, State, hibernate};
        Pid when node(Pid) =:= node() ->
            %% Session is on this node
            {reply, {ok, node()}, State, hibernate};
        RemotePid when is_pid(RemotePid) ->
            %% Session is on remote node
            SessionNode = node(RemotePid),
            %% Check if node is up
            case net_adm:ping(SessionNode) of
                pong ->
                    {reply, {ok, SessionNode}, State, hibernate};
                pang ->
                    %% Node is down, attempt failover
                    case do_failover(SessionId, SessionNode, State) of
                        {ok, NewNode, NewState} ->
                            {reply, {ok, NewNode}, NewState, hibernate};
                        {error, _Reason} = Error ->
                            {reply, Error, State, hibernate}
                    end
            end
    end;

handle_call({migrate_session, SessionId, TargetNode, BackupNodes}, _From, State) ->
    %% Migrate session to target node
    case do_migrate_session(SessionId, TargetNode, BackupNodes, State) of
        {ok, NewState} ->
            {reply, {ok, TargetNode}, NewState, hibernate};
        {error, Reason} ->
            {reply, {error, Reason}, State, hibernate}
    end;

handle_call({backup_session, SessionId, Session}, _From, State) ->
    %% Delegate to session replicator
    Result = erlmcp_session_replicator:replicate(SessionId, Session),
    {reply, Result, State, hibernate};

handle_call({get_session_node, SessionId}, _From, State) ->
    Key = {n, g, {?SESSION_LOCATION_KEY, SessionId}},
    case gproc:where(Key) of
        undefined ->
            {reply, {error, not_found}, State, hibernate};
        Pid ->
            SessionNode = node(Pid),
            {reply, {ok, SessionNode}, State, hibernate}
    end;

handle_call({handle_failover, SessionId, FailedNode}, _From, State) ->
    case do_failover(SessionId, FailedNode, State) of
        {ok, NewNode, NewState} ->
            {reply, {ok, NewNode}, NewState, hibernate};
        {error, Reason} ->
            {reply, {error, Reason}, State, hibernate}
    end;

handle_call(get_affinity_map, _From, State) ->
    %% Return sanitized affinity map
    AffinityMap = maps:map(fun(_SessionId, Location) ->
                                   #{node => Location#session_location.node,
                                     backup_nodes => Location#session_location.backup_nodes}
                           end, State#state.affinity_map),
    {reply, AffinityMap, State, hibernate};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_cast({invalidate_session, SessionId}, State) ->
    %% Remove session from affinity map
    NewAffinityMap = maps:remove(SessionId, State#state.affinity_map),

    %% Unregister from gproc
    Key = {n, g, {?SESSION_LOCATION_KEY, SessionId}},
    try
        gproc:unreg(Key)
    catch
        _:_ ->
            ok
    end,

    ?LOG_INFO("Invalidated session affinity: ~p", [SessionId]),
    {noreply, State#state{affinity_map = NewAffinityMap}, hibernate};

handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

-spec handle_info(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_info({nodeup, Node, _InfoList}, State) ->
    ?LOG_INFO("Node up: ~p (session affinity)", [Node]),
    {noreply, State, hibernate};

handle_info({nodedown, Node, InfoList}, State) ->
    Reason = proplists:get_value(reason, InfoList, unknown),
    ?LOG_WARNING("Node down: ~p (reason: ~p), handling session failover", [Node, Reason]),

    %% Find all sessions on failed node and trigger failover
    NewState = handle_node_failure(Node, State),
    {noreply, NewState, hibernate};

handle_info(_Info, State) ->
    {noreply, State, hibernate}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    net_kernel:monitor_nodes(false),
    ?LOG_INFO("erlmcp_session_affinity terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Migrate session to target node
-spec do_migrate_session(session_id(), session_node(), [session_node()], state()) ->
    {ok, state()} | {error, term()}.
do_migrate_session(SessionId, TargetNode, BackupNodes, State) ->
    %% Check if target node is available
    case net_adm:ping(TargetNode) of
        pang ->
            {error, {target_node_unavailable, TargetNode}};
        pong ->
            %% Fetch session from current node
            case erlmcp_session_backend:fetch(SessionId) of
                {ok, Session} ->
                    %% Replicate to target node
                    case rpc:call(TargetNode, erlmcp_session_backend, store, [SessionId, Session], 5000) of
                        ok ->
                            %% Update affinity map
                            Location = #session_location{
                                session_id = SessionId,
                                node = TargetNode,
                                backup_nodes = BackupNodes,
                                last_updated = erlang:system_time(millisecond)
                            },
                            NewAffinityMap = maps:put(SessionId, Location, State#state.affinity_map),

                            %% Register in gproc
                            Key = {n, g, {?SESSION_LOCATION_KEY, SessionId}},
                            gproc:reg_or_update(Key, TargetNode),

                            ?LOG_INFO("Migrated session ~p to node ~p", [SessionId, TargetNode]),
                            {ok, State#state{affinity_map = NewAffinityMap}};
                        {error, Reason} ->
                            {error, {migration_failed, Reason}}
                    end;
                {error, not_found} ->
                    {error, session_not_found}
            end
    end.

%% @doc Handle failover for failed node
-spec do_failover(session_id(), session_node(), state()) ->
    {ok, session_node(), state()} | {error, term()}.
do_failover(SessionId, FailedNode, State) ->
    %% Find backup location from replicator
    case erlmcp_session_replicator:get_replicas(SessionId) of
        {ok, [BackupNode | _]} ->
            %% Check if backup node is available
            case net_adm:ping(BackupNode) of
                pong ->
                    %% Restore session from backup
                    case restore_session_from_backup(SessionId, BackupNode) of
                        ok ->
                            %% Update affinity map
                            Location = #session_location{
                                session_id = SessionId,
                                node = BackupNode,
                                backup_nodes = [],
                                last_updated = erlang:system_time(millisecond)
                            },
                            NewAffinityMap = maps:put(SessionId, Location, State#state.affinity_map),

                            ?LOG_INFO("Failover: session ~p restored on node ~p (was on ~p)",
                                     [SessionId, BackupNode, FailedNode]),
                            {ok, BackupNode, State#state{affinity_map = NewAffinityMap}};
                        {error, Reason} ->
                            {error, {failover_failed, Reason}}
                    end;
                pang ->
                    {error, {backup_node_unavailable, BackupNode}}
            end;
        {error, not_found} ->
            {error, no_backup_available}
    end.

%% @doc Restore session from backup node
-spec restore_session_from_backup(session_id(), session_node()) -> ok | {error, term()}.
restore_session_from_backup(SessionId, BackupNode) ->
    %% Fetch session from backup node via RPC
    case rpc:call(BackupNode, erlmcp_session_backend, fetch, [SessionId], 5000) of
        {ok, Session} ->
            %% Store session locally
            case erlmcp_session_backend:store(SessionId, Session) of
                ok ->
                    ok;
                {error, Reason} ->
                    {error, {store_failed, Reason}}
            end;
        {error, not_found} ->
            {error, session_not_on_backup};
        {error, Reason} ->
            {error, {rpc_failed, Reason}}
    end.

%% @doc Handle node failure - trigger failover for all sessions on that node
-spec handle_node_failure(session_node(), state()) -> state().
handle_node_failure(FailedNode, State) ->
    %% Find all sessions on failed node
    SessionsOnNode = maps:fold(fun(SessionId, Location, Acc) ->
                                       case Location#session_location.node of
                                           FailedNode ->
                                               [SessionId | Acc];
                                           _ ->
                                               Acc
                                       end
                               end, [], State#state.affinity_map),

    %% Trigger failover for each session
    lists:foldl(fun(SessionId, AccState) ->
                       case do_failover(SessionId, FailedNode, AccState) of
                           {ok, _NewNode, NewState} ->
                               NewState;
                           {error, Reason} ->
                               ?LOG_ERROR("Failover failed for session ~p: ~p", [SessionId, Reason]),
                               AccState
                       end
               end, State, SessionsOnNode).
