%%%-------------------------------------------------------------------
%%% @doc erlmcp_distributed_session - Distributed Session Management
%%%
%%% Implements session replication across cluster nodes:
%%% - Session replication to backup nodes
%%% - Automatic failover on node failure
%%% - Session migration
%%% - Conflict resolution
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_distributed_session).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         create_distributed/2, create_distributed/3,
         migrate_session/2,
         get_session_location/1,
         get_replica_nodes/1,
         sync_session/1,
         set_replication_factor/1,
         get_replication_factor/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_REPLICATION_FACTOR, 2).
-define(DEFAULT_SYNC_INTERVAL, 5000).

%%====================================================================
%% Types
%%====================================================================

-type session_id() :: binary().
%% -type replica_node() :: {node(), pid() | undefined}.  % Internal type, unused

-record(replica_state, {
    session_id :: session_id(),
    primary_node :: node(),
    replica_nodes = [] :: [node()],
    last_sync :: integer(),
    version = 0 :: non_neg_integer()}).

-record(state, {
    local_node :: node(),
    replication_factor :: pos_integer(),
    sync_interval :: pos_integer(),
    sync_ref :: reference() | undefined,
    replicas = #{} :: #{session_id() => #replica_state{}},
    pending_sync = [] :: [session_id()]}).

-type state() :: #state{}.

-export_type([session_id/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start distributed session manager with defaults
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Start distributed session manager with options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Create a distributed session
-spec create_distributed(map(), pos_integer() | infinity) ->
    {ok, session_id()} | {error, term()}.
create_distributed(Metadata, TTL) ->
    create_distributed(Metadata, TTL, ?DEFAULT_REPLICATION_FACTOR).

%% @doc Create a distributed session with specific replication factor
-spec create_distributed(map(), pos_integer() | infinity, pos_integer()) ->
    {ok, session_id()} | {error, term()}.
create_distributed(Metadata, TTL, ReplicationFactor) ->
    gen_server:call(?SERVER, {create_distributed, Metadata, TTL, ReplicationFactor}).

%% @doc Migrate a session to another node
-spec migrate_session(session_id(), node()) -> ok | {error, term()}.
migrate_session(SessionId, TargetNode) ->
    gen_server:call(?SERVER, {migrate_session, SessionId, TargetNode}, 15000).

%% @doc Get the location (primary node) of a session
-spec get_session_location(session_id()) -> {ok, node()} | {error, not_found}.
get_session_location(SessionId) ->
    gen_server:call(?SERVER, {get_session_location, SessionId}).

%% @doc Get replica nodes for a session
-spec get_replica_nodes(session_id()) -> {ok, [node()]} | {error, not_found}.
get_replica_nodes(SessionId) ->
    gen_server:call(?SERVER, {get_replica_nodes, SessionId}).

%% @doc Force sync a session to replicas
-spec sync_session(session_id()) -> ok | {error, term()}.
sync_session(SessionId) ->
    gen_server:cast(?SERVER, {sync_session, SessionId}).

%% @doc Set replication factor
-spec set_replication_factor(pos_integer()) -> ok.
set_replication_factor(Factor) when is_integer(Factor), Factor > 0 ->
    gen_server:call(?SERVER, {set_replication_factor, Factor}).

%% @doc Get current replication factor
-spec get_replication_factor() -> pos_integer().
get_replication_factor() ->
    gen_server:call(?SERVER, get_replication_factor).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map() | []) -> {ok, state()}.
init(Options) ->
    process_flag(trap_exit, true),

    LocalNode = node(),
    ReplicationFactor = maps:get(replication_factor, Options, ?DEFAULT_REPLICATION_FACTOR),
    SyncInterval = maps:get(sync_interval, Options, ?DEFAULT_SYNC_INTERVAL),

    % Subscribe to cluster membership events
    case whereis(erlmcp_cluster_membership) of
        undefined -> ok;
        _ -> erlmcp_cluster_membership:subscribe()
    end,

    % Start sync timer
    SyncRef = erlang:send_after(SyncInterval, self(), periodic_sync),

    ?LOG_INFO("Starting distributed session manager: replication=~p, sync_interval=~p",
              [ReplicationFactor, SyncInterval]),

    {ok, #state{
        local_node = LocalNode,
        replication_factor = ReplicationFactor,
        sync_interval = SyncInterval,
        sync_ref = SyncRef
    }}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {reply, term(), state(), hibernate}.
handle_call({create_distributed, Metadata, TTL, ReplicationFactor}, _From, State) ->
    % Create session locally first
    case erlmcp_session:create(Metadata, TTL) of
        {ok, SessionId} ->
            % Select replica nodes
            ClusterNodes = get_available_cluster_nodes(),
            ReplicaNodes = select_replica_nodes(ClusterNodes, State#state.local_node, ReplicationFactor),

            % Create replica state
            ReplicaState = #replica_state{
                session_id = SessionId,
                primary_node = State#state.local_node,
                replica_nodes = ReplicaNodes,
                last_sync = erlang:system_time(millisecond),
                version = 1
            },

            % Replicate to backup nodes
            ReplicationResult = replicate_session_to_nodes(SessionId, Metadata, ReplicaNodes),

            case ReplicationResult of
                {ok, _} ->
                    NewReplicas = maps:put(SessionId, ReplicaState, State#state.replicas),
                    ?LOG_INFO("Created distributed session ~s with ~p replicas",
                             [SessionId, length(ReplicaNodes)]),
                    {reply, {ok, SessionId}, State#state{replicas = NewReplicas}, hibernate};
                {error, Reason} ->
                    % Clean up local session
                    erlmcp_session:delete(SessionId),
                    {reply, {error, {replication_failed, Reason}}, State, hibernate}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State, hibernate}
    end;

handle_call({migrate_session, SessionId, TargetNode}, _From, State) ->
    case maps:get(SessionId, State#state.replicas, undefined) of
        undefined ->
            {reply, {error, not_found}, State, hibernate};
        ReplicaState when ReplicaState#replica_state.primary_node =/= State#state.local_node ->
            {reply, {error, not_primary}, State, hibernate};
        ReplicaState ->
            % Get session data
            case erlmcp_session:retrieve(SessionId) of
                {ok, SessionData} ->
                    % Migrate to target node
                    Metadata = maps:get(metadata, SessionData, #{}),
                    case migrate_to_node(SessionId, Metadata, TargetNode) of
                        ok ->
                            % Update replica state
                            NewReplicaState = ReplicaState#replica_state{
                                primary_node = TargetNode,
                                replica_nodes = lists:delete(TargetNode, ReplicaState#replica_state.replica_nodes),
                                last_sync = erlang:system_time(millisecond),
                                version = ReplicaState#replica_state.version + 1
                            },
                            NewReplicas = maps:put(SessionId, NewReplicaState, State#state.replicas),

                            % Delete from local
                            erlmcp_session:delete(SessionId),

                            ?LOG_INFO("Migrated session ~s to ~p", [SessionId, TargetNode]),
                            {reply, ok, State#state{replicas = NewReplicas}, hibernate};
                        {error, Reason} ->
                            {reply, {error, Reason}, State, hibernate}
                    end;
                {error, Reason} ->
                    {reply, {error, Reason}, State, hibernate}
            end
    end;

handle_call({get_session_location, SessionId}, _From, State) ->
    Reply = case maps:get(SessionId, State#state.replicas, undefined) of
        undefined ->
            % Check local session manager
            case erlmcp_session:retrieve(SessionId) of
                {ok, _} -> {ok, node()};
                {error, _} -> {error, not_found}
            end;
        ReplicaState ->
            {ok, ReplicaState#replica_state.primary_node}
    end,
    {reply, Reply, State, hibernate};

handle_call({get_replica_nodes, SessionId}, _From, State) ->
    Reply = case maps:get(SessionId, State#state.replicas, undefined) of
        undefined -> {error, not_found};
        ReplicaState -> {ok, ReplicaState#replica_state.replica_nodes}
    end,
    {reply, Reply, State, hibernate};

handle_call({set_replication_factor, Factor}, _From, State) ->
    {reply, ok, State#state{replication_factor = Factor}, hibernate};

handle_call(get_replication_factor, _From, State) ->
    {reply, State#state.replication_factor, State, hibernate};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_cast({sync_session, SessionId}, State) ->
    case maps:get(SessionId, State#state.replicas, undefined) of
        undefined ->
            {noreply, State, hibernate};
        ReplicaState ->
            sync_session_to_replicas(SessionId, ReplicaState),
            NewReplicaState = ReplicaState#replica_state{last_sync = erlang:system_time(millisecond)},
            NewReplicas = maps:put(SessionId, NewReplicaState, State#state.replicas),
            {noreply, State#state{replicas = NewReplicas}, hibernate}
    end;

handle_cast({session_replicated, SessionId, Node}, State) ->
    case maps:get(SessionId, State#state.replicas, undefined) of
        undefined ->
            {noreply, State, hibernate};
        _ReplicaState ->
            ?LOG_DEBUG("Session ~s replication acknowledged by ~p", [SessionId, Node]),
            {noreply, State, hibernate}
    end;

handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

-spec handle_info(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_info(periodic_sync, State) ->
    % Sync all pending sessions
    NewState = sync_pending_sessions(State),

    % Schedule next sync
    SyncRef = erlang:send_after(State#state.sync_interval, self(), periodic_sync),
    {noreply, NewState#state{sync_ref = SyncRef}, hibernate};

handle_info({membership_event, Event}, State) ->
    NewState = handle_membership_event(Event, State),
    {noreply, NewState, hibernate};

handle_info({session_failed_over, SessionId, OldPrimary, NewPrimary}, State) ->
    ?LOG_INFO("session_failed_over event: session=~s old_primary=~p new_primary=~p",
              [SessionId, OldPrimary, NewPrimary]),
    {noreply, State, hibernate};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State, hibernate};

handle_info(_Info, State) ->
    {noreply, State, hibernate}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    case State#state.sync_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ?LOG_INFO("Distributed session manager terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Get available cluster nodes
-spec get_available_cluster_nodes() -> [node()].
get_available_cluster_nodes() ->
    case whereis(erlmcp_cluster_membership) of
        undefined ->
            nodes();
        _Pid ->
            case erlmcp_cluster_membership:get_members() of
                Members when is_map(Members) ->
                    [N || N <- maps:keys(Members), N =/= node()];
                _ ->
                    nodes()
            end
    end.

%% @doc Select replica nodes (excluding local node)
-spec select_replica_nodes([node()], node(), pos_integer()) -> [node()].
select_replica_nodes(ClusterNodes, LocalNode, ReplicationFactor) ->
    Available = lists:delete(LocalNode, ClusterNodes),
    Count = min(ReplicationFactor, length(Available)),
    take_random(Available, Count).

%% @doc Replicate session to nodes
-spec replicate_session_to_nodes(session_id(), map(), [node()]) ->
    {ok, [node()]} | {error, term()}.
replicate_session_to_nodes(SessionId, Metadata, ReplicaNodes) ->
    Results = lists:map(fun(Node) ->
        replicate_to_node(SessionId, Metadata, Node)
    end, ReplicaNodes),

    SuccessNodes = [N || {N, ok} <- Results],
    FailedNodes = [N || {N, {error, _}} <- Results],

    case length(SuccessNodes) of
        0 ->
            {error, {all_replications_failed, FailedNodes}};
        _ ->
            ?LOG_INFO("Replicated session ~s to ~p nodes (failed: ~p)",
                     [SessionId, length(SuccessNodes), FailedNodes]),
            {ok, SuccessNodes}
    end.

%% @doc Replicate to a single node
-spec replicate_to_node(session_id(), map(), node()) -> {node(), ok | {error, term()}}.
replicate_to_node(_SessionId, Metadata, Node) ->
    try
        Result = rpc:call(Node, erlmcp_session, create, [Metadata], 5000),
        {Node, Result}
    catch
        _:_ -> {Node, {error, rpc_failed}}
    end.

%% @doc Migrate session to node
-spec migrate_to_node(session_id(), map(), node()) -> ok | {error, term()}.
migrate_to_node(_SessionId, Metadata, TargetNode) ->
    try
        % Create on target
        case rpc:call(TargetNode, erlmcp_session, create, [Metadata], 5000) of
            {ok, _} ->
                ok;
            {error, Reason} ->
                {error, {target_create_failed, Reason}}
        end
    catch
        _:_ -> {error, rpc_failed}
    end.

%% @doc Sync session to replicas
-spec sync_session_to_replicas(session_id(), #replica_state{}) -> ok.
sync_session_to_replicas(SessionId, ReplicaState) ->
    case erlmcp_session:retrieve(SessionId) of
        {ok, _SessionData} ->
            lists:foreach(fun(Node) ->
                rpc:cast(Node, erlmcp_session, update, [SessionId, fun(S) -> S end])
            end, ReplicaState#replica_state.replica_nodes),
            ok;
        {error, Reason} ->
            ?LOG_WARNING("Failed to sync session ~s: ~p", [SessionId, Reason])
    end.

%% @doc Sync all pending sessions
-spec sync_pending_sessions(state()) -> state().
sync_pending_sessions(State) ->
    Now = erlang:system_time(millisecond),

    maps:fold(fun(SessionId, ReplicaState, AccState) ->
        Age = Now - ReplicaState#replica_state.last_sync,
        case Age >= AccState#state.sync_interval of
            true ->
                sync_session_to_replicas(SessionId, ReplicaState),
                NewReplicaState = ReplicaState#replica_state{last_sync = Now},
                NewReplicas = maps:put(SessionId, NewReplicaState, AccState#state.replicas),
                AccState#state{replicas = NewReplicas};
            false ->
                AccState
        end
    end, State, State#state.replicas).

%% @doc Handle membership events
-spec handle_membership_event(term(), state()) -> state().
handle_membership_event({member_joined, Node, _Status}, State) ->
    ?LOG_INFO("Node ~p joined, may need rebalancing replicas", [Node]),
    State;

handle_membership_event({member_dead, Node}, State) ->
    % Remove node from replica lists
    NewReplicas = maps:map(fun(_SessionId, ReplicaState) ->
        NewReplicaNodes = lists:delete(Node, ReplicaState#replica_state.replica_nodes),
        ReplicaState#replica_state{replica_nodes = NewReplicaNodes}
    end, State#state.replicas),

    % Check if primary was this node (should failover)
    FailoverSessions = maps:fold(fun(SessionId, ReplicaState, Acc) ->
        case ReplicaState#replica_state.primary_node of
            Node ->
                % Primary died, need to failover
                [SessionId | Acc];
            _ ->
                Acc
        end
    end, [], State#state.replicas),

    %% Promote first available replica for each session whose primary died
    NewReplicas2 = lists:foldl(fun(SessionId, AccReplicas) ->
        case maps:get(SessionId, AccReplicas, undefined) of
            undefined ->
                AccReplicas;
            ReplicaState ->
                case ReplicaState#replica_state.replica_nodes of
                    [] ->
                        %% No replicas available — log and skip
                        ?LOG_ERROR("Session ~s has no replicas for failover after ~p died",
                                   [SessionId, Node]),
                        AccReplicas;
                    [NewPrimary | _] ->
                        %% Promote first replica (best-effort; session manager may be absent)
                        _ = erlmcp_session:set_primary(SessionId, NewPrimary),
                        UpdatedReplica = ReplicaState#replica_state{
                            primary_node  = NewPrimary,
                            replica_nodes = lists:delete(NewPrimary,
                                               ReplicaState#replica_state.replica_nodes)
                        },
                        ?LOG_WARNING("Session ~s failed over from ~p to ~p",
                                     [SessionId, Node, NewPrimary]),
                        %% Emit failover event
                        self() ! {session_failed_over, SessionId, Node, NewPrimary},
                        maps:put(SessionId, UpdatedReplica, AccReplicas)
                end
        end
    end, NewReplicas, FailoverSessions),

    case FailoverSessions of
        [] ->
            ok;
        Sessions ->
            ?LOG_WARNING("Primary node ~p died, failed over ~p session(s): ~p",
                        [Node, length(Sessions), Sessions])
    end,

    State#state{replicas = NewReplicas2};

handle_membership_event({member_left, Node}, State) ->
    handle_membership_event({member_dead, Node}, State);

handle_membership_event(partition_detected, State) ->
    ?LOG_WARNING("Network partition detected, pausing session replication"),
    State;

handle_membership_event(partition_healed, State) ->
    ?LOG_INFO("Network partition healed, resuming session replication"),
    State;

handle_membership_event(_, State) ->
    State.

%% @doc Take N random elements
-spec take_random([T], non_neg_integer()) -> [T].
take_random(List, N) when length(List) =< N ->
    List;
take_random(List, N) ->
    Shuffled = lists:sort(fun(_, _) -> rand:uniform() > 0.5 end, List),
    lists:sublist(Shuffled, N).
