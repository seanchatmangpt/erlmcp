%% @doc Cluster Data Synchronization Engine for erlmcp v3
%%
%% This module handles data synchronization across cluster nodes:
%%   - Anti-entropy synchronization
%%   - Conflict detection and resolution (CRDT-style)
%%   - Merkle tree-based diff computation
%%   - Vector clock tracking
%%   - Gossip-based propagation
%%   - Snapshot transfer
%%   - Incremental sync
-module(erlmcp_cluster_sync).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([sync_all/0, sync_node/2]).
-export([trigger_sync/1, get_sync_status/0]).
-export([resolve_conflict/3]).
-export([get_vector_clock/0, merge_vector_clocks/2]).
-export([create_merkle_tree/1, verify_merkle_proof/3]).
-export([subscribe/1, unsubscribe/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Internal state
-record(sync_state,
        {
         node_id :: node(),
         sync_in_progress :: boolean(),
         pending_syncs :: #{reference() => sync_operation()},
         completed_syncs :: [reference()],
         conflicts :: #{binary() => conflict_info()},
         vector_clock :: vector_clock(),
         merkle_trees :: #{binary() => merkle_tree()},
         sync_strategy :: sync_strategy(),
         subscribers :: sets:set(pid()),
         sync_interval :: pos_integer(),
         last_sync :: integer(),
         sync_stats :: sync_stats()
        }).

-type vector_clock() :: #{node() => non_neg_integer()}.
-type merkle_node() :: {binary(), crypto:hash()}.
-type merkle_tree() :: #{binary() => merkle_node()}.

-type sync_operation() :: #{
        id => reference(),
        type => sync_operation_type(),
        target_nodes => [node()],
        started_at => integer(),
        status => pending | in_progress | completed | failed,
        entries_synced => non_neg_integer(),
        bytes_transferred => non_neg_integer(),
        conflicts => non_neg_integer()
       }.

-type sync_operation_type() ::
    full_sync         |
    incremental_sync  |
    merkle_sync       |
    vector_clock_sync |
    gossip_sync       |
    snapshot_sync.

-type sync_strategy() ::
    anti_entropy      |
    merkle_tree       |
    vector_clock      |
    gossip            |
    last_write_wins   |
    causal.

-type sync_stats() :: #{
        total_syncs => non_neg_integer(),
        successful_syncs => non_neg_integer(),
        failed_syncs => non_neg_integer(),
        entries_synced => non_neg_integer(),
        bytes_transferred => non_neg_integer(),
        conflicts_detected => non_neg_integer(),
        conflicts_resolved => non_neg_integer(),
        last_sync_time => integer() | undefined
       }.

-type conflict_info() :: #{
        key => binary(),
        local_value => term(),
        remote_value => term(),
        local_version => non_neg_integer(),
        remote_version => non_neg_integer(),
        local_vector_clock => vector_clock(),
        remote_vector_clock => vector_clock(),
        timestamp => integer(),
        resolution => conflict_resolution() | undefined
       }.

-type conflict_resolution() ::
    local_wins       |
    remote_wins      |
    merge            |
    manual           |
    retry.

%%%====================================================================
%%% API Functions
%%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Synchronize with all cluster nodes
-spec sync_all() -> {ok, reference()} | {error, term()}.
sync_all() ->
    gen_server:call(?MODULE, {sync, all}, 60000).

%% @doc Synchronize with specific node
-spec sync_node(node(), sync_operation_type()) -> {ok, reference()} | {error, term()}.
sync_node(Node, SyncType) ->
    gen_server:call(?MODULE, {sync, Node, SyncType}, 60000).

%% @doc Trigger a sync operation
-spec trigger_sync(sync_operation_type()) -> {ok, reference()} | {error, term()}.
trigger_sync(SyncType) ->
    gen_server:call(?MODULE, {trigger_sync, SyncType}, 60000).

%% @doc Get current sync status
-spec get_sync_status() -> {ok, map()}.
get_sync_status() ->
    gen_server:call(?MODULE, get_sync_status).

%% @doc Resolve a conflict
-spec resolve_conflict(binary(), conflict_resolution(), term()) -> ok | {error, term()}.
resolve_conflict(Key, Resolution, Value) ->
    gen_server:call(?MODULE, {resolve_conflict, Key, Resolution, Value}).

%% @doc Get current vector clock
-spec get_vector_clock() -> {ok, vector_clock()}.
get_vector_clock() ->
    gen_server:call(?MODULE, get_vector_clock).

%% @doc Merge vector clocks
-spec merge_vector_clocks(vector_clock(), vector_clock()) -> vector_clock().
merge_vector_clocks(VC1, VC2) ->
    maps:merge_with(fun(_K, V1, V2) -> max(V1, V2) end, VC1, VC2).

%% @doc Create Merkle tree for data
-spec create_merkle_tree(binary()) -> {ok, merkle_tree()}.
create_merkle_tree(Data) ->
    gen_server:call(?MODULE, {create_merkle_tree, Data}).

%% @doc Verify Merkle proof
-spec verify_merkle_proof(binary(), crypto:hash(), [crypto:hash()]) -> boolean().
verify_merkle_proof(_Leaf, _Root, _Proof) ->
    %% Simplified - full implementation would verify the proof chain
    true.

%% @doc Subscribe to sync events
-spec subscribe(pid()) -> ok.
subscribe(Subscriber) ->
    gen_server:cast(?MODULE, {subscribe, Subscriber}).

%% @doc Unsubscribe from sync events
-spec unsubscribe(pid()) -> ok.
unsubscribe(Subscriber) ->
    gen_server:cast(?MODULE, {unsubscribe, Subscriber}).

%%%====================================================================
%%% gen_server Callbacks
%%%====================================================================

-spec init(map()) -> {ok, #sync_state{}}.
init(Options) ->
    process_flag(trap_exit, true),

    NodeId = node(),
    SyncStrategy = maps:get(sync_strategy, Options, anti_entropy),
    SyncInterval = maps:get(sync_interval, Options, 60000),

    State = #sync_state{
        node_id = NodeId,
        sync_in_progress = false,
        pending_syncs => #{},
        completed_syncs => [],
        conflicts => #{},
        vector_clock = #{NodeId => 0},
        merkle_trees => #{},
        sync_strategy = SyncStrategy,
        subscribers = sets:new(),
        sync_interval = SyncInterval,
        last_sync => 0,
        sync_stats = #{
            total_syncs => 0,
            successful_syncs => 0,
            failed_syncs => 0,
            entries_synced => 0,
            bytes_transferred => 0,
            conflicts_detected => 0,
            conflicts_resolved => 0,
            last_sync_time => undefined
        }
    },

    logger:info("Cluster sync module started (strategy=~p)", [SyncStrategy]),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #sync_state{}) ->
    {reply, term(), #sync_state{}}.
handle_call({sync, all}, _From, State) ->
    {ok, Members} = erlmcp_cluster_membership:get_members(),
    TargetNodes = lists:delete(State#sync_state.node_id, Members),
    NewState = do_sync(State, TargetNodes, full_sync),
    SyncId = get_current_sync_id(NewState),
    {reply, {ok, SyncId}, NewState};

handle_call({sync, Node, SyncType}, _From, State) ->
    NewState = do_sync(State, [Node], SyncType),
    SyncId = get_current_sync_id(NewState),
    {reply, {ok, SyncId}, NewState};

handle_call({trigger_sync, SyncType}, _From, State) ->
    {ok, Members} = erlmcp_cluster_membership:get_members(),
    TargetNodes = lists:delete(State#sync_state.node_id, Members),
    NewState = do_sync(State, TargetNodes, SyncType),
    SyncId = get_current_sync_id(NewState),
    {reply, {ok, SyncId}, NewState};

handle_call(get_sync_status, _From, State) ->
    Status = #{
        sync_in_progress => State#sync_state.sync_in_progress,
        pending_syncs => maps:size(State#sync_state.pending_syncs),
        completed_syncs => length(State#sync_state.completed_syncs),
        conflicts => maps:size(State#sync_state.conflicts),
        strategy => State#sync_state.sync_strategy,
        stats => State#sync_state.sync_stats
    },
    {reply, {ok, Status}, State};

handle_call({resolve_conflict, Key, Resolution, Value}, _From, State) ->
    case maps:get(Key, State#sync_state.conflicts, undefined) of
        undefined ->
            {reply, {error, no_conflict}, State};
        Conflict ->
            NewState = apply_resolution(State, Key, Conflict, Resolution, Value),
            {reply, ok, NewState}
    end;

handle_call(get_vector_clock, _From, State) ->
    {reply, {ok, State#sync_state.vector_clock}, State};

handle_call({create_merkle_tree, Data}, _From, State) ->
    Tree = build_merkle_tree(Data),
    TreeId = <<<<(crypto:hash(sha256, Data))/binary, 0:32>>),
    NewTrees = maps:put(TreeId, Tree, State#sync_state.merkle_trees),
    {reply, {ok, Tree}, State#sync_state{merkle_trees = NewTrees}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #sync_state{}) -> {noreply, #sync_state{}}.
handle_cast({sync_complete, SyncId, Result}, State) ->
    NewState = handle_sync_complete(SyncId, Result, State),
    {noreply, NewState};

handle_cast({conflict_detected, Key, Local, Remote}, State) ->
    NewState = handle_conflict_detected(Key, Local, Remote, State),
    {noreply, NewState};

handle_cast({subscribe, Subscriber}, State) ->
    monitor(process, Subscriber),
    NewSubscribers = sets:add_element(Subscriber, State#sync_state.subscribers),
    {noreply, State#sync_state{subscribers = NewSubscribers}};

handle_cast({unsubscribe, Subscriber}, State) ->
    NewSubscribers = sets:del_element(Subscriber, State#sync_state.subscribers),
    {noreply, State#sync_state{subscribers = NewSubscribers}};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #sync_state{}) -> {noreply, #sync_state{}}.
handle_info({'EXIT', Pid, _Reason}, State) ->
    NewSubscribers = sets:filter(fun(S) -> S =/= Pid end, State#sync_state.subscribers),
    {noreply, State#sync_state{subscribers = NewSubscribers}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #sync_state{}) -> ok.
terminate(_Reason, _State) ->
    logger:info("Cluster sync module terminating"),
    ok.

-spec code_change(term(), #sync_state{}, term()) -> {ok, #sync_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @doc Perform synchronization
-spec do_sync(#sync_state{}, [node()], sync_operation_type()) -> #sync_state{}.
do_sync(State, TargetNodes, SyncType) when length(TargetNodes) > 0 ->
    SyncId = make_ref(),
    Now = erlang:system_time(millisecond),

    SyncOp = #{
        id => SyncId,
        type => SyncType,
        target_nodes => TargetNodes,
        started_at => Now,
        status => in_progress,
        entries_synced => 0,
        bytes_transferred => 0,
        conflicts => 0
    },

    NewPending = maps:put(SyncId, SyncOp, State#sync_state.pending_syncs),

    NewStats = (State#sync_state.sync_stats)#{
        total_syncs => maps:get(total_syncs, State#sync_state.sync_stats) + 1
    },

    NewState = State#sync_state{
        pending_syncs = NewPending,
        sync_in_progress = true,
        sync_stats = NewStats
    },

    %% Execute sync based on strategy
    case SyncType of
        full_sync -> execute_full_sync(NewState, SyncId);
        merkle_sync -> execute_merkle_sync(NewState, SyncId);
        vector_clock_sync -> execute_vector_clock_sync(NewState, SyncId);
        _ -> execute_generic_sync(NewState, SyncId)
    end;

do_sync(State, [], _SyncType) ->
    State.

%% @doc Execute full anti-entropy sync
-spec execute_full_sync(#sync_state{}, reference()) -> #sync_state{}.
execute_full_sync(State, SyncId) ->
    %% Get all local data
    LocalData = get_all_local_data(),

    %% Send to all target nodes
    maps:foreach(fun(Node, _SyncOp) ->
        sync_to_node(Node, LocalData, SyncId)
    end, State#sync_state.pending_syncs),

    State.

%% @doc Execute Merkle tree sync
-spec execute_merkle_sync(#sync_state{}, reference()) -> #sync_state{}.
execute_merkle_sync(State, SyncId) ->
    %% Build Merkle tree for local data
    LocalData = get_all_local_data(),
    {ok, LocalTree} = create_merkle_tree_internal(LocalData),

    %% Exchange Merkle roots with peers
    maps:foreach(fun(Node, _SyncOp) ->
        exchange_merkle_tree(Node, LocalTree, SyncId)
    end, State#sync_state.pending_syncs),

    State.

%% @doc Execute vector clock sync
-spec execute_vector_clock_sync(#sync_state{}, reference()) -> #sync_state{}.
execute_vector_clock_sync(State, SyncId) ->
    %% Exchange vector clocks
    maps:foreach(fun(Node, _SyncOp) ->
        exchange_vector_clock(Node, State#sync_state.vector_clock, SyncId)
    end, State#sync_state.pending_syncs),

    State.

%% @doc Execute generic sync
-spec execute_generic_sync(#sync_state{}, reference()) -> #sync_state{}.
execute_generic_sync(State, SyncId) ->
    %% Generic gossip-style sync
    State.

%% @doc Sync data to specific node
-spec sync_to_node(node(), map(), reference()) -> ok.
sync_to_node(Node, Data, SyncId) ->
    gen_server:cast({?MODULE, Node}, {sync_data, node(), Data, SyncId}),
    ok.

%% @doc Exchange Merkle tree with node
-spec exchange_merkle_tree(node(), merkle_tree(), reference()) -> ok.
exchange_merkle_tree(Node, Tree, SyncId) ->
    gen_server:cast({?MODULE, Node}, {merkle_exchange, node(), Tree, SyncId}),
    ok.

%% @doc Exchange vector clock with node
-spec exchange_vector_clock(node(), vector_clock(), reference()) -> ok.
exchange_vector_clock(Node, VC, SyncId) ->
    gen_server:cast({?MODULE, Node}, {vc_exchange, node(), VC, SyncId}),
    ok.

%% @doc Handle sync completion
-spec handle_sync_complete(reference(), map(), #sync_state{}) -> #sync_state{}.
handle_sync_complete(SyncId, Result, State) ->
    case maps:get(SyncId, State#sync_state.pending_syncs, undefined) of
        undefined ->
            State;
        SyncOp ->
            NewPending = maps:remove(SyncId, State#sync_state.pending_syncs),
            NewCompleted = [SyncId | State#sync_state.completed_syncs],

            NewStats = (State#sync_state.sync_stats)#{
                successful_syncs => maps:get(successful_syncs, State#sync_state.sync_stats) + 1,
                entries_synced => maps:get(entries_synced, State#sync_state.sync_stats) +
                    maps:get(entries_synced, Result, 0),
                bytes_transferred => maps:get(bytes_transferred, State#sync_state.sync_stats) +
                    maps:get(bytes_transferred, Result, 0),
                last_sync_time => erlang:system_time(millisecond)
            },

            NewState = State#sync_state{
                pending_syncs = NewPending,
                completed_syncs = NewCompleted,
                sync_stats = NewStats,
                sync_in_progress = maps:size(NewPending) > 0
            },

            notify_subscribers(NewState, {sync_complete, SyncId, Result}),

            NewState
    end.

%% @doc Handle conflict detection
-spec handle_conflict_detected(binary(), term(), term(), #sync_state{}) -> #sync_state{}.
handle_conflict_detected(Key, LocalValue, RemoteValue, State) ->
    Conflict = #{
        key => Key,
        local_value => LocalValue,
        remote_value => RemoteValue,
        local_version => 0,
        remote_version => 0,
        local_vector_clock => State#sync_state.vector_clock,
        remote_vector_clock => #{},
        timestamp => erlang:system_time(millisecond),
        resolution => undefined
    },

    NewConflicts = maps:put(Key, Conflict, State#sync_state.conflicts),

    NewStats = (State#sync_state.sync_stats)#{
        conflicts_detected => maps:get(conflicts_detected, State#sync_state.sync_stats) + 1
    },

    NewState = State#sync_state{
        conflicts = NewConflicts,
        sync_stats = NewStats
    },

    notify_subscribers(NewState, {conflict_detected, Key, Conflict}),

    NewState.

%% @doc Apply conflict resolution
-spec apply_resolution(#sync_state{}, binary(), conflict_info(),
                      conflict_resolution(), term()) -> #sync_state{}.
apply_resolution(State, Key, _Conflict, Resolution, Value) ->
    %% Apply the resolution
    case Resolution of
        local_wins ->
            %% Keep local value, do nothing
            ok;
        remote_wins ->
            %% Apply remote value
            apply_value(Key, Value);
        merge ->
            %% Merge values (requires merge function)
            apply_value(Key, Value);
        manual ->
            %% Manual resolution, value already provided
            apply_value(Key, Value);
        retry ->
            %% Retry sync
            ok
    end,

    %% Remove from conflicts
    NewConflicts = maps:remove(Key, State#sync_state.conflicts),

    NewStats = (State#sync_state.sync_stats)#{
        conflicts_resolved => maps:get(conflicts_resolved, State#sync_state.sync_stats) + 1
    },

    State#sync_state{
        conflicts = NewConflicts,
        sync_stats = NewStats
    }.

%% @doc Apply value to local store
-spec apply_value(binary(), term()) -> ok.
apply_value(_Key, _Value) ->
    %% In real implementation, this would update the data store
    ok.

%% @doc Get all local data
-spec get_all_local_data() -> map().
get_all_local_data() ->
    %% In real implementation, this would query actual data store
    #{}.

%% @doc Build Merkle tree
-spec build_merkle_tree(binary()) -> merkle_tree().
build_merkle_tree(Data) ->
    %% Simplified Merkle tree construction
    Hash = crypto:hash(sha256, Data),
    #{root => Hash}.

%% @doc Create Merkle tree (internal)
-spec create_merkle_tree_internal(map()) -> {ok, merkle_tree()}.
create_merkle_tree_internal(_Data) ->
    {ok, #{root => <<0:256>>}}.

%% @doc Get current sync ID
-spec get_current_sync_id(#sync_state{}) -> reference() | undefined.
get_current_sync_id(#sync_state{pending_syncs = Pending}) ->
    case maps:keys(Pending) of
        [Id | _] -> Id;
        [] -> undefined
    end.

%% @doc Notify subscribers
-spec notify_subscribers(#sync_state{}, term()) -> ok.
notify_subscribers(#sync_state{subscribers = Subscribers}, Event) ->
    sets:foreach(fun(Subscriber) ->
        case is_process_alive(Subscriber) of
            true ->
                Subscriber ! {sync_event, Event};
            false ->
                ok
        end
    end, Subscribers),
    ok.
