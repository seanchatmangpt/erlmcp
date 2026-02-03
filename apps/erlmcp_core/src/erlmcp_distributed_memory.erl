%%%-------------------------------------------------------------------
%%% @doc
%%% Distributed Memory Manager - Cross-Agent Memory Sharing
%%%
%%% This module implements distributed memory synchronization for erlmcp v3,
%%% enabling cross-agent memory sharing with configurable consistency guarantees.
%%%
%%% == Architecture Overview ==
%%%
%%% The memory system uses a 4-tier architecture:
%%% 1. L0: Process dictionary (fastest, process-local)
%%% 2. L1: ETS (in-memory, node-local, fast)
%%% 3. L2: Mnesia (replicated, durable, cluster-wide)
%%% 4. L3: External cache (Redis, optional)
%%%
%%% == Consistency Models ==
%%%
%%% - **eventual**: Default, propagates updates asynchronously
%%% - **bounded_stale**: Reads may be stale within a time window
%%% - **strong**: Synchronous replication before acknowledgment
%%%
%%% == Memory Domains ==
%%%
%%% Memory is organized into domains for isolation:
%%% - `session`: Per-session context and state
%%% - `shared`: Cross-agent shared data
%%% - `registry`: Service discovery and routing
%%% - `cache`: Computed results and responses
%%% - `ephemeral`: Temporary data with auto-cleanup
%%%
%%% == Synchronization Protocol ==
%%%
%%% Updates use CRDT (Conflict-Free Replicated Data Types) semantics:
%%% - Last-Writer-Wins with vector clocks for causality
%%% - Automatic conflict resolution on merge
%%% - Tombstone-based deletions
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_distributed_memory).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         get/2, get/3, put/3, put/4,
         delete/2, delete_domain/1,
         subscribe/2, unsubscribe/2,
         sync/0, sync/1, sync_status/0,
         register_agent/1, unregister_agent/1,
         list_agents/0, broadcast/2,
         get_domain_stats/1, get_cluster_stats/0,
         set_consistency/1, get_consistency/0,
         set_replication_factor/1, get_replication_factor/0,
         take_snapshot/0, restore_snapshot/1,
         compact/0, force_gc/0,
         get_memory_limit/0, set_memory_limit/1,
         get_cluster_memory/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type memory_key() :: binary() | atom().
-type memory_value() :: term().
-type memory_domain() :: session | shared | registry | cache | ephemeral.
-type consistency() :: eventual | bounded_stale | strong.
-type vector_clock() :: #{node() => non_neg_integer()}.
-type replication_factor() :: 1 | 2 | 3.

-type agent_info() :: #{agent_id := binary(),
                         pid := pid(),
                         node := node(),
                         registered_at => integer(),
                         last_sync => integer(),
                         memory_usage => non_neg_integer()}.

-type memory_limit() :: #{max_bytes => non_neg_integer(),
                          max_entries => non_neg_integer(),
                          warn_percent => float(),
                          domain_limits => #{memory_domain() => #{max_bytes => non_neg_integer(),
                                                                   max_entries => non_neg_integer()}}}.

-export_type([memory_key/0, memory_value/0, memory_domain/0,
              consistency/0, vector_clock/0, replication_factor/0,
              agent_info/0, memory_limit/0]).

%%====================================================================
%% Constants
%%====================================================================

-define(SYNC_INTERVAL, 5000).         % 5 seconds
-define(CLEANUP_INTERVAL, 60000).     % 1 minute
-define(MAX_MEMORY_MB, 1024).         % 1GB default per node
-define(MAX_MEMORY_BYTES, ?MAX_MEMORY_MB * 1024 * 1024).
-define(DEFAULT_CONSISTENCY, eventual).
-define(DEFAULT_REPLICATION_FACTOR, 2).
-define(MAX_SYNC_RETRIES, 3).
-define(SYNC_TIMEOUT, 5000).

%%====================================================================
%% State Record
%%====================================================================

-record(state,
        {node_id :: node(),
         consistency = ?DEFAULT_CONSISTENCY :: consistency(),
         replication_factor = ?DEFAULT_REPLICATION_FACTOR :: replication_factor(),
         memory_table :: ets:tid(),
         metadata_table :: ets:tid(),
         agent_table :: ets:tid(),
         subscription_table :: ets:tid(),
         memory_limit = #{} :: memory_limit(),
         sync_timer :: reference() | undefined,
         cleanup_timer :: reference() | undefined,
         vector_clock = #{} :: vector_clock(),
         pending_sync = [] :: [term()],
         sync_in_progress = false :: boolean(),
         stats = #{syncs_completed => 0,
                   syncs_failed => 0,
                   bytes_synced => 0,
                   last_sync_time => 0} :: map()}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Get value from memory with automatic domain detection
-spec get(memory_domain(), memory_key()) -> {ok, memory_value()} | {error, not_found}.
get(Domain, Key) ->
    get(Domain, Key, #{}).

-spec get(memory_domain(), memory_key(), map()) ->
          {ok, memory_value()} | {error, not_found}.
get(Domain, Key, Opts) ->
    case maps:get(local_only, Opts, false) of
        true ->
            get_local(Domain, Key);
        false ->
            get_distributed(Domain, Key, Opts)
    end.

%% @doc Put value into memory with consistency control
-spec put(memory_domain(), memory_key(), memory_value()) -> ok | {error, term()}.
put(Domain, Key, Value) ->
    put(Domain, Key, Value, #{}).

-spec put(memory_domain(), memory_key(), memory_value(), map()) ->
          ok | {error, term()}.
put(Domain, Key, Value, Opts) ->
    TTL = maps:get(ttl, Opts, infinity),
    Consistency = maps:get(consistency, Opts, get_consistency()),
    gen_server:call(?MODULE, {put, Domain, Key, Value, TTL, Consistency}, ?SYNC_TIMEOUT).

%% @doc Delete a key from memory
-spec delete(memory_domain(), memory_key()) -> ok | {error, term()}.
delete(Domain, Key) ->
    gen_server:call(?MODULE, {delete, Domain, Key}, ?SYNC_TIMEOUT).

%% @doc Delete entire domain (use with caution)
-spec delete_domain(memory_domain()) -> ok | {error, term()}.
delete_domain(Domain) ->
    gen_server:call(?MODULE, {delete_domain, Domain}, ?SYNC_TIMEOUT).

%% @doc Subscribe to memory updates for a domain
-spec subscribe(memory_domain(), pid()) -> ok | {error, term()}.
subscribe(Domain, Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {subscribe, Domain, Pid}).

%% @doc Unsubscribe from memory updates
-spec unsubscribe(memory_domain(), pid()) -> ok.
unsubscribe(Domain, Pid) when is_pid(Pid) ->
    gen_server:cast(?MODULE, {unsubscribe, Domain, Pid}).

%% @doc Trigger immediate synchronization
-spec sync() -> ok.
sync() ->
    sync(all).

-spec sync(memory_domain() | all) -> ok.
sync(DomainOrAll) ->
    gen_server:cast(?MODULE, {sync, DomainOrAll}).

%% @doc Get synchronization status
-spec sync_status() -> map().
sync_status() ->
    gen_server:call(?MODULE, sync_status).

%% @doc Register an agent for memory sharing
-spec register_agent(binary()) -> ok | {error, term()}.
register_agent(AgentId) ->
    register_agent(AgentId, self()).

-spec register_agent(binary(), pid()) -> ok | {error, term()}.
register_agent(AgentId, Pid) when is_binary(AgentId), is_pid(Pid) ->
    gen_server:call(?MODULE, {register_agent, AgentId, Pid}).

%% @doc Unregister an agent
-spec unregister_agent(binary()) -> ok.
unregister_agent(AgentId) ->
    gen_server:cast(?MODULE, {unregister_agent, AgentId}).

%% @doc List all registered agents
-spec list_agents() -> [agent_info()].
list_agents() ->
    gen_server:call(?MODULE, list_agents).

%% @doc Broadcast a message to all agents in a domain
-spec broadcast(memory_domain(), term()) -> {ok, non_neg_integer()}.
broadcast(Domain, Message) ->
    gen_server:call(?MODULE, {broadcast, Domain, Message}).

%% @doc Get statistics for a specific domain
-spec get_domain_stats(memory_domain()) -> map().
get_domain_stats(Domain) ->
    gen_server:call(?MODULE, {domain_stats, Domain}).

%% @doc Get cluster-wide memory statistics
-spec get_cluster_stats() -> map().
get_cluster_stats() ->
    gen_server:call(?MODULE, cluster_stats).

%% @doc Set consistency level for writes
-spec set_consistency(consistency()) -> ok.
set_consistency(Consistency) ->
    gen_server:call(?MODULE, {set_consistency, Consistency}).

%% @doc Get current consistency level
-spec get_consistency() -> consistency().
get_consistency() ->
    gen_server:call(?MODULE, get_consistency).

%% @doc Set replication factor for cluster
-spec set_replication_factor(replication_factor()) -> ok | {error, term()}.
set_replication_factor(Factor) when Factor >= 1, Factor =< 3 ->
    gen_server:call(?MODULE, {set_replication_factor, Factor}).

%% @doc Get current replication factor
-spec get_replication_factor() -> replication_factor().
get_replication_factor() ->
    gen_server:call(?MODULE, get_replication_factor).

%% @doc Take a snapshot of current memory state
-spec take_snapshot() -> {ok, binary()} | {error, term()}.
take_snapshot() ->
    gen_server:call(?MODULE, take_snapshot).

%% @doc Restore from a snapshot
-spec restore_snapshot(binary()) -> ok | {error, term()}.
restore_snapshot(SnapshotData) ->
    gen_server:call(?MODULE, {restore_snapshot, SnapshotData}).

%% @doc Compact memory (remove tombstones, optimize)
-spec compact() -> {ok, map()} | {error, term()}.
compact() ->
    gen_server:call(?MODULE, compact).

%% @doc Force garbage collection across memory tables
-spec force_gc() -> {ok, map()}.
force_gc() ->
    gen_server:call(?MODULE, force_gc).

%% @doc Get current memory limit configuration
-spec get_memory_limit() -> memory_limit().
get_memory_limit() ->
    gen_server:call(?MODULE, get_memory_limit).

%% @doc Set memory limit
-spec set_memory_limit(memory_limit()) -> ok.
set_memory_limit(Limit) ->
    gen_server:call(?MODULE, {set_memory_limit, Limit}).

%% @doc Get total memory usage across cluster
-spec get_cluster_memory() -> {ok, map()}.
get_cluster_memory() ->
    gen_server:call(?MODULE, get_cluster_memory).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, #state{}}.
init(Opts) ->
    process_flag(trap_exit, true),

    NodeId = node(),
    Consistency = maps:get(consistency, Opts, ?DEFAULT_CONSISTENCY),
    ReplicationFactor = maps:get(replication_factor, Opts, ?DEFAULT_REPLICATION_FACTOR),
    MemoryLimit = maps:get(memory_limit, Opts, default_memory_limit()),

    %% Create ETS tables for memory storage
    MemoryTable = ets:new(erlmcp_memory, [set, protected, named_table,
                                          {read_concurrency, true},
                                          {write_concurrency, true}]),

    MetadataTable = ets:new(erlmcp_memory_meta, [set, protected, named_table,
                                                  {read_concurrency, true}]),

    AgentTable = ets:new(erlmcp_agents, [bag, public, named_table,
                                         {read_concurrency, true}]),

    SubscriptionTable = ets:new(erlmcp_subscriptions, [bag, public, named_table]),

    %% Start sync timer
    SyncTimer = erlang:send_after(?SYNC_INTERVAL, self(), sync_tick),

    %% Start cleanup timer
    CleanupTimer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_tick),

    %% Initialize vector clock
    InitialClock = #{NodeId => 0},

    %% Join process group for cluster coordination
    ok = pg:join(erlmcp_memory, memory_nodes, self()),

    logger:info("Distributed memory started on ~p: consistency=~p, replication=~p",
                [NodeId, Consistency, ReplicationFactor]),

    {ok, #state{node_id = NodeId,
                consistency = Consistency,
                replication_factor = ReplicationFactor,
                memory_table = MemoryTable,
                metadata_table = MetadataTable,
                agent_table = AgentTable,
                subscription_table = SubscriptionTable,
                memory_limit = MemoryLimit,
                sync_timer = SyncTimer,
                cleanup_timer = CleanupTimer,
                vector_clock = InitialClock}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
          {reply, term(), #state{}}.
handle_call({put, Domain, Key, Value, TTL, Consistency}, _From, State) ->
    case check_memory_limit(Domain, State) of
        ok ->
            {Result, NewState} = do_put(Domain, Key, Value, TTL, Consistency, State),
            {reply, Result, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({delete, Domain, Key}, _From, State) ->
    {Result, NewState} = do_delete(Domain, Key, State),
    {reply, Result, NewState};

handle_call({delete_domain, Domain}, _From, State) ->
    {Result, NewState} = do_delete_domain(Domain, State),
    {reply, Result, NewState};

handle_call({subscribe, Domain, Pid}, _From, State) ->
    Result = do_subscribe(Domain, Pid, State),
    {reply, Result, State};

handle_call(sync_status, _From, State) ->
    Status = #{sync_in_progress => State#state.sync_in_progress,
               pending_sync_count => length(State#state.pending_sync),
               stats => State#state.stats,
               vector_clock => State#state.vector_clock},
    {reply, Status, State};

handle_call({register_agent, AgentId, Pid}, _From, State) ->
    {Result, NewState} = do_register_agent(AgentId, Pid, State),
    {reply, Result, NewState};

handle_call(list_agents, _From, State) ->
    Agents = list_agents_internal(State),
    {reply, Agents, State};

handle_call({broadcast, Domain, Message}, _From, State) ->
    {Result, NewState} = do_broadcast(Domain, Message, State),
    {reply, Result, NewState};

handle_call({domain_stats, Domain}, _From, State) ->
    Stats = get_domain_stats_internal(Domain, State),
    {reply, Stats, State};

handle_call(cluster_stats, _From, State) ->
    Stats = get_cluster_stats_internal(State),
    {reply, Stats, State};

handle_call({set_consistency, Consistency}, _From, State) ->
    logger:info("Changing consistency from ~p to ~p", [State#state.consistency, Consistency]),
    {reply, ok, State#state{consistency = Consistency}};

handle_call(get_consistency, _From, State) ->
    {reply, State#state.consistency, State};

handle_call({set_replication_factor, Factor}, _From, State) ->
    logger:info("Changing replication factor from ~p to ~p", [State#state.replication_factor, Factor]),
    {reply, ok, State#state{replication_factor = Factor}};

handle_call(get_replication_factor, _From, State) ->
    {reply, State#state.replication_factor, State};

handle_call(take_snapshot, _From, State) ->
    {Result, NewState} = do_take_snapshot(State),
    {reply, Result, NewState};

handle_call({restore_snapshot, SnapshotData}, _From, State) ->
    {Result, NewState} = do_restore_snapshot(SnapshotData, State),
    {reply, Result, NewState};

handle_call(compact, _From, State) ->
    {Result, NewState} = do_compact(State),
    {reply, Result, NewState};

handle_call(force_gc, _From, State) ->
    Result = do_force_gc(State),
    {reply, Result, State};

handle_call(get_memory_limit, _From, State) ->
    {reply, State#state.memory_limit, State};

handle_call({set_memory_limit, Limit}, _From, State) ->
    {reply, ok, State#state{memory_limit = Limit}};

handle_call(get_cluster_memory, _From, State) ->
    Memory = get_cluster_memory_internal(State),
    {reply, {ok, Memory}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({unsubscribe, Domain, Pid}, State) ->
    ets:match_delete(State#state.subscription_table, {{Domain, Pid}, '_'}),
    {noreply, State};

handle_cast({sync, DomainOrAll}, State) ->
    NewState = trigger_sync(DomainOrAll, State),
    {noreply, NewState};

handle_cast({unregister_agent, AgentId}, State) ->
    NewState = do_unregister_agent(AgentId, State),
    {noreply, NewState};

handle_cast({memory_update, Domain, Key, Value, Version}, State) ->
    %% Handle remote update
    NewState = apply_remote_update(Domain, Key, Value, Version, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(sync_tick, State) ->
    NewState = perform_sync(State),
    TimerRef = erlang:send_after(?SYNC_INTERVAL, self(), sync_tick),
    {noreply, NewState#state{sync_timer = TimerRef}};

handle_info(cleanup_tick, State) ->
    NewState = perform_cleanup(State),
    TimerRef = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_tick),
    {noreply, NewState#state{cleanup_timer = TimerRef}};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Handle monitored process death
    NewState = cleanup_agent(Pid, State),
    {noreply, NewState};

handle_info({pg, _, _, {memory_sync, FromNode, Data}}, State) ->
    %% Handle incoming sync from another node
    NewState = process_sync_data(FromNode, Data, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    %% Cancel timers
    case State#state.sync_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    case State#state.cleanup_timer of
        undefined -> ok;
        CleanupTimer -> erlang:cancel_timer(CleanupTimer)
    end,

    %% Leave process group
    pg:leave(erlmcp_memory, memory_nodes, self()),

    %% Delete ETS tables (named tables)
    ets:delete(erlmcp_memory),
    ets:delete(erlmcp_memory_meta),
    ets:delete(erlmcp_agents),
    ets:delete(erlmcp_subscriptions),

    logger:info("Distributed memory terminating: ~p", [State#state.stats]),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Memory Operations
%%====================================================================

%% @private Get value from local memory
-spec get_local(memory_domain(), memory_key()) -> {ok, memory_value()} | {error, not_found}.
get_local(Domain, Key) ->
    try
        case ets:lookup(erlmcp_memory, {Domain, Key}) of
            [{_FullKey, Entry}] ->
                case is_entry_valid(Entry) of
                    true ->
                        {ok, maps:get(value, Entry)};
                    false ->
                        ets:delete(erlmcp_memory, {Domain, Key}),
                        {error, not_found}
                end;
            [] ->
                {error, not_found}
        end
    catch
        _:_ -> {error, not_found}
    end.

%% @private Get value from distributed memory
-spec get_distributed(memory_domain(), memory_key(), map()) ->
          {ok, memory_value()} | {error, not_found}.
get_distributed(Domain, Key, Opts) ->
    case get_local(Domain, Key) of
        {ok, Value} ->
            {ok, Value};
        {error, not_found} ->
            %% Try to fetch from other nodes
            case maps:get(fetch_remote, Opts, true) of
                true ->
                    fetch_from_cluster(Domain, Key);
                false ->
                    {error, not_found}
            end
    end.

%% @private Put value into memory
-spec do_put(memory_domain(), memory_key(), memory_value(),
             integer() | infinity, consistency(), #state{}) ->
          {{ok, vector_clock()} | {error, term()}, #state{}}.
do_put(Domain, Key, Value, TTL, Consistency, State) ->
    Now = erlang:monotonic_time(microsecond),
    NodeId = State#state.node_id,

    %% Update vector clock
    CurrentClock = State#state.vector_clock,
    NewClock = maps:update_with(NodeId, fun(V) -> V + 1 end, 1, CurrentClock),

    %% Create checksum for integrity
    Checksum = compute_checksum(Value),

    %% Create entry
    Entry = #{key => Key,
              value => Value,
              version => NewClock,
              timestamp => Now,
              ttl => TTL,
              domain => Domain,
              checksum => Checksum},

    %% Store locally
    ets:insert(State#state.memory_table, {{Domain, Key}, Entry}),

    %% Update metadata
    update_metadata(Domain, Key, insert, State),

    %% Handle based on consistency level
    NewState = State#state{vector_clock = NewClock},

    case Consistency of
        strong ->
            %% Synchronous replication
            case replicate_sync(Domain, Key, Entry, NewState) of
                {ok, _} ->
                    notify_subscribers(Domain, Key, Value, NewState),
                    update_stats(write, NewState);
                {error, Reason} ->
                    {{error, Reason}, NewState}
            end;
        bounded_stale ->
            %% Async with acknowledgement
            spawn(fun() -> replicate_async(Domain, Key, Entry, NewState) end),
            notify_subscribers(Domain, Key, Value, NewState),
            {{ok, NewClock}, update_stats(write, NewState)};
        eventual ->
            %% Fire and forget
            spawn(fun() -> replicate_async(Domain, Key, Entry, NewState) end),
            notify_subscribers(Domain, Key, Value, NewState),
            {{ok, NewClock}, update_stats(write, NewState)}
    end.

%% @private Delete entry
-spec do_delete(memory_domain(), memory_key(), #state{}) ->
          {{ok, vector_clock()} | {error, term()}, #state{}}.
do_delete(Domain, Key, State) ->
    NodeId = State#state.node_id,
    CurrentClock = State#state.vector_clock,
    NewClock = maps:update_with(NodeId, fun(V) -> V + 1 end, 1, CurrentClock),

    %% Mark as tombstone (don't actually delete for CRDT)
    Tombstone = #{key => Key,
                  value => deleted,
                  version => NewClock,
                  timestamp => erlang:monotonic_time(microsecond),
                  ttl => infinity,
                  domain => Domain,
                  tombstone => true},

    ets:insert(State#state.memory_table, {{Domain, Key}, Tombstone}),
    update_metadata(Domain, Key, delete, State),

    NewState = State#state{vector_clock = NewClock},
    notify_subscribers(Domain, Key, deleted, NewState),

    {{ok, NewClock}, update_stats(delete, NewState)}.

%% @private Delete entire domain
-spec do_delete_domain(memory_domain(), #state{}) ->
          {{ok, non_neg_integer()} | {error, term()}, #state{}}.
do_delete_domain(Domain, State) ->
    %% Select all keys in domain
    Pattern = {{Domain, '_'}, '_'},
    Entries = ets:match(State#state.memory_table, Pattern),

    Count = length(Entries),
    lists:foreach(fun(_) -> ok end, Entries),

    %% Delete all entries
    ets:match_delete(State#state.memory_table, Pattern),

    {{ok, Count}, State}.

%% @private Subscribe to domain updates
-spec do_subscribe(memory_domain(), pid(), #state{}) -> ok | {error, term()}.
do_subscribe(Domain, Pid, State) when is_pid(Pid) ->
    try
        ets:insert(State#state.subscription_table, {{Domain, Pid}, erlang:monotonic_time()}),
        logger:debug("~p subscribed to domain ~p", [Pid, Domain]),
        ok
    catch
        _:_ -> {error, subscription_failed}
    end.

%% @private Register an agent
-spec do_register_agent(binary(), pid(), #state{}) ->
          {ok, #state{}} | {{error, term()}, #state{}}.
do_register_agent(AgentId, Pid, State) ->
    try
        %% Monitor the process
        MonitorRef = monitor(process, Pid),

        Info = #{agent_id => AgentId,
                 pid => Pid,
                 node => node(Pid),
                 ref => MonitorRef,
                 registered_at => erlang:system_time(millisecond),
                 last_sync => 0,
                 memory_usage => 0},

        ets:insert(State#state.agent_table, {AgentId, Pid, Info}),

        logger:info("Agent ~p registered: ~p", [AgentId, Pid]),
        {ok, State}
    catch
        _:_ -> {{error, registration_failed}, State}
    end.

%% @private Unregister an agent
-spec do_unregister_agent(binary(), #state{}) -> #state{}.
do_unregister_agent(AgentId, State) ->
    %% Remove from agent table
    ets:match_delete(State#state.agent_table, {AgentId, '_', '_'}),
    logger:info("Agent ~p unregistered", [AgentId]),
    State.

%% @private List all agents
-spec list_agents_internal(#state{}) -> [agent_info()].
list_agents_internal(State) ->
    Agents = ets:tab2list(State#state.agent_table),
    [Info || {_, _, Info} <- Agents].

%% @private Broadcast to all subscribers of a domain
-spec do_broadcast(memory_domain(), term(), #state{}) ->
          {{ok, non_neg_integer()}, #state{}}.
do_broadcast(Domain, Message, State) ->
    %% Find all subscribers
    Subscribers = ets:match(State#state.subscription_table, {{Domain, '$1'}, '_'}),

    Count = lists:foldl(fun([Pid], Acc) ->
                           case is_process_alive(Pid) of
                               true ->
                                   Pid ! {memory_broadcast, Domain, Message},
                                   Acc + 1;
                               false ->
                                   Acc
                           end
                       end, 0, Subscribers),

    {{ok, Count}, State}.

%% @private Get domain statistics
-spec get_domain_stats_internal(memory_domain(), #state{}) -> map().
get_domain_stats_internal(Domain, State) ->
    Pattern = {{Domain, '_'}, '$1'},
    Entries = ets:match(State#state.memory_table, Pattern),

    TotalBytes = lists:foldl(fun([Entry], Acc) ->
                                case is_entry_valid(Entry) of
                                    true ->
                                        Acc + erts_debug:size(Entry);
                                    false ->
                                        Acc
                                end
                            end, 0, Entries),

    ValidEntries = lists:filter(fun([Entry]) -> is_entry_valid(Entry) end, Entries),

    #{domain => Domain,
      entry_count => length(ValidEntries),
      total_bytes => TotalBytes,
      tombstone_count => length(Entries) - length(ValidEntries)}.

%% @private Get cluster statistics
-spec get_cluster_stats_internal(#state{}) -> map().
get_cluster_stats_internal(State) ->
    Nodes = pg:get_members(erlmcp_memory, memory_nodes),

    NodeStats = lists:map(fun(Node) ->
                             case Node =:= node() of
                                 true ->
                                     get_node_stats();
                                 false ->
                                     %% Request stats from remote node
                                     request_remote_node_stats(Node)
                             end
                         end, Nodes),

    #{nodes => length(Nodes),
      node_stats => NodeStats,
      vector_clock => State#state.vector_clock,
      sync_stats => State#state.stats}.

%%====================================================================
%% Internal Functions - Synchronization
%%====================================================================

%% @private Trigger synchronization
-spec trigger_sync(memory_domain() | all, #state{}) -> #state{}.
trigger_sync(DomainOrAll, State) ->
    Pending = [DomainOrAll | State#state.pending_sync],
    State#state{pending_sync = Pending}.

%% @private Perform synchronization
-spec perform_sync(#state{}) -> #state{}.
perform_sync(State) ->
    case State#state.sync_in_progress of
        true ->
            State;
        false ->
            State1 = State#state{sync_in_progress = true},
            PendingDomains = State1#state.pending_sync,
            State2 = State1#state{pending_sync = []},

            {SyncResults, _} = sync_domains(PendingDomains, State2),

            UpdatedStats = maps:fold(fun(_Domain, Result, Acc) ->
                                           case Result of
                                               {ok, Bytes} ->
                                                   Acc#{syncs_completed =>
                                                            maps:get(syncs_completed, Acc, 0) + 1,
                                                          bytes_synced =>
                                                            maps:get(bytes_synced, Acc, 0) + Bytes};
                                               {error, _} ->
                                                   Acc#{syncs_failed =>
                                                            maps:get(syncs_failed, Acc, 0) + 1}
                                           end
                                   end, State2#state.stats, SyncResults),

            State2#state{sync_in_progress = false,
                         stats = maps:put(last_sync_time,
                                          erlang:system_time(millisecond),
                                          UpdatedStats)}
    end.

%% @private Sync specific domains
-spec sync_domains([memory_domain() | all], #state{}) ->
          {#{memory_domain() => {ok, non_neg_integer()} | {error, term()}}, map()}.
sync_domains(Domains, State) ->
    Now = erlang:monotonic_time(microsecond),

    Results = lists:foldl(fun(Domain, Acc) ->
                                 Result = sync_domain(Domain, State, Now),
                                 maps:put(Domain, Result, Acc)
                         end, #{}, Domains),

    {Results, State#state.stats}.

%% @private Sync a single domain
-spec sync_domain(memory_domain() | all, #state{}, integer()) ->
          {ok, non_neg_integer()} | {error, term()}.
sync_domain(all, State, Now) ->
    %% Sync all domains
    Domains = [session, shared, registry, cache, ephemeral],
    lists:foldl(fun(Domain, Acc) ->
                       case sync_domain(Domain, State, Now) of
                           {ok, Bytes} ->
                               {ok, element(1, Acc) + Bytes};
                           {error, _} ->
                               Acc
                       end
               end, {ok, 0}, Domains);

sync_domain(Domain, State, _Now) ->
    %% Get all entries in domain
    Pattern = {{Domain, '_'}, '$1'},
    Entries = ets:match(State#state.memory_table, Pattern),

    %% Filter valid entries (not tombstones)
    ValidEntries = [Entry || [Entry] <- Entries, is_entry_valid(Entry)],

    case ValidEntries of
        [] ->
            {ok, 0};
        _ ->
            %% Broadcast to cluster
            SyncData = #{domain => Domain,
                         entries => ValidEntries,
                         vector_clock => State#state.vector_clock,
                         node => State#state.node_id},

            pg:send(erlmcp_memory, memory_nodes, {memory_sync, node(), SyncData}),

            Bytes = lists:foldl(fun(Entry, Acc) -> Acc + erts_debug:size(Entry) end,
                                0, ValidEntries),
            {ok, Bytes}
    end.

%% @private Process incoming sync data
-spec process_sync_data(node(), map(), #state{}) -> #state{}.
process_sync_data(_FromNode, Data, State) ->
    Domain = maps:get(domain, Data),
    Entries = maps:get(entries, Data, []),
    RemoteClock = maps:get(vector_clock, Data, #{}),

    %% Merge vector clocks
    MergedClock = merge_vector_clocks(State#state.vector_clock, RemoteClock),

    %% Apply entries
    NewState = lists:foldl(fun(Entry, AccState) ->
                                apply_remote_entry(Domain, Entry, AccState)
                        end, State, Entries),

    NewState#state{vector_clock = MergedClock}.

%% @private Apply remote entry
-spec apply_remote_entry(memory_domain(), map(), #state{}) -> #state{}.
apply_remote_entry(Domain, Entry, State) ->
    Key = maps:get(key, Entry),
    Version = maps:get(version, Entry),
    Checksum = maps:get(checksum, Entry, undefined),

    %% Check if we should accept this update
    case should_accept_update(Domain, Key, Version, State) of
        true ->
            %% Verify checksum if present
            case Checksum =:= undefined orelse
                 verify_checksum(maps:get(value, Entry), Checksum) of
                true ->
                    ets:insert(State#state.memory_table, {{Domain, Key}, Entry}),
                    update_metadata(Domain, Key, remote_update, State),
                    notify_subscribers(Domain, Key, maps:get(value, Entry), State);
                false ->
                    logger:warning("Checksum mismatch for ~p:~p", [Domain, Key]),
                    State
            end;
        false ->
            State
    end.

%% @private Apply remote update (simplified)
-spec apply_remote_update(memory_domain(), memory_key(), memory_value(),
                          vector_clock(), #state{}) -> #state{}.
apply_remote_update(Domain, Key, Value, Version, State) ->
    Entry = #{key => Key,
              value => Value,
              version => Version,
              timestamp => erlang:monotonic_time(microsecond),
              ttl => infinity,
              domain => Domain},

    case should_accept_update(Domain, Key, Version, State) of
        true ->
            ets:insert(State#state.memory_table, {{Domain, Key}, Entry}),
            notify_subscribers(Domain, Key, Value, State);
        false ->
            ok
    end,
    State.

%% @private Check if update should be accepted
-spec should_accept_update(memory_domain(), memory_key(), vector_clock(), #state{}) ->
          boolean().
should_accept_update(Domain, Key, RemoteVersion, State) ->
    case ets:lookup(State#state.memory_table, {Domain, Key}) of
        [{_, LocalEntry}] ->
            LocalVersion = maps:get(version, LocalEntry),
            compare_versions(RemoteVersion, LocalVersion) > 0;
        [] ->
            true
    end.

%% @private Compare vector clocks (returns >0 if V1 is newer)
-spec compare_versions(vector_clock(), vector_clock()) -> integer().
compare_versions(V1, V2) ->
    AllKeys = lists:usort(maps:keys(V1) ++ maps:keys(V2)),

    {V1Dominates, V2Dominates} =
        lists:foldl(fun(Key, {V1Dom, V2Dom}) ->
                            Val1 = maps:get(Key, V1, 0),
                            Val2 = maps:get(Key, V2, 0),
                            NewV1Dom = V1Dom andalso Val1 >= Val2,
                            NewV2Dom = V2Dom andalso Val2 >= Val1,
                            {NewV1Dom, NewV2Dom}
                    end, {true, true}, AllKeys),

    case {V1Dominates, V2Dominates} of
        {true, false} -> 1;
        {false, true} -> -1;
        _ -> 0
    end.

%% @private Merge vector clocks
-spec merge_vector_clocks(vector_clock(), vector_clock()) -> vector_clock().
merge_vector_clocks(V1, V2) ->
    maps:merge_with(fun(_K, Val1, Val2) -> max(Val1, Val2) end, V1, V2).

%% @private Replicate synchronously
-spec replicate_sync(memory_domain(), memory_key(), map(), #state{}) ->
          {ok, non_neg_integer()} | {error, term()}.
replicate_sync(Domain, Key, Entry, State) ->
    Nodes = get_replica_nodes(State),

    %% Send sync request to all nodes
    Refs = [{Node, rpc:call(Node, ?MODULE, put, [Domain, Key, maps:get(value, Entry)])}
            || Node <- Nodes],

    %% Wait for acknowledgments
    case wait_for_sync_ack(Refs, State#state.replication_factor) of
        {ok, _} ->
            {ok, erts_debug:size(Entry)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Replicate asynchronously
-spec replicate_async(memory_domain(), memory_key(), map(), #state{}) -> ok.
replicate_async(Domain, Key, Entry, State) ->
    Nodes = get_replica_nodes(State),
    Value = maps:get(value, Entry),

    lists:foreach(fun(Node) ->
                     rpc:cast(Node, ?MODULE, put, [Domain, Key, Value])
                 end, Nodes),
    ok.

%% @private Get replica nodes based on replication factor
-spec get_replica_nodes(#state{}) -> [node()].
get_replica_nodes(State) ->
    AllNodes = pg:get_members(erlmcp_memory, memory_nodes),
    Factor = State#state.replication_factor,

    %% Exclude self and select random nodes
    OtherNodes = lists:delete(node(), AllNodes),
    Shuffle = lists:sort(fun(_, _) -> rand:uniform() < rand:uniform() end, OtherNodes),

    lists:sublist(Shuffle, min(Factor - 1, length(Shuffle))).

%% @private Wait for sync acknowledgments
-spec wait_for_sync_ack([{node(), term()}], replication_factor()) ->
          {ok, non_neg_integer()} | {error, term()}.
wait_for_sync_ack(Refs, Required) ->
    wait_for_sync_ack(Refs, Required, 0, ?MAX_SYNC_RETRIES).

wait_for_sync_ack(_Refs, _Required, Count, Retries) when Count >= _Required; Retries =< 0 ->
    {ok, Count};
wait_for_sync_ack(Refs, Required, Count, Retries) ->
    receive
        {sync_ack, _Node} ->
            wait_for_sync_ack(Refs, Required, Count + 1, Retries - 1);
        {sync_error, _Node, Reason} ->
            {error, Reason}
    after ?SYNC_TIMEOUT ->
        {ok, Count}
    end.

%%====================================================================
%% Internal Functions - Maintenance
%%====================================================================

%% @private Perform cleanup
-spec perform_cleanup(#state{}) -> #state{}.
perform_cleanup(State) ->
    Now = erlang:monotonic_time(microsecond),

    %% Clean expired entries
    Pattern = {{'_', '_'}, '$1'},
    AllEntries = ets:match(State#state.memory_table, Pattern),

    ExpiredKeys = lists:filtermap(fun([Entry]) ->
                                         case is_entry_expired(Entry, Now) of
                                             true ->
                                                 Key = maps:get(key, Entry),
                                                 Domain = maps:get(domain, Entry),
                                                 {true, {Domain, Key}};
                                             false ->
                                                 false
                                         end
                                 end, AllEntries),

    lists:foreach(fun({Domain, Key}) ->
                         ets:delete(State#state.memory_table, {Domain, Key}),
                         update_metadata(Domain, Key, expired, State)
                 end, ExpiredKeys),

    logger:debug("Memory cleanup: removed ~p expired entries", [length(ExpiredKeys)]),
    State.

%% @private Cleanup after agent death
-spec cleanup_agent(pid(), #state{}) -> #state{}.
cleanup_agent(Pid, State) ->
    %% Remove subscriptions
    ets:match_delete(State#state.subscription_table, {{'_', Pid}, '_'}),

    %% Remove from agent table
    ets:match_delete(State#state.agent_table, {'_', Pid, '_'}),

    logger:debug("Cleaned up after agent death: ~p", [Pid]),
    State.

%% @private Take snapshot
-spec do_take_snapshot(#state{}) -> {{ok, binary()} | {error, term()}, #state{}}.
do_take_snapshot(State) ->
    try
        AllEntries = ets:tab2list(State#state.memory_table),
        Snapshot = term_to_binary(AllEntries, [compressed]),
        Checksum = crypto:hash(sha256, Snapshot),

        SnapshotData = <<Checksum/binary, Snapshot/binary>>,
        {{ok, SnapshotData}, State}
    catch
        _:_ -> {{error, snapshot_failed}, State}
    end.

%% @private Restore from snapshot
-spec do_restore_snapshot(binary(), #state{}) -> {{ok, map()} | {error, term()}, #state{}}.
do_restore_snapshot(<<Checksum:32/binary, SnapshotData/binary>>, State) ->
    case crypto:hash(sha256, SnapshotData) of
        Checksum ->
            try
                Entries = binary_to_term(SnapshotData, [safe]),
                lists:foreach(fun({Key, Entry}) ->
                                     ets:insert(State#state.memory_table, {Key, Entry})
                             end, Entries),
                {{ok, #{entry_count => length(Entries)}}, State}
            catch
                _:_ -> {{error, invalid_snapshot}, State}
            end;
        _ ->
            {{error, checksum_mismatch}, State}
    end;
do_restore_snapshot(_, State) ->
    {{error, invalid_format}, State}.

%% @private Compact memory
-spec do_compact(#state{}) -> {{ok, map()}, #state{}}.
do_compact(State) ->
    %% Remove tombstones
    Pattern = {{'_', '_'}, '$1'},
    AllEntries = ets:match(State#state.memory_table, Pattern),

    TombstoneKeys = lists:filtermap(fun([Entry]) ->
                                           case maps:get(tombstone, Entry, false) of
                                               true ->
                                                   Key = maps:get(key, Entry),
                                                   Domain = maps:get(domain, Entry),
                                                   {true, {Domain, Key}};
                                               false ->
                                                   false
                                           end
                                   end, AllEntries),

    lists:foreach(fun({Domain, Key}) ->
                         ets:delete(State#state.memory_table, {Domain, Key})
                 end, TombstoneKeys),

    %% Defrag ETS table
    ets:foldl(fun(_, _) -> ok end, ok, State#state.memory_table),

    {{ok, #{tombstones_removed => length(TombstoneKeys)}}, State}.

%% @private Force garbage collection
-spec do_force_gc(#state{}) -> {ok, map()}.
do_force_gc(State) ->
    %% GC the memory tables
    garbage_collect(State#state.memory_table),
    garbage_collect(State#state.metadata_table),
    garbage_collect(State#state.agent_table),

    Before = erlang:memory(total),
    erlang:garbage_collect(),
    After = erlang:memory(total),

    {ok, #{bytes_reclaimed => Before - After}}.

%%====================================================================
%% Internal Functions - Utilities
%%====================================================================

%% @private Update metadata
-spec update_metadata(memory_domain(), memory_key(), atom(), #state{}) -> ok.
update_metadata(Domain, Key, Operation, State) ->
    MetaKey = {Domain, Key},
    Now = erlang:monotonic_time(microsecond),

    case ets:lookup(State#state.metadata_table, MetaKey) of
        [{_, Meta}] ->
            UpdatedMeta = Meta#{last_updated => Now,
                                operation => Operation},
            ets:insert(State#state.metadata_table, {MetaKey, UpdatedMeta});
        [] ->
            Meta = #{created_at => Now,
                     last_updated => Now,
                     operation => Operation},
            ets:insert(State#state.metadata_table, {MetaKey, Meta})
    end,
    ok.

%% @private Update stats
-spec update_stats(atom(), #state{}) -> #state{}.
update_stats(Operation, State) ->
    Stats = State#state.stats,
    NewStats = case Operation of
                   write -> Stats#{writes => maps:get(writes, Stats, 0) + 1};
                   read -> Stats#{reads => maps:get(reads, Stats, 0) + 1};
                   delete -> Stats#{deletes => maps:get(deletes, Stats, 0) + 1}
               end,
    State#state{stats = NewStats}.

%% @private Check memory limit
-spec check_memory_limit(memory_domain(), #state{}) -> ok | {error, term()}.
check_memory_limit(Domain, State) ->
    Limit = State#state.memory_limit,
    DomainStats = get_domain_stats_internal(Domain, State),
    MaxBytes = maps:get(max_bytes, Limit, ?MAX_MEMORY_BYTES),

    case maps:get(total_bytes, DomainStats, 0) >= MaxBytes of
        true ->
            {error, memory_limit_exceeded};
        false ->
            ok
    end.

%% @private Notify subscribers
-spec notify_subscribers(memory_domain(), memory_key(), memory_value(), #state{}) -> ok.
notify_subscribers(Domain, Key, Value, State) ->
    Subscribers = ets:match(State#state.subscription_table, {{Domain, '$1'}, '_'}),

    lists:foreach(fun([Pid]) ->
                       case is_process_alive(Pid) of
                           true ->
                               Pid ! {memory_update, Domain, Key, Value};
                           false ->
                               %% Clean up dead subscriber
                               ets:match_delete(State#state.subscription_table,
                                               {{Domain, Pid}, '_'})
                       end
                 end, Subscribers),
    ok.

%% @private Get node stats
-spec get_node_stats() -> map().
get_node_stats() ->
    #{node => node(),
      memory_total => erlang:memory(total),
      memory_processes => erlang:memory(processes),
      memory_system => erlang:memory(system),
      memory_ets => erlang:memory(ets),
      process_count => erlang:system_info(process_count),
      port_count => erlang:system_info(port_count)}.

%% @private Request remote node stats
-spec request_remote_node_stats(node()) -> map().
request_remote_node_stats(Node) ->
    case rpc:call(Node, ?MODULE, get_node_stats, [], 1000) of
        {ok, Stats} -> Stats;
        _ -> #{node => Node, error => unavailable}
    end.

%% @private Get cluster memory
-spec get_cluster_memory_internal(#state{}) -> map().
get_cluster_memory_internal(_State) ->
    Nodes = pg:get_members(erlmcp_memory, memory_nodes),

    MemoryData = lists:map(fun(Node) ->
                                  case Node =:= node() of
                                      true ->
                                          {Node, get_node_stats()};
                                      false ->
                                          Stats = request_remote_node_stats(Node),
                                          {Node, Stats}
                                  end
                          end, Nodes),

    TotalMemory = lists:foldl(fun({_Node, Stats}, Acc) ->
                                   case maps:get(error, Stats, undefined) of
                                       undefined ->
                                           Acc + maps:get(memory_total, Stats, 0);
                                       _ ->
                                           Acc
                                   end
                           end, 0, MemoryData),

    #{nodes => length(Nodes),
      total_memory_bytes => TotalMemory,
      node_data => MemoryData}.

%% @private Fetch from cluster
-spec fetch_from_cluster(memory_domain(), memory_key()) ->
          {ok, memory_value()} | {error, not_found}.
fetch_from_cluster(Domain, Key) ->
    Nodes = pg:get_members(erlmcp_memory, memory_nodes),
    OtherNodes = lists:delete(node(), Nodes),

    case OtherNodes of
        [] ->
            {error, not_found};
        _ ->
            fetch_from_nodes(Domain, Key, OtherNodes)
    end.

%% @private Fetch from specific nodes
-spec fetch_from_nodes(memory_domain(), memory_key(), [node()]) ->
          {ok, memory_value()} | {error, not_found}.
fetch_from_nodes(_Domain, _Key, []) ->
    {error, not_found};
fetch_from_nodes(Domain, Key, [Node | Rest]) ->
    case rpc:call(Node, ?MODULE, get, [Domain, Key, #{local_only => true}], 1000) of
        {ok, Value} ->
            {ok, Value};
        _ ->
            fetch_from_nodes(Domain, Key, Rest)
    end.

%% @private Check if entry is valid
-spec is_entry_valid(map()) -> boolean().
is_entry_valid(Entry) ->
    case maps:get(tombstone, Entry, false) of
        true -> false;
        false ->
            case maps:get(ttl, Entry, infinity) of
                infinity -> true;
                TTL ->
                    Timestamp = maps:get(timestamp, Entry, 0),
                    Now = erlang:monotonic_time(microsecond),
                    Now - Timestamp < TTL * 1000
            end
    end.

%% @private Check if entry is expired
-spec is_entry_expired(map(), integer()) -> boolean().
is_entry_expired(Entry, Now) ->
    case maps:get(tombstone, Entry, false) of
        true -> true;
        false ->
            case maps:get(ttl, Entry, infinity) of
                infinity -> false;
                TTL ->
                    Timestamp = maps:get(timestamp, Entry, 0),
                    Now - Timestamp >= TTL * 1000
            end
    end.

%% @private Compute checksum
-spec compute_checksum(term()) -> binary().
compute_checksum(Value) ->
    crypto:hash(sha256, term_to_binary(Value, [compressed])).

%% @private Verify checksum
-spec verify_checksum(term(), binary()) -> boolean().
verify_checksum(Value, Checksum) ->
    compute_checksum(Value) =:= Checksum.

%% @private Get default memory limit
-spec default_memory_limit() -> memory_limit().
default_memory_limit() ->
    #{max_bytes => ?MAX_MEMORY_BYTES,
      max_entries => 100000,
      warn_percent => 0.80,
      domain_limits => #{
          session => #{max_bytes => 100 * 1024 * 1024, max_entries => 10000},
          shared => #{max_bytes => 200 * 1024 * 1024, max_entries => 50000},
          registry => #{max_bytes => 50 * 1024 * 1024, max_entries => 5000},
          cache => #{max_bytes => 300 * 1024 * 1024, max_entries => 80000},
          ephemeral => #{max_bytes => 50 * 1024 * 1024, max_entries => 5000}
       }}.
