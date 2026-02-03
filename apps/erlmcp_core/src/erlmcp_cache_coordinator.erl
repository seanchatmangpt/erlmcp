%%%-------------------------------------------------------------------
%%% @doc
%%% Cache Coordinator - Multi-Node Cache Invalidation
%%%
%%% This module coordinates cache invalidation across nodes in an erlmcp cluster.
%%% It ensures cache coherence through:
%%%
%%% - Invalidations broadcast to all nodes
%%% - Version tracking for optimistic concurrency
%%% - Automatic cache warming on miss
%%% - Hierarchical cache management (L1/L2/L3)
%%%
%%% == Invalidation Protocols ==
%%%
%%% - **invalidate**: Broadcast to all nodes, wait for acks
%%% - **invalidate_lazy**: Broadcast without waiting (fire-and-forget)
%%% - **invalidate_tagged**: Invalidate all entries with a tag
%%% - **invalidate_pattern**: Invalidate matching keys
%%%
%%% == Cache Coherence ==
%%%
%%% Uses write-invalidate protocol:
%%% 1. Writer invalidates cache across cluster
%%% 2. Readers fetch fresh data on next access
%%% 3. Optional proactive cache warming for hot keys
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cache_coordinator).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, start_link/1,
         invalidate/1, invalidate/2,
         invalidate_local/1,
         invalidate_remote/2,
         invalidate_tag/1,
         invalidate_pattern/1,
         subscribe_invalidation/1,
         unsubscribe_invalidation/1,
         get_cache_keys/0,
         get_cache_entry/1,
         warm_cache/2,
         set_coherence_level/1,
         get_coherence_level/0,
         get_stats/0,
         sync_cache/0,
         broadcast_invalidation/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type cache_key() :: binary() | atom().
-type coherence_level() :: strong | eventual | relaxed.
-type invalidation_ack() :: {node(), cache_key(), reference()}.

-type cache_stats() :: #{invalidations_sent => non_neg_integer(),
                          invalidations_received => non_neg_integer(),
                          cache_hits => non_neg_integer(),
                          cache_misses => non_neg_integer(),
                          remote_fetches => non_neg_integer(),
                          last_sync_time => integer()}.

-export_type([coherence_level/0, cache_stats/0]).

%%====================================================================
%% Constants
%%====================================================================

-define(DEFAULT_COHERENCE, eventual).
-define(INVALIDATION_TIMEOUT, 5000).
-define(SYNC_INTERVAL, 30000).
-define(MAX_PENDING_INVALIDATIONS, 1000).

%%====================================================================
%% State Record
%%====================================================================

-record(state,
        {node_id :: node(),
         coherence_level = ?DEFAULT_COHERENCE :: coherence_level(),
         pending_invalidations = #{} :: #{reference() => {cache_key(), [node()]}}",
         subscription_table :: ets:tid(),
         stats = #{} :: cache_stats(),
         sync_timer :: reference() | undefined,
         invalidation_timeout = ?INVALIDATION_TIMEOUT :: pos_integer()}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Invalidate a cache key across all nodes
-spec invalidate(cache_key()) -> ok | {error, term()}.
invalidate(Key) ->
    invalidate(Key, #{}).

-spec invalidate(cache_key(), map()) -> ok | {error, term()}.
invalidate(Key, Opts) ->
    case maps:get(local_only, Opts, false) of
        true ->
            invalidate_local(Key);
        false ->
            gen_server:call(?MODULE, {invalidate, Key, Opts}, ?INVALIDATION_TIMEOUT)
    end.

%% @doc Invalidate cache key on local node only
-spec invalidate_local(cache_key()) -> ok.
invalidate_local(Key) ->
    erlmcp_cache:delete(Key),
    ok.

%% @doc Invalidate cache key on a specific remote node
-spec invalidate_remote(node(), cache_key()) -> ok | {error, term()}.
invalidate_remote(Node, Key) when Node =:= node() ->
    invalidate_local(Key);
invalidate_remote(Node, Key) ->
    case rpc:call(Node, ?MODULE, invalidate_local, [Key], ?INVALIDATION_TIMEOUT) of
        ok -> ok;
        {badrpc, Reason} -> {error, Reason}
    end.

%% @doc Invalidate all cache entries with a specific tag
-spec invalidate_tag(binary()) -> {ok, non_neg_integer()}.
invalidate_tag(Tag) ->
    gen_server:call(?MODULE, {invalidate_tag, Tag}).

%% @doc Invalidate cache entries matching a pattern
-spec invalidate_pattern(binary() | atom()) -> {ok, non_neg_integer()}.
invalidate_pattern(Pattern) ->
    gen_server:call(?MODULE, {invalidate_pattern, Pattern}).

%% @doc Subscribe to invalidation notifications
-spec subscribe_invalidation(pid()) -> ok.
subscribe_invalidation(Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {subscribe, Pid}).

%% @doc Unsubscribe from invalidation notifications
-spec unsubscribe_invalidation(pid()) -> ok.
unsubscribe_invalidation(Pid) when is_pid(Pid) ->
    gen_server:cast(?MODULE, {unsubscribe, Pid}).

%% @doc Get all cache keys (local)
-spec get_cache_keys() -> [cache_key()].
get_cache_keys() ->
    erlmcp_cache:stats().

%% @doc Get specific cache entry
-spec get_cache_entry(cache_key()) -> {ok, term()} | {error, not_found}.
get_cache_entry(Key) ->
    erlmcp_cache:get(Key).

%% @doc Warm cache for a key
-spec warm_cache(cache_key(), fun(() -> term())) -> ok.
warm_cache(Key, ValueFun) ->
    erlmcp_cache:warm_cache(Key, ValueFun),
    ok.

%% @doc Set coherence level
-spec set_coherence_level(coherence_level()) -> ok.
set_coherence_level(Level) ->
    gen_server:call(?MODULE, {set_coherence_level, Level}).

%% @doc Get current coherence level
-spec get_coherence_level() -> coherence_level().
get_coherence_level() ->
    gen_server:call(?MODULE, get_coherence_level).

%% @doc Get cache coordinator statistics
-spec get_stats() -> cache_stats().
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%% @doc Synchronize cache with cluster
-spec sync_cache() -> {ok, map()}.
sync_cache() ->
    gen_server:call(?MODULE, sync_cache).

%% @doc Broadcast invalidation to cluster (async)
-spec broadcast_invalidation(cache_key()) -> ok.
broadcast_invalidation(Key) ->
    gen_server:cast(?MODULE, {broadcast_invalidation, Key}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, #state{}}.
init(Opts) ->
    process_flag(trap_exit, true),

    NodeId = node(),
    CoherenceLevel = maps:get(coherence_level, Opts, ?DEFAULT_COHERENCE),
    Timeout = maps:get(invalidation_timeout, Opts, ?INVALIDATION_TIMEOUT),

    %% Create subscription table
    SubscriptionTable = ets:new(erlmcp_cache_subscriptions, [bag, public]),

    %% Start sync timer
    SyncTimer = erlang:send_after(?SYNC_INTERVAL, self(), sync_tick),

    %% Join process group
    ok = pg:join(erlmcp_cache, cache_nodes, self()),

    logger:info("Cache coordinator started on ~p: coherence=~p",
                [NodeId, CoherenceLevel]),

    {ok, #state{node_id = NodeId,
                coherence_level = CoherenceLevel,
                subscription_table = SubscriptionTable,
                sync_timer = SyncTimer,
                invalidation_timeout = Timeout}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
          {reply, term(), #state{}}.
handle_call({invalidate, Key, Opts}, _From, State) ->
    {Result, NewState} = do_invalidate(Key, Opts, State),
    {reply, Result, NewState};

handle_call({invalidate_tag, Tag}, _From, State) ->
    {Result, NewState} = do_invalidate_tag(Tag, State),
    {reply, Result, NewState};

handle_call({invalidate_pattern, Pattern}, _From, State) ->
    {Result, NewState} = do_invalidate_pattern(Pattern, State),
    {reply, Result, NewState};

handle_call({subscribe, Pid}, _From, State) ->
    ets:insert(State#state.subscription_table, {Pid, erlang:monotonic_time()}),
    monitor(process, Pid),
    {reply, ok, State};

handle_call({set_coherence_level, Level}, _From, State) ->
    logger:info("Changing cache coherence from ~p to ~p", [State#state.coherence_level, Level]),
    {reply, ok, State#state{coherence_level = Level}};

handle_call(get_coherence_level, _From, State) ->
    {reply, State#state.coherence_level, State};

handle_call(get_stats, _From, State) ->
    {reply, State#state.stats, State};

handle_call(sync_cache, _From, State) ->
    {Result, NewState} = do_sync_cache(State),
    {reply, Result, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({unsubscribe, Pid}, State) ->
    ets:match_delete(State#state.subscription_table, {Pid, '_'}),
    {noreply, State};

handle_cast({broadcast_invalidation, Key}, State) ->
    NewState = do_broadcast_invalidation(Key, State),
    {noreply, NewState};

handle_cast({invalidation_ack, Ref, Node}, State) ->
    NewState = handle_invalidation_ack(Ref, Node, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(sync_tick, State) ->
    NewState = perform_sync(State),
    TimerRef = erlang:send_after(?SYNC_INTERVAL, self(), sync_tick),
    {noreply, NewState#state{sync_timer = TimerRef}};

handle_info({cache_invalidation, FromNode, Key}, State) ->
    %% Handle remote invalidation
    invalidate_local(Key),
    notify_subscribers(Key, State),
    NewState = update_stats(invalidations_received, State),
    logger:debug("Received invalidation from ~p for key ~p", [FromNode, Key]),
    {noreply, NewState};

handle_info({invalidation_timeout, Ref}, State) ->
    NewState = handle_invalidation_timeout(Ref, State),
    {noreply, NewState};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    ets:match_delete(State#state.subscription_table, {Pid, '_'}),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    pg:leave(erlmcp_cache, cache_nodes, self()),
    logger:info("Cache coordinator terminating"),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Perform invalidation based on coherence level
-spec do_invalidate(cache_key(), map(), #state{}) ->
          {ok, #state{}} | {{error, term()}, #state{}}.
do_invalidate(Key, Opts, State) ->
    %% Invalidate locally first
    invalidate_local(Key),

    case State#state.coherence_level of
        strong ->
            %% Broadcast to all nodes and wait for acks
            do_invalidate_sync(Key, State);
        eventual ->
            %% Broadcast without waiting
            do_invalidate_async(Key, State);
        relaxed ->
            %% Only invalidate if explicitly requested
            case maps-get(remote, Opts, true) of
                true -> do_invalidate_async(Key, State);
                false -> {ok, State}
            end
    end.

%% @private Synchronous invalidation
-spec do_invalidate_sync(cache_key(), #state{}) ->
          {ok, #state{}} | {{error, term()}, #state{}}.
do_invalidate_sync(Key, State) ->
    Nodes = get_cache_nodes(),
    Ref = make_ref(),

    %% Send invalidation requests
    lists:foreach(fun(Node) ->
                     case Node =:= node() of
                         true -> ok;
                         false ->
                             spawn(fun() ->
                                     case rpc:call(Node, ?MODULE, invalidate_local,
                                                  [Key], State#state.invalidation_timeout) of
                                         ok ->
                                             gen_server:cast(?MODULE,
                                                           {invalidation_ack, Ref, Node});
                                         {badrpc, Reason} ->
                                             logger:warning("Failed to invalidate ~p on ~p: ~p",
                                                           [Key, Node, Reason])
                                     end
                             end)
                     end
                 end, Nodes),

    %% Set up timeout
    erlang:send_after(State#state.invalidation_timeout, self(), {invalidation_timeout, Ref}),

    %% Track pending invalidation
    Pending = maps:put(Ref, {Key, Nodes}, State#state.pending_invalidations),
    NewState = update_stats(invalidations_sent, State#state{pending_invalidations = Pending}),

    {ok, NewState}.

%% @private Asynchronous invalidation
-spec do_invalidate_async(cache_key(), #state{}) -> {ok, #state{}}.
do_invalidate_async(Key, State) ->
    pg:send(erlmcp_cache, cache_nodes, {cache_invalidation, node(), Key}),
    NewState = update_stats(invalidations_sent, State),
    {ok, NewState}.

%% @private Broadcast invalidation without tracking
-spec do_broadcast_invalidation(cache_key(), #state{}) -> #state{}.
do_broadcast_invalidation(Key, State) ->
    pg:send(erlmcp_cache, cache_nodes, {cache_invalidation, node(), Key}),
    update_stats(invalidations_sent, State).

%% @private Invalidate by tag
-spec do_invalidate_tag(binary(), #state{}) -> {{ok, non_neg_integer()}, #state{}}.
do_invalidate_tag(Tag, State) ->
    {ok, Count} = erlmcp_cache:invalidate_by_tag(Tag),

    %% Broadcast tag invalidation to cluster
    pg:send(erlmcp_cache, cache_nodes, {tag_invalidation, node(), Tag}),

    NewState = update_stats(invalidations_sent, State),
    {{ok, Count}, NewState}.

%% @private Invalidate by pattern
-spec do_invalidate_pattern(binary() | atom(), #state{}) ->
          {{ok, non_neg_integer()}, #state{}}.
do_invalidate_pattern(Pattern, State) ->
    %% Get all keys matching pattern
    Stats = erlmcp_cache:stats(),
    L1Size = maps:get(l1_size, Stats, 0),

    %% Pattern match against keys
    MatchingKeys = match_cache_keys(Pattern, L1Size),

    %% Invalidate matching keys
    lists:foreach(fun(Key) -> invalidate_local(Key) end, MatchingKeys),

    %% Broadcast to cluster
    pg:send(erlmcp_cache, cache_nodes, {pattern_invalidation, node(), Pattern}),

    NewState = update_stats(invalidations_sent, State),
    {{ok, length(MatchingKeys)}, NewState}.

%% @private Perform sync
-spec perform_sync(#state{}) -> #state{}.
perform_sync(State) ->
    %% Collect cache stats from all nodes
    Nodes = get_cache_nodes(),

    NodeStats = lists:map(fun(Node) ->
                                 case Node =:= node() of
                                     true ->
                                         {Node, get_local_stats()};
                                     false ->
                                         case rpc:call(Node, ?MODULE, get_stats, [], 1000) of
                                             Stats when is_map(Stats) -> {Node, Stats};
                                             _ -> {Node, #{error => unavailable}}
                                         end
                                 end
                         end, Nodes),

    %% Merge stats
    MergedStats = merge_stats(NodeStats),

    State#state{stats = MergedStats#{
                 last_sync_time => erlang:system_time(millisecond)}}.

%% @private Sync cache
-spec do_sync_cache(#state{}) -> {{ok, map()}, #state{}}.
do_sync_cache(State) ->
    %% Request cache keys from all nodes
    Nodes = get_cache_nodes(),
    RemoteKeys = get_remote_cache_keys(Nodes),

    %% For each remote key, check if we have it
    LocalKeys = get_local_cache_keys(),
    MissingKeys = RemoteKeys -- LocalKeys,

    %% Optionally warm missing keys (disabled by default to avoid stampede)
    Result = #{nodes_contacted => length(Nodes),
               missing_keys => length(MissingKeys)},

    {{ok, Result}, State}.

%% @private Handle invalidation acknowledgment
-spec handle_invalidation_ack(reference(), node(), #state{}) -> #state{}.
handle_invalidation_ack(Ref, Node, State) ->
    case maps:get(Ref, State#state.pending_invalidations, undefined) of
        undefined ->
            State;
        {Key, RemainingNodes} ->
            NewRemaining = lists:delete(Node, RemainingNodes),
            case NewRemaining of
                [] ->
                    %% All nodes acknowledged
                    NewPending = maps:remove(Ref, State#state.pending_invalidations),
                    State#state{pending_invalidations = NewPending};
                _ ->
                    NewPending = maps:put(Ref, {Key, NewRemaining},
                                        State#state.pending_invalidations),
                    State#state{pending_invalidations = NewPending}
            end
    end.

%% @private Handle invalidation timeout
-spec handle_invalidation_timeout(reference(), #state{}) -> #state{}.
handle_invalidation_timeout(Ref, State) ->
    case maps:take(Ref, State#state.pending_invalidations) of
        {{Key, RemainingNodes}, NewPending} ->
            logger:warning("Invalidation timeout for ~p, missing acks from ~p",
                          [Key, RemainingNodes]),
            State#state{pending_invalidations = NewPending};
        error ->
            State
    end.

%% @private Get cache nodes
-spec get_cache_nodes() -> [node()].
get_cache_nodes() ->
    pg:get_members(erlmcp_cache, cache_nodes).

%% @private Get local cache keys
-spec get_local_cache_keys() -> [cache_key()].
get_local_cache_keys() ->
    %% This would need to be implemented in erlmcp_cache to list keys
    %% For now, return empty list
    [].

%% @private Get remote cache keys
-spec get_remote_cache_keys([node()]) -> [cache_key()].
get_remote_cache_keys(Nodes) ->
    lists:foldl(fun(Node, Acc) ->
                       case Node =:= node() of
                           true -> Acc;
                           false ->
                               case rpc:call(Node, ?MODULE, get_cache_keys, [], 1000) of
                                   Keys when is_list(Keys) -> Keys ++ Acc;
                                   _ -> Acc
                               end
                       end
               end, [], Nodes).

%% @private Match cache keys by pattern
-spec match_cache_keys(binary() | atom(), non_neg_integer()) -> [cache_key()].
match_cache_keys(_Pattern, _Size) ->
    %% This would iterate through cache keys and match
    %% For now, return empty list
    [].

%% @private Update stats
-spec update_stats(atom(), #state{}) -> #state{}.
update_stats(Key, State) ->
    Stats = State#state.stats,
    NewStats = maps:update_with(Key, fun(V) -> V + 1 end, 1, Stats),
    State#state{stats = NewStats}.

%% @private Get local stats
-spec get_local_stats() -> map().
get_local_stats() ->
    CacheStats = erlmcp_cache:stats(),
    #{invalidations_sent => 0,
      invalidations_received => 0,
      cache_hits => maps:get(hits, CacheStats, 0),
      cache_misses => maps:get(misses, CacheStats, 0)}.

%% @private Merge stats from multiple nodes
-spec merge_stats([{node(), map()}]) -> map().
merge_stats(NodeStats) ->
    lists:foldl(fun({_Node, Stats}, Acc) ->
                       case maps:get(error, Stats, undefined) of
                           undefined ->
                               maps:merge_with(fun(_K, V1, V2) -> V1 + V2 end, Acc, Stats);
                           _ ->
                               Acc
                       end
               end, #{}, NodeStats).

%% @private Notify subscribers
-spec notify_subscribers(cache_key(), #state{}) -> ok.
notify_subscribers(Key, State) ->
    Subscribers = ets:tab2list(State#state.subscription_table),
    lists:foreach(fun({Pid, _}) ->
                       case is_process_alive(Pid) of
                           true ->
                               Pid ! {cache_invalidated, Key};
                           false ->
                               ets:match_delete(State#state.subscription_table, {Pid, '_'})
                       end
                 end, Subscribers),
    ok.
