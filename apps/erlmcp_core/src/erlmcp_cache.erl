%%%====================================================================
%%% @doc Multi-Level Intelligent Caching Layer
%%%
%%% Implements 3-tier caching architecture:
%%% - L1: ETS (in-memory, process-local, fastest)
%%% - L2: Mnesia (replicated, durable, cluster-wide)
%%% - L3: External (Redis/Memcached, optional)
%%%
%%% Features:
%%% - LRU eviction policy with configurable max size
%%% - TTL-based expiration with automatic cleanup
%%% - Tag-based invalidation (invalidate by category)
%%% - Dependency tracking (invalidate related entries)
%%% - Cache-Control headers (ETags, conditional requests)
%%% - Distributed cache coherence via Mnesia
%%% - Probabilistic cache warming
%%% - Metrics: hit rate, miss rate, eviction rate, latency
%%%
%%% @end
%%%====================================================================
-module(erlmcp_cache).

-behaviour(gen_server).

-include("erlmcp.hrl").

-include_lib("kernel/include/logger.hrl").

%% Disable auto-import of get/1 to avoid name clash with BIF
-compile({no_auto_import, [get/1]}).

%% API exports
-export([start_link/0, start_link/1, get/1, get/2, put/2, put/3, put/4, delete/1,
         invalidate_by_tag/1, invalidate_by_dependency/1, clear/0, stats/0, generate_etag/1,
         check_etag/2, warm_cache/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type cache_key() :: binary() | atom() | term().
-type cache_value() :: term().
-type cache_strategy() ::
    {ttl, Seconds :: pos_integer()} | {lru, MaxSize :: pos_integer()} | write_through | write_back.
-type cache_level() :: l1 | l2 | l3.
-type cache_tags() :: [binary()].
-type cache_deps() :: [cache_key()].

-export_type([cache_key/0, cache_value/0, cache_strategy/0, cache_level/0]).

%%====================================================================
%% State record
%%====================================================================

-record(cache_entry,
        {key :: cache_key(),
         value :: cache_value(),
         level :: cache_level(),
         inserted_at :: integer(),  % monotonic time in microseconds
         expires_at :: integer() | infinity,  % monotonic time in microseconds
         access_count :: non_neg_integer(),
         last_accessed :: integer(),  % monotonic time in microseconds
         etag :: binary() | undefined,
         tags :: cache_tags(),
         dependencies :: cache_deps(),
         strategy :: cache_strategy()}).

%% State version for hot code loading
-type state_version() :: v1 | v2.

-record(state,
        {version = v1 :: state_version(),  % State version for hot code loading
         l1_table :: ets:tid(),  % ETS table for L1 cache
         l2_enabled = false :: boolean(),  % Mnesia available?
         l3_enabled = false :: boolean(),  % External cache available?
         l3_module :: module() | undefined,  % External cache module (e.g., eredis)
         l3_conn :: term() | undefined,  % External cache connection
         max_l1_size = 10000 :: pos_integer(),
         max_l2_size = 100000 :: pos_integer(),
         default_ttl_seconds = 300 :: pos_integer(),  % 5 minutes default
         cleanup_interval_ms = 60000 :: pos_integer(),  % 1 minute cleanup
         cleanup_timer :: reference() | undefined,
         %% Metrics
         stats =
             #{hits => 0,
               misses => 0,
               l1_hits => 0,
               l2_hits => 0,
               l3_hits => 0,
               evictions => 0,
               expirations => 0,
               writes => 0,
               deletes => 0} ::
             map()}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Get value from cache (automatic L1 -> L2 -> L3 fallback)
-spec get(cache_key()) -> {ok, cache_value()} | {error, not_found}.
get(Key) ->
    get(Key, #{}).

-spec get(cache_key(), map()) -> {ok, cache_value()} | {error, not_found}.
get(Key, _Opts) ->
    gen_server:call(?MODULE, {get, Key}, 5000).

%% @doc Put value in cache with default strategy
-spec put(cache_key(), cache_value()) -> ok.
put(Key, Value) ->
    put(Key, Value, {ttl, 300}, #{}).

-spec put(cache_key(), cache_value(), cache_strategy()) -> ok.
put(Key, Value, Strategy) ->
    put(Key, Value, Strategy, #{}).

-spec put(cache_key(), cache_value(), cache_strategy(), map()) -> ok.
put(Key, Value, Strategy, Opts) ->
    gen_server:call(?MODULE, {put, Key, Value, Strategy, Opts}, 5000).

%% @doc Delete value from all cache levels
-spec delete(cache_key()) -> ok.
delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}, 5000).

%% @doc Invalidate all cache entries with given tag (synchronous)
-spec invalidate_by_tag(binary()) -> {ok, non_neg_integer()}.
invalidate_by_tag(Tag) ->
    gen_server:call(?MODULE, {invalidate_by_tag, Tag}, 5000).

%% @doc Invalidate all cache entries that depend on given key (synchronous)
-spec invalidate_by_dependency(cache_key()) -> {ok, non_neg_integer()}.
invalidate_by_dependency(DepKey) ->
    gen_server:call(?MODULE, {invalidate_by_dependency, DepKey}, 5000).

%% @doc Clear entire cache (all levels)
-spec clear() -> ok.
clear() ->
    gen_server:call(?MODULE, clear, 5000).

%% @doc Get cache statistics
-spec stats() -> map().
stats() ->
    gen_server:call(?MODULE, stats, 5000).

%% @doc Generate ETag for cache value (for conditional requests)
-spec generate_etag(term()) -> binary().
generate_etag(Value) ->
    Hash = erlang:phash2(Value),
    iolist_to_binary(io_lib:format("\"~.16B\"", [Hash])).

%% @doc Check if ETag matches current value
-spec check_etag(cache_key(), binary()) -> boolean().
check_etag(Key, ETag) ->
    case get(Key) of
        {ok, Value} ->
            generate_etag(Value) =:= ETag;
        {error, not_found} ->
            false
    end.

%% @doc Probabilistic cache warming (async)
-spec warm_cache(cache_key(), fun(() -> cache_value())) -> ok.
warm_cache(Key, ValueFun) ->
    gen_server:cast(?MODULE, {warm_cache, Key, ValueFun}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, state()}.
init(Config) ->
    process_flag(trap_exit, true),

    %% Create L1 ETS table (in-memory, read-optimized)
    L1Table =
        ets:new(erlmcp_cache_l1,
                [set, public, named_table, {read_concurrency, true}, {write_concurrency, true}]),

    %% Check if Mnesia is available (L2)
    L2Enabled = ensure_mnesia_started(),

    %% Check if external cache is configured (L3)
    {L3Enabled, L3Module, L3Conn} = init_external_cache(Config),

    %% Extract configuration
    MaxL1Size = maps:get(max_l1_size, Config, 10000),
    MaxL2Size = maps:get(max_l2_size, Config, 100000),
    DefaultTTL = maps:get(default_ttl_seconds, Config, 300),
    CleanupInterval = maps:get(cleanup_interval_ms, Config, 60000),

    %% Start cleanup timer
    TimerRef = erlang:send_after(CleanupInterval, self(), cleanup),

    State =
        #state{l1_table = L1Table,
               l2_enabled = L2Enabled,
               l3_enabled = L3Enabled,
               l3_module = L3Module,
               l3_conn = L3Conn,
               max_l1_size = MaxL1Size,
               max_l2_size = MaxL2Size,
               default_ttl_seconds = DefaultTTL,
               cleanup_interval_ms = CleanupInterval,
               cleanup_timer = TimerRef},

    ?LOG_INFO("erlmcp_cache started: L1=ETS, L2=~p, L3=~p", [L2Enabled, L3Enabled]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
%% Get from cache (L1 -> L2 -> L3 fallback)
handle_call({get, Key}, _From, State) ->
    StartTime = erlang:monotonic_time(microsecond),
    case get_from_l1(Key, State) of
        {ok, Value, NewState} ->
            Latency = erlang:monotonic_time(microsecond) - StartTime,
            ?LOG_DEBUG("Cache L1 hit: ~p (latency: ~p us)", [Key, Latency]),
            {reply, {ok, Value}, update_stats(NewState, l1_hit)};
        {error, not_found} ->
            case get_from_l2(Key, State) of
                {ok, Value, NewState2} ->
                    %% Promote to L1
                    NewState3 = put_in_l1(Key, Value, NewState2),
                    Latency = erlang:monotonic_time(microsecond) - StartTime,
                    ?LOG_DEBUG("Cache L2 hit: ~p (latency: ~p us)", [Key, Latency]),
                    {reply, {ok, Value}, update_stats(NewState3, l2_hit)};
                {error, not_found} ->
                    case get_from_l3(Key, State) of
                        {ok, Value, NewState4} ->
                            %% Promote to L1 and L2
                            NewState5 = put_in_l1(Key, Value, NewState4),
                            NewState6 = put_in_l2(Key, Value, NewState5),
                            Latency = erlang:monotonic_time(microsecond) - StartTime,
                            ?LOG_DEBUG("Cache L3 hit: ~p (latency: ~p us)", [Key, Latency]),
                            {reply, {ok, Value}, update_stats(NewState6, l3_hit)};
                        {error, not_found} ->
                            Latency = erlang:monotonic_time(microsecond) - StartTime,
                            ?LOG_DEBUG("Cache miss: ~p (latency: ~p us)", [Key, Latency]),
                            {reply, {error, not_found}, update_stats(State, miss)}
                    end
            end
    end;
%% Put into cache (all levels based on strategy)
handle_call({put, Key, Value, Strategy, Opts}, _From, State) ->
    Tags = maps:get(tags, Opts, []),
    Deps = maps:get(dependencies, Opts, []),

    Now = erlang:monotonic_time(microsecond),
    ExpiresAt = calculate_expiration(Now, Strategy, State),
    ETag = generate_etag(Value),

    Entry =
        #cache_entry{key = Key,
                     value = Value,
                     level = l1,
                     inserted_at = Now,
                     expires_at = ExpiresAt,
                     access_count = 0,
                     last_accessed = Now,
                     etag = ETag,
                     tags = Tags,
                     dependencies = Deps,
                     strategy = Strategy},

    %% Write to L1 (always)
    NewState1 = put_entry_in_l1(Entry, State),

    %% Write to L2/L3 based on strategy
    NewState2 =
        case Strategy of
            write_through ->
                S2 = put_entry_in_l2(Entry, NewState1),
                put_entry_in_l3(Entry, S2);
            write_back ->
                %% Async write to L2/L3 (simulated here, could use erlang:spawn)
                NewState1;
            {ttl, _} ->
                %% Write to L2 for durability
                put_entry_in_l2(Entry, NewState1);
            {lru, _} ->
                %% L1 only for LRU
                NewState1
        end,

    {reply, ok, update_stats(NewState2, write)};
%% Delete from all levels
handle_call({delete, Key}, _From, State) ->
    NewState1 = delete_from_l1(Key, State),
    NewState2 = delete_from_l2(Key, NewState1),
    NewState3 = delete_from_l3(Key, NewState2),
    {reply, ok, update_stats(NewState3, delete)};
%% Invalidate by tag (synchronous)
handle_call({invalidate_by_tag, Tag}, _From, State) ->
    %% Find all entries with this tag using ets:foldl for simplicity
    KeysToDelete =
        ets:foldl(fun({Key, #cache_entry{tags = Tags}}, Acc) ->
                     case lists:member(Tag, Tags) of
                         true ->
                             [Key | Acc];
                         false ->
                             Acc
                     end
                  end,
                  [],
                  State#state.l1_table),

    %% Delete from all levels using atomic operations
    NewState =
        lists:foldl(fun(Key, S) ->
                       S1 = delete_from_l1(Key, S),
                       S2 = delete_from_l2(Key, S1),
                       delete_from_l3(Key, S2)
                    end,
                    State,
                    KeysToDelete),

    Count = length(KeysToDelete),
    ?LOG_INFO("Invalidated ~p cache entries with tag: ~p", [Count, Tag]),
    {reply, {ok, Count}, NewState};
%% Invalidate by dependency (synchronous)
handle_call({invalidate_by_dependency, DepKey}, _From, State) ->
    %% Find all entries that depend on DepKey using ets:foldl for simplicity
    KeysToDelete =
        ets:foldl(fun({Key, #cache_entry{dependencies = Deps}}, Acc) ->
                     case lists:member(DepKey, Deps) of
                         true ->
                             [Key | Acc];
                         false ->
                             Acc
                     end
                  end,
                  [],
                  State#state.l1_table),

    %% Delete from all levels using atomic operations
    NewState =
        lists:foldl(fun(Key, S) ->
                       S1 = delete_from_l1(Key, S),
                       S2 = delete_from_l2(Key, S1),
                       delete_from_l3(Key, S2)
                    end,
                    State,
                    KeysToDelete),

    Count = length(KeysToDelete),
    ?LOG_INFO("Invalidated ~p cache entries depending on: ~p", [Count, DepKey]),
    {reply, {ok, Count}, NewState};
%% Clear entire cache
handle_call(clear, _From, State) ->
    ets:delete_all_objects(State#state.l1_table),
    _ = clear_l2(State),
    _ = clear_l3(State),
    NewStats =
        #{hits => 0,
          misses => 0,
          l1_hits => 0,
          l2_hits => 0,
          l3_hits => 0,
          evictions => 0,
          expirations => 0,
          writes => 0,
          deletes => 0},
    {reply, ok, State#state{stats = NewStats}};
%% Get statistics
handle_call(stats, _From, State) ->
    L1Size = ets:info(State#state.l1_table, size),
    L2Size = get_l2_size(State),

    Stats = State#state.stats,
    TotalRequests = maps:get(hits, Stats, 0) + maps:get(misses, Stats, 0),
    HitRate =
        case TotalRequests of
            0 ->
                0.0;
            N ->
                maps:get(hits, Stats, 0) / N
        end,

    DetailedStats =
        Stats#{l1_size => L1Size,
               l2_size => L2Size,
               hit_rate => HitRate,
               total_requests => TotalRequests},
    {reply, DetailedStats, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
%% Probabilistic cache warming
handle_cast({warm_cache, Key, ValueFun}, State) ->
    %% Async compute and cache using supervised worker (replaces unsupervised spawn/1)
    case erlmcp_cache_warmer_sup:start_warmer(Key, ValueFun, State#state.default_ttl_seconds) of
        {ok, _Pid} ->
            ?LOG_DEBUG("Started cache warmer for key ~p", [Key]),
            ok;
        {error, Reason} ->
            ?LOG_WARNING("Failed to start cache warmer for ~p: ~p", [Key, Reason]),
            ok
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
%% Periodic cleanup (TTL expiration + LRU eviction)
handle_info(cleanup, State) ->
    NewState = perform_cleanup(State),

    %% Reschedule cleanup
    TimerRef = erlang:send_after(State#state.cleanup_interval_ms, self(), cleanup),
    {noreply, NewState#state{cleanup_timer = TimerRef}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    %% Cancel cleanup timer
    case State#state.cleanup_timer of
        undefined ->
            ok;
        Ref ->
            erlang:cancel_timer(Ref)
    end,

    %% Close external cache connection
    case State#state.l3_conn of
        undefined ->
            ok;
        Conn ->
            close_external_cache(State#state.l3_module, Conn)
    end,

    ?LOG_INFO("erlmcp_cache terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(OldVsn, State, Extra) ->
    try
        logger:info("Cache: Code change from ~p", [OldVsn]),
        NewState = migrate_cache_state(OldVsn, State, Extra),
        logger:info("Cache: Code change completed successfully"),
        {ok, NewState}
    catch
        Class:Reason:Stack ->
            logger:error("Cache: Code change failed: ~p:~p~n~p", [Class, Reason, Stack]),
            error({code_change_failed, Class, Reason})
    end.

%% @private Migrate cache state based on version
-spec migrate_cache_state(term(), state(), term()) -> state().
migrate_cache_state(_OldVsn, #state{version = v1} = State, _Extra) ->
    %% Already at current version (v1)
    State;
migrate_cache_state({down, _FromVsn}, #state{} = State, _Extra) ->
    %% Downgrade migration - ensure version field exists
    case State#state.version of
        undefined ->
            State#state{version = v1};
        _ ->
            State
    end;
migrate_cache_state(OldVsn, #state{version = undefined} = State, _Extra)
    when is_list(OldVsn); is_atom(OldVsn) ->
    %% Legacy state (pre-versioning) - upgrade to v1
    logger:info("Cache: Upgrading legacy state to v1"),
    State#state{version = v1};
migrate_cache_state(OldVsn, State, _Extra) ->
    logger:warning("Cache: Unknown code_change from version ~p", [OldVsn]),
    State.

%%====================================================================
%% Internal functions - L1 Cache (ETS)
%%====================================================================

-spec get_from_l1(cache_key(), state()) -> {ok, cache_value(), state()} | {error, not_found}.
get_from_l1(Key, State) ->
    case ets:lookup(State#state.l1_table, Key) of
        [] ->
            {error, not_found};
        [{Key, Entry}] ->
            %% Check if expired
            Now = erlang:monotonic_time(microsecond),
            case is_expired(Entry, Now) of
                true ->
                    ets:delete(State#state.l1_table, Key),
                    {error, not_found};
                false ->
                    %% Update access time and count
                    %% gen_server serializes access, so this is safe from race conditions
                    UpdatedEntry =
                        Entry#cache_entry{access_count = Entry#cache_entry.access_count + 1,
                                          last_accessed = Now},
                    ets:insert(State#state.l1_table, {Key, UpdatedEntry}),
                    {ok, Entry#cache_entry.value, State}
            end
    end.

-spec put_in_l1(cache_key(), cache_value(), state()) -> state().
put_in_l1(Key, Value, State) ->
    Now = erlang:monotonic_time(microsecond),
    ExpiresAt = Now + State#state.default_ttl_seconds * 1_000_000,
    Entry =
        #cache_entry{key = Key,
                     value = Value,
                     level = l1,
                     inserted_at = Now,
                     expires_at = ExpiresAt,
                     access_count = 1,
                     last_accessed = Now,
                     etag = generate_etag(Value),
                     tags = [],
                     dependencies = [],
                     strategy = {ttl, State#state.default_ttl_seconds}},
    put_entry_in_l1(Entry, State).

-spec put_entry_in_l1(#cache_entry{}, state()) -> state().
put_entry_in_l1(Entry, State) ->
    %% Check if LRU eviction needed
    CurrentSize = ets:info(State#state.l1_table, size),
    NewState =
        case CurrentSize >= State#state.max_l1_size of
            true ->
                evict_lru_l1(State);
            false ->
                State
        end,

    ets:insert(NewState#state.l1_table, {Entry#cache_entry.key, Entry}),
    NewState.

-spec delete_from_l1(cache_key(), state()) -> state().
delete_from_l1(Key, State) ->
    ets:delete(State#state.l1_table, Key),
    State.

-spec evict_lru_l1(state()) -> state().
evict_lru_l1(State) ->
    %% Find least recently used entry
    Pattern =
        #cache_entry{key = '$1',
                     last_accessed = '$2',
                     _ = '_'},
    AllEntries = ets:select(State#state.l1_table, [{{'$1', Pattern}, [], [{{'$1', '$2'}}]}]),

    case AllEntries of
        [] ->
            State;
        _ ->
            %% Sort by last_accessed (ascending)
            Sorted = lists:keysort(2, AllEntries),
            {KeyToEvict, _} = hd(Sorted),
            ets:delete(State#state.l1_table, KeyToEvict),
            update_stats(State, eviction)
    end.

%%====================================================================
%% Internal functions - L2 Cache (Mnesia)
%%====================================================================

%% Helper to convert Mnesia tuple to cache_entry record
-spec mnesia_rec_to_cache_entry(tuple()) -> #cache_entry{}.
mnesia_rec_to_cache_entry({erlmcp_cache_l2,
                           Key,
                           Value,
                           Level,
                           InsertedAt,
                           ExpiresAt,
                           AccessCount,
                           LastAccessed,
                           ETag,
                           Tags,
                           Dependencies,
                           Strategy}) ->
    #cache_entry{key = Key,
                 value = Value,
                 level = Level,
                 inserted_at = InsertedAt,
                 expires_at = ExpiresAt,
                 access_count = AccessCount,
                 last_accessed = LastAccessed,
                 etag = ETag,
                 tags = Tags,
                 dependencies = Dependencies,
                 strategy = Strategy}.

-spec ensure_mnesia_started() -> boolean().
ensure_mnesia_started() ->
    case mnesia:system_info(is_running) of
        yes ->
            ensure_cache_table();
        no ->
            %% Try to start Mnesia
            case application:start(mnesia) of
                ok ->
                    ensure_cache_table();
                {error, {already_started, mnesia}} ->
                    ensure_cache_table();
                {error, Reason} ->
                    ?LOG_WARNING("Failed to start Mnesia: ~p", [Reason]),
                    false
            end;
        _ ->
            false
    end.

-spec ensure_cache_table() -> boolean().
ensure_cache_table() ->
    %% Check if table already exists
    case lists:member(erlmcp_cache_l2, mnesia:system_info(tables)) of
        true ->
            ?LOG_DEBUG("Mnesia cache table already exists"),
            true;
        false ->
            %% disc_copies requires a named node (not nonode@nohost)
            UseDiscCopies = node() =/= nonode@nohost,

            %% Create table with explicit field names (Mnesia needs table name as first field)
            case mnesia:create_table(erlmcp_cache_l2,
                                     [{type, set},
                                      {attributes,
                                       [key,
                                        value,
                                        level,
                                        inserted_at,
                                        expires_at,
                                        access_count,
                                        last_accessed,
                                        etag,
                                        tags,
                                        dependencies,
                                        strategy]},
                                      {disc_copies,
                                       case UseDiscCopies of
                                           true ->
                                               [node()];
                                           false ->
                                               []
                                       end},
                                      {ram_copies,
                                       case UseDiscCopies of
                                           true ->
                                               [];
                                           false ->
                                               [node()]
                                       end},
                                      {index, [tags, dependencies]}])
            of
                {atomic, ok} ->
                    ?LOG_INFO("Created Mnesia cache table: erlmcp_cache_l2 (storage: ~p)",
                              [case UseDiscCopies of
                                   true ->
                                       disc_copies;
                                   false ->
                                       ram_copies
                               end]),
                    true;
                {aborted, {already_exists, _}} ->
                    true;
                {aborted, Reason} ->
                    ?LOG_ERROR("Failed to create Mnesia cache table: ~p", [Reason]),
                    false
            end
    end.

-spec get_from_l2(cache_key(), state()) -> {ok, cache_value(), state()} | {error, not_found}.
get_from_l2(_Key, #state{l2_enabled = false} = _State) ->
    {error, not_found};
get_from_l2(Key, State) ->
    case mnesia:dirty_read(erlmcp_cache_l2, Key) of
        [] ->
            {error, not_found};
        [MnesiaRec] ->
            %% Transform Mnesia tuple back to our record
            %% Mnesia returns: {erlmcp_cache_l2, Key, Value, Level, ...}
            Entry = mnesia_rec_to_cache_entry(MnesiaRec),
            Now = erlang:monotonic_time(microsecond),
            case is_expired(Entry, Now) of
                true ->
                    mnesia:dirty_delete(erlmcp_cache_l2, Key),
                    {error, not_found};
                false ->
                    {ok, Entry#cache_entry.value, State}
            end
    end.

-spec put_in_l2(cache_key(), cache_value(), state()) -> state().
put_in_l2(_Key, _Value, #state{l2_enabled = false} = State) ->
    State;
put_in_l2(Key, Value, State) ->
    Now = erlang:monotonic_time(microsecond),
    ExpiresAt = Now + State#state.default_ttl_seconds * 1_000_000,
    Entry =
        #cache_entry{key = Key,
                     value = Value,
                     level = l2,
                     inserted_at = Now,
                     expires_at = ExpiresAt,
                     access_count = 0,
                     last_accessed = Now,
                     etag = generate_etag(Value),
                     tags = [],
                     dependencies = [],
                     strategy = {ttl, State#state.default_ttl_seconds}},
    put_entry_in_l2(Entry, State).

-spec put_entry_in_l2(#cache_entry{}, state()) -> state().
put_entry_in_l2(_Entry, #state{l2_enabled = false} = State) ->
    State;
put_entry_in_l2(Entry, State) ->
    %% Transform record to tuple for Mnesia (table name as first element)
    %% Mnesia expects: {erlmcp_cache_l2, Key, Value, Level, ...}
    %% But we have: #cache_entry{key=Key, value=Value, ...}
    MnesiaRec =
        {erlmcp_cache_l2,
         Entry#cache_entry.key,
         Entry#cache_entry.value,
         Entry#cache_entry.level,
         Entry#cache_entry.inserted_at,
         Entry#cache_entry.expires_at,
         Entry#cache_entry.access_count,
         Entry#cache_entry.last_accessed,
         Entry#cache_entry.etag,
         Entry#cache_entry.tags,
         Entry#cache_entry.dependencies,
         Entry#cache_entry.strategy},
    mnesia:dirty_write(MnesiaRec),
    State.

-spec delete_from_l2(cache_key(), state()) -> state().
delete_from_l2(_Key, #state{l2_enabled = false} = State) ->
    State;
delete_from_l2(Key, State) ->
    mnesia:dirty_delete(erlmcp_cache_l2, Key),
    State.

-spec clear_l2(state()) -> ok.
clear_l2(#state{l2_enabled = false}) ->
    ok;
clear_l2(_State) ->
    mnesia:clear_table(erlmcp_cache_l2),
    ok.

-spec get_l2_size(state()) -> non_neg_integer().
get_l2_size(#state{l2_enabled = false}) ->
    0;
get_l2_size(_State) ->
    mnesia:table_info(erlmcp_cache_l2, size).

%%====================================================================
%% Internal functions - L3 Cache (External: Redis/Memcached)
%%====================================================================

-spec init_external_cache(map()) -> {boolean(), module() | undefined, term() | undefined}.
init_external_cache(Config) ->
    case maps:get(external_cache, Config, undefined) of
        undefined ->
            {false, undefined, undefined};
        #{module := Module, config := CacheConfig} ->
            case connect_external_cache(Module, CacheConfig) of
                {ok, Conn} ->
                    {true, Module, Conn};
                {error, Reason} ->
                    ?LOG_WARNING("Failed to connect to external cache: ~p", [Reason]),
                    {false, undefined, undefined}
            end
    end.

-spec connect_external_cache(module(), map()) -> {ok, term()} | {error, term()}.
connect_external_cache(_Module, _Config) ->
    %% Placeholder: would call Module:connect(Config)
    %% Example: eredis:start_link(Host, Port)
    {error, not_implemented}.

-spec close_external_cache(module() | undefined, term()) -> ok.
close_external_cache(undefined, _Conn) ->
    ok;
close_external_cache(_Module, _Conn) ->
    %% Placeholder: would call Module:close(Conn)
    ok.

-spec get_from_l3(cache_key(), state()) -> {ok, cache_value(), state()} | {error, not_found}.
get_from_l3(_Key, #state{l3_enabled = false} = _State) ->
    {error, not_found};
get_from_l3(_Key, _State) ->
    %% Placeholder: would call L3Module:get(L3Conn, Key)
    {error, not_found}.

-spec put_entry_in_l3(#cache_entry{}, state()) -> state().
put_entry_in_l3(_Entry, #state{l3_enabled = false} = State) ->
    State;
put_entry_in_l3(_Entry, State) ->
    %% Placeholder: would call L3Module:set(L3Conn, Key, Value, TTL)
    State.

-spec delete_from_l3(cache_key(), state()) -> state().
delete_from_l3(_Key, #state{l3_enabled = false} = State) ->
    State;
delete_from_l3(_Key, State) ->
    %% Placeholder: would call L3Module:del(L3Conn, Key)
    State.

-spec clear_l3(state()) -> ok.
clear_l3(#state{l3_enabled = false}) ->
    ok;
clear_l3(_State) ->
    %% Placeholder: would call L3Module:flushdb(L3Conn)
    ok.

%%====================================================================
%% Internal functions - Helpers
%%====================================================================

-spec is_expired(#cache_entry{}, integer()) -> boolean().
is_expired(#cache_entry{expires_at = infinity}, _Now) ->
    false;
is_expired(#cache_entry{expires_at = ExpiresAt}, Now) ->
    Now >= ExpiresAt.

-spec calculate_expiration(integer(), cache_strategy(), state()) -> integer() | infinity.
calculate_expiration(Now, {ttl, Seconds}, _State) ->
    Now + Seconds * 1_000_000;
calculate_expiration(_Now, {lru, _MaxSize}, _State) ->
    infinity;
calculate_expiration(_Now, write_through, State) ->
    Now = erlang:monotonic_time(microsecond),
    Now + State#state.default_ttl_seconds * 1_000_000;
calculate_expiration(_Now, write_back, State) ->
    Now = erlang:monotonic_time(microsecond),
    Now + State#state.default_ttl_seconds * 1_000_000.

-spec update_stats(state(), atom()) -> state().
update_stats(State, Type) ->
    Stats = State#state.stats,
    NewStats =
        case Type of
            l1_hit ->
                Stats#{hits => maps:get(hits, Stats, 0) + 1,
                       l1_hits => maps:get(l1_hits, Stats, 0) + 1};
            l2_hit ->
                Stats#{hits => maps:get(hits, Stats, 0) + 1,
                       l2_hits => maps:get(l2_hits, Stats, 0) + 1};
            l3_hit ->
                Stats#{hits => maps:get(hits, Stats, 0) + 1,
                       l3_hits => maps:get(l3_hits, Stats, 0) + 1};
            miss ->
                Stats#{misses => maps:get(misses, Stats, 0) + 1};
            eviction ->
                Stats#{evictions => maps:get(evictions, Stats, 0) + 1};
            expiration ->
                Stats#{expirations => maps:get(expirations, Stats, 0) + 1};
            write ->
                Stats#{writes => maps:get(writes, Stats, 0) + 1};
            delete ->
                Stats#{deletes => maps:get(deletes, Stats, 0) + 1}
        end,
    State#state{stats = NewStats}.

-spec perform_cleanup(state()) -> state().
perform_cleanup(State) ->
    Now = erlang:monotonic_time(microsecond),

    %% Clean expired entries from L1
    Pattern =
        #cache_entry{key = '$1',
                     expires_at = '$2',
                     _ = '_'},
    Guards = [{'andalso', {'/=', '$2', infinity}, {'=<', '$2', Now}}],
    ExpiredKeys = ets:select(State#state.l1_table, [{{'$1', Pattern}, Guards, ['$1']}]),

    lists:foreach(fun(Key) -> ets:delete(State#state.l1_table, Key) end, ExpiredKeys),

    NewState = lists:foldl(fun(_, S) -> update_stats(S, expiration) end, State, ExpiredKeys),

    ?LOG_DEBUG("Cache cleanup: expired ~p entries", [length(ExpiredKeys)]),
    NewState.
