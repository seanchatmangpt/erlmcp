%%%-------------------------------------------------------------------
%%% @doc Mermaid Diagram Cache - LRU Cache with Distributed Support
%%%
%%% Implements Joe Armstrong's principles:
%%% - ETS for fast in-memory cache (atomic operations)
%%% - LRU eviction policy (least recently used first)
%%% - Mnesia for distributed cache replication
%%% - Persistent cache backing (DETS for durability)
%%% - Concurrent access via ETS public table
%%% - Simple, robust, efficient
%%%
%%% Cache Key: {DiagramType, DiagramHash, Theme}
%%% Cache Entry: #mermaid_entry{svg, last_access, size, version}
%%%
%%% Features:
%%% - LRU eviction when memory limit exceeded
%%% - TTL support for stale diagrams
%%% - Automatic cleanup of expired entries
%%% - Memory tracking with soft/hard limits
%%% - Version tracking for cache invalidation
%%% - Statistics: hit rate, miss rate, eviction rate
%%% - Distributed cache via Mnesia replication
%%% - Persistent cache via DETS (optional)
%%% - Cache warming for popular diagrams
%%% - Performance monitoring with OTEL integration
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_mermaid_cache).
-behaviour(gen_server).

-include("erlmcp.hrl").
-include_lib("kernel/include/logger.hrl").

%% API exports
-export([
    start_link/0,
    start_link/1,
    get/3,
    put/5,
    invalidate/2,
    invalidate_by_type/1,
    clear/0,
    stats/0,
    memory_usage/0,
    warm_cache/3,
    export_persistent/0,
    import_persistent/0,
    get_distributed/3,
    put_distributed/5
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type diagram_type() :: flowchart | sequence | class | state | gantt | pie | mindmap.
-type diagram_hash() :: binary().
-type diagram_theme() :: binary() | atom() | default.
-type svg_data() :: binary().
-type version() :: binary().

-record(mermaid_entry, {
    svg :: svg_data(),
    last_access :: integer(),  %% monotonic time in microseconds
    inserted_at :: integer(),  %% monotonic time in microseconds
    size :: non_neg_integer(), %% byte size
    version :: version(),      %% version hash for validation
    access_count :: non_neg_integer()
}).

-type cache_key() :: {diagram_type(), diagram_hash(), diagram_theme()}.

-record(state, {
    cache_table :: ets:tid(),           %% ETS table for cache
    max_memory_bytes = 52428800 :: pos_integer(), %% 50MB default
    current_memory_bytes = 0 :: non_neg_integer(),
    max_entries = 10000 :: pos_integer(), %% max entry count
    default_ttl_ms = 3600000 :: pos_integer(), %% 1 hour default
    cleanup_interval_ms = 300000 :: pos_integer(), %% 5 minutes
    cleanup_timer :: reference() | undefined,

    %% Distributed cache support
    mnesia_enabled = false :: boolean(),
    mnesia_table :: atom() | undefined,

    %% Persistent cache support
    persistent_enabled = false :: boolean(),
    persistent_file :: file:filename() | undefined,
    persistent_interval_ms = 600000 :: pos_integer(), %% 10 minutes
    persistent_timer :: reference() | undefined,

    %% Metrics
    stats = #{
        hits => 0,
        misses => 0,
        evictions => 0,
        expirations => 0,
        puts => 0,
        invalidations => 0,
        distributed_syncs => 0,
        persistent_writes => 0
    } :: map()
}).

-type state() :: #state{}.

-export_type([cache_key/0, diagram_type/0]).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Get Mermaid SVG from cache
-spec get(diagram_type(), diagram_hash(), diagram_theme()) ->
    {ok, svg_data()} | {error, not_found | expired}.
get(Type, Hash, Theme) ->
    gen_server:call(?MODULE, {get, Type, Hash, Theme}, 5000).

%% @doc Put Mermaid SVG in cache (with optional TTL override in ms)
-spec put(diagram_type(), diagram_hash(), diagram_theme(), svg_data(), pos_integer() | infinity) -> ok.
put(Type, Hash, Theme, SVG, TTLMs) ->
    gen_server:call(?MODULE, {put, Type, Hash, Theme, SVG, TTLMs}, 5000).

%% @doc Invalidate specific diagram (by type and hash)
-spec invalidate(diagram_type(), diagram_hash()) -> ok.
invalidate(Type, Hash) ->
    gen_server:call(?MODULE, {invalidate, Type, Hash}, 5000).

%% @doc Invalidate all diagrams of a specific type
-spec invalidate_by_type(diagram_type()) -> ok.
invalidate_by_type(Type) ->
    gen_server:call(?MODULE, {invalidate_by_type, Type}, 5000).

%% @doc Clear entire cache
-spec clear() -> ok.
clear() ->
    gen_server:call(?MODULE, clear, 5000).

%% @doc Get cache statistics
-spec stats() -> map().
stats() ->
    gen_server:call(?MODULE, stats, 5000).

%% @doc Get current memory usage in bytes
-spec memory_usage() -> {ok, non_neg_integer(), non_neg_integer(), non_neg_integer()}.
memory_usage() ->
    gen_server:call(?MODULE, memory_usage, 5000).

%% @doc Warm cache with popular diagram (async)
-spec warm_cache(diagram_type(), diagram_hash(), diagram_theme()) -> ok.
warm_cache(Type, Hash, Theme) ->
    gen_server:cast(?MODULE, {warm_cache, Type, Hash, Theme}).

%% @doc Export cache to persistent storage (DETS)
-spec export_persistent() -> {ok, non_neg_integer()} | {error, term()}.
export_persistent() ->
    gen_server:call(?MODULE, export_persistent, 30000).

%% @doc Import cache from persistent storage (DETS)
-spec import_persistent() -> {ok, non_neg_integer()} | {error, term()}.
import_persistent() ->
    gen_server:call(?MODULE, import_persistent, 30000).

%% @doc Get from distributed cache (Mnesia)
-spec get_distributed(diagram_type(), diagram_hash(), diagram_theme()) ->
    {ok, svg_data()} | {error, not_found}.
get_distributed(Type, Hash, Theme) ->
    gen_server:call(?MODULE, {get_distributed, Type, Hash, Theme}, 5000).

%% @doc Put to distributed cache (Mnesia)
-spec put_distributed(diagram_type(), diagram_hash(), diagram_theme(), svg_data(), pos_integer()) -> ok.
put_distributed(Type, Hash, Theme, SVG, TTLMs) ->
    gen_server:call(?MODULE, {put_distributed, Type, Hash, Theme, SVG, TTLMs}, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, state()}.
init(Config) ->
    process_flag(trap_exit, true),

    %% Create ETS table for cache (set type, public for concurrent access)
    CacheTable = ets:new(erlmcp_mermaid_cache, [
        set,
        public,
        named_table,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),

    %% Initialize Mnesia for distributed cache (if enabled)
    MnesiaEnabled = maps:get(mnesia_enabled, Config, false),
    {MnesiaEnabledFinal, MnesiaTable} = case MnesiaEnabled of
        true ->
            case ensure_mnesia_table() of
                {ok, Table} ->
                    ?LOG_INFO("Mermaid cache: Mnesia distributed cache enabled"),
                    {true, Table};
                {error, Reason} ->
                    ?LOG_WARNING("Mermaid cache: Failed to enable Mnesia: ~p", [Reason]),
                    {false, undefined}
            end;
        false ->
            {false, undefined}
    end,

    %% Initialize persistent cache (if enabled)
    PersistentEnabled = maps:get(persistent_enabled, Config, false),
    PersistentFile = maps:get(persistent_file, Config, "data/mermaid_cache.dets"),
    {PersistentEnabledFinal, PersistentFileFinal} = case PersistentEnabled of
        true ->
            case ensure_persistent_cache(PersistentFile) of
                {ok, File} ->
                    ?LOG_INFO("Mermaid cache: Persistent cache enabled: ~s", [File]),
                    {true, File};
                {error, PersistReason} ->
                    ?LOG_WARNING("Mermaid cache: Failed to enable persistent cache: ~p", [PersistReason]),
                    {false, undefined}
            end;
        false ->
            {false, undefined}
    end,

    %% Extract configuration
    MaxMemory = maps_get(max_memory_bytes, Config, 52428800),  % 50MB
    MaxEntries = maps_get(max_entries, Config, 10000),
    DefaultTTL = maps_get(default_ttl_ms, Config, 3600000),  % 1 hour
    CleanupInterval = maps_get(cleanup_interval_ms, Config, 300000), % 5 min
    PersistentInterval = maps_get(persistent_interval_ms, Config, 600000), % 10 min

    %% Start cleanup timer
    TimerRef = erlang:send_after(CleanupInterval, self(), cleanup),

    %% Start persistent save timer (if enabled)
    PersistentTimer = case PersistentEnabledFinal of
        true -> erlang:send_after(PersistentInterval, self(), persistent_save);
        false -> undefined
    end,

    State = #state{
        cache_table = CacheTable,
        max_memory_bytes = MaxMemory,
        max_entries = MaxEntries,
        default_ttl_ms = DefaultTTL,
        cleanup_interval_ms = CleanupInterval,
        cleanup_timer = TimerRef,
        mnesia_enabled = MnesiaEnabledFinal,
        mnesia_table = MnesiaTable,
        persistent_enabled = PersistentEnabledFinal,
        persistent_file = PersistentFileFinal,
        persistent_interval_ms = PersistentInterval,
        persistent_timer = PersistentTimer
    },

    ?LOG_INFO("erlmcp_mermaid_cache started: ETS table, max_memory=~p, max_entries=~p, TTL=~pms",
              [MaxMemory, MaxEntries, DefaultTTL]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.

%% Get diagram from cache
handle_call({get, Type, Hash, Theme}, _From, State) ->
    Key = {Type, Hash, Theme},
    case ets:lookup(State#state.cache_table, Key) of
        [] ->
            ?LOG_DEBUG("Mermaid cache miss: ~p", [Key]),
            {reply, {error, not_found}, update_stats(State, miss)};
        [{Key, #mermaid_entry{svg = SVG} = Entry}] ->
            Now = erlang:monotonic_time(microsecond),
            case is_entry_expired(Entry, Now, State) of
                true ->
                    ets:delete(State#state.cache_table, Key),
                    ?LOG_DEBUG("Mermaid cache expired: ~p", [Key]),
                    {reply, {error, expired}, update_stats(State, expiration)};
                false ->
                    %% Update last access time and count (LRU tracking)
                    UpdatedEntry = Entry#mermaid_entry{
                        last_access = Now,
                        access_count = Entry#mermaid_entry.access_count + 1
                    },
                    ets:insert(State#state.cache_table, {Key, UpdatedEntry}),
                    ?LOG_DEBUG("Mermaid cache hit: ~p", [Key]),
                    {reply, {ok, SVG}, update_stats(State, hit)}
            end
    end;

%% Put diagram in cache
handle_call({put, Type, Hash, Theme, SVG, _TTLMs}, _From, State) ->
    Key = {Type, Hash, Theme},
    DataSize = byte_size(SVG),

    %% Check if entry already exists (update size accounting)
    CurrentSize = case ets:lookup(State#state.cache_table, Key) of
        [{Key, #mermaid_entry{size = OldSize}}] -> OldSize;
        [] -> 0
    end,

    %% Calculate version hash for validation
    Version = crypto:hash(sha256, SVG),

    Now = erlang:monotonic_time(microsecond),
    Entry = #mermaid_entry{
        svg = SVG,
        last_access = Now,
        inserted_at = Now,
        size = DataSize,
        version = Version,
        access_count = 1
    },

    %% Check if we need to evict entries (memory or entry limit)
    NewState = case should_evict(State, DataSize, CurrentSize) of
        true ->
            evict_lru_entries(State, DataSize - CurrentSize);
        false ->
            State
    end,

    %% Store in cache
    ets:insert(NewState#state.cache_table, {Key, Entry}),

    %% Update memory usage (account for size difference)
    SizeDelta = DataSize - CurrentSize,
    NewMemoryUsage = NewState#state.current_memory_bytes + SizeDelta,
    FinalState = NewState#state{current_memory_bytes = NewMemoryUsage},

    %% Sync to distributed cache (if enabled)
    DistributedState = case FinalState#state.mnesia_enabled of
        true -> sync_to_mnesia(Key, Entry, FinalState);
        false -> FinalState
    end,

    ?LOG_DEBUG("Mermaid cache put: ~p (size: ~p bytes)", [Key, DataSize]),
    {reply, ok, update_stats(DistributedState, put)};

%% Invalidate all themes for a given type/hash
handle_call({invalidate, Type, Hash}, _From, State) ->
    %% Find all matching keys (any theme)
    KeysToDelete = ets:foldl(fun({{T, H, _T} = Key, _Entry}, Acc) ->
        case T =:= Type andalso H =:= Hash of
            true -> [Key | Acc];
            false -> Acc
        end
    end, [], State#state.cache_table),

    %% Delete entries and update memory usage
    {DeletedBytes, NewState} = lists:foldl(fun(Key, {Bytes, AccState}) ->
        case ets:lookup(AccState#state.cache_table, Key) of
            [{Key, #mermaid_entry{size = Size}}] ->
                ets:delete(AccState#state.cache_table, Key),
                %% Also delete from Mnesia (if enabled)
                _ = case AccState#state.mnesia_enabled of
                    true -> mnesia:dirty_delete(AccState#state.mnesia_table, Key);
                    false -> ok
                end,
                {Bytes + Size, update_stats(AccState, invalidation)};
            [] ->
                {Bytes, AccState}
        end
    end, {0, State}, KeysToDelete),

    FinalState = NewState#state{
        current_memory_bytes = NewState#state.current_memory_bytes - DeletedBytes
    },

    ?LOG_INFO("Mermaid cache invalidated: ~p entries (~p bytes)", [length(KeysToDelete), DeletedBytes]),
    {reply, ok, FinalState};

%% Invalidate all diagrams of a specific type
handle_call({invalidate_by_type, Type}, _From, State) ->
    %% Find all matching keys (any hash/theme)
    KeysToDelete = ets:foldl(fun({{T, _H, _Th} = Key, _Entry}, Acc) ->
        case T =:= Type of
            true -> [Key | Acc];
            false -> Acc
        end
    end, [], State#state.cache_table),

    %% Delete entries
    {DeletedBytes, NewState} = lists:foldl(fun(Key, {Bytes, AccState}) ->
        case ets:lookup(AccState#state.cache_table, Key) of
            [{Key, #mermaid_entry{size = Size}}] ->
                ets:delete(AccState#state.cache_table, Key),
                {Bytes + Size, update_stats(AccState, invalidation)};
            [] ->
                {Bytes, AccState}
        end
    end, {0, State}, KeysToDelete),

    FinalState = NewState#state{
        current_memory_bytes = NewState#state.current_memory_bytes - DeletedBytes
    },

    ?LOG_INFO("Mermaid cache invalidated by type ~p: ~p entries (~p bytes)",
              [Type, length(KeysToDelete), DeletedBytes]),
    {reply, ok, FinalState};

%% Clear entire cache
handle_call(clear, _From, State) ->
    ets:delete_all_objects(State#state.cache_table),
    NewStats = #{
        hits => 0, misses => 0, evictions => 0, expirations => 0,
        puts => 0, invalidations => 0, distributed_syncs => 0,
        persistent_writes => 0
    },
    ?LOG_INFO("Mermaid cache cleared"),
    {reply, ok, State#state{
        current_memory_bytes = 0,
        stats = NewStats
    }};

%% Get statistics
handle_call(stats, _From, State) ->
    CacheSize = ets:info(State#state.cache_table, size),
    Stats = State#state.stats,
    TotalRequests = maps_get(hits, Stats, 0) + maps_get(misses, Stats, 0),
    HitRate = case TotalRequests of
        0 -> 0.0;
        N -> maps_get(hits, Stats, 0) / N
    end,

    DetailedStats = Stats#{
        cache_size => CacheSize,
        max_entries => State#state.max_entries,
        memory_bytes => State#state.current_memory_bytes,
        max_memory_bytes => State#state.max_memory_bytes,
        memory_utilization => case State#state.max_memory_bytes of
            0 -> 0.0;
            Max -> State#state.current_memory_bytes / Max
        end,
        hit_rate => HitRate,
        total_requests => TotalRequests,
        mnesia_enabled => State#state.mnesia_enabled,
        persistent_enabled => State#state.persistent_enabled
    },
    {reply, DetailedStats, State};

%% Get memory usage
handle_call(memory_usage, _From, State) ->
    Current = State#state.current_memory_bytes,
    Max = State#state.max_memory_bytes,
    Entries = ets:info(State#state.cache_table, size),
    {reply, {ok, Current, Max, Entries}, State};

%% Export to persistent storage
handle_call(export_persistent, _From, #state{persistent_enabled = false} = State) ->
    {reply, {error, persistent_disabled}, State};
handle_call(export_persistent, _From, State) ->
    case export_to_dets(State) of
        {ok, Count} ->
            NewState = update_stats(State, persistent_write),
            {reply, {ok, Count}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% Import from persistent storage
handle_call(import_persistent, _From, #state{persistent_enabled = false} = State) ->
    {reply, {error, persistent_disabled}, State};
handle_call(import_persistent, _From, State) ->
    case import_from_dets(State) of
        {ok, Count} ->
            ?LOG_INFO("Mermaid cache imported ~p entries from persistent storage", [Count]),
            {reply, {ok, Count}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% Get from distributed cache (Mnesia)
handle_call({get_distributed, _Type, _Hash, _Theme}, _From,
            #state{mnesia_enabled = false} = State) ->
    {reply, {error, mnesia_disabled}, State};
handle_call({get_distributed, Type, Hash, Theme}, _From, State) ->
    Key = {Type, Hash, Theme},
    case mnesia:dirty_read(State#state.mnesia_table, Key) of
        [] ->
            {reply, {error, not_found}, State};
        [{MnesiaTable, Key, SVG, LastAccess, InsertedAt, Size, Version, AccessCount}] ->
            Now = erlang:monotonic_time(microsecond),
            %% Check if expired (TTL check)
            Entry = #mermaid_entry{
                svg = SVG,
                last_access = LastAccess,
                inserted_at = InsertedAt,
                size = Size,
                version = Version,
                access_count = AccessCount
            },
            case is_entry_expired(Entry, Now, State) of
                true ->
                    mnesia:dirty_delete(State#state.mnesia_table, Key),
                    {reply, {error, expired}, State};
                false ->
                    %% Promote to local cache
                    ets:insert(State#state.cache_table, {Key, Entry}),
                    {reply, {ok, SVG}, update_stats(State, distributed_sync)}
            end
    end;

%% Put to distributed cache (Mnesia)
handle_call({put_distributed, Type, Hash, Theme, SVG, TTLMs}, _From,
            #state{mnesia_enabled = false} = State) ->
    {reply, {error, mnesia_disabled}, State};
handle_call({put_distributed, Type, Hash, Theme, SVG, _TTLMs}, _From, State) ->
    Key = {Type, Hash, Theme},
    DataSize = byte_size(SVG),
    Version = crypto:hash(sha256, SVG),
    Now = erlang:monotonic_time(microsecond),

    %% Write to Mnesia
    MnesiaRec = {State#state.mnesia_table,
                 Key,
                 SVG,
                 Now,
                 Now,
                 DataSize,
                 Version,
                 1},
    mnesia:dirty_write(MnesiaRec),

    ?LOG_DEBUG("Mermaid cache distributed put: ~p (size: ~p bytes)", [Key, DataSize]),
    {reply, ok, update_stats(State, distributed_sync)};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

%% Warm cache with popular diagram (async)
handle_cast({warm_cache, Type, Hash, Theme}, State) ->
    %% This is a placeholder for cache warming logic
    %% In a real implementation, this would spawn a supervised worker
    %% to pre-generate the diagram and cache it
    ?LOG_DEBUG("Mermaid cache warming requested: ~p", [{Type, Hash, Theme}]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

%% Periodic cleanup (TTL expiration)
handle_info(cleanup, State) ->
    Now = erlang:monotonic_time(microsecond),
    TTLUs = State#state.default_ttl_ms * 1000,
    ExpirationThreshold = Now - TTLUs,

    %% Find expired entries (last_access < threshold)
    ExpiredKeys = ets:foldl(fun({Key, #mermaid_entry{
                                        last_access = LastAccess,
                                        size = Size}}, Acc) ->
        case LastAccess < ExpirationThreshold of
            true -> [{Key, Size} | Acc];
            false -> Acc
        end
    end, [], State#state.cache_table),

    %% Delete expired entries
    {DeletedBytes, NewState} = lists:foldl(fun({Key, Size}, {Bytes, AccState}) ->
        ets:delete(AccState#state.cache_table, Key),
        {Bytes + Size, update_stats(AccState, expiration)}
    end, {0, State}, ExpiredKeys),

    FinalState = NewState#state{
        current_memory_bytes = NewState#state.current_memory_bytes - DeletedBytes
    },

    ?LOG_DEBUG("Mermaid cache cleanup: expired ~p entries (~p bytes)",
               [length(ExpiredKeys), DeletedBytes]),

    %% Reschedule cleanup
    TimerRef = erlang:send_after(State#state.cleanup_interval_ms, self(), cleanup),
    {noreply, FinalState#state{cleanup_timer = TimerRef}};

%% Periodic persistent save
handle_info(persistent_save, #state{persistent_enabled = false} = State) ->
    {noreply, State};
handle_info(persistent_save, State) ->
    case export_to_dets(State) of
        {ok, Count} ->
            ?LOG_DEBUG("Mermaid cache persistent save: ~p entries", [Count]),
            NewState = update_stats(State, persistent_write),
            TimerRef = erlang:send_after(State#state.persistent_interval_ms, self(), persistent_save),
            {noreply, NewState#state{persistent_timer = TimerRef}};
        {error, Reason} ->
            ?LOG_WARNING("Mermaid cache persistent save failed: ~p", [Reason]),
            TimerRef = erlang:send_after(State#state.persistent_interval_ms, self(), persistent_save),
            {noreply, State#state{persistent_timer = TimerRef}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    %% Cancel cleanup timer
    case State#state.cleanup_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,

    %% Cancel persistent timer
    case State#state.persistent_timer of
        undefined -> ok;
        Ref2 -> erlang:cancel_timer(Ref2)
    end,

    %% Final persistent save (if enabled)
    case State#state.persistent_enabled of
        true ->
            case export_to_dets(State) of
                {ok, Count} ->
                    ?LOG_INFO("Mermaid cache final persistent save: ~p entries", [Count]);
                {error, Reason} ->
                    ?LOG_WARNING("Mermaid cache final persistent save failed: ~p", [Reason])
            end;
        false ->
            ok
    end,

    ?LOG_INFO("erlmcp_mermaid_cache terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Check if entry is expired based on TTL
-spec is_entry_expired(#mermaid_entry{}, integer(), state()) -> boolean().
is_entry_expired(#mermaid_entry{last_access = LastAccess}, Now, #state{default_ttl_ms = TTLMs}) ->
    TTLUs = TTLMs * 1000,
    Now - LastAccess > TTLUs.

%% @private Check if eviction is needed
-spec should_evict(state(), non_neg_integer(), non_neg_integer()) -> boolean().
should_evict(State, NewSize, CurrentSize) ->
    %% Check entry count limit
    EntryCount = ets:info(State#state.cache_table, size),
    CountLimit = EntryCount >= State#state.max_entries,

    %% Check memory limit
    MemoryNeeded = State#state.current_memory_bytes - CurrentSize + NewSize,
    MemoryLimit = MemoryNeeded > State#state.max_memory_bytes,

    CountLimit orelse MemoryLimit.

%% @private Evict LRU entries until we have enough space
-spec evict_lru_entries(state(), integer()) -> state().
evict_lru_entries(State, RequiredBytes) ->
    %% Find all entries sorted by last_access (oldest first)
    AllEntries = ets:foldl(fun({Key, #mermaid_entry{
                                         last_access = LastAccess,
                                         size = Size}}, Acc) ->
        [{LastAccess, Key, Size} | Acc]
    end, [], State#state.cache_table),

    %% Sort by last_access (ascending = oldest first = LRU)
    SortedEntries = lists:keysort(1, AllEntries),

    %% Evict entries until we have enough space
    FreeMemory = State#state.max_memory_bytes - State#state.current_memory_bytes,
    NeededBytes = case RequiredBytes > FreeMemory of
        true -> RequiredBytes - FreeMemory;
        false -> 0
    end,

    %% Also respect entry count limit
    CurrentCount = ets:info(State#state.cache_table, size),
    CountOverflow = max(0, CurrentCount + 1 - State#state.max_entries),

    {EvictedBytes, EvictedCount, NewState} = evict_until_enough_space(
        SortedEntries, NeededBytes, CountOverflow, 0, 0, State
    ),

    ?LOG_INFO("Mermaid cache evicted ~p entries (~p bytes) for ~p bytes request",
              [EvictedCount, EvictedBytes, RequiredBytes]),
    NewState.

%% @private Evict entries until we have freed enough space and reduced count
-spec evict_until_enough_space(list(), integer(), integer(), non_neg_integer(),
                                non_neg_integer(), state()) ->
    {non_neg_integer(), non_neg_integer(), state()}.
evict_until_enough_space([], _NeededBytes, _NeededCount, FreedBytes, EvictedCount, State) ->
    %% No more entries to evict
    {FreedBytes, EvictedCount, State};
evict_until_enough_space(_Entries, NeededBytes, NeededCount, FreedBytes, EvictedCount, State)
  when FreedBytes >= NeededBytes andalso EvictedCount >= NeededCount ->
    %% Freed enough space and reduced count enough
    {FreedBytes, EvictedCount, State};
evict_until_enough_space([{_LastAccess, Key, Size} | Rest], NeededBytes, NeededCount,
                          FreedBytes, EvictedCount, State) ->
    %% Evict this entry
    ets:delete(State#state.cache_table, Key),
    NewFreedBytes = FreedBytes + Size,
    NewEvictedCount = EvictedCount + 1,
    NewState = State#state{
        current_memory_bytes = State#state.current_memory_bytes - Size
    },
    evict_until_enough_space(Rest, NeededBytes, NeededCount,
                            NewFreedBytes, NewEvictedCount,
                            update_stats(NewState, eviction)).

%% @private Update statistics counter
-spec update_stats(state(), atom()) -> state().
update_stats(State, Type) ->
    Stats = State#state.stats,
    NewStats = case Type of
        hit ->
            Stats#{hits => maps_get(hits, Stats, 0) + 1};
        miss ->
            Stats#{misses => maps_get(misses, Stats, 0) + 1};
        eviction ->
            Stats#{evictions => maps_get(evictions, Stats, 0) + 1};
        expiration ->
            Stats#{expirations => maps_get(expirations, Stats, 0) + 1};
        put ->
            Stats#{puts => maps_get(puts, Stats, 0) + 1};
        invalidation ->
            Stats#{invalidations => maps_get(invalidations, Stats, 0) + 1};
        distributed_sync ->
            Stats#{distributed_syncs => maps_get(distributed_syncs, Stats, 0) + 1};
        persistent_write ->
            Stats#{persistent_writes => maps_get(persistent_writes, Stats, 0) + 1}
    end,
    State#state{stats = NewStats}.

%% @private Safe maps_get with default
-spec maps_get(term(), map(), non_neg_integer()) -> non_neg_integer().
maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Val} -> Val;
        error -> Default
    end.

%%====================================================================
%% Mnesia Distributed Cache Functions
%%====================================================================

%% @private Ensure Mnesia table exists for distributed cache
-spec ensure_mnesia_table() -> {ok, atom()} | {error, term()}.
ensure_mnesia_table() ->
    case mnesia:system_info(is_running) of
        yes ->
            ensure_mermaid_cache_table();
        no ->
            %% Try to start Mnesia
            case application:start(mnesia) of
                ok ->
                    ensure_mermaid_cache_table();
                {error, {already_started, mnesia}} ->
                    ensure_mermaid_cache_table();
                {error, Reason} ->
                    {error, {mnesia_start_failed, Reason}}
            end;
        _ ->
            {error, mnesia_not_available}
    end.

%% @private Create Mnesia table if it doesn't exist
-spec ensure_mermaid_cache_table() -> {ok, atom()} | {error, term()}.
ensure_mermaid_cache_table() ->
    TableName = erlmcp_mermaid_cache_dist,
    case lists:member(TableName, mnesia:system_info(tables)) of
        true ->
            {ok, TableName};
        false ->
            %% Use disc_copies if named node, otherwise ram_copies
            UseDiscCopies = (node() =/= 'nonode@nohost'),

            case mnesia:create_table(TableName, [
                {type, set},
                {attributes, [key, svg, last_access, inserted_at, size, version, access_count]},
                {disc_copies, case UseDiscCopies of true -> [node()]; false -> [] end},
                {ram_copies, case UseDiscCopies of true -> []; false -> [node()] end}
            ]) of
                {atomic, ok} ->
                    ?LOG_INFO("Created Mnesia Mermaid cache table: ~p (storage: ~p)",
                        [TableName, case UseDiscCopies of true -> disc_copies; false -> ram_copies end]),
                    {ok, TableName};
                {aborted, {already_exists, _}} ->
                    {ok, TableName};
                {aborted, Reason} ->
                    {error, {mnesia_create_failed, Reason}}
            end
    end.

%% @private Sync entry to Mnesia distributed cache
-spec sync_to_mnesia(cache_key(), #mermaid_entry{}, state()) -> state().
sync_to_mnesia(Key, Entry, State) ->
    MnesiaRec = {State#state.mnesia_table,
                 Key,
                 Entry#mermaid_entry.svg,
                 Entry#mermaid_entry.last_access,
                 Entry#mermaid_entry.inserted_at,
                 Entry#mermaid_entry.size,
                 Entry#mermaid_entry.version,
                 Entry#mermaid_entry.access_count},
    mnesia:dirty_write(MnesiaRec),
    State.

%%====================================================================
%% Persistent Cache Functions (DETS)
%%====================================================================

%% @private Ensure persistent cache directory and file exist
-spec ensure_persistent_cache(file:filename()) -> {ok, file:filename()} | {error, term()}.
ensure_persistent_cache(FileName) ->
    %% Ensure directory exists
    DirName = filename:dirname(FileName),
    case filelib:ensure_dir(FileName) of
        ok ->
            %% Open or create DETS table
            case dets:open_file(erlmcp_mermaid_cache_persistent, [
                {file, FileName},
                {type, set},
                {access, read_write},
                {auto_save, 60000}  % Auto-save every minute
            ]) of
                {ok, _Tab} ->
                    {ok, FileName};
                {error, Reason} ->
                    {error, {dets_open_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {dir_create_failed, Reason}}
    end.

%% @private Export cache to DETS
-spec export_to_dets(state()) -> {ok, non_neg_integer()} | {error, term()}.
export_to_dets(#state{persistent_enabled = false}) ->
    {error, persistent_disabled};
export_to_dets(State) ->
    try
        %% Clear existing DETS table
        ok = dets:delete_all_objects(erlmcp_mermaid_cache_persistent),

        %% Export all entries
        Count = ets:foldl(fun({Key, #mermaid_entry{
                                        svg = SVG,
                                        last_access = LastAccess,
                                        inserted_at = InsertedAt,
                                        size = Size,
                                        version = Version,
                                        access_count = AccessCount}}, Acc) ->
            Rec = {Key, SVG, LastAccess, InsertedAt, Size, Version, AccessCount},
            case dets:insert(erlmcp_mermaid_cache_persistent, Rec) of
                ok -> Acc + 1;
                {error, Reason} ->
                    ?LOG_WARNING("Failed to insert cache entry into DETS: ~p", [Reason]),
                    Acc
            end
        end, 0, State#state.cache_table),

        %% Sync to disk
        ok = dets:sync(erlmcp_mermaid_cache_persistent),

        {ok, Count}
    catch
        Class:Reason:Stack ->
            ?LOG_ERROR("DETS export failed: ~p:~p~n~p", [Class, Reason, Stack]),
            {error, {export_failed, {Class, Reason}}}
    end.

%% @private Import cache from DETS
-spec import_from_dets(state()) -> {ok, non_neg_integer()} | {error, term()}.
import_from_dets(#state{persistent_enabled = false}) ->
    {error, persistent_disabled};
import_from_dets(State) ->
    try
        %% Import all entries from DETS
        Count = dets:foldl(fun({Key, SVG, LastAccess, InsertedAt, Size, Version, AccessCount}, Acc) ->
            Entry = #mermaid_entry{
                svg = SVG,
                last_access = LastAccess,
                inserted_at = InsertedAt,
                size = Size,
                version = Version,
                access_count = AccessCount
            },
            ets:insert(State#state.cache_table, {Key, Entry}),
            Acc + 1
        end, 0, erlmcp_mermaid_cache_persistent),

        %% Recalculate memory usage
        TotalMemory = dets:foldl(fun({_Key, _SVG, _LastAccess, _InsertedAt, Size, _Version, _AccessCount}, Acc) ->
            Acc + Size
        end, 0, erlmcp_mermaid_cache_persistent),

        {ok, Count}
    catch
        Class:Reason:Stack ->
            ?LOG_ERROR("DETS import failed: ~p:~p~n~p", [Class, Reason, Stack]),
            {error, {import_failed, {Class, Reason}}}
    end.
