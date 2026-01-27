-module(erlmcp_icon_cache).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    cache_icon/3,
    cache_icon/4,
    get_cached_icon/1,
    invalidate_icon/1,
    invalidate_all/0,
    get_cache_stats/0,
    set_ttl/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-type icon_uri() :: binary().
-type icon_metadata() :: map().
-type ttl_ms() :: pos_integer().
-type cache_result() :: {ok, icon_metadata()} | {expired, icon_metadata()} | not_found.

-record(cache_entry, {
    uri :: icon_uri(),
    metadata :: icon_metadata(),
    cached_at :: integer(),
    expires_at :: integer()
}).

-record(state, {
    cache = #{} :: #{icon_uri() => #cache_entry{}},
    ttl_ms :: ttl_ms(),
    stats :: map()
}).

-type state() :: #state{}.

-define(DEFAULT_TTL_MS, 3600000).
-define(CLEANUP_INTERVAL_MS, 300000).

%% API

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec cache_icon(icon_uri(), icon_metadata(), ttl_ms()) -> ok.
cache_icon(Uri, Metadata, TtlMs) when is_binary(Uri), is_map(Metadata), is_integer(TtlMs) ->
    cache_icon(Uri, Metadata, TtlMs, erlang:monotonic_time(millisecond)).

-spec cache_icon(icon_uri(), icon_metadata(), ttl_ms(), integer()) -> ok.
cache_icon(Uri, Metadata, TtlMs, CachedAt)
  when is_binary(Uri), is_map(Metadata), is_integer(TtlMs), is_integer(CachedAt) ->
    gen_server:call(?MODULE, {cache_icon, Uri, Metadata, TtlMs, CachedAt}).

-spec get_cached_icon(icon_uri()) -> cache_result().
get_cached_icon(Uri) when is_binary(Uri) ->
    gen_server:call(?MODULE, {get_cached_icon, Uri}).

-spec invalidate_icon(icon_uri()) -> ok.
invalidate_icon(Uri) when is_binary(Uri) ->
    gen_server:cast(?MODULE, {invalidate_icon, Uri}).

-spec invalidate_all() -> ok.
invalidate_all() ->
    gen_server:cast(?MODULE, invalidate_all).

-spec get_cache_stats() -> map().
get_cache_stats() ->
    gen_server:call(?MODULE, get_cache_stats).

-spec set_ttl(ttl_ms()) -> ok.
set_ttl(TtlMs) when is_integer(TtlMs), TtlMs > 0 ->
    gen_server:call(?MODULE, {set_ttl, TtlMs}).

%% gen_server callbacks

-spec init([]) -> {ok, state()}.
init([]) ->
    TtlMs = get_configured_ttl(),
    State = #state{
        ttl_ms = TtlMs,
        cache = #{},
        stats = #{hits => 0, misses => 0, expirations => 0, invalidations => 0}
    },
    erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_expired),
    logger:info("Icon cache initialized with TTL: ~pms", [TtlMs]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.

handle_call({cache_icon, Uri, Metadata, TtlMs, CachedAt}, _From, State) ->
    ExpiresAt = CachedAt + TtlMs,
    Entry = #cache_entry{uri = Uri, metadata = Metadata, cached_at = CachedAt, expires_at = ExpiresAt},
    NewCache = maps:put(Uri, Entry, State#state.cache),
    {reply, ok, State#state{cache = NewCache}};

handle_call({get_cached_icon, Uri}, _From, State) ->
    Now = erlang:monotonic_time(millisecond),
    case maps:get(Uri, State#state.cache, undefined) of
        undefined ->
            Stats = State#state.stats,
            NewStats = maps:put(misses, maps:get(misses, Stats, 0) + 1, Stats),
            {reply, not_found, State#state{stats = NewStats}};
        #cache_entry{expires_at = ExpiresAt, metadata = Metadata} ->
            case Now < ExpiresAt of
                true ->
                    Stats = State#state.stats,
                    NewStats = maps:put(hits, maps:get(hits, Stats, 0) + 1, Stats),
                    {reply, {ok, Metadata}, State#state{stats = NewStats}};
                false ->
                    Stats = State#state.stats,
                    NewStats = maps:put(expirations, maps:get(expirations, Stats, 0) + 1, Stats),
                    ExpiredCache = maps:remove(Uri, State#state.cache),
                    {reply, {expired, Metadata}, State#state{cache = ExpiredCache, stats = NewStats}}
            end
    end;

handle_call({set_ttl, TtlMs}, _From, State) ->
    logger:info("Icon cache TTL updated to: ~pms", [TtlMs]),
    {reply, ok, State#state{ttl_ms = TtlMs}};

handle_call(get_cache_stats, _From, State) ->
    Stats = State#state.stats,
    AllStats = maps:merge(Stats, #{cache_size => maps:size(State#state.cache), current_ttl => State#state.ttl_ms}),
    {reply, AllStats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast({invalidate_icon, Uri}, State) ->
    NewCache = maps:remove(Uri, State#state.cache),
    Stats = State#state.stats,
    NewStats = maps:put(invalidations, maps:get(invalidations, Stats, 0) + 1, Stats),
    logger:debug("Icon cache invalidated: ~s", [Uri]),
    {noreply, State#state{cache = NewCache, stats = NewStats}};

handle_cast(invalidate_all, State) ->
    Count = maps:size(State#state.cache),
    logger:info("Icon cache fully cleared (~p entries)", [Count]),
    {noreply, State#state{cache = #{}}};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

handle_info(cleanup_expired, State) ->
    Now = erlang:monotonic_time(millisecond),
    Cache = State#state.cache,
    CleanedCache = maps:filter(fun(_Uri, #cache_entry{expires_at = ExpiresAt}) ->
        Now < ExpiresAt
    end, Cache),
    ExpiredCount = maps:size(Cache) - maps:size(CleanedCache),
    NewState = case ExpiredCount > 0 of
        true ->
            logger:debug("Icon cache cleanup removed ~p expired entries", [ExpiredCount]),
            State#state{cache = CleanedCache};
        false ->
            State
    end,
    erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_expired),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    logger:info("Icon cache shutting down"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

-spec get_configured_ttl() -> ttl_ms().
get_configured_ttl() ->
    case application:get_env(erlmcp, icon_cache_ttl_ms, undefined) of
        undefined -> ?DEFAULT_TTL_MS;
        TtlMs when is_integer(TtlMs), TtlMs > 0 -> TtlMs;
        _ -> ?DEFAULT_TTL_MS
    end.
