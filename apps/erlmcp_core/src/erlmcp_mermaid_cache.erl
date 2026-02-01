-module(erlmcp_mermaid_cache).
-behaviour(gen_server).

%% API
-export([start_link/0,
         get/3,
         put/4,
         invalidate/2,
         clear/0,
         stats/0,
         hit_rate/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_TTL_MS, 3600000). % 1 hour
-define(DEFAULT_MAX_SIZE, 10000).

%% State record
-record(state, {
    cache_tab :: ets:tid(),
    ttl_ms :: pos_integer(),
    max_size :: pos_integer(),
    hits :: non_neg_integer(),
    misses :: non_neg_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the cache server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get cached diagram
-spec get(binary(), binary(), map()) -> {ok, binary()} | {error, not_found}.
get(DiagramCode, DiagramType, Options) ->
    gen_server:call(?SERVER, {get, DiagramCode, DiagramType, Options}).

%% @doc Put diagram in cache
-spec put(binary(), binary(), map(), binary()) -> ok.
put(DiagramCode, DiagramType, Options, SVG) ->
    gen_server:call(?SERVER, {put, DiagramCode, DiagramType, Options, SVG}).

%% @doc Invalidate cache entry
-spec invalidate(binary(), binary()) -> ok.
invalidate(DiagramCode, DiagramType) ->
    gen_server:call(?SERVER, {invalidate, DiagramCode, DiagramType}).

%% @doc Clear entire cache
-spec clear() -> ok.
clear() ->
    gen_server:call(?SERVER, clear).

%% @doc Get cache statistics
-spec stats() -> map().
stats() ->
    gen_server:call(?SERVER, stats).

%% @doc Get cache hit rate
-spec hit_rate() -> float().
hit_rate() ->
    gen_server:call(?SERVER, hit_rate).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    CacheTab = ets:new(mermaid_cache, [
        named_table,
        public,
        set,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    TTL = application:get_env(erlmcp_core, mermaid_cache_ttl_ms, ?DEFAULT_TTL_MS),
    MaxSize = application:get_env(erlmcp_core, mermaid_cache_max_size, ?DEFAULT_MAX_SIZE),
    {ok, #state{
        cache_tab = CacheTab,
        ttl_ms = TTL,
        max_size = MaxSize,
        hits = 0,
        misses = 0
    }}.

%% @private
handle_call({get, DiagramCode, DiagramType, Options}, _From, State) ->
    Key = generate_cache_key(DiagramCode, DiagramType, Options),
    case ets:lookup(State#state.cache_tab, Key) of
        [{Key, {SVG, Timestamp, AccessCount}}] ->
            case is_fresh(Timestamp, State#state.ttl_ms) of
                true ->
                    % Update access count for LRU
                    ets:insert(State#state.cache_tab, {Key, {SVG, Timestamp, AccessCount + 1}}),
                    NewState = State#state{hits = State#state.hits + 1},
                    {reply, {ok, SVG}, NewState};
                false ->
                    ets:delete(State#state.cache_tab, Key),
                    NewState = State#state{misses = State#state.misses + 1},
                    {reply, {error, not_found}, NewState}
            end;
        [] ->
            NewState = State#state{misses = State#state.misses + 1},
            {reply, {error, not_found}, NewState}
    end;

handle_call({put, DiagramCode, DiagramType, Options, SVG}, _From, State) ->
    Key = generate_cache_key(DiagramCode, DiagramType, Options),
    Timestamp = erlang:system_time(millisecond),

    % Check if cache is full
    case ets:info(State#state.cache_tab, size) >= State#state.max_size of
        true ->
            evict_lru(State#state.cache_tab);
        false ->
            ok
    end,

    ets:insert(State#state.cache_tab, {Key, {SVG, Timestamp, 0}}),
    {reply, ok, State};

handle_call({invalidate, DiagramCode, DiagramType}, _From, State) ->
    Key = generate_cache_key(DiagramCode, DiagramType, #{}),
    ets:delete(State#state.cache_tab, Key),
    {reply, ok, State};

handle_call(clear, _From, State) ->
    ets:delete_all_objects(State#state.cache_tab),
    NewState = State#state{hits = 0, misses = 0},
    {reply, ok, NewState};

handle_call(stats, _From, State) ->
    Stats = #{
        size => ets:info(State#state.cache_tab, size),
        memory => ets:info(State#state.cache_tab, memory),
        memory_mb => ets:info(State#state.cache_tab, memory) / (1024 * 1024),
        ttl_ms => State#state.ttl_ms,
        max_size => State#state.max_size,
        hits => State#state.hits,
        misses => State#state.misses,
        hit_rate => calculate_hit_rate(State#state.hits, State#state.misses)
    },
    {reply, Stats, State};

handle_call(hit_rate, _From, State) ->
    Rate = calculate_hit_rate(State#state.hits, State#state.misses),
    {reply, Rate, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    ets:delete(State#state.cache_tab),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
generate_cache_key(DiagramCode, DiagramType, Options) ->
    Theme = maps:get(<<"theme">>, Options, <<"default">>),
    Width = maps:get(<<"width">>, Options, <<>>),
    Height = maps:get(<<"height">>, Options, <<>>),
    Data = <<DiagramCode/binary, DiagramType/binary, Theme/binary, Width/binary, Height/binary>>,
    crypto:hash(sha256, Data).

%% @private
is_fresh(Timestamp, TTL) ->
    (erlang:system_time(millisecond) - Timestamp) < TTL.

%% @private
evict_lru(Tab) ->
    % Find entry with lowest access count and oldest timestamp
    case ets:foldl(fun({Key, {_Value, Timestamp, AccessCount}}, {MinKey, MinTS, MinCount} = Acc) ->
        case {AccessCount < MinCount, AccessCount =:= MinCount andalso Timestamp < MinTS} of
            {true, _} -> {Key, Timestamp, AccessCount};
            {_, true} -> {Key, Timestamp, AccessCount};
            _ -> Acc
        end
    end, {undefined, infinity, infinity}, Tab) of
        {undefined, _, _} -> ok;
        {Key, _, _} -> ets:delete(Tab, Key)
    end.

%% @private
calculate_hit_rate(0, 0) -> 0.0;
calculate_hit_rate(Hits, Misses) ->
    Total = Hits + Misses,
    case Total of
        0 -> 0.0;
        _ -> Hits / Total
    end.
