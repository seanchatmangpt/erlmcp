%%% @doc MCP Advisor Service
%%%
%%% A discovery and recommendation service that helps find and recommend
%%% MCP servers based on natural language queries. Ported from mcpadvisor
%%% (TypeScript) to erlmcp.
%%%
%%% Features:
%%% - MCP server discovery from multiple providers
%%% - Natural language search queries
%%% - Hybrid search (text + vector similarity)
%%% - Multiple pluggable search providers
%%% - Caching with configurable TTL
%%%
%%% @end
-module(erlmcp_advisor).

-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    start_link/1,
    stop/0,
    %% Core discovery API
    search/1,
    search/2,
    recommend/1,
    recommend/2,
    %% Provider management
    register_provider/2,
    unregister_provider/1,
    list_providers/0,
    %% Cache management
    clear_cache/0,
    get_cache_stats/0,
    %% Status
    get_status/0,
    health_check/0
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

%%====================================================================
%% Type Definitions
%%====================================================================

-type provider_id() :: atom().
-type provider_module() :: module().
-type provider_config() :: map().

-type search_query() :: binary() | string().
-type search_options() :: #{
    limit => pos_integer(),
    min_similarity => float(),
    providers => [provider_id()],
    timeout => pos_integer()
}.

-type search_result() :: #{
    id => binary(),
    title => binary(),
    description => binary(),
    url => binary() | undefined,
    github_url => binary() | undefined,
    similarity => float(),
    provider => provider_id(),
    metadata => map()
}.

-type recommendation() :: #{
    server => search_result(),
    confidence => float(),
    reasons => [binary()]
}.

-export_type([
    provider_id/0,
    search_query/0,
    search_options/0,
    search_result/0,
    recommendation/0
]).

%%====================================================================
%% State Record
%%====================================================================

-record(state, {
    providers = #{} :: #{provider_id() => {provider_module(), provider_config()}},
    cache :: ets:tid(),
    cache_ttl = 300000 :: pos_integer(),  %% 5 minutes default
    default_limit = 10 :: pos_integer(),
    default_min_similarity = 0.3 :: float(),
    search_timeout = 5000 :: pos_integer(),
    last_discovery = undefined :: integer() | undefined,
    discovery_interval = 60000 :: pos_integer(),
    discovery_timer :: reference() | undefined,
    stats = #{} :: map()
}).

-type state() :: #state{}.

%%====================================================================
%% Macros
%%====================================================================

-define(SERVER, ?MODULE).
-define(CACHE_TABLE, erlmcp_advisor_cache).
-define(DEFAULT_TIMEOUT, 5000).
-define(HIBERNATE_AFTER, 30000).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the advisor service with default configuration
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the advisor service with custom configuration
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) when is_map(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config,
                          [{hibernate_after, ?HIBERNATE_AFTER}]).

%% @doc Stop the advisor service
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Search for MCP servers using natural language query
-spec search(search_query()) -> {ok, [search_result()]} | {error, term()}.
search(Query) ->
    search(Query, #{}).

%% @doc Search with options
-spec search(search_query(), search_options()) -> {ok, [search_result()]} | {error, term()}.
search(Query, Options) when is_list(Query) ->
    search(list_to_binary(Query), Options);
search(Query, Options) when is_binary(Query), is_map(Options) ->
    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
    gen_server:call(?SERVER, {search, Query, Options}, Timeout + 1000).

%% @doc Get a recommendation for a specific use case
-spec recommend(search_query()) -> {ok, recommendation()} | {error, term()}.
recommend(Query) ->
    recommend(Query, #{}).

%% @doc Recommend with options
-spec recommend(search_query(), search_options()) -> {ok, recommendation()} | {error, term()}.
recommend(Query, Options) when is_list(Query) ->
    recommend(list_to_binary(Query), Options);
recommend(Query, Options) when is_binary(Query), is_map(Options) ->
    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
    gen_server:call(?SERVER, {recommend, Query, Options}, Timeout + 1000).

%% @doc Register a search provider
-spec register_provider(provider_id(), {provider_module(), provider_config()}) ->
    ok | {error, term()}.
register_provider(ProviderId, {Module, Config}) when is_atom(ProviderId),
                                                     is_atom(Module),
                                                     is_map(Config) ->
    gen_server:call(?SERVER, {register_provider, ProviderId, Module, Config}).

%% @doc Unregister a search provider
-spec unregister_provider(provider_id()) -> ok | {error, term()}.
unregister_provider(ProviderId) when is_atom(ProviderId) ->
    gen_server:call(?SERVER, {unregister_provider, ProviderId}).

%% @doc List all registered providers
-spec list_providers() -> [{provider_id(), provider_module()}].
list_providers() ->
    gen_server:call(?SERVER, list_providers).

%% @doc Clear the search cache
-spec clear_cache() -> ok.
clear_cache() ->
    gen_server:call(?SERVER, clear_cache).

%% @doc Get cache statistics
-spec get_cache_stats() -> map().
get_cache_stats() ->
    gen_server:call(?SERVER, get_cache_stats).

%% @doc Get advisor service status
-spec get_status() -> {ok, map()} | {error, term()}.
get_status() ->
    gen_server:call(?SERVER, get_status).

%% @doc Health check
-spec health_check() -> ok | {error, term()}.
health_check() ->
    gen_server:call(?SERVER, health_check, 2000).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init(map()) -> {ok, state()}.
init(Config) ->
    process_flag(trap_exit, true),

    %% Create ETS cache table
    CacheTid = ets:new(?CACHE_TABLE, [
        set,
        public,
        named_table,
        {read_concurrency, true},
        {write_concurrency, auto}
    ]),

    %% Load configuration
    CacheTTL = maps:get(cache_ttl, Config,
                        application:get_env(erlmcp_core, advisor_cache_ttl, 300000)),
    DefaultLimit = maps:get(default_limit, Config,
                            application:get_env(erlmcp_core, advisor_default_limit, 10)),
    MinSimilarity = maps:get(min_similarity, Config,
                             application:get_env(erlmcp_core, advisor_min_similarity, 0.3)),
    SearchTimeout = maps:get(search_timeout, Config,
                             application:get_env(erlmcp_core, advisor_search_timeout, 5000)),
    DiscoveryInterval = maps:get(discovery_interval, Config,
                                  application:get_env(erlmcp_core, advisor_discovery_interval, 60000)),

    %% Initialize state
    State = #state{
        providers = #{},
        cache = CacheTid,
        cache_ttl = CacheTTL,
        default_limit = DefaultLimit,
        default_min_similarity = MinSimilarity,
        search_timeout = SearchTimeout,
        discovery_interval = DiscoveryInterval,
        stats = #{
            searches => 0,
            cache_hits => 0,
            cache_misses => 0,
            provider_errors => 0,
            started_at => erlang:system_time(millisecond)
        }
    },

    %% Register default providers
    State2 = register_default_providers(State),

    %% Start discovery timer
    TimerRef = erlang:send_after(DiscoveryInterval, self(), discovery_tick),
    State3 = State2#state{discovery_timer = TimerRef},

    logger:info("erlmcp_advisor started", #{
        providers => maps:keys(State3#state.providers),
        cache_ttl => CacheTTL,
        discovery_interval => DiscoveryInterval
    }),

    {ok, State3}.

-spec handle_call(term(), {pid(), reference()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({search, Query, Options}, _From, State) ->
    {Result, State2} = do_search(Query, Options, State),
    {reply, Result, State2};

handle_call({recommend, Query, Options}, _From, State) ->
    {Result, State2} = do_recommend(Query, Options, State),
    {reply, Result, State2};

handle_call({register_provider, Id, Module, Config}, _From, State) ->
    case validate_provider(Module) of
        ok ->
            Providers = maps:put(Id, {Module, Config}, State#state.providers),
            logger:info("Registered advisor provider", #{id => Id, module => Module}),
            {reply, ok, State#state{providers = Providers}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({unregister_provider, Id}, _From, State) ->
    case maps:is_key(Id, State#state.providers) of
        true ->
            Providers = maps:remove(Id, State#state.providers),
            logger:info("Unregistered advisor provider", #{id => Id}),
            {reply, ok, State#state{providers = Providers}};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call(list_providers, _From, State) ->
    ProviderList = [{Id, Module} || {Id, {Module, _}} <- maps:to_list(State#state.providers)],
    {reply, ProviderList, State};

handle_call(clear_cache, _From, State) ->
    ets:delete_all_objects(State#state.cache),
    logger:debug("Advisor cache cleared"),
    {reply, ok, State};

handle_call(get_cache_stats, _From, State) ->
    Stats = #{
        size => ets:info(State#state.cache, size),
        memory => ets:info(State#state.cache, memory),
        ttl => State#state.cache_ttl
    },
    {reply, Stats, State};

handle_call(get_status, _From, State) ->
    Status = #{
        providers => maps:keys(State#state.providers),
        provider_count => maps:size(State#state.providers),
        cache_size => ets:info(State#state.cache, size),
        cache_memory => ets:info(State#state.cache, memory),
        cache_ttl => State#state.cache_ttl,
        stats => State#state.stats,
        discovery_interval => State#state.discovery_interval,
        last_discovery => State#state.last_discovery
    },
    {reply, {ok, Status}, State};

handle_call(health_check, _From, State) ->
    %% Simple health check - verify we have providers and cache is accessible
    case maps:size(State#state.providers) > 0 of
        true ->
            case catch ets:info(State#state.cache, size) of
                N when is_integer(N) -> {reply, ok, State};
                _ -> {reply, {error, cache_unavailable}, State}
            end;
        false ->
            {reply, {error, no_providers}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({provider_result, ProviderId, Results}, State) ->
    %% Handle async provider results (for future use)
    logger:debug("Received provider results", #{
        provider => ProviderId,
        count => length(Results)
    }),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(discovery_tick, State) ->
    %% Perform periodic discovery from registry
    State2 = perform_discovery(State),

    %% Clean expired cache entries
    State3 = clean_expired_cache(State2),

    %% Schedule next tick
    TimerRef = erlang:send_after(State3#state.discovery_interval, self(), discovery_tick),
    {noreply, State3#state{
        discovery_timer = TimerRef,
        last_discovery = erlang:system_time(millisecond)
    }};

handle_info({'EXIT', Pid, Reason}, State) ->
    logger:warning("Advisor received EXIT", #{pid => Pid, reason => Reason}),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(Reason, State) ->
    %% Cancel discovery timer
    case State#state.discovery_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,

    %% Clean up ETS table
    catch ets:delete(State#state.cache),

    logger:info("erlmcp_advisor terminating", #{reason => Reason}),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Register default providers
-spec register_default_providers(state()) -> state().
register_default_providers(State) ->
    %% Register the registry provider (discovers local MCP servers)
    Providers = #{
        registry => {erlmcp_advisor_provider_registry, #{}},
        offline => {erlmcp_advisor_provider_offline, #{}}
    },
    State#state{providers = Providers}.

%% @private Validate a provider module
-spec validate_provider(module()) -> ok | {error, term()}.
validate_provider(Module) ->
    RequiredExports = [
        {search, 2},
        {info, 0}
    ],
    case code:ensure_loaded(Module) of
        {module, Module} ->
            Exports = Module:module_info(exports),
            case lists:all(fun(E) -> lists:member(E, Exports) end, RequiredExports) of
                true -> ok;
                false -> {error, {missing_exports, RequiredExports}}
            end;
        {error, Reason} ->
            {error, {module_load_failed, Reason}}
    end.

%% @private Execute search across providers
-spec do_search(binary(), search_options(), state()) ->
    {{ok, [search_result()]} | {error, term()}, state()}.
do_search(Query, Options, State) ->
    %% Check cache first
    CacheKey = make_cache_key(Query, Options),
    case lookup_cache(CacheKey, State) of
        {ok, CachedResults} ->
            Stats = maps:update_with(cache_hits, fun(V) -> V + 1 end,
                                     State#state.stats),
            {{ok, CachedResults}, State#state{stats = Stats}};
        miss ->
            %% Execute search across providers
            Stats = maps:update_with(cache_misses, fun(V) -> V + 1 end,
                                     State#state.stats),
            Stats2 = maps:update_with(searches, fun(V) -> V + 1 end, Stats),
            State2 = State#state{stats = Stats2},

            {Results, State3} = execute_search(Query, Options, State2),

            %% Cache results
            case Results of
                {ok, SearchResults} ->
                    store_cache(CacheKey, SearchResults, State3),
                    {Results, State3};
                _ ->
                    {Results, State3}
            end
    end.

%% @private Execute search across all or specified providers
-spec execute_search(binary(), search_options(), state()) ->
    {{ok, [search_result()]} | {error, term()}, state()}.
execute_search(Query, Options, State) ->
    Limit = maps:get(limit, Options, State#state.default_limit),
    MinSimilarity = maps:get(min_similarity, Options, State#state.default_min_similarity),
    Timeout = maps:get(timeout, Options, State#state.search_timeout),

    %% Determine which providers to use
    ProviderIds = case maps:get(providers, Options, undefined) of
        undefined -> maps:keys(State#state.providers);
        Ids when is_list(Ids) -> Ids
    end,

    %% Execute searches in parallel
    Providers = [{Id, maps:get(Id, State#state.providers)}
                 || Id <- ProviderIds,
                    maps:is_key(Id, State#state.providers)],

    Self = self(),
    Ref = make_ref(),

    %% Spawn search workers
    Pids = [spawn_link(fun() ->
        Result = try
            {Module, Config} = ProviderSpec,
            case Module:search(Query, #{
                limit => Limit,
                min_similarity => MinSimilarity,
                config => Config
            }) of
                {ok, Results} ->
                    %% Tag results with provider ID
                    TaggedResults = [R#{provider => ProviderId} || R <- Results],
                    {ok, TaggedResults};
                Error ->
                    Error
            end
        catch
            Type:Error:Stack ->
                logger:error("Provider search failed", #{
                    provider => ProviderId,
                    type => Type,
                    error => Error,
                    stack => Stack
                }),
                {error, {provider_error, ProviderId, Error}}
        end,
        Self ! {Ref, ProviderId, Result}
    end) || {ProviderId, ProviderSpec} <- Providers],

    %% Collect results with timeout
    {AllResults, Errors} = collect_results(Ref, length(Pids), Timeout, [], []),

    %% Update error stats
    State2 = case Errors of
        [] -> State;
        _ ->
            Stats = maps:update_with(provider_errors,
                                     fun(V) -> V + length(Errors) end,
                                     State#state.stats),
            State#state{stats = Stats}
    end,

    %% Merge and sort results by similarity
    MergedResults = merge_results(AllResults, Limit, MinSimilarity),

    {{ok, MergedResults}, State2}.

%% @private Collect results from provider workers
-spec collect_results(reference(), non_neg_integer(), pos_integer(),
                      [[search_result()]], [term()]) ->
    {[[search_result()]], [term()]}.
collect_results(_Ref, 0, _Timeout, Results, Errors) ->
    {Results, Errors};
collect_results(Ref, Remaining, Timeout, Results, Errors) ->
    receive
        {Ref, _ProviderId, {ok, ProviderResults}} ->
            collect_results(Ref, Remaining - 1, Timeout,
                           [ProviderResults | Results], Errors);
        {Ref, ProviderId, {error, Reason}} ->
            collect_results(Ref, Remaining - 1, Timeout,
                           Results, [{ProviderId, Reason} | Errors])
    after Timeout ->
        logger:warning("Provider search timeout", #{remaining => Remaining}),
        {Results, [{timeout, Remaining} | Errors]}
    end.

%% @private Merge and sort results from multiple providers
-spec merge_results([[search_result()]], pos_integer(), float()) -> [search_result()].
merge_results(ResultLists, Limit, MinSimilarity) ->
    %% Flatten all results
    AllResults = lists:flatten(ResultLists),

    %% Filter by minimum similarity
    Filtered = [R || R <- AllResults,
                     maps:get(similarity, R, 0.0) >= MinSimilarity],

    %% Sort by similarity descending
    Sorted = lists:sort(fun(A, B) ->
        maps:get(similarity, A, 0.0) >= maps:get(similarity, B, 0.0)
    end, Filtered),

    %% Deduplicate by ID (keep highest similarity)
    Deduped = deduplicate_by_id(Sorted),

    %% Take top N
    lists:sublist(Deduped, Limit).

%% @private Remove duplicates keeping first occurrence (highest similarity)
-spec deduplicate_by_id([search_result()]) -> [search_result()].
deduplicate_by_id(Results) ->
    {Deduped, _Seen} = lists:foldl(fun(R, {Acc, Seen}) ->
        Id = maps:get(id, R, maps:get(title, R, <<>>)),
        case sets:is_element(Id, Seen) of
            true -> {Acc, Seen};
            false -> {[R | Acc], sets:add_element(Id, Seen)}
        end
    end, {[], sets:new()}, Results),
    lists:reverse(Deduped).

%% @private Execute recommendation
-spec do_recommend(binary(), search_options(), state()) ->
    {{ok, recommendation()} | {error, term()}, state()}.
do_recommend(Query, Options, State) ->
    %% Get search results first
    Options2 = Options#{limit => 5},  %% Get top 5 candidates
    case do_search(Query, Options2, State) of
        {{ok, []}, State2} ->
            {{error, no_matching_servers}, State2};
        {{ok, [TopResult | _Rest] = Results}, State2} ->
            %% Build recommendation from top result
            Recommendation = #{
                server => TopResult,
                confidence => maps:get(similarity, TopResult, 0.0),
                reasons => generate_reasons(TopResult, Query, Results)
            },
            {{ok, Recommendation}, State2};
        {Error, State2} ->
            {Error, State2}
    end.

%% @private Generate recommendation reasons
-spec generate_reasons(search_result(), binary(), [search_result()]) -> [binary()].
generate_reasons(TopResult, Query, AllResults) ->
    Reasons = [],

    %% High similarity reason
    Reasons2 = case maps:get(similarity, TopResult, 0.0) of
        Sim when Sim >= 0.9 ->
            [<<"Excellent match with your query">> | Reasons];
        Sim when Sim >= 0.7 ->
            [<<"Good match with your query">> | Reasons];
        _ ->
            Reasons
    end,

    %% Description match reason
    Reasons3 = case maps:get(description, TopResult, <<>>) of
        Desc when byte_size(Desc) > 0 ->
            case binary:match(string:lowercase(Desc), string:lowercase(Query)) of
                nomatch -> Reasons2;
                _ -> [<<"Description matches your search terms">> | Reasons2]
            end;
        _ ->
            Reasons2
    end,

    %% Popularity reason (if multiple results, being top is significant)
    Reasons4 = case length(AllResults) of
        N when N > 3 ->
            [<<"Top ranked among ", (integer_to_binary(N))/binary, " candidates">> | Reasons3];
        _ ->
            Reasons3
    end,

    lists:reverse(Reasons4).

%% @private Create cache key
-spec make_cache_key(binary(), search_options()) -> binary().
make_cache_key(Query, Options) ->
    %% Include relevant options in cache key
    Limit = maps:get(limit, Options, 10),
    MinSim = maps:get(min_similarity, Options, 0.3),
    Providers = maps:get(providers, Options, all),
    KeyData = term_to_binary({Query, Limit, MinSim, Providers}),
    crypto:hash(sha256, KeyData).

%% @private Lookup in cache
-spec lookup_cache(binary(), state()) -> {ok, [search_result()]} | miss.
lookup_cache(Key, State) ->
    case ets:lookup(State#state.cache, Key) of
        [{Key, Results, Timestamp}] ->
            Now = erlang:system_time(millisecond),
            case Now - Timestamp < State#state.cache_ttl of
                true -> {ok, Results};
                false ->
                    ets:delete(State#state.cache, Key),
                    miss
            end;
        [] ->
            miss
    end.

%% @private Store in cache
-spec store_cache(binary(), [search_result()], state()) -> true.
store_cache(Key, Results, State) ->
    Timestamp = erlang:system_time(millisecond),
    ets:insert(State#state.cache, {Key, Results, Timestamp}).

%% @private Clean expired cache entries
-spec clean_expired_cache(state()) -> state().
clean_expired_cache(State) ->
    Now = erlang:system_time(millisecond),
    TTL = State#state.cache_ttl,

    %% Use match spec for efficient filtering
    MatchSpec = [{{'$1', '$2', '$3'},
                  [{'<', '$3', Now - TTL}],
                  ['$1']}],
    ExpiredKeys = ets:select(State#state.cache, MatchSpec),

    %% Delete expired entries
    lists:foreach(fun(Key) ->
        ets:delete(State#state.cache, Key)
    end, ExpiredKeys),

    case ExpiredKeys of
        [] -> ok;
        _ -> logger:debug("Cleaned expired cache entries", #{count => length(ExpiredKeys)})
    end,

    State.

%% @private Perform discovery from local registry
-spec perform_discovery(state()) -> state().
perform_discovery(State) ->
    %% Discover MCP servers from the local registry
    try
        Servers = erlmcp_registry:list_servers(local),
        logger:debug("Discovery found servers", #{count => length(Servers)}),
        State
    catch
        _:Reason ->
            logger:warning("Discovery failed", #{reason => Reason}),
            State
    end.
