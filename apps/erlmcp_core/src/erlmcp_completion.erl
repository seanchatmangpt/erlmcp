-module(erlmcp_completion).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    start_link/1,
    complete/3,
    complete/4,
    add_completion_handler/3,
    add_completion_handler/4,
    remove_completion_handler/2,
    stop/1,
    jaro_winkler_similarity/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type client() :: pid().
-type completion_ref() :: binary().
-type argument() :: #{
    name := binary(),
    value => binary() | undefined
}.
-type completion_item() :: #{
    value := binary(),
    label => binary()
}.
-type completion_result() :: #{
    completions := [completion_item()],
    hasMore := boolean(),
    total => non_neg_integer()
}.
-type completion_handler() :: fun((completion_ref(), argument()) -> {ok, [completion_item()]} | {error, term()}).
-type rate_limit_key() :: {client(), completion_ref()}.

-record(state, {
    handlers :: #{completion_ref() => completion_handler()},
    rate_limits :: #{rate_limit_key() => {integer(), integer()}},  % {count, window_start}
    cache :: ets:tid(),
    cache_ttl :: pos_integer(),
    max_results :: pos_integer(),
    rate_limit :: pos_integer(),  % requests per second
    ranking_threshold :: float()
}).

%% Completion error codes (MCP 2025-11-25)
-define(MCP_ERROR_COMPLETION_REF_NOT_FOUND, -32102).
-define(MCP_ERROR_COMPLETION_INVALID_ARGUMENT, -32103).
-define(MCP_ERROR_COMPLETION_HANDLER_FAILED, -32104).
-define(MCP_ERROR_COMPLETION_RATE_LIMITED, -32101).

-define(DEFAULT_CACHE_TTL, 3600).  % 1 hour
-define(DEFAULT_MAX_RESULTS, 10).
-define(DEFAULT_RATE_LIMIT, 10).  % 10 req/sec
-define(DEFAULT_RANKING_THRESHOLD, 0.7).
-define(RATE_LIMIT_WINDOW, 1000).  % 1 second window

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

-spec complete(client(), completion_ref(), argument()) -> {ok, completion_result()} | {error, term()}.
complete(Client, Ref, Argument) ->
    complete(Client, Ref, Argument, 5000).

-spec complete(client(), completion_ref(), argument(), timeout()) -> {ok, completion_result()} | {error, term()}.
complete(Client, Ref, Argument, Timeout) when is_pid(Client), is_binary(Ref), is_map(Argument) ->
    gen_server:call(Client, {complete, Ref, Argument}, Timeout).

-spec add_completion_handler(pid(), completion_ref(), completion_handler()) -> ok | {error, term()}.
add_completion_handler(Server, Ref, Handler) ->
    add_completion_handler(Server, Ref, Handler, undefined).

-spec add_completion_handler(pid(), completion_ref(), completion_handler(), atom() | undefined) -> ok | {error, term()}.
add_completion_handler(Server, Ref, Handler, Type) when is_pid(Server), is_binary(Ref) ->
    gen_server:call(Server, {add_completion_handler, Ref, Handler, Type}).

-spec remove_completion_handler(pid(), completion_ref()) -> ok | {error, term()}.
remove_completion_handler(Server, Ref) when is_pid(Server), is_binary(Ref) ->
    gen_server:call(Server, {remove_completion_handler, Ref}).

-spec stop(pid()) -> ok.
stop(Server) when is_pid(Server) ->
    gen_server:stop(Server).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, #state{}}.
init(Options) ->
    CacheTTL = maps:get(cache_ttl, Options, ?DEFAULT_CACHE_TTL),
    MaxResults = maps:get(max_results, Options, ?DEFAULT_MAX_RESULTS),
    RateLimit = maps:get(rate_limit, Options, ?DEFAULT_RATE_LIMIT),
    RankingThreshold = maps:get(ranking_threshold, Options, ?DEFAULT_RANKING_THRESHOLD),

    Cache = ets:new(completion_cache, [
        set,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),

    State = #state{
        handlers = #{},
        rate_limits = #{},
        cache = Cache,
        cache_ttl = CacheTTL,
        max_results = MaxResults,
        rate_limit = RateLimit,
        ranking_threshold = RankingThreshold
    },
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {noreply, #state{}}.
handle_call({complete, Ref, Argument}, From, State) ->
    Client = element(1, From),
    case check_rate_limit(Client, Ref, State) of
        {ok, NewRateLimits} ->
            case do_complete(Ref, Argument, State) of
                {ok, Result} ->
                    {reply, {ok, Result}, State#state{rate_limits = NewRateLimits}};
                {error, _Reason} = Error ->
                    {reply, Error, State#state{rate_limits = NewRateLimits}}
            end;
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({add_completion_handler, Ref, Handler, _Type}, _From, State) ->
    case validate_completion_ref(Ref) of
        ok ->
            NewHandlers = maps:put(Ref, Handler, State#state.handlers),
            {reply, ok, State#state{handlers = NewHandlers}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({remove_completion_handler, Ref}, _From, State) ->
    NewHandlers = maps:remove(Ref, State#state.handlers),
    {reply, ok, State#state{handlers = NewHandlers}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    ets:delete(State#state.cache),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Rate limiting check (token bucket algorithm)
-spec check_rate_limit(client(), completion_ref(), #state{}) ->
    {ok, #{rate_limit_key() => {integer(), integer()}}} | {error, term()}.
check_rate_limit(Client, Ref, State) ->
    Key = {Client, Ref},
    Now = erlang:system_time(millisecond),
    WindowStart = Now - ?RATE_LIMIT_WINDOW,

    case maps:get(Key, State#state.rate_limits, {0, 0}) of
        {Count, StartTime} when StartTime >= WindowStart, Count >= State#state.rate_limit ->
            {error, {completion_rate_limited, ?MCP_ERROR_COMPLETION_RATE_LIMITED,
                     <<"Rate limit exceeded">>}};
        {Count, StartTime} when StartTime >= WindowStart ->
            {ok, maps:put(Key, {Count + 1, StartTime}, State#state.rate_limits)};
        _ ->
            {ok, maps:put(Key, {1, Now}, State#state.rate_limits)}
    end.

%% Execute completion with caching
-spec do_complete(completion_ref(), argument(), #state{}) ->
    {ok, completion_result()} | {error, term()}.
do_complete(Ref, Argument, State) ->
    CacheKey = {Ref, Argument},

    case ets:lookup(State#state.cache, CacheKey) of
        [{CacheKey, Result, Expiry}] ->
            Now = erlang:system_time(second),
            case Expiry > Now of
                true -> {ok, Result};
                false ->
                    % Cache expired, continue to handler invocation
                    case maps:get(Ref, State#state.handlers, undefined) of
                        undefined ->
                            {error, {completion_ref_not_found, ?MCP_ERROR_COMPLETION_REF_NOT_FOUND,
                                     <<"Completion reference not found">>}};
                        Handler ->
                            invoke_completion_handler(Ref, Argument, Handler, State)
                    end
            end;
        _ ->
            case maps:get(Ref, State#state.handlers, undefined) of
                undefined ->
                    {error, {completion_ref_not_found, ?MCP_ERROR_COMPLETION_REF_NOT_FOUND,
                             <<"Completion reference not found">>}};
                Handler ->
                    invoke_completion_handler(Ref, Argument, Handler, State)
            end
    end.

%% Invoke completion handler with error handling
-spec invoke_completion_handler(completion_ref(), argument(), completion_handler(), #state{}) ->
    {ok, completion_result()} | {error, term()}.
invoke_completion_handler(Ref, Argument, Handler, State) ->
    try
        case Handler(Ref, Argument) of
            {ok, Items} when is_list(Items) ->
                RankedItems = rank_completions(Argument, Items, State),
                Result = #{
                    completions => RankedItems,
                    hasMore => length(RankedItems) >= State#state.max_results,
                    total => length(RankedItems)
                },
                cache_result({Ref, Argument}, Result, State),
                {ok, Result};
            {error, _Reason} ->
                {error, {completion_handler_failed, ?MCP_ERROR_COMPLETION_HANDLER_FAILED,
                         <<"Completion handler failed">>}};
            _Invalid ->
                {error, {completion_handler_invalid_response, ?MCP_ERROR_COMPLETION_INVALID_ARGUMENT,
                         <<"Invalid handler response">>}}
        end
    catch
        _:Reason:Stack ->
            logger:error("Completion handler crashed: ~p~nStack: ~p", [Reason, Stack]),
            {error, {completion_handler_crashed, ?MCP_ERROR_COMPLETION_HANDLER_FAILED,
                     <<"Completion handler crashed">>}}
    end.

%% Rank completion items using Jaro-Winkler similarity
-spec rank_completions(argument(), [completion_item()], #state{}) -> [completion_item()].
rank_completions(Argument, Items, State) ->
    TargetValue = maps:get(value, Argument, <<>>),

    ScoredItems = lists:map(fun(Item) ->
        Value = maps:get(value, Item, <<>>),
        Score = jaro_winkler_similarity(TargetValue, Value),
        Item#{score => Score}
    end, Items),

    FilteredItems = lists:filter(fun(Item) ->
        maps:get(score, Item, 0.0) >= State#state.ranking_threshold
    end, ScoredItems),

    SortedItems = lists:sort(fun(A, B) ->
        maps:get(score, A, 0.0) >= maps:get(score, B, 0.0)
    end, FilteredItems),

    lists:sublist(SortedItems, State#state.max_results).

%% Jaro-Winkler similarity algorithm
-spec jaro_winkler_similarity(binary(), binary()) -> float().
jaro_winkler_similarity(S1, S2) ->
    Len1 = byte_size(S1),
    Len2 = byte_size(S2),

    if
        Len1 =:= 0 andalso Len2 =:= 0 -> 1.0;
        Len1 =:= 0 orelse Len2 =:= 0 -> 0.0;
        true ->
            Jaro = jaro_similarity(S1, S2),
            PrefixLength = common_prefix_length(S1, S2, 4),
            Jaro + (PrefixLength * 0.1 * (1.0 - Jaro))
    end.

%% Jaro similarity calculation
-spec jaro_similarity(binary(), binary()) -> float().
jaro_similarity(S1, S2) ->
    Len1 = byte_size(S1),
    Len2 = byte_size(S2),
    MatchDistance = max(Len1, Len2) div 2 - 1,

    S1Chars = binary_to_list(S1),
    S2Chars = binary_to_list(S2),

    {Matches1, Matches2} = find_matches(S1Chars, S2Chars, MatchDistance),
    Matches = length(Matches1),

    if
        Matches =:= 0 -> 0.0;
        true ->
            Transpositions = count_transpositions(Matches1, Matches2),
            (Matches / Len1 + Matches / Len2 + (Matches - Transpositions / 2) / Matches) / 3.0
    end.

%% Find matching characters within distance
-spec find_matches(list(), list(), integer()) -> {list(), list()}.
find_matches(S1, S2, Distance) ->
    find_matches(S1, S2, Distance, [], []).

find_matches([], _S2, _Distance, Acc1, Acc2) ->
    {lists:reverse(Acc1), lists:reverse(Acc2)};
find_matches([C1 | Rest1], S2, Distance, Acc1, Acc2) ->
    case find_match(C1, S2, Distance, 0) of
        {found, Index} ->
            {Before, [C2 | After]} = lists:split(Index, S2),
            find_matches(Rest1, Before ++ After, Distance, [C1 | Acc1], [C2 | Acc2]);
        not_found ->
            find_matches(Rest1, S2, Distance, Acc1, Acc2)
    end.

find_match(_C1, [], _Distance, _Index) ->
    not_found;
find_match(_C1, [_C2 | _Rest], Distance, Index) when Index > Distance ->
    not_found;
find_match(C1, [C2 | Rest], Distance, Index) ->
    if
        C1 =:= C2 -> {found, Index};
        true -> find_match(C1, Rest, Distance, Index + 1)
    end.

%% Count transpositions between match lists
-spec count_transpositions(list(), list()) -> non_neg_integer().
count_transpositions(List1, List2) ->
    count_transpositions(List1, List2, 0).

count_transpositions([], [], Acc) ->
    Acc;
count_transpositions([C1 | Rest1], [C2 | Rest2], Acc) when C1 =/= C2 ->
    count_transpositions(Rest1, Rest2, Acc + 1);
count_transpositions([_C1 | Rest1], [_C2 | Rest2], Acc) ->
    count_transpositions(Rest1, Rest2, Acc).

%% Common prefix length (max 4)
-spec common_prefix_length(binary(), binary(), integer()) -> float().
common_prefix_length(<<>>, _, _Max) -> 0.0;
common_prefix_length(_, <<>>, _Max) -> 0.0;
common_prefix_length(<<C1, Rest1/binary>>, <<C2, Rest2/binary>>, Max) when C1 =:= C2, Max > 0 ->
    1.0 + common_prefix_length(Rest1, Rest2, Max - 1);
common_prefix_length(_, _, _) -> 0.0.

%% Validate completion reference format
-spec validate_completion_ref(completion_ref()) -> ok | {error, term()}.
validate_completion_ref(Ref) when is_binary(Ref), byte_size(Ref) > 0 ->
    case binary:match(Ref, <<"\n">>) of
        nomatch -> ok;
        _ -> {error, {invalid_completion_ref, ?MCP_ERROR_COMPLETION_INVALID_ARGUMENT,
                      <<"Completion reference contains newline">>}}
    end;
validate_completion_ref(_) ->
    {error, {invalid_completion_ref, ?MCP_ERROR_COMPLETION_INVALID_ARGUMENT,
             <<"Completion reference must be a non-empty binary">>}}.

%% Cache completion result
-spec cache_result({completion_ref(), argument()}, completion_result(), #state{}) -> ok.
cache_result(Key, Result, State) ->
    Expiry = erlang:system_time(second) + State#state.cache_ttl,
    ets:insert(State#state.cache, {Key, Result, Expiry}),
    ok.
