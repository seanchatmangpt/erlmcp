-module(erlmcp_completion).

-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([start_link/0, start_link/1, complete/3, complete/4, stream_completion/3,
         stream_completion/4, cancel_completion/2, get_cached_completion/3,
         add_completion_handler/3, add_completion_handler/4, add_completion_handler/5,
         remove_completion_handler/2, stop/1, jaro_winkler_similarity/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Stream process exports
-export([stream_loop/7]).

%%====================================================================
%% Types
%%====================================================================

-type client() :: pid().
-type completion_ref() :: binary() | #{type := binary(), name := binary()}.
-type argument() :: #{name := binary(), value => binary() | undefined}.
-type completion_context() :: #{arguments => map(), type => binary()}.

                      %% <<"tool">>, <<"resource">>, <<"prompt">>

-type completion_item() :: #{value := binary(), label => binary()}.
-type completion_result() ::
    #{completions := [completion_item()],
      hasMore := boolean(),
      total => non_neg_integer()}.
-type completion_handler() ::
    fun((completion_ref(), argument(), completion_context()) ->
            {ok, [completion_item()]} | {error, term()}).
-type rate_limit_key() :: {client(), completion_ref()}.
-type completion_id() :: binary().
-type stream_state() :: pending | streaming | completed | cancelled.
-type stream_chunk() :: {chunk, binary()} | {done, completion_result()}.

-record(completion_stream,
        {id :: completion_id(),
         client :: client(),
         ref :: completion_ref(),
         argument :: argument(),
         context :: completion_context(),
         status :: stream_state(),
         chunks :: queue:queue(binary()),
         handler :: completion_handler(),
         created_at :: integer()}).
-record(state,
        {handlers :: #{completion_ref() => {completion_handler(), binary()}},  % {handler, type}
         rate_limits :: #{rate_limit_key() => {integer(), integer()}},  % {count, window_start}
         cache :: ets:tid(),
         cache_ttl :: pos_integer(),
         cache_max_size :: pos_integer(),
         cache_current_size :: non_neg_integer(),
         max_results :: pos_integer(),
         rate_limit :: pos_integer(),  % requests per second
         ranking_threshold :: float(),
         streams :: #{completion_id() => #completion_stream{}}}).

%% Completion error codes (MCP 2025-11-25)
-define(MCP_ERROR_COMPLETION_REF_NOT_FOUND, -32102).
-define(MCP_ERROR_COMPLETION_INVALID_ARGUMENT, -32103).
-define(MCP_ERROR_COMPLETION_HANDLER_FAILED, -32104).
-define(MCP_ERROR_COMPLETION_RATE_LIMITED, -32101).
-define(DEFAULT_CACHE_TTL, 3600).  % 1 hour
-define(DEFAULT_CACHE_MAX_SIZE, 1000).  % Max 1000 cached results
-define(DEFAULT_MAX_RESULTS, 10).
-define(DEFAULT_RATE_LIMIT, 10).  % 10 req/sec
-define(DEFAULT_RANKING_THRESHOLD, 0.7).
-define(RATE_LIMIT_WINDOW, 1000).  % 1 second window
-define(STREAM_CHUNK_SIZE, 1024).  % 1KB chunks for streaming

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

-spec complete(client(), completion_ref(), argument()) ->
                  {ok, completion_result()} | {error, term()}.
complete(Client, Ref, Argument) ->
    complete(Client, Ref, Argument, #{}).

-spec complete(client(), completion_ref(), argument(), completion_context() | timeout()) ->
                  {ok, completion_result()} | {error, term()}.
complete(Client, Ref, Argument, Context) when is_map(Context) ->
    complete(Client, Ref, Argument, Context, 5000);
complete(Client, Ref, Argument, Timeout) when is_integer(Timeout) ->
    complete(Client, Ref, Argument, #{}, Timeout).

-spec complete(client(), completion_ref(), argument(), completion_context(), timeout()) ->
                  {ok, completion_result()} | {error, term()}.
complete(Client, Ref, Argument, Context, Timeout)
    when is_pid(Client), is_map(Argument), is_map(Context) ->
    gen_server:call(Client, {complete, Ref, Argument, Context}, Timeout).

%% Streaming completion API
-spec stream_completion(client(), completion_ref(), argument()) ->
                           {ok, completion_id(), pid()} | {error, term()}.
stream_completion(Client, Ref, Argument) ->
    stream_completion(Client, Ref, Argument, #{}).

-spec stream_completion(client(), completion_ref(), argument(), completion_context()) ->
                           {ok, completion_id(), pid()} | {error, term()}.
stream_completion(Client, Ref, Argument, Context)
    when is_pid(Client), is_map(Argument), is_map(Context) ->
    gen_server:call(Client, {stream_completion, Ref, Argument, Context}, 5000).

-spec cancel_completion(client(), completion_id()) -> ok | {error, term()}.
cancel_completion(Client, CompletionId) when is_pid(Client), is_binary(CompletionId) ->
    gen_server:call(Client, {cancel_completion, CompletionId}, 5000).

-spec get_cached_completion(client(), completion_ref(), argument()) ->
                               {ok, completion_result()} | not_found | {error, term()}.
get_cached_completion(Client, Ref, Argument) when is_pid(Client), is_map(Argument) ->
    gen_server:call(Client, {get_cached, Ref, Argument}, 5000).

-spec add_completion_handler(pid(), completion_ref(), completion_handler()) -> ok | {error, term()}.
add_completion_handler(Server, Ref, Handler) ->
    add_completion_handler(Server, Ref, Handler, <<"general">>).

%% Backward compatibility - converts atom to binary
-spec add_completion_handler(pid(),
                             completion_ref(),
                             completion_handler(),
                             binary() | atom() | undefined) ->
                                ok | {error, term()}.
add_completion_handler(Server, Ref, Handler, Type) when is_pid(Server), is_binary(Ref) ->
    TypeBin =
        case Type of
            undefined ->
                <<"general">>;
            Atom when is_atom(Atom) ->
                atom_to_binary(Atom, utf8);
            Binary when is_binary(Binary) ->
                Binary
        end,
    gen_server:call(Server, {add_completion_handler, Ref, Handler, TypeBin}).

-spec add_completion_handler(pid(),
                             completion_ref(),
                             completion_handler(),
                             binary(),
                             completion_context()) ->
                                ok | {error, term()}.
add_completion_handler(Server, Ref, Handler, Type, _Context)
    when is_pid(Server), is_binary(Ref), is_binary(Type) ->
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
    CacheMaxSize = maps:get(cache_max_size, Options, ?DEFAULT_CACHE_MAX_SIZE),
    MaxResults = maps:get(max_results, Options, ?DEFAULT_MAX_RESULTS),
    RateLimit = maps:get(rate_limit, Options, ?DEFAULT_RATE_LIMIT),
    RankingThreshold = maps:get(ranking_threshold, Options, ?DEFAULT_RANKING_THRESHOLD),

    Cache =
        ets:new(completion_cache,
                [set, public, {read_concurrency, true}, {write_concurrency, true}]),

    State =
        #state{handlers = #{},
               rate_limits = #{},
               cache = Cache,
               cache_ttl = CacheTTL,
               cache_max_size = CacheMaxSize,
               cache_current_size = 0,
               max_results = MaxResults,
               rate_limit = RateLimit,
               ranking_threshold = RankingThreshold,
               streams = #{}},
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
                     {reply, term(), #state{}} | {noreply, #state{}}.
handle_call({complete, Ref, Argument, Context}, From, State) ->
    Client = element(1, From),
    case check_rate_limit(Client, Ref, State) of
        {ok, NewRateLimits} ->
            case do_complete(Ref, Argument, Context, State) of
                {ok, Result} ->
                    {reply, {ok, Result}, State#state{rate_limits = NewRateLimits}};
                {error, _Reason} = Error ->
                    {reply, Error, State#state{rate_limits = NewRateLimits}}
            end;
        {error, _} = Error ->
            {reply, Error, State}
    end;
handle_call({stream_completion, Ref, Argument, Context}, _From, State) ->
    case do_stream_completion(Ref, Argument, Context, State) of
        {ok, CompletionId, StreamPid} ->
            NewStreams =
                maps:put(CompletionId,
                         #completion_stream{id = CompletionId,
                                            client = self(),
                                            ref = Ref,
                                            argument = Argument,
                                            context = Context,
                                            status = streaming,
                                            chunks = queue:new(),
                                            handler =
                                                maps:get(Ref, State#state.handlers, undefined),
                                            created_at = erlang:system_time(millisecond)},
                         State#state.streams),
            {reply, {ok, CompletionId, StreamPid}, State#state{streams = NewStreams}};
        {error, _Reason} = Error ->
            {reply, Error, State}
    end;
handle_call({cancel_completion, CompletionId}, _From, State) ->
    case maps:get(CompletionId, State#state.streams, undefined) of
        undefined ->
            {reply, {error, {completion_not_found, -32102, <<"Completion not found">>}}, State};
        Stream ->
            Stream#completion_stream.client ! {cancel, CompletionId},
            NewStreams = maps:remove(CompletionId, State#state.streams),
            {reply, ok, State#state{streams = NewStreams}}
    end;
handle_call({get_cached, Ref, Argument}, _From, State) ->
    CacheKey = {Ref, Argument},
    case ets:lookup(State#state.cache, CacheKey) of
        [{CacheKey, Result, Expiry}] ->
            Now = erlang:system_time(second),
            case Expiry > Now of
                true ->
                    {reply, {ok, Result}, State};
                false ->
                    {reply, not_found, State}
            end;
        _ ->
            {reply, not_found, State}
    end;
handle_call({add_completion_handler, Ref, Handler, Type}, _From, State) ->
    case validate_completion_ref(Ref) of
        ok ->
            NewHandlers = maps:put(Ref, {Handler, Type}, State#state.handlers),
            % Invalidate cache for this ref when handler changes
            invalidate_cache_for_ref(Ref, State#state.cache),
            {reply, ok, State#state{handlers = NewHandlers}};
        {error, _} = Error ->
            {reply, Error, State}
    end;
handle_call({remove_completion_handler, Ref}, _From, State) ->
    NewHandlers = maps:remove(Ref, State#state.handlers),
    % Invalidate cache for this ref when handler removed
    invalidate_cache_for_ref(Ref, State#state.cache),
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
            {error,
             {completion_rate_limited,
              ?MCP_ERROR_COMPLETION_RATE_LIMITED,
              <<"Rate limit exceeded">>}};
        {Count, StartTime} when StartTime >= WindowStart ->
            {ok, maps:put(Key, {Count + 1, StartTime}, State#state.rate_limits)};
        _ ->
            {ok, maps:put(Key, {1, Now}, State#state.rate_limits)}
    end.

%% Execute completion with caching
-spec do_complete(completion_ref(), argument(), completion_context(), #state{}) ->
                     {ok, completion_result()} | {error, term()}.
do_complete(Ref, Argument, Context, State) ->
    CacheKey = {Ref, Argument, Context},

    case ets:lookup(State#state.cache, CacheKey) of
        [{CacheKey, Result, Expiry}] ->
            Now = erlang:system_time(second),
            case Expiry > Now of
                true ->
                    {ok, Result};
                false ->
                    % Cache expired, continue to handler invocation
                    case maps:get(Ref, State#state.handlers, undefined) of
                        undefined ->
                            {error,
                             {completion_ref_not_found,
                              ?MCP_ERROR_COMPLETION_REF_NOT_FOUND,
                              <<"Completion reference not found">>}};
                        {Handler, _Type} ->
                            invoke_completion_handler(Ref, Argument, Context, Handler, State)
                    end
            end;
        _ ->
            case maps:get(Ref, State#state.handlers, undefined) of
                undefined ->
                    {error,
                     {completion_ref_not_found,
                      ?MCP_ERROR_COMPLETION_REF_NOT_FOUND,
                      <<"Completion reference not found">>}};
                {Handler, _Type} ->
                    invoke_completion_handler(Ref, Argument, Context, Handler, State)
            end
    end.

%% Invoke completion handler with error handling
-spec invoke_completion_handler(completion_ref(),
                                argument(),
                                completion_context(),
                                completion_handler(),
                                #state{}) ->
                                   {ok, completion_result()} | {error, term()}.
invoke_completion_handler(Ref, Argument, Context, Handler, State) ->
    try
        case Handler(Ref, Argument, Context) of
            {ok, Items} when is_list(Items) ->
                RankedItems = rank_completions(Argument, Items, State),
                Result =
                    #{completions => RankedItems,
                      hasMore => length(RankedItems) >= State#state.max_results,
                      total => length(RankedItems)},
                cache_result({Ref, Argument, Context}, Result, State),
                {ok, Result};
            {error, _Reason} ->
                {error,
                 {completion_handler_failed,
                  ?MCP_ERROR_COMPLETION_HANDLER_FAILED,
                  <<"Completion handler failed">>}};
            _Invalid ->
                {error,
                 {completion_handler_invalid_response,
                  ?MCP_ERROR_COMPLETION_INVALID_ARGUMENT,
                  <<"Invalid handler response">>}}
        end
    catch
        _:Reason:Stack ->
            logger:error("Completion handler crashed: ~p~nStack: ~p", [Reason, Stack]),
            {error,
             {completion_handler_crashed,
              ?MCP_ERROR_COMPLETION_HANDLER_FAILED,
              <<"Completion handler crashed">>}}
    end.

%% Rank completion items using Jaro-Winkler similarity
-spec rank_completions(argument(), [completion_item()], #state{}) -> [completion_item()].
rank_completions(Argument, Items, State) ->
    TargetValue = maps:get(value, Argument, <<>>),

    ScoredItems =
        lists:map(fun(Item) ->
                     Value = maps:get(value, Item, <<>>),
                     Score = jaro_winkler_similarity(TargetValue, Value),
                     Item#{score => Score}
                  end,
                  Items),

    FilteredItems =
        lists:filter(fun(Item) -> maps:get(score, Item, 0.0) >= State#state.ranking_threshold end,
                     ScoredItems),

    SortedItems =
        lists:sort(fun(A, B) -> maps:get(score, A, 0.0) >= maps:get(score, B, 0.0) end,
                   FilteredItems),

    lists:sublist(SortedItems, State#state.max_results).

%% Jaro-Winkler similarity algorithm
-spec jaro_winkler_similarity(binary(), binary()) -> float().
jaro_winkler_similarity(S1, S2) ->
    Len1 = byte_size(S1),
    Len2 = byte_size(S2),

    if Len1 =:= 0 andalso Len2 =:= 0 ->
           1.0;
       Len1 =:= 0 orelse Len2 =:= 0 ->
           0.0;
       true ->
           Jaro = jaro_similarity(S1, S2),
           PrefixLength = common_prefix_length(S1, S2, 4),
           Jaro + PrefixLength * 0.1 * (1.0 - Jaro)
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

    if Matches =:= 0 ->
           0.0;
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
    if C1 =:= C2 ->
           {found, Index};
       true ->
           find_match(C1, Rest, Distance, Index + 1)
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
common_prefix_length(<<>>, _, _Max) ->
    0.0;
common_prefix_length(_, <<>>, _Max) ->
    0.0;
common_prefix_length(<<C1, Rest1/binary>>, <<C2, Rest2/binary>>, Max) when C1 =:= C2, Max > 0 ->
    1.0 + common_prefix_length(Rest1, Rest2, Max - 1);
common_prefix_length(_, _, _) ->
    0.0.

%% Validate completion reference format
-spec validate_completion_ref(completion_ref()) -> ok | {error, term()}.
validate_completion_ref(Ref) when is_binary(Ref), byte_size(Ref) > 0 ->
    case binary:match(Ref, <<"\n">>) of
        nomatch ->
            ok;
        _ ->
            {error,
             {invalid_completion_ref,
              ?MCP_ERROR_COMPLETION_INVALID_ARGUMENT,
              <<"Completion reference contains newline">>}}
    end;
validate_completion_ref(_) ->
    {error,
     {invalid_completion_ref,
      ?MCP_ERROR_COMPLETION_INVALID_ARGUMENT,
      <<"Completion reference must be a non-empty binary">>}}.

%% Cache completion result with size limit enforcement
-spec cache_result({completion_ref(), argument(), completion_context()},
                   completion_result(),
                   #state{}) ->
                      ok.
cache_result(Key, Result, State) ->
    % Check cache size limit
    CacheSize = ets:info(State#state.cache, size),
    case CacheSize >= State#state.cache_max_size of
        true ->
            % Evict oldest entries (LRU) - delete 10% of cache
            evict_cache_entries(State#state.cache, trunc(State#state.cache_max_size * 0.1));
        false ->
            ok
    end,

    Expiry = erlang:system_time(second) + State#state.cache_ttl,
    ets:insert(State#state.cache, {Key, Result, Expiry}),
    ok.

%% Evict cache entries (LRU approximation)
-spec evict_cache_entries(ets:tid(), non_neg_integer()) -> ok.
evict_cache_entries(Cache, Count) ->
    % Get all keys and delete first Count entries
    Keys = ets:foldl(fun({Key, _Value, _Expiry}, Acc) -> [Key | Acc] end, [], Cache),
    ToDelete = lists:sublist(Keys, Count),
    lists:foreach(fun(Key) -> ets:delete(Cache, Key) end, ToDelete),
    ok.

%% Invalidate cache for a specific ref
-spec invalidate_cache_for_ref(completion_ref(), ets:tid()) -> ok.
invalidate_cache_for_ref(Ref, Cache) ->
    % Delete all cache entries for this ref
    Pattern = {Ref, '_', '_'},
    ets:match_delete(Cache, Pattern),
    ok.

%% Streaming completion implementation
-spec do_stream_completion(completion_ref(), argument(), completion_context(), #state{}) ->
                              {ok, completion_id(), pid()} | {error, term()}.
do_stream_completion(Ref, Argument, Context, State) ->
    case maps:get(Ref, State#state.handlers, undefined) of
        undefined ->
            {error,
             {completion_ref_not_found,
              ?MCP_ERROR_COMPLETION_REF_NOT_FOUND,
              <<"Completion reference not found">>}};
        {Handler, _Type} ->
            CompletionId = generate_completion_id(),
            Pid = spawn_link(?MODULE,
                             stream_loop,
                             [CompletionId,
                              Ref,
                              Argument,
                              Context,
                              Handler,
                              State#state.max_results,
                              State#state.ranking_threshold]),
            {ok, CompletionId, Pid}
    end.

%% Stream loop - handles streaming completion delivery
-spec stream_loop(completion_id(),
                  completion_ref(),
                  argument(),
                  completion_context(),
                  completion_handler(),
                  pos_integer(),
                  float()) ->
                     ok.
stream_loop(CompletionId, Ref, Argument, Context, Handler, MaxResults, RankingThreshold) ->
    receive
        {cancel, CompletionId} ->
            logger:info("Stream completion cancelled: ~p", [CompletionId]),
            exit(normal);
        {get_chunk, Caller} ->
            % Invoke handler and stream results in chunks
            try Handler(Ref, Argument, Context) of
                {ok, Items} when is_list(Items) ->
                    RankedItems =
                        rank_completions_with_threshold(Argument, Items, RankingThreshold),
                    PaginatedItems = lists:sublist(RankedItems, MaxResults),

                    % Send items in chunks
                    Chunks = chunkify_items(PaginatedItems, ?STREAM_CHUNK_SIZE),
                    send_chunks(Caller, Chunks),

                    % Send final result
                    Result =
                        #{completions => PaginatedItems,
                          hasMore => length(RankedItems) >= MaxResults,
                          total => length(RankedItems)},
                    Caller ! {done, CompletionId, Result},
                    exit(normal);
                {error, Reason} ->
                    Caller ! {error, Reason},
                    exit(normal)
            catch
                _:Error:Stack ->
                    logger:error("Stream handler crashed: ~p~nStack: ~p", [Error, Stack]),
                    Caller
                    ! {error,
                       {completion_handler_crashed,
                        ?MCP_ERROR_COMPLETION_HANDLER_FAILED,
                        <<"Completion handler crashed">>}},
                    exit(normal)
            end
    after 30000 ->  % 30 second timeout
        logger:warning("Stream completion timeout: ~p", [CompletionId]),
        exit(timeout)
    end.

%% Rank completions with custom threshold
-spec rank_completions_with_threshold(argument(), [completion_item()], float()) ->
                                         [completion_item()].
rank_completions_with_threshold(Argument, Items, Threshold) ->
    TargetValue = maps:get(value, Argument, <<>>),

    ScoredItems =
        lists:map(fun(Item) ->
                     Value = maps:get(value, Item, <<>>),
                     Score = jaro_winkler_similarity(TargetValue, Value),
                     Item#{score => Score}
                  end,
                  Items),

    FilteredItems =
        lists:filter(fun(Item) -> maps:get(score, Item, 0.0) >= Threshold end, ScoredItems),

    lists:sort(fun(A, B) -> maps:get(score, A, 0.0) >= maps:get(score, B, 0.0) end, FilteredItems).

%% Chunkify completion items
-spec chunkify_items([completion_item()], pos_integer()) -> [[completion_item()]].
chunkify_items(Items, MaxSize) ->
    chunkify_items(Items, MaxSize, []).

chunkify_items([], _MaxSize, Acc) ->
    lists:reverse(Acc);
chunkify_items(Items, MaxSize, Acc) ->
    {Chunk, Rest} = lists:split(min(MaxSize, length(Items)), Items),
    chunkify_items(Rest, MaxSize, [Chunk | Acc]).

%% Send chunks to caller
-spec send_chunks(pid(), [[completion_item()]]) -> ok.
send_chunks(_Caller, []) ->
    ok;
send_chunks(Caller, [Chunk | Rest]) ->
    Caller ! {chunk, Chunk},
    timer:sleep(10),  % Small delay between chunks
    send_chunks(Caller, Rest).

%% Generate unique completion ID
-spec generate_completion_id() -> completion_id().
generate_completion_id() ->
    Binary = term_to_binary({self(), erlang:unique_integer([positive])}),
    base64:encode(Binary).
