%%%-------------------------------------------------------------------
%%% @doc SwarmFlow Rust NIF - High-Performance Compute Kernels
%%%
%%% This module provides Erlang bindings to Rust NIFs for:
%%% - Conformance scoring (token-based replay)
%%% - Alignment computation (A* search)
%%% - Patch search (candidate generation)
%%% - Event log compression
%%% - SIMD-accelerated vector operations
%%%
%%% The NIF library is loaded from priv/swf_rust_nif.so
%%% @end
%%%-------------------------------------------------------------------
-module(swf_rust_nif).

-export([
    %% Token Replay
    replay_tokens/2,

    %% Alignment
    compute_alignment/3,
    calculate_fitness/2,

    %% Patch Search
    search_patches/3,

    %% Compression
    compress_events/1,
    decompress_events/1,

    %% SIMD Vector Operations
    simd_dot_product/2,
    simd_l2_distance/2,
    simd_cosine_similarity/2,

    %% Utility
    nif_loaded/0,
    benchmark/2
]).

-on_load(init/0).

-define(NIF_NOT_LOADED, erlang:nif_error(nif_not_loaded)).

%% =============================================================================
%% NIF Loading
%% =============================================================================

-spec init() -> ok | {error, term()}.
init() ->
    PrivDir = case code:priv_dir(swarmflow_os) of
        {error, bad_name} ->
            %% Application not started, try relative path
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    filename:join([filename:dirname(Filename), "..", "priv"]);
                _ ->
                    "priv"
            end;
        Dir ->
            Dir
    end,
    SoName = filename:join(PrivDir, "swf_rust_nif"),
    case erlang:load_nif(SoName, 0) of
        ok ->
            ok;
        {error, {reload, _}} ->
            %% Already loaded
            ok;
        {error, Reason} ->
            logger:warning("Failed to load NIF ~s: ~p", [SoName, Reason]),
            {error, Reason}
    end.

%% @doc Check if NIF is loaded
-spec nif_loaded() -> boolean().
nif_loaded() ->
    try
        _ = replay_tokens([], []),
        true
    catch
        error:nif_not_loaded -> false
    end.

%% =============================================================================
%% Token Replay - Fast token replay on marking
%% =============================================================================

%% @doc Replay a sequence of transitions on an initial marking.
%%
%% InitialMarking: list of {PlaceId, TokenCount}
%% Transitions: list of {TransitionId, ConsumedList, ProducedList}
%%   where ConsumedList/ProducedList are [{PlaceId, Count}]
%%
%% Returns:
%%   {ok, #{marking => FinalMarking,
%%          consumed => TotalConsumed,
%%          produced => TotalProduced,
%%          success_count => Count,
%%          failed => FailedTransitionIds}}
%%
-spec replay_tokens(InitialMarking, Transitions) -> Result when
    InitialMarking :: [{PlaceId :: non_neg_integer(), Count :: non_neg_integer()}],
    Transitions :: [{TransitionId :: non_neg_integer(),
                     Consumed :: [{non_neg_integer(), non_neg_integer()}],
                     Produced :: [{non_neg_integer(), non_neg_integer()}]}],
    Result :: {ok, map()} | {error, term()}.
replay_tokens(_InitialMarking, _Transitions) ->
    ?NIF_NOT_LOADED.

%% =============================================================================
%% Alignment - A* search for optimal alignment
%% =============================================================================

%% @doc Compute optimal alignment between trace and model using A* search.
%%
%% Trace: list of ActivityIds (the observed sequence)
%% Model: {Transitions, InitialMarking, FinalMarking}
%%   where Transitions = [{Id, Consumed, Produced, OptionalActivityId}]
%% Costs: {SyncCost, ModelCost, LogCost}
%%
%% Returns:
%%   {ok, #{alignment => [{MoveType, Id}],
%%          cost => TotalCost,
%%          iterations => SearchIterations}}
%%
-spec compute_alignment(Trace, Model, Costs) -> Result when
    Trace :: [ActivityId :: non_neg_integer()],
    Model :: {Transitions :: list(), InitialMarking :: list(), FinalMarking :: list()},
    Costs :: {SyncCost :: non_neg_integer(),
              ModelCost :: non_neg_integer(),
              LogCost :: non_neg_integer()},
    Result :: {ok, map()} | {error, term()}.
compute_alignment(_Trace, _Model, _Costs) ->
    ?NIF_NOT_LOADED.

%% @doc Calculate fitness score from alignment results.
%%
%% Alignment: list of {MoveType, Id} where MoveType is sync|model|log
%% Params: {TotalEvents, TotalTransitions}
%%
%% Returns metrics:
%%   {ok, #{fitness => 0.0..1.0,
%%          precision => 0.0..1.0,
%%          generalization => 0.0..1.0,
%%          simplicity => 0.0..1.0,
%%          f_measure => 0.0..1.0,
%%          sync_moves => Count,
%%          model_moves => Count,
%%          log_moves => Count}}
%%
-spec calculate_fitness(Alignment, Params) -> Result when
    Alignment :: [{MoveType :: binary() | atom(), Id :: non_neg_integer()}],
    Params :: {TotalEvents :: non_neg_integer(), TotalTransitions :: non_neg_integer()},
    Result :: {ok, map()} | {error, term()}.
calculate_fitness(_Alignment, _Params) ->
    ?NIF_NOT_LOADED.

%% =============================================================================
%% Patch Search - Candidate generation and evaluation
%% =============================================================================

%% @doc Search for improvement patches.
%%
%% Model: {Places, Transitions, Arcs}
%%   Places = [PlaceId]
%%   Transitions = [{Id, OptionalActivityId}]
%%   Arcs = [{Source, Target, Type}] where Type is "place_to_trans" | "trans_to_place"
%% Log: [{ActivityId, Frequency}] - activity frequency summary
%% Config: {MaxPatches, MinSupport, BeamWidth}
%%
%% Returns:
%%   {ok, #{patches => [{PatchType, Source, Target, DeltaFitness}],
%%          candidates => CandidateCount}}
%%
-spec search_patches(Model, Log, Config) -> Result when
    Model :: {Places :: [non_neg_integer()],
              Transitions :: [{non_neg_integer(), non_neg_integer() | undefined}],
              Arcs :: [{non_neg_integer(), non_neg_integer(), binary()}]},
    Log :: [{ActivityId :: non_neg_integer(), Frequency :: non_neg_integer()}],
    Config :: {MaxPatches :: non_neg_integer(),
               MinSupport :: non_neg_integer(),
               BeamWidth :: non_neg_integer()},
    Result :: {ok, map()} | {error, term()}.
search_patches(_Model, _Log, _Config) ->
    ?NIF_NOT_LOADED.

%% =============================================================================
%% Compression - Efficient encoding of event logs
%% =============================================================================

%% @doc Compress an event batch using dictionary + delta encoding.
%%
%% Events: list of {CaseId, ActivityId, Timestamp, Attributes}
%%   where Attributes = [{Key, Value}]
%%
%% Returns:
%%   {ok, #{compressed => binary(),
%%          original_size => Bytes,
%%          compressed_size => Bytes,
%%          ratio => Float}}
%%
-spec compress_events(Events) -> Result when
    Events :: [{CaseId :: non_neg_integer(),
                ActivityId :: non_neg_integer(),
                Timestamp :: non_neg_integer(),
                Attributes :: [{binary(), binary()}]}],
    Result :: {ok, map()} | {error, term()}.
compress_events(_Events) ->
    ?NIF_NOT_LOADED.

%% @doc Decompress a compressed event batch.
%%
%% Compressed: binary from compress_events/1
%%
%% Returns:
%%   {ok, #{decompressed => Events,
%%          event_count => Count}}
%%
-spec decompress_events(Compressed :: binary()) -> Result when
    Result :: {ok, map()} | {error, term()}.
decompress_events(_Compressed) ->
    ?NIF_NOT_LOADED.

%% =============================================================================
%% SIMD Vector Operations
%% =============================================================================

%% @doc SIMD-accelerated dot product for embedding similarity.
%%
%% Both vectors must have the same dimension.
%% Uses AVX2 instructions when available.
%%
-spec simd_dot_product(VecA :: [float()], VecB :: [float()]) ->
    {ok, float()} | {error, dimension_mismatch}.
simd_dot_product(_VecA, _VecB) ->
    ?NIF_NOT_LOADED.

%% @doc SIMD-accelerated L2 (Euclidean) distance.
%%
%% Returns sqrt(sum((a[i] - b[i])^2))
%%
-spec simd_l2_distance(VecA :: [float()], VecB :: [float()]) ->
    {ok, float()} | {error, dimension_mismatch}.
simd_l2_distance(_VecA, _VecB) ->
    ?NIF_NOT_LOADED.

%% @doc SIMD-accelerated cosine similarity.
%%
%% Returns dot(a,b) / (norm(a) * norm(b))
%% Result is in range [-1.0, 1.0]
%%
-spec simd_cosine_similarity(VecA :: [float()], VecB :: [float()]) ->
    {ok, float()} | {error, dimension_mismatch}.
simd_cosine_similarity(_VecA, _VecB) ->
    ?NIF_NOT_LOADED.

%% =============================================================================
%% Benchmarking
%% =============================================================================

%% @doc Run a benchmark of the specified function.
%%
%% Function: atom name of the function to benchmark
%% Iterations: number of iterations to run
%%
%% Returns timing statistics in microseconds.
%%
-spec benchmark(Function :: atom(), Iterations :: pos_integer()) ->
    {ok, #{min => float(), max => float(), avg => float(), total => float()}}.
benchmark(Function, Iterations) when Iterations > 0 ->
    {TestData, RunFn} = get_benchmark_data(Function),

    Times = lists:map(
        fun(_) ->
            {Time, _Result} = timer:tc(fun() -> RunFn(TestData) end),
            Time
        end,
        lists:seq(1, Iterations)
    ),

    Min = lists:min(Times),
    Max = lists:max(Times),
    Total = lists:sum(Times),
    Avg = Total / Iterations,

    {ok, #{
        min => Min / 1000.0,    %% Convert to ms
        max => Max / 1000.0,
        avg => Avg / 1000.0,
        total => Total / 1000.0,
        iterations => Iterations
    }}.

%% @private Generate test data for benchmarks
get_benchmark_data(replay_tokens) ->
    Marking = [{1, 1}],
    Transitions = [
        {1, [{1, 1}], [{2, 1}]},
        {2, [{2, 1}], [{3, 1}]},
        {3, [{3, 1}], [{4, 1}]}
    ],
    {{Marking, Transitions}, fun({M, T}) -> replay_tokens(M, T) end};

get_benchmark_data(compute_alignment) ->
    Trace = [1, 2, 3, 4, 5],
    Transitions = [
        {1, [{0, 1}], [{1, 1}], 1},
        {2, [{1, 1}], [{2, 1}], 2},
        {3, [{2, 1}], [{3, 1}], 3},
        {4, [{3, 1}], [{4, 1}], 4},
        {5, [{4, 1}], [{5, 1}], 5}
    ],
    Model = {Transitions, [{0, 1}], [{5, 1}]},
    Costs = {0, 1, 1},
    {{Trace, Model, Costs}, fun({T, M, C}) -> compute_alignment(T, M, C) end};

get_benchmark_data(compress_events) ->
    Events = [
        {1, 100, 1000000, [{<<"key1">>, <<"value1">>}]},
        {1, 101, 1000001, [{<<"key1">>, <<"value2">>}]},
        {1, 102, 1000002, [{<<"key2">>, <<"value1">>}]},
        {2, 100, 1000003, [{<<"key1">>, <<"value1">>}]},
        {2, 101, 1000004, [{<<"key2">>, <<"value2">>}]}
    ],
    {Events, fun(E) -> compress_events(E) end};

get_benchmark_data(simd_dot_product) ->
    Dim = 256,
    VecA = [rand:uniform() || _ <- lists:seq(1, Dim)],
    VecB = [rand:uniform() || _ <- lists:seq(1, Dim)],
    {{VecA, VecB}, fun({A, B}) -> simd_dot_product(A, B) end};

get_benchmark_data(simd_cosine_similarity) ->
    Dim = 256,
    VecA = [rand:uniform() || _ <- lists:seq(1, Dim)],
    VecB = [rand:uniform() || _ <- lists:seq(1, Dim)],
    {{VecA, VecB}, fun({A, B}) -> simd_cosine_similarity(A, B) end};

get_benchmark_data(_) ->
    {undefined, fun(_) -> {error, unknown_function} end}.

%% =============================================================================
%% Erlang Fallbacks (used when NIF not loaded)
%% =============================================================================

%% These are pure Erlang implementations used as fallbacks.
%% They are slower but functionally equivalent.

-ifdef(ENABLE_FALLBACKS).

-spec erlang_dot_product([float()], [float()]) -> float().
erlang_dot_product(VecA, VecB) when length(VecA) =:= length(VecB) ->
    lists:sum([A * B || {A, B} <- lists:zip(VecA, VecB)]).

-spec erlang_l2_distance([float()], [float()]) -> float().
erlang_l2_distance(VecA, VecB) when length(VecA) =:= length(VecB) ->
    SumSq = lists:sum([math:pow(A - B, 2) || {A, B} <- lists:zip(VecA, VecB)]),
    math:sqrt(SumSq).

-spec erlang_cosine_similarity([float()], [float()]) -> float().
erlang_cosine_similarity(VecA, VecB) when length(VecA) =:= length(VecB) ->
    Dot = erlang_dot_product(VecA, VecB),
    NormA = math:sqrt(lists:sum([A * A || A <- VecA])),
    NormB = math:sqrt(lists:sum([B * B || B <- VecB])),
    case NormA * NormB of
        0.0 -> 0.0;
        Denom -> Dot / Denom
    end.

-endif.
