-module(erlmcp_sets_benchmark).
%%%
%%% @doc Benchmark OTP 28 Sets Performance (Tuple vs Map Backend)
%%%
%%% This benchmark compares the performance of the old tuple-based sets
%%% implementation vs the new OTP 28 map-based backend for MCP capability sets.
%%%
%%% Run with:
%%% ```bash
%%% rebar3 shell --apps erlmcp_core
%%% 1> l(erlmcp_sets_benchmark).
%%% 2> erlmcp_sets_benchmark:run_all().
%%% ```
%%%
%%% Or compile and run directly:
%%% ```bash
%%% erlc -I apps/erlmcp_core/include -o . bench/erlmcp_sets_benchmark.erl
%%% erl -pa . -eval "erlmcp_sets_benchmark:run_all(), init:stop()."
%%% ```
%%%
%%% @end
%%%

-export([
    run_all/0,
    benchmark_create/1,
    benchmark_add/1,
    benchmark_lookup/1,
    benchmark_intersection/1,
    benchmark_subset/1,
    benchmark_memory/1
]).

-define(SIZES, [100, 1000, 10000]).
-define(ITERATIONS, 1000).

%%%====================================================================
%%% Benchmark Entry Points
%%%====================================================================

%% @doc Run all benchmarks and print results.
-spec run_all() -> ok.
run_all() ->
    io:format("~n=== OTP 28 Sets Benchmark (Tuple vs Map Backend) ===~n~n"),
    io:format("Testing sizes: ~p~n", [?SIZES]),
    io:format("Iterations per operation: ~p~n~n", [?ITERATIONS]),

    lists:foreach(fun(Size) ->
        io:format("~n--- Benchmarking Size: ~p ---~n", [Size]),
        benchmark_create(Size),
        benchmark_add(Size),
        benchmark_lookup(Size),
        benchmark_intersection(Size),
        benchmark_subset(Size),
        benchmark_memory(Size)
    end, ?SIZES),

    io:format("~n=== Benchmark Complete ===~n"),
    ok.

%%%====================================================================
%%% Individual Benchmarks
%%%====================================================================

%% @doc Benchmark set creation.
-spec benchmark_create(pos_integer()) -> ok.
benchmark_create(Size) ->
    io:format("~nCreate ~p elements:~n", [Size]),

    % Tuple backend (old)
    {TimeTuple, _} = timer:tc(fun() ->
        [sets:new([{version, 1}]) || _ <- lists:seq(1, ?ITERATIONS)]
    end),
    io:format("  Tuple backend: ~.3f ms~n", [TimeTuple / 1000]),

    % Map backend (OTP 28 default)
    {TimeMap, _} = timer:tc(fun() ->
        [sets:new() || _ <- lists:seq(1, ?ITERATIONS)]
    end),
    io:format("  Map backend (OTP 28): ~.3f ms~n", [TimeMap / 1000]),
    io:format("  Speedup: ~.2fx~n", [TimeTuple / TimeMap]),
    ok.

%% @doc Benchmark adding elements.
-spec benchmark_add(pos_integer()) -> ok.
benchmark_add(Size) ->
    io:format("~nAdd ~p elements:~n", [Size]),

    Elements = [<<"cap_", (integer_to_binary(N))/binary>> || N <- lists:seq(1, Size)],

    % Tuple backend
    TupleSet = sets:new([{version, 1}]),
    {TimeTuple, _} = timer:tc(fun() ->
        lists:foldl(fun(Elem, Set) ->
            sets:add_element(Elem, Set)
        end, TupleSet, Elements)
    end),
    io:format("  Tuple backend: ~.3f ms~n", [TimeTuple / 1000]),

    % Map backend
    MapSet = sets:new(),
    {TimeMap, _} = timer:tc(fun() ->
        lists:foldl(fun(Elem, Set) ->
            sets:add_element(Elem, Set)
        end, MapSet, Elements)
    end),
    io:format("  Map backend (OTP 28): ~.3f ms~n", [TimeMap / 1000]),
    io:format("  Speedup: ~.2fx~n", [TimeTuple / TimeMap]),
    ok.

%% @doc Benchmark element lookup (is_element).
-spec benchmark_lookup(pos_integer()) -> ok.
benchmark_lookup(Size) ->
    io:format("~nLookup (is_element) in ~p elements:~n", [Size]),

    Elements = [<<"cap_", (integer_to_binary(N))/binary>> || N <- lists:seq(1, Size)],
    TupleSet = lists:foldl(fun(Elem, Set) -> sets:add_element(Elem, Set) end,
                          sets:new([{version, 1}]), Elements),
    MapSet = sets:from_list(Elements),

    % Lookup random elements
    Lookups = [<<"cap_", (integer_to_binary(rand:uniform(Size)))>>/binary ||
               _ <- lists:seq(1, ?ITERATIONS)],

    {TimeTuple, _} = timer:tc(fun() ->
        lists:foreach(fun(Elem) ->
            sets:is_element(Elem, TupleSet)
        end, Lookups)
    end),
    io:format("  Tuple backend: ~.3f ms~n", [TimeTuple / 1000]),

    {TimeMap, _} = timer:tc(fun() ->
        lists:foreach(fun(Elem) ->
            sets:is_element(Elem, MapSet)
        end, Lookups)
    end),
    io:format("  Map backend (OTP 28): ~.3f ms~n", [TimeMap / 1000]),
    io:format("  Speedup: ~.2fx~n", [TimeTuple / TimeMap]),
    ok.

%% @doc Benchmark set intersection.
-spec benchmark_intersection(pos_integer()) -> ok.
benchmark_intersection(Size) ->
    io:format("~nIntersection of two ~p-element sets (50% overlap):~n", [Size]),

    % Create two sets with ~50% overlap
    Set1Elements = [<<"cap_", (integer_to_binary(N))/binary>> || N <- lists:seq(1, Size)],
    Set2Elements = [<<"cap_", (integer_to_binary(N))/binary>> || N <- lists:seq(Size div 2, Size + (Size div 2))],

    TupleSet1 = sets:from_list(Set1Elements, [{version, 1}]),
    TupleSet2 = sets:from_list(Set2Elements, [{version, 1}]),

    MapSet1 = sets:from_list(Set1Elements),
    MapSet2 = sets:from_list(Set2Elements),

    % Benchmark intersection
    {TimeTuple, _} = timer:tc(fun() ->
        [sets:intersection(TupleSet1, TupleSet2) || _ <- lists:seq(1, ?ITERATIONS)]
    end),
    io:format("  Tuple backend: ~.3f ms~n", [TimeTuple / 1000]),

    {TimeMap, _} = timer:tc(fun() ->
        [sets:intersection(MapSet1, MapSet2) || _ <- lists:seq(1, ?ITERATIONS)]
    end),
    io:format("  Map backend (OTP 28): ~.3f ms~n", [TimeMap / 1000]),
    io:format("  Speedup: ~.2fx~n", [TimeTuple / TimeMap]),
    ok.

%% @doc Benchmark subset check.
-spec benchmark_subset(pos_integer()) -> ok.
benchmark_subset(Size) ->
    io:format("~nSubset check (is_subset) in ~p elements:~n", [Size]),

    Elements = [<<"cap_", (integer_to_binary(N))/binary>> || N <- lists:seq(1, Size)],
    SubsetElements = lists:sublist(Elements, Size div 2),

    TupleSet = sets:from_list(Elements, [{version, 1}]),
    TupleSubset = sets:from_list(SubsetElements, [{version, 1}]),

    MapSet = sets:from_list(Elements),
    MapSubset = sets:from_list(SubsetElements),

    {TimeTuple, _} = timer:tc(fun() ->
        [sets:is_subset(TupleSubset, TupleSet) || _ <- lists:seq(1, ?ITERATIONS)]
    end),
    io:format("  Tuple backend: ~.3f ms~n", [TimeTuple / 1000]),

    {TimeMap, _} = timer:tc(fun() ->
        [sets:is_subset(MapSubset, MapSet) || _ <- lists:seq(1, ?ITERATIONS)]
    end),
    io:format("  Map backend (OTP 28): ~.3f ms~n", [TimeMap / 1000]),
    io:format("  Speedup: ~.2fx~n", [TimeTuple / TimeMap]),
    ok.

%% @doc Benchmark memory usage.
-spec benchmark_memory(pos_integer()) -> ok.
benchmark_memory(Size) ->
    io:format("~nMemory usage for ~p elements:~n", [Size]),

    Elements = [<<"cap_", (integer_to_binary(N))/binary>> || N <- lists:seq(1, Size)],

    TupleSet = sets:from_list(Elements, [{version, 1}]),
    MapSet = sets:from_list(Elements),

    TupleWords = erts_debug:size(TupleSet),
    MapWords = erts_debug:size(MapSet),

    io:format("  Tuple backend: ~p words~n", [TupleWords]),
    io:format("  Map backend (OTP 28): ~p words~n", [MapWords]),
    io:format("  Memory reduction: ~.1f%~n", [
        ((TupleWords - MapWords) / TupleWords) * 100
    ]),
    ok.
