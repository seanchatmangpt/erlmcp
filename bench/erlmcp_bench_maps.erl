-module(erlmcp_bench_maps).

%% Benchmark OTP 26-28 Map Enhancements
%%
%% Compares map operation performance across OTP versions
%% and demonstrates optimizations for MCP data structures

-export([run_all/0, run_all/1, run_microbenchmarks/0, run_macrobenchmarks/0]).
-export([benchmark_filter_keys/0, benchmark_put_nested/0, benchmark_merge_capabilities/0]).
-export([benchmark_otp26_vs_28/0, generate_report/0]).

-define(WARMUP_ITERATIONS, 1000).
-define(BENCHMARK_ITERATIONS, 10000).
-define(MACRO_ITERATIONS, 100000).
-define(LARGE_MAP_SIZE, 10000).

%%%====================================================================
%%% API Functions
%%%====================================================================

%% @doc Run all map benchmarks with default settings
-spec run_all() -> ok.
run_all() ->
    run_all([]).

%% @doc Run all benchmarks with options
%% Options:
%%   - {iterations, N} - Number of iterations (default: 10000)
%%   - {size, N} - Map size for benchmarks (default: 10000)
%%   - {verbose, bool} - Verbose output (default: false)
-spec run_all([proplists:property()]) -> ok.
run_all(Options) ->
    Iterations = proplists:get_value(iterations, Options, ?BENCHMARK_ITERATIONS),
    Size = proplists:get_value(size, Options, ?LARGE_MAP_SIZE),
    Verbose = proplists:get_value(verbose, Options, false),

    io:format("~n=== OTP 26-28 Map Enhancement Benchmarks ===~n"),
    io:format("Iterations: ~p, Map Size: ~p~n~n", [Iterations, Size]),

    %% OTP Version detection
    {Major, Minor} = get_otp_version(),
    io:format("Erlang/OTP ~B.~B~n~n", [Major, Minor]),

    %% Microbenchmarks (fast operations)
    io:format("--- Microbenchmarks ---~n"),
    benchmark_filter_keys_micro(Iterations, Size, Verbose),
    benchmark_put_nested_micro(Iterations, Verbose),
    benchmark_merge_capabilities_micro(Iterations, Verbose),

    %% Macrobenchmarks (realistic workloads)
    io:format("~n--- Macrobenchmarks ---~n"),
    benchmark_capability_negotiation(Iterations, Verbose),
    benchmark_resource_filtering(Size, Verbose),
    benchmark_metadata_operations(Iterations, Verbose),

    %% OTP version comparison
    io:format("~n--- OTP Version Comparison ---~n"),
    benchmark_otp26_vs_28(Iterations, Verbose),

    ok.

%% @doc Run only microbenchmarks
-spec run_microbenchmarks() -> ok.
run_microbenchmarks() ->
    io:format("~n=== Map Microbenchmarks ===~n~n"),
    benchmark_filter_keys_micro(?BENCHMARK_ITERATIONS, 1000, false),
    benchmark_put_nested_micro(?BENCHMARK_ITERATIONS, false),
    benchmark_merge_capabilities_micro(?BENCHMARK_ITERATIONS, false),
    ok.

%% @doc Run only macrobenchmarks
-spec run_macrobenchmarks() -> ok.
run_macrobenchmarks() ->
    io:format("~n=== Map Macrobenchmarks ===~n~n"),
    benchmark_capability_negotiation(?BENCHMARK_ITERATIONS, false),
    benchmark_resource_filtering(?LARGE_MAP_SIZE, false),
    benchmark_metadata_operations(?BENCHMARK_ITERATIONS, false),
    ok.

%%%====================================================================
%%% Individual Benchmarks
%%%====================================================================

%% @doc Benchmark filter_keys operation (OTP 27 optimization)
-spec benchmark_filter_keys() -> {ok, map()}.
benchmark_filter_keys() ->
    %% Create large map with mixed prefixes
    Map = generate_large_map(?LARGE_MAP_SIZE),

    %% Warmup
    lists:foreach(fun(_) ->
        erlmcp_map_utils:filter_keys(Map, <<"root_">>)
    end, lists:seq(1, ?WARMUP_ITERATIONS)),

    %% Benchmark
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            erlmcp_map_utils:filter_keys(Map, <<"root_">>)
        end, lists:seq(1, ?BENCHMARK_ITERATIONS))
    end),

    AvgUs = Time / ?BENCHMARK_ITERATIONS,
    {ok, #{
        operation => filter_keys,
        map_size => ?LARGE_MAP_SIZE,
        iterations => ?BENCHMARK_ITERATIONS,
        total_us => Time,
        avg_us => AvgUs,
        ops_per_sec => 1000000 / AvgUs
    }}.

%% @doc Benchmark put_nested operation (OTP 28 optimization)
-spec benchmark_put_nested() -> {ok, map()}.
benchmark_put_nested() ->
    Paths = generate_nested_paths(?BENCHMARK_ITERATIONS, 5),

    %% Warmup
    lists:foreach(fun(Path) ->
        erlmcp_map_utils:put_nested(#{}, Path, value)
    end, Paths),

    %% Benchmark
    {Time, _} = timer:tc(fun() ->
        lists:foldl(fun(Path, Acc) ->
            erlmcp_map_utils:put_nested(Acc, Path, value)
        end, #{}, Paths)
    end),

    AvgUs = Time / ?BENCHMARK_ITERATIONS,
    {ok, #{
        operation => put_nested,
        iterations => ?BENCHMARK_ITERATIONS,
        total_us => Time,
        avg_us => AvgUs,
        ops_per_sec => 1000000 / AvgUs
    }}.

%% @doc Benchmark merge_capabilities operation
-spec benchmark_merge_capabilities() -> {ok, map()}.
benchmark_merge_capabilities() ->
    Caps = generate_capability_maps(100),

    %% Warmup
    lists:foreach(fun(_) ->
        erlmcp_map_utils:merge_capabilities(Caps)
    end, lists:seq(1, ?WARMUP_ITERATIONS)),

    %% Benchmark
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            erlmcp_map_utils:merge_capabilities(Caps)
        end, lists:seq(1, ?BENCHMARK_ITERATIONS))
    end),

    AvgUs = Time / ?BENCHMARK_ITERATIONS,
    {ok, #{
        operation => merge_capabilities,
        maps_count => 100,
        iterations => ?BENCHMARK_ITERATIONS,
        total_us => Time,
        avg_us => AvgUs,
        ops_per_sec => 1000000 / AvgUs
    }}.

%% @doc Compare OTP 26 vs OTP 28 performance
-spec benchmark_otp26_vs_28() -> ok.
benchmark_otp26_vs_28() ->
    benchmark_otp26_vs_28(?BENCHMARK_ITERATIONS, false).

%% @doc Generate comprehensive benchmark report
-spec generate_report() -> ok.
generate_report() ->
    {ok, FilterRes} = benchmark_filter_keys(),
    {ok, PutRes} = benchmark_put_nested(),
    {ok, MergeRes} = benchmark_merge_capabilities(),

    Report = [
        "# OTP 26-28 Map Enhancement Benchmark Report\n",
        "Generated: " ++ erlang:system_time(millisecond) ++ "\n\n",
        "## Filter Keys Performance\n",
        format_result(FilterRes),
        "\n## Put Nested Performance\n",
        format_result(PutRes),
        "\n## Merge Capabilities Performance\n",
        format_result(MergeRes),
        "\n## OTP Version Comparison\n",
        get_otp_version_info()
    ],

    file:write_file("bench/MAP_ENHANCEMENTS_REPORT.md", Report),
    io:format("Report written to bench/MAP_ENHANCEMENTS_REPORT.md~n"),
    ok.

%%%====================================================================
%%% Internal Benchmark Functions
%%%====================================================================

%% @doc Microbenchmark for filter_keys
benchmark_filter_keys_micro(Iterations, MapSize, Verbose) ->
    Map = generate_large_map(MapSize),

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            erlmcp_map_utils:filter_keys(Map, <<"root_">>)
        end, lists:seq(1, Iterations))
    end),

    AvgUs = Time / Iterations,
    OpsPerSec = 1000000 / AvgUs,

    io:format("filter_keys (~p entries):~n", [MapSize]),
    io:format("  Total: ~.2f ms, Avg: ~.2f us~n", [Time/1000, AvgUs]),
    io:format("  Throughput: ~.0f ops/sec~n~n", [OpsPerSec]),

    maybe_print_detailed(Verbose, "filter_keys", Time, Iterations),
    ok.

%% @doc Microbenchmark for put_nested
benchmark_put_nested_micro(Iterations, Verbose) ->
    Paths = generate_nested_paths(Iterations, 5),

    {Time, _} = timer:tc(fun() ->
        lists:foldl(fun(Path, Acc) ->
            erlmcp_map_utils:put_nested(Acc, Path, value)
        end, #{}, Paths)
    end),

    AvgUs = Time / Iterations,
    OpsPerSec = 1000000 / AvgUs,

    io:format("put_nested (depth=5):~n"),
    io:format("  Total: ~.2f ms, Avg: ~.2f us~n", [Time/1000, AvgUs]),
    io:format("  Throughput: ~.0f ops/sec~n~n", [OpsPerSec]),

    maybe_print_detailed(Verbose, "put_nested", Time, Iterations),
    ok.

%% @doc Microbenchmark for merge_capabilities
benchmark_merge_capabilities_micro(Iterations, Verbose) ->
    Caps = generate_capability_maps(100),

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            erlmcp_map_utils:merge_capabilities(Caps)
        end, lists:seq(1, Iterations))
    end),

    AvgUs = Time / Iterations,
    OpsPerSec = 1000000 / AvgUs,

    io:format("merge_capabilities (100 maps):~n"),
    io:format("  Total: ~.2f ms, Avg: ~.2f us~n", [Time/1000, AvgUs]),
    io:format("  Throughput: ~.0f ops/sec~n~n", [OpsPerSec]),

    maybe_print_detailed(Verbose, "merge_capabilities", Time, Iterations),
    ok.

%% @doc Macrobenchmark: MCP capability negotiation
benchmark_capability_negotiation(Iterations, Verbose) ->
    %% Simulate realistic capability negotiation scenario
    ClientCaps = generate_capability_maps(50),
    ServerCaps = generate_capability_maps(50),

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            Merged = erlmcp_map_utils:merge_capabilities(ClientCaps ++ ServerCaps),
            %% Simulate filtering experimental features
            erlmcp_map_utils:filter_by_prefix(Merged, <<"experimental_">>)
        end, lists:seq(1, Iterations))
    end),

    AvgUs = Time / Iterations,
    OpsPerSec = 1000000 / AvgUs,

    io:format("capability_negotiation (100 caps + filter):~n"),
    io:format("  Total: ~.2f ms, Avg: ~.2f us~n", [Time/1000, AvgUs]),
    io:format("  Throughput: ~.0f ops/sec~n~n", [OpsPerSec]),

    maybe_print_detailed(Verbose, "capability_negotiation", Time, Iterations),
    ok.

%% @doc Macrobenchmark: Resource filtering
benchmark_resource_filtering(MapSize, Verbose) ->
    Map = generate_resource_map(MapSize),

    {Time, Result} = timer:tc(fun() ->
        erlmcp_map_utils:filter_by_prefix(Map, <<"resource://">>)
    end),

    FilteredSize = maps:size(Result),
    io:format("resource_filtering (~p -> ~p entries):~n", [MapSize, FilteredSize]),
    io:format("  Total: ~.2f ms~n", [Time/1000]),
    io:format("  Reduction: ~.1f%~n~n", [(1 - FilteredSize/MapSize) * 100]),

    maybe_print_detailed(Verbose, "resource_filtering", Time, 1),
    ok.

%% @doc Macrobenchmark: Metadata operations
benchmark_metadata_operations(Iterations, Verbose) ->
    %% Simulate metadata CRUD operations
    InitialMap = generate_metadata_map(1000),

    {Time, _} = timer:tc(fun() ->
        lists:foldl(fun(I, Acc) ->
            Key = <<"meta_", (integer_to_binary(I))/binary>>,
            %% Update nested metadata
            erlmcp_map_utils:update_nested(
                Acc,
                [<<"metadata">>, Key],
                fun(V) -> V + 1 end,
                0
            )
        end, InitialMap, lists:seq(1, Iterations))
    end),

    AvgUs = Time / Iterations,
    OpsPerSec = 1000000 / AvgUs,

    io:format("metadata_operations (nested updates):~n"),
    io:format("  Total: ~.2f ms, Avg: ~.2f us~n", [Time/1000, AvgUs]),
    io:format("  Throughput: ~.0f ops/sec~n~n", [OpsPerSec]),

    maybe_print_detailed(Verbose, "metadata_operations", Time, Iterations),
    ok.

%% @doc OTP 26 vs 28 comparison
benchmark_otp26_vs_28(Iterations, Verbose) ->
    %% Test maps:put/3 optimization (OTP 28)
    MapSmall = #{},
    Keys = [k1, k2, k3, k4, k5],

    %% Benchmark map construction
    {TimePut, _} = timer:tc(fun() ->
        lists:foldl(fun(K, Acc) ->
            maps:put(K, value, Acc)
        end, MapSmall, Keys)
    end),

    %% Benchmark map update
    {TimeUpdate, _} = timer:tc(fun() ->
        lists:foldl(fun(K, Acc) ->
            maps:put(K, updated_value, Acc)
        end, MapSmall, Keys)
    end),

    io:format("maps:put/3 comparison:~n"),
    io:format("  Construction (5 keys): ~.2f us~n", [TimePut]),
    io:format("  Update (5 keys): ~.2f us~n~n", [TimeUpdate]),

    %% Test maps:filter/2 optimization (OTP 27)
    LargeMap = generate_large_map(1000),

    {TimeFilter, Filtered} = timer:tc(fun() ->
        maps:filter(fun(K, _V) ->
            case K of
                <<"test_", _/binary>> -> true;
                _ -> false
            end
        end, LargeMap)
    end),

    io:format("maps:filter/2 comparison (1000 entries):~n"),
    io:format("  Total: ~.2f us, Filtered: ~p entries~n~n", [TimeFilter, maps:size(Filtered)]),

    maybe_print_detailed(Verbose, "otp_comparison", TimePut + TimeUpdate + TimeFilter, 1),
    ok.

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% @doc Get OTP version
-spec get_otp_version() -> {Major::byte(), Minor::byte()}.
get_otp_version() ->
    case erlang:system_info(otp_release) of
        [$R,N0,N1|_] -> {list_to_integer([N0]), list_to_integer([N1])};
        [N0,N1,N2|_] -> {list_to_integer([N0,N1]), list_to_integer([N2])};
        _ -> {26, 0}
    end.

%% @doc Get OTP version info for report
-spec get_otp_version_info() -> iolist().
get_otp_version_info() ->
    {Major, Minor} = get_otp_version(),
    io_lib:format("- **OTP Version**: ~B.~B~n", [Major, Minor]) ++
    case Major of
        28 -> "- **Compile-Time Optimization**: maps:put/3 enabled\n";
        27 -> "- **Pattern Matching**: maps:filter/2 enhanced\n";
        _ -> "- **Baseline**: OTP 26 compatibility mode\n"
    end.

%% @doc Format benchmark result
-spec format_result(map()) -> iolist().
format_result(#{operation := Op, total_us := Total, avg_us := Avg, ops_per_sec := Ops}) ->
    io_lib:format("- **~s**: ~.2f ms total, ~.2f us avg, ~.0f ops/sec~n",
                  [Op, Total/1000, Avg, Ops]).

%% @doc Generate large map with mixed prefixes
-spec generate_large_map(pos_integer()) -> map().
generate_large_map(Size) ->
    lists:foldl(fun(I, Acc) ->
        Prefix = case I rem 3 of
            0 -> <<"root_">>;
            1 -> <<"resource_">>;
            2 -> <<"other_">>
        end,
        Key = <<Prefix/binary, (integer_to_binary(I))/binary>>,
        maps:put(Key, I, Acc)
    end, #{}, lists:seq(1, Size)).

%% @doc Generate nested paths
-spec generate_nested_paths(pos_integer(), pos_integer()) -> [[term()]].
generate_nested_paths(Count, Depth) ->
    [lists:seq(I, I + Depth - 1) || I <- lists:seq(1, Count)].

%% @doc Generate capability maps
-spec generate_capability_maps(pos_integer()) -> [map()].
generate_capability_maps(Count) ->
    [generate_capability_map(I) || I <- lists:seq(1, Count)].

%% @doc Generate single capability map
-spec generate_capability_map(pos_integer()) -> map().
generate_capability_map(N) ->
    Base = N rem 3,
    case Base of
        0 -> #{<<"roots">> => true, <<"subscribe">> => true};
        1 -> #{<<"tools">> => true, <<"listChanged">> => false};
        2 -> #{<<"sampling">> => true, <<"modelPreferences">> => #{}}
    end.

%% @doc Generate resource map
-spec generate_resource_map(pos_integer()) -> map().
generate_resource_map(Size) ->
    lists:foldl(fun(I, Acc) ->
        Prefix = case I rem 2 of
            0 -> <<"resource://">>;
            1 -> <<"other://">>
        end,
        Key = <<Prefix/binary, (integer_to_binary(I))/binary>>,
        maps:put(Key, #{<<"uri">> => Key, <<"data">> => I}, Acc)
    end, #{}, lists:seq(1, Size)).

%% @doc Generate metadata map
-spec generate_metadata_map(pos_integer()) -> map().
generate_metadata_map(Size) ->
    lists:foldl(fun(I, Acc) ->
        Key = <<"meta_", (integer_to_binary(I))/binary>>,
        maps:put(Key, #{<<"value">> => I, <<"count">> => 0}, Acc)
    end, #{}, lists:seq(1, Size)).

%% @doc Print detailed stats if verbose
maybe_print_detailed(false, _Op, _Time, _Iter) -> ok;
maybe_print_detailed(true, Op, Time, Iter) ->
    io:format("  [~s] ~p iterations in ~.2f ms~n", [Op, Iter, Time/1000]).
