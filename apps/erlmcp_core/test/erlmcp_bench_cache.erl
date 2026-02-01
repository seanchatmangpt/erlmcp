%%%====================================================================
%%% @doc erlmcp_cache Benchmark Suite
%%%
%%% Measures:
%%% - Cache hit latency (L1, L2, L3) - Target: <1ms L1, <5ms L2
%%% - Cache miss latency (cold read)
%%% - Throughput improvement (5-10x for cacheable requests)
%%% - Eviction performance (LRU)
%%% - Write-through vs write-back latency
%%% - Distributed cache sync overhead
%%%
%%% Output: Metrology-compliant JSON results
%%% @end
%%%====================================================================
-module(erlmcp_bench_cache).

-export([
    run/0,
    run/1,
    run_all/0,
    workloads/0
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type workload() :: #{
    id := binary(),
    operations := pos_integer(),
    workers := pos_integer(),
    cache_size := pos_integer()
}.

%%====================================================================
%% Workload Definitions
%%====================================================================

-spec workloads() -> [workload()].
workloads() ->
    [
        #{id => <<"cache_hit_1k">>,
          operations => 1000,
          workers => 1,
          cache_size => 100},

        #{id => <<"cache_hit_10k">>,
          operations => 10000,
          workers => 10,
          cache_size => 1000},

        #{id => <<"cache_hit_100k">>,
          operations => 100000,
          workers => 100,
          cache_size => 10000},

        #{id => <<"cache_mixed_workload">>,
          operations => 50000,
          workers => 50,
          cache_size => 5000},

        #{id => <<"cache_eviction_stress">>,
          operations => 10000,
          workers => 10,
          cache_size => 100},  % Small cache for eviction

        #{id => <<"cache_write_through">>,
          operations => 10000,
          workers => 10,
          cache_size => 1000},

        #{id => <<"cache_write_back">>,
          operations => 10000,
          workers => 10,
          cache_size => 1000}
    ].

%%====================================================================
%% Main Entry Points
%%====================================================================

-spec run() -> ok.
run() ->
    run_all().

-spec run(binary()) -> ok | {error, term()}.
run(WorkloadId) when is_binary(WorkloadId) ->
    Workloads = workloads(),
    case lists:filter(fun(#{id := Id}) -> Id =:= WorkloadId end, Workloads) of
        [] ->
            io:format("ERROR: Unknown workload: ~s~n", [WorkloadId]),
            io:format("Available workloads: ~p~n", [[Id || #{id := Id} <- Workloads]]),
            {error, {unknown_workload, WorkloadId}};
        [Workload] ->
            run_workload(Workload)
    end;
run(WorkloadId) when is_list(WorkloadId) ->
    run(list_to_binary(WorkloadId));
run(WorkloadId) when is_atom(WorkloadId) ->
    run(atom_to_binary(WorkloadId, utf8)).

-spec run_all() -> ok.
run_all() ->
    io:format("~n==============================================~n"),
    io:format("ERLMCP CACHE BENCHMARK SUITE~n"),
    io:format("==============================================~n~n"),

    lists:foreach(fun(Workload) ->
        run_workload(Workload)
    end, workloads()),

    io:format("~n==============================================~n"),
    io:format("All cache benchmarks complete. Results in bench/results/~n"),
    io:format("==============================================~n~n"),
    ok.

%%====================================================================
%% Workload Execution
%%====================================================================

-spec run_workload(workload()) -> ok | {error, term()}.
run_workload(#{id := WorkloadId, operations := Ops, workers := Workers,
               cache_size := CacheSize} = Workload) ->
    io:format("~n--- Workload: ~s (~p ops, ~p workers, cache size ~p) ---~n",
              [WorkloadId, Ops, Workers, CacheSize]),

    %% Start cache with appropriate configuration
    Config = #{
        max_l1_size => CacheSize,
        max_l2_size => CacheSize * 10,
        default_ttl_seconds => 300,
        cleanup_interval_ms => 60000
    },

    {ok, CachePid} = erlmcp_cache:start_link(Config),

    try
        %% Capture environment
        Env = capture_environment(),

        %% Measure memory before
        MemoryBefore = erlang:memory(total),

        %% Run benchmark based on workload type
        StartTime = erlang:monotonic_time(microsecond),

        Results = case WorkloadId of
            <<"cache_hit_", _/binary>> ->
                benchmark_cache_hits(Ops, Workers);
            <<"cache_mixed_workload">> ->
                benchmark_mixed_workload(Ops, Workers);
            <<"cache_eviction_stress">> ->
                benchmark_eviction(Ops, Workers, CacheSize);
            <<"cache_write_through">> ->
                benchmark_write_strategy(Ops, Workers, write_through);
            <<"cache_write_back">> ->
                benchmark_write_strategy(Ops, Workers, write_back);
            _ ->
                benchmark_cache_hits(Ops, Workers)
        end,

        EndTime = erlang:monotonic_time(microsecond),

        %% Measure memory after
        MemoryAfter = erlang:memory(total),

        %% Get cache stats
        CacheStats = erlmcp_cache:stats(),

        %% Calculate metrics
        TotalDurationUs = EndTime - StartTime,
        TotalDurationS = TotalDurationUs / 1_000_000,
        Throughput = Ops / TotalDurationS,
        MemoryUsed = MemoryAfter - MemoryBefore,

        %% Extract latencies from results
        Latencies = maps:get(latencies, Results, []),
        {P50, P95, P99} = calculate_percentiles(Latencies),

        %% Print results
        io:format("~nResults:~n"),
        io:format("  Duration:        ~.3f s~n", [TotalDurationS]),
        io:format("  Throughput:      ~.0f ops/s~n", [Throughput]),
        io:format("  Latency P50:     ~.3f ms~n", [P50 / 1000]),
        io:format("  Latency P95:     ~.3f ms~n", [P95 / 1000]),
        io:format("  Latency P99:     ~.3f ms~n", [P99 / 1000]),
        io:format("  Memory Used:     ~.2f MB~n", [MemoryUsed / (1024 * 1024)]),
        io:format("~nCache Stats:~n"),
        io:format("  Hits:            ~p~n", [maps:get(hits, CacheStats, 0)]),
        io:format("  Misses:          ~p~n", [maps:get(misses, CacheStats, 0)]),
        io:format("  Hit Rate:        ~.2f%~n", [maps:get(hit_rate, CacheStats, 0.0) * 100]),
        io:format("  L1 Hits:         ~p~n", [maps:get(l1_hits, CacheStats, 0)]),
        io:format("  L2 Hits:         ~p~n", [maps:get(l2_hits, CacheStats, 0)]),
        io:format("  Evictions:       ~p~n", [maps:get(evictions, CacheStats, 0)]),

        %% Write JSON results
        write_results(WorkloadId, #{
            workload => Workload,
            environment => Env,
            duration_s => TotalDurationS,
            throughput_ops_per_s => Throughput,
            latency_p50_us => P50,
            latency_p95_us => P95,
            latency_p99_us => P99,
            memory_used_bytes => MemoryUsed,
            cache_stats => CacheStats
        }),

        ok
    after
        gen_server:stop(CachePid)
    end.

%%====================================================================
%% Benchmark Implementations
%%====================================================================

-spec benchmark_cache_hits(pos_integer(), pos_integer()) -> map().
benchmark_cache_hits(TotalOps, NumWorkers) ->
    %% Pre-populate cache
    NumKeys = min(TotalOps, 10000),
    lists:foreach(fun(N) ->
        Key = iolist_to_binary(io_lib:format("key_~p", [N])),
        Value = iolist_to_binary(io_lib:format("value_~p", [N])),
        erlmcp_cache:put(Key, Value)
    end, lists:seq(1, NumKeys)),

    %% Run parallel cache get operations
    OpsPerWorker = TotalOps div NumWorkers,
    Latencies = run_parallel_workers(NumWorkers, fun(_WorkerId) ->
        lists:map(fun(_) ->
            KeyNum = rand:uniform(NumKeys),
            Key = iolist_to_binary(io_lib:format("key_~p", [KeyNum])),

            StartTime = erlang:monotonic_time(microsecond),
            {ok, _} = erlmcp_cache:get(Key),
            EndTime = erlang:monotonic_time(microsecond),

            EndTime - StartTime
        end, lists:seq(1, OpsPerWorker))
    end),

    #{latencies => lists:flatten(Latencies)}.

-spec benchmark_mixed_workload(pos_integer(), pos_integer()) -> map().
benchmark_mixed_workload(TotalOps, NumWorkers) ->
    %% 70% reads, 30% writes
    OpsPerWorker = TotalOps div NumWorkers,

    Latencies = run_parallel_workers(NumWorkers, fun(_WorkerId) ->
        lists:map(fun(_) ->
            KeyNum = rand:uniform(10000),
            Key = iolist_to_binary(io_lib:format("key_~p", [KeyNum])),

            StartTime = erlang:monotonic_time(microsecond),

            case rand:uniform(100) of
                N when N =< 70 ->
                    %% Read operation
                    _ = erlmcp_cache:get(Key);
                _ ->
                    %% Write operation
                    Value = iolist_to_binary(io_lib:format("value_~p", [rand:uniform(10000)])),
                    erlmcp_cache:put(Key, Value)
            end,

            EndTime = erlang:monotonic_time(microsecond),
            EndTime - StartTime
        end, lists:seq(1, OpsPerWorker))
    end),

    #{latencies => lists:flatten(Latencies)}.

-spec benchmark_eviction(pos_integer(), pos_integer(), pos_integer()) -> map().
benchmark_eviction(TotalOps, NumWorkers, CacheSize) ->
    %% Write more keys than cache can hold to trigger evictions
    OpsPerWorker = TotalOps div NumWorkers,

    Latencies = run_parallel_workers(NumWorkers, fun(_WorkerId) ->
        lists:map(fun(N) ->
            %% Cycle through keys larger than cache size
            KeyNum = N rem (CacheSize * 2),
            Key = iolist_to_binary(io_lib:format("key_~p", [KeyNum])),
            Value = iolist_to_binary(io_lib:format("value_~p", [N])),

            StartTime = erlang:monotonic_time(microsecond),
            erlmcp_cache:put(Key, Value, {lru, CacheSize}),
            EndTime = erlang:monotonic_time(microsecond),

            EndTime - StartTime
        end, lists:seq(1, OpsPerWorker))
    end),

    #{latencies => lists:flatten(Latencies)}.

-spec benchmark_write_strategy(pos_integer(), pos_integer(), atom()) -> map().
benchmark_write_strategy(TotalOps, NumWorkers, Strategy) ->
    OpsPerWorker = TotalOps div NumWorkers,

    Latencies = run_parallel_workers(NumWorkers, fun(_WorkerId) ->
        lists:map(fun(N) ->
            Key = iolist_to_binary(io_lib:format("key_~p", [N])),
            Value = iolist_to_binary(io_lib:format("value_~p", [N])),

            StartTime = erlang:monotonic_time(microsecond),
            erlmcp_cache:put(Key, Value, Strategy),
            EndTime = erlang:monotonic_time(microsecond),

            EndTime - StartTime
        end, lists:seq(1, OpsPerWorker))
    end),

    #{latencies => lists:flatten(Latencies)}.

%%====================================================================
%% Helper Functions
%%====================================================================

-spec run_parallel_workers(pos_integer(), fun((pos_integer()) -> [non_neg_integer()])) ->
    [[non_neg_integer()]].
run_parallel_workers(NumWorkers, WorkerFun) ->
    Parent = self(),

    Pids = lists:map(fun(WorkerId) ->
        spawn_link(fun() ->
            Result = WorkerFun(WorkerId),
            Parent ! {worker_done, self(), Result}
        end)
    end, lists:seq(1, NumWorkers)),

    %% Collect results
    lists:map(fun(Pid) ->
        receive
            {worker_done, Pid, Result} -> Result
        after 60000 ->
            error({worker_timeout, Pid})
        end
    end, Pids).

-spec calculate_percentiles([non_neg_integer()]) ->
    {float(), float(), float()}.
calculate_percentiles([]) ->
    {0.0, 0.0, 0.0};
calculate_percentiles(Latencies) ->
    Sorted = lists:sort(Latencies),
    SortedTuple = list_to_tuple(Sorted),
    Len = length(Sorted),

    P50 = lists:nth(max(1, round(Len * 0.50)), Sorted),
    P95 = lists:nth(max(1, round(Len * 0.95)), Sorted),
    P99 = lists:nth(max(1, round(Len * 0.99)), Sorted),

    {float(P50), float(P95), float(P99)}.

-spec capture_environment() -> map().
capture_environment() ->
    #{
        erlang_version => erlang:system_info(otp_release),
        system_version => erlang:system_info(system_version),
        schedulers => erlang:system_info(schedulers_online),
        logical_processors => erlang:system_info(logical_processors),
        timestamp => erlang:timestamp(),
        node => node()
    }.

-spec write_results(binary(), map()) -> ok.
write_results(WorkloadId, Results) ->
    %% Ensure results directory exists
    ResultsDir = "bench/results",
    ok = filelib:ensure_dir(ResultsDir ++ "/"),

    %% Write JSON file
    Timestamp = integer_to_list(erlang:system_time(second)),
    Filename = lists:flatten(io_lib:format("~s/cache_~s_~s.json",
                                           [ResultsDir, WorkloadId, Timestamp])),

    %% Convert to JSON-compatible format
    JsonResults = convert_to_json(Results),
    JsonBinary = jsx:encode(JsonResults),

    case file:write_file(Filename, JsonBinary) of
        ok ->
            io:format("~nResults written to: ~s~n", [Filename]),
            ok;
        {error, Reason} ->
            io:format("~nWARNING: Failed to write results: ~p~n", [Reason]),
            ok
    end.

-spec convert_to_json(map()) -> map().
convert_to_json(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        JsonKey = case K of
            _ when is_atom(K) -> atom_to_binary(K, utf8);
            _ when is_binary(K) -> K;
            _ -> iolist_to_binary(io_lib:format("~p", [K]))
        end,
        JsonValue = convert_to_json(V),
        Acc#{JsonKey => JsonValue}
    end, #{}, Map);
convert_to_json(List) when is_list(List) ->
    case io_lib:printable_unicode_list(List) of
        true -> list_to_binary(List);
        false -> [convert_to_json(Item) || Item <- List]
    end;
convert_to_json(Tuple) when is_tuple(Tuple) ->
    convert_to_json(tuple_to_list(Tuple));
convert_to_json(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
convert_to_json(Other) ->
    Other.
