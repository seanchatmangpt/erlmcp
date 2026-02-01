%%%====================================================================
%%% ERLMCP BATCHING BENCHMARK - Measure 2-5x Throughput Improvement
%%%====================================================================
%%% Compares: Single vs Batched request throughput
%%% Workloads: 1K, 10K, 100K operations
%%% Strategies: Size-based, Time-based, Adaptive
%%% Output: Metrology-compliant JSON
%%%====================================================================

-module(erlmcp_bench_batch).

-export([
    run/0,
    run/1,
    run_all/0,
    workloads/0
]).

-include_lib("kernel/include/logger.hrl").

%% Workload definitions
-spec workloads() -> [map()].
workloads() ->
    [
        #{id => <<"batch_1k">>, operations => 1000, description => <<"1K operations">>},
        #{id => <<"batch_10k">>, operations => 10000, description => <<"10K operations">>},
        #{id => <<"batch_100k">>, operations => 100000, description => <<"100K operations">>}
    ].

%% Main entry points
-spec run() -> ok.
run() ->
    run_all().

-spec run(binary()) -> ok | {error, term()}.
run(WorkloadId) when is_binary(WorkloadId) ->
    Workloads = workloads(),
    case lists:filter(fun(#{id := Id}) -> Id =:= WorkloadId end, Workloads) of
        [] ->
            io:format("ERROR: Unknown workload: ~s~n", [WorkloadId]),
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
    io:format("ERLMCP BATCHING BENCHMARK SUITE~n"),
    io:format("Comparing Single vs Batched Request Throughput~n"),
    io:format("==============================================~n~n"),

    lists:foreach(fun(Workload) ->
        run_workload(Workload)
    end, workloads()),

    io:format("~n==============================================~n"),
    io:format("All benchmarks complete. Results in bench/results/~n"),
    io:format("==============================================~n~n"),
    ok.

%% Run a single workload
-spec run_workload(map()) -> ok | {error, term()}.
run_workload(#{id := WorkloadId, operations := Ops} = _Workload) ->
    io:format("~n--- Workload: ~s (~p operations) ---~n", [WorkloadId, Ops]),

    % Capture environment
    Env = capture_environment(),

    % Benchmark 1: Single requests (baseline)
    io:format("Running baseline (single requests)...~n"),
    SingleResult = benchmark_single_requests(Ops),

    % Benchmark 2: Size-based batching
    io:format("Running size-based batching (batch size 10)...~n"),
    SizeBatchResult = benchmark_batched_requests(Ops, {size, 10}),

    % Benchmark 3: Time-based batching
    io:format("Running time-based batching (10ms)...~n"),
    TimeBatchResult = benchmark_batched_requests(Ops, {time, 10}),

    % Benchmark 4: Adaptive batching
    io:format("Running adaptive batching...~n"),
    AdaptiveBatchResult = benchmark_batched_requests(Ops, {adaptive, #{min => 5, max => 50}}),

    % Calculate improvements
    SingleThroughput = maps:get(throughput_msg_per_s, SingleResult),
    SizeImprovement = maps:get(throughput_msg_per_s, SizeBatchResult) / SingleThroughput,
    TimeImprovement = maps:get(throughput_msg_per_s, TimeBatchResult) / SingleThroughput,
    AdaptiveImprovement = maps:get(throughput_msg_per_s, AdaptiveBatchResult) / SingleThroughput,

    % Build report
    Report = #{
        workload_id => WorkloadId,
        benchmark => <<"request_batching">>,
        timestamp => erlang:system_time(second),
        environment => Env,
        operations => Ops,
        baseline => SingleResult,
        size_based_batching => SizeBatchResult#{improvement_factor => round_float(SizeImprovement, 2)},
        time_based_batching => TimeBatchResult#{improvement_factor => round_float(TimeImprovement, 2)},
        adaptive_batching => AdaptiveBatchResult#{improvement_factor => round_float(AdaptiveImprovement, 2)},
        summary => #{
            max_improvement_factor => round_float(lists:max([SizeImprovement, TimeImprovement, AdaptiveImprovement]), 2),
            best_strategy => best_strategy(SizeImprovement, TimeImprovement, AdaptiveImprovement),
            target_met => (SizeImprovement >= 2.0 orelse TimeImprovement >= 2.0 orelse AdaptiveImprovement >= 2.0)
        }
    },

    % Write to file
    Timestamp = erlang:system_time(second),
    Filename = io_lib:format("bench/results/batch_~s_~p.json", [WorkloadId, Timestamp]),
    write_report(Filename, Report),

    % Print summary
    io:format("~n✓ Results:~n"),
    io:format("  Baseline:  ~.2f msg/s~n", [SingleThroughput]),
    io:format("  Size-based: ~.2f msg/s (~.2fx improvement)~n",
        [maps:get(throughput_msg_per_s, SizeBatchResult), SizeImprovement]),
    io:format("  Time-based: ~.2f msg/s (~.2fx improvement)~n",
        [maps:get(throughput_msg_per_s, TimeBatchResult), TimeImprovement]),
    io:format("  Adaptive:  ~.2f msg/s (~.2fx improvement)~n",
        [maps:get(throughput_msg_per_s, AdaptiveBatchResult), AdaptiveImprovement]),
    io:format("  Report: ~s~n", [Filename]),

    TargetMet = maps:get(target_met, maps:get(summary, Report)),
    case TargetMet of
        true -> io:format("✓ Target (2-5x improvement) MET~n");
        false -> io:format("✗ Target (2-5x improvement) NOT MET~n")
    end,

    ok.

%%====================================================================
%% Benchmark Implementations
%%====================================================================

%% Benchmark single requests (no batching)
benchmark_single_requests(NumOps) ->
    % Simple echo executor
    Executor = fun(Request) ->
        {ok, Request}
    end,

    StartTime = erlang:monotonic_time(microsecond),
    MemoryBefore = erlang:memory(total),

    % Send requests one by one
    Latencies = lists:map(fun(I) ->
        ReqStart = erlang:monotonic_time(microsecond),
        _Result = Executor({I, <<"test_method">>, #{index => I}}),
        ReqEnd = erlang:monotonic_time(microsecond),
        ReqEnd - ReqStart
    end, lists:seq(1, NumOps)),

    EndTime = erlang:monotonic_time(microsecond),
    MemoryAfter = erlang:memory(total),

    % Calculate metrics
    DurationUs = EndTime - StartTime,
    DurationS = DurationUs / 1_000_000,
    Throughput = NumOps / DurationS,

    Percentiles = calculate_percentiles(Latencies),

    #{
        strategy => <<"single_requests">>,
        duration_s => round_float(DurationS, 2),
        throughput_msg_per_s => round_float(Throughput, 2),
        latency_p50_us => round_float(maps:get(p50, Percentiles), 1),
        latency_p95_us => round_float(maps:get(p95, Percentiles), 1),
        latency_p99_us => round_float(maps:get(p99, Percentiles), 1),
        memory_delta_mib => round_float((MemoryAfter - MemoryBefore) / (1024 * 1024), 1),
        precision => <<"microsecond">>,
        scope => <<"per_node">>
    }.

%% Benchmark batched requests
benchmark_batched_requests(NumOps, Strategy) ->
    % Batch executor (simulates batch processing)
    Executor = fun(Requests) ->
        % Simulate batch processing (slightly faster per request)
        [{ok, Req} || Req <- Requests]
    end,

    {ok, Batcher} = erlmcp_batch:start_link(Executor, #{
        strategy => Strategy,
        parallel_workers => 4
    }),

    StartTime = erlang:monotonic_time(microsecond),
    MemoryBefore = erlang:memory(total),

    % Send all requests
    Refs = lists:map(fun(I) ->
        ReqStart = erlang:monotonic_time(microsecond),
        {ok, Ref} = erlmcp_batch:add_request(Batcher,
            <<"test_method">>,
            #{index => I, start_time => ReqStart}),
        Ref
    end, lists:seq(1, NumOps)),

    % Flush final batch
    erlmcp_batch:flush(Batcher),

    % Collect all results and calculate latencies
    Latencies = lists:map(fun(Ref) ->
        receive
            {batch_result, Ref, {ok, Result}} ->
                EndTime = erlang:monotonic_time(microsecond),
                StartTime2 = maps:get(start_time, maps:get(params, Result)),
                EndTime - StartTime2
        after 30000 ->
            0  % Timeout
        end
    end, Refs),

    EndTime = erlang:monotonic_time(microsecond),
    MemoryAfter = erlang:memory(total),

    % Get batch statistics
    Stats = erlmcp_batch:get_stats(Batcher),
    erlmcp_batch:stop(Batcher),

    % Calculate metrics
    DurationUs = EndTime - StartTime,
    DurationS = DurationUs / 1_000_000,
    Throughput = NumOps / DurationS,

    Percentiles = calculate_percentiles(Latencies),

    StrategyName = case Strategy of
        {size, N} -> <<"size_", (integer_to_binary(N))/binary>>;
        {time, Ms} -> <<"time_", (integer_to_binary(Ms))/binary, "ms">>;
        {adaptive, _} -> <<"adaptive">>;
        _ -> <<"unknown">>
    end,

    #{
        strategy => StrategyName,
        duration_s => round_float(DurationS, 2),
        throughput_msg_per_s => round_float(Throughput, 2),
        latency_p50_us => round_float(maps:get(p50, Percentiles), 1),
        latency_p95_us => round_float(maps:get(p95, Percentiles), 1),
        latency_p99_us => round_float(maps:get(p99, Percentiles), 1),
        memory_delta_mib => round_float((MemoryAfter - MemoryBefore) / (1024 * 1024), 1),
        batch_stats => #{
            total_batches => maps:get(total_batches, Stats),
            avg_batch_size => round_float(maps:get(avg_batch_size, Stats), 1),
            avg_batch_latency_us => round_float(maps:get(avg_latency_us, Stats), 1)
        },
        precision => <<"microsecond">>,
        scope => <<"per_node">>
    }.

%%====================================================================
%% Helper Functions
%%====================================================================

calculate_percentiles(Latencies) ->
    Sorted = lists:sort(Latencies),
    SortedTuple = list_to_tuple(Sorted),
    Len = length(Sorted),

    P50 = lists:nth(max(1, Len div 2), Sorted),
    P95 = lists:nth(max(1, round(Len * 0.95)), Sorted),
    P99 = lists:nth(max(1, round(Len * 0.99)), Sorted),

    #{
        p50 => P50,
        p95 => P95,
        p99 => P99
    }.

round_float(Float, Decimals) when is_float(Float) ->
    list_to_float(io_lib:format("~.*f", [Decimals, Float]));
round_float(Int, _Decimals) when is_integer(Int) ->
    float(Int);
round_float(Float, _Decimals) ->
    Float.

best_strategy(SizeImprovement, TimeImprovement, AdaptiveImprovement) ->
    Max = lists:max([SizeImprovement, TimeImprovement, AdaptiveImprovement]),
    if
        Max == SizeImprovement -> <<"size_based">>;
        Max == TimeImprovement -> <<"time_based">>;
        true -> <<"adaptive">>
    end.

capture_environment() ->
    {OtpRelease, _} = string:to_integer(erlang:system_info(otp_release)),
    {ErtsVersion, _} = case string:to_integer(erlang:system_info(version)) of
        {error, _} -> {0, []};
        Result -> Result
    end,

    #{
        otp_release => OtpRelease,
        erts_version => ErtsVersion,
        schedulers => erlang:system_info(schedulers_online),
        os => list_to_binary(os:type_to_string()),
        arch => list_to_binary(erlang:system_info(system_architecture))
    }.

os_type_to_string() ->
    case os:type() of
        {unix, darwin} -> "darwin";
        {unix, linux} -> "linux";
        {win32, nt} -> "windows";
        {Family, Name} -> atom_to_list(Family) ++ "_" ++ atom_to_list(Name)
    end.

write_report(Filename, Report) ->
    % Ensure directory exists
    filelib:ensure_dir(Filename),

    % Encode as JSON
    Json = jsx:encode(Report, [space, indent]),

    % Write to file
    file:write_file(Filename, Json).
