%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive Observability Performance Benchmark for erlmcp
%%%
%%% This benchmark suite measures:
%%% - OTEL overhead and tracing impact
%%% - Metrics collection throughput
%%% - Tracing performance
%%% - Chaos engineering overhead
%%% - Registry routing performance (target: 553K msg/s)
%%% - Queue performance (target: 971K msg/s)
%%% - Connection scalability (target: 40-50K connections)
%%% - Regression analysis against Jan 2026 baselines
%%%
%%% == Usage ==
%%% ```erlang
%%% %% Run full observability benchmark
%%% erlmcp_bench_observability:run(<<"observability_full_2026_02_01">>).
%%%
%%% %% Run individual components
%%% erlmcp_bench_observability:run_otel_benchmark(<<"otel_overhead">>).
%%% erlmcp_bench_observability:run_metrics_benchmark(<<"metrics_throughput">>).
%%% erlmcp_bench_observability:run_tracing_benchmark(<<"tracing_impact">>).
%%% erlmcp_bench_observability:run_chaos_benchmark(<<"chaos_overhead">>).
%%% erlmcp_bench_observability:run_scalability_benchmark(<<"connection_scaling">>).
%%%
%%% %% Regression analysis
%%% erlmcp_bench_observability:run_regression_analysis().
%%% ```
%%%
%%% == Expected Results ==
%%% - OTEL overhead: <5% performance impact
%%% - Metrics throughput: >1M metrics/sec
%%% - Tracing impact: <10% latency increase
%%% - Chaos overhead: <15% performance degradation
%%% - Registry: 553K+ msg/s (baseline)
%%% - Queue: 971K+ msg/s (baseline)
%%% - Connections: 40K+ per node (baseline)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_observability).

-export([run/1, run_otel_benchmark/1, run_metrics_benchmark/1, run_tracing_benchmark/1,
         run_chaos_benchmark/1, run_scalability_benchmark/1, run_regression_analysis/0]).
-export([init_benchmark/1, run_phase/3, analyze_results/2, generate_report/2]).

-include_lib("opentelemetry/include/otel_tracer.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% API Functions
%%====================================================================

-spec run(binary()) -> map().
run(WorkloadId) ->
    io:format("Starting comprehensive observability benchmark: ~p~n", [WorkloadId]),

    %% Initialize benchmark environment
    {ok, _} = application:ensure_all_started(erlmcp),
    {ok, _} = application:ensure_all_started(opentelemetry),

    %% Initialize OTEL with minimal configuration
    ok = erlmcp_otel:init(#{
        service_name => <<"erlmcp-bench">>,
        exporters => [dummy],  %% Use dummy exporter to avoid network overhead
        sampling => always_on,
        resource_attributes => #{<<"service.name">> => <<"erlmcp-bench">>}
    }),

    %% Run all benchmark phases
    Results = #{
        otel_overhead => run_otel_benchmark(WorkloadId),
        metrics_throughput => run_metrics_benchmark(WorkloadId),
        tracing_impact => run_tracing_benchmark(WorkloadId),
        chaos_overhead => run_chaos_benchmark(WorkloadId),
        scalability => run_scalability_benchmark(WorkloadId),
        regression => run_regression_analysis()
    },

    %% Generate comprehensive report
    Report = generate_report(WorkloadId, Results),

    %% Save results
    save_results(WorkloadId, Results, Report),

    io:format("Observability benchmark completed: ~p~n", [WorkloadId]),
    Report.

-spec run_otel_benchmark(binary()) -> map().
run_otel_benchmark(WorkloadId) ->
    io:format("Running OTEL overhead benchmark...~n"),

    %% Benchmark parameters
    OpCount = 100000,
    WarmupCount = 10000,

    %% Baseline without OTEL
    io:format("Measuring baseline without OTEL...~n"),
    BaselineResults = measure_baseline_operations(OpCount, WarmupCount, []),

    %% With OTEL enabled
    io:format("Measuring with OTEL enabled...~n"),
    OtelResults = measure_otel_operations(OpCount, WarmupCount, []),

    %% Calculate overhead
    Overhead = calculate_overhead(BaselineResults, OtelResults),

    #{
        workload_id => WorkloadId,
        component => <<"otel_overhead">>,
        operations => OpCount,
        baseline => BaselineResults,
        with_otel => OtelResults,
        overhead => Overhead,
        timestamp => erlang:system_time(millisecond)
    }.

-spec run_metrics_benchmark(binary()) -> map().
run_metrics_benchmark(WorkloadId) ->
    io:format("Running metrics collection throughput benchmark...~n"),

    %% Test different metrics rates
    Rates = [10000, 50000, 100000, 200000, 500000],
    DurationPerRate = 5000,  % 5 seconds per rate

    Results = lists:map(fun(Rate) ->
        io:format("Testing metrics rate: ~p metrics/sec~n", [Rate]),
        Result = measure_metrics_throughput(Rate, DurationPerRate),
        Result
    end, Rates),

    %% Find sustainable throughput
    Sustainable = find_sustainable_throughput(Results),

    #{
        workload_id => WorkloadId,
        component => <<"metrics_throughput">>,
        test_rates => Rates,
        results => Results,
        sustainable_throughput => Sustainable,
        timestamp => erlang:system_time(millisecond)
    }.

-spec run_tracing_benchmark(binary()) -> map().
run_tracing_benchmark(WorkloadId) ->
    io:format("Running tracing impact benchmark...~n"),

    %% Test different span depths and complexities
    SpanConfigs = [
        {<<"simple_span">>, 1, 10},
        {<<"nested_span_3">>, 3, 100},
        {<<"nested_span_5">>, 5, 50},
        {<<"complex_span">>, 10, 25}
    ],

    Results = lists:map(fun({SpanName, Depth, Count}) ->
        io:format("Testing ~p depth=~p count=~p~n", [SpanName, Depth, Count]),
        Result = measure_tracing_impact(SpanName, Depth, Count),
        Result
    end, SpanConfigs),

    #{
        workload_id => WorkloadId,
        component => <<"tracing_impact">>,
        span_configs => SpanConfigs,
        results => Results,
        timestamp => erlang:system_time(millisecond)
    }.

-spec run_chaos_benchmark(binary()) -> map().
run_chaos_benchmark(WorkloadId) ->
    io:format("Running chaos engineering performance benchmark...~n"),

    %% Test various chaos scenarios
    ChaosScenarios = [
        {<<"process_kill">>, 1000, 5},
        {<<"network_latency">>, 5000, 10},
        {<<"cpu_spike">>, 3000, 15},
        {<<"memory_pressure">>, 2000, 20}
    ],

    Results = lists:map(fun({Scenario, Duration, Intensity}) ->
        io:format("Testing chaos scenario: ~p (~p ms, ~p intensity)~n",
                 [Scenario, Duration, Intensity]),
        Result = measure_chaos_impact(Scenario, Duration, Intensity),
        Result
    end, ChaosScenarios),

    #{
        workload_id => WorkloadId,
        component => <<"chaos_overhead">>,
        scenarios => ChaosScenarios,
        results => Results,
        timestamp => erlang:system_time(millisecond)
    }.

-spec run_scalability_benchmark(binary()) -> map().
run_scalability_benchmark(WorkloadId) ->
    io:format("Running connection scalability benchmark...~n"),

    %% Test different connection counts
    ConnectionCounts = [1000, 5000, 10000, 20000, 40000, 50000],
    DurationPerTest = 10000,  % 10 seconds per test

    Results = lists:map(fun(Count) ->
        io:format("Testing ~p connections...~n", [Count]),
        Result = measure_connection_scaling(Count, DurationPerTest),
        Result
    end, ConnectionCounts),

    %% Find maximum sustainable connections
    MaxSustainable = find_max_sustainable_connections(Results),

    #{
        workload_id => WorkloadId,
        component => <<"connection_scaling">>,
        connection_counts => ConnectionCounts,
        results => Results,
        max_sustainable => MaxSustainable,
        timestamp => erlang:system_time(millisecond)
    }.

-spec run_regression_analysis() -> map().
run_regression_analysis() ->
    io:format("Running regression analysis against Jan 2026 baselines...~n"),

    %% Load baseline data
    BaselineData = load_baseline_data(),

    %% Run current benchmarks
    CurrentResults = #{
        registry => measure_registry_performance(),
        queue => measure_queue_performance(),
        overall => measure_overall_performance()
    },

    %% Compare against baselines
    RegressionAnalysis = compare_with_baselines(CurrentResults, BaselineData),

    #{
        component => <<"regression_analysis">>,
        baseline_data => BaselineData,
        current_results => CurrentResults,
        analysis => RegressionAnalysis,
        timestamp => erlang:system_time(millisecond)
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

measure_baseline_operations(OpCount, WarmupCount, _ExtraOpts) ->
    %% Measure operations without OTEL
    Start = erlang:monotonic_time(millisecond),

    %% Warmup
    lists:foreach(fun(_) -> dummy_operation() end, lists:seq(1, WarmupCount)),

    %% Measurement
    OpStart = erlang:monotonic_time(millisecond),
    lists:foreach(fun(_) -> dummy_operation() end, lists:seq(1, OpCount)),
    OpEnd = erlang:monotonic_time(millisecond),

    Duration = OpEnd - OpStart,
    Throughput = OpCount / (Duration / 1000.0),

    #{
        operations => OpCount,
        duration_ms => Duration,
        throughput_per_sec => Throughput,
        latency_avg_us => (Duration * 1000) / OpCount
    }.

measure_otel_operations(OpCount, WarmupCount, _ExtraOpts) ->
    %% Measure operations with OTEL tracing
    Start = erlang:monotonic_time(millisecond),

    %% Warmup with spans
    lists:foreach(fun(_) ->
        erlmcp_otel:with_span(<<"warmup">>, #{}, fun() -> dummy_operation() end)
    end, lists:seq(1, WarmupCount)),

    %% Measurement with spans
    OpStart = erlang:monotonic_time(millisecond),
    lists:foreach(fun(_) ->
        erlmcp_otel:with_span(<<"benchmark">>, #{<<"operation.id">> => erlang:system_time(millisecond)},
                              fun() -> dummy_operation() end)
    end, lists:seq(1, OpCount)),
    OpEnd = erlang:monotonic_time(millisecond),

    Duration = OpEnd - OpStart,
    Throughput = OpCount / (Duration / 1000.0),

    #{
        operations => OpCount,
        duration_ms => Duration,
        throughput_per_sec => Throughput,
        latency_avg_us => (Duration * 1000) / OpCount
    }.

calculate_overhead(Baseline, Otel) ->
    BaselineThroughput = maps:get(throughput_per_sec, Baseline),
    OtelThroughput = maps:get(throughput_per_sec, Otel),

    OverheadPercent = ((OtelThroughput - BaselineThroughput) / BaselineThroughput) * 100,

    #{
        throughput_degradation_percent => OverheadPercent,
        baseline_throughput => BaselineThroughput,
        otel_throughput => OtelThroughput,
        overhead_acceptable => abs(OverheadPercent) < 5.0
    }.

measure_metrics_throughput(TargetRate, DurationMs) ->
    Start = erlang:monotonic_time(millisecond),
    End = Start + DurationMs,

    RecordCount = 0,

    %% Record metrics at target rate
    erlmcp_metrics:start_link(),

    record_metrics_at_rate(Start, End, TargetRate, 0),

    %% Calculate actual throughput
    ActualDuration = erlang:monotonic_time(millisecond) - Start,
    ActualRate = RecordCount / (ActualDuration / 1000.0),

    #{
        target_rate => TargetRate,
        actual_rate => ActualRate,
        duration_ms => ActualDuration,
        efficiency => ActualRate / TargetRate,
        success => ActualRate >= TargetRate * 0.95  % 95% efficiency threshold
    }.

record_metrics_at_rate(_Start, _End, _Rate, Count) when Count >= 100000 ->
    ok;
record_metrics_at_rate(Start, End, Rate, Count) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    if CurrentTime >= End ->
        ok;
    true ->
        %% Record metric
        erlmcp_metrics:record_transport_operation(
            bench, stdio, <<"record">>, 1.0
        ),

        %% Calculate delay to maintain target rate
        NextTarget = Start + (Count + 1) * (1000 / Rate),
        Delay = max(0, NextTarget - CurrentTime),

        timer:sleep(trunc(Delay)),
        record_metrics_at_rate(Start, End, Rate, Count + 1)
    end.

measure_tracing_impact(SpanName, Depth, Count) ->
    %% Measure impact of nested tracing
    Start = erlang:monotonic_time(millisecond),

    %% Create nested spans
    SpanCtx = erlmcp_otel:start_span(SpanName, #{
        <<"benchmark.depth">> => Depth,
        <<"benchmark.count">> => Count
    }),

    %% Execute operations within spans
    lists:foreach(fun(_) ->
        nested_operation_with_spans(Depth)
    end, lists:seq(1, Count)),

    erlmcp_otel:end_span(SpanCtx),

    Duration = erlang:monotonic_time(millisecond) - Start,
    Throughput = Count / (Duration / 1000.0),

    #{
        span_name => SpanName,
        depth => Depth,
        count => Count,
        duration_ms => Duration,
        throughput_per_sec => Throughput,
        latency_avg_us => (Duration * 1000) / Count
    }.

nested_operation_with_spans(0) ->
    dummy_operation();
nested_operation_with_spans(Depth) ->
    SpanCtx = erlmcp_otel:start_span(<<"nested">>, #{
        <<"depth">> => Depth
    }),
    try
        nested_operation_with_spans(Depth - 1)
    after
        erlmcp_otel:end_span(SpanCtx)
    end.

measure_chaos_impact(Scenario, Duration, Intensity) ->
    %% Start baseline measurement
    BaselineStart = erlang:monotonic_time(millisecond),
    BaselineOps = measure_operations_during_time(Duration),
    BaselineEnd = erlang:monotonic_time(millisecond),

    %% Start chaos scenario
    ChaosPid = spawn_chaos_scenario(Scenario, Intensity),

    %% Measure during chaos
    ChaosStart = erlang:monotonic_time(millisecond),
    ChaosOps = measure_operations_during_time(Duration),
    ChaosEnd = erlang:monotonic_time(millisecond),

    %% Stop chaos
    exit(ChaosPid, normal),

    %% Calculate impact
    BaselineThroughput = BaselineOps / (Duration / 1000.0),
    ChaosThroughput = ChaosOps / (Duration / 1000.0),
    Impact = ((ChaosThroughput - BaselineThroughput) / BaselineThroughput) * 100,

    #{
        scenario => Scenario,
        duration_ms => Duration,
        intensity => Intensity,
        baseline_throughput => BaselineThroughput,
        chaos_throughput => ChaosThroughput,
        impact_percent => Impact,
        severity => case abs(Impact) of
                       I when I < 5 -> low;
                       I when I < 15 -> medium;
                       _ -> high
                   end
    }.

spawn_chaos_scenario(<<"process_kill">>, Intensity) ->
    %% Kill random processes
    spawn(fun() ->
        lists:foreach(fun(_) ->
            %% Kill a non-critical process
            case erlang:system_info(process_count) of
                Count when Count > 10 ->
                    Pids = erlang:processes(),
                    RandomPid = lists:nth(rand:uniform(length(Pids)), Pids),
                    if RandomPid =/= self() ->
                        exit(RandomPid, kill);
                    true ->
                        ok
                    end;
                _ ->
                    ok
            end,
            timer:sleep(trunc(1000 / Intensity))
        end, lists:seq(1, 100))
    end);

spawn_chaos_scenario(<<"network_latency">>, Intensity) ->
    %% Simulate network latency
    spawn(fun() ->
        lists:foreach(fun(_) ->
            %% Add artificial delay
            Delay = rand:uniform(100) / Intensity,
            timer:sleep(trunc(Delay)),
            ok
        end, lists:seq(1, 1000))
    end);

spawn_chaos_scenario(<<"cpu_spike">>, Intensity) ->
    %% CPU-intensive workload
    spawn(fun() ->
        lists:foreach(fun(_) ->
            %% CPU-heavy computation
            lists:foldl(fun(X, Acc) -> X * Acc end, 1, lists:seq(1, 1000 * Intensity)),
            timer:sleep(1)
        end, lists:seq(1, 100))
    end);

spawn_chaos_scenario(<<"memory_pressure">>, Intensity) ->
    %% Memory-intensive workload
    spawn(fun() ->
        lists:foreach(fun(_) ->
            %% Allocate large amounts of memory
            _BigList = lists:seq(1, 1000 * Intensity),
            timer:sleep(1)
        end, lists:seq(1, 100))
    end).

measure_operations_during_time(DurationMs) ->
    Start = erlang:monotonic_time(millisecond),
    End = Start + DurationMs,
    Count = measure_ops_loop(Start, End, 0),
    Count.

measure_ops_loop(_Start, End, Count) when Count >= 100000 ->
    Count;
measure_ops_loop(Start, End, Count) ->
    Current = erlang:monotonic_time(millisecond),
    if Current >= End ->
        Count;
    true ->
        dummy_operation(),
        measure_ops_loop(Start, End, Count + 1)
    end.

measure_connection_scaling(Count, DurationMs) ->
    %% Create connections and measure performance
    Start = erlang:monotonic_time(millisecond),

    %% Create connection processes
    ConnectionPids = lists:map(fun(_) ->
        spawn(fun() ->
            %% Simulate connection work
            timer:sleep(DurationMs),
            dummy_operation()
        end)
    end, lists:seq(1, Count)),

    %% Measure message throughput during connections
    MsgCount = measure_message_throughput(DurationMs),

    %% Cleanup
    lists:foreach(fun(Pid) -> exit(Pid, normal) end, ConnectionPids),

    Duration = erlang:monotonic_time(millisecond) - Start,
    Throughput = MsgCount / (Duration / 1000.0),

    #{
        connection_count => Count,
        duration_ms => Duration,
        message_throughput => Throughput,
        throughput_per_connection => Throughput / Count,
        success => Throughput >= 1000  % Minimum acceptable throughput
    }.

measure_message_throughput(DurationMs) ->
    Start = erlang:monotonic_time(millisecond),
    End = Start + DurationMs,
    count_messages(Start, End, 0).

count_messages(_Start, End, Count) when Count >= 1000000 ->
    Count;
count_messages(Start, End, Count) ->
    Current = erlang:monotonic_time(millisecond),
    if Current >= End ->
        Count;
    true ->
        %% Send a message to self and count it
        self() ! {benchmark_msg, Count},
        receive
            {benchmark_msg, _} -> ok
        after 0 -> ok
        end,
        count_messages(Start, End, Count + 1)
    end.

measure_registry_performance() ->
    %% Measure registry routing performance
    OpCount = 100000,
    WarmupCount = 10000,

    %% Initialize registry
    ok = erlmcp_registry:start_link(),

    %% Register some test servers
    lists:foreach(fun(I) ->
        ServerId = list_to_binary("server_" ++ integer_to_list(I)),
        Pid = spawn(fun() -> server_loop() end),
        ok = erlmcp_registry:register_server(ServerId, Pid, #{})
    end, lists:seq(1, 100)),

    %% Warmup
    lists:foreach(fun(I) ->
        ServerId = list_to_binary("server_" ++ integer_to_list(I)),
        erlmcp_registry:find_server(ServerId)
    end, lists:seq(1, WarmupCount)),

    %% Measurement
    Start = erlang:monotonic_time(millisecond),
    lists:foreach(fun(I) ->
        ServerId = list_to_binary("server_" ++ integer_to_list(I)),
        erlmcp_registry:find_server(ServerId)
    end, lists:seq(1, OpCount)),
    End = erlang:monotonic_time(millisecond),

    Duration = End - Start,
    Throughput = OpCount / (Duration / 1000.0),

    #{
        operations => OpCount,
        duration_ms => Duration,
        throughput_per_sec => Throughput,
        latency_avg_us => (Duration * 1000) / OpCount,
        meets_baseline => Throughput >= 553000  % 553K msg/s baseline
    }.

measure_queue_performance() ->
    %% Measure queue performance
    OpCount = 100000,
    WarmupCount = 10000,

    %% Create test queue
    Queue = queue:new(),

    %% Warmup
    lists:foreach(fun(_) ->
        queue:in(test_data(), Queue)
    end, lists:seq(1, WarmupCount)),

    %% Measurement
    Start = erlang:monotonic_time(millisecond),
    lists:foreach(fun(_) ->
        _ = queue:in(test_data(), Queue),
        _ = queue:out(Queue)
    end, lists:seq(1, OpCount)),
    End = erlang:monotonic_time(millisecond),

    Duration = End - Start,
    Throughput = OpCount / (Duration / 1000.0),

    #{
        operations => OpCount,
        duration_ms => Duration,
        throughput_per_sec => Throughput,
        latency_avg_us => (Duration * 1000) / OpCount,
        meets_baseline => Throughput >= 971000  % 971K msg/s baseline
    }.

measure_overall_performance() ->
    %% Measure overall system performance
    OpCount = 100000,
    WarmupCount = 10000,

    %% Warmup
    lists:foreach(fun(_) ->
        complete_operation()
    end, lists:seq(1, WarmupCount)),

    %% Measurement
    Start = erlang:monotonic_time(millisecond),
    lists:foreach(fun(_) ->
        complete_operation()
    end, lists:seq(1, OpCount)),
    End = erlang:monotonic_time(millisecond),

    Duration = End - Start,
    Throughput = OpCount / (Duration / 1000.0),

    #{
        operations => OpCount,
        duration_ms => Duration,
        throughput_per_sec => Throughput,
        latency_avg_us => (Duration * 1000) / OpCount
    }.

load_baseline_data() ->
    %% Load Jan 2026 baseline data
    BaselineFile = "/Users/sac/erlmcp/bench/results/v2_1_baseline/EXECUTIVE_SUMMARY.md",
    case file:read_file(BaselineFile) of
        {ok, Content} ->
            parse_baseline_data(Content);
        {error, _} ->
            %% Use hardcoded baseline
            #{
                registry => 553000,  % 553K msg/s
                queue => 971000,     % 971K msg/s
                overall => 2520000   % 2.52M msg/s
            }
    end.

parse_baseline_data(Content) ->
    %% Parse baseline data from markdown content
    Lines = binary:split(Content, <<"\n">>, [global]),
    parse_baseline_lines(Lines, #{}).

parse_baseline_lines([], Acc) ->
    Acc;
parse_baseline_lines([Line | Rest], Acc) ->
    %% Look for throughput values
    case binary:match(Line, <<"Registry">>) of
        nomatch ->
            parse_baseline_lines(Rest, Acc);
        _ ->
            %% Extract registry value
            case re:run(Line, <<"Registry: ([0-9,]+K) ops/sec">>, [{capture, all_but_first, binary}]) of
                nomatch ->
                    parse_baseline_lines(Rest, Acc);
                {match, [Match]} ->
                    Value = parse_throughput_value(Match),
                    parse_baseline_lines(Rest, Acc#{registry => Value})
            end
    end.

parse_throughput_value(<<"K">>) -> 1000;
parse_throughput_value(<<"M">>) -> 1000000;
parse_throughput_value(Value) ->
    %% Parse numeric value with K/M suffix
    case binary:split(Value, <<["K", "M"]>>) of
        [Num] -> binary_to_integer(Num) * 1000;
        [Num, _] -> binary_to_integer(Num) * 1000000
    end.

compare_with_baselines(Current, Baseline) ->
    RegistryCurrent = maps:get(throughput_per_sec, maps:get(registry, Current)),
    RegistryBaseline = maps:get(registry, Baseline),
    RegistryRegression = ((RegistryCurrent - RegistryBaseline) / RegistryBaseline) * 100,

    QueueCurrent = maps:get(throughput_per_sec, maps:get(queue, Current)),
    QueueBaseline = maps:get(queue, Baseline),
    QueueRegression = ((QueueCurrent - QueueBaseline) / QueueBaseline) * 100,

    OverallCurrent = maps:get(throughput_per_sec, maps:get(overall, Current)),
    OverallBaseline = maps:get(overall, Baseline),
    OverallRegression = ((OverallCurrent - OverallBaseline) / OverallBaseline) * 100,

    #{
        registry => #{
            current => RegistryCurrent,
            baseline => RegistryBaseline,
            regression_percent => RegistryRegression,
            acceptable => abs(RegistryRegression) =< 10.0
        },
        queue => #{
            current => QueueCurrent,
            baseline => QueueBaseline,
            regression_percent => QueueRegression,
            acceptable => abs(QueueRegression) =< 10.0
        },
        overall => #{
            current => OverallCurrent,
            baseline => OverallBaseline,
            regression_percent => OverallRegression,
            acceptable => abs(OverallRegression) =< 10.0
        },
        overall_regression_acceptable =>
            abs(RegistryRegression) =< 10.0 andalso
            abs(QueueRegression) =< 10.0 andalso
            abs(OverallRegression) =< 10.0
    }.

find_sustainable_throughput(Results) ->
    %% Find the highest rate with >95% efficiency
    lists:foldl(fun(Result, Acc) ->
        case maps:get(efficiency, Result) of
            Eff when Eff >= 0.95 andalso maps:get(actual_rate, Result) > maps:get(actual_rate, Acc) ->
                Result;
            _ ->
                Acc
        end
    end, #{actual_rate => 0}, Results).

find_max_sustainable_connections(Results) ->
    %% Find maximum connections with >90% success rate
    lists:foldl(fun(Result, Acc) ->
        case maps:get(success, Result) of
            true when maps:get(connection_count, Result) > maps:get(connection_count, Acc) ->
                Result;
            _ ->
                Acc
        end
    end, #{connection_count => 0}, Results).

dummy_operation() ->
    %% Simple dummy operation for benchmarking
    lists:foldl(fun(X, Acc) -> X * Acc end, 1, lists:seq(1, 10)).

complete_operation() ->
    %% Complete system operation
    erlmcp_otel:with_span(<<"complete_operation">>, #{}, fun() ->
        dummy_operation(),
        timer:sleep(1)
    end).

server_loop() ->
    receive
        _ -> server_loop()
    end.

test_data() ->
    binary:copy(<<"test_data">>, 100).

generate_report(WorkloadId, Results) ->
    #{
        workload_id => WorkloadId,
        timestamp => erlang:system_time(millisecond),
        summary => generate_summary(Results),
        recommendations => generate_recommendations(Results),
        overall_grade => calculate_overall_grade(Results)
    }.

generate_summary(Results) ->
    OtelOverhead = maps:get(overhead, maps:get(otel_overhead, Results)),
    MetricsResults = maps:get(results, maps:get(metrics_throughput, Results)),
    TracingResults = maps:get(results, maps:get(tracing_impact, Results)),
    ChaosResults = maps:get(results, maps:get(chaos_overhead, Results)),
    ScalingResults = maps:get(results, maps:get(scalability, Results)),
    RegressionResults = maps:get(analysis, maps:get(regression, Results)),

    #{
        otel_overhead_percent => maps:get(throughput_degradation_percent, OtelOverhead),
        metrics_max_throughput => find_max_throughput(MetricsResults),
        tracing_max_impact => find_max_tracing_impact(TracingResults),
        chaos_max_impact => find_max_chaos_impact(ChaosResults),
        scaling_max_connections => maps:get(max_sustainable, ScalingResults),
        registry_regression => maps:get(regression_percent, maps:get(registry, RegressionResults)),
        queue_regression => maps:get(regression_percent, maps:get(queue, RegressionResults)),
        overall_regression => maps:get(regression_percent, maps:get(overall, RegressionResults))
    }.

generate_recommendations(Results) ->
    Recommendations = [],

    %% OTEL recommendations
    OtelOverhead = maps:get(overhead, maps:get(otel_overhead, Results)),
    case maps:get(overhead_acceptable, OtelOverhead) of
        false ->
            [{otel_overhead, "Consider sampling strategies or async exporters"} | Recommendations];
        true ->
            Recommendations
    end,

    %% Metrics recommendations
    MetricsResults = maps:get(results, maps:get(metrics_throughput, Results)),
    case find_min_efficiency(MetricsResults) of
        Eff when Eff < 0.9 ->
            [{metrics, "Optimize metrics collection or increase batching"} | Recommendations];
        _ ->
            Recommendations
    end,

    %% Tracing recommendations
    TracingResults = maps:get(results, maps:get(tracing_impact, Results)),
    case find_max_tracing_impact(TracingResults) of
        Impact when Impact > 20 ->
            [{tracing, "Reduce span depth or implement sampling"} | Recommendations];
        _ ->
            Recommendations
    end,

    %% Chaos recommendations
    ChaosResults = maps:get(results, maps:get(chaos_overhead, Results)),
    case find_max_chaos_impact(ChaosResults) of
        Impact when Impact > 30 ->
            [{chaos, "Improve resilience to chaos scenarios"} | Recommendations];
        _ ->
            Recommendations
    end,

    %% Scaling recommendations
    ScalingResults = maps:get(results, maps:get(scalability, Results)),
    MaxConn = maps:get(max_sustainable, ScalingResults),
    case MaxConn of
        Conn when Conn < 40000 ->
            [{scaling, "Optimize connection handling for better scalability"} | Recommendations];
        _ ->
            Recommendations
    end,

    %% Regression recommendations
    RegressionResults = maps:get(analysis, maps:get(regression, Results)),
    case maps:get(overall_regression_acceptable, RegressionResults) of
        false ->
            [{regression, "Performance regression detected - investigate and optimize"} | Recommendations];
        true ->
            Recommendations
    end.

find_max_throughput(Results) ->
    lists:foldl(fun(Result, Acc) ->
        case maps:get(actual_rate, Result) > Acc of
            true -> maps:get(actual_rate, Result);
            false -> Acc
        end
    end, 0, Results).

find_min_efficiency(Results) ->
    lists:foldl(fun(Result, Acc) ->
        case maps:get(efficiency, Result) < Acc of
            true -> maps:get(efficiency, Result);
            false -> Acc
        end
    end, 1.0, Results).

find_max_tracing_impact(Results) ->
    lists:foldl(fun(Result, Acc) ->
        case maps:get(latency_avg_us, Result) > Acc of
            true -> maps:get(latency_avg_us, Result);
            false -> Acc
        end
    end, 0, Results).

find_max_chaos_impact(Results) ->
    lists:foldl(fun(Result, Acc) ->
        case maps:get(impact_percent, Result) > Acc of
            true -> maps:get(impact_percent, Result);
            false -> Acc
        end
    end, 0, Results).

calculate_overall_grade(Results) ->
    %% Calculate overall performance grade
    OtelGrade = case maps:get(overhead_acceptable, maps:get(overhead, maps:get(otel_overhead, Results))) of
                    true -> "A";
                    false -> "B"
                end,

    MetricsGrade = case find_min_efficiency(maps:get(results, maps:get(metrics_throughput, Results))) of
                      Eff when Eff >= 0.95 -> "A";
                      Eff when Eff >= 0.8 -> "B";
                      _ -> "C"
                  end,

    TracingGrade = case find_max_tracing_impact(maps:get(results, maps:get(tracing_impact, Results))) of
                      Lat when Lat =< 100 -> "A";
                      Lat when Lat =< 200 -> "B";
                      _ -> "C"
                  end,

    ChaosGrade = case find_max_chaos_impact(maps:get(results, maps:get(chaos_overhead, Results))) of
                    Impact when Impact =< 10 -> "A";
                    Impact when Impact =< 20 -> "B";
                    _ -> "C"
                end,

    ScalingGrade = case maps:get(connection_count, maps:get(max_sustainable, maps:get(results, maps:get(scalability, Results)))) of
                     Conn when Conn >= 50000 -> "A";
                     Conn when Conn >= 40000 -> "B";
                     _ -> "C"
                 end,

    RegressionGrade = case maps:get(overall_regression_acceptable, maps:get(analysis, maps:get(regression, Results))) of
                        true -> "A";
                        false -> "C"
                     end,

    #{
        components => #{
            otel => OtelGrade,
            metrics => MetricsGrade,
            tracing => TracingGrade,
            chaos => ChaosGrade,
            scaling => ScalingGrade,
            regression => RegressionGrade
        },
        overall => calculate_final_grade([OtelGrade, MetricsGrade, TracingGrade, ChaosGrade, ScalingGrade, RegressionGrade])
    }.

calculate_final_grade(Grades) ->
    %% Count grades
    CountA = lists:foldl(fun("A", Acc) -> Acc + 1; (_, Acc) -> Acc end, 0, Grades),
    CountB = lists:foldl(fun("B", Acc) -> Acc + 1; (_, Acc) -> Acc end, 0, Grades),
    CountC = lists:foldl(fun("C", Acc) -> Acc + 1; (_, Acc) -> Acc end, 0, Grades),

    %% Determine final grade
    if
        CountA >= 5 -> "A";
        CountA >= 4 -> "B";
        CountB >= 4 -> "B";
        true -> "C"
    end.

save_results(WorkloadId, Results, Report) ->
    %% Save results to JSON file
    Timestamp = erlang:system_time(millisecond),
    FileName = list_to_binary("observability_bench_" ++ integer_to_list(Timestamp) ++ ".json"),
    FilePath = filename:join(["/Users/sac/erlmcp/bench/results", FileName]),

    ResultData = #{
        workload_id => WorkloadId,
        timestamp => Timestamp,
        results => Results,
        report => Report,
        environment => #{
            os => erlang:system_info(system),
            otp_version => erlang:system_info(otp_release),
            schedulers => erlang:system_info(schedulers_online),
            cpu_count => erlang:system_info(logical_processors)
        }
    },

    JsonData = jsx:encode(ResultData),
    file:write_file(FilePath, JsonData),

    io:format("Results saved to: ~p~n", [FilePath]).