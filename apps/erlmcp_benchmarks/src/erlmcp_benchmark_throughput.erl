%% @doc Enterprise Throughput Benchmark Module
%%
%% Implements high-throughput benchmarking for 10K+ requests/second requirements
%% with comprehensive metrics collection and analysis.
%%
%% @end
-module(erlmcp_benchmark_throughput).

-export([run/1]).

%%====================================================================
%% RECORD DEFINITIONS
%%====================================================================

-record(test_config, {
    duration :: pos_integer(),
    target_throughput :: pos_integer(),
    ramp_up_time :: pos_integer(),
    measurement_interval :: pos_integer(),
    scenarios :: list()
}).

-record(measurement, {
    timestamp :: pos_integer(),
    request_rate :: pos_integer(),
    actual_throughput :: float(),
    success_rate :: float(),
    average_latency :: float(),
    p95_latency :: float(),
    p99_latency :: float(),
    error_rate :: float(),
    resource_usage :: map()
}).

-record(benchmark_result, {
    average_throughput :: float(),
    max_throughput :: float(),
    sustainable_throughput :: float(),
    success_rate :: float(),
    latency_metrics :: map(),
    resource_metrics :: map(),
    measurements :: [#measurement{}],
    bottlenecks :: list(),
    recommendations :: list()
}).

%%====================================================================
%% PUBLIC API
%%====================================================================

run(TestConfig) when is_map(TestConfig) ->
    %% Convert map to record
    Config = #test_config{
        duration = maps:get(duration, TestConfig, 300000),
        target_throughput = maps:get(target_throughput, TestConfig, 10000),
        ramp_up_time = maps:get(ramp_up_time, TestConfig, 30000),
        measurement_interval = maps:get(measurement_interval, TestConfig, 5000),
        scenarios = maps:get(scenarios, TestConfig, [])
    },

    lager:info("Starting throughput benchmark: ~p req/s target",
              [Config#test_config.target_throughput]),

    %% Initialize benchmark environment
    ok = initialize_benchmark_environment(),

    %% Execute each scenario
    ScenarioResults = lists:map(fun execute_scenario/1, Config#test_config.scenarios),

    %% Analyze results across all scenarios
    FinalResult = analyze_throughput_results(ScenarioResults),

    %% Cleanup
    ok = cleanup_benchmark_environment(),

    lager:info("Throughput benchmark completed: ~p req/s average",
              [FinalResult#benchmark_result.average_throughput]),

    FinalResult.

%%====================================================================
%% PRIVATE FUNCTIONS
%%====================================================================

execute_scenario({ScenarioType, ScenarioConfig}) ->
    lager:info("Executing scenario: ~p", [ScenarioType]),

    %% Initialize load generator
    LoadGen = start_load_generator(ScenarioType, ScenarioConfig),

    %% Start metrics collection
    MetricsCollector = start_metrics_collection(),

    %% Run benchmark with dynamic rate adjustment
    Measurements = run_dynamic_benchmark(
        LoadGen,
        MetricsCollector,
        ScenarioConfig
    ),

    %% Stop services
    stop_load_generator(LoadGen),
    stop_metrics_collection(MetricsCollector),

    %% Analyze scenario results
    analyze_scenario_results(ScenarioType, Measurements).

initialize_benchmark_environment() ->
    %% Initialize monitoring services
    ok = erlmcp_benchmark_monitor:start(),

    %% Initialize performance counters
    ok = erlmcp_performance_counters:init(),

    %% Configure network parameters for high throughput
    ok = configure_network_for_throughput(),

    %% Initialize distributed tracing
    ok = erlmcp_tracing:init(),

    ok.

cleanup_benchmark_environment() ->
    %% Stop monitoring services
    ok = erlmcp_benchmark_monitor:stop(),

    %% Save performance counters
    ok = erlmcp_performance_counters:save(),

    %% Reset network configuration
    ok = reset_network_configuration(),

    %% Cleanup tracing
    ok = erlmcp_tracing:cleanup(),

    ok.

configure_network_for_throughput() ->
    %% Increase socket buffer sizes
    application:set_env(kernel, inet_dist_buffer_size, 8192),
    application:set_env(kernel, inet_dist_listen_min, 9100),
    application:set_env(kernel, inet_dist_listen_max, 9105),

    %% Configure TCP parameters
    case os:type() of
        {unix, _} ->
            %% Linux-specific optimizations
            os:cmd("sysctl -w net.core.rmem_max=134217728"),
            os:cmd("sysctl -w net.core.wmem_max=134217728"),
            os:cmd("sysctl -w net.ipv4.tcp_rmem='4096 87380 134217728'"),
            os:cmd("sysctl -w net.ipv4.tcp_wmem='4096 65536 134217728'");
        _ ->
            ok
    end,

    ok.

reset_network_configuration() ->
    %% Reset to default values
    application:set_env(kernel, inet_dist_buffer_size, 8192),
    application:set_env(kernel, inet_dist_listen_min, 9100),
    application:set_env(kernel, inet_dist_listen_max, 9105),

    %% Reset TCP parameters
    case os:type() of
        {unix, _} ->
            os:cmd("sysctl -w net.core.rmem_max=134217728"),
            os:cmd("sysctl -w net.core.wmem_max=134217728");
        _ ->
            ok
    end,

    ok.

start_load_generator(ScenarioType, ScenarioConfig) ->
    %% Create load generator process
    LoadGenPid = spawn(fun() -> load_generator_loop(ScenarioType, ScenarioConfig) end),

    %% Configure load generator
    ok = configure_load_generator(LoadGenPid, ScenarioType, ScenarioConfig),

    LoadGenPid.

configure_load_generator(LoadGenPid, ScenarioType, ScenarioConfig) ->
    %% Send configuration to load generator
    LoadGenPid ! {configure, ScenarioType, ScenarioConfig},

    %% Wait for confirmation
    receive
        {configured, LoadGenPid} -> ok
    after 5000 ->
        lager:error("Timeout configuring load generator"),
        error(timeout)
    end.

load_generator_loop(ScenarioType, ScenarioConfig) ->
    %% Initialize state
    State = initialize_load_generator_state(ScenarioType, ScenarioConfig),

    %% Main load generation loop
    load_generation_loop(State).

initialize_load_generator_state(ScenarioType, ScenarioConfig) ->
    #{
        type => ScenarioType,
        config => ScenarioConfig,
        requests => [],
        responses => [],
        current_rate => 0,
        target_rate => maps:get(initial_rate, ScenarioConfig, 100),
        ramp_up => true,
        start_time => erlang:monotonic_time(millisecond)
    }.

load_generation_loop(State) ->
    receive
        {configure, NewType, NewConfig} ->
            %% Reconfigure load generator
            NewState = State#{
                type => NewType,
                config => NewConfig,
                current_rate => maps:get(initial_rate, NewConfig, 100),
                ramp_up => true
            },
            load_generation_loop(NewState);

        {adjust_rate, NewRate} ->
            %% Adjust request rate
            NewState = State#{current_rate => NewRate},
            load_generation_loop(NewState);

        {get_metrics, From} ->
            %% Return current metrics
            Metrics = calculate_load_generator_metrics(State),
            From ! {metrics, Metrics},
            load_generation_loop(State);

        stop ->
            %% Stop load generation
            ok;

        _ ->
            %% Handle other messages
            load_generation_loop(State)
    end.

calculate_load_generator_metrics(State) ->
    #{
        requests_sent => length(State#(requests)),
        responses_received => length(State#(responses)),
        current_rate => State#(current_rate),
        success_rate => calculate_success_rate(State),
        average_latency => calculate_average_latency(State),
        timestamp => erlang:monotonic_time(millisecond)
    }.

calculate_success_rate(State) ->
    Total = length(State#(responses)),
    Success = length([R || R <- State#(responses), R =:= success]),
    case Total > 0 of
        true -> Success / Total;
        false -> 0.0
    end.

calculate_average_latency(State) ->
    Latencies = [L || {_, L, _} <- State#(responses)],
    case Latencies of
        [] -> 0.0;
        _ -> lists:sum(Latencies) / length(Latencies)
    end.

start_metrics_collection() ->
    %% Start metrics collection process
    MetricsPid = spawn(fun() -> metrics_collection_loop([]) end),
    MetricsPid.

metrics_collection_loop(Measurements) ->
    receive
        {add_measurement, Measurement} ->
            NewMeasurements = [Measurement | Measurements],
            metrics_collection_loop(NewMeasurements);

        {get_measurements, From} ->
            From ! {measurements, lists:reverse(Measurements)},
            metrics_collection_loop(Measurements);

        {clear_measurements} ->
            metrics_collection_loop([]);

        stop ->
            ok
    end.

run_dynamic_benchmark(LoadGen, MetricsCollector, ScenarioConfig) ->
    %% Calculate measurement intervals
    Duration = maps:get(duration, ScenarioConfig, 300000),
    Interval = maps:get(measurement_interval, ScenarioConfig, 5000),
    NumIntervals = Duration div Interval,

    %% Initialize metrics collection
    Measurements = [],

    %% Main benchmark loop
    benchmark_loop(
        LoadGen,
        MetricsCollector,
        Measurements,
        NumIntervals,
        Interval,
        ScenarioConfig
    ).

benchmark_loop(LoadGen, MetricsCollector, Measurements, 0, _Interval, _ScenarioConfig) ->
    %% Benchmark complete
    MetricsCollector ! {get_measurements, self()},
    receive
        {measurements, M} -> M
    end;

benchmark_loop(LoadGen, MetricsCollector, Measurements, Remaining, Interval, ScenarioConfig) ->
    %% Start of interval
    StartTime = erlang:monotonic_time(millisecond),

    %% Send rate adjustment if needed
    case should_adjust_rate(LoadGen, ScenarioConfig, Measurements) of
        true ->
            NewRate = calculate_new_rate(LoadGen, ScenarioConfig, Measurements),
            LoadGen ! {adjust_rate, NewRate};
        false ->
            ok
    end,

    %% Wait for measurement interval
    wait_for_interval(Interval, LoadGen, MetricsCollector),

    %% Collect measurements
    LoadGen ! {get_metrics, self()},
    receive
        {metrics, Metrics} ->
            NewMeasurement = #measurement{
                timestamp = erlang:monotonic_time(millisecond),
                request_rate = Metrics#(current_rate),
                actual_throughput = Metrics#(success_rate) * Metrics#(current_rate),
                success_rate = Metrics#(success_rate),
                average_latency = Metrics#(average_latency),
                error_rate = 1.0 - Metrics#(success_rate),
                resource_usage = collect_resource_usage()
            }
    end,

    MetricsCollector ! {add_measurement, NewMeasurement},

    %% Continue loop
    benchmark_loop(
        LoadGen,
        MetricsCollector,
        [NewMeasurement | Measurements],
        Remaining - 1,
        Interval,
        ScenarioConfig
    ).

wait_for_interval(Interval, LoadGen, MetricsCollector) ->
    EndTime = erlang:monotonic_time(millisecond) + Interval,
    wait_loop(EndTime, LoadGen, MetricsCollector).

wait_loop(EndTime, LoadGen, MetricsCollector) ->
    Now = erlang:monotonic_time(millisecond),
    case Now >= EndTime of
        true ->
            ok;
        false ->
            receive
                {metrics_request, From} ->
                    LoadGen ! {get_metrics, self()},
                    receive
                        {metrics, Metrics} ->
                            From ! {metrics_response, Metrics}
                    end,
                    wait_loop(EndTime, LoadGen, MetricsCollector)
            after 100 ->
                wait_loop(EndTime, LoadGen, MetricsCollector)
            end
    end.

should_adjust_rate(LoadGen, ScenarioConfig, Measurements) ->
    %% Check if we need to adjust the rate based on performance
    case Measurements of
        [] -> true;
        [_] -> true;
        [Prev, Current | _] ->
            SuccessRateThreshold = maps:get(success_rate_threshold, ScenarioConfig, 0.95),
            CurrentSuccessRate = Current#measurement.success_rate,
            PrevSuccessRate = Prev#measurement.success_rate,

            %% Adjust if success rate is too low or improving
            CurrentSuccessRate < SuccessRateThreshold orelse
            (CurrentSuccessRate > PrevSuccessRate and CurrentSuccessRate < SuccessRateThreshold)
    end.

calculate_new_rate(LoadGen, ScenarioConfig, Measurements) ->
    %% Calculate new request rate based on current performance
    case Measurements of
        [] ->
            maps:get(initial_rate, ScenarioConfig, 100);
        [Current | _] ->
            SuccessRate = Current#measurement.success_rate,
            TargetSuccessRate = maps:get(success_rate_threshold, ScenarioConfig, 0.95),
            CurrentRate = Current#measurement.request_rate,

            if
                SuccessRate >= TargetSuccessRate ->
                    %% Increase rate
                    min(CurrentRate * 1.1, maps:get(max_rate, ScenarioConfig, 50000));
                SuccessRate < TargetSuccessRate ->
                    %% Decrease rate
                    max(CurrentRate * 0.9, maps:get(min_rate, ScenarioConfig, 10))
            end
    end.

collect_resource_usage() ->
    %% Collect system resource usage
    #{cpu => collect_cpu_usage(),
      memory => collect_memory_usage(),
      network => collect_network_usage(),
      disk => collect_disk_usage()}.

collect_cpu_usage() ->
    %% Get CPU usage percentage
    case erlmcp_system_monitor:get_cpu_usage() of
        {ok, Usage} -> Usage;
        {error, _} -> 0.0
    end.

collect_memory_usage() ->
    %% Get memory usage
    case erlmcp_system_monitor:get_memory_usage() of
        {ok, Usage} -> Usage;
        {error, _} -> 0.0
    end.

collect_network_usage() ->
    %% Get network usage
    case erlmcp_system_monitor:get_network_usage() of
        {ok, Usage} -> Usage;
        {error, _} -> 0.0
    end.

collect_disk_usage() ->
    %% Get disk usage
    case erlmcp_system_monitor:get_disk_usage() of
        {ok, Usage} -> Usage;
        {error, _} -> 0.0
    end.

stop_load_generator(LoadGenPid) ->
    LoadGenPid ! stop,
    receive
        stopped -> ok
    after 5000 ->
        lager:error("Timeout stopping load generator")
    end.

stop_metrics_collection(MetricsCollector) ->
    MetricsCollector ! stop,
    receive
        stopped -> ok
    after 5000 ->
        lager:error("Timeout stopping metrics collection")
    end.

analyze_scenario_results(ScenarioType, Measurements) ->
    %% Calculate throughput statistics
    Throughputs = [M#measurement.actual_throughput || M <- Measurements],
    AverageThroughput = lists:sum(Throughputs) / length(Throughputs),
    MaxThroughput = lists:max(Throughputs),

    %% Calculate latency statistics
    Latencies = [M#measurement.average_latency || M <- Measurements],
    P95Latency = calculate_percentile(Latencies, 95),
    P99Latency = calculate_percentile(Latencies, 99),

    %% Calculate success rate
    SuccessRates = [M#measurement.success_rate || M <- Measurements],
    AverageSuccessRate = lists:sum(SuccessRates) / length(SuccessRates),

    %% Identify bottlenecks
    Bottlenecks = identify_throughput_bottlenecks(Measurements),

    %% Generate recommendations
    Recommendations = generate_throughput_recommendations(ScenarioType, Measurements, Bottlenecks),

    #benchmark_result{
        average_throughput = AverageThroughput,
        max_throughput = MaxThroughput,
        sustainable_throughput = calculate_sustainable_throughput(Measurements),
        success_rate = AverageSuccessRate,
        latency_metrics = #{
            average => lists:sum(Latencies) / length(Latencies),
            p95 => P95Latency,
            p99 => P99Latency,
            max => lists:max(Latencies)
        },
        resource_metrics = analyze_resource_metrics(Measurements),
        measurements = Measurements,
        bottlenecks = Bottlenecks,
        recommendations = Recommendations
    }.

calculate_percentile(List, Percentile) ->
    case List of
        [] -> 0.0;
        _ ->
            Sorted = lists:sort(List),
            Index = trunc(Percentile / 100 * length(Sorted)),
            lists:nth(Index + 1, Sorted)
    end.

calculate_sustainable_throughput(Measurements) ->
    %% Calculate the highest throughput that can be sustained with >95% success rate
    SustainableMeasurements = [M || M <- Measurements, M#measurement.success_rate >= 0.95],
    case SustainableMeasurements of
        [] -> 0.0;
        _ ->
            Throughputs = [M#measurement.actual_throughput || M <- SustainableMeasurements],
            lists:sum(Throughputs) / length(Throughputs)
    end.

analyze_resource_metrics(Measurements) ->
    %% Analyze resource usage patterns
    CPUUsages = [M#measurement.resource_usage#(cpu) || M <- Measurements],
    MemoryUsages = [M#measurement.resource_usage#(memory) || M <- Measurements],

    #{
        cpu => #{
            average => lists:sum(CPUUsages) / length(CPUUsages),
            max => lists:max(CPUUsages),
            p95 => calculate_percentile(CPUUsages, 95)
        },
        memory => #{
            average => lists:sum(MemoryUsages) / length(MemoryUsages),
            max => lists:max(MemoryUsages),
            p95 => calculate_percentile(MemoryUsages, 95)
        }
    }.

identify_throughput_bottlenecks(Measurements) ->
    %% Identify performance bottlenecks
    Bottlenecks = [],

    %% Check CPU bottlenecks
    HighCPU = [M || M <- Measurements, M#measurement.resource_usage#(cpu) > 80.0],
    case HighCPU of
        [_|_] -> [cpu_bottleneck | Bottlenecks];
        [] -> Bottlenecks
    end,

    %% Check memory bottlenecks
    HighMemory = [M || M <- Measurements, M#measurement.resource_usage#(memory) > 90.0],
    case HighMemory of
        [_|_] -> [memory_bottleneck | Bottlenecks];
        [] -> Bottlenecks
    end,

    %% Check network bottlenecks
    HighNetwork = [M || M <- Measurements, M#measurement.resource_usage#(network) > 80.0],
    case HighNetwork of
        [_|_] -> [network_bottleneck | Bottlenecks];
        [] -> Bottlenecks
    end,

    Bottlenecks.

generate_throughput_recommendations(ScenarioType, Measurements, Bottlenecks) ->
    %% Generate optimization recommendations
    Recommendations = [],

    %% General throughput recommendations
    ThroughputRecommendations = generate_throughput_recommendations_based_on_data(ScenarioType, Measurements),

    %% Bottleneck-specific recommendations
    BottleneckRecommendations = generate_bottleneck_recommendations(Bottlenecks),

    ThroughputRecommendations ++ BottleneckRecommendations.

generate_throughput_recommendations_based_on_data(ScenarioType, Measurements) ->
    case ScenarioType of
        simple_message ->
            [{increase_concurrency, "Consider increasing concurrency for simple message processing"},
             {optimize_serialization, "Optimize message serialization for better throughput"}];
        complex_query ->
            [{add_caching, "Add caching layer for frequently accessed queries"},
             {optimize_queries, "Optimize database queries with proper indexing"}];
        bulk_operation ->
            [{increase_batch_size, "Increase batch size for better throughput"},
             {parallel_processing, "Implement parallel processing for bulk operations"}];
        streaming_data ->
            [{stream_optimization, "Optimize streaming data handling with backpressure"},
             {buffer_management, "Implement proper buffer management for streaming"}];
        _ -> []
    end.

generate_bottleneck_recommendations(Bottlenecks) ->
    lists:foldl(fun(Bottleneck, Acc) ->
        case Bottleneck of
            cpu_bottleneck ->
                [{cpu_optimization, "Consider CPU optimization: distribute load, optimize algorithms"} | Acc];
            memory_bottleneck ->
                [{memory_optimization, "Memory optimization: implement connection pooling, reduce memory overhead"} | Acc];
            network_bottleneck ->
                [{network_optimization, "Network optimization: increase buffer sizes, use compression"} | Acc];
            _ -> Acc
        end
    end, [], Bottlenecks).

analyze_throughput_results(ScenarioResults) ->
    %% Combine results from all scenarios
    Throughputs = [R#benchmark_result.average_throughput || R <- ScenarioResults],
    MaxThroughputs = [R#benchmark_result.max_throughput || R <- ScenarioResults],
    SustainableThroughputs = [R#benchmark_result.sustainable_throughput || R <- ScenarioResults],
    SuccessRates = [R#benchmark_result.success_rate || R <- ScenarioResults],

    #benchmark_result{
        average_throughput = lists:sum(Throughputs) / length(Throughputs),
        max_throughput = lists:max(MaxThroughputs),
        sustainable_throughput = lists:sum(SustainableThroughputs) / length(SustainableThroughputs),
        success_rate = lists:sum(SuccessRates) / length(SuccessRates),
        latency_metrics = aggregate_latency_metrics(ScenarioResults),
        resource_metrics = aggregate_resource_metrics(ScenarioResults),
        measurements = aggregate_measurements(ScenarioResults),
        bottlenecks = aggregate_bottlenecks(ScenarioResults),
        recommendations = aggregate_recommendations(ScenarioResults)
    }.

aggregate_latency_metrics(ScenarioResults) ->
    %% Aggregate latency metrics from all scenarios
    AllLatencies = lists:foldl(fun(R, Acc) ->
        Latencies = R#benchmark_result.latency_metrics,
        [Latencies#(average), Latencies#(p95), Latencies#(p99), Latencies#(max)] ++ Acc
    end, [], ScenarioResults),

    #{
        average = lists:sum(AllLatencies) / length(AllLatencies),
        p95 = calculate_percentile(AllLatencies, 95),
        p99 = calculate_percentile(AllLatencies, 99),
        max = lists:max(AllLatencies)
    }.

aggregate_resource_metrics(ScenarioResults) ->
    %% Aggregate resource metrics from all scenarios
    CPUUsages = lists:foldl(fun(R, Acc) ->
        Metrics = R#benchmark_result.resource_metrics,
        [Metrics#(cpu#(average)), Metrics#(cpu#(max)), Metrics#(cpu#(p95))] ++ Acc
    end, [], ScenarioResults),

    MemoryUsages = lists:foldl(fun(R, Acc) ->
        Metrics = R#benchmark_result.resource_metrics,
        [Metrics#(memory#(average)), Metrics#(memory#(max)), Metrics#(memory#(p95))] ++ Acc
    end, [], ScenarioResults),

    #{
        cpu => #{
            average => lists:sum(CPUUsages) / length(CPUUsages),
            max => lists:max(CPUUsages),
            p95 => calculate_percentile(CPUUsages, 95)
        },
        memory => #{
            average => lists:sum(MemoryUsages) / length(MemoryUsages),
            max => lists:max(MemoryUsages),
            p95 => calculate_percentile(MemoryUsages, 95)
        }
    }.

aggregate_measurements(ScenarioResults) ->
    %% Flatten all measurements
    lists:foldl(fun(R, Acc) ->
        R#benchmark_result.measurements ++ Acc
    end, [], ScenarioResults).

aggregate_bottlenecks(ScenarioResults) ->
    %% Collect all unique bottlenecks
    Bottlenecks = lists:foldl(fun(R, Acc) ->
        R#benchmark_result.bottlenecks ++ Acc
    end, [], ScenarioResults),

    %% Remove duplicates while preserving order
    lists:reverse(lists:foldl(fun(B, Acc) ->
        case lists:member(B, Acc) of
            true -> Acc;
            false -> [B | Acc]
        end
    end, [], Bottlenecks)).

aggregate_recommendations(ScenarioResults) ->
    %% Aggregate recommendations from all scenarios
    Recommendations = lists:foldl(fun(R, Acc) ->
        R#benchmark_result.recommendations ++ Acc
    end, [], ScenarioResults),

    %% Remove duplicates and group by type
    UniqueRecs = lists:reverse(lists:foldl(fun(R, Acc) ->
        case lists:member({R#(1), R#(2)}, Acc) of
            true -> Acc;
            false -> [R | Acc]
        end
    end, [], Recommendations)),

    UniqueRecs.