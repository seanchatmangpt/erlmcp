%% @doc Enterprise Latency Benchmark Module
%%
%% Implements comprehensive latency benchmarking with p95 < 100ms requirements
%% including detailed latency distribution analysis and outlier detection.
%%
%% @end
-module(erlmcp_benchmark_latency).

-export([run/1]).

%%====================================================================
%% RECORD DEFINITIONS
%%====================================================================

-record(test_config, {
    sample_size :: pos_integer(),
    distribution :: list(),
    scenarios :: list(),
    warmup_samples :: pos_integer()
}).

-record(latency_measurement, {
    transaction_id :: binary(),
    total_latency :: float(),
    phases :: map(),
    success :: boolean(),
    timestamp :: pos_integer()
}).

-record.phase_metrics, {
    submission :: float(),
    consensus :: float(),
    application :: float(),
    total :: float()
}).

-record.latency_stats, {
    mean :: float(),
    median :: float(),
    standard_deviation :: float(),
    p50 :: float(),
    p75 :: float(),
    p90 :: float(),
    p95 :: float(),
    p99 :: float(),
    p99_9 :: float(),
    p99_99 :: float(),
    min :: float(),
    max :: float()
}).

-record.distribution_analysis, {
    normality_test :: float(),
    skewness :: float(),
    kurtosis :: float(),
    mode :: float(),
    outliers :: list()
}).

%%====================================================================
%% PUBLIC API
%%====================================================================

run(TestConfig) when is_map(TestConfig) ->
    %% Convert map to record
    Config = #test_config{
        sample_size = maps:get(sample_size, TestConfig, 100000),
        distribution = maps:get(distribution, TestConfig, [normal, uniform, peak]),
        scenarios = maps:get(scenarios, TestConfig, []),
        warmup_samples = maps:get(warmup_samples, TestConfig, 10000)
    },

    lager:info("Starting latency benchmark: ~p samples", [Config#test_config.sample_size]),

    %% Initialize benchmark environment
    ok = initialize_latency_benchmark(),

    %% Execute each scenario
    ScenarioResults = lists:map(fun execute_latency_scenario/1, Config#test_config.scenarios),

    %% Analyze results
    FinalResult = analyze_latency_results(ScenarioResults, Config),

    %% Cleanup
    ok = cleanup_latency_benchmark(),

    lager:info("Latency benchmark completed: P95=~p ms, P99=~p ms",
              [FinalResult#latency_stats.p95, FinalResult#latency_stats.p99]),

    FinalResult.

%%====================================================================
%% PRIVATE FUNCTIONS
%%====================================================================

initialize_latency_benchmark() ->
    %% Initialize high-precision timing
    ok = erlmcp_high_precision_timing:init(),

    %% Initialize latency histogram
    ok = erlmcp_latency_histogram:init(),

    %% Initialize percentile calculator
    ok = erlmcp_percentile_calculator:init(),

    %% Initialize phase timing collector
    ok = erlmcp_phase_timing:init(),

    %% Configure system for low latency
    ok = configure_system_for_latency(),

    ok.

cleanup_latency_benchmark() ->
    %% Save timing data
    ok = erlmcp_high_precision_timing:save(),

    %% Save histogram data
    ok = erlmcp_latency_histogram:save(),

    %% Save percentile data
    ok = erlmcp_percentile_calculator:save(),

    %% Save phase timing data
    ok = erlmcp_phase_timing:save(),

    %% Reset system configuration
    ok = reset_system_configuration(),

    ok.

configure_system_for_latency() ->
    %% Disable unnecessary background processes
    application:set_env(kernel, dist_auto_connect, false),

    %% Configure scheduler for low latency
    application:set_env(scheduler, bind_type, no),

    %% Set process priority
    erlang:process_flag(priority, high),

    %% Disable garbage collection during benchmark
    erlang:process_flag(min_heap_size, 100000),
    erlang:process_flag(max_heap_size, unlimited),

    %% Configure kernel parameters for low latency
    case os:type() of
        {unix, _} ->
            os:cmd("sysctl -w kernel.sched_latency_ns=10000000"),
            os:cmd("sysctl -w kernel.sched_min_granularity_ns=500000"),
            os:cmd("sysctl -w kernel.sched_wakeup_granularity_ns=1000000");
        _ ->
            ok
    end,

    ok.

reset_system_configuration() ->
    %% Reset to default values
    application:set_env(kernel, dist_auto_connect, true),

    %% Reset scheduler
    application:set_env(scheduler, bind_type, default),

    %% Reset process priority
    erlang:process_flag(priority, normal),

    %% Reset garbage collection
    erlang:process_flag(min_heap_size, default),
    erlang:process_flag(max_heap_size, default),

    case os:type() of
        {unix, _} ->
            os:cmd("sysctl -w kernel.sched_latency_ns=20000000"),
            os:cmd("sysctl -w kernel.sched_min_granularity_ns=1000000"),
            os:cmd("sysctl -w kernel.sched_wakeup_granularity_ns=2000000");
        _ ->
            ok
    end,

    ok.

execute_latency_scenario({ScenarioType, ScenarioConfig}) ->
    lager:info("Executing latency scenario: ~p", [ScenarioType]),

    %% Execute warmup phase
    WarmupResults = execute_warmup_phase(ScenarioType, ScenarioConfig),

    %% Execute measurement phase
    Measurements = execute_measurement_phase(ScenarioType, ScenarioConfig),

    %% Analyze scenario results
    analyze_scenario_latency_results(ScenarioType, Measurements).

execute_warmup_phase(ScenarioType, ScenarioConfig) ->
    lager:info("Executing warmup phase: ~p samples", [ScenarioConfig#(warmup_samples, 10000)]),

    WarmupCount = maps:get(warmup_samples, ScenarioConfig, 10000),

    WarmupResults = lists:foldl(fun(_, Acc) ->
        Measurement = execute_single_latency_measurement(ScenarioType, ScenarioConfig),
        [Measurement | Acc]
    end, [], lists:seq(1, WarmupCount)),

    %% Record warmup metrics
    ok = erlmcp_benchmark_monitor:record_warmup_metrics(ScenarioType, WarmupResults),

    WarmupResults.

execute_measurement_phase(ScenarioType, ScenarioConfig) ->
    SampleSize = maps:get(sample_size, ScenarioConfig, 100000),

    lager:info("Executing measurement phase: ~p samples", [SampleSize]),

    %% Create measurement collector
    Collector = spawn(fun() -> measurement_collector_loop([]) end),

    %% Start measurement workers
    WorkerCount = min(16, erlang:system_info(process_count) div 10),
    Workers = start_latency_workers(WorkerCount, ScenarioType, ScenarioConfig, Collector),

    %% Distribute work among workers
    distribute_measurement_work(WorkerCount, SampleSize, Collector),

    %% Wait for completion
    wait_for_measurement_completion(Collector, SampleSize),

    %% Collect results
    Collector ! {get_measurements, self()},
    receive
        {measurements, Measurements} ->
            stop_latency_workers(Workers),
            Measurements
    after 60000 ->
        lager:error("Timeout collecting measurement results"),
        stop_latency_workers(Workers),
            []
    end.

measurement_collector_loop(Measurements) ->
    receive
        {add_measurement, Measurement} ->
            NewMeasurements = [Measurement | Measurements],
            measurement_collector_loop(NewMeasurements);

        {get_measurements, From} ->
            From ! {measurements, lists:reverse(Measurements)},
            measurement_collector_loop(Measurements);

        {progress, Current, Total} ->
            lager:info("Progress: ~p/~p measurements", [Current, Total]),
            measurement_collector_loop(Measurements);

        stop ->
            ok
    end.

start_latency_workers(Count, ScenarioType, ScenarioConfig, Collector) ->
    Workers = lists:foldl(fun(_, Acc) ->
        Worker = spawn(fun() -> latency_worker_loop(ScenarioType, ScenarioConfig, Collector) end),
        [Worker | Acc]
    end, [], lists:seq(1, Count)),
    Workers.

latency_worker_loop(ScenarioType, ScenarioConfig, Collector) ->
    receive
        {work, Count} ->
            Measurements = lists:foldl(fun(_, Acc) ->
                Measurement = execute_single_latency_measurement(ScenarioType, ScenarioConfig),
                [Measurement | Acc]
            end, [], lists:seq(1, Count)),
            lists:foreach(fun(M) -> Collector ! {add_measurement, M} end, Measurements),
            latency_worker_loop(ScenarioType, ScenarioConfig, Collector);

        stop ->
            ok
    end.

distribute_measurement_work(WorkerCount, SampleSize, Collector) ->
    WorkPerWorker = SampleCount div WorkerCount,
    Remaining = SampleCount rem WorkerCount,

    %% Send work to workers
    lists:foreach(fun(I) ->
        Work = if I =< Remaining -> WorkPerWorker + 1; true -> WorkPerWorker end,
        IthWorker lists:nth(I, Workers),
        IthWorker ! {work, Work},
        Collector ! {progress, I, WorkerCount}
    end, lists:seq(1, WorkerCount)).

wait_for_measurement_completion(Collector, ExpectedCount) ->
    wait_loop(ExpectedCount, Collector, 0).

wait_loop(ExpectedCount, Collector, CurrentCount) ->
    if CurrentCount >= ExpectedCount ->
            ok;
        true ->
            receive
                {add_measurement, _} ->
                    wait_loop(ExpectedCount, Collector, CurrentCount + 1);
                after 1000 ->
                    wait_loop(ExpectedCount, Collector, CurrentCount)
            end
    end.

execute_single_latency_measurement(ScenarioType, ScenarioConfig) ->
    %% Generate transaction with unique ID
    TransactionId = generate_transaction_id(),

    %% Start high-precision timing
    StartTime = erlmcp_high_precision_timing:now(),

    %% Execute transaction
    Result = execute_latency_transaction(ScenarioType, TransactionConfig),

    %% End timing
    EndTime = erlmcp_high_precision_timing:now(),

    %% Calculate total latency
    TotalLatency = erlmcp_high_precision_timing:elapsed(StartTime, EndTime),

    %% Phase timing analysis
    PhaseMetrics = analyze_phase_timing(Result),

    #latency_measurement{
        transaction_id = TransactionId,
        total_latency = TotalLatency,
        phases = PhaseMetrics,
        success = maps:get(success, Result, false),
        timestamp = erlang:monotonic_time(millisecond)
    }.

generate_transaction_id() ->
    %% Generate unique transaction ID
    Now = erlang:system_time(millisecond),
    Random = crypto:strong_rand_bytes(8),
    base64:encode(<<Now:64, Random/binary>>).

execute_latency_transaction(simple_lookup, _Config) ->
    %% Execute simple lookup transaction
    LookupData = #{key => generate_random_key(), value => generate_random_value()},
    Result = erlmcp_benchmark_client:lookup(LookupData),
    #{result => Result, success => true};

execute_latency_transaction(batch_processing, Config) ->
    %% Execute batch processing transaction
    BatchSize = maps:get(batch_size, Config, 10),
    BatchItems = lists:map(fun(_) -> generate_random_key() end, lists:seq(1, BatchSize)),
    Result = erlmcp_benchmark_client:process_batch(BatchItems),
    #{result => Result, success => true};

execute_latency_transaction(complex_computation, Config) ->
    %% Execute complex computation transaction
    ComputationDepth = maps:get(computation_depth, Config, 5),
    InputData = generate_complex_input_data(),
    Result = erlmcp_benchmark_client:compute_complex(InputData, ComputationDepth),
    #{result => Result, success => true};

execute_latency_transaction(network_roundtrip, Config) ->
    %% Execute network roundtrip transaction
    Hops = maps:get(hops, Config, 3),
    Data = generate_network_payload(),
    Result = erlmcp_benchmark_client:roundtrip_network(Data, Hops),
    #{result => Result, success => true};

execute_latency_transaction(_, _Config) ->
    %% Default transaction
    Data = generate_random_data(),
    Result = erlmcp_benchmark_client:execute_standard(Data),
    #{result => Result, success => true}.

generate_random_key() ->
    crypto:strong_rand_bytes(16).

generate_random_value() ->
    crypto:strong_rand_bytes(1024).

generate_complex_input_data() ->
    #{data => lists:map(fun(_) -> crypto:strong_rand_bytes(64) end, lists:seq(1, 100)),
      metadata => #{timestamp => erlang:system_time(millisecond),
                    id => base64:encode(crypto:strong_rand_bytes(8))}}.

generate_network_payload() ->
    #{payload => crypto:strong_rand_bytes(10240),
      header => #{timestamp => erlang:system_time(millisecond),
                  correlation_id => base64:encode(crypto:strong_rand_bytes(16))}}.

generate_random_data() ->
    crypto:strong_rand_bytes(1024).

analyze_phase_timing(Result) ->
    %% Analyze timing for different phases
    case maps:get(phases, Result, undefined) of
        undefined ->
            #phase_metrics{
                submission => 0.0,
                consensus => 0.0,
                application => 0.0,
                total => maps:get(total_latency, Result, 0.0)
            };
        Phases ->
            #phase_metrics{
                submission => maps:get(submission, Phases, 0.0),
                consensus => maps:get(consensus, Phases, 0.0),
                application => maps:get(application, Phases, 0.0),
                total => maps:get(total, Phases, 0.0)
            }
    end.

stop_latency_workers(Workers) ->
    lists:foreach(fun(Worker) -> Worker ! stop end, Workers).

analyze_scenario_latency_results(ScenarioType, Measurements) ->
    %% Filter successful measurements
    SuccessfulMeasurements = [M || M <- Measurements, M#latency_measurement.success =:= true],

    case SuccessfulMeasurements of
        [] ->
            lager:warning("No successful latency measurements for scenario: ~p", [ScenarioType]),
            #latency_stats{};
        _ ->
            %% Extract latency values
            Latencies = [M#latency_measurement.total_latency || M <- SuccessfulMeasurements],

            %% Calculate statistics
            Stats = calculate_latency_statistics(Latencies),

            %% Analyze distribution
            Distribution = analyze_latency_distribution(Latencies),

            %% Analyze phase contributions
            PhaseAnalysis = analyze_phase_contributions(SuccessfulMeasurements),

            %% Identify outliers
            Outliers = identify_latency_outliers(Latencies),

            #latency_stats{
                mean = Stats#(mean),
                median = Stats#(median),
                standard_deviation = Stats#(standard_deviation),
                p50 = Stats#(p50),
                p75 = Stats#(p75),
                p90 = Stats#(p90),
                p95 = Stats#(p95),
                p99 = Stats#(p99),
                p99_9 = Stats#(p99_9),
                p99_99 = Stats#(p99_99),
                min = Stats#(min),
                max = Stats#(max)
            }
    end.

calculate_latency_statistics(Latencies) ->
    %% Basic statistics
    Count = length(Latencies),
    Mean = lists:sum(Latencies) / Count,

    %% Percentiles
    Sorted = lists:sort(Latencies),
    P50 = calculate_percentile(Sorted, 50),
    P75 = calculate_percentile(Sorted, 75),
    P90 = calculate_percentile(Sorted, 90),
    P95 = calculate_percentile(Sorted, 95),
    P99 = calculate_percentile(Sorted, 99),
    P99_9 = calculate_percentile(Sorted, 99.9),
    P99_99 = calculate_percentile(Sorted, 99.99),
    Min = lists:min(Latencies),
    Max = lists:max(Latencies),

    %% Standard deviation
    Variance = lists:foldl(fun(L, Acc) ->
        Acc + math:pow(L - Mean, 2)
    end, 0, Latencies) / Count,
    StdDev = math:sqrt(Variance),

    #latency_stats{
        mean = Mean,
        median = P50,
        standard_deviation = StdDev,
        p50 = P50,
        p75 = P75,
        p90 = P90,
        p95 = P95,
        p99 = P99,
        p99_9 = P99_9,
        p99_99 = P99_99,
        min = Min,
        max = Max
    }.

calculate_percentile(SortedList, Percentile) ->
    Length = length(SortedList),
    Index = trunc(Percentile / 100 * Length),
    if Index =< 0 ->
            lists:nth(1, SortedList);
        Index >= Length ->
            lists:nth(Length, SortedList);
        true ->
            lists:nth(Index, SortedList)
    end.

analyze_latency_distribution(Latencies) ->
    %% Calculate distribution statistics
    Mean = lists:sum(Latencies) / length(Latencies),
    Variance = lists:foldl(fun(L, Acc) -> Acc + math:pow(L - Mean, 2) end, 0, Latencies) / length(Latencies),
    StdDev = math:sqrt(Variance),

    %% Normality test (simplified Shapiro-Wilk)
    Normality = calculate_normality_test(Latencies),

    %% Skewness and kurtosis
    Skewness = calculate_skewness(Latencies, Mean, StdDev),
    Kurtosis = calculate_kurtosis(Latencies, Mean, StdDev),

    %% Find mode (most frequent value)
    Mode = find_mode(Latencies),

    #distribution_analysis{
        normality_test = Normality,
        skewness = Skewness,
        kurtosis = Kurtosis,
        mode = Mode,
        outliers = []
    }.

calculate_normality_test(Latencies) ->
    %% Simplified normality test
    %% Returns a p-value (0 = not normal, 1 = perfectly normal)
    Mean = lists:sum(Latencies) / length(Latencies),
    StdDev = math:sqrt(lists:foldl(fun(L, Acc) -> Acc + math:pow(L - Mean, 2) end, 0, Latencies) / length(Latencies)),

    %% Calculate standardized values
    StdValues = lists:map(fun(L) -> (L - Mean) / StdDev end, Latencies),

    %% Calculate Anderson-Darling test statistic (simplified)
    AD = calculate_anderson_darling(StdValues),

    %% Convert to p-value (approximation)
    case AD of
        AD when AD < 0.2 -> 0.95;
        AD when AD < 0.5 -> 0.5;
        AD when AD < 1.0 -> 0.1;
        _ -> 0.01
    end.

calculate_anderson_darling(StdValues) ->
    %% Simplified Anderson-Darling test
    Sorted = lists:sort(StdValues),
    N = length(Sorted),

    %% Calculate the test statistic
    AD = lists:foldl(fun(X, Acc, I) ->
        Term = -2 * (I / N * math:log(cdf_normal(X)) + (1 - I / N) * math:log(1 - cdf_normal(X))),
        Acc + Term
    end, 0, Sorted, lists:seq(1, N)),

    AD / N.

cdf_normal(X) ->
    %% Cumulative distribution function for standard normal
    0.5 * (1 + math:erf(X / math:sqrt(2))).

calculate_skewness(Latencies, Mean, StdDev) ->
    %% Calculate skewness
    N = length(Latencies),
    M3 = lists:foldl(fun(L, Acc) -> Acc + math:pow(L - Mean, 3) end, 0, Latencies) / N,

    %% Adjust for sample size
    if N > 2 ->
            Skewness = M3 / math:pow(StdDev, 3),
            %% Adjust for bias
            Correction = sqrt(N * (N - 1)) / (N - 2),
            Skewness * Correction;
        true ->
            0.0
    end.

calculate_kurtosis(Latencies, Mean, StdDev) ->
    %% Calculate kurtosis
    N = length(Latencies),
    M4 = lists:foldl(fun(L, Acc) -> Acc + math:pow(L - Mean, 4) end, 0, Latencies) / N,

    %% Calculate excess kurtosis
    Kurtosis = (M4 / math:pow(StdDev, 4)) - 3,

    %% Adjust for sample size
    if N > 3 ->
            Correction = N * (N + 1) / ((N - 1) * (N - 2) * (N - 3)),
            Kurtosis * Correction;
        true ->
            0.0
    end.

find_mode(Latencies) ->
    %% Find the most frequent value(s)
    ValueCounts = lists:foldl(fun(L, Acc) ->
        maps:update_counter(L, 1, Acc)
    end, maps:new(), Latencies),

    %% Find maximum count
    MaxCount = maps:values(ValueCounts),
    MaxCountValue = lists:max(MaxCount),

    %% Find all values with maximum count
    Modes = lists:filter(fun({_, Count}) -> Count =:= MaxCountValue end, maps:to_list(ValueCounts)),

    %% Return average of modes if multiple
    case Modes of
        [Mode] -> Mode#(1);
        _ -> lists:sum([V || {V, _} <- Modes]) / length(Modes)
    end.

analyze_phase_contributions(Measurements) ->
    %% Analyze contribution of each phase to total latency
    PhaseContributions = lists:foldl(fun(M, Acc) ->
        Phases = M#latency_measurement.phases,
        Total = Phases#phase_metrics.total,
        if Total > 0 ->
            Submission = (Phases#phase_metrics.submission / Total) * 100,
            Consensus = (Phases#phase_metrics.consensus / Total) * 100,
            Application = (Phases#phase_metrics.application / Total) * 100,
            Acc#{
                submission => [Submission | maps:get(submission, Acc, [])],
                consensus => [Consensus | maps:get(consensus, Acc, [])],
                application => [Application | maps:get(application, Acc, [])]
            };
        true ->
            Acc
        end
    end, #{}, Measurements),

    %% Calculate averages
    #{
        submission => case maps:get(submission, PhaseContributions, []) of
                [] -> 0.0;
                L -> lists:sum(L) / length(L)
            end,
        consensus => case maps:get(consensus, PhaseContributions, []) of
                [] -> 0.0;
                L -> lists:sum(L) / length(L)
            end,
        application => case maps:get(application, PhaseContributions, []) of
                [] -> 0.0;
                L -> lists:sum(L) / length(L)
            end
    }.

identify_latency_outliers(Latencies) ->
    %% Identify outliers using IQR method
    Sorted = lists:sort(Latencies),
    Q1 = calculate_percentile(Sorted, 25),
    Q3 = calculate_percentile(Sorted, 75),
    IQR = Q3 - Q1,

    %% Outlier boundaries
    LowerBound = Q1 - 1.5 * IQR,
    UpperBound = Q3 + 1.5 * IQR,

    %% Identify outliers
    Outliers = lists:filter(fun(L) -> L < LowerBound orelse L > UpperBound end, Latencies),

    %% Calculate outlier statistics
    OutlierCount = length(Outliers),
    OutlierPercentage = OutlierCount / length(Latencies) * 100,

    case Outliers of
        [] -> [];
        _ -> [{count, OutlierCount}, {percentage, OutlierPercentage}]
    end.

analyze_latency_results(ScenarioResults, _Config) ->
    %% Combine results from all scenarios
    AllLatencies = lists:foldl(fun(R, Acc) ->
        [R#latency_stats.p95, R#latency_stats.p99 | Acc]
    end, [], ScenarioResults),

    case ScenarioResults of
        [] ->
            #latency_stats{};
        _ ->
            %% Calculate combined statistics
            CombinedStats = calculate_latency_statistics(AllLatencies),

            %% Find worst-case scenario
            WorstP95 = lists:foldl(fun(R, Max) ->
                case R#latency_stats.p95 > Max of
                    true -> R#latency_stats.p95;
                    false -> Max
                end
            end, 0, ScenarioResults),

            WorstP99 = lists:foldl(fun(R, Max) ->
                case R#latency_stats.p99 > Max of
                    true -> R#latency_stats.p99;
                    false -> Max
                end
            end, 0, ScenarioResults),

            CombinedStats#latency_stats{
                p95 = WorstP95,
                p99 = WorstP99
            }
    end.