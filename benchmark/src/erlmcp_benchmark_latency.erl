%% @doc Latency Benchmark Suite
%% Implements enterprise-level latency testing with sub-second response time
## requirements (p95 < 100ms, p99 < 200ms) for Fortune 500 scale.
%% @copyright 2026 erlmcp
%% @version 3.0.0
-module(erlmcp_benchmark_latency).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    make_config/1,
    run/2,
    stop/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Types
-type test_config() :: #{
    duration => pos_integer(),
    rate => pos_integer(),
    nodes => pos_integer(),
    warmup => pos_integer(),
    scenario => atom(),
    percentiles => [integer()],
    tail_analysis => boolean()
}.

-type latency_result() :: #{
    timestamp => integer(),
    scenario => atom(),
    percentiles => map(),
    tail_analysis => map(),
    distribution_analysis => map(),
    outlier_detection => map(),
    performance_guarantees => map(),
    optimization_opportunities => map()
}.

-type request_latency() :: #{
    request_id => binary(),
    timestamp => integer(),
    request_time => integer(),
    response_time => integer(),
    phase => 'warmup' | 'measurement' | 'cooldown',
    circuit_breaker_active => boolean(),
    cache_hit => boolean(),
    connection_pool_wait_time => integer() | undefined
}.

-type percentile_metric() :: #{
    p50 => integer(),
    p95 => integer(),
    p99 => integer(),
    p99_9 => integer(),
    p99_99 => integer(),
    max => integer(),
    min => integer()
}.

-define(LATENCY_TAB, erlmcp_benchmark_latency_metrics).
-define(TAIL_TAB, erlmcp_benchmark_tail_metrics).
-define(DIST_TAB, erlmcp_benchmark_distribution_metrics).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

make_config(Config) ->
    DefaultConfig = #{
        duration => 60000,    % 1 minute
        rate => 10000,       % 10K requests/second
        nodes => 5,          % 5 nodes for latency testing
        warmup => 15000,     % 15 seconds warmup
        scenario => default_latency_scenario,
        percentiles => [50, 95, 99, 99.9, 99.99],
        tail_analysis => true
    },
    maps:merge(DefaultConfig, Config).

run(Scenario, Config) ->
    gen_server:call(?SERVER, {run_latency_benchmark, Scenario, Config}).

stop() ->
    gen_server:call(?SERVER, stop).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize latency-specific metrics tables
    case ets:info(?LATENCY_TAB) of
        undefined ->
            ets:new(?LATENCY_TAB, [
                set,
                public,
                named_table,
                {keypos, #request_latency.timestamp},
                {write_concurrency, true},
                {read_concurrency, true}
            ]),
            ets:new(?TAIL_TAB, [set, public, named_table, {keypos, #request_latency.timestamp}]),
            ets:new(?DIST_TAB, [set, public, named_table, {keypos, #request_latency.timestamp}]);
        _ ->
            ok
    end,

    %% Initialize latency-specific monitoring
    ok = initialize_latency_monitoring(),

    %% Configure latency optimization
    ok = configure_latency_optimization(),

    %% Initialize circuit breakers
    ok = initialize_circuit_breakers(),

    %% Configure tail analysis
    ok = configure_tail_analysis(),

    State = #{
        start_time => undefined,
        end_time => undefined,
        config => undefined,
        scenario => undefined,
        warmup_phase => undefined,
        measurement_phase => undefined,
        outlier_detection => undefined,
        performance_guarantees => undefined,
        optimization_opportunities => #{}
    },

    {ok, State}.

handle_call({run_latency_benchmark, Scenario, Config}, _From, State) ->
    %% Validate configuration for latency testing
    case validate_latency_config(Config) of
        {ok, ValidConfig} ->
            %% Initialize latency benchmark environment
            case prepare_latency_environment(ValidConfig) of
                {ok, Environment} ->
                    %% Execute latency benchmark
                    {ok, Results} = execute_latency_benchmark(
                        Scenario, ValidConfig, Environment, State
                    ),

                    %% Analyze latency characteristics
                    AnalyzedResults = analyze_latency_characteristics(Results, ValidConfig),

                    %% Generate comprehensive latency report
                    Report = generate_latency_report(AnalyzedResults, ValidConfig),

                    {reply, {ok, Report}, update_state(State, ValidConfig, Scenario, Report)};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({latency_measurement, Metrics}, State) ->
    %% Store latency measurement
    store_latency_measurement(Metrics),

    %% Update phase-specific metrics
    UpdatedState = update_phase_metrics(State, Metrics),

    %% Check for outliers
    case detect_outliers(Metrics) of
        [] -> ok;
        Outliers ->
            handle_latency_outliers(Outliers, State)
    end,

    %% Check for completion
    case check_latency_completion(State) of
        true ->
            gen_server:cast(?SERVER, latency_benchmark_complete);
        false ->
            ok
    end,

    {noreply, UpdatedState};

handle_info({phase_complete, warmup, Results}, State) ->
    %% Warmup phase completed
    WarmupAnalysis = analyze_warmup_results(Results),
    UpdatedState = State#warmup_phase => WarmupAnalysis,
    {noreply, UpdatedState};

handle_info({phase_complete, measurement, Results}, State) ->
    %% Measurement phase completed
    MeasurementAnalysis = analyze_measurement_results(Results),
    UpdatedState = State#measurement_phase => MeasurementAnalysis,
    {noreply, UpdatedState};

handle_info(latency_benchmark_complete, State) ->
    %% Generate final latency analysis
    FinalAnalysis = compile_final_latency_analysis(State),

    %% Generate comprehensive report
    Report = generate_latency_report(FinalAnalysis, State#config),

    %% Validate SLA requirements
    SLAVerification = validate_sla_requirements(Report),

    %% Generate optimization recommendations
    Recommendations = generate_latency_optimization_recommendations(FinalAnalysis),

    %% Save benchmark results
    ok = save_latency_benchmark_report(Report),

    %% Notify completion
    ok = notify_latency_benchmark_complete(Report, SLAVerification, Recommendations),

    {noreply, State#{
        end_time => erlang:system_time(millisecond),
        final_analysis => FinalAnalysis,
        sla_verification => SLAVerification,
        recommendations => Recommendations
    }}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cleanup latency-specific resources
    ok = cleanup_latency_resources(),

    %% Export latency metrics
    ok = export_latency_metrics(),

    %% Clear metrics tables
    ets:delete_all_objects(?LATENCY_TAB),
    ets:delete_all_objects(?TAIL_TAB),
    ets:delete_all_objects(?DIST_TAB),

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

validate_latency_config(Config) ->
    Required = [duration, rate, scenario],
    Missing = [Field || Field <- Required, not maps:is_key(Field, Config)],

    case Missing of
        [] ->
            %% Validate rate for latency testing
            Rate = maps:get(rate, Config),
            case Rate of
                R when R > 0, R =< 50000 -> % Max 50K req/s for accurate latency
                    {ok, Config};
                _ ->
                    {error, {invalid_latency_rate, Rate}}
            end;
        _ ->
            {error, {missing_required_fields, Missing}}
    end.

initialize_latency_monitoring() ->
    %% Initialize high-frequency latency monitoring
    ok = erlmcp_latency_monitor:start(),
    ok = erlmcp_latency_monitor:configure_enterprise_monitoring(),
    ok.

configure_latency_optimization() ->
    %% Configure latency optimization techniques
    ok = erlmcp_latency_optimizer:start(),
    ok = erlmcp_latency_optimizer:configure_enterprise_settings(),
    ok.

initialize_circuit_breakers() ->
    %% Initialize circuit breakers for fault tolerance
    ok = erlmcp_circuit_breaker:initialize_enterprise_breakers(),
    ok.

configure_tail_analysis() ->
    %% Configure specialized tail analysis for percentile calculations
    ok = erlmcp_tail_analyzer:start(),
    ok = erlmcp_tail_analyzer:configure_enterprise_parameters(),
    ok.

prepare_latency_environment(Config) ->
    %% Prepare specialized environment for latency testing
    NodeCount = maps:get(nodes, Config, 5),

    %% Configure low-latency network settings
    ok = configure_low_latency_network(),

    %% Initialize latency-sensitive caches
    ok = initialize_latency_sensitive_caches(),

    %% Configure connection optimization
    ok = configure_latency_optimized_connections(),

    %% Setup latency monitoring infrastructure
    ok = setup_latency_monitoring(),

    %% Validate system readiness for low-latency operations
    case validate_latency_readiness(NodeCount) of
        {ok, _} ->
            {ok, #{nodes => NodeCount}};
        {error, Reason} ->
            {error, Reason}
    end.

configure_low_latency_network() ->
    %% Configure network for optimal latency
    ok = erlmcp_network_config:configure_latency_optimized(),
    ok.

initialize_latency_sensitive_caches() ->
    %% Initialize caches optimized for low latency
    CacheConfig = #{
        size => 10000,
        ttl => 100,  % 100ms TTL for latency-sensitive data
        eviction_strategy => lfu
    },

    ok = erlmcp_cache_manager:initialize_latency_cache(CacheConfig),
    ok.

configure_latency_optimized_connections() ->
    %% Configure connection pools for low latency
    ConnectionConfig = #{
        pool_size => 100,
        max_wait_time => 50,  % 50ms max wait
        acquire_timeout => 100,
        validation_interval => 5000
    },

    ok = erlmcp_connection_pool:configure_latency_optimized(ConnectionConfig),
    ok.

setup_latency_monitoring() ->
    %% Setup high-frequency latency monitoring
    ok = erlmcp_latency_monitor:start_collection([
        request_round_trip,
        application_processing,
        network_round_trip,
        cache_lookup,
        database_query
    ]),
    ok.

validate_latency_readiness(NodeCount) ->
    %% Validate system readiness for low-latency operations
    case erlmcp_cluster_manager:check_node_health(NodeCount) of
        {ok, HealthyNodes} when length(HealthyNodes) =:= NodeCount ->
            %% Check network latency between nodes
            LatencyResults = check_network_latency(HealthyNodes),
            case validate_network_latency(LatencyResults) of
                true ->
                    {ok, HealthyNodes};
                false ->
                    {error, network_latency_too_high}
            end;
        {ok, HealthyNodes} when length(HealthyNodes) < NodeCount ->
            {error, insufficient_healthy_nodes};
        {error, Reason} ->
            {error, Reason}
    end.

check_network_latency(Nodes) ->
    %% Check network latency between all nodes
    NodesPairs = [ {N1, N2} || N1 <- Nodes, N2 <- Nodes, N1 < N2 ],
    Results = [ check_latency_pair(Pair) || Pair <- NodesPairs ],
    Results.

check_latency_pair({Node1, Node2}) ->
    %% Check latency between two nodes
    Start = erlang:system_time(microsecond),
    case erlmcp_cluster_manager:ping_node(Node1, Node2) of
        {ok, _} ->
            End = erlang:system_time(microsecond),
            {Node1, Node2, End - Start};
        {error, _} ->
            {Node1, Node2, undefined}
    end.

validate_network_latency(Results) ->
    %% Validate that all node pairs meet latency requirements
    Latencies = [L || {_, _, L} <- Results, L =/= undefined],
    case Latencies of
        [] -> false;
        _ ->
            MaxLatency = lists:max(Latencies),
            MaxLatency < 50000  % 50ms max inter-node latency
    end.

execute_latency_benchmark(Scenario, Config, Environment, State) ->
    %% Execute the latency benchmark across all phases

    %% Start warmup phase
    WarmupResults = start_warmup_phase(Config, State),

    %% Start measurement phase
    MeasurementResults = start_measurement_phase(Config, State),

    %% Combine results
    {ok, #{warmup => WarmupResults, measurement => MeasurementResults}}.

start_warmup_phase(Config, State) ->
    WarmupConfig = maps:merge(Config, #{
        phase => warmup,
        duration => maps:get(warmup, Config, 15000),
        rate => maps:get(rate, Config, 10000)
    }),

    %% Start warmup request generation
    ok = erlmcp_latency_generator:start_warmup(WarmupConfig),

    %% Start warmup monitoring
    WarmupMonitorRef = start_phase_monitoring(warmup, WarmupConfig),

    %% Return warmup configuration for tracking
    WarmupConfig#{monitor_ref => WarmupMonitorRef}.

start_measurement_phase(Config, State) ->
    MeasurementConfig = maps:merge(Config, #{
        phase => measurement,
        duration => maps:get(duration, Config, 60000),
        rate => maps:get(rate, Config, 10000),
        percentiles => maps:get(percentiles, Config, [50, 95, 99])
    }),

    %% Start measurement request generation
    ok = erlmcp_latency_generator:start_measurement(MeasurementConfig),

    %% Start tail analysis for percentiles
    ok = start_tail_analysis(MeasurementConfig),

    %% Start outlier detection
    ok = start_outlier_detection(MeasurementConfig),

    %% Start distribution analysis
    ok = start_distribution_analysis(MeasurementConfig),

    %% Start measurement monitoring
    MeasurementMonitorRef = start_phase_monitoring(measurement, MeasurementConfig),

    MeasurementConfig#{monitor_ref => MeasurementMonitorRef}.

start_phase_monitoring(Phase, Config) ->
    %% Start monitoring for a specific phase
    MonitorRef = erlang:send_after(
        maps:get(duration, Config) div 2,
        self(),
        {phase_complete, Phase, #{}}
    ),

    {ok, MonitorRef}.

store_latency_measurement(Metrics) ->
    %% Store latency measurement in appropriate tables
    ets:insert(?LATENCY_TAB, Metrics),

    %% Store in tail analysis table if applicable
    case maps:get(phase, Metrics) of
        measurement ->
            ets:insert(?TAIL_TAB, Metrics);
        _ ->
            ok
    end,

    %% Store in distribution analysis table
    ets:insert(?DIST_TAB, Metrics),

    ok.

update_phase_metrics(State, Metrics) ->
    %% Update phase-specific metrics based on new measurement
    Phase = maps:get(phase, Metrics, unknown),
    Timestamp = maps:get(timestamp, Metrics),

    case Phase of
        warmup ->
            update_warmup_metrics(State, Metrics);
        measurement ->
            update_measurement_metrics(State, Metrics);
        _ ->
            State
    end.

update_warmup_metrics(State, Metrics) ->
    %% Update warmup phase metrics
    WarmupMetrics = case maps:get(warmup_phase, State, undefined) of
        undefined -> [Metrics];
        Existing -> [Metrics | Existing]
    end,

    State#{warmup_phase => WarmupMetrics}.

update_measurement_metrics(State, Metrics) ->
    %% Update measurement phase metrics
    MeasurementMetrics = case maps:get(measurement_phase, State, undefined) of
        undefined -> [Metrics];
        Existing -> [Metrics | Existing]
    end,

    State#{measurement_phase => MeasurementMetrics}.

detect_outliers(Metrics) ->
    %% Detect latency outliers using statistical methods
    ResponseTime = maps:get(response_time, Metrics),
    RecentMeasurements = get_recent_measurements(measurement),

    case RecentMeasurements of
        [] -> [];
        _ ->
            Mean = calculate_mean_recent_response_time(RecentMeasurements),
            StdDev = calculate_std_dev_recent_response_time(RecentMeasurements),

            %% Z-score > 3 indicates outlier
            ZScore = (ResponseTime - Mean) / StdDev,
            case abs(ZScore) > 3 of
                true -> [Metrics];
                false -> []
            end
    end.

get_recent_measurements(Phase) ->
    %% Get recent measurements for statistical analysis
    AllMeasurements = ets:tab2list(?LATENCY_TAB),
    Recent = lists:filter(fun(M) ->
        maps:get(phase, M) =:= Phase andalso
        erlang:system_time(millisecond) - maps:get(timestamp, M) < 10000
    end, AllMeasurements),

    Recent.

calculate_mean_recent_response_time(Measurements) ->
    Values = [maps:get(response_time, M) || M <- Measurements],
    case Values of
        [] -> 0;
        _ -> lists:sum(Values) / length(Values)
    end.

calculate_std_dev_recent_response_time(Measurements) ->
    Values = [maps:get(response_time, M) || M <- Measurements],
    Mean = calculate_mean_recent_response_time(Measurements),
    SquaredDiffs = [(V - Mean) * (V - Mean) || V <- Values],
    math:sqrt(lists:sum(SquaredDiffs) / length(Values)).

handle_latency_outliers(Outliers, State) ->
    %% Handle detected latency outliers
    ok = record_outlier_incident(Outliers),

    %% Analyze outlier patterns
    OutlierAnalysis = analyze_outlier_patterns(Outliers),

    %% Apply outlier mitigation
    ok = apply_outlier_mitigation(Outliers),

    UpdateState = State#{outlier_detection => OutlierAnalysis},

    {noreply, UpdateState}.

record_outlier_incident(Outliers) ->
    %% Record outlier incident for analysis
    Incident = #{
        timestamp => erlang:system_time(millisecond),
        outliers => Outliers,
        incident_type => latency_outlier,
        severity => calculate_outlier_severity(Outliers)
    },

    ok = erlmcp_incident_manager:record_incident(Incident),
    ok.

calculate_outlier_severity(Outliers) ->
    %% Calculate severity of outlier incident
    MaxResponseTime = lists:max([maps:get(response_time, O) || O <- Outliers]),
    case MaxResponseTime of
        T when T > 1000 -> high;
        T when T > 500 -> medium;
        _ -> low
    end.

analyze_outlier_patterns(Outliers) ->
    %% Analyze patterns in outlier occurrences
    PatternAnalysis = #{
        total_outliers => length(Outliers),
        average_outlier_duration => calculate_average_outlier_duration(Outliers),
        outlier_distribution_by_phase => group_outliers_by_phase(Outliers),
        outlier_distribution_by_node => group_outliers_by_node(Outliers),
        likely_causes => identify_possible_causes(Outliers)
    },

    PatternAnalysis.

calculate_average_outlier_duration(Outliers) ->
    Durations = [maps:get(response_time, O) || O <- Outliers],
    case Durations of
        [] -> 0;
        _ -> lists:sum(Durations) / length(Durations)
    end.

group_outliers_by_phase(Outliers) ->
    lists:foldl(fun(O, Acc) ->
        Phase = maps:get(phase, O),
        maps:update_counter(Phase, 1, Acc)
    end, #{}, Outliers).

group_outliers_by_node(Outliers) ->
    lists:foldl(fun(O, Acc) ->
        Node = maps:get(node, O, unknown),
        maps:update_counter(Node, 1, Acc)
    end, #{}, Outliers).

identify_possible_causes(Outliers) ->
    %% Identify possible causes for outliers
    Causes = [],

    %% Check for network-related causes
    case check_network_anomalies() of
        true -> [network_latency | Causes];
        false -> Causes
    end,

    %% Check for resource-related causes
    case check_resource_anomalies() of
        true -> [resource_constrained | Causes];
        false -> Causes
    end,

    %% Check for load-related causes
    case check_load_anomalies() of
        true -> [burst_load | Causes];
        false -> Causes
    end,

    Causes.

check_network_anomalies() ->
    %% Check for network-related anomalies
    case erlmcp_network_monitor:check_anomalies() of
        {ok, Anomalies} -> length(Anomalies) > 0;
        {error, _} -> false
    end.

check_resource_anomalies() ->
    %% Check for resource-related anomalies
    case erlmcp_resource_monitor:check_anomalies() of
        {ok, Anomalies} -> length(Anomalies) > 0;
        {error, _} -> false
    end.

check_load_anomalies() ->
    %% Check for load-related anomalies
    case erlmcp_load_monitor:check_anomalies() of
        {ok, Anomalies} -> length(Anomalies) > 0;
        {error, _} -> false
    end.

apply_outlier_mitigation(Outliers) ->
    %% Apply appropriate mitigation for outliers
    ok = erlmcp_mitigation_manager:apply_latency_mitigation(Outliers),
    ok.

start_tail_analysis(Config) ->
    %% Start tail analysis for accurate percentile calculations
    ok = erlmcp_tail_analyzer:start_analysis(Config),
    ok.

start_outlier_detection(Config) ->
    %% Start outlier detection process
    ok = erlmcp_outlier_detector:start(Config),
    ok.

start_distribution_analysis(Config) ->
    %% Start distribution analysis for latency patterns
    ok = erlmcp_distribution_analyzer:start(Config),
    ok.

check_latency_completion(State) ->
    %% Check if latency benchmark has completed
    case State#warmup_phase =/= undefined andalso State#measurement_phase =/= undefined of
        true ->
            %% Check if enough data has been collected
            MeasurementCount = length(maps:get(measurement_phase, State, [])),
            MeasurementCount >= maps:get(required_samples, State#config, 1000);
        false ->
            false
    end.

compile_final_latency_analysis(State) ->
    %% Compile all latency measurements into final analysis
    AllMeasurements = ets:tab2list(?LATENCY_TAB),

    %% Calculate percentiles
    Percentiles = calculate_latent_percentiles(AllMeasurements, State#config),

    %% Perform tail analysis
    TailAnalysis = compile_tail_analysis(AllMeasurements),

    %% Distribution analysis
    DistributionAnalysis = compile_distribution_analysis(AllMeasurements),

    %% Outlier analysis
    OutlierAnalysis = compile_outlier_analysis(State#outlier_detection),

    %% Performance guarantees
    PerformanceGuarantees = calculate_performance_guarantees(Percentiles),

    %% Optimization opportunities
    OptimizationOpportunities = identify_optimization_opportunities(
        Percentiles, TailAnalysis, DistributionAnalysis
    ),

    #{
        timestamp => erlang:system_time(millisecond),
        measurements_count => length(AllMeasurements),
        percentiles => Percentiles,
        tail_analysis => TailAnalysis,
        distribution_analysis => DistributionAnalysis,
        outlier_analysis => OutlierAnalysis,
        performance_guarantees => PerformanceGuarantees,
        optimization_opportunities => OptimizationOpportunities
    }.

calculate_latent_percentiles(Measurements, Config) ->
    %% Calculate latency percentiles
    ResponseTimes = [maps:get(response_time, M) || M <- Measurements,
                  maps:is_key(response_time, M)],

    case ResponseTimes of
        [] -> #{};
        _ ->
            Sorted = lists:sort(ResponseTimes),
            PercentileKeys = maps:get(percentiles, Config, [50, 95, 99]),

            #{
                p50 => calculate_percentile_value(50, Sorted),
                p95 => calculate_percentile_value(95, Sorted),
                p99 => calculate_percentile_value(99, Sorted),
                p99_9 => calculate_percentile_value(99.9, Sorted),
                p99_99 => calculate_percentile_value(99.99, Sorted),
                max => lists:max(ResponseTimes),
                min => lists:min(ResponseTimes),
                mean => lists:sum(ResponseTimes) / length(ResponseTimes)
            }
    end.

calculate_percentile_value(P, Sorted) ->
    Length = length(Sorted),
    Index = trunc((P / 100) * Length),
    case Sorted of
        [] -> 0;
        _ when Index > 0 -> lists:nth(Index, Sorted);
        _ -> 0
    end.

compile_tail_analysis(Measurements) ->
    %% Analyze tail behavior of latency distribution
    TailMeasurements = lists:filter(fun(M) ->
        maps:get(response_time, M) > calculate_percentile_value(95,
            [maps:get(response_time, M) || M <- Measurements])
    end, Measurements),

    case TailMeasurements of
        [] -> #{};
        _ ->
            TailValues = [maps:get(response_time, M) || M <- TailMeasurements],
            #{
                tail_measurements_count => length(TailMeasurements),
                tail_percent => length(TailMeasurements) / length(Measurements) * 100,
                tail_average => lists:sum(TailValues) / length(TailValues),
                tail_max => lists:max(TailValues),
                tail_min => lists:min(TailValues),
                tail_std_dev => calculate_std_dev(TailValues),
                tail_pattern => analyze_tail_pattern(TailValues)
            }
    end.

analyze_tail_pattern(TailValues) ->
    %% Analyze pattern in tail values (bursty, stable, etc.)
    Differences = [V2 - V1 || {V1, V2} <- lists:zip(lists:sublist(TailValues, length(TailValues)-1), TailValues)],

    case Differences of
        [] -> stable;
        _ ->
            AvgDiff = lists:sum(Differences) / length(Differences),
            StdDev = calculate_std_dev(Differences),

            case StdDev / AvgDiff of
                Ratio when Ratio < 0.1 -> stable;
                Ratio when Ratio < 0.5 -> gradual;
                Ratio when Ratio < 1.0 -> variable;
                _ -> volatile
            end
    end.

compile_distribution_analysis(Measurements) ->
    %% Analyze overall distribution characteristics
    ResponseTimes = [maps:get(response_time, M) || M <- Measurements,
                  maps:is_key(response_time, M)],

    case ResponseTimes of
        [] -> #{};
        _ ->
            #{
                distribution_type => identify_distribution_type(ResponseTimes),
                kurtosis => calculate_kurtosis(ResponseTimes),
                skewness => calculate_skewness(ResponseTimes),
                clusters => identify_latency_clusters(ResponseTimes),
                density => calculate_distribution_density(ResponseTimes)
            }
    end.

identify_distribution_type(ResponseTimes) ->
    %% Identify type of distribution (normal, exponential, etc.)
    Mean = lists:sum(ResponseTimes) / length(ResponseTimes),
    StdDev = calculate_std_dev(ResponseTimes),

    CoefficientOfVariation = StdDev / Mean,

    case CoefficientOfVariation of
        CV when CV < 0.3 -> normal;
        CV when CV < 1.0 -> lognormal;
        _ -> exponential
    end.

calculate_kurtosis(ResponseTimes) ->
    %% Calculate kurtosis of distribution
    Mean = lists:sum(ResponseTimes) / length(ResponseTimes),
    FourthMoment = lists:sum([(X - Mean)^4 || X <- ResponseTimes]) / length(ResponseTimes),
    Variance = calculate_variance(ResponseTimes),

    case Variance of
        0 -> 0;
        _ -> FourthMoment / (Variance * Variance)
    end.

calculate_skewness(ResponseTimes) ->
    %% Calculate skewness of distribution
    Mean = lists:sum(ResponseTimes) / length(ResponseTimes),
    ThirdMoment = lists:sum([(X - Mean)^3 || X <- ResponseTimes]) / length(ResponseTimes),
    StdDev = calculate_std_dev(ResponseTimes),

    case StdDev of
        0 -> 0;
        _ -> ThirdMoment / (StdDev * StdDev * StdDev)
    end.

identify_latency_clusters(ResponseTimes) ->
    %% Identify clusters in latency distribution
    Sorted = lists:sort(ResponseTimes),
    Differences = [V2 - V1 || {V1, V2} <- lists:zip(lists:sublist(Sorted, length(Sorted)-1), Sorted)],

    %% Find significant gaps (indicating clusters)
    Threshold = lists:median(Differences) * 2,
    ClusterBoundaries = [I || {I, D} <- lists:zip(lists:seq(1, length(Differences)), Differences), D > Threshold],

    case ClusterBoundaries of
        [] -> [ResponseTimes];
        _ ->
            Clusters = [],
            ClusterStart = 1,
            lists:foldl(fun(Boundary, Acc) ->
                Cluster = lists:sublist(Sorted, ClusterStart, Boundary - ClusterStart),
                [Cluster | Acc]
            end, [], ClusterBoundaries)
    end.

calculate_distribution_density(ResponseTimes) ->
    %% Calculate density distribution
    BinWidth = calculate_optimal_bin_width(ResponseTimes),
    Bins = create_bins(ResponseTimes, BinWidth),
    calculate_bin_densities(Bins).

calculate_optimal_bin_width(ResponseTimes) ->
    %% Calculate optimal bin width using Freedman-Diaconis rule
    IQR = calculate_iqr(ResponseTimes),
    BinWidth = 2 * IQR / math:pow(length(ResponseTimes), 1/3),
    max(1, round(BinWidth)).

calculate_iqr(ResponseTimes) ->
    %% Calculate interquartile range
    Sorted = lists:sort(ResponseTimes),
    Length = length(Sorted),
    Q1Index = trunc(Length * 0.25),
    Q3Index = trunc(Length * 0.75),
    lists:nth(Q3Index, Sorted) - lists:nth(Q1Index, Sorted).

compile_outlier_analysis(OutlierDetection) ->
    %% Compile outlier detection results
    case maps:get(outlier_detection, OutlierDetection, undefined) of
        undefined -> #{};
        Analysis ->
            #{
                total_outlier_incidents => maps:get(total_outliers, Analysis, 0),
                average_outlier_duration => maps:get(average_outlier_duration, Analysis, 0),
                outlier_distribution => maps:get(outlier_distribution_by_phase, Analysis, #{}),
                common_causes => maps:get(likely_causes, Analysis, []),
                impact_assessment => assess_outlier_impact(Analysis)
            }
    end.

assess_outlier_impact(OutlierAnalysis) ->
    %% Assess the impact of outliers on system performance
    HighOutlierRatio = case lists:filter(fun({_, Count}) -> Count > 10 end,
                                maps:to_list(maps:get(outlier_distribution_by_phase, OutlierAnalysis, #{}))) of
        [] -> low;
        _ when length(_) < 3 -> medium;
        _ -> high
    end,

    #{
        severity => HighOutlierRatio,
        business_impact => calculate_business_impact(HighOutlierRatio),
        system_stability => assess_system_stability(HighOutlierRatio)
    }.

calculate_business_impact(HighOutlierRatio) ->
    %% Calculate business impact based on outlier severity
    case HighOutlierRatio of
        low -> minimal;
        medium -> moderate;
        high -> significant
    end.

assess_system_stability(HighOutlierRatio) ->
    %% Assess system stability based on outliers
    case HighOutlierRatio of
        low -> stable;
        medium => acceptable;
        high => unstable
    end.

calculate_performance_guarantees(Percentiles) ->
    %% Calculate performance guarantees based on percentiles
    P95 = maps:get(p95, Percentiles, 0),
    P99 = maps:get(p99, Percentiles, 0),

    #{
        p95_guarantee => P95 < 100,  % 100ms guarantee
        p99_guarantee => P99 < 200,  % 200ms guarantee
        overall_latency_profile => determine_latency_profile(P95, P99),
        compliance_slas => check_sla_compliance(Percentiles),
        robustness_score => calculate_robustness_score(Percentiles)
    }.

determine_latency_profile(P95, P99) ->
    %% Determine overall latency profile
    TailRatio = P99 / P95,
    case TailRatio of
        R when R < 1.5 -> excellent;
        R when R < 2.0 -> good;
        R when R < 3.0 -> acceptable;
        _ -> poor
    end.

check_sla_compliance(Percentiles) ->
    %% Check compliance with SLA requirements
    #{
        p95_sla_compliance => maps:get(p95, Percentiles, 0) < 100,
        p99_sla_compliance => maps:get(p99, Percentiles, 0) < 200,
        p99_9_sla_compliance => maps:get(p99_9, Percentiles, 0) < 500,
        overall_compliance => (
            (maps:get(p95, Percentiles, 0) < 100) and
            (maps:get(p99, Percentiles, 0) < 200) and
            (maps:get(p99_9, Percentiles, 0) < 500)
        )
    }.

calculate_robustness_score(Percentiles) ->
    %% Calculate overall robustness score
    P95 = maps:get(p95, Percentiles, 0),
    P99 = maps:get(p99, Percentiles, 0),
    TailRatio = P99 / P95,

    BaseScore = 100,
    P95Penalty = max(0, (P95 - 100) / 100 * 20),
    TailRatioPenalty = max(0, (TailRatio - 2) * 10),

    max(0, BaseScore - P95Penalty - TailRatioPenalty).

identify_optimization_opportunities(Percentiles, TailAnalysis, DistributionAnalysis) ->
    %% Identify specific optimization opportunities
    Opportunities = [],

    %% Check for cache optimization
    case should_optimize_cache(Percentiles, TailAnalysis) of
        true -> [cache_optimization | Opportunities];
        false -> Opportunities
    end,

    %% Check for connection pool optimization
    case should_optimize_connection_pools(Percentiles, DistributionAnalysis) of
        true -> [connection_pool_optimization | Opportunities];
        false -> Opportunities
    end,

    %% Check for network optimization
    case should_optimize_network(Percentiles, TailAnalysis) of
        true -> [network_optimization | Opportunities];
        false -> Opportunities
    end,

    %% Check for database optimization
    case should_optimize_database(DistributionAnalysis) of
        true -> [database_optimization | Opportunities];
        false -> Opportunities
    end,

    %% Check for application optimization
    case should_optimize_application(Percentiles, DistributionAnalysis) of
        true -> [application_optimization | Opportunities];
        false -> Opportunities
    end,

    Opportunities.

should_optimize_cache(Percentiles, TailAnalysis) ->
    %% Determine if cache optimization is needed
    P99 = maps:get(p99, Percentiles, 0),
    TailPattern = maps:get(tail_pattern, TailAnalysis, stable),

    (P99 > 150) or (TailPattern =:= volatile).

should_optimize_connection_pools(Percentiles, DistributionAnalysis) ->
    %% Determine if connection pool optimization is needed
    Max = maps:get(max, Percentiles, 0),
    DistributionType = maps:get(distribution_type, DistributionAnalysis, normal),

    (Max > 200) or (DistributionType =:= exponential).

should_optimize_network(Percentiles, TailAnalysis) ->
    %% Determine if network optimization is needed
    P99 = maps:get(p99, Percentiles, 0),
    TailPercent = maps:get(tail_percent, TailAnalysis, 0),

    (P99 > 150) or (TailPercent > 5).

should_optimize_database(DistributionAnalysis) ->
    %% Determine if database optimization is needed
    Skewness = maps:get(skewness, DistributionAnalysis, 0),
    Kurtosis = maps:get(kurtosis, DistributionAnalysis, 0),

    (abs(Skewness) > 1) or (Kurtosis > 3).

should_optimize_application(Percentiles, DistributionAnalysis) ->
    %% Determine if application optimization is needed
    Mean = maps:get(mean, Percentiles, 0),
    StdDev = calculate_std_dev([maps:get(p50, Percentiles, 0),
                               maps:get(p95, Percentiles, 0),
                               maps:get(p99, Percentiles, 0)]),

    (Mean > 100) or (StdDev > 50).

generate_latency_report(Analysis, Config) ->
    %% Generate comprehensive latency benchmark report
    #{
        timestamp => erlang:system_time(millisecond),
        scenario => maps:get(scenario, Config, unknown),
        configuration => Config,
        results => Analysis,
        recommendations => generate_latency_recommendations(Analysis),
        next_steps => identify_next_steps(Analysis),
        capacity_planning => generate_latency_capacity_planning(Analysis),
        monitoring_dashboard => generate_monitoring_dashboard(Analysis)
    }.

generate_latency_recommendations(Analysis) ->
    %% Generate specific recommendations based on latency analysis
    Recommendations = #{
        immediate => [],
        short_term => [],
        long_term => []
    },

    %% Check for immediate actions (critical SLA violations)
    Percentiles = maps:get(percentiles, Analysis, #{}),
    case maps:get(p99, Percentiles, 0) > 300 of
        true ->
            Recommendations#{
                immediate => [
                    "Investigate root cause of high P99 latency",
                    "Implement circuit breakers to protect critical paths",
                    "Review resource allocation for bottleneck nodes"
                ]
            };
        false ->
            Recommendations
    end,

    %% Check for short-term optimizations
    case maps:get(tail_pattern, maps:get(tail_analysis, Analysis, #{}), stable) of
        volatile ->
            Recommendations#{
                short_term => [
                    "Implement request queuing to handle bursts",
                    "Add auto-scaling to handle load variations",
                    "Configure adaptive timeouts based on system load"
                ]
            };
        _ ->
            Recommendations
    end,

    %% Check for long-term improvements
    Distribution = maps:get(distribution_type, maps:get(distribution_analysis, Analysis, #{}), normal),
    case Distribution of
        exponential ->
            Recommendations#{
                long_term => [
                    "Implement exponential backoff and retry strategies",
                    "Consider caching frequently accessed data",
                    "Review data access patterns for optimization"
                ]
            };
        lognormal ->
            Recommendations#{
                long_term => [
                    "Implement logarithmic scale monitoring",
                    "Consider probabilistic data structures",
                    "Optimize for typical workload patterns"
                ]
            };
        _ ->
            Recommendations
    end.

identify_next_steps(Analysis) ->
    %% Identify next steps based on latency analysis
    NextSteps = [],

    %% Capacity planning
    case maps:get(p99, maps:get(percentiles, Analysis, #{}), 0) > 200 of
        true -> [review_capacity_planning | NextSteps];
        false -> NextSteps
    end,

    %% Monitoring improvements
    case maps:get(tail_pattern, maps:get(tail_analysis, Analysis, #{}), stable) =:= volatile of
        true -> [enhance_monitoring | NextSteps];
        false -> NextSteps
    end,

    %% Performance optimization
    case maps:get(optimization_opportunities, Analysis, []) of
        [] -> NextSteps;
        Opportunities -> [optimization_required | NextSteps]
    end,

    NextSteps.

generate_latency_capacity_planning(Analysis) ->
    %% Generate capacity planning recommendations based on latency analysis
    Percentiles = maps:get(percentiles, Analysis, #{}),
    TailAnalysis = maps:get(tail_analysis, Analysis, #{}),

    #{
        current_capacity => analyze_current_capacity(Analysis),
        scaling_requirements => calculate_scaling_requirements(Analysis),
        cost_implications => calculate_cost_implications(Analysis),
        timeline_recommendations => generate_timeline_recommendations(Analysis),
        risk_assessment => assess_capacity_risks(Analysis)
    }.

analyze_current_capacity(Analysis) ->
    %% Analyze current system capacity based on latency metrics
    P95 = maps:get(p95, maps:get(percentiles, Analysis, #{}), 0),
    CurrentThroughput = estimate_current_throughput(Analysis),

    #{
        throughput_capacity => CurrentThroughput,
        latency_capacity => P95,
        resource_utilization => estimate_resource_utilization(Analysis),
        bottleneck_identification => identify_current_bottlenecks(Analysis)
    }.

calculate_scaling_requirements(Analysis) ->
    %% Calculate scaling requirements based on growth projections
    P99 = maps:get(p99, maps:get(percentiles, Analysis, #{}), 0),
    GrowthProjection = project_load_growth(Analysis),

    ScalingFactors = #{
        cpu => calculate_cpu_scaling_factor(P99, GrowthProjection),
        memory => calculate_memory_scaling_factor(P99, GrowthProjection),
        network => calculate_network_scaling_factor(P99, GrowthProjection),
        nodes => calculate_node_scaling_factor(P99, GrowthProjection)
    },

    #{
        current_load => GrowthProjection#current_load,
        projected_load => GrowthProjection#projected_load,
        scaling_factors => ScalingFactors,
        estimated_cost => estimate_scaling_cost(ScalingFactors)
    }.

calculate_cost_implications(Analysis) ->
    %% Calculate cost implications of capacity planning
    ScalingRequirements = calculate_scaling_requirements(Analysis),
    InfrastructureCosts = calculate_infrastructure_costs(ScalingRequirements),
    OperationalCosts = calculate_operational_costs(ScalingRequirements),

    #{
        capital_expenditure => InfrastructureCosts,
        operational_expenditure => OperationalCosts,
        total_cost => InfrastructureCosts + OperationalCosts,
        roi_analysis => perform_roi_analysis(ScalingRequirements)
    }.

generate_timeline_recommendations(Analysis) ->
    %% Generate timeline recommendations for capacity planning
    ImmediateActions = identify_immediate_actions(Analysis),
    ShortTermActions = identify_short_term_actions(Analysis),
    LongTermActions = identify_long_term_actions(Analysis),

    #{
        immediate => ImmediateActions,
        short_term => ShortTermActions,
        long_term => LongTermActions,
        critical_milestones => identify_critical_milestones(Analysis)
    }.

assess_capacity_risks(Analysis) ->
    %% Assess risks associated with capacity planning
    LatencyRisks = assess_latency_risks(Analysis),
    ScalingRisks = assess_scaling_risks(Analysis),
    OperationalRisks = assess_operational_risks(Analysis),

    #{
        risk_score => calculate_overall_risk_score(LatencyRisks, ScalingRisks, OperationalRisks),
        risk_mitigation => identify_risk_mitigation_strategies(Analysis),
        contingency_plans => generate_contingency_plans(Analysis)
    }.

generate_monitoring_dashboard(Analysis) ->
    %% Generate monitoring dashboard specifications
    DashboardConfig = #{
        primary_metrics => [
            {p99_latency, "P99 Latency (ms)", "critical"},
            {throughput, "Throughput (req/s)", "primary"},
            {error_rate, "Error Rate (%)", "warning"},
            {resource_utilization, "Resource Utilization (%)", "secondary"}
        ],
        thresholds => generate_thresholds(Analysis),
        alerting => generate_alerting_configuration(Analysis),
        visualization => generate_visualization_configurations(Analysis)
    },

    DashboardConfig.

generate_thresholds(Analysis) ->
    %% Generate threshold configuration for monitoring
    Percentiles = maps:get(percentiles, Analysis, #{}),

    #{
        p99 => maps:get(p99, Percentiles, 0) * 1.1,  % 10% buffer
        p95 => maps:get(p95, Percentiles, 0) * 1.05,
        throughput => estimate_throughput_threshold(Analysis),
        error_rate => 0.01  % 1% error rate
    }.

generate_alerting_configuration(Analysis) ->
    %% Generate alerting configuration
    #{
        critical_alerts => [
            "P99 latency exceeds 300ms",
            "Error rate exceeds 5%",
            "System availability below 99.9%"
        ],
        warning_alerts => [
            "P99 latency exceeds 200ms",
            "Error rate exceeds 1%",
            "CPU utilization exceeds 80%"
        ],
        notification_channels => ["email", "slack", "pagerduty"],
        escalation_policy => generate_escalation_policy(Analysis)
    }.

generate_visualization_configurations(Analysis) ->
    %% Generate visualization configurations
    #{
        latency_distribution => generate_latency_distribution_chart(Analysis),
        throughput_over_time => generate_throughput_time_series(Analysis),
        resource_utilization => generate_resource_utilization_charts(Analysis),
        heatmap_configurations => generate_heatmap_configurations(Analysis)
    }.

save_latency_benchmark_report(Report) ->
    %% Save latency benchmark report to persistent storage
    ReportId = generate_latency_report_id(),
    ok = erlmcp_benchmark_storage:save_latency_report(ReportId, Report),
    ok.

notify_latency_benchmark_complete(Report, SLAVerification, Recommendations) ->
    %% Notify external systems of benchmark completion
    Notification = #{
        type => benchmark_complete,
        category => latency,
        report => Report,
        sla_compliance => SLAVerification,
        recommendations => Recommendations
    },

    ok = erlmcp_notification_manager:notify_latency_benchmark_complete(Notification),
    ok.

cleanup_latency_resources() ->
    %% Cleanup all latency-specific resources
    ok = erlmcp_latency_monitor:stop(),
    ok = erlmcp_latency_optimizer:stop(),
    ok = erlmcp_circuit_breaker:cleanup(),
    ok = erlmcp_tail_analyzer:stop(),
    ok.

export_latency_metrics() ->
    %% Export all latency metrics to external systems
    Metrics = ets:tab2list(?LATENCY_TAB),
    ok = erlmcp_metrics_exporter:export_latency_metrics(Metrics),
    ok.

update_state(State, Config, Scenario, Report) ->
    State#{
        start_time => erlang:system_time(millisecond),
        config => Config,
        scenario => Scenario,
        report => Report
    }.

analyze_warmup_results(Results) ->
    %% Analyze warmup phase results
    #{
        warmup_duration => maps:get(duration, Results),
        warmup_rate => maps:get(rate, Results),
        warmup_effectiveness => calculate_warmup_effectiveness(Results)
    }.

analyze_measurement_results(Results) ->
    %% Analyze measurement phase results
    #{
        measurement_duration => maps:get(duration, Results),
        measurement_rate => maps:get(rate, Results),
        measurement_quality => calculate_measurement_quality(Results)
    }.

validate_sla_requirements(Report) ->
    %% Validate compliance with SLA requirements
    Percentiles = maps:get(percentiles, maps:get(results, Report, #{}), #{}),
    #{
        p99_sla => maps:get(p99, Percentiles, 0) < 200,
        p95_sla => maps:get(p95, Percentiles, 0) < 100,
        overall_compliance => (
            (maps:get(p99, Percentiles, 0) < 200) and
            (maps:get(p95, Percentiles, 0) < 100)
        )
    }.

generate_latency_optimization_recommendations(Analysis) ->
    %% Generate optimization recommendations for latency
    Recommendations = [],

    %% Tail optimization
    case maps:get(tail_pattern, maps:get(tail_analysis, Analysis, #{}), stable) of
        volatile ->
            Recommendations#{tail_optimization => "Implement request queuing and adaptive timeouts"};
        _ ->
            Recommendations
    end,

    %% Distribution optimization
    case maps:get(distribution_type, maps:get(distribution_analysis, Analysis, #{}), normal) of
        exponential ->
            Recommendations#{distribution_optimization => "Implement exponential backoff and caching"};
        lognormal ->
            Recommendations#{distribution_optimization => "Consider probabilistic data structures"};
        _ ->
            Recommendations
    end,

    Recommendations.

calculate_warmup_effectiveness(Results) ->
    %% Calculate effectiveness of warmup phase
    WarmupRate = maps:get(rate, Results),
    MeasurementRate = maps:get(measurement_rate, Results),
    case WarmupRate of
        0 -> 0;
        _ -> min(1.0, MeasurementRate / WarmupRate)
    end.

calculate_measurement_quality(Results) ->
    %% Calculate quality of measurements
    TotalMeasurements = maps:get(measurements_count, Results),
    ValidMeasurements = maps:get(valid_measurements, Results, TotalMeasurements),
    case TotalMeasurements of
        0 -> 0;
        _ -> ValidMeasurements / TotalMeasurements
    end.

generate_latency_report_id() ->
    "lat_bench_" ++ integer_to_list(erlang:system_time(millisecond), 36).