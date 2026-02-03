%% @doc Horizontal scalability validation module for erlmcp v3
%% Validates horizontal scaling capabilities including:
%% - Node scaling validation
%% - Connection scaling validation
%% - Throughput scaling validation
%% - Resource utilization validation
%% - Load distribution validation
%% - Auto-scaling validation

-module(erlmcp_horizontal_scalability_validator).
-behaviour(gen_server).
-export([start_link/0, validate/1, run_scaling_test/2, get_scaling_metrics/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("erlmcp_core/include/erlmcp.hrl").

%% Records
-record(node_metrics, {
    id :: binary(),
    connections :: non_neg_integer(),
    cpu_percent :: non_neg_integer(),
    memory_percent :: non_neg_integer(),
    rps :: non_neg_integer(),
    timestamp :: integer()
}).

-record(test_result, {
    node_count :: non_neg_integer(),
    total_connections :: non_neg_integer(),
    total_rps :: non_neg_integer(),
    avg_latency :: float(),
    error_rate :: float(),
    cpu_utilization :: float(),
    memory_utilization :: float(),
    timestamp :: integer()
}).

-record(state, {
    test_nodes :: list(binary()),
    current_results :: list(#test_result{}),
    scaling_thresholds :: map(),
    test_config :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

validate(Config) ->
    gen_server:call(?MODULE, {validate, Config}, 30000).

run_scaling_test(NodeCount, TestDuration) ->
    gen_server:call(?MODULE, {run_scaling_test, NodeCount, TestDuration}, 60000).

get_scaling_metrics() ->
    gen_server:call(?MODULE, get_scaling_metrics).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize test nodes
    TestNodes = initialize_test_nodes(),

    %% Scaling thresholds
    ScalingThresholds = #{
        cpu_utilization_high => 80,
        cpu_utilization_optimal => 60,
        memory_utilization_high => 85,
        memory_utilization_optimal => 70,
        latency_threshold => 100,
        error_rate_threshold => 0.01,
        connection_efficiency => 0.8,
        scaling_efficiency => 0.9
    },

    %% Test configuration
    TestConfig = #{
        max_nodes => 100,
        test_duration => 300000,
        load_rampup => 60000,
        monitoring_interval => 5000,
        baseline_connections => 10000,
        baseline_rps => 100000
    },

    %% State initialization
    State = #state{
        test_nodes = TestNodes,
        current_results = [],
        scaling_thresholds = ScalingThresholds,
        test_config = TestConfig
    },

    %% Start periodic scaling validation
    schedule_scaling_check(),

    {ok, State}.

handle_call({validate, Config}, _From, State) ->
    %% Run horizontal scaling validation
    Results = run_horizontal_validation(Config, State),

    %% Generate validation report
    Report = generate_validation_report(Results),

    {reply, Report, State#state{current_results = Results}};

handle_call({run_scaling_test, NodeCount, TestDuration}, _From, State) ->
    %% Run scaling test
    TestResults = run_scaling_test(NodeCount, TestDuration, State),

    %% Analyze scaling performance
    Analysis = analyze_scaling_performance(TestResults),

    {reply, {test_results, TestResults, analysis, Analysis}, State};

handle_call(get_scaling_metrics, _From, State) ->
    %% Get current scaling metrics
    Metrics = collect_scaling_metrics(),

    {reply, Metrics, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(scaling_check, State) ->
    %% Perform periodic scaling check
    CheckResults = run_periodic_scaling_check(State),

    %% Update results
    UpdatedResults = State#state.current_results ++ CheckResults,
    NewState = State#state{current_results = UpdatedResults},

    %% Check for scaling issues
    check_scaling_issues(CheckResults),

    {noreply, NewState}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Initialize test nodes
initialize_test_nodes() ->
    %% Create test nodes for scaling validation
    Nodes = [
        "node-1", "node-2", "node-3", "node-4", "node-5",
        "node-6", "node-7", "node-8", "node-9", "node-10"
    ],

    %% Start test nodes
    lists:foreach(fun(Node) ->
        start_test_node(Node)
    end, Nodes),

    Nodes.

%% Start test node
start_test_node(NodeId) ->
    %% Implement test node startup
    ok.

%% Run horizontal scaling validation
run_horizontal_validation(Config, State) ->
    %% Get scaling test levels
    TestLevels = get_test_levels(Config),

    %% Run validation for each level
    Results = lists:map(fun(Level) ->
        run_validation_level(Level, State)
    end, TestLevels),

    %% Analyze scaling efficiency
    ScalingEfficiency = analyze_scaling_efficiency(Results),

    %% Validate against thresholds
    ValidationResults = validate_against_thresholds(Results, ScalingEfficiency),

    ValidationResults.

%% Get test levels
get_test_levels(Config) ->
    #{
        min_nodes := MinNodes,
        max_nodes := MaxNodes,
        step_size := StepSize
    } = maps:get(test_levels, Config, #{
        min_nodes => 1,
        max_nodes => 10,
        step_size => 1
    }),

    [MinNodes + I * StepSize || I <- lists:seq(0, (MaxNodes - MinNodes) div StepSize)].

%% Run validation level
run_validation_level(NodeCount, State) ->
    %% Setup test environment
    setup_test_environment(NodeCount),

    %% Generate test load
    LoadConfig = generate_test_load(NodeCount, State),

    %% Run validation test
    TestResult = run_test(NodeCount, LoadConfig, State),

    %% Cleanup test environment
    cleanup_test_environment(),

    TestResult.

%% Generate test load
generate_test_load(NodeCount, State) ->
    #{
        baseline_connections := BaseConn,
        baseline_rps := BaseRPS
    } = State#state.test_config,

    CalculateLoad = fun(N) ->
        {
            min(BaseConn * N, 100000), % Max 100K connections
            min(BaseRPS * N, 1000000)   % Max 1M RPS
        }
    end,

    CalculateLoad(NodeCount).

%% Run test
run_test(NodeCount, LoadConfig, State) ->
    #{
        connections := Connections,
        rps := RPS
    } = LoadConfig,

    #{
        test_duration := Duration,
        monitoring_interval := Interval
    } = State#state.test_config,

    %% Start monitoring
    start_monitoring(Interval),

    %% Generate load
    start_load_generation(NodeCount, Connections, RPS),

    %% Monitor for duration
    wait_for_duration(Duration),

    %% Stop monitoring
    MonitoringResults = stop_monitoring(),

    %% Calculate aggregate metrics
    AggregateMetrics = aggregate_metrics(MonitoringResults),

    %% Create test result
    TestResult = #test_result{
        node_count = NodeCount,
        total_connections = Connections,
        total_rps = RPS,
        avg_latency = AggregateMetrics#avg_latency,
        error_rate = AggregateMetrics#error_rate,
        cpu_utilization = AggregateMetrics#cpu_utilization,
        memory_utilization = AggregateMetrics#memory_utilization,
        timestamp = erlang:system_time(millisecond)
    },

    TestResult.

%% Aggregate metrics
aggregate_metrics(MonitoringResults) ->
    %% Calculate aggregate metrics from monitoring results
    TotalLatency = lists:sum([M#node_metrics.latency || M <- MonitoringResults]),
    TotalCPU = lists:sum([M#node_metrics.cpu_percent || M <- MonitoringResults]),
    TotalMemory = lists:sum([M#node_metrics.memory_percent || M <- MonitoringResults]),
    TotalErrors = lists:sum([M#node_metrics.error_count || M <- MonitoringResults]),
    TotalRequests = lists:sum([M#node_metrics.request_count || M <- MonitoringResults]),

    #{
        avg_latency => TotalLatency / length(MonitoringResults),
        error_rate => TotalErrors / max(TotalRequests, 1),
        cpu_utilization => TotalCPU / length(MonitoringResults),
        memory_utilization => TotalMemory / length(MonitoringResults)
    }.

%% Analyze scaling efficiency
analyze_scaling_efficiency(Results) ->
    %% Calculate scaling efficiency metrics
    NodeCounts = [R#test_result.node_count || R <- Results],
    Throughputs = [R#test_result.total_rps || R <- Results],
    ConnectionEfficiencies = [calculate_connection_efficiency(R) || R <- Results],

    %% Calculate linear scaling efficiency
    LinearEfficiency = calculate_linear_efficiency(NodeCounts, Throughputs),

    %% Calculate resource scaling efficiency
    ResourceEfficiency = calculate_resource_efficiency(Results),

    #{
        linear_efficiency => LinearEfficiency,
        connection_efficiency => lists:sum(ConnectionEfficiencies) / length(ConnectionEfficiencies),
        resource_efficiency => ResourceEfficiency,
        overall_efficiency => (LinearEfficiency + ResourceEfficiency) / 2
    }.

%% Calculate connection efficiency
calculate_connection_efficiency(Result) ->
    TargetConnections = Result#test_result.node_count * 10000,
    ActualConnections = Result#test_result.total_connections,

    ActualConnections / min(TargetConnections, 100000).

%% Calculate linear efficiency
calculate_linear_efficiency(NodeCounts, Throughputs) ->
    %% Calculate efficiency as (actual throughput) / (expected throughput)
    case length(NodeCounts) of
        0 -> 0.0;
        1 -> 1.0; % Single node baseline
        _ ->
            %% Calculate expected throughput based on linear scaling
            ExpectedThroughput = calculate_expected_throughput(NodeCounts, Throughputs),
            ActualThroughput = lists:last(Throughputs),
            ActualThroughput / max(ExpectedThroughput, 1)
    end.

%% Calculate expected throughput
calculate_expected_throughput(NodeCounts, Throughputs) ->
    %% Linear regression for expected throughput
    {_, Slope, _} = linear_regression(NodeCounts, Throughputs),

    Expected = lists:last(NodeCounts) * Slope,
    Expected.

%% Linear regression
linear_regression(X, Y) ->
    N = length(X),
    SumX = lists:sum(X),
    SumY = lists:sum(Y),
    SumXY = lists:sum([X_i * Y_i || {X_i, Y_i} <- lists:zip(X, Y)]),
    SumX2 = lists:sum([X_i * X_i || X_i <- X]),

    SXX = SumX2 - (SumX * SumX) / N,
    SXY = SumXY - (SumX * SumY) / N,

    Slope = SXY / SXX,
    Intercept = (SumY - Slope * SumX) / N,

    {Intercept, Slope, R2}.

%% Calculate resource efficiency
calculate_resource_efficiency(Results) ->
    %% Calculate how efficiently resources are utilized at scale
    UtilizationRatios = lists:map(fun(R) ->
        #{
            cpu => R#test_result.cpu_utilization,
            memory => R#test_result.memory_utilization,
            connections => R#test_result.total_connections / (R#test_result.node_count * 10000)
        }
    end, Results),

    %% Calculate average utilization ratio
    AvgUtilization = lists:sum([U#{cpu} || U <- UtilizationRatios]) / length(UtilizationRatios),
    AvgMemory = lists:sum([U#{memory} || U <- UtilizationRatios]) / length(UtilizationRatios),
    AvgConnections = lists:sum([U#{connections} || U <- UtilizationRatios]) / length(UtilizationRatios),

    %% Optimal utilization is around 70%
    MinUtilization = min(AvgUtilization, AvgMemory, AvgConnections) / 0.7,

    MinUtilization.

%% Validate against thresholds
validate_against_thresholds(Results, ScalingEfficiency) ->
    #{
        cpu_utilization_high := CpuHigh,
        memory_utilization_high := MemHigh,
        latency_threshold := LatencyThreshold,
        error_rate_threshold := ErrorThreshold,
        scaling_efficiency := ScalingThreshold
    } = State#state.scaling_thresholds,

    %% Check each result
    ValidationResults = lists:map(fun(Result) ->
        validate_result(Result, ScalingEfficiency, State#state.scaling_thresholds)
    end, Results),

    %% Aggregate results
    AggregateValidation = aggregate_validation(ValidationResults),

    %% Generate validation report
    #{
        test_results => Results,
        scaling_efficiency => ScalingEfficiency,
        validation_results => ValidationResults,
        aggregate_validation => AggregateValidation,
        recommendations => generate_recommendations(Results, ScalingEfficiency, AggregateValidation)
    }.

%% Validate individual result
validate_result(Result, ScalingEfficiency, Thresholds) ->
    #{
        cpu_utilization := CpuThreshold,
        memory_utilization := MemThreshold,
        latency_threshold := LatencyThreshold,
        error_rate_threshold := ErrorThreshold,
        scaling_efficiency := ScalingThreshold
    } = Thresholds,

    ResultValidation = #{
        node_count => Result#test_result.node_count,
        connections => Result#test_result.total_connections,
        rps => Result#test_result.total_rps,
        checks => #{
            cpu_utilization => check_threshold(Result#test_result.cpu_utilization, CpuThreshold),
            memory_utilization => check_threshold(Result#test_result.memory_utilization, MemThreshold),
            latency => check_threshold(Result#test_result.avg_latency, LatencyThreshold),
            error_rate => check_threshold(Result#test_result.error_rate, ErrorThreshold),
            scaling_efficiency => check_threshold(ScalingEfficiency#overall_efficiency, ScalingThreshold)
        },
        status => determine_overall_status(#{
            cpu_utilization => Result#test_result.cpu_utilization,
            memory_utilization => Result#test_result.memory_utilization,
            latency => Result#test_result.avg_latency,
            error_rate => Result#test_result.error_rate,
            scaling_efficiency => ScalingEfficiency#overall_efficiency
        }, Thresholds)
    },

    ResultValidation.

%% Check threshold
check_threshold(Value, Threshold) ->
    if
        Value <= Threshold * 0.8 -> optimal;
        Value <= Threshold -> acceptable;
        Value <= Threshold * 1.2 -> warning;
        true -> critical
    end.

%% Determine overall status
determine_overall_status(Metrics, Thresholds) ->
    #{
        cpu_utilization := CpuThreshold,
        memory_utilization := MemThreshold,
        latency_threshold := LatencyThreshold,
        error_rate_threshold := ErrorThreshold,
        scaling_efficiency := ScalingThreshold
    } = Thresholds,

    CpuStatus = check_threshold(Metrics#{cpu_utilization}, CpuThreshold),
    MemoryStatus = check_threshold(Metrics#{memory_utilization}, MemThreshold),
    LatencyStatus = check_threshold(Metrics#{latency}, LatencyThreshold),
    ErrorStatus = check_threshold(Metrics#{error_rate}, ErrorThreshold),
    ScalingStatus = check_threshold(Metrics#{scaling_efficiency}, ScalingThreshold),

    %% Overall status based on worst metric
    Statuses = [CpuStatus, MemoryStatus, LatencyStatus, ErrorStatus, ScalingStatus],
    case lists:member(critical, Statuses) of
        true -> critical;
        _ ->
            case lists:member(warning, Statuses) of
                true -> warning;
                _ -> acceptable
            end
    end.

%% Aggregate validation results
aggregate_validation(ValidationResults) ->
    Total = length(ValidationResults),
    Optimal = length(lists:filter(fun(R) -> R#status =:= optimal end, ValidationResults)),
    Acceptable = length(lists:filter(fun(R) -> R#status =:= acceptable end, ValidationResults)),
    Warning = length(lists:filter(fun(R) -> R#status =:= warning end, ValidationResults)),
    Critical = length(lists:filter(fun(R) -> R#status =:= critical end, ValidationResults)),

    #{
        total => Total,
        optimal => Optimal,
        acceptable => Acceptable,
        warning => Warning,
        critical => Critical,
        score => (Optimal * 100 + Acceptable * 80 + Warning * 60 + Critical * 0) / max(Total, 1)
    }.

%% Generate recommendations
generate_recommendations(Results, ScalingEfficiency, AggregateValidation) ->
    Recommendations = [],

    %% Check for scaling bottlenecks
    case ScalingEfficiency#overall_efficiency < 0.8 of
        true ->
            [recommend_scale_optimization() | Recommendations];
        false ->
            Recommendations
    end,

    %% Check for resource bottlenecks
    ResourceBottlenecks = identify_resource_bottlenecks(Results),
    case ResourceBottlenecks of
        [] -> Recommendations;
        _ ->
            [recommend_resource_optimization(ResourceBottlenecks) | Recommendations]
    end,

    %% Check for connection bottlenecks
    case check_connection_bottlenecks(Results) of
        true -> [recommend_connection_optimization() | Recommendations];
        false -> Recommendations
    end,

    %% Check for CPU bottlenecks
    CpuIssues = lists:filter(fun(R) -> R#test_result.cpu_utilization > 90 end, Results),
    case CpuIssues of
        [] -> Recommendations;
        _ -> [recommend_cpu_optimization() | Recommendations]
    end,

    %% Check for memory issues
    MemoryIssues = lists:filter(fun(R) -> R#test_result.memory_utilization > 90 end, Results),
    case MemoryIssues of
        [] -> Recommendations;
        _ -> [recommend_memory_optimization() | Recommendations]
    end,

    Recommendations.

%% Recommend scale optimization
recommend_scale_optimization() ->
    #{
        recommendation => optimize_scale_configuration,
        priority => high,
        description => "Horizontal scaling efficiency is below optimal",
        actions => [
            "Review load balancing configuration",
            "Check node resource allocation",
            "Optimize connection pooling"
        ]
    }.

%% Recommend resource optimization
recommend_resource_optimization(Bottlenecks) ->
    #{
        recommendation => optimize_resource_allocation,
        priority => medium,
        bottleneck => Bottlenecks,
        description => "Resource bottlenecks detected",
        actions => [
            "Increase CPU allocation for bottlenecked nodes",
            "Optimize memory usage patterns",
            "Consider instance type upgrades"
        ]
    }.

%% Recommend connection optimization
recommend_connection_optimization() ->
    #{
        recommendation => optimize_connection_handling,
        priority => medium,
        description => "Connection handling optimization needed",
        actions => [
            "Increase connection pool size",
            "Optimize TCP settings",
            "Implement connection reuse"
        ]
    }.

%% Recommend CPU optimization
recommend_cpu_optimization() ->
    #{
        recommendation => optimize_cpu_usage,
        priority => high,
        description => "High CPU utilization detected",
        actions => [
            "Profile CPU-intensive operations",
            "Optimize algorithms",
            "Consider load shedding strategies"
        ]
    }.

%% Recommend memory optimization
recommend_memory_optimization() ->
    #{
        recommendation => optimize_memory_usage,
        priority => high,
        description => "High memory utilization detected",
        actions => [
            "Implement garbage collection optimization",
            "Reduce memory footprint",
            "Use more efficient data structures"
        ]
    }.

%% Identify resource bottlenecks
identify_resource_bottlenecks(Results) ->
    lists:filter(fun(Result) ->
        Result#test_result.cpu_utilization > 80 orelse
        Result#test_result.memory_utilization > 80
    end, Results).

%% Check connection bottlenecks
check_connection_bottlenecks(Results) ->
    lists:any(fun(Result) ->
        Efficiency = Result#test_result.total_connections / (Result#test_result.node_count * 10000),
        Efficiency < 0.7
    end, Results).

%% Generate validation report
generate_validation_report(Results) ->
    #{
        test_type => horizontal_scaling,
        timestamp => erlang:system_time(millisecond),
        summary => generate_test_summary(Results),
        scaling_efficiency => analyze_scaling_efficiency(Results),
        performance_metrics => extract_performance_metrics(Results),
        validation_results => generate_validation_results(Results),
        recommendations => generate_recommendations(Results, analyze_scaling_efficiency(Results), aggregate_validation([]))
    }.

%% Generate test summary
generate_test_summary(Results) ->
    TotalNodes = lists:sum([R#test_result.node_count || R <- Results]),
    TotalConnections = lists:sum([R#test_result.total_connections || R <- Results]),
    TotalRPS = lists:sum([R#test_result.total_rps || R <- Results]),
    AvgLatency = lists:sum([R#test_result.avg_latency || R <- Results]) / length(Results),
    AvgErrorRate = lists:sum([R#test_result.error_rate || R <- Results]) / length(Results),

    #{
        total_tests => length(Results),
        total_nodes => TotalNodes,
        total_connections => TotalConnections,
        total_rps => TotalRPS,
        avg_latency => AvgLatency,
        avg_error_rate => AvgErrorRate,
        scaling_efficiency => calculate_scaling_efficiency_summary(Results)
    }.

%% Calculate scaling efficiency summary
calculate_scaling_efficiency_summary(Results) ->
    NodeCounts = [R#test_result.node_count || R <- Results],
    Throughputs = [R#test_result.total_rps || R <- Results],

    {_, Slope, _} = linear_regression(NodeCounts, Throughputs),
    Intercept = hd(Throughputs) - Slope * hd(NodeCounts),

    Efficiency = case lists:last(Throughputs) of
        0 -> 0;
        _ ->
            (lists:last(Throughputs) - Intercept) / (Slope * lists:last(NodeCounts))
    end,

    min(max(Efficiency, 0), 1).

%% Extract performance metrics
extract_performance_metrics(Results) ->
    lists:map(fun(Result) ->
        #{
            node_count => Result#test_result.node_count,
            connections => Result#test_result.total_connections,
            rps => Result#test_result.total_rps,
            latency => Result#test_result.avg_latency,
            error_rate => Result#test_result.error_rate,
            cpu => Result#test_result.cpu_utilization,
            memory => Result#test_result.memory_utilization
        }
    end, Results).

%% Generate validation results
generate_validation_results(Results) ->
    lists:map(fun(Result) ->
        #{
            node_count => Result#test_result.node_count,
            connections => Result#test_result.total_connections,
            rps => Result#test_result.total_rps,
            latency => Result#test_result.avg_latency,
            error_rate => Result#test_result.error_rate,
            cpu => Result#test_result.cpu_utilization,
            memory => Result#test_result.memory_utilization,
            status => determine_result_status(Result)
        }
    end, Results).

%% Determine result status
determine_result_status(Result) ->
    if
        Result#test_result.avg_latency > 100 -> critical;
        Result#test_result.error_rate > 0.01 -> critical;
        Result#test_result.cpu_utilization > 90 -> critical;
        Result#test_result.memory_utilization > 90 -> critical;
        Result#test_result.avg_latency > 50 -> warning;
        Result#test_result.error_rate > 0.005 -> warning;
        Result#test_result.cpu_utilization > 80 -> warning;
        Result#test_result.memory_utilization > 80 -> warning;
        true -> acceptable
    end.

%% Run scaling test
run_scaling_test(NodeCount, TestDuration, State) ->
    %% Setup test
    setup_test_environment(NodeCount),

    %% Configure load
    {Connections, RPS} = generate_test_load(NodeCount, State),

    %% Start monitoring
    start_monitoring(State#state.test_config.monitoring_interval),

    %% Ramp up load
    ramp_up_load(NodeCount, Connections, RPS, State#state.test_config.load_rampup),

    %% Sustain load
    sustain_load(TestDuration - State#state.test_config.load_rampup),

    %% Ramp down
    ramp_down_load(State#state.test_config.load_rampup),

    %% Collect results
    Results = collect_monitoring_results(),

    %% Cleanup
    cleanup_test_environment(),

    Results.

%% Ramp up load
ramp_up_load(NodeCount, TargetConnections, TargetRPS, RampupDuration) ->
    Steps = 10,
    StepDuration = RampupDuration div Steps,

    Rampup = fun(Step) ->
        Connections = (Step / Steps) * TargetConnections,
        RPS = (Step / Steps) * TargetRPS,

        %% Load generation logic here
        start_load_generation(NodeCount, round(Connections), round(RPS)),

        timer:sleep(StepDuration)
    end,

    lists:foreach(Rampup, lists:seq(1, Steps)).

%% Sustain load
sustain_load(Duration) ->
    %% Generate sustained load
    start_sustained_load(),

    %% Monitor for duration
    timer:sleep(Duration).

%% Ramp down load
ramp_down_load(RampdownDuration) ->
    Steps = 10,
    StepDuration = RampdownDuration div Steps,

    Rampdown = fun(Step) ->
        Factor = (Steps - Step) / Steps,
        reduce_load_by_factor(Factor),
        timer:sleep(StepDuration)
    end,

    lists:foreach(Rampdown, lists:seq(1, Steps)).

%% Collect monitoring results
collect_monitoring_results() ->
    %% Collect and aggregate monitoring results
    MonitoringData = get_monitoring_data(),

    %% Process and return results
    process_monitoring_results(MonitoringData).

%% Start monitoring
start_monitoring(Interval) ->
    %% Start periodic monitoring
    start_monitoring_process(Interval),

    %% Initialize monitoring metrics
    initialize_metrics(),

    ok.

%% Stop monitoring
stop_monitoring() ->
    %% Stop monitoring process
    stop_monitoring_process(),

    %% Collect final metrics
    get_final_metrics().

%% Schedule scaling check
schedule_scaling_check() ->
    %% Schedule periodic scaling validation
    Schedule = get_scaling_check_schedule(),

    %% Send periodic message
    erlang:send_after(Schedule, self(), scaling_check),

    ok.

%% Get scaling check schedule
get_scaling_check_schedule() ->
    case application:get_env(erlmcp, scaling_check_interval) of
        {ok, Interval} -> Interval;
        undefined -> 300000  % 5 minutes
    end.

%% Setup test environment
setup_test_environment(NodeCount) ->
    %% Scale nodes to required count
    scale_nodes(NodeCount),

    %% Initialize test data
    initialize_test_data(),

    %% Clear previous metrics
    clear_metrics(),

    ok.

%% Cleanup test environment
cleanup_test_environment() ->
    %% Stop load generation
    stop_load_generation(),

    %% Clear test data
    cleanup_test_data(),

    %% Reset metrics
    reset_metrics(),

    ok.

%% Start load generation
start_load_generation(NodeCount, Connections, RPS) ->
    %% Start load generation processes
    LoadGenerators = start_load_generators(NodeCount, Connections, RPS),

    %% Store load generator references
    store_load_generators(LoadGenerators),

    ok.

%% Stop load generation
stop_load_generation() ->
    %% Get load generators
    LoadGenerators = get_load_generators(),

    %% Stop load generators
    lists:foreach(fun(Pid) ->
        Pid ! stop
    end, LoadGenerators),

    ok.

%% Check scaling issues
check_scaling_issues(Results) ->
    %% Check for critical issues
    CriticalIssues = lists:filter(fun(R) -> R#status =:= critical end, Results),

    case CriticalIssues of
        [] -> ok;
        _ ->
            %% Send alert
            send_scaling_alert(CriticalIssues)
    end.

%% Send scaling alert
send_scaling_alert(Issues) ->
    %% Send alert to monitoring system
    erlmcp_alerting:send_alert(
        scaling_issues,
        #{issues => Issues},
        high
    ).

%% Collect scaling metrics
collect_scaling_metrics() ->
    %% Get current node metrics
    NodeMetrics = collect_node_metrics(),

    %% Get current system metrics
    SystemMetrics = collect_system_metrics(),

    %% Combine metrics
    Combined = NodeMetrics ++ SystemMetrics,

    %% Format for return
    lists:map(fun(M) ->
        #{
            node => M#node_metrics.id,
            connections => M#node_metrics.connections,
            cpu => M#node_metrics.cpu_percent,
            memory => M#node_metrics.memory_percent,
            rps => M#node_metrics.rps,
            timestamp => M#node_metrics.timestamp
        }
    end, Combined).

%% Periodic scaling check
run_periodic_scaling_check(State) ->
    %% Get current node count
    CurrentNodes = get_current_node_count(),

    %% Collect metrics
    Metrics = collect_scaling_metrics(),

    %% Analyze scaling performance
    Analysis = analyze_scaling_performance_at_scale(Metrics, CurrentNodes),

    %% Check against thresholds
    CheckResult = check_scaling_thresholds(Analysis, State#state.scaling_thresholds),

    %% Generate recommendation if needed
    Recommendations = case CheckResult of
        ok -> [];
        issues -> generate_scaling_recommendations(issues, Analysis)
    end,

    %% Return results
    [CheckResult | Recommendations].

%% Analyalyze scaling performance at scale
analyze_scaling_performance_at_scale(Metrics, NodeCount) ->
    %% Calculate aggregate metrics
    TotalConnections = lists:sum([M#{connections} || M <- Metrics]),
    TotalRPS = lists:sum([M#{rps} || M <- Metrics]),
    AvgCPU = lists:sum([M#{cpu} || M <- Metrics]) / length(Metrics),
    AvgMemory = lists:sum([M#{memory} || M <- Metrics]) / length(Metrics),

    %% Calculate efficiency metrics
    ConnectionsPerNode = TotalConnections / max(NodeCount, 1),
    RPSPerNode = TotalRPS / max(NodeCount, 1),

    #{
        node_count => NodeCount,
        total_connections => TotalConnections,
        total_rps => TotalRPS,
        avg_cpu => AvgCPU,
        avg_memory => AvgMemory,
        connections_per_node => ConnectionsPerNode,
        rps_per_node => RPSPerNode,
        efficiency => calculate_efficiency(Metrics)
    }.

%% Calculate efficiency
calculate_efficiency(Metrics) ->
    %% Calculate overall efficiency based on multiple metrics
    CPUUtilization = lists:sum([M#{cpu} || M <- Metrics]) / length(Metrics),
    MemoryUtilization = lists:sum([M#{memory} || M <- Metrics]) / length(Metrics),
    ConnectionEfficiency = lists:sum([M#{connections} || M <- Metrics]) / length(Metrics) / 10000,

    %% Combine metrics (lower is better for utilization, higher for efficiency)
    Efficiency = 1.0 - (CPUUtilization / 100 + MemoryUtilization / 100) / 2,
    Efficiency * ConnectionEfficiency.

%% Check scaling thresholds
check_scaling_thresholds(Analysis, Thresholds) ->
    #{
        cpu_utilization_high := CpuHigh,
        memory_utilization_high := MemHigh,
        connections_per_node := ConnPerNode,
        rps_per_node := RPSPerNode
    } = Thresholds,

    %% Check CPU threshold
    CpuStatus = case Analysis#{avg_cpu} of
        V when V > CpuHigh -> critical;
        V when V > CpuHigh * 0.8 -> warning;
        _ -> acceptable
    end,

    %% Check memory threshold
    MemoryStatus = case Analysis#{avg_memory} of
        V when V > MemHigh -> critical;
        V when V > MemHigh * 0.8 -> warning;
        _ -> acceptable
    end,

    %% Check connection efficiency
    ConnStatus = case Analysis#{connections_per_node} of
        V when V < 8000 -> critical;
        V when V < 9000 -> warning;
        _ -> acceptable
    end,

    %% Check RPS efficiency
    RPSStatus = case Analysis#{rps_per_node} of
        V when V < 80000 -> critical;
        V when V < 90000 -> warning;
        _ -> acceptable
    end,

    %% Determine overall status
    Statuses = [CpuStatus, MemoryStatus, ConnStatus, RPSStatus],
    case lists:member(critical, Statuses) of
        true -> critical;
        _ ->
            case lists:member(warning, Statuses) of
                true -> warning;
                _ -> acceptable
            end
    end,

    #{
        timestamp => erlang:system_time(millisecond),
        analysis => Analysis,
        status => lists:last([CpuStatus, MemoryStatus, ConnStatus, RPSStatus]),
        checks => #{
            cpu => CpuStatus,
            memory => MemoryStatus,
            connections => ConnStatus,
            rps => RPSStatus
        }
    }.

%% Generate scaling recommendations
generate_scaling_recommendations(issues, Analysis) ->
    Recommendations = [],

    %% Check for CPU issues
    case Analysis#{avg_cpu} > 80 of
        true ->
            [recommend_cpu_scaling() | Recommendations];
        false ->
            Recommendations
    end,

    %% Check for memory issues
    case Analysis#{avg_memory} > 80 of
        true ->
            [recommend_memory_scaling() | Recommendations];
        false ->
            Recommendations
    end,

    %% Check for connection issues
    case Analysis#{connections_per_node} < 8000 of
        true ->
            [recommend_connection_scaling() | Recommendations];
        false ->
            Recommendations
    end,

    %% Check for RPS issues
    case Analysis#{rps_per_node} < 80000 of
        true ->
            [recommend_throughput_scaling() | Recommendations];
        false ->
            Recommendations
    end,

    Recommendations.

%% Recommend CPU scaling
recommend_cpu_scaling() ->
    #{
        type => cpu_scaling,
        recommendation => increase_cpu_allocation,
        priority => high,
        reason => "High CPU utilization detected",
        actions => [
            "Increase instance CPU count",
            "Optimize CPU-intensive operations",
            "Consider load balancing improvements"
        ]
    }.

%% Recommend memory scaling
recommend_memory_scaling() ->
    #{
        type => memory_scaling,
        recommendation => increase_memory_allocation,
        priority => high,
        reason => "High memory utilization detected",
        actions => [
            "Increase instance memory",
            "Optimize memory usage patterns",
            "Implement memory pooling"
        ]
    }.

%% Recommend connection scaling
recommend_connection_scaling() ->
    #{
        type => connection_scaling,
        recommendation => optimize_connection_handling,
        priority => medium,
        reason => "Low connection efficiency detected",
        actions => [
            "Increase connection pool size",
            "Optimize connection lifecycle",
            "Implement connection reuse"
        ]
    }.

%% Recommend throughput scaling
recommend_throughput_scaling() ->
    #{
        type => throughput_scaling,
        recommendation => increase_throughput_capacity,
        priority => medium,
        reason => "Low throughput per node detected",
        actions => [
            "Increase RPS capacity",
            "Optimize request processing",
            "Implement request batching"
        ]
    }.

%% Scale nodes
scale_nodes(NodeCount) ->
    %% Implement node scaling logic
    CurrentNodes = get_current_node_count(),

    case NodeCount > CurrentNodes of
        true ->
            %% Scale up
            ScaleUpCount = NodeCount - CurrentNodes,
            scale_up_nodes(ScaleUpCount);
        false ->
            %% Scale down
            ScaleDownCount = CurrentNodes - NodeCount,
            scale_down_nodes(ScaleDownCount)
    end,

    ok.

%% Initialize test data
initialize_test_data() ->
    %% Create test data for validation
    ok.

%% Clear metrics
clear_metrics() ->
    %% Clear previous metrics
    ok.

%% Reset metrics
reset_metrics() ->
    %% Reset metrics after test
    ok.

%% Start monitoring process
start_monitoring_process(Interval) ->
    %% Start monitoring process
    ok.

%% Stop monitoring process
stop_monitoring_process() ->
    %% Stop monitoring process
    ok.

%% Initialize metrics
initialize_metrics() ->
    %% Initialize metrics storage
    ok.

%% Get final metrics
get_final_metrics() ->
    %% Get final metrics after test
    [].

%% Store load generators
store_load_generators(Generators) ->
    %% Store load generator references
    ok.

%% Get load generators
get_load_generators() ->
    %% Get stored load generators
    [].

%% Start load generators
start_load_generators(NodeCount, Connections, RPS) ->
    %% Start load generator processes
    [].

%% Reduce load by factor
reduce_load_by_factor(Factor) ->
    %% Reduce load generation by factor
    ok.

%% Initialize test data
initialize_test_data() ->
    %% Create test data
    ok.

%% Cleanup test data
cleanup_test_data() ->
    %% Clean up test data
    ok.

%% Start sustained load
start_sustained_load() ->
    %% Start sustained load generation
    ok.