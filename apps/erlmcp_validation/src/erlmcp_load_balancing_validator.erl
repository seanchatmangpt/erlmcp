%% @doc Load balancing validation module for erlmcp v3
%% Validates load balancing strategies and configurations including:
%% - Load distribution validation
%% - Health check validation
%% - Failover validation
%% - Performance validation
%% - Strategy validation
%% - Routing validation

-module(erlbcp_load_balancing_validator).
-behaviour(gen_server).
-export([start_link/0, validate/1, run_failover_test/3, get_lb_metrics/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("erlmcp_core/include/erlmcp.hrl").

%% Records
-record.lb_config, {
    strategy :: round_robin | least_conn | ip_hash | least_time,
    health_check_interval :: integer(),
    health_check_timeout :: integer(),
    health_check_threshold :: integer(),
    failover_timeout :: integer(),
    max_connections :: integer(),
    node_weights :: map()
}.

-record.node_status, {
    id :: binary(),
    address :: binary(),
    port :: integer(),
    status :: healthy | unhealthy | draining,
    connections :: integer(),
    cpu_util :: float(),
    memory_util :: float(),
    response_time :: integer(),
    last_check :: integer()
}.

#lb_test_result, {
    test_id :: binary(),
    strategy :: binary(),
    total_requests :: integer(),
    healthy_nodes :: integer(),
    unhealthy_nodes :: integer(),
    avg_response_time :: float(),
    error_rate :: float(),
    load_distribution :: map(),
    health_check_effectiveness :: float(),
    failover_time :: integer()
}.

-record.state, {
    lb_config :: #lb_config{},
    node_status :: list(#node_status{}),
    test_results :: list(#lb_test_result{}),
    validation_thresholds :: map()
}.

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

validate(Config) ->
    gen_server:call(?MODULE, {validate, Config}, 30000).

run_failover_test(NodeCount, FailurePattern, TestDuration) ->
    gen_server:call(?MODULE, {run_failover_test, NodeCount, FailurePattern, TestDuration}, 60000).

get_lb_metrics() ->
    gen_server:call(?MODULE, get_lb_metrics).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize load balancing configuration
    LBConfig = initialize_lb_config(),

    %% Initialize node status
    NodeStatus = initialize_node_status(),

    %% Validation thresholds
    ValidationThresholds = #{
        response_time_high => 100,
        response_time_medium => 50,
        error_rate_threshold => 0.01,
        health_check_interval => 5000,
        health_check_timeout => 30000,
        failover_time => 10000,
        load_distribution_variance => 0.2,
        health_check_effectiveness => 0.95
    },

    %% State initialization
    State = #state{
        lb_config = LBConfig,
        node_status = NodeStatus,
        test_results = [],
        validation_thresholds = ValidationThresholds
    },

    %% Schedule periodic validation
    schedule_validation(),

    %% Start health monitoring
    start_health_monitoring(),

    {ok, State}.

handle_call({validate, Config}, _From, State) ->
    %% Parse configuration
    LBConfig = parse_lb_config(Config),

    %% Run validation tests
    TestResults = run_validation_tests(LBConfig, State),

    %% Generate validation report
    Report = generate_validation_report(TestResults),

    %% Update state
    NewState = State#state{
        lb_config = LBConfig,
        test_results = [TestResults | State#state.test_results]
    },

    {reply, Report, NewState};

handle_call({run_failover_test, NodeCount, FailurePattern, TestDuration}, _From, State) ->
    %% Set up test environment
    TestNodes = setup_test_nodes(NodeCount),

    %% Initialize failure pattern
    initialize_failure_pattern(TestNodes, FailurePattern),

    %% Run failover test
    TestResults = run_failover_test(NodeCount, TestDuration, State),

    %% Analyze failover performance
    Analysis = analyze_failover_performance(TestResults),

    %% Cleanup test environment
    cleanup_test_environment(),

    {reply, {results, TestResults, analysis, Analysis}, State};

handle_call(get_lb_metrics, _From, State) ->
    %% Get current load balancing metrics
    Metrics = collect_lb_metrics(),

    {reply, Metrics, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(health_check, State) ->
    %% Run health checks
    NewNodeStatus = run_health_checks(State#state.lb_config, State#state.node_status),

    %% Update node status
    UpdatedState = State#state{node_status = NewNodeStatus},

    %% Check for unhealthy nodes
    check_unhealthy_nodes(NewNodeStatus),

    {noreply, UpdatedState};

handle_info(validation, State) ->
    %% Run periodic validation
    TestResults = run_periodic_validation(State),

    %% Update state
    NewState = State#state{
        test_results = [TestResults | State#state.test_results]
    },

    %% Check for issues
    check_lb_issues(TestResults),

    {noreply, NewState}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Initialize load balancing configuration
initialize_lb_config() ->
    #lb_config{
        strategy = least_conn,
        health_check_interval = 5000,
        health_check_timeout = 30000,
        health_check_threshold = 3,
        failover_timeout = 10000,
        max_connections = 10000,
        node_weights = #{
            "node-1" => 10,
            "node-2" => 10,
            "node-3" => 10
        }
    }.

%% Initialize node status
initialize_node_status() ->
    %% Initialize healthy nodes
    Nodes = ["node-1", "node-2", "node-3"],
    lists:map(fun(NodeId) ->
        #node_status{
            id = NodeId,
            address = "127.0.0.1",
            port = 8080,
            status = healthy,
            connections = 0,
            cpu_util = 0.0,
            memory_util = 0.0,
            response_time = 0,
            last_check = erlang:system_time(millisecond)
        }
    end, Nodes).

%% Parse load balancing configuration
parse_lb_config(Config) ->
    #lb_config{
        strategy = maps:get(strategy, Config, least_conn),
        health_check_interval = maps:get(health_check_interval, Config, 5000),
        health_check_timeout = maps:get(health_check_timeout, Config, 30000),
        health_check_threshold = maps:get(health_check_threshold, Config, 3),
        failover_timeout = maps:get(failover_timeout, Config, 10000),
        max_connections = maps:get(max_connections, Config, 10000),
        node_weights = maps:get(node_weights, Config, #{})
    }.

%% Run validation tests
run_validation_tests(LBConfig, State) ->
    %% Set up test environment
    setup_test_environment(),

    %% Run distribution validation
    DistributionResult = run_distribution_validation(LBConfig),

    %% Run health check validation
    HealthCheckResult = run_health_check_validation(LBConfig),

    %% Run performance validation
    PerformanceResult = run_performance_validation(LBConfig),

    %% Run failover validation
    FailoverResult = run_failover_validation(LBConfig),

    %% Run routing validation
    RoutingResult = run_routing_validation(LBConfig),

    %% Collect metrics
    Metrics = collect_lb_metrics(),

    %% Calculate scores
    DistributionScore = calculate_distribution_score(DistributionResult),
    HealthScore = calculate_health_score(HealthCheckResult),
    PerformanceScore = calculate_performance_score(PerformanceResult),
    FailoverScore = calculate_failover_score(FailoverResult),
    RoutingScore = calculate_routing_score(RoutingResult),

    OverallScore = (DistributionScore + HealthScore + PerformanceScore + FailoverScore + RoutingScore) / 5,

    %% Create test result
    TestResult = #lb_test_result{
        test_id = generate_test_id(),
        strategy = atom_to_binary(LBConfig#lb_config.strategy),
        total_requests = Metrics#{total_requests},
        healthy_nodes = count_healthy_nodes(State#state.node_status),
        unhealthy_nodes = count_unhealthy_nodes(State#state.node_status),
        avg_response_time = Metrics#{avg_response_time},
        error_rate = Metrics#{error_rate},
        load_distribution = DistributionResult#{distribution},
        health_check_effectiveness = HealthScore,
        failover_time = FailoverResult#{failover_time}
    },

    %% Cleanup test environment
    cleanup_test_environment(),

    #lb_test_result{
        test_result = TestResult,
        metrics = Metrics,
        distribution_score = DistributionScore,
        health_score = HealthScore,
        performance_score = PerformanceScore,
        failover_score = FailoverScore,
        routing_score = RoutingScore,
        overall_score = OverallScore,
        timestamp = erlang:system_time(millisecond)
    }.

%% Run distribution validation
run_distribution_validation(LBConfig) ->
    #lb_config{strategy = Strategy} = LBConfig,

    %% Generate test load
    TestLoad = generate_test_load(),

    %% Calculate expected distribution
    ExpectedDistribution = calculate_expected_distribution(Strategy, TestLoad),

    %% Actual distribution
    ActualDistribution = measure_actual_distribution(Strategy, TestLoad),

    %% Analyze distribution
    DistributionAnalysis = analyze_distribution(ExpectedDistribution, ActualDistribution),

    #{
        strategy => Strategy,
        expected => ExpectedDistribution,
        actual => ActualDistribution,
        distribution => DistributionAnalysis,
        score => calculate_distribution_efficiency(DistributionAnalysis)
    }.

%% Generate test load
generate_test_load() ->
    %% Generate test request patterns
    #{
        requests => 10000,
        concurrent_connections => 1000,
        distribution => random,
        duration => 300000
    }.

%% Calculate expected distribution
calculate_expected_distribution(Strategy, TestLoad) ->
    #{
        requests := Requests,
        concurrent_connections := Connections
    } = TestLoad,

    case Strategy of
        round_robin ->
            calculate_round_robin_distribution(Requests, Connections);
        least_conn ->
            calculate_least_conn_distribution(Requests, Connections);
        ip_hash ->
            calculate_ip_hash_distribution(Requests, Connections);
        least_time ->
            calculate_least_time_distribution(Requests, Connections)
    end.

%% Calculate round robin distribution
calculate_round_robin_distribution(Requests, Connections) ->
    NodeCount = 3, % Based on initial configuration
    PerNode = Requests div NodeCount,
    Remainder = Requests rem NodeCount,

    Distribution = lists:map(fun(NodeId) ->
        Count = case NodeId =< Remainder of
            true -> PerNode + 1;
            false -> PerNode
        end,
        {NodeId, Count}
    end, lists:seq(1, NodeCount)),

    maps:from_list(Distribution).

%% Calculate least connections distribution
calculate_least_conn_distribution(Requests, Connections) ->
    %% Simulate load balancing with least connections
    NodeStatus = initialize_node_status(),

    %% Distribute connections based on current load
    Remaining = Requests,
    RemainingConnections = Connections,

    lists:foldl(fun(_Step, Distribution) ->
        %% Find node with least connections
        {NodeId, Node} = find_least_connected_node(NodeStatus),

        %% Add connections to this node
        NewConnections = Node#node_status.connections + 1,

        %% Update node status
        UpdatedNode = Node#node_status{connections = NewConnections},
        UpdatedStatus = lists:keyreplace(NodeId, #node_status.id, NodeStatus, UpdatedNode),

        %% Update distribution
        Count = maps:get(NodeId, Distribution, 0) + 1,
        Distribution#{NodeId => Count}
    end, #{}, lists:seq(1, Requests)).

%% Find least connected node
find_least_connected_node(NodeStatus) ->
    %% Find node with minimum connections
    lists:foldl(fun(Node, Min) ->
        case Node#node_status.status of
            healthy ->
                case Node#node_status.connections of
                    Current when Current < Min -> Node;
                    _ -> Min
                end;
            _ -> Min
        end
    end, {0, undefined}, NodeStatus).

%% Calculate IP hash distribution
calculate_ip_hash_distribution(Requests, Connections) ->
    %% Simulate IP hash distribution
    IPAddresses = generate_test_ip_addresses(Requests),

    %% Distribute based on IP hash
    Distribution = lists:foldl(fun(IP, Acc) ->
        Hash = erlang:phash2(IP, 3), % 3 nodes
        NodeId = "node-" ++ integer_to_binary(Hash + 1),
        Count = maps:get(NodeId, Acc, 0) + 1,
        Acc#{NodeId => Count}
    end, #{}, IPAddresses),

    Distribution.

%% Calculate least time distribution
calculate_least_time_distribution(Requests, Connections) ->
    %% Simulate least time distribution
    ResponseTimes = generate_test_response_times(Requests),

    %% Distribute based on response time
    Distribution = lists:foldl fun({IP, RT}, Acc) ->
        Hash = erlang:phash2(IP, 3),
        NodeId = "node-" ++ integer_to_binary(Hash + 1),
        Count = maps:get(NodeId, Acc, 0) + 1,
        Acc#{NodeId => Count}
    end, #{}, ResponseTimes),

    Distribution.

%% Generate test IP addresses
generate_test_ip_addresses(Count) ->
    %% Generate random IP addresses
    lists:map(fun(_) ->
        IP = <<(rand:uniform(255)), (rand:uniform(255)),
               (rand:uniform(255)), (rand:uniform(255))>>,
        IP
    end, lists:seq(1, Count)).

%% Generate test response times
generate_test_response_times(Count) ->
    %% Generate random response times
    IPs = generate_test_ip_addresses(Count),
    lists:zip(IPs, [rand:uniform(100) || _ <- lists:seq(1, Count)]).

%% Measure actual distribution
measure_actual_distribution(Strategy, TestLoad) ->
    %% Simulate load balancing with actual distribution
    #{
        requests := Requests
    } = TestLoad,

    %% Send test requests
    Results = simulate_requests(Requests, Strategy),

    %% Count distribution
    Distribution = count_request_distribution(Results),

    Distribution.

%% Simulate requests
simulate_requests(Requests, Strategy) ->
    %% Simulate load balancing requests
    lists:map(fun(RequestId) ->
        SimulateRequest(RequestId, Strategy)
    end, lists:seq(1, Requests)).

%% Simulate single request
simulate_request(RequestId, Strategy) ->
    %% Select node based on strategy
    SelectedNode = select_node_by_strategy(RequestId, Strategy),

    %% Simulate request processing
    ResponseTime = rand:uniform(100),

    #{
        request_id => RequestId,
        node_id => SelectedNode,
        response_time => ResponseTime,
        success => rand:uniform(100) =< 99  % 99% success rate
    }.

%% Select node by strategy
select_node_by_strategy(RequestId, Strategy) ->
    case Strategy of
        round_robin ->
            % Round robin selection
            NodeId = (RequestId rem 3) + 1,
            "node-" ++ integer_to_binary(NodeId);
        least_conn ->
            % Least connections (simplified)
            "node-" ++ integer_to_binary(rand:uniform(3));
        ip_hash ->
            % IP hash
            Hash = erlang:phash2(RequestId, 3),
            "node-" ++ integer_to_binary(Hash + 1);
        least_time ->
            % Least time (random for simulation)
            "node-" ++ integer_to_binary(rand:uniform(3))
    end.

%% Count request distribution
count_request_distribution(Results) ->
    lists:foldl(fun(Result, Acc) ->
        NodeId = Result#{node_id},
        Count = maps:get(NodeId, Acc, 0) + 1,
        Acc#{NodeId => Count}
    end, #{}, Results).

%% Analyze distribution
analyze_distribution(Expected, Actual) ->
    NodeIds = maps:keys(Expected),
    NodeIds = maps:keys(Actual), % Should have same nodes

    %% Calculate distribution metrics
    DistributionMetrics = lists:map(fun(NodeId) ->
        ExpectedCount = maps:get(NodeId, Expected, 0),
        ActualCount = maps:get(NodeId, Actual, 0),
        Deviation = abs(ActualCount - ExpectedCount) / max(ExpectedCount, 1),

        #{
            node_id => NodeId,
            expected => ExpectedCount,
            actual => ActualCount,
            deviation => Deviation
        }
    end, NodeIds),

    #{
        total_requests => lists:sum([E#{expected} || E <- Expected]),
        node_count => length(NodeIds),
        distribution_metrics => DistributionMetrics,
        max_deviation => lists:max([D#{deviation} || D <- DistributionMetrics]),
        avg_deviation => lists:sum([D#{deviation} || D <- DistributionMetrics]) / length(DistributionMetrics)
    }.

%% Calculate distribution efficiency
calculate_distribution_efficiency(Analysis) ->
    #{
        max_deviation := MaxDeviation,
        avg_deviation := AvgDeviation
    } = Analysis,

    %% Score based on deviation (lower deviation = higher score)
    MaxScore = max(0, 1 - MaxDeviation),
    AvgScore = max(0, 1 - AvgDeviation),

    (MaxScore + AvgScore) / 2.

%% Run health check validation
run_health_check_validation(LBConfig) ->
    #lb_config{
        health_check_interval = Interval,
        health_check_timeout = Timeout,
        health_check_threshold = Threshold
    } = LBConfig,

    %% Simulate health checks
    HealthCheckResults = simulate_health_checks(Interval, Timeout, Threshold),

    %% Analyze health check effectiveness
    Effectiveness = analyze_health_checks(HealthCheckResults),

    Effectiveness.

%% Simulate health checks
simulate_health_checks(Interval, Timeout, Threshold) ->
    %% Generate health check results
    Duration = 300000, % 5 minutes
    CheckCount = Duration div Interval,

    lists:map(fun(CheckId) ->
        NodeStatus = simulate_node_health(),
        HealthCheck = #{
            check_id => CheckId,
            timestamp => erlang:system_time(millisecond) + CheckId * Interval,
            node_status => NodeStatus,
            timeout => Timeout,
            threshold => Threshold,
            result => evaluate_health_check(NodeStatus, Threshold)
        }
    end, lists:seq(1, CheckCount)).

%% Simulate node health
simulate_node_health() ->
    case rand:uniform(100) of
        V when V =< 95 -> healthy;  % 95% healthy
        V when V =< 98 -> unhealthy; % 3% unhealthy
        _ -> draining  % 2% draining
    end.

%% Evaluate health check
evaluate_health_check(NodeStatus, Threshold) ->
    case NodeStatus of
        healthy -> pass;
        unhealthy ->
            case rand:uniform(100) of
                V when V =< Threshold -> pass;
                _ -> fail
            end;
        draining -> pass
    end.

%% Analyze health checks
analyze_health_checks(HealthCheckResults) ->
    TotalChecks = length(HealthCheckResults),
    PassedChecks = lists:sum([1 || C <- HealthCheckResults, C#{result} =:= pass]),

    #{
        total_checks => TotalChecks,
        passed_checks => PassedChecks,
        failed_checks => TotalChecks - PassedChecks,
        effectiveness => PassedChecks / max(TotalChecks, 1),
        failed_nodes => identify_failed_nodes(HealthCheckResults)
    }.

%% Identify failed nodes
identify_failed_nodes(HealthCheckResults) ->
    %% Find nodes that failed health checks
    lists:foldl(fun(HealthCheck, Acc) ->
        case HealthCheck#{result} of
            fail ->
                NodeStatus = HealthCheck#{node_status},
                NodeId = NodeStatus#{node_id},
                case maps:get(NodeId, Acc, 0) of
                    0 -> Acc#{NodeId => 1};
                    Count -> Acc#{NodeId => Count + 1}
                end;
            _ -> Acc
        end
    end, #{}, HealthCheckResults).

%% Run performance validation
run_performance_validation(LBConfig) ->
    #lb_config{strategy = Strategy} = LBConfig,

    %% Test different load levels
    LoadLevels = [1000, 5000, 10000, 50000, 100000],

    PerformanceResults = lists:map(fun(Load) ->
        run_load_test(Load, Strategy)
    end, LoadLevels),

    #{
        load_levels => LoadLevels,
        results => PerformanceResults,
        analysis => analyze_performance_results(PerformanceResults)
    }.

%% Run load test
run_load_test(Load, Strategy) ->
    %% Simulate load test
    Results = simulate_requests(Load, Strategy),

    %% Calculate performance metrics
    ResponseTimes = [R#{response_time} || R <- Results],
    SuccessRates = [R#{success} || R <- Results],

    #{
        load => Load,
        avg_response_time => lists:sum(ResponseTimes) / length(ResponseTimes),
        p95_response_time => calculate_percentile(ResponseTimes, 95),
        p99_response_time => calculate_percentile(ResponseTimes, 99),
        error_rate => calculate_error_rate(SuccessRates),
        throughput => Load
    }.

%% Calculate percentile
calculate_percentile(List, Percentile) ->
    Sorted = lists:sort(List),
    Length = length(Sorted),
    Index = ceil(Length * Percentile / 100),
    lists:nth(Index, Sorted).

%% Calculate error rate
calculate_error_rate(SuccessRates) ->
    Failed = lists:sum([0 || S <- SuccessRates, S =:= false]),
    Total = length(SuccessRates),

    Failed / max(Total, 1).

%% Analyze performance results
analyze_performance_results(PerformanceResults) ->
    %% Analyze performance at different load levels
    AvgResponseTimes = [R#{avg_response_time} || R <- PerformanceResults],
    P95ResponseTimes = [R#{p95_response_time} || R <- PerformanceResults],
    ErrorRates = [R#{error_rate} || R <- PerformanceResults],

    #{
        avg_response_trend => calculate_trend(AvgResponseTimes),
        p95_response_trend => calculate_trend(P95ResponseTimes),
        error_rate_trend => calculate_trend(ErrorRates),
        saturation_point => find_saturation_point(PerformanceResults)
    }.

%% Calculate trend
calculate_trend(Values) ->
    case Values of
        [] -> no_trend;
        _ ->
            First = hd(Values),
            Last = lists:last(Values),
            Change = (Last - First) / max(First, 1),
            case Change of
                V when V < -0.1 -> decreasing;
                V when V > 0.1 -> increasing;
                _ -> stable
            end
    end.

%% Find saturation point
find_saturation_point(PerformanceResults) ->
    %% Find load level where error rate starts increasing significantly
    lists:foldl(fun(Result, Acc) ->
        ErrorRate = Result#{error_rate},
        Throughput = Result#{throughput},

        case ErrorRate > 0.01 of
            true -> {Throughput, ErrorRate};
            false -> Acc
        end
    end, undefined, PerformanceResults).

%% Run failover validation
run_failover_validation(LBConfig) ->
    #lb_config{
        failover_timeout = Timeout,
        strategy = Strategy
    } = LBConfig,

    %% Simulate failover scenario
    FailoverResults = simulate_failover(Strategy, Timeout),

    #{
        failover_time => measure_failover_time(FailoverResults),
        success_rate => calculate_failover_success_rate(FailoverResults),
        data_integrity => verify_data_integrity(FailoverResults),
        impact => measure_failover_impact(FailoverResults)
    }.

%% Simulate failover
simulate_failover(Strategy, Timeout) ->
    %% Simulate node failure
    FailureTime = 5000,
    RecoveryTime = 15000,

    SimulatedResults = lists:map(fun(Time) ->
        case Time of
            T when T < FailureTime ->
                % Before failure
                #{
                    time => T,
                    status => normal,
                    selected_node => select_node_by_strategy(T, Strategy),
                    success => true
                };
            T when T < RecoveryTime ->
                % During failure
                #{
                    time => T,
                    status => failover,
                    selected_node => select_node_by_strategy(T, Strategy),
                    success => rand:uniform(100) =< 80  % 80% success during failover
                };
            _ ->
                % After recovery
                #{
                    time => T,
                    status => recovered,
                    selected_node => select_node_by_strategy(T, Strategy),
                    success => rand:uniform(100) =< 99  % Back to normal
                }
        end
    end, lists:seq(0, 30000, 100)),

    SimulatedResults.

%% Measure failover time
measure_failover_time(FailoverResults) ->
    %% Find failover start and end
    FailureTime = find_failover_start(FailoverResults),
    RecoveryTime = find_failover_end(FailoverResults),

    case {FailureTime, RecoveryTime} of
        {Start, End} when is_integer(Start) and is_integer(End) ->
            End - Start;
        _ -> 0
    end.

%% Find failover start
find_failover_start(FailoverResults) ->
    lists:foldl fun(Result, Acc) ->
        case Result#{status} of
            failover when Acc =:= undefined -> Result#{time};
            _ -> Acc
        end
    end, undefined, FailoverResults).

%% Find failover end
find_failover_end(FailoverResults) ->
    lists:foldl fun(Result, Acc) ->
        case Result#{status} of
            recovered when Acc =:= undefined -> Result#{time};
            _ -> Acc
        end
    end, undefined, FailoverResults).

%% Calculate failover success rate
calculate_failover_success_rate(FailoverResults) ->
    Total = length(FailoverResults),
    Success = lists:sum([1 || R <- FailoverResults, R#{success} =:= true]),

    Success / max(Total, 1).

%% Verify data integrity
verify_data_integrity(FailoverResults) ->
    %% Simulate data integrity check
    true. % Placeholder implementation

%% Measure failover impact
measure_failover_impact(FailoverResults) ->
    %% Measure performance impact during failover
    BeforeFailure = lists:takewhile(fun(R) -> R#{status} =/= failover end, FailoverResults),
    DuringFailover = lists:filter(fun(R) -> R#{status} =:= failover end, FailoverResults),
    AfterRecovery = lists:dropwhile(fun(R) -> R#{status} =/= recovered end, FailoverResults),

    #{
        response_impact => measure_response_impact(BeforeFailure, DuringFailover, AfterRecovery),
        throughput_impact => measure_throughput_impact(BeforeFailure, DuringFailover, AfterRecovery)
    }.

%% Measure response impact
measure_response_impact(Before, During, After) ->
    BeforeAvg = lists:sum([R#{response_time} || R <- Before]) / max(length(Before), 1),
    DuringAvg = lists:sum([R#{response_time} || R <- During]) / max(length(During), 1),
    AfterAvg = lists:sum([R#{response_time} || R <- After]) / max(length(After), 1),

    #{
        before => BeforeAvg,
        during => DuringAvg,
        after => AfterAvg,
        increase => DuringAvg - BeforeAvg,
        recovery => AfterAvg - DuringAvg
    }.

%% Measure throughput impact
measure_throughput_impact(Before, During, After) ->
    BeforeSuccess = lists:sum([1 || R <- Before, R#{success} =:= true]) / max(length(Before), 1),
    DuringSuccess = lists:sum([1 || R <- During, R#{success} =:= true]) / max(length(During), 1),
    AfterSuccess = lists:sum([1 || R <- After, R#{success} =:= true]) / max(length(After), 1),

    #{
        before => BeforeSuccess,
        during => DuringSuccess,
        after => AfterSuccess,
        decrease => DuringSuccess - BeforeSuccess,
        recovery => AfterSuccess - DuringSuccess
    }.

%% Run routing validation
run_routing_validation(LBConfig) ->
    #lb_config{strategy = Strategy} = LBConfig,

    %% Test routing correctness
    RoutingTests = test_routing_correctness(Strategy),

    #{
        routing_tests => RoutingTests,
        accuracy => calculate_routing_accuracy(RoutingTests),
        consistency => check_routing_consistency(RoutingTests)
    }.

%% Test routing correctness
test_routing_correctness(Strategy) ->
    %% Test various request patterns
    TestRequests = [
        {<<"test1">>, 1},
        {<<"test2">>, 2},
        {<<"test3">>, 3},
        {<<"test4">>, 4},
        {<<"test5">>, 5}
    ],

    lists:map fun({RequestId, Step}) ->
        ExpectedNode = calculate_expected_node(RequestId, Strategy, Step),
        ActualNode = select_node_by_strategy(RequestId, Strategy),

        #{
            request_id => RequestId,
            step => Step,
            expected_node => ExpectedNode,
            actual_node => ActualNode,
            correct => ExpectedNode =:= ActualNode
        }
    end, TestRequests).

%% Calculate expected node
calculate_expected_node(RequestId, Strategy, Step) ->
    case Strategy of
        round_robin ->
            Expected = (RequestId + Step - 1) rem 3 + 1,
            "node-" ++ integer_to_binary(Expected);
        least_conn ->
            "node-1"; % Simplified
        ip_hash ->
            Hash = erlang:phash2(RequestId, 3),
            "node-" ++ integer_to_binary(Hash + 1);
        least_time ->
            "node-1" % Simplified
    end.

%% Calculate routing accuracy
calculate_routing_accuracy(RoutingTests) ->
    Correct = lists:sum([1 || T <- RoutingTests, T#{correct} =:= true]),
    Total = length(RoutingTests),

    Correct / max(Total, 1).

%% Check routing consistency
check_routing_consistency(RoutingTests) ->
    %% Check that same request always goes to same node (except for round-robin)
    Grouped = group_by_request_id(RoutingTests),
    Consistency = lists:map(fun({RequestId, Tests}) ->
        UniqueNodes = sets:to_list(sets:from_list([T#{actual_node} || T <- Tests])),
        #{
            request_id => RequestId,
            consistent => length(UniqueNodes) =< 1,
            nodes => UniqueNodes
        }
    end, Grouped),

    #{
        consistency_rate => lists:sum([1 || C <- Consistency, C#{consistent} =:= true]) / max(length(Consistency), 1),
        inconsistencies => [C#{request_id} || C <- Consistency, C#{consistent} =:= false]
    }.

%% Group by request ID
group_by_request_id(RoutingTests) ->
    lists:foldl(fun(Test, Acc) ->
        RequestId = Test#{request_id},
        case maps:get(RequestId, Acc, []) of
            [] -> Acc#{RequestId => [Test]};
            Tests -> Acc#{RequestId => [Test | Tests]}
        end
    end, #{}, RoutingTests).

%% Calculate distribution score
calculate_distribution_score(DistributionResult) ->
    #{
        score := Score
    } = DistributionResult,

    Score.

%% Calculate health score
calculate_health_score(HealthCheckResult) ->
    #{
        effectiveness := Effectiveness
    } = HealthCheckResult,

    Effectiveness.

%% Calculate performance score
calculate_performance_score(PerformanceResult) ->
    #{
        analysis := Analysis
    } = PerformanceResult,

    #{
        error_rate_trend := ErrorTrend,
        saturation_point := Saturation
    } = Analysis,

    Score = case ErrorTrend of
        increasing -> 0.5;
        stable -> 0.8;
        decreasing -> 1.0
    end,

    case Saturation of
        undefined -> Score;  % No saturation
        _ -> min(Score, 0.7) % Adjust for saturation
    end.

%% Calculate failover score
calculate_failover_score(FailoverResult) ->
    #{
        failover_time := FailoverTime,
        success_rate := SuccessRate,
        impact := Impact
    } = FailoverResult,

    TimeScore = case FailoverTime of
        T when T =< 5000 -> 1.0;
        T when T =< 10000 -> 0.8;
        T when T =< 20000 -> 0.5;
        _ -> 0.0
    end,

    SuccessScore = SuccessRate,

    ImpactScore = case Impact#{response_impact}#{increase} of
        I when I =< 50 -> 1.0;
        I when I =< 100 -> 0.8;
        I when I =< 200 -> 0.5;
        _ -> 0.0
    end,

    (TimeScore + SuccessScore + ImpactScore) / 3.

%% Calculate routing score
calculate_routing_score(RoutingResult) ->
    #{
        accuracy := Accuracy,
        consistency := Consistency
    } = RoutingResult,

    ConsistencyScore = Consistency#{consistency_rate},

    (Accuracy + ConsistencyScore) / 2.

%% Generate validation report
generate_validation_report(TestResults) ->
    #lb_test_result{
        test_result = TestResult,
        metrics = Metrics,
        distribution_score = DistScore,
        health_score = HealthScore,
        performance_score = PerfScore,
        failover_score = FailoverScore,
        routing_score = RoutingScore,
        overall_score = OverallScore
    } = TestResults,

    #{
        test_type => load_balancing_validation,
        timestamp => erlang:system_time(millisecond),
        summary => #{
            strategy => TestResult#{strategy},
            total_requests => TestResult#{total_requests},
            healthy_nodes => TestResult#{healthy_nodes},
            unhealthy_nodes => TestResult#{unhealthy_nodes},
            avg_response_time => TestResult#{avg_response_time},
            error_rate => TestResult#{error_rate},
            scores => #{
                distribution => DistScore,
                health => HealthScore,
                performance => PerfScore,
                failover => FailoverScore,
                routing => RoutingScore,
                overall => OverallScore
            }
        },
        load_distribution => TestResult#{load_distribution},
        health_check_effectiveness => TestResult#{health_check_effectiveness},
        failover_time => TestResult#{failover_time},
        recommendations => generate_lb_recommendations(TestResults),
        next_steps => determine_lb_next_steps(TestResults)
    }.

%% Generate load balancing recommendations
generate_lb_recommendations(TestResults) ->
    Recommendations = [],

    %% Check distribution score
    case TestResults#{distribution_score} < 0.8 of
        true ->
            [recommend_distribution_improvement() | Recommendations];
        false ->
            Recommendations
    end,

    %% Check health score
    case TestResults#{health_score} < 0.9 of
        true ->
            [recommend_health_check_improvement() | Recommendations];
        false ->
            Recommendations
    end,

    %% Check performance score
    case TestResults#{performance_score} < 0.8 of
        true ->
            [recommend_performance_optimization() | Recommendations];
        false ->
            Recommendations
    end,

    %% Check failover score
    case TestResults#{failover_score} < 0.8 of
        true ->
            [recommend_failover_improvement() | Recommendations];
        false ->
            Recommendations
    end,

    %% Check routing score
    case TestResults#{routing_score} < 0.9 of
        true ->
            [recommend_routing_optimization() | Recommendations];
        false ->
            Recommendations
    end,

    Recommendations.

%% Recommend distribution improvement
recommend_distribution_improvement() ->
    #{
        recommendation => improve_distribution,
        priority => medium,
        reason => "Load distribution score below threshold",
        actions => [
            "Adjust node weights",
            "Change load balancing strategy",
            "Implement dynamic load balancing"
        ]
    }.

%% Recommend health check improvement
recommend_health_check_improvement() ->
    #{
        recommendation => improve_health_checks,
        priority => high,
        reason => "Health check effectiveness below threshold",
        actions => [
            "Adjust health check interval",
            "Increase health check timeout",
            "Improve health check endpoints"
        ]
    }.

%% Recommend performance optimization
recommend_performance_optimization() ->
    #{
        recommendation => optimize_performance,
        priority => medium,
        reason => "Performance score below threshold",
        actions => [
            "Implement connection pooling",
            "Optimize request routing",
            "Add caching layer"
        ]
    }.

%% Recommend failover improvement
recommend_failover_improvement() ->
    #{
        recommendation => improve_failover,
        priority => high,
        reason => "Failover score below threshold",
        actions => [
            "Reduce failover timeout",
            "Add node monitoring",
            "Implement proactive failover"
        ]
    }.

%% Recommend routing optimization
recommend_routing_optimization() ->
    #{
        recommendation => optimize_routing,
        priority => medium,
        reason => "Routing score below threshold",
        actions => [
            "Review routing algorithm",
            "Add routing validation",
            "Implement routing consistency checks"
        ]
    }.

%% Determine next steps
determine_lb_next_steps(TestResults) ->
    NextSteps = [],

    %% Check if strategy change needed
    case needs_strategy_change(TestResults) of
        true -> [recommend_strategy_change() | NextSteps];
        false -> NextSteps
    end,

    %% Check if infrastructure changes needed
    case needs_infrastructure_changes(TestResults) of
        true -> [recommend_infrastructure_changes() | NextSteps];
        false -> NextSteps
    end,

    %% Check if monitoring needed
    case needs_monitoring_improvements(TestResults) of
        true -> [recommend_monitoring_improvements() | NextSteps];
        false -> NextSteps
    end,

    NextSteps.

%% Check if strategy change needed
needs_strategy_change(TestResults) ->
    TestResults#{distribution_score} < 0.6 orelse
    TestResults#{performance_score} < 0.6.

%% Check if infrastructure changes needed
needs_infrastructure_changes(TestResults) ->
    TestResults#{failover_score} < 0.6 orelse
    TestResults#{routing_score} < 0.7.

%% Check if monitoring improvements needed
needs_monitoring_improvements(TestResults) ->
    TestResults#{health_score} < 0.8.

%% Recommend strategy change
recommend_strategy_change() ->
    #{
        action => change_strategy,
        current_strategy => least_conn,
        recommended_strategy => least_time,
        timeline => 7200  % 2 hours
    }.

%% Recommend infrastructure changes
recommend_infrastructure_changes() ->
    #{
        action => infrastructure_changes,
        description => "Add more nodes to improve distribution",
        recommended_nodes => 4,
        timeline => 14400  % 4 hours
    }.

%% Recommend monitoring improvements
recommend_monitoring_improvements() ->
    #{
        action => improve_monitoring,
        description => "Enhance monitoring for better health detection",
        timeline => 3600  % 1 hour
    }.

%% Collect load balancing metrics
collect_lb_metrics() ->
    %% Get current metrics
    #{
        total_requests => get_total_requests(),
        healthy_nodes => count_healthy_nodes(),
        unhealthy_nodes => count_unhealthy_nodes(),
        avg_response_time => get_avg_response_time(),
        error_rate => get_error_rate()
    }.

%% Get total requests
get_total_requests() ->
    0. % Placeholder implementation

%% Count healthy nodes
count_healthy_nodes(NodeStatus) ->
    lists:sum([1 || N <- NodeStatus, N#{status} =:= healthy]).

%% Count unhealthy nodes
count_unhealthy_nodes(NodeStatus) ->
    lists:sum([1 || N <- NodeStatus, N#{status} =:= unhealthy orelse N#{status} =:= draining]).

%% Get average response time
get_avg_response_time() ->
    0. % Placeholder implementation

%% Get error rate
get_error_rate() ->
    0.0. % Placeholder implementation

%% Run failover test
run_failover_test(NodeCount, TestDuration, State) ->
    %% Initialize test nodes
    TestNodes = create_test_nodes(NodeCount),

    %% Generate test load
    TestLoad = generate_test_load(),

    %% Monitor before failure
    BeforeFailure = monitor_test_performance(TestNodes, TestLoad, 5000),

    %% Inject failure
    inject_failure(TestNodes),

    %% Monitor during failure
    DuringFailure = monitor_test_performance(TestNodes, TestLoad, 5000),

    %% Recover failure
    recover_failure(TestNodes),

    %% Monitor after recovery
    AfterRecovery = monitor_test_performance(TestNodes, TestLoad, 5000),

    %% Generate test result
    TestResult = #lb_test_result{
        test_id = generate_test_id(),
        strategy = "least_conn",
        total_requests = TestLoad#{requests},
        healthy_nodes = NodeCount - 1, % One node failed
        unhealthy_nodes = 1,
        avg_response_time = calculate_avg_response([BeforeFailure, DuringFailure, AfterRecovery]),
        error_rate = calculate_avg_error([BeforeFailure, DuringFailure, AfterRecovery]),
        load_distribution = measure_distribution_after_failover(TestNodes),
        health_check_effectiveness = 0.9,
        failover_time = measure_failover_time(TestNodes)
    },

    TestResult.

%% Create test nodes
create_test_nodes(NodeCount) ->
    lists:map(fun(Id) ->
        #node_status{
            id = "node-" ++ integer_to_binary(Id),
            address = "127.0.0.1",
            port = 8080 + Id,
            status = healthy,
            connections = 0,
            cpu_util = 0.0,
            memory_util = 0.0,
            response_time = 0,
            last_check = erlang:system_time(millisecond)
        }
    end, lists:seq(1, NodeCount)).

%% Monitor test performance
monitor_test_performance(TestNodes, TestLoad, Duration) ->
    %% Monitor performance during test
    Start = erlang:system_time(millisecond),
    End = Start + Duration,

    while(fun() -> erlang:system_time(millisecond) < End end,
        fun() ->
            %% Send test requests
            Requests = TestLoad#{requests},
            simulate_requests(Requests, least_conn),

            %% Update node metrics
            lists:foreach fun(Node) ->
                update_node_metrics(Node)
            end, TestNodes)
    end,

    TestNodes.

%% Inject failure
inject_failure(TestNodes) ->
    %% Simulate node failure
    FailedNode = hd(TestNodes),
    FailedNode#node_status{status = unhealthy},

    ok.

%% Recover failure
recover_failure(TestNodes) ->
    %% Simulate node recovery
    FailedNode = hd(TestNodes),
    FailedNode#node_status{status = healthy},

    ok.

%% Update node metrics
update_node_metrics(Node) ->
    %% Update node metrics
    ResponseTime = rand:uniform(100),
    CpuUtil = rand:uniform(50),
    MemoryUtil = rand:uniform(60),

    Node#node_status{
        response_time = ResponseTime,
        cpu_util = CpuUtil,
        memory_util = MemoryUtil,
        last_check = erlang:system_time(millisecond)
    }.

%% Calculate average response time
calculate_avg_response(Results) ->
    ResponseTimes = lists:flatten([N#{response_time} || N <- Results]),
    lists:sum(ResponseTimes) / length(ResponseTimes).

%% Calculate average error rate
calculate_avg_error(Results) ->
    ErrorRates = lists:flatten([N#{error_rate} || N <- Results]),
    lists:sum(ErrorRates) / length(ErrorRates).

%% Measure distribution after failover
measure_distribution_after_failover(TestNodes) ->
    %% Measure distribution after failover
    #{
        healthy_nodes => lists:sum([1 || N <- TestNodes, N#{status} =:= healthy]),
        unhealthy_nodes => lists:sum([1 || N <- TestNodes, N#{status} =/= healthy])
    }.

%% Measure failover time
measure_failover_time(TestNodes) ->
    10000. % Placeholder implementation

%% Analyze failover performance
analyze_failover_performance(TestResults) ->
    #{
        failover_time := FailoverTime,
        error_rate := ErrorRate,
        recovery_metrics := RecoveryMetrics
    } = TestResults,

    #{
        effectiveness => calculate_failover_effectiveness(FailoverTime, ErrorRate),
        recommendations => generate_failover_recommendations(TestResults)
    }.

%% Calculate failover effectiveness
calculate_failover_effectiveness(FailoverTime, ErrorRate) ->
    TimeScore = case FailoverTime of
        T when T =< 5000 -> 1.0;
        T when T =< 10000 -> 0.8;
        _ -> 0.5
    end,

    ErrorScore = case ErrorRate of
        E when E =< 0.01 -> 1.0;
        E when E =< 0.05 -> 0.8;
        _ -> 0.5
    end,

    (TimeScore + ErrorScore) / 2.

%% Generate failover recommendations
generate_failover_recommendations(TestResults) ->
    Recommendations = [],

    case TestResults#{failover_time} > 10000 of
        true ->
            [recommend_failover_optimization() | Recommendations];
        false ->
            Recommendations
    end,

    case TestResults#{error_rate} > 0.05 of
        true ->
            [recommend_error_handling_improvement() | Recommendations];
        false ->
            Recommendations
    end,

    Recommendations.

%% Recommend failover optimization
recommend_failover_optimization() ->
    #{
        recommendation => optimize_failover,
        priority => high,
        reason => "Failover time too long",
        actions => [
            "Reduce health check interval",
            "Implement faster failover detection",
            "Optimize node selection algorithm"
        ]
    }.

%% Recommend error handling improvement
recommend_error_handling_improvement() ->
    #{
        recommendation => improve_error_handling,
        priority => medium,
        reason => "High error rate during failover",
        actions => [
            "Implement retry mechanism",
            "Add error recovery circuit breaker",
            "Improve error logging"
        ]
    }.

%% Setup test environment
setup_test_environment() ->
    %% Setup test environment
    ok.

%% Cleanup test environment
cleanup_test_environment() ->
    %% Cleanup test environment
    ok.

%% Setup test nodes
setup_test_nodes(NodeCount) ->
    %% Setup test nodes
    [].

%% Initialize failure pattern
initialize_failure_pattern(TestNodes, FailurePattern) ->
    %% Initialize failure pattern
    ok.

%% Cleanup test environment
cleanup_test_environment() ->
    %% Cleanup test environment
    ok.

%% Run health checks
run_health_checks(LBConfig, NodeStatus) ->
    %% Run health checks for all nodes
    lists:map fun(Node) ->
        HealthResult = check_node_health(LBConfig, Node),
        Node#node_status{status = HealthResult}
    end, NodeStatus.

%% Check node health
check_node_health(LBConfig, Node) ->
    #lb_config{
        health_check_timeout = Timeout,
        health_check_threshold = Threshold
    } = LBConfig,

    %% Simulate health check
    case simulate_health_check(Node, Timeout) of
        pass -> healthy;
        fail ->
            case Node#node_status.status of
                unhealthy -> draining;  % Already unhealthy, move to draining
                _ -> unhealthy
            end
    end.

%% Simulate health check
simulate_health_check(Node, Timeout) ->
    %% Simulate health check
    case rand:uniform(100) of
        V when V =< 95 -> pass;
        _ -> fail
    end.

%% Count healthy nodes
count_healthy_nodes(NodeStatus) ->
    lists:sum([1 || N <- NodeStatus, N#{status} =:= healthy]).

%% Count unhealthy nodes
count_unhealthy_nodes(NodeStatus) ->
    lists:sum([1 || N <- NodeStatus, N#{status} =/= healthy]).

%% Check unhealthy nodes
check_unhealthy_nodes(NodeStatus) ->
    %% Check for unhealthy nodes
    UnhealthyNodes = [N || N <- NodeStatus, N#{status} =/= healthy],

    case UnhealthyNodes of
        [] -> ok;
        _ ->
            %% Send alert for unhealthy nodes
            send_unhealthy_node_alert(UnhealthyNodes)
    end.

%% Send unhealthy node alert
send_unhealthy_node_alert(UnhealthyNodes) ->
    %% Send alert
    erlmcp_alerting:send_alert(
        unhealthy_nodes_detected,
        #{unhealthy_nodes => UnhealthyNodes},
        medium
    ).

%% Schedule validation
schedule_validation() ->
    Schedule = get_validation_schedule(),

    erlang:send_after(Schedule, self(), validation),

    ok.

%% Get validation schedule
get_validation_schedule() ->
    case application:get_env(erlmcp, lb_validation_interval) of
        {ok, Interval} -> Interval;
        undefined -> 300000  % 5 minutes
    end.

%% Start health monitoring
start_health_monitoring() ->
    %% Start health monitoring
    Schedule = get_health_check_schedule(),

    erlang:send_after(Schedule, self(), health_check),

    ok.

%% Get health check schedule
get_health_check_schedule() ->
    case application:get_env(erlmcp, health_check_interval) of
        {ok, Interval} -> Interval;
        undefined -> 5000  % 5 seconds
    end.

%% Run periodic validation
run_periodic_validation(State) ->
    %% Get current LB configuration
    CurrentConfig = get_current_lb_config(),

    %% Run validation
    Results = run_validation_tests(CurrentConfig, State),

    Results.

%% Get current LB configuration
get_current_lb_config() ->
    %% Get current load balancing configuration
    #lb_config{
        strategy = least_conn,
        health_check_interval = 5000,
        health_check_timeout = 30000,
        health_check_threshold = 3,
        failover_timeout = 10000,
        max_connections = 10000,
        node_weights = #{}
    }.

%% Check load balancing issues
check_lb_issues(TestResults) ->
    #lb_test_result{
        overall_score = OverallScore,
        distribution_score = DistributionScore,
        health_score = HealthScore,
        performance_score = PerformanceScore,
        failover_score = FailoverScore
    } = TestResults,

    %% Check for critical issues
    case OverallScore < 0.7 of
        true ->
            send_overall_score_alert(TestResults);
        false ->
            ok
    end,

    %% Check individual scores
    if
        DistributionScore < 0.6 ->
            send_distribution_alert(TestResults);
        HealthScore < 0.7 ->
            send_health_alert(TestResults);
        PerformanceScore < 0.6 ->
            send_performance_alert(TestResults);
        FailoverScore < 0.6 ->
            send_failover_alert(TestResults);
        true ->
            ok
    end.

%% Send overall score alert
send_overall_score_alert(TestResults) ->
    %% Send overall score alert
    erlmcp_alerting:send_alert(
        lb_overall_score_low,
        #{test_results => TestResults},
        high
    ).

%% Send distribution alert
send_distribution_alert(TestResults) ->
    %% Send distribution alert
    erlmcp_alerting:send_alert(
        lb_distribution_issues,
        #{test_results => TestResults},
        medium
    ).

%% Send health alert
send_health_alert(TestResults) ->
    %% Send health alert
    erlmcp_alerting:send_alert(
        lb_health_issues,
        #{test_results => TestResults},
        high
    ).

%% Send performance alert
send_performance_alert(TestResults) ->
    %% Send performance alert
    erlmcp_alerting:send_alert(
        lb_performance_issues,
        #{test_results => TestResults},
        medium
    ).

%% Send failover alert
send_failover_alert(TestResults) ->
    %% Send failover alert
    erlmcp_alerting:send_alert(
        lb_failover_issues,
        #{test_results => TestResults},
        high
    ).

%% Generate test ID
generate_test_id() ->
    %% Generate unique test ID
    integer_to_binary(erlang:system_time(millisecond)).