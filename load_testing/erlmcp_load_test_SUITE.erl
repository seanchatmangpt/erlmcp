-module(erlmcp_load_test_SUITE).

-author("erlmcp AGI Swarm").
-vsn("3.0.0").

%% Suite exports
-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    %% Core load testing
    concurrent_user_simulation/1,
    throughput_testing/1,
    stress_testing_breaking_points/1,
    latency_analysis_optimization/1,

    %% Resource and infrastructure testing
    resource_utilization_monitoring/1,
    database_load_testing/1,
    network_performance_testing/1,
    cache_effectiveness_testing/1,
    connection_pool_testing/1,
    load_balancing_validation/1,
    failover_testing_under_load/1,

    %% Advanced testing
    performance_regression_detection/1,
    peak_load_handling/1,
    multi_protocol_testing/1,

    %% Enterprise features
    load_test_environment_setup/1,
    performance_metrics_collection/1,
    performance_analysis_dashboard/1,
    regression_testing_automation/1,
    load_testing_procedures/1,
    capacity_planning_tools/1,
    performance_optimization_recommendations/1
]).

%% Include dependencies
-include_lib("common/include/test.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% SUITE CONFIGURATION
%%====================================================================

suite() ->
    [{timetrap, {hours, 24}},  % 24-hour timeout for comprehensive suite
     {require, erlmcp},
     {require, observability},
     {require, load_generator}].

all() ->
    [
        %% Core load testing
        concurrent_user_simulation,
        throughput_testing,
        stress_testing_breaking_points,
        latency_analysis_optimization,

        %% Resource and infrastructure testing
        resource_utilization_monitoring,
        database_load_testing,
        network_performance_testing,
        cache_effectiveness_testing,
        connection_pool_testing,
        load_balancing_validation,
        failover_testing_under_load,

        %% Advanced testing
        performance_regression_detection,
        peak_load_handling,
        multi_protocol_testing,

        %% Enterprise features
        load_test_environment_setup,
        performance_metrics_collection,
        performance_analysis_dashboard,
        regression_testing_automation,
        load_testing_procedures,
        capacity_planning_tools,
        performance_optimization_recommendations
    ].

%%====================================================================
%% SUITE INITIALIZATION
%%====================================================================

init_per_suite(Config) ->
    %% Initialize load testing infrastructure
    {ok, _} = application:ensure_all_started(erlmcp),
    {ok, _} = application:ensure_all_started(observability),
    {ok, _} = application:ensure_all_started(load_generator),

    %% Initialize distributed load testing nodes
    case net_adm:ping('load_test_node1@127.0.0.1') of
        pong -> ok;
        pang ->
            %% Start load test nodes
            start_load_test_nodes()
    end,

    %% Initialize monitoring and metrics collection
    initialize_monitoring(),

    %% Configure load testing parameters
    LoadConfig = #{
        max_concurrent_users => 10000,
        target_throughput => 10000,
        test_duration => 3600000,  % 1 hour
        warmup_duration => 300000,  % 5 minutes
        ramp_up_duration => 600000,  % 10 minutes
        ramp_down_duration => 600000,  % 10 minutes
        sampling_interval => 1000  % 1 second
    },

    Config ++ [{load_config, LoadConfig}].

end_per_suite(Config) ->
    %% Cleanup load testing infrastructure
    cleanup_load_testing(),

    %% Stop monitoring
    stop_monitoring(),

    %% Generate final report
    generate_load_test_report(Config),

    ok.

%%====================================================================
%% TEST CASE INITIALIZATION
%%====================================================================

init_per_testcase(concurrent_user_simulation, Config) ->
    %% Initialize concurrent user simulation
    UserSimulationConfig = #{
        user_profiles => generate_user_profiles(10000),
        session_patterns => generate_session_patterns(),
        behavior_models => generate_behavior_models()
    },
    Config ++ [{user_simulation_config, UserSimulationConfig}];

init_per_testcase(throughput_testing, Config) ->
    %% Initialize throughput testing
    ThroughputConfig = #{
        request_types => generate_request_types(),
        payload_variations => generate_payload_variations(),
        error_scenarios => generate_error_scenarios()
    },
    Config ++ [{throughput_config, ThroughputConfig}];

init_per_testcase(stress_testing_breaking_points, Config) ->
    %% Initialize stress testing
    StressConfig = #{
        load_profiles => generate_load_profiles(),
        failure_injection => generate_failure_injection(),
        system_limits => determine_system_limits()
    },
    Config ++ [{stress_config, StressConfig}];

init_per_testcase(latency_analysis_optimization, Config) ->
    %% Initialize latency analysis
    LatencyConfig = #{
        percentiles => [50, 75, 90, 95, 99, 99.9, 99.99],
        latency_thresholds => #{
            critical => 1000,
            warning => 500,
            acceptable => 100
        },
        optimization_strategies => generate_optimization_strategies()
    },
    Config ++ [{latency_config, LatencyConfig}];

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_Testcase, Config) ->
    %% Cleanup test case specific resources
    cleanup_test_resources(Config),
    ok.

%%====================================================================
%% CORE LOAD TESTING TEST CASES
%%====================================================================

concurrent_user_simulation(Config) ->
    #{max_concurrent_users := MaxUsers} = ?config(load_config, Config),
    #{user_simulation_config := UserConfig} = Config,

    %% Start user simulation
    {ok, SimulationPid} = erlmcp_user_simulation:start(UserConfig),

    %% Ramp up users
    ok = erlmcp_user_simulation:ramp_up(SimulationPid, MaxUsers),

    %% Monitor user simulation
    Results = monitor_user_simulation(SimulationPid, MaxUsers),

    %% Analyze results
    Analysis = analyze_concurrent_user_results(Results),

    %% Verify performance requirements
    verify_concurrent_user_performance(Analysis),

    %% Cleanup
    ok = erlmcp_user_simulation:stop(SimulationPid),

    %% Store results
    store_test_results(concurrent_user_simulation, Analysis),

    Analysis.

throughput_testing(Config) ->
    #{target_throughput := TargetThroughput} = ?config(load_config, Config),
    #{throughput_config := ThroughputConfig} = Config,

    %% Start throughput test
    {ok, ThroughputPid} = erlmcp_throughput_test:start(ThroughputConfig),

    %% Ramp up to target throughput
    ok = erlmcp_throughput_test:ramp_up(ThroughputPid, TargetThroughput),

    %% Maintain target throughput
    Results = monitor_throughput_test(ThroughputPid, TargetThroughput),

    %% Analyze throughput performance
    Analysis = analyze_throughput_results(Results),

    %% Verify throughput requirements
    verify_throughput_performance(Analysis, TargetThroughput),

    %% Cleanup
    ok = erlmcp_throughput_test:stop(ThroughputPid),

    %% Store results
    store_test_results(throughput_testing, Analysis),

    Analysis.

stress_testing_breaking_points(Config) ->
    #{stress_config := StressConfig} = Config,

    %% Start stress testing
    {ok, StressPid} = erlmcp_stress_test:start(StressConfig),

    %% Execute stress scenarios
    Results = execute_stress_scenarios(StressPid),

    %% Identify breaking points
    BreakingPoints = identify_breaking_points(Results),

    %% Analyze system behavior under stress
    Analysis = analyze_stress_results(Results, BreakingPoints),

    %% Verify system resilience
    verify_system_resilience(Analysis, BreakingPoints),

    %% Cleanup
    ok = erlmcp_stress_test:stop(StressPid),

    %% Store results
    store_test_results(stress_testing, Analysis),

    Analysis.

latency_analysis_optimization(Config) ->
    #{latency_config := LatencyConfig} = Config,

    %% Start latency test
    {ok, LatencyPid} = erlmcp_latency_test:start(LatencyConfig),

    %% Collect latency measurements
    Results = collect_latency_measurements(LatencyPid),

    %% Analyze latency distribution
    LatencyAnalysis = analyze_latency_distribution(Results, LatencyConfig),

    %% Identify optimization opportunities
    OptimizationOpportunities = identify_optimization_opportunities(LatencyAnalysis),

    %% Apply optimizations
    OptimizedResults = apply_optimizations(OptimizationOpportunities),

    %% Verify optimization effectiveness
    verify_optimization_effectiveness(LatencyAnalysis, OptimizedResults),

    %% Cleanup
    ok = erlmcp_latency_test:stop(LatencyPid),

    %% Store results
    store_test_results(latency_analysis, LatencyAnalysis),

    LatencyAnalysis.

%%====================================================================
%% RESOURCE AND INFRASTRUCTURE TESTING TEST CASES
%%====================================================================

resource_utilization_monitoring(Config) ->
    %% Start resource monitoring
    {ok, MonitorPid} = erlmcp_resource_monitor:start(),

    %% Execute load test while monitoring resources
    Results = execute_load_test_with_monitoring(MonitorPid),

    ##% Analyze resource utilization
    ResourceAnalysis = analyze_resource_utilization(Results),

    %% Identify resource bottlenecks
    Bottlenecks = identify_resource_bottlenecks(ResourceAnalysis),

    %% Generate resource recommendations
    Recommendations = generate_resource_recommendations(ResourceAnalysis, Bottlenecks),

    %% Cleanup
    ok = erlmcp_resource_monitor:stop(MonitorPid),

    %% Store results
    store_test_results(resource_monitoring, ResourceAnalysis),

    ResourceAnalysis.

database_load_testing(Config) ->
    %% Initialize database load testing
    {ok, DbTestPid} = erlmcp_database_test:start(),

    %% Execute database load tests
    Results = execute_database_load_tests(DbTestPid),

    %% Analyze database performance
    DbAnalysis = analyze_database_performance(Results),

    %% Identify database bottlenecks
    DbBottlenecks = identify_database_bottlenecks(DbAnalysis),

    ##% Validate database scaling
    ScalingValidation = validate_database_scaling(DbAnalysis),

    %% Cleanup
    ok = erlmcp_database_test:stop(DbTestPid),

    %% Store results
    store_test_results(database_load_testing, DbAnalysis),

    DbAnalysis.

network_performance_testing(Config) ->
    %% Start network performance testing
    {ok, NetworkTestPid} = erlmcp_network_test:start(),

    %% Execute network load tests
    Results = execute_network_load_tests(NetworkTestPid),

    %% Analyze network performance
    NetworkAnalysis = analyze_network_performance(Results),

    ##% Identify network bottlenecks
    NetworkBottlenecks = identify_network_bottlenecks(NetworkAnalysis),

    %% Validate network configurations
    NetworkValidation = validate_network_configurations(NetworkAnalysis),

    %% Cleanup
    ok = erlmcp_network_test:stop(NetworkTestPid),

    %% Store results
    store_test_results(network_performance, NetworkAnalysis),

    NetworkAnalysis.

cache_effectiveness_testing(Config) ->
    %% Start cache effectiveness testing
    {ok, CacheTestPid} = erlmcp_cache_test:start(),

    %% Execute cache load tests
    Results = execute_cache_load_tests(CacheTestPid),

    %% Analyze cache effectiveness
    CacheAnalysis = analyze_cache_effectiveness(Results),

    ##% Identify cache inefficiencies
    CacheInefficiencies = identify_cache_inefficiencies(CacheAnalysis),

    ##% Optimize cache configuration
    CacheOptimization = optimize_cache_configuration(CacheAnalysis, CacheInefficiencies),

    %% Cleanup
    ok = erlmcp_cache_test:stop(CacheTestPid),

    %% Store results
    store_test_results(cache_effectiveness, CacheAnalysis),

    CacheAnalysis.

connection_pool_testing(Config) ->
    %% Start connection pool testing
    {ok, PoolTestPid} = erlmcp_pool_test:start(),

    %% Execute connection pool load tests
    Results = execute_connection_pool_tests(PoolTestPid),

    %% Analyze connection pool performance
    PoolAnalysis = analyze_connection_pool_performance(Results),

    ##% Identify pool inefficiencies
    PoolInefficiencies = identify_pool_inefficiencies(PoolAnalysis),

    ##% Validate pool configuration
    PoolValidation = validate_pool_configuration(PoolAnalysis),

    %% Cleanup
    ok = erlmcp_pool_test:stop(PoolTestPid),

    %% Store results
    store_test_results(connection_pool, PoolAnalysis),

    PoolAnalysis.

load_balancing_validation(Config) ->
    %% Start load balancing validation
    {ok, LBTestPid} = erlmcp_lb_test:start(),

    %% Execute load balancing tests
    Results = execute_load_balancing_tests(LBTestPid),

    %% Analyze load balancing effectiveness
    LBAnalysis = analyze_load_balancing_effectiveness(Results),

    ##% Identify imbalances
    Imbalances = identify_load_imbalances(LBAnalysis),

    ##% Validate load distribution
    LBValidation = validate_load_distribution(LBAnalysis, Imbalances),

    %% Cleanup
    ok = erlmcp_lb_test:stop(LBTestPid),

    %% Store results
    store_test_results(load_balancing, LBAnalysis),

    LBAnalysis.

failover_testing_under_load(Config) ->
    %% Start failover testing
    {ok, FailoverTestPid} = erlmcp_failover_test:start(),

    %% Execute failover scenarios under load
    Results = execute_failover_scenarios(FailoverTestPid),

    %% Analyze failover performance
    FailoverAnalysis = analyze_failover_performance(Results),

    ##% Identify failover issues
    FailoverIssues = identify_failover_issues(FailoverAnalysis),

    ##% Validate failover effectiveness
    FailoverValidation = validate_failover_effectiveness(FailoverAnalysis, FailoverIssues),

    %% Cleanup
    ok = erlmcp_failover_test:stop(FailoverTestPid),

    %% Store results
    store_test_results(failover_testing, FailoverAnalysis),

    FailoverAnalysis.

%%====================================================================
%% ADVANCED TESTING TEST CASES
%%====================================================================

performance_regression_detection(Config) ->
    %% Start regression detection
    {ok, RegressionPid} = erlmcp_regression_test:start(),

    %% Execute baseline comparison
    Results = execute_baseline_comparison(RegressionPid),

    ##% Detect regressions
    Regressions = detect_performance_regressions(Results),

    ##% Analyze regression impact
    RegressionImpact = analyze_regression_impact(Regressions),

    ##% Validate regression detection
    RegressionValidation = validate_regression_detection(Results, Regressions),

    %% Cleanup
    ok = erlmcp_regression_test:stop(RegressionPid),

    %% Store results
    store_test_results(regression_detection, Regressions),

    Regressions.

peak_load_handling(Config) ->
    %% Start peak load testing
    {ok, PeakTestPid} = erlmcp_peak_test:start(),

    %% Execute peak load scenarios
    Results = execute_peak_load_scenarios(PeakTestPid),

    ##% Analyze peak load performance
    PeakAnalysis = analyze_peak_load_performance(Results),

    ##% Identify peak handling issues
    PeakIssues = identify_peak_handling_issues(PeakAnalysis),

    ##% Validate peak handling capabilities
    PeakValidation = validate_peak_handling(PeakAnalysis, PeakIssues),

    %% Cleanup
    ok = erlmcp_peak_test:stop(PeakTestPid),

    %% Store results
    store_test_results(peak_load_handling, PeakAnalysis),

    PeakAnalysis.

multi_protocol_testing(Config) ->
    %% Start multi-protocol testing
    {ok, MultiProtocolPid} = erlmcp_multi_protocol_test:start(),

    %% Execute multi-protocol load tests
    Results = execute_multi_protocol_load_tests(MultiProtocolPid),

    ##% Analyze multi-protocol performance
    MultiProtocolAnalysis = analyze_multi_protocol_performance(Results),

    ##% Identify protocol-specific issues
    ProtocolIssues = identify_protocol_issues(MultiProtocolAnalysis),

    ##% Validate protocol compatibility
    ProtocolValidation = validate_protocol_compatibility(MultiProtocolAnalysis, ProtocolIssues),

    %% Cleanup
    ok = erlmcp_multi_protocol_test:stop(MultiProtocolPid),

    %% Store results
    store_test_results(multi_protocol_testing, MultiProtocolAnalysis),

    MultiProtocolAnalysis.

%%====================================================================
%% ENTERPRISE FEATURE TEST CASES
%%====================================================================

load_test_environment_setup(Config) ->
    %% Setup load testing environment
    SetupResults = setup_load_test_environment(),

    ##% Validate environment setup
    SetupValidation = validate_environment_setup(SetupResults),

    %% Store results
    store_test_results(environment_setup, SetupValidation),

    SetupValidation.

performance_metrics_collection(Config) ->
    %% Start metrics collection
    {ok, MetricsPid} = erlmcp_metrics_collector:start(),

    %% Execute metrics collection
    Results = collect_performance_metrics(MetricsPid),

    ##% Analyze metrics collection
    MetricsAnalysis = analyze_metrics_collection(Results),

    ##% Validate metrics completeness
    MetricsValidation = validate_metrics_completeness(MetricsAnalysis),

    %% Cleanup
    ok = erlmcp_metrics_collector:stop(MetricsPid),

    %% Store results
    store_test_results(metrics_collection, MetricsAnalysis),

    MetricsAnalysis.

performance_analysis_dashboard(Config) ->
    %% Start performance analysis
    {ok, DashboardPid} = erlmcp_dashboard:start(),

    %% Execute performance analysis
    Results = execute_performance_analysis(DashboardPid),

    ##% Analyze dashboard effectiveness
    DashboardAnalysis = analyze_dashboard_effectiveness(Results),

    ##% Validate dashboard usability
    DashboardValidation = validate_dashboard_usability(DashboardAnalysis),

    %% Cleanup
    ok = erlmcp_dashboard:stop(DashboardPid),

    %% Store results
    store_test_results(performance_dashboard, DashboardAnalysis),

    DashboardAnalysis.

regression_testing_automation(Config) ->
    %% Start regression testing automation
    {ok, RegressionAutoPid} = erlmcp_regression_auto:start(),

    %% Execute automated regression tests
    Results = execute_automated_regression_tests(RegressionAutoPid),

    ##% Analyze automation effectiveness
    AutomationAnalysis = analyze_automation_effectiveness(Results),

    ##% Validate automation reliability
    AutomationValidation = validate_automation_reliability(AutomationAnalysis),

    %% Cleanup
    ok = erlmcp_regression_auto:stop(RegressionAutoPid),

    %% Store results
    store_test_results(regression_automation, AutomationAnalysis),

    AutomationAnalysis.

load_testing_procedures(Config) ->
    %% Execute load testing procedures
    Results = execute_load_testing_procedures(),

    ##% Analyze procedures effectiveness
    ProceduresAnalysis = analyze_procedures_effectiveness(Results),

    ##% Validate procedures completeness
    ProceduresValidation = validate_procedures_completeness(ProceduresAnalysis),

    %% Store results
    store_test_results(procedures, ProceduresAnalysis),

    ProceduresAnalysis.

capacity_planning_tools(Config) ->
    %% Start capacity planning
    {ok, CapacityPid} = erlmcp_capacity_planning:start(),

    %% Execute capacity planning analysis
    Results = execute_capacity_planning(CapacityPid),

    ##% Analyze capacity requirements
    CapacityAnalysis = analyze_capacity_requirements(Results),

    ##% Validate capacity predictions
    CapacityValidation = validate_capacity_predictions(CapacityAnalysis),

    %% Cleanup
    ok = erlmcp_capacity_planning:stop(CapacityPid),

    %% Store results
    store_test_results(capacity_planning, CapacityAnalysis),

    CapacityAnalysis.

performance_optimization_recommendations(Config) ->
    %% Generate performance optimization recommendations
    Recommendations = generate_optimization_recommendations(),

    ##% Analyze recommendation effectiveness
    RecommendationsAnalysis = analyze_recommendations_effectiveness(Recommendations),

    ##% Validate recommendation feasibility
    RecommendationsValidation = validate_recommendations_feasibility(RecommendationsAnalysis),

    %% Store results
    store_test_results(optimization_recommendations, RecommendationsAnalysis),

    RecommendationsAnalysis.

%%====================================================================
%% HELPER FUNCTIONS
%%====================================================================

%% Load testing infrastructure setup
start_load_test_nodes() ->
    %% Start distributed load testing nodes
    NodeSpecs = [
        {load_test_node1, 15000},
        {load_test_node2, 15001},
        {load_test_node3, 15002},
        {load_test_node4, 15003},
        {load_test_node5, 15004}
    ],

    lists:foreach(fun({NodeName, Port}) ->
        spawn(fun() ->
            %% Start load test node
            {ok, _} = application:ensure_all_started(erlmcp),
            {ok, _} = application:ensure_all_started(load_generator),

            %% Register load test services
            register_load_test_services(NodeName),

            %% Keep node alive
            receive
                shutdown -> ok
            end
        end)
    end, NodeSpecs).

%% Initialize monitoring
initialize_monitoring() ->
    %% Start system monitoring
    {ok, _} = erlmcp_monitor:start(),

    %% Initialize metrics collection
    {ok, _} = erlmcp_metrics:start(),

    %% Initialize alerting
    {ok, _} = erlmcp_alert:start(),

    ok.

%% Cleanup load testing
cleanup_load_testing() ->
    %% Stop monitoring
    ok = erlmcp_monitor:stop(),

    %% Stop metrics collection
    ok = erlmcp_metrics:stop(),

    %% Stop alerting
    ok = erlmcp_alert:stop(),

    %% Clean up test data
    cleanup_test_data(),

    ok.

%% Stop monitoring
stop_monitoring() ->
    %% Stop monitoring services
    ok = erlmcp_monitor:stop(),

    %% Stop metrics collection
    ok = erlmcp_metrics:stop(),

    %% Stop alerting
    ok = erlmcp_alert:stop(),

    ok.

%% Generate load test report
generate_load_test_report(Config) ->
    %% Collect all test results
    AllResults = collect_all_test_results(Config),

    ##% Generate comprehensive report
    Report = erlmcp_load_test_report:generate(AllResults),

    %% Save report
    save_load_test_report(Report),

    ok.

%% Test monitoring functions
monitor_user_simulation(Pid, MaxUsers) ->
    monitor_loop(Pid, MaxUsers, user_simulation, 300000).

monitor_throughput_test(Pid, TargetThroughput) ->
    monitor_loop(Pid, TargetThroughput, throughput_test, 300000).

monitor_loop(Pid, Target, Type, Timeout) ->
    StartTime = erlang:monotonic_time(millisecond),
    Results = [],

    monitor_loop(Pid, Target, Type, Timeout, StartTime, Results).

monitor_loop(_Pid, _Target, _Type, Timeout, StartTime, Results) when
    erlang:monotonic_time(millisecond) - StartTime > Timeout ->
    Results;

monitor_loop(Pid, Target, Type, Timeout, StartTime, Results) ->
    %% Collect metrics
    Metrics = collect_metrics(Pid, Type, Target),

    %% Check for anomalies
    Anomalies = detect_anomalies(Metrics),

    %% Store results
    NewResults = [Metrics#{anomalies => Anomalies} | Results],

    %% Continue monitoring
    timer:sleep(1000),
    monitor_loop(Pid, Target, Type, Timeout, StartTime, NewResults).

%% Analysis functions
analyze_concurrent_user_results(Results) ->
    #{
        total_users => calculate_total_users(Results),
        active_users => calculate_active_users(Results),
        success_rate => calculate_success_rate(Results),
        response_time => calculate_response_time(Results),
        throughput => calculate_throughput(Results),
        cpu_usage => calculate_cpu_usage(Results),
        memory_usage => calculate_memory_usage(Results)
    }.

analyze_throughput_results(Results) ->
    #{
        target_throughput => get_target_throughput(Results),
        actual_throughput => calculate_actual_throughput(Results),
        success_rate => calculate_success_rate(Results),
        response_time => calculate_response_time(Results),
        error_rate => calculate_error_rate(Results),
        bottleneck => identify_throughput_bottleneck(Results)
    }.

analyze_stress_results(Results, BreakingPoints) ->
    #{
        breaking_points => BreakingPoints,
        system_resilience => calculate_system_resilience(Results),
        failure_tolerance => calculate_failure_tolerance(Results),
        recovery_time => calculate_recovery_time(Results),
        degradation_pattern => analyze_degradation_pattern(Results)
    }.

analyze_latency_distribution(Results, Config) ->
    Percentiles = ?config(percentiles, Config),

    #{
        percentiles => calculate_latency_percentiles(Results, Percentiles),
        average_latency => calculate_average_latency(Results),
        latency_distribution => calculate_latency_distribution(Results),
        threshold_violations => detect_threshold_violations(Results, Config),
        optimization_opportunities => identify_latency_optimization_opportunities(Results)
    }.

%% Verification functions
verify_concurrent_user_performance(Analysis) ->
    #{success_rate := SuccessRate, response_time := ResponseTime} = Analysis,

    ?assert(SuccessRate >= 0.95, "Success rate must be >= 95%"),
    ?assert(ResponseTime =< 1000, "Response time must be <= 1000ms"),

    ok.

verify_throughput_performance(Analysis, Target) ->
    #{actual_throughput := Actual, success_rate := SuccessRate} = Analysis,

    ?assert(Actual >= Target * 0.95, "Throughput must be >= 95% of target"),
    ?assert(SuccessRate >= 0.95, "Success rate must be >= 95%"),

    ok.

verify_system_resilience(Analysis, BreakingPoints) ->
    #{system_resilience := Resilience} = Analysis,

    ?assert(Resilience >= 0.8, "System resilience must be >= 80%"),

    %% Check that breaking points are properly handled
    lists:foreach(fun(Point) ->
        ?assert(is_breaking_point_handled(Point), "Breaking point not handled")
    end, BreakingPoints),

    ok.

%% Storage functions
store_test_results(TestName, Results) ->
    %% Store results in database
    erlmcp_test_results:store(TestName, Results),

    %% Store in metrics for trending
    erlmcp_metrics:store(test_results, TestName, Results),

    ok.

cleanup_test_resources(Config) ->
    %% Clean up test-specific resources
    cleanup_temporary_files(),
    clear_test_cache(),
    reset_test_state(),

    ok.

cleanup_test_data() ->
    %% Clean up all test data
    erlmcp_test_data:cleanup(),

    ok.

%%====================================================================
%% INTERNAL HELPER FUNCTIONS
%%====================================================================

generate_user_profiles(Count) ->
    generate_user_profiles(Count, []).

generate_user_profiles(0, Acc) ->
    Acc;

generate_user_profiles(Count, Acc) ->
    Profile = #{
        id => erlmcp_utils:uuid(),
        behavior_type => random_user_behavior(),
        request_pattern => generate_request_pattern(),
        think_time => generate_think_time(),
        session_duration => generate_session_duration()
    },

    generate_user_profiles(Count - 1, [Profile | Acc]).

random_user_behavior() ->
    lists:nth(rand:uniform(4), [read_heavy, write_heavy, mixed, bursty]).

generate_request_pattern() ->
    #{
        interval => rand:uniform(5000) + 100,
        burst_size => rand:uniform(10) + 1,
        think_time => rand:uniform(2000) + 100
    }.

generate_think_time() ->
    rand:uniform(5000) + 1000.

generate_session_duration() ->
    rand:uniform(300000) + 60000.  % 1-6 minutes

generate_session_patterns() ->
    [
        #{
            name => constant_load,
            description => "Constant load pattern",
            pattern => {constant, 100}
        },
        #{
            name => ramp_up,
            description => "Ramp up pattern",
            pattern => {ramp, {0, 1000, 300000}}
        },
        #{
            name => spike,
            description => "Spike pattern",
            pattern => {spike, {100, 1000, 10000, 100}}
        }
    ].

generate_behavior_models() ->
    [
        #{
            model => exponential_backoff,
            parameters => #{
                base_delay => 100,
                max_delay => 5000,
                jitter => 0.1
            }
        },
        #{
            model => linear_backoff,
            parameters => #{
                initial_delay => 100,
                increment => 100,
                max_delay => 5000
            }
        },
        #{
            model => constant_rate,
            parameters => #{
                rate => 100,
                burst => 10
            }
        }
    ].

generate_request_types() ->
    [
        #{
            name => simple_query,
            weight => 0.3,
            payload => generate_simple_payload(),
            timeout => 5000
        },
        #{
            name => complex_operation,
            weight => 0.2,
            payload => generate_complex_payload(),
            timeout => 10000
        },
        #{
            name => bulk_operation,
            weight => 0.1,
            payload => generate_bulk_payload(),
            timeout => 15000
        },
        #{
            name => stream_operation,
            weight => 0.2,
            payload => generate_stream_payload(),
            timeout => 30000
        },
        #{
            name => file_transfer,
            weight => 0.2,
            payload => generate_file_payload(),
            timeout => 60000
        }
    ].

generate_simple_payload() ->
    #{data => erlmcp_utils:random_string(100)}.

generate_complex_payload() ->
    #{data => erlmcp_utils:random_string(1000), metadata => generate_metadata()}.

generate_bulk_payload() ->
    #{items => [generate_simple_payload() || _ <- lists:seq(1, 100)]}.

generate_stream_payload() ->
    #{stream_id => erlmcp_utils:uuid(), format => json}.

generate_file_payload() ->
    #{file_id => erlmcp_utils:uuid(), size => 1024 * 1024}.

generate_metadata() ->
    #{
        timestamp => erlang:system_time(millisecond),
        version => "1.0",
        tags => ["test", "performance", "load"]
    }.

generate_error_scenarios() ->
    [
        #{
            name => network_timeout,
            probability => 0.01,
            error => timeout,
            recovery => retry
        },
        #{
            name => server_error,
            probability => 0.005,
            error => server_error,
            recovery => retry
        },
        #{
            name => rate_limit,
            probability => 0.02,
            error => rate_limited,
            recovery => backoff
        }
    ].

generate_load_profiles() ->
    [
        #{
            name => linear_ramp,
            profile => {linear, {0, 10000, 600000}},
            duration => 600000
        },
        #{
            name => step_ramp,
            profile => {step, {100, 1000, 10000, 5}},
            duration => 300000
        },
        #{
            name => sinusoidal,
            profile => {sinusoidal, {1000, 10000, 300000}},
            duration => 300000
        }
    ].

generate_failure_injection() ->
    [
        #{
            type => process_crash,
            probability => 0.001,
            target => random
        },
        #{
            type => network_partition,
            probability => 0.002,
            target => cluster
        },
        #{
            type => resource_exhaustion,
            probability => 0.005,
            target => memory
        }
    ].

determine_system_limits() ->
    #{
        max_connections => 50000,
        max_throughput => 50000,
        max_latency => 5000,
        max_cpu => 80,
        max_memory => 85
    }.

generate_optimization_strategies() ->
    [
        #{
            name => connection_pool_optimization,
            parameters => #{
                pool_size => 100,
                max_overflow => 20,
                idle_timeout => 30000
            }
        },
        #{
            name => caching_optimization,
            parameters => #{
                cache_size => 1000,
                ttl => 60000,
                strategy => lru
            }
        },
        #{
            name => batching_optimization,
            parameters => #{
                batch_size => 100,
                batch_timeout => 1000
            }
        }
    ].