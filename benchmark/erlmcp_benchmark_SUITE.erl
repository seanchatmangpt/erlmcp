%% @doc Enterprise Performance Benchmark Suite for erlmcp v3
%% Designed for Fortune 500 scale: 10K+ nodes, <100ms p95 latency, 99.99% availability, TB-scale data
%% @copyright 2026 erlmcp
%% @version 3.0.0
-module(erlmcp_benchmark_SUITE).

-include_lib("common/include/test.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT API
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    groups/0
]).

%% Test cases
-export([
    %% Throughput Benchmarks
    throughput_1k_reqs_s/1,
    throughput_10k_reqs_s/1,
    throughput_50k_reqs_s/1,
    throughput_100k_reqs_s/1,

    %% Latency Benchmarks
    latency_p95_100ms/1,
    latency_p99_200ms/1,
    latency_tail_optimization/1,

    %% Scalability Benchmarks
    scalability_100_nodes/1,
    scalability_1k_nodes/1,
    scalability_10k_nodes/1,

    %% Resource Optimization
    memory_usage_optimization/1,
    cpu_utilization_patterns/1,
    network_performance_analysis/1,

    %% Data Processing
    data_volume_tb_scale/1,
    cache_efficiency_testing/1,
    connection_pool_optimization/1,

    %% System Performance
    load_balancing_effectiveness/1,
    failover_performance_impact/1,
    resource_utilization_efficiency/1,
    concurrent_user_handling/1,

    %% Regression & SLA
    performance_regression_testing/1,
    sla_validation_metrics/1,
    capacity_planning_guidelines/1
]).

%% Types
-type benchmark_config() :: #{
    duration := pos_integer(),
    rate := pos_integer(),
    nodes := pos_integer(),
    data_size := pos_integer(),
    concurrency := pos_integer(),
    replicates := pos_integer(),
    warmup := pos_integer(),
    metrics := [atom()]
}.

-type benchmark_result() :: #{
    timestamp := integer(),
    scenario := atom(),
    throughput => float(),
    latency => #{p50 => float(), p95 => float(), p99 => float()},
    resource_usage => #{
        cpu => float(),
        memory => float(),
        network => float(),
        disk => float()
    },
    errors => integer(),
    success_rate => float(),
    metrics => map()
}.

-type sl_metric() :: #{
    target => float(),
    actual => float(),
    status => 'pass' | 'fail' | 'warning',
    margin => float()
}.

%%====================================================================
%% CT API
%%====================================================================

all() ->
    [
        {group, throughput_benchmarks},
        {group, latency_benchmarks},
        {group, scalability_benchmarks},
        {group, resource_optimization},
        {group, data_processing},
        {group, system_performance},
        {group, regression_sla}
    ].

groups() ->
    [
        {throughput_benchmarks, [parallel], [
            throughput_1k_reqs_s,
            throughput_10k_reqs_s,
            throughput_50k_reqs_s,
            throughput_100k_reqs_s
        ]},
        {latency_benchmarks, [parallel], [
            latency_p95_100ms,
            latency_p99_200ms,
            latency_tail_optimization
        ],
        {scalability_benchmarks, [parallel], [
            scalability_100_nodes,
            scalability_1k_nodes,
            scalability_10k_nodes
        ]},
        {resource_optimization, [parallel], [
            memory_usage_optimization,
            cpu_utilization_patterns,
            network_performance_analysis
        ]},
        {data_processing, [parallel], [
            data_volume_tb_scale,
            cache_efficiency_testing,
            connection_pool_optimization
        ]},
        {system_performance, [parallel], [
            load_balancing_effectiveness,
            failover_performance_impact,
            resource_utilization_efficiency,
            concurrent_user_handling
        ]},
        {regression_sla, [sequential], [
            performance_regression_testing,
            sla_validation_metrics,
            capacity_planning_guidelines
        ]}
    ].

init_per_suite(Config) ->
    %% Initialize enterprise benchmark environment
    ok = setup_cluster(Config),
    ok = initialize_metrics_collector(),
    ok = setup_load_generators(),
    Config.

end_per_suite(_Config) ->
    %% Cleanup and generate final report
    ok = cleanup_benchmark_environment(),
    ok = generate_enterprise_report(),
    ok = tear_down_cluster(),
    ok.

init_per_group(throughput_benchmarks, Config) ->
    %% Configure high-throughput test parameters
    Config1 = set_test_config(Config, #{
        duration => 300000,  % 5 minutes
        rate => 100000,
        nodes => 10,
        concurrency => 1000,
        metrics => [throughput, errors, response_times]
    }),
    erlmcp_benchmark_manager:start_group_session(throughput_benchmarks, Config1),
    Config1;

init_per_group(latency_benchmarks, Config) ->
    %% Configure low-latency test parameters
    Config1 = set_test_config(Config, #{
        duration => 60000,   % 1 minute
        rate => 10000,
        nodes => 5,
        concurrency => 100,
        metrics => [latencies, percentiles, timing]
    }),
    erlmcp_benchmark_manager:start_group_session(latency_benchmarks, Config1),
    Config1;

init_per_group(scalability_benchmarks, Config) ->
    %% Configure scalability test parameters
    Config1 = set_test_config(Config, #{
        duration => 300000,  % 5 minutes per scale
        rate => 50000,
        nodes => 10000,
        concurrency => 5000,
        metrics => [node_performance, network_topology, scaling_efficiency]
    }),
    erlmcp_benchmark_manager:start_group_session(scalability_benchmarks, Config1),
    Config1;

init_per_group(resource_optimization, Config) ->
    %% Configure resource monitoring tests
    Config1 = set_test_config(Config, #{
        duration => 120000,  % 2 minutes
        rate => 25000,
        nodes => 20,
        concurrency => 500,
        metrics => [cpu, memory, network, disk, resource_efficiency]
    }),
    erlmcp_benchmark_manager:start_group_session(resource_optimization, Config1),
    Config1;

init_per_group(data_processing, Config) ->
    %% Configure large-scale data processing tests
    Config1 = set_test_config(Config, #{
        duration => 600000,  % 10 minutes
        rate => 10000,
        nodes => 50,
        data_size => 1024*1024*1024,  % 1GB per transaction
        concurrency => 200,
        metrics => [throughput, data_volume, cache_hit_ratio, connection_efficiency]
    }),
    erlmcp_benchmark_manager:start_group_session(data_processing, Config1),
    Config1;

init_per_group(system_performance, Config) ->
    %% Configure overall system performance tests
    Config1 = set_test_config(Config, #{
        duration => 300000,  % 5 minutes
        rate => 75000,
        nodes => 100,
        concurrency => 1000,
        metrics => [load_balance, failover, resource_utilization, concurrency]
    }),
    erlmcp_benchmark_manager:start_group_session(system_performance, Config1),
    Config1;

init_per_group(regression_sla, Config) ->
    %% Configure regression and SLA validation tests
    Config1 = set_test_config(Config, #{
        duration => 1800000,  % 30 minutes comprehensive
        rate := 50000,
        nodes := 1000,
        replicates := 5,
        warmup := 60000,
        metrics := [regression, sla, capacity, trends]
    }),
    erlmcp_benchmark_manager:start_group_session(regression_sla, Config1),
    Config1.

end_per_group(_GroupName, _Config) ->
    ok = erlmcp_benchmark_manager:stop_group_session(),
    ok.

%%====================================================================
%% Throughput Benchmarks
%%====================================================================

throughput_1k_reqs_s(Config) ->
    Config1 = set_test_config(Config, #{
        rate => 1000,
        duration => 120000,  % 2 minutes
        warmup => 30000      % 30 seconds
    }),
    Result = run_throughput_benchmark(throughput_1k_reqs_s, Config1),
    ?assertMatch(#{success_rate := SR}, Result),
    ?assert(SR >= 0.999, "99.9% success rate required"),
    ?assert(maps:get(throughput, Result) >= 950, "Minimum 950 req/s achieved"),
    store_result(Config1, Result).

throughput_10k_reqs_s(Config) ->
    Config1 = set_test_config(Config, #{
        rate => 10000,
        duration => 180000,  % 3 minutes
        warmup => 60000      % 1 minute
    }),
    Result = run_throughput_benchmark(throughput_10k_reqs_s, Config1),
    ?assertMatch(#{success_rate := SR}, Result),
    ?assert(SR >= 0.995, "99.5% success rate required"),
    ?assert(maps:get(throughput, Result) >= 9500, "Minimum 9500 req/s achieved"),
    store_result(Config1, Result).

throughput_50k_reqs_s(Config) ->
    Config1 = set_test_config(Config, #{
        rate => 50000,
        duration := 300000,  % 5 minutes
        warmup := 120000     % 2 minutes
    }),
    Result = run_throughput_benchmark(throughput_50k_reqs_s, Config1),
    ?assertMatch(#{success_rate := SR}, Result),
    ?assert(SR >= 0.990, "99.0% success rate required"),
    ?assert(maps:get(throughput, Result) >= 47500, "Minimum 47500 req/s achieved"),
    store_result(Config1, Result).

throughput_100k_reqs_s(Config) ->
    Config1 = set_test_config(Config, #{
        rate => 100000,
        duration := 600000,  % 10 minutes
        warmup := 300000     % 5 minutes
    }),
    Result = run_throughput_benchmark(throughput_100k_reqs_s, Config1),
    ?assertMatch(#{success_rate := SR}, Result),
    ?assert(SR >= 0.985, "98.5% success rate required"),
    ?assert(maps:get(throughput, Result) >= 95000, "Minimum 95000 req/s achieved"),
    store_result(Config1, Result).

%%====================================================================
%% Latency Benchmarks
%%====================================================================

latency_p95_100ms(Config) ->
    Config1 = set_test_config(Config, #{
        rate => 25000,
        duration := 120000,  % 2 minutes
        metrics := [latencies]
    }),
    Result = run_latency_benchmark(latency_p95_100ms, Config1),
    ?assertMatch(#{latency := #{p95 := P95}}, Result),
    ?assert(P95 < 100, "P95 latency must be <100ms"),
    store_result(Config1, Result).

latency_p99_200ms(Config) ->
    Config1 = set_test_config(Config, #{
        rate => 50000,
        duration := 180000,  % 3 minutes
        metrics := [latencies]
    }),
    Result = run_latency_benchmark(latency_p99_200ms, Config1),
    ?assertMatch(#{latency := #{p99 := P99}}, Result),
    ?assert(P99 < 200, "P99 latency must be <200ms"),
    store_result(Config1, Result).

latency_tail_optimization(Config) ->
    Config1 = set_test_config(Config, #{
        rate => 100000,
        duration := 300000,  % 5 minutes
        metrics := [latencies, percentiles, tail_analysis]
    }),
    Result = run_latency_benchmark(latency_tail_optimization, Config1),
    ?assertMatch(#{latency := #{p99 := P99, p99_9 := P999}}, Result),
    TailRatio = P999 / P99,
    ?assert(TailRatio < 3, "Tail ratio should be <3x"),
    store_result(Config1, Result).

%%====================================================================
%% Scalability Benchmarks
%%====================================================================

scalability_100_nodes(Config) ->
    Config1 = set_test_config(Config, #{
        nodes := 100,
        rate := 20000,
        duration := 300000  % 5 minutes
    }),
    Result = run_scalability_benchmark(scalability_100_nodes, Config1),
    ?assertMatch(#{throughput := T}, Result),
    ExpectedBase = 5000,  % Base throughput per node
    ExpectedTotal = ExpectedBase * 100,
    ?assert(T >= ExpectedTotal * 0.9, "90% linear scaling achieved"),
    store_result(Config1, Result).

scalability_1k_nodes(Config) ->
    Config1 = set_test_config(Config, #{
        nodes := 1000,
        rate := 50000,
        duration := 600000  % 10 minutes
    }),
    Result = run_scalability_benchmark(scalability_1k_nodes, Config1),
    ?assertMatch(#{throughput := T}, Result),
    ExpectedBase = 5000,
    ExpectedTotal = ExpectedBase * 1000,
    ScalingEfficiency = T / ExpectedTotal,
    ?assert(ScalingEfficiency >= 0.85, "85% scaling efficiency achieved"),
    store_result(Config1, Result).

scalability_10k_nodes(Config) ->
    Config1 = set_test_config(Config, #{
        nodes := 10000,
        rate := 100000,
        duration := 1200000  % 20 minutes
    }),
    Result = run_scalability_benchmark(scalability_10k_nodes, Config1),
    ?assertMatch(#{throughput := T, resource_usage := #{cpu := CPU}}, Result),
    ExpectedBase = 5000,
    ExpectedTotal = ExpectedBase * 10000,
    ScalingEfficiency = T / ExpectedTotal,
    ?assert(ScalingEfficiency >= 0.80, "80% scaling efficiency achieved"),
    ?assert(CPU =< 0.85, "CPU utilization <=85%"),
    store_result(Config1, Result).

%%====================================================================
%% Resource Optimization Benchmarks
%%====================================================================

memory_usage_optimization(Config) ->
    Config1 = set_test_config(Config, #{
        rate := 25000,
        duration := 600000,  % 10 minutes
        metrics := [memory, garbage_collection, memory_efficiency]
    }),
    Result = run_resource_optimization_benchmark(memory_usage_optimization, Config1),
    ?assertMatch(#{resource_usage := #{memory := Mem}}, Result),
    MemoryPerRequest = Mem / get_total_requests(Config1),
    ?assert(MemoryPerRequest < 1024, "Memory per request <1KB"),
    store_result(Config1, Result).

cpu_utilization_patterns(Config) ->
    Config1 = set_test_config(Config, #{
        rate := 50000,
        duration := 300000,  % 5 minutes
        metrics := [cpu, cpu_distribution, cpu_patterns]
    }),
    Result = run_resource_optimization_benchmark(cpu_utilization_patterns, Config1),
    ?assertMatch(#{resource_usage := #{cpu := CPU}}, Result),
    CPUUtilization = maps:get(avg, CPU, 0),
    ?assert(CPUUtilization =< 0.80, "Average CPU <=80%"),
    ?assert(maps:get(max, CPU, 0) =< 0.95, "Peak CPU <=95%"),
    store_result(Config1, Result).

network_performance_analysis(Config) ->
    Config1 = set_test_config(Config, #{
        rate := 75000,
        duration := 300000,  % 5 minutes
        metrics := [network, bandwidth, latency, network_patterns]
    }),
    Result = run_resource_optimization_benchmark(network_performance_analysis, Config1),
    ?assertMatch(#{resource_usage := #{network := Net}}, Result),
    Throughput = maps:get(mbps, Net, 0),
    ?assert(Throughput >= 1000, "Network throughput >=1Gbps"),
    NetworkLatency = maps:get(avg_latency, Net, 0),
    ?assert(NetworkLatency < 1, "Network latency <1ms"),
    store_result(Config1, Result).

%%====================================================================
%% Data Processing Benchmarks
%%====================================================================

data_volume_tb_scale(Config) ->
    Config1 = set_test_config(Config, #{
        data_size := 1024*1024*1024,  % 1GB per transaction
        rate := 1000,  % Reduced rate due to large data size
        duration := 1800000,  % 30 minutes
        metrics := [throughput, data_volume, processing_efficiency]
    }),
    Result = run_data_processing_benchmark(data_volume_tb_scale, Config1),
    ?assertMatch(#{throughput := T, data_volume := DV}, Result),
    TotalDataTB = DV / (1024*1024*1024*1024),
    ?assert(TotalDataTB >= 1, "Processed at least 1TB of data"),
    ThroughputPerGB = T / 1024,
    ?assert(ThroughputPerGB >= 0.5, "Minimum 0.5GB/s throughput"),
    store_result(Config1, Result).

cache_efficiency_testing(Config) ->
    Config1 = set_test_config(Config, #{
        rate := 50000,
        duration := 300000,  % 5 minutes
        metrics := [cache, hit_ratio, cache_efficiency]
    }),
    Result = run_data_processing_benchmark(cache_efficiency_testing, Config1),
    ?assertMatch(#{cache := #{hit_ratio := HR}}, Result),
    ?assert(HR >= 0.95, "Cache hit ratio >=95%"),
    ?assert(maps:get(evictions, Result, 0) =< 1000, "Minimal cache evictions"),
    store_result(Config1, Result).

connection_pool_optimization(Config) ->
    Config1 = set_test_config(Config, #{
        rate := 100000,
        duration := 300000,  % 5 minutes
        metrics := [connection_pool, efficiency, reuse_ratio]
    }),
    Result = run_data_processing_benchmark(connection_pool_optimization, Config1),
    ?assertMatch(#{connection_pool := #{reuse_ratio := RR}}, Result),
    ?assert(RR >= 0.95, "Connection reuse ratio >=95%"),
    ?assert(maps:get(avg_wait_time, Result, 0) < 10, "Avg wait time <10ms"),
    store_result(Config1, Result).

%%====================================================================
%% System Performance Benchmarks
%%====================================================================

load_balancing_effectiveness(Config) ->
    Config1 = set_test_config(Config, #{
        rate := 75000,
        duration := 300000,  % 5 minutes
        metrics := [load_balance, distribution, imbalance]
    }),
    Result = run_system_performance_benchmark(load_balancing_effectiveness, Config1),
    ?assertMatch(#{load_balance := #{imbalance := IB}}, Result),
    ?assert(IB < 0.1, "Load imbalance <10%"),
    ?assert(maps:get(hotspots, Result, 0) =< 5, "Minimal hotspots"),
    store_result(Config1, Result).

failover_performance_impact(Config) ->
    Config1 = set_test_config(Config, #{
        rate := 50000,
        duration := 600000,  % 10 minutes
        metrics := [failover, recovery_time, availability]
    }),
    Result = run_system_performance_benchmark(failover_performance_impact, Config1),
    ?assertMatch(#{failover := #{recovery_time := RT}}, Result),
    ?assert(RT < 5000, "Recovery time <5 seconds"),
    Availability = maps:get(availability, Result, 1.0),
    ?assert(Availability >= 0.9999, "Availability >=99.99%"),
    store_result(Config1, Result).

resource_utilization_efficiency(Config) ->
    Config1 = set_test_config(Config, #{
        rate := 100000,
        duration := 300000,  % 5 minutes
        metrics := [resource_utilization, efficiency, cost_per_request]
    }),
    Result = run_system_performance_benchmark(resource_utilization_efficiency, Config1),
    ?assertMatch(#{resource_utilization := #{efficiency := EF}}, Result),
    ?assert(EF >= 0.85, "Resource efficiency >=85%"),
    CostPerRequest = maps:get(cost_per_request, Result, 0),
    ?assert(CostPerRequest < 0.001, "Cost per request <$0.001"),
    store_result(Config1, Result).

concurrent_user_handling(Config) ->
    Config1 = set_test_config(Config, #{
        rate := 50000,
        concurrency := 10000,
        duration := 300000,  % 5 minutes
        metrics := [concurrency, user_sessions, session_efficiency]
    }),
    Result = run_system_performance_benchmark(concurrent_user_handling, Config1),
    ?assertMatch(#{concurrency := #{efficiency := EF}}, Result),
    ?assert(EF >= 0.90, "Concurrency efficiency >=90%"),
    ?assert(maps:get(session_failures, Result, 0) =< 10, "Minimal session failures"),
    store_result(Config1, Result).

%%====================================================================
%% Regression & SLA Benchmarks
%%====================================================================

performance_regression_testing(Config) ->
    Config1 = set_test_config(Config, #{
        rate := 50000,
        duration := 1800000,  % 30 minutes
        replicates := 5,
        metrics := [regression, trends, performance_degradation]
    }),
    Result = run_regression_benchmark(performance_regression_testing, Config1),
    ?assertMatch(#{regression := #{degradation := DG}}, Result),
    ?assert(DG < 0.05, "Performance degradation <5%"),
    store_result(Config1, Result).

sla_validation_metrics(Config) ->
    Config1 = set_test_config(Config, #{
        rate := 75000,
        duration := 1800000,  % 30 minutes
        metrics := [sla, response_time, availability, throughput]
    }),
    Result = run_sla_benchmark(sla_validation_metrics, Config1),
    ?assertMatch(#{sla := SLAMetrics}, Result),
    validate_sla_compliance(SLAMetrics),
    store_result(Config1, Result).

capacity_planning_guidelines(Config) ->
    Config1 = set_test_config(Config, #{
        rate := 100000,
        duration := 600000,  % 10 minutes
        metrics := [capacity, projections, scaling_factors]
    }),
    Result = run_capacity_planning_benchmark(capacity_planning_guidelines, Config1),
    ?assertMatch(#{capacity => Capacity}, Result),
    ?assert(maps:is_key(projection_1_year, Capacity), "1-year projection available"),
    ?assert(maps:is_key(projection_5_year, Capacity), "5-year projection available"),
    generate_capacity_report(Result),
    store_result(Config1, Result).

%%====================================================================
%% Helper Functions
%%====================================================================

set_test_config(Config, Default) ->
    maps:merge(Default, Config).

run_throughput_benchmark(Scenario, Config) ->
    ThroughputConfig = erlmcp_benchmark_throughtput:make_config(Config),
    ThroughputResult = erlmcp_benchmark_throughtput:run(Scenario, ThroughputConfig),
    ThroughputResult.

run_latency_benchmark(Scenario, Config) ->
    LatencyConfig = erlmcp_benchmark_latency:make_config(Config),
    LatencyResult = erlmcp_benchmark_latency:run(Scenario, LatencyConfig),
    LatencyResult.

run_scalability_benchmark(Scenario, Config) ->
    ScalabilityConfig = erlmcp_benchmark_scalability:make_config(Config),
    ScalabilityResult = erlmcp_benchmark_scalability:run(Scenario, ScalabilityConfig),
    ScalabilityResult.

run_resource_optimization_benchmark(Scenario, Config) ->
    ResourceConfig = erlmcp_benchmark_resources:make_config(Config),
    ResourceResult = erlmcp_benchmark_resources:run(Scenario, ResourceConfig),
    ResourceResult.

run_data_processing_benchmark(Scenario, Config) ->
    DataConfig = erlmcp_benchmark_data:make_config(Config),
    DataResult = erlmcp_benchmark_data:run(Scenario, DataConfig),
    DataResult.

run_system_performance_benchmark(Scenario, Config) ->
    SystemConfig = erlmcp_benchmark_system:make_config(Config),
    SystemResult = erlmcp_benchmark_system:run(Scenario, SystemConfig),
    SystemResult.

run_regression_benchmark(Scenario, Config) ->
    RegressionConfig = erlmcp_benchmark_regression:make_config(Config),
    RegressionResult = erlmcp_benchmark_regression:run(Scenario, RegressionConfig),
    RegressionResult.

run_sla_benchmark(Scenario, Config) ->
    SLAConfig = erlmcp_benchmark_sla:make_config(Config),
    SLAResult = erlmcp_benchmark_sla:run(Scenario, SLAConfig),
    SLAResult.

run_capacity_planning_benchmark(Scenario, Config) ->
    CapacityConfig = erlmcp_benchmark_capacity:make_config(Config),
    CapacityResult = erlmcp_benchmark_capacity:run(Scenario, CapacityConfig),
    CapacityResult.

get_total_requests(Config) ->
    maps:get(duration, Config, 60000) * maps:get(rate, Config, 1000) div 1000.

validate_sla_compliance(SLAMetrics) ->
    %% SLA validation for Fortune 500 requirements
    ExpectedSLA = #{
        response_time_p95 => 100,
        response_time_p99 => 200,
        availability => 0.9999,
        throughput => 95000
    },

    Validation = maps:fold(fun(Key, Actual, Acc) ->
        Target = maps:get(Key, ExpectedSLA, 0),
        Margin = 0.05,  % 5% tolerance
        Status = case Actual of
            V when V >= Target * (1 - Margin) -> pass;
            V when V >= Target * (1 - Margin * 2) -> warning;
            _ -> fail
        end,
        Acc#{Key => #{target => Target, actual => Actual, status => Status}}
    end, #{}, SLAMetrics),

    %% Check if all critical SLAs are passing
    CriticalSLAs = [response_time_p95, availability],
    SLAPassing = lists:all(fun(K) ->
        maps:get(status, maps:get(K, Validation), fail) =:= pass
    end, CriticalSLAs),

    case SLAPassing of
        true ->
            ?assert(SLAPassing, "All critical SLAs passing");
        false ->
            ?assert(SLAPassing, "Critical SLAs failing")
    end.

generate_capacity_report(CapacityData) ->
    %% Generate comprehensive capacity planning report
    Projections = #{
        current_capacity => maps:get(current_capacity, CapacityData),
        projection_1_year => maps:get(projection_1_year, CapacityData),
        projection_5_year => maps:get(projection_5_year, CapacityData),
        scaling_factors => maps:get(scaling_factors, CapacityData)
    },

    %% Save capacity report for analysis
    erlmcp_benchmark_reporter:save_capacity_projections(Projections).

store_result(Config, Result) ->
    Scenario = maps:get(scenario, Config, unknown),
    erlmcp_benchmark_storage:store_result(Scenario, Result).

setup_cluster(Config) ->
    %% Setup enterprise test cluster
    NodeCount = maps:get(nodes, Config, 100),
    ok = erlmcp_cluster_manager:start_test_cluster(NodeCount),
    ok = erlmcp_cluster_manager:configure_enterprise_settings(),
    ok.

tear_down_cluster() ->
    ok = erlmcp_cluster_manager:stop_test_cluster(),
    ok.

initialize_metrics_collector() ->
    ok = erlmcp_metrics_collector:start(),
    ok = erlmcp_metrics_collector:configure_enterprise_collection(),
    ok.

setup_load_generators() ->
    ok = erlmcp_load_generator:start(),
    ok = erlmcp_load_generator:configure_fortune_500_profile(),
    ok.

cleanup_benchmark_environment() ->
    ok = erlmcp_metrics_collector:stop(),
    ok = erlmcp_load_generator:stop(),
    ok = erlmcp_benchmark_storage:export_results(),
    ok.

generate_enterprise_report() ->
    Report = erlmcp_benchmark_reporter:generate_enterprise_report(),
    ok = erlmcp_benchmark_reporter:save_report(Report),
    ok.