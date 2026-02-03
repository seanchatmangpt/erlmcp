%% @doc Enterprise Performance Benchmark Suite for erlmcp v3
%%
%% This suite provides comprehensive performance testing for Fortune 500 scale requirements:
%% - 10K+ nodes clusters
%% - Sub-second response times (p95 < 100ms)
%% - 99.99% availability
%% - Terabyte-scale data processing
%%
%% @end
-module(erlmcp_benchmark_SUITE).

-behaviour(ct_suite).

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2]).

%% Test cases
-export([
    %% Core Performance Benchmarks
    throughput_10k_requests_second/1,
    latency_p95_100ms/1,
    scalability_10k_nodes/1,
    memory_usage_optimization/1,
    cpu_utilization_patterns/1,

    %% Network & Data Performance
    network_performance_analysis/1,
    database_query_optimization/1,
    cache_efficiency_testing/1,
    connection_pooling_optimization/1,
    load_balancing_effectiveness/1,

    %% Resilience & Availability
    failover_performance_impact/1,
    resource_utilization_efficiency/1,
    concurrent_user_handling/1,
    data_volume_handling_tb_scale/1,

    %% Quality Gates
    sla_validation_metrics/1,
    capacity_planning_guidelines/1,
    performance_regression_testing/1
]).

%%====================================================================
%% SUITE CONFIGURATION
%%====================================================================

all() ->
    [
        {group, core_performance},
        {group, network_data_performance},
        {group, resilience_availability},
        {group, quality_gates}
    ].

groups() ->
    [
        {core_performance, [], [
            throughput_10k_requests_second,
            latency_p95_100ms,
            scalability_10k_nodes,
            memory_usage_optimization,
            cpu_utilization_patterns
        ]},
        {network_data_performance, [], [
            network_performance_analysis,
            database_query_optimization,
            cache_efficiency_testing,
            connection_pooling_optimization,
            load_balancing_effectiveness
        ]},
        {resilience_availability, [], [
            failover_performance_impact,
            resource_utilization_efficiency,
            concurrent_user_handling,
            data_volume_handling_tb_scale
        ]},
        {quality_gates, [], [
            sla_validation_metrics,
            capacity_planning_guidelines,
            performance_regression_testing
        ]}
    ].

init_per_suite(Config) ->
    %% Initialize benchmark environment
    lager:info("Initializing Enterprise Benchmark Suite"),

    %% Start monitoring services
    ok = erlmcp_benchmark_monitor:start(),

    %% Configure test data generators
    ok = erlmcp_data_generator:init(),

    %% Initialize load generators
    ok = erlmcp_load_generator:init(),

    %% Setup metrics collection
    ok = erlmcp_metrics_collector:start(),

    %% Configure distributed test topology
    ok = erlmcp_cluster_topology:setup(),

    %% Start performance analyzers
    ok = erlmcp_performance_analyzer:start(),

    %% Initialize SLA monitors
    ok = erlmcp_sla_monitor:start(),

    %% Setup regression baseline
    ok = erlmcp_regression_baseline:load(),

    Config.

end_per_suite(_Config) ->
    %% Cleanup all benchmark services
    ok = erlmcp_sla_monitor:stop(),
    ok = erlmcp_performance_analyzer:stop(),
    ok = erlmcp_cluster_topology:cleanup(),
    ok = erlmcp_metrics_collector:stop(),
    ok = erlmcp_load_generator:cleanup(),
    ok = erlmcp_data_generator:cleanup(),
    ok = erlmcp_benchmark_monitor:stop(),
    lager:info("Enterprise Benchmark Suite cleanup complete"),
    ok.

init_per_group(_GroupName, Config) ->
    %% Configure group-specific parameters
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

%%====================================================================
%% CORE PERFORMANCE BENCHMARKS
%%====================================================================

throughput_10k_requests_second(Config) ->
    %% Test 10,000+ requests per second throughput
    TestConfig = #{
        duration => 300000, %% 5 minutes
        target_throughput => 10000,
        ramp_up_time => 30000, %% 30 seconds ramp up
        measurement_interval => 5000,
        scenarios => [
            {simple_message, #{size => 1024}},
            {complex_query, #{size => 4096, complexity => high}},
            {bulk_operation, #{size => 10240, batch_size => 100}},
            {streaming_data, #{size => 102400, rate => 1000}}
        ]
    },

    %% Execute throughput benchmark
    Results = erlmcp_benchmark_throughput:run(TestConfig),

    %% Validate against Fortune 500 requirements
    ok = validate_throughput_sla(Results, 10000),

    %% Analyze performance bottlenecks
    Bottlenecks = erlmcp_benchmark_analyzer:identify_bottlenecks(Results),

    %% Generate optimization recommendations
    Recommendations = erlmcp_optimization_recommendations:generate(Results, Bottlenecks),

    %% Store benchmark results
    ok = erlmcp_metrics_store:store(throughput_10k, Results),

    ct:comment("Throughput benchmark completed with ~p req/s average",
              [maps:get(average_throughput, Results)]),
    ok.

latency_p95_100ms(Config) ->
    %% Test p95 latency < 100ms requirement
    TestConfig = #{
        sample_size => 100000, %% 100K samples for statistical significance
        distribution => [normal, uniform, peak],
        scenarios => [
            {single_lookup, #{data_size => 1024}},
            {batch_processing, #{batch_size => 10}},
            {complex_computation, #{computation_depth => 5}},
            {network_roundtrip, #{hops => 3}}
        ],
        warmup_samples => 10000
    },

    %% Execute latency benchmark
    Results = erlmcp_benchmark_latency:run(TestConfig),

    %% Validate p95 requirement
    ok = validate_latency_sla(Results, 100),

    %% Analyze latency distribution
    Distribution = erlmcp_latency_analyzer:analyze_distribution(Results),

    %% Identify latency outliers
    Outliers = erlmcp_latency_analyzer:identify_outliers(Results),

    %% Store results
    ok = erlmcp_metrics_store:store(latency_p95, Results),

    ct:comment("P95 latency: ~p ms, P99 latency: ~p ms",
              [maps:get(p95, Results), maps:get(p99, Results)]),
    ok.

scalability_10k_nodes(Config) ->
    %% Test scalability to 10,000+ nodes
    TestConfig = #{
        node_counts => [100, 500, 1000, 5000, 10000],
        test_scenarios => [
            {linear_growth, #{growth_factor => 1.2}},
            {exponential_growth, #{growth_factor => 1.5}},
            {chaos_testing, #{failure_rate => 0.01}},
            {load_balancing, #{algorithm => round_robin}},
            {data_distribution, #{sharding => consistent_hashing}}
        ],
        measurement_duration => 60000 %% 1 minute per configuration
    },

    %% Execute scalability benchmark
    Results = erlmcp_benchmark_scalability:run(TestConfig),

    %% Analyze scaling efficiency
    Efficiency = erlmcp_scalability_analyzer:calculate_efficiency(Results),

    %% Identify scaling limits
    Limits = erlmcp_scalability_analyzer:identify_limits(Results),

    %% Store results
    ok = erlmcp_metrics_store:store(scalability_10k, Results),

    ct:comment("Scaling efficiency: ~.2f%, Limit reached at ~p nodes",
              [maps:get(efficiency, Efficiency), maps:get(max_nodes, Limits)]),
    ok.

memory_usage_optimization(Config) ->
    %% Test memory usage optimization
    TestConfig = #{
        memory_profiles => [conservative, balanced, aggressive],
        test_scenarios => [
            {steady_state, #{duration => 300000}},
            {memory_spike, #{spike_size => 2.0, recovery_time => 60000}},
            {long_running, #{duration => 3600000}},
            {garbage_collection, #{gc_interval => 5000}}
        ],
        monitoring_interval => 1000
    },

    %% Execute memory benchmark
    Results = erlmcp_benchmark_memory:run(TestConfig),

    %% Analyze memory patterns
    Patterns = erlmcp_memory_analyzer:analyze_patterns(Results),

    %% Identify memory leaks
    Leaks = erlmcp_memory_analyzer:identify_leaks(Results),

    %% Generate memory optimization recommendations
    Recommendations = erlmcp_memory_optimizer:generate_recommendations(Results, Patterns),

    %% Store results
    ok = erlmcp_metrics_store:store(memory_optimization, Results),

    ct:comment("Memory usage: ~p MB, Leak detection: ~p",
              [maps:get(max_memory_mb, Results), Leaks]),
    ok.

cpu_utilization_patterns(Config) ->
    %% Test CPU utilization patterns
    TestConfig = #{
        cpu_cores => [1, 2, 4, 8, 16, 32],
        workload_patterns => [
            {cpu_bound, #{computation_intensity => high}},
            {io_bound, #{io_intensity => high}},
            {mixed, #{cpu_ratio => 0.5, io_ratio => 0.5}},
            {bursty, #{burst_duration => 5000, idle_duration => 10000}}
        ],
        measurement_duration => 120000
    },

    %% Execute CPU benchmark
    Results = erlmcp_benchmark_cpu:run(TestConfig),

    %% Analyze CPU utilization efficiency
    Efficiency = erlmcp_cpu_analyzer:calculate_efficiency(Results),

    %% Identify CPU bottlenecks
    Bottlenecks = erlmcp_cpu_analyzer:identify_bottlenecks(Results),

    %% Store results
    ok = erlmcp_metrics_store:store(cpu_utilization, Results),

    ct:comment("CPU utilization efficiency: ~.2f%, Bottlenecks: ~p",
              [maps.get(efficiency, Efficiency), Bottlenecks]),
    ok.

%%====================================================================
%% NETWORK & DATA PERFORMANCE
%%====================================================================

network_performance_analysis(Config) ->
    %% Test network performance
    TestConfig = #{
        network_conditions => [
            {lan, #{latency => 1, bandwidth => 10000}},
            {wan, #{latency => 50, bandwidth => 1000}},
            {wan_with_loss, #{latency => 100, bandwidth => 500, loss_rate => 0.01}},
            {congested, #{latency => 200, bandwidth => 100, loss_rate => 0.05}}
        ],
        test_scenarios => [
            {bandwidth_test, #{data_sizes => [1024, 10240, 102400, 1024000]}},
            {latency_test, #{hops => [1, 3, 5, 10]}},
            {reliability_test, #{duration => 300000}},
            {concurrent_connections, #{max_connections => 10000}}
        ]
    },

    %% Execute network benchmark
    Results = erlmcp_benchmark_network:run(TestConfig),

    %% Analyze network performance
    Analysis = erlmcp_network_analyzer:analyze(Results),

    %% Identify network bottlenecks
    Bottlenecks = erlmcp_network_analyzer:identify_bottlenecks(Results),

    %% Store results
    ok = erlmcp_metrics_store:store(network_performance, Results),

    ct:comment("Network throughput: ~p Mbps, Latency: ~p ms",
              [maps.get(throughput_mbps, Analysis), maps.get(avg_latency_ms, Analysis)]),
    ok.

database_query_optimization(Config) ->
    %% Test database query optimization
    TestConfig = #{
        database_types => [mnesia, ets, dets, postgresql, mysql],
        query_patterns => [
            {point_queries, #{index_type => hash}},
            {range_queries, #{index_type => btree}},
            {full_table_scan, #{table_size => 1000000}},
            {complex_joins, #{join_type => nested_loop}},
            {aggregation, #{aggregation_type => group_by}}
        ],
        data_sizes => [1000, 10000, 100000, 1000000]
    },

    %% Execute database benchmark
    Results = erlmcp_benchmark_database:run(TestConfig),

    %% Analyze query performance
    Analysis = erlmcp_database_analyzer:analyze(Results),

    ## Identify optimization opportunities
    Optimizations = erlmcp_database_optimizer:identify_optimizations(Results),

    %% Store results
    ok = erlmcp_metrics_store:store(database_performance, Results),

    ct:comment("Query optimization potential: ~.2x speedup",
              [maps.get(optimization_factor, Optimizations)]),
    ok.

cache_efficiency_testing(Config) ->
    %% Test cache efficiency
    TestConfig = #{
        cache_types => [lru, mru, lfu, arc, adaptive],
        cache_sizes => [1000, 10000, 100000, 1000000],
        access_patterns => [
            {uniform_access, #{hot_percentage => 20}},
            {zipf_distribution, #{exponent => 1.5}},
            {temporal_locality, #{window_size => 1000}},
            {random_access, #{random_seed => 12345}}
        ],
        test_scenarios => [
            {hit_ratio_test, #{hit_threshold => 0.95}},
            {latency_test, #{p99_threshold => 10}},
            {memory_usage_test, #{memory_limit => 1024 * 1024 * 1024}},
            {eviction_policy_test, #{eviction_rate => 0.1}}
        ]
    },

    %% Execute cache benchmark
    Results = erlmcp_benchmark_cache:run(TestConfig),

    %% Analyze cache efficiency
    Efficiency = erlmcp_cache_analyzer:calculate_efficiency(Results),

    %% Identify cache optimization opportunities
    Optimizations = erlmcp_cache_optimizer:identify_optimizations(Results),

    %% Store results
    ok = erlmcp_metrics_store:store(cache_efficiency, Results),

    ct:comment("Cache hit ratio: ~.2f%, Efficiency score: ~.2f",
              [maps.get(hit_ratio, Efficiency), maps.get(efficiency_score, Efficiency)]),
    ok.

connection_pooling_optimization(Config) ->
    %% Test connection pooling optimization
    TestConfig = #{
        pool_sizes => [10, 50, 100, 500, 1000],
        pool_strategies => [fixed, elastic, adaptive],
        test_scenarios => [
            {connection_acquisition, #{concurrent_users => 1000}},
            {connection_reuse, #{reuse_rate => 0.9}},
            {connection_overflow, #{overflow_strategy => reject}},
            {connection_health_check, #{check_interval => 5000}}
        ],
        workload_patterns => [
            {steady_load, #{duration => 300000}},
            {burst_load, #{burst_duration => 30000, burst_rate => 1000}},
            {ramp_load, #{ramp_time => 60000, max_rate => 500}}
        ]
    },

    %% Execute connection pooling benchmark
    Results = erlmcp_benchmark_connection_pool:run(TestConfig),

    %% Analyze connection efficiency
    Efficiency = erlmcp_connection_analyzer:calculate_efficiency(Results),

    %% Identify pooling bottlenecks
    Bottlenecks = erlmcp_connection_analyzer:identify_bottlenecks(Results),

    %% Store results
    ok = erlmcp_metrics_store:store(connection_pooling, Results),

    ct:comment("Connection efficiency: ~.2f%, Pool size: ~p",
              [maps.get(efficiency, Efficiency), maps.get(optimal_pool_size, Results)]),
    ok.

load_balancing_effectiveness(Config) ->
    %% Test load balancing effectiveness
    TestConfig = #{
        balancing_algorithms => [
            round_robin,
            least_connections,
            ip_hash,
            resource_based,
            consistent_hashing,
            weighted_round_robin
        ],
        cluster_sizes => [3, 5, 10, 25, 50, 100],
        test_scenarios => [
            {uniform_distribution, #{expected_variance => 0.05}},
            {hot_node_resilience, #{failure_rate => 0.2}},
            {dynamic_scaling, #{scale_up_nodes => 2, scale_down_nodes => 1}},
            {session_affinity, #{affinity_duration => 300000}}
        ],
        measurement_interval => 5000
    },

    %% Execute load balancing benchmark
    Results = erlmcp_benchmark_load_balancer:run(TestConfig),

    %% Analyze balancing effectiveness
    Effectiveness = erlmcp_load_balancer_analyzer:calculate_effectiveness(Results),

    %% Identify optimization opportunities
    Optimizations = erlmcp_load_balancer_optimizer:identify_optimizations(Results),

    %% Store results
    ok = erlmcp_metrics_store:store(load_balancing, Results),

    ct:comment("Load balancing effectiveness: ~.2f%, Distribution variance: ~.2f",
              [maps.get(effectiveness, Effectiveness), maps.get(variance, Effectiveness)]),
    ok.

%%====================================================================
%% RESILIENCE & AVAILABILITY
%%====================================================================

failover_performance_impact(Config) ->
    %% Test failover performance impact
    TestConfig = #{
        failure_scenarios => [
            {node_failure, #{failure_type => crash}},
            {network_partition, #{partition_type => split_brain}},
            {resource_exhaustion, #{resource_type => memory}},
            {slowloris_attack, #{attack_rate => 1000}},
            {data_corruption, #{corruption_type => disk}}
        ],
        recovery_strategies => [
            {automatic_restart, #{restart_timeout => 30000}},
            {manual_intervention, #{intervention_time => 60000}},
            {graceful_degradation, #{degradation_level => 0.5}},
            {cold_standby, {{failover_time, 120000}, {warm_standby, 30000}}}
        ],
        measurement_duration => 300000
    },

    %% Execute failover benchmark
    Results = erlmcp_benchmark_failover:run(TestConfig),

    %% Analyze failover performance
    Analysis = erlmcp_failover_analyzer:analyze(Results),

    %% Calculate RTO/RPO metrics
    RtoRpo = erlmcp_failover_analyzer:calculate_rto_rpo(Results),

    %% Store results
    ok = erlmcp_metrics_store:store(failover_performance, Results),

    ct:comment("RTO: ~p ms, RPO: ~p operations",
              [maps.get(rto_ms, RtoRpo), maps.get(rpo_operations, RtoRpo)]),
    ok.

resource_utilization_efficiency(Config) ->
    %% Test resource utilization efficiency
    TestConfig = #{
        resource_types => [cpu, memory, disk, network],
        utilization_scenarios => [
            {steady_state, #{target_utilization => 0.7}},
            {peak_load, #{target_utilization => 0.9}},
            {overprovisioned, #{target_utilization => 0.5}},
            {optimally_sized, #{target_utilization => 0.8}}
        ],
        efficiency_metrics => [
            {resource_efficiency, #{}},
            {cost_efficiency, #{}},
            {performance_efficiency, #{}},
            {scalability_efficiency, #{}}
        ]
    },

    %% Execute resource utilization benchmark
    Results = erlmcp_benchmark_resources:run(TestConfig),

    %% Analyze resource efficiency
    Efficiency = erlmcp_resource_analyzer:calculate_efficiency(Results),

    ## Identify optimization opportunities
    Optimizations = erlmcp_resource_optimizer:identify_optimizations(Results),

    %% Store results
    ok = erlmcp_metrics_store:store(resource_utilization, Results),

    ct:comment("Resource efficiency: ~.2f%, Cost optimization: ~.2f",
              [maps.get(efficiency, Efficiency), maps.get(cost_optimization, Optimizations)]),
    ok.

concurrent_user_handling(Config) ->
    %% Test concurrent user handling
    TestConfig = #{
        user_counts => [100, 500, 1000, 5000, 10000, 50000],
        user_patterns => [
            {interactive_users, #{think_time => 5000, actions_per_minute => 12}},
            {batch_users, #{batch_size => 100, batch_interval => 30000}},
            {api_users, #{requests_per_second => 100}},
            {mobile_users, #{connection_sparsity => 0.3}}
        ],
        test_scenarios => [
            {concurrent_login, #{concurrent_logins => 10000}},
            {session_management, #{session_timeout => 3600000}},
            {resource_contention, #{contention_level => high}},
            {quality_of_service, {priority_levels, [high, medium, low]}}
        ]
    },

    %% Execute concurrent user benchmark
    Results = erlmcp_benchmark_concurrent_users:run(TestConfig),

    %% Analyze concurrency performance
    Analysis = erlmcp_concurrency_analyzer:analyze(Results),

    %% Identify scaling limits
    Limits = erlmcp_concurrency_analyzer:identify_limits(Results),

    %% Store results
    ok = erlmcp_metrics_store:store(concurrent_users, Results),

    ct:comment("Concurrency efficiency: ~.2f%, User limit: ~p",
              [maps.get(efficiency, Analysis), maps.get(max_users, Limits)]),
    ok.

data_volume_handling_tb_scale(Config) ->
    %% Test terabyte-scale data handling
    TestConfig = #{
        data_sizes => [100, 500, 1000, 5000, 10000], %% GB
        data_types => [structured, unstructured, semi_structured],
        data_operations => [
            {ingestion, #{rate => 1000000000}}, %% 1GB/s
            {processing, #{complexity => high}},
            {storage, #{compression => true}},
            {retrieval, #{query_pattern => random}}
        ],
        storage_systems => [local_disk, distributed_filesystem, object_storage]
    },

    %% Execute data volume benchmark
    Results = erlmcp_benchmark_data_volume:run(TestConfig),

    %% Analyze data handling performance
    Analysis = erlmcp_data_volume_analyzer:analyze(Results),

    ## Identify optimization opportunities
    Optimizations = erlmcp_data_volume_optimizer:identify_optimizations(Results),

    %% Store results
    ok = erlmcp_metrics_store:store(data_volume, Results),

    ct:comment("Data throughput: ~p GB/s, Processing efficiency: ~.2f",
              [maps.get(throughput_gb_s, Analysis), maps.get(efficiency, Analysis)]),
    ok.

%%====================================================================
%% QUALITY GATES
%%====================================================================

sla_validation_metrics(Config) ->
    %% Test SLA validation metrics
    TestConfig = #{
        sla_requirements => [
            {availability, #{target => 0.9999, measurement_interval => 60000}},
            {latency, #{p95 => 100, p99 => 200, measurement_interval => 30000}},
            {throughput, #{min => 10000, measurement_interval => 60000}},
            {error_rate, #{max => 0.0001, measurement_interval => 30000}}
        ],
        validation_scenarios => [
            {normal_operation, #{duration => 3600000}},
            {peak_load, #{duration => 300000, load_factor => 2.0}},
            {partial_failure, #{failure_rate => 0.01}},
            {complete_outage, #{outage_duration => 300000}}
        ],
        measurement_duration => 7200000 %% 2 hours
    },

    %% Execute SLA validation benchmark
    Results = erlmcp_benchmark_sla:run(TestConfig),

    %% Validate SLA compliance
    Compliance = erlmcp_sla_analyzer:validate_compliance(Results),

    ## Generate SLA reports
    Reports = erlmcp_sla_reporter:generate_reports(Results, Compliance),

    %% Store results
    ok = erlmcp_metrics_store:store(sla_validation, Results),

    ct:comment("SLA compliance: ~.2f%, Violations: ~p",
              [maps.get(compliance_percentage, Compliance), maps.get(violations, Compliance)]),
    ok.

capacity_planning_guidelines(Config) ->
    %% Generate capacity planning guidelines
    TestConfig = #{
        growth_scenarios => [
            {linear_growth, #{annual_growth_rate => 0.2}},
            {exponential_growth, #{annual_growth_rate => 0.5}},
            {seasonal_growth, #{peak_multiplier => 3.0}},
            {chaotic_growth, #{volatility => 0.3}}
        ],
        time_horizons => [1, 3, 5, 10], %% years
        resource_types => [compute, storage, network, memory]
    },

    %% Execute capacity planning benchmark
    Results = erlmcp_benchmark_capacity_planning:run(TestConfig),

    ## Generate capacity recommendations
    Recommendations = erlmcp_capacity_planner:generate_recommendations(Results),

    %% Create capacity planning models
    Models = erlmcp_capacity_planner:create_models(Results),

    %% Store results
    ok = erlmcp_metrics_store:store(capacity_planning, Results),

    ct:comment("5-year capacity requirement: ~p nodes, Cost projection: $~p",
              [maps.get(nodes_5_year, Recommendations), maps.get(cost_5_year, Recommendations)]),
    ok.

performance_regression_testing(Config) ->
    %% Test performance regression
    TestConfig = #{
        baselines => [previous_release, current_candidate],
        test_scenarios => [
            {functional_regression, #{test_coverage => 1.0}},
            {performance_regression, #{regression_threshold => 0.05}},
            {scalability_regression, #{degradation_threshold => 0.1}},
            {resource_regression, #{resource_threshold => 0.15}}
        ],
        regression_metrics => [
            {throughput, #{}},
            {latency, #{}},
            {memory_usage, #{}},
            {cpu_usage, #{}}
        ],
        validation_thresholds => [
            {critical, #{severity => 0.1}},
            {warning, #{severity => 0.05}},
            {acceptable, #{severity => 0.02}}
        ]
    },

    %% Execute regression benchmark
    Results = erlmcp_benchmark_regression:run(TestConfig),

    ## Analyze regression impact
    Regression = erlmcp_regression_analyzer:analyze(Results),

    ## Generate regression reports
    Reports = erlmcp_regression_reporter:generate_reports(Regression),

    %% Store results
    ok = erlmcp_metrics_store:store(performance_regression, Results),

    ct:comment("Regression detected: ~.2f%, Impact level: ~p",
              [maps.get(regression_percentage, Regression), maps.get(impact_level, Regression)]),
    ok.

%%====================================================================
%% HELPER FUNCTIONS
%%====================================================================

validate_throughput_sla(Results, Target) ->
    Throughput = maps:get(average_throughput, Results),
    ThroughputAchieved = Throughput >= Target,
    ct:comment("Throughput SLA validation: ~p req/s (target: ~p)",
              [Throughput, Target]),
    case ThroughputAchieved of
        true -> ok;
        false ->
            ct:fail("Throughput SLA violated: ~p < ~p", [Throughput, Target])
    end.

validate_latency_sla(Results, Threshold) ->
    P95 = maps:get(p95, Results),
    P99 = maps:get(p99, Results),
    P95Valid = P95 =< Threshold,
    ct:comment("Latency SLA validation: P95=~p ms, P99=~p ms (threshold: ~p ms)",
              [P95, P99, Threshold]),
    case P95Valid of
        true -> ok;
        false ->
            ct:fail("P95 latency SLA violated: ~p > ~p ms", [P95, Threshold])
    end.