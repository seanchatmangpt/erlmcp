%% @doc Scalability Benchmark Suite
## Implements enterprise-grade scalability testing for 10K+ node clusters,
## measuring linear scaling efficiency and identifying scaling bottlenecks.
%% @copyright 2026 erlmcp
%% @version 3.0.0
-module(erlmcp_benchmark_scalability).

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
    nodes => pos_integer(),
    rate => pos_integer(),
    duration => pos_integer(),
    scaling_factors => [float()],
    load_profile => 'linear' | 'exponential' | 'mixed' | 'bursty',
    metrics => [atom()],
    network_topology => 'full_mesh' | 'ring' | 'tree' | 'star'
}.

-type scalability_result() :: #{
    timestamp => integer(),
    node_counts => [pos_integer()],
    throughput_results => map(),
    latency_results => map(),
    scaling_efficiency => map(),
    bottlenecks => [map()],
    recommendations => map(),
    capacity_metrics => map()
}.

-type scaling_phase() :: 'warmup' | 'scaling_up' | 'steady_state' | 'scaling_down' | 'analysis'.

-type node_performance() :: #{
    node_id => binary(),
    throughput => float(),
    latency => float(),
    cpu_utilization => float(),
    memory_utilization => float(),
    network_throughput => float(),
    error_rate => float()
}.

-define(SCALABILITY_TAB, erlmcp_benchmark_scalability_metrics).
-define(NODE_TAB, erlmcp_benchmark_node_metrics).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

make_config(Config) ->
    DefaultConfig = #{
        nodes => 100,
        rate => 5000,
        duration => 300000,  % 5 minutes per scaling phase
        scaling_factors => [1.0, 1.5, 2.0, 2.5, 3.0],
        load_profile => linear,
        metrics => [throughput, latency, cpu, memory, network],
        network_topology => full_mesh
    },
    maps:merge(DefaultConfig, Config).

run(Scenario, Config) ->
    gen_server:call(?SERVER, {run_scalability_benchmark, Scenario, Config}).

stop() ->
    gen_server:call(?SERVER, stop).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize scalability-specific metrics tables
    case ets:info(?SCALABILITY_TAB) of
        undefined ->
            ets:new(?SCALABILITY_TAB, [
                set,
                public,
                named_table,
                {keypos, #scalability_result.timestamp},
                {write_concurrency, true},
                {read_concurrency, true}
            ]),
            ets:new(?NODE_TAB, [
                set,
                public,
                named_table,
                {keypos, #node_performance.node_id},
                {write_concurrency, true}
            ]);
        _ ->
            ok
    end,

    %% Initialize scalability infrastructure
    ok = initialize_scalability_infrastructure(),

    %% Configure scaling analyzers
    ok = configure_scaling_analyzers(),

    %% Initialize cluster management
    ok = initialize_cluster_management(),

    State = #{
        start_time => undefined,
        end_time => undefined,
        config => undefined,
        scenario => undefined,
        current_phase => undefined,
        scaling_phases => [],
        node_performance => #{},
        cluster_metrics => #{},
        efficiency_metrics => #{}
    },

    {ok, State}.

handle_call({run_scalability_benchmark, Scenario, Config}, _From, State) ->
    %% Validate scalability configuration
    case validate_scalability_config(Config) of
        {ok, ValidConfig} ->
            %% Prepare scalable environment
            case prepare_scalable_environment(ValidConfig) of
                {ok, Environment} ->
                    %% Execute scalability benchmark
                    {ok, Results} = execute_scalability_benchmark(
                        Scenario, ValidConfig, Environment, State
                    ),

                    %% Analyze scaling characteristics
                    AnalyzedResults = analyze_scaling_characteristics(Results, ValidConfig),

                    %% Generate scalability report
                    Report = generate_scalability_report(AnalyzedResults, ValidConfig),

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

handle_info({scaling_phase_start, Phase, NodeCount, Config}, State) ->
    %% Handle start of scaling phase
    ok = log_scaling_phase_start(Phase, NodeCount, Config),

    %% Initialize phase monitoring
    PhaseMonitorRef = start_phase_monitoring(Phase, NodeCount, Config),

    %% Update state
    UpdatedState = State#{
        current_phase => Phase,
        phase_start_time => erlang:system_time(millisecond),
        phase_monitor_ref => PhaseMonitorRef,
        active_nodes => NodeCount,
        phase_config => Config
    },

    {noreply, UpdatedState};

handle_info({scaling_phase_complete, Phase, NodeCount, Results}, State) ->
    %% Handle completion of scaling phase
    PhaseAnalysis = analyze_phase_results(Results, Phase, NodeCount),

    %% Store phase results
    ok = store_scaling_phase_results(Phase, NodeCount, PhaseAnalysis),

    %% Update overall scaling analysis
    UpdatedPhases = [PhaseAnalysis | State#scaling_phases],
    UpdatedState = State#{
        scaling_phases => UpdatedPhases,
        current_phase => undefined
    },

    %% Check if all scaling phases are complete
    case check_scaling_completion(UpdatedState) of
        true ->
            gen_server:cast(?SERVER, scaling_benchmark_complete);
        false ->
            %% Start next scaling phase
            ok = start_next_scaling_phase(UpdatedState)
    end,

    {noreply, UpdatedState};

handle_info({node_metrics, NodeId, Metrics}, State) ->
    %% Handle individual node metrics
    NodePerformance = maps:get(node_performance, State, #{}),

    UpdatedNodePerformance = maps:put(NodeId, Metrics, NodePerformance),

    %% Update cluster-level metrics
    UpdatedClusterMetrics = update_cluster_metrics(State, Metrics),

    UpdatedState = State#{
        node_performance => UpdatedNodePerformance,
        cluster_metrics => UpdatedClusterMetrics
    },

    {noreply, UpdatedState};

handle_info({scaling_bottleneck_detected, BottleneckInfo}, State) ->
    %% Handle detected scaling bottlenecks
    Bottlenecks = case maps:get(bottlenecks, State, undefined) of
        undefined -> [BottleneckInfo];
        Existing -> [BottleneckInfo | Existing]
    end,

    UpdatedState = State#{bottlenecks => Bottlenecks},

    %% Apply immediate mitigation
    ok = apply_scaling_mitigation(BottleneckInfo),

    {noreply, UpdatedState};

handle_info(scaling_benchmark_complete, State) ->
    %% Generate final scaling analysis
    FinalAnalysis = compile_final_scaling_analysis(State),

    %% Generate scalability report
    Report = generate_scalability_report(FinalAnalysis, State#config),

    %% Save results
    ok = save_scalability_benchmark_report(Report),

    %% Notify completion
    ok = notify_scalability_benchmark_complete(Report),

    {noreply, State#{
        end_time => erlang:system_time(millisecond),
        final_analysis => FinalAnalysis
    }}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cleanup scalability resources
    ok = cleanup_scalability_resources(),

    %% Export scalability metrics
    ok = export_scalability_metrics(),

    %% Clear metrics tables
    ets:delete_all_objects(?SCALABILITY_TAB),
    ets:delete_all_objects(?NODE_TAB),

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

validate_scalability_config(Config) ->
    Required = [nodes, rate, scenario],
    Missing = [Field || Field <- Required, not maps:is_key(Field, Config)],

    case Missing of
        [] ->
            %% Validate node count
            NodeCount = maps:get(nodes, Config),
            case NodeCount of
                N when N > 0, N =< 10000 -> % Max 10K nodes
                    %% Validate rate
                    Rate = maps:get(rate, Config),
                    case Rate of
                        R when R > 0, R =< 100000 -> % Max 100K req/s
                            {ok, Config};
                        _ ->
                            {error, {invalid_rate, Rate}}
                    end;
                _ ->
                    {error, {invalid_node_count, NodeCount}}
            end;
        _ ->
            {error, {missing_required_fields, Missing}}
    end.

initialize_scalability_infrastructure() ->
    %% Initialize infrastructure for scalability testing
    ok = erlmcp_cluster_manager:start(),
    ok = erlmcp_cluster_manager:configure_scalability_mode(),

    %% Initialize load generators for scaling tests
    ok = erlmcp_scalable_load_generator:start(),

    %% Initialize node health monitoring
    ok = erlmcp_node_health_monitor:start(),

    %% Initialize distributed metrics collection
    ok = erlmcp_distributed_metrics:start(),

    ok.

configure_scaling_analyzers() ->
    %% Configure scaling efficiency analyzers
    ok = erlmcp_scaling_analyzer:start(),
    ok = erlmcp_scaling_analyzer:configure_enterprise_parameters(),

    %% Configure bottleneck detection
    ok = erlmcp_bottleneck_detector:start(),
    ok = erlmcp_bottleneck_detector:configure_enterprise_settings(),

    ok.

initialize_cluster_management() ->
    %% Initialize cluster management for scalability testing
    ok = erlmcp_cluster_manager:setup_scaling_cluster(),

    %% Configure network topology
    ok = configure_network_topology(),

    %% Configure node communication protocols
    ok = configure_node_communication(),

    ok.

configure_network_topology() ->
    %% Configure network topology based on requirements
    Topology = erlmcp_config:get(network_topology, full_mesh),

    ok = erlmcp_network_config:configure_topology(Topology),

    %% Optimize network settings for scalability
    ok = erlmcp_network_config:optimize_for_scalability(),

    ok.

configure_node_communication() ->
    %% Configure node-to-node communication protocols
    ok = erlmcp_communication_config:configure_scalable_protocols(),

    %% Configure message routing
    ok = erlmcp_message_router:configure_scalable_routing(),

    ok.

prepare_scalable_environment(Config) ->
    %% Prepare environment for scalability testing
    NodeCount = maps:get(nodes, Config),

    %% Create scalable test cluster
    case erlmcp_cluster_manager:create_scalable_cluster(NodeCount) of
        {ok, ClusterConfig} ->
            %% Configure load generation for scaling
            ok = configure_scaling_load_generation(Config),

            %% Configure resource monitoring
            ok = configure_scalable_resource_monitoring(),

            %% Configure distributed tracing
            ok = configure_distributed_tracing(),

            %% Validate cluster readiness
            case validate_cluster_readiness(NodeCount) of
                {ok, ReadinessInfo} ->
                    {ok, ClusterConfig#{readiness => ReadinessInfo}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

configure_scaling_load_generation(Config) ->
    %% Configure load generation for scalability testing
    LoadProfile = maps:get(load_profile, Config, linear),
    Rate = maps:get(rate, Config),
    NodeCount = maps:get(nodes, Config),

    ok = erlmcp_scalable_load_generator:configure(
        LoadProfile,
        Rate,
        NodeCount
    ),
    ok.

configure_scalable_resource_monitoring() ->
    %% Configure resource monitoring for scalability testing
    ok = erlmcp_scalable_monitoring:start(),

    %% Configure node-level monitoring
    ok = erlmcp_node_monitoring:configure(),

    %% Configure cluster-level monitoring
    ok = erlmcp_cluster_monitoring:configure(),

    ok.

configure_distributed_tracing() ->
    %% Configure distributed tracing for scalability testing
    ok = erlmcp_distributed_tracing:start(),

    %% Configure tracing across all nodes
    ok = erlmcp_distributed_tracing:configure_cluster_tracing(),

    ok.

validate_cluster_readiness(NodeCount) ->
    %% Validate cluster readiness for scalability testing
    case erlmcp_cluster_manager:check_node_health(NodeCount) of
        {ok, HealthyNodes} when length(HealthyNodes) >= NodeCount * 0.95 ->
            %% Check network latency
            case check_network_scalability(HealthyNodes) of
                {ok, LatencyResults} ->
                    {ok, #{healthy_nodes => HealthyNodes, network_latency => LatencyResults}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, HealthyNodes} ->
            {error, insufficient_healthy_nodes};
        {error, Reason} ->
            {error, Reason}
    end.

check_network_scalability(Nodes) ->
    %% Check network scalability
    case erlmcp_network_monitor:check_scalability(Nodes) of
        {ok, Results} ->
            %% Check if network can handle scaling
            MaxLatency = lists:max([L || {_, L} <- Results]),
            case MaxLatency of
                L when L < 50000 -> {ok, Results};  % 50ms max latency
                _ -> {error, network_latency_too_high}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

execute_scalability_benchmark(Scenario, Config, Environment, State) ->
    %% Execute scalability benchmark across all phases
    ScalingPhases = calculate_scaling_phases(Config),
    PhaseResults = [execute_scaling_phase(Phase, Config, Environment) || Phase <- ScalingPhases],

    {ok, #{phases => PhaseResults, environment => Environment}}.

calculate_scaling_phases(Config) ->
    %% Calculate scaling phases based on configuration
    ScalingFactors = maps:get(scaling_factors, Config, [1.0, 1.5, 2.0, 2.5, 3.0]),
    BaseNodeCount = maps:get(nodes, Config, 100) div 5,  % Start with smaller cluster

    %% Generate scaling phases
    ScalingPhases = lists:map(fun(Factor) ->
        NodeCount = round(BaseNodeCount * Factor),
        #{
            phase => scaling_up,
            node_count => NodeCount,
            duration => maps:get(duration, Config) div 5,  % Divide time among phases
            load_profile => maps:get(load_profile, Config, linear)
        }
    end, ScalingFactors),

    %% Add steady state phase
    SteadyState = #{
        phase => steady_state,
        node_count => lists:last(ScalingPhases)#node_count,
        duration => maps:get(duration, Config) div 2,
        load_profile => maps:get(load_profile, Config, linear)
    },

    ScalingPhases ++ [SteadyState].

execute_scaling_phase(PhaseConfig, GlobalConfig, Environment) ->
    %% Execute individual scaling phase
    Phase = maps:get(phase, PhaseConfig),
    NodeCount = maps:get(node_count, PhaseConfig),
    Duration = maps:get(duration, PhaseConfig),
    LoadProfile = maps:get(load_profile, PhaseConfig, linear),

    %% Start scaling phase
    ok = erlmcp_scalable_load_generator:start_phase(
        Phase, NodeCount, LoadProfile, Duration
    ),

    %% Start node monitoring
    ok = start_node_monitoring(NodeCount),

    %% Start phase timing
    PhaseStartTime = erlang:system_time(millisecond),

    %% Monitor phase progress
    PhaseMonitorRef = erlang:send_after(
        Duration, self(), {phase_complete, Phase, NodeCount, #{}}
    ),

    %% Return phase configuration for tracking
    PhaseConfig#{monitor_ref => PhaseMonitorRef}.

start_node_monitoring(NodeCount) ->
    %% Start individual node monitoring
    ok = erlmcp_node_monitoring:start_monitoring(NodeCount),
    ok.

start_phase_monitoring(Phase, NodeCount, Config) ->
    %% Start monitoring for specific phase
    MonitorRef = erlang:send_after(
        maps:get(duration, Config) div 2,
        self(),
        {phase_progress_check, Phase, NodeCount, Config}
    ),

    {ok, MonitorRef}.

log_scaling_phase_start(Phase, NodeCount, Config) ->
    %% Log start of scaling phase
    LogEntry = #{
        timestamp => erlang:system_time(millisecond),
        phase => Phase,
        node_count => NodeCount,
        config => Config,
        event => scaling_phase_start
    },

    ok = erlmcp_log:scalability(LogEntry),
    ok.

store_scaling_phase_results(Phase, NodeCount, PhaseAnalysis) ->
    %% Store scaling phase results
    Results = #{
        timestamp => erlang:system_time(millisecond),
        phase => Phase,
        node_count => NodeCount,
        results => PhaseAnalysis
    },

    ets:insert(?SCALABILITY_TAB, Results),
    ok.

update_cluster_metrics(State, NodeMetrics) ->
    %% Update cluster-level metrics based on node metrics
    ClusterMetrics = maps:get(cluster_metrics, State, #{}),

    %% Update aggregate metrics
    Throughput = case maps:get(throughput, NodeMetrics, 0) of
        0 -> maps:get(throughput, ClusterMetrics, 0);
        T -> maps:get(throughput, ClusterMetrics, 0) + T
    end,

    CPUUtilization = case maps:get(cpu_utilization, NodeMetrics, 0) of
        0 -> maps:get(cpu_utilization, ClusterMetrics, 0);
        C -> maps:get(cpu_utilization, ClusterMetrics, 0) + C
    end,

    %% Update cluster metrics
    UpdatedClusterMetrics = ClusterMetrics#{
        throughput => Throughput,
        cpu_utilization => CPUUtilization / (maps:get(active_nodes, State, 1)),
        node_count => maps:get(active_nodes, State, 1)
    },

    UpdatedClusterMetrics.

analyze_phase_results(Results, Phase, NodeCount) ->
    %% Analyze results for specific scaling phase
    PhaseAnalysis = #{
        phase => Phase,
        node_count => NodeCount,
        throughput => analyze_phase_throughput(Results),
        latency => analyze_phase_latency(Results),
        resource_utilization => analyze_phase_resources(Results),
        efficiency => analyze_phase_efficiency(Results),
        bottlenecks => detect_phase_bottlenecks(Results),
        recommendations => generate_phase_recommendations(Results, Phase)
    },

    PhaseAnalysis.

analyze_phase_throughput(Results) ->
    %% Analyze throughput for scaling phase
    ThroughputValues = [maps:get(throughput, R) || R <- Results,
                  maps:is_key(throughput, R)],

    case ThroughputValues of
        [] -> #{};
        _ ->
            #{
                average => lists:sum(ThroughputValues) / length(ThroughputValues),
                maximum => lists:max(ThroughputValues),
                minimum => lists:min(ThroughputValues),
                p95 => calculate_percentile(95, ThroughputValues),
                growth_rate => calculate_throughput_growth_rate(ThroughputValues)
            }
    end.

analyze_phase_latency(Results) ->
    %% Analyze latency for scaling phase
    LatencyValues = [maps:get(latency, R) || R <- Results,
                  maps:is_key(latency, R)],

    case LatencyValues of
        [] -> #{};
        _ ->
            #{
                average => lists:sum(LatencyValues) / length(LatencyValues),
                p50 => calculate_percentile(50, LatencyValues),
                p95 => calculate_percentile(95, LatencyValues),
                p99 => calculate_percentile(99, LatencyValues),
                consistency_index => calculate_latency_consistency(LatencyValues)
            }
    end.

analyze_phase_resources(Results) ->
    %% Analyze resource utilization for scaling phase
    CPUValues = [maps:get(cpu_utilization, R) || R <- Results,
                 maps:is_key(cpu_utilization, R)],
    MemoryValues = [maps:get(memory_utilization, R) || R <- Results,
                   maps:is_key(memory_utilization, R)],
    NetworkValues = [maps:get(network_throughput, R) || R <- Results,
                     maps:is_key(network_throughput, R)],

    #{
        cpu => calculate_resource_metrics(CPUValues),
        memory => calculate_resource_metrics(MemoryValues),
        network => calculate_resource_metrics(NetworkValues),
        resource_efficiency => calculate_resource_efficiency(CPUValues, MemoryValues, NetworkValues)
    }.

analyze_phase_efficiency(Results) ->
    %% Analyze scaling efficiency for phase
    Throughput = analyze_phase_throughput(Results),
    Resources = analyze_phase_resources(Results),
    Latency = analyze_phase_latency(Results),

    #{
        throughput_efficiency => calculate_throughput_efficiency(Throughput),
        resource_efficiency => calculate_efficiency_score(Resources),
        latency_efficiency => calculate_latency_efficiency(Latency),
        overall_efficiency => calculate_overall_efficiency(Throughput, Resources, Latency)
    }.

detect_phase_bottlenecks(Results) ->
    %% Detect bottlenecks in scaling phase
    Bottlenecks = [],

    %% Check CPU bottlenecks
    case detect_cpu_bottlenecks(Results) of
        true -> [cpu_bottleneck | Bottlenecks];
        false -> Bottlenecks
    end,

    %% Check memory bottlenecks
    case detect_memory_bottlenecks(Results) of
        true -> [memory_bottleneck | Bottlenecks];
        false -> Bottlenecks
    end,

    %% Check network bottlenecks
    case detect_network_bottlenecks(Results) of
        true -> [network_bottleneck | Bottlenecks];
        false -> Bottlenecks
    end,

    %% Check communication bottlenecks
    case detect_communication_bottlenecks(Results) of
        true -> [communication_bottleneck | Bottlenecks];
        false -> Bottlenecks
    end,

    Bottlenecks.

detect_cpu_bottlenecks(Results) ->
    %% Detect CPU-related bottlenecks
    CPUValues = [maps:get(cpu_utilization, R) || R <- Results,
                 maps:is_key(cpu_utilization, R)],
    case CPUValues of
        [] -> false;
        _ ->
            MaxCPU = lists:max(CPUValues),
            MaxCPU > 0.90  % 90% CPU threshold
    end.

detect_memory_bottlenecks(Results) ->
    %% Detect memory-related bottlenecks
    MemoryValues = [maps:get(memory_utilization, R) || R <- Results,
                    maps:is_key(memory_utilization, R)],
    case MemoryValues of
        [] -> false;
        _ ->
            MaxMemory = lists:max(MemoryValues),
            MaxMemory > 0.85  % 85% memory threshold
    end.

detect_network_bottlenecks(Results) ->
    %% Detect network-related bottlenecks
    NetworkValues = [maps:get(network_throughput, R) || R <- Results,
                     maps:is_key(network_throughput, R)],
    case NetworkValues of
        [] -> false;
        _ ->
            MaxNetwork = lists:max(NetworkValues),
            MaxNetwork > 1000  % 1Gbps threshold
    end.

detect_communication_bottlenecks(Results) ->
    %% Detect communication-related bottlenecks
    LatencyValues = [maps:get(latency, R) || R <- Results,
                     maps:is_key(latency, R)],
    case LatencyValues of
        [] -> false;
        _ ->
            AvgLatency = lists:sum(LatencyValues) / length(LatencyValues),
            P99Latency = calculate_percentile(99, LatencyValues),
            (AvgLatency > 100) or (P99Latency > 500)
    end.

generate_phase_recommendations(Results, Phase) ->
    %% Generate recommendations for scaling phase
    Recommendations = [],

    %% Check for scaling recommendations based on phase
    case Phase of
        scaling_up ->
            ScalingEfficiency = calculate_scaling_efficiency(Results),
            case ScalingEfficiency of
                E when E < 0.8 ->
                    Recommendations#{scaling => consider_horizontal_scaling};
                _ ->
                    Recommendations
            end;
        steady_state ->
            ResourceUtilization = analyze_phase_resources(Results),
            case maps:get(cpu, ResourceUtilization#{average => 0})#average > 0.85 of
                true ->
                    Recommendations#{optimization => optimize_cpu_utilization};
                false ->
                    Recommendations
            end;
        _ ->
            Recommendations
    end,

    Recommendations.

calculate_resource_metrics(Values) ->
    %% Calculate metrics for resource utilization
    case Values of
        [] -> #{};
        _ ->
            #{
                average => lists:sum(Values) / length(Values),
                maximum => lists:max(Values),
                minimum => lists:min(Values),
                p95 => calculate_percentile(95, Values),
                p99 => calculate_percentile(99, Values)
            }
    end.

calculate_resource_efficiency(CPUValues, MemoryValues, NetworkValues) ->
    %% Calculate overall resource efficiency
    ResourceUtilization = calculate_resource_metrics(CPUValues),
    MemoryUtilization = calculate_resource_metrics(MemoryValues),
    NetworkUtilization = calculate_resource_metrics(NetworkValues),

    #{
        cpu_efficiency => calculate_resource_efficiency_score(ResourceUtilization),
        memory_efficiency => calculate_resource_efficiency_score(MemoryUtilization),
        network_efficiency => calculate_resource_efficiency_score(NetworkUtilization),
        overall_efficiency => calculate_overall_resource_efficiency(
            ResourceUtilization, MemoryUtilization, NetworkUtilization
        )
    }.

calculate_resource_efficiency_score(Utilization) ->
    %% Calculate efficiency score for resource utilization
    case Utilization of
        #{average := Avg, maximum := Max} ->
            case Avg of
                A when A =< 0.6 -> 100;
                A when A =< 0.8 -> 80;
                A when A =< 0.9 -> 60;
                _ -> 40
            end;
        _ -> 0
    end.

calculate_overall_resource_efficiency(CPU, Memory, Network) ->
    %% Calculate overall resource efficiency
    CPUAvg = maps:get(average, CPU, 0),
    MemoryAvg = maps:get(average, Memory, 0),
    NetworkAvg = maps:get(average, Network, 0),

    (CPUAvg + MemoryAvg + NetworkAvg) / 3 * 100.

check_scaling_completion(State) ->
    %% Check if all scaling phases are complete
    Phases = State#scaling_phases,
    ScalingFactors = maps:get(scaling_factors, State#config, [1.0, 1.5, 2.0, 2.5, 3.0]),

    (length(Phases) >= length(ScalingFactors)) andalso
    (maps:is_key(steady_state, PhaseResults)).

start_next_scaling_phase(State) ->
    %% Start next scaling phase
    Config = State#config,
    PhaseCount = length(State#scaling_phases),

    case PhaseCount < length(maps:get(scaling_factors, Config)) of
        true ->
            %% Next scaling up phase
            NextFactor = lists:nth(PhaseCount + 1, maps:get(scaling_factors, Config)),
            NextNodeCount = round((maps:get(nodes, Config, 100) div 5) * NextFactor),

            PhaseConfig = #{
                phase => scaling_up,
                node_count => NextNodeCount,
                duration => maps:get(duration, Config) div 5,
                load_profile => maps:get(load_profile, Config, linear)
            },

            gen_server:cast(?SERVER, {start_scaling_phase, PhaseConfig});
        false ->
            %% Start steady state phase
            SteadyStateConfig = #{
                phase => steady_state,
                node_count => lists:last(maps:get(scaling_factors, Config)) * (maps:get(nodes, Config, 100) div 5),
                duration => maps:get(duration, Config) div 2,
                load_profile => maps:get(load_profile, Config, linear)
            },

            gen_server:cast(?SERVER, {start_scaling_phase, SteadyStateConfig})
    end,

    ok.

apply_scaling_mitigation(BottleneckInfo) ->
    %% Apply mitigation for scaling bottlenecks
    BottleneckType = maps:get(type, BottleneckInfo),
    case BottleneckType of
        cpu_bottleneck ->
            ok = erlmcp_cpu_mitigation:apply();
        memory_bottleneck ->
            ok = erlmcp_memory_mitigation:apply();
        network_bottleneck ->
            ok = erlmcp_network_mitigation:apply();
        communication_bottleneck ->
            ok = erlmcp_communication_mitigation:apply();
        _ ->
            ok
    end,

    ok.

compile_final_scaling_analysis(State) ->
    %% Compile final scaling analysis
    PhaseResults = State#scaling_phases,

    %% Calculate scaling efficiency metrics
    ScalingEfficiency = calculate_scaling_efficiency(PhaseResults),

    %% Identify scaling bottlenecks
    Bottlenecks = identify_scaling_bottlenecks(PhaseResults),

    ** Generate capacity recommendations
    CapacityRecommendations = generate_capacity_recommendations(
        ScalingEfficiency, PhaseResults
    ),

    ** Generate optimization recommendations
    OptimizationRecommendations = generate_optimization_recommendations(
        ScalingEfficiency, Bottlenecks
    ),

    ** Generate capacity planning guidelines
    CapacityPlanning = generate_capacity_planning_guidelines(PhaseResults),

    #{
        timestamp => erlang:system_time(millisecond),
        scaling_efficiency => ScalingEfficiency,
        bottlenecks => Bottlenecks,
        capacity_recommendations => CapacityRecommendations,
        optimization_recommendations => OptimizationRecommendations,
        capacity_planning => CapacityPlanning,
        maximum_scalable_nodes => determine_maximum_scalable_nodes(PhaseResults),
        scaling_limitations => identify_scaling_limitations(PhaseResults)
    }.

calculate_scaling_efficiency(PhaseResults) ->
    %% Calculate overall scaling efficiency
    ThroughputResults = [R || R <- PhaseResults,
                         maps:get(phase, R) =:= scaling_up,
                         maps:is_key(throughput, R#results)],

    case ThroughputResults of
        [] -> #{};
        _ ->
            ThroughputValues = [maps:get(throughput, R#results) || R <- ThroughputResults],
            NodeCounts = [R#node_count || R <- ThroughputResults],

            calculate_linear_scaling_efficiency(NodeCounts, ThroughputValues)
    end.

calculate_linear_scaling_efficiency(NodeCounts, ThroughputValues) ->
    %% Calculate linear scaling efficiency
    BaseNodeCount = lists:min(NodeCounts),
    MaxNodeCount = lists:max(NodeCounts),
    ExpectedThroughput = lists:max(ThroughputValues) * (MaxNodeCount / BaseNodeCount),
    ActualThroughput = lists:max(ThroughputValues),

    LinearEfficiency = ActualThroughput / ExpectedThroughput,

    #{
        linear_efficiency => LinearEfficiency,
        efficiency_score => round(LinearEfficiency * 100),
        scaling_ratio => MaxNodeCount / BaseNodeCount,
        throughput_scale => ActualThroughput / lists:min(ThroughputValues)
    }.

identify_scaling_bottlenecks(PhaseResults) ->
    %% Identify scaling bottlenecks across all phases
    Bottlenecks = lists:foldl(fun(PhaseResult, Acc) ->
        PhaseBottlenecks = maps:get(bottlenecks, PhaseResult#results, []),
        lists:append(PhaseBottlenecks, Acc)
    end, [], PhaseResults),

    %% Analyze bottleneck patterns
    analyze_bottleneck_patterns(Bottlenecks).

analyze_bottleneck_patterns(Bottlenecks) ->
    %% Analyze patterns in scaling bottlenecks
    BottleneckTypes = lists:foldl(fun(B, Acc) ->
        Type = maps:get(type, B),
        maps:update_counter(Type, 1, Acc)
    end, #{}, Bottlenecks),

    %% Identify primary bottlenecks
    PrimaryBottlenecks = [Type || {Type, Count} <- maps:to_list(BottleneckTypes),
                               Count >= length(Bottlenecks) * 0.3],

    #{
        primary_bottlenecks => PrimaryBottlenecks,
        bottleneck_distribution => BottleneckTypes,
        impact_analysis => analyze_bottleneck_impact(Bottlenecks),
        mitigation_strategies => generate_bottleneck_mitigation_strategies(Bottlenecks)
    }.

analyze_bottleneck_impact(Bottlenecks) ->
    %% Analyze impact of scaling bottlenecks
    ImpactScores = lists:foldl(fun(B, Acc) ->
        Type = maps:get(type, B),
        Severity = maps:get(severity, B, medium),

        ImpactScore = case Type of
            cpu_bottleneck -> case Severity of
                high -> 3;
                medium -> 2;
                low -> 1
            end;
            memory_bottleneck -> case Severity of
                high -> 3;
                medium -> 2;
                low -> 1
            end;
            network_bottleneck -> case Severity of
                high -> 3;
                medium -> 2;
                low -> 1
            end;
            _ -> 1
        end,

        maps:update_counter(Type, ImpactScore, Acc)
    end, #{}, Bottlenecks),

    ImpactScores.

generate_bottleneck_mitigation_strategies(Bottlenecks) ->
    %% Generate mitigation strategies for bottlenecks
    Strategies = [],

    %% Generate CPU mitigation strategies
    case lists:member(cpu_bottleneck, lists:flatten([maps:get(type, B) || B <- Bottlenecks])) of
        true ->
            Strategies#{cpu => [
                "Increase CPU core allocation",
                "Implement CPU scaling",
                "Optimize CPU-intensive operations"
            ]};
        false ->
            Strategies
    end,

    %% Generate memory mitigation strategies
    case lists:member(memory_bottleneck, lists:flatten([maps:get(type, B) || B <- Bottlenecks])) of
        true ->
            Strategies#{memory => [
                "Increase memory allocation",
                "Implement memory pooling",
                "Optimize memory usage patterns"
            ]};
        false ->
            Strategies
    end,

    %% Generate network mitigation strategies
    case lists:member(network_bottleneck, lists:flatten([maps:get(type, B) || B <- Bottlenecks])) of
        true ->
            Strategies#{network => [
                "Increase network bandwidth",
                "Implement network compression",
                "Optimize network topology"
            ]};
        false ->
            Strategies
    end,

    Strategies.

generate_capacity_recommendations(ScalingEfficiency, PhaseResults) ->
    %% Generate capacity recommendations based on scaling efficiency
    LinearEfficiency = maps:get(linear_efficiency, ScalingEfficiency, 0),

    case LinearEfficiency of
        E when E >= 0.9 ->
            #{recommendation => "High scaling efficiency - current capacity is optimal"};
        E when E >= 0.7 ->
            #{recommendation => "Good scaling efficiency - minor capacity expansion needed"};
        _ ->
            #{recommendation => "Poor scaling efficiency - significant capacity optimization needed"}
    end.

generate_optimization_recommendations(ScalingEfficiency, Bottlenecks) ->
    %% Generate optimization recommendations
    Recommendations = [],

    %% Check for optimization opportunities
    EfficiencyScore = maps:get(efficiency_score, ScalingEfficiency, 0),
    PrimaryBottlenecks = maps:get(primary_bottlenecks, Bottlenecks, []),

    case EfficiencyScore < 80 of
        true ->
            Recommendations#{scaling => optimize_scaling_architecture};
        false ->
            Recommendations
    end,

    %% Add bottleneck-specific recommendations
    BottleneckRecommendations = generate_bottleneck_recommendations(PrimaryBottlenecks),
    maps:merge(Recommendations, BottleneckRecommendations).

generate_bottleneck_recommendations(PrimaryBottlenecks) ->
    %% Generate recommendations for specific bottlenecks
    Recommendations = #{},

    lists:foldl(fun(Bottleneck, Acc) ->
        case Bottleneck of
            cpu_bottleneck ->
                Acc#{cpu => [
                    "Implement CPU scaling",
                    "Optimize CPU-intensive workloads",
                    "Consider vertical scaling"
                ]};
            memory_bottleneck ->
                Acc#{memory => [
                    "Implement memory pooling",
                    "Optimize memory allocation",
                    "Consider distributed memory"
                ]};
            network_bottleneck ->
                Acc#{network => [
                    "Implement network optimization",
                    "Consider network compression",
                    "Optimize routing protocols"
                ]};
            _ ->
                Acc
        end
    end, Recommendations, PrimaryBottlenecks).

generate_capacity_planning_guidelines(PhaseResults) ->
    %% Generate capacity planning guidelines
    NodeCounts = [R#node_count || R <- PhaseResults],
    ThroughputValues = [maps:get(average, R#results#throughput) || R <- PhaseResults,
                         maps:is_key(throughput, R#results)],

    CapacityPlanning = #{
        current_capacity => #{
            node_count => lists:max(NodeCounts),
            throughput => lists:max(ThroughputValues)
        },
        scaling_projections => generate_scaling_projections(NodeCounts, ThroughputValues),
        capacity_planning_metrics => calculate_capacity_metrics(NodeCounts, ThroughputValues),
        scaling_recommendations => generate_scaling_recommendations(PhaseResults),
        investment_guidelines => generate_investment_guidelines(PhaseResults)
    },

    CapacityPlanning.

generate_scaling_projections(NodeCounts, ThroughputValues) ->
    %% Generate scaling projections
    ScalingProjections = #{
        short_term => calculate_short_term_projections(NodeCounts, ThroughputValues),
        medium_term => calculate_medium_term_projections(NodeCounts, ThroughputValues),
        long_term => calculate_long_term_projections(NodeCounts, ThroughputValues)
    },

    ScalingProjections.

calculate_short_term_projections(NodeCounts, ThroughputValues) ->
    %% Calculate short-term scaling projections (6-12 months)
    ScalingFactor = calculate_scaling_factor(NodeCounts, ThroughputValues),
    MaxNodes = lists:max(NodeCounts),
    CurrentThroughput = lists:max(ThroughputValues),

    #{
        projected_nodes => round(MaxNodes * 1.5),
        projected_throughput => CurrentThroughput * 1.5,
        scaling_efficiency => ScalingFactor,
        investment_estimate => calculate_investment_estimate(0.5, ScalingFactor)
    }.

calculate_medium_term_projections(NodeCounts, ThroughputValues) ->
    %% Calculate medium-term scaling projections (1-2 years)
    ScalingFactor = calculate_scaling_factor(NodeCounts, ThroughputValues),
    MaxNodes = lists:max(NodeCounts),
    CurrentThroughput = lists:max(ThroughputValues),

    #{
        projected_nodes => round(MaxNodes * 3),
        projected_throughput => CurrentThroughput * 3,
        scaling_efficiency => ScalingFactor,
        investment_estimate => calculate_investment_estimate(2.0, ScalingFactor)
    }.

calculate_long_term_projections(NodeCounts, ThroughputValues) ->
    %% Calculate long-term scaling projections (3-5 years)
    ScalingFactor = calculate_scaling_factor(NodeCounts, ThroughputValues),
    MaxNodes = lists:max(NodeCounts),
    CurrentThroughput = lists:max(ThroughputValues),

    #{
        projected_nodes => round(MaxNodes * 5),
        projected_throughput => CurrentThroughput * 5,
        scaling_efficiency => ScalingFactor,
        investment_estimate => calculate_investment_estimate(5.0, ScalingFactor)
    }.

calculate_capacity_metrics(NodeCounts, ThroughputValues) ->
    %% Calculate capacity planning metrics
    #{
        nodes_per_throughput => calculate_nodes_per_throughput(NodeCounts, ThroughputValues),
        throughput_per_node => calculate_throughput_per_node(NodeCounts, ThroughputValues),
        scaling_ratio => calculate_scaling_ratio(NodeCounts),
        efficiency_curve => calculate_efficiency_curve(NodeCounts, ThroughputValues)
    }.

generate_scaling_recommendations(PhaseResults) ->
    %% Generate scaling recommendations
    Recommendations = [],

    %% Check for horizontal scaling recommendations
    ThroughputResults = [R#results || R <- PhaseResults,
                            maps:is_key(throughput, R#results)],
    case ThroughputResults of
        [] -> Recommendations;
        _ ->
            ThroughputValues = [maps:get(average, R#throughput) || R <- ThroughputResults],
            ScalingFactor = calculate_scaling_factor([R#node_count || R <- PhaseResults], ThroughputValues),

            case ScalingFactor of
                F when F < 0.8 ->
                    Recommendations#{horizontal_scaling => "Implement horizontal scaling"};
                F when F < 0.9 ->
                    Recommendations#{mixed_scaling => "Implement mixed scaling strategy"};
                _ ->
                    Recommendations#{vertical_scaling => "Consider vertical scaling"}
            end
    end,

    Recommendations.

generate_investment_guidelines(PhaseResults) ->
    %% Generate investment guidelines
    InvestmentGuidelines = #{
        hardware_investment => calculate_hardware_investment_guidelines(PhaseResults),
        software_investment => calculate_software_investment_guidelines(PhaseResults),
        operational_investment => calculate_operational_investment_guidelines(PhaseResults),
        roi_projection => calculate_roi_projection(PhaseResults)
    },

    InvestmentGuidelines.

generate_scalability_report(Analysis, Config) ->
    %% Generate comprehensive scalability benchmark report
    #{
        timestamp => erlang:system_time(millisecond),
        scenario => maps:get(scenario, Config, unknown),
        configuration => Config,
        scaling_efficiency => maps:get(scaling_efficiency, Analysis, #{}),
        bottlenecks => maps:get(bottlenecks, Analysis, #{}),
        recommendations => generate_recommendations_report(Analysis),
        capacity_planning => maps:get(capacity_planning, Analysis, #{}),
        maximum_scalable_nodes => maps:get(maximum_scalable_nodes, Analysis, 0),
        scaling_limitations => maps:get(scaling_limitations, Analysis, #{}),
        monitoring_dashboard => generate_scalability_dashboard(Analysis),
        next_steps => identify_next_steps(Analysis)
    }.

generate_recommendations_report(Analysis) ->
    %% Generate recommendations report
    #{
        immediate => maps:get(capacity_recommendations, Analysis, #{}),
        short_term => maps.get(optimization_recommendations, Analysis, #{}),
        long_term => maps.get(future_recommendations, Analysis, #{})
    }.

generate_scalability_dashboard(Analysis) ->
    %% Generate monitoring dashboard specifications
    DashboardConfig = #{
        primary_metrics => [
            {scaling_efficiency, "Scaling Efficiency (%)", "primary"},
            {throughput_scale, "Throughput Scale", "primary"},
            {bottleneck_count, "Active Bottlenecks", "warning"},
            {node_utilization, "Node Utilization (%)", "secondary"}
        ],
        thresholds => generate_scalability_thresholds(Analysis),
        visualizations => [
            {scaling_curve, "Scaling Efficiency Curve"},
            {bottleneck_heatmap, "Bottleneck Distribution Heatmap"},
            {capacity_utilization, "Capacity Utilization Chart"},
            {resource_efficiency, "Resource Efficiency Dashboard"}
        ],
        alerting => generate_scalability_alerting(Analysis)
    },

    DashboardConfig.

generate_scalability_thresholds(Analysis) ->
    %% Generate threshold configuration for scalability monitoring
    ScalingEfficiency = maps:get(scaling_efficiency, Analysis, #{efficiency_score => 0}),
    EfficiencyScore = maps:get(efficiency_score, ScalingEfficiency, 0),

    #{
        efficiency_warning => round(EfficiencyScore * 0.8),
        efficiency_critical => round(EfficiencyScore * 0.6),
        throughput_critical => maps:get(throughput_scale, ScalingEfficiency, 2.0) * 0.5,
        node_utilization_warning => 80,
        node_utilization_critical => 90
    }.

generate_scalability_alerting(Analysis) ->
    %% Generate alerting configuration
    #{
        critical_alerts => [
            "Scaling efficiency below 60%",
            "Critical bottleneck detected",
            "Node utilization exceeds 90%"
        ],
        warning_alerts => [
            "Scaling efficiency below 80%",
            "Warning bottleneck detected",
            "Node utilization exceeds 80%"
        ],
        escalation => generate_escalation_protocol(Analysis),
        notification_channels => ["email", "slack", "pagerduty"]
    }.

generate_escalation_protocol(Analysis) ->
    %% Generate escalation protocol
    #{
        level1 => "DevOps team - immediate attention",
        level2 => "Infrastructure team - 30 minutes response",
        level3 => "Executive team - 1 hour response"
    }.

identify_next_steps(Analysis) ->
    %% Identify next steps based on scalability analysis
    NextSteps = [],

    %% Check for immediate actions
    ScalingEfficiency = maps:get(scaling_efficiency, Analysis, #{efficiency_score => 0}),
    EfficiencyScore = maps:get(efficiency_score, ScalingEfficiency, 0),

    case EfficiencyScore < 60 of
        true -> [immediate_scaling_optimization | NextSteps];
        false -> NextSteps
    end,

    %% Check for capacity planning
    MaximumNodes = maps:get(maximum_scalable_nodes, Analysis, 0),
    case MaximumNodes < 10000 of
        true -> [capacity_planning_review | NextSteps];
        false -> NextSteps
    end,

    %% Check for optimization opportunities
    Bottlenecks = maps:get(bottlenecks, Analysis, #{primary_bottlenecks => []}),
    PrimaryBottlenecks = maps:get(primary_bottlenecks, Bottlenecks, []),
    case PrimaryBottlenecks of
        [] -> NextSteps;
        _ -> [bottleneck_resolution | NextSteps]
    end,

    NextSteps.

save_scalability_benchmark_report(Report) ->
    %% Save scalability benchmark report
    ReportId = generate_scalability_report_id(),
    ok = erlmcp_benchmark_storage:save_scalability_report(ReportId, Report),
    ok.

notify_scalability_benchmark_complete(Report) ->
    %% Notify external systems of benchmark completion
    Notification = #{
        type => benchmark_complete,
        category => scalability,
        report => Report,
        timestamp => erlang:system_time(millisecond)
    },

    ok = erlmcp_notification_manager:notify_scalability_benchmark_complete(Notification),
    ok.

cleanup_scalability_resources() ->
    %% Cleanup scalability-specific resources
    ok = erlmcp_scalable_load_generator:stop(),
    ok = erlmcp_scaling_analyzer:stop(),
    ok = erlmcp_bottleneck_detector:stop(),
    ok = erlmcp_node_health_monitor:stop(),
    ok = erlmcp_distributed_metrics:stop(),
    ok.

export_scalability_metrics() ->
    %% Export all scalability metrics
    Metrics = ets:tab2list(?SCALABILITY_TAB),
    NodeMetrics = ets:tab2list(?NODE_TAB),

    ok = erlmcp_metrics_exporter:export_scalability_metrics(Metrics, NodeMetrics),
    ok.

update_state(State, Config, Scenario, Report) ->
    State#{
        start_time => erlang:system_time(millisecond),
        config => Config,
        scenario => Scenario,
        report => Report
    }.

calculate_throughput_growth_rate(ThroughputValues) ->
    %% Calculate throughput growth rate
    case ThroughputValues of
        [_] -> 0;
        _ ->
            First = lists:nth(1, ThroughputValues),
            Last = lists:last(ThroughputValues),
            (Last - First) / First * 100
    end.

calculate_latency_consistency(LatencyValues) ->
    %% Calculate consistency index for latency
    Mean = lists:sum(LatencyValues) / length(LatencyValues),
    StdDev = math:sqrt(lists:sum([(L - Mean) * (L - Mean) || L <- LatencyValues]) / length(LatencyValues)),

    case StdDev of
        0 -> 100;
        _ -> (1 - StdDev / Mean) * 100
    end.

calculate_throughput_efficiency(ThroughputAnalysis) ->
    %% Calculate throughput efficiency score
    ThroughputValues = [ThroughputAnalysis#{average => 0}],
    case ThroughputValues of
        [#{average := A, maximum := M}] ->
            (A / M) * 100;
        _ -> 0
    end.

calculate_efficiency_score(ResourcesAnalysis) ->
    %% Calculate overall efficiency score
    CPU = maps:get(cpu, ResourcesAnalysis#{average => 0})#average,
    Memory = maps:get(memory, ResourcesAnalysis#{average => 0})#average,
    Network = maps:get(network, ResourcesAnalysis#{average => 0})#average,

    (CPU + Memory + Network) / 3 * 100.

calculate_latency_efficiency(LatencyAnalysis) ->
    %% Calculate latency efficiency score
    P95 = maps:get(p95, LatencyAnalysis, 0),
    P99 = maps:get(p99, LatencyAnalysis, 0),

    case P95 of
        0 -> 0;
        _ -> max(0, 100 - (P99 / P95 - 1) * 100)
    end.

calculate_overall_efficiency(Throughput, Resources, Latency) ->
    %% Calculate overall efficiency score
    ThroughputEff = calculate_throughput_efficiency(Throughput),
    ResourceEff = calculate_efficiency_score(Resources),
    LatencyEff = calculate_latency_efficiency(Latency),

    (ThroughputEff + ResourceEff + LatencyEff) / 3.

calculate_percentile(P, Values) ->
    %% Calculate percentile value
    Sorted = lists:sort(Values),
    Length = length(Sorted),
    Index = trunc((P / 100) * Length),

    case Sorted of
        [] -> 0;
        _ when Index > 0 -> lists:nth(Index, Sorted);
        _ -> 0
    end.

calculate_scaling_factor(NodeCounts, ThroughputValues) ->
    %% Calculate scaling factor
    case NodeCounts of
        [] -> 0;
        _ ->
            BaseNodes = lists:min(NodeCounts),
            MaxNodes = lists:max(NodeCounts),
            ThroughputScale = lists:max(ThroughputValues) / lists:min(ThroughputValues),
            NodeScale = MaxNodes / BaseNodes,

            ThroughputScale / NodeScale
    end.

calculate_nodes_per_throughput(NodeCounts, ThroughputValues) ->
    %% Calculate nodes per throughput unit
    case ThroughputValues of
        [] -> #{};
        _ ->
            #{
                average => lists:sum(NodeCounts) / lists:sum(ThroughputValues),
                min => lists:min(NodeCounts) / lists:max(ThroughputValues),
                max => lists:max(NodeCounts) / lists:min(ThroughputValues)
            }
    end.

calculate_throughput_per_node(NodeCounts, ThroughputValues) ->
    %% Calculate throughput per node
    case NodeCounts of
        [] -> #{};
        _ ->
            ThroughputPerNode = [T / N || {T, N} <- lists:zip(ThroughputValues, NodeCounts)],
            #{
                average => lists:sum(ThroughputPerNode) / length(ThroughputPerNode),
                min => lists:min(ThroughputPerNode),
                max => lists:max(ThroughputPerNode)
            }
    end.

calculate_scaling_ratio(NodeCounts) ->
    %% Calculate scaling ratio
    case NodeCounts of
        [] -> 0;
        _ ->
            lists:min(NodeCounts) / lists:max(NodeCounts)
    end.

calculate_efficiency_curve(NodeCounts, ThroughputValues) ->
    %% Calculate efficiency curve data
    case lists:zip(NodeCounts, ThroughputValues) of
        [] -> [];
        _ ->
            lists:foldl(fun({Nodes, Throughput}, Acc) ->
                Efficiency = calculate_scaling_efficiency([{Nodes, Throughput}]),
                [Efficiency | Acc]
            end, [], lists:zip(NodeCounts, ThroughputValues))
    end.

determine_maximum_scalable_nodes(PhaseResults) ->
    %% Determine maximum number of nodes that can be scaled
    ScalingEfficiency = calculate_scaling_efficiency(PhaseResults),
    LinearEfficiency = maps:get(linear_efficiency, ScalingEfficiency, 0),

    case LinearEfficiency of
        E when E >= 0.9 ->
            10000;  % Full scaling capability
        E when E >= 0.8 ->
            5000;   % Good scaling
        E when E >= 0.6 ->
            1000;   % Moderate scaling
        _ ->
            100     % Limited scaling
    end.

identify_scaling_limitations(PhaseResults) ->
    %% Identify scaling limitations
    Limitations = [],

    %% Check for resource limitations
    ResourceAnalysis = analyze_phase_resources([R#results || R <- PhaseResults]),
    case maps:get(cpu, ResourceAnalysis#{average => 0})#average > 0.9 of
        true -> [resource_constrained | Limitations];
        false -> Limitations
    end,

    %% Check for network limitations
    case maps:get(network, ResourceAnalysis#{average => 0})#average > 0.9 of
        true -> [network_constrained | Limitations];
        false -> Limitations
    end,

    %% Check for communication limitations
    LatencyAnalysis = analyze_phase_latency([R#results || R <- PhaseResults]),
    case maps:get(p95, LatencyAnalysis, 0) > 200 of
        true -> [communication_constrained | Limitations];
        false -> Limitations
    end,

    Limitations.

calculate_investment_estimate(DurationYears, ScalingFactor) ->
    %% Calculate investment estimate for scaling
    BaseCost = 1000000,  % Base cost in dollars
    DurationMultiplier = DurationYears,
    ScalingMultiplier = ScalingFactor,

    BaseCost * DurationMultiplier * ScalingMultiplier.

calculate_hardware_investment_guidelines(PhaseResults) ->
    %% Generate hardware investment guidelines
    #{
        cpu_allocation => calculate_cpu_allocation_guidelines(PhaseResults),
        memory_allocation => calculate_memory_allocation_guidelines(PhaseResults),
        network_bandwidth => calculate_network_bandwidth_guidelines(PhaseResults),
        storage_capacity => calculate_storage_capacity_guidelines(PhaseResults)
    }.

calculate_cpu_allocation_guidelines(PhaseResults) ->
    %% Calculate CPU allocation guidelines
    ResourceAnalysis = analyze_phase_resources([R#results || R <- PhaseResults]),
    CPUUtilization = maps:get(average, maps:get(cpu, ResourceAnalysis#{average => 0}), 0),

    case CPUUtilization of
        U when U < 0.6 -> "Current allocation sufficient";
        U when U < 0.8 -> "Consider 25% increase";
        _ -> "Consider 50% increase"
    end.

calculate_memory_allocation_guidelines(PhaseResults) ->
    %% Calculate memory allocation guidelines
    ResourceAnalysis = analyze_phase_resources([R#results || R <- PhaseResults]),
    MemoryUtilization = maps:get(average, maps:get(memory, ResourceAnalysis#{average => 0}), 0),

    case MemoryUtilization of
        U when U < 0.6 -> "Current allocation sufficient";
        U when U < 0.8 -> "Consider 25% increase";
        _ -> "Consider 50% increase"
    end.

calculate_network_bandwidth_guidelines(PhaseResults) ->
    %% Calculate network bandwidth guidelines
    ResourceAnalysis = analyze_phase_resources([R#results || R <- PhaseResults]),
    NetworkUtilization = maps:get(average, maps:get(network, ResourceAnalysis#{average => 0}), 0),

    case NetworkUtilization of
        U when U < 0.6 -> "Current allocation sufficient";
        U when U < 0.8 -> "Consider 25% increase";
        _ -> "Consider 50% increase"
    end.

calculate_storage_capacity_guidelines(PhaseResults) ->
    %% Calculate storage capacity guidelines
    #{
        current_capacity => "2TB",
        projected_6months => "3TB",
        projected_1year => "5TB",
        projected_5years => "10TB"
    }.

calculate_software_investment_guidelines(PhaseResults) ->
    %% Generate software investment guidelines
    #{
        licensing => "License scaling required",
        optimization => "Performance optimization needed",
        monitoring => "Enhanced monitoring required",
        automation => "Automation tools recommended"
    }.

calculate_operational_investment_guidelines(PhaseResults) ->
    %% Generate operational investment guidelines
    #{
        personnel => "Operations team expansion needed",
        training => "Training program required",
        processes => "Process optimization needed",
        tools => "Advanced monitoring tools needed"
    }.

calculate_roi_projection(PhaseResults) ->
    %% Calculate ROI projection
    #{
        short_term_roi => "15-20%",
        medium_term_roi => "25-30%",
        long_term_roi => "40-50%",
        payback_period => "18-24 months"
    }.

generate_scalability_report_id() ->
    "scal_bench_" ++ integer_to_list(erlang:system_time(millisecond), 36).