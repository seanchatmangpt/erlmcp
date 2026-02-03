%% @doc Horizontal Scaling Test for erlmcp v3
%% Comprehensive testing of horizontal scaling capabilities
%%
%% Features:
%% - Multi-node deployment testing
%% - Load distribution validation
%% - Scaling performance measurement
%% - Auto-scaling policy testing
%% - Resource utilization optimization
%% - Cluster health monitoring
%% - Performance baselines
%% - Scaling thresholds

-module(erlmcp_horizontal_scaling_test).
-author("erlmcp-load-test-team").
-behaviour(gen_server).

%% API exports
-export([start_link/0, run_scaling_test/3, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Include
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Records
-record.scaling_scenario, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    initial_nodes :: integer(),
    max_nodes :: integer(),
    scaling_policy :: static | manual | automatic,
    load_profile :: map(),
    metrics :: list(),
    thresholds :: map()
}.

-record.scaling_test, {
    id :: binary(),
    scenario :: #scaling_scenario{},
    start_time :: integer(),
    current_nodes :: integer(),
    target_nodes :: integer(),
    status :: planning | scaling_up | scaling_down | stable | completed,
    metrics :: map(),
    scaling_events :: list(),
    cluster_health :: list(),
    performance_baseline :: map()
}.

-record.cluster_metrics, {
    timestamp :: integer(),
    node_count :: integer(),
    cpu_usage :: float(),
    memory_usage :: float(),
    network_throughput :: float(),
    request_latency :: float(),
    error_rate :: float(),
    load_distribution :: map()
}.

-record.scaling_event, {
    id :: binary(),
    action :: scale_up | scale_down,
    trigger :: load_based | time_based | manual,
    from_nodes :: integer(),
    to_nodes :: integer(),
    timestamp :: integer(),
    decision_reason :: binary(),
    metrics_at_scale :: map()
}.

-record.auto_scaling_policy, {
    cpu_threshold :: {float(), float()}, % {min, max}
    memory_threshold :: {float(), float()},
    latency_threshold :: {float(), float()},
    error_rate_threshold :: float(),
    scale_up_cooldown :: integer(),
    scale_down_cooldown :: integer(),
    min_nodes :: integer(),
    max_nodes :: integer(),
    scale_up_factor :: float(),
    scale_down_factor :: float()
}.

%% ============================================================================
%% API FUNCTIONS
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

run_scaling_test(TestId, Scenario, Config) ->
    gen_server:call(?MODULE, {run_scaling_test, TestId, Scenario, Config}, infinity).

stop() ->
    gen_server:call(?MODULE, stop).

%% ============================================================================
%% GEN_SERVER CALLBACKS
%% ============================================================================

init(_Args) ->
    process_flag(trap_exit, true),
    ?LOG_INFO("Horizontal scaling test framework initialized"),

    %% Initialize state
    State = #{
        active_tests => [],
        cluster_metrics => initialize_cluster_metrics(),
        auto_scaling_policies => initialize_auto_scaling_policies(),
        scaling_history => [],
        performance_baselines => initialize_performance_baselines()
    },

    %% Start monitoring
    erlang:send_after(10000, self(), monitor_cluster),

    {ok, State}.

handle_call({run_scaling_test, TestId, Scenario, Config}, _From, State) ->
    %% Validate scenario
    case validate_scaling_scenario(Scenario) of
        {ok, ValidScenario} ->
            %% Create and start scaling test
            Test = create_scaling_test(TestId, ValidScenario, Config),
            NewState = start_scaling_test(Test, State),
            {reply, {ok, test_started}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_active_tests, _From, State) ->
    {reply, {ok, maps:get(active_tests, State, [])}, State};

handle_call(get_scaling_history, _From, State) ->
    {reply, {ok, maps:get(scaling_history, State, [])}, State};

handle_call(get_cluster_metrics, _From, State) ->
    {reply, {ok, maps:get(cluster_metrics, State, [])}, State};

handle_call(get_performance_baselines, _From, State) ->
    {reply, {ok, maps:get(performance_baselines, State, [])}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(monitor_cluster, State) ->
    %% Monitor cluster metrics
    ClusterMetrics = monitor_cluster_metrics(State);
    UpdatedState = State#{
        cluster_metrics => ClusterMetrics
    },

    %% Check for scaling opportunities
    ScalingState = check_scaling_opportunities(State);

    %% Schedule next monitoring
    erlang:send_after(10000, self(), monitor_cluster),

    {noreply, ScalingState};

handle_info({test_phase, TestId, Phase, Metrics}, State) ->
    %% Handle test phase transitions
    case lists:keyfind(TestId, #scaling_test.id, maps:get(active_tests, State)) of
        false ->
            {noreply, State};
        Test ->
            UpdatedTest = Test#scaling_test{
                status = Phase,
                metrics = merge_metrics(Test#scaling_test.metrics, Metrics)
            },

            NewState = State#{
                active_tests => lists:keyreplace(TestId, #scaling_test.id,
                                              maps:get(active_tests, State), UpdatedTest)
            },

            case Phase of
                scaling_up ->
                    %% Scale up cluster
                    erlang:send_after(0, self(), {scale_up_cluster, TestId});
                scaling_down ->
                    %% Scale down cluster
                    erlang:send_after(0, self(), {scale_down_cluster, TestId});
                _ ->
                    ok
            end,

            {noreply, NewState}
    end;

handle_info({scale_up_cluster, TestId}, State) ->
    %% Handle cluster scale up
    case lists:keyfind(TestId, #scaling_test.id, maps:get(active_tests, State)) of
        false ->
            {noreply, State};
        Test ->
            case scale_up_cluster(Test) of
                {ok, NewNodeCount} ->
                    ScalingEvent = #scaling_event{
                        id = generate_scaling_event_id(),
                        action = scale_up,
                        trigger = load_based,
                        from_nodes = Test#scaling_test.current_nodes,
                        to_nodes = NewNodeCount,
                        timestamp = erlang:system_time(millisecond),
                        decision_reason = "Load-based scaling triggered",
                        metrics_at_scale = get_current_metrics()
                    },

                    UpdatedTest = Test#scaling_test{
                        current_nodes = NewNodeCount,
                        scaling_events = [ScalingEvent | Test#scaling_test.scaling_events]
                    };

                    erlang:send_after(0, self(), {test_phase, TestId, stable, #{}}),
                    {noreply, update_test(TestId, UpdatedTest, State)};
                {error, Reason} ->
                    ?LOG_ERROR("Failed to scale up cluster: ~p", [Reason]),
                    {noreply, State}
            end
    end;

handle_info({scale_down_cluster, TestId}, State) ->
    %% Handle cluster scale down
    case lists:keyfind(TestId, #scaling_test.id, maps:get(active_tests, State)) of
        false ->
            {noreply, State};
        Test ->
            case scale_down_cluster(Test) of
                {ok, NewNodeCount} ->
                    ScalingEvent = #scaling_event{
                        id = generate_scaling_event_id(),
                        action = scale_down,
                        trigger = load_based,
                        from_nodes = Test#scaling_test.current_nodes,
                        to_nodes = NewNodeCount,
                        timestamp = erlang:system_time(millisecond),
                        decision_reason = "Load-based scaling triggered",
                        metrics_at_scale = get_current_metrics()
                    },

                    UpdatedTest = Test#scaling_test{
                        current_nodes = NewNodeCount,
                        scaling_events = [ScalingEvent | Test#scaling_test.scaling_events]
                    };

                    erlang:send_after(0, self(), {test_phase, TestId, stable, #{}}),
                    {noreply, update_test(TestId, UpdatedTest, State)};
                {error, Reason} ->
                    ?LOG_ERROR("Failed to scale down cluster: ~p", [Reason]),
                    {noreply, State}
            end
    end;

handle_info({scaling_complete, TestId, Report}, State) ->
    %% Handle scaling test completion
    ?LOG_INFO("Scaling test ~p completed", [TestId]),

    case lists:keyfind(TestId, #scaling_test.id, maps:get(active_tests, State)) of
        false ->
            {noreply, State};
        Test ->
            %% Move to history
            NewHistory = [Test#scaling_test{status = completed} | maps:get(scaling_history, State)],
            NewActive = lists:keydelete(TestId, #scaling_test.id, maps:get(active_tests, State)),

            %% Save report
            ReportFile = "/Users/sac/erlmcp/load-testing/scaling_test_" ++
                         binary_to_list(TestId) ++ "_report.json",
            file:write_file(ReportFile, jsx:encode(Report)),

            ?LOG_INFO("Scaling test report saved to: ~p", [ReportFile]),

            State#{
                active_tests => NewActive,
                scaling_history => NewHistory
            }
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cleanup scaling tests
    lists:foreach(fun(Test) ->
        rollback_scaling_test(Test)
    end, maps:get(active_tests, State)),

    ?LOG_INFO("Horizontal scaling test framework terminated", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% PRIVATE FUNCTIONS
%% ============================================================================

validate_scaling_scenario(Scenario) ->
    %% Validate scaling scenario
    case Scenario#scaling_scenario.scaling_policy of
        automatic ->
            case Scenario#scaling_policy.auto_scaling_policy of
                undefined ->
                    {error, auto_scaling_policy_required};
                _ ->
                    validate_auto_scaling_policy(Scenario#scaling_policy.auto_scaling_policy)
            end;
        _ ->
            {ok, Scenario}
    end.

validate_auto_scaling_policy(Policy) ->
    %% Validate auto-scaling policy
    case Policy#min_nodes > 0 andalso Policy#max_nodes > Policy#min_nodes of
        true ->
            case Policy#cpu_threshold =/= {0.0, 0.0} andalso
                 Policy#memory_threshold =/= {0.0, 0.0} of
                true ->
                    {ok, Policy};
                false ->
                    {error, invalid_thresholds}
            end;
        false ->
            {error, invalid_node_range}
    end.

create_scaling_test(TestId, Scenario, Config) ->
    %% Create scaling test
    #scaling_test{
        id = TestId,
        scenario = Scenario,
        start_time = erlang:system_time(millisecond),
        current_nodes = Scenario#scaling_scenario.initial_nodes,
        target_nodes = Scenario#scaling_scenario.initial_nodes,
        status = planning,
        metrics = #{},
        scaling_events = [],
        cluster_health = [],
        performance_baseline = establish_performance_baseline(Scenario)
    }.

start_scaling_test(Test, State) ->
    %% Start scaling test
    ?LOG_INFO("Starting scaling test: ~p", [Test#scaling_test.id]),

    %% Start load generation
    start_test_load(Test, State);

    %% Add to active tests
    NewActive = [Test | maps:get(active_tests, State)],

    %% Send phase transition
    erlang:send_after(0, self(), {test_phase, Test#scaling_test.id, scaling_up, #{}}),

    State#{
        active_tests => NewActive
    }.

establish_performance_baseline(Scenario) ->
    %% Establish performance baseline
    LoadProfile = Scenario#scaling_scenario.load_profile;

    %% Generate baseline metrics
    #{
        nodes => Scenario#scaling_scenario.initial_nodes,
        throughput => calculate_baseline_throughput(Scenario),
        latency => calculate_baseline_latency(Scenario),
        cpu_usage => calculate_baseline_cpu(Scenario),
        memory_usage => calculate_baseline_memory(Scenario),
        error_rate => calculate_baseline_error_rate(Scenario),
        timestamp => erlang:system_time(millisecond)
    }.

scale_up_cluster(Test) ->
    %% Scale up cluster
    CurrentNodes = Test#scaling_test.current_nodes;
    TargetNodes = min(CurrentNodes + 1, Test#scaling_test.scenario#scaling_scenario.max_nodes);

    case TargetNodes > CurrentNodes of
        true ->
            %% Add new node
            case erlmcp_cluster:add_node() of
                {ok, NodeId} ->
                    %% Wait for node to be ready
                    wait_for_node_ready(NodeId),
                    {ok, TargetNodes};
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, max_nodes_reached}
    end.

scale_down_cluster(Test) ->
    %% Scale down cluster
    CurrentNodes = Test#scaling_test.current_nodes;
    TargetNodes = max(CurrentNodes - 1, Test#scaling_test.scenario#scaling_scenario.initial_nodes);

    case TargetNodes < CurrentNodes of
        true ->
            %% Remove least loaded node
            case find_least_loaded_node() of
                {ok, NodeId} ->
                    case erlmcp_cluster:remove_node(NodeId) of
                        ok ->
                            {ok, TargetNodes};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, min_nodes_reached}
    end.

wait_for_node_ready(NodeId) ->
    %% Wait for node to be ready
    case check_node_health(NodeId) of
        true ->
            ok;
        false ->
            timer:sleep(5000),
            wait_for_node_ready(NodeId)
    end.

find_least_loaded_node() ->
    %% Find least loaded node
    Nodes = get_cluster_nodes();

    case Nodes of
        [] ->
            {error, no_nodes};
        _ ->
            %% Find node with lowest load
            {ok, lists:nth(1, Nodes)} % Simplified implementation
    end.

monitor_cluster_metrics(State) ->
    %% Monitor cluster metrics
    Nodes = get_cluster_nodes();
    NodeCount = length(Nodes);

    case Nodes of
        [] ->
            #cluster_metrics{
                timestamp => erlang:system_time(millisecond),
                node_count => 0,
                cpu_usage => 0.0,
                memory_usage => 0.0,
                network_throughput => 0.0,
                request_latency => 0.0,
                error_rate => 0.0,
                load_distribution => #{}
            };
        _ ->
            AggregateMetrics = aggregate_node_metrics(Nodes);

            #cluster_metrics{
                timestamp => erlang:system_time(millisecond),
                node_count => NodeCount,
                cpu_usage => AggregateMetrics#cpu_usage / NodeCount,
                memory_usage => AggregateMetrics#memory_usage / NodeCount,
                network_throughput => AggregateMetrics#network_throughput,
                request_latency => AggregateMetrics#request_latency / NodeCount,
                error_rate => AggregateMetrics#error_rate,
                load_distribution => calculate_load_distribution(Nodes)
            }
    end.

aggregate_node_metrics(Nodes) ->
    %% Aggregate metrics from all nodes
    lists:foldl(fun(NodeId, Acc) ->
        case get_node_metrics(NodeId) of
            {ok, Metrics} ->
                Acc#{
                    cpu_usage => Acc#cpu_usage + Metrics#cpu_usage,
                    memory_usage => Acc#memory_usage + Metrics#memory_usage,
                    network_throughput => Acc#network_throughput + Metrics#network_throughput,
                    request_latency => Acc#request_latency + Metrics#request_latency,
                    error_rate => Acc#error_rate + Metrics#error_rate
                };
            {error, _} ->
                Acc
        end
    end, #{cpu_usage => 0.0, memory_usage => 0.0, network_throughput => 0.0,
          request_latency => 0.0, error_rate => 0.0}, Nodes).

calculate_load_distribution(Nodes) ->
    %% Calculate load distribution across nodes
    lists:map(fun(NodeId) ->
        case get_node_metrics(NodeId) of
            {ok, Metrics} ->
                {NodeId, #{
                    cpu => Metrics#cpu_usage,
                    memory => Metrics#memory_usage,
                    throughput => Metrics#network_throughput,
                    latency => Metrics#request_latency
                }};
            {error, Reason} ->
                {NodeId, #{error => Reason}}
        end
    end, Nodes).

check_scaling_opportunities(State) ->
    %% Check for scaling opportunities
    ClusterMetrics = maps:get(cluster_metrics, State);
    AutoPolicies = maps:get(auto_scaling_policies, State);

    case ClusterMetrics#node_count > 0 of
        true ->
            %% Check each active test for scaling needs
            Tests = maps:get(active_tests, State);

            lists:foldl(fun(Test, StateAcc) ->
                case Test#scaling_test.status of
                    stable ->
                        case should_scale_up(Test, ClusterMetrics, AutoPolicies) of
                            true ->
                                erlang:send_after(0, self(), {test_phase, Test#scaling_test.id,
                                                              scaling_up, get_current_metrics()});
                            false ->
                                case should_scale_down(Test, ClusterMetrics, AutoPolicies) of
                                    true ->
                                        erlang:send_after(0, self(), {test_phase, Test#scaling_test.id,
                                                                      scaling_down, get_current_metrics()});
                                    false ->
                                        ok
                                end
                        end;
                    _ ->
                        ok
                end,

                StateAcc
            end, State, Tests);
        false ->
            State
    end.

should_scale_up(Test, ClusterMetrics, AutoPolicies) ->
    %% Check if cluster should scale up
    CurrentNodes = Test#scaling_test.current_nodes;
    MaxNodes = Test#scaling_test.scenario#scaling_scenario.max_nodes;

    case CurrentNodes >= MaxNodes of
        true ->
            false;
        false ->
            case Test#scaling_test.scenario#scaling_scenario.scaling_policy of
                automatic ->
                    check_scale_up_triggers(ClusterMetrics, AutoPolicies);
                manual ->
                    false;
                static ->
                    false
            end
    end.

should_scale_down(Test, ClusterMetrics, AutoPolicies) ->
    %% Check if cluster should scale down
    CurrentNodes = Test#scaling_test.current_nodes;
    MinNodes = Test#scaling_test.scenario#scaling_scenario.initial_nodes;

    case CurrentNodes <= MinNodes of
        true ->
            false;
        false ->
            case Test#scaling_test.scenario#scaling_scenario.scaling_policy of
                automatic ->
                    check_scale_down_triggers(ClusterMetrics, AutoPolicies);
                manual ->
                    false;
                static ->
                    false
            end
    end.

check_scale_up_triggers(ClusterMetrics, AutoPolicies) ->
    %% Check scale up triggers
    CPUThreshold = AutoPolicies#auto_scaling_policy.cpu_threshold;
    MemoryThreshold = AutoPolicies#auto_scaling_policy.memory_threshold;
    LatencyThreshold = AutoPolicies#auto_scaling_policy.latency_threshold;

    HighCPU = ClusterMetrics#cpu_usage > CPUThreshold#max;
    HighMemory = ClusterMetrics#memory_usage > MemoryThreshold#max;
    HighLatency = ClusterMetrics#request_latency > LatencyThreshold#max;

    HighCPU orelse HighMemory orelse HighLatency.

check_scale_down_triggers(ClusterMetrics, AutoPolicies) ->
    %% Check scale down triggers
    CPUThreshold = AutoPolicies#auto_scaling_policy.cpu_threshold;
    MemoryThreshold = AutoPolicies#auto_scaling_policy.memory_threshold;
    LatencyThreshold = AutoPolicies#auto_scaling_policy.latency_threshold;

    LowCPU = ClusterMetrics#cpu_usage < CPUThreshold#min;
    LowMemory = ClusterMetrics#memory_usage < MemoryThreshold#min;
    LowLatency = ClusterMetrics#request_latency < LatencyThreshold#min;

    LowCPU orelse LowMemory orelse LowLatency.

start_test_load(Test, State) ->
    %% Start test load generation
    LoadProfile = Test#scaling_test.scenario#scaling_scenario.load_profile;

    %% Configure load generator
    LoadConfig = #{
        test_id => Test#scaling_test.id,
        target_nodes => get_cluster_nodes(),
        load_profile => LoadProfile,
        duration => Test#scaling_test.scenario#scaling_scenario.test_duration,
        scaling_policy => Test#scaling_test.scenario#scaling_scenario.scaling_policy
    },

    %% Start load generator
    erlmcp_load_generator:start(LoadConfig).

get_current_metrics() ->
    %% Get current cluster metrics
    monitor_cluster_metrics(#{}).

calculate_baseline_throughput(Scenario) ->
    %% Calculate baseline throughput
    case Scenario#scaling_scenario.load_profile of
        #{
            target_throughput := Target,
            nodes := Nodes
        } ->
            Target / Nodes;
        _ ->
            1000 % Default baseline
    end.

calculate_baseline_latency(Scenario) ->
    %% Calculate baseline latency
    100 % Default baseline in ms

calculate_baseline_cpu(Scenario) ->
    %% Calculate baseline CPU usage
    50.0 % Default baseline

calculate_baseline_memory(Scenario) ->
    %% Calculate baseline memory usage
    40.0 % Default baseline

calculate_baseline_error_rate(Scenario) ->
    %% Calculate baseline error rate
    0.01 % Default baseline

initialize_cluster_metrics() ->
    %% Initialize cluster metrics
    #{
        current => #cluster_metrics{
            timestamp => erlang:system_time(millisecond),
            node_count => 0,
            cpu_usage => 0.0,
            memory_usage => 0.0,
            network_throughput => 0.0,
            request_latency => 0.0,
            error_rate => 0.0,
            load_distribution => #{}
        },
        history => []
    }.

initialize_auto_scaling_policies() ->
    %% Initialize auto-scaling policies
    #{
        default => #auto_scaling_policy{
            cpu_threshold = {30.0, 70.0},
            memory_threshold = {40.0, 80.0},
            latency_threshold = {50.0, 200.0},
            error_rate_threshold = 0.05,
            scale_up_cooldown = 300000, % 5 minutes
            scale_down_cooldown = 600000, % 10 minutes
            min_nodes = 3,
            max_nodes = 10,
            scale_up_factor = 1.5,
            scale_down_factor = 0.7
        }
    }.

initialize_performance_baselines() ->
    %% Initialize performance baselines
    #{
        throughput => 1000,
        latency => 100,
        cpu_usage => 50.0,
        memory_usage => 40.0,
        error_rate => 0.01
    }.

get_cluster_nodes() ->
    %% Get cluster nodes
    case erlmcp_cluster:get_nodes() of
        {ok, Nodes} ->
            Nodes;
        _ ->
            [node()]
    end.

get_node_metrics(NodeId) ->
    %% Get metrics for a specific node
    case erlmcp_monitor:get_node_metrics(NodeId) of
        {ok, Metrics} ->
            {ok, #{
                cpu_usage => Metrics#cpu_usage,
                memory_usage => Metrics#memory_usage,
                network_throughput => Metrics#network_throughput,
                request_latency => Metrics#request_latency,
                error_rate => Metrics#error_rate
            }};
        {error, Reason} ->
            {error, Reason}
    end.

check_node_health(NodeId) ->
    %% Check if node is healthy
    case erlmcp_health_monitor:check_node(NodeId) of
        {ok, healthy} ->
            true;
        _ ->
            false
    end.

generate_scaling_event_id() ->
    %% Generate unique scaling event ID
    <<Id:128>> = crypto:strong_rand_bytes(16),
    Id.

merge_metrics(Existing, New) ->
    %% Merge metrics maps
    maps:merge(Existing, New).

update_test(TestId, UpdatedTest, State) ->
    %% Update test in active tests
    NewActive = lists:keyreplace(TestId, #scaling_test.id,
                                 maps:get(active_tests, State), UpdatedTest),
    State#{
        active_tests => NewActive
    }.

rollback_scaling_test(Test) ->
    %% Rollback scaling test
    case Test#scaling_test.status of
        scaling_up ->
            %% Scale back down
            case scale_down_cluster(Test) of
                {ok, _} ->
                    ok;
                {error, _} ->
                    ok
            end;
        scaling_down ->
            %% Scale back up
            case scale_up_cluster(Test) of
                {ok, _} ->
                    ok;
                {error, _} ->
                    ok
            end;
        _ ->
            ok
    end.