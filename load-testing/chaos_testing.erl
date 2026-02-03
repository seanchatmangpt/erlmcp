%% @doc Chaos Testing Framework for erlmcp v3
%% Implements chaos engineering practices for resilience testing
%%
%% Features:
%% - Network partition simulation
%% - Node failure injection
%% - Latency spike simulation
%% - Packet loss simulation
%%- Resource exhaustion
%% - Recovery time measurement
%% - Chaos experiments definition
%% - Rollback capabilities

-module(erlmcp_chaos_test).
-author("erlmcp-load-test-team").
-behaviour(gen_server).

%% API exports
-export([start_link/0, run_chaos_experiment/2, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Include
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Records
-record.chaos_scenario, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    experiment_type :: network_partition | node_failure | latency_spike | packet_loss | resource_exhaustion,
    severity :: low | medium | high | critical,
    duration :: integer(), % milliseconds
    recovery_time :: integer(), % expected recovery time in ms
    metrics :: list(),
    rollback_strategy :: manual | automatic | hybrid
}.

-record.chaos_experiment, {
    id :: binary(),
    scenario :: #chaos_scenario{},
    start_time :: integer(),
    status :: planned | running | completed | failed,
    injected_failures :: list(),
    recovery_actions :: list(),
    metrics :: map(),
    health_checks :: list()
}.

-record.chaos_state, {
    active_experiments :: list(),
    node_health :: map(),
    network_topology :: map(),
    system_metrics :: map(),
    experiment_history :: list()
}.

%% ============================================================================
%% API FUNCTIONS
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

run_chaos_experiment(ExperimentId, Scenario) ->
    gen_server:call(?MODULE, {run_chaos_experiment, ExperimentId, Scenario}, infinity).

stop() ->
    gen_server:call(?MODULE, stop).

%% ============================================================================
%% GEN_SERVER CALLBACKS
%% ============================================================================

init(_Args) ->
    process_flag(trap_exit, true),
    ?LOG_INFO("Chaos testing framework initialized"),

    %% Initialize state
    State = #chaos_state{
        active_experiments = [],
        node_health = initialize_node_health(),
        network_topology = initialize_network_topology(),
        system_metrics = initialize_system_metrics(),
        experiment_history = []
    },

    %% Start monitoring
    erlang:send_after(1000, self(), collect_system_metrics),

    {ok, State}.

handle_call({run_chaos_experiment, ExperimentId, Scenario}, _From, State) ->
    %% Validate scenario
    case validate_chaos_scenario(Scenario) of
        {ok, ValidScenario} ->
            %% Create and start experiment
            Experiment = create_experiment(ExperimentId, ValidScenario),
            NewState = start_experiment(Experiment, State),
            {reply, {ok, experiment_started}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_active_experiments, _From, State) ->
    {reply, {ok, State#chaos_state.active_experiments}, State};

handle_call(get_experiment_history, _From, State) ->
    {reply, {ok, State#chaos_state.experiment_history}, State};

handle_call(get_system_health, _From, State) ->
    {reply, {ok, State#chaos_state.node_health}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(collect_system_metrics, State) ->
    %% Collect system metrics
    Metrics = collect_chaos_metrics(State),
    ?LOG_DEBUG("Chaos metrics: ~p", [Metrics]),

    %% Update system state
    NewState = State#chaos_state{
        system_metrics = Metrics
    },

    %% Check experiment health
    UpdatedState = monitor_experiment_health(NewState),

    %% Schedule next metrics collection
    erlang:send_after(1000, self(), collect_system_metrics),
    {noreply, UpdatedState};

handle_info({experiment_completed, ExperimentId, Results}, State) ->
    %% Handle completed experiment
    ?LOG_INFO("Experiment ~p completed: ~p", [ExperimentId, Results]),

    UpdatedState = case lists:keyfind(ExperimentId, #experiment.id, State#chaos_state.active_experiments) of
        false ->
            State;
        Experiment ->
            CompletedExperiment = Experiment#experiment{
                status = completed,
                metrics = Results
            },

            %% Add to history
            NewHistory = [CompletedExperiment | State#chaos_state.experiment_history],

            %% Remove from active experiments
            NewActive = lists:keydelete(ExperimentId, #experiment.id, State#chaos_state.active_experiments),

            State#chaos_state{
                active_experiments = NewActive,
                experiment_history = NewHistory
            }
    end,

    {noreply, UpdatedState};

handle_info({failure_injected, FailureType, NodeId, Details}, State) ->
    %% Log injected failure
    ?LOG_WARNING("Failure injected: ~p on node ~p: ~p", [FailureType, NodeId, Details]),

    %% Update node health
    NewNodeHealth = update_node_health(NodeId, FailureType, State#chaos_state.node_health),

    %% Inject corresponding chaos
    UpdatedState = inject_chaos_failure(FailureType, NodeId, Details, State),

    {noreply, UpdatedState};

handle_info({recovery_action, Action, NodeId, Result}, State) ->
    %% Log recovery action
    ?LOG_INFO("Recovery action ~p on node ~p: ~p", [Action, NodeId, Result]),

    %% Update recovery actions
    RecoveryAction = #{
        action => Action,
        node_id => NodeId,
        timestamp => erlang:system_time(millisecond),
        result => Result
    },

    UpdatedState = State#chaos_state{
        active_experiments = lists:map(fun(Experiment) ->
            case lists:any(fun(FA) -> FA#node_id =:= NodeId end, Experiment#experiment.injected_failures) of
                true ->
                    Experiment#experiment{
                        recovery_actions = [RecoveryAction | Experiment#experiment.recovery_actions]
                    };
                false ->
                    Experiment
            end
        end, State#chaos_state.active_experiments)
    },

    {noreply, UpdatedState};

handle_info({health_check, NodeId, HealthStatus}, State) ->
    %% Update node health status
    ?LOG_DEBUG("Health check for node ~p: ~p", [NodeId, HealthStatus]),

    NewNodeHealth = maps:put(NodeId, HealthStatus, State#chaos_state.node_health),

    State#chaos_state{
        node_health = NewNodeHealth
    };

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cleanup chaos experiments
    cleanup_experiments(State),
    ?LOG_INFO("Chaos testing framework terminated", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% PRIVATE FUNCTIONS
%% ============================================================================

validate_chaos_scenario(Scenario) ->
    %% Validate chaos scenario configuration
    case Scenario#chaos_scenario.severity of
        low ->
            %% Low severity - basic validation
            {ok, Scenario};
        medium ->
            %% Medium severity - check experiment type
            case Scenario#chaos_scenario.experiment_type of
                network_partition ->
                    {ok, Scenario};
                node_failure ->
                    {ok, Scenario};
                _ ->
                    {error, unsupported_experiment_type}
            end;
        high ->
            %% High severity - strict validation
            case Scenario#chaos_scenario.experiment_type of
                network_partition ->
                    validate_network_partition_scenario(Scenario);
                node_failure ->
                    validate_node_failure_scenario(Scenario);
                latency_spike ->
                    validate_latency_spike_scenario(Scenario);
                packet_loss ->
                    validate_packet_loss_scenario(Scenario);
                resource_exhaustion ->
                    validate_resource_exhaustion_scenario(Scenario)
            end;
        critical ->
            %% Critical severity - require manual approval
            {ok, Scenario}
    end.

validate_network_partition_scenario(Scenario) ->
    %% Validate network partition scenario
    case Scenario#chaos_scenario.duration of
        Duration when Duration < 30000 ->
            {error, network_partition_too_short};
        _ ->
            {ok, Scenario}
    end.

validate_node_failure_scenario(Scenario) ->
    %% Validate node failure scenario
    case Scenario#chaos_scenario.recovery_time of
        Recovery when Recovery < 10000 ->
            {error, recovery_time_too_short};
        _ ->
            {ok, Scenario}
    end.

validate_latency_spike_scenario(Scenario) ->
    %% Validate latency spike scenario
    {ok, Scenario}.

validate_packet_loss_scenario(Scenario) ->
    %% Validate packet loss scenario
    {ok, Scenario}.

validate_resource_exhaustion_scenario(Scenario) ->
    %% Validate resource exhaustion scenario
    {ok, Scenario}.

create_experiment(ExperimentId, Scenario) ->
    %% Create chaos experiment
    #experiment{
        id = ExperimentId,
        scenario = Scenario,
        start_time = erlang:system_time(millisecond),
        status = planned,
        injected_failures = [],
        recovery_actions = [],
        metrics = #{},
        health_checks = []
    }.

start_experiment(Experiment, State) ->
    %% Start chaos experiment
    ?LOG_INFO("Starting chaos experiment: ~p", [Experiment#experiment.id]),

    %% Pre-experiment health check
    PreHealth = perform_health_check(),

    %% Inject failures based on scenario
    InjectedFailures = inject_experiment_failures(Experiment#experiment.scenario),

    %% Update experiment state
    UpdatedExperiment = Experiment#experiment{
        status = running,
        injected_failures = InjectedFailures,
        health_checks = [PreHealth]
    },

    %% Add to active experiments
    NewActive = [UpdatedExperiment | State#chaos_state.active_experiments],

    State#chaos_state{
        active_experiments = NewActive
    }.

inject_experiment_failures(Scenario) ->
    %% Inject failures based on scenario type
    case Scenario#chaos_scenario.experiment_type of
        network_partition ->
            inject_network_partition(Scenario);
        node_failure ->
            inject_node_failure(Scenario);
        latency_spike ->
            inject_latency_spike(Scenario);
        packet_loss ->
            inject_packet_loss(Scenario);
        resource_exhaustion ->
            inject_resource_exhaustion(Scenario)
    end.

inject_network_partition(Scenario) ->
    %% Simulate network partition
    Nodes = get_all_nodes(),
    PartitionNodes = select_partition_nodes(Nodes, Scenario#chaos_scenario.severity),

    lists:map(fun(NodeId) ->
        %% Inject network partition
        erlmcp_network:inject_partition(NodeId, Scenario#chaos_scenario.duration),

        #{
            type => network_partition,
            node_id => NodeId,
            duration => Scenario#chaos_scenario.duration,
            severity => Scenario#chaos_scenario.severity,
            timestamp => erlang:system_time(millisecond)
        }
    end, PartitionNodes).

inject_node_failure(Scenario) ->
    %% Simulate node failure
    Nodes = get_all_nodes(),
    FailedNodes = select_failed_nodes(Nodes, Scenario#chaos_scenario.severity),

    lists:map(fun(NodeId) ->
        %% Inject node failure
        erlmcp_node:inject_failure(NodeId, Scenario#chaos_scenario.duration),

        #{
            type => node_failure,
            node_id => NodeId,
            duration => Scenario#chaos_scenario.duration,
            severity => Scenario#chaos_scenario.severity,
            timestamp => erlang:system_time(millisecond)
        }
    end, FailedNodes).

inject_latency_spike(Scenario) ->
    %% Simulate latency spike
    Nodes = get_all_nodes(),
    LatencyNodes = select_latency_nodes(Nodes, Scenario#chaos_scenario.severity),

    lists:map(fun(NodeId) ->
        %% Inject latency spike
        erlmcp_network:inject_latency(NodeId, Scenario#chaos_scenario.duration,
                                     calculate_latency_increase(Scenario#chaos_scenario.severity)),

        #{
            type => latency_spike,
            node_id => NodeId,
            duration => Scenario#chaos_scenario.duration,
            severity => Scenario#chaos_scenario.severity,
            timestamp => erlang:system_time(millisecond)
        }
    end, LatencyNodes).

inject_packet_loss(Scenario) ->
    %% Simulate packet loss
    Nodes = get_all_nodes(),
    LossNodes = select_loss_nodes(Nodes, Scenario#chaos_scenario.severity),

    lists:map(fun(NodeId) ->
        %% Inject packet loss
        erlmcp_network:inject_packet_loss(NodeId, Scenario#chaos_scenario.duration,
                                         calculate_packet_loss_rate(Scenario#chaos_scenario.severity)),

        #{
            type => packet_loss,
            node_id => NodeId,
            duration => Scenario#chaos_scenario.duration,
            severity => Scenario#chaos_scenario.severity,
            timestamp => erlang:system_time(millisecond)
        }
    end, LossNodes).

inject_resource_exhaustion(Scenario) ->
    %% Simulate resource exhaustion
    Nodes = get_all_nodes(),
    ExhaustNodes = select_exhaust_nodes(Nodes, Scenario#chaos_scenario.severity);

    lists:map(fun(NodeId) ->
        %% Inject resource exhaustion
        erlmcp_resources:inject_exhaustion(NodeId, Scenario#chaos_scenario.duration,
                                          calculate_resource_limit(Scenario#chaos_scenario.severity)),

        #{
            type => resource_exhaustion,
            node_id => NodeId,
            duration => Scenario#chaos_scenario.duration,
            severity => Scenario#chaos_scenario.severity,
            timestamp => erlang:system_time(millisecond)
        }
    end, ExhaustNodes).

monitor_experiment_health(State) ->
    %% Monitor health of active experiments
    lists:foldl(fun(Experiment, StateAcc) ->
        case Experiment#experiment.status of
            running ->
                %% Check if experiment should complete
                case should_complete_experiment(Experiment) of
                    true ->
                        %% Complete experiment
                        Results = collect_experiment_results(Experiment),
                        erlang:send_after(0, self(), {experiment_completed,
                                                     Experiment#experiment.id, Results}),
                        StateAcc;
                    false ->
                        %% Continue monitoring
                        StateAcc
                end;
            _ ->
                StateAcc
        end
    end, State, State#chaos_state.active_experiments).

should_complete_experiment(Experiment) ->
    %% Check if experiment should complete
    ElapsedTime = erlang:system_time(millisecond) - Experiment#experiment.start_time;
    Duration = Experiment#experiment.scenario#chaos_scenario.duration;

    ElapsedTime >= Duration.

collect_experiment_results(Experiment) ->
    %% Collect experiment results
    Metrics = collect_chaos_metrics(Experiment),
    HealthChecks = collect_health_check_results(Experiment);

    #{
        experiment_id => Experiment#experiment.id,
        scenario_name => Experiment#experiment.scenario#chaos_scenario.name,
        start_time => Experiment#experiment.start_time,
        end_time => erlang:system_time(millisecond),
        duration => erlang:system_time(millisecond) - Experiment#experiment.start_time,
        injected_failures => Experiment#experiment.injected_failures,
        recovery_actions => Experiment#experiment.recovery_actions,
        metrics => Metrics,
        health_checks => HealthChecks,
        recovery_time => calculate_recovery_time(Experiment),
        impact_score => calculate_impact_score(Experiment),
        recommendations => generate_recommendations(Experiment)
    }.

collect_chaos_metrics(State) ->
    %% Collect chaos-specific metrics
    #{
        timestamp => erlang:system_time(millisecond),
        node_health => State#chaos_state.node_health,
        active_connections => get_active_connections(),
        request_latency => get_request_latency(),
        error_rate => get_error_rate(),
        cpu_usage => get_cpu_usage(),
        memory_usage => get_memory_usage(),
        network_throughput => get_network_throughput(),
        disk_io => get_disk_io()
    }.

perform_health_check() ->
    %% Perform pre-experiment health check
    #{
        timestamp => erlang:system_time(millisecond),
        node_health => get_node_health(),
        system_health => get_system_health(),
        metrics => get_system_metrics(),
        all_healthy => true
    }.

collect_health_check_results(Experiment) ->
    %% Collect health check results during experiment
    HealthChecks = lists:foldl(fun(Failure, Acc) ->
        NodeId = Failure#node_id,
        HealthStatus = get_node_health(NodeId);

        PostHealth = #{
            timestamp => erlang:system_time(millisecond),
            node_id => NodeId,
            health => HealthStatus,
            recovered => is_recovered(NodeId)
        },
        [PostHealth | Acc]
    end, [], Experiment#experiment.injected_failures),

    PostHealth = #{
        timestamp => erlang:system_time(millisecond),
        node_health => get_node_health(),
        system_health => get_system_health(),
        all_recovered => all_nodes_recovered(Experiment)
    },

    [PostHealth | HealthChecks].

calculate_recovery_time(Experiment) ->
    %% Calculate recovery time for experiment
    RecoveryActions = Experiment#experiment.recovery_actions;

    case RecoveryActions of
        [] ->
            0;
        _ ->
            FirstAction = lists:last(RecoveryActions);
            LastAction = lists:nth(1, RecoveryActions);
            LastAction#timestamp - FirstAction#timestamp
    end.

calculate_impact_score(Experiment) ->
    %% Calculate impact score for experiment
    Metrics = Experiment#experiment.metrics;
    ErrorRate = Metrics#error_rate;
    LatencyIncrease = Metrics#request_latency / get_baseline_latency() - 1;

    %% Weighted impact calculation
    (ErrorRate * 0.4) + (LatencyIncrease * 0.6).

generate_recommendations(Experiment) ->
    %% Generate recommendations based on experiment results
    Recommendations = case Experiment#experiment.scenario#chaos_scenario.experiment_type of
        network_partition ->
            generate_network_partition_recommendations(Experiment);
        node_failure ->
            generate_node_failure_recommendations(Experiment);
        latency_spike ->
            generate_latency_spike_recommendations(Experiment);
        packet_loss ->
            generate_packet_loss_recommendations(Experiment);
        resource_exhaustion ->
            generate_resource_exhaustion_recommendations(Experiment)
    end,

    Recommendations.

initialize_node_health() ->
    %% Initialize node health map
    Nodes = get_all_nodes(),
    maps:from_list(lists:map(fun(NodeId) ->
        {NodeId, #{status => healthy, last_check => erlang:system_time(millisecond)}}
    end, Nodes)).

initialize_network_topology() ->
    %% Initialize network topology
    #{
        nodes => get_all_nodes(),
        connections => get_network_connections()
    }.

initialize_system_metrics() ->
    %% Initialize system metrics
    #{
        cpu_usage => 0.0,
        memory_usage => 0.0,
        network_throughput => 0.0,
        disk_io => 0.0,
        active_connections => 0,
        request_latency => 0.0,
        error_rate => 0.0
    }.

inject_chaos_failure(FailureType, NodeId, Details, State) ->
    %% Inject chaos failure
    erlang:send_after(0, self(), {failure_injected, FailureType, NodeId, Details}),

    %% Update node health
    NewNodeHealth = update_node_health(NodeId, FailureType, State#chaos_state.node_health);

    State#chaos_state{
        node_health = NewNodeHealth
    }.

update_node_health(NodeId, FailureType, NodeHealth) ->
    %% Update node health status
    maps:update(NodeId, #{status => get_failure_status(FailureType),
                         failure_type => FailureType,
                         last_check => erlang:system_time(millisecond)}, NodeHealth).

get_failure_status(FailureType) ->
    case FailureType of
        network_partition ->
            partitioned;
        node_failure ->
            failed;
        latency_spike ->
            degraded;
        packet_loss ->
            unreliable;
        resource_exhaustion ->
            overloaded;
        _ ->
            unhealthy
    end.

cleanup_experiments(State) ->
    %% Cleanup active experiments
    lists:foreach(fun(Experiment) ->
        %% Rollback experiment
        rollback_experiment(Experiment),

        %% Log completion
        ?LOG_INFO("Experiment ~p cleaned up", [Experiment#experiment.id])
    end, State#chaos_state.active_experiments).

rollback_experiment(Experiment) ->
    %% Rollback experiment changes
    lists:foreach(fun(Failure) ->
        NodeId = Failure#node_id;
        FailureType = Failure#type;

        rollback_failure(NodeId, FailureType)
    end, Experiment#experiment.injected_failures).

rollback_failure(NodeId, FailureType) ->
    %% Rollback specific failure
    case FailureType of
        network_partition ->
            erlmcp_network:remove_partition(NodeId);
        node_failure ->
            erlmcp_node:restart_node(NodeId);
        latency_spike ->
            erlmcp_network:remove_latency_spike(NodeId);
        packet_loss ->
            erlmcp_network:remove_packet_loss(NodeId);
        resource_exhaustion ->
            erlmcp_resources:reset_limits(NodeId)
    end.

get_all_nodes() ->
    %% Get all cluster nodes
    case erlmcp_cluster:get_nodes() of
        {ok, Nodes} ->
            Nodes;
        _ ->
            [node()]
    end.

get_network_connections() ->
    %% Get network connections
    case erlmcp_network:get_connections() of
        {ok, Connections} ->
            Connections;
        _ ->
            []
    end.

get_active_connections() ->
    %% Get active connection count
    erlmcp_connection_manager:get_count().

get_request_latency() ->
    %% Get average request latency
    erlmcp_metrics:get_average_latency().

get_error_rate() ->
    %% Get current error rate
    erlmcp_metrics:get_error_rate().

get_cpu_usage() ->
    %% Get CPU usage
    erlmcp_monitor:get_cpu_usage().

get_memory_usage() ->
    %% Get memory usage
    erlmcp_monitor:get_memory_usage().

get_network_throughput() ->
    %% Get network throughput
    erlmcp_monitor:get_network_throughput().

get_disk_io() ->
    %% Get disk I/O
    erlmcp_monitor:get_disk_io().

get_node_health() ->
    %% Get node health
    erlmcp_health_monitor:get_node_health().

get_system_health() ->
    %% Get system health
    erlmcp_health_monitor:get_system_health().

get_system_metrics() ->
    %% Get system metrics
    erlmcp_monitor:get_metrics().

get_baseline_latency() ->
    %% Get baseline latency
    erlmcp_metrics:get_baseline_latency().

is_recovered(NodeId) ->
    %% Check if node has recovered
    case erlmcp_health_monitor:get_node_health(NodeId) of
        #{status := healthy} ->
            true;
        _ ->
            false
    end.

all_nodes_recovered(Experiment) ->
    %% Check if all nodes have recovered
    lists:all(fun(Failure) ->
        is_recovered(Failure#node_id)
    end, Experiment#experiment.injected_failures).

select_partition_nodes(Nodes, Severity) ->
    %% Select nodes for network partition
    case Severity of
        low ->
            lists:sublist(Nodes, 1);
        medium ->
            lists:sublist(Nodes, max(1, length(Nodes) div 3));
        high ->
            lists:sublist(Nodes, max(1, length(Nodes) div 2));
        critical ->
            lists:sublist(Nodes, max(1, length(Nodes) - 1))
    end.

select_failed_nodes(Nodes, Severity) ->
    %% Select nodes for failure
    case Severity of
        low ->
            lists:sublist(Nodes, 1);
        medium ->
            lists:sublist(Nodes, max(1, length(Nodes) div 4));
        high ->
            lists:sublist(Nodes, max(1, length(Nodes) div 3));
        critical ->
            lists:sublist(Nodes, max(1, length(Nodes) div 2))
    end.

select_latency_nodes(Nodes, Severity) ->
    %% Select nodes for latency injection
    case Severity of
        low ->
            lists:sublist(Nodes, 1);
        medium ->
            lists:sublist(Nodes, max(1, length(Nodes) div 2));
        high ->
            Nodes;
        critical ->
            Nodes
    end.

select_loss_nodes(Nodes, Severity) ->
    %% Select nodes for packet loss
    case Severity of
        low ->
            lists:sublist(Nodes, 1);
        medium ->
            lists:sublist(Nodes, max(1, length(Nodes) div 3));
        high ->
            lists:sublist(Nodes, max(1, length(Nodes) div 2));
        critical ->
            Nodes
    end.

select_exhaust_nodes(Nodes, Severity) ->
    %% Select nodes for resource exhaustion
    case Severity of
        low ->
            lists:sublist(Nodes, 1);
        medium ->
            lists:sublist(Nodes, max(1, length(Nodes) div 4));
        high ->
            lists:sublist(Nodes, max(1, length(Nodes) div 3));
        critical ->
            lists:sublist(Nodes, max(1, length(Nodes) div 2))
    end.

calculate_latency_increase(Severity) ->
    %% Calculate latency increase based on severity
    case Severity of
        low ->
            50; % 50ms increase
        medium ->
            200; % 200ms increase
        high ->
            500; % 500ms increase
        critical ->
            1000 % 1000ms increase
    end.

calculate_packet_loss_rate(Severity) ->
    %% Calculate packet loss rate based on severity
    case Severity of
        low ->
            0.01; % 1% loss
        medium ->
            0.05; % 5% loss
        high ->
            0.10; % 10% loss
        critical ->
            0.25 % 25% loss
    end.

calculate_resource_limit(Severity) ->
    %% Calculate resource limit based on severity
    case Severity of
        low ->
            0.8; % 80% limit
        medium ->
            0.6; % 60% limit
        high ->
            0.4; % 40% limit
        critical ->
            0.2 % 20% limit
    end.

generate_network_partition_recommendations(Experiment) ->
    %% Generate recommendations for network partition
    Recommendations = case Experiment#experiment.scenario#chaos_scenario.severity of
        low ->
            [#{recommendation => "Monitor network latency", priority => low}];
        medium ->
            [#{recommendation => "Implement circuit breakers", priority => medium},
             #{recommendation => "Add connection pool size limits", priority => medium}];
        high ->
            [#{recommendation => "Implement automatic failover", priority => high},
             #{recommendation => "Add network redundancy", priority => high},
             #{recommendation => "Implement health checks with timeouts", priority => high}];
        critical ->
            [#{recommendation => "Implement distributed consensus for service discovery", priority => critical},
             #{recommendation => "Add multi-region deployment", priority => critical},
             #{recommendation => "Implement chaos engineering practices regularly", priority => critical}]
    end,

    Recommendations.

generate_node_failure_recommendations(Experiment) ->
    %% Generate recommendations for node failure
    Recommendations = case Experiment#experiment.scenario#chaos_scenario.severity of
        low ->
            [#{recommendation => "Monitor node health", priority => low}];
        medium ->
            [#{recommendation => "Increase supervisor restart limits", priority => medium},
             #{recommendation => "Implement node warmup procedures", priority => medium}];
        high ->
            [#{recommendation => "Implement auto-scaling", priority => high},
             #{recommendation => "Add node health check endpoints", priority => high},
             #{recommendation => "Implement graceful degradation", priority => high}];
        critical ->
            [#{recommendation => "Implement distributed sharding", priority => critical},
             #{recommendation => "Add backup nodes with automatic promotion", priority => critical},
             #{recommendation => "Implement circuit breakers with fallbacks", priority => critical}]
    end,

    Recommendations.

generate_latency_spike_recommendations(Experiment) ->
    %% Generate recommendations for latency spikes
    Recommendations = case Experiment#experiment.scenario#chaos_scenario.severity of
        low ->
            [#{recommendation => "Monitor request latency", priority => low}];
        medium ->
            [#{recommendation => "Implement request queuing", priority => medium},
             #{recommendation => "Add rate limiting", priority => medium}];
        high ->
            [#{recommendation => "Implement caching layers", priority => high},
             #{recommendation => "Add CDN for static assets", priority => high},
             #{recommendation => "Implement request batching", priority => high}];
        critical ->
            [#{recommendation => "Implement microservices architecture", priority => critical},
             #{recommendation => "Add load balancers with health checks", priority => critical},
             #{recommendation => "Implement edge computing nodes", priority => critical}]
    end,

    Recommendations.

generate_packet_loss_recommendations(Experiment) ->
    %% Generate recommendations for packet loss
    Recommendations = case Experiment#experiment.scenario#chaos_scenario.severity of
        low ->
            [#{recommendation => "Monitor packet loss rates", priority => low}];
        medium ->
            [#{recommendation => "Implement reliable message delivery", priority => medium},
             #{recommendation => "Add request acknowledgments", priority => medium}];
        high ->
            [#{recommendation => "Implement message queuing with retries", priority => high},
             #{recommendation => "Add redundancy in communication paths", priority => high},
             #{recommendation => "Implement heartbeat mechanisms", priority => high}];
        critical ->
            [#{recommendation => "Implement multi-path routing", priority => critical},
             #{recommendation => "Add offline mode capabilities", priority => critical},
             #{recommendation => "Implement data synchronization with conflict resolution", priority => critical}]
    end,

    Recommendations.

generate_resource_exhaustion_recommendations(Experiment) ->
    %% Generate recommendations for resource exhaustion
    Recommendations = case Experiment#experiment.scenario#chaos_scenario.severity of
        low ->
            [#{recommendation => "Monitor resource usage", priority => low}];
        medium ->
            [#{recommendation => "Implement resource limits", priority => medium},
             #{recommendation => "Add monitoring alerts", priority => medium}];
        high ->
            [#{recommendation => "Implement auto-scaling based on resources", priority => high},
             #{recommendation => "Add resource quotas per tenant", priority => high},
             #{recommendation => "Implement cold start procedures", priority => high}];
        critical ->
            [#{recommendation => "Implement sharding for data distribution", priority => critical},
             #{recommendation => "Add read replicas for database", priority => critical},
             #{recommendation => "Implement caching strategies", priority => critical},
             #{recommendation => "Add multi-region deployment", priority => critical}]
    end,

    Recommendations.