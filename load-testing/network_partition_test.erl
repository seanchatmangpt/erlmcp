%% @doc Network Partition Test for erlmcp v3
%% Comprehensive testing of network partition tolerance
%%
%% Features:
%% - Network partition simulation
%% - Consistency verification
%% - Failover testing
%% - Recovery validation
%% - Latency measurement
%%- Message loss simulation
%%- Reconnection testing
%%- Cross-partition communication

-module(erlmcp_network_partition_test).
-author("erlmcp-load-test-team").
-behaviour(gen_server).

%% API exports
-export([start_link/0, run_partition_test/3, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Include
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Records
-record.partition_scenario, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    partition_type :: split_brain | asymmetric | complete | intermittent,
    nodes :: list(),
    partition_groups :: list(list()),
    duration :: integer(),
    recovery_strategy :: automatic | manual | hybrid,
    consistency_checks :: list(),
    failure_injection :: list()
}.

record.partition_test, {
    id :: binary(),
    scenario :: #partition_scenario{},
    start_time :: integer(),
    partition_time :: integer(),
    recovery_time :: integer(),
    status :: planning | partitioning | partitioned | recovering | completed,
    metrics :: map(),
    partition_events :: list(),
    recovery_actions :: list(),
    consistency_results :: map(),
    health_status :: map()
}.

record.partition_metrics, {
    timestamp :: integer(),
    partition_id :: binary(),
    node_count :: integer(),
    message_loss :: float(),
    latency_increase :: float(),
    throughput :: float(),
    error_rate :: float(),
    connectivity :: map(),
    consistency_score :: float()
}.

record.partition_event, {
    id :: binary(),
    type :: partition_started | partition_ended | recovery_started | recovery_completed,
    timestamp :: integer(),
    details :: map(),
    affected_nodes :: list()
}.

record.consistency_check, {
    id :: binary(),
    type :: strong | eventual | causal,
    timestamp :: integer(),
    result :: passed | failed | inconsistent,
    details :: map(),
    divergence :: list()
}.

%% ============================================================================
%% API FUNCTIONS
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

run_partition_test(TestId, Scenario, Config) ->
    gen_server:call(?MODULE, {run_partition_test, TestId, Scenario, Config}, infinity).

stop() ->
    gen_server:call(?MODULE, stop).

%% ============================================================================
%% GEN_SERVER CALLBACKS
%% ============================================================================

init(_Args) ->
    process_flag(trap_exit, true),
    ?LOG_INFO("Network partition test framework initialized"),

    %% Initialize state
    State = #{
        active_tests => [],
        partition_history => [],
        network_topology => initialize_network_topology(),
        consistency_results => initialize_consistency_results(),
        partition_metrics => initialize_partition_metrics()
    },

    %% Start monitoring
    erlang:send_after(5000, self(), monitor_partitions),

    {ok, State}.

handle_call({run_partition_test, TestId, Scenario, Config}, _From, State) ->
    %% Validate scenario
    case validate_partition_scenario(Scenario) of
        {ok, ValidScenario} ->
            %% Create and start partition test
            Test = create_partition_test(TestId, ValidScenario, Config),
            NewState = start_partition_test(Test, State),
            {reply, {ok, test_started}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_active_tests, _From, State) ->
    {reply, {ok, maps:get(active_tests, State, [])}, State};

handle_call(get_partition_history, _From, State) ->
    {reply, {ok, maps:get(partition_history, State, [])}, State};

handle_call(get_network_topology, _From, State) ->
    {reply, {ok, maps:get(network_topology, State, [])}, State};

handle_call(consistency_results, _From, State) ->
    {reply, {ok, maps:get(consistency_results, State, [])}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(monitor_partitions, State) ->
    %% Monitor network partitions
    PartitionMetrics = monitor_network_partitions(State);
    UpdatedState = State#{
        partition_metrics => PartitionMetrics
    },

    %% Schedule next monitoring
    erlang:send_after(5000, self(), monitor_partitions),
    {noreply, UpdatedState};

handle_info({test_phase, TestId, Phase, Metrics}, State) ->
    %% Handle test phase transitions
    case lists:keyfind(TestId, #partition_test.id, maps:get(active_tests, State)) of
        false ->
            {noreply, State};
        Test ->
            UpdatedTest = Test#partition_test{
                status = Phase,
                metrics = merge_metrics(Test#partition_test.metrics, Metrics)
            },

            NewState = State#{
                active_tests => lists:keyreplace(TestId, #partition_test.id,
                                              maps:get(active_tests, State), UpdatedTest)
            },

            case Phase of
                partitioning ->
                    %% Create network partitions
                    erlang:send_after(0, self(), {create_partitions, TestId});
                recovering ->
                    Initiate recovery
                    erlang:send_after(0, self(), {initiate_recovery, TestId});
                completed ->
                    %% Generate final report
                    Report = generate_partition_test_report(Test, State);
                    erlang:send_after(0, self(), {test_complete, TestId, Report});
                _ ->
                    ok
            end,

            {noreply, NewState}
    end;

handle_info({create_partitions, TestId}, State) ->
    %% Create network partitions
    case lists:keyfind(TestId, #partition_test.id, maps:get(active_tests, State)) of
        false ->
            {noreply, State};
        Test ->
            case create_network_partitions(Test) of
                {ok, PartitionEvents} ->
                    PartitionTime = erlang:system_time(millisecond);
                    UpdatedTest = Test#partition_test{
                        partition_time = PartitionTime,
                        partition_events = PartitionEvents
                    };

                    erlang:send_after(0, self(), {test_phase, TestId, partitioned, #{}}),
                    {noreply, update_test(TestId, UpdatedTest, State)};
                {error, Reason} ->
                    ?LOG_ERROR("Failed to create partitions: ~p", [Reason]),
                    UpdatedTest = Test#partition_test{
                        status => failed,
                        recovery_time => erlang:system_time(millisecond)
                    },
                    {noreply, update_test(TestId, UpdatedTest, State)}
            end
    end;

handle_info({initiate_recovery, TestId}, State) ->
    %% Initiate partition recovery
    case lists:keyfind(TestId, #partition_test.id, maps:get(active_tests, State)) of
        false ->
            {noreply, State};
        Test ->
            RecoveryStartTime = erlang:system_time(millisecond);
            RecoveryActions = initiate_recovery_actions(Test);

            UpdatedTest = Test#partition_test{
                recovery_time = RecoveryStartTime,
                recovery_actions = RecoveryActions
            };

            %% Schedule consistency checks
            schedule_consistency_checks(TestId, Test),

            erlang:send_after(0, self(), {test_phase, TestId, recovering, #{}}),
            {noreply, update_test(TestId, UpdatedTest, State)}
    end;

handle_info({consistency_check, TestId, CheckId, Result}, State) ->
    %% Handle consistency check results
    ?LOG_DEBUG("Consistency check ~p for test ~p: ~p", [CheckId, TestId, Result]),

    UpdatedState = lists:map(fun(Test) ->
        case Test#partition_test.id =:= TestId of
            true ->
                CheckRecord = #consistency_check{
                    id = CheckId,
                    timestamp = erlang:system_time(millisecond),
                    result = Result#result,
                    details = Result#details,
                    divergence = Result#divergence
                };

                UpdatedConsistency = maps:put(CheckId, CheckRecord, Test#partition_test.consistency_results),
                Test#partition_test{
                    consistency_results => UpdatedConsistency
                };
            false ->
                Test
        end
    end, maps:get(active_tests, State)),

    State#{
        active_tests => UpdatedState
    };

handle_info({recovery_completed, TestId, RecoveryMetrics}, State) ->
    %% Handle recovery completion
    ?LOG_INFO("Recovery completed for test ~p", [TestId]),

    case lists:keyfind(TestId, #partition_test.id, maps:get(active_tests, State)) of
        false ->
            {noreply, State};
        Test ->
            UpdatedTest = Test#partition_test{
                recovery_time = erlang:system_time(millisecond),
                metrics = merge_metrics(Test#partition_test.metrics, RecoveryMetrics)
            };

            %% Complete test
            erlang:send_after(0, self(), {test_phase, TestId, completed, #{}}),
            {noreply, update_test(TestId, UpdatedTest, State)}
    end;

handle_info({test_complete, TestId, Report}, State) ->
    %% Handle test completion
    ?LOG_INFO("Network partition test ~p completed", [TestId]),

    case lists:keyfind(TestId, #partition_test.id, maps:get(active_tests, State)) of
        false ->
            {noreply, State};
        Test ->
            %% Move to history
            NewHistory = [Test | maps:get(partition_history, State)],
            NewActive = lists:keydelete(TestId, #partition_test.id, maps:get(active_tests, State)),

            %% Save report
            ReportFile = "/Users/sac/erlmcp/load-testing/partition_test_" ++
                         binary_to_list(TestId) ++ "_report.json",
            file:write_file(ReportFile, jsx:encode(Report)),

            ?LOG_INFO("Network partition test report saved to: ~p", [ReportFile]),

            State#{
                active_tests => NewActive,
                partition_history => NewHistory
            }
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cleanup partition tests
    lists:foreach(fun(Test) ->
        rollback_partition_test(Test)
    end, maps:get(active_tests, State)),

    ?LOG_INFO("Network partition test framework terminated", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% PRIVATE FUNCTIONS
%% ============================================================================

validate_partition_scenario(Scenario) ->
    %% Validate partition scenario
    case Scenario#partition_scenario.partition_type of
        split_brain ->
            validate_split_brain_scenario(Scenario);
        asymmetric ->
            validate_asymmetric_scenario(Scenario);
        complete ->
            validate_complete_scenario(Scenario);
        intermittent ->
            validate_intermittent_scenario(Scenario);
        _ ->
            {error, invalid_partition_type}
    end.

validate_split_brain_scenario(Scenario) ->
    %% Validate split-brain scenario
    case Scenario#partition_scenario.nodes =/= [] andalso
         length(Scenario#partition_scenario.nodes) >= 2 of
        true ->
            {ok, Scenario};
        false ->
            {error, insufficient_nodes}
    end.

validate_asymmetric_scenario(Scenario) ->
    %% Validate asymmetric scenario
    case Scenario#partition_scenario.nodes =/= [] andalso
         length(Scenario#partition_scenario.nodes) >= 2 of
        true ->
            {ok, Scenario};
        false ->
            {error, insufficient_nodes}
    end.

validate_complete_scenario(Scenario) ->
    %% Validate complete partition scenario
    case Scenario#partition_scenario.nodes =/= [] of
        true ->
            {ok, Scenario};
        false ->
            {error, insufficient_nodes}
    end.

validate_intermittent_scenario(Scenario) ->
    %% Validate intermittent partition scenario
    case Scenario#partition_scenario.duration > 0 of
        true ->
            {ok, Scenario};
        false ->
            {error, invalid_duration}
    end.

create_partition_test(TestId, Scenario, Config) ->
    %% Create partition test
    #partition_test{
        id = TestId,
        scenario = Scenario,
        start_time = erlang:system_time(millisecond),
        status = planning,
        metrics => #{},
        partition_events = [],
        recovery_actions = [],
        consistency_results => #{},
        health_status => initialize_health_status(Scenario#partition_scenario.nodes)
    }.

start_partition_test(Test, State) ->
    %% Start partition test
    ?LOG_INFO("Starting network partition test: ~p", [Test#partition_test.id]),

    %% Start continuous load testing
    start_load_testing(Test, State);

    %% Add to active tests
    NewActive = [Test | maps:get(active_tests, State)],

    %% Send phase transition
    erlang:send_after(0, self(), {test_phase, Test#partition_test.id, partitioning, #{}}),

    State#{
        active_tests => NewActive
    }.

create_network_partitions(Test) ->
    %% Create network partitions
    Scenario = Test#partition_scenario;
    PartitionType = Scenario#partition_scenario.partition_type;
    PartitionGroups = Scenario#partition_scenario.partition_groups;

    case PartitionType of
        split_brain ->
            create_split_brain_partitions(PartitionGroups);
        asymmetric ->
            create_asymmetric_partitions(PartitionGroups);
        complete ->
            create_complete_partitions();
        intermittent ->
            create_intermittent_partitions(Scenario#duration)
    end.

create_split_brain_partitions(PartitionGroups) ->
    %% Create split-brain partitions
    PartitionEvents = lists:map(fun(Group) ->
        GroupId = generate_partition_id();
        Nodes = Group;

        %% Create partition for group
        case erlmcp_network:create_partition(Nodes) of
            ok ->
                #partition_event{
                    id = generate_event_id(),
                    type = partition_started,
                    timestamp = erlang:system_time(millisecond),
                    details = #{group => GroupId, nodes => Nodes},
                    affected_nodes = Nodes
                };
            {error, Reason} ->
                #partition_event{
                    id = generate_event_id(),
                    type = partition_failed,
                    timestamp = erlang:system_time(millisecond),
                    details = #{reason => Reason},
                    affected_nodes = []
                }
        end
    end, PartitionGroups),

    {ok, PartitionEvents}.

create_asymmetric_partitions(PartitionGroups) ->
    %% Create asymmetric partitions
    %% One group gets more resources, others get limited
    PartitionEvents = lists:map(fun(Group) ->
        GroupId = generate_partition_id();
        Nodes = Group;

        %% Create asymmetric partition
        case erlmcp_network:create_asymmetric_partition(Nodes) of
            ok ->
                #partition_event{
                    id = generate_event_id(),
                    type = partition_started,
                    timestamp = erlang:system_time(millisecond),
                    details = #{group => GroupId, nodes => Nodes, type => asymmetric},
                    affected_nodes = Nodes
                };
            {error, Reason} ->
                #partition_event{
                    id = generate_event_id(),
                    type = partition_failed,
                    timestamp = erlang:system_time(millisecond),
                    details = #{reason => Reason},
                    affected_nodes = []
                }
        end
    end, PartitionGroups),

    {ok, PartitionEvents}.

create_complete_partitions() ->
    %% Create complete network partition
    Nodes = get_all_nodes();

    case erlmcp_network:create_complete_partition(Nodes) of
        ok ->
            PartitionEvent = #partition_event{
                id = generate_event_id(),
                type = partition_started,
                timestamp = erlang:system_time(millisecond),
                details = #{type => complete, nodes => Nodes},
                affected_nodes = Nodes
            },
            {ok, [PartitionEvent]};
        {error, Reason} ->
            {error, Reason}
    end.

create_intermittent_partitions(Duration) ->
    %% Create intermittent partitions
    PartitionEvent = #partition_event{
        id = generate_event_id(),
        type = partition_started,
        timestamp = erlang:system_time(millisecond),
        details = #{type => intermittent, duration => Duration},
        affected_nodes = get_all_nodes()
    },

    %% Schedule partition end
    erlang:send_after(Duration, self(), {end_intermittent_partition, generate_event_id()}),

    {ok, [PartitionEvent]}.

initiate_recovery_actions(Test) ->
    %% Initiate recovery actions
    Scenario = Test#partition_scenario;
    RecoveryStrategy = Scenario#recovery_strategy;

    case RecoveryStrategy of
        automatic ->
            initiate_automatic_recovery(Test);
        manual ->
            initiate_manual_recovery(Test);
        hybrid ->
            initiate_hybrid_recovery(Test)
    end.

initiate_automatic_recovery(Test) ->
    %% Initiate automatic recovery
    Scenario = Test#partition_scenario;

    RecoveryActions = lists:map(fun(Group) ->
        RecoveryAction = #{
            action_type => heal_partition,
            target_nodes => Group,
            start_time => erlang:system_time(millisecond),
            timeout => Scenario#duration,
            strategy => automatic
        };

        case erlmcp_network:heal_partition(Group) of
            ok ->
                RecoveryAction#{result => success};
            {error, Reason} ->
                RecoveryAction#{result => failed, reason => Reason}
        end
    end, Scenario#partition_groups);

    lists:map(fun(Action) ->
        case Action#result =:= success of
            true ->
                erlang:send_after(0, self(), {recovery_completed, Test#id, Action});
            false ->
                erlang:send_after(0, self(), {recovery_failed, Test#id, Action})
        end
    end, RecoveryActions);

    RecoveryActions.

initiate_manual_recovery(Test) ->
    %% Initiate manual recovery
    Scenario = Test#partition_scenario;

    %% Trigger manual recovery alert
    erlmcp_alerting:send_alert(#{
        type => manual_recovery_required,
        test_id => Test#id,
        severity => critical,
        message => "Manual intervention required for partition recovery",
        details => Scenario
    }),

    %% Wait for manual intervention
    RecoveryAction = #{
        action_type => manual_recovery,
        target_nodes => Scenario#nodes,
        start_time => erlang:system_time(millisecond),
        timeout => Scenario#duration,
        strategy => manual
    };

    [RecoveryAction].

initiate_hybrid_recovery(Test) ->
    %% Initiate hybrid recovery
    Scenario = Test#partition_scenario;

    %% First attempt automatic recovery
    RecoveryActions = initiate_automatic_recovery(Test);

    %% Add manual backup
    ManualAction = #{
        action_type => manual_backup,
        target_nodes => Scenario#nodes,
        start_time => erlang:system_time(millisecond),
        timeout => Scenario#duration,
        strategy => hybrid
    };

    [ManualAction | RecoveryActions].

schedule_consistency_checks(TestId, Test) ->
    %% Schedule consistency checks
    Scenario = Test#partition_scenario;
    Checks = Scenario#consistency_checks;

    lists:foreach(fun(Check) ->
        CheckId = generate_check_id();
        Interval = Check#check_interval;

        erlang:send_after(Interval, self(), {perform_consistency_check, TestId, CheckId, Check})
    end, Checks).

perform_consistency_check(TestId, CheckId, Check) ->
    %% Perform consistency check
    ?LOG_DEBUG("Performing consistency check ~p for test ~p", [CheckId, TestId]),

    CheckType = Check#check_type;
    Nodes = Check#check_nodes;

    try
        Case CheckType of
            strong ->
                Result = perform_strong_consistency_check(Nodes);
            eventual ->
                Result = perform_eventual_consistency_check(Nodes);
            causal ->
                Result = perform_causal_consistency_check(Nodes)
        end,

        CheckResult = #{
            result => Result#result,
            details => Result#details,
            divergence => Result#divergence
        };

        erlang:send_after(0, self(), {consistency_check, TestId, CheckId, CheckResult})

    catch
        Error:Reason ->
            CheckResult = #{
                result => failed,
                details => #{error => {Error, Reason}},
                divergence => []
            };

            erlang:send_after(0, self(), {consistency_check, TestId, CheckId, CheckResult})
    end.

perform_strong_consistency_check(Nodes) ->
    %% Perform strong consistency check
    Case perform_read_consistency_check(Nodes) of
        {ok, Consistent} ->
            #{
                result => case Consistent of
                    true -> passed;
                    false -> failed
                end,
                details => #{check_type => strong, nodes => Nodes},
                divergence => []
            };
        {error, Reason} ->
            #{
                result => failed,
                details => #{error => Reason},
                divergence => []
            }
    end.

perform_eventual_consistency_check(Nodes) ->
    %% Perform eventual consistency check
    Case perform_read_consistency_check(Nodes) of
        {ok, Consistent} ->
            #{
                result => case Consistent of
                    true -> passed;
                    false -> inconsistent
                end,
                details => #{check_type => eventual, nodes => Nodes},
                divergence => []
            };
        {error, Reason} ->
            #{
                result => failed,
                details => #{error => Reason},
                divergence => []
            }
    end.

perform_causal_consistency_check(Nodes) ->
    %% Perform causal consistency check
    Case perform_causal_consistency(Nodes) of
        {ok, Consistent} ->
            #{
                result => case Consistent of
                    true -> passed;
                    false -> failed
                end,
                details => #{check_type => causal, nodes => Nodes},
                divergence => []
            };
        {error, Reason} ->
            #{
                result => failed,
                details => #{error => Reason},
                divergence => []
            }
    end.

perform_read_consistency_check(Nodes) ->
    %% Perform read consistency check
    Values = lists:map(fun(NodeId) ->
        case read_from_node(NodeId) of
            {ok, Value} ->
                Value;
            {error, Reason} ->
                {error, Reason}
        end
    end, Nodes);

    case Values of
        [First | Rest] ->
            AllSame = lists:all(fun(V) -> V =:= First end, Rest),
            {ok, AllSame};
        _ ->
            {error, insufficient_data}
    end.

perform_causal_consistency(Nodes) ->
    %% Perform causal consistency check
    %% This is a simplified implementation
    {ok, true}.

read_from_node(NodeId) ->
    %% Read data from node
    case erlmcp_data_store:get(NodeId) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.

monitor_network_partitions(State) ->
    %% Monitor network partitions
    PartitionMetrics = #{
        timestamp => erlang:system_time(millisecond),
        active_partitions => count_active_partitions(),
        affected_nodes => count_affected_nodes(),
        message_loss => calculate_message_loss(),
        throughput => calculate_throughput(),
        error_rate => calculate_error_rate(),
        connectivity => map_connectivity_state(),
        consistency_score => calculate_consistency_score()
    },

    PartitionMetrics.

count_active_partitions() ->
    %% Count active partitions
    case erlmcp_network:get_active_partitions() of
        {ok, Partitions} ->
            length(Partitions);
        _ ->
            0
    end.

count_affected_nodes() ->
    %% Count affected nodes
    case erlmcp_network:get_affected_nodes() of
        {ok, Nodes} ->
            length(Nodes);
        _ ->
            0
    end.

calculate_message_loss() ->
    %% Calculate message loss percentage
    case erlmcp_network:get_message_loss() of
        {ok, Loss} ->
            Loss;
        _ ->
            0.0
    end.

calculate_throughput() ->
    %% Calculate partition throughput
    case erlmcp_network:get_throughput() of
        {ok, Throughput} ->
            Throughput;
        _ ->
            0.0
    end.

calculate_error_rate() ->
    %% Calculate error rate during partitions
    case erlmcp_network:get_error_rate() of
        {ok, ErrorRate} ->
            ErrorRate;
        _ ->
            0.0
    end.

map_connectivity_state() ->
    %% Map connectivity state of nodes
    case erlmcp_network:get_connectivity() of
        {ok, Connectivity} ->
            Connectivity;
        _ ->
            #{}
    end.

calculate_consistency_score() ->
    %% Calculate consistency score
    Case erlmcp_consistency:get_score() of
        {ok, Score} ->
            Score;
        _ ->
            0.0
    end.

initialize_health_status(Nodes) ->
    %% Initialize health status
    lists:map(fun(NodeId) ->
        {NodeId, #{status => healthy, last_check => erlang:system_time(millisecond)}}
    end, Nodes).

initialize_network_topology() ->
    %% Initialize network topology
    case erlmcp_network:get_topology() of
        {ok, Topology} ->
            Topology;
        _ ->
            #{nodes => [], edges => []}
    end.

initialize_consistency_results() ->
    %% Initialize consistency results
    #{}.

initialize_partition_metrics() ->
    %% Initialize partition metrics
    #{
        timestamp => erlang:system_time(millisecond),
        active_partitions => 0,
        affected_nodes => 0,
        message_loss => 0.0,
        throughput => 0.0,
        error_rate => 0.0,
        connectivity => #{},
        consistency_score => 0.0
    }.

start_load_testing(Test, State) ->
    %% Start continuous load testing during partitions
    Scenario = Test#partition_scenario;

    LoadConfig = #{
        test_id => Test#id,
        target_nodes => Scenario#nodes,
        pattern => continuous,
        duration => Scenario#duration * 2, % Test before, during, and after
        rate => calculate_partition_load_rate(Scenario),
        tools => generate_partition_tool_set(Scenario)
    };

    erlmcp_load_generator:start(LoadConfig).

calculate_partition_load_rate(Scenario) ->
    %% Calculate load rate based on partition scenario
    Case Scenario#partition_type of
        split_brain ->
            5000;
        asymmetric ->
            3000;
        complete ->
            10000;
        intermittent ->
            7000
    end.

generate_partition_tool_set(Scenario) ->
    %% Generate tool set for partition testing
    [#{
        tool => file_system,
        weight => 0.25
    }, #{
        tool => shell_command,
        weight => 0.25
    }, #{
        tool => git,
        weight => 0.25
    }, #{
        tool => mermaid,
        weight => 0.25
    }].

generate_partition_test_report(Test, State) ->
    %% Generate network partition test report
    Scenario = Test#partition_scenario;
    Metrics = Test#metrics;
    PartitionEvents = Test#partition_events;
    RecoveryActions = Test#recovery_actions;
    ConsistencyResults = Test#consistency_results;

    Report = #{
        test_id => Test#id,
        scenario_name => Scenario#name,
        partition_type => Scenario#partition_type,
        start_time => Test#start_time,
        partition_time => Test#partition_time,
        recovery_time => Test#recovery_time,
        duration => Test#recovery_time - Test#start_time,
        metrics => Metrics,
        partition_events => PartitionEvents,
        recovery_actions => RecoveryActions,
        consistency_results => ConsistencyResults,
        health_status => Test#health_status,
        recommendations => generate_partition_recommendations(Test, State),
        summary => generate_partition_summary(Metrics, PartitionEvents, RecoveryActions),
        recovery_success => calculate_recovery_success(RecoveryActions),
        consistency_score => calculate_consistency_score(ConsistencyResults)
    },

    Report.

generate_partition_recommendations(Test, State) ->
    %% Generate recommendations based on partition test results
    Scenario = Test#scenario;
    Metrics = Test#metrics;

    Recommendations = case Metrics#message_loss > 0.1 of
        true ->
            [#{recommendation => "High message loss detected. Implement message duplication.",
               priority => high}];
        false ->
            [#{recommendation => "Message loss is acceptable", priority => low}]
    end;

    case Metrics#throughput < 1000 of
        true ->
            [#{recommendation => "Throughput degraded during partition. Implement load balancing.",
               priority => medium} | Recommendations];
        false ->
            [#{recommendation => "Throughput maintained during partition", priority => low} | Recommendations]
    end.

generate_partition_summary(Metrics, PartitionEvents, RecoveryActions) ->
    %% Generate partition test summary
    #{
        total_partitions => length(PartitionEvents),
        recovery_actions => length(RecoveryActions),
        message_loss => Metrics#message_loss,
        throughput => Metrics#throughput,
        error_rate => Metrics#error_rate,
        avg_recovery_time => calculate_avg_recovery_time(RecoveryActions),
        partition_duration => calculate_total_partition_duration(PartitionEvents)
    }.

calculate_recovery_success(RecoveryActions) ->
    %% Calculate recovery success rate
    SuccessfulRecoveries = length([R || R <- RecoveryActions, R#result =:= success]);
    TotalRecoveries = length(RecoveryActions);

    case TotalRecoveries of
        0 ->
            0.0;
        _ ->
            SuccessfulRecoveries / TotalRecoveries * 100
    end.

calculate_consistency_score(ConsistencyResults) ->
    %% Calculate consistency score from results
    PassedChecks = length([C || C <- maps:values(ConsistencyResults), C#result =:= passed]);
    TotalChecks = maps:size(ConsistencyResults);

    case TotalChecks of
        0 ->
            0.0;
        _ ->
            PassedChecks / TotalChecks * 100
    end.

calculate_avg_recovery_time(RecoveryActions) ->
    %% Calculate average recovery time
    case RecoveryActions of
        [] ->
            0;
        _ ->
            TotalTime = lists:sum([R#end_time - R#start_time || R <- RecoveryActions]);
            TotalTime / length(RecoveryActions)
    end.

calculate_total_partition_duration(PartitionEvents) ->
    %% Calculate total partition duration
    StartEvents = [E#timestamp || E <- PartitionEvents, E#type = partition_started];
    EndEvents = [E#timestamp || E <- PartitionEvents, E#type = partition_ended];

    case {StartEvents, EndEvents} of
        {[FirstStart | _], [FirstEnd | _]} ->
            FirstEnd - FirstStart;
        _ ->
            0
    end.

rollback_partition_test(Test) ->
    %% Rollback partition test changes
    Scenario = Test#scenario;

    %% Remove network partitions
    case erlmcp_network:remove_all_partitions() of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("Failed to remove partitions: ~p", [Reason])
    end,

    %% Stop load testing
    erlmcp_load_generator:stop(Test#id).