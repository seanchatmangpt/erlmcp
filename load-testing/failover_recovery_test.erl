%% @doc Failover and Recovery Testing for erlmcp v3
%% Comprehensive testing of failover mechanisms and recovery procedures
%%
%% Features:
%% - Node failure simulation
%% - Recovery time measurement
%% - Data consistency verification
%% - Service availability monitoring
%% - Recovery strategy testing
%% - Rollback capabilities
%% - Performance degradation tracking

-module(erlmcp_failover_recovery_test).
-author("erlmcp-load-test-team").
-behaviour(gen_server).

%% API exports
-export([start_link/0, run_failover_test/3, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Include
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Records
-record.failover_scenario, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    failure_type :: node_failure | network_partition | process_crash | disk_failure,
    target_nodes :: list(),
    recovery_strategy :: automatic | manual | hybrid,
    recovery_timeout :: integer(), % milliseconds
    consistency_checks :: list(),
    health_check_interval :: integer()
}.

-record.failover_test, {
    id :: binary(),
    scenario :: #failover_scenario{},
    start_time :: integer(),
    failure_time :: integer(),
    recovery_start_time :: integer(),
    recovery_complete_time :: integer(),
    status :: planning | running | failed | completed,
    metrics :: map(),
    consistency_results :: list(),
    recovery_actions :: list(),
    service_availability :: list()
}.

-record.service_status, {
    service_name :: binary(),
    status :: healthy | degraded | failed | recovering,
    last_check :: integer(),
    availability :: float(), % percentage
    response_time :: float(),
    error_rate :: float()
}.

-record.recovery_action, {
    id :: binary(),
    action_type :: restart | failover | rollback | heal,
    target :: binary(),
    start_time :: integer(),
    end_time :: integer(),
    result :: success | failed | timeout,
    details :: map()
}.

%% ============================================================================
%% API FUNCTIONS
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

run_failover_test(TestId, Scenario, LoadConfig) ->
    gen_server:call(?MODULE, {run_failover_test, TestId, Scenario, LoadConfig}, infinity).

stop() ->
    gen_server:call(?MODULE, stop).

%% ============================================================================
%% GEN_SERVER CALLBACKS
%% ============================================================================

init(_Args) ->
    process_flag(trap_exit, true),
    ?LOG_INFO("Failover and recovery test framework initialized"),

    %% Initialize state
    State = #{
        active_tests => [],
        service_health => initialize_service_health(),
        cluster_status => initialize_cluster_status(),
        test_history => [],
        recovery_strategies => initialize_recovery_strategies()
    },

    %% Start monitoring
    erlang:send_after(5000, self(), monitor_services),

    {ok, State}.

handle_call({run_failover_test, TestId, Scenario, LoadConfig}, _From, State) ->
    %% Validate scenario
    case validate_failover_scenario(Scenario) of
        {ok, ValidScenario} ->
            %% Create and start failover test
            Test = create_failover_test(TestId, ValidScenario, LoadConfig),
            NewState = start_failover_test(Test, State),
            {reply, {ok, test_started}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_active_tests, _From, State) ->
    {reply, {ok, maps:get(active_tests, State, [])}, State};

handle_call(get_test_history, _From, State) ->
    {reply, {ok, maps:get(test_history, State, [])}, State};

handle_call(get_service_health, _From, State) ->
    {reply, {ok, maps:get(service_health, State, [])}, State};

handle_call(get_cluster_status, _From, State) ->
    {reply, {ok, maps:get(cluster_status, State, [])}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(monitor_services, State) ->
    %% Monitor service health
    ServiceHealth = monitor_service_health(State),
    ClusterStatus = monitor_cluster_status(State);

    UpdatedState = State#{
        service_health => ServiceHealth,
        cluster_status => ClusterStatus
    },

    %% Schedule next monitoring
    erlang:send_after(5000, self(), monitor_services),
    {noreply, UpdatedState};

handle_info({test_phase, TestId, Phase, Metrics}, State) ->
    %% Handle test phase transitions
    case lists:keyfind(TestId, #failover_test.id, maps:get(active_tests, State)) of
        false ->
            {noreply, State};
        Test ->
            UpdatedTest = Test#failover_test{
                status => Phase,
                metrics => merge_metrics(Test#failover_test.metrics, Metrics)
            },

            case Phase of
                running ->
                    %% Start failure injection
                    erlang:send_after(0, self(), {inject_failure, TestId});
                _ ->
                    ok
            end,

            NewState = State#{
                active_tests => lists:keyreplace(TestId, #failover_test.id,
                                              maps:get(active_tests, State), UpdatedTest)
            },

            {noreply, NewState}
    end;

handle_info({inject_failure, TestId}, State) ->
    %% Inject failure for test
    case lists:keyfind(TestId, #failover_test.id, maps:get(active_tests, State)) of
        false ->
            {noreply, State};
        Test ->
            %% Inject failure based on scenario
            FailureTime = erlang:system_time(millisecond);
            UpdatedTest = Test#failover_test{
                failure_time = FailureTime,
                status = failed
            };

            case inject_failure(Test#failover_test.scenario) of
                {ok, FailureDetails} ->
                    %% Record failure
                    RecordFailure = #{
                        test_id => TestId,
                        failure_type => Test#failover_test.scenario#failover_scenario.failure_type,
                        target_nodes => Test#failover_test.scenario#failover_scenario.target_nodes,
                        failure_time => FailureTime,
                        details => FailureDetails
                    },

                    %% Start recovery monitoring
                    erlang:send_after(0, self(), {monitor_recovery, TestId}),

                    NewState = State#{
                        active_tests => lists:keyreplace(TestId, #failover_test.id,
                                                      maps:get(active_tests, State), UpdatedTest)
                    },

                    {noreply, NewState};
                {error, Reason} ->
                    %% Failed to inject failure
                    ?LOG_ERROR("Failed to inject failure for test ~p: ~p", [TestId, Reason]),
                    {noreply, State}
            end
    end;

handle_info({monitor_recovery, TestId}, State) ->
    %% Monitor recovery process
    case lists:keyfind(TestId, #failover_test.id, maps:get(active_tests, State)) of
        false ->
            {noreply, State};
        Test ->
            %% Check if recovery should start
            RecoveryStartTime = erlang:system_time(millisecond);
            RecoveryDelay = RecoveryStartTime - Test#failover_test.failure_time;

            case RecoveryDelay >= Test#failover_test.scenario#failover_scenario.recovery_timeout of
                true ->
                    %% Start recovery
                    start_recovery(TestId, Test, State);
                false ->
                    %% Continue monitoring
                    erlang:send_after(1000, self(), {monitor_recovery, TestId}),
                    {noreply, State}
            end
    end;

handle_info({recovery_action, ActionId, Result}, State) ->
    %% Handle recovery action completion
    ?LOG_INFO("Recovery action ~p completed: ~p", [ActionId, Result]),

    %% Update test results
    UpdatedState = lists:map(fun(Test) ->
        case lists:any(fun(RA) -> RA#recovery_action.id =:= ActionId end,
                      Test#failover_test.recovery_actions) of
            true ->
                Test;
            false ->
                %% Add new recovery action
                Action = #recovery_action{
                    id = ActionId,
                    action_type = Result#action_type,
                    target = Result#target,
                    start_time = Result#start_time,
                    end_time = Result#end_time,
                    result = Result#result,
                    details = Result#details
                },

                Test#failover_test{
                    recovery_actions = [Action | Test#failover_test.recovery_actions]
                }
        end
    end, maps:get(active_tests, State)),

    State#{
        active_tests => UpdatedState
    };

handle_info({consistency_check, TestId, CheckId, Result}, State) ->
    %% Handle consistency check results
    ?LOG_DEBUG("Consistency check ~p for test ~p: ~p", [CheckId, TestId, Result]),

    UpdatedState = lists:map(fun(Test) ->
        case Test#failover_test.id =:= TestId of
            true ->
                CheckResult = #{
                    check_id => CheckId,
                    timestamp => erlang:system_time(millisecond),
                    result => Result#result,
                    details => Result#details
                },

                Test#failover_test{
                    consistency_results = [CheckResult | Test#failover_test.consistency_results]
                };
            false ->
                Test
        end
    end, maps:get(active_tests, State)),

    State#{
        active_tests => UpdatedState
    };

handle_info({test_complete, TestId, Report}, State) ->
    %% Handle test completion
    ?LOG_INFO("Failover test ~p completed", [TestId]),

    case lists:keyfind(TestId, #failover_test.id, maps:get(active_tests, State)) of
        false ->
            {noreply, State};
        Test ->
            %% Move to history
            NewHistory = [Test | maps:get(test_history, State)],
            NewActive = lists:keydelete(TestId, #failover_test.id, maps:get(active_tests, State)),

            %% Save report
            ReportFile = "/Users/sac/erlmcp/load-testing/failover_test_" ++
                         binary_to_list(TestId) ++ "_report.json",
            file:write_file(ReportFile, jsx:encode(Report)),

            ?LOG_INFO("Test report saved to: ~p", [ReportFile]),

            State#{
                active_tests => NewActive,
                test_history => NewHistory
            }
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cleanup failover tests
    lists:foreach(fun(Test) ->
        rollback_test(Test)
    end, maps:get(active_tests, State)),

    ?LOG_INFO("Failover and recovery test framework terminated", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% PRIVATE FUNCTIONS
%% ============================================================================

validate_failover_scenario(Scenario) ->
    %% Validate failover scenario
    case Scenario#failover_scenario.recovery_strategy of
        automatic ->
            case Scenario#failover_scenario.failure_type of
                node_failure ->
                    {ok, Scenario};
                network_partition ->
                    {ok, Scenario};
                process_crash ->
                    {ok, Scenario};
                disk_failure ->
                    {error, disk_failure_not_supported};
                _ ->
                    {error, unsupported_failure_type}
            end;
        manual ->
            {ok, Scenario};
        hybrid ->
            case Scenario#failover_scenario.failure_type of
                node_failure ->
                    {ok, Scenario};
                network_partition ->
                    {ok, Scenario};
                process_crash ->
                    {ok, Scenario};
                disk_failure ->
                    {error, disk_failure_not_supported};
                _ ->
                    {error, unsupported_failure_type}
            end
    end.

create_failover_test(TestId, Scenario, LoadConfig) ->
    %% Create failover test
    #failover_test{
        id = TestId,
        scenario = Scenario,
        start_time = erlang:system_time(millisecond),
        status = planning,
        metrics = #{},
        consistency_results = [],
        recovery_actions = [],
        service_availability = []
    }.

start_failover_test(Test, State) ->
    %% Start failover test
    ?LOG_INFO("Starting failover test: ~p", [Test#failover_test.id]),

    %% Create load for test
    start_test_load(Test, State);

    %% Add to active tests
    NewActive = [Test | maps:get(active_tests, State)],

    %% Send phase transition
    erlang:send_after(0, self(), {test_phase, Test#failover_test.id, running, #{}}),

    State#{
        active_tests => NewActive
    }.

start_test_load(Test, State) ->
    %% Start test load generation
    LoadConfig = #{
        test_id => Test#failover_test.id,
        target_nodes => Test#failover_test.scenario#failover_scenario.target_nodes,
        load_pattern => generate_load_pattern(Test#failover_test.scenario),
        duration => Test#failover_test.scenario#failover_scenario.recovery_timeout * 2
    },

    %% Start load generator
    erlmcp_load_generator:start(LoadConfig),

    ok.

inject_failure(Scenario) ->
    %% Inject failure based on scenario
    case Scenario#failover_scenario.failure_type of
        node_failure ->
            inject_node_failure(Scenario);
        network_partition ->
            inject_network_partition(Scenario);
        process_crash ->
            inject_process_crash(Scenario);
        disk_failure ->
            inject_disk_failure(Scenario)
    end.

inject_node_failure(Scenario) ->
    %% Inject node failure
    TargetNodes = Scenario#failover_scenario.target_nodes;

    lists:foreach(fun(NodeId) ->
        %% Stop node
        case erlmcp_node:stop(NodeId) of
            ok ->
                ?LOG_INFO("Node ~p stopped", [NodeId]);
            {error, Reason} ->
                ?LOG_ERROR("Failed to stop node ~p: ~p", [NodeId, Reason])
        end
    end, TargetNodes),

    {ok, #{
        action => node_failure,
        nodes => TargetNodes,
        timestamp => erlang:system_time(millisecond)
    }}.

inject_network_partition(Scenario) ->
    %% Inject network partition
    TargetNodes = Scenario#failover_scenario.target_nodes;

    lists:foreach(fun(NodeId) ->
        case erlmcp_network:inject_partition(NodeId, Scenario#recovery_timeout) of
            ok ->
                ?LOG_INFO("Network partition injected on node ~p", [NodeId]);
            {error, Reason} ->
                ?LOG_ERROR("Failed to inject partition on node ~p: ~p", [NodeId, Reason])
        end
    end, TargetNodes),

    {ok, #{
        action => network_partition,
        nodes => TargetNodes,
        duration => Scenario#recovery_timeout,
        timestamp => erlang:system_time(millisecond)
    }}.

inject_process_crash(Scenario) ->
    %% Inject process crash
    TargetNodes = Scenario#failover_scenario.target_nodes;

    lists:foreach(fun(NodeId) ->
        case erlmcp_node:crash_process(NodeId, Scenario#health_check_interval) of
            ok ->
                ?LOG_INFO("Process crash injected on node ~p", [NodeId]);
            {error, Reason} ->
                ?LOG_ERROR("Failed to crash process on node ~p: ~p", [NodeId, Reason])
        end
    end, TargetNodes),

    {ok, #{
        action => process_crash,
        nodes => TargetNodes,
        timestamp => erlang:system_time(millisecond)
    }}.

inject_disk_failure(Scenario) ->
    %% Inject disk failure (not implemented)
    {error, disk_failure_not_supported}.

start_recovery(TestId, Test, State) ->
    %% Start recovery process
    ?LOG_INFO("Starting recovery for test ~p", [TestId]),

    RecoveryStartTime = erlang:system_time(millisecond);
    UpdatedTest = Test#failover_test{
        recovery_start_time = RecoveryStartTime,
        status = recovering
    };

    %% Execute recovery strategy
    case Test#failover_test.scenario#failover_scenario.recovery_strategy of
        automatic ->
            execute_automatic_recovery(TestId, Test, State);
        manual ->
            execute_manual_recovery(TestId, Test, State);
        hybrid ->
            execute_hybrid_recovery(TestId, Test, State)
    end,

    %% Start consistency checks
    start_consistency_checks(TestId, Test);

    NewState = State#{
        active_tests => lists:keyreplace(TestId, #failover_test.id,
                                      maps:get(active_tests, State), UpdatedTest)
    },

    {noreply, NewState}.

execute_automatic_recovery(TestId, Test, State) ->
    %% Execute automatic recovery
    RecoveryAction = execute_recovery_action(failover, Test);

    %% Track recovery action
    erlang:send_after(0, self(), {recovery_action, RecoveryAction#id, RecoveryAction}),

    %% Schedule completion check
    erlang:send_after(Test#scenario#recovery_timeout, self(), {check_recovery_completion, TestId}).

execute_manual_recovery(TestId, Test, State) ->
    %% Execute manual recovery
    %% This would typically trigger external alerts and wait for operator action
    ?LOG_INFO("Manual recovery initiated for test ~p", [TestId]),

    %% Trigger alert
    erlmcp_alerting:send_alert(#{
        type => manual_recovery_required,
        test_id => TestId,
        severity => critical,
        message => "Manual intervention required for recovery"
    }),

    %% Wait for manual intervention
    erlang:send_after(30000, self(), {check_manual_recovery, TestId}).

execute_hybrid_recovery(TestId, Test, State) ->
    %% Execute hybrid recovery
    RecoveryAction = execute_recovery_action(failover, Test);

    %% Track recovery action
    erlang:send_after(0, self(), {recovery_action, RecoveryAction#id, RecoveryAction}),

    %% Also trigger alert
    erlmcp_alerting:send_alert(#{
        type => hybrid_recovery_started,
        test_id => TestId,
        severity => high,
        message => "Hybrid recovery initiated"
    }),

    %% Schedule completion check
    erlang:send_after(Test#scenario#recovery_timeout, self(), {check_recovery_completion, TestId}).

execute_recovery_action(ActionType, Test) ->
    %% Execute recovery action
    ActionId = generate_action_id();
    StartTime = erlang:system_time(millisecond);

    try
        case ActionType of
            failover ->
                Result = execute_failover_action(Test);
            restart ->
                Result = execute_restart_action(Test);
            heal ->
                Result = execute_heal_action(Test)
        end,

        EndTime = erlang:system_time(millisecond);

        #recovery_action{
            id = ActionId,
            action_type = ActionType,
            target = Test#scenario#target_nodes,
            start_time = StartTime,
            end_time = EndTime,
            result = success,
            details = Result
        }

    catch
        Error:Reason ->
            EndTime = erlang:system_time(millisecond);

            #recovery_action{
                id = ActionId,
                action_type = ActionType,
                target = Test#scenario#target_nodes,
                start_time = StartTime,
                end_time = EndTime,
                result = failed,
                details = #{
                    error => {Error, Reason},
                    stack_trace => erlang:get_stacktrace()
                }
            }
    end.

execute_failover_action(Test) ->
    %% Execute failover action
    TargetNodes = Test#scenario#target_nodes;

    lists:foreach(fun(NodeId) ->
        case erlmcp_node:failover(NodeId) of
            ok ->
                ?LOG_INFO("Failover completed on node ~p", [NodeId]);
            {error, Reason} ->
                ?LOG_ERROR("Failover failed on node ~p: ~p", [NodeId, Reason])
        end
    end, TargetNodes),

    #{action => failover, nodes => TargetNodes}.

execute_restart_action(Test) ->
    %% Execute restart action
    TargetNodes = Test#scenario#target_nodes;

    lists:foreach(fun(NodeId) ->
        case erlmcp_node:restart(NodeId) of
            ok ->
                ?LOG_INFO("Restart completed on node ~p", [NodeId]);
            {error, Reason} ->
                ?LOG_ERROR("Restart failed on node ~p: ~p", [NodeId, Reason])
        end
    end, TargetNodes),

    #{action => restart, nodes => TargetNodes}.

execute_heal_action(Test) ->
    %% Execute heal action
    TargetNodes = Test#scenario#target_nodes;

    lists:foreach(fun(NodeId) ->
        case erlmcp_node:heal(NodeId) of
            ok ->
                ?LOG_INFO("Heal completed on node ~p", [NodeId]);
            {error, Reason} ->
                ?LOG_ERROR("Heal failed on node ~p: ~p", [NodeId, Reason])
        end
    end, TargetNodes),

    #{action => heal, nodes => TargetNodes}.

start_consistency_checks(TestId, Test) ->
    %% Start consistency checks
    Checks = Test#scenario#consistency_checks;

    lists:foreach(fun(Check) ->
        erlang:send_after(0, self(), {perform_consistency_check, TestId, Check})
    end, Checks).

perform_consistency_check(TestId, Check) ->
    %% Perform consistency check
    CheckId = generate_check_id();
    StartTime = erlang:system_time(millisecond);

    try
        Result = execute_consistency_check(Check),
        EndTime = erlang:system_time(millisecond);

        CheckResult = #{
            check_id => CheckId,
            result => success,
            details => Result,
            duration => EndTime - StartTime
        };

        erlang:send_after(0, self(), {consistency_check, TestId, CheckId, CheckResult})

    catch
        Error:Reason ->
            EndTime = erlang:system_time(millisecond);

            CheckResult = #{
                check_id => CheckId,
                result => failed,
                details => #{error => {Error, Reason}},
                duration => EndTime - StartTime
            };

            erlang:send_after(0, self(), {consistency_check, TestId, CheckId, CheckResult})
    end.

execute_consistency_check(Check) ->
    %% Execute consistency check
    case Check#check_type of
        data_consistency ->
            check_data_consistency();
        service_consistency ->
            check_service_consistency();
        state_consistency ->
            check_state_consistency();
        _ ->
            {error, unknown_check_type}
    end.

check_data_consistency() ->
    %% Check data consistency across nodes
    Nodes = get_cluster_nodes();
    Consistent = true;

    lists:foldl(fun(NodeId, Acc) ->
        case check_node_data(NodeId) of
            ok ->
                Acc;
            {error, Reason} ->
                ?LOG_ERROR("Data inconsistency on node ~p: ~p", [NodeId, Reason]),
                false
        end
    end, Consistent, Nodes).

check_service_consistency() ->
    %% Check service consistency
    Services = get_active_services();

    lists:foldl(fun(Service, Acc) ->
        case check_service_status(Service) of
            healthy ->
                Acc;
            _ ->
                false
        end
    end, true, Services).

check_state_consistency() ->
    %% Check state consistency
    Nodes = get_cluster_nodes();

    lists:foldl(fun(NodeId, Acc) ->
        case check_node_state(NodeId) of
            consistent ->
                Acc;
            _ ->
                false
        end
    end, true, Nodes).

monitor_service_health(State) ->
    %% Monitor service health
    Services = get_active_services();

    lists:map(fun(Service) ->
        monitor_service(Service)
    end, Services).

monitor_service(Service) ->
    %% Monitor individual service
    StartTime = erlang:system_time(millisecond);

    case check_service_availability(Service) of
        {available, ResponseTime} ->
            #service_status{
                service_name = Service,
                status = healthy,
                last_check = StartTime,
                availability = 100.0,
                response_time = ResponseTime,
                error_rate = 0.0
            };
        {degraded, ResponseTime} ->
            #service_status{
                service_name = Service,
                status = degraded,
                last_check = StartTime,
                availability = 95.0,
                response_time = ResponseTime,
                error_rate = 0.05
            };
        {unavailable, ErrorRate} ->
            #service_status{
                service_name = Service,
                status = failed,
                last_check = StartTime,
                availability = 0.0,
                response_time = 0.0,
                error_rate = ErrorRate
            }
    end.

monitor_cluster_status(State) ->
    %% Monitor cluster status
    Nodes = get_cluster_nodes();
    HealthyNodes = lists:filter(fun(NodeId) -> check_node_health(NodeId) end, Nodes);

    #{
        total_nodes => length(Nodes),
        healthy_nodes => length(HealthyNodes),
        unhealthy_nodes => length(Nodes) - length(HealthyNodes),
        cluster_health => case length(HealthyNodes) / length(Nodes) of
                           Ratio when Ratio > 0.8 -> healthy;
                           Ratio when Ratio > 0.5 -> degraded;
                           _ -> failed
                         end
    }.

initialize_service_health() ->
    %% Initialize service health
    Services = get_active_services();

    lists:map(fun(Service) ->
        #service_status{
            service_name = Service,
            status = healthy,
            last_check => erlang:system_time(millisecond),
            availability => 100.0,
            response_time => 0.0,
            error_rate => 0.0
        }
    end, Services).

initialize_cluster_status() ->
    %% Initialize cluster status
    Nodes = get_cluster_nodes();
    HealthyNodes = lists:filter(fun(NodeId) -> check_node_health(NodeId) end, Nodes);

    #{
        total_nodes => length(Nodes),
        healthy_nodes => length(HealthyNodes),
        unhealthy_nodes => length(Nodes) - length(HealthyNodes),
        cluster_health => case length(HealthyNodes) / length(Nodes) of
                           Ratio when Ratio > 0.8 -> healthy;
                           Ratio when Ratio > 0.5 -> degraded;
                           _ -> failed
                         end
    }.

initialize_recovery_strategies() ->
    %% Initialize recovery strategies
    #{
        automatic => #{
            strategy => automatic,
            timeout => 30000,
            retries => 3,
            actions => [failover, restart, heal]
        },
        manual => #{
            strategy => manual,
            timeout => 300000,
            alert_threshold => 10000
        },
        hybrid => #{
            strategy => hybrid,
            automatic_timeout => 15000,
            manual_timeout => 120000,
            trigger_threshold => 0.5
        }
    }.

generate_action_id() ->
    %% Generate unique action ID
    <<Id:128>> = crypto:strong_rand_bytes(16),
    Id.

generate_check_id() ->
    %% Generate unique check ID
    <<Id:64>> = crypto:strong_rand_bytes(8),
    Id.

generate_load_pattern(Scenario) ->
    %% Generate load pattern based on scenario
    case Scenario#failure_type of
        node_failure ->
            #{
                pattern => spike,
                duration => Scenario#recovery_timeout,
                rate => calculate_failure_load_rate(Scenario),
                tools => generate_failure_tool_set(Scenario)
            };
        network_partition ->
            #{
                pattern => burst,
                duration => Scenario#recovery_timeout * 2,
                rate => calculate_partition_load_rate(Scenario),
                tools => generate_partition_tool_set(Scenario)
            };
        process_crash ->
            #{
                pattern => constant,
                duration => Scenario#recovery_timeout,
                rate => calculate_crash_load_rate(Scenario),
                tools => generate_crash_tool_set(Scenario)
            }
    end.

calculate_failure_load_rate(Scenario) ->
    %% Calculate load rate for failure scenario
    case Scenario#severity of
        low ->
            1000;
        medium ->
            5000;
        high ->
            10000;
        critical ->
            20000
    end.

calculate_partition_load_rate(Scenario) ->
    %% Calculate load rate for partition scenario
    case Scenario#severity of
        low ->
            500;
        medium ->
            2000;
        high ->
            5000;
        critical ->
            10000
    end.

calculate_crash_load_rate(Scenario) ->
    %% Calculate load rate for crash scenario
    case Scenario#severity of
        low ->
            2000;
        medium ->
            5000;
        high ->
            10000;
        critical ->
            15000
    end.

generate_failure_tool_set(Scenario) ->
    %% Generate tool set for failure scenario
    [#{
        tool => file_system,
        weight => 0.3
    }, #{
        tool => shell_command,
        weight => 0.4
    }, #{
        tool => git,
        weight => 0.3
    }].

generate_partition_tool_set(Scenario) ->
    %% Generate tool set for partition scenario
    [#{
        tool => file_search,
        weight => 0.2
    }, #{
        tool => shell_command,
        weight => 0.3
    }, #{
        tool => mermaid,
        weight => 0.2
    }, #{
        tool => file_system,
        weight => 0.3
    }].

generate_crash_tool_set(Scenario) ->
    %% Generate tool set for crash scenario
    [#{
        tool => shell_command,
        weight => 0.5
    }, #{
        tool => file_system,
        weight => 0.3
    }, #{
        tool => git,
        weight => 0.2
    }].

merge_metrics(Existing, New) ->
    %% Merge metrics
    maps:merge(Existing, New).

check_node_health(NodeId) ->
    %% Check node health
    case erlmcp_health_monitor:check_node(NodeId) of
        {ok, healthy} ->
            true;
        _ ->
            false
    end.

get_cluster_nodes() ->
    %% Get cluster nodes
    case erlmcp_cluster:get_nodes() of
        {ok, Nodes} ->
            Nodes;
        _ ->
            [node()]
    end.

get_active_services() ->
    %% Get active services
    case erlmcp_service_registry:get_services() of
        {ok, Services} ->
            Services;
        _ ->
            [mcp, registry, transport]
    end.

check_service_availability(Service) ->
    %% Check service availability
    try
        StartTime = erlang:system_time(millisecond);
        Result = erlmcp_service:call(Service, ping),
        EndTime = erlang:system_time(millisecond);

        case Result of
            pong ->
                {available, EndTime - StartTime};
            _ ->
                {degraded, EndTime - StartTime}
        end
    catch
        _:_ ->
            {unavailable, 1.0}
    end.

check_service_status(Service) ->
    %% Check service status
    case erlmcp_service_registry:get_status(Service) of
        {ok, Status} ->
            Status;
        _ ->
            unknown
    end.

check_node_data(NodeId) ->
    %% Check node data consistency
    case erlmcp_data_checker:check_node(NodeId) of
        {ok, consistent} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

check_node_state(NodeId) ->
    %% Check node state consistency
    case erlmcp_state_checker:check_node(NodeId) of
        {ok, consistent} ->
            consistent;
        _ ->
            inconsistent
    end.

rollback_test(Test) ->
    %% Rollback test changes
    case Test#status of
        running ->
            %% Stop ongoing test
            erlmcp_load_generator:stop(Test#id);
        _ ->
            ok
    end.

merge_metrics(Existing, New) ->
    %% Merge metrics maps
    maps:merge(Existing, New).