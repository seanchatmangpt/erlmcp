%%%-------------------------------------------------------------------
%%% @doc chaos_engineering_examples - Chaos Engineering Examples
%%%
%%% Practical examples of using the erlmcp chaos engineering framework
%%% for resilience testing and failure injection.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chaos_engineering_examples).

-export([
    example_gradual_resilience_test/0,
    example_network_partition_test/0,
    example_memory_recovery_test/0,
    example_combined_chaos/0,
    example_production_gameday/0
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Basic Examples
%%====================================================================

%% @doc Example 1: Gradual resilience test
%% Start with low chaos and gradually increase to stress test system
example_gradual_resilience_test() ->
    ?LOG_INFO("Starting gradual resilience test", []),
    
    Phases = [
        #{rate => 0.05, duration => 60000, description => "5% for 1 minute"},
        #{rate => 0.10, duration => 120000, description => "10% for 2 minutes"},
        #{rate => 0.20, duration => 300000, description => "20% for 5 minutes"}
    ],
    
    Results = lists:map(
        fun(Phase) ->
            ?LOG_INFO("Phase: ~s", [maps:get(description, Phase)]),
            
            Config = Phase#{
                experiment => kill_servers,
                target => erlmcp_server,
                max_blast_radius => maps:get(rate, Phase)
            },
            
            {ok, ExpId} = erlmcp_chaos:run(Config),
            
            % Monitor during phase
            monitor_experiment(ExpId, maps:get(duration, Phase)),
            
            % Get final status
            {ok, Status} = erlmcp_chaos:get_experiment_status(ExpId),
            
            ?LOG_INFO("Phase complete: ~p", [Status]),
            Status
        end,
        Phases
    ),
    
    {ok, Results}.

%% @doc Example 2: Network partition test
%% Test distributed system behavior during network split
example_network_partition_test() ->
    ?LOG_INFO("Starting network partition test", []),
    
    % Get baseline cluster state
    BaselineState = #{
        nodes => erlang:nodes(),
        healthy => true
    },
    
    % Start partition
    {ok, ExpId} = erlmcp_chaos:run(#{
        experiment => network_partition,
        duration => 60000
    }),
    
    ?LOG_INFO("Network partition active", []),
    
    % Check state during partition
    timer:sleep(30000),
    PartitionState = #{
        nodes => erlang:nodes(),
        timestamp => erlang:timestamp()
    },
    
    % Wait for partition to heal
    timer:sleep(35000),
    
    % Check recovery
    RecoveryState = #{
        nodes => erlang:nodes(),
        timestamp => erlang:timestamp()
    },
    
    #{
        baseline => BaselineState,
        during_partition => PartitionState,
        after_recovery => RecoveryState
    }.

%% @doc Example 3: Memory recovery test
%% Test system recovery from memory pressure
example_memory_recovery_test() ->
    ?LOG_INFO("Starting memory recovery test", []),
    
    % Baseline
    BaselineMemory = get_memory_stats(),
    
    % Start memory exhaustion
    {ok, ExpId} = erlmcp_chaos:run(#{
        experiment => resource_memory,
        target_percent => 0.85,
        duration => 60000,
        auto_rollback => true
    }),
    
    % Monitor memory during experiment
    MemorySamples = collect_memory_samples(ExpId, 60000, 5000),
    
    % Check recovery
    timer:sleep(10000),
    RecoveredMemory = get_memory_stats(),
    
    #{
        baseline => BaselineMemory,
        samples => MemorySamples,
        recovered => RecoveredMemory,
        recovery_percentage => calculate_recovery_percentage(
            BaselineMemory, RecoveredMemory
        )
    }.

%%====================================================================
%% Advanced Examples
%%====================================================================

%% @doc Example 4: Combined chaos (multiple simultaneous experiments)
example_combined_chaos() ->
    ?LOG_INFO("Starting combined chaos test", []),
    
    % Start multiple experiments
    Experiments = [
        {network_latency, #{
            experiment => network_latency,
            latency => 100,
            rate => 0.2,
            duration => 300000
        }},
        {process_kills, #{
            experiment => kill_random,
            rate => 0.05,
            duration => 300000
        }},
        {memory_pressure, #{
            experiment => resource_memory,
            target_percent => 0.80,
            duration => 300000
        }}
    ],
    
    % Start all experiments
    ExperimentIds = lists:map(
        fun({Name, Config}) ->
            ?LOG_INFO("Starting experiment: ~p", [Name]),
            {ok, ExpId} = erlmcp_chaos:run(Config),
            {Name, ExpId}
        end,
        Experiments
    ),
    
    % Monitor system under combined chaos
    MonitorResults = monitor_system_health(300000),
    
    % Stop all experiments
    lists:foreach(
        fun({Name, ExpId}) ->
            ?LOG_INFO("Stopping experiment: ~p", [Name]),
            erlmcp_chaos:stop_experiment(ExpId)
        end,
        ExperimentIds
    ),
    
    #{
        experiments => ExperimentIds,
        system_health => MonitorResults
    }.

%% @doc Example 5: Production game day simulation
%% Simulate real production chaos scenarios
example_production_gameday() ->
    ?LOG_INFO("Starting production game day", []),
    
    Scenarios = [
        % Scenario 1: Database connection loss
        #{
            name => "db_connection_loss",
            experiment => kill_servers,
            target => database_pool,
            rate => 0.3,
            duration => 120000,
            expected_behavior => "Circuit breaker opens, graceful degradation"
        },
        
        % Scenario 2: API gateway slowdown
        #{
            name => "api_gateway_slowdown",
            experiment => network_latency,
            latency => 2000,
            rate => 0.5,
            duration => 180000,
            expected_behavior => "Request timeouts, retries trigger"
        },
        
        % Scenario 3: Memory spike
        #{
            name => "memory_spike",
            experiment => resource_memory,
            target_percent => 0.90,
            duration => 120000,
            expected_behavior => "Backpressure activates, requests refused"
        }
    ],
    
    % Run each scenario and collect results
    Results = lists:map(
        fun(Scenario) ->
            run_scenario_with_validation(Scenario)
        end,
        Scenarios
    ),
    
    % Generate report
    Report = generate_gameday_report(Results),
    
    ?LOG_INFO("Production game day complete", []),
    Report.

%%====================================================================
%% Helper Functions
%%====================================================================

-spec monitor_experiment(binary(), pos_integer()) -> ok.
monitor_experiment(ExperimentId, Duration) ->
    EndTime = erlang:system_time(millisecond) + Duration,
    monitor_loop(ExperimentId, EndTime).

-spec monitor_loop(binary(), integer()) -> ok.
monitor_loop(ExperimentId, EndTime) ->
    Now = erlang:system_time(millisecond),
    case Now >= EndTime of
        true ->
            ok;
        false ->
            case erlmcp_chaos:get_experiment_status(ExperimentId) of
                {ok, Status} ->
                    Incidents = maps:get(incidents, Status),
                    case length(Incidents) > 0 of
                        true ->
                            ?LOG_WARNING("Incidents detected: ~p", [Incidents]);
                        false ->
                            ok
                    end;
                _ ->
                    ok
            end,
            timer:sleep(5000),
            monitor_loop(ExperimentId, EndTime)
    end.

-spec get_memory_stats() -> map().
get_memory_stats() ->
    #{
        total => erlang:memory(total),
        processes => erlang:memory(processes),
        system => erlang:memory(system),
        binary => erlang:memory(binary),
        timestamp => erlang:timestamp()
    }.

-spec collect_memory_samples(binary(), pos_integer(), pos_integer()) -> [map()].
collect_memory_samples(ExperimentId, Duration, Interval) ->
    EndTime = erlang:system_time(millisecond) + Duration,
    collect_samples_loop(ExperimentId, EndTime, Interval, []).

-spec collect_samples_loop(binary(), integer(), pos_integer(), [map()]) -> [map()].
collect_samples_loop(_ExperimentId, EndTime, _Interval, Samples) when erlang:system_time(millisecond) >= EndTime ->
    lists:reverse(Samples);
collect_samples_loop(ExperimentId, EndTime, Interval, Samples) ->
    Sample = get_memory_stats(),
    timer:sleep(Interval),
    collect_samples_loop(ExperimentId, EndTime, Interval, [Sample | Samples]).

-spec calculate_recovery_percentage(map(), map()) -> float().
calculate_recovery_percentage(Baseline, Recovered) ->
    BaselineTotal = maps:get(total, Baseline),
    RecoveredTotal = maps:get(total, Recovered),
    (BaselineTotal / max(1, RecoveredTotal)) * 100.

-spec monitor_system_health(pos_integer()) -> map().
monitor_system_health(Duration) ->
    EndTime = erlang:system_time(millisecond) + Duration,
    Samples = collect_health_samples(EndTime, []),
    
    #{
        sample_count => length(Samples),
        avg_healthy_percentage => calculate_avg_health(Samples),
        incidents => count_health_incidents(Samples)
    }.

-spec collect_health_samples(integer(), [map()]) -> [map()].
collect_health_samples(EndTime, Samples) when erlang:system_time(millisecond) >= EndTime ->
    lists:reverse(Samples);
collect_health_samples(EndTime, Samples) ->
    try
        Health = erlmcp_health_monitor:get_system_health(),
        timer:sleep(10000),
        collect_health_samples(EndTime, [Health | Samples])
    catch
        _:_ ->
            collect_health_samples(EndTime, Samples)
    end.

-spec calculate_avg_health([map()]) -> float().
calculate_avg_health([]) ->
    0.0;
calculate_avg_health(Samples) ->
    HealthyCount = length([S || S <- Samples, 
        maps:get(overall_status, S, unknown) =:= healthy]),
    (HealthyCount / length(Samples)) * 100.

-spec count_health_incidents([map()]) -> non_neg_integer().
count_health_incidents(Samples) ->
    length([S || S <- Samples, 
        maps:get(overall_status, S, unknown) =:= unhealthy]).

-spec run_scenario_with_validation(map()) -> map().
run_scenario_with_validation(Scenario) ->
    Name = maps:get(name, Scenario),
    ExpectedBehavior = maps:get(expected_behavior, Scenario),
    
    ?LOG_INFO("Running scenario: ~p", [Name]),
    
    % Remove non-config keys
    Config = maps:without([name, expected_behavior], Scenario),
    
    % Run experiment
    {ok, ExpId} = erlmcp_chaos:run(Config),
    
    % Monitor
    Duration = maps:get(duration, Config),
    HealthSamples = collect_health_samples(
        erlang:system_time(millisecond) + Duration, 
        []
    ),
    
    % Get final status
    {ok, Status} = erlmcp_chaos:get_experiment_status(ExpId),
    
    #{
        scenario => Name,
        expected => ExpectedBehavior,
        status => Status,
        health_samples => HealthSamples,
        passed => validate_scenario(Status, ExpectedBehavior)
    }.

-spec validate_scenario(map(), string()) -> boolean().
validate_scenario(Status, _ExpectedBehavior) ->
    % Simple validation: experiment completed without excessive incidents
    Incidents = maps:get(incidents, Status, []),
    length(Incidents) < 10.

-spec generate_gameday_report([map()]) -> map().
generate_gameday_report(Results) ->
    TotalScenarios = length(Results),
    PassedScenarios = length([R || R <- Results, maps:get(passed, R, false)]),
    
    #{
        timestamp => erlang:timestamp(),
        total_scenarios => TotalScenarios,
        passed => PassedScenarios,
        failed => TotalScenarios - PassedScenarios,
        success_rate => (PassedScenarios / max(1, TotalScenarios)) * 100,
        scenarios => Results
    }.
