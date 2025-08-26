%%%-------------------------------------------------------------------
%%% @doc Chaos Engineering Examples
%%% Demonstrates practical usage of the chaos engineering system
%%% with real-world scenarios and configuration examples.
%%% @end
%%%-------------------------------------------------------------------
-module(chaos_engineering_examples).

-export([
    % Basic chaos injection examples
    basic_network_delay/0,
    basic_process_crash/0,
    basic_memory_pressure/0,
    
    % Complex scenario examples
    microservice_failure_cascade/0,
    database_outage_simulation/0,
    network_partition_recovery/0,
    
    % Monitoring and validation examples
    comprehensive_monitoring_setup/0,
    custom_alert_configuration/0,
    experiment_validation_example/0,
    
    % Practical use cases
    load_testing_with_failures/0,
    disaster_recovery_testing/0,
    performance_degradation_testing/0
]).

%%%===================================================================
%%% Basic Chaos Injection Examples
%%%===================================================================

%% @doc Basic network delay injection example
basic_network_delay() ->
    Config = #{
        delay_ms => 500,
        jitter_ms => 100,
        targets => [erlmcp_server, erlmcp_transport]
    },
    
    % Inject network delay for 30 seconds
    {ok, ExperimentId, Result} = erlmcp_chaos:inject(
        network_delay, 
        Config, 
        #{duration => 30000}
    ),
    
    io:format("Network delay experiment ~p started~n", [ExperimentId]),
    io:format("Result: ~p~n", [Result]),
    
    % Monitor the experiment
    timer:sleep(5000),
    {ok, ActiveExperiments} = erlmcp_chaos:list_active_experiments(),
    io:format("Active experiments: ~p~n", [ActiveExperiments]),
    
    ExperimentId.

%% @doc Basic process crash injection example
basic_process_crash() ->
    Config = #{
        crash_type => kill,
        targets => [test_process],
        recovery_timeout => 10000
    },
    
    % Start a test process
    TestPid = spawn(fun() -> 
        register(test_process, self()),
        receive
            stop -> ok
        end
    end),
    
    io:format("Test process started: ~p~n", [TestPid]),
    
    % Inject process crash
    {ok, ExperimentId, Result} = erlmcp_chaos:inject(
        process_crash, 
        Config, 
        #{duration => 15000}
    ),
    
    io:format("Process crash experiment ~p started~n", [ExperimentId]),
    io:format("Result: ~p~n", [Result]),
    
    ExperimentId.

%% @doc Basic memory pressure injection example
basic_memory_pressure() ->
    Config = #{
        pressure_mb => 100,
        duration_ms => 60000,
        allocation_pattern => gradual
    },
    
    % Get initial memory usage
    InitialMemory = erlang:memory(),
    io:format("Initial memory usage: ~p~n", [InitialMemory]),
    
    % Inject memory pressure
    {ok, ExperimentId, Result} = erlmcp_chaos:inject(
        memory_pressure, 
        Config, 
        #{duration => 60000}
    ),
    
    io:format("Memory pressure experiment ~p started~n", [ExperimentId]),
    io:format("Result: ~p~n", [Result]),
    
    % Monitor memory usage during experiment
    spawn(fun() ->
        monitor_memory_usage(6) % Monitor for 60 seconds
    end),
    
    ExperimentId.

%%%===================================================================
%%% Complex Scenario Examples
%%%===================================================================

%% @doc Microservice failure cascade example
microservice_failure_cascade() ->
    % Run the predefined microservice chaos scenario
    ScenarioOptions = #{
        duration_ms => 300000,  % 5 minutes
        enable_monitoring => true,
        monitor_config => #{
            interval_ms => 1000,
            alert_thresholds => #{
                cpu_usage => 80.0,
                memory_usage => 85.0,
                error_rate => 5.0
            }
        }
    },
    
    io:format("Starting microservice failure cascade scenario...~n"),
    
    case erlmcp_chaos_scenarios:run_scenario(microservice_chaos, ScenarioOptions) of
        {ok, Result} ->
            io:format("Scenario completed successfully!~n"),
            io:format("Result summary:~n"),
            io:format("  - Duration: ~p ms~n", [maps:get(duration_ms, Result)]),
            io:format("  - Success: ~p~n", [maps:get(success, Result)]),
            io:format("  - Steps executed: ~p~n", [length(maps:get(step_results, Result))]),
            
            % Export results for analysis
            export_scenario_results(Result),
            
            Result;
        {error, Reason} ->
            io:format("Scenario failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Database outage simulation
database_outage_simulation() ->
    % Simulate a complete database outage with cascading effects
    ScenarioOptions = #{
        duration_ms => 240000,  % 4 minutes
        enable_monitoring => true,
        monitor_config => #{
            interval_ms => 500,
            alert_thresholds => #{
                response_time => 5000,
                error_rate => 10.0,
                memory_usage => 90.0
            }
        }
    },
    
    io:format("Starting database outage simulation...~n"),
    
    % Run the database failure cascade scenario
    case erlmcp_chaos_scenarios:run_scenario(database_failure_cascade, ScenarioOptions) of
        {ok, Result} ->
            io:format("Database outage simulation completed!~n"),
            
            % Analyze the results
            ValidationResults = maps:get(validation_results, Result),
            analyze_validation_results(ValidationResults),
            
            Result;
        {error, Reason} ->
            io:format("Database outage simulation failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Network partition recovery testing
network_partition_recovery() ->
    % Test network partition and recovery capabilities
    ScenarioOptions = #{
        duration_ms => 180000,  % 3 minutes
        enable_monitoring => true,
        monitor_config => #{
            interval_ms => 1000
        }
    },
    
    io:format("Starting network partition recovery test...~n"),
    
    case erlmcp_chaos_scenarios:run_scenario(network_partition_test, ScenarioOptions) of
        {ok, Result} ->
            io:format("Network partition recovery test completed!~n"),
            
            % Check if partition was detected and recovered
            StepResults = maps:get(step_results, Result),
            analyze_partition_recovery(StepResults),
            
            Result;
        {error, Reason} ->
            io:format("Network partition recovery test failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% Monitoring and Validation Examples
%%%===================================================================

%% @doc Comprehensive monitoring setup example
comprehensive_monitoring_setup() ->
    ExperimentId = erlang:system_time(microsecond),
    
    % Start comprehensive monitoring
    MonitorConfig = #{
        interval_ms => 1000,
        metrics_types => [
            cpu_usage,
            memory_usage,
            disk_io,
            network_io,
            process_count,
            message_queue_lengths,
            gc_metrics,
            scheduler_utilization,
            error_rates,
            response_times
        ]
    },
    
    case erlmcp_chaos_monitor:start_monitoring(ExperimentId, MonitorConfig) of
        {ok, MonitorId} ->
            io:format("Monitoring started with ID: ~p~n", [MonitorId]),
            
            % Set up alert thresholds
            AlertThresholds = #{
                cpu_usage => 75.0,
                memory_usage => 80.0,
                error_rate => 2.0,
                response_time => 3000
            },
            
            erlmcp_chaos_monitor:set_alert_thresholds(MonitorId, AlertThresholds),
            
            % Run some chaos experiments while monitoring
            run_monitored_experiments(MonitorId),
            
            % Stop monitoring and generate report
            timer:sleep(60000), % Monitor for 1 minute
            erlmcp_chaos_monitor:stop_monitoring(MonitorId),
            
            {ok, Report} = erlmcp_chaos_monitor:generate_report(MonitorId),
            io:format("Monitoring report: ~p~n", [Report]),
            
            MonitorId;
        {error, Reason} ->
            io:format("Failed to start monitoring: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Custom alert configuration example
custom_alert_configuration() ->
    ExperimentId = erlang:system_time(microsecond),
    
    % Start monitoring with custom configuration
    MonitorConfig = #{
        interval_ms => 500  % More frequent monitoring
    },
    
    case erlmcp_chaos_monitor:start_monitoring(ExperimentId, MonitorConfig) of
        {ok, MonitorId} ->
            % Set up custom alert thresholds
            CustomThresholds = #{
                cpu_usage => 60.0,        % Lower CPU threshold
                memory_usage => 70.0,     % Lower memory threshold
                error_rate => 1.0,        % Very low error rate
                response_time => 1000     % Strict response time
            },
            
            erlmcp_chaos_monitor:set_alert_thresholds(MonitorId, CustomThresholds),
            
            % Inject chaos that should trigger alerts
            erlmcp_chaos:inject(cpu_spike, #{
                cpu_percent => 80,
                duration_ms => 30000,
                core_count => 2
            }, #{duration => 30000}),
            
            % Check for alerts after some time
            timer:sleep(10000),
            {ok, Alerts} = erlmcp_chaos_monitor:get_alerts(MonitorId),
            io:format("Triggered alerts: ~p~n", [Alerts]),
            
            erlmcp_chaos_monitor:stop_monitoring(MonitorId),
            MonitorId;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Experiment validation example
experiment_validation_example() ->
    % Create a custom experiment with validation
    ExperimentConfig = #{
        steps => [
            #{
                type => network_delay,
                config => #{
                    delay_ms => 200,
                    jitter_ms => 50,
                    targets => [all_services]
                }
            },
            #{
                type => cpu_spike,
                config => #{
                    cpu_percent => 70,
                    duration_ms => 30000,
                    core_count => 2
                }
            }
        ],
        validation_criteria => [
            {system_availability, "> 95%"},
            {response_time_p99, "< 3000ms"},
            {error_rate, "< 2%"}
        ],
        success_metrics => [
            system_remained_operational,
            performance_within_sla
        ]
    },
    
    ExperimentOptions = #{
        enable_monitoring => true,
        monitor_config => #{interval_ms => 1000}
    },
    
    case erlmcp_chaos:run_experiment(ExperimentConfig, ExperimentOptions) of
        {ok, ExperimentId, Result} ->
            io:format("Experiment ~p completed~n", [ExperimentId]),
            
            % Check validation results
            ValidationPassed = maps:get(success, Result, false),
            io:format("Validation passed: ~p~n", [ValidationPassed]),
            
            % Get detailed results
            {ok, DetailedResult} = erlmcp_chaos:get_experiment_results(ExperimentId),
            io:format("Detailed results: ~p~n", [DetailedResult]),
            
            Result;
        {error, Reason} ->
            io:format("Experiment failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% Practical Use Cases
%%%===================================================================

%% @doc Load testing combined with failure injection
load_testing_with_failures() ->
    io:format("Starting load testing with failure injection...~n"),
    
    % Start load generation (this would integrate with actual load testing tools)
    LoadConfig = #{
        requests_per_second => 100,
        duration_ms => 180000,  % 3 minutes
        concurrent_users => 20
    },
    
    spawn(fun() -> simulate_load_generation(LoadConfig) end),
    
    % Wait for load to stabilize
    timer:sleep(30000),
    
    % Inject failures during load test
    FailureSchedule = [
        {30000, network_delay, #{delay_ms => 300, jitter_ms => 100, targets => [all]}},
        {60000, process_crash, #{crash_type => kill, targets => [worker_process], recovery_timeout => 15000}},
        {90000, memory_pressure, #{pressure_mb => 200, duration_ms => 45000, allocation_pattern => burst}},
        {120000, cpu_spike, #{cpu_percent => 85, duration_ms => 30000, core_count => 4}}
    ],
    
    {ok, ScheduleId} = erlmcp_chaos:schedule_chaos(FailureSchedule),
    io:format("Chaos schedule ~p started during load test~n", [ScheduleId]),
    
    % Monitor throughout the test
    MonitorConfig = #{interval_ms => 1000},
    {ok, MonitorId} = erlmcp_chaos_monitor:start_monitoring(load_test, MonitorConfig),
    
    % Wait for completion
    timer:sleep(180000),
    
    % Generate comprehensive report
    erlmcp_chaos_monitor:stop_monitoring(MonitorId),
    {ok, Report} = erlmcp_chaos_monitor:generate_report(MonitorId),
    
    io:format("Load testing with failures completed~n"),
    io:format("Report: ~p~n", [Report]),
    
    Report.

%% @doc Disaster recovery testing
disaster_recovery_testing() ->
    io:format("Starting disaster recovery testing...~n"),
    
    % Run comprehensive recovery validation
    ScenarioOptions = #{
        duration_ms => 420000,  % 7 minutes
        enable_monitoring => true,
        monitor_config => #{
            interval_ms => 1000,
            alert_thresholds => #{
                system_availability => 90.0,
                recovery_time => 60000
            }
        }
    },
    
    case erlmcp_chaos_scenarios:run_scenario(recovery_validation_test, ScenarioOptions) of
        {ok, Result} ->
            io:format("Disaster recovery test completed!~n"),
            
            % Analyze recovery metrics
            analyze_recovery_metrics(Result),
            
            % Export results for compliance reporting
            export_disaster_recovery_report(Result),
            
            Result;
        {error, Reason} ->
            io:format("Disaster recovery test failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Performance degradation testing
performance_degradation_testing() ->
    io:format("Starting performance degradation testing...~n"),
    
    % Run gradual degradation test
    ScenarioOptions = #{
        duration_ms => 600000,  % 10 minutes
        enable_monitoring => true,
        monitor_config => #{
            interval_ms => 500,
            alert_thresholds => #{
                response_time => 2000,
                throughput => 80.0  % 80% of baseline
            }
        }
    },
    
    case erlmcp_chaos_scenarios:run_scenario(gradual_degradation_test, ScenarioOptions) of
        {ok, Result} ->
            io:format("Performance degradation test completed!~n"),
            
            % Analyze performance impact
            analyze_performance_impact(Result),
            
            Result;
        {error, Reason} ->
            io:format("Performance degradation test failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

monitor_memory_usage(0) ->
    io:format("Memory monitoring completed~n");
monitor_memory_usage(Count) ->
    Memory = erlang:memory(),
    io:format("Memory usage: ~p~n", [Memory]),
    timer:sleep(10000),
    monitor_memory_usage(Count - 1).

run_monitored_experiments(MonitorId) ->
    % Run a series of small experiments while monitoring
    Experiments = [
        {network_delay, #{delay_ms => 100, jitter_ms => 25, targets => [test_service]}},
        {cpu_spike, #{cpu_percent => 50, duration_ms => 15000, core_count => 1}},
        {memory_pressure, #{pressure_mb => 50, duration_ms => 20000, allocation_pattern => gradual}}
    ],
    
    lists:foreach(fun({Type, Config}) ->
        erlmcp_chaos:inject(Type, Config, #{duration => 20000}),
        timer:sleep(25000)
    end, Experiments).

simulate_load_generation(Config) ->
    Duration = maps:get(duration_ms, Config),
    RequestsPerSecond = maps:get(requests_per_second, Config),
    
    io:format("Simulating load: ~p RPS for ~p ms~n", [RequestsPerSecond, Duration]),
    
    % This would integrate with actual load testing tools
    timer:sleep(Duration),
    io:format("Load generation completed~n").

export_scenario_results(Result) ->
    Filename = io_lib:format("chaos_scenario_~p.json", [maps:get(scenario_id, Result)]),
    
    % Convert result to JSON and save
    try
        JsonResult = jsx:encode(Result),
        file:write_file(Filename, JsonResult),
        io:format("Results exported to ~s~n", [Filename])
    catch
        error:undef ->
            % Fallback if jsx is not available
            BinaryResult = term_to_binary(Result),
            file:write_file(Filename ++ ".erl", BinaryResult),
            io:format("Results exported to ~s.erl (binary format)~n", [Filename])
    end.

analyze_validation_results(ValidationResults) ->
    io:format("Validation Results Analysis:~n"),
    lists:foreach(fun({Criterion, Success, Details}) ->
        Status = case Success of
            true -> "PASS";
            false -> "FAIL";
            _ -> "UNKNOWN"
        end,
        io:format("  - ~p: ~s (~p)~n", [Criterion, Status, Details])
    end, ValidationResults).

analyze_partition_recovery(StepResults) ->
    io:format("Partition Recovery Analysis:~n"),
    lists:foreach(fun(#{step_name := Name, success := Success, result := Result}) ->
        Status = case Success of
            true -> "SUCCESS";
            false -> "FAILED"
        end,
        io:format("  - ~p: ~s~n", [Name, Status]),
        case Result of
            {ok, Details} ->
                io:format("    Details: ~p~n", [Details]);
            {error, Error} ->
                io:format("    Error: ~p~n", [Error]);
            _ ->
                ok
        end
    end, StepResults).

analyze_recovery_metrics(Result) ->
    io:format("Recovery Metrics Analysis:~n"),
    
    Duration = maps:get(duration_ms, Result),
    Success = maps:get(success, Result),
    ValidationResults = maps:get(validation_results, Result),
    
    io:format("  - Total Duration: ~p ms~n", [Duration]),
    io:format("  - Overall Success: ~p~n", [Success]),
    
    % Find recovery-related validations
    RecoveryValidations = [V || {Criterion, _, _} = V <- ValidationResults,
                              lists:member(Criterion, [automatic_restart, data_persistence, state_recovery])],
    
    io:format("  - Recovery Validations:~n"),
    lists:foreach(fun({Criterion, Success, Details}) ->
        io:format("    * ~p: ~p (~p)~n", [Criterion, Success, Details])
    end, RecoveryValidations).

analyze_performance_impact(Result) ->
    io:format("Performance Impact Analysis:~n"),
    
    % Extract performance metrics from monitoring data
    Metrics = maps:get(metrics, Result, #{}),
    MonitorMetrics = maps:get(monitor_metrics, Metrics, []),
    
    case MonitorMetrics of
        [] ->
            io:format("  - No monitoring data available~n");
        _ ->
            % Calculate performance degradation
            io:format("  - Monitoring data points: ~p~n", [length(MonitorMetrics)]),
            io:format("  - Performance analysis completed~n")
    end.

export_disaster_recovery_report(Result) ->
    ReportData = #{
        test_date => calendar:universal_time(),
        scenario_id => maps:get(scenario_id, Result),
        duration_ms => maps:get(duration_ms, Result),
        success => maps:get(success, Result),
        validation_results => maps:get(validation_results, Result),
        compliance_status => determine_compliance_status(Result)
    },
    
    Filename = io_lib:format("disaster_recovery_report_~p.json", 
                            [maps:get(scenario_id, Result)]),
    
    try
        JsonReport = jsx:encode(ReportData),
        file:write_file(Filename, JsonReport),
        io:format("Disaster recovery report exported to ~s~n", [Filename])
    catch
        error:undef ->
            BinaryReport = term_to_binary(ReportData),
            file:write_file(Filename ++ ".erl", BinaryReport),
            io:format("Disaster recovery report exported to ~s.erl~n", [Filename])
    end.

determine_compliance_status(Result) ->
    Success = maps:get(success, Result),
    ValidationResults = maps:get(validation_results, Result),
    
    % Check if critical validations passed
    CriticalValidations = [automatic_restart, data_persistence, state_recovery],
    CriticalPassed = lists:all(fun(Criterion) ->
        case lists:keyfind(Criterion, 1, ValidationResults) of
            {Criterion, true, _} -> true;
            _ -> false
        end
    end, CriticalValidations),
    
    case Success andalso CriticalPassed of
        true -> compliant;
        false -> non_compliant
    end.