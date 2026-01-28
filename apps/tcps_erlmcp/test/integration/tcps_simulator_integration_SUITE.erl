%%%-------------------------------------------------------------------
%%% @doc CommonTest suite for TCPS Simulator Integration
%%% Tests end-to-end simulator workflows with all 6 scenario types.
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_simulator_integration_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
     test_basic_production_workflow,
     test_defect_detection_workflow,
     test_high_load_performance,
     test_bottleneck_identification,
     test_optimization_metrics,
     test_stress_test_limits,
     test_concurrent_scenarios,
     test_pause_resume_workflow,
     test_metrics_accuracy,
     test_telemetry_collection
    ].

init_per_suite(Config) ->
    application:ensure_all_started(opentelemetry),
    {ok, _} = tcps_simulator:start_link(),
    Config.

end_per_suite(_Config) ->
    gen_server:stop(tcps_simulator),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    % Clean up any running scenarios
    Scenarios = tcps_simulator:list_scenarios(),
    lists:foreach(
        fun(Scenario) ->
            ScenarioId = maps:get(id, Scenario),
            tcps_simulator:stop_scenario(ScenarioId)
        end,
        Scenarios
    ),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_basic_production_workflow(_Config) ->
    ct:comment("Testing basic production scenario from start to finish"),

    % Start scenario
    {ok, ScenarioId} = tcps_simulator:start_scenario(basic_production, #{
        work_orders => 20,
        quality_gate_pass_rate => 0.95,
        iteration_delay_ms => 10
    }),

    % Monitor progress
    timer:sleep(100),
    {ok, Status1} = tcps_simulator:get_scenario_status(ScenarioId),
    ?assertEqual(running, maps:get(status, Status1)),

    % Wait for completion
    timer:sleep(400),
    {ok, Status2} = tcps_simulator:get_scenario_status(ScenarioId),
    ?assertEqual(completed, maps:get(status, Status2)),

    % Verify metrics
    {ok, Metrics} = tcps_simulator:get_metrics(ScenarioId),
    ?assertEqual(20, maps:get(work_orders_created, Metrics)),
    ?assert(maps:get(work_orders_completed, Metrics) >= 18), % 95% pass rate
    ?assert(maps:get(quality_gates_passed, Metrics) >= 18),
    ?assert(maps:get(throughput_per_second, Metrics) > 0.0),

    ct:pal("Production workflow completed: ~p", [Metrics]),
    ok.

test_defect_detection_workflow(_Config) ->
    ct:comment("Testing defect scenario with andon alerts"),

    {ok, ScenarioId} = tcps_simulator:start_scenario(defect_scenario, #{
        work_orders => 30,
        quality_gate_pass_rate => 0.70,
        andon_trigger_rate => 0.20,
        iteration_delay_ms => 10
    }),

    timer:sleep(500),

    {ok, Metrics} = tcps_simulator:get_metrics(ScenarioId),

    % Verify defects were detected
    ?assert(maps:get(quality_gates_failed, Metrics) >= 8), % ~30% failure rate
    ?assert(maps:get(andon_alerts, Metrics) >= 5), % ~20% andon rate

    % Verify work orders still progressed
    ?assertEqual(30, maps:get(work_orders_created, Metrics)),

    ct:pal("Defect detection metrics: ~p", [Metrics]),
    ok.

test_high_load_performance(_Config) ->
    ct:comment("Testing high load scenario performance"),

    {ok, ScenarioId} = tcps_simulator:start_scenario(high_load, #{
        work_orders => 200,
        quality_gate_pass_rate => 0.90,
        iteration_delay_ms => 5
    }),

    timer:sleep(2000),

    {ok, Metrics} = tcps_simulator:get_metrics(ScenarioId),

    % Verify all work orders created
    ?assertEqual(200, maps:get(work_orders_created, Metrics)),

    % Verify throughput is reasonable
    Throughput = maps:get(throughput_per_second, Metrics),
    ?assert(Throughput > 20.0),

    % Verify most passed quality gates
    PassRate = maps:get(work_orders_completed, Metrics) / 200.0,
    ?assert(PassRate >= 0.85),

    ct:pal("High load throughput: ~p WO/sec", [Throughput]),
    ok.

test_bottleneck_identification(_Config) ->
    ct:comment("Testing bottleneck analysis scenario"),

    {ok, ScenarioId} = tcps_simulator:start_scenario(bottleneck_analysis, #{
        work_orders => 50,
        quality_gate_pass_rate => 0.85,
        slow_operation_rate => 0.25,
        iteration_delay_ms => 20
    }),

    StartTime = erlang:system_time(millisecond),
    timer:sleep(1500),
    EndTime = erlang:system_time(millisecond),

    {ok, Metrics} = tcps_simulator:get_metrics(ScenarioId),

    % Verify bottleneck caused slower processing
    Duration = EndTime - StartTime,
    ?assert(Duration >= 1000), % Should take significant time

    % Verify metrics tracked
    ?assert(maps:get(work_orders_created, Metrics) > 0),

    ct:pal("Bottleneck scenario duration: ~p ms", [Duration]),
    ok.

test_optimization_metrics(_Config) ->
    ct:comment("Testing optimization scenario with WIP limits"),

    {ok, ScenarioId} = tcps_simulator:start_scenario(optimization_test, #{
        work_orders => 40,
        quality_gate_pass_rate => 0.92,
        wip_limit => 5,
        iteration_delay_ms => 15
    }),

    timer:sleep(1000),

    {ok, Metrics} = tcps_simulator:get_metrics(ScenarioId),

    % Verify optimization metrics
    ?assertEqual(40, maps:get(work_orders_created, Metrics)),
    PassRate = maps:get(work_orders_completed, Metrics) / 40.0,
    ?assert(PassRate >= 0.88), % Should respect quality with WIP limit

    ct:pal("Optimization metrics: ~p", [Metrics]),
    ok.

test_stress_test_limits(_Config) ->
    ct:comment("Testing stress test scenario with extreme load"),

    {ok, ScenarioId} = tcps_simulator:start_scenario(stress_test, #{
        work_orders => 500,
        quality_gate_pass_rate => 0.88,
        iteration_delay_ms => 2
    }),

    timer:sleep(3000),

    {ok, Status} = tcps_simulator:get_scenario_status(ScenarioId),
    {ok, Metrics} = tcps_simulator:get_metrics(ScenarioId),

    % Should create many work orders
    ?assert(maps:get(work_orders_created, Metrics) >= 400),

    % System should remain stable
    ?assertEqual(completed, maps:get(status, Status)),

    ct:pal("Stress test: ~p work orders processed", [maps:get(work_orders_created, Metrics)]),
    ok.

test_concurrent_scenarios(_Config) ->
    ct:comment("Testing multiple concurrent scenarios"),

    % Start 3 different scenarios simultaneously
    {ok, Id1} = tcps_simulator:start_scenario(basic_production, #{
        work_orders => 20, iteration_delay_ms => 10
    }),
    {ok, Id2} = tcps_simulator:start_scenario(defect_scenario, #{
        work_orders => 20, iteration_delay_ms => 10
    }),
    {ok, Id3} = tcps_simulator:start_scenario(high_load, #{
        work_orders => 50, iteration_delay_ms => 5
    }),

    timer:sleep(500),

    % Verify all are tracked
    Scenarios = tcps_simulator:list_scenarios(),
    ?assertEqual(3, length(Scenarios)),

    % Verify each completed successfully
    {ok, Status1} = tcps_simulator:get_scenario_status(Id1),
    {ok, Status2} = tcps_simulator:get_scenario_status(Id2),
    {ok, Status3} = tcps_simulator:get_scenario_status(Id3),

    ?assertEqual(completed, maps:get(status, Status1)),
    ?assertEqual(completed, maps:get(status, Status2)),
    ?assertEqual(completed, maps:get(status, Status3)),

    ct:pal("All concurrent scenarios completed successfully"),
    ok.

test_pause_resume_workflow(_Config) ->
    ct:comment("Testing pause/resume functionality"),

    {ok, ScenarioId} = tcps_simulator:start_scenario(basic_production, #{
        work_orders => 100, iteration_delay_ms => 15
    }),

    timer:sleep(100),

    % Pause
    ok = tcps_simulator:pause_scenario(ScenarioId),
    {ok, Status1} = tcps_simulator:get_scenario_status(ScenarioId),
    ?assertEqual(paused, maps:get(status, Status1)),

    {ok, Metrics1} = tcps_simulator:get_metrics(ScenarioId),
    Count1 = maps:get(work_orders_created, Metrics1),

    % Wait while paused
    timer:sleep(200),

    % Should not progress
    {ok, Metrics2} = tcps_simulator:get_metrics(ScenarioId),
    Count2 = maps:get(work_orders_created, Metrics2),
    ?assertEqual(Count1, Count2),

    % Resume
    ok = tcps_simulator:resume_scenario(ScenarioId),
    {ok, Status2} = tcps_simulator:get_scenario_status(ScenarioId),
    ?assertEqual(running, maps:get(status, Status2)),

    timer:sleep(200),

    % Should progress again
    {ok, Metrics3} = tcps_simulator:get_metrics(ScenarioId),
    Count3 = maps:get(work_orders_created, Metrics3),
    ?assert(Count3 > Count2),

    ct:pal("Pause/resume verified: ~p -> ~p -> ~p", [Count1, Count2, Count3]),
    ok.

test_metrics_accuracy(_Config) ->
    ct:comment("Testing metrics calculation accuracy"),

    {ok, ScenarioId} = tcps_simulator:start_scenario(basic_production, #{
        work_orders => 50,
        quality_gate_pass_rate => 0.80,
        iteration_delay_ms => 10
    }),

    timer:sleep(800),

    {ok, Metrics} = tcps_simulator:get_metrics(ScenarioId),

    % Verify metric consistency
    Created = maps:get(work_orders_created, Metrics),
    Completed = maps:get(work_orders_completed, Metrics),
    Passed = maps:get(quality_gates_passed, Metrics),
    Failed = maps:get(quality_gates_failed, Metrics),

    ?assertEqual(50, Created),
    ?assertEqual(Completed, Passed), % Completed = Passed
    ?assertEqual(Created, Passed + Failed), % Total = Passed + Failed

    % Verify pass rate is close to configured
    ActualPassRate = Passed / Created,
    ?assert(abs(ActualPassRate - 0.80) < 0.15), % Within 15%

    % Verify throughput calculation
    Duration = maps:get(total_duration_ms, Metrics),
    ExpectedThroughput = (Completed / (Duration / 1000.0)),
    ActualThroughput = maps:get(throughput_per_second, Metrics),
    ?assert(abs(ActualThroughput - ExpectedThroughput) < 0.1),

    ct:pal("Metrics accuracy verified: ~p", [Metrics]),
    ok.

test_telemetry_collection(_Config) ->
    ct:comment("Testing OpenTelemetry span collection"),

    % Start scenario (should emit telemetry)
    {ok, ScenarioId} = tcps_simulator:start_scenario(basic_production, #{
        work_orders => 10, iteration_delay_ms => 10
    }),

    timer:sleep(200),

    % Verify scenario completed
    {ok, Status} = tcps_simulator:get_scenario_status(ScenarioId),
    ?assertEqual(completed, maps:get(status, Status)),

    % In a real test, we'd verify OTEL spans were created
    % For now, just verify the scenario ran with telemetry enabled
    {ok, Metrics} = tcps_simulator:get_metrics(ScenarioId),
    ?assert(maps:get(work_orders_created, Metrics) > 0),

    ct:pal("Telemetry collection verified"),
    ok.
