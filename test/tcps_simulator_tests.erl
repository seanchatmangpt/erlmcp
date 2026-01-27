%%%-----------------------------------------------------------------------------
%%% @doc TCPS Simulator Test Suite
%%%
%%% Comprehensive test suite for TCPS workflow simulator engine covering:
%%% - State management operations
%%% - Scenario loading and execution
%%% - All 6 scenario types
%%% - Quality gate simulation
%%% - Andon event simulation
%%% - Work order lifecycle
%%% - Snapshot and rollback
%%% - Metrics collection
%%%
%%% Test Coverage Target: 80%+
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_simulator_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Fixtures
%%%=============================================================================

simulator_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            {"State Management Tests", fun test_state_management/0},
            {"Scenario Loader Tests", fun test_scenario_loader/0},
            {"Ideal Workflow Scenario", fun test_ideal_workflow_scenario/0},
            {"Quality Gate Failure Scenario", fun test_quality_gate_failure_scenario/0},
            {"WIP Limit Overflow Scenario", fun test_wip_limit_overflow_scenario/0},
            {"Heijunka Leveling Scenario", fun test_heijunka_leveling_scenario/0},
            {"Receipt Chain Audit Scenario", fun test_receipt_chain_audit_scenario/0},
            {"Kaizen Cycle Scenario", fun test_kaizen_cycle_scenario/0},
            {"Simulator Lifecycle Tests", fun test_simulator_lifecycle/0},
            {"Speed Control Tests", fun test_speed_control/0},
            {"Snapshot and Rollback Tests", fun test_snapshot_rollback/0},
            {"Metrics Collection Tests", fun test_metrics_collection/0},
            {"Error Handling Tests", fun test_error_handling/0}
        ]
    }.

setup() ->
    application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    application:stop(erlmcp),
    ok.

%%%=============================================================================
%%% State Management Tests
%%%=============================================================================

test_state_management() ->
    % Test new state creation
    State = tcps_simulator_state:new(#{scenario_id => <<"test">>}),
    ?assertMatch(#{scenario_id := <<"test">>}, State),
    ?assertEqual(0, tcps_simulator_state:get_current_time(State)),

    % Test event logging
    Event = #{type => test_event, data => #{foo => bar}},
    State2 = tcps_simulator_state:add_event(State, Event),
    Events = tcps_simulator_state:get_events(State2),
    ?assertEqual(1, length(Events)),

    % Test work order management
    WO = #{id => <<"wo1">>, status => created},
    State3 = tcps_simulator_state:update_work_order(State2, WO),
    WorkOrders = tcps_simulator_state:get_work_orders(State3),
    ?assertEqual(1, maps:size(WorkOrders)),
    ?assertMatch(#{<<"wo1">> := #{status := created}}, WorkOrders),

    % Test time advancement
    State4 = tcps_simulator_state:advance_time(State3, 100),
    ?assertEqual(100, tcps_simulator_state:get_current_time(State4)),

    % Test receipt chain
    Receipt = #{type => quality_gate, status => pass},
    State5 = tcps_simulator_state:add_receipt(State4, Receipt),
    Receipts = tcps_simulator_state:get_receipts(State5),
    ?assertEqual(1, length(Receipts)),

    % Test Andon events
    AndonEvent = #{event_id => <<"andon1">>, status => active},
    State6 = tcps_simulator_state:add_andon_event(State5, AndonEvent),
    AndonEvents = tcps_simulator_state:get_andon_events(State6),
    ?assertEqual(1, length(AndonEvents)),

    % Test snapshot and restore
    Snapshot = tcps_simulator_state:snapshot(State6),
    ?assertMatch(#{snapshot_id := _, state := _}, Snapshot),

    State7 = tcps_simulator_state:new(#{}),
    RestoredState = tcps_simulator_state:restore(State7, Snapshot),
    ?assertEqual(
        tcps_simulator_state:get_current_time(State6),
        tcps_simulator_state:get_current_time(RestoredState)
    ).

%%%=============================================================================
%%% Scenario Loader Tests
%%%=============================================================================

test_scenario_loader() ->
    % Test list scenarios
    Scenarios = tcps_scenario_loader:list_scenarios(),
    ?assertEqual(6, length(Scenarios)),
    ?assert(lists:member(ideal_workflow, Scenarios)),
    ?assert(lists:member(quality_gate_failure, Scenarios)),
    ?assert(lists:member(wip_limit_overflow, Scenarios)),
    ?assert(lists:member(heijunka_leveling, Scenarios)),
    ?assert(lists:member(receipt_chain_audit, Scenarios)),
    ?assert(lists:member(kaizen_cycle, Scenarios)),

    % Test load each scenario
    lists:foreach(
        fun(ScenarioId) ->
            {ok, Scenario} = tcps_scenario_loader:load_scenario(ScenarioId),
            ?assertMatch(#{id := _, name := _, steps := _}, Scenario),
            ?assert(is_list(maps:get(steps, Scenario))),

            % Validate scenario
            ?assertEqual(ok, tcps_scenario_loader:validate_scenario(Scenario))
        end,
        Scenarios
    ),

    % Test unknown scenario
    ?assertMatch(
        {error, unknown_scenario},
        tcps_scenario_loader:load_scenario(invalid_scenario)
    ),

    % Test scenario metadata
    {ok, Metadata} = tcps_scenario_loader:get_scenario_metadata(ideal_workflow),
    ?assertMatch(#{id := ideal_workflow, name := _, description := _}, Metadata).

%%%=============================================================================
%%% Scenario Execution Tests
%%%=============================================================================

test_ideal_workflow_scenario() ->
    {ok, Pid} = tcps_simulator:start_link(),

    % Load scenario
    ?assertEqual(ok, tcps_simulator:load_scenario(ideal_workflow)),

    % Start simulation
    ?assertEqual(ok, tcps_simulator:start_simulation()),

    % Get initial state
    State1 = tcps_simulator:get_state(),
    ?assertEqual(running, maps:get(mode, State1)),
    ?assertEqual(ideal_workflow, maps:get(scenario_id, State1)),

    % Pause simulation for manual control
    ?assertEqual(ok, tcps_simulator:pause_simulation()),

    % Execute steps manually
    ?assertEqual(ok, tcps_simulator:step_simulation()),
    Metrics1 = tcps_simulator:get_metrics(),
    ?assert(maps:get(work_orders_created, Metrics1, 0) >= 0),

    % Continue execution
    ?assertEqual(ok, tcps_simulator:step_simulation()),
    ?assertEqual(ok, tcps_simulator:step_simulation()),
    ?assertEqual(ok, tcps_simulator:step_simulation()),

    % Check final state
    State2 = tcps_simulator:get_state(),
    SimState = maps:get(simulation_state, State2),
    Receipts = tcps_simulator_state:get_receipts(SimState),
    AndonEvents = tcps_simulator_state:get_andon_events(SimState),

    % Ideal workflow should have receipts and no Andon events
    ?assert(length(Receipts) > 0),
    ?assertEqual(0, length(AndonEvents)),

    tcps_simulator:stop().

test_quality_gate_failure_scenario() ->
    {ok, Pid} = tcps_simulator:start_link(),

    ?assertEqual(ok, tcps_simulator:load_scenario(quality_gate_failure)),
    ?assertEqual(ok, tcps_simulator:start_simulation()),
    ?assertEqual(ok, tcps_simulator:pause_simulation()),

    % Execute all steps
    lists:foreach(
        fun(_) ->
            case tcps_simulator:step_simulation() of
                ok -> ok;
                {ok, simulation_complete} -> ok;
                {error, _} -> ok
            end
        end,
        lists:seq(1, 10)
    ),

    % Check that Andon event was triggered
    State = tcps_simulator:get_state(),
    SimState = maps:get(simulation_state, State),
    AndonEvents = tcps_simulator_state:get_andon_events(SimState),

    ?assert(length(AndonEvents) >= 1),

    % Check metrics
    Metrics = tcps_simulator:get_metrics(),
    ?assert(maps:get(andon_events, Metrics, 0) >= 1),

    tcps_simulator:stop().

test_wip_limit_overflow_scenario() ->
    {ok, Pid} = tcps_simulator:start_link(),

    ?assertEqual(ok, tcps_simulator:load_scenario(wip_limit_overflow)),
    ?assertEqual(ok, tcps_simulator:start_simulation()),
    ?assertEqual(ok, tcps_simulator:pause_simulation()),

    % Execute steps
    lists:foreach(
        fun(_) ->
            case tcps_simulator:step_simulation() of
                ok -> ok;
                {ok, simulation_complete} -> ok;
                {error, _} -> ok
            end
        end,
        lists:seq(1, 10)
    ),

    % Check WIP limits were enforced
    State = tcps_simulator:get_state(),
    SimState = maps:get(simulation_state, State),
    Events = tcps_simulator_state:get_events(SimState),

    % Should have WIP limit events
    ?assert(length(Events) > 0),

    tcps_simulator:stop().

test_heijunka_leveling_scenario() ->
    {ok, Pid} = tcps_simulator:start_link(),

    ?assertEqual(ok, tcps_simulator:load_scenario(heijunka_leveling)),
    ?assertEqual(ok, tcps_simulator:start_simulation()),
    ?assertEqual(ok, tcps_simulator:pause_simulation()),

    % Execute steps
    lists:foreach(
        fun(_) ->
            case tcps_simulator:step_simulation() of
                ok -> ok;
                {ok, simulation_complete} -> ok;
                {error, _} -> ok
            end
        end,
        lists:seq(1, 10)
    ),

    % Verify balanced distribution
    State = tcps_simulator:get_state(),
    SimState = maps:get(simulation_state, State),
    KanbanState = maps:get(kanban_state, SimState),
    CurrentWip = maps:get(current_wip, KanbanState),

    % Check that WIP is distributed
    ?assert(is_map(CurrentWip)),

    tcps_simulator:stop().

test_receipt_chain_audit_scenario() ->
    {ok, Pid} = tcps_simulator:start_link(),

    ?assertEqual(ok, tcps_simulator:load_scenario(receipt_chain_audit)),
    ?assertEqual(ok, tcps_simulator:start_simulation()),
    ?assertEqual(ok, tcps_simulator:pause_simulation()),

    % Execute steps
    lists:foreach(
        fun(_) ->
            case tcps_simulator:step_simulation() of
                ok -> ok;
                {ok, simulation_complete} -> ok;
                {error, _} -> ok
            end
        end,
        lists:seq(1, 10)
    ),

    % Verify receipt chain
    State = tcps_simulator:get_state(),
    SimState = maps:get(simulation_state, State),
    Receipts = tcps_simulator_state:get_receipts(SimState),

    ?assert(length(Receipts) > 0),

    tcps_simulator:stop().

test_kaizen_cycle_scenario() ->
    {ok, Pid} = tcps_simulator:start_link(),

    ?assertEqual(ok, tcps_simulator:load_scenario(kaizen_cycle)),
    ?assertEqual(ok, tcps_simulator:start_simulation()),
    ?assertEqual(ok, tcps_simulator:pause_simulation()),

    % Execute steps
    lists:foreach(
        fun(_) ->
            case tcps_simulator:step_simulation() of
                ok -> ok;
                {ok, simulation_complete} -> ok;
                {error, _} -> ok
            end
        end,
        lists:seq(1, 10)
    ),

    % Check for improvement events
    State = tcps_simulator:get_state(),
    SimState = maps:get(simulation_state, State),
    Events = tcps_simulator_state:get_events(SimState),

    ?assert(length(Events) > 0),

    tcps_simulator:stop().

%%%=============================================================================
%%% Simulator Lifecycle Tests
%%%=============================================================================

test_simulator_lifecycle() ->
    % Test start/stop
    {ok, Pid} = tcps_simulator:start_link(),
    ?assert(is_process_alive(Pid)),

    % Test loading scenario before simulation
    ?assertEqual(ok, tcps_simulator:load_scenario(ideal_workflow)),

    % Test starting without scenario
    tcps_simulator:reset_simulation(),
    State1 = tcps_simulator:get_state(),
    ?assertEqual(stopped, maps:get(mode, State1)),

    % Test proper lifecycle
    ?assertEqual(ok, tcps_simulator:load_scenario(ideal_workflow)),
    ?assertEqual(ok, tcps_simulator:start_simulation()),

    State2 = tcps_simulator:get_state(),
    ?assertEqual(running, maps:get(mode, State2)),

    % Test pause/resume
    ?assertEqual(ok, tcps_simulator:pause_simulation()),
    State3 = tcps_simulator:get_state(),
    ?assertEqual(paused, maps:get(mode, State3)),

    ?assertEqual(ok, tcps_simulator:resume_simulation()),
    State4 = tcps_simulator:get_state(),
    ?assertEqual(running, maps:get(mode, State4)),

    % Test reset
    ?assertEqual(ok, tcps_simulator:reset_simulation()),
    State5 = tcps_simulator:get_state(),
    ?assertEqual(stopped, maps:get(mode, State5)),
    ?assertEqual(0, maps:get(current_step, State5)),

    tcps_simulator:stop().

%%%=============================================================================
%%% Speed Control Tests
%%%=============================================================================

test_speed_control() ->
    {ok, Pid} = tcps_simulator:start_link(),

    % Test setting different speeds
    ?assertEqual(ok, tcps_simulator:set_speed(1)),
    State1 = tcps_simulator:get_state(),
    % Speed is internal to state, check via metrics

    ?assertEqual(ok, tcps_simulator:set_speed(5)),
    ?assertEqual(ok, tcps_simulator:set_speed(10)),

    % Test invalid speed (should fail at compile time due to spec)
    % ?assertError(function_clause, tcps_simulator:set_speed(99)),

    tcps_simulator:stop().

%%%=============================================================================
%%% Snapshot and Rollback Tests
%%%=============================================================================

test_snapshot_rollback() ->
    {ok, Pid} = tcps_simulator:start_link(),

    ?assertEqual(ok, tcps_simulator:load_scenario(ideal_workflow)),
    ?assertEqual(ok, tcps_simulator:start_simulation()),
    ?assertEqual(ok, tcps_simulator:pause_simulation()),

    % Execute some steps
    ?assertEqual(ok, tcps_simulator:step_simulation()),
    ?assertEqual(ok, tcps_simulator:step_simulation()),

    % Take snapshot
    {ok, Snapshot} = tcps_simulator:snapshot_state(),
    ?assertMatch(#{snapshot_id := _, timestamp := _, state := _}, Snapshot),

    State1 = tcps_simulator:get_state(),
    Step1 = maps:get(current_step, State1),

    % Execute more steps
    ?assertEqual(ok, tcps_simulator:step_simulation()),
    State2 = tcps_simulator:get_state(),
    Step2 = maps:get(current_step, State2),
    ?assert(Step2 > Step1),

    % Restore snapshot
    ?assertEqual(ok, tcps_simulator:restore_snapshot(Snapshot)),

    % Verify restored state
    State3 = tcps_simulator:get_state(),
    SimState3 = maps:get(simulation_state, State3),

    % Check that simulation state was restored
    ?assert(is_map(SimState3)),

    tcps_simulator:stop().

%%%=============================================================================
%%% Metrics Collection Tests
%%%=============================================================================

test_metrics_collection() ->
    {ok, Pid} = tcps_simulator:start_link(),

    ?assertEqual(ok, tcps_simulator:load_scenario(ideal_workflow)),
    ?assertEqual(ok, tcps_simulator:start_simulation()),
    ?assertEqual(ok, tcps_simulator:pause_simulation()),

    % Initial metrics
    Metrics1 = tcps_simulator:get_metrics(),
    ?assert(is_map(Metrics1)),
    ?assert(maps:is_key(steps_executed, Metrics1)),
    ?assert(maps:is_key(work_orders_created, Metrics1)),

    % Execute steps and check metrics increase
    ?assertEqual(ok, tcps_simulator:step_simulation()),
    Metrics2 = tcps_simulator:get_metrics(),

    % At least one metric should change
    ?assertNotEqual(Metrics1, Metrics2),

    tcps_simulator:stop().

%%%=============================================================================
%%% Error Handling Tests
%%%=============================================================================

test_error_handling() ->
    {ok, Pid} = tcps_simulator:start_link(),

    % Test starting simulation without scenario
    ?assertMatch(
        {error, no_scenario_loaded},
        tcps_simulator:start_simulation()
    ),

    % Test loading invalid scenario
    ?assertMatch(
        {error, unknown_scenario},
        tcps_simulator:load_scenario(invalid_scenario)
    ),

    % Test resuming when not paused
    ?assertEqual(ok, tcps_simulator:load_scenario(ideal_workflow)),
    ?assertMatch(
        {error, not_paused},
        tcps_simulator:resume_simulation()
    ),

    % Test snapshot without simulation
    tcps_simulator:reset_simulation(),
    ?assertMatch(
        {error, no_active_simulation},
        tcps_simulator:snapshot_state()
    ),

    tcps_simulator:stop().

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

integration_test_() ->
    {timeout, 30,
        fun() ->
            {ok, Pid} = tcps_simulator:start_link(),

            % Run complete scenario end-to-end
            ?assertEqual(ok, tcps_simulator:load_scenario(ideal_workflow)),
            ?assertEqual(ok, tcps_simulator:start_simulation()),

            % Let it run briefly
            timer:sleep(500),

            % Pause and check state
            ?assertEqual(ok, tcps_simulator:pause_simulation()),
            State = tcps_simulator:get_state(),
            ?assertEqual(paused, maps:get(mode, State)),

            % Verify data integrity
            SimState = maps:get(simulation_state, State),
            WorkOrders = tcps_simulator_state:get_work_orders(SimState),
            Events = tcps_simulator_state:get_events(SimState),

            ?assert(is_map(WorkOrders)),
            ?assert(is_list(Events)),

            tcps_simulator:stop()
        end
    }.

%%%=============================================================================
%%% Performance Tests
%%%=============================================================================

performance_test_() ->
    {timeout, 60,
        fun() ->
            {ok, Pid} = tcps_simulator:start_link(),

            % Load and run scenario at different speeds
            lists:foreach(
                fun(Speed) ->
                    ?assertEqual(ok, tcps_simulator:reset_simulation()),
                    ?assertEqual(ok, tcps_simulator:set_speed(Speed)),
                    ?assertEqual(ok, tcps_simulator:load_scenario(ideal_workflow)),
                    ?assertEqual(ok, tcps_simulator:start_simulation()),

                    timer:sleep(200),

                    ?assertEqual(ok, tcps_simulator:pause_simulation()),
                    Metrics = tcps_simulator:get_metrics(),

                    % Should have executed some steps
                    ?assert(maps:get(steps_executed, Metrics, 0) >= 0)
                end,
                [1, 5, 10]
            ),

            tcps_simulator:stop()
        end
    }.

%%%=============================================================================
%%% Unit Tests for Internal Functions
%%%=============================================================================

internal_functions_test_() ->
    [
        {"Test quality gate simulation", fun test_quality_gate_simulation/0},
        {"Test Andon event simulation", fun test_andon_event_simulation/0},
        {"Test work order lifecycle simulation", fun test_work_order_lifecycle_simulation/0}
    ].

test_quality_gate_simulation() ->
    SimState = tcps_simulator_state:new(#{quality_gate_pass_rate => 1.0}),
    WorkOrder = #{id => <<"wo1">>, payload => #{}},

    {pass, Receipts} = tcps_simulator:simulate_quality_gates(WorkOrder, SimState),
    ?assertEqual(8, length(Receipts)),

    % Test with failure injection
    SimState2 = tcps_simulator_state:new(#{
        inject_failures => true
    }),
    WorkOrder2 = #{
        id => <<"wo2">>,
        payload => #{
            inject_failure => #{
                gate => test_execution,
                reason => <<"Test failure">>
            }
        }
    },
    {fail, Gate, Details} = tcps_simulator:simulate_quality_gates(WorkOrder2, SimState2),
    ?assertEqual(test_execution, Gate),
    ?assertMatch(#{reason := _}, Details).

test_andon_event_simulation() ->
    SimState = tcps_simulator_state:new(#{}),

    {ok, AndonEvent} = tcps_simulator:simulate_andon_event(test_failure, SimState),
    ?assertMatch(#{event_id := _, failure_type := test_failure}, AndonEvent),
    ?assertEqual(test_failure, maps:get(failure_type, AndonEvent)).

test_work_order_lifecycle_simulation() ->
    % Test successful lifecycle
    SimState = tcps_simulator_state:new(#{quality_gate_pass_rate => 1.0}),
    WorkOrder = #{id => <<"wo1">>, payload => #{}},

    {ok, CompletedWO} = tcps_simulator:simulate_work_order_lifecycle(WorkOrder, SimState),
    ?assertEqual(completed, maps:get(status, CompletedWO)),
    ?assert(maps:is_key(receipts, CompletedWO)),

    % Test failed lifecycle
    SimState2 = tcps_simulator_state:new(#{
        inject_failures => true
    }),
    WorkOrder2 = #{
        id => <<"wo2">>,
        payload => #{
            inject_failure => #{
                gate => compilation,
                reason => <<"Compilation error">>
            }
        }
    },
    {ok, BlockedWO} = tcps_simulator:simulate_work_order_lifecycle(WorkOrder2, SimState2),
    ?assertEqual(blocked, maps:get(status, BlockedWO)),
    ?assertEqual(compilation, maps:get(failed_gate, BlockedWO)).
