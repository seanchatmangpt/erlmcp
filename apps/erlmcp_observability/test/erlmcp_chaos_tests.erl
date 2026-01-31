%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_chaos module following Chicago School TDD
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior through API calls only
%%% - Use REAL erlmcp_chaos gen_server (no mocks, no dummy processes)
%%% - NO internal state inspection (test API boundaries only)
%%% - NO record duplication (respect encapsulation)
%%% - Split into focused modules (this file: framework & lifecycle tests)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Setup/teardown for chaos framework
chaos_test_() ->
    {setup,
     fun setup_chaos/0,
     fun cleanup_chaos/1,
     [
         {"Start chaos framework", fun test_start_chaos_framework/0},
         {"Dry run experiment", fun test_dry_run/0},
         {"Run experiment", fun test_run_experiment/0},
         {"Stop experiment", fun test_stop_experiment/0},
         {"Get experiment status", fun test_get_experiment_status/0},
         {"Get active experiments", fun test_get_active_experiments/0},
         {"Stop all experiments", fun test_stop_all_experiments/0}
     ]}.

setup_chaos() ->
    % Start chaos framework with safety enabled
    {ok, Pid} = erlmcp_chaos:start_link([{safety_enabled, true}]),
    Pid.

cleanup_chaos(Pid) ->
    % Stop all experiments and framework
    erlmcp_chaos:stop_all_experiments(),
    catch gen_server:stop(Pid),
    catch unregister(erlmcp_chaos).

%%====================================================================
%% Framework Lifecycle Tests
%%====================================================================

%% Test starting chaos framework
test_start_chaos_framework() ->
    % Framework should start successfully
    {ok, Pid} = erlmcp_chaos:start_link([{safety_enabled, true}]),
    ?assert(is_pid(Pid)),
    gen_server:stop(Pid).

%%====================================================================
%% Dry Run Tests
%%====================================================================

%% Test dry run mode (safety validation without execution)
test_dry_run() ->
    {ok, _Pid} = erlmcp_chaos:start_link([{safety_enabled, true}]),

    Config = #{
        experiment => kill_servers,
        target => erlmcp_server,
        rate => 0.1,
        duration => 5000
    },

    {ok, Result} = erlmcp_chaos:dry_run(Config),

    % Verify dry run returns safety analysis
    ?assertMatch(#{
        experiment_type := kill_servers,
        estimated_blast_radius := _,
        safety_checks := _,
        risks := _,
        recommendations := _
    }, Result),

    erlmcp_chaos:stop_all_experiments(),
    gen_server:stop(whereis(erlmcp_chaos)).

%%====================================================================
%% Experiment Execution Tests
%%====================================================================

%% Test running an experiment
test_run_experiment() ->
    {ok, _Pid} = erlmcp_chaos:start_link([{safety_enabled, true}]),

    Config = #{
        experiment => network_latency,
        latency => 100,
        rate => 0.2,
        duration => 2000,
        max_blast_radius => 0.3
    },

    {ok, ExperimentId} = erlmcp_chaos:run(Config),

    % Verify experiment ID is returned
    ?assert(is_binary(ExperimentId) orelse is_atom(ExperimentId)),

    % Stop the experiment
    ok = erlmcp_chaos:stop_experiment(ExperimentId),

    erlmcp_chaos:stop_all_experiments(),
    gen_server:stop(whereis(erlmcp_chaos)).

%% Test stopping an experiment
test_stop_experiment() ->
    {ok, _Pid} = erlmcp_chaos:start_link([{safety_enabled, true}]),

    Config = #{
        experiment => network_latency,
        latency => 50,
        rate => 0.1,
        duration => 5000
    },

    {ok, ExperimentId} = erlmcp_chaos:run(Config),

    % Let it run briefly
    timer:sleep(500),

    % Stop experiment
    ok = erlmcp_chaos:stop_experiment(ExperimentId),

    % Verify experiment is stopped
    {ok, Status} = erlmcp_chaos:get_experiment_status(ExperimentId),
    State = maps:get(state, Status),
    ?assert(lists:member(State, [stopped, completed, failed])),

    erlmcp_chaos:stop_all_experiments(),
    gen_server:stop(whereis(erlmcp_chaos)).

%%====================================================================
%% Status Query Tests
%%====================================================================

%% Test getting experiment status
test_get_experiment_status() ->
    {ok, _Pid} = erlmcp_chaos:start_link([{safety_enabled, true}]),

    Config = #{
        experiment => kill_random,
        rate => 0.05,
        interval => 1000,
        duration => 3000
    },

    {ok, ExperimentId} = erlmcp_chaos:run(Config),

    % Get experiment status
    {ok, Status} = erlmcp_chaos:get_experiment_status(ExperimentId),

    % Verify status structure (observable behavior)
    ?assertMatch(#{
        id := ExperimentId,
        type := kill_random,
        state := running,
        start_time := _,
        targets_affected := _,
        total_targets := _,
        blast_radius := _
    }, Status),

    % Stop experiment
    ok = erlmcp_chaos:stop_experiment(ExperimentId),

    erlmcp_chaos:stop_all_experiments(),
    gen_server:stop(whereis(erlmcp_chaos)).

%% Test getting active experiments
test_get_active_experiments() ->
    {ok, _Pid} = erlmcp_chaos:start_link([{safety_enabled, true}]),

    Config = #{
        experiment => network_latency,
        latency => 50,
        rate => 0.1,
        duration => 3000
    },

    {ok, _ExperimentId} = erlmcp_chaos:run(Config),

    % Get active experiments
    ActiveExperiments = erlmcp_chaos:get_active_experiments(),

    % Should have at least one active experiment
    ?assert(is_list(ActiveExperiments)),
    ?assert(length(ActiveExperiments) > 0),

    % Stop all
    erlmcp_chaos:stop_all_experiments(),
    gen_server:stop(whereis(erlmcp_chaos)).

%%====================================================================
%% Cleanup Tests
%%====================================================================

%% Test stopping all experiments
test_stop_all_experiments() ->
    {ok, _Pid} = erlmcp_chaos:start_link([{safety_enabled, true}]),

    % Run multiple experiments
    Config1 = #{
        experiment => network_latency,
        latency => 50,
        rate => 0.1,
        duration => 10000
    },

    Config2 = #{
        experiment => kill_random,
        rate => 0.05,
        duration => 10000
    },

    {ok, Exp1} = erlmcp_chaos:run(Config1),
    {ok, Exp2} = erlmcp_chaos:run(Config2),

    % Verify both are running
    ActiveExperiments = erlmcp_chaos:get_active_experiments(),
    ?assert(length(ActiveExperiments) >= 2),

    % Stop all
    ok = erlmcp_chaos:stop_all_experiments(),

    % Verify no active experiments
    EmptyActive = erlmcp_chaos:get_active_experiments(),
    ?assertEqual(0, length(EmptyActive)),

    gen_server:stop(whereis(erlmcp_chaos)).

%%====================================================================
%% Edge Case Tests
%%====================================================================

%% Test experiment lifecycle (run to completion)
experiment_lifecycle_test_() ->
    {setup,
     fun setup_chaos/0,
     fun cleanup_chaos/1,
     fun(_Pid) ->
         [
          ?_test(begin
              % Run short experiment that completes on its own
              Config = #{
                  experiment => network_latency,
                  latency => 50,
                  rate => 0.1,
                  duration => 1000  % Short duration
              },

              {ok, ExperimentId} = erlmcp_chaos:run(Config),

              % Wait for completion
              timer:sleep(1500),

              % Should be completed
              {ok, FinalStatus} = erlmcp_chaos:get_experiment_status(ExperimentId),
              ?assertMatch(#{state := State} when State =:= completed orelse State =:= stopped,
                           FinalStatus)
          end)
         ]
     end}.

%% Test getting status of non-existent experiment
nonexistent_experiment_test_() ->
    {setup,
     fun setup_chaos/0,
     fun cleanup_chaos/1,
     fun(_Pid) ->
         [
          ?_test(begin
              % Query non-existent experiment
              Result = erlmcp_chaos:get_experiment_status(<<"nonexistent_exp">>),
              ?assertEqual({error, not_found}, Result)
          end)
         ]
     end}.

%% Test stopping non-existent experiment
stop_nonexistent_experiment_test_() ->
    {setup,
     fun setup_chaos/0,
     fun cleanup_chaos/1,
     fun(_Pid) ->
         [
          ?_test(begin
              % Stop non-existent experiment
              Result = erlmcp_chaos:stop_experiment(<<"nonexistent_exp">>),
              % Should return ok or error, but not crash
              case Result of
                  ok -> ?assert(true);
                  {error, _} -> ?assert(true)
              end
          end)
         ]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

%% Test complete chaos workflow
chaos_workflow_test_() ->
    {setup,
     fun setup_chaos/0,
     fun cleanup_chaos/1,
     fun(_Pid) ->
         [
          ?_test(begin
              % Dry run first
              Config = #{
                  experiment => network_latency,
                  latency => 100,
                  rate => 0.2,
                  duration => 2000,
                  max_blast_radius => 0.3
              },

              {ok, DryRunResult} = erlmcp_chaos:dry_run(Config),
              ?assertMatch(#{experiment_type := network_latency}, DryRunResult),

              % Run experiment
              {ok, ExperimentId} = erlmcp_chaos:run(Config),

              % Verify it's running
              {ok, Status} = erlmcp_chaos:get_experiment_status(ExperimentId),
              ?assertEqual(running, maps:get(state, Status)),

              % Check active experiments
              ActiveExperiments = erlmcp_chaos:get_active_experiments(),
              ?assert(length(ActiveExperiments) > 0),

              % Stop experiment
              ok = erlmcp_chaos:stop_experiment(ExperimentId),

              % Verify stopped
              {ok, StoppedStatus} = erlmcp_chaos:get_experiment_status(ExperimentId),
              StoppedState = maps:get(state, StoppedStatus),
              ?assert(lists:member(StoppedState, [stopped, completed, failed]))
          end)
         ]
     end}.
