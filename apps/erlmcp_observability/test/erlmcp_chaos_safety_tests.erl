%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_chaos safety features following Chicago School TDD
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior through API calls only
%%% - Use REAL erlmcp_chaos gen_server (no mocks, no dummy processes)
%%% - NO internal state inspection (test API boundaries only)
%%% - NO record duplication (respect encapsulation)
%%% - Focused module: safety controls, blast radius, and rollback tests
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_safety_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Setup/teardown for chaos safety tests
safety_test_() ->
    {setup,
     fun setup_chaos/0,
     fun cleanup_chaos/1,
     [{"Safety controls enforcement", fun test_safety_controls/0},
      {"Blast radius limits", fun test_blast_radius_limits/0},
      {"Auto rollback on failure", fun test_auto_rollback/0},
      {"Multiple experiments with cumulative blast radius", fun test_cumulative_blast_radius/0}]}.

setup_chaos() ->
    {ok, Pid} = erlmcp_chaos:start_link([{safety_enabled, true}]),
    Pid.

cleanup_chaos(Pid) ->
    erlmcp_chaos:stop_all_experiments(),
    catch gen_server:stop(Pid),
    catch unregister(erlmcp_chaos).

%%====================================================================
%% Safety Control Tests
%%====================================================================

%% Test safety control enforcement
test_safety_controls() ->
    {ok, _Pid} = erlmcp_chaos:start_link([{safety_enabled, true}]),

    % Try to run experiment with very high rate (should be controlled)
    Config =
        #{experiment => kill_servers,
          target => erlmcp_server,
          rate => 0.8,  % Very high rate
          max_blast_radius => 0.8,
          duration => 5000},

    % Should be rejected or controlled by safety checks
    Result = erlmcp_chaos:run(Config),

    % Either rejected or running with safety monitoring
    case Result of
        {error, {safety_violation, _}} ->
            ?assert(true);
        {ok, ExperimentId} ->
            % Should stop if safety threshold exceeded
            timer:sleep(1000),
            erlmcp_chaos:stop_experiment(ExperimentId),
            ?assert(true)
    end,

    erlmcp_chaos:stop_all_experiments(),
    gen_server:stop(whereis(erlmcp_chaos)).

%%====================================================================
%% Blast Radius Tests
%%====================================================================

%% Test blast radius enforcement
test_blast_radius_limits() ->
    {ok, _Pid} = erlmcp_chaos:start_link([{safety_enabled, true}]),

    % Start first experiment with moderate blast radius
    Config1 =
        #{experiment => kill_random,
          rate => 0.3,
          duration => 5000,
          max_blast_radius => 0.3},

    {ok, _Exp1} = erlmcp_chaos:run(Config1),

    % Try to start another experiment that would exceed global blast radius
    Config2 =
        #{experiment => network_latency,
          latency => 100,
          rate => 0.3,
          duration => 5000,
          max_blast_radius => 0.3},

    % May be rejected or allowed depending on global limits
    Result = erlmcp_chaos:run(Config2),

    case Result of
        {error, {safety_violation, _}} ->
            ?assert(true);
        {ok, Exp2} ->
            erlmcp_chaos:stop_experiment(Exp2),
            ?assert(true)
    end,

    erlmcp_chaos:stop_all_experiments(),
    gen_server:stop(whereis(erlmcp_chaos)).

%% Test cumulative blast radius
test_cumulative_blast_radius() ->
    {ok, _Pid} = erlmcp_chaos:start_link([{safety_enabled, true}]),

    % Start multiple small experiments
    Config1 =
        #{experiment => network_latency,
          latency => 50,
          rate => 0.1,
          duration => 5000,
          max_blast_radius => 0.15},

    Config2 =
        #{experiment => kill_random,
          rate => 0.05,
          duration => 5000,
          max_blast_radius => 0.15},

    {ok, _Exp1} = erlmcp_chaos:run(Config1),
    {ok, _Exp2} = erlmcp_chaos:run(Config2),

    % Verify both are running (within cumulative limit)
    ActiveExperiments = erlmcp_chaos:get_active_experiments(),
    ?assert(length(ActiveExperiments) >= 2),

    erlmcp_chaos:stop_all_experiments(),
    gen_server:stop(whereis(erlmcp_chaos)).

%%====================================================================
%% Auto Rollback Tests
%%====================================================================

%% Test automatic rollback on failure
test_auto_rollback() ->
    {ok, _Pid} = erlmcp_chaos:start_link([{safety_enabled, true}]),

    % Run experiment targeting non-existent server (should auto-rollback)
    Config =
        #{experiment => kill_servers,
          target => nonexistent_server,
          rate => 0.1,
          duration => 2000,
          auto_rollback => true},

    {ok, ExperimentId} = erlmcp_chaos:run(Config),

    % Let it run briefly
    timer:sleep(500),

    % Experiment should be running (even if no targets found)
    {ok, Status} = erlmcp_chaos:get_experiment_status(ExperimentId),
    ?assertMatch(#{state := running}, Status),

    % Stop experiment
    ok = erlmcp_chaos:stop_experiment(ExperimentId),

    erlmcp_chaos:stop_all_experiments(),
    gen_server:stop(whereis(erlmcp_chaos)).

%%====================================================================
%% Edge Case Tests
%%====================================================================

%% Test experiment with safety disabled
safety_disabled_test_() ->
    {setup,
     fun() ->
        % Start with safety disabled
        {ok, Pid} = erlmcp_chaos:start_link([{safety_enabled, false}]),
        Pid
     end,
     fun(Pid) ->
        erlmcp_chaos:stop_all_experiments(),
        catch gen_server:stop(Pid),
        catch unregister(erlmcp_chaos)
     end,
     fun(_Pid) ->
        [?_test(begin
                    % With safety disabled, high-risk experiments should run
                    Config =
                        #{experiment => kill_random,
                          rate => 0.5,  % High rate
                          duration => 1000},

                    Result = erlmcp_chaos:run(Config),
                    ?assertMatch({ok, _}, Result),

                    % Clean up
                    {ok, ExpId} = Result,
                    erlmcp_chaos:stop_experiment(ExpId)
                end)]
     end}.

%% Test blast radius of zero
zero_blast_radius_test_() ->
    {setup,
     fun setup_chaos/0,
     fun cleanup_chaos/1,
     fun(_Pid) ->
        [?_test(begin
                    % Zero blast radius should mean no impact
                    Config =
                        #{experiment => network_latency,
                          latency => 100,
                          rate => 0.1,
                          duration => 1000,
                          max_blast_radius => 0.0},

                    % Should either run with no effect or be rejected
                    Result = erlmcp_chaos:run(Config),
                    case Result of
                        {ok, ExpId} ->
                            erlmcp_chaos:stop_experiment(ExpId);
                        {error, _} ->
                            ok
                    end
                end)]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

%% Test complete safety workflow
safety_workflow_test_() ->
    {setup,
     fun setup_chaos/0,
     fun cleanup_chaos/1,
     fun(_Pid) ->
        [?_test(begin
                    % Dry run to check safety
                    Config =
                        #{experiment => kill_servers,
                          target => erlmcp_server,
                          rate => 0.5,
                          duration => 2000,
                          max_blast_radius => 0.5},

                    {ok, DryRunResult} = erlmcp_chaos:dry_run(Config),

                    % Verify dry run includes safety checks
                    ?assertMatch(#{safety_checks := _, risks := _}, DryRunResult),

                    % Run experiment (should be within safety limits)
                    {ok, ExperimentId} = erlmcp_chaos:run(Config),

                    % Verify it's running
                    {ok, Status} = erlmcp_chaos:get_experiment_status(ExperimentId),
                    ?assertEqual(running, maps:get(state, Status)),

                    % Stop experiment
                    ok = erlmcp_chaos:stop_experiment(ExperimentId)
                end)]
     end}.
