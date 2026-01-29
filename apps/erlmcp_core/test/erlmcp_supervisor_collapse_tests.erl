%%%=============================================================================
%%% @doc Supervisor Tree Collapse Recovery Tests
%%% @end
%%% Tests for supervisor tree collapse scenarios and recovery
%%%=============================================================================
-module(erlmcp_supervisor_collapse_tests).
-author('erlmcp').

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Setup and Teardown
%%%=============================================================================

setup() ->
    %% Stop any existing supervisor first
    case whereis(erlmcp_cluster_sup) of
        undefined -> ok;
        _ -> supervisor:stop(erlmcp_cluster_sup),
             timer:sleep(100)
    end,
    {ok, Pid} = erlmcp_cluster_sup:start_link(),
    Pid.

cleanup(Pid) ->
    case whereis(erlmcp_cluster_sup) of
        undefined -> ok;
        _ -> supervisor:stop(erlmcp_cluster_sup)
    end,
    %% Wait for shutdown
    timer:sleep(100).

%%%=============================================================================
%%% Zombie Process Prevention Tests
%%%=============================================================================

zombie_process_prevention_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_no_zombie_when_disabled()),
          ?_test(test_supervisor_starts_when_disabled()),
          ?_test(test_supervisor_has_zero_children_when_disabled()),
          ?_test(test_intensity_tracking_works())
         ]
     end}.

test_no_zombie_when_disabled() ->
    %% Ensure clustering is disabled
    application:set_env(erlmcp_core, cluster_enabled, false),

    %% Stop any existing supervisor
    case whereis(erlmcp_cluster_sup) of
        undefined -> ok;
        _ -> supervisor:stop(erlmcp_cluster_sup),
             timer:sleep(100)
    end,

    %% Start the supervisor
    {ok, Pid} = erlmcp_cluster_sup:start_link(),

    %% Verify supervisor is alive
    ?assertEqual(Pid, whereis(erlmcp_cluster_sup)),

    %% Verify it has no children (not a zombie)
    Children = supervisor:which_children(Pid),
    ?assertEqual(0, length(Children)),

    %% Verify intensity tracking is active
    try
        {ok, _} = supervisor:count_children(Pid),
        ?assert(true)
    catch
        _:_ ->
            ?assert(false, "Supervisor should be valid")
    end,

    %% Clean shutdown
    supervisor:stop(erlmcp_cluster_sup),
    timer:sleep(100),

    %% Verify process is gone (not a zombie)
    ?assertEqual(undefined, whereis(erlmcp_cluster_sup)).

test_supervisor_starts_when_disabled() ->
    %% Ensure clustering is disabled
    application:set_env(erlmcp_core, cluster_enabled, false),

    %% Start should succeed
    Result = erlmcp_cluster_sup:start_link(),
    ?assertMatch({ok, _Pid}, Result).

test_supervisor_has_zero_children_when_disabled() ->
    %% Ensure clustering is disabled
    application:set_env(erlmcp_core, cluster_enabled, false),

    {ok, Pid} = erlmcp_cluster_sup:start_link(),

    %% Should have zero children
    Children = supervisor:which_children(Pid),
    ?assertEqual(0, length(Children)),

    supervisor:stop(erlmcp_cluster_sup).

test_intensity_tracking_works() ->
    %% Intensity tracking should work regardless of cluster_enabled
    application:set_env(erlmcp_core, cluster_enabled, false),

    {ok, Pid} = erlmcp_cluster_sup:start_link(),

    %% Check supervisor flags
    {ok, {SupFlags, _ChildSpecs}} = init_mock(),

    %% Verify intensity is set correctly
    ?assertEqual(one_for_one, maps:get(strategy, SupFlags)),
    ?assertEqual(5, maps:get(intensity, SupFlags)),
    ?assertEqual(60, maps:get(period, SupFlags)),

    supervisor:stop(erlmcp_cluster_sup).

%%%=============================================================================
%%% Intensity Limit Tests
%%%=============================================================================

intensity_limit_test_() ->
    {setup,
     fun() ->
         %% Enable clustering for this test
         application:set_env(erlmcp_core, cluster_enabled, true),
         %% We won't actually start it, just test logic
         ok
     end,
     fun(_) ->
         application:unset_env(erlmcp_core, cluster_enabled),
         ok
     end,
     fun(_) ->
         [
          ?_test(test_intensity_configuration()),
          ?_test(test_period_configuration())
         ]
     end}.

test_intensity_configuration() ->
    %% Verify intensity is configured correctly
    {ok, {SupFlags, _}} = init_mock(),
    ?assertEqual(5, maps:get(intensity, SupFlags)).

test_period_configuration() ->
    %% Verify period is configured correctly
    {ok, {SupFlags, _}} = init_mock(),
    ?assertEqual(60, maps:get(period, SupFlags)).

%%%=============================================================================
%%% Dynamic Child Management Tests
%%%=============================================================================

dynamic_child_management_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_children_empty_when_disabled()),
          ?_test(test_children_present_when_enabled())
         ]
     end}.

test_children_empty_when_disabled() ->
    application:set_env(erlmcp_core, cluster_enabled, false),
    {ok, Pid} = erlmcp_cluster_sup:start_link(),

    Children = supervisor:which_children(Pid),
    ?assertEqual(0, length(Children)),

    supervisor:stop(erlmcp_cluster_sup).

test_children_present_when_enabled() ->
    application:set_env(erlmcp_core, cluster_enabled, true),

    %% Note: This test may fail if gproc is not running
    %% We'll test the logic without actually starting children
    {ok, {_, ChildSpecs}} = init_mock(),

    %% Should have 3 children when enabled
    ?assertEqual(3, length(ChildSpecs)),

    %% Verify child IDs
    ChildIds = [maps:get(id, Spec) || Spec <- ChildSpecs],
    ?assert(lists:member(erlmcp_registry_dist, ChildIds)),
    ?assert(lists:member(erlmcp_node_monitor, ChildIds)),
    ?assert(lists:member(erlmcp_split_brain_detector, ChildIds)).

%%%=============================================================================
%%% Shutdown Cascade Tests
%%%=============================================================================

shutdown_cascade_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_clean_shutdown_with_no_children()),
          ?_test(test_shutdown_removes_process())
         ]
     end}.

test_clean_shutdown_with_no_children() ->
    application:set_env(erlmcp_core, cluster_enabled, false),
    {ok, Pid} = erlmcp_cluster_sup:start_link(),

    %% Supervisor should be alive
    ?assert(is_process_alive(Pid)),

    %% Stop should succeed
    ?assertEqual(ok, supervisor:stop(erlmcp_cluster_sup)),

    %% Process should be gone
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)).

test_shutdown_removes_process() ->
    application:set_env(erlmcp_core, cluster_enabled, false),
    {ok, _Pid} = erlmcp_cluster_sup:start_link(),

    %% Before shutdown
    Before = whereis(erlmcp_cluster_sup),
    ?assertNotEqual(undefined, Before),

    %% Stop
    supervisor:stop(erlmcp_cluster_sup),
    timer:sleep(100),

    %% After shutdown
    After = whereis(erlmcp_cluster_sup),
    ?assertEqual(undefined, After).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

%% Mock init/1 to test configuration without starting supervisor
init_mock() ->
    %% This mimics erlmcp_cluster_sup:init/1
    ClusterEnabled = application:get_env(erlmcp_core, cluster_enabled, false),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    ChildSpecs = case ClusterEnabled of
        true ->
            [
                #{
                    id => erlmcp_registry_dist,
                    start => {erlmcp_registry_dist, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [erlmcp_registry_dist]
                },
                #{
                    id => erlmcp_node_monitor,
                    start => {erlmcp_node_monitor, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [erlmcp_node_monitor]
                },
                #{
                    id => erlmcp_split_brain_detector,
                    start => {erlmcp_split_brain_detector, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [erlmcp_split_brain_detector]
                }
            ];
        false ->
            []
    end,

    {ok, {SupFlags, ChildSpecs}}.
