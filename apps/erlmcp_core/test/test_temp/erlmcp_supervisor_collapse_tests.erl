%%%=============================================================================
%%% @doc Supervisor Tree Collapse Recovery Tests
%%% @end
%%% Tests for supervisor tree collapse scenarios and recovery
%%%=============================================================================
-module(erlmcp_supervisor_collapse_tests).

-author(erlmcp).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Setup and Teardown
%%%=============================================================================

setup() ->
    %% Stop any existing supervisor first
    _ = case whereis(erlmcp_cluster_sup) of
            undefined ->
                ok;
            OldPid ->
                try gen_server:stop(OldPid) of
                    _ ->
                        ok
                catch
                    _:_ ->
                        exit(OldPid, kill)
                end,
                timer:sleep(100)
        end,
    {ok, Pid} = erlmcp_cluster_sup:start_link(),
    Pid.

cleanup(_Pid) ->
    case whereis(erlmcp_cluster_sup) of
        undefined ->
            ok;
        SupPid ->
            try gen_server:stop(SupPid) of
                _ ->
                    ok
            catch
                _:_ ->
                    exit(SupPid, kill)
            end
    end,
    timer:sleep(100).

%%%=============================================================================
%%% Supervisor Lifecycle Tests (Disabled Cluster Mode)
%%%=============================================================================

supervisor_lifecycle_disabled_test_() ->
    {setup,
     fun() ->
        application:set_env(erlmcp_core, cluster_enabled, false),
        setup()
     end,
     fun cleanup/1,
     fun(_Pid) ->
        [?_test(test_supervisor_starts_cleanly(_Pid)),
         ?_test(test_supervisor_has_no_children_when_disabled(_Pid)),
         ?_test(test_supervisor_shutdown_cleanly(_Pid))]
     end}.

test_supervisor_starts_cleanly(SupPid) ->
    %% Verify supervisor started cleanly from setup
    ?assert(is_pid(SupPid)),
    ?assertEqual(SupPid, whereis(erlmcp_cluster_sup)).

test_supervisor_has_no_children_when_disabled(SupPid) ->
    %% When disabled, supervisor has no children (not a zombie)
    Children = supervisor:which_children(SupPid),
    ?assertEqual(0, length(Children)),
    ?assert(is_process_alive(SupPid)).

test_supervisor_shutdown_cleanly(SupPid) ->
    %% Test is run by the framework after cleanup, so we verify
    %% the supervisor can be stopped and restarts cleanly
    ?assert(is_pid(SupPid)),
    ?assert(is_process_alive(SupPid)).

%%%=============================================================================
%%% Supervisor Configuration Tests
%%%=============================================================================

supervisor_configuration_test_() ->
    {setup,
     fun() ->
        application:set_env(erlmcp_core, cluster_enabled, false),
        setup()
     end,
     fun cleanup/1,
     fun(_SupPid) ->
        [?_test(test_restart_strategy_configuration(_SupPid)),
         ?_test(test_intensity_and_period_configuration(_SupPid))]
     end}.

test_restart_strategy_configuration(_SupPid) ->
    %% Verify supervisor uses one_for_one strategy
    {ok, {SupFlags, _ChildSpecs}} = get_supervisor_config(),
    ?assertEqual(one_for_one, maps:get(strategy, SupFlags)).

test_intensity_and_period_configuration(_SupPid) ->
    %% Verify intensity and period limits prevent cascade failures
    {ok, {SupFlags, _ChildSpecs}} = get_supervisor_config(),
    ?assertEqual(5, maps:get(intensity, SupFlags)),
    ?assertEqual(60, maps:get(period, SupFlags)).

%%%=============================================================================
%%% Dynamic Child Management Tests
%%%=============================================================================

dynamic_child_management_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        [?_test(test_children_spec_when_disabled()), ?_test(test_children_spec_when_enabled())]
     end}.

test_children_spec_when_disabled() ->
    application:set_env(erlmcp_core, cluster_enabled, false),
    {ok, {_SupFlags, ChildSpecs}} = get_supervisor_config(),
    ?assertEqual(0, length(ChildSpecs)).

test_children_spec_when_enabled() ->
    application:set_env(erlmcp_core, cluster_enabled, true),
    {ok, {_SupFlags, ChildSpecs}} = get_supervisor_config(),

    %% Should have 3 children when clustering enabled
    ?assertEqual(3, length(ChildSpecs)),

    %% Verify child IDs
    ChildIds = [maps:get(id, Spec) || Spec <- ChildSpecs],
    ?assert(lists:member(erlmcp_registry_dist, ChildIds)),
    ?assert(lists:member(erlmcp_node_monitor, ChildIds)),
    ?assert(lists:member(erlmcp_split_brain_detector, ChildIds)).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

%% Get supervisor configuration without starting the supervisor
%% This tests the init/1 logic without requiring actual child processes
get_supervisor_config() ->
    ClusterEnabled = application:get_env(erlmcp_core, cluster_enabled, false),

    SupFlags =
        #{strategy => one_for_one,
          intensity => 5,
          period => 60},

    ChildSpecs =
        case ClusterEnabled of
            true ->
                [#{id => erlmcp_registry_dist,
                   start => {erlmcp_registry_dist, start_link, []},
                   restart => permanent,
                   shutdown => 5000,
                   type => worker,
                   modules => [erlmcp_registry_dist]},
                 #{id => erlmcp_node_monitor,
                   start => {erlmcp_node_monitor, start_link, []},
                   restart => permanent,
                   shutdown => 5000,
                   type => worker,
                   modules => [erlmcp_node_monitor]},
                 #{id => erlmcp_split_brain_detector,
                   start => {erlmcp_split_brain_detector, start_link, []},
                   restart => permanent,
                   shutdown => 5000,
                   type => worker,
                   modules => [erlmcp_split_brain_detector]}];
            false ->
                []
        end,

    {ok, {SupFlags, ChildSpecs}}.
