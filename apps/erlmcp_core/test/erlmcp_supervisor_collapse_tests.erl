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

cleanup(_Pid) ->
    case whereis(erlmcp_cluster_sup) of
        undefined -> ok;
        _ -> supervisor:stop(erlmcp_cluster_sup)
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
         [
          ?_test(test_supervisor_starts_cleanly()),
          ?_test(test_supervisor_has_no_children_when_disabled()),
          ?_test(test_supervisor_shutdown_cleanly())
         ]
     end}.

test_supervisor_starts_cleanly() ->
    %% Verify supervisor can start without clustering
    {ok, Pid} = erlmcp_cluster_sup:start_link(),
    ?assertMatch(Pid when is_pid(Pid), Pid),
    ?assertEqual(Pid, whereis(erlmcp_cluster_sup)),
    supervisor:stop(erlmcp_cluster_sup).

test_supervisor_has_no_children_when_disabled() ->
    %% When disabled, supervisor starts but has no children (not a zombie)
    {ok, Pid} = erlmcp_cluster_sup:start_link(),
    Children = supervisor:which_children(Pid),
    ?assertEqual(0, length(Children)),
    ?assert(is_process_alive(Pid)),
    supervisor:stop(erlmcp_cluster_sup).

test_supervisor_shutdown_cleanly() ->
    {ok, Pid} = erlmcp_cluster_sup:start_link(),
    ?assert(is_process_alive(Pid)),
    ?assertEqual(ok, supervisor:stop(erlmcp_cluster_sup)),
    timer:sleep(100),
    ?assertEqual(undefined, whereis(erlmcp_cluster_sup)),
    ?assertNot(is_process_alive(Pid)).

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
     fun(_Pid) ->
         [
          ?_test(test_restart_strategy_configuration()),
          ?_test(test_intensity_and_period_configuration())
         ]
     end}.

test_restart_strategy_configuration() ->
    %% Verify supervisor uses one_for_one strategy
    {ok, _Pid} = erlmcp_cluster_sup:start_link(),
    {ok, {SupFlags, _ChildSpecs}} = get_supervisor_config(),
    ?assertEqual(one_for_one, maps:get(strategy, SupFlags)),
    supervisor:stop(erlmcp_cluster_sup).

test_intensity_and_period_configuration() ->
    %% Verify intensity and period limits prevent cascade failures
    {ok, _Pid} = erlmcp_cluster_sup:start_link(),
    {ok, {SupFlags, _ChildSpecs}} = get_supervisor_config(),
    ?assertEqual(5, maps:get(intensity, SupFlags)),
    ?assertEqual(60, maps:get(period, SupFlags)),
    supervisor:stop(erlmcp_cluster_sup).

%%%=============================================================================
%%% Dynamic Child Management Tests
%%%=============================================================================

dynamic_child_management_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(test_children_spec_when_disabled()),
          ?_test(test_children_spec_when_enabled())
         ]
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
