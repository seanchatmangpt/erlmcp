-module(erlmcp_recovery_manager_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests for erlmcp_recovery_manager module
%%====================================================================

%%--------------------------------------------------------------------
%% Basic Lifecycle Tests
%%--------------------------------------------------------------------

start_stop_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),
    ?assert(is_pid(Pid)),
    gen_server:stop(Pid).

start_with_options_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link([{default_policy, #{max_failures => 3}}]),
    ?assert(is_pid(Pid)),
    gen_server:stop(Pid).

%%--------------------------------------------------------------------
%% Component Registration Tests
%%--------------------------------------------------------------------

register_component_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),
    Policy = #{strategy => restart, max_failures => 5},
    Result = erlmcp_recovery_manager:register_component(test_component, Pid, Policy),
    ?assertEqual(ok, Result),
    gen_server:stop(Pid).

unregister_component_test() ->
    {ok, MgrPid} = erlmcp_recovery_manager:start_link(),
    Policy = #{strategy => restart},
    ok = erlmcp_recovery_manager:register_component(test_comp, MgrPid, Policy),
    ok = erlmcp_recovery_manager:unregister_component(test_comp),
    %% Verify component is unregistered
    {error, not_found} = erlmcp_recovery_manager:get_recovery_status(test_comp),
    gen_server:stop(MgrPid).

%%--------------------------------------------------------------------
%% Recovery Status Tests
%%--------------------------------------------------------------------

get_recovery_status_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),
    Policy = #{strategy => restart, max_failures => 3},
    ok = erlmcp_recovery_manager:register_component(my_component, Pid, Policy),
    {ok, Status} = erlmcp_recovery_manager:get_recovery_status(my_component),
    ?assert(is_map(Status)),
    ?assertMatch(#{id := my_component}, Status),
    ?assertMatch(#{pid := Pid}, Status),
    ?assertMatch(#{failures := 0}, Status),
    ?assertMatch(#{circuit_state := closed}, Status),
    gen_server:stop(Pid).

get_recovery_status_not_found_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),
    Result = erlmcp_recovery_manager:get_recovery_status(nonexistent),
    ?assertEqual({error, not_found}, Result),
    gen_server:stop(Pid).

get_all_recovery_status_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),
    Policy = #{strategy => restart},
    ok = erlmcp_recovery_manager:register_component(comp1, Pid, Policy),
    ok = erlmcp_recovery_manager:register_component(comp2, Pid, Policy),
    AllStatus = erlmcp_recovery_manager:get_all_recovery_status(),
    ?assert(is_map(AllStatus)),
    ?assert(maps:is_key(comp1, AllStatus)),
    ?assert(maps:is_key(comp2, AllStatus)),
    gen_server:stop(Pid).

%%--------------------------------------------------------------------
%% Circuit Breaker Tests
%%--------------------------------------------------------------------

get_circuit_status_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),
    Policy = #{strategy => restart},
    ok = erlmcp_recovery_manager:register_component(my_comp, Pid, Policy),
    Status = erlmcp_recovery_manager:get_circuit_status(my_comp),
    ?assertEqual(closed, Status),
    gen_server:stop(Pid).

get_circuit_status_not_found_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),
    Status = erlmcp_recovery_manager:get_circuit_status(nonexistent),
    ?assertEqual(not_found, Status),
    gen_server:stop(Pid).

get_all_circuit_status_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),
    Policy = #{strategy => circuit_breaker},
    ok = erlmcp_recovery_manager:register_component(comp1, Pid, Policy),
    ok = erlmcp_recovery_manager:register_component(comp2, Pid, Policy),
    AllStatus = erlmcp_recovery_manager:get_circuit_status(),
    ?assert(is_map(AllStatus)),
    ?assertEqual(2, maps:size(AllStatus)),
    gen_server:stop(Pid).

%%--------------------------------------------------------------------
%% Recovery Policy Tests
%%--------------------------------------------------------------------

set_recovery_policy_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),
    Policy = #{strategy => restart},
    ok = erlmcp_recovery_manager:register_component(my_comp, Pid, Policy),
    NewPolicy = #{strategy => circuit_breaker, max_failures => 3},
    Result = erlmcp_recovery_manager:set_recovery_policy(my_comp, NewPolicy),
    ?assertEqual(ok, Result),
    {ok, RetrievedPolicy} = erlmcp_recovery_manager:get_recovery_policy(my_comp),
    ?assertEqual(circuit_breaker, maps:get(strategy, RetrievedPolicy)),
    gen_server:stop(Pid).

set_recovery_policy_not_found_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),
    Policy = #{strategy => restart},
    Result = erlmcp_recovery_manager:set_recovery_policy(nonexistent, Policy),
    ?assertEqual({error, not_found}, Result),
    gen_server:stop(Pid).

get_recovery_policy_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),
    Policy = #{strategy => graceful_degradation, max_failures => 10},
    ok = erlmcp_recovery_manager:register_component(my_comp, Pid, Policy),
    {ok, RetrievedPolicy} = erlmcp_recovery_manager:get_recovery_policy(my_comp),
    ?assertEqual(graceful_degradation, maps:get(strategy, RetrievedPolicy)),
    ?assertEqual(10, maps:get(max_failures, RetrievedPolicy)),
    gen_server:stop(Pid).

get_recovery_policy_not_found_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),
    Result = erlmcp_recovery_manager:get_recovery_policy(nonexistent),
    ?assertEqual({error, not_found}, Result),
    gen_server:stop(Pid).

%%--------------------------------------------------------------------
%% Metrics Tests
%%--------------------------------------------------------------------

get_metrics_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),
    Metrics = erlmcp_recovery_manager:get_metrics(),
    ?assert(is_map(Metrics)),
    ?assert(maps:is_key(total_components, Metrics)),
    ?assert(maps:is_key(total_failures, Metrics)),
    ?assert(maps:is_key(total_recoveries, Metrics)),
    gen_server:stop(Pid).

reset_metrics_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),
    Policy = #{strategy => restart},
    ok = erlmcp_recovery_manager:register_component(comp1, Pid, Policy),
    ok = erlmcp_recovery_manager:unregister_component(comp1),
    ok = erlmcp_recovery_manager:reset_metrics(),
    Metrics = erlmcp_recovery_manager:get_metrics(),
    ?assertEqual(0, maps:get(total_failures, Metrics)),
    ?assertEqual(0, maps:get(total_recoveries, Metrics)),
    gen_server:stop(Pid).

%%--------------------------------------------------------------------
%% Recovery Trigger Tests
%%--------------------------------------------------------------------

trigger_recovery_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),
    Policy = #{strategy => restart, max_failures => 5},
    ok = erlmcp_recovery_manager:register_component(my_comp, Pid, Policy),
    ok = erlmcp_recovery_manager:trigger_recovery(my_comp, test_failure),
    %% Give it time to process
    timer:sleep(100),
    {ok, Status} = erlmcp_recovery_manager:get_recovery_status(my_comp),
    ?assertMatch(#{failures := Failures} when Failures > 0, Status),
    gen_server:stop(Pid).

trigger_recovery_with_options_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),
    Policy = #{strategy => restart},
    ok = erlmcp_recovery_manager:register_component(my_comp, Pid, Policy),
    ok = erlmcp_recovery_manager:trigger_recovery(my_comp, custom_failure, #{custom => true}),
    {ok, Status} = erlmcp_recovery_manager:get_recovery_status(my_comp),
    ?assert(is_map(Status)),
    gen_server:stop(Pid).

%%--------------------------------------------------------------------
%% Integration Tests
%%--------------------------------------------------------------------

full_recovery_lifecycle_test() ->
    {ok, MgrPid} = erlmcp_recovery_manager:start_link(),
    Policy = #{strategy => restart, max_failures => 3},

    %% Register component
    ok = erlmcp_recovery_manager:register_component(lifecycle_comp, MgrPid, Policy),

    %% Check initial status
    {ok, InitialStatus} = erlmcp_recovery_manager:get_recovery_status(lifecycle_comp),
    ?assertEqual(0, maps:get(failures, InitialStatus)),
    ?assertEqual(closed, maps:get(circuit_state, InitialStatus)),

    %% Trigger recovery
    ok = erlmcp_recovery_manager:trigger_recovery(lifecycle_comp, simulated_failure),
    timer:sleep(50),

    %% Check status after failure
    {ok, AfterFailureStatus} = erlmcp_recovery_manager:get_recovery_status(lifecycle_comp),
    ?assert(maps:get(failures, AfterFailureStatus) > 0),

    %% Check circuit status
    CircuitState = erlmcp_recovery_manager:get_circuit_status(lifecycle_comp),
    ?assert(lists:member(CircuitState, [closed, open, half_open])),

    %% Unregister component
    ok = erlmcp_recovery_manager:unregister_component(lifecycle_comp),

    %% Verify unregistered
    {error, not_found} = erlmcp_recovery_manager:get_recovery_status(lifecycle_comp),

    gen_server:stop(MgrPid).
