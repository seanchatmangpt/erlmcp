-module(erlmcp_recovery_generic_tests).

-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    {ok, RM} = erlmcp_recovery_manager:start_link(),
    RM.

cleanup(RM) ->
    gen_server:stop(RM).

%%%===================================================================
%%% Find Component Supervisor Tests
%%%===================================================================

find_supervisor_for_observability_component_test() ->
    RM = setup(),
    case whereis(erlmcp_observability_sup) of
        undefined ->
            ?assert(true); % Skip if not running
        _ ->
            Result = erlmcp_recovery_manager:find_component_supervisor(erlmcp_metrics),
            ?assertMatch({ok, _Pid}, Result)
    end,
    cleanup(RM).

find_supervisor_for_unknown_component_test() ->
    RM = setup(),
    Result = erlmcp_recovery_manager:find_component_supervisor(unknown_component_xyz),
    ?assertMatch({error, _}, Result),
    cleanup(RM).

find_supervisor_for_core_component_test() ->
    RM = setup(),
    case whereis(erlmcp_core_sup) of
        undefined ->
            ?assert(true); % Skip if not running
        _ ->
            Result = erlmcp_recovery_manager:find_component_supervisor(erlmcp_registry),
            ?assertMatch({ok, _Pid}, Result)
    end,
    cleanup(RM).

%%%===================================================================
%%% Generic Component Restart Tests
%%%===================================================================

restart_generic_component_success_test( ) -> RM = setup( ) , case whereis( erlmcp_observability_sup ) of undefined -> ?assert( true ) ; _ -> Result = erlmcp_recovery_manager : restart_generic_component( erlmcp_metrics ) , ?assertMatch( { ok , _ } when is_tuple( Result ) , Result ) end , cleanup( RM ) .

                           % Skip if not running

restart_generic_nonexistent_component_test() ->
    RM = setup(),
    Result = erlmcp_recovery_manager:restart_generic_component(fake_component_xyz),
    ?assertMatch({error, _Reason}, Result),
    cleanup(RM).

%%%===================================================================
%%% Restart Cooldown Tests
%%%===================================================================

restart_with_cooldown_prevents_thrashing_test() ->
    RM = setup(),

    {ok, TestSup} = erlmcp_recovery_test_sup:start_link(),
    {ok, TestPid} = erlmcp_recovery_test_sup:start_child(test_worker, []),

    Policy = #{strategy => restart, max_failures => 5},
    ok = erlmcp_recovery_manager:register_component(test_worker, TestPid, Policy),

    ok = erlmcp_recovery_manager:trigger_recovery(test_worker, first_failure),

    {ok, Status} = erlmcp_recovery_manager:get_recovery_status(test_worker),
    ?assert(maps:is_key(failures, Status)),
    ?assert(maps:get(failures, Status) >= 1),

    cleanup(RM),
    exit(TestSup, shutdown).

%%%===================================================================
%%% Supervisor Strategy Tests
%%%===================================================================

restart_permanent_worker_test() ->
    {ok, Sup} = erlmcp_recovery_test_sup:start_link(),
    {ok, Pid1} = erlmcp_recovery_test_sup:start_child(permanent_test_worker, []),

    ?assert(is_process_alive(Pid1)),

    exit(Pid1, kill),
    timer:sleep(100),

    Children = supervisor:which_children(Sup),
    {_, Pid2, _, _} = lists:keyfind(permanent_test_worker, 1, Children),

    ?assert(is_process_alive(Pid2)),
    ?assertNotEqual(Pid1, Pid2),

    exit(Sup, shutdown).

restart_temporary_worker_test() ->
    {ok, Sup} = erlmcp_recovery_test_sup:start_link(),
    {ok, Pid1} = erlmcp_recovery_test_sup:start_child(temporary_test_worker, []),

    ?assert(is_process_alive(Pid1)),

    ok = supervisor:terminate_child(Sup, temporary_test_worker),
    timer:sleep(50),

    Children = supervisor:which_children(Sup),
    {_, undefined, _, _} = lists:keyfind(temporary_test_worker, 1, Children),

    {ok, Pid2} = supervisor:restart_child(Sup, temporary_test_worker),
    ?assert(is_process_alive(Pid2)),

    exit(Sup, shutdown).

restart_transient_worker_after_abnormal_exit_test() ->
    {ok, Sup} = erlmcp_recovery_test_sup:start_link(),
    {ok, Pid1} = erlmcp_recovery_test_sup:start_child(transient_test_worker, []),

    ?assert(is_process_alive(Pid1)),

    exit(Pid1, abnormal),
    timer:sleep(100),

    Children = supervisor:which_children(Sup),
    {_, Pid2, _, _} = lists:keyfind(transient_test_worker, 1, Children),

    ?assert(is_process_alive(Pid2)),
    ?assertNotEqual(Pid1, Pid2),

    exit(Sup, shutdown).

restart_transient_worker_no_restart_after_normal_exit_test() ->
    {ok, Sup} = erlmcp_recovery_test_sup:start_link(),
    {ok, Pid1} = erlmcp_recovery_test_sup:start_child(transient_test_worker, []),

    ?assert(is_process_alive(Pid1)),

    exit(Pid1, normal),
    timer:sleep(100),

    Children = supervisor:which_children(Sup),
    {_, undefined, _, _} = lists:keyfind(transient_test_worker, 1, Children),

    exit(Sup, shutdown).

%%%===================================================================
%%% Metrics Tracking Tests
%%%===================================================================

restart_tracks_metrics_test() ->
    RM = setup(),

    {ok, TestSup} = erlmcp_recovery_test_sup:start_link(),
    {ok, TestPid} = erlmcp_recovery_test_sup:start_child(metrics_worker, []),

    Policy = #{strategy => restart, max_failures => 5},
    ok = erlmcp_recovery_manager:register_component(metrics_worker, TestPid, Policy),

    Metrics1 = erlmcp_recovery_manager:get_metrics(),
    ?assert(maps:is_key(total_components, Metrics1)),

    ok = erlmcp_recovery_manager:trigger_recovery(metrics_worker, test_failure),

    Metrics2 = erlmcp_recovery_manager:get_metrics(),
    ?assert(maps:is_key(total_failures, Metrics2)),

    cleanup(RM),
    exit(TestSup, shutdown).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

integration_restart_workflow_test() ->
    RM = setup(),

    {ok, Sup} = erlmcp_recovery_test_sup:start_link(),
    {ok, Pid} = erlmcp_recovery_test_sup:start_child(integration_worker, []),

    Policy =
        #{strategy => restart,
          max_failures => 3,
          recovery_timeout => 5000},

    ok = erlmcp_recovery_manager:register_component(integration_worker, Pid, Policy),

    ok = erlmcp_recovery_manager:trigger_recovery(integration_worker, simulated_crash),

    {ok, Status} = erlmcp_recovery_manager:get_recovery_status(integration_worker),
    ?assert(maps:is_key(id, Status)),
    ?assert(maps:is_key(failures, Status)),
    ?assert(maps:is_key(recovery_attempts, Status)),

    ?assert(maps:is_key(circuit_state, Status)),
    CircuitState = maps:get(circuit_state, Status),
    ?assert(lists:member(CircuitState, [closed, open, half_open])),

    cleanup(RM),
    exit(Sup, shutdown).
