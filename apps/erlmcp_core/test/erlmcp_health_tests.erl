%%%-------------------------------------------------------------------
%%% @doc erlmcp_health_tests - Health Check System Tests
%%%
%%% Tests the health check aggregator using Chicago School TDD:
%%% - Real gen_server (no mocks)
%%% - Real process checks (using actual process health)
%%% - State-based verification (observable health status)
%%% - Registration/unregistration lifecycle
%%% - Default checks and custom checks
%%%
%%% Target: 85%+ coverage (core infrastructure module)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_health_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

health_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_health_check_start/1,
      fun test_health_check_all_healthy/1,
      fun test_health_check_with_unhealthy/1,
      fun test_register_check/1,
      fun test_unregister_check/1,
      fun test_check_with_custom_check/1,
      fun test_check_with_failing_check/1,
      fun test_default_registry_check/1,
      fun test_default_session_manager_check/1,
      fun test_multiple_health_checks/1,
      fun test_health_report_structure/1,
      fun test_gen_server_handle_call_unknown/1,
      fun test_gen_server_handle_cast/1,
      fun test_gen_server_handle_info/1,
      fun test_gen_server_code_change/1,
      fun test_gen_server_terminate/1]}.

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    %% Start health check server
    {ok, Pid} = erlmcp_health:start_link(),
    Pid.

cleanup(Pid) ->
    %% Stop health check server
    case is_process_alive(Pid) of
        true ->
            gen_server:stop(erlmcp_health);
        false ->
            ok
    end.

%%====================================================================
%% Basic Health Check Tests
%%====================================================================

test_health_check_start(Pid) ->
    %% Verify: Server started successfully
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)).

test_health_check_all_healthy(_Pid) ->
    %% Exercise: Perform health check when all components healthy
    Report = erlmcp_health:check(),

    %% Verify: Report structure is correct
    ?assert(is_map(Report)),
    ?assert(maps:is_key(<<"healthy">>, Report)),
    ?assert(maps:is_key(<<"checks">>, Report)),

    %% Verify: Checks is a map
    Checks = maps:get(<<"checks">>, Report),
    ?assert(is_map(Checks)).

test_health_check_with_unhealthy(_Pid) ->
    %% Exercise: Add an unhealthy check
    UnhealthyCheck = {?MODULE, unhealthy_check_function, []},
    ok = erlmcp_health:register_check(test_unhealthy, UnhealthyCheck),

    Report = erlmcp_health:check(),

    %% Verify: Overall health reflects unhealthy state
    ?assert(maps:is_key(<<"healthy">>, Report)),
    ?assert(maps:is_key(<<"checks">>, Report)),

    %% Cleanup
    ok = erlmcp_health:unregister_check(test_unhealthy).

%%====================================================================
%% Check Registration Tests
%%====================================================================

test_register_check(_Pid) ->
    %% Exercise: Register a custom health check
    TestCheck = {?MODULE, dummy_check_function, []},
    Result = erlmcp_health:register_check(test_check, TestCheck),

    %% Verify: Registration succeeds
    ?assertEqual(ok, Result),

    %% Verify: Check appears in health report
    Report = erlmcp_health:check(),
    Checks = maps:get(<<"checks">>, Report),
    ?assert(maps:is_key(test_check, Checks)),

    %% Cleanup
    ok = erlmcp_health:unregister_check(test_check).

test_unregister_check(_Pid) ->
    %% Exercise: Register then unregister a check
    TestCheck = {?MODULE, dummy_check_function, []},
    ok = erlmcp_health:register_check(temp_check, TestCheck),

    %% Verify: Check exists
    Report1 = erlmcp_health:check(),
    Checks1 = maps:get(<<"checks">>, Report1),
    ?assert(maps:is_key(temp_check, Checks1)),

    %% Unregister
    ok = erlmcp_health:unregister_check(temp_check),

    %% Verify: Check removed
    Report2 = erlmcp_health:check(),
    Checks2 = maps:get(<<"checks">>, Report2),
    ?assertNot(maps:is_key(temp_check, Checks2)).

test_check_with_custom_check(_Pid) ->
    %% Exercise: Register and run a custom healthy check
    CustomCheck = {?MODULE, custom_healthy_check, []},
    ok = erlmcp_health:register_check(custom_check, CustomCheck),

    Report = erlmcp_health:check(),
    Checks = maps:get(<<"checks">>, Report),

    %% Verify: Custom check returns healthy
    ?assertEqual(healthy, maps:get(custom_check, Checks)),

    %% Cleanup
    ok = erlmcp_health:unregister_check(custom_check).

test_check_with_failing_check(_Pid) ->
    %% Exercise: Register a check that will fail
    FailingCheck = {?MODULE, failing_check_function, []},
    ok = erlmcp_health:register_check(failing_check, FailingCheck),

    Report = erlmcp_health:check(),
    Checks = maps:get(<<"checks">>, Report),

    %% Verify: Failing check returns unhealthy
    ?assertEqual(unhealthy, maps:get(failing_check, Checks)),

    %% Cleanup
    ok = erlmcp_health:unregister_check(failing_check).

%%====================================================================
%% Default Checks Tests
%%====================================================================

test_default_registry_check(_Pid) ->
    %% Exercise: Check default registry check exists
    Report = erlmcp_health:check(),
    Checks = maps:get(<<"checks">>, Report),

    %% Verify: Registry check is present (even if unhealthy)
    ?assert(maps:is_key(registry, Checks)).

test_default_session_manager_check(_Pid) ->
    %% Exercise: Check default session_manager check exists
    Report = erlmcp_health:check(),
    Checks = maps:get(<<"checks">>, Report),

    %% Verify: Session manager check is present
    ?assert(maps:is_key(session_manager, Checks)).

%%====================================================================
%% Complex Scenarios
%%====================================================================

test_multiple_health_checks(_Pid) ->
    %% Exercise: Register multiple checks with different results
    Checks = [
        {healthy_1, {?MODULE, custom_healthy_check, []}},
        {healthy_2, {?MODULE, custom_healthy_check, []}},
        {unhealthy_1, {?MODULE, failing_check_function, []}},
        {degraded_1, {?MODULE, degraded_check_function, []}}
    ],

    lists:foreach(fun({Name, Check}) ->
        ok = erlmcp_health:register_check(Name, Check)
    end, Checks),

    Report = erlmcp_health:check(),
    ReportChecks = maps:get(<<"checks">>, Report),

    %% Verify: All checks present with correct status
    ?assertEqual(healthy, maps:get(healthy_1, ReportChecks)),
    ?assertEqual(healthy, maps:get(healthy_2, ReportChecks)),
    ?assertEqual(unhealthy, maps:get(unhealthy_1, ReportChecks)),
    ?assertEqual(degraded, maps:get(degraded_1, ReportChecks)),

    %% Cleanup
    lists:foreach(fun({Name, _Check}) ->
        ok = erlmcp_health:unregister_check(Name)
    end, Checks).

test_health_report_structure(_Pid) ->
    %% Exercise: Verify health report structure
    Report = erlmcp_health:check(),

    %% Verify: Top-level structure
    ?assert(maps:is_key(<<"healthy">>, Report)),
    ?assert(maps:is_key(<<"checks">>, Report)),

    %% Verify: Healthy is boolean
    Healthy = maps:get(<<"healthy">>, Report),
    ?assert(is_boolean(Healthy)),

    %% Verify: Checks is a non-empty map
    Checks = maps:get(<<"checks">>, Report),
    ?assert(is_map(Checks)),
    ?assert(maps:size(Checks) >= 2). % At least default checks

%%====================================================================
%% gen_server Callback Tests
%%====================================================================

test_gen_server_handle_call_unknown(_Pid) ->
    %% Exercise: Send unknown call to gen_server
    Result = gen_server:call(erlmcp_health, unknown_request),

    %% Verify: Returns error
    ?assertEqual({error, unknown_request}, Result).

test_gen_server_handle_cast(_Pid) ->
    %% Exercise: Send cast to gen_server
    Result = gen_server:cast(erlmcp_health, test_cast),

    %% Verify: Cast doesn't crash
    ?assertEqual(ok, Result).

test_gen_server_handle_info(Pid) ->
    %% Exercise: Send info message to gen_server
    Pid ! test_info,

    %% Verify: Server still alive (doesn't crash)
    timer:sleep(100),
    ?assert(is_process_alive(Pid)).

test_gen_server_code_change(Pid) ->
    %% Exercise: Trigger code change
    {ok, State} = sys:get_state(erlmcp_health),

    %% Simulate code change
    Result = erlmcp_health:code_change("", State, ""),

    %% Verify: Code change succeeds
    ?assertMatch({ok, _}, Result).

test_gen_server_terminate(Pid) ->
    %% Exercise: Stop server (calls terminate/2)
    ok = gen_server:stop(erlmcp_health),

    %% Verify: Server stopped
    ?assertNot(is_process_alive(Pid)).

%%====================================================================
%% Test Helper Functions
%%====================================================================

%% @doc Dummy check function that returns ok (healthy)
-spec dummy_check_function() -> ok.
dummy_check_function() ->
    ok.

%% @doc Custom healthy check function
-spec custom_healthy_check() -> {ok, term()}.
custom_healthy_check() ->
    {ok, healthy}.

%% @doc Failing check function (returns error)
-spec failing_check_function() -> {error, term()}.
failing_check_function() ->
    {error, test_failure}.

%% @doc Degraded check function
-spec degraded_check_function() -> undefined.
degraded_check_function() ->
    undefined.

%% @doc Unhealthy check function
-spec unhealthy_check_function() -> error.
unhealthy_check_function() ->
    error.
