%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_health_monitor module following Chicago School TDD
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior through API calls only
%%% - Use REAL erlmcp_health_monitor gen_server (no mocks, no dummy processes)
%%% - NO internal state inspection (test API boundaries only)
%%% - NO record duplication (respect encapsulation)
%%% - Split into focused modules (component health tests)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_health_monitor_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Setup/teardown for health monitor
health_monitor_test_() ->
    {setup,
     fun setup_health_monitor/0,
     fun cleanup_health_monitor/1,
     [
         {"Start and stop health monitor", fun test_start_stop/0},
         {"Register component", fun test_register_component/0},
         {"Unregister component", fun test_unregister_component/0},
         {"Get component health", fun test_get_component_health/0},
         {"Get all component health", fun test_get_all_component_health/0},
         {"Get system health", fun test_get_system_health/0},
         {"Set health check config", fun test_set_health_check_config/0},
         {"Report circuit breaker", fun test_report_circuit_breaker/0},
         {"Report degradation", fun test_report_degradation/0},
         {"Reset health status", fun test_reset_health_status/0}
     ]}.

setup_health_monitor() ->
    {ok, Pid} = erlmcp_health_monitor:start_link(),
    Pid.

cleanup_health_monitor(Pid) ->
    catch gen_server:stop(Pid),
    catch unregister(erlmcp_health_monitor).

%%====================================================================
%% Lifecycle Tests
%%====================================================================

%% Test starting and stopping health monitor
test_start_stop() ->
    % Start health monitor
    {ok, Pid} = erlmcp_health_monitor:start_link(),
    ?assert(is_pid(Pid)),

    % Stop health monitor
    ok = gen_server:stop(Pid),
    ?assertNot(is_process_alive(Pid)).

%%====================================================================
%% Component Registration Tests
%%====================================================================

%% Test registering a component
test_register_component() ->
    {ok, Pid} = erlmcp_health_monitor:start_link(),
    TestPid = self(),

    % Register component
    Result = erlmcp_health_monitor:register_component(test_component, TestPid),
    ?assertEqual(ok, Result),

    gen_server:stop(Pid).

%% Test unregistering a component
test_unregister_component() ->
    {ok, MonitorPid} = erlmcp_health_monitor:start_link(),
    TestPid = self(),

    % Register component
    ok = erlmcp_health_monitor:register_component(temp_comp, TestPid),

    % Unregister component
    ok = erlmcp_health_monitor:unregister_component(temp_comp),

    % Verify component is not found
    Result = erlmcp_health_monitor:get_component_health(temp_comp),
    ?assertEqual(not_found, Result),

    gen_server:stop(MonitorPid).

%%====================================================================
%% Health Query Tests
%%====================================================================

%% Test getting component health
test_get_component_health() ->
    {ok, MonitorPid} = erlmcp_health_monitor:start_link(),
    TestPid = self(),

    % Register component
    ok = erlmcp_health_monitor:register_component(test_comp, TestPid),

    % Get component health
    Result = erlmcp_health_monitor:get_component_health(test_comp),

    % Should return a health status atom (unknown, healthy, unhealthy, degraded)
    ?assert(is_atom(Result)),
    ?assert(lists:member(Result, [unknown, healthy, unhealthy, degraded])),

    gen_server:stop(MonitorPid).

%% Test getting health of non-existent component
get_component_health_not_found_test_() ->
    {setup,
     fun setup_health_monitor/0,
     fun cleanup_health_monitor/1,
     fun(_Pid) ->
         [
          ?_test(begin
              % Query non-existent component
              Result = erlmcp_health_monitor:get_component_health(nonexistent),
              ?assertEqual(not_found, Result)
          end)
         ]
     end}.

%% Test getting all component health
test_get_all_component_health() ->
    {ok, MonitorPid} = erlmcp_health_monitor:start_link(),
    TestPid = self(),

    % Register multiple components
    ok = erlmcp_health_monitor:register_component(comp1, TestPid),
    ok = erlmcp_health_monitor:register_component(comp2, TestPid),

    % Get all component health
    Result = erlmcp_health_monitor:get_all_component_health(),

    % Should return a map with at least 2 components
    ?assert(is_map(Result)),
    ?assert(maps:size(Result) >= 2),

    gen_server:stop(MonitorPid).

%% Test getting system health
test_get_system_health() ->
    {ok, MonitorPid} = erlmcp_health_monitor:start_link(),

    % Get system health
    Result = erlmcp_health_monitor:get_system_health(),

    % Verify structure
    ?assert(is_map(Result)),
    ?assert(maps:is_key(overall_status, Result)),
    ?assert(maps:is_key(system_metrics, Result)),

    gen_server:stop(MonitorPid).

%%====================================================================
%% Configuration Tests
%%====================================================================

%% Test setting health check configuration
test_set_health_check_config() ->
    {ok, MonitorPid} = erlmcp_health_monitor:start_link(),
    TestPid = self(),

    % Register component
    ok = erlmcp_health_monitor:register_component(config_comp, TestPid),

    % Set health check config
    Config = #{
        check_interval => 60000,
        timeout => 10000
    },
    Result = erlmcp_health_monitor:set_health_check_config(config_comp, Config),
    ?assertEqual(ok, Result),

    gen_server:stop(MonitorPid).

%%====================================================================
%% Circuit Breaker Tests
%%====================================================================

%% Test reporting circuit breaker state
test_report_circuit_breaker() ->
    {ok, MonitorPid} = erlmcp_health_monitor:start_link(),
    TestPid = self(),

    % Register component
    ok = erlmcp_health_monitor:register_component(cb_comp, TestPid),

    % Report circuit breaker open
    ok = erlmcp_health_monitor:report_circuit_breaker(cb_comp, open),

    % Verify circuit breaker is reflected in health status
    AllHealth = erlmcp_health_monitor:get_all_component_health(),
    CbCompHealth = maps:get(cb_comp, AllHealth),

    % Circuit breaker should be active (observable behavior)
    ?assertEqual(true, maps:get(circuit_breaker_active, CbCompHealth)),

    gen_server:stop(MonitorPid).

%%====================================================================
%% Degradation Tests
%%====================================================================

%% Test reporting degradation
test_report_degradation() ->
    {ok, MonitorPid} = erlmcp_health_monitor:start_link(),
    TestPid = self(),

    % Register component
    ok = erlmcp_health_monitor:register_component(degraded_comp, TestPid),

    % Report degradation
    ok = erlmcp_health_monitor:report_degradation(degraded_comp),

    % Verify status is degraded
    Status = erlmcp_health_monitor:get_component_health(degraded_comp),
    ?assertEqual(degraded, Status),

    gen_server:stop(MonitorPid).

%%====================================================================
%% Reset Tests
%%====================================================================

%% Test resetting health status
test_reset_health_status() ->
    {ok, MonitorPid} = erlmcp_health_monitor:start_link(),
    TestPid = self(),

    % Register component
    ok = erlmcp_health_monitor:register_component(reset_comp, TestPid),

    % Report degradation
    ok = erlmcp_health_monitor:report_degradation(reset_comp),

    % Verify degraded status
    DegradedStatus = erlmcp_health_monitor:get_component_health(reset_comp),
    ?assertEqual(degraded, DegradedStatus),

    % Reset health status
    ok = erlmcp_health_monitor:reset_health_status(),

    % Verify status is reset to unknown
    ResetStatus = erlmcp_health_monitor:get_component_health(reset_comp),
    ?assertEqual(unknown, ResetStatus),

    gen_server:stop(MonitorPid).

%%====================================================================
%% Edge Case Tests
%%====================================================================

%% Test registering component with health check function
register_with_check_fun_test_() ->
    {setup,
     fun setup_health_monitor/0,
     fun cleanup_health_monitor/1,
     fun(_Pid) ->
         [
          ?_test(begin
              TestPid = self(),

              % Define a health check function
              CheckFun = fun() -> healthy end,

              % Register component with health check function
              Result = erlmcp_health_monitor:register_component(
                  checked_comp,
                  TestPid,
                  CheckFun
              ),
              ?assertEqual(ok, Result)
          end)
         ]
     end}.

%% Test multiple registrations of same component
multiple_registration_test_() ->
    {setup,
     fun setup_health_monitor/0,
     fun cleanup_health_monitor/1,
     fun(_Pid) ->
         [
          ?_test(begin
              TestPid = self(),

              % Register component twice
              ok = erlmcp_health_monitor:register_component(multi_comp, TestPid),
              Result = erlmcp_health_monitor:register_component(multi_comp, TestPid),

              % Should return ok (updates existing registration)
              ?assertEqual(ok, Result)
          end)
         ]
     end}.

%% Test system metrics structure
system_metrics_structure_test_() ->
    {setup,
     fun setup_health_monitor/0,
     fun cleanup_health_monitor/1,
     fun(_Pid) ->
         [
          ?_test(begin
              % Get system health
              SystemHealth = erlmcp_health_monitor:get_system_health(),

              % Verify system metrics structure
              SystemMetrics = maps:get(system_metrics, SystemHealth),
              ?assert(is_map(SystemMetrics)),
              ?assert(maps:is_key(memory_status, SystemMetrics)),
              ?assert(maps:is_key(process_count, SystemMetrics)),
              ?assert(maps:is_key(system_load, SystemMetrics)),
              ?assert(maps:is_key(last_check, SystemMetrics))
          end)
         ]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

%% Test complete health monitoring workflow
health_monitor_workflow_test_() ->
    {setup,
     fun setup_health_monitor/0,
     fun cleanup_health_monitor/1,
     fun(_Pid) ->
         [
          ?_test(begin
              TestPid = self(),

              % Register multiple components
              ok = erlmcp_health_monitor:register_component(server1, TestPid),
              ok = erlmcp_health_monitor:register_component(server2, TestPid),

              % Set configuration for one component
              Config = #{check_interval => 30000},
              ok = erlmcp_health_monitor:set_health_check_config(server1, Config),

              % Report degradation on one component
              ok = erlmcp_health_monitor:report_degradation(server1),

              % Report circuit breaker on another
              ok = erlmcp_health_monitor:report_circuit_breaker(server2, open),

              % Verify individual component health
              Server1Health = erlmcp_health_monitor:get_component_health(server1),
              ?assertEqual(degraded, Server1Health),

              Server2Health = erlmcp_health_monitor:get_component_health(server2),
              ?assertEqual(unhealthy, Server2Health),

              % Verify all component health
              AllHealth = erlmcp_health_monitor:get_all_component_health(),
              ?assertEqual(2, maps:size(AllHealth)),

              % Verify system health
              SystemHealth = erlmcp_health_monitor:get_system_health(),
              ?assert(is_map(SystemHealth)),
              ?assert(maps:is_key(overall_status, SystemHealth)),

              % Reset all health status
              ok = erlmcp_health_monitor:reset_health_status(),

              % Verify reset
              ResetServer1 = erlmcp_health_monitor:get_component_health(server1),
              ?assertEqual(unknown, ResetServer1),

              ResetServer2 = erlmcp_health_monitor:get_component_health(server2),
              ?assertEqual(unknown, ResetServer2)
          end)
         ]
     end}.
