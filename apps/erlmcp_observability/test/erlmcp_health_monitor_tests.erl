-module(erlmcp_health_monitor_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests for erlmcp_health_monitor module
%%====================================================================

%% Use fixture-based tests to ensure proper cleanup
start_stop_test_() ->
    {setup,
     fun() ->
         %% Setup: Start health monitor
         {ok, Pid} = erlmcp_health_monitor:start_link(),
         Pid
     end,
     fun(Pid) ->
         %% Teardown: Stop health monitor
         catch gen_server:stop(Pid)
     end,
     fun(Pid) ->
         [?_assert(is_pid(Pid))]
     end}.

register_component_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_health_monitor:start_link(),
         Pid
     end,
     fun(Pid) ->
         catch gen_server:stop(Pid)
     end,
     fun(Pid) ->
         Result = erlmcp_health_monitor:register_component(test_component, Pid),
         [?_assertEqual(ok, Result)]
     end}.

get_system_health_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_health_monitor:start_link(),
         Pid
     end,
     fun(Pid) ->
         catch gen_server:stop(Pid)
     end,
     fun(_Pid) ->
         Result = erlmcp_health_monitor:get_system_health(),
         [?_assert(is_map(Result)),
          ?_assert(maps:is_key(overall_status, Result)),
          ?_assert(maps:is_key(system_metrics, Result))]
     end}.

get_component_health_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_health_monitor:start_link(),
         Pid
     end,
     fun(Pid) ->
         catch gen_server:stop(Pid)
     end,
     fun(Pid) ->
         erlmcp_health_monitor:register_component(test_comp, Pid),
         Result = erlmcp_health_monitor:get_component_health(test_comp),
         %% Should return a health status atom (unknown, healthy, unhealthy, degraded)
         [?_assert(is_atom(Result))]
     end}.

get_component_health_not_found_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_health_monitor:start_link(),
         Pid
     end,
     fun(Pid) ->
         catch gen_server:stop(Pid)
     end,
     fun(_Pid) ->
         Result = erlmcp_health_monitor:get_component_health(nonexistent),
         [?_assertEqual(not_found, Result)]
     end}.

get_all_component_health_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_health_monitor:start_link(),
         Pid
     end,
     fun(Pid) ->
         catch gen_server:stop(Pid)
     end,
     fun(Pid) ->
         erlmcp_health_monitor:register_component(comp1, Pid),
         erlmcp_health_monitor:register_component(comp2, Pid),
         Result = erlmcp_health_monitor:get_all_component_health(),
         [?_assert(is_map(Result)),
          ?_assert(maps:size(Result) >= 2)]
     end}.

health_check_lifecycle_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_health_monitor:start_link(),
         Pid
     end,
     fun(Pid) ->
         catch gen_server:stop(Pid)
     end,
     fun(Pid) ->
         erlmcp_health_monitor:register_component(server1, Pid),
         erlmcp_health_monitor:register_component(server2, Pid),

         AllHealth = erlmcp_health_monitor:get_all_component_health(),
         SystemHealth = erlmcp_health_monitor:get_system_health(),

         [?_assert(maps:size(AllHealth) >= 2),
          ?_assert(is_map(SystemHealth)),
          ?_assert(maps:is_key(overall_status, SystemHealth))]
     end}.

unregister_component_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_health_monitor:start_link(),
         Pid
     end,
     fun(Pid) ->
         catch gen_server:stop(Pid)
     end,
     fun(Pid) ->
         erlmcp_health_monitor:register_component(temp_comp, Pid),
         ok = erlmcp_health_monitor:unregister_component(temp_comp),
         Result = erlmcp_health_monitor:get_component_health(temp_comp),
         [?_assertEqual(not_found, Result)]
     end}.

set_health_check_config_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_health_monitor:start_link(),
         Pid
     end,
     fun(Pid) ->
         catch gen_server:stop(Pid)
     end,
     fun(Pid) ->
         erlmcp_health_monitor:register_component(config_comp, Pid),
         Config = #{check_interval => 60000, timeout => 10000},
         Result = erlmcp_health_monitor:set_health_check_config(config_comp, Config),
         [?_assertEqual(ok, Result)]
     end}.

report_circuit_breaker_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_health_monitor:start_link(),
         Pid
     end,
     fun(Pid) ->
         catch gen_server:stop(Pid)
     end,
     fun(Pid) ->
         erlmcp_health_monitor:register_component(cb_comp, Pid),
         ok = erlmcp_health_monitor:report_circuit_breaker(cb_comp, open),
         AllHealth = erlmcp_health_monitor:get_all_component_health(),
         CbCompHealth = maps:get(cb_comp, AllHealth),
         [?_assertEqual(true, maps:get(circuit_breaker_active, CbCompHealth))]
     end}.

report_degradation_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_health_monitor:start_link(),
         Pid
     end,
     fun(Pid) ->
         catch gen_server:stop(Pid)
     end,
     fun(Pid) ->
         erlmcp_health_monitor:register_component(degraded_comp, Pid),
         ok = erlmcp_health_monitor:report_degradation(degraded_comp),
         Status = erlmcp_health_monitor:get_component_health(degraded_comp),
         [?_assertEqual(degraded, Status)]
     end}.

reset_health_status_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_health_monitor:start_link(),
         Pid
     end,
     fun(Pid) ->
         catch gen_server:stop(Pid)
     end,
     fun(Pid) ->
         erlmcp_health_monitor:register_component(reset_comp, Pid),
         ok = erlmcp_health_monitor:report_degradation(reset_comp),

         ok = erlmcp_health_monitor:reset_health_status(),
         ResetStatus = erlmcp_health_monitor:get_component_health(reset_comp),

         [?_assertEqual(unknown, ResetStatus)]
     end}.
