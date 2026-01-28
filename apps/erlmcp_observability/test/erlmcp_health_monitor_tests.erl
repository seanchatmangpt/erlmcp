-module(erlmcp_health_monitor_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests for erlmcp_health_monitor module
%%====================================================================

start_stop_test() ->
    {ok, Pid} = erlmcp_health_monitor:start_link(),
    ?assert(is_pid(Pid)),
    gen_server:stop(Pid).

register_component_test() ->
    {ok, Pid} = erlmcp_health_monitor:start_link(),
    Result = erlmcp_health_monitor:register_component(test_component, test_pid),
    ?assertEqual(ok, Result),
    gen_server:stop(Pid).

get_system_health_test() ->
    {ok, Pid} = erlmcp_health_monitor:start_link(),
    Result = erlmcp_health_monitor:get_system_health(),
    ?assert(is_map(Result)),
    gen_server:stop(Pid).

get_component_health_test() ->
    {ok, Pid} = erlmcp_health_monitor:start_link(),
    erlmcp_health_monitor:register_component(test_comp, self()),
    Result = erlmcp_health_monitor:get_component_health(test_comp),
    ?assert(is_map(Result) orelse Result =:= {error, not_found}),
    gen_server:stop(Pid).

get_all_component_health_test() ->
    {ok, Pid} = erlmcp_health_monitor:start_link(),
    erlmcp_health_monitor:register_component(comp1, self()),
    erlmcp_health_monitor:register_component(comp2, self()),
    Result = erlmcp_health_monitor:get_all_component_health(),
    ?assert(is_list(Result)),
    gen_server:stop(Pid).

health_check_lifecycle_test() ->
    {ok, Pid} = erlmcp_health_monitor:start_link(),

    erlmcp_health_monitor:register_component(server1, self()),
    erlmcp_health_monitor:register_component(server2, self()),

    AllHealth = erlmcp_health_monitor:get_all_component_health(),
    ?assertEqual(2, length(AllHealth)),

    SystemHealth = erlmcp_health_monitor:get_system_health(),
    ?assert(is_map(SystemHealth)),

    gen_server:stop(Pid).
