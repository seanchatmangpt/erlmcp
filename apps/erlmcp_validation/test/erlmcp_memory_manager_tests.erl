-module(erlmcp_memory_manager_tests).
-include_lib("eunit/include/eunit.hrl").

%% Basic cache test
cache_basic_test() ->
    Spec = #{version => <<"2025-11-25">>},
    ?assertMatch({ok, _}, erlmcp_memory_manager:cache_spec(Spec)).

%% Memory usage test
memory_usage_test() ->
    {ok, Stats} = erlmcp_memory_manager:get_memory_usage(),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(used_percent, Stats)).

%% Pressure check test  
pressure_check_test() ->
    Pressure = erlmcp_memory_manager:check_memory_pressure(),
    ?assert(lists:member(Pressure, [low, medium, high, critical])).
