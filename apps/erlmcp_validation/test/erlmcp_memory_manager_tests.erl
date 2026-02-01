-module(erlmcp_memory_manager_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Setup
%%%====================================================================

setup() ->
    case erlmcp_memory_manager:start_link() of
        {ok, Pid} ->
            Pid;
        {error, {already_started, Pid}} ->
            Pid
    end.

cleanup(_Pid) ->
    %% Don't stop the server between tests to avoid restart issues
    ok.

%%%====================================================================
%%% Test Cases
%%%====================================================================

%% Basic cache test
cache_basic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [?_test(begin
                    Spec = #{version => <<"2025-11-25">>},
                    ?assertMatch({ok, _}, erlmcp_memory_manager:cache_spec(Spec))
                end)]
     end}.

%% Memory usage test
memory_usage_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [?_test(begin
                    {ok, Stats} = erlmcp_memory_manager:get_memory_usage(),
                    ?assert(is_map(Stats)),
                    ?assert(maps:is_key(used_percent, Stats))
                end)]
     end}.

%% Pressure check test
pressure_check_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [?_test(begin
                    Pressure = erlmcp_memory_manager:check_memory_pressure(),
                    ?assert(lists:member(Pressure, [low, medium, high, critical]))
                end)]
     end}.
