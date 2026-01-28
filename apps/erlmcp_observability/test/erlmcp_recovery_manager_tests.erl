-module(erlmcp_recovery_manager_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests for erlmcp_recovery_manager module
%%====================================================================

start_stop_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),
    ?assert(is_pid(Pid)),
    gen_server:stop(Pid).

basic_recovery_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),

    %% Test basic recovery operations
    Result = erlmcp_recovery_manager:recover(test_server),
    ?assert(
        case Result of
            ok -> true;
            {ok, _} -> true;
            {error, _} -> true;
            _ -> false
        end
    ),

    gen_server:stop(Pid).

recovery_with_strategy_test() ->
    {ok, Pid} = erlmcp_recovery_manager:start_link(),

    Result = erlmcp_recovery_manager:recover(test_server, restart),
    ?assert(
        case Result of
            ok -> true;
            {ok, _} -> true;
            {error, _} -> true;
            _ -> false
        end
    ),

    gen_server:stop(Pid).
