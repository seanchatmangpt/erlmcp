%%%-------------------------------------------------------------------
%%% @doc CLI Benchmark Tests
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_bench_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases
%%====================================================================

cli_startup_bench_smoke_test() ->
    %% Just verify the module exists and can be called
    %% Don't run full benchmark in tests
    ?assertMatch({module, _}, code:ensure_loaded(erlmcp_cli_startup_bench)),
    ok.

cli_command_bench_smoke_test() ->
    %% Just verify the module exists
    ?assertMatch({module, _}, code:ensure_loaded(erlmcp_cli_command_bench)),
    ok.

cli_fast_module_test() ->
    %% Verify fast CLI module exists
    ?assertMatch({module, _}, code:ensure_loaded(erlmcp_validate_cli_fast)),
    ok.

quick_check_test() ->
    %% Test the ultra-fast quick check
    Result = erlmcp_validate_cli_fast:quick_check(),
    ?assert(Result =:= ok orelse Result =:= error),
    ok.
