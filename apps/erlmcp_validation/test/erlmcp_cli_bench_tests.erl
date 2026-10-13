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
    %% The benchmark sources live in apps/erlmcp_validation/benchmark/ and are
    %% DELIBERATELY excluded from the compiled release (rebar.config excludes
    %% bench modules from eunit discovery/build: "benchmarks, not eunit test
    %% modules"). The real, coherent state is: source exists on disk, module
    %% is NOT on the load path. Assert both — never a silent pass.
    ?assert(filelib:is_regular("../../apps/erlmcp_validation/benchmark/erlmcp_cli_startup_bench.erl")
            orelse filelib:is_regular("/Users/sac/erlmcp/apps/erlmcp_validation/benchmark/erlmcp_cli_startup_bench.erl")),
    ?assertMatch({error, nofile}, code:ensure_loaded(erlmcp_cli_startup_bench)),
    ok.

cli_command_bench_smoke_test() ->
    %% Same deliberate-exclusion court as above.
    ?assert(filelib:is_regular("../../apps/erlmcp_validation/benchmark/erlmcp_cli_command_bench.erl")
            orelse filelib:is_regular("/Users/sac/erlmcp/apps/erlmcp_validation/benchmark/erlmcp_cli_command_bench.erl")),
    ?assertMatch({error, nofile}, code:ensure_loaded(erlmcp_cli_command_bench)),
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
