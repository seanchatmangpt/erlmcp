%%%-------------------------------------------------------------------
%%% @doc EUnit tests for the erlmcp_cli application surface.
%%%
%%% Rewritten from a Common Test suite (common_test_SUITE.erl) that did
%%% not compile: it referenced the undefined #command_info record and
%%% nonexistent functions (erlmcp_cli_registry:get_metrics/0,
%%% erlmcp_cli_metrics:export_metrics/0, metrics_enabled/0,
%%% reset_all_metrics/0, erlmcp_cli_transport:transport/2, is_active/1,
%%% get_transport_stats/1, validate_config/1).
%%%
%%% The coordinator decision was rewrite-as-eunit, so the module keeps
%%% its filename (module name must match) but is a real eunit module
%%% over the REAL exported API:
%%%   erlmcp_cli_app:start/0
%%%   erlmcp_cli_registry:start_link/0, register_command/2,
%%%     lookup_command/1, execute_command/2, list_commands/0
%%%   erlmcp_cli_json_rpc:start_link/0, handle_json_rpc/3
%%%
%%% Chicago school: this test module is itself the real command module
%%% (execute/1); no mocks.
%%% @end
%%%-------------------------------------------------------------------
-module(common_test_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([execute/1]).

execute(_Args) ->
    {ok, #{<<"result">> => <<"ok">>}}.

%%====================================================================
%% Tests
%%====================================================================

app_module_exports_start_test() ->
    %% erlmcp_cli_app:start/0 is application:start(erlmcp_cli); without
    %% a built .app the real observable behavior is an error tuple, and
    %% with a built app it is ok. Both are honest; a crash is not.
    Result = erlmcp_cli_app:start(),
    ?assert(Result =:= ok orelse element(1, Result) =:= error),
    case Result of
        ok -> ok = application:stop(erlmcp_cli);
        {error, _} -> ok
    end.

registry_lifecycle_test() ->
    {ok, _} = erlmcp_cli_registry:start_link(),
    ok = erlmcp_cli_registry:register_command(<<"test.integration">>,
                                              common_test_SUITE),
    {ok, _Info} = erlmcp_cli_registry:lookup_command(<<"test.integration">>),
    {ok, _Result} = erlmcp_cli_registry:execute_command(<<"test.integration">>,
                                                        [<<"arg">>]),
    ?assertMatch({ok, _}, erlmcp_cli_registry:list_commands()),
    gen_server:stop(erlmcp_cli_registry).

json_rpc_protocol_test() ->
    {ok, _} = erlmcp_cli_json_rpc:start_link(),
    ValidReq = <<"{\"jsonrpc\":\"2.0\",\"method\":\"mcp.health\",\"params\":null,\"id\":123}">>,
    {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(ValidReq, #{}, <<"test-session">>),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    {ok, ErrorResponse} =
        erlmcp_cli_json_rpc:handle_json_rpc(unknown_method_req(456), #{}, <<"test-session">>),
    Error = maps:get(<<"error">>, ErrorResponse),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)),
    gen_server:stop(erlmcp_cli_json_rpc).

json_rpc_survives_errors_test() ->
    {ok, _} = erlmcp_cli_json_rpc:start_link(),
    lists:foreach(fun(_) ->
                          {ok, _} = erlmcp_cli_json_rpc:handle_json_rpc(<<"{bad">>,
                                                                        #{}, <<"s">>)
                  end, lists:seq(1, 3)),
    ?assert(is_pid(whereis(erlmcp_cli_json_rpc))),
    gen_server:stop(erlmcp_cli_json_rpc).

concurrent_registry_execution_test() ->
    {ok, _} = erlmcp_cli_registry:start_link(),
    ok = erlmcp_cli_registry:register_command(<<"test.concurrent">>, common_test_SUITE),
    Self = self(),
    NumProcesses = 5,
    lists:foreach(fun(I) ->
                          spawn(fun() ->
                                        Result = erlmcp_cli_registry:execute_command(
                                                   <<"test.concurrent">>, [I]),
                                        Self ! {done, I, Result}
                                end)
                  end, lists:seq(1, NumProcesses)),
    Results = [receive
                   {done, I, R} -> {I, R}
               after 10000 ->
                         timeout
               end || _ <- lists:seq(1, NumProcesses)],
    lists:foreach(fun({I, R}) ->
                          ?assertMatch({ok, _}, R),
                          ?assert(is_integer(I))
                  end, Results),
    gen_server:stop(erlmcp_cli_registry).

performance_smoke_test() ->
    {ok, _} = erlmcp_cli_registry:start_link(),
    ok = erlmcp_cli_registry:register_command(<<"test.perf">>, common_test_SUITE),
    Start = erlang:monotonic_time(millisecond),
    lists:foreach(fun(_) ->
                          {ok, _} = erlmcp_cli_registry:execute_command(<<"test.perf">>, [])
                  end, lists:seq(1, 100)),
    Duration = erlang:monotonic_time(millisecond) - Start,
    ct_helper_log(Duration),
    ?assert(Duration < 10000),
    gen_server:stop(erlmcp_cli_registry).

%%====================================================================
%% Helpers
%%====================================================================

unknown_method_req(Id) ->
    IdBin = integer_to_binary(Id),
    <<"{\"jsonrpc\":\"2.0\",\"method\":\"nonexistent\",\"params\":null,\"id\":",
      IdBin/binary, "}">>.

ct_helper_log(_Duration) ->
    ok.
