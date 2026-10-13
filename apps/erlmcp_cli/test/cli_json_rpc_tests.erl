%%%-------------------------------------------------------------------
%%% @doc EUnit tests for erlmcp_cli_json_rpc (real exported API).
%%%
%%% Rewritten from pseudo-code that used ?fail/1 (undefined macro), a
%%% `#` python comment, and invented functions parse_json_rpc/2,
%%% make_success_response/1, make_error_response/1, make_request_id/0,
%%% stop/0 — none exported by the real module.
%%%
%%% Real API under test (apps/erlmcp_cli/src/erlmcp_cli_json_rpc.erl):
%%%   start_link/0, handle_json_rpc/3
%%%
%%% JSON-RPC 2.0 contract asserted: error responses come back as
%%% {ok, ErrorMap} with code -32700 (parse), -32600 (invalid request),
%%% -32601 (method not found), and "id" => null on errors.
%%% @end
%%%-------------------------------------------------------------------
-module(cli_json_rpc_tests).

-include_lib("eunit/include/eunit.hrl").

start_stop_lifecycle_test() ->
    ?assertMatch({ok, _}, erlmcp_cli_json_rpc:start_link()),
    ok = gen_server:stop(erlmcp_cli_json_rpc).

method_not_found_test() ->
    {ok, _} = erlmcp_cli_json_rpc:start_link(),
    Request = <<"{\"jsonrpc\":\"2.0\",\"method\":\"no.such.method\","
                 "\"params\":null,\"id\":1}">>,
    {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(Request, #{}, <<"s">>),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(-32601, error_code(Response)),
    ?assertEqual(null, maps:get(<<"id">>, Response)),
    gen_server:stop(erlmcp_cli_json_rpc).

parse_error_test() ->
    {ok, _} = erlmcp_cli_json_rpc:start_link(),
    {ok, Response} =
        erlmcp_cli_json_rpc:handle_json_rpc(<<"{not valid json">>, #{}, <<"s">>),
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(-32700, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Parse error">>, maps:get(<<"message">>, Error)),
    gen_server:stop(erlmcp_cli_json_rpc).

invalid_request_missing_jsonrpc_test() ->
    {ok, _} = erlmcp_cli_json_rpc:start_link(),
    {ok, Response} =
        erlmcp_cli_json_rpc:handle_json_rpc(<<"{\"method\":\"m\",\"id\":1}">>, #{}, <<"s">>),
    ?assertEqual(-32600, error_code(Response)),
    gen_server:stop(erlmcp_cli_json_rpc).

invalid_request_bad_version_test() ->
    {ok, _} = erlmcp_cli_json_rpc:start_link(),
    {ok, Response} =
        erlmcp_cli_json_rpc:handle_json_rpc(<<"{\"jsonrpc\":\"1.0\",\"method\":\"m\",\"id\":1}">>,
                                            #{}, <<"s">>),
    ?assertEqual(-32600, error_code(Response)),
    gen_server:stop(erlmcp_cli_json_rpc).

invalid_request_missing_method_test() ->
    {ok, _} = erlmcp_cli_json_rpc:start_link(),
    {ok, Response} =
        erlmcp_cli_json_rpc:handle_json_rpc(<<"{\"jsonrpc\":\"2.0\",\"id\":1}">>, #{}, <<"s">>),
    ?assertEqual(-32600, error_code(Response)),
    gen_server:stop(erlmcp_cli_json_rpc).

error_id_is_null_test() ->
    {ok, _} = erlmcp_cli_json_rpc:start_link(),
    {ok, Response} =
        erlmcp_cli_json_rpc:handle_json_rpc(<<"{broken">>, #{}, <<"s">>),
    ?assertEqual(null, maps:get(<<"id">>, Response)),
    gen_server:stop(erlmcp_cli_json_rpc).

server_stays_alive_after_errors_test() ->
    {ok, _} = erlmcp_cli_json_rpc:start_link(),
    lists:foreach(fun(_) ->
                          {ok, _} = erlmcp_cli_json_rpc:handle_json_rpc(<<"garbage">>, #{}, <<"s">>)
                  end, lists:seq(1, 5)),
    Pid = whereis(erlmcp_cli_json_rpc),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(erlmcp_cli_json_rpc).

%%====================================================================
%% Helpers
%%====================================================================

error_code(Response) when is_map(Response) ->
    maps:get(<<"code">>, maps:get(<<"error">>, Response)).
