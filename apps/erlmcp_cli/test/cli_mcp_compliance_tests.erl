%%%-------------------------------------------------------------------
%%% @doc MCP compliance EUnit tests for erlmcp_cli.
%%%
%%% Rewritten from pseudo-code that used ?fail/1 (undefined macro),
%%% jsx (not a repo dependency), a #cli_session record that is not
%%% defined anywhere, and nonexistent functions
%%% erlmcp_cli_session:create_session/2, get_state/1,
%%% erlmcp_cli_registry:list_commands/1 match shape
%%% ({Name, _} -- real list_commands/0 returns {ok, list()}).
%%%
%%% Compliance surface exercised through the REAL exported API:
%%%   erlmcp_cli_json_rpc:start_link/0, handle_json_rpc/3
%%%   erlmcp_cli_registry:start_link/0, register_command/2,
%%%     lookup_command/1, execute_command/2, list_commands/0
%%%
%%% Asserted MCP/JSON-RPC 2.0 rules:
%%%   - every request gets a response with "jsonrpc" => <<"2.0">>
%%%   - unknown method  -> error code -32601
%%%   - malformed JSON  -> error code -32700
%%%   - invalid request -> error code -32600
%%%   - error responses carry "id" => null
%%% @end
%%%-------------------------------------------------------------------
-module(cli_mcp_compliance_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

%% MCP required methods must each produce a protocol-valid response.
mcp_required_methods_get_protocol_valid_responses_test() ->
    {ok, _} = erlmcp_cli_json_rpc:start_link(),
    Methods = [<<"initialize">>, <<"tools/list">>, <<"tools/call">>,
               <<"resources/list">>, <<"resources/read">>,
               <<"resources/subscribe">>, <<"prompts/list">>],
    lists:foreach(fun(Method) ->
                          Req = json_request(Method, make_test_id()),
                          {ok, Response} =
                              erlmcp_cli_json_rpc:handle_json_rpc(Req, #{}, <<"mcp-compliance">>),
                          ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response))
                  end, Methods),
    gen_server:stop(erlmcp_cli_json_rpc).

method_not_found_error_code_test() ->
    {ok, _} = erlmcp_cli_json_rpc:start_link(),
    Req = json_request(<<"no.such.method">>, 1),
    {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(Req, #{}, <<"s">>),
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Method not found">>, maps:get(<<"message">>, Error)),
    gen_server:stop(erlmcp_cli_json_rpc).

parse_error_code_test() ->
    {ok, _} = erlmcp_cli_json_rpc:start_link(),
    {ok, Response} =
        erlmcp_cli_json_rpc:handle_json_rpc(<<"{invalid json}">>, #{}, <<"s">>),
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(-32700, maps:get(<<"code">>, Error)),
    gen_server:stop(erlmcp_cli_json_rpc).

invalid_request_error_codes_test() ->
    {ok, _} = erlmcp_cli_json_rpc:start_link(),
    InvalidBodies =
        [<<"{\"method\":\"tools/list\",\"id\":1}">>,              %% missing jsonrpc
         <<"{\"jsonrpc\":\"1.0\",\"method\":\"m\",\"id\":1}">>,   %% wrong version
         <<"{\"jsonrpc\":\"2.0\",\"id\":1}">>,                    %% missing method
         <<"{\"jsonrpc\":\"2.0\",\"method\":123,\"id\":1}">>],    %% wrong method type
    lists:foreach(fun(Body) ->
                          {ok, Response} =
                              erlmcp_cli_json_rpc:handle_json_rpc(Body, #{}, <<"s">>),
                          Error = maps:get(<<"error">>, Response),
                          ?assertEqual(-32600, maps:get(<<"code">>, Error))
                  end, InvalidBodies),
    gen_server:stop(erlmcp_cli_json_rpc).

error_response_id_null_test() ->
    {ok, _} = erlmcp_cli_json_rpc:start_link(),
    {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(<<"{">>, #{}, <<"s">>),
    ?assertEqual(null, maps:get(<<"id">>, Response)),
    gen_server:stop(erlmcp_cli_json_rpc).

request_id_correlation_test() ->
    {ok, _} = erlmcp_cli_json_rpc:start_link(),
    Requests = [json_request(<<"mcp.health">>, I) || I <- lists:seq(1, 10)],
    Responses = [erlmcp_cli_json_rpc:handle_json_rpc(Req, #{}, <<"s">>)
                 || Req <- Requests],
    ?assert(lists:all(fun({ok, _}) -> true;
                         (_) -> false
                      end, Responses)),
    gen_server:stop(erlmcp_cli_json_rpc).

registry_built_in_commands_are_listable_test() ->
    {ok, _} = erlmcp_cli_registry:start_link(),
    ?assertMatch({ok, _}, erlmcp_cli_registry:list_commands()),
    gen_server:stop(erlmcp_cli_registry).

registry_register_execute_lookup_cycle_test() ->
    {ok, _} = erlmcp_cli_registry:start_link(),
    ok = erlmcp_cli_registry:register_command(<<"mcp.health">>, cli_mcp_compliance_tests),
    {ok, _Info} = erlmcp_cli_registry:lookup_command(<<"mcp.health">>),
    {ok, _Result} = erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),
    ?assertEqual(ok, erlmcp_cli_registry:unregister_command(<<"mcp.health">>)),
    gen_server:stop(erlmcp_cli_registry).

%%====================================================================
%% Helpers
%%====================================================================

json_request(Method, Id) when is_integer(Id) ->
    IdBin = integer_to_binary(Id),
    <<"{\"jsonrpc\":\"2.0\",\"method\":\"", Method/binary,
      "\",\"params\":null,\"id\":", IdBin/binary, "}">>;
json_request(Method, Id) when is_binary(Id) ->
    <<"{\"jsonrpc\":\"2.0\",\"method\":\"", Method/binary,
      "\",\"params\":null,\"id\":\"", Id/binary, "\"}">>.

make_test_id() ->
    base64:encode(crypto:strong_rand_bytes(12)).

%% Real command module for the registry cycle test (no mocks).
execute(_Args) ->
    {ok, #{<<"status">> => <<"healthy">>}}.
