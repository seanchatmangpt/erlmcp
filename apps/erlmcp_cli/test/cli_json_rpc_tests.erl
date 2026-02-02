%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for erlmcp_cli_json_rpc module
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cli_json_rpc_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test JSON-RPC server startup
json_rpc_server_test() ->
    %% Start the JSON-RPC server
    {ok, Pid} = erlmcp_cli_json_rpc:start_link(),

    %% Verify server is running
    ?assert(is_process_alive(Pid)),

    %% Cleanup
    erlmcp_cli_json_rpc:stop(),
    timer:sleep(50),
    ok.

%% @doc Test valid JSON-RPC request parsing
valid_json_rpc_test() ->
    %% Create a valid JSON-RPC request
    RequestJson = <<"{\"jsonrpc\":\"2.0\",\"method\":\"mcp.health\",\"params\":null,\"id\":1}">>,

    %% Parse the request
    {ok, Request} = erlmcp_cli_json_rpc:parse_json_rpc(RequestJson, undefined),

    %% Verify parsed request
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Request)),
    ?assertEqual(<<"mcp.health">>, maps:get(<<"method">>, Request)),
    ?assertEqual(null, maps:get(<<"params">>, Request)),
    ?assertEqual(1, maps:get(<<"id">>, Request)).

%% @doc Test invalid JSON-RPC request - missing jsonrpc field
invalid_jsonrpc_test() ->
    InvalidRequestJson = <<"{\"method\":\"mcp.health\",\"params\":null,\"id\":1}">>,

    %% Should return error
    {error, Reason} = erlmcp_cli_json_rpc:parse_json_rpc(InvalidRequestJson, undefined),
    ?assert(is_tuple(Reason)),
    ?assertEqual({invalid_request, missing_jsonrpc_version}, Reason).

%% @doc Test invalid JSON-RPC request - invalid jsonrpc version
invalid_jsonrpc_version_test() ->
    InvalidRequestJson = <<"{\"jsonrpc\":\"1.0\",\"method\":\"mcp.health\",\"params\":null,\"id\":1}">>,

    %% Should return error
    {error, Reason} = erlmcp_cli_json_rpc:parse_json_rpc(InvalidRequestJson, undefined),
    ?assert(is_tuple(Reason)),
    ?assertEqual({invalid_request, invalid_jsonrpc_version}, Reason).

%% @doc Test invalid JSON-RPC request - missing method field
missing_method_test() ->
    InvalidRequestJson = <<"{\"jsonrpc\":\"2.0\",\"params\":null,\"id\":1}">>,

    %% Should return error
    {error, Reason} = erlmcp_cli_json_rpc:parse_json_rpc(InvalidRequestJson, undefined),
    ?assert(is_tuple(Reason)),
    ?assertEqual({invalid_request, missing_method}, Reason).

%% @doc Test invalid JSON-RPC request - invalid method type
invalid_method_type_test() ->
    InvalidRequestJson = <<"{\"jsonrpc\":\"2.0\",\"method\":123,\"params\":null,\"id\":1}">>,

    %% Should return error
    {error, Reason} = erlmcp_cli_json_rpc:parse_json_rpc(InvalidRequestJson, undefined),
    ?assert(is_tuple(Reason)),
    ?assertEqual({invalid_request, invalid_method}, Reason).

%% @doc Test JSON-RPC response generation
response_generation_test() ->
    %% Test success response
    Result = #{<<"status">> => <<"ok">>, <<"timestamp">> => 1234567890},
    SuccessResponse = erlmcp_cli_json_rpc:make_success_response(<<"test-id">>, Result),

    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, SuccessResponse)),
    ?assertEqual(Result, maps:get(<<"result">>, SuccessResponse)),
    ?assertEqual(<<"test-id">>, maps:get(<<"id">>, SuccessResponse)),

    %% Test error response
    ErrorResponse = erlmcp_cli_json_rpc:make_error_response({method_not_found, <<"test.method">>}),

    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, ErrorResponse)),
    ?assert(is_map(maps:get(<<"error">>, ErrorResponse))),
    ?assertEqual(-32601, maps:get(<<"code">>, maps:get(<<"error">>, ErrorResponse))),
    ?assertEqual(<<"Method not found">>, maps:get(<<"message">>, maps:get(<<"error">>, ErrorResponse))),
    ?assertEqual(null, maps:get(<<"id">>, ErrorResponse)).

%% @doc Test request ID generation
request_id_test() ->
    %% Generate multiple request IDs
    Id1 = erlmcp_cli_json_rpc:make_request_id(),
    Id2 = erlmcp_cli_json_rpc:make_request_id(),

    %% Verify they are binary
    ?assert(is_binary(Id1)),
    ?assert(is_binary(Id2)),

    %% Verify they are unique
    ?assertNotEqual(Id1, Id2),

    %% Verify they are base64 encoded
    try
        _Decoded = base64:decode(Id1),
        ok
    catch
        error:badarg ->
            ?fail("Request ID is not valid base64")
    end.

%% @doc Test JSON-RPC error response format
error_response_format_test() ->
    %% Test different error types
    ErrorTypes = [
        {parse_error, "Parse failed"},
        {invalid_request, "Invalid request"},
        {method_not_found, <<"test.method">>},
        {invalid_params, {invalid_type, integer}},
        {internal_error, "Internal server error"}
    ],

    lists:foreach(fun(ErrorType) ->
        Response = erlmcp_cli_json_rpc:make_error_response(ErrorType),
        ErrorMap = maps:get(<<"error">>, Response),

        ?assert(is_map(ErrorMap)),
        ?assert(is_integer(maps:get(<<"code">>, ErrorMap))),
        ?assert(is_binary(maps:get(<<"message">>, ErrorMap))),

        %% Verify error codes are valid JSON-RPC error codes
        Code = maps:get(<<"code">>, ErrorMap),
        ?assert(Code =< -32600 orelse Code =:= -32603)
    end, ErrorTypes).

%% @doc Test JSON-RPC method execution
method_execution_test() ->
    %% This test requires the command registry to be running
    %% We'll test the path through the system

    %% Start required processes
    {ok, RegistryPid} = erlmcp_cli_registry:start_link(),
    {ok, JsonRpcPid} = erlmcp_cli_json_rpc:start_link(),

    %% Wait for initialization
    timer:sleep(100),

    %% Test handling JSON-RPC request
    RequestJson = <<"{\"jsonrpc\":\"2.0\",\"method\":\"mcp.health\",\"params\":null,\"id\":\"test-123\"}">>,
    Headers = #{},
    SessionId = <<"test-session">>,

    %% Handle the request
    Response = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, Headers, SessionId),

    %% Should succeed
    ?assertMatch({ok, _}, Response),

    %% Verify response structure
    {ok, ResponseMap} = Response,
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, ResponseMap)),
    ?assert(is_binary(maps:get(<<"id">>, ResponseMap))),
    ?assert(is_map(maps:get(<<"result">>, ResponseMap))),

    %% Cleanup
    erlmcp_cli_json_rpc:stop(),
    erlmcp_cli_registry:stop(),
    timer:sleep(50),
    ok.

%% @doc Test malformed JSON handling
malformed_json_test() ->
    %% Test malformed JSON
    MalformedJson = <<"{\"jsonrpc\":\"2.0\",\"method\":\"mcp.health\",\"params\":null,\"id\":1">>,  # Missing closing brace

    %% Should return parse error
    {error, Reason} = erlmcp_cli_json_rpc:parse_json_rpc(MalformedJson, undefined),
    ?assert(is_tuple(Reason)),
    ?assertEqual({parse_failed, _}, Reason).

%% @doc Test concurrent JSON-RPC requests
concurrent_requests_test() ->
    %% Start required processes
    {ok, RegistryPid} = erlmcp_cli_registry:start_link(),
    {ok, JsonRpcPid} = erlmcp_cli_json_rpc:start_link(),

    %% Wait for initialization
    timer:sleep(100),

    %% Create multiple concurrent requests
    RequestCount = 10,
    Requests = lists:map(fun(I) ->
        Json = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                           <<"method">> => <<"mcp.health">>,
                           <<"params">> => null,
                           <<"id">> => integer_to_binary(I)}),
        erlmcp_cli_json_rpc:handle_json_rpc(Json, #{}, <<"test-session">>)
    end, lists:seq(1, RequestCount)),

    %% Wait for all requests to complete
    Results = lists:map(fun(Result) ->
        receive
            {ok, _} -> ok
        after 5000 ->
            timeout
        end
    end, Requests),

    %% Verify all requests completed successfully
    ?assertEqual(RequestCount, length([R || R <- Results, R =:= ok])),

    %% Cleanup
    erlmcp_cli_json_rpc:stop(),
    erlmcp_cli_registry:stop(),
    timer:sleep(50),
    ok.

%% @doc Test error handling for invalid session IDs
invalid_session_test() ->
    %% Test with invalid session ID
    RequestJson = <<"{\"jsonrpc\":\"2.0\",\"method\":\"mcp.health\",\"params\":null,\"id\":1}">>,
    InvalidSessionId = <<>>,

    %% Should handle gracefully
    Response = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, InvalidSessionId),
    ?assertMatch({ok, _}, Response),

    %% The request should still be processed despite invalid session ID
    {ok, ResponseMap} = Response,
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, ResponseMap)),
    ?assert(is_binary(maps:get(<<"id">>, ResponseMap))).