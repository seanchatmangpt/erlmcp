%%%-------------------------------------------------------------------
%%% @doc
%%% Simple unit tests for erlmcp_json_rpc module.
%%%
%%% This test suite focuses on JSON-RPC 2.0 protocol compliance
%%% without depending on other complex modules.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_json_rpc_simple_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Request Tests
%%====================================================================

request_test_() ->
    [
        ?_test(test_encode_request_integer()),
        ?_test(test_encode_request_binary()),
        ?_test(test_encode_request_null()),
        ?_test(test_encode_request_with_params()),
        ?_test(test_encode_request_no_params()),
        ?_test(test_decode_request_valid()),
        ?_test(test_decode_request_invalid())
    ].

test_encode_request_integer() ->
    Id = 123,
    Method = <<"test.method">>,
    Params = #{<<"key">> => <<"value">>},
    Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"method\":\"test.method\",\"params\":{\"key\":\"value\"}}">>,
    ?assertEqual(Expected, Encoded).

test_encode_request_binary() ->
    Id = <<"custom-id">>,
    Method = <<"resources/list">>,
    Params = [],
    Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":\"custom-id\",\"method\":\"resources/list\",\"params\":[]}">>,
    ?assertEqual(Expected, Encoded).

test_encode_request_null() ->
    Id = null,
    Method = <<"initialize">>,
    Params = undefined,
    Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"initialize\"}">>,
    ?assertEqual(Expected, Encoded).

test_encode_request_with_params() ->
    Id = 1,
    Method = <<"tools/call">>,
    Params = #{<<"name">> => <<"calculator">>, <<"arguments">> => #{<<"a">> => 1, <<"b">> => 2}},
    Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"calculator\",\"arguments\":{\"a\":1,\"b\":2}}}">>,
    ?assertEqual(Expected, Encoded).

test_encode_request_no_params() ->
    Id = 42,
    Method = <<"prompts/get">>,
    Params = undefined,
    Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":42,\"method\":\"prompts/get\"}">>,
    ?assertEqual(Expected, Encoded).

test_decode_request_valid() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"method\":\"test.method\",\"params\":{\"key\":\"value\"}}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_request{id = 123, method = <<"test.method">>, params = #{<<"key">> := <<"value">>}}, Decoded).

test_decode_request_invalid() ->
    Json = <<"{\"jsonrpc\":\"1.0\",\"id\":123,\"method\":\"test.method\"}">>,
    {error, {invalid_request, {wrong_version, <<"1.0">>}}} = erlmcp_json_rpc:decode_message(Json).

%%====================================================================
%% Response Tests
%%====================================================================

response_test_() ->
    [
        ?_test(test_encode_response_success()),
        ?_test(test_encode_error_response()),
        ?_test(test_decode_response_success()),
        ?_test(test_decode_error_response())
    ].

test_encode_response_success() ->
    Id = 123,
    Result = #{<<"status">> => success, <<"data">> => [1, 2, 3]},
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"result\":{\"status\":\"success\",\"data\":[1,2,3]}}">>,
    ?assertEqual(Expected, Encoded).

test_encode_error_response() ->
    Id = 456,
    Code = -32602,
    Message = <<"Invalid parameters">>,
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":456,\"error\":{\"code\":-32602,\"message\":\"Invalid parameters\"}}">>,
    ?assertEqual(Expected, Encoded).

test_decode_response_success() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"result\":{\"status\":\"success\"}}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_response{id = 123, result = #{<<"status">> := success}, error = undefined}, Decoded).

test_decode_error_response() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":456,\"error\":{\"code\":-32602,\"message\":\"Invalid parameters\"}}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_response{id = 456, result = undefined,
                                   error = #{<<"code">> := -32602, <<"message">> := <<"Invalid parameters">>}}, Decoded).

%%====================================================================
%% Notification Tests
%%====================================================================

notification_test_() ->
    [
        ?_test(test_encode_notification()),
        ?_test(test_decode_notification()),
        ?_test(test_decode_notification_invalid())
    ].

test_encode_notification() ->
    Method = <<"resources/updated">>,
    Params = #{<<"uri">> => <<"file.txt">>, <<"event">> => <<"created">>},
    Encoded = erlmcp_json_rpc:encode_notification(Method, Params),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"method\":\"resources/updated\",\"params\":{\"uri\":\"file.txt\",\"event\":\"created\"}}">>,
    ?assertEqual(Expected, Encoded).

test_decode_notification() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"resources/updated\",\"params\":{\"uri\":\"file.txt\",\"event\":\"created\"}}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_notification{method = <<"resources/updated">>,
                                      params = #{<<"uri">> := <<"file.txt">>, <<"event">> := <<"created">>}}, Decoded).

test_decode_notification_invalid() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"method\":\"test\"}">>,
    {error, {invalid_request, unknown_message_type}} = erlmcp_json_rpc:decode_message(Json).

%%====================================================================
%% Batch Tests
%%====================================================================

batch_test_() ->
    [
        ?_test(test_encode_batch()),
        ?_test(test_decode_batch()),
        ?_test(test_is_batch_request())
    ].

test_encode_batch() ->
    Messages = [
        erlmcp_json_rpc:encode_request(1, <<"resources/list">>, undefined),
        erlmcp_json_rpc:encode_response(2, #{<<"status">> => success}),
        erlmcp_json_rpc:encode_notification(<<"progress">>, #{<<"current">> => 50})
    ],
    Encoded = erlmcp_json_rpc:encode_batch(Messages),
    Expected = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"resources/list\"},{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"status\":\"success\"}},{\"jsonrpc\":\"2.0\",\"method\":\"progress\",\"params\":{\"current\":50}}]">>,
    ?assertEqual(Expected, Encoded).

test_decode_batch() ->
    Json = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"resources/list\"},{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":\"success\"}]">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_batch(Json),
    ?assertEqual(2, length(Decoded)),
    ?assertMatch([#json_rpc_request{id = 1, method = <<"resources/list">>, params = undefined},
                 #json_rpc_response{id = 2, result = <<"success">>, error = undefined}], Decoded).

test_is_batch_request() ->
    BatchJson = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}]">>,
    SingleJson = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}">>,
    ?assertEqual(true, erlmcp_json_rpc:is_batch_request(BatchJson)),
    ?assertEqual(false, erlmcp_json_rpc:is_batch_request(SingleJson)).

%%====================================================================
%% Error Code Validation Tests
%%====================================================================

error_validation_test_() ->
    [
        ?_test(test_valid_error_codes()),
        ?_test(test_invalid_error_codes()),
        ?_test(test_error_helpers())
    ].

test_valid_error_codes() ->
    ValidCodes = [
        -32700,  % Parse error
        -32600,  % Invalid Request
        -32601,  % Method not found
        -32602,  % Invalid params
        -32603,  % Internal error
        -32001,  % Resource not found
        -32002,  % Tool not found
        -32003,  % Prompt not found
        -32004,  % Capability not supported
        -32005,  % Not initialized
        -32006,  % Subscription failed
        -32007,  % Validation failed
        -32008,  % Transport error
        -32009,  % Timeout
        -32010,  % Rate limited
        -32011,  % Tool description too long
        -32012   % Message too large
    ],
    lists:foreach(fun(Code) ->
        ?assert(true =:= erlmcp_json_rpc:validate_error_code(Code))
    end, ValidCodes).

test_invalid_error_codes() ->
    InvalidCodes = [
        -32701,  % Below parse error
        -32604,  % Between invalid params and internal error
        -32100,  % Below server error range
        -31999,  % Above server error range
        0,       % Zero
        32700    % Above parse error
    ],
    lists:foreach(fun(Code) ->
        ?assert(false =:= erlmcp_json_rpc:validate_error_code(Code))
    end, InvalidCodes).

test_error_helpers() ->
    % Test error helper functions
    ErrorJson = erlmcp_json_rpc:error_method_not_found(123, <<"test.method">>),
    Decoded = jsx:decode(ErrorJson),
    ?assertEqual(123, maps:get(<<"id">>, Decoded)),
    ?assertEqual(-32601, maps:get(<<"error">>, Decoded, #{<<"code">> => 0})#{<<"code">>}),
    ?assertEqual(<<"Method not found">>, maps:get(<<"error">>, Decoded, #{<<"message">> => <<>>})#{<<"message">>}),
    ?assertEqual(<<"test.method">>, maps:get(<<"error">>, Decoded, #{<<"data">> => #{}})#{<<"data">> => #{}}#{<<"method">>}).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [
        ?_test(test_round_trip()),
        ?_test(test_create_error()),
        ?_test(test_create_error_with_data())
    ].

test_round_trip() ->
    Original = #json_rpc_request{
        id = 123,
        method = <<"tools/call">>,
        params = #{<<"name">> => <<"calculator">>, <<"arguments">> => #{<<"a">> => 1, <<"b">> => 2}}
    },
    Encoded = erlmcp_json_rpc:encode_request(Original#json_rpc_request.id, Original#json_rpc_request.method, Original#json_rpc_request.params),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Encoded),
    ?assertEqual(Original#json_rpc_request.id, Decoded#json_rpc_request.id),
    ?assertEqual(Original#json_rpc_request.method, Decoded#json_rpc_request.method),
    ?assertEqual(Original#json_rpc_request.params, Decoded#json_rpc_request.params).

test_create_error() ->
    Error = erlmcp_json_rpc:create_error(-32602, <<"Invalid params">>, #{<<"details">> => <<"Missing required field">>}),
    Expected = #mcp_error{
        code = -32602,
        message = <<"Invalid params">>,
        data = #{<<"details">> => <<"Missing required field">>}
    },
    ?assertEqual(Expected, Error).

test_create_error_with_data() ->
    Error = erlmcp_json_rpc:create_error_with_data(-32001, <<"Resource not found">>, uri, <<"file.txt">>),
    Expected = #mcp_error{
        code = -32001,
        message = <<"Resource not found">>,
        data = #{<<"uri">> => <<"file.txt">>}
    },
    ?assertEqual(Expected, Error).