%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive unit tests for erlmcp_json_rpc module.
%%%
%%% Tests JSON-RPC 2.0 protocol compliance including:
%%% - Request encoding/decoding
%%% - Response encoding/decoding
%%% - Notification encoding/decoding
%%% - Batch request handling
%%% - Error code validation
%%% - Message size limits
%%% - Protocol version negotiation
%%% - Message flow validation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_json_rpc_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Data Generators
%%====================================================================

%% Valid JSON-RPC ID values
-define(TEST_IDS, [
    {integer_id, 1},
    {integer_id, 42},
    {integer_id, 0},
    {binary_id, <<"test-id-123">>},
    {binary_id, <<"uuid-12345678-1234-5678-1234-567812345678">>},
    {null_id, null}
]).

%% Valid parameter values
-define(TEST_PARAMS, [
    {object_params, #{<<"arg1">> => <<"value1">>, <<"arg2">> => 42}},
    {array_params, [<<"value1">>, 42, true]},
    {empty_params, #{}},
    {empty_array_params, []},
    {no_params, undefined}
]).

%% Method names
-define(TEST_METHODS, [
    <<"resources/list">>,
    <<"tools/call">>,
    <<"resources/read">>,
    <<"prompts/get">>,
    <<"initialize">>,
    <<"custom.method/name">>
]).

%%====================================================================
%% Request Encoding/Decoding Tests
%%====================================================================

%% Test basic request encoding with different ID types
encode_request_basic_test_() ->
    [
        ?_test(encode_request_integer_id()),
        ?_test(encode_request_binary_id()),
        ?_test(encode_request_null_id()),
        ?_test(encode_request_with_params()),
        ?_test(encode_request_without_params())
    ].

encode_request_integer_id() ->
    Id = 123,
    Method = <<"test.method">>,
    Params = #{<<"key">> => <<"value">>},
    Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"method\":\"test.method\",\"params\":{\"key\":\"value\"}}">>,
    ?assertEqual(Expected, Encoded).

encode_request_binary_id() ->
    Id = <<"custom-id">>,
    Method = <<"resources/list">>,
    Params = [],
    Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":\"custom-id\",\"method\":\"resources/list\",\"params\":[]}">>,
    ?assertEqual(Expected, Encoded).

encode_request_null_id() ->
    Id = null,
    Method = <<"initialize">>,
    Params = undefined,
    Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"initialize\"}">>,
    ?assertEqual(Expected, Encoded).

encode_request_with_params() ->
    Id = 1,
    Method = <<"tools/call">>,
    Params = #{<<"name">> => <<"calculator">>, <<"arguments">> => #{<<"a">> => 1, <<"b">> => 2}},
    Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"calculator\",\"arguments\":{\"a\":1,\"b\":2}}}">>,
    ?assertEqual(Expected, Encoded).

encode_request_without_params() ->
    Id = 42,
    Method = <<"prompts/get">>,
    Params = undefined,
    Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":42,\"method\":\"prompts/get\"}">>,
    ?assertEqual(Expected, Encoded).

%% Test request decoding
decode_request_valid_test_() ->
    [
        ?_test(decode_request_integer_id()),
        ?_test(decode_request_binary_id()),
        ?_test(decode_request_null_id()),
        ?_test(decode_request_with_object_params()),
        ?_test(decode_request_with_array_params()),
        ?_test(decode_request_no_params())
    ].

decode_request_integer_id() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"method\":\"test.method\",\"params\":{\"key\":\"value\"}}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_request{id = 123, method = <<"test.method">>, params = #{<<"key">> := <<"value">>}}, Decoded).

decode_request_binary_id() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":\"custom-id\",\"method\":\"resources/list\",\"params\":[]}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_request{id = <<"custom-id">>, method = <<"resources/list">>, params = []}, Decoded).

decode_request_null_id() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":null,\"method\":\"initialize\"}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_request{id = null, method = <<"initialize">>, params = undefined}, Decoded).

decode_request_with_object_params() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"calculator\",\"arguments\":{\"a\":1,\"b\":2}}}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_request{id = 1, method = <<"tools/call">>,
                                  params = #{<<"name">> := <<"calculator">>,
                                           <<"arguments">> := #{<<"a">> := 1, <<"b">> := 2}}}, Decoded).

decode_request_with_array_params() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":42,\"method\":\"resources/read\",\"params\":[\"resource1\",123,true]}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_request{id = 42, method = <<"resources/read">>, params = [<<"resource1">>, 123, true]}, Decoded).

decode_request_no_params() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"prompts/get\"}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_request{id = 1, method = <<"prompts/get">>, params = undefined}, Decoded).

%% Test request decoding errors
decode_request_error_test_() ->
    [
        ?_test(decode_request_missing_jsonrpc()),
        ?_test(decode_request_wrong_version()),
        ?_test(decode_request_missing_id()),
        ?_test(decode_request_missing_method()),
        ?_test(decode_request_invalid_method()),
        ?_test(decode_request_invalid_json()),
        ?_test(decode_request_invalid_params_type())
    ].

decode_request_missing_jsonrpc() ->
    Json = <<"{\"id\":123,\"method\":\"test.method\"}">>,
    {error, {invalid_request, missing_jsonrpc}} = erlmcp_json_rpc:decode_message(Json).

decode_request_wrong_version() ->
    Json = <<"{\"jsonrpc\":\"1.0\",\"id\":123,\"method\":\"test.method\"}">>,
    {error, {invalid_request, {wrong_version, <<"1.0">>}}} = erlmcp_json_rpc:decode_message(Json).

decode_request_missing_id() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test.method\"}">>,
    {error, {invalid_request, unknown_message_type}} = erlmcp_json_rpc:decode_message(Json).

decode_request_missing_method() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":123}">>,
    {error, {invalid_request, unknown_message_type}} = erlmcp_json_rpc:decode_message(Json).

decode_request_invalid_method() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"method\":123}">>,
    {error, {invalid_request, {invalid_method, 123}}} = erlmcp_json_rpc:decode_message(Json).

decode_request_invalid_json() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"method\":\"test.method\",}">>,
    {error, {parse_error, invalid_json}} = erlmcp_json_rpc:decode_message(Json).

decode_request_invalid_params_type() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"method\":\"test.method\",\"params\":\"invalid\"}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_request{id = 123, method = <<"test.method">>, params = undefined}, Decoded).

%%====================================================================
%% Response Encoding/Decoding Tests
%%====================================================================

%% Test response encoding
encode_response_success_test_() ->
    [
        ?_test(encode_response_integer_id()),
        ?_test(encode_response_binary_id()),
        ?_test(encode_response_null_id()),
        ?_test(encode_response_simple_result()),
        ?_test(encode_response_complex_result()),
        ?_test(encode_response_empty_result())
    ].

encode_response_integer_id() ->
    Id = 123,
    Result = #{<<"status">> => success, <<"data">> => [1, 2, 3]},
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"result\":{\"status\":\"success\",\"data\":[1,2,3]}}">>,
    ?assertEqual(Expected, Encoded).

encode_response_binary_id() ->
    Id = <<"req-123">>,
    Result = <<"operation completed">>,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":\"req-123\",\"result\":\"operation completed\"}">>,
    ?assertEqual(Expected, Encoded).

encode_response_null_id() ->
    Id = null,
    Result = null,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":null,\"result\":null}">>,
    ?assertEqual(Expected, Encoded).

encode_response_simple_result() ->
    Id = 1,
    Result = true,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":true}">>,
    ?assertEqual(Expected, Encoded).

encode_response_complex_result() ->
    Id = 42,
    Result = #{
        <<"resources">> => [#{<<"uri">> => <<"res1">>}, #{<<"uri">> => <<"res2">>}],
        <<"metadata">> => #{<<"count\">> => 2}
    },
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":42,\"result\":{\"resources\":[{\"uri\":\"res1\"},{\"uri\":\"res2\"}],\"metadata\":{\"count\":2}}}">>,
    ?assertEqual(Expected, Encoded).

encode_response_empty_result() ->
    Id = 99,
    Result = undefined,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":99,\"result\":null}">>,
    ?assertEqual(Expected, Encoded).

%% Test response decoding
decode_response_success_test_() ->
    [
        ?_test(decode_response_integer_id()),
        ?_test(decode_response_binary_id()),
        ?_test(decode_response_null_id()),
        ?_test(decode_response_simple_result()),
        ?_test(decode_response_complex_result())
    ].

decode_response_integer_id() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"result\":{\"status\":\"success\"}}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_response{id = 123, result = #{<<"status">> := success}, error = undefined}, Decoded).

decode_response_binary_id() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":\"req-123\",\"result\":\"data\"}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_response{id = <<"req-123">>, result = <<"data">>, error = undefined}, Decoded).

decode_response_null_id() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":null,\"result\":null}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_response{id = null, result = null, error = undefined}, Decoded).

decode_response_simple_result() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":42,\"result\":true}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_response{id = 42, result = true, error = undefined}, Decoded).

decode_response_complex_result() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[{\"name\":\"calc\"}]}}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_response{id = 1, result = #{<<"tools">> := [#{<<"name">> := <<"calc">>}]}, error = undefined}, Decoded).

%% Test error response encoding
encode_error_response_test_() ->
    [
        ?_test(encode_error_response_basic()),
        ?_test(encode_error_response_with_data()),
        ?_test(encode_error_response_with_binary_data()),
        ?_test(encode_error_response_with_map_data()),
        ?_test(encode_error_response_invalid_code_fallback()),
        ?_test(error_method_not_found()),
        ?_test(error_invalid_params()),
        ?_test(error_resource_not_found()),
        ?_test(error_tool_not_found()),
        ?_test(error_prompt_not_found()),
        ?_test(error_capability_not_supported()),
        ?_test(error_not_initialized()),
        ?_test(error_validation_failed()),
        ?_test(error_internal()),
        ?_test(error_parse()),
        ?_test(error_message_too_large())
    ].

encode_error_response_basic() ->
    Id = 123,
    Code = ?JSONRPC_INVALID_PARAMS,
    Message = <<"Invalid parameters">>,
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"error\":{\"code\":-32602,\"message\":\"Invalid parameters\"}}">>,
    ?assertEqual(Expected, Encoded).

encode_error_response_with_data() ->
    Id = 456,
    Code = ?MCP_ERROR_RESOURCE_NOT_FOUND,
    Message = <<"Resource not found">>,
    Data = #{<<"uri">> => <<"file.txt">>, <<"details">> => <<"File does not exist">>},
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":456,\"error\":{\"code\":-32001,\"message\":\"Resource not found\",\"data\":{\"uri\":\"file.txt\",\"details\":\"File does not exist\"}}}">>,
    ?assertEqual(Expected, Encoded).

encode_error_response_with_binary_data() ->
    Id = 789,
    Code = ?MCP_ERROR_VALIDATION_FAILED,
    Message = <<"Validation failed">>,
    Data = <<"Invalid input format">>,
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":789,\"error\":{\"code\":-32007,\"message\":\"Validation failed\",\"data\":{\"details\":\"Invalid input format\"}}}">>,
    ?assertEqual(Expected, Encoded).

encode_error_response_with_map_data() ->
    Id = 999,
    Code = ?MCP_ERROR_TOOL_NOT_FOUND,
    Message = <<"Tool not found">>,
    Data = #{<<"tool">> => <<"calculator">>, <<"available_tools">> => [<<"math">>, <<"text">>]},
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":999,\"error\":{\"code\":-32002,\"message\":\"Tool not found\",\"data\":{\"tool\":\"calculator\",\"available_tools\":[\"math\",\"text\"]}}}">>,
    ?assertEqual(Expected, Encoded).

encode_error_response_invalid_code_fallback() ->
    Id = 111,
    Code = -99999,  % Invalid code, should fallback to internal error
    Message = <<"Custom error">>,
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":111,\"error\":{\"code\":-32603,\"message\":\"Internal error\"}}">>,
    ?assertEqual(Expected, Encoded).

error_method_not_found() ->
    Method = <<"nonexistent.method">>,
    Encoded = erlmcp_json_rpc:error_method_not_found(123, Method),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"error\":{\"code\":-32601,\"message\":\"Method not found\",\"data\":{\"method\":\"nonexistent.method\"}}}">>,
    ?assertEqual(Expected, Encoded).

error_invalid_params() ->
    Details = <<"Parameter 'name' is required">>,
    Encoded = erlmcp_json_rpc:error_invalid_params(456, Details),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":456,\"error\":{\"code\":-32602,\"message\":\"Invalid params\",\"data\":{\"details\":\"Parameter 'name' is required\"}}}">>,
    ?assertEqual(Expected, Encoded).

error_resource_not_found() ->
    Uri = <<"file:///nonexistent.txt">>,
    Encoded = erlmcp_json_rpc:error_resource_not_found(789, Uri),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":789,\"error\":{\"code\":-32001,\"message\":\"Resource not found\",\"data\":{\"uri\":\"file:///nonexistent.txt\"}}}">>,
    ?assertEqual(Expected, Encoded).

error_tool_not_found() ->
    ToolName = <<"unknown_tool">>,
    Encoded = erlmcp_json_rpc:error_tool_not_found(999, ToolName),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":999,\"error\":{\"code\":-32002,\"message\":\"Tool not found\",\"data\":{\"tool\":\"unknown_tool\"}}}">>,
    ?assertEqual(Expected, Encoded).

error_prompt_not_found() ->
    PromptName = <<"missing_prompt">>,
    Encoded = erlmcp_json_rpc:error_prompt_not_found(1000, PromptName),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":1000,\"error\":{\"code\":-32003,\"message\":\"Prompt not found\",\"data\":{\"prompt\":\"missing_prompt\"}}}">>,
    ?assertEqual(Expected, Encoded).

error_capability_not_supported() ->
    Capability = <<"unknown_capability">>,
    Encoded = erlmcp_json_rpc:error_capability_not_supported(1001, Capability),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":1001,\"error\":{\"code\":-32004,\"message\":\"Capability not supported\",\"data\":{\"capability\":\"unknown_capability\"}}}">>,
    ?assertEqual(Expected, Encoded).

error_not_initialized() ->
    Encoded = erlmcp_json_rpc:error_not_initialized(1002),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":1002,\"error\":{\"code\":-32005,\"message\":\"Server not initialized\"}}">>,
    ?assertEqual(Expected, Encoded).

error_validation_failed() ->
    Details = <<"Schema validation failed">>,
    Encoded = erlmcp_json_rpc:error_validation_failed(1003, Details),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":1003,\"error\":{\"code\":-32007,\"message\":\"Validation failed\",\"data\":{\"details\":\"Schema validation failed\"}}}">>,
    ?assertEqual(Expected, Encoded).

error_internal() ->
    Encoded = erlmcp_json_rpc:error_internal(1004),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":1004,\"error\":{\"code\":-32603,\"message\":\"Internal error\"}}">>,
    ?assertEqual(Expected, Encoded).

error_parse() ->
    Encoded = erlmcp_json_rpc:error_parse(1005),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":1005,\"error\":{\"code\":-32700,\"message\":\"Parse error\"}}">>,
    ?assertEqual(Expected, Encoded).

error_message_too_large() ->
    MaxSize = 16777216,
    Encoded = erlmcp_json_rpc:error_message_too_large(1006, MaxSize),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"id\":1006,\"error\":{\"code\":-32012,\"message\":\"Message size exceeds maximum allowed\",\"data\":{\"maxSize\":16777216,\"unit\":\"bytes\",\"maxSizeReadable\":\"16.00 MB\"}}}">>,
    ?assertEqual(Expected, Encoded).

%% Test error response decoding
decode_error_response_test_() ->
    [
        ?_test(decode_error_basic()),
        ?_test(decode_error_with_data()),
        ?_test(decode_error_different_codes())
    ].

decode_error_basic() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"error\":{\"code\":-32602,\"message\":\"Invalid parameters\"}}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ExpectedError = #{<<"code">> => -32602, <<"message">> => <<"Invalid parameters">>},
    ?assertMatch(#json_rpc_response{id = 123, result = undefined, error = ExpectedError}, Decoded).

decode_error_with_data() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":456,\"error\":{\"code\":-32001,\"message\":\"Resource not found\",\"data\":{\"uri\":\"file.txt\"}}}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ExpectedError = #{<<"code">> => -32001, <<"message">> => <<"Resource not found">>,
                     <<"data">> => #{<<"uri">> => <<"file.txt">>}},
    ?assertMatch(#json_rpc_response{id = 456, result = undefined, error = ExpectedError}, Decoded).

decode_error_different_codes() ->
    TestCases = [
        {-32700, ?JSONRPC_PARSE_ERROR},
        {-32600, ?JSONRPC_INVALID_REQUEST},
        {-32601, ?JSONRPC_METHOD_NOT_FOUND},
        {-32602, ?JSONRPC_INVALID_PARAMS},
        {-32603, ?JSONRPC_INTERNAL_ERROR},
        {-32001, ?MCP_ERROR_RESOURCE_NOT_FOUND},
        {-32002, ?MCP_ERROR_TOOL_NOT_FOUND}
    ],
    lists:foreach(fun({Code, ExpectedName}) ->
        Json = io_lib:format("{\"jsonrpc\":\"2.0\",\"id\":789,\"error\":{\"code\":~p,\"message\":\"~s\"}}", [Code, ExpectedName]),
        {ok, Decoded} = erlmcp_json_rpc:decode_message(iolist_to_binary(Json)),
        ExpectedError = #{<<"code">> => Code, <<"message">> => ExpectedName},
        ?assertMatch(#json_rpc_response{id = 789, result = undefined, error = ExpectedError}, Decoded)
    end, TestCases).

%%====================================================================
%% Notification Encoding/Decoding Tests
%%====================================================================

%% Test notification encoding
encode_notification_test_() ->
    [
        ?_test(encode_notification_with_params()),
        ?_test(encode_notification_with_array_params()),
        ?_test(encode_notification_no_params()),
        ?_test(encode_notification_empty_params())
    ].

encode_notification_with_params() ->
    Method = <<"resources/updated">>,
    Params = #{<<"uri">> => <<"file.txt">>, <<"event">> => <<"created">>},
    Encoded = erlmcp_json_rpc:encode_notification(Method, Params),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"method\":\"resources/updated\",\"params\":{\"uri\":\"file.txt\",\"event\":\"created\"}}">>,
    ?assertEqual(Expected, Encoded).

encode_notification_with_array_params() ->
    Method = <<"notifications/progress">>,
    Params = [50, 100, <<"Processing...">>],
    Encoded = erlmcp_json_rpc:encode_notification(Method, Params),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"method\":\"notifications/progress\",\"params\":[50,100,\"Processing...\"]}">>,
    ?assertEqual(Expected, Encoded).

encode_notification_no_params() ->
    Method = <<"notifications/initialized">>,
    Params = undefined,
    Encoded = erlmcp_json_rpc:encode_notification(Method, Params),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}">>,
    ?assertEqual(Expected, Encoded).

encode_notification_empty_params() ->
    Method = <<"custom/event">>,
    Params = #{},
    Encoded = erlmcp_json_rpc:encode_notification(Method, Params),
    Expected = <<"{\"jsonrpc\":\"2.0\",\"method\":\"custom/event\",\"params\":{}}">>,
    ?assertEqual(Expected, Encoded).

%% Test notification decoding
decode_notification_test_() ->
    [
        ?_test(decode_notification_with_object_params()),
        ?_test(decode_notification_with_array_params()),
        ?_test(decode_notification_no_params()),
        ?_test(decode_notification_empty_params())
    ].

decode_notification_with_object_params() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"resources/updated\",\"params\":{\"uri\":\"file.txt\",\"event\":\"created\"}}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_notification{method = <<"resources/updated">>,
                                      params = #{<<"uri">> := <<"file.txt">>,
                                               <<"event">> := <<"created">>}}, Decoded).

decode_notification_with_array_params() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"notifications/progress\",\"params\":[50,100,\"Processing...\"]}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_notification{method = <<"notifications/progress">>,
                                      params = [50, 100, <<"Processing...">>]}, Decoded).

decode_notification_no_params() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_notification{method = <<"notifications/initialized">>, params = undefined}, Decoded).

decode_notification_empty_params() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"custom/event\",\"params\":{}}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_notification{method = <<"custom/event">>, params = #{}}, Decoded).

%% Test notification decoding errors
decode_notification_error_test_() ->
    [
        ?_test(decode_notification_missing_method()),
        ?_test(decode_notification_invalid_method()),
        ?_test(decode_notification_has_id())
    ].

decode_notification_missing_method() ->
    Json = <<"{\"jsonrpc\":\"2.0\"}">>,
    {error, {invalid_request, unknown_message_type}} = erlmcp_json_rpc:decode_message(Json).

decode_notification_invalid_method() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":123}">>,
    {error, {invalid_request, {invalid_method, 123}}} = erlmcp_json_rpc:decode_message(Json).

decode_notification_has_id() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"method\":\"resources/updated\"}">>,
    {error, {invalid_request, unknown_message_type}} = erlmcp_json_rpc:decode_message(Json).

%%====================================================================
%% Batch Request Handling Tests
%%====================================================================

%% Test batch encoding
encode_batch_test_() ->
    [
        ?_test(encode_batch_mixed()),
        ?_test(encode_batch_all_requests()),
        ?_test(encode_batch_all_responses()),
        ?_test(encode_batch_all_notifications()),
        ?_test(encode_batch_empty())
    ].

encode_batch_mixed() ->
    Messages = [
        erlmcp_json_rpc:encode_request(1, <<"resources/list">>, undefined),
        erlmcp_json_rpc:encode_response(2, #{<<"status">> => success}),
        erlmcp_json_rpc:encode_notification(<<"progress">>, #{<<"current">> => 50})
    ],
    Encoded = erlmcp_json_rpc:encode_batch(Messages),
    Expected = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"resources/list\"},{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"status\":\"success\"}},{\"jsonrpc\":\"2.0\",\"method\":\"progress\",\"params\":{\"current\":50}}]">>,
    ?assertEqual(Expected, Encoded).

encode_batch_all_requests() ->
    Messages = [
        erlmcp_json_rpc:encode_request(1, <<"tools/call">>, #{<<"name">> => <<"math">>}),
        erlmcp_json_rpc:encode_request(2, <<"resources/read">>, [<<"file1.txt">>])
    ],
    Encoded = erlmcp_json_rpc:encode_batch(Messages),
    Expected = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"math\"}},{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"resources/read\",\"params\":[\"file1.txt\"]}]">>,
    ?assertEqual(Expected, Encoded).

encode_batch_all_responses() ->
    Messages = [
        erlmcp_json_rpc:encode_response(1, <<"ok">>),
        erlmcp_json_rpc:encode_error_response(2, ?JSONRPC_METHOD_NOT_FOUND, <<"Method not found">>)
    ],
    Encoded = erlmcp_json_rpc:encode_batch(Messages),
    Expected = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"ok\"},{\"jsonrpc\":\"2.0\",\"id\":2,\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}]">>,
    ?assertEqual(Expected, Encoded).

encode_batch_all_notifications() ->
    Messages = [
        erlmcp_json_rpc:encode_notification(<<"resources/updated">>, #{<<"uri">> => <<"file.txt">>}),
        erlmcp_json_rpc:encode_notification(<<"notifications/initialized">>, undefined)
    ],
    Encoded = erlmcp_json_rpc:encode_batch(Messages),
    Expected = <<"[{\"jsonrpc\":\"2.0\",\"method\":\"resources/updated\",\"params\":{\"uri\":\"file.txt\"}},{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}]">>,
    ?assertEqual(Expected, Encoded).

encode_batch_empty() ->
    Messages = [],
    Encoded = erlmcp_json_rpc:encode_batch(Messages),
    Expected = <<">>">>,
    ?assertEqual(Expected, Encoded).

%% Test batch decoding
decode_batch_valid_test_() ->
    [
        ?_test(decode_batch_mixed()),
        ?_test(decode_batch_all_requests()),
        ?_test(decode_batch_with_errors()),
        ?_test(decode_batch_single_message())
    ].

decode_batch_mixed() ->
    Json = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"resources/list\"},{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":\"success\"},{\"jsonrpc\":\"2.0\",\"method\":\"progress\",\"params\":{\"current\":50}}]">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_batch(Json),
    Length = length(Decoded),
    ?assertEqual(3, Length),
    ?assertMatch([#json_rpc_request{id = 1, method = <<"resources/list">>, params = undefined},
                 #json_rpc_response{id = 2, result = <<"success">>, error = undefined},
                 #json_rpc_notification{method = <<"progress">>, params = #{<<"current">> := 50}}], Decoded).

decode_batch_all_requests() ->
    Json = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"math\"}},{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"resources/read\",\"params\":[\"file1.txt\"]}]">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_batch(Json),
    ?assertEqual(2, length(Decoded)),
    ?assertMatch([#json_rpc_request{id = 1, method = <<"tools/call">>, params = #{<<"name">> := <<"math">>}},
                 #json_rpc_request{id = 2, method = <<"resources/read">>, params = [<<"file1.txt">>]}], Decoded).

decode_batch_with_errors() ->
    Json = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"resources/list\"},{\"jsonrpc\":\"2.0\",\"id\":2,\"error\":{\"code\":-32601,\"message\":\"Method not found\"}}]">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_batch(Json),
    ?assertEqual(2, length(Decoded)),
    ?assertMatch([#json_rpc_request{id = 1, method = <<"resources/list">>, params = undefined},
                 #json_rpc_response{id = 2, result = undefined,
                                   error = #{<<"code">> := -32601, <<"message">> := <<"Method not found">>}}], Decoded).

decode_batch_single_message() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"resources/list\"}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_batch(Json),
    ?assertEqual(1, length(Decoded)),
    ?assertMatch([#json_rpc_request{id = 1, method = <<"resources/list">>, params = undefined}], Decoded).

%% Test batch decoding errors
decode_batch_error_test_() ->
    [
        ?_test(decode_batch_empty()),
        ?_test(decode_batch_invalid_json()),
        ?_test(decode_batch_invalid_array_elements()),
        ?_test(decode_batch_not_array_or_object())
    ].

decode_batch_empty() ->
    Json = <<">>">>,
    {error, {invalid_request, empty_batch}} = erlmcp_json_rpc:decode_batch(Json).

decode_batch_invalid_json() ->
    Json = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"},]">>,
    {error, {parse_error, invalid_json}} = erlmcp_json_rpc:decode_batch(Json).

decode_batch_invalid_array_elements() ->
    Json = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}, {\"invalid\": \"object\"}]">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_batch(Json),
    ?assertEqual(1, length(Decoded)),
    ?assertMatch([#json_rpc_request{id = 1, method = <<"test">>, params = undefined}], Decoded).

decode_batch_not_array_or_object() ->
    Json = <<>"">>,
    {error, {invalid_json, not_array_or_object}} = erlmcp_json_rpc:decode_batch(Json).

%% Test batch detection
is_batch_request_test_() ->
    [
        ?_test(is_batch_true()),
        ?_test(is_batch_false()),
        ?_test(is_batch_invalid_json())
    ].

is_batch_true() ->
    Json = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"},{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"test2\"}]">>,
    ?assertEqual(true, erlmcp_json_rpc:is_batch_request(Json)).

is_batch_false() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}">>,
    ?assertEqual(false, erlmcp_json_rpc:is_batch_request(Json)).

is_batch_invalid_json() ->
    Json = <<"[{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",}">>,
    ?assertEqual(false, erlmcp_json_rpc:is_batch_request(Json)).

%%====================================================================
%% Error Code Validation Tests
%%====================================================================

validate_error_code_test_() ->
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
    InvalidCodes = [
        -32701,  % Below parse error range
        -32604,  % Between invalid params and internal error
        -32099,  % Below server error min
        -32000,  % Above server error max
        -99999,  % Way out of range
        0,       % Zero
        32700,   % Above parse error range
        32000    % In positive range
    ],
    [
        ?_assertEqual(lists:duplicate(length(ValidCodes), true),
                     [erlmcp_json_rpc:validate_error_code(Code) || Code <- ValidCodes]),
        ?_assertEqual(lists:duplicate(length(InvalidCodes), false),
                     [erlmcp_json_rpc:validate_error_code(Code) || Code <- InvalidCodes])
    ].

%%====================================================================
%% Message Size Limits Tests
%%====================================================================

%% Test message size validation with different transport types
message_size_validation_test_() ->
    [
        ?_test(message_size_validation_http()),
        ?_test(message_size_validation_tcp()),
        ?_test(message_size_validation_websocket()),
        ?_test(message_size_validation_stdio()),
        ?_test(message_size_validation_sse()),
        ?_test(message_size_validation_default()),
        ?_test(message_size_validation_within_limits()),
        ?_test(message_size_validation_exceeds_limits())
    ].

message_size_validation_http() ->
    SmallMessage = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}">>,
    LargeMessage = binary:copy(<<"x">>, ?MCP_DEFAULT_HTTP_BODY_SIZE_LIMIT + 1),
    ?assertEqual(ok, erlmcp_json_rpc:decode_message(SmallMessage, http)),
    ?assertEqual({error, {message_too_large, _}}, erlmcp_json_rpc:decode_message(LargeMessage, http)).

message_size_validation_tcp() ->
    SmallMessage = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}">>,
    LargeMessage = binary:copy(<<"x">>, ?MCP_DEFAULT_MESSAGE_SIZE_LIMIT + 1),
    ?assertEqual(ok, erlmcp_json_rpc:decode_message(SmallMessage, tcp)),
    ?assertEqual({error, {message_too_large, _}}, erlmcp_json_rpc:decode_message(LargeMessage, tcp)).

message_size_validation_websocket() ->
    SmallMessage = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}">>,
    LargeMessage = binary:copy(<<"x">>, ?MCP_DEFAULT_WS_MESSAGE_SIZE_LIMIT + 1),
    ?assertEqual(ok, erlmcp_json_rpc:decode_message(SmallMessage, websocket)),
    ?assertEqual({error, {message_too_large, _}}, erlmcp_json_rpc:decode_message(LargeMessage, websocket)).

message_size_validation_stdio() ->
    SmallMessage = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}">>,
    LargeMessage = binary:copy(<<"x">>, ?MCP_DEFAULT_MESSAGE_SIZE_LIMIT + 1),
    ?assertEqual(ok, erlmcp_json_rpc:decode_message(SmallMessage, stdio)),
    ?assertEqual({error, {message_too_large, _}}, erlmcp_json_rpc:decode_message(LargeMessage, stdio)).

message_size_validation_sse() ->
    SmallMessage = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}">>,
    LargeMessage = binary:copy(<<"x">>, ?MCP_DEFAULT_SSE_EVENT_SIZE_LIMIT + 1),
    ?assertEqual(ok, erlmcp_json_rpc:decode_message(SmallMessage, sse)),
    ?assertEqual({error, {message_too_large, _}}, erlmcp_json_rpc:decode_message(LargeMessage, sse)).

message_size_validation_default() ->
    SmallMessage = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}">>,
    LargeMessage = binary:copy(<<"x">>, ?MCP_DEFAULT_MESSAGE_SIZE_LIMIT + 1),
    ?assertEqual(ok, erlmcp_json_rpc:decode_message(SmallMessage, default)),
    ?assertEqual({error, {message_too_large, _}}, erlmcp_json_rpc:decode_message(LargeMessage, default)).

message_size_validation_within_limits() ->
    % Create a message exactly at the limit
    MaxMessage = binary:copy(<<"x">>, ?MCP_DEFAULT_MESSAGE_SIZE_LIMIT),
    ?assertEqual(ok, erlmcp_json_rpc:decode_message(MaxMessage, default)),

    % Create a message just under the limit
    UnderLimitMessage = binary:copy(<<"x">>, ?MCP_DEFAULT_MESSAGE_SIZE_LIMIT - 1),
    ?assertEqual(ok, erlmcp_json_rpc:decode_message(UnderLimitMessage, default)).

message_size_validation_exceeds_limits() ->
    % Create messages that exceed various limits
    ExceedHttp = binary:copy(<<"x">>, ?MCP_DEFAULT_HTTP_BODY_SIZE_LIMIT + 1),
    ExceedTcp = binary:copy(<<"x">>, ?MCP_DEFAULT_MESSAGE_SIZE_LIMIT + 1),
    ExceedWs = binary:copy(<<"x">>, ?MCP_DEFAULT_WS_MESSAGE_SIZE_LIMIT + 1),
    ExceedSse = binary:copy(<<"x">>, ?MCP_DEFAULT_SSE_EVENT_SIZE_LIMIT + 1),

    ?assertEqual({error, {message_too_large, _}}, erlmcp_json_rpc:decode_message(ExceedHttp, http)),
    ?assertEqual({error, {message_too_large, _}}, erlmcp_json_rpc:decode_message(ExceedTcp, tcp)),
    ?assertEqual({error, {message_too_large, _}}, erlmcp_json_rpc:decode_message(ExceedWs, websocket)),
    ?assertEqual({error, {message_too_large, _}}, erlmcp_json_rpc:decode_message(ExceedSse, sse)).

%% Test error message generation for large messages
error_message_too_large_format_test_() ->
    [
        ?_test(error_message_contains_max_size()),
        ?_test(error_message_contains_unit()),
        ?_test(error_message_contains_readable_format())
    ].

error_message_contains_max_size() ->
    MaxSize = 16777216,
    ErrorJson = erlmcp_json_rpc:error_message_too_large(1, MaxSize),
    Decoded = jsx:decode(ErrorJson),
    ?assertEqual(MaxSize, maps:get(<<"maxSize">>, Decoded)).

error_message_contains_unit() ->
    MaxSize = 16777216,
    ErrorJson = erlmcp_json_rpc:error_message_too_large(1, MaxSize),
    Decoded = jsx:decode(ErrorJson),
    ?assertEqual(<<"bytes">>, maps:get(<<"unit">>, Decoded)).

error_message_contains_readable_format() ->
    MaxSize = 16777216,
    ErrorJson = erlmcp_json_rpc:error_message_too_large(1, MaxSize),
    Decoded = jsx:decode(ErrorJson),
    ?assertEqual(<<"16.00 MB">>, maps:get(<<"maxSizeReadable">>, Decoded)).

%%====================================================================
%% Protocol Version Negotiation Tests
%%====================================================================

protocol_version_validation_test_() ->
    [
        ?_test(protocol_version_valid_2_0()),
        ?_test(protocol_version_invalid_1_0()),
        ?_test(protocol_version_invalid_2_1()),
        ?_test(protocol_version_missing_field()),
        ?_test(protocol_version_wrong_type())
    ].

protocol_version_valid_2_0() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_request{id = 1, method = <<"test">>, params = undefined}, Decoded).

protocol_version_invalid_1_0() ->
    Json = <<"{\"jsonrpc\":\"1.0\",\"id\":1,\"method\":\"test\"}">>,
    {error, {invalid_request, {wrong_version, <<"1.0">>}}} = erlmcp_json_rpc:decode_message(Json).

protocol_version_invalid_2_1() ->
    Json = <<"{\"jsonrpc\":\"2.1\",\"id\":1,\"method\":\"test\"}">>,
    {error, {invalid_request, {wrong_version, <<"2.1">>}}} = erlmcp_json_rpc:decode_message(Json).

protocol_version_missing_field() ->
    Json = <<"{\"id\":1,\"method\":\"test\"}">>,
    {error, {invalid_request, missing_jsonrpc}} = erlmcp_json_rpc:decode_message(Json).

protocol_version_wrong_type() ->
    Json = <<"{\"jsonrpc\":2.0,\"id\":1,\"method\":\"test\"}">>,
    {error, {parse_error, invalid_json}} = erlmcp_json_rpc:decode_message(Json).

%%====================================================================
%% Message Flow Validation Tests
%%====================================================================

%% Test round-trip encoding/decoding
round_trip_test_() ->
    [
        ?_test(round_trip_request()),
        ?_test(round_trip_response()),
        ?_test(round_trip_notification()),
        ?_test(round_trip_error()),
        ?_test(round_trip_batch())
    ].

round_trip_request() ->
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

round_trip_response() ->
    Original = #json_rpc_response{
        id = 456,
        result = #{<<"status">> => success, <<"value">> => 42},
        error = undefined
    },
    Encoded = erlmcp_json_rpc:encode_response(Original#json_rpc_response.id, Original#json_rpc_response.result),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Encoded),
    ?assertEqual(Original#json_rpc_response.id, Decoded#json_rpc_response.id),
    ?assertEqual(Original#json_rpc_response.result, Decoded#json_rpc_response.result),
    ?assertEqual(Original#json_rpc_response.error, Decoded#json_rpc_response.error).

round_trip_notification() ->
    Original = #json_rpc_notification{
        method = <<"resources/updated">>,
        params = #{<<"uri">> => <<"file.txt">>, <<"event">> => <<"created">>}
    },
    Encoded = erlmcp_json_rpc:encode_notification(Original#json_rpc_notification.method, Original#json_rpc_notification.params),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Encoded),
    ?assertEqual(Original#json_rpc_notification.method, Decoded#json_rpc_notification.method),
    ?assertEqual(Original#json_rpc_notification.params, Decoded#json_rpc_notification.params).

round_trip_error() ->
    OriginalError = #{<<"code">> => -32602, <<"message">> => <<"Invalid params">>},
    Original = #json_rpc_response{
        id = 789,
        result = undefined,
        error = OriginalError
    },
    Encoded = erlmcp_json_rpc:encode_error_response(Original#json_rpc_response.id, maps:get(<<"code">>, OriginalError), maps:get(<<"message">>, OriginalError)),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Encoded),
    ?assertEqual(Original#json_rpc_response.id, Decoded#json_rpc_response.id),
    ?assertEqual(Original#json_rpc_response.result, Decoded#json_rpc_response.result),
    ?assertEqual(Original#json_rpc_response.error, Decoded#json_rpc_response.error).

round_trip_batch() ->
    OriginalRequests = [
        #json_rpc_request{id = 1, method = <<"resources/list">>, params = undefined},
        #json_rpc_request{id = 2, method = <<"tools/call">>, params = #{<<"name">> => <<"math">>}}
    ],
    EncodedBatch = erlmcp_json_rpc:encode_batch(OriginalRequests),
    {ok, DecodedBatch} = erlmcp_json_rpc:decode_batch(EncodedBatch),
    ?assertEqual(length(OriginalRequests), length(DecodedBatch)),
    lists:foreach(fun({Original, Decoded}) ->
        ?assertEqual(Original#json_rpc_request.id, Decoded#json_rpc_request.id),
        ?assertEqual(Original#json_rpc_request.method, Decoded#json_rpc_request.method),
        ?assertEqual(Original#json_rpc_request.params, Decoded#json_rpc_request.params)
    end, lists:zip(OriginalRequests, DecodedBatch)).

%% Test message ID handling across different message types
message_id_consistency_test_() ->
    [
        ?_test(request_must_have_id()),
        ?_test(response_must_have_id()),
        ?_test(notification_must_not_have_id()),
        ?_test(error_response_must_have_id())
    ].

request_must_have_id() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"method\":\"test\"}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_request{id = 123, _}, Decoded).

response_must_have_id() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":456,\"result\":\"success\"}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_response{id = 456, _}, Decoded).

notification_must_not_have_id() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\"}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_notification{method = <<"test">>, _}, Decoded).

error_response_must_have_id() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":789,\"error\":{\"code\":-32602,\"message\":\"Invalid params\"}}">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch(#json_rpc_response{id = 789, _}, Decoded).

%%====================================================================
%% Performance Tests
%%====================================================================

performance_test_() ->
    {setup,
        fun() ->
            % Setup test data
            LargeData = binary:copy(<<"x">>, 1024),  % 1KB data
            {LargeData, 1000}
        end,
        fun(_Cleanup) ->
            % Cleanup
            ok
        end,
        fun({LargeData, Count}) ->
            [
                ?_test(encode_performance(LargeData, Count)),
                ?_test(decode_performance(LargeData, Count)),
                ?_test(batch_performance(Count))
            ]
        end}.

encode_performance(LargeData, Count) ->
    % Test encoding performance with large data
    TestRequests = [
        erlmcp_json_rpc:encode_request(I, <<"tools/call">>, #{<<"data">> => LargeData})
        || I <- lists:seq(1, Count)
    ],
    % Verify all encodings are valid
    lists:foreach(fun(Request) ->
        {ok, _} = erlmcp_json_rpc:decode_message(Request)
    end, TestRequests).

decode_performance(LargeData, Count) ->
    % Test decoding performance with large data
    TestRequests = [
        erlmcp_json_rpc:encode_request(I, <<"tools/call">>, #{<<"data">> => LargeData})
        || I <- lists:seq(1, Count)
    ],
    % Verify all decodings are fast enough
    lists:foreach(fun(Request) ->
        {ok, _} = erlmcp_json_rpc:decode_message(Request)
    end, TestRequests).

batch_performance(Count) ->
    % Test batch processing performance
    Messages = [
        erlmcp_json_rpc:encode_request(I, <<"resources/list">>, undefined)
        || I <- lists:seq(1, Count)
    ],
    BatchJson = erlmcp_json_rpc:encode_batch(Messages),
    {ok, DecodedBatch} = erlmcp_json_rpc:decode_batch(BatchJson),
    ?assertEqual(Count, length(DecodedBatch)).

%%====================================================================
%% Edge Cases and Boundary Conditions
%%====================================================================

edge_cases_test_() ->
    [
        ?_test(unicode_in_method_name()),
        ?_test(unicode_in_params()),
        ?_test(very_long_method_name()),
        ?_test(very_deep_nested_params()),
        ?_test(special_characters_in_message()),
        ?_test(empty_strings_and_nulls()),
        ?_test(negative_numbers_in_params()),
        ?_test(floating_point_numbers()),
        ?_test(booleans_in_params()),
        ?_test(large_numbers())
    ].

unicode_in_method_name() ->
    Method = <<"méthode/测试/テスト">>,
    Params = #{<<"key">> => <<"value 値 価値">>},
    Json = erlmcp_json_rpc:encode_request(1, Method, Params),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertEqual(Method, Decoded#json_rpc_request.method).

unicode_in_params() ->
    Params = #{
        <<"unicode_key">> => <<"value 値 価値">>,
        <<"list">> => [<<"item1 中文">>, <<"item2 日本語">>]
    },
    Json = erlmcp_json_rpc:encode_request(1, <<"test">>, Params),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertEqual(Params, Decoded#json_rpc_request.params).

very_long_method_name() ->
    LongMethod = binary:copy(<<"a">>, 1000),
    Json = erlmcp_json_rpc:encode_request(1, LongMethod, undefined),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertEqual(LongMethod, Decoded#json_rpc_request.method).

very_deep_nested_params() ->
    DeepParams = create_deep_params(10),
    Json = erlmcp_json_rpc:encode_request(1, <<"test">>, DeepParams),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertEqual(DeepParams, Decoded#json_rpc_request.params).

special_characters_in_message() ->
    SpecialChars = #{
        <<"quotes">> => <<"\"quoted\" 'single'">>,
        <<"newlines">> => <<"line1\nline2\r\nline3">>,
        <<"escapes">> => <<"backslash\\slash">>,
        <<"unicode">> => <<"ñáéíóú 中文 日本語">>
    },
    Json = erlmcp_json_rpc:encode_request(1, <<"test">>, SpecialChars),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertEqual(SpecialChars, Decoded#json_rpc_request.params).

empty_strings_and_nulls() ->
    Params = #{
        <<"empty_string">> => <<>>,
        <<"null_value">> => null,
        <<"false_value">> => false
    },
    Json = erlmcp_json_rpc:encode_request(1, <<"test">>, Params),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertEqual(Params, Decoded#json_rpc_request.params).

negative_numbers_in_params() ->
    Params = #{
        <<"negative_int">> => -42,
        <<"negative_float">> => -3.14,
        <<"zero">> => 0
    },
    Json = erlmcp_json_rpc:encode_request(1, <<"test">>, Params),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertEqual(Params, Decoded#json_rpc_request.params).

floating_point_numbers() ->
    Params = #{
        <<"pi">> => 3.14159,
        <<"e">> => 2.71828,
        <<"nan">> => 0.0/0.0,
        <<"inf">> => 1.0/0.0
    },
    Json = erlmcp_json_rpc:encode_request(1, <<"test">>, Params),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    % NaN and Inf might be converted to null by JSON parser
    CleanParams = maps:without([<<"nan">>, <<"inf">>], Params),
    CleanDecoded = maps:without([<<"nan">>, <<"inf">>], Decoded#json_rpc_request.params),
    ?assertEqual(CleanParams, CleanDecoded).

large_numbers() ->
    Params = #{
        <<"large_int">> <<>(1 bsl 60),  % 2^60
        <<"small_int">> <<>(-1 bsl 30),
        <<"max_json_num">> <<>(9007199254740991)  % Max safe JSON integer
    },
    Json = erlmcp_json_rpc:encode_request(1, <<"test">>, Params),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Json),
    ?assertEqual(Params, Decoded#json_rpc_request.params).

%% Helper function to create deeply nested parameters
create_deep_params(0) -> #{};
create_deep_params(N) -> #{<<"level">> => N, <<"nested">> => create_deep_params(N - 1)}.

%%====================================================================
%% Integration Tests with erlmcp_json_rpc module
%%====================================================================

integration_test_() ->
    [
        ?_test(error_creation()),
        ?_test(error_creation_with_data()),
        ?_test(valid_error_codes()),
        ?_test(error_code_validation_edge_cases())
    ].

error_creation() ->
    Error = erlmcp_json_rpc:create_error(-32602, <<"Invalid params">>, #{<<"details">> => <<"Missing required field">>}),
    Expected = #mcp_error{
        code = -32602,
        message = <<"Invalid params">>,
        data = #{<<"details">> => <<"Missing required field">>}
    },
    ?assertEqual(Expected, Error).

error_creation_with_data() ->
    Error = erlmcp_json_rpc:create_error_with_data(-32001, <<"Resource not found">>, uri, <<"file.txt">>),
    Expected = #mcp_error{
        code = -32001,
        message = <<"Resource not found">>,
        data = #{<<"uri">> => <<"file.txt">>}
    },
    ?assertEqual(Expected, Error).

valid_error_codes() ->
    ValidCodes = ?VALID_ERROR_CODES,
    lists:foreach(fun(Code) ->
        ?assert(true =:= erlmcp_json_rpc:validate_error_code(Code))
    end, ValidCodes).

error_code_validation_edge_cases() ->
    % Test codes just outside valid ranges
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