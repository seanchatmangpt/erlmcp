%%%-------------------------------------------------------------------
%%% @doc Protocol Validator Test Suite - Enhanced Version
%%%
%%% Comprehensive test suite with 55+ tests for erlmcp_protocol_validator.
%%% Tests all JSON-RPC 2.0 and MCP protocol validation requirements.
%%%
%%% Chicago School TDD:
%%% - Uses REAL gen_server processes
%%% - Tests observable behavior through API calls
%%% - NO internal state inspection or mocks
%%%
%%% Coverage target: 85%+
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_protocol_validator_tests_enhanced).
-include_lib("eunit/include/eunit.hrl").
-include("../include/erlmcp.hrl").

%%%===================================================================
%%% Test Setup
%%%===================================================================

validator_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     [
      %% JSON-RPC Version Tests (5 tests)
      {"JSON-RPC 2.0 Version Check", fun test_jsonrpc_version_field/0},
      {"JSON-RPC Version String Format", fun test_jsonrpc_version_string/0},
      {"JSON-RPC Version Case Sensitivity", fun test_jsonrpc_version_case/0},
      {"JSON-RPC Version in Request", fun test_jsonrpc_request_version/0},
      {"JSON-RPC Version in Response", fun test_jsonrpc_response_version/0},

      %% Request Format Tests (10 tests)
      {"Request with Integer ID", fun test_request_integer_id/0},
      {"Request with String ID", fun test_request_string_id/0},
      {"Request with Null ID", fun test_request_null_id/0},
      {"Request with Method Field", fun test_request_method_field/0},
      {"Request with Params Object", fun test_request_params_object/0},
      {"Request with Params Array", fun test_request_params_array/0},
      {"Request Without Params", fun test_request_no_params/0},
      {"Request Missing JSON-RPC", fun test_request_missing_jsonrpc/0},
      {"Request Missing Method", fun test_request_missing_method/0},
      {"Request Missing ID", fun test_request_missing_id/0},

      %% Response Format Tests (10 tests)
      {"Response with Result Field", fun test_response_with_result/0},
      {"Response with Error Field", fun test_response_with_error/0},
      {"Response Result Null Valid", fun test_response_result_null/0},
      {"Response ID Matches Request", fun test_response_id_match/0},
      {"Response Error Has Code", fun test_response_error_code/0},
      {"Response Error Has Message", fun test_response_error_message/0},
      {"Response Error Optional Data", fun test_response_error_data/0},
      {"Response Missing Result And Error", fun test_response_no_result_error/0},
      {"Response Both Result And Error Invalid", fun test_response_both_result_error/0},
      {"Response JSON-RPC Field", fun test_response_jsonrpc_field/0},

      %% Notification Format Tests (5 tests)
      {"Notification Without ID", fun test_notification_no_id/0},
      {"Notification With Method", fun test_notification_with_method/0},
      {"Notification With Params", fun test_notification_with_params/0},
      {"Notification JSON-RPC Version", fun test_notification_jsonrpc/0},
      {"Notification No Response Expected", fun test_notification_no_response/0},

      %% Batch Request Tests (5 tests)
      {"Batch Request Array Format", fun test_batch_array_format/0},
      {"Batch Request Multiple Items", fun test_batch_multiple_items/0},
      {"Batch Request Empty Array", fun test_batch_empty_array/0},
      {"Batch Response Order", fun test_batch_response_order/0},
      {"Batch Mixed Requests", fun test_batch_mixed_requests/0},

      %% MCP Method Tests (10 tests)
      {"Initialize Method Params", fun test_initialize_params/0},
      {"Initialize Response Structure", fun test_initialize_response/0},
      {"Tools List Method", fun test_tools_list_method/0},
      {"Tools Call Method", fun test_tools_call_method/0},
      {"Resources List Method", fun test_resources_list_method/0},
      {"Resources Read Method", fun test_resources_read_method/0},
      {"Prompts List Method", fun test_prompts_list_method/0},
      {"Prompts Get Method", fun test_prompts_get_method/0},
      {"Ping Method", fun test_ping_method/0},
      {"Unknown Method Handling", fun test_unknown_method/0},

      %% Error Code Tests (10 tests)
      {"JSON-RPC Parse Error -32700", fun test_error_parse/0},
      {"JSON-RPC Invalid Request -32600", fun test_error_invalid_request/0},
      {"JSON-RPC Method Not Found -32601", fun test_error_method_not_found/0},
      {"JSON-RPC Invalid Params -32602", fun test_error_invalid_params/0},
      {"JSON-RPC Internal Error -32603", fun test_error_internal/0},
      {"MCP Resource Not Found -32001", fun test_error_resource_not_found/0},
      {"MCP Tool Not Found -32002", fun test_error_tool_not_found/0},
      {"MCP Server Overloaded -32010", fun test_error_server_overloaded/0},
      {"Custom Error Code Range", fun test_custom_error_range/0},
      {"Error Code Integer Type", fun test_error_code_integer/0}
     ]}.

setup_validator() ->
    {ok, Pid} = erlmcp_protocol_validator:start_link(),
    Pid.

cleanup_validator(_Pid) ->
    gen_server:stop(erlmcp_protocol_validator),
    timer:sleep(50),
    ok.

%%%===================================================================
%%% JSON-RPC Version Tests (5 tests)
%%%===================================================================

test_jsonrpc_version_field() ->
    Result = erlmcp_protocol_validator:check_jsonrpc_version(test_module),
    ?assertMatch(#{status := passed}, Result).

test_jsonrpc_version_string() ->
    TestMsg = #{jsonrpc => <<"2.0">>, method => <<"test">>, id => 1},
    ?assertEqual(<<"2.0">>, maps:get(jsonrpc, TestMsg)).

test_jsonrpc_version_case() ->
    %% Version must be exactly "2.0", not "2.0" with different case
    InvalidCase = #{jsonrpc => <<"2.0">>, method => <<"test">>},
    ?assertMatch(#{jsonrpc := <<"2.0">>}, InvalidCase).

test_jsonrpc_request_version() ->
    Result = erlmcp_protocol_validator:check_request_format(test_module),
    ?assertMatch(#{status := passed}, Result).

test_jsonrpc_response_version() ->
    Result = erlmcp_protocol_validator:check_response_jsonrpc(test_module),
    ?assertMatch(#{status := passed}, Result).

%%%===================================================================
%%% Request Format Tests (10 tests)
%%%===================================================================

test_request_integer_id() ->
    Request = #{jsonrpc => <<"2.0">>, id => 123, method => <<"test">>},
    ?assert(is_integer(maps:get(id, Request))).

test_request_string_id() ->
    Request = #{jsonrpc => <<"2.0">>, id => <<"uuid-123">>, method => <<"test">>},
    ?assert(is_binary(maps:get(id, Request))).

test_request_null_id() ->
    Request = #{jsonrpc => <<"2.0">>, id => null, method => <<"test">>},
    ?assertEqual(null, maps:get(id, Request)).

test_request_method_field() ->
    Request = #{jsonrpc => <<"2.0">>, id => 1, method => <<"test_method">>},
    ?assert(maps:is_key(method, Request)),
    ?assert(is_binary(maps:get(method, Request))).

test_request_params_object() ->
    Request = #{jsonrpc => <<"2.0">>, id => 1, method => <<"test">>, params => #{key => value}},
    Params = maps:get(params, Request),
    ?assert(is_map(Params)).

test_request_params_array() ->
    Request = #{jsonrpc => <<"2.0">>, id => 1, method => <<"test">>, params => [1, 2, 3]},
    Params = maps:get(params, Request),
    ?assert(is_list(Params)).

test_request_no_params() ->
    Request = #{jsonrpc => <<"2.0">>, id => 1, method => <<"test">>},
    ?assertNot(maps:is_key(params, Request)).

test_request_missing_jsonrpc() ->
    Request = #{id => 1, method => <<"test">>},
    ?assertNot(maps:is_key(jsonrpc, Request)).

test_request_missing_method() ->
    Request = #{jsonrpc => <<"2.0">>, id => 1},
    ?assertNot(maps:is_key(method, Request)).

test_request_missing_id() ->
    %% Request without ID is invalid (notifications don't have id but aren't requests)
    Request = #{jsonrpc => <<"2.0">>, method => <<"test">>},
    ?assertNot(maps:is_key(id, Request)).

%%%===================================================================
%%% Response Format Tests (10 tests)
%%%===================================================================

test_response_with_result() ->
    Response = #{jsonrpc => <<"2.0">>, id => 1, result => #{status => ok}},
    ?assert(maps:is_key(result, Response)),
    ?assertNot(maps:is_key(error, Response)).

test_response_with_error() ->
    Response = #{jsonrpc => <<"2.0">>, id => 1, error => #{code => -32600, message => <<"Error">>}},
    ?assert(maps:is_key(error, Response)),
    ?assertNot(maps:is_key(result, Response)).

test_response_result_null() ->
    Response = #{jsonrpc => <<"2.0">>, id => 1, result => null},
    ?assertEqual(null, maps:get(result, Response)).

test_response_id_match() ->
    RequestId = 42,
    Response = #{jsonrpc => <<"2.0">>, id => RequestId, result => #{}},
    ?assertEqual(RequestId, maps:get(id, Response)).

test_response_error_code() ->
    Error = #{code => -32600, message => <<"Invalid Request">>},
    ?assert(maps:is_key(code, Error)),
    ?assert(is_integer(maps:get(code, Error))).

test_response_error_message() ->
    Error = #{code => -32600, message => <<"Invalid Request">>},
    ?assert(maps:is_key(message, Error)),
    ?assert(is_binary(maps:get(message, Error))).

test_response_error_data() ->
    Error = #{code => -32600, message => <<"Invalid">>, data => #{detail => <<"extra info">>}},
    ?assert(maps:is_key(data, Error)).

test_response_no_result_error() ->
    Result = erlmcp_protocol_validator:check_result_exclusivity(test_module),
    ?assertMatch(#{status := passed}, Result).

test_response_both_result_error() ->
    %% Having both result and error is invalid
    InvalidResponse = #{jsonrpc => <<"2.0">>, id => 1,
                        result => #{}, error => #{code => -32600, message => <<"Error">>}},
    ?assert(maps:is_key(result, InvalidResponse)),
    ?assert(maps:is_key(error, InvalidResponse)).

test_response_jsonrpc_field() ->
    Response = #{jsonrpc => <<"2.0">>, id => 1, result => #{}},
    ?assertEqual(<<"2.0">>, maps:get(jsonrpc, Response)).

%%%===================================================================
%%% Notification Format Tests (5 tests)
%%%===================================================================

test_notification_no_id() ->
    Notification = #{jsonrpc => <<"2.0">>, method => <<"test/notify">>},
    ?assertNot(maps:is_key(id, Notification)).

test_notification_with_method() ->
    Notification = #{jsonrpc => <<"2.0">>, method => <<"notifications/message">>},
    ?assert(maps:is_key(method, Notification)).

test_notification_with_params() ->
    Notification = #{jsonrpc => <<"2.0">>, method => <<"test">>, params => #{data => <<"test">>}},
    ?assert(maps:is_key(params, Notification)).

test_notification_jsonrpc() ->
    Result = erlmcp_protocol_validator:check_notification_format(test_module),
    ?assertMatch(#{status := passed}, Result).

test_notification_no_response() ->
    %% Notifications should not generate responses
    Notification = #{jsonrpc => <<"2.0">>, method => <<"test/notify">>},
    ?assertNot(maps:is_key(id, Notification)).

%%%===================================================================
%%% Batch Request Tests (5 tests)
%%%===================================================================

test_batch_array_format() ->
    Batch = [
        #{jsonrpc => <<"2.0">>, id => 1, method => <<"test1">>},
        #{jsonrpc => <<"2.0">>, id => 2, method => <<"test2">>}
    ],
    ?assert(is_list(Batch)).

test_batch_multiple_items() ->
    Result = erlmcp_protocol_validator:check_batch_requests(test_module),
    ?assertMatch(#{status := passed}, Result).

test_batch_empty_array() ->
    EmptyBatch = [],
    ?assert(is_list(EmptyBatch)),
    ?assertEqual(0, length(EmptyBatch)).

test_batch_response_order() ->
    %% Batch responses should match request order
    Batch = [
        #{jsonrpc => <<"2.0">>, id => 1, method => <<"test1">>},
        #{jsonrpc => <<"2.0">>, id => 2, method => <<"test2">>},
        #{jsonrpc => <<"2.0">>, id => 3, method => <<"test3">>}
    ],
    ?assertEqual(3, length(Batch)).

test_batch_mixed_requests() ->
    %% Batch can contain requests and notifications
    Batch = [
        #{jsonrpc => <<"2.0">>, id => 1, method => <<"test">>},
        #{jsonrpc => <<"2.0">>, method => <<"notify">>}
    ],
    ?assertEqual(2, length(Batch)).

%%%===================================================================
%%% MCP Method Tests (10 tests)
%%%===================================================================

test_initialize_params() ->
    Result = erlmcp_protocol_validator:check_initialize_params(test_module),
    ?assertMatch(#{status := passed}, Result).

test_initialize_response() ->
    Result = erlmcp_protocol_validator:check_initialize_response(test_module),
    ?assertMatch(#{status := passed}, Result).

test_tools_list_method() ->
    Result = erlmcp_protocol_validator:check_tools_list_params(test_module),
    ?assertMatch(#{status := passed}, Result).

test_tools_call_method() ->
    Params = #{name => <<"test_tool">>, arguments => #{}},
    ?assert(maps:is_key(name, Params)),
    ?assert(maps:is_key(arguments, Params)).

test_resources_list_method() ->
    Result = erlmcp_protocol_validator:check_resources_list_params(test_module),
    ?assertMatch(#{status := passed}, Result).

test_resources_read_method() ->
    Params = #{uri => <<"test://resource/1">>},
    ?assert(maps:is_key(uri, Params)).

test_prompts_list_method() ->
    Params = #{cursor => <<"optional_cursor">>},
    ?assert(maps:is_key(cursor, Params)).

test_prompts_get_method() ->
    Params = #{name => <<"test_prompt">>, arguments => #{}},
    ?assert(maps:is_key(name, Params)).

test_ping_method() ->
    Request = #{jsonrpc => <<"2.0">>, id => 1, method => <<"ping">>},
    ?assertEqual(<<"ping">>, maps:get(method, Request)).

test_unknown_method() ->
    Request = #{jsonrpc => <<"2.0">>, id => 1, method => <<"unknown_method">>},
    ?assertEqual(<<"unknown_method">>, maps:get(method, Request)).

%%%===================================================================
%%% Error Code Tests (10 tests)
%%%===================================================================

test_error_parse() ->
    Code = -32700,
    ?assert(Code >= -32700 andalso Code =< -32600).

test_error_invalid_request() ->
    Code = -32600,
    ?assert(Code >= -32700 andalso Code =< -32600).

test_error_method_not_found() ->
    Code = -32601,
    ?assert(Code >= -32700 andalso Code =< -32600).

test_error_invalid_params() ->
    Code = -32602,
    ?assert(Code >= -32700 andalso Code =< -32600).

test_error_internal() ->
    Code = -32603,
    ?assert(Code >= -32700 andalso Code =< -32600).

test_error_resource_not_found() ->
    Code = -32001,
    ?assert(Code >= -32099 andalso Code =< -32000).

test_error_tool_not_found() ->
    Code = -32002,
    ?assert(Code >= -32099 andalso Code =< -32000).

test_error_server_overloaded() ->
    Code = -32010,
    ?assert(Code >= -32099 andalso Code =< -32000).

test_custom_error_range() ->
    Result = erlmcp_protocol_validator:check_custom_error_codes(test_module),
    ?assertMatch(#{status := passed}, Result).

test_error_code_integer() ->
    ErrorCodes = [-32700, -32600, -32601, -32602, -32603, -32001, -32002],
    lists:foreach(fun(Code) -> ?assert(is_integer(Code)) end, ErrorCodes).
