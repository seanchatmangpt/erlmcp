%%%-------------------------------------------------------------------
%%% @doc
%%% Protocol Validator Test Suite
%%%
%%% 50+ test cases covering JSON-RPC 2.0 and MCP protocol validation.
%%% Chicago School TDD: Real JSON messages, state-based verification, NO mocks.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_protocol_validator_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

all() ->
    [%% JSON-RPC 2.0 validation tests (10 tests)
     test_valid_request,
     test_valid_response_with_result, test_valid_response_with_error, test_valid_notification,
     test_invalid_json, test_missing_jsonrpc_field, test_wrong_jsonrpc_version,
     test_invalid_request_id_negative, test_invalid_request_id_float,
     test_result_and_error_exclusive,
     %% Request ID validation tests (5 tests)
     test_request_id_null,
     test_request_id_integer, test_request_id_string, test_request_id_invalid_type,
     test_request_id_negative_integer,
     %% Error code validation tests (8 tests)
     test_error_code_parse_error,
     test_error_code_invalid_request, test_error_code_method_not_found,
     test_error_code_invalid_params, test_error_code_internal_error,
     test_error_code_mcp_resource_not_found, test_error_code_experimental_range,
     test_error_code_invalid,
     %% Initialize method tests (5 tests)
     test_initialize_valid_params,
     test_initialize_missing_protocol_version, test_initialize_missing_capabilities,
     test_initialize_missing_client_info, test_initialize_invalid_client_info,
     %% Resources methods tests (8 tests)
     test_resources_list_no_params,
     test_resources_list_with_cursor, test_resources_list_invalid_cursor, test_resources_read_valid,
     test_resources_read_missing_uri, test_resources_subscribe_valid,
     test_resources_unsubscribe_valid, test_resources_unsubscribe_missing_uri,
     %% Tools methods tests (6 tests)
     test_tools_list_no_params,
     test_tools_list_with_cursor, test_tools_call_valid, test_tools_call_missing_name,
     test_tools_call_with_arguments, test_tools_call_invalid_arguments,
     %% Prompts methods tests (5 tests)
     test_prompts_list_no_params,
     test_prompts_get_valid, test_prompts_get_missing_name, test_prompts_get_with_arguments,
     test_prompts_get_invalid_arguments,
     %% Sampling method tests (3 tests)
     test_sampling_create_message_valid,
     test_sampling_create_message_missing_messages, test_sampling_create_message_invalid_messages,
     %% Logging method tests (3 tests)
     test_logging_set_level_valid,
     test_logging_set_level_invalid, test_logging_set_level_missing,
     %% Ping method test (1 test)
     test_ping_method,
     %% MCP message validation tests (5 tests)
     test_mcp_message_unknown_method,
     test_mcp_message_response_valid, test_mcp_message_error_response_valid,
     test_mcp_message_error_invalid_code, test_mcp_message_missing_fields,
     %% Additional validation tests (5 tests)
     test_validate_capabilities,
     test_validate_resource, test_validate_tool, test_validate_prompt, test_validate_content,
     %% Edge cases (3 tests)
     test_empty_params,
     test_null_values, test_large_request_id].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp_validation),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp_validation),
    ok.

%%%===================================================================
%%% JSON-RPC 2.0 Validation Tests
%%%===================================================================

test_valid_request(_Config) ->
    Json =
        jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                     <<"id">> => 1,
                     <<"method">> => <<"test">>,
                     <<"params">> => #{}}),
    {ok, #json_rpc_request{id = 1, method = <<"test">>}} =
        erlmcp_protocol_validator:validate_json_rpc(Json),
    ok.

test_valid_response_with_result(_Config) ->
    Json =
        jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                     <<"id">> => 1,
                     <<"result">> => #{<<"status">> => <<"ok">>}}),
    {ok, #json_rpc_response{id = 1, result = Result}} =
        erlmcp_protocol_validator:validate_json_rpc(Json),
    ?assert(is_map(Result)),
    ok.

test_valid_response_with_error(_Config) ->
    Json =
        jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                     <<"id">> => 1,
                     <<"error">> =>
                         #{<<"code">> => -32600, <<"message">> => <<"Invalid Request">>}}),
    {ok, #json_rpc_response{id = 1, error = Error}} =
        erlmcp_protocol_validator:validate_json_rpc(Json),
    ?assert(is_map(Error)),
    ok.

test_valid_notification(_Config) ->
    Json =
        jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                     <<"method">> => <<"notification">>,
                     <<"params">> => #{}}),
    {ok, #json_rpc_notification{method = <<"notification">>}} =
        erlmcp_protocol_validator:validate_json_rpc(Json),
    ok.

test_invalid_json(_Config) ->
    Json = <<"{invalid json}">>,
    {error, {parse_error, invalid_json}} = erlmcp_protocol_validator:validate_json_rpc(Json),
    ok.

test_missing_jsonrpc_field(_Config) ->
    Json = jsx:encode(#{<<"id">> => 1, <<"method">> => <<"test">>}),
    {error, {invalid_request, missing_jsonrpc}} = erlmcp_protocol_validator:validate_json_rpc(Json),
    ok.

test_wrong_jsonrpc_version(_Config) ->
    Json =
        jsx:encode(#{<<"jsonrpc">> => <<"1.0">>,
                     <<"id">> => 1,
                     <<"method">> => <<"test">>}),
    {error, {invalid_request, {wrong_version, <<"1.0">>}}} =
        erlmcp_protocol_validator:validate_json_rpc(Json),
    ok.

test_invalid_request_id_negative(_Config) ->
    %% Negative integers not allowed as request IDs
    Json =
        jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                     <<"id">> => -1,
                     <<"method">> => <<"test">>}),
    {error, {error, invalid_request_id}} = erlmcp_protocol_validator:validate_json_rpc(Json),
    ok.

test_invalid_request_id_float(_Config) ->
    %% Floats not allowed as request IDs
    {error, invalid_request_id} = erlmcp_protocol_validator:validate_request_id(1.5),
    ok.

test_result_and_error_exclusive(_Config) ->
    %% Response cannot have both result and error
    Json =
        jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                     <<"id">> => 1,
                     <<"result">> => #{},
                     <<"error">> => #{<<"code">> => -32600, <<"message">> => <<"Error">>}}),
    {error, {invalid_request, result_and_error_exclusive}} =
        erlmcp_protocol_validator:validate_json_rpc(Json),
    ok.

%%%===================================================================
%%% Request ID Validation Tests
%%%===================================================================

test_request_id_null(_Config) ->
    ok = erlmcp_protocol_validator:validate_request_id(null),
    ok.

test_request_id_integer(_Config) ->
    ok = erlmcp_protocol_validator:validate_request_id(123),
    ok = erlmcp_protocol_validator:validate_request_id(0),
    ok.

test_request_id_string(_Config) ->
    ok = erlmcp_protocol_validator:validate_request_id(<<"test-id">>),
    ok = erlmcp_protocol_validator:validate_request_id(<<"uuid-12345">>),
    ok.

test_request_id_invalid_type(_Config) ->
    {error, invalid_request_id} = erlmcp_protocol_validator:validate_request_id([1, 2, 3]),
    {error, invalid_request_id} = erlmcp_protocol_validator:validate_request_id(#{id => 1}),
    ok.

test_request_id_negative_integer(_Config) ->
    {error, invalid_request_id} = erlmcp_protocol_validator:validate_request_id(-1),
    ok.

%%%===================================================================
%%% Error Code Validation Tests
%%%===================================================================

test_error_code_parse_error(_Config) ->
    ok = erlmcp_protocol_validator:validate_error_code(?JSONRPC_PARSE_ERROR),
    ok.

test_error_code_invalid_request(_Config) ->
    ok = erlmcp_protocol_validator:validate_error_code(?JSONRPC_INVALID_REQUEST),
    ok.

test_error_code_method_not_found(_Config) ->
    ok = erlmcp_protocol_validator:validate_error_code(?JSONRPC_METHOD_NOT_FOUND),
    ok.

test_error_code_invalid_params(_Config) ->
    ok = erlmcp_protocol_validator:validate_error_code(?JSONRPC_INVALID_PARAMS),
    ok.

test_error_code_internal_error(_Config) ->
    ok = erlmcp_protocol_validator:validate_error_code(?JSONRPC_INTERNAL_ERROR),
    ok.

test_error_code_mcp_resource_not_found(_Config) ->
    ok = erlmcp_protocol_validator:validate_error_code(?MCP_ERROR_RESOURCE_NOT_FOUND),
    ok.

test_error_code_experimental_range(_Config) ->
    %% Experimental error codes (1090-1099)
    ok = erlmcp_protocol_validator:validate_error_code(1090),
    ok = erlmcp_protocol_validator:validate_error_code(1099),
    ok.

test_error_code_invalid(_Config) ->
    {error, invalid_error_code} = erlmcp_protocol_validator:validate_error_code(999),
    {error, invalid_error_code} = erlmcp_protocol_validator:validate_error_code(-99999),
    ok.

%%%===================================================================
%%% Initialize Method Tests
%%%===================================================================

test_initialize_valid_params(_Config) ->
    Params =
        #{?MCP_FIELD_PROTOCOL_VERSION => <<"2025-11-25">>,
          ?MCP_FIELD_CAPABILITIES => #{<<"tools">> => #{}, <<"resources">> => #{}},
          ?MCP_FIELD_CLIENT_INFO =>
              #{?MCP_INFO_NAME => <<"test_client">>, ?MCP_INFO_VERSION => <<"1.0.0">>}},
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_INITIALIZE, Params),
    ok.

test_initialize_missing_protocol_version(_Config) ->
    Params =
        #{?MCP_FIELD_CAPABILITIES => #{},
          ?MCP_FIELD_CLIENT_INFO =>
              #{?MCP_INFO_NAME => <<"test">>, ?MCP_INFO_VERSION => <<"1.0">>}},
    {error, _} = erlmcp_protocol_validator:validate_params(?MCP_METHOD_INITIALIZE, Params),
    ok.

test_initialize_missing_capabilities(_Config) ->
    Params =
        #{?MCP_FIELD_PROTOCOL_VERSION => <<"2025-11-25">>,
          ?MCP_FIELD_CLIENT_INFO =>
              #{?MCP_INFO_NAME => <<"test">>, ?MCP_INFO_VERSION => <<"1.0">>}},
    {error, _} = erlmcp_protocol_validator:validate_params(?MCP_METHOD_INITIALIZE, Params),
    ok.

test_initialize_missing_client_info(_Config) ->
    Params = #{?MCP_FIELD_PROTOCOL_VERSION => <<"2025-11-25">>, ?MCP_FIELD_CAPABILITIES => #{}},
    {error, _} = erlmcp_protocol_validator:validate_params(?MCP_METHOD_INITIALIZE, Params),
    ok.

test_initialize_invalid_client_info(_Config) ->
    Params =
        #{?MCP_FIELD_PROTOCOL_VERSION => <<"2025-11-25">>,
          ?MCP_FIELD_CAPABILITIES => #{},
          ?MCP_FIELD_CLIENT_INFO => #{?MCP_INFO_NAME => <<"test">>}},
    %% Missing version
    {error, _} = erlmcp_protocol_validator:validate_params(?MCP_METHOD_INITIALIZE, Params),
    ok.

%%%===================================================================
%%% Resources Methods Tests
%%%===================================================================

test_resources_list_no_params(_Config) ->
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_RESOURCES_LIST, undefined),
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_RESOURCES_LIST, #{}),
    ok.

test_resources_list_with_cursor(_Config) ->
    Params = #{?MCP_PARAM_CURSOR => <<"cursor123">>},
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_RESOURCES_LIST, Params),
    ok.

test_resources_list_invalid_cursor(_Config) ->
    Params = #{?MCP_PARAM_CURSOR => 123},  % Should be string
    {error, _} = erlmcp_protocol_validator:validate_params(?MCP_METHOD_RESOURCES_LIST, Params),
    ok.

test_resources_read_valid(_Config) ->
    Params = #{?MCP_PARAM_URI => <<"file:///test.txt">>},
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_RESOURCES_READ, Params),
    ok.

test_resources_read_missing_uri(_Config) ->
    Params = #{},
    {error, <<"Missing required parameter: uri">>} =
        erlmcp_protocol_validator:validate_params(?MCP_METHOD_RESOURCES_READ, Params),
    ok.

test_resources_subscribe_valid(_Config) ->
    Params = #{?MCP_PARAM_URI => <<"weather://city">>},
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_RESOURCES_SUBSCRIBE, Params),
    ok.

test_resources_unsubscribe_valid(_Config) ->
    Params = #{?MCP_PARAM_URI => <<"weather://city">>},
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_RESOURCES_UNSUBSCRIBE, Params),
    ok.

test_resources_unsubscribe_missing_uri(_Config) ->
    Params = #{},
    {error, <<"Missing required parameter: uri">>} =
        erlmcp_protocol_validator:validate_params(?MCP_METHOD_RESOURCES_UNSUBSCRIBE, Params),
    ok.

%%%===================================================================
%%% Tools Methods Tests
%%%===================================================================

test_tools_list_no_params(_Config) ->
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_TOOLS_LIST, undefined),
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_TOOLS_LIST, #{}),
    ok.

test_tools_list_with_cursor(_Config) ->
    Params = #{?MCP_PARAM_CURSOR => <<"cursor123">>},
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_TOOLS_LIST, Params),
    ok.

test_tools_call_valid(_Config) ->
    Params =
        #{?MCP_PARAM_NAME => <<"test_tool">>,
          ?MCP_PARAM_ARGUMENTS => #{<<"arg1">> => <<"value1">>}},
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_TOOLS_CALL, Params),
    ok.

test_tools_call_missing_name(_Config) ->
    Params = #{?MCP_PARAM_ARGUMENTS => #{}},
    {error, <<"Missing required parameter: name">>} =
        erlmcp_protocol_validator:validate_params(?MCP_METHOD_TOOLS_CALL, Params),
    ok.

test_tools_call_with_arguments(_Config) ->
    Params =
        #{?MCP_PARAM_NAME => <<"test_tool">>, ?MCP_PARAM_ARGUMENTS => #{<<"key">> => <<"value">>}},
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_TOOLS_CALL, Params),
    ok.

test_tools_call_invalid_arguments(_Config) ->
    Params =
        #{?MCP_PARAM_NAME => <<"test_tool">>,
          ?MCP_PARAM_ARGUMENTS => <<"invalid">>},  % Should be object
    {error, <<"arguments must be an object">>} =
        erlmcp_protocol_validator:validate_params(?MCP_METHOD_TOOLS_CALL, Params),
    ok.

%%%===================================================================
%%% Prompts Methods Tests
%%%===================================================================

test_prompts_list_no_params(_Config) ->
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_PROMPTS_LIST, undefined),
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_PROMPTS_LIST, #{}),
    ok.

test_prompts_get_valid(_Config) ->
    Params =
        #{?MCP_PARAM_NAME => <<"test_prompt">>,
          ?MCP_PARAM_ARGUMENTS => #{<<"arg">> => <<"value">>}},
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_PROMPTS_GET, Params),
    ok.

test_prompts_get_missing_name(_Config) ->
    Params = #{},
    {error, <<"Missing required parameter: name">>} =
        erlmcp_protocol_validator:validate_params(?MCP_METHOD_PROMPTS_GET, Params),
    ok.

test_prompts_get_with_arguments(_Config) ->
    Params =
        #{?MCP_PARAM_NAME => <<"test_prompt">>,
          ?MCP_PARAM_ARGUMENTS => #{<<"language">> => <<"erlang">>}},
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_PROMPTS_GET, Params),
    ok.

test_prompts_get_invalid_arguments(_Config) ->
    Params =
        #{?MCP_PARAM_NAME => <<"test_prompt">>,
          ?MCP_PARAM_ARGUMENTS => [<<"invalid">>]},  % Should be object
    {error, <<"arguments must be an object">>} =
        erlmcp_protocol_validator:validate_params(?MCP_METHOD_PROMPTS_GET, Params),
    ok.

%%%===================================================================
%%% Sampling Method Tests
%%%===================================================================

test_sampling_create_message_valid(_Config) ->
    Params = #{?MCP_PARAM_MESSAGES => [#{<<"role">> => <<"user">>, <<"content">> => <<"test">>}]},
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_SAMPLING_CREATE_MESSAGE, Params),
    ok.

test_sampling_create_message_missing_messages(_Config) ->
    Params = #{},
    {error, _} =
        erlmcp_protocol_validator:validate_params(?MCP_METHOD_SAMPLING_CREATE_MESSAGE, Params),
    ok.

test_sampling_create_message_invalid_messages(_Config) ->
    Params = #{?MCP_PARAM_MESSAGES => <<"invalid">>},  % Should be array
    {error, <<"messages must be an array">>} =
        erlmcp_protocol_validator:validate_params(?MCP_METHOD_SAMPLING_CREATE_MESSAGE, Params),
    ok.

%%%===================================================================
%%% Logging Method Tests
%%%===================================================================

test_logging_set_level_valid(_Config) ->
    Params = #{?MCP_PARAM_LEVEL => debug},
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_LOGGING_SET_LEVEL, Params),
    ok.

test_logging_set_level_invalid(_Config) ->
    Params = #{?MCP_PARAM_LEVEL => invalid_level},
    {error, <<"Invalid log level">>} =
        erlmcp_protocol_validator:validate_params(?MCP_METHOD_LOGGING_SET_LEVEL, Params),
    ok.

test_logging_set_level_missing(_Config) ->
    Params = #{},
    {error, <<"Missing required parameter: level">>} =
        erlmcp_protocol_validator:validate_params(?MCP_METHOD_LOGGING_SET_LEVEL, Params),
    ok.

%%%===================================================================
%%% Ping Method Test
%%%===================================================================

test_ping_method(_Config) ->
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_PING, undefined),
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_PING, #{}),
    ok.

%%%===================================================================
%%% MCP Message Validation Tests
%%%===================================================================

test_mcp_message_unknown_method(_Config) ->
    Message = #{?JSONRPC_FIELD_METHOD => <<"unknown_method">>, ?JSONRPC_FIELD_PARAMS => #{}},
    {error, ?JSONRPC_METHOD_NOT_FOUND, _} = erlmcp_protocol_validator:validate_mcp_message(Message),
    ok.

test_mcp_message_response_valid(_Config) ->
    Message = #{?JSONRPC_FIELD_RESULT => #{<<"status">> => <<"ok">>}},
    {ok, _} = erlmcp_protocol_validator:validate_mcp_message(Message),
    ok.

test_mcp_message_error_response_valid(_Config) ->
    Message =
        #{?JSONRPC_FIELD_ERROR =>
              #{?JSONRPC_ERROR_FIELD_CODE => -32600,
                ?JSONRPC_ERROR_FIELD_MESSAGE => <<"Invalid Request">>}},
    {ok, _} = erlmcp_protocol_validator:validate_mcp_message(Message),
    ok.

test_mcp_message_error_invalid_code(_Config) ->
    Message =
        #{?JSONRPC_FIELD_ERROR =>
              #{?JSONRPC_ERROR_FIELD_CODE => 999,  % Invalid code
                ?JSONRPC_ERROR_FIELD_MESSAGE => <<"Error">>}},
    {error, ?JSONRPC_INVALID_REQUEST, _} = erlmcp_protocol_validator:validate_mcp_message(Message),
    ok.

test_mcp_message_missing_fields(_Config) ->
    Message = #{},
    {error, ?JSONRPC_INVALID_REQUEST, _} = erlmcp_protocol_validator:validate_mcp_message(Message),
    ok.

%%%===================================================================
%%% Additional Validation Tests
%%%===================================================================

test_validate_capabilities(_Config) ->
    %% Valid capabilities
    ok =
        erlmcp_protocol_validator:validate_capabilities(#{<<"tools">> => #{},
                                                          <<"resources">> =>
                                                              #{<<"subscribe">> => true}}),
    %% Invalid capabilities (not a map)
    {error, _} = erlmcp_protocol_validator:validate_capabilities(<<"invalid">>),
    ok.

test_validate_resource(_Config) ->
    %% Valid resource
    ok =
        erlmcp_protocol_validator:validate_resource(#{?MCP_PARAM_URI => <<"file:///test.txt">>,
                                                      ?MCP_PARAM_NAME => <<"Test Resource">>}),
    %% Missing name
    {error, _} =
        erlmcp_protocol_validator:validate_resource(#{?MCP_PARAM_URI => <<"file:///test.txt">>}),
    ok.

test_validate_tool(_Config) ->
    %% Valid tool
    ok =
        erlmcp_protocol_validator:validate_tool(#{?MCP_PARAM_NAME => <<"test_tool">>,
                                                  ?MCP_PARAM_INPUT_SCHEMA =>
                                                      #{<<"type">> => <<"object">>,
                                                        <<"properties">> => #{}}}),
    %% Missing inputSchema
    {error, _} = erlmcp_protocol_validator:validate_tool(#{?MCP_PARAM_NAME => <<"test_tool">>}),
    ok.

test_validate_prompt(_Config) ->
    %% Valid prompt
    ok = erlmcp_protocol_validator:validate_prompt(#{?MCP_PARAM_NAME => <<"test_prompt">>}),
    %% Missing name
    {error, _} = erlmcp_protocol_validator:validate_prompt(#{?MCP_PARAM_DESCRIPTION => <<"Test">>}),
    ok.

test_validate_content(_Config) ->
    %% Valid text content
    ok =
        erlmcp_protocol_validator:validate_content(#{?MCP_PARAM_TYPE => ?MCP_CONTENT_TYPE_TEXT,
                                                     ?MCP_PARAM_TEXT => <<"Hello">>}),
    %% Valid image content
    ok =
        erlmcp_protocol_validator:validate_content(#{?MCP_PARAM_TYPE => ?MCP_CONTENT_TYPE_IMAGE,
                                                     ?MCP_PARAM_DATA => <<"base64data">>,
                                                     ?MCP_PARAM_MIME_TYPE => <<"image/png">>}),
    %% Missing type
    {error, <<"Missing required field: content.type">>} =
        erlmcp_protocol_validator:validate_content(#{?MCP_PARAM_TEXT => <<"Hello">>}),
    ok.

%%%===================================================================
%%% Edge Cases Tests
%%%===================================================================

test_empty_params(_Config) ->
    %% Empty params should be valid for methods that don't require params
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_RESOURCES_LIST, #{}),
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_TOOLS_LIST, #{}),
    ok.

test_null_values(_Config) ->
    %% Null request ID is valid
    ok = erlmcp_protocol_validator:validate_request_id(null),
    %% Null params equivalent to undefined
    ok = erlmcp_protocol_validator:validate_params(?MCP_METHOD_PING, undefined),
    ok.

test_large_request_id(_Config) ->
    %% Large integer IDs should be valid
    LargeId = 999999999999,
    ok = erlmcp_protocol_validator:validate_request_id(LargeId),
    %% Large string IDs should be valid
    LargeStringId = <<"very-long-uuid-12345678-1234-1234-1234-123456789012">>,
    ok = erlmcp_protocol_validator:validate_request_id(LargeStringId),
    ok.
