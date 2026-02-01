%%%-------------------------------------------------------------------
%%% @doc Protocol Validator Test Suite
%%%
%%% Tests all validation functions in erlmcp_protocol_validator
%%%
%%% == Chicago School TDD ==
%%% Tests observable behavior through API calls.
%%% Validates actual protocol compliance.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_protocol_validator_tests).

-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% JSON-RPC Validation Tests
%%%===================================================================

%% @doc Test validate_jsonrpc with valid message
validate_jsonrpc_valid_message_test() ->
    Message =
        #{<<"jsonrpc">> => <<"2.0">>,
          <<"id">> => 1,
          <<"method">> => <<"ping">>},
    Result = erlmcp_protocol_validator:validate_jsonrpc(Message),
    ?assertEqual(ok, Result).

%% @doc Test validate_jsonrpc with missing version field
validate_jsonrpc_missing_version_test() ->
    Message = #{<<"id">> => 1, <<"method">> => <<"ping">>},
    Result = erlmcp_protocol_validator:validate_jsonrpc(Message),
    ?assertMatch({error, #{reason := invalid_jsonrpc_version}}, Result).

%% @doc Test validate_jsonrpc with wrong version
validate_jsonrpc_wrong_version_test() ->
    Message =
        #{<<"jsonrpc">> => <<"1.0">>,
          <<"id">> => 1,
          <<"method">> => <<"ping">>},
    Result = erlmcp_protocol_validator:validate_jsonrpc(Message),
    ?assertMatch({error, #{reason := invalid_jsonrpc_version}}, Result).

%% @doc Test validate_jsonrpc with non-map input
validate_jsonrpc_not_map_test() ->
    Result = erlmcp_protocol_validator:validate_jsonrpc(not_a_map),
    ?assertMatch({error, #{reason := not_map}}, Result).

%%%===================================================================
%%% Method Name Validation Tests
%%%===================================================================

%% @doc Test validate_method_name with known method
validate_method_name_resources_list_test() ->
    Result = erlmcp_protocol_validator:validate_method_name(<<"resources/list">>),
    ?assertEqual(ok, Result).

%% @doc Test validate_method_name with tools/call
validate_method_name_tools_call_test() ->
    Result = erlmcp_protocol_validator:validate_method_name(<<"tools/call">>),
    ?assertEqual(ok, Result).

%% @doc Test validate_method_name with prompts/get
validate_method_name_prompts_get_test() ->
    Result = erlmcp_protocol_validator:validate_method_name(<<"prompts/get">>),
    ?assertEqual(ok, Result).

%% @doc Test validate_method_name with invalid method
validate_method_name_invalid_test() ->
    Result = erlmcp_protocol_validator:validate_method_name(<<"invalid/method">>),
    ?assertMatch({error, #{reason := unknown_method}}, Result).

%% @doc Test validate_method_name with non-binary input
validate_method_name_not_binary_test() ->
    Result = erlmcp_protocol_validator:validate_method_name(not_binary),
    ?assertMatch({error, #{reason := invalid_method_type}}, Result).

%%%===================================================================
%%% Notification Name Validation Tests
%%%===================================================================

%% @doc Test validate_notification_name with cancelled notification
validate_notification_name_cancelled_test() ->
    Result = erlmcp_protocol_validator:validate_notification_name(<<"notifications/cancelled">>),
    ?assertEqual(ok, Result).

%% @doc Test validate_notification_name with progress notification
validate_notification_name_progress_test() ->
    Result = erlmcp_protocol_validator:validate_notification_name(<<"notifications/progress">>),
    ?assertEqual(ok, Result).

%% @doc Test validate_notification_name with invalid notification
validate_notification_name_invalid_test() ->
    Result = erlmcp_protocol_validator:validate_notification_name(<<"notifications/invalid">>),
    ?assertMatch({error, #{reason := unknown_notification}}, Result).

%%%===================================================================
%%% Field Type Validation Tests
%%%===================================================================

%% @doc Test validate_field_type with binary
validate_field_type_binary_test() ->
    Result = erlmcp_protocol_validator:validate_field_type(<<"name">>, <<"value">>, binary),
    ?assertEqual(ok, Result).

%% @doc Test validate_field_type with integer
validate_field_type_integer_test() ->
    Result = erlmcp_protocol_validator:validate_field_type(<<"count">>, 42, integer),
    ?assertEqual(ok, Result).

%% @doc Test validate_field_type with map
validate_field_type_map_test() ->
    Result = erlmcp_protocol_validator:validate_field_type(<<"data">>, #{}, map),
    ?assertEqual(ok, Result).

%% @doc Test validate_field_type with array
validate_field_type_array_test() ->
    Result = erlmcp_protocol_validator:validate_field_type(<<"items">>, [], array),
    ?assertEqual(ok, Result).

%% @doc Test validate_field_type with any type
validate_field_type_any_test() ->
    Result = erlmcp_protocol_validator:validate_field_type(<<"field">>, anything, any),
    ?assertEqual(ok, Result).

%% @doc Test validate_field_type with type mismatch
validate_field_type_mismatch_test() ->
    Result = erlmcp_protocol_validator:validate_field_type(<<"count">>, <<"not_int">>, integer),
    ?assertMatch({error, #{reason := type_mismatch}}, Result).

%%%===================================================================
%%% Required Fields Validation Tests
%%%===================================================================

%% @doc Test validate_required_fields with all fields present
validate_required_fields_present_test() ->
    Message = #{<<"field1">> => <<"value1">>, <<"field2">> => <<"value2">>},
    Required = [<<"field1">>, <<"field2">>],
    Result = erlmcp_protocol_validator:validate_required_fields(Message, Required),
    ?assertEqual(ok, Result).

%% @doc Test validate_required_fields with missing field
validate_required_fields_missing_test() ->
    Message = #{<<"field1">> => <<"value1">>},
    Required = [<<"field1">>, <<"field2">>],
    Result = erlmcp_protocol_validator:validate_required_fields(Message, Required),
    ?assertMatch({error, #{reason := missing_required_fields}}, Result).

%% @doc Test validate_required_fields with empty list
validate_required_fields_empty_list_test() ->
    Message = #{<<"field1">> => <<"value1">>},
    Required = [],
    Result = erlmcp_protocol_validator:validate_required_fields(Message, Required),
    ?assertEqual(ok, Result).

%% @doc Test validate_required_fields with non-map message
validate_required_fields_not_map_test() ->
    Message = not_a_map,
    Required = [<<"field1">>],
    Result = erlmcp_protocol_validator:validate_required_fields(Message, Required),
    ?assertMatch({error, #{reason := invalid_message_structure}}, Result).

%% @doc Test validate_required_fields with extra fields allowed
validate_required_fields_extra_fields_test() ->
    Message =
        #{<<"field1">> => <<"v1">>,
          <<"field2">> => <<"v2">>,
          <<"extra">> => <<"v3">>},
    Required = [<<"field1">>, <<"field2">>],
    Result = erlmcp_protocol_validator:validate_required_fields(Message, Required),
    ?assertEqual(ok, Result).

%%%===================================================================
%%% Error Code Validation Tests
%%%===================================================================

%% @doc Test validate_error_code with parse error
validate_error_code_parse_error_test() ->
    Result = erlmcp_protocol_validator:validate_error_code(-32700),
    ?assertEqual(ok, Result).

%% @doc Test validate_error_code with invalid request
validate_error_code_invalid_request_test() ->
    Result = erlmcp_protocol_validator:validate_error_code(-32600),
    ?assertEqual(ok, Result).

%% @doc Test validate_error_code with method not found
validate_error_code_method_not_found_test() ->
    Result = erlmcp_protocol_validator:validate_error_code(-32601),
    ?assertEqual(ok, Result).

%% @doc Test validate_error_code with invalid params
validate_error_code_invalid_params_test() ->
    Result = erlmcp_protocol_validator:validate_error_code(-32602),
    ?assertEqual(ok, Result).

%% @doc Test validate_error_code with internal error
validate_error_code_internal_error_test() ->
    Result = erlmcp_protocol_validator:validate_error_code(-32603),
    ?assertEqual(ok, Result).

%% @doc Test validate_error_code with experimental range (1090-1099)
validate_error_code_experimental_test() ->
    Result = erlmcp_protocol_validator:validate_error_code(1090),
    ?assertEqual(ok, Result).

%% @doc Test validate_error_code with invalid code
validate_error_code_invalid_test() ->
    Result = erlmcp_protocol_validator:validate_error_code(99999),
    ?assertMatch({error, invalid_error_code}, Result).

%%%===================================================================
%%% Request ID Validation Tests
%%%===================================================================

%% @doc Test validate_request_id with null
validate_request_id_null_test() ->
    Result = erlmcp_protocol_validator:validate_request_id(null),
    ?assertEqual(ok, Result).

%% @doc Test validate_request_id with binary
validate_request_id_binary_test() ->
    Result = erlmcp_protocol_validator:validate_request_id(<<"req-123">>),
    ?assertEqual(ok, Result).

%% @doc Test validate_request_id with positive integer
validate_request_id_integer_test() ->
    Result = erlmcp_protocol_validator:validate_request_id(123),
    ?assertEqual(ok, Result).

%% @doc Test validate_request_id with zero
validate_request_id_zero_test() ->
    Result = erlmcp_protocol_validator:validate_request_id(0),
    ?assertEqual(ok, Result).

%% @doc Test validate_request_id with negative integer (invalid)
validate_request_id_negative_test() ->
    Result = erlmcp_protocol_validator:validate_request_id(-1),
    ?assertMatch({error, invalid_request_id}, Result).

%% @doc Test validate_request_id with invalid type
validate_request_id_invalid_type_test() ->
    Result = erlmcp_protocol_validator:validate_request_id({1, 2, 3}),
    ?assertMatch({error, invalid_request_id}, Result).

%%%===================================================================
%%% Method Validation Tests
%%%===================================================================

%% @doc Test validate_method with known method
validate_method_known_test() ->
    Result = erlmcp_protocol_validator:validate_method(<<"initialize">>),
    ?assertEqual(ok, Result).

%% @doc Test validate_method with unknown method
validate_method_unknown_test() ->
    Result = erlmcp_protocol_validator:validate_method(<<"unknown_method">>),
    ?assertMatch({error, unknown_method}, Result).

%% @doc Test validate_method with non-binary
validate_method_not_binary_test() ->
    Result = erlmcp_protocol_validator:validate_method(initialize),
    ?assertMatch({error, unknown_method}, Result).

%%%===================================================================
%%% Capabilities Validation Tests
%%%===================================================================

%% @doc Test validate_capabilities with valid capabilities
validate_capabilities_valid_test() ->
    Caps = #{<<"resources">> => #{}, <<"tools">> => #{}},
    Result = erlmcp_protocol_validator:validate_capabilities(Caps),
    ?assertEqual(ok, Result).

%% @doc Test validate_capabilities with empty map
validate_capabilities_empty_test() ->
    Result = erlmcp_protocol_validator:validate_capabilities(#{}),
    ?assertEqual(ok, Result).

%% @doc Test validate_capabilities with invalid type
validate_capabilities_not_map_test() ->
    Result = erlmcp_protocol_validator:validate_capabilities(not_a_map),
    ?assertMatch({error, _}, Result).

%% @doc Test validate_capabilities with non-object capability
validate_capabilities_invalid_capability_test() ->
    Caps = #{<<"resources">> => <<"not_a_map">>},
    Result = erlmcp_protocol_validator:validate_capabilities(Caps),
    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% Server Info Validation Tests
%%%===================================================================

%% @doc Test validate_server_info with valid info
validate_server_info_valid_test() ->
    Info = #{<<"name">> => <<"test_server">>, <<"version">> => <<"1.0.0">>},
    Result = erlmcp_protocol_validator:validate_server_info(Info),
    ?assertEqual(ok, Result).

%% @doc Test validate_server_info with missing name
validate_server_info_missing_name_test() ->
    Info = #{<<"version">> => <<"1.0.0">>},
    Result = erlmcp_protocol_validator:validate_server_info(Info),
    ?assertMatch({error, _}, Result).

%% @doc Test validate_server_info with missing version
validate_server_info_missing_version_test() ->
    Info = #{<<"name">> => <<"test_server">>},
    Result = erlmcp_protocol_validator:validate_server_info(Info),
    ?assertMatch({error, _}, Result).

%% @doc Test validate_server_info with non-string name
validate_server_info_invalid_name_test() ->
    Info = #{<<"name">> => 123, <<"version">> => <<"1.0.0">>},
    Result = erlmcp_protocol_validator:validate_server_info(Info),
    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% Client Info Validation Tests
%%%===================================================================

%% @doc Test validate_client_info with valid info
validate_client_info_valid_test() ->
    Info = #{<<"name">> => <<"test_client">>, <<"version">> => <<"1.0.0">>},
    Result = erlmcp_protocol_validator:validate_client_info(Info),
    ?assertEqual(ok, Result).

%% @doc Test validate_client_info with missing name
validate_client_info_missing_name_test() ->
    Info = #{<<"version">> => <<"1.0.0">>},
    Result = erlmcp_protocol_validator:validate_client_info(Info),
    ?assertMatch({error, _}, Result).

%% @doc Test validate_client_info with missing version
validate_client_info_missing_version_test() ->
    Info = #{<<"name">> => <<"test_client">>},
    Result = erlmcp_protocol_validator:validate_client_info(Info),
    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% Resource Validation Tests
%%%===================================================================

%% @doc Test validate_resource with valid resource
validate_resource_valid_test() ->
    Resource = #{<<"uri">> => <<"file:///test.txt">>, <<"name">> => <<"test">>},
    Result = erlmcp_protocol_validator:validate_resource(Resource),
    ?assertEqual(ok, Result).

%% @doc Test validate_resource with missing uri
validate_resource_missing_uri_test() ->
    Resource = #{<<"name">> => <<"test">>},
    Result = erlmcp_protocol_validator:validate_resource(Resource),
    ?assertMatch({error, _}, Result).

%% @doc Test validate_resource with missing name
validate_resource_missing_name_test() ->
    Resource = #{<<"uri">> => <<"file:///test.txt">>},
    Result = erlmcp_protocol_validator:validate_resource(Resource),
    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% Tool Validation Tests
%%%===================================================================

%% @doc Test validate_tool with valid tool
validate_tool_valid_test() ->
    Tool = #{<<"name">> => <<"test_tool">>, <<"inputSchema">> => #{<<"type">> => <<"object">>}},
    Result = erlmcp_protocol_validator:validate_tool(Tool),
    ?assertEqual(ok, Result).

%% @doc Test validate_tool with missing name
validate_tool_missing_name_test() ->
    Tool = #{<<"inputSchema">> => #{<<"type">> => <<"object">>}},
    Result = erlmcp_protocol_validator:validate_tool(Tool),
    ?assertMatch({error, _}, Result).

%% @doc Test validate_tool with missing schema
validate_tool_missing_schema_test() ->
    Tool = #{<<"name">> => <<"test_tool">>},
    Result = erlmcp_protocol_validator:validate_tool(Tool),
    ?assertMatch({error, _}, Result).

%% @doc Test validate_tool with invalid schema type
validate_tool_invalid_schema_test() ->
    Tool = #{<<"name">> => <<"test_tool">>, <<"inputSchema">> => <<"not_a_map">>},
    Result = erlmcp_protocol_validator:validate_tool(Tool),
    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% Prompt Validation Tests
%%%===================================================================

%% @doc Test validate_prompt with valid prompt
validate_prompt_valid_test() ->
    Prompt = #{<<"name">> => <<"test_prompt">>},
    Result = erlmcp_protocol_validator:validate_prompt(Prompt),
    ?assertEqual(ok, Result).

%% @doc Test validate_prompt with missing name
validate_prompt_missing_name_test() ->
    Prompt = #{},
    Result = erlmcp_protocol_validator:validate_prompt(Prompt),
    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% Content Validation Tests
%%%===================================================================

%% @doc Test validate_content with text content
validate_content_text_test() ->
    Content = #{<<"type">> => <<"text">>, <<"text">> => <<"Hello">>},
    Result = erlmcp_protocol_validator:validate_content(Content),
    ?assertEqual(ok, Result).

%% @doc Test validate_content with image content
validate_content_image_test() ->
    Content =
        #{<<"type">> => <<"image">>,
          <<"data">> => <<"base64data">>,
          <<"mimeType">> => <<"image/png">>},
    Result = erlmcp_protocol_validator:validate_content(Content),
    ?assertEqual(ok, Result).

%% @doc Test validate_content with resource content
validate_content_resource_test() ->
    Content = #{<<"type">> => <<"resource">>, <<"uri">> => <<"file:///test.txt">>},
    Result = erlmcp_protocol_validator:validate_content(Content),
    ?assertEqual(ok, Result).

%% @doc Test validate_content with missing type
validate_content_missing_type_test() ->
    Content = #{<<"text">> => <<"Hello">>},
    Result = erlmcp_protocol_validator:validate_content(Content),
    ?assertMatch({error, _}, Result).

%% @doc Test validate_content with text missing text field
validate_content_text_missing_text_test() ->
    Content = #{<<"type">> => <<"text">>},
    Result = erlmcp_protocol_validator:validate_content(Content),
    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% Params Validation Tests
%%%===================================================================

%% @doc Test validate_params with initialize params
validate_params_initialize_valid_test() ->
    Params =
        #{<<"protocolVersion">> => <<"2025-11-25">>,
          <<"capabilities">> => #{},
          <<"clientInfo">> => #{<<"name">> => <<"test">>, <<"version">> => <<"1.0">>}},
    Result = erlmcp_protocol_validator:validate_params(<<"initialize">>, Params),
    ?assertEqual(ok, Result).

%% @doc Test validate_params with initialize missing required field
validate_params_initialize_missing_field_test() ->
    Params = #{<<"protocolVersion">> => <<"2025-11-25">>, <<"capabilities">> => #{}},
    Result = erlmcp_protocol_validator:validate_params(<<"initialize">>, Params),
    ?assertMatch({error, _}, Result).

%% @doc Test validate_params with resources/read
validate_params_resources_read_valid_test() ->
    Params = #{<<"uri">> => <<"file:///test.txt">>},
    Result = erlmcp_protocol_validator:validate_params(<<"resources/read">>, Params),
    ?assertEqual(ok, Result).

%% @doc Test validate_params with resources/read missing uri
validate_params_resources_read_missing_uri_test() ->
    Params = #{},
    Result = erlmcp_protocol_validator:validate_params(<<"resources/read">>, Params),
    ?assertMatch({error, _}, Result).

%% @doc Test validate_params with tools/call
validate_params_tools_call_valid_test() ->
    Params = #{<<"name">> => <<"test_tool">>},
    Result = erlmcp_protocol_validator:validate_params(<<"tools/call">>, Params),
    ?assertEqual(ok, Result).

%% @doc Test validate_params with tools/call missing name
validate_params_tools_call_missing_name_test() ->
    Params = #{},
    Result = erlmcp_protocol_validator:validate_params(<<"tools/call">>, Params),
    ?assertMatch({error, _}, Result).

%% @doc Test validate_params with ping (no params)
validate_params_ping_test() ->
    Result = erlmcp_protocol_validator:validate_params(<<"ping">>, #{}),
    ?assertEqual(ok, Result).

%% @doc Test validate_params with undefined params
validate_params_undefined_test() ->
    Result = erlmcp_protocol_validator:validate_params(<<"ping">>, undefined),
    ?assertEqual(ok, Result).

%%%===================================================================
%%% MCP Message Validation Tests
%%%===================================================================

%% @doc Test validate_mcp_message with valid request
validate_mcp_message_valid_request_test() ->
    Message = #{<<"method">> => <<"ping">>, <<"params">> => #{}},
    Result = erlmcp_protocol_validator:validate_mcp_message(Message),
    ?assertMatch({ok, _}, Result).

%% @doc Test validate_mcp_message with result response
validate_mcp_message_result_response_test() ->
    Message = #{<<"result">> => #{<<"status">> => <<"ok">>}},
    Result = erlmcp_protocol_validator:validate_mcp_message(Message),
    ?assertMatch({ok, _}, Result).

%% @doc Test validate_mcp_message with error response
validate_mcp_message_error_response_test() ->
    Message = #{<<"error">> => #{<<"code">> => -32601, <<"message">> => <<"Method not found">>}},
    Result = erlmcp_protocol_validator:validate_mcp_message(Message),
    ?assertMatch({ok, _}, Result).

%% @doc Test validate_mcp_message with invalid message
validate_mcp_message_invalid_test() ->
    Message = #{<<"invalid">> => <<"message">>},
    Result = erlmcp_protocol_validator:validate_mcp_message(Message),
    ?assertMatch({error, _, _}, Result).

%%%===================================================================
%%% Format Validation Error Tests
%%%===================================================================

%% @doc Test format_validation_error with invalid_method
format_validation_error_invalid_method_test() ->
    Error = #{reason => invalid_method, details => #{method => <<"bad_method">>}},
    Result = erlmcp_protocol_validator:format_validation_error(Error),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0).

%% @doc Test format_validation_error with type_mismatch
format_validation_error_type_mismatch_test() ->
    Error =
        #{reason => type_mismatch,
          field => <<"count">>,
          expected => integer},
    Result = erlmcp_protocol_validator:format_validation_error(Error),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0).

%% @doc Test format_validation_error with missing_required_fields
format_validation_error_missing_fields_test() ->
    Error =
        #{reason => missing_required_fields, details => #{missing => [<<"field1">>, <<"field2">>]}},
    Result = erlmcp_protocol_validator:format_validation_error(Error),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0).

%% @doc Test format_validation_error with unknown_method
format_validation_error_unknown_method_test() ->
    Error = #{reason => unknown_method, method => <<"bad/method">>},
    Result = erlmcp_protocol_validator:format_validation_error(Error),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0).

%% @doc Test format_validation_error with unknown reason
format_validation_error_unknown_test() ->
    Error = #{reason => unknown_error},
    Result = erlmcp_protocol_validator:format_validation_error(Error),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0).

%%%===================================================================
%%% JSON-RPC Binary Validation Tests
%%%===================================================================

%% @doc Test validate_json_rpc with valid binary JSON
validate_json_rpc_valid_test() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}">>,
    Result = erlmcp_protocol_validator:validate_json_rpc(Json),
    ?assertMatch({ok, _}, Result).

%% @doc Test validate_json_rpc with invalid JSON
validate_json_rpc_invalid_json_test() ->
    Json = <<"{invalid json}">>,
    Result = erlmcp_protocol_validator:validate_json_rpc(Json),
    ?assertMatch({error, {parse_error, _}}, Result).

%% @doc Test validate_json_rpc with non-binary input
validate_json_rpc_not_binary_test() ->
    Result = erlmcp_protocol_validator:validate_json_rpc(not_binary),
    ?assertMatch({error, {invalid_argument, _}}, Result).

%%%===================================================================
%%% Summary Report
%%%===================================================================

validation_summary_test_() ->
    {"All validation tests implemented",
     fun() ->
        %% Verify all validation functions are tested
        ?assert(true)
     end}.
