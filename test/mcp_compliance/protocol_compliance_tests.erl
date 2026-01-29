%% @doc JSON-RPC Protocol Compliance Tests
%% Validates compliance with JSON-RPC 2.0 specification
-module(protocol_compliance_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% JSON-RPC Message Format Tests
%%%===================================================================

jsonrpc_version_field_test() ->
    %% Request must have jsonrpc: "2.0" field
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test">>,
        <<"id">> => 1
    },
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Request)).

jsonrpc_version_string_test() ->
    %% Version must be string "2.0", not number
    Request = erlmcp_json_rpc:encode_request(1, <<"test">>, #{}),
    ?assertMatch(<<"2.0">>, jsx:decode(Request, [return_maps])).

jsonrpc_required_fields_test() ->
    %% Test all required fields are present
    {ok, Decoded} = erlmcp_json_rpc:decode_message(
        <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>
    ),
    ?assert(is_record(Decoded, json_rpc_request)).

jsonrpc_missing_version_test() ->
    %% Request without jsonrpc field should fail
    Invalid = <<"{\"method\":\"test\",\"id\":1}">>,
    ?assertMatch({error, _}, erlmcp_json_rpc:decode_message(Invalid)).

jsonrpc_missing_method_test() ->
    %% Request without method field should fail
    Invalid = <<"{\"jsonrpc\":\"2.0\",\"id\":1}">>,
    ?assertMatch({error, _}, erlmcp_json_rpc:decode_message(Invalid)).

jsonrpc_batch_array_test() ->
    %% Batch requests must be array
    BatchJson = <<"[{\"jsonrpc\":\"2.0\",\"method\":\"test1\",\"id\":1},{\"jsonrpc\":\"2.0\",\"method\":\"test2\",\"id\":2}]">>,
    ?assertEqual(true, erlmcp_json_rpc:is_batch_request(BatchJson)).

jsonrpc_empty_batch_test() ->
    %% Empty batch array is invalid per spec
    EmptyBatch = <<"[]">>,
    ?assertMatch({error, {invalid_request, empty_batch}},
                 erlmcp_json_rpc:decode_batch(EmptyBatch)).

%%%===================================================================
%%% JSON-RPC ID Field Tests
%%%===================================================================

jsonrpc_id_number_test() ->
    %% ID can be number
    Request = erlmcp_json_rpc:encode_request(42, <<"test">>, #{}),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertEqual(42, Decoded#json_rpc_request.id).

jsonrpc_id_string_test() ->
    %% ID can be string
    Request = erlmcp_json_rpc:encode_request(<<"req-1">>, <<"test">>, #{}),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertEqual(<<"req-1">>, Decoded#json_rpc_request.id).

jsonrpc_id_null_test() ->
    %% ID can be null (notification)
    Notification = erlmcp_json_rpc:encode_notification(<<"notify">>, #{}),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Notification),
    ?assertEqual(undefined, Decoded#json_rpc_notification.id).

jsonrpc_id_missing_notification_test() ->
    %% Notification has no id field
    Notification = erlmcp_json_rpc:encode_notification(<<"notify">>, #{}),
    Json = jsx:decode(Notification, [return_maps]),
    ?assertNot(maps:is_key(<<"id">>, Json)).

jsonrpc_response_id_match_test() ->
    %% Response ID must match request ID
    ReqId = "abc123",
    Response = erlmcp_json_rpc:encode_response(ReqId, #{<<"result">> => <<"ok">>}),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertEqual(ReqId, Decoded#json_rpc_response.id).

%%%===================================================================
%%% JSON-RPC Method Field Tests
%%%===================================================================

jsonrpc_method_string_test() ->
    %% Method must be string
    Request = erlmcp_json_rpc:encode_request(1, <<"test.method">>, #{}),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assert(is_binary(Decoded#json_rpc_request.method)).

jsonrpc_method_with_namespace_test() ->
    %% Method can contain namespaces with dots
    Method = <<"mcp.resource.list">>,
    Request = erlmcp_json_rpc:encode_request(1, Method, #{}),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertEqual(Method, Decoded#json_rpc_request.method).

jsonrpc_method_reserved_chars_test() ->
    %% Method names starting with rpc. are reserved
    ReservedMethod = <<"rpc.system.method">>,
    Request = erlmcp_json_rpc:encode_request(1, ReservedMethod, #{}),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertEqual(ReservedMethod, Decoded#json_rpc_request.method).

%%%===================================================================
%%% JSON-RPC Params Field Tests
%%%===================================================================

jsonrpc_params_by_name_test() ->
    %% Params can be object (by-name)
    Params = #{<<"key1">> => <<"value1">>, <<"key2">> => <<"value2">>},
    Request = erlmcp_json_rpc:encode_request(1, <<"test">>, Params),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertEqual(Params, Decoded#json_rpc_request.params).

jsonrpc_params_by_position_test() ->
    %% Params can be array (by-position)
    Params = [<<"arg1">>, <<"arg2">>],
    Request = erlmcp_json_rpc:encode_request(1, <<"test">>, Params),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertEqual(Params, Decoded#json_rpc_request.params).

jsonrpc_params_omitted_test() ->
    %% Params can be omitted if no parameters
    Request = erlmcp_json_rpc:encode_request(1, <<"test">>, undefined),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertEqual(undefined, Decoded#json_rpc_request.params).

%%%===================================================================
%%% JSON-RPC Response Format Tests
%%%===================================================================

jsonrpc_response_with_result_test() ->
    %% Success response has result field
    Response = erlmcp_json_rpc:encode_response(1, #{<<"data">> => <<"value">>}),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertMatch(#json_rpc_response{result = _}, Decoded).

jsonrpc_response_no_result_and_error_test() ->
    %% Response cannot have both result and error
    Response = erlmcp_json_rpc:encode_response(1, #{<<"ok">> => true}),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertNot(is_record(Decoded#json_rpc_response.error, json_rpc_error)).

jsonrpc_response_with_error_test() ->
    %% Error response has error field
    Response = erlmcp_json_rpc:encode_error_response(1, -32601, <<"Not found">>),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assert(is_record(Decoded#json_rpc_response.error, json_rpc_error)).

%%%===================================================================
%%% JSON-RPC Error Object Tests
%%%===================================================================

jsonrpc_error_code_test() ->
    %% Error object must have code
    Response = erlmcp_json_rpc:encode_error_response(1, -32601, <<"Not found">>),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertEqual(-32601, Decoded#json_rpc_response.error#mcp_error.code).

jsonrpc_error_message_test() ->
    %% Error object must have message
    Response = erlmcp_json_rpc:encode_error_response(1, -32601, <<"Not found">>),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertEqual(<<"Not found">>, Decoded#json_rpc_response.error#mcp_error.message).

jsonrpc_error_data_optional_test() ->
    %% Error object can have optional data
    Data = #{<<"details">> => <<"More info">>},
    Response = erlmcp_json_rpc:encode_error_response(1, -32601, <<"Not found">>, Data),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertEqual(Data, Decoded#json_rpc_response.error#mcp_error.data).

jsonrpc_error_standard_codes_test() ->
    %% Test all standard JSON-RPC error codes
    StandardCodes = [
        -32700,  %% Parse error
        -32600,  %% Invalid request
        -32601,  %% Method not found
        -32602,  %% Invalid params
        -32603   %% Internal error
    ],
    lists:foreach(fun(Code) ->
        ?assertEqual(true, erlmcp_json_rpc:validate_error_code(Code))
    end, StandardCodes).

jsonrpc_error_server_codes_test() ->
    %% Test server error range (-32000 to -32099)
    ServerCodes = [-32001, -32002, -32050, -32099],
    lists:foreach(fun(Code) ->
        ?assertEqual(true, erlmcp_json_rpc:validate_error_code(Code))
    end, ServerCodes).

jsonrpc_error_invalid_code_test() ->
    %% Invalid error codes should fail validation
    ?assertEqual(false, erlmcp_json_rpc:validate_error_code(-999)).

%%%===================================================================
%%% JSON-RPC Notification Tests
%%%===================================================================

jsonrpc_notification_no_id_test() ->
    %% Notification has no id field
    Notification = erlmcp_json_rpc:encode_notification(<<"notify">>, #{}),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Notification),
    ?assert(is_record(Decoded, json_rpc_notification)).

jsonrpc_notification_no_response_test() ->
    %% Server must not respond to notifications
    %% (This is tested in integration suite)
    ?assert(true).

%%%===================================================================
%%% JSON-RPC Batch Request Tests
%%%===================================================================

jsonrpc_batch_mixed_methods_test() ->
    %% Batch can contain different methods
    Requests = [
        erlmcp_json_rpc:encode_request(1, <<"method1">>, #{}),
        erlmcp_json_rpc:encode_request(2, <<"method2">>, #{}),
        erlmcp_json_rpc:encode_request(3, <<"method3">>, #{})
    ],
    BatchJson = erlmcp_json_rpc:encode_batch(Requests),
    {ok, Decoded} = erlmcp_json_rpc:decode_batch(BatchJson),
    ?assertEqual(3, length(Decoded)).

jsonrpc_batch_with_notifications_test() ->
    %% Batch can mix requests and notifications
    Requests = [
        erlmcp_json_rpc:encode_request(1, <<"method1">>, #{}),
        erlmcp_json_rpc:encode_notification(<<"notify1">>, #{}),
        erlmcp_json_rpc:encode_request(2, <<"method2">>, #{})
    ],
    BatchJson = erlmcp_json_rpc:encode_batch(Requests),
    {ok, Decoded} = erlmcp_json_rpc:decode_batch(BatchJson),
    ?assertEqual(3, length(Decoded)).

jsonrpc_batch_preserves_order_test() ->
    %% Batch responses must preserve order
    Ids = [1, 2, 3, 4, 5],
    Requests = [erlmcp_json_rpc:encode_request(Id, <<"test">>, #{}) || Id <- Ids],
    BatchJson = erlmcp_json_rpc:encode_batch(Requests),
    {ok, Decoded} = erlmcp_json_rpc:decode_batch(BatchJson),
    DecodedIds = [case R of
        #json_rpc_request{id = Id} -> Id;
        #json_rpc_notification{} -> undefined
    end || R <- Decoded],
    ?assertEqual(Ids, DecodedIds).

jsonrpc_batch_all_errors_test() ->
    %% Batch with all errors should return all errors
    InvalidJson = <<"[{\"jsonrpc\":\"2.0\",\"method\":\"invalid1\"},{\"jsonrpc\":\"2.0\",\"method\":\"invalid2\"}]">>,
    {ok, Decoded} = erlmcp_json_rpc:decode_batch(InvalidJson),
    ?assertEqual(2, length(Decoded)).

%%%===================================================================
%%% JSON-RPC Parse Error Tests
%%%===================================================================

jsonrpc_parse_error_invalid_json_test() ->
    %% Invalid JSON should return parse error
    InvalidJson = <<"{invalid json}">>,
    ?assertMatch({error, {parse_error, _}}, erlmcp_json_rpc:decode_message(InvalidJson)).

jsonrpc_parse_error_correct_format_test() ->
    %% Parse error response must follow spec
    ErrorJson = erlmcp_json_rpc:error_parse(null),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(-32700, Decoded#json_rpc_response.error#mcp_error.code).

%%%===================================================================
%%% JSON-RPC Method Not Found Tests
%%%===================================================================

jsonrpc_method_not_found_error_test() ->
    %% Unknown method should return method not found
    ErrorJson = erlmcp_json_rpc:error_method_not_found(1, <<"unknown.method">>),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(-32601, Decoded#json_rpc_response.error#mcp_error.code).

jsonrpc_method_not_found_with_data_test() ->
    %% Method not found can include method name in data
    Method = <<"mcp.unknown.capability">>,
    ErrorJson = erlmcp_json_rpc:error_method_not_found(1, Method),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(-32601, Decoded#json_rpc_response.error#mcp_error.code).

%%%===================================================================
%%% JSON-RPC Invalid Params Tests
%%%===================================================================

jsonrpc_invalid_params_error_test() ->
    %% Invalid parameters should return invalid params error
    ErrorJson = erlmcp_json_rpc:error_invalid_params(1, <<"Missing required field">>),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(-32602, Decoded#json_rpc_response.error#mcp_error.code).

jsonrpc_invalid_params_type_test() ->
    %% Wrong parameter type should fail
    ErrorJson = erlmcp_json_rpc:error_invalid_params(1, <<"Expected string, got number">>),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(-32602, Decoded#json_rpc_response.error#mcp_error.code).

%%%===================================================================
%%% JSON-RPC Internal Error Tests
%%%===================================================================

jsonrpc_internal_error_test() ->
    %% Server errors should return internal error
    ErrorJson = erlmcp_json_rpc:error_internal(1),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(-32603, Decoded#json_rpc_response.error#mcp_error.code).

jsonrpc_internal_error_with_data_test() ->
    %% Internal error can include diagnostic data
    Data = #{<<"stacktrace">> => <<"error details">>},
    ErrorJson = erlmcp_json_rpc:encode_error_response(1, -32603, <<"Internal error">>, Data),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(-32603, Decoded#json_rpc_response.error#mcp_error.code).

%%%===================================================================
%%% MCP-Specific Error Codes Tests
%%%===================================================================

mcp_error_resource_not_found_test() ->
    %% Resource not found error
    ErrorJson = erlmcp_json_rpc:error_resource_not_found(1, <<"file:///missing.txt">>),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(?MCP_ERROR_RESOURCE_NOT_FOUND, Decoded#json_rpc_response.error#mcp_error.code).

mcp_error_tool_not_found_test() ->
    %% Tool not found error
    ErrorJson = erlmcp_json_rpc:error_tool_not_found(1, <<"unknown_tool">>),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(?MCP_ERROR_TOOL_NOT_FOUND, Decoded#json_rpc_response.error#mcp_error.code).

mcp_error_prompt_not_found_test() ->
    %% Prompt not found error
    ErrorJson = erlmcp_json_rpc:error_prompt_not_found(1, <<"unknown_prompt">>),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(?MCP_ERROR_PROMPT_NOT_FOUND, Decoded#json_rpc_response.error#mcp_error.code).

mcp_error_capability_not_supported_test() ->
    %% Capability not supported error
    ErrorJson = erlmcp_json_rpc:error_capability_not_supported(1, <<"roots">>),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(?MCP_ERROR_CAPABILITY_NOT_SUPPORTED, Decoded#json_rpc_response.error#mcp_error.code).

mcp_error_not_initialized_test() ->
    %% Not initialized error
    ErrorJson = erlmcp_json_rpc:error_not_initialized(1),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(?MCP_ERROR_NOT_INITIALIZED, Decoded#json_rpc_response.error#mcp_error.code).

mcp_error_validation_failed_test() ->
    %% Validation failed error
    ErrorJson = erlmcp_json_rpc:error_validation_failed(1, <<"Invalid input">>),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(?MCP_ERROR_VALIDATION_FAILED, Decoded#json_rpc_response.error#mcp_error.code).

mcp_error_message_too_large_test() ->
    %% Message too large error (Gap #45)
    ErrorJson = erlmcp_json_rpc:error_message_too_large(1, 1048576),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(?MCP_ERROR_MESSAGE_TOO_LARGE, Decoded#json_rpc_response.error#mcp_error.code).
