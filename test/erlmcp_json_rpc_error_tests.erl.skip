-module(erlmcp_json_rpc_error_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite: JSON-RPC Error Response Structure (Gap #5)
%%====================================================================
%%
%% This test suite validates error response structure according to
%% JSON-RPC 2.0 specification and MCP 2025-11-25 requirements.
%%
%% Key requirements:
%% 1. All error responses include data field when appropriate
%% 2. Error codes are validated against whitelist
%% 3. Helper functions generate properly structured errors
%% 4. Data field contains details and context information
%% 5. Invalid error codes are rejected or converted to internal error
%%====================================================================

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Error Code Validation Tests
%%====================================================================

error_code_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_validate_parse_error_code()),
             ?_test(test_validate_invalid_request_code()),
             ?_test(test_validate_method_not_found_code()),
             ?_test(test_validate_invalid_params_code()),
             ?_test(test_validate_internal_error_code()),
             ?_test(test_validate_mcp_resource_not_found_code()),
             ?_test(test_validate_mcp_tool_not_found_code()),
             ?_test(test_validate_mcp_prompt_not_found_code()),
             ?_test(test_validate_mcp_capability_not_supported_code()),
             ?_test(test_validate_mcp_not_initialized_code()),
             ?_test(test_validate_mcp_timeout_code()),
             ?_test(test_validate_invalid_code()),
             ?_test(test_validate_out_of_range_code()),
             ?_test(test_validate_custom_code())
         ]
     end}.

test_validate_parse_error_code() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32700)).

test_validate_invalid_request_code() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32600)).

test_validate_method_not_found_code() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32601)).

test_validate_invalid_params_code() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32602)).

test_validate_internal_error_code() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32603)).

test_validate_mcp_resource_not_found_code() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32001)).

test_validate_mcp_tool_not_found_code() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32002)).

test_validate_mcp_prompt_not_found_code() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32003)).

test_validate_mcp_capability_not_supported_code() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32004)).

test_validate_mcp_not_initialized_code() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32005)).

test_validate_mcp_timeout_code() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32009)).

test_validate_invalid_code() ->
    ?assertNot(erlmcp_json_rpc:validate_error_code(-99999)).

test_validate_out_of_range_code() ->
    ?assertNot(erlmcp_json_rpc:validate_error_code(32000)).

test_validate_custom_code() ->
    ?assertNot(erlmcp_json_rpc:validate_error_code(-32050)).

%%====================================================================
%% Error Response Structure Tests
%%====================================================================

error_response_structure_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_error_response_without_data()),
             ?_test(test_error_response_with_map_data()),
             ?_test(test_error_response_with_binary_data()),
             ?_test(test_error_response_with_null_data()),
             ?_test(test_error_response_json_structure()),
             ?_test(test_error_response_has_required_fields()),
             ?_test(test_error_response_data_field_when_provided()),
             ?_test(test_error_response_invalid_code_fallback())
         ]
     end}.

test_error_response_without_data() ->
    Response = erlmcp_json_rpc:encode_error_response(1, -32601, <<"Method not found">>),
    ?assertMatch(<<"{", _/binary>>, Response),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(-32601, maps:get(<<"code">>, maps:get(<<"error">>, Decoded))),
    ?assert(not maps:is_key(<<"data">>, maps:get(<<"error">>, Decoded))).

test_error_response_with_map_data() ->
    Data = #{<<"method">> => <<"invalid_method">>},
    Response = erlmcp_json_rpc:encode_error_response(1, -32601, <<"Method not found">>, Data),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(Data, maps:get(<<"data">>, Error)).

test_error_response_with_binary_data() ->
    Response = erlmcp_json_rpc:encode_error_response(1, -32602, <<"Invalid params">>, <<"Missing required field">>),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    DataObj = maps:get(<<"data">>, Error),
    ?assertEqual(<<"Missing required field">>, maps:get(<<"details">>, DataObj)).

test_error_response_with_null_data() ->
    Response = erlmcp_json_rpc:encode_error_response(1, -32603, <<"Internal error">>, null),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(not maps:is_key(<<"data">>, Error)).

test_error_response_json_structure() ->
    Response = erlmcp_json_rpc:encode_error_response(42, -32602, <<"Invalid params">>, #{<<"field">> => <<"test">>}),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(42, maps:get(<<"id">>, Decoded)),
    ?assertMatch(#{<<"code">> := -32602, <<"message">> := <<"Invalid params">>, <<"data">> := _}, maps:get(<<"error">>, Decoded)).

test_error_response_has_required_fields() ->
    Response = erlmcp_json_rpc:encode_error_response(1, -32600, <<"Invalid Request">>, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(maps:is_key(<<"code">>, Error)),
    ?assert(maps:is_key(<<"message">>, Error)).

test_error_response_data_field_when_provided() ->
    Data = #{<<"context">> => <<"additional info">>},
    Response = erlmcp_json_rpc:encode_error_response(5, -32001, <<"Resource not found">>, Data),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(maps:is_key(<<"data">>, Error)),
    ?assertEqual(Data, maps:get(<<"data">>, Error)).

test_error_response_invalid_code_fallback() ->
    %% Invalid code should be converted to internal error
    Response = erlmcp_json_rpc:encode_error_response(1, -99999, <<"Custom error">>, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(?JSONRPC_INTERNAL_ERROR, maps:get(<<"code">>, Error)).

%%====================================================================
%% Error Helper Function Tests
%%====================================================================

error_helper_functions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_error_method_not_found()),
             ?_test(test_error_invalid_params_binary()),
             ?_test(test_error_invalid_params_list()),
             ?_test(test_error_resource_not_found()),
             ?_test(test_error_tool_not_found()),
             ?_test(test_error_prompt_not_found()),
             ?_test(test_error_capability_not_supported()),
             ?_test(test_error_not_initialized()),
             ?_test(test_error_validation_failed()),
             ?_test(test_error_internal()),
             ?_test(test_error_parse()),
             ?_test(test_error_helpers_include_data_field())
         ]
     end}.

test_error_method_not_found() ->
    Response = erlmcp_json_rpc:error_method_not_found(1, <<"invalid_method">>),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(?JSONRPC_METHOD_NOT_FOUND, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"invalid_method">>, maps:get(<<"method">>, maps:get(<<"data">>, Error))).

test_error_invalid_params_binary() ->
    Response = erlmcp_json_rpc:error_invalid_params(1, <<"Missing required field">>),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Missing required field">>, maps:get(<<"details">>, maps:get(<<"data">>, Error))).

test_error_invalid_params_list() ->
    Response = erlmcp_json_rpc:error_invalid_params(2, "Missing parameter"),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, maps:get(<<"code">>, Error)),
    Details = maps:get(<<"details">>, maps:get(<<"data">>, Error)),
    ?assertEqual(<<"Missing parameter">>, Details).

test_error_resource_not_found() ->
    Response = erlmcp_json_rpc:error_resource_not_found(3, <<"file://missing.txt">>),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(?MCP_ERROR_RESOURCE_NOT_FOUND, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"file://missing.txt">>, maps:get(<<"uri">>, maps:get(<<"data">>, Error))).

test_error_tool_not_found() ->
    Response = erlmcp_json_rpc:error_tool_not_found(4, <<"calculator">>),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(?MCP_ERROR_TOOL_NOT_FOUND, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"calculator">>, maps:get(<<"tool">>, maps:get(<<"data">>, Error))).

test_error_prompt_not_found() ->
    Response = erlmcp_json_rpc:error_prompt_not_found(5, <<"summarize">>),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(?MCP_ERROR_PROMPT_NOT_FOUND, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"summarize">>, maps:get(<<"prompt">>, maps:get(<<"data">>, Error))).

test_error_capability_not_supported() ->
    Response = erlmcp_json_rpc:error_capability_not_supported(6, <<"sampling">>),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(?MCP_ERROR_CAPABILITY_NOT_SUPPORTED, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"sampling">>, maps:get(<<"capability">>, maps:get(<<"data">>, Error))).

test_error_not_initialized() ->
    Response = erlmcp_json_rpc:error_not_initialized(7),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(?MCP_ERROR_NOT_INITIALIZED, maps:get(<<"code">>, Error)),
    ?assertEqual(?MCP_MSG_NOT_INITIALIZED, maps:get(<<"message">>, Error)).

test_error_validation_failed() ->
    Response = erlmcp_json_rpc:error_validation_failed(8, <<"JSON schema validation failed">>),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(?MCP_ERROR_VALIDATION_FAILED, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"JSON schema validation failed">>, maps:get(<<"details">>, maps:get(<<"data">>, Error))).

test_error_internal() ->
    Response = erlmcp_json_rpc:error_internal(9),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(?JSONRPC_INTERNAL_ERROR, maps:get(<<"code">>, Error)),
    ?assertEqual(?JSONRPC_MSG_INTERNAL_ERROR, maps:get(<<"message">>, Error)).

test_error_parse() ->
    Response = erlmcp_json_rpc:error_parse(10),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(?JSONRPC_PARSE_ERROR, maps:get(<<"code">>, Error)),
    ?assertEqual(?JSONRPC_MSG_PARSE_ERROR, maps:get(<<"message">>, Error)).

test_error_helpers_include_data_field() ->
    %% All helpers with contextual data should include data field
    MethodError = erlmcp_json_rpc:error_method_not_found(1, <<"test">>),
    ResourceError = erlmcp_json_rpc:error_resource_not_found(1, <<"uri">>),
    ToolError = erlmcp_json_rpc:error_tool_not_found(1, <<"tool">>),

    MethodDecoded = jsx:decode(MethodError, [return_maps]),
    ResourceDecoded = jsx:decode(ResourceError, [return_maps]),
    ToolDecoded = jsx:decode(ToolError, [return_maps]),

    ?assert(maps:is_key(<<"data">>, maps:get(<<"error">>, MethodDecoded))),
    ?assert(maps:is_key(<<"data">>, maps:get(<<"error">>, ResourceDecoded))),
    ?assert(maps:is_key(<<"data">>, maps:get(<<"error">>, ToolDecoded))).

%%====================================================================
%% Round-Trip Tests: Encode -> Decode -> Verify
%%====================================================================

round_trip_tests_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_roundtrip_method_not_found()),
             ?_test(test_roundtrip_resource_not_found()),
             ?_test(test_roundtrip_invalid_params()),
             ?_test(test_roundtrip_custom_data_field()),
             ?_test(test_roundtrip_preserves_id()),
             ?_test(test_roundtrip_complex_data_structure())
         ]
     end}.

test_roundtrip_method_not_found() ->
    OrigId = 42,
    OrigMethod = <<"nonexistent">>,
    Encoded = erlmcp_json_rpc:error_method_not_found(OrigId, OrigMethod),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(OrigId, maps:get(<<"id">>, Decoded)),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(OrigMethod, maps:get(<<"method">>, maps:get(<<"data">>, Error))).

test_roundtrip_resource_not_found() ->
    OrigId = 99,
    OrigUri = <<"file://path/to/file.txt">>,
    Encoded = erlmcp_json_rpc:error_resource_not_found(OrigId, OrigUri),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(OrigId, maps:get(<<"id">>, Decoded)),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(OrigUri, maps:get(<<"uri">>, maps:get(<<"data">>, Error))).

test_roundtrip_invalid_params() ->
    OrigId = 100,
    OrigDetails = <<"Field 'x' is required">>,
    Encoded = erlmcp_json_rpc:error_invalid_params(OrigId, OrigDetails),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(OrigDetails, maps:get(<<"details">>, maps:get(<<"data">>, Error))).

test_roundtrip_custom_data_field() ->
    CustomData = #{
        <<"field1">> => <<"value1">>,
        <<"field2">> => 123,
        <<"nested">> => #{<<"key">> => <<"val">>}
    },
    Encoded = erlmcp_json_rpc:encode_error_response(1, -32602, <<"Test">>, CustomData),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(CustomData, maps:get(<<"data">>, Error)).

test_roundtrip_preserves_id() ->
    Ids = [1, <<"string-id">>, null, 999999],
    Results = [
        begin
            Encoded = erlmcp_json_rpc:encode_error_response(Id, -32603, <<"Error">>, undefined),
            Decoded = jsx:decode(Encoded, [return_maps]),
            maps:get(<<"id">>, Decoded)
        end || Id <- Ids
    ],
    ?assertEqual(Ids, Results).

test_roundtrip_complex_data_structure() ->
    ComplexData = #{
        <<"supported_versions">> => [<<"2025-11-25">>, <<"2024-11-05">>],
        <<"requested">> => <<"1.0.0">>,
        <<"error_info">> => #{
            <<"code">> => -32602,
            <<"type">> => <<"version_mismatch">>
        }
    },
    Encoded = erlmcp_json_rpc:encode_error_response(50, -32602, <<"Unsupported protocol version">>, ComplexData),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(ComplexData, maps:get(<<"data">>, Error)).

%%====================================================================
%% Edge Cases and Boundary Tests
%%====================================================================

edge_case_tests_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_error_with_empty_string_data()),
             ?_test(test_error_with_empty_map_data()),
             ?_test(test_error_with_special_characters_in_data()),
             ?_test(test_error_with_unicode_in_data()),
             ?_test(test_error_with_very_long_data()),
             ?_test(test_error_response_with_null_id()),
             ?_test(test_error_response_with_string_id()),
             ?_test(test_error_response_with_integer_id()),
             ?_test(test_multiple_sequential_errors())
         ]
     end}.

test_error_with_empty_string_data() ->
    Response = erlmcp_json_rpc:error_invalid_params(1, <<"">>),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(<<>>, maps:get(<<"details">>, maps:get(<<"data">>, Error))).

test_error_with_empty_map_data() ->
    Response = erlmcp_json_rpc:encode_error_response(1, -32602, <<"Error">>, #{}),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(#{}, maps:get(<<"data">>, Error)).

test_error_with_special_characters_in_data() ->
    Data = #{<<"message">> => <<"Error: \n\t\r\\ \"quotes\"">>},
    Response = erlmcp_json_rpc:encode_error_response(1, -32602, <<"Error">>, Data),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assert(maps:is_key(<<"data">>, maps:get(<<"error">>, Decoded))).

test_error_with_unicode_in_data() ->
    Data = #{<<"message">> => <<"Error: 你好 🚀 ñ">>},
    Response = erlmcp_json_rpc:encode_error_response(1, -32602, <<"Error">>, Data),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(maps:is_key(<<"data">>, Error)).

test_error_with_very_long_data() ->
    LongString = binary:copy(<<"x">>, 10000),
    Response = erlmcp_json_rpc:error_invalid_params(1, LongString),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(LongString, maps:get(<<"details">>, maps:get(<<"data">>, Error))).

test_error_response_with_null_id() ->
    Response = erlmcp_json_rpc:encode_error_response(null, -32602, <<"Error">>, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(null, maps:get(<<"id">>, Decoded)).

test_error_response_with_string_id() ->
    Response = erlmcp_json_rpc:encode_error_response(<<"request-123">>, -32602, <<"Error">>, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"request-123">>, maps:get(<<"id">>, Decoded)).

test_error_response_with_integer_id() ->
    Response = erlmcp_json_rpc:encode_error_response(999999, -32602, <<"Error">>, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(999999, maps:get(<<"id">>, Decoded)).

test_multiple_sequential_errors() ->
    Errors = [
        erlmcp_json_rpc:error_method_not_found(1, <<"m1">>),
        erlmcp_json_rpc:error_invalid_params(2, <<"p1">>),
        erlmcp_json_rpc:error_resource_not_found(3, <<"r1">>),
        erlmcp_json_rpc:error_tool_not_found(4, <<"t1">>),
        erlmcp_json_rpc:error_prompt_not_found(5, <<"pr1">>)
    ],
    ?assertEqual(5, length(Errors)),
    Results = [jsx:decode(E, [return_maps]) || E <- Errors],
    Ids = [maps:get(<<"id">>, R) || R <- Results],
    ?assertEqual([1, 2, 3, 4, 5], Ids).

%%====================================================================
%% Data Structure Validation Tests
%%====================================================================

data_structure_tests_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_error_object_has_code_field()),
             ?_test(test_error_object_has_message_field()),
             ?_test(test_error_code_is_integer()),
             ?_test(test_error_message_is_binary()),
             ?_test(test_error_data_is_map_when_present()),
             ?_test(test_response_has_jsonrpc_version()),
             ?_test(test_response_has_id_field()),
             ?_test(test_response_has_error_field()),
             ?_test(test_response_does_not_have_result_with_error()),
             ?_test(test_error_codes_match_constants())
         ]
     end}.

test_error_object_has_code_field() ->
    Response = erlmcp_json_rpc:encode_error_response(1, -32602, <<"Error">>, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(maps:is_key(<<"code">>, Error)).

test_error_object_has_message_field() ->
    Response = erlmcp_json_rpc:encode_error_response(1, -32602, <<"Error">>, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(maps:is_key(<<"message">>, Error)).

test_error_code_is_integer() ->
    Response = erlmcp_json_rpc:encode_error_response(1, -32602, <<"Error">>, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    Code = maps:get(<<"code">>, Error),
    ?assert(is_integer(Code)).

test_error_message_is_binary() ->
    Response = erlmcp_json_rpc:encode_error_response(1, -32602, <<"Error">>, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    Message = maps:get(<<"message">>, Error),
    ?assert(is_binary(Message)).

test_error_data_is_map_when_present() ->
    Data = #{<<"key">> => <<"value">>},
    Response = erlmcp_json_rpc:encode_error_response(1, -32602, <<"Error">>, Data),
    Decoded = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    DataField = maps:get(<<"data">>, Error),
    ?assert(is_map(DataField)).

test_response_has_jsonrpc_version() ->
    Response = erlmcp_json_rpc:encode_error_response(1, -32602, <<"Error">>, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)).

test_response_has_id_field() ->
    Response = erlmcp_json_rpc:encode_error_response(42, -32602, <<"Error">>, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assert(maps:is_key(<<"id">>, Decoded)).

test_response_has_error_field() ->
    Response = erlmcp_json_rpc:encode_error_response(1, -32602, <<"Error">>, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assert(maps:is_key(<<"error">>, Decoded)).

test_response_does_not_have_result_with_error() ->
    Response = erlmcp_json_rpc:encode_error_response(1, -32602, <<"Error">>, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertNot(maps:is_key(<<"result">>, Decoded)).

test_error_codes_match_constants() ->
    TestCases = [
        {erlmcp_json_rpc:error_method_not_found(1, <<"m">>), ?JSONRPC_METHOD_NOT_FOUND},
        {erlmcp_json_rpc:error_invalid_params(1, <<"p">>), ?JSONRPC_INVALID_PARAMS},
        {erlmcp_json_rpc:error_resource_not_found(1, <<"r">>), ?MCP_ERROR_RESOURCE_NOT_FOUND},
        {erlmcp_json_rpc:error_tool_not_found(1, <<"t">>), ?MCP_ERROR_TOOL_NOT_FOUND},
        {erlmcp_json_rpc:error_prompt_not_found(1, <<"p">>), ?MCP_ERROR_PROMPT_NOT_FOUND},
        {erlmcp_json_rpc:error_capability_not_supported(1, <<"c">>), ?MCP_ERROR_CAPABILITY_NOT_SUPPORTED},
        {erlmcp_json_rpc:error_not_initialized(1), ?MCP_ERROR_NOT_INITIALIZED},
        {erlmcp_json_rpc:error_internal(1), ?JSONRPC_INTERNAL_ERROR},
        {erlmcp_json_rpc:error_parse(1), ?JSONRPC_PARSE_ERROR}
    ],

    Results = [
        begin
            Decoded = jsx:decode(Response, [return_maps]),
            Error = maps:get(<<"error">>, Decoded),
            Code = maps:get(<<"code">>, Error),
            Code =:= ExpectedCode
        end || {Response, ExpectedCode} <- TestCases
    ],

    ?assert(lists:all(fun(R) -> R end, Results)).

%%====================================================================
%% Create Error Record Tests
%%====================================================================

create_error_record_tests_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_create_error_basic()),
             ?_test(test_create_error_with_data()),
             ?_test(test_create_error_record_code()),
             ?_test(test_create_error_record_message()),
             ?_test(test_create_error_record_data())
         ]
     end}.

test_create_error_basic() ->
    Error = erlmcp_json_rpc:create_error(-32602, <<"Invalid params">>, undefined),
    ?assertEqual(-32602, Error#mcp_error.code),
    ?assertEqual(<<"Invalid params">>, Error#mcp_error.message),
    ?assertEqual(undefined, Error#mcp_error.data).

test_create_error_with_data() ->
    Data = #{<<"field">> => <<"value">>},
    Error = erlmcp_json_rpc:create_error(-32602, <<"Invalid params">>, Data),
    ?assertEqual(Data, Error#mcp_error.data).

test_create_error_record_code() ->
    Error = erlmcp_json_rpc:create_error(-32001, <<"Not found">>, undefined),
    ?assertEqual(-32001, Error#mcp_error.code).

test_create_error_record_message() ->
    Error = erlmcp_json_rpc:create_error(-32602, <<"Test message">>, undefined),
    ?assertEqual(<<"Test message">>, Error#mcp_error.message).

test_create_error_record_data() ->
    Data = #{<<"key">> => <<"value">>},
    Error = erlmcp_json_rpc:create_error(-32602, <<"Msg">>, Data),
    ?assertEqual(Data, Error#mcp_error.data).
