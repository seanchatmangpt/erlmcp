-module(erlmcp_error_response_id_consistency_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite: Error Response ID Consistency (Gap #44)
%%====================================================================
%%
%% This test suite validates error response ID consistency according to
%% JSON-RPC 2.0 specification and MCP 2025-11-25 requirements.
%%
%% Key requirements:
%% 1. Error responses must have matching `id` field from request
%% 2. Only use `null` id when request ID was invalid/missing
%% 3. Preserve request ID in error responses
%% 4. Return error with request ID for invalid params, etc.
%% 5. JSON-RPC 2.0 compliance
%%
%% Test coverage:
%% - Error with valid request ID includes ID
%% - Parse error uses null ID
%% - Invalid request uses null ID
%% - Method not found includes ID
%% - Invalid params includes ID
%% - ID matches original request
%% - Complex ID types (string, integer)
%% - Edge cases and boundaries
%%====================================================================

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Test Group 1: Error with Valid Request ID Should Include ID
%%====================================================================

error_with_valid_id_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_error_method_not_found_preserves_integer_id()),
             ?_test(test_error_invalid_params_preserves_integer_id()),
             ?_test(test_error_resource_not_found_preserves_integer_id()),
             ?_test(test_error_tool_not_found_preserves_integer_id()),
             ?_test(test_error_prompt_not_found_preserves_integer_id()),
             ?_test(test_error_internal_preserves_integer_id()),
             ?_test(test_error_capability_not_supported_preserves_integer_id()),
             ?_test(test_error_validation_failed_preserves_integer_id())
         ]
     end}.

test_error_method_not_found_preserves_integer_id() ->
    RequestId = 42,
    Response = erlmcp_json_rpc:error_method_not_found(RequestId, <<"invalid_method">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(RequestId, ResponseId).

test_error_invalid_params_preserves_integer_id() ->
    RequestId = 100,
    Response = erlmcp_json_rpc:error_invalid_params(RequestId, <<"Missing field">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(RequestId, ResponseId).

test_error_resource_not_found_preserves_integer_id() ->
    RequestId = 77,
    Response = erlmcp_json_rpc:error_resource_not_found(RequestId, <<"file://missing.txt">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(RequestId, ResponseId).

test_error_tool_not_found_preserves_integer_id() ->
    RequestId = 99,
    Response = erlmcp_json_rpc:error_tool_not_found(RequestId, <<"calculator">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(RequestId, ResponseId).

test_error_prompt_not_found_preserves_integer_id() ->
    RequestId = 55,
    Response = erlmcp_json_rpc:error_prompt_not_found(RequestId, <<"summarize">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(RequestId, ResponseId).

test_error_internal_preserves_integer_id() ->
    RequestId = 123,
    Response = erlmcp_json_rpc:error_internal(RequestId),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(RequestId, ResponseId).

test_error_capability_not_supported_preserves_integer_id() ->
    RequestId = 88,
    Response = erlmcp_json_rpc:error_capability_not_supported(RequestId, <<"sampling">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(RequestId, ResponseId).

test_error_validation_failed_preserves_integer_id() ->
    RequestId = 44,
    Response = erlmcp_json_rpc:error_validation_failed(RequestId, <<"JSON schema failed">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(RequestId, ResponseId).

%%====================================================================
%% Test Group 2: Parse Error Should Use Null ID
%%====================================================================

parse_error_uses_null_id_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_parse_error_uses_null_id()),
             ?_test(test_parse_error_null_id_explicit()),
             ?_test(test_parse_error_null_id_in_json())
         ]
     end}.

test_parse_error_uses_null_id() ->
    Response = erlmcp_json_rpc:error_parse(null),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(null, ResponseId).

test_parse_error_null_id_explicit() ->
    Response = erlmcp_json_rpc:encode_error_response(null, ?JSONRPC_PARSE_ERROR, ?JSONRPC_MSG_PARSE_ERROR, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(null, ResponseId).

test_parse_error_null_id_in_json() ->
    Response = erlmcp_json_rpc:error_parse(null),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(null, maps:get(<<"id">>, Decoded)).

%%====================================================================
%% Test Group 3: Invalid Request Should Use Null ID
%%====================================================================

invalid_request_uses_null_id_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_invalid_request_null_id()),
             ?_test(test_invalid_request_error_code()),
             ?_test(test_invalid_request_message())
         ]
     end}.

test_invalid_request_null_id() ->
    Response = erlmcp_json_rpc:encode_error_response(null, ?JSONRPC_INVALID_REQUEST, ?JSONRPC_MSG_INVALID_REQUEST, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(null, ResponseId).

test_invalid_request_error_code() ->
    Response = erlmcp_json_rpc:encode_error_response(null, ?JSONRPC_INVALID_REQUEST, ?JSONRPC_MSG_INVALID_REQUEST, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    ErrorCode = maps:get(<<"code">>, maps:get(<<"error">>, Decoded)),
    ?assertEqual(?JSONRPC_INVALID_REQUEST, ErrorCode).

test_invalid_request_message() ->
    Response = erlmcp_json_rpc:encode_error_response(null, ?JSONRPC_INVALID_REQUEST, ?JSONRPC_MSG_INVALID_REQUEST, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    Message = maps:get(<<"message">>, maps:get(<<"error">>, Decoded)),
    ?assertEqual(?JSONRPC_MSG_INVALID_REQUEST, Message).

%%====================================================================
%% Test Group 4: Method Not Found Should Include Request ID
%%====================================================================

method_not_found_preserves_id_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_method_not_found_with_integer_id()),
             ?_test(test_method_not_found_with_string_id()),
             ?_test(test_method_not_found_with_binary_id()),
             ?_test(test_method_not_found_includes_method_in_data())
         ]
     end}.

test_method_not_found_with_integer_id() ->
    RequestId = 456,
    Response = erlmcp_json_rpc:error_method_not_found(RequestId, <<"resources/invalid">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(RequestId, ResponseId).

test_method_not_found_with_string_id() ->
    RequestId = <<"request-123">>,
    Response = erlmcp_json_rpc:error_method_not_found(RequestId, <<"tools/bad">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(RequestId, ResponseId).

test_method_not_found_with_binary_id() ->
    RequestId = <<"unique-req-id">>,
    Response = erlmcp_json_rpc:error_method_not_found(RequestId, <<"prompts/missing">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(RequestId, ResponseId).

test_method_not_found_includes_method_in_data() ->
    MethodName = <<"nonexistent/method">>,
    Response = erlmcp_json_rpc:error_method_not_found(1, MethodName),
    Decoded = jsx:decode(Response, [return_maps]),
    Data = maps:get(<<"data">>, maps:get(<<"error">>, Decoded)),
    ?assertEqual(MethodName, maps:get(<<"method">>, Data)).

%%====================================================================
%% Test Group 5: Invalid Params Should Include Request ID
%%====================================================================

invalid_params_preserves_id_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_invalid_params_with_integer_id()),
             ?_test(test_invalid_params_with_string_id()),
             ?_test(test_invalid_params_with_complex_id()),
             ?_test(test_invalid_params_includes_details_in_data())
         ]
     end}.

test_invalid_params_with_integer_id() ->
    RequestId = 789,
    Response = erlmcp_json_rpc:error_invalid_params(RequestId, <<"Missing required field">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(RequestId, ResponseId).

test_invalid_params_with_string_id() ->
    RequestId = <<"params-request">>,
    Response = erlmcp_json_rpc:error_invalid_params(RequestId, <<"Invalid argument">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(RequestId, ResponseId).

test_invalid_params_with_complex_id() ->
    RequestId = <<"complex-id-with-special-chars">>,
    Response = erlmcp_json_rpc:error_invalid_params(RequestId, <<"Validation error">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(RequestId, ResponseId).

test_invalid_params_includes_details_in_data() ->
    Details = <<"Field 'x' must be positive">>,
    Response = erlmcp_json_rpc:error_invalid_params(1, Details),
    Decoded = jsx:decode(Response, [return_maps]),
    Data = maps:get(<<"data">>, maps:get(<<"error">>, Decoded)),
    ?assertEqual(Details, maps:get(<<"details">>, Data)).

%%====================================================================
%% Test Group 6: ID Matching Original Request
%%====================================================================

id_matching_original_request_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_id_roundtrip_integer()),
             ?_test(test_id_roundtrip_string()),
             ?_test(test_id_roundtrip_large_integer()),
             ?_test(test_id_roundtrip_special_chars_in_string()),
             ?_test(test_multiple_requests_maintain_correct_ids())
         ]
     end}.

test_id_roundtrip_integer() ->
    OriginalId = 42,
    Response = erlmcp_json_rpc:error_invalid_params(OriginalId, <<"Error">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(OriginalId, ResponseId).

test_id_roundtrip_string() ->
    OriginalId = <<"request-xyz">>,
    Response = erlmcp_json_rpc:error_resource_not_found(OriginalId, <<"file://missing">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(OriginalId, ResponseId).

test_id_roundtrip_large_integer() ->
    OriginalId = 9999999999,
    Response = erlmcp_json_rpc:error_tool_not_found(OriginalId, <<"tool">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(OriginalId, ResponseId).

test_id_roundtrip_special_chars_in_string() ->
    OriginalId = <<"request-123-abc_XYZ.test">>,
    Response = erlmcp_json_rpc:error_prompt_not_found(OriginalId, <<"prompt">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(OriginalId, ResponseId).

test_multiple_requests_maintain_correct_ids() ->
    Ids = [1, <<"id-2">>, 3, <<"id-4">>, 5],
    Responses = [
        erlmcp_json_rpc:error_method_not_found(Id, <<"method">>) || Id <- Ids
    ],
    DecodedIds = [
        maps:get(<<"id">>, jsx:decode(R, [return_maps])) || R <- Responses
    ],
    ?assertEqual(Ids, DecodedIds).

%%====================================================================
%% Test Group 7: Encode Error Response with Explicit ID Parameter
%%====================================================================

encode_error_response_with_id_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_encode_error_response_preserves_integer_id()),
             ?_test(test_encode_error_response_preserves_string_id()),
             ?_test(test_encode_error_response_preserves_null_id()),
             ?_test(test_encode_error_response_with_all_params())
         ]
     end}.

test_encode_error_response_preserves_integer_id() ->
    Id = 555,
    Response = erlmcp_json_rpc:encode_error_response(Id, -32602, <<"Invalid">>, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

test_encode_error_response_preserves_string_id() ->
    Id = <<"test-request">>,
    Response = erlmcp_json_rpc:encode_error_response(Id, -32602, <<"Invalid">>, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

test_encode_error_response_preserves_null_id() ->
    Id = null,
    Response = erlmcp_json_rpc:encode_error_response(Id, -32602, <<"Invalid">>, undefined),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(null, maps:get(<<"id">>, Decoded)).

test_encode_error_response_with_all_params() ->
    Id = 999,
    Code = -32001,
    Message = <<"Resource not found">>,
    Data = #{<<"uri">> => <<"file://missing">>},
    Response = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(Code, maps:get(<<"code">>, Error)),
    ?assertEqual(Message, maps:get(<<"message">>, Error)),
    ?assertEqual(Data, maps:get(<<"data">>, Error)).

%%====================================================================
%% Test Group 8: Edge Cases and Boundary Conditions
%%====================================================================

edge_case_tests_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_id_with_empty_string()),
             ?_test(test_id_with_unicode_string()),
             ?_test(test_id_with_very_long_string()),
             ?_test(test_id_zero()),
             ?_test(test_id_negative_integer()),
             ?_test(test_error_with_and_without_data_preserves_id()),
             ?_test(test_concurrent_requests_maintain_ids())
         ]
     end}.

test_id_with_empty_string() ->
    Response = erlmcp_json_rpc:error_invalid_params(<<"">>, <<"Error">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(<<"">>, ResponseId).

test_id_with_unicode_string() ->
    UnicodeId = <<"request-🚀-中文">>,
    Response = erlmcp_json_rpc:error_invalid_params(UnicodeId, <<"Error">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(UnicodeId, ResponseId).

test_id_with_very_long_string() ->
    LongId = binary:copy(<<"x">>, 1000),
    Response = erlmcp_json_rpc:error_invalid_params(LongId, <<"Error">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ResponseId = maps:get(<<"id">>, Decoded),
    ?assertEqual(LongId, ResponseId).

test_id_zero() ->
    Response = erlmcp_json_rpc:error_invalid_params(0, <<"Error">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(0, maps:get(<<"id">>, Decoded)).

test_id_negative_integer() ->
    Response = erlmcp_json_rpc:error_invalid_params(-999, <<"Error">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertEqual(-999, maps:get(<<"id">>, Decoded)).

test_error_with_and_without_data_preserves_id() ->
    Id = 777,
    Response1 = erlmcp_json_rpc:encode_error_response(Id, -32602, <<"Error">>, undefined),
    Response2 = erlmcp_json_rpc:encode_error_response(Id, -32602, <<"Error">>, #{<<"key">> => <<"value">>}),
    Decoded1 = jsx:decode(Response1, [return_maps]),
    Decoded2 = jsx:decode(Response2, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded1)),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded2)).

test_concurrent_requests_maintain_ids() ->
    %% Simulate multiple concurrent errors
    Requests = [
        {1, <<"method-1">>},
        {2, <<"method-2">>},
        {3, <<"method-3">>},
        {<<"req-4">>, <<"method-4">>},
        {<<"req-5">>, <<"method-5">>}
    ],
    Responses = [
        erlmcp_json_rpc:error_method_not_found(Id, Method) || {Id, Method} <- Requests
    ],
    DecodedIds = [
        maps:get(<<"id">>, jsx:decode(R, [return_maps])) || R <- Responses
    ],
    ExpectedIds = [Id || {Id, _} <- Requests],
    ?assertEqual(ExpectedIds, DecodedIds).

%%====================================================================
%% Test Group 9: JSON-RPC 2.0 Compliance
%%====================================================================

jsonrpc_compliance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_error_response_has_jsonrpc_field()),
             ?_test(test_error_response_has_id_field()),
             ?_test(test_error_response_has_error_field()),
             ?_test(test_error_response_no_result_field()),
             ?_test(test_error_response_structure_is_valid_json()),
             ?_test(test_error_id_before_error_field_in_response())
         ]
     end}.

test_error_response_has_jsonrpc_field() ->
    Response = erlmcp_json_rpc:error_invalid_params(1, <<"Error">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assert(maps:is_key(<<"jsonrpc">>, Decoded)),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)).

test_error_response_has_id_field() ->
    Response = erlmcp_json_rpc:error_invalid_params(1, <<"Error">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assert(maps:is_key(<<"id">>, Decoded)).

test_error_response_has_error_field() ->
    Response = erlmcp_json_rpc:error_invalid_params(1, <<"Error">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assert(maps:is_key(<<"error">>, Decoded)).

test_error_response_no_result_field() ->
    Response = erlmcp_json_rpc:error_invalid_params(1, <<"Error">>),
    Decoded = jsx:decode(Response, [return_maps]),
    ?assertNot(maps:is_key(<<"result">>, Decoded)).

test_error_response_structure_is_valid_json() ->
    Response = erlmcp_json_rpc:error_invalid_params(1, <<"Error">>),
    %% Should not throw
    Decoded = jsx:decode(Response, [return_maps]),
    ?assert(is_map(Decoded)).

test_error_id_before_error_field_in_response() ->
    Response = erlmcp_json_rpc:error_invalid_params(42, <<"Error">>),
    Decoded = jsx:decode(Response, [return_maps]),
    Id = maps:get(<<"id">>, Decoded),
    ?assertEqual(42, Id),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(is_map(Error)).

%%====================================================================
%% Test Group 10: Real-World Scenarios
%%====================================================================

real_world_scenarios_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_sequence_of_method_not_found_errors()),
             ?_test(test_sequence_of_resource_errors()),
             ?_test(test_mixed_error_types_with_ids()),
             ?_test(test_error_response_chain())
         ]
     end}.

test_sequence_of_method_not_found_errors() ->
    Methods = [<<"resources/list">>, <<"tools/call">>, <<"prompts/get">>],
    Ids = [1, 2, 3],
    Responses = [
        erlmcp_json_rpc:error_method_not_found(Id, Method) ||
        {Id, Method} <- lists:zip(Ids, Methods)
    ],
    DecodedIds = [
        maps:get(<<"id">>, jsx:decode(R, [return_maps])) || R <- Responses
    ],
    ?assertEqual(Ids, DecodedIds).

test_sequence_of_resource_errors() ->
    Uris = [<<"file://missing1">>, <<"file://missing2">>, <<"file://missing3">>],
    Ids = [100, 101, 102],
    Responses = [
        erlmcp_json_rpc:error_resource_not_found(Id, Uri) ||
        {Id, Uri} <- lists:zip(Ids, Uris)
    ],
    DecodedIds = [
        maps:get(<<"id">>, jsx:decode(R, [return_maps])) || R <- Responses
    ],
    ?assertEqual(Ids, DecodedIds).

test_mixed_error_types_with_ids() ->
    TestCases = [
        {1, erlmcp_json_rpc:error_method_not_found(1, <<"method">>)},
        {2, erlmcp_json_rpc:error_invalid_params(2, <<"params">>)},
        {3, erlmcp_json_rpc:error_resource_not_found(3, <<"uri">>)},
        {4, erlmcp_json_rpc:error_tool_not_found(4, <<"tool">>)},
        {5, erlmcp_json_rpc:error_prompt_not_found(5, <<"prompt">>)}
    ],
    Results = [
        begin
            Decoded = jsx:decode(Response, [return_maps]),
            ResponseId = maps:get(<<"id">>, Decoded),
            ResponseId =:= ExpectedId
        end || {ExpectedId, Response} <- TestCases
    ],
    ?assert(lists:all(fun(R) -> R end, Results)).

test_error_response_chain() ->
    %% Simulate a chain of requests and errors
    RequestIds = [<<"req-1">>, <<"req-2">>, <<"req-3">>, <<"req-4">>, <<"req-5">>],
    Methods = [
        <<"resources/list">>,
        <<"tools/call">>,
        <<"prompts/get">>,
        <<"invalid">>,
        <<"another/invalid">>
    ],
    Responses = [
        erlmcp_json_rpc:error_method_not_found(Id, Method) ||
        {Id, Method} <- lists:zip(RequestIds, Methods)
    ],
    DecodedIds = [
        maps:get(<<"id">>, jsx:decode(R, [return_maps])) || R <- Responses
    ],
    ?assertEqual(RequestIds, DecodedIds).
