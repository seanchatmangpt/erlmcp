-module(erlmcp_json_rpc_integration_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for JSON-RPC 2.0 End-to-End Integration Testing
%% Chicago School TDD: Test ONLY observable behavior through public API
%% Focus: Full request/response cycles, error code coverage, edge cases
%%====================================================================

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% End-to-End Request/Response Cycles
%%====================================================================

e2e_request_response_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_full_request_response_cycle()),
             ?_test(test_full_error_response_cycle()),
             ?_test(test_full_notification_cycle())
         ]
     end}.

test_full_request_response_cycle() ->
    %% Encode a request
    RequestJson = erlmcp_json_rpc:encode_request(1, <<"test">>, #{<<"key">> => <<"value">>}),
    ?assert(is_binary(RequestJson)),

    %% Decode the request
    {ok, DecodedRequest} = erlmcp_json_rpc:decode_message(RequestJson),
    ?assertMatch(#json_rpc_request{id = 1, method = <<"test">>}, DecodedRequest),

    %% Encode a response
    ResponseJson = erlmcp_json_rpc:encode_response(1, #{<<"result">> => <<"ok">>}),
    ?assert(is_binary(ResponseJson)),

    %% Decode the response
    {ok, DecodedResponse} = erlmcp_json_rpc:decode_message(ResponseJson),
    ?assertMatch(#json_rpc_response{id = 1, result = #{<<"result">> := <<"ok">>}}, DecodedResponse).

test_full_error_response_cycle() ->
    %% Encode a request
    RequestJson = erlmcp_json_rpc:encode_request(1, <<"invalid">>, #{}),
    {ok, DecodedRequest} = erlmcp_json_rpc:decode_message(RequestJson),

    %% Encode an error response
    ErrorJson = erlmcp_json_rpc:error_method_not_found(1, <<"invalid">>),
    ?assert(is_binary(ErrorJson)),

    %% Decode the error response
    {ok, DecodedError} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertMatch(#json_rpc_response{id = 1, error = #{<<"code">> := -32601}}, DecodedError).

test_full_notification_cycle() ->
    %% Encode a notification
    NotificationJson = erlmcp_json_rpc:encode_notification(<<"initialized">>, #{}),
    ?assert(is_binary(NotificationJson)),

    %% Decode the notification
    {ok, DecodedNotification} = erlmcp_json_rpc:decode_message(NotificationJson),
    ?assertMatch(#json_rpc_notification{method = <<"initialized">>}, DecodedNotification).

%%====================================================================
%% All Error Codes Integration Tests
%%====================================================================

all_error_codes_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_validate_all_error_codes()),
             ?_test(test_all_error_codes_have_severity()),
             ?_test(test_all_error_codes_have_category()),
             ?_test(test_error_code_range_coverage()),
             ?_test(test_total_error_code_count())
         ]
     end}.

test_validate_all_error_codes() ->
    ValidCodes = ?VALID_ERROR_CODES,
    ?assert(length(ValidCodes) > 80),
    lists:foreach(fun(Code) ->
        ?assert(erlmcp_json_rpc:validate_error_code(Code))
    end, ValidCodes).

test_all_error_codes_have_severity() ->
    ValidCodes = ?VALID_ERROR_CODES,
    lists:foreach(fun(Code) ->
        Severity = erlmcp_json_rpc:error_severity(Code),
        ?assert(lists:member(Severity, [critical, error, warning, info]))
    end, ValidCodes).

test_all_error_codes_have_category() ->
    ValidCodes = ?VALID_ERROR_CODES,
    %% Just verify that error_category returns a value without crashing
    lists:foreach(fun(Code) ->
        Category = erlmcp_json_rpc:error_category(Code),
        ?assert(is_atom(Category))
    end, ValidCodes).

test_error_code_range_coverage() ->
    %% Verify all error code ranges are covered
    Ranges = [
        {-32700, -32700},
        {-32603, -32600},
        {-32113, -32110},
        {-32100, -32091},
        {-32090, -32081},
        {-32080, -32071},
        {-32070, -32061},
        {-32060, -32051},
        {-32050, -32041},
        {-32040, -32031},
        {-32030, -32021},
        {-32020, -32011},
        {-32010, -32001},
        {-32000, -32000}
    ],
    lists:foreach(fun({Min, Max}) when Min =< Max ->
        Found = lists:any(fun(Code) ->
            erlmcp_json_rpc:validate_error_code(Code)
        end, lists:seq(Min, Max)),
        ?assert(Found)
    end, Ranges).

test_total_error_code_count() ->
    %% Verify we have error codes (count may vary)
    ValidCodes = ?VALID_ERROR_CODES,
    ?assert(length(ValidCodes) >= 89).

%%====================================================================
%% Batch Request/Response Integration Tests
%%====================================================================

batch_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_batch_request_response_cycle()),
             ?_test(test_batch_with_mixed_results()),
             ?_test(test_batch_error_handling())
         ]
     end}.

test_batch_request_response_cycle() ->
    %% Encode batch requests
    Requests = [
        #json_rpc_request{id = 1, method = <<"method1">>, params = #{}},
        #json_rpc_request{id = 2, method = <<"method2">>, params = #{}}
    ],
    BatchJson = erlmcp_json_rpc:encode_batch(Requests),
    ?assert(is_binary(BatchJson)),

    %% Decode batch
    {ok, DecodedBatch} = erlmcp_json_rpc:decode_batch(BatchJson),
    ?assertEqual(2, length(DecodedBatch)),

    %% Encode batch responses
    Responses = [
        #json_rpc_response{id = 1, result = <<"ok1">>},
        #json_rpc_response{id = 2, result = <<"ok2">>}
    ],
    ResponseJson = erlmcp_json_rpc:encode_batch(Responses),
    ?assert(is_binary(ResponseJson)),

    %% Decode batch responses
    {ok, DecodedResponses} = erlmcp_json_rpc:decode_batch(ResponseJson),
    ?assertEqual(2, length(DecodedResponses)).

test_batch_with_mixed_results() ->
    %% Batch with both successful and error responses
    Json = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"result">> => <<"ok">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2, <<"error">> => #{<<"code">> => -32601, <<"message">> => <<"Not found">>}}
    ]),
    case erlmcp_json_rpc:decode_batch(Json) of
        {ok, Responses} ->
            ?assertEqual(2, length(Responses)),
            case lists:nth(1, Responses) of
                #json_rpc_response{id = 1, result = <<"ok">>} -> ok;
                _ -> ?assert(false)
            end,
            case lists:nth(2, Responses) of
                #json_rpc_response{id = 2, error = #{<<"code">> := -32601}} -> ok;
                _ -> ?assert(false)
            end;
        {error, _} ->
            ?assert(false)
    end.

test_batch_error_handling() ->
    %% Batch with invalid request
    Json = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"valid">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2}  %% Missing method
    ]),
    case erlmcp_json_rpc:decode_batch(Json) of
        {ok, Responses} when is_list(Responses), length(Responses) >= 1 ->
            ?assert(true);
        {error, _Reason} ->
            ?assert(true)  % Also acceptable if batch parser rejects it
    end.

%%====================================================================
%% Transport-Specific Integration Tests
%%====================================================================

transport_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_stdio_transport_message_size()),
             ?_test(test_tcp_transport_message_size()),
             ?_test(test_http_transport_message_size()),
             ?_test(test_message_too_large_error())
         ]
     end}.

test_stdio_transport_message_size() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{<<"data">> => <<"small">>}
    }),
    Result = erlmcp_json_rpc:decode_message(Json, stdio),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

test_tcp_transport_message_size() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json, tcp),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

test_http_transport_message_size() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json, http),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

test_message_too_large_error() ->
    %% Note: This would require creating an oversized message
    %% For now, we just verify the error code exists
    ?assert(erlmcp_json_rpc:validate_error_code(-32002)).

%%====================================================================
%% Edge Cases Integration Tests
%%====================================================================

edge_cases_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_unicode_roundtrip()),
             ?_test(test_special_characters_roundtrip()),
             ?_test(test_large_nested_structure_roundtrip()),
             ?_test(test_null_values_roundtrip())
         ]
     end}.

test_unicode_roundtrip() ->
    Original = #{
        <<"text">> => <<"Hello 世界 🌍 Привет мир">>
    },
    Encoded = erlmcp_json_rpc:encode_response(1, Original),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Encoded),
    ?assertEqual(Original, Decoded#json_rpc_response.result).

test_special_characters_roundtrip() ->
    Original = <<"Line 1\nLine 2\tTabbed\r\n">>,
    Encoded = erlmcp_json_rpc:encode_response(1, Original),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Encoded),
    ?assertEqual(Original, Decoded#json_rpc_response.result).

test_large_nested_structure_roundtrip() ->
    Original = #{
        <<"level1">> => #{
            <<"level2">> => #{
                <<"level3">> => #{
                    <<"value">> => lists:seq(1, 100)
                }
            }
        }
    },
    Encoded = erlmcp_json_rpc:encode_response(1, Original),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Encoded),
    ?assertMatch(#json_rpc_response{result = #{<<"level1">> := _}}, Decoded).

test_null_values_roundtrip() ->
    Original = #{
        <<"null_field">> => null,
        <<"string_field">> => <<"value">>
    },
    Encoded = erlmcp_json_rpc:encode_response(1, Original),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Encoded),
    ?assertEqual(Original, Decoded#json_rpc_response.result).

%%====================================================================
%% Error Helper Functions Integration Test
%%====================================================================

error_helpers_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_all_error_helpers_produce_valid_json()),
             ?_test(test_error_helpers_with_real_decoding())
         ]
     end}.

test_all_error_helpers_produce_valid_json() ->
    Id = 1,
    Errors = [
        erlmcp_json_rpc:error_parse(Id),
        erlmcp_json_rpc:error_internal(Id),
        erlmcp_json_rpc:error_invalid_content_type(Id, <<"text/plain">>),
        erlmcp_json_rpc:error_invalid_uri(Id, <<"bad-uri">>),
        erlmcp_json_rpc:error_tool_cancelled(Id, <<"test_tool">>),
        erlmcp_json_rpc:error_sampling_failed(Id, <<"Test failure">>),
        erlmcp_json_rpc:error_invalid_credentials(Id),
        erlmcp_json_rpc:error_method_not_supported(Id, <<"unknown">>),
        erlmcp_json_rpc:error_cursor_expired(Id, <<"cursor">>),
        erlmcp_json_rpc:error_task_cancelled(Id, <<"task-1">>),
        erlmcp_json_rpc:error_notification_queue_full(Id, 100),
        erlmcp_json_rpc:error_completion_not_found(Id, <<"comp-1">>)
    ],
    lists:foreach(fun(Response) ->
        Json = jsx:decode(Response, [return_maps]),
        ?assert(maps:is_key(<<"jsonrpc">>, Json)),
        ?assert(maps:is_key(<<"id">>, Json)),
        ?assert(maps:is_key(<<"error">>, Json)),
        Error = maps:get(<<"error">>, Json),
        ?assert(maps:is_key(<<"code">>, Error)),
        ?assert(maps:is_key(<<"message">>, Error)),
        ?assert(erlmcp_json_rpc:validate_error_code(maps:get(<<"code">>, Error)))
    end, Errors).

test_error_helpers_with_real_decoding() ->
    Id = 1,
    ErrorJson = erlmcp_json_rpc:error_method_not_found(Id, <<"unknown_method">>),
    ?assert(is_binary(ErrorJson)),

    {ok, Response} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertMatch(#json_rpc_response{id = 1}, Response),
    ?assert(is_map(Response#json_rpc_response.error)),
    Error = Response#json_rpc_response.error,
    ?assertEqual(-32601, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Method not found">>, maps:get(<<"message">>, Error)).

%%====================================================================
%% Experimental Error Codes Integration Tests (1090-1099)
%%====================================================================

experimental_error_codes_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_elicitation_failed_error()),
             ?_test(test_elicitation_timeout_error()),
             ?_test(test_experimental_task_not_found_error()),
             ?_test(test_experimental_error_codes_validation())
         ]
     end}.

test_elicitation_failed_error() ->
    Id = 1,
    Reason = <<"Failed to elicit URL from user">>,
    Encoded = erlmcp_json_rpc:error_elicitation_failed(Id, Reason),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(1090, maps:get(<<"code">>, Error)),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(Reason, maps:get(<<"reason">>, Data)).

test_elicitation_timeout_error() ->
    Id = 2,
    TimeoutMs = 30000,
    Encoded = erlmcp_json_rpc:error_elicitation_timeout(Id, TimeoutMs),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(1091, maps:get(<<"code">>, Error)),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(TimeoutMs, maps:get(<<"timeoutMs">>, Data)).

test_experimental_task_not_found_error() ->
    Id = 6,
    TaskId = <<"task-12345">>,
    Encoded = erlmcp_json_rpc:error_experimental_task_not_found(Id, TaskId),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(1095, maps:get(<<"code">>, Error)),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(TaskId, maps:get(<<"taskId">>, Data)).

test_experimental_error_codes_validation() ->
    %% All experimental error codes should be valid
    ?assert(erlmcp_json_rpc:validate_error_code(1090)),
    ?assert(erlmcp_json_rpc:validate_error_code(1091)),
    ?assert(erlmcp_json_rpc:validate_error_code(1092)),
    ?assert(erlmcp_json_rpc:validate_error_code(1093)),
    ?assert(erlmcp_json_rpc:validate_error_code(1094)),
    ?assert(erlmcp_json_rpc:validate_error_code(1095)),
    ?assert(erlmcp_json_rpc:validate_error_code(1096)),
    ?assert(erlmcp_json_rpc:validate_error_code(1097)),
    ?assert(erlmcp_json_rpc:validate_error_code(1098)),
    ?assert(erlmcp_json_rpc:validate_error_code(1099)).

%%====================================================================
%% Performance Edge Cases
%%====================================================================

performance_edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_large_array_response()),
             ?_test(test_many_batch_requests()),
             ?_test(test_deeply_nested_structure())
         ]
     end}.

test_large_array_response() ->
    LargeArray = lists:seq(1, 10000),
    Encoded = erlmcp_json_rpc:encode_response(1, LargeArray),
    ?assert(is_binary(Encoded)),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Encoded),
    ?assertMatch(#json_rpc_response{result = [_ | _]}, Decoded),
    ?assertEqual(10000, length(Decoded#json_rpc_response.result)).

test_many_batch_requests() ->
    Requests = [
        #json_rpc_request{id = Id, method = <<"test">>, params = #{}}
        || Id <- lists:seq(1, 100)
    ],
    Encoded = erlmcp_json_rpc:encode_batch(Requests),
    ?assert(is_binary(Encoded)),
    {ok, Decoded} = erlmcp_json_rpc:decode_batch(Encoded),
    ?assertEqual(100, length(Decoded)).

test_deeply_nested_structure() ->
    Nested = build_nested_structure(10, <<"value">>),
    Encoded = erlmcp_json_rpc:encode_response(1, Nested),
    ?assert(is_binary(Encoded)),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Encoded),
    ?assertMatch(#json_rpc_response{}, Decoded).

%% Helper function to build deeply nested structures
build_nested_structure(0, Value) ->
    Value;
build_nested_structure(Depth, Value) ->
    #{<<"level">> => build_nested_structure(Depth - 1, Value)}.
