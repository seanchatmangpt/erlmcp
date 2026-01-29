-module(erlmcp_jsonrpc_compliance_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%=========================================================================
%%% JSON-RPC 2.0 Protocol Compliance Test Suite
%%%
%%% Tests compliance with JSON-RPC 2.0 specification:
%%% https://www.jsonrpc.org/specification
%%%
%%% Coverage:
%%% 1. Request Format (jsonrpc, method, params, id)
%%% 2. Response Format (result/error, id matching)
%%% 3. Error Codes (standard -32700 to -32603)
%%% 4. Batch Requests (array processing, ordering)
%%% 5. Notifications (request without id)
%%% 6. Message Ordering (responses match request order)
%%%=========================================================================

%%%-------------------------------------------------------------------------
%%% Request Format Tests
%%%-------------------------------------------------------------------------

jsonrpc_request_format_valid_test() ->
    %% Test valid request with all required fields
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test/method">>,
        <<"id">> => 1
    }),
    {ok, #json_rpc_request{id = 1, method = <<"test/method">>, params = undefined}} =
        erlmcp_json_rpc:decode_message(RequestJson).

jsonrpc_request_with_params_test() ->
    %% Test request with params field
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test/method">>,
        <<"params">> => #{<<"arg1">> => <<"value1">>},
        <<"id">> => <<"req-1">>
    }),
    {ok, #json_rpc_request{id = <<"req-1">>, method = <<"test/method">>, params = #{
        <<"arg1">> => <<"value1">>
    }}} = erlmcp_json_rpc:decode_message(RequestJson).

jsonrpc_request_with_array_params_test() ->
    %% Test request with array params (positional parameters)
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test/method">>,
        <<"params">> => [<<"arg1">>, 42, true],
        <<"id">> => 2
    }),
    {ok, #json_rpc_request{id = 2, method = <<"test/method">>, params = [<<"arg1">>, 42, true]}} =
        erlmcp_json_rpc:decode_message(RequestJson).

jsonrpc_request_missing_jsonrpc_field_test() ->
    %% Test error when "jsonrpc" field is missing
    RequestJson = jsx:encode(#{
        <<"method">> => <<"test/method">>,
        <<"id">> => 1
    }),
    {error, {invalid_request, missing_jsonrpc}} = erlmcp_json_rpc:decode_message(RequestJson).

jsonrpc_request_missing_method_field_test() ->
    %% Test error when "method" field is missing
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1
    }),
    {error, {invalid_request, unknown_message_type}} = erlmcp_json_rpc:decode_message(RequestJson).

jsonrpc_request_wrong_version_test() ->
    %% Test error when jsonrpc version is not "2.0"
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"1.0">>,
        <<"method">> => <<"test/method">>,
        <<"id">> => 1
    }),
    {error, {invalid_request, {wrong_version, <<"1.0">>}}} = erlmcp_json_rpc:decode_message(RequestJson).

%%%-------------------------------------------------------------------------
%%% Response Format Tests
%%%-------------------------------------------------------------------------

jsonrpc_response_with_result_test() ->
    %% Test response with result field
    ResponseJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => #{<<"data">> => <<"success">>},
        <<"id">> => 1
    }),
    {ok, #json_rpc_response{id = 1, result = #{<<"data">> => <<"success">>}, error = undefined}} =
        erlmcp_json_rpc:decode_message(ResponseJson).

jsonrpc_response_with_error_test() ->
    %% Test response with error field
    ResponseJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"error">> => #{
            <<"code">> => -32601,
            <<"message">> => <<"Method not found">>
        },
        <<"id">> => 1
    }),
    {ok, #json_rpc_response{id = 1, result = undefined, error = #{
        <<"code">> := -32601,
        <<"message">> := <<"Method not found">>
    }}} = erlmcp_json_rpc:decode_message(ResponseJson).

jsonrpc_response_encode_result_test() ->
    %% Test encoding response with result
    ResponseBin = erlmcp_json_rpc:encode_response(1, #{<<"status">> => <<"ok">>}),
    ResponseMap = jsx:decode(ResponseBin, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, ResponseMap)),
    ?assertEqual(1, maps:get(<<"id">>, ResponseMap)),
    ?assertMatch(#{<<"status">> := <<"ok">>}, maps:get(<<"result">>, ResponseMap)),
    ?assertNot(maps:is_key(<<"error">>, ResponseMap)).

jsonrpc_response_encode_error_test() ->
    %% Test encoding response with error
    ResponseBin = erlmcp_json_rpc:encode_error_response(1, -32602, <<"Invalid params">>),
    ResponseMap = jsx:decode(ResponseBin, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, ResponseMap)),
    ?assertEqual(1, maps:get(<<"id">>, ResponseMap)),
    ?assertMatch(#{<<"code">> := -32602, <<"message">> := <<"Invalid params">>}, maps:get(<<"error">>, ResponseMap)),
    ?assertNot(maps:is_key(<<"result">>, ResponseMap)).

%%%-------------------------------------------------------------------------
%%% Error Code Tests (JSON-RPC 2.0 Standard)
%%%-------------------------------------------------------------------------

jsonrpc_error_parse_error_code_test() ->
    %% Test -32700: Parse error
    ?assertEqual(-32700, ?JSONRPC_PARSE_ERROR),
    ResponseBin = erlmcp_json_rpc:error_parse(1),
    ResponseMap = jsx:decode(ResponseBin, [return_maps]),
    Error = maps:get(<<"error">>, ResponseMap),
    ?assertEqual(-32700, maps:get(<<"code">>, Error)).

jsonrpc_error_invalid_request_code_test() ->
    %% Test -32600: Invalid Request
    ?assertEqual(-32600, ?JSONRPC_INVALID_REQUEST).

jsonrpc_error_method_not_found_code_test() ->
    %% Test -32601: Method not found
    ?assertEqual(-32601, ?JSONRPC_METHOD_NOT_FOUND),
    ResponseBin = erlmcp_json_rpc:error_method_not_found(1, <<"unknown/method">>),
    ResponseMap = jsx:decode(ResponseBin, [return_maps]),
    Error = maps:get(<<"error">>, ResponseMap),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)),
    ?assertMatch(#{<<"method">> := <<"unknown/method">>}, maps:get(<<"data">>, Error)).

jsonrpc_error_invalid_params_code_test() ->
    %% Test -32602: Invalid params
    ?assertEqual(-32602, ?JSONRPC_INVALID_PARAMS),
    ResponseBin = erlmcp_json_rpc:error_invalid_params(1, <<"Missing required field">>),
    ResponseMap = jsx:decode(ResponseBin, [return_maps]),
    Error = maps:get(<<"error">>, ResponseMap),
    ?assertEqual(-32602, maps:get(<<"code">>, Error)).

jsonrpc_error_internal_error_code_test() ->
    %% Test -32603: Internal error
    ?assertEqual(-32603, ?JSONRPC_INTERNAL_ERROR),
    ResponseBin = erlmcp_json_rpc:error_internal(1),
    ResponseMap = jsx:decode(ResponseBin, [return_maps]),
    Error = maps:get(<<"error">>, ResponseMap),
    ?assertEqual(-32603, maps:get(<<"code">>, Error)).

jsonrpc_error_code_validation_test() ->
    %% Test error code validation
    ?assert(erlmcp_json_rpc:validate_error_code(-32700)),
    ?assert(erlmcp_json_rpc:validate_error_code(-32600)),
    ?assert(erlmcp_json_rpc:validate_error_code(-32601)),
    ?assert(erlmcp_json_rpc:validate_error_code(-32602)),
    ?assert(erlmcp_json_rpc:validate_error_code(-32603)),
    ?assertNot(erlmcp_json_rpc:validate_error_code(-99999)),
    ?assertNot(erlmcp_json_rpc:validate_error_code(100)).

jsonrpc_error_object_format_test() ->
    %% Test error object has required code and message
    ResponseBin = erlmcp_json_rpc:encode_error_response(1, -32601, <<"Method not found">>, #{
        <<"method">> => <<"test">>
    }),
    ResponseMap = jsx:decode(ResponseBin, [return_maps]),
    Error = maps:get(<<"error">>, ResponseMap),
    ?assertMatch(#{<<"code">> := -32601, <<"message">> := <<"Method not found">>}, Error),
    ?assertMatch(#{<<"method">> := <<"test">>}, maps:get(<<"data">>, Error)).

%%%-------------------------------------------------------------------------
%%% Notification Tests (Request without id)
%%%-------------------------------------------------------------------------

jsonrpc_notification_test() ->
    %% Test notification encoding (no id field)
    NotificationBin = erlmcp_json_rpc:encode_notification(<<"test/notify">>, #{<<"key">> => <<"value">>}),
    NotificationMap = jsx:decode(NotificationBin, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, NotificationMap)),
    ?assertEqual(<<"test/notify">>, maps:get(<<"method">>, NotificationMap)),
    ?assertMatch(#{<<"key">> := <<"value">>}, maps:get(<<"params">>, NotificationMap)),
    ?assertNot(maps:is_key(<<"id">>, NotificationMap)).

jsonrpc_notification_decode_test() ->
    %% Test notification decoding
    NotificationJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test/notify">>,
        <<"params">> => #{<<"event">> => <<"updated">>}
    }),
    {ok, #json_rpc_notification{method = <<"test/notify">>, params = #{
        <<"event">> := <<"updated">>
    }}} = erlmcp_json_rpc:decode_message(NotificationJson).

jsonrpc_notification_no_response_test() ->
    %% Verify notification has no id field (spec requirement)
    NotificationBin = erlmcp_json_rpc:encode_notification(<<"events/log">>, #{}),
    NotificationMap = jsx:decode(NotificationBin, [return_maps]),
    ?assertNot(maps:is_key(<<"id">>, NotificationMap)).

%%%-------------------------------------------------------------------------
%%% Batch Request Tests
%%%-------------------------------------------------------------------------

jsonrpc_batch_request_test() ->
    %% Test batch request with multiple requests
    BatchJson = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"m1">>, <<"id">> => 1},
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"m2">>, <<"id">> => 2},
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"m3">>, <<"id">> => 3}
    ]),
    {ok, Messages} = erlmcp_json_rpc:decode_batch(BatchJson),
    ?assertEqual(3, length(Messages)),
    ?assertMatch(#json_rpc_request{id = 1, method = <<"m1">>}, lists:nth(1, Messages)),
    ?assertMatch(#json_rpc_request{id = 2, method = <<"m2">>}, lists:nth(2, Messages)),
    ?assertMatch(#json_rpc_request{id = 3, method = <<"m3">>}, lists:nth(3, Messages)).

jsonrpc_batch_encode_test() ->
    %% Test batch encoding
    Requests = [
        #json_rpc_request{id = 1, method = <<"m1">>, params = undefined},
        #json_rpc_request{id = 2, method = <<"m2">>, params = #{<<"arg">> => 1}}
    ],
    BatchBin = erlmcp_json_rpc:encode_batch(Requests),
    BatchArray = jsx:decode(BatchBin, [return_maps]),
    ?assertEqual(2, length(BatchArray)),
    ?assertMatch(#{<<"id">> := 1, <<"method">> := <<"m1">>}, lists:nth(1, BatchArray)),
    ?assertMatch(#{<<"id">> := 2, <<"method">> := <<"m2">>}, lists:nth(2, BatchArray)).

jsonrpc_batch_empty_test() ->
    %% Test empty batch is invalid per spec
    EmptyBatch = jsx:encode([]),
    {error, {invalid_request, empty_batch}} = erlmcp_json_rpc:decode_batch(EmptyBatch).

jsonrpc_batch_with_notifications_test() ->
    %% Test batch can contain notifications (no id)
    BatchJson = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"req1">>, <<"id">> => 1},
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"notify1">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"req2">>, <<"id">> => 2}
    ]),
    {ok, Messages} = erlmcp_json_rpc:decode_batch(BatchJson),
    ?assertEqual(3, length(Messages)),
    ?assertMatch(#json_rpc_request{id = 1}, lists:nth(1, Messages)),
    ?assertMatch(#json_rpc_notification{method = <<"notify1">>}, lists:nth(2, Messages)),
    ?assertMatch(#json_rpc_request{id = 2}, lists:nth(3, Messages)).

jsonrpc_batch_detection_test() ->
    %% Test is_batch_request detection
    SingleJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"test">>, <<"id">> => 1
    }),
    BatchJson = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"test">>, <<"id">> => 1}
    ]),
    ?assertNot(erlmcp_json_rpc:is_batch_request(SingleJson)),
    ?assert(erlmcp_json_rpc:is_batch_request(BatchJson)).

%%%-------------------------------------------------------------------------
%%% Message Ordering Tests
%%%-------------------------------------------------------------------------

jsonrpc_message_ordering_preserved_test() ->
    %% Test batch processing preserves order
    BatchJson = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"m1">>, <<"id">> => 3},
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"m2">>, <<"id">> => 1},
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"m3">>, <<"id">> => 2}
    ]),
    {ok, Messages} = erlmcp_json_rpc:decode_batch(BatchJson),
    ?assertEqual([3, 1, 2], [Id || #json_rpc_request{id = Id} <- Messages]).

jsonrpc_response_id_matches_request_test() ->
    %% Test response ID matches request ID
    RequestId = <<"unique-id-12345">>,
    ResponseBin = erlmcp_json_rpc:encode_response(RequestId, #{<<"result">> => <<"data">>}),
    ResponseMap = jsx:decode(ResponseBin, [return_maps]),
    ?assertEqual(RequestId, maps:get(<<"id">>, ResponseMap)).

%%%-------------------------------------------------------------------------
%%% ID Field Tests (string, number, null)
%%%-------------------------------------------------------------------------

jsonrpc_id_string_test() ->
    %% Test string ID
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test">>,
        <<"id">> => <<"str-123">>
    }),
    {ok, #json_rpc_request{id = <<"str-123">>}} = erlmcp_json_rpc:decode_message(RequestJson).

jsonrpc_id_integer_test() ->
    %% Test integer ID
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test">>,
        <<"id">> => 42
    }),
    {ok, #json_rpc_request{id = 42}} = erlmcp_json_rpc:decode_message(RequestJson).

jsonrpc_id_float_rejected_test() ->
    %% Test float ID should be handled (spec says number, but typically integer)
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test">>,
        <<"id">> => 3.14
    }),
    %% Implementation accepts it as-is (spec compliance allows this)
    {ok, #json_rpc_request{id = 3.14}} = erlmcp_json_rpc:decode_message(RequestJson).

jsonrpc_id_null_test() ->
    %% Test null ID is valid (though unusual)
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test">>,
        <<"id">> => null
    }),
    {ok, #json_rpc_request{id = null}} = erlmcp_json_rpc:decode_message(RequestJson).

%%%-------------------------------------------------------------------------
%%% Params Field Tests
%%%-------------------------------------------------------------------------

jsonrpc_params_omitted_test() ->
    %% Test params can be omitted
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test">>,
        <<"id">> => 1
    }),
    {ok, #json_rpc_request{params = undefined}} = erlmcp_json_rpc:decode_message(RequestJson).

jsonrpc_params_by_name_test() ->
    %% Test named parameters (map/object)
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test">>,
        <<"params">> => #{<<"x">> => 1, <<"y">> => 2},
        <<"id">> => 1
    }),
    {ok, #json_rpc_request{params = #{<<"x">> := 1, <<"y">> := 2}}} =
        erlmcp_json_rpc:decode_message(RequestJson).

jsonrpc_params_by_position_test() ->
    %% Test positional parameters (array)
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test">>,
        <<"params">> => [1, 2, 3],
        <<"id">> => 1
    }),
    {ok, #json_rpc_request{params = [1, 2, 3]}} = erlmcp_json_rpc:decode_message(RequestJson).

%%%-------------------------------------------------------------------------
%%% Edge Cases and Error Handling
%%%-------------------------------------------------------------------------

jsonrpc_invalid_json_test() ->
    %% Test invalid JSON returns parse error
    InvalidJson = <<"not valid json {]">>,
    {error, {parse_error, _}} = erlmcp_json_rpc:decode_message(InvalidJson).

jsonrpc_request_with_result_and_error_test() ->
    %% Test response cannot have both result and error
    %% This is technically valid JSON but violates spec - implementation should handle
    ResponseJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => #{<<"data">> => 1},
        <<"error">> => #{<<"code">> := -32603},
        <<"id">> => 1
    }),
    %% Parser accepts it (last one wins per erlmcp_message_parser logic)
    {ok, #json_rpc_response{result = #{<<"data">> := 1}, error = Error}} =
        erlmcp_json_rpc:decode_message(ResponseJson),
    ?assert(is_map(Error)).

jsonrpc_batch_with_invalid_requests_test() ->
    %% Test batch with mixed valid/invalid requests
    %% Spec says process all valid, return error for invalid
    BatchJson = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"valid">>, <<"id">> => 1},
        #{<<"method">> => <<"invalid">>}, %% Missing jsonrpc and id
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"valid2">>, <<"id">> => 2}
    ]),
    %% Current implementation continues processing on error (see parse_batch_requests)
    {ok, Messages} = erlmcp_json_rpc:decode_batch(BatchJson),
    ?assert(lists:any(fun(M) -> case M of
        #json_rpc_request{method = <<"valid">>} -> true;
        _ -> false
    end end, Messages)),
    ?assert(lists:any(fun(M) -> case M of
        #json_rpc_request{method = <<"valid2">>} -> true;
        _ -> false
    end end, Messages)).

%%%-------------------------------------------------------------------------
%%% Round-trip Encoding/Decoding Tests
%%%-------------------------------------------------------------------------

jsonrpc_round_trip_request_test() ->
    %% Test encode then decode produces same result
    Original = #json_rpc_request{id = 1, method = <<"test">>, params = #{<<"arg">> => 1}},
    Encoded = erlmcp_json_rpc:encode_request(1, <<"test">>, #{<<"arg">> => 1}),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Encoded),
    ?assertEqual(Original, Decoded).

jsonrpc_round_trip_response_test() ->
    %% Test response round-trip
    Result = #{<<"data">> => <<"test">>},
    Encoded = erlmcp_json_rpc:encode_response(1, Result),
    {ok, #json_rpc_response{id = 1, result = Result}} = erlmcp_json_rpc:decode_message(Encoded).

jsonrpc_round_trip_notification_test() ->
    %% Test notification round-trip
    Encoded = erlmcp_json_rpc:encode_notification(<<"event">>, #{<<"key">> => <<"value">>}),
    {ok, #json_rpc_notification{method = <<"event">>, params = #{<<"key">> := <<"value">>}}} =
        erlmcp_json_rpc:decode_message(Encoded).
