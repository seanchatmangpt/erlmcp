-module(erlmcp_json_rpc_encoding_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for JSON-RPC 2.0 Encoding/Decoding
%% Chicago School TDD: Test ONLY observable behavior through public API
%% Focus: encode_request, encode_response, encode_notification, decode_message
%%====================================================================

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Request Encoding Tests
%%====================================================================

encode_request_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_encode_request_with_id_and_params()),
             ?_test(test_encode_request_with_string_id()),
             ?_test(test_encode_request_empty_params()),
             ?_test(test_encode_request_with_object_params()),
             ?_test(test_encode_request_with_array_params())
         ]
     end}.

test_encode_request_with_id_and_params() ->
    Id = 1,
    Method = <<"initialize">>,
    Params = #{<<"version">> => <<"1.0">>},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)).

test_encode_request_with_string_id() ->
    Id = <<"req-123">>,
    Method = <<"list_resources">>,
    Params = #{},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

test_encode_request_empty_params() ->
    Id = 2,
    Method = <<"resources/list">>,
    Params = #{},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)).

test_encode_request_with_object_params() ->
    Id = 3,
    Method = <<"tools/call">>,
    Params = #{<<"name">> => <<"calc">>, <<"args">> => #{<<"x">> => 10, <<"y">> => 20}},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    DecodedParams = maps:get(<<"params">>, Decoded),
    ?assertEqual(<<"calc">>, maps:get(<<"name">>, DecodedParams)).

test_encode_request_with_array_params() ->
    Id = 4,
    Method = <<"batch">>,
    Params = [1, 2, 3, <<"test">>],
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(is_list(maps:get(<<"params">>, Decoded))).

%%====================================================================
%% Response Encoding Tests
%%====================================================================

encode_response_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_encode_response_simple()),
             ?_test(test_encode_response_with_object()),
             ?_test(test_encode_response_with_null()),
             ?_test(test_encode_response_with_array()),
             ?_test(test_encode_response_with_string_id())
         ]
     end}.

test_encode_response_simple() ->
    Id = 1,
    ResultVal = <<"success">>,
    Encoded = erlmcp_json_rpc:encode_response(Id, ResultVal),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(ResultVal, maps:get(<<"result">>, Decoded)),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

test_encode_response_with_object() ->
    Id = 2,
    ResultVal = #{<<"status">> => <<"ok">>, <<"message">> => <<"done">>},
    Encoded = erlmcp_json_rpc:encode_response(Id, ResultVal),
    Decoded = jsx:decode(Encoded, [return_maps]),
    DecodedResult = maps:get(<<"result">>, Decoded),
    ?assertEqual(<<"ok">>, maps:get(<<"status">>, DecodedResult)).

test_encode_response_with_null() ->
    Id = 3,
    Encoded = erlmcp_json_rpc:encode_response(Id, null),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(null, maps:get(<<"result">>, Decoded)).

test_encode_response_with_array() ->
    Id = 4,
    ResultVal = [1, 2, 3, <<"test">>],
    Encoded = erlmcp_json_rpc:encode_response(Id, ResultVal),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assert(is_list(maps:get(<<"result">>, Decoded))).

test_encode_response_with_string_id() ->
    Id = <<"response-456">>,
    ResultVal = #{<<"ok">> => true},
    Encoded = erlmcp_json_rpc:encode_response(Id, ResultVal),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

%%====================================================================
%% Notification Encoding Tests
%%====================================================================

encode_notification_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_encode_notification_basic()),
             ?_test(test_encode_notification_with_params()),
             ?_test(test_encode_notification_no_id())
         ]
     end}.

test_encode_notification_basic() ->
    Method = <<"resource/changed">>,
    Params = #{<<"uri">> => <<"resource://test">>},
    Encoded = erlmcp_json_rpc:encode_notification(Method, Params),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)),
    ?assertNot(maps:is_key(<<"id">>, Decoded)).

test_encode_notification_with_params() ->
    Method = <<"tool/progress">>,
    Params = #{<<"progress">> => 50, <<"total">> => 100},
    Encoded = erlmcp_json_rpc:encode_notification(Method, Params),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)).

test_encode_notification_no_id() ->
    Method = <<"initialized">>,
    Params = #{},
    Encoded = erlmcp_json_rpc:encode_notification(Method, Params),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertNot(maps:is_key(<<"id">>, Decoded)).

%%====================================================================
%% Decoding Tests
%%====================================================================

decode_message_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_decode_request()),
             ?_test(test_decode_response()),
             ?_test(test_decode_error_response()),
             ?_test(test_decode_notification()),
             ?_test(test_decode_invalid_json()),
             ?_test(test_decode_incomplete_message())
         ]
     end}.

test_decode_request() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{}
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, _}, Result).

test_decode_response() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => <<"ok">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, _}, Result).

test_decode_error_response() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"error">> => #{
            <<"code">> => -32600,
            <<"message">> => <<"Invalid Request">>
        }
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, _}, Result).

test_decode_notification() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"initialized">>,
        <<"params">> => #{}
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, _}, Result).

test_decode_invalid_json() ->
    InvalidJson = <<"not valid json {">>,
    Result = erlmcp_json_rpc:decode_message(InvalidJson),
    ?assertMatch({error, _}, Result).

test_decode_incomplete_message() ->
    IncompleteJson = jsx:encode(#{<<"id">> => 1}),
    Result = erlmcp_json_rpc:decode_message(IncompleteJson),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% Batch Operations Tests
%%====================================================================

batch_operations_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_is_batch_request_true()),
             ?_test(test_is_batch_request_false()),
             ?_test(test_encode_batch()),
             ?_test(test_decode_batch())
         ]
     end}.

test_is_batch_request_true() ->
    Json = <<"[{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}]">>,
    ?assert(erlmcp_json_rpc:is_batch_request(Json)).

test_is_batch_request_false() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
    ?assertNot(erlmcp_json_rpc:is_batch_request(Json)).

test_encode_batch() ->
    Requests = [
        #json_rpc_request{id = 1, method = <<"method1">>, params = #{}},
        #json_rpc_request{id = 2, method = <<"method2">>, params = #{}}
    ],
    Result = erlmcp_json_rpc:encode_batch(Requests),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0).

test_decode_batch() ->
    Json = jsx:encode([
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 1,
            <<"method">> => <<"test1">>,
            <<"params">> => #{}
        },
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 2,
            <<"method">> => <<"test2">>,
            <<"params">> => #{}
        }
    ]),
    Result = erlmcp_json_rpc:decode_batch(Json),
    ?assertMatch({ok, _}, Result).

%%====================================================================
%% Edge Cases: Unicode and Special Values
%%====================================================================

encoding_edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_encode_unicode_content()),
             ?_test(test_encode_with_nested_maps()),
             ?_test(test_encode_with_large_array()),
             ?_test(test_encode_with_special_floats())
         ]
     end}.

test_encode_unicode_content() ->
    Result = erlmcp_json_rpc:encode_request(1, <<"test">>, #{<<"text">> => <<"Привет мир">>}),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"test">>, maps:get(<<"method">>, Decoded)).

test_encode_with_nested_maps() ->
    Params = #{
        <<"level1">> => #{
            <<"level2">> => #{
                <<"level3">> => <<"deep">>
            }
        }
    },
    Result = erlmcp_json_rpc:encode_request(1, <<"test">>, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(maps:is_key(<<"params">>, Decoded)).

test_encode_with_large_array() ->
    Array = lists:seq(1, 1000),
    Result = erlmcp_json_rpc:encode_request(1, <<"test">>, Array),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(is_list(maps:get(<<"params">>, Decoded))).

test_encode_with_special_floats() ->
    Result = erlmcp_json_rpc:encode_response(1, 3.14159),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(3.14159, maps:get(<<"result">>, Decoded)).

%%====================================================================
%% Decoding Edge Cases
%%====================================================================

decoding_edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_decode_unicode_content()),
             ?_test(test_decode_empty_string()),
             ?_test(test_decode_with_null_values()),
             ?_test(test_decode_batch_single_request()),
             ?_test(test_decode_response_with_null_result()),
             ?_test(test_decode_with_extra_fields())
         ]
     end}.

test_decode_unicode_content() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{<<"text">> => <<"Hello 世界 🌍">>}
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

test_decode_empty_string() ->
    Result = erlmcp_json_rpc:decode_message(<<"">>),
    ?assertMatch({error, _}, Result).

test_decode_with_null_values() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => null
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

test_decode_batch_single_request() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>
    }),
    Result = erlmcp_json_rpc:decode_batch(Json),
    ?assertMatch({ok, [_]}, Result).

test_decode_response_with_null_result() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => null
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_response{result = null}}, Result).

test_decode_with_extra_fields() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{},
        <<"extraField">> => <<"ignored">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{}}, Result).
