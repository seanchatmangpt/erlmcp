-module(erlmcp_json_rpc_response_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for JSON-RPC 2.0 Response Generation
%% Chicago School TDD: Test ONLY observable behavior through public API
%% Focus: encode_response, successful responses, result structures
%%====================================================================

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Successful Response Tests
%%====================================================================

successful_response_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_encode_response_with_string_result()),
             ?_test(test_encode_response_with_numeric_result()),
             ?_test(test_encode_response_with_boolean_result()),
             ?_test(test_encode_response_with_null_result()),
             ?_test(test_encode_response_with_object_result()),
             ?_test(test_encode_response_with_array_result())
         ]
     end}.

test_encode_response_with_string_result() ->
    Id = 1,
    Result = <<"operation successful">>,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(Result, maps:get(<<"result">>, Decoded)),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertNot(maps:is_key(<<"error">>, Decoded)).

test_encode_response_with_numeric_result() ->
    Id = 2,
    Result = 42,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(Result, maps:get(<<"result">>, Decoded)),
    ?assertNot(maps:is_key(<<"error">>, Decoded)).

test_encode_response_with_boolean_result() ->
    Id = 3,
    Result = true,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(Result, maps:get(<<"result">>, Decoded)),
    ?assertNot(maps:is_key(<<"error">>, Decoded)).

test_encode_response_with_null_result() ->
    Id = 4,
    Encoded = erlmcp_json_rpc:encode_response(Id, null),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(null, maps:get(<<"result">>, Decoded)),
    ?assertNot(maps:is_key(<<"error">>, Decoded)).

test_encode_response_with_object_result() ->
    Id = 5,
    Result = #{
        <<"status">> => <<"ok">>,
        <<"data">> => #{<<"count">> => 10}
    },
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ResultMap = maps:get(<<"result">>, Decoded),
    ?assertEqual(<<"ok">>, maps:get(<<"status">>, ResultMap)),
    ?assertNot(maps:is_key(<<"error">>, Decoded)).

test_encode_response_with_array_result() ->
    Id = 6,
    Result = [1, 2, 3, <<"four">>],
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assert(is_list(maps:get(<<"result">>, Decoded))),
    ?assertNot(maps:is_key(<<"error">>, Decoded)).

%%====================================================================
%% Response ID Tests
%%====================================================================

response_id_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_response_with_numeric_id()),
             ?_test(test_response_with_string_id()),
             ?_test(test_response_with_null_id())
         ]
     end}.

test_response_with_numeric_id() ->
    Id = 123,
    Result = <<"ok">>,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

test_response_with_string_id() ->
    Id = <<"req-abc-123">>,
    Result = <<"ok">>,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

test_response_with_null_id() ->
    Id = null,
    Result = <<"ok">>,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(null, maps:get(<<"id">>, Decoded)).

%%====================================================================
%% Response Structure Validation
%%====================================================================

response_structure_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_response_has_jsonrpc_version()),
             ?_test(test_response_has_result_or_error()),
             ?_test(test_response_success_has_result()),
             ?_test(test_response_error_not_present())
         ]
     end}.

test_response_has_jsonrpc_version() ->
    Id = 1,
    Result = <<"ok">>,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)).

test_response_has_result_or_error() ->
    Id = 1,
    Result = <<"data">>,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assert(maps:is_key(<<"result">>, Decoded)).

test_response_success_has_result() ->
    Id = 1,
    Result = #{<<"key">> => <<"value">>},
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(Result, maps:get(<<"result">>, Decoded)).

test_response_error_not_present() ->
    Id = 1,
    Result = <<"ok">>,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertNot(maps:is_key(<<"error">>, Decoded)).

%%====================================================================
%% Decoding Successful Responses
%%====================================================================

decode_response_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_decode_successful_response()),
             ?_test(test_decode_response_with_object_result()),
             ?_test(test_decode_response_with_array_result()),
             ?_test(test_decode_response_with_null_result())
         ]
     end}.

test_decode_successful_response() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => <<"success">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_response{result = <<"success">>}}, Result).

test_decode_response_with_object_result() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => #{<<"status">> => <<"ok">>}
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_response{}}, Result),
    {ok, Response} = Result,
    ?assertEqual(#{<<"status">> => <<"ok">>}, Response#json_rpc_response.result).

test_decode_response_with_array_result() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => [1, 2, 3]
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    {ok, Response} = Result,
    ?assertEqual([1, 2, 3], Response#json_rpc_response.result).

test_decode_response_with_null_result() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => null
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    {ok, Response} = Result,
    ?assertEqual(null, Response#json_rpc_response.result).

%%====================================================================
%% Batch Response Tests
%%====================================================================

batch_response_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_encode_batch_responses()),
             ?_test(test_decode_batch_responses()),
             ?_test(test_batch_response_mixed_types())
         ]
     end}.

test_encode_batch_responses() ->
    Responses = [
        #json_rpc_response{id = 1, result = <<"ok1">>},
        #json_rpc_response{id = 2, result = <<"ok2">>}
    ],
    Encoded = erlmcp_json_rpc:encode_batch(Responses),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assert(is_list(Decoded)),
    ?assertEqual(2, length(Decoded)).

test_decode_batch_responses() ->
    Json = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"result">> => <<"ok1">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2, <<"result">> => <<"ok2">>}
    ]),
    Result = erlmcp_json_rpc:decode_batch(Json),
    ?assertMatch({ok, [_ | _]}, Result),
    {ok, Responses} = Result,
    ?assertEqual(2, length(Responses)).

test_batch_response_mixed_types() ->
    Json = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"result">> => <<"ok">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2, <<"error">> => #{<<"code">> => -32600, <<"message">> => <<"Error">>}}
    ]),
    Result = erlmcp_json_rpc:decode_batch(Json),
    ?assertMatch({ok, [_ | _]}, Result).

%%====================================================================
%% Complex Result Types
%%====================================================================

complex_result_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_response_with_nested_objects()),
             ?_test(test_response_with_large_array()),
             ?_test(test_response_with_special_characters()),
             ?_test(test_response_with_unicode())
         ]
     end}.

test_response_with_nested_objects() ->
    Id = 1,
    Result = #{
        <<"level1">> => #{
            <<"level2">> => #{
                <<"value">> => <<"deep">>
            }
        }
    },
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assert(maps:is_key(<<"level1">>, maps:get(<<"result">>, Decoded))).

test_response_with_large_array() ->
    Id = 1,
    Result = lists:seq(1, 1000),
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ResultArray = maps:get(<<"result">>, Decoded),
    ?assertEqual(1000, length(ResultArray)).

test_response_with_special_characters() ->
    Id = 1,
    Result = <<"Line 1\nLine 2\tTabbed">>,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(Result, maps:get(<<"result">>, Decoded)).

test_response_with_unicode() ->
    Id = 1,
    Result = <<"Hello 世界 🌍 Привет мир">>,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(Result, maps:get(<<"result">>, Decoded)).

%%====================================================================
%% Response vs Notification Distinction
%%====================================================================

response_notification_distinction_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_response_has_id()),
             ?_test(test_notification_no_id())
         ]
     end}.

test_response_has_id() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => <<"ok">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_response{id = 1}}, Result).

test_notification_no_id() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"notify">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_notification{}}, Result).
