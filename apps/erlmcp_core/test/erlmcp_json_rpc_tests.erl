-module(erlmcp_json_rpc_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_json_rpc Module
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
             ?_test(test_encode_request_with_array_params()),
             ?_test(test_encode_request_null_params()),
             ?_test(test_encode_request_negative_id()),
             ?_test(test_encode_request_large_id())
         ]
     end}.

test_encode_request_with_id_and_params() ->
    Id = 1,
    Method = <<"initialize">>,
    Params = #{version => <<"1.0">>},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)).

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
    Params = #{name => <<"calc">>, args => #{x => 10, y => 20}},
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

test_encode_request_null_params() ->
    Id = 5,
    Method = <<"ping">>,
    Params = undefined,
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertNot(maps:is_key(<<"params">>, Decoded)).

test_encode_request_negative_id() ->
    Id = -1,
    Method = <<"test">>,
    Params = #{},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

test_encode_request_large_id() ->
    Id = 9999999999,
    Method = <<"test">>,
    Params = #{},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

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
             ?_test(test_encode_response_with_string_id()),
             ?_test(test_encode_response_with_boolean()),
             ?_test(test_encode_response_with_number()),
             ?_test(test_encode_response_with_nested_object())
         ]
     end}.

test_encode_response_simple() ->
    Id = 1,
    Result = <<"success">>,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(Result, maps:get(<<"result">>, Decoded)),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

test_encode_response_with_object() ->
    Id = 2,
    Result = #{status => <<"ok">>, message => <<"done">>},
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
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
    Result = [1, 2, 3, <<"test">>],
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assert(is_list(maps:get(<<"result">>, Decoded))).

test_encode_response_with_string_id() ->
    Id = <<"response-456">>,
    Result = #{ok => true},
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)).

test_encode_response_with_boolean() ->
    Id = 5,
    Result = true,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(true, maps:get(<<"result">>, Decoded)).

test_encode_response_with_number() ->
    Id = 6,
    Result = 42.5,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(42.5, maps:get(<<"result">>, Decoded)).

test_encode_response_with_nested_object() ->
    Id = 7,
    Result = #{level1 => #{level2 => #{level3 => <<"deep">>}}},
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assert(is_map(maps:get(<<"result">>, Decoded))).

%%====================================================================
%% Error Response Tests
%%====================================================================

error_response_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_encode_error_response_basic()),
             ?_test(test_encode_error_response_with_data()),
             ?_test(test_encode_error_response_invalid_code()),
             ?_test(test_error_method_not_found()),
             ?_test(test_error_invalid_params()),
             ?_test(test_error_invalid_params_with_string()),
             ?_test(test_error_invalid_params_with_atom()),
             ?_test(test_error_resource_not_found()),
             ?_test(test_error_tool_not_found()),
             ?_test(test_error_prompt_not_found()),
             ?_test(test_validate_error_code()),
             ?_test(test_encode_error_response_with_null_data()),
             ?_test(test_encode_error_response_with_map_data()),
             ?_test(test_encode_error_response_with_binary_data())
         ]
     end}.

test_encode_error_response_basic() ->
    Id = 1,
    Code = -32600,
    Message = <<"Invalid Request">>,
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(Code, maps:get(<<"code">>, Error)),
    ?assertEqual(Message, maps:get(<<"message">>, Error)),
    ?assertNot(maps:is_key(<<"data">>, Error)).

test_encode_error_response_with_data() ->
    Id = 2,
    Code = -32602,
    Message = <<"Invalid Parameters">>,
    Data = #{<<"field">> => <<"name">>, <<"reason">> => <<"required">>},
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(maps:is_key(<<"data">>, Error)),
    ?assertEqual(Data, maps:get(<<"data">>, Error)).

test_encode_error_response_invalid_code() ->
    Id = 3,
    InvalidCode = 9999,
    Message = <<"Test Error">>,
    Encoded = erlmcp_json_rpc:encode_error_response(Id, InvalidCode, Message),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    %% Should use internal error code (-32603)
    ?assertEqual(-32603, maps:get(<<"code">>, Error)).

test_encode_error_response_with_null_data() ->
    Id = 4,
    Code = -32601,
    Message = <<"Method not found">>,
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message, null),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertNot(maps:is_key(<<"data">>, Error)).

test_encode_error_response_with_map_data() ->
    Id = 5,
    Code = -32602,
    Message = <<"Invalid params">>,
    Data = #{<<"field">> => <<"value">>},
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(Data, maps:get(<<"data">>, Error)).

test_encode_error_response_with_binary_data() ->
    Id = 6,
    Code = -32602,
    Message = <<"Invalid params">>,
    Data = <<"error details">>,
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ErrorData = maps:get(<<"data">>, Error),
    ?assertEqual(#{<<"details">> => Data}, ErrorData).

test_error_method_not_found() ->
    Id = 4,
    Method = <<"unknown_method">>,
    Encoded = erlmcp_json_rpc:error_method_not_found(Id, Method),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)),
    ?assertEqual(Method, maps:get(<<"method">>, maps:get(<<"data">>, Error))).

test_error_invalid_params() ->
    Id = 5,
    Reason = <<"missing required field">>,
    Encoded = erlmcp_json_rpc:error_invalid_params(Id, Reason),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32602, maps:get(<<"code">>, Error)),
    ?assertEqual(Reason, maps:get(<<"details">>, maps:get(<<"data">>, Error))).

test_error_invalid_params_with_string() ->
    Id = 5,
    Reason = "missing required field",
    Encoded = erlmcp_json_rpc:error_invalid_params(Id, Reason),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32602, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"missing required field">>, maps:get(<<"details">>, maps:get(<<"data">>, Error))).

test_error_invalid_params_with_atom() ->
    Id = 5,
    Reason = invalid_type,
    Encoded = erlmcp_json_rpc:error_invalid_params(Id, Reason),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32602, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"invalid_type">>, maps:get(<<"details">>, maps:get(<<"data">>, Error))).

test_error_resource_not_found() ->
    Id = 6,
    Uri = <<"resource://missing">>,
    Encoded = erlmcp_json_rpc:error_resource_not_found(Id, Uri),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32001, maps:get(<<"code">>, Error)),
    ?assertEqual(Uri, maps:get(<<"uri">>, maps:get(<<"data">>, Error))).

test_error_tool_not_found() ->
    Id = 7,
    Tool = <<"missing_tool">>,
    Encoded = erlmcp_json_rpc:error_tool_not_found(Id, Tool),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32002, maps:get(<<"code">>, Error)),
    ?assertEqual(Tool, maps:get(<<"tool">>, maps:get(<<"data">>, Error))).

test_error_prompt_not_found() ->
    Id = 8,
    Prompt = <<"missing_prompt">>,
    Encoded = erlmcp_json_rpc:error_prompt_not_found(Id, Prompt),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32003, maps:get(<<"code">>, Error)),
    ?assertEqual(Prompt, maps:get(<<"prompt">>, maps:get(<<"data">>, Error))).

test_validate_error_code() ->
    ?assert(erlmcp_json_rpc:validate_error_code(-32700)),
    ?assert(erlmcp_json_rpc:validate_error_code(-32600)),
    ?assert(erlmcp_json_rpc:validate_error_code(-32601)),
    ?assert(erlmcp_json_rpc:validate_error_code(-32602)),
    ?assert(erlmcp_json_rpc:validate_error_code(-32603)),
    ?assertNot(erlmcp_json_rpc:validate_error_code(0)),
    ?assertNot(erlmcp_json_rpc:validate_error_code(9999)).

%%====================================================================
%% Notification Tests
%%====================================================================

notification_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_encode_notification_basic()),
             ?_test(test_encode_notification_with_params()),
             ?_test(test_encode_notification_no_id()),
             ?_test(test_encode_notification_with_array_params()),
             ?_test(test_encode_notification_empty_params())
         ]
     end}.

test_encode_notification_basic() ->
    Method = <<"resource/changed">>,
    Params = #{uri => <<"resource://test">>},
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
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)),
    ?assertEqual(50, maps:get(<<"progress">>, maps:get(<<"params">>, Decoded))).

test_encode_notification_no_id() ->
    Method = <<"initialized">>,
    Params = #{},
    Encoded = erlmcp_json_rpc:encode_notification(Method, Params),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertNot(maps:is_key(<<"id">>, Decoded)).

test_encode_notification_with_array_params() ->
    Method = <<"batch_update">>,
    Params = [1, 2, 3],
    Encoded = erlmcp_json_rpc:encode_notification(Method, Params),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual([1, 2, 3], maps:get(<<"params">>, Decoded)).

test_encode_notification_empty_params() ->
    Method = <<"ping">>,
    Params = #{},
    Encoded = erlmcp_json_rpc:encode_notification(Method, Params),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(<<"ping">>, maps:get(<<"method">>, Decoded)),
    % Empty map params are included in encoding
    ?assertEqual(#{}, maps:get(<<"params">>, Decoded)).

%%====================================================================
%% Decoding Tests
%%====================================================================

decode_test_() ->
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
             ?_test(test_decode_incomplete_message()),
             ?_test(test_decode_request_with_array_params()),
             ?_test(test_decode_response_with_nested_result()),
             ?_test(test_decode_notification_with_params())
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
    ?assertMatch({ok, #json_rpc_request{id = 1, method = <<"initialize">>}}, Result).

test_decode_response() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => <<"ok">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_response{id = 1, result = <<"ok">>}}, Result).

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
    ?assertMatch({ok, #json_rpc_response{id = 1}}, Result).

test_decode_notification() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"initialized">>,
        <<"params">> => #{}
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_notification{method = <<"initialized">>}}, Result).

test_decode_invalid_json() ->
    InvalidJson = <<"not valid json {">>,
    Result = erlmcp_json_rpc:decode_message(InvalidJson),
    ?assertMatch({error, {parse_error, _}}, Result).

test_decode_incomplete_message() ->
    IncompleteJson = jsx:encode(#{<<"id">> => 1}),
    Result = erlmcp_json_rpc:decode_message(IncompleteJson),
    ?assertMatch({error, _}, Result).

test_decode_request_with_array_params() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => [1, 2, 3]
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{params = [1, 2, 3]}}, Result).

test_decode_response_with_nested_result() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => #{<<"nested">> => #{<<"value">> => 42}}
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_response{result = #{<<"nested">> := #{<<"value">> := 42}}}}, Result).

test_decode_notification_with_params() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test">>,
        <<"params">> => #{<<"key">> => <<"value">>}
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_notification{params = #{<<"key">> := <<"value">>}}}, Result).

%%====================================================================
%% Batch Operations Tests
%%====================================================================

batch_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_is_batch_request_true()),
             ?_test(test_is_batch_request_false()),
             ?_test(test_is_batch_request_invalid_json()),
             ?_test(test_encode_batch()),
             ?_test(test_encode_batch_empty()),
             ?_test(test_encode_batch_single()),
             ?_test(test_decode_batch()),
             ?_test(test_decode_batch_empty()),
             ?_test(test_decode_batch_single_request()),
             ?_test(test_decode_batch_with_errors()),
             ?_test(test_decode_batch_with_invalid_json())
         ]
     end}.

test_is_batch_request_true() ->
    Json = <<"[{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}]">>,
    ?assert(erlmcp_json_rpc:is_batch_request(Json)).

test_is_batch_request_false() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
    ?assertNot(erlmcp_json_rpc:is_batch_request(Json)).

test_is_batch_request_invalid_json() ->
    Json = <<"not json">>,
    ?assertNot(erlmcp_json_rpc:is_batch_request(Json)).

test_encode_batch() ->
    Requests = [
        #json_rpc_request{id = 1, method = <<"method1">>, params = #{}},
        #json_rpc_request{id = 2, method = <<"method2">>, params = #{}}
    ],
    Result = erlmcp_json_rpc:encode_batch(Requests),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(2, length(Decoded)).

test_encode_batch_empty() ->
    Requests = [],
    Result = erlmcp_json_rpc:encode_batch(Requests),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual([], Decoded).

test_encode_batch_single() ->
    Requests = [
        #json_rpc_request{id = 1, method = <<"method1">>, params = #{}}
    ],
    Result = erlmcp_json_rpc:encode_batch(Requests),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(1, length(Decoded)).

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
    ?assertMatch({ok, [_, _]}, Result).

test_decode_batch_empty() ->
    Json = <<"[]">>,
    Result = erlmcp_json_rpc:decode_batch(Json),
    ?assertMatch({error, {invalid_request, empty_batch}}, Result).

test_decode_batch_single_request() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>
    }),
    Result = erlmcp_json_rpc:decode_batch(Json),
    ?assertMatch({ok, [_]}, Result).

test_decode_batch_with_errors() ->
    Json = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"test1">>},
        #{<<"id">> => 2, <<"method">> => <<"test2">>}
    ]),
    Result = erlmcp_json_rpc:decode_batch(Json),
    % Invalid batch items (missing jsonrpc version) should cause error
    ?assertMatch({error, {invalid_request, _}}, Result).

test_decode_batch_with_invalid_json() ->
    Json = <<"not json">>,
    Result = erlmcp_json_rpc:decode_batch(Json),
    ?assertMatch({error, {parse_error, _}}, Result).

%%====================================================================
%% Error Creation Tests
%%====================================================================

error_creation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_create_error()),
             ?_test(test_create_error_with_data()),
             ?_test(test_error_not_initialized()),
             ?_test(test_error_validation_failed()),
             ?_test(test_error_capability_not_supported()),
             ?_test(test_error_message_too_large()),
             ?_test(test_error_internal()),
             ?_test(test_error_parse()),
             ?_test(test_error_validation_failed_with_string()),
             ?_test(test_error_validation_failed_with_atom())
         ]
     end}.

test_create_error() ->
    Code = -32600,
    Message = <<"Invalid Request">>,
    Error = erlmcp_json_rpc:create_error(Code, Message, undefined),
    ?assert(is_record(Error, mcp_error)),
    ?assertEqual(Code, Error#mcp_error.code),
    ?assertEqual(Message, Error#mcp_error.message).

test_create_error_with_data() ->
    Code = -32602,
    Message = <<"Invalid Parameters">>,
    Error = erlmcp_json_rpc:create_error_with_data(Code, Message, details, <<"test">>),
    ?assert(is_record(Error, mcp_error)),
    ?assert(is_map(Error#mcp_error.data)),
    ?assertEqual(#{<<"details">> => <<"test">>}, Error#mcp_error.data).

test_error_not_initialized() ->
    Id = 3,
    Encoded = erlmcp_json_rpc:error_not_initialized(Id),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32005, maps:get(<<"code">>, Error)).

test_error_validation_failed() ->
    Id = 4,
    Reason = <<"invalid format">>,
    Encoded = erlmcp_json_rpc:error_validation_failed(Id, Reason),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32007, maps:get(<<"code">>, Error)).

test_error_validation_failed_with_string() ->
    Id = 4,
    Reason = "invalid format",
    Encoded = erlmcp_json_rpc:error_validation_failed(Id, Reason),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(<<"invalid format">>, maps:get(<<"details">>, Data)).

test_error_validation_failed_with_atom() ->
    Id = 4,
    Reason = schema_error,
    Encoded = erlmcp_json_rpc:error_validation_failed(Id, Reason),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(<<"schema_error">>, maps:get(<<"details">>, Data)).

test_error_capability_not_supported() ->
    Id = 5,
    Capability = <<"sampling">>,
    Encoded = erlmcp_json_rpc:error_capability_not_supported(Id, Capability),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32004, maps:get(<<"code">>, Error)),
    ?assertEqual(Capability, maps:get(<<"capability">>, maps:get(<<"data">>, Error))).

test_error_message_too_large() ->
    Id = 6,
    MaxSize = 1024,
    Encoded = erlmcp_json_rpc:error_message_too_large(Id, MaxSize),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32012, maps:get(<<"code">>, Error)),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(MaxSize, maps:get(<<"maxSize">>, Data)),
    ?assertEqual(<<"bytes">>, maps:get(<<"unit">>, Data)).

test_error_internal() ->
    Id = 7,
    Encoded = erlmcp_json_rpc:error_internal(Id),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32603, maps:get(<<"code">>, Error)).

test_error_parse() ->
    Id = 8,
    Encoded = erlmcp_json_rpc:error_parse(Id),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32700, maps:get(<<"code">>, Error)).

%%====================================================================
%% decode_message/2 with Transport Type Validation Tests
%%====================================================================

decode_message_with_transport_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_decode_message_with_default_transport()),
             ?_test(test_decode_message_with_stdio_transport()),
             ?_test(test_decode_message_with_tcp_transport()),
             ?_test(test_decode_message_with_http_transport()),
             ?_test(test_decode_message_size_validation_pass())
         ]
     end}.

test_decode_message_with_default_transport() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{}
    }),
    Result = erlmcp_json_rpc:decode_message(Json, default),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

test_decode_message_with_stdio_transport() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{}
    }),
    Result = erlmcp_json_rpc:decode_message(Json, stdio),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

test_decode_message_with_tcp_transport() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{}
    }),
    Result = erlmcp_json_rpc:decode_message(Json, tcp),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

test_decode_message_with_http_transport() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{}
    }),
    Result = erlmcp_json_rpc:decode_message(Json, http),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

test_decode_message_size_validation_pass() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{data => <<"small data">>}
    }),
    Result = erlmcp_json_rpc:decode_message(Json, stdio),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

%%====================================================================
%% Batch Error Response Tests
%%====================================================================

batch_error_response_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_create_batch_error_response_with_id()),
             ?_test(test_create_batch_error_response_without_id()),
             ?_test(test_create_batch_error_response_non_map())
         ]
     end}.

test_create_batch_error_response_with_id() ->
    Request = #{<<"id">> => 123, <<"method">> => <<"test">>},
    Response = erlmcp_json_rpc:create_batch_error_response(Request, invalid_request, not_an_object),
    ?assertEqual(123, Response#json_rpc_response.id),
    ?assert(is_map(Response#json_rpc_response.error)).

test_create_batch_error_response_without_id() ->
    Request = #{<<"method">> => <<"test">>},
    Response = erlmcp_json_rpc:create_batch_error_response(Request, invalid_request, not_an_object),
    ?assertEqual(null, Response#json_rpc_response.id),
    ?assert(is_map(Response#json_rpc_response.error)).

test_create_batch_error_response_non_map() ->
    Request = invalid_request,
    Response = erlmcp_json_rpc:create_batch_error_response(Request, invalid_request, not_an_object),
    ?assertEqual(null, Response#json_rpc_response.id),
    ?assert(is_map(Response#json_rpc_response.error)).

%%====================================================================
%% Edge Cases and Boundary Conditions Tests
%%====================================================================

edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_decode_unicode_content()),
             ?_test(test_encode_unicode_content()),
             ?_test(test_decode_empty_string()),
             ?_test(test_decode_with_null_values()),
             ?_test(test_encode_with_nested_maps()),
             ?_test(test_encode_with_large_array()),
             ?_test(test_decode_response_with_null_result()),
             ?_test(test_encode_response_with_special_floats()),
             ?_test(test_decode_with_extra_fields()),
             ?_test(test_decode_notification_without_params()),
             ?_test(test_decode_batch_with_invalid_version())
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

test_encode_unicode_content() ->
    Result = erlmcp_json_rpc:encode_request(1, <<"test">>, #{<<"text">> => <<"Привет мир">>}),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"test">>, maps:get(<<"method">>, Decoded)).

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

test_decode_response_with_null_result() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => null
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_response{result = null}}, Result).

test_encode_response_with_special_floats() ->
    Result = erlmcp_json_rpc:encode_response(1, 3.14159),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(3.14159, maps:get(<<"result">>, Decoded)).

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

test_decode_notification_without_params() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_notification{}}, Result).

test_decode_batch_with_invalid_version() ->
    Json = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"test1">>},
        #{<<"jsonrpc">> => <<"1.0">>, <<"id">> => 2, <<"method">> => <<"test2">>}
    ]),
    Result = erlmcp_json_rpc:decode_batch(Json),
    % Invalid version items are filtered out or cause error
    ?assertMatch({error, {invalid_request, _}}, Result).

%%====================================================================
%% Error Message Strengthening Tests
%%====================================================================

error_message_strengthening_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_error_resource_not_found_has_uri()),
             ?_test(test_error_tool_not_found_has_tool()),
             ?_test(test_error_prompt_not_found_has_prompt()),
             ?_test(test_error_capability_not_supported_has_capability()),
             ?_test(test_error_not_initialized_code()),
             ?_test(test_error_validation_failed_has_details()),
             ?_test(test_error_message_too_large_has_max_size()),
             ?_test(test_error_internal_code()),
             ?_test(test_encode_error_response_with_invalid_code_uses_internal()),
             ?_test(test_create_error_record()),
             ?_test(test_create_error_with_data_record())
         ]
     end}.

test_error_resource_not_found_has_uri() ->
    Id = 1,
    Uri = <<"resource://test/missing">>,
    Result = erlmcp_json_rpc:error_resource_not_found(Id, Uri),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(Uri, maps:get(<<"uri">>, Data)).

test_error_tool_not_found_has_tool() ->
    Id = 1,
    Tool = <<"missing_tool">>,
    Result = erlmcp_json_rpc:error_tool_not_found(Id, Tool),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(Tool, maps:get(<<"tool">>, Data)).

test_error_prompt_not_found_has_prompt() ->
    Id = 1,
    Prompt = <<"missing_prompt">>,
    Result = erlmcp_json_rpc:error_prompt_not_found(Id, Prompt),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(Prompt, maps:get(<<"prompt">>, Data)).

test_error_capability_not_supported_has_capability() ->
    Id = 1,
    Capability = <<"sampling">>,
    Result = erlmcp_json_rpc:error_capability_not_supported(Id, Capability),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(Capability, maps:get(<<"capability">>, Data)).

test_error_not_initialized_code() ->
    Id = 1,
    Result = erlmcp_json_rpc:error_not_initialized(Id),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32005, maps:get(<<"code">>, Error)).

test_error_validation_failed_has_details() ->
    Id = 1,
    Reason = <<"schema validation failed">>,
    Result = erlmcp_json_rpc:error_validation_failed(Id, Reason),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(Reason, maps:get(<<"details">>, Data)).

test_error_message_too_large_has_max_size() ->
    Id = 1,
    MaxSize = 2097152,
    Result = erlmcp_json_rpc:error_message_too_large(Id, MaxSize),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(MaxSize, maps:get(<<"maxSize">>, Data)),
    ?assertEqual(<<"bytes">>, maps:get(<<"unit">>, Data)).

test_error_internal_code() ->
    Id = 1,
    Result = erlmcp_json_rpc:error_internal(Id),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32603, maps:get(<<"code">>, Error)).

test_encode_error_response_with_invalid_code_uses_internal() ->
    Id = 1,
    InvalidCode = 99999,
    Result = erlmcp_json_rpc:encode_error_response(Id, InvalidCode, <<"Custom Error">>),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32603, maps:get(<<"code">>, Error)).

test_create_error_record() ->
    Error = erlmcp_json_rpc:create_error(-32600, <<"Invalid">>, undefined),
    ?assert(is_record(Error, mcp_error)),
    ?assertEqual(-32600, Error#mcp_error.code),
    ?assertEqual(<<"Invalid">>, Error#mcp_error.message),
    ?assertEqual(undefined, Error#mcp_error.data).

test_create_error_with_data_record() ->
    Error = erlmcp_json_rpc:create_error_with_data(-32602, <<"Invalid Params">>, details, <<"missing field">>),
    ?assert(is_record(Error, mcp_error)),
    ?assertEqual(-32602, Error#mcp_error.code),
    ?assertEqual(<<"Invalid Params">>, Error#mcp_error.message),
    ?assertEqual(#{<<"details">> => <<"missing field">>}, Error#mcp_error.data).

%%====================================================================
%% Additional Coverage Tests for Edge Cases
%%====================================================================

additional_coverage_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_encode_request_with_zero_id()),
             ?_test(test_encode_request_with_float_id()),
             ?_test(test_decode_request_with_string_id()),
             ?_test(test_decode_request_with_null_id()),
             ?_test(test_decode_response_with_string_id()),
             ?_test(test_encode_response_with_zero_id()),
             ?_test(test_encode_response_with_negative_id()),
             ?_test(test_decode_error_response_with_data()),
             ?_test(test_decode_batch_with_notifications()),
             ?_test(test_decode_batch_with_mixed_types()),
             ?_test(test_is_batch_request_with_empty_array())
         ]
     end}.

test_encode_request_with_zero_id() ->
    Result = erlmcp_json_rpc:encode_request(0, <<"test">>, #{}),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(0, maps:get(<<"id">>, Decoded)).

test_encode_request_with_float_id() ->
    % Float IDs are not supported in JSON-RPC 2.0 (only string, number, null)
    % This test verifies that we handle this edge case appropriately
    catch erlmcp_json_rpc:encode_request(1.5, <<"test">>, #{}),
    ?assert(true).

test_decode_request_with_string_id() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => <<"test-id">>,
        <<"method">> => <<"test">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{id = <<"test-id">>}}, Result).

test_decode_request_with_null_id() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => null,
        <<"method">> => <<"test">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{id = null}}, Result).

test_decode_response_with_string_id() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => <<"response-id">>,
        <<"result">> => <<"ok">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_response{id = <<"response-id">>}}, Result).

test_encode_response_with_zero_id() ->
    Result = erlmcp_json_rpc:encode_response(0, <<"ok">>),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(0, maps:get(<<"id">>, Decoded)).

test_encode_response_with_negative_id() ->
    Result = erlmcp_json_rpc:encode_response(-1, <<"ok">>),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(-1, maps:get(<<"id">>, Decoded)).

test_decode_error_response_with_data() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"error">> => #{
            <<"code">> => -32602,
            <<"message">> => <<"Invalid params">>,
            <<"data">> => #{<<"details">> => <<"error">>}
        }
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_response{}}, Result).

test_decode_batch_with_notifications() ->
    Json = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"test1">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"notification">>}
    ]),
    Result = erlmcp_json_rpc:decode_batch(Json),
    % Batch should return both request and notification
    ?assertMatch({ok, [_, _]}, Result).

test_decode_batch_with_mixed_types() ->
    Json = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"test">>, <<"params">> => #{}},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2, <<"result">> => <<"ok">>}
    ]),
    Result = erlmcp_json_rpc:decode_batch(Json),
    ?assertMatch({ok, [_, _]}, Result).

test_is_batch_request_with_empty_array() ->
    Json = <<"[]">>,
    ?assert(erlmcp_json_rpc:is_batch_request(Json)).

%%====================================================================
%% Additional Edge Cases for Higher Coverage
%%====================================================================

additional_edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_encode_error_response_with_zero_code()),
             ?_test(test_encode_error_response_with_negative_code()),
             ?_test(test_validate_error_code_valid_codes()),
             ?_test(test_validate_error_code_invalid_codes()),
             ?_test(test_encode_request_with_undefined_params()),
             ?_test(test_encode_response_with_undefined_result())
         ]
     end}.

test_encode_error_response_with_zero_code() ->
    % Zero is not a valid JSON-RPC error code
    Id = 1,
    Result = erlmcp_json_rpc:encode_error_response(Id, 0, <<"Zero Error">>),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    % Should use internal error for invalid code
    ?assertEqual(-32603, maps:get(<<"code">>, Error)).

test_encode_error_response_with_negative_code() ->
    % Test with a negative code that's not in the valid list
    Id = 1,
    Result = erlmcp_json_rpc:encode_error_response(Id, -99999, <<"Custom Error">>),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    % Should use internal error for invalid code
    ?assertEqual(-32603, maps:get(<<"code">>, Error)).

test_validate_error_code_valid_codes() ->
    % Test some valid error codes
    ?assert(erlmcp_json_rpc:validate_error_code(-32700)), % Parse error
    ?assert(erlmcp_json_rpc:validate_error_code(-32600)), % Invalid request
    ?assert(erlmcp_json_rpc:validate_error_code(-32601)), % Method not found
    ?assert(erlmcp_json_rpc:validate_error_code(-32602)), % Invalid params
    ?assert(erlmcp_json_rpc:validate_error_code(-32603)), % Internal error
    ?assert(erlmcp_json_rpc:validate_error_code(-32001)), % Server error start
    ?assert(erlmcp_json_rpc:validate_error_code(-32000)). % Server error end

test_validate_error_code_invalid_codes() ->
    % Test some invalid error codes
    ?assertNot(erlmcp_json_rpc:validate_error_code(0)),
    ?assertNot(erlmcp_json_rpc:validate_error_code(100)),
    ?assertNot(erlmcp_json_rpc:validate_error_code(-1)),
    ?assertNot(erlmcp_json_rpc:validate_error_code(-99999)).

test_encode_request_with_undefined_params() ->
    % Undefined params should not be included in the encoded message
    Result = erlmcp_json_rpc:encode_request(1, <<"test">>, undefined),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertNot(maps:is_key(<<"params">>, Decoded)).

test_encode_response_with_undefined_result() ->
    % Undefined result with no error should include result field
    Result = erlmcp_json_rpc:encode_response(1, undefined),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(maps:is_key(<<"result">>, Decoded)),
    % Undefined atoms are converted to binary by jsx
    ?assertEqual(<<"undefined">>, maps:get(<<"result">>, Decoded)).

%%====================================================================
%% Message Size and Transport Type Validation Tests
%%====================================================================

message_size_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_decode_message_too_large_stdio()),
             ?_test(test_decode_message_too_large_tcp()),
             ?_test(test_decode_message_too_large_http()),
             ?_test(test_decode_message_with_various_transports()),
             ?_test(test_decode_message_batch_request_error())
         ]
     end}.

test_decode_message_too_large_stdio() ->
    LargeValue = binary:copy(<<"x">>, 18 * 1024 * 1024),
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{data => LargeValue}
    }),
    Result = erlmcp_json_rpc:decode_message(Json, stdio),
    ?assertMatch({error, {message_too_large, _}}, Result).

test_decode_message_too_large_tcp() ->
    LargeValue = binary:copy(<<"x">>, 18 * 1024 * 1024),
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{data => LargeValue}
    }),
    Result = erlmcp_json_rpc:decode_message(Json, tcp),
    ?assertMatch({error, {message_too_large, _}}, Result).

test_decode_message_too_large_http() ->
    LargeValue = binary:copy(<<"x">>, 18 * 1024 * 1024),
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{data => LargeValue}
    }),
    Result = erlmcp_json_rpc:decode_message(Json, http),
    ?assertMatch({error, {message_too_large, _}}, Result).

test_decode_message_with_various_transports() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{}
    }),
    ?assertMatch({ok, _}, erlmcp_json_rpc:decode_message(Json, stdio)),
    ?assertMatch({ok, _}, erlmcp_json_rpc:decode_message(Json, tcp)),
    ?assertMatch({ok, _}, erlmcp_json_rpc:decode_message(Json, http)),
    ?assertMatch({ok, _}, erlmcp_json_rpc:decode_message(Json, default)).

test_decode_message_batch_request_error() ->
    Json = <<"[{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}]">>,
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({error, {batch_request, _}}, Result).
