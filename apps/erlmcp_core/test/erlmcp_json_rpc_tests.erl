-module(erlmcp_json_rpc_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp_core/include/erlmcp.hrl").

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
             ?_test(test_encode_request_with_array_params())
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
             ?_test(test_error_resource_not_found()),
             ?_test(test_error_tool_not_found()),
             ?_test(test_error_prompt_not_found()),
             ?_test(test_validate_error_code())
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
    ?assertEqual(Message, maps:get(<<"message">>, Error)).

test_encode_error_response_with_data() ->
    Id = 2,
    Code = -32602,
    Message = <<"Invalid Parameters">>,
    Data = #{field => <<"name">>, reason => <<"required">>},
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(maps:is_key(<<"data">>, Error)).

test_encode_error_response_invalid_code() ->
    Id = 3,
    InvalidCode = 9999,
    Message = <<"Test Error">>,
    Encoded = erlmcp_json_rpc:encode_error_response(Id, InvalidCode, Message),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    %% Should use internal error code (-32603)
    ?assert(is_integer(maps:get(<<"code">>, Error))).

test_error_method_not_found() ->
    Id = 4,
    Method = <<"unknown_method">>,
    Encoded = erlmcp_json_rpc:error_method_not_found(Id, Method),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)).

test_error_invalid_params() ->
    Id = 5,
    Reason = <<"missing required field">>,
    Encoded = erlmcp_json_rpc:error_invalid_params(Id, Reason),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32602, maps:get(<<"code">>, Error)).

test_error_resource_not_found() ->
    Id = 6,
    Uri = <<"resource://missing">>,
    Encoded = erlmcp_json_rpc:error_resource_not_found(Id, Uri),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(is_map(Error)).

test_error_tool_not_found() ->
    Id = 7,
    Tool = <<"missing_tool">>,
    Encoded = erlmcp_json_rpc:error_tool_not_found(Id, Tool),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(is_map(Error)).

test_error_prompt_not_found() ->
    Id = 8,
    Prompt = <<"missing_prompt">>,
    Encoded = erlmcp_json_rpc:error_prompt_not_found(Id, Prompt),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(is_map(Error)).

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
             ?_test(test_encode_notification_no_id())
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
    Params = #{progress => 50, total => 100},
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

batch_test_() ->
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
             ?_test(test_error_parse())
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
    ?assert(is_map(Error#mcp_error.data)).

test_error_not_initialized() ->
    Id = 3,
    Encoded = erlmcp_json_rpc:error_not_initialized(Id),
    ?assert(is_binary(Encoded)).

test_error_validation_failed() ->
    Id = 4,
    Reason = <<"invalid format">>,
    Encoded = erlmcp_json_rpc:error_validation_failed(Id, Reason),
    ?assert(is_binary(Encoded)).

test_error_capability_not_supported() ->
    Id = 5,
    Capability = <<"sampling">>,
    Encoded = erlmcp_json_rpc:error_capability_not_supported(Id, Capability),
    ?assert(is_binary(Encoded)).

test_error_message_too_large() ->
    Id = 6,
    MaxSize = 1024,
    Encoded = erlmcp_json_rpc:error_message_too_large(Id, MaxSize),
    ?assert(is_binary(Encoded)).

test_error_internal() ->
    Id = 7,
    Encoded = erlmcp_json_rpc:error_internal(Id),
    ?assert(is_binary(Encoded)).

test_error_parse() ->
    Id = 8,
    Encoded = erlmcp_json_rpc:error_parse(Id),
    ?assert(is_binary(Encoded)).
