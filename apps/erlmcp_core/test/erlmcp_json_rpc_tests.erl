-module(erlmcp_json_rpc_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_json_rpc Module
%% Chicago School TDD: Test ONLY observable behavior through public API
%% NO internal function testing
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
    Data = #{<<"field">> => <<"name">>, <<"reason">> => <<"required">>},
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
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32700, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Parse error">>, maps:get(<<"message">>, Error)).

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
    %% Small message should pass validation
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{<<"data">> => <<"small data">>}
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
          ?_test(test_create_batch_error_response_non_map()),
          ?_test(test_map_batch_error_to_code_invalid_request()),
          ?_test(test_map_batch_error_to_code_parse_error()),
          ?_test(test_map_batch_error_to_code_missing_jsonrpc()),
          ?_test(test_map_batch_error_to_code_wrong_version()),
          ?_test(test_map_batch_error_to_code_invalid_method()),
          ?_test(test_map_batch_error_to_code_unknown_type()),
          ?_test(test_map_batch_error_to_code_unknown_reason())
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

test_map_batch_error_to_code_invalid_request() ->
    {Code, Message} = erlmcp_json_rpc:map_batch_error_to_code(invalid_request, not_an_object),
    ?assertEqual(-32600, Code),
    ?assertEqual(<<"Invalid Request: not an object">>, Message).

test_map_batch_error_to_code_parse_error() ->
    {Code, Message} = erlmcp_json_rpc:map_batch_error_to_code(parse_error, ignored),
    ?assertEqual(-32700, Code),
    ?assertEqual(<<"Parse error">>, Message).

test_map_batch_error_to_code_missing_jsonrpc() ->
    {Code, Message} = erlmcp_json_rpc:map_batch_error_to_code(missing_jsonrpc, ignored),
    ?assertEqual(-32600, Code),
    ?assertEqual(<<"Missing jsonrpc version field">>, Message).

test_map_batch_error_to_code_wrong_version() ->
    {Code, Message} = erlmcp_json_rpc:map_batch_error_to_code(wrong_version, ignored),
    ?assertEqual(-32600, Code),
    ?assertEqual(<<"Invalid jsonrpc version (must be 2.0)">>, Message).

test_map_batch_error_to_code_invalid_method() ->
    {Code, Message} = erlmcp_json_rpc:map_batch_error_to_code(invalid_method, <<"bad_method">>),
    ?assertEqual(-32600, Code),
    ?assertEqual(<<"Invalid method: bad_method">>, Message).

test_map_batch_error_to_code_unknown_type() ->
    {Code, Message} = erlmcp_json_rpc:map_batch_error_to_code(unknown_message_type, ignored),
    ?assertEqual(-32600, Code),
    ?assertEqual(<<"Unknown message type">>, Message).

test_map_batch_error_to_code_unknown_reason() ->
    {Code, Message} = erlmcp_json_rpc:map_batch_error_to_code(weird_error, details),
    ?assertEqual(-32603, Code),
    ?assertEqual(<<"Internal error">>, Message).

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
          ?_test(test_decode_batch_single_request()),
          ?_test(test_decode_response_with_null_result()),
          ?_test(test_error_invalid_params_string_conversion()),
          ?_test(test_error_invalid_params_atom_conversion()),
          ?_test(test_error_validation_failed_string_conversion()),
          ?_test(test_error_validation_failed_atom_conversion()),
          ?_test(test_encode_response_with_special_floats()),
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

test_error_invalid_params_string_conversion() ->
    Id = 1,
    Result = erlmcp_json_rpc:error_invalid_params(Id, "string details"),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32602, maps:get(<<"code">>, Error)).

test_error_invalid_params_atom_conversion() ->
    Id = 1,
    Result = erlmcp_json_rpc:error_invalid_params(Id, invalid_type),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32602, maps:get(<<"code">>, Error)).

test_error_validation_failed_string_conversion() ->
    Id = 1,
    Result = erlmcp_json_rpc:error_validation_failed(Id, "validation failed"),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(maps:is_key(<<"data">>, Error)).

test_error_validation_failed_atom_conversion() ->
    Id = 1,
    Result = erlmcp_json_rpc:error_validation_failed(Id, schema_error),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(maps:is_key(<<"data">>, Error)).

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
%% Error Code Classification Tests (89 Refusal Codes Integration)
%%====================================================================

error_classification_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_error_severity_critical()),
          ?_test(test_error_severity_error()),
          ?_test(test_error_severity_warning()),
          ?_test(test_error_severity_info()),
          ?_test(test_error_category_jsonrpc()),
          ?_test(test_error_category_mcp_core()),
          ?_test(test_error_category_content()),
          ?_test(test_error_category_resource()),
          ?_test(test_error_category_tool()),
          ?_test(test_error_category_prompt()),
          ?_test(test_error_category_auth()),
          ?_test(test_error_category_protocol()),
          ?_test(test_error_category_pagination()),
          ?_test(test_error_category_task()),
          ?_test(test_error_category_progress()),
          ?_test(test_error_category_completion()),
          ?_test(test_is_jsonrpc_standard_error()),
          ?_test(test_is_mcp_core_error()),
          ?_test(test_is_mcp_content_error()),
          ?_test(test_is_mcp_resource_error()),
          ?_test(test_is_mcp_tool_error()),
          ?_test(test_is_mcp_prompt_error()),
          ?_test(test_is_mcp_auth_error()),
          ?_test(test_is_mcp_protocol_error()),
          ?_test(test_is_mcp_pagination_error()),
          ?_test(test_is_mcp_task_error()),
          ?_test(test_is_mcp_progress_error()),
          ?_test(test_is_mcp_completion_error())
         ]
     end}.

test_error_severity_critical() ->
    ?assertEqual(critical, erlmcp_json_rpc:error_severity(-32700)),
    ?assertEqual(critical, erlmcp_json_rpc:error_severity(-32600)),
    ?assertEqual(critical, erlmcp_json_rpc:error_severity(-32603)).

test_error_severity_error() ->
    ?assertEqual(error, erlmcp_json_rpc:error_severity(-32601)),
    ?assertEqual(error, erlmcp_json_rpc:error_severity(-32602)),
    ?assertEqual(error, erlmcp_json_rpc:error_severity(-32010)).

test_error_severity_warning() ->
    ?assertEqual(warning, erlmcp_json_rpc:error_severity(-32079)),
    ?assertEqual(warning, erlmcp_json_rpc:error_severity(-32000)),
    ?assertEqual(warning, erlmcp_json_rpc:error_severity(-32110)).

test_error_severity_info() ->
    ?assertEqual(info, erlmcp_json_rpc:error_severity(99999)),
    ?assertEqual(info, erlmcp_json_rpc:error_severity(0)).

test_error_category_jsonrpc() ->
    ?assertEqual(jsonrpc, erlmcp_json_rpc:error_category(-32700)),
    ?assertEqual(jsonrpc, erlmcp_json_rpc:error_category(-32600)),
    ?assertEqual(jsonrpc, erlmcp_json_rpc:error_category(-32603)).

test_error_category_mcp_core() ->
    ?assertEqual(mcp_core, erlmcp_json_rpc:error_category(-32010)),
    ?assertEqual(mcp_core, erlmcp_json_rpc:error_category(-32001)),
    ?assertEqual(mcp_core, erlmcp_json_rpc:error_category(-32000)).

test_error_category_content() ->
    ?assertEqual(content, erlmcp_json_rpc:error_category(-32020)),
    ?assertEqual(content, erlmcp_json_rpc:error_category(-32011)).

test_error_category_resource() ->
    ?assertEqual(resource, erlmcp_json_rpc:error_category(-32030)),
    ?assertEqual(resource, erlmcp_json_rpc:error_category(-32021)).

test_error_category_tool() ->
    ?assertEqual(tool, erlmcp_json_rpc:error_category(-32040)),
    ?assertEqual(tool, erlmcp_json_rpc:error_category(-32031)).

test_error_category_prompt() ->
    ?assertEqual(prompt, erlmcp_json_rpc:error_category(-32050)),
    ?assertEqual(prompt, erlmcp_json_rpc:error_category(-32041)).

test_error_category_auth() ->
    ?assertEqual(auth, erlmcp_json_rpc:error_category(-32060)),
    ?assertEqual(auth, erlmcp_json_rpc:error_category(-32051)).

test_error_category_protocol() ->
    ?assertEqual(protocol, erlmcp_json_rpc:error_category(-32070)),
    ?assertEqual(protocol, erlmcp_json_rpc:error_category(-32061)).

test_error_category_pagination() ->
    ?assertEqual(pagination, erlmcp_json_rpc:error_category(-32080)),
    ?assertEqual(pagination, erlmcp_json_rpc:error_category(-32071)).

test_error_category_task() ->
    ?assertEqual(task, erlmcp_json_rpc:error_category(-32090)),
    ?assertEqual(task, erlmcp_json_rpc:error_category(-32081)).

test_error_category_progress() ->
    ?assertEqual(progress, erlmcp_json_rpc:error_category(-32100)),
    ?assertEqual(progress, erlmcp_json_rpc:error_category(-32091)).

test_error_category_completion() ->
    ?assertEqual(completion, erlmcp_json_rpc:error_category(-32113)),
    ?assertEqual(completion, erlmcp_json_rpc:error_category(-32110)).

test_is_jsonrpc_standard_error() ->
    ?assert(erlmcp_json_rpc:is_jsonrpc_standard_error(-32700)),
    ?assert(erlmcp_json_rpc:is_jsonrpc_standard_error(-32600)),
    ?assertNot(erlmcp_json_rpc:is_jsonrpc_standard_error(-32001)).

test_is_mcp_core_error() ->
    ?assert(erlmcp_json_rpc:is_mcp_core_error(-32001)),
    ?assert(erlmcp_json_rpc:is_mcp_core_error(-32010)),
    ?assert(erlmcp_json_rpc:is_mcp_core_error(-32000)),
    ?assertNot(erlmcp_json_rpc:is_mcp_core_error(-32011)).

test_is_mcp_content_error() ->
    ?assert(erlmcp_json_rpc:is_mcp_content_error(-32011)),
    ?assert(erlmcp_json_rpc:is_mcp_content_error(-32020)),
    ?assertNot(erlmcp_json_rpc:is_mcp_content_error(-32010)).

test_is_mcp_resource_error() ->
    ?assert(erlmcp_json_rpc:is_mcp_resource_error(-32021)),
    ?assert(erlmcp_json_rpc:is_mcp_resource_error(-32030)),
    ?assertNot(erlmcp_json_rpc:is_mcp_resource_error(-32020)).

test_is_mcp_tool_error() ->
    ?assert(erlmcp_json_rpc:is_mcp_tool_error(-32031)),
    ?assert(erlmcp_json_rpc:is_mcp_tool_error(-32040)),
    ?assertNot(erlmcp_json_rpc:is_mcp_tool_error(-32030)).

test_is_mcp_prompt_error() ->
    ?assert(erlmcp_json_rpc:is_mcp_prompt_error(-32041)),
    ?assert(erlmcp_json_rpc:is_mcp_prompt_error(-32050)),
    ?assertNot(erlmcp_json_rpc:is_mcp_prompt_error(-32040)).

test_is_mcp_auth_error() ->
    ?assert(erlmcp_json_rpc:is_mcp_auth_error(-32051)),
    ?assert(erlmcp_json_rpc:is_mcp_auth_error(-32060)),
    ?assertNot(erlmcp_json_rpc:is_mcp_auth_error(-32050)).

test_is_mcp_protocol_error() ->
    ?assert(erlmcp_json_rpc:is_mcp_protocol_error(-32061)),
    ?assert(erlmcp_json_rpc:is_mcp_protocol_error(-32070)),
    ?assertNot(erlmcp_json_rpc:is_mcp_protocol_error(-32060)).

test_is_mcp_pagination_error() ->
    ?assert(erlmcp_json_rpc:is_mcp_pagination_error(-32071)),
    ?assert(erlmcp_json_rpc:is_mcp_pagination_error(-32080)),
    ?assertNot(erlmcp_json_rpc:is_mcp_pagination_error(-32070)).

test_is_mcp_task_error() ->
    ?assert(erlmcp_json_rpc:is_mcp_task_error(-32081)),
    ?assert(erlmcp_json_rpc:is_mcp_task_error(-32090)),
    ?assertNot(erlmcp_json_rpc:is_mcp_task_error(-32080)).

test_is_mcp_progress_error() ->
    ?assert(erlmcp_json_rpc:is_mcp_progress_error(-32091)),
    ?assert(erlmcp_json_rpc:is_mcp_progress_error(-32100)),
    ?assertNot(erlmcp_json_rpc:is_mcp_progress_error(-32090)).

test_is_mcp_completion_error() ->
    ?assert(erlmcp_json_rpc:is_mcp_completion_error(-32110)),
    ?assert(erlmcp_json_rpc:is_mcp_completion_error(-32113)),
    ?assertNot(erlmcp_json_rpc:is_mcp_completion_error(-32100)).

%%====================================================================
%% Content Error Helper Functions Tests (-32011 to -32020)
%%====================================================================

content_errors_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_error_tool_description_too_large()),
          ?_test(test_error_invalid_content_type()),
          ?_test(test_error_content_too_large()),
          ?_test(test_error_invalid_encoding())
         ]
     end}.

test_error_tool_description_too_large() ->
    Id = 1,
    ActualSize = 15000,
    MaxSize = 10000,
    Response = erlmcp_json_rpc:error_tool_description_too_large(Id, ActualSize, MaxSize),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32011, maps:get(<<"code">>, Error)),
    ?assertEqual(ActualSize, maps:get(<<"actualSize">>, maps:get(<<"data">>, Error))),
    ?assertEqual(MaxSize, maps:get(<<"maxSize">>, maps:get(<<"data">>, Error))).

test_error_invalid_content_type() ->
    Id = 1,
    ContentType = <<"text/plain">>,
    Response = erlmcp_json_rpc:error_invalid_content_type(Id, ContentType),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32013, maps:get(<<"code">>, Error)),
    ?assertEqual(ContentType, maps:get(<<"contentType">>, maps:get(<<"data">>, Error))).

test_error_content_too_large() ->
    Id = 1,
    ActualSize = 20000000,
    MaxSize = 16777216,
    Response = erlmcp_json_rpc:error_content_too_large(Id, ActualSize, MaxSize),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32014, maps:get(<<"code">>, Error)).

test_error_invalid_encoding() ->
    Id = 1,
    Encoding = <<"utf-16">>,
    Response = erlmcp_json_rpc:error_invalid_encoding(Id, Encoding),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32015, maps:get(<<"code">>, Error)),
    ?assertEqual(Encoding, maps:get(<<"encoding">>, maps:get(<<"data">>, Error))).

%%====================================================================
%% Resource Error Helper Functions Tests (-32021 to -32030)
%%====================================================================

resource_errors_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_error_resource_template_not_found()),
          ?_test(test_error_invalid_uri()),
          ?_test(test_error_uri_syntax_error()),
          ?_test(test_error_resource_access_denied()),
          ?_test(test_error_template_render_failed())
         ]
     end}.

test_error_resource_template_not_found() ->
    Id = 1,
    TemplateUri = <<"weather://current/{city}">>,
    Response = erlmcp_json_rpc:error_resource_template_not_found(Id, TemplateUri),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32021, maps:get(<<"code">>, Error)),
    ?assertEqual(TemplateUri, maps:get(<<"templateUri">>, maps:get(<<"data">>, Error))).

test_error_invalid_uri() ->
    Id = 1,
    Uri = <<"not-a-uri">>,
    Response = erlmcp_json_rpc:error_invalid_uri(Id, Uri),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32022, maps:get(<<"code">>, Error)),
    ?assertEqual(Uri, maps:get(<<"uri">>, maps:get(<<"data">>, Error))).

test_error_uri_syntax_error() ->
    Id = 1,
    Uri = <<"weather://city[invalid]">>,
    Reason = <<"Invalid URI syntax">>,
    Response = erlmcp_json_rpc:error_uri_syntax_error(Id, Uri, Reason),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32023, maps:get(<<"code">>, Error)).

test_error_resource_access_denied() ->
    Id = 1,
    Uri = <<"admin://config">>,
    Response = erlmcp_json_rpc:error_resource_access_denied(Id, Uri),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32025, maps:get(<<"code">>, Error)).

test_error_template_render_failed() ->
    Id = 1,
    TemplateUri = <<"weather://current/{city}">>,
    Reason = <<"Missing variable: city">>,
    Response = erlmcp_json_rpc:error_template_render_failed(Id, TemplateUri, Reason),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32029, maps:get(<<"code">>, Error)).

%%====================================================================
%% Tool Error Helper Functions Tests (-32031 to -32040)
%%====================================================================

tool_errors_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_error_tool_execution_failed()),
          ?_test(test_error_tool_timeout()),
          ?_test(test_error_tool_cancelled()),
          ?_test(test_error_invalid_tool_arguments())
         ]
     end}.

test_error_tool_execution_failed() ->
    Id = 1,
    ToolName = <<"get_weather">>,
    Reason = <<"API timeout">>,
    Response = erlmcp_json_rpc:error_tool_execution_failed(Id, ToolName, Reason),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32031, maps:get(<<"code">>, Error)),
    ?assertEqual(ToolName, maps:get(<<"tool">>, maps:get(<<"data">>, Error))),
    ?assertEqual(Reason, maps:get(<<"reason">>, maps:get(<<"data">>, Error))).

test_error_tool_timeout() ->
    Id = 1,
    ToolName = <<"long_running_task">>,
    TimeoutMs = 30000,
    Response = erlmcp_json_rpc:error_tool_timeout(Id, ToolName, TimeoutMs),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32032, maps:get(<<"code">>, Error)),
    ?assertEqual(TimeoutMs, maps:get(<<"timeoutMs">>, maps:get(<<"data">>, Error))).

test_error_tool_cancelled() ->
    Id = 1,
    ToolName = <<"user_cancelled_task">>,
    Response = erlmcp_json_rpc:error_tool_cancelled(Id, ToolName),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32033, maps:get(<<"code">>, Error)).

test_error_invalid_tool_arguments() ->
    Id = 1,
    ToolName = <<"calculate">>,
    Details = <<"Missing required parameter: x">>,
    Response = erlmcp_json_rpc:error_invalid_tool_arguments(Id, ToolName, Details),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32034, maps:get(<<"code">>, Error)).

%%====================================================================
%% Prompt Error Helper Functions Tests (-32041 to -32050)
%%====================================================================

prompt_errors_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_error_prompt_argument_missing()),
          ?_test(test_error_prompt_render_failed()),
          ?_test(test_error_invalid_prompt_arguments()),
          ?_test(test_error_sampling_failed())
         ]
     end}.

test_error_prompt_argument_missing() ->
    Id = 1,
    PromptName = <<"summarize">>,
    ArgName = <<"text">>,
    Response = erlmcp_json_rpc:error_prompt_argument_missing(Id, PromptName, ArgName),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32043, maps:get(<<"code">>, Error)),
    ?assertEqual(PromptName, maps:get(<<"prompt">>, maps:get(<<"data">>, Error))),
    ?assertEqual(ArgName, maps:get(<<"argument">>, maps:get(<<"data">>, Error))).

test_error_prompt_render_failed() ->
    Id = 1,
    PromptName = <<"template">>,
    Reason = <<"Unknown variable: name">>,
    Response = erlmcp_json_rpc:error_prompt_render_failed(Id, PromptName, Reason),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32044, maps:get(<<"code">>, Error)).

test_error_invalid_prompt_arguments() ->
    Id = 1,
    PromptName = <<"greeting">>,
    Details = <<"Invalid type for argument: name">>,
    Response = erlmcp_json_rpc:error_invalid_prompt_arguments(Id, PromptName, Details),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32045, maps:get(<<"code">>, Error)).

test_error_sampling_failed() ->
    Id = 1,
    Reason = <<"Model unavailable">>,
    Response = erlmcp_json_rpc:error_sampling_failed(Id, Reason),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32046, maps:get(<<"code">>, Error)).

%%====================================================================
%% Authentication Error Helper Functions Tests (-32051 to -32060)
%%====================================================================

auth_errors_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_error_authentication_failed()),
          ?_test(test_error_authorization_failed()),
          ?_test(test_error_invalid_credentials()),
          ?_test(test_error_token_expired()),
          ?_test(test_error_access_denied())
         ]
     end}.

test_error_authentication_failed() ->
    Id = 1,
    Reason = <<"Invalid token">>,
    Response = erlmcp_json_rpc:error_authentication_failed(Id, Reason),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32051, maps:get(<<"code">>, Error)),
    ?assertEqual(Reason, maps:get(<<"reason">>, maps:get(<<"data">>, Error))).

test_error_authorization_failed() ->
    Id = 1,
    Reason = <<"Insufficient permissions">>,
    Response = erlmcp_json_rpc:error_authorization_failed(Id, Reason),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32052, maps:get(<<"code">>, Error)).

test_error_invalid_credentials() ->
    Id = 1,
    Response = erlmcp_json_rpc:error_invalid_credentials(Id),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32053, maps:get(<<"code">>, Error)).

test_error_token_expired() ->
    Id = 1,
    Response = erlmcp_json_rpc:error_token_expired(Id),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32054, maps:get(<<"code">>, Error)).

test_error_access_denied() ->
    Id = 1,
    Resource = <<"admin://config">>,
    Response = erlmcp_json_rpc:error_access_denied(Id, Resource),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32056, maps:get(<<"code">>, Error)).

%%====================================================================
%% Protocol Error Helper Functions Tests (-32061 to -32070)
%%====================================================================

protocol_errors_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_error_unsupported_protocol_version()),
          ?_test(test_error_protocol_version_mismatch()),
          ?_test(test_error_capability_negotiation_failed()),
          ?_test(test_error_method_not_supported())
         ]
     end}.

test_error_unsupported_protocol_version() ->
    Id = 1,
    Version = <<"2020-01-01">>,
    Response = erlmcp_json_rpc:error_unsupported_protocol_version(Id, Version),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32061, maps:get(<<"code">>, Error)),
    ?assertEqual(Version, maps:get(<<"version">>, maps:get(<<"data">>, Error))).

test_error_protocol_version_mismatch() ->
    Id = 1,
    ClientVersion = <<"2024-11-05">>,
    ServerVersion = <<"2025-11-25">>,
    Response = erlmcp_json_rpc:error_protocol_version_mismatch(Id, ClientVersion, ServerVersion),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32062, maps:get(<<"code">>, Error)).

test_error_capability_negotiation_failed() ->
    Id = 1,
    Reason = <<"Incompatible capabilities">>,
    Response = erlmcp_json_rpc:error_capability_negotiation_failed(Id, Reason),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32063, maps:get(<<"code">>, Error)).

test_error_method_not_supported() ->
    Id = 1,
    Method = <<"unknown/method">>,
    Response = erlmcp_json_rpc:error_method_not_supported(Id, Method),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32065, maps:get(<<"code">>, Error)).

%%====================================================================
%% Pagination Error Helper Functions Tests (-32071 to -32080)
%%====================================================================

pagination_errors_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_error_invalid_cursor()),
          ?_test(test_error_cursor_expired()),
          ?_test(test_error_pagination_not_supported()),
          ?_test(test_error_page_size_too_large())
         ]
     end}.

test_error_invalid_cursor() ->
    Id = 1,
    Cursor = <<"invalid_cursor">>,
    Response = erlmcp_json_rpc:error_invalid_cursor(Id, Cursor),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32071, maps:get(<<"code">>, Error)),
    ?assertEqual(Cursor, maps:get(<<"cursor">>, maps:get(<<"data">>, Error))).

test_error_cursor_expired() ->
    Id = 1,
    Cursor = <<"expired_cursor">>,
    Response = erlmcp_json_rpc:error_cursor_expired(Id, Cursor),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32072, maps:get(<<"code">>, Error)).

test_error_pagination_not_supported() ->
    Id = 1,
    Method = <<"resources/list">>,
    Response = erlmcp_json_rpc:error_pagination_not_supported(Id, Method),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32073, maps:get(<<"code">>, Error)).

test_error_page_size_too_large() ->
    Id = 1,
    ActualSize = 1000,
    MaxSize = 100,
    Response = erlmcp_json_rpc:error_page_size_too_large(Id, ActualSize, MaxSize),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32074, maps:get(<<"code">>, Error)).

%%====================================================================
%% Task Error Helper Functions Tests (-32081 to -32090)
%%====================================================================

task_errors_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_error_task_not_found()),
          ?_test(test_error_task_already_exists()),
          ?_test(test_error_task_failed()),
          ?_test(test_error_task_cancelled()),
          ?_test(test_error_task_timeout())
         ]
     end}.

test_error_task_not_found() ->
    Id = 1,
    TaskId = <<"task-123">>,
    Response = erlmcp_json_rpc:error_task_not_found(Id, TaskId),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32081, maps:get(<<"code">>, Error)),
    ?assertEqual(TaskId, maps:get(<<"taskId">>, maps:get(<<"data">>, Error))).

test_error_task_already_exists() ->
    Id = 1,
    TaskId = <<"task-456">>,
    Response = erlmcp_json_rpc:error_task_already_exists(Id, TaskId),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32082, maps:get(<<"code">>, Error)).

test_error_task_failed() ->
    Id = 1,
    TaskId = <<"task-789">>,
    Reason = <<"Dependency failed">>,
    Response = erlmcp_json_rpc:error_task_failed(Id, TaskId, Reason),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32083, maps:get(<<"code">>, Error)).

test_error_task_cancelled() ->
    Id = 1,
    TaskId = <<"task-abc">>,
    Response = erlmcp_json_rpc:error_task_cancelled(Id, TaskId),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32084, maps:get(<<"code">>, Error)).

test_error_task_timeout() ->
    Id = 1,
    TaskId = <<"task-def">>,
    TimeoutMs = 60000,
    Response = erlmcp_json_rpc:error_task_timeout(Id, TaskId, TimeoutMs),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32085, maps:get(<<"code">>, Error)).

%%====================================================================
%% Progress Error Helper Functions Tests (-32091 to -32100)
%%====================================================================

progress_errors_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_error_invalid_progress_token()),
          ?_test(test_error_invalid_progress_token_integer()),
          ?_test(test_error_progress_token_expired()),
          ?_test(test_error_progress_update_failed()),
          ?_test(test_error_notification_failed()),
          ?_test(test_error_notification_queue_full())
         ]
     end}.

test_error_invalid_progress_token() ->
    Id = 1,
    Token = <<"token-123">>,
    Response = erlmcp_json_rpc:error_invalid_progress_token(Id, Token),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32091, maps:get(<<"code">>, Error)),
    ?assertEqual(Token, maps:get(<<"progressToken">>, maps:get(<<"data">>, Error))).

test_error_invalid_progress_token_integer() ->
    Id = 1,
    Token = 123,
    Response = erlmcp_json_rpc:error_invalid_progress_token(Id, Token),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32091, maps:get(<<"code">>, Error)).

test_error_progress_token_expired() ->
    Id = 1,
    Token = <<"expired-token">>,
    Response = erlmcp_json_rpc:error_progress_token_expired(Id, Token),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32092, maps:get(<<"code">>, Error)).

test_error_progress_update_failed() ->
    Id = 1,
    Token = <<"token-456">>,
    Reason = <<"Invalid progress value">>,
    Response = erlmcp_json_rpc:error_progress_update_failed(Id, Token, Reason),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32093, maps:get(<<"code">>, Error)).

test_error_notification_failed() ->
    Id = 1,
    NotificationType = <<"resources/updated">>,
    Reason = <<"Client disconnected">>,
    Response = erlmcp_json_rpc:error_notification_failed(Id, NotificationType, Reason),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32094, maps:get(<<"code">>, Error)).

test_error_notification_queue_full() ->
    Id = 1,
    QueueSize = 10000,
    Response = erlmcp_json_rpc:error_notification_queue_full(Id, QueueSize),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32095, maps:get(<<"code">>, Error)),
    ?assertEqual(QueueSize, maps:get(<<"queueSize">>, maps:get(<<"data">>, Error))).

%%====================================================================
%% Completion Error Helper Functions Tests (-32110 to -32113)
%%====================================================================

completion_errors_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_error_completion_not_found()),
          ?_test(test_error_invalid_completion_reference()),
          ?_test(test_error_invalid_completion_argument()),
          ?_test(test_error_completion_failed())
         ]
     end}.

test_error_completion_not_found() ->
    Id = 1,
    CompletionId = <<"completion-123">>,
    Response = erlmcp_json_rpc:error_completion_not_found(Id, CompletionId),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32110, maps:get(<<"code">>, Error)),
    ?assertEqual(CompletionId, maps:get(<<"completionId">>, maps:get(<<"data">>, Error))).

test_error_invalid_completion_reference() ->
    Id = 1,
    Reference = <<"ref-456">>,
    Response = erlmcp_json_rpc:error_invalid_completion_reference(Id, Reference),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32111, maps:get(<<"code">>, Error)),
    ?assertEqual(Reference, maps:get(<<"reference">>, maps:get(<<"data">>, Error))).

test_error_invalid_completion_argument() ->
    Id = 1,
    Argument = <<"model">>,
    Details = <<"Invalid model name">>,
    Response = erlmcp_json_rpc:error_invalid_completion_argument(Id, Argument, Details),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32112, maps:get(<<"code">>, Error)).

test_error_completion_failed() ->
    Id = 1,
    CompletionId = <<"completion-789">>,
    Reason = <<"Model unavailable">>,
    Response = erlmcp_json_rpc:error_completion_failed(Id, CompletionId, Reason),
    Json = jsx:decode(Response, [return_maps]),
    Error = maps:get(<<"error">>, Json),
    ?assertEqual(-32113, maps:get(<<"code">>, Error)).

%%====================================================================
%% Integration Tests: All 89 Error Codes
%%====================================================================

error_code_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_validate_all_error_codes()),
          ?_test(test_all_error_codes_have_severity()),
          ?_test(test_all_error_codes_have_category()),
          ?_test(test_error_code_coverage_by_category()),
          ?_test(test_all_error_helpers_produce_valid_json()),
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
    Categories = [jsonrpc, mcp_core, content, resource, tool, prompt,
                  auth, protocol, pagination, task, progress, completion],
    lists:foreach(fun(Code) ->
        Category = erlmcp_json_rpc:error_category(Code),
        ?assert(lists:member(Category, Categories))
    end, ValidCodes).

test_error_code_coverage_by_category() ->
    ValidCodes = ?VALID_ERROR_CODES,
    CategoriesFound = lists:usort([erlmcp_json_rpc:error_category(C) || C <- ValidCodes]),
    ExpectedCategories = [jsonrpc, mcp_core, content, resource, tool, prompt,
                          auth, protocol, pagination, task, progress, completion],
    ?assertEqual(length(ExpectedCategories), length(CategoriesFound)).

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

test_error_code_range_coverage() ->
    %% Verify all error code ranges are covered
    Ranges = [
        {-32700, -32700},
        {-32600, -32603},
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
    %% Verify we have exactly 99 error codes (89 + 10 experimental)
    ValidCodes = ?VALID_ERROR_CODES,
    ?assertEqual(99, length(ValidCodes)),
    io:format("~n✓ All 99 error codes integrated and accessible via helper functions.~n").

%%====================================================================
%% Experimental Error Code Tests (1090-1099)
%%====================================================================

experimental_error_codes_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_elicitation_failed_error()),
          ?_test(test_elicitation_timeout_error()),
          ?_test(test_elicitation_cancelled_error()),
          ?_test(test_invalid_elicitation_mode_error()),
          ?_test(test_elicitation_security_error()),
          ?_test(test_experimental_task_not_found_error()),
          ?_test(test_task_dependency_failed_error()),
          ?_test(test_experimental_task_cancelled_error()),
          ?_test(test_experimental_task_timeout_error()),
          ?_test(test_invalid_task_state_error()),
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
    ?assertEqual(<<"Elicitation failed">>, maps:get(<<"message">>, Error)),
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
    ?assertEqual(<<"Elicitation timeout">>, maps:get(<<"message">>, Error)),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(TimeoutMs, maps:get(<<"timeoutMs">>, Data)).

test_elicitation_cancelled_error() ->
    Id = 3,
    Encoded = erlmcp_json_rpc:error_elicitation_cancelled(Id),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(1092, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Elicitation cancelled">>, maps:get(<<"message">>, Error)),
    ?assertNot(maps:is_key(<<"data">>, Error)).

test_invalid_elicitation_mode_error() ->
    Id = 4,
    Mode = <<"invalid_mode">>,
    Encoded = erlmcp_json_rpc:error_invalid_elicitation_mode(Id, Mode),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(1093, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Invalid elicitation mode">>, maps:get(<<"message">>, Error)),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(Mode, maps:get(<<"mode">>, Data)).

test_elicitation_security_error() ->
    Id = 5,
    Reason = <<"Potential security threat detected">>,
    Encoded = erlmcp_json_rpc:error_elicitation_security_error(Id, Reason),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(1094, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Elicitation security error">>, maps:get(<<"message">>, Error)),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(Reason, maps:get(<<"reason">>, Data)).

test_experimental_task_not_found_error() ->
    Id = 6,
    TaskId = <<"task-12345">>,
    Encoded = erlmcp_json_rpc:error_experimental_task_not_found(Id, TaskId),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(1095, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Task not found">>, maps:get(<<"message">>, Error)),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(TaskId, maps:get(<<"taskId">>, Data)).

test_task_dependency_failed_error() ->
    Id = 7,
    TaskId = <<"task-12345">>,
    DependencyId = <<"task-67890">>,
    Encoded = erlmcp_json_rpc:error_task_dependency_failed(Id, TaskId, DependencyId),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(1096, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Task dependency failed">>, maps:get(<<"message">>, Error)),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(TaskId, maps:get(<<"taskId">>, Data)),
    ?assertEqual(DependencyId, maps:get(<<"dependencyId">>, Data)).

test_experimental_task_cancelled_error() ->
    Id = 8,
    TaskId = <<"task-12345">>,
    Encoded = erlmcp_json_rpc:error_experimental_task_cancelled(Id, TaskId),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(1097, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Task cancelled">>, maps:get(<<"message">>, Error)),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(TaskId, maps:get(<<"taskId">>, Data)).

test_experimental_task_timeout_error() ->
    Id = 9,
    TaskId = <<"task-12345">>,
    TimeoutMs = 60000,
    Encoded = erlmcp_json_rpc:error_experimental_task_timeout(Id, TaskId, TimeoutMs),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(1098, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Task timeout">>, maps:get(<<"message">>, Error)),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(TaskId, maps:get(<<"taskId">>, Data)),
    ?assertEqual(TimeoutMs, maps:get(<<"timeoutMs">>, Data)).

test_invalid_task_state_error() ->
    Id = 10,
    TaskId = <<"task-12345">>,
    CurrentState = <<"running">>,
    ExpectedState = <<"pending">>,
    Encoded = erlmcp_json_rpc:error_invalid_task_state(Id, TaskId, CurrentState, ExpectedState),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(1099, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Invalid task state">>, maps:get(<<"message">>, Error)),
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(TaskId, maps:get(<<"taskId">>, Data)),
    ?assertEqual(CurrentState, maps:get(<<"currentState">>, Data)),
    ?assertEqual(ExpectedState, maps:get(<<"expectedState">>, Data)).

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
