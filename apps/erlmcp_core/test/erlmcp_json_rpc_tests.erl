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
