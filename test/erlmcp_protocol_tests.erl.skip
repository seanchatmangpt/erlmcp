-module(erlmcp_protocol_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for JSON-RPC Protocol (erlmcp_json_rpc)
%%====================================================================

%%====================================================================
%% Request Encoding Tests
%%====================================================================

encode_request_test_() ->
    [
        ?_test(test_encode_simple_request()),
        ?_test(test_encode_request_with_params()),
        ?_test(test_encode_request_with_map_params()),
        ?_test(test_encode_request_with_list_params())
    ].

test_encode_simple_request() ->
    Id = 1,
    Method = <<"test_method">>,
    Params = #{},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)).

test_encode_request_with_params() ->
    Id = 2,
    Method = <<"method_with_params">>,
    Params = #{key => <<"value">>, number => 42},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)),
    ?assertMatch(#{<<"key">> := _, <<"number">> := _}, maps:get(<<"params">>, Decoded)).

test_encode_request_with_map_params() ->
    Id = 3,
    Method = <<"map_params">>,
    Params = #{
        nested => #{
            inner => <<"value">>
        }
    },
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{<<"nested">> := #{<<"inner">> := _}}, maps:get(<<"params">>, Decoded)).

test_encode_request_with_list_params() ->
    Id = 4,
    Method = <<"list_params">>,
    Params = [1, 2, 3],
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual([1, 2, 3], maps:get(<<"params">>, Decoded)).

%%====================================================================
%% Response Encoding Tests
%%====================================================================

encode_response_test_() ->
    [
        ?_test(test_encode_simple_response()),
        ?_test(test_encode_response_with_result()),
        ?_test(test_encode_error_response())
    ].

test_encode_simple_response() ->
    Id = 1,
    Result = <<"success">>,
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(Id, maps:get(<<"id">>, Decoded)),
    ?assertEqual(Result, maps:get(<<"result">>, Decoded)).

test_encode_response_with_result() ->
    Id = 2,
    Result = #{
        data => [1, 2, 3],
        status => <<"ok">>
    },
    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertMatch(#{<<"data">> := _, <<"status">> := _}, maps:get(<<"result">>, Decoded)).

test_encode_error_response() ->
    Id = 3,
    Code = -32600,
    Message = <<"Invalid Request">>,
    Encoded = erlmcp_json_rpc:encode_error_response(Id, Code, Message),
    ?assert(is_binary(Encoded)),
    Decoded = jsx:decode(Encoded, [return_maps]),
    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(Code, maps:get(<<"code">>, Error)),
    ?assertEqual(Message, maps:get(<<"message">>, Error)).

%%====================================================================
%% Notification Encoding Tests
%%====================================================================

encode_notification_test_() ->
    [
        ?_test(test_encode_notification()),
        ?_test(test_encode_notification_with_params())
    ].

test_encode_notification() ->
    Method = <<"notification">>,
    Params = #{},
    Result = erlmcp_json_rpc:encode_notification(Method, Params),
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)),
    ?assertNot(maps:is_key(<<"id">>, Decoded)).

test_encode_notification_with_params() ->
    Method = <<"event_occurred">>,
    Params = #{
        event_type => <<"resource_updated">>,
        resource_uri => <<"test://resource/1">>
    },
    Result = erlmcp_json_rpc:encode_notification(Method, Params),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(Method, maps:get(<<"method">>, Decoded)),
    ?assertMatch(#{<<"event_type">> := _, <<"resource_uri">> := _}, maps:get(<<"params">>, Decoded)).

%%====================================================================
%% Message Decoding Tests
%%====================================================================

decode_message_test_() ->
    [
        ?_test(test_decode_request()),
        ?_test(test_decode_response()),
        ?_test(test_decode_notification()),
        ?_test(test_decode_error_response()),
        ?_test(test_decode_invalid_json()),
        ?_test(test_decode_missing_version())
    ].

test_decode_request() ->
    Json = jsx:encode(#{
        jsonrpc => <<"2.0">>,
        id => 1,
        method => <<"test">>,
        params => #{}
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{id = 1, method = <<"test">>}}, Result).

test_decode_response() ->
    Json = jsx:encode(#{
        jsonrpc => <<"2.0">>,
        id => 1,
        result => <<"success">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_response{id = 1, result = <<"success">>}}, Result).

test_decode_notification() ->
    Json = jsx:encode(#{
        jsonrpc => <<"2.0">>,
        method => <<"notification">>,
        params => #{}
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_notification{method = <<"notification">>}}, Result).

test_decode_error_response() ->
    Json = jsx:encode(#{
        jsonrpc => <<"2.0">>,
        id => 1,
        error => #{
            code => -32600,
            message => <<"Invalid Request">>
        }
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_response{id = 1, error = #{}}}, Result).

test_decode_invalid_json() ->
    InvalidJson = <<"{invalid json}">>,
    Result = erlmcp_json_rpc:decode_message(InvalidJson),
    ?assertMatch({error, {parse_error, _}}, Result).

test_decode_missing_version() ->
    Json = jsx:encode(#{
        id => 1,
        method => <<"test">>,
        params => #{}
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({error, {invalid_request, _}}, Result).

%%====================================================================
%% Error Creation Tests
%%====================================================================

create_error_test_() ->
    [
        ?_test(test_create_parse_error()),
        ?_test(test_create_invalid_request()),
        ?_test(test_create_method_not_found()),
        ?_test(test_create_invalid_params()),
        ?_test(test_create_internal_error())
    ].

test_create_parse_error() ->
    Error = erlmcp_json_rpc:create_error(-32700, <<"Parse error">>, undefined),
    ?assertMatch(#mcp_error{code = -32700, message = <<"Parse error">>}, Error).

test_create_invalid_request() ->
    Error = erlmcp_json_rpc:create_error(-32600, <<"Invalid Request">>, undefined),
    ?assertEqual(-32600, Error#mcp_error.code).

test_create_method_not_found() ->
    Error = erlmcp_json_rpc:create_error(-32601, <<"Method not found">>, <<"test_method">>),
    ?assertEqual(<<"test_method">>, Error#mcp_error.data).

test_create_invalid_params() ->
    Error = erlmcp_json_rpc:create_error(-32602, <<"Invalid params">>, undefined),
    ?assertEqual(-32602, Error#mcp_error.code).

test_create_internal_error() ->
    ErrorData = #{reason => <<"crashed">>},
    Error = erlmcp_json_rpc:create_error(-32603, <<"Internal error">>, ErrorData),
    ?assertMatch(#mcp_error{code = -32603, data = ErrorData}, Error).

%%====================================================================
%% Round-trip Tests
%%====================================================================

roundtrip_test_() ->
    [
        ?_test(test_request_roundtrip()),
        ?_test(test_response_roundtrip()),
        ?_test(test_notification_roundtrip())
    ].

test_request_roundtrip() ->
    Id = 100,
    Method = <<"roundtrip_method">>,
    Params = #{key => <<"value">>, num => 42},

    Encoded = erlmcp_json_rpc:encode_request(Id, Method, Params),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Encoded),

    ?assertMatch(#json_rpc_request{}, Decoded),
    ?assertEqual(Id, Decoded#json_rpc_request.id),
    ?assertEqual(Method, Decoded#json_rpc_request.method).

test_response_roundtrip() ->
    Id = 101,
    Result = #{status => <<"ok">>, data => [1, 2, 3]},

    Encoded = erlmcp_json_rpc:encode_response(Id, Result),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Encoded),

    ?assertMatch(#json_rpc_response{}, Decoded),
    ?assertEqual(Id, Decoded#json_rpc_response.id).

test_notification_roundtrip() ->
    Method = <<"event">>,
    Params = #{type => <<"update">>},

    Encoded = erlmcp_json_rpc:encode_notification(Method, Params),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Encoded),

    ?assertMatch(#json_rpc_notification{}, Decoded),
    ?assertEqual(Method, Decoded#json_rpc_notification.method).

%%====================================================================
%% Edge Cases and Validation Tests
%%====================================================================

edge_cases_test_() ->
    [
        ?_test(test_null_id()),
        ?_test(test_string_id()),
        ?_test(test_empty_params()),
        ?_test(test_large_payload())
    ].

test_null_id() ->
    Method = <<"test">>,
    Params = #{},
    Result = erlmcp_json_rpc:encode_request(null, Method, Params),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Result),
    ?assertEqual(null, Decoded#json_rpc_request.id).

test_string_id() ->
    Method = <<"test">>,
    Params = #{},
    Result = erlmcp_json_rpc:encode_request(<<"string-id">>, Method, Params),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Result),
    ?assertEqual(<<"string-id">>, Decoded#json_rpc_request.id).

test_empty_params() ->
    Id = 200,
    Method = <<"empty">>,
    Result = erlmcp_json_rpc:encode_request(Id, Method, #{}),
    {ok, _} = erlmcp_json_rpc:decode_message(Result),
    ?assert(true).

test_large_payload() ->
    Id = 300,
    Method = <<"large">>,
    LargeData = lists:duplicate(1000, #{key => <<"value">>, data => lists:seq(1, 100)}),
    Params = #{large_data => LargeData},
    Result = erlmcp_json_rpc:encode_request(Id, Method, Params),
    ?assert(is_binary(Result)),
    {ok, _} = erlmcp_json_rpc:decode_message(Result),
    ?assert(true).
