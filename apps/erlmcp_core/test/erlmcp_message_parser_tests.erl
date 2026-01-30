-module(erlmcp_message_parser_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp/include/erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_message_parser Module
%% Chicago School TDD - Real message parsing, no mocks
%%====================================================================

%%====================================================================
%% JSON-RPC Version Validation Tests
%%====================================================================

validate_jsonrpc_version_test_() ->
    [
        ?_test(test_valid_version()),
        ?_test(test_invalid_version()),
        ?_test(test_missing_version())
    ].

test_valid_version() ->
    Data = #{<<"jsonrpc">> => <<"2.0">>},
    ?assertEqual(ok, erlmcp_message_parser:validate_jsonrpc_version(Data)).

test_invalid_version() ->
    Data = #{<<"jsonrpc">> => <<"1.0">>},
    Result = erlmcp_message_parser:validate_jsonrpc_version(Data),
    ?assertMatch({error, {invalid_request, {wrong_version, <<"1.0">>}}}, Result).

test_missing_version() ->
    Data = #{},
    Result = erlmcp_message_parser:validate_jsonrpc_version(Data),
    ?assertMatch({error, {invalid_request, missing_jsonrpc}}, Result).

%%====================================================================
%% Request Parsing Tests
%%====================================================================

parse_request_test_() ->
    [
        ?_test(test_parse_valid_request()),
        ?_test(test_parse_request_with_params()),
        ?_test(test_parse_request_invalid_method())
    ].

test_parse_valid_request() ->
    Id = 1,
    Method = <<"test/method">>,
    Data = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"method">> => Method
    },
    Result = erlmcp_message_parser:parse_json_rpc(Data),
    ?assertMatch({ok, #json_rpc_request{id = 1, method = Method, params = undefined}}, Result).

test_parse_request_with_params() ->
    Id = <<"test-id">>,
    Method = <<"resources/list">>,
    Params = #{<<"filter">> => <<"test">>},
    Data = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"method">> => Method,
        <<"params">> => Params
    },
    Result = erlmcp_message_parser:parse_json_rpc(Data),
    ?assertMatch({ok, #json_rpc_request{id = <<"test-id">>, method = Method, params = Params}}, Result).

test_parse_request_invalid_method() ->
    Id = 1,
    Method = 123,  % Invalid: not a binary
    Data = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"method">> => Method,
        <<"params">> => #{}
    },
    Result = erlmcp_message_parser:parse_request(Id, Method, Data),
    ?assertMatch({error, {invalid_request, {invalid_method, 123}}}, Result).

%%====================================================================
%% Response Parsing Tests
%%====================================================================

parse_response_test_() ->
    [
        ?_test(test_parse_response_success()),
        ?_test(test_parse_response_error()),
        ?_test(test_parse_response_null_result())
    ].

test_parse_response_success() ->
    Id = 42,
    Result = #{<<"status">> => <<"ok">>},
    Data = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => Result
    },
    ParseResult = erlmcp_message_parser:parse_json_rpc(Data),
    ?assertMatch({ok, #json_rpc_response{id = 42, result = Result, error = undefined}}, ParseResult).

test_parse_response_error() ->
    Id = <<"req-123">>,
    Error = #{
        <<"code">> => -32600,
        <<"message">> => <<"Invalid request">>
    },
    Data = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => Error
    },
    ParseResult = erlmcp_message_parser:parse_json_rpc(Data),
    ?assertMatch({ok, #json_rpc_response{id = <<"req-123">>, result = undefined, error = Error}}, ParseResult).

test_parse_response_null_result() ->
    Id = null,
    Result = null,
    Data = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => Result
    },
    ParseResult = erlmcp_message_parser:parse_json_rpc(Data),
    ?assertMatch({ok, #json_rpc_response{id = null, result = null}}, ParseResult).

%%====================================================================
%% Notification Parsing Tests
%%====================================================================

parse_notification_test_() ->
    [
        ?_test(test_parse_notification()),
        ?_test(test_parse_notification_with_params()),
        ?_test(test_parse_notification_invalid_method())
    ].

test_parse_notification() ->
    Method = <<"notifications/resources/list_changed">>,
    Data = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method
    },
    Result = erlmcp_message_parser:parse_json_rpc(Data),
    ?assertMatch({ok, #json_rpc_notification{method = Method, params = undefined}}, Result).

test_parse_notification_with_params() ->
    Method = <<"notifications/progress">>,
    Params = #{<<"progress">> => 50, <<"total">> => 100},
    Data = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method,
        <<"params">> => Params
    },
    Result = erlmcp_message_parser:parse_json_rpc(Data),
    ?assertMatch({ok, #json_rpc_notification{method = Method, params = Params}}, Result).

test_parse_notification_invalid_method() ->
    Method = 456,  % Invalid: not a binary
    Data = #{<<"params">> => #{}},
    Result = erlmcp_message_parser:parse_notification(Method, Data),
    ?assertMatch({error, {invalid_request, {invalid_method, 456}}}, Result).

%%====================================================================
%% Message Type Detection Tests
%%====================================================================

parse_by_type_test_() ->
    [
        ?_test(test_detect_request()),
        ?_test(test_detect_response_result()),
        ?_test(test_detect_response_error()),
        ?_test(test_detect_notification()),
        ?_test(test_unknown_message_type())
    ].

test_detect_request() ->
    Data = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test/method">>
    },
    Result = erlmcp_message_parser:parse_json_rpc(Data),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

test_detect_response_result() ->
    Data = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => #{}
    },
    Result = erlmcp_message_parser:parse_json_rpc(Data),
    ?assertMatch({ok, #json_rpc_response{result = #{}}}, Result).

test_detect_response_error() ->
    Data = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"error">> => #{<<"code">> => -32600}
    },
    Result = erlmcp_message_parser:parse_json_rpc(Data),
    ?assertMatch({ok, #json_rpc_response{error = #{<<"code">> := -32600}}}, Result).

test_detect_notification() ->
    Data = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"notification/test">>
    },
    Result = erlmcp_message_parser:parse_json_rpc(Data),
    ?assertMatch({ok, #json_rpc_notification{}}, Result).

test_unknown_message_type() ->
    Data = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"unknown">> => <<"field">>
    },
    Result = erlmcp_message_parser:parse_by_type(Data),
    ?assertMatch({error, {invalid_request, unknown_message_type}}, Result).

%%====================================================================
%% ID Decoding Tests
%%====================================================================

decode_id_test_() ->
    [
        ?_test(test_decode_null_id()),
        ?_test(test_decode_binary_id()),
        ?_test(test_decode_integer_id()),
        ?_test(test_decode_other_id())
    ].

test_decode_null_id() ->
    ?assertEqual(null, erlmcp_message_parser:decode_id(null)).

test_decode_binary_id() ->
    Id = <<"request-123">>,
    ?assertEqual(Id, erlmcp_message_parser:decode_id(Id)).

test_decode_integer_id() ->
    Id = 42,
    ?assertEqual(42, erlmcp_message_parser:decode_id(Id)).

test_decode_other_id() ->
    %% Non-standard ID types are passed through
    Id = {custom, id},
    ?assertEqual(Id, erlmcp_message_parser:decode_id(Id)).

%%====================================================================
%% Parameter Validation Tests
%%====================================================================

validate_params_test_() ->
    [
        ?_test(test_validate_undefined_params()),
        ?_test(test_validate_map_params()),
        ?_test(test_validate_list_params()),
        ?_test(test_validate_invalid_params())
    ].

test_validate_undefined_params() ->
    ?assertEqual(undefined, erlmcp_message_parser:validate_params(undefined)).

test_validate_map_params() ->
    Params = #{<<"key">> => <<"value">>},
    ?assertEqual(Params, erlmcp_message_parser:validate_params(Params)).

test_validate_list_params() ->
    Params = [1, 2, 3],
    ?assertEqual(Params, erlmcp_message_parser:validate_params(Params)).

test_validate_invalid_params() ->
    %% Invalid params types return undefined
    ?assertEqual(undefined, erlmcp_message_parser:validate_params(<<"string">>)),
    ?assertEqual(undefined, erlmcp_message_parser:validate_params(123)),
    ?assertEqual(undefined, erlmcp_message_parser:validate_params(atom)).

%%====================================================================
%% Integration Tests - Full Message Parsing
%%====================================================================

integration_test_() ->
    [
        ?_test(test_full_request_parsing()),
        ?_test(test_full_response_parsing()),
        ?_test(test_full_notification_parsing()),
        ?_test(test_malformed_messages())
    ].

test_full_request_parsing() ->
    %% Simulate a complete JSON-RPC request message
    Message = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 100,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => <<"calculator">>,
            <<"arguments">> => #{<<"op">> => <<"add">>, <<"a">> => 5, <<"b">> => 3}
        }
    },
    {ok, Parsed} = erlmcp_message_parser:parse_json_rpc(Message),
    ?assertMatch(#json_rpc_request{id = 100, method = <<"tools/call">>}, Parsed),
    #json_rpc_request{params = Params} = Parsed,
    ?assertMatch(#{<<"name">> := <<"calculator">>}, Params).

test_full_response_parsing() ->
    %% Simulate a complete JSON-RPC response message
    Message = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 200,
        <<"result">> => #{
            <<"content">> => [#{
                <<"type">> => <<"text">>,
                <<"text">> => <<"Result: 8">>
            }]
        }
    },
    {ok, Parsed} = erlmcp_message_parser:parse_json_rpc(Message),
    ?assertMatch(#json_rpc_response{id = 200, error = undefined}, Parsed),
    #json_rpc_response{result = Result} = Parsed,
    ?assertMatch(#{<<"content">> := _}, Result).

test_full_notification_parsing() ->
    %% Simulate a complete notification message
    Message = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"notifications/resources/updated">>,
        <<"params">> => #{
            <<"uri">> => <<"file:///test.txt">>,
            <<"metadata">> => #{<<"modified">> => true}
        }
    },
    {ok, Parsed} = erlmcp_message_parser:parse_json_rpc(Message),
    ?assertMatch(#json_rpc_notification{method = <<"notifications/resources/updated">>}, Parsed),
    #json_rpc_notification{params = Params} = Parsed,
    ?assertMatch(#{<<"uri">> := <<"file:///test.txt">>}, Params).

test_malformed_messages() ->
    %% Test various malformed messages
    ?assertMatch({error, {invalid_request, missing_jsonrpc}},
                 erlmcp_message_parser:parse_json_rpc(#{})),
    ?assertMatch({error, {invalid_request, {wrong_version, <<"1.0">>}}},
                 erlmcp_message_parser:parse_json_rpc(#{<<"jsonrpc">> => <<"1.0">>})),
    ?assertMatch({error, {invalid_request, unknown_message_type}},
                 erlmcp_message_parser:parse_json_rpc(#{
                     <<"jsonrpc">> => <<"2.0">>,
                     <<"data">> => <<"unknown">>
                 })).
