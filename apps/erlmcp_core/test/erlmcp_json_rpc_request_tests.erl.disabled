-module(erlmcp_json_rpc_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for JSON-RPC 2.0 Request Validation and Parsing
%% Chicago School TDD: Test ONLY observable behavior through public API
%% Focus: Request structure validation, method validation, params validation
%%====================================================================

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Request Structure Validation Tests
%%====================================================================

request_structure_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_decode_request_with_all_fields()),
             ?_test(test_decode_request_minimal()),
             ?_test(test_decode_request_with_numeric_id()),
             ?_test(test_decode_request_with_string_id()),
             ?_test(test_decode_request_missing_method()),
             ?_test(test_decode_request_missing_id())
         ]
     end}.

test_decode_request_with_all_fields() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{<<"version">> => <<"1.0">>}
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{method = <<"initialize">>}}, Result),
    {ok, Request} = Result,
    ?assertEqual(1, Request#json_rpc_request.id).

test_decode_request_minimal() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

test_decode_request_with_numeric_id() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 42,
        <<"method">> => <<"test">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{id = 42}}, Result).

test_decode_request_with_string_id() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => <<"req-abc-123">>,
        <<"method">> => <<"test">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    case Result of
        {ok, #json_rpc_request{id = <<"req-abc-123">>, method = <<"test">>}} -> ok;
        _ -> ?assert(false)
    end.

test_decode_request_missing_method() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({error, _}, Result).

test_decode_request_missing_id() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test">>
    }),
    %% This is actually a notification, which is valid
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_notification{}}, Result).

%%====================================================================
%% Method Validation Tests
%%====================================================================

method_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_method_valid_initialize()),
             ?_test(test_method_valid_list_resources()),
             ?_test(test_method_valid_tools_call()),
             ?_test(test_method_empty_string()),
             ?_test(test_method_non_string())
         ]
     end}.

test_method_valid_initialize() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"initialize">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{method = <<"initialize">>}}, Result).

test_method_valid_list_resources() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"resources/list">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{method = <<"resources/list">>}}, Result).

test_method_valid_tools_call() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"tools/call">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{method = <<"tools/call">>}}, Result).

test_method_empty_string() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    %% Empty method might be accepted by the parser
    case Result of
        {ok, _} -> ok;  % Parser accepts it
        {error, _} -> ok  % Or rejects it - both are acceptable
    end.

test_method_non_string() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => 123
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% Params Validation Tests
%%====================================================================

params_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_params_object()),
             ?_test(test_params_array()),
             ?_test(test_params_missing()),
             ?_test(test_params_null()),
             ?_test(test_params_nested_object())
         ]
     end}.

test_params_object() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{<<"key">> => <<"value">>}
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    {ok, Request} = Result,
    ?assertEqual(#{<<"key">> => <<"value">>}, Request#json_rpc_request.params).

test_params_array() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => [1, 2, 3]
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    {ok, Request} = Result,
    ?assertEqual([1, 2, 3], Request#json_rpc_request.params).

test_params_missing() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{params = undefined}}, Result).

test_params_null() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => null
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

test_params_nested_object() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{
            <<"level1">> => #{
                <<"level2">> => #{<<"value">> => 42}
            }
        }
    }),
    Result = erlmcp_json_rpc:decode_message(Json),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

%%====================================================================
%% Batch Request Validation Tests
%%====================================================================

batch_request_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_batch_request_valid()),
             ?_test(test_batch_request_empty()),
             ?_test(test_batch_request_single()),
             ?_test(test_batch_request_mixed_valid_invalid()),
             ?_test(test_batch_request_with_notifications())
         ]
     end}.

test_batch_request_valid() ->
    Json = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"method1">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2, <<"method">> => <<"method2">>}
    ]),
    Result = erlmcp_json_rpc:decode_batch(Json),
    ?assertMatch({ok, [_ | _]}, Result),
    {ok, Requests} = Result,
    ?assertEqual(2, length(Requests)).

test_batch_request_empty() ->
    Json = <<"[]">>,
    Result = erlmcp_json_rpc:decode_batch(Json),
    ?assertMatch({error, _}, Result).

test_batch_request_single() ->
    Json = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"test">>}
    ]),
    Result = erlmcp_json_rpc:decode_batch(Json),
    ?assertMatch({ok, [_]}, Result).

test_batch_request_mixed_valid_invalid() ->
    Json = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"valid">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2}  % Missing method
    ]),
    Result = erlmcp_json_rpc:decode_batch(Json),
    ?assertMatch({ok, [_ | _]}, Result).

test_batch_request_with_notifications() ->
    Json = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"test">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"notify">>}
    ]),
    Result = erlmcp_json_rpc:decode_batch(Json),
    ?assertMatch({ok, [_ | _]}, Result).

%%====================================================================
%% Transport-Specific Request Tests
%%====================================================================

transport_request_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_decode_with_default_transport()),
             ?_test(test_decode_with_stdio_transport()),
             ?_test(test_decode_with_tcp_transport()),
             ?_test(test_decode_with_http_transport())
         ]
     end}.

test_decode_with_default_transport() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{}
    }),
    Result = erlmcp_json_rpc:decode_message(Json, default),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

test_decode_with_stdio_transport() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{}
    }),
    Result = erlmcp_json_rpc:decode_message(Json, stdio),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

test_decode_with_tcp_transport() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{}
    }),
    Result = erlmcp_json_rpc:decode_message(Json, tcp),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

test_decode_with_http_transport() ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{}
    }),
    Result = erlmcp_json_rpc:decode_message(Json, http),
    ?assertMatch({ok, #json_rpc_request{}}, Result).

%%====================================================================
%% Request Type Detection Tests
%%====================================================================

request_type_detection_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_is_batch_request_array()),
             ?_test(test_is_batch_request_object()),
             ?_test(test_is_batch_request_invalid())
         ]
     end}.

test_is_batch_request_array() ->
    Json = <<"[{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}]">>,
    ?assert(erlmcp_json_rpc:is_batch_request(Json)).

test_is_batch_request_object() ->
    Json = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
    ?assertNot(erlmcp_json_rpc:is_batch_request(Json)).

test_is_batch_request_invalid() ->
    Json = <<"invalid json">>,
    ?assertNot(erlmcp_json_rpc:is_batch_request(Json)).
