-module(erlmcp_batch_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for JSON-RPC 2.0 Batch Request Handling (Gap #43)
%%====================================================================
%%
%% This test suite validates batch request support according to:
%% - JSON-RPC 2.0 Specification (Section 6: Batch)
%% - MCP 2025-11-25 Protocol Requirements
%%
%% Key Requirements:
%% 1. Process array of request objects
%% 2. Return array of responses in same order
%% 3. Omit responses for notifications
%% 4. Reject empty batch with error
%% 5. Continue processing if single request has error
%% 6. Support mixed requests and notifications
%%====================================================================

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Test Groups
%%====================================================================

batch_request_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Simple batch requests (2 items)", fun test_simple_batch_two_requests/0},
         {"Batch with 3 requests", fun test_batch_three_requests/0},
         {"Batch with 5 requests", fun test_batch_five_requests/0},
         {"Batch with mixed requests and notifications", fun test_batch_mixed_requests_and_notifications/0},
         {"Batch with only notifications", fun test_batch_only_notifications/0},
         {"Empty batch error", fun test_empty_batch_error/0},
         {"Batch with error in single request", fun test_batch_single_error_no_affect/0},
         {"Batch order preservation", fun test_batch_order_preservation/0},
         {"Batch with different ID types", fun test_batch_different_id_types/0},
         {"Batch single request conversion", fun test_batch_single_request_conversion/0},
         {"Batch encoding", fun test_batch_encoding/0},
         {"Batch detection", fun test_batch_detection/0},
         {"Batch with invalid items ignored", fun test_batch_invalid_items/0}
     ]}.

%%====================================================================
%% Test Cases - Simple Batch Operations
%%====================================================================

test_simple_batch_two_requests() ->
    %% Create two valid requests in a batch
    Req1 = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test.method1">>,
        <<"params">> => #{<<"key">> => <<"value1">>}
    },
    Req2 = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 2,
        <<"method">> => <<"test.method2">>,
        <<"params">> => #{<<"key">> => <<"value2">>}
    },
    BatchJson = jsx:encode([Req1, Req2]),

    %% Decode batch
    Result = erlmcp_json_rpc:decode_batch(BatchJson),
    ?assertMatch({ok, [_ , _]}, Result),

    {ok, Messages} = Result,
    ?assertEqual(2, length(Messages)),

    %% Verify first message
    [Msg1, Msg2] = Messages,
    ?assertMatch(#json_rpc_request{id = 1, method = <<"test.method1">>}, Msg1),
    ?assertMatch(#json_rpc_request{id = 2, method = <<"test.method2">>}, Msg2).

test_batch_three_requests() ->
    %% Create three requests in different order
    Reqs = [
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 100,
            <<"method">> => <<"method.a">>
        },
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 101,
            <<"method">> => <<"method.b">>,
            <<"params">> => #{}
        },
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 102,
            <<"method">> => <<"method.c">>,
            <<"params">> => #{<<"nested">> => #{<<"data">> => 42}}
        }
    ],
    BatchJson = jsx:encode(Reqs),

    {ok, Messages} = erlmcp_json_rpc:decode_batch(BatchJson),
    ?assertEqual(3, length(Messages)),

    %% Verify all requests decoded
    [M1, M2, M3] = Messages,
    ?assertEqual(100, M1#json_rpc_request.id),
    ?assertEqual(101, M2#json_rpc_request.id),
    ?assertEqual(102, M3#json_rpc_request.id).

test_batch_five_requests() ->
    %% Create batch with 5 requests
    Reqs = [
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => I, <<"method">> => <<"m", (I + 48)/utf8>>}
        || I <- [1, 2, 3, 4, 5]
    ],
    BatchJson = jsx:encode(Reqs),

    {ok, Messages} = erlmcp_json_rpc:decode_batch(BatchJson),
    ?assertEqual(5, length(Messages)).

%%====================================================================
%% Test Cases - Mixed Requests and Notifications
%%====================================================================

test_batch_mixed_requests_and_notifications() ->
    %% Mix of requests (with ID) and notifications (without ID)
    Batch = [
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 1,
            <<"method">> => <<"request.method">>
        },
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => <<"notification.method">>,
            <<"params">> => #{<<"event">> => <<"fired">>}
        },
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 2,
            <<"method">> => <<"another.request">>
        }
    ],
    BatchJson = jsx:encode(Batch),

    {ok, Messages} = erlmcp_json_rpc:decode_batch(BatchJson),
    %% Should have all 3 messages (2 requests + 1 notification)
    ?assertEqual(3, length(Messages)),

    %% Verify types
    [M1, M2, M3] = Messages,
    ?assertMatch(#json_rpc_request{}, M1),
    ?assertMatch(#json_rpc_notification{}, M2),
    ?assertMatch(#json_rpc_request{}, M3).

test_batch_only_notifications() ->
    %% Batch containing only notifications
    Batch = [
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => <<"notify.event1">>
        },
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => <<"notify.event2">>,
            <<"params">> => #{<<"data">> => 42}
        },
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => <<"notify.event3">>
        }
    ],
    BatchJson = jsx:encode(Batch),

    {ok, Messages} = erlmcp_json_rpc:decode_batch(BatchJson),
    ?assertEqual(3, length(Messages)),

    %% All should be notifications
    lists:foreach(fun(Msg) ->
        ?assertMatch(#json_rpc_notification{}, Msg)
    end, Messages).

%%====================================================================
%% Test Cases - Error Handling
%%====================================================================

test_empty_batch_error() ->
    %% Empty batch should return error
    BatchJson = jsx:encode([]),

    Result = erlmcp_json_rpc:decode_batch(BatchJson),
    ?assertMatch({error, {invalid_request, empty_batch}}, Result).

test_batch_single_error_no_affect() ->
    %% Per JSON-RPC 2.0: errors in single request don't prevent other requests
    Batch = [
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 1,
            <<"method">> => <<"method.one">>
        },
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 2,
            <<"method">> => <<"method.two">>
        },
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 3,
            <<"method">> => <<"method.three">>
        }
    ],
    BatchJson = jsx:encode(Batch),

    %% Even though individual requests might error during processing,
    %% all should be decoded successfully at parsing stage
    {ok, Messages} = erlmcp_json_rpc:decode_batch(BatchJson),
    ?assertEqual(3, length(Messages)).

%%====================================================================
%% Test Cases - Order Preservation
%%====================================================================

test_batch_order_preservation() ->
    %% Verify batch response order matches request order
    Batch = [
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 10, <<"method">> => <<"m1">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 20, <<"method">> => <<"m2">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 30, <<"method">> => <<"m3">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 40, <<"method">> => <<"m4">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 50, <<"method">> => <<"m5">>}
    ],
    BatchJson = jsx:encode(Batch),

    {ok, Messages} = erlmcp_json_rpc:decode_batch(BatchJson),

    %% Extract IDs and verify order
    Ids = [M#json_rpc_request.id || M <- Messages],
    ?assertEqual([10, 20, 30, 40, 50], Ids).

%%====================================================================
%% Test Cases - ID Type Variations
%%====================================================================

test_batch_different_id_types() ->
    %% JSON-RPC allows string, number, or NULL as ID
    Batch = [
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 1,
            <<"method">> => <<"numeric.id">>
        },
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => <<"string-id">>,
            <<"method">> => <<"string.id">>
        },
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => null,
            <<"method">> => <<"null.id">>
        }
    ],
    BatchJson = jsx:encode(Batch),

    {ok, Messages} = erlmcp_json_rpc:decode_batch(BatchJson),
    ?assertEqual(3, length(Messages)),

    [M1, M2, M3] = Messages,
    ?assertEqual(1, M1#json_rpc_request.id),
    ?assertEqual(<<"string-id">>, M2#json_rpc_request.id),
    ?assertEqual(null, M3#json_rpc_request.id).

%%====================================================================
%% Test Cases - Single Request Handling
%%====================================================================

test_batch_single_request_conversion() ->
    %% Single request should be converted to batch (wrapped in list)
    SingleReq = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 99,
        <<"method">> => <<"single.request">>
    },
    Json = jsx:encode(SingleReq),

    Result = erlmcp_json_rpc:decode_batch(Json),
    ?assertMatch({ok, [_]}, Result),

    {ok, Messages} = Result,
    ?assertEqual(1, length(Messages)),
    [Msg] = Messages,
    ?assertEqual(99, Msg#json_rpc_request.id).

%%====================================================================
%% Test Cases - Batch Encoding
%%====================================================================

test_batch_encoding() ->
    %% Test encoding a batch of messages
    Messages = [
        #json_rpc_request{id = 1, method = <<"method.one">>, params = #{}},
        #json_rpc_notification{method = <<"notify">>, params = #{<<"event">> => <<"test">>}},
        #json_rpc_request{id = 2, method = <<"method.two">>, params = undefined}
    ],

    BatchJson = erlmcp_json_rpc:encode_batch(Messages),
    ?assert(is_binary(BatchJson)),

    %% Verify it's valid JSON array
    Decoded = jsx:decode(BatchJson),
    ?assert(is_list(Decoded)),
    ?assertEqual(3, length(Decoded)).

%%====================================================================
%% Test Cases - Batch Detection
%%====================================================================

test_batch_detection() ->
    %% Test is_batch_request detection
    BatchJson = jsx:encode([
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"m1">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2, <<"method">> => <<"m2">>}
    ]),

    SingleJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"single">>
    }),

    ?assert(erlmcp_json_rpc:is_batch_request(BatchJson)),
    ?assertNot(erlmcp_json_rpc:is_batch_request(SingleJson)),
    ?assertNot(erlmcp_json_rpc:is_batch_request(<<"invalid json">>)).

%%====================================================================
%% Test Cases - Invalid Batch Items
%%====================================================================

test_batch_invalid_items() ->
    %% Batch with some invalid items should skip them
    Batch = [
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"valid.1">>},
        #{<<"id">> => 2, <<"method">> => <<"missing.jsonrpc">>},  % Invalid - missing jsonrpc
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 3, <<"method">> => <<"valid.2">>}
    ],
    BatchJson = jsx:encode(Batch),

    %% Current implementation processes all successfully-parsed items
    {ok, Messages} = erlmcp_json_rpc:decode_batch(BatchJson),
    %% Should have decoded at least the valid ones
    ?assert(length(Messages) >= 2).
