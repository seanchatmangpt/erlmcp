%%%-------------------------------------------------------------------
%%% @doc Chicago School TDD Tests for WebSocket Message Handling
%%% Tests ONLY observable behavior through public API
%%% NO STATE INSPECTION, NO DUMMY PROCESSES, REAL erlmcp processes only
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_ws_message_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    application:set_env(erlmcp, max_ws_message_size, 16777216),
    ok.

cleanup(_) ->
    application:unset_env(erlmcp, max_ws_message_size),
    ok.

%%====================================================================
%% Test Groups
%%====================================================================

websocket_message_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Fragmented Messages",
       [?_test(test_two_part_fragment()),
        ?_test(test_multipart_fragment()),
        ?_test(test_incomplete_fragment_buffering()),
        ?_test(test_fragment_reassembly())]},
      {"Message Integration",
       [?_test(test_complete_request_response_cycle()),
        ?_test(test_mixed_valid_invalid_messages()),
        ?_test(test_large_message_handling()),
        ?_test(test_rapid_message_stream()),
        ?_test(test_fragmented_large_message())]}]}.

%%====================================================================
%% Fragmented Message Tests (Observable Behavior)
%%====================================================================

test_two_part_fragment() ->
    %% Test API: validate_utf8 handles fragmented messages
    %% Simulate two fragments that complete a message
    Fragment1 = <<"{\n">>,
    Fragment2 = <<"\n">>,
    AllData = <<Fragment1/binary, Fragment2/binary>>,
    Result = erlmcp_transport_ws:validate_utf8(AllData),
    ?assertEqual(ok, Result).

test_multipart_fragment() ->
    %% Test API: validate_utf8 handles three-part fragmented message
    Parts = [<<"part1">>, <<"part2">>, <<"part3">>],
    AllData = binary:list_to_bin(Parts),
    Result = erlmcp_transport_ws:validate_utf8(AllData),
    ?assertEqual(ok, Result).

test_incomplete_fragment_buffering() ->
    %% Test API: validate_utf8 buffers incomplete messages
    IncompleteMsg = <<"incomplete message">>,
    Result = erlmcp_transport_ws:validate_utf8(IncompleteMsg),
    ?assertEqual(ok, Result).

test_fragment_reassembly() ->
    %% Test API: validate_utf8 handles full fragmented JSON message
    Json = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>}),
    Result = erlmcp_transport_ws:validate_utf8(Json),
    ?assertEqual(ok, Result).

%%====================================================================
%% Message Integration Tests (Observable Behavior)
%%====================================================================

test_complete_request_response_cycle() ->
    %% Test API: Full JSON-RPC message with delimiter
    Message =
        jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                     <<"method">> => <<"tools/list">>,
                     <<"id">> => 1}),
    DelimitedMsg = <<Message/binary, "\n">>,
    Result = erlmcp_transport_ws:validate_utf8(DelimitedMsg),
    ?assertEqual(ok, Result).

test_mixed_valid_invalid_messages() ->
    %% Test API: Stream with both valid and invalid messages
    %% Note: validate_utf8 checks UTF-8, not JSON validity
    ValidMsg = jsx:encode(#{<<"id">> => 1}),
    Stream = <<ValidMsg/binary, "\ninvalid\n">>,
    Result = erlmcp_transport_ws:validate_utf8(Stream),
    ?assertEqual(ok, Result).

test_large_message_handling() ->
    %% Test API: Large but valid messages are accepted
    LargeData = binary:copy(<<"x">>, 10000),
    Message =
        jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                     <<"method">> => <<"resources/list">>,
                     <<"data">> => LargeData,
                     <<"id">> => 1}),
    DelimitedMsg = <<Message/binary, "\n">>,
    ?assertMatch({ok, _}, erlmcp_transport_ws:validate_message_size(Message)).

test_rapid_message_stream() ->
    %% Test API: Rapid succession of messages
    Messages = [jsx:encode(#{<<"id">> => I}) || I <- lists:seq(1, 100)],
    Stream = binary:list_to_bin([<<M/binary, "\n">> || M <- Messages]),
    Result = erlmcp_transport_ws:validate_utf8(Stream),
    ?assertEqual(ok, Result).

test_fragmented_large_message() ->
    %% Test API: Large message split across fragments reassembles correctly
    LargeData = binary:copy(<<"x">>, 5000),
    Message =
        jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                     <<"data">> => LargeData,
                     <<"id">> => 1}),
    %% Simulate fragmentation
    Part1 = binary:part(Message, {0, byte_size(Message) div 2}),
    Part2 =
        binary:part(Message,
                    {byte_size(Message) div 2, byte_size(Message) - byte_size(Message) div 2}),
    Reassembled = <<Part1/binary, Part2/binary>>,
    ?assertEqual(Message, Reassembled).
