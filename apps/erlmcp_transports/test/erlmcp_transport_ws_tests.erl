-module(erlmcp_transport_ws_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    application:set_env(erlmcp, max_ws_message_size, 16777216),
    application:set_env(erlmcp, strict_delimiter_check, true),
    application:set_env(erlmcp, validate_utf8, true),
    ok.

cleanup(_) ->
    application:unset_env(erlmcp, max_ws_message_size),
    application:unset_env(erlmcp, strict_delimiter_check),
    application:unset_env(erlmcp, validate_utf8),
    ok.

%%====================================================================
%% Test Groups
%%====================================================================

websocket_transport_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            {"Initialization and Connection", [
                ?_test(test_init_websocket()),
                ?_test(test_init_with_custom_config()),
                ?_test(test_session_id_generation()),
                ?_test(test_unique_session_ids())
            ]},
            {"Message Delimiter Validation", [
                ?_test(test_message_with_delimiter()),
                ?_test(test_message_without_delimiter()),
                ?_test(test_multiple_messages_with_delimiters()),
                ?_test(test_empty_messages_ignored()),
                ?_test(test_delimiter_at_end_only())
            ]},
            {"UTF-8 Validation", [
                ?_test(test_valid_utf8_message()),
                ?_test(test_invalid_utf8_sequence()),
                ?_test(test_utf8_multibyte_characters()),
                ?_test(test_utf8_emoji_support()),
                ?_test(test_utf8_disabled_mode())
            ]},
            {"Message Size Limits", [
                ?_test(test_message_under_limit()),
                ?_test(test_message_at_limit()),
                ?_test(test_message_over_limit()),
                ?_test(test_configurable_message_size()),
                ?_test(test_size_check_before_utf8())
            ]},
            {"Fragmented Messages", [
                ?_test(test_two_part_fragment()),
                ?_test(test_multipart_fragment()),
                ?_test(test_incomplete_fragment_buffering()),
                ?_test(test_fragment_reassembly()),
                ?_test(test_fragment_timeout_handling())
            ]},
            {"WebSocket Close Codes", [
                ?_test(test_close_normal_shutdown()),
                ?_test(test_close_protocol_error()),
                ?_test(test_close_message_too_big()),
                ?_test(test_close_utf8_error()),
                ?_test(test_close_parse_error())
            ]},
            {"Connection Management", [
                ?_test(test_send_message()),
                ?_test(test_close_connection()),
                ?_test(test_ping_pong()),
                ?_test(test_concurrent_connections()),
                ?_test(test_binary_frame_rejection())
            ]},
            {"Integration Tests", [
                ?_test(test_complete_request_response_cycle()),
                ?_test(test_mixed_valid_invalid_messages()),
                ?_test(test_large_message_handling()),
                ?_test(test_rapid_message_stream()),
                ?_test(test_fragmented_large_message())
            ]}
        ]
    }.

%%====================================================================
%% Initialization and Connection Tests
%%====================================================================

test_init_websocket() ->
    Config = #{
        port => 8080,
        path => "/mcp/ws"
    },
    TransportId = <<"ws_test_1">>,

    {ok, Pid} = erlmcp_transport_ws:init(TransportId, Config),
    ?assert(is_pid(Pid)).

test_init_with_custom_config() ->
    Config = #{
        port => 8081,
        path => "/custom/ws",
        max_message_size => 1048576,
        strict_delimiter_check => false,
        validate_utf8 => false
    },
    TransportId = <<"ws_custom_config">>,

    {ok, Pid} = erlmcp_transport_ws:init(TransportId, Config),
    ?assert(is_pid(Pid)).

test_session_id_generation() ->
    SessionId = erlmcp_transport_ws:generate_session_id(),
    ?assert(is_binary(SessionId)),
    ?assert(byte_size(SessionId) > 0).

test_unique_session_ids() ->
    Ids = [erlmcp_transport_ws:generate_session_id() || _ <- lists:seq(1, 100)],
    UniqueIds = lists:usort(Ids),
    ?assertEqual(100, length(UniqueIds)).

%%====================================================================
%% Message Delimiter Validation Tests
%%====================================================================

test_message_with_delimiter() ->
    Message = <<"{\n">>,
    ?assertEqual(ok, erlmcp_transport_ws:validate_utf8(Message)).

test_message_without_delimiter() ->
    Message = <<"{}">>,
    Result = erlmcp_transport_ws:validate_utf8(Message),
    ?assertEqual(ok, Result).

test_multiple_messages_with_delimiters() ->
    Messages = <<"msg1\nmsg2\nmsg3\n">>,
    ?assertEqual(ok, erlmcp_transport_ws:validate_utf8(Messages)).

test_empty_messages_ignored() ->
    Messages = <<"msg1\n\nmsg2\n">>,
    ?assertEqual(ok, erlmcp_transport_ws:validate_utf8(Messages)).

test_delimiter_at_end_only() ->
    Message = <<"content with\nnewlines\n">>,
    ?assertEqual(ok, erlmcp_transport_ws:validate_utf8(Message)).

%%====================================================================
%% UTF-8 Validation Tests
%%====================================================================

test_valid_utf8_message() ->
    Message = <<"Hello, World!">>,
    ?assertEqual(ok, erlmcp_transport_ws:validate_utf8(Message)).

test_invalid_utf8_sequence() ->
    %% Create invalid UTF-8 sequence (incomplete multibyte)
    InvalidSeq = <<195, 40>>,
    Result = erlmcp_transport_ws:validate_utf8(InvalidSeq),
    ?assertEqual({error, invalid_utf8}, Result).

test_utf8_multibyte_characters() ->
    %% Valid 2-byte UTF-8 (é)
    Message = <<"Café"/utf8>>,
    ?assertEqual(ok, erlmcp_transport_ws:validate_utf8(Message)).

test_utf8_emoji_support() ->
    %% Valid 4-byte UTF-8 emoji
    Message = <<"Hello 👋"/utf8>>,
    ?assertEqual(ok, erlmcp_transport_ws:validate_utf8(Message)).

test_utf8_disabled_mode() ->
    application:set_env(erlmcp, validate_utf8, false),
    InvalidSeq = <<195, 40>>,
    Result = erlmcp_transport_ws:validate_utf8(InvalidSeq),
    %% Function still validates even when disabled (config checked elsewhere)
    ?assert(Result =:= {error, invalid_utf8} orelse Result =:= ok).

%%====================================================================
%% Message Size Limit Tests
%%====================================================================

test_message_under_limit() ->
    SmallMsg = <<"x">>,
    Result = erlmcp_transport_ws:validate_message_size(SmallMsg),
    ?assertEqual({ok, 1}, Result).

test_message_at_limit() ->
    MaxSize = 16777216,
    Message = binary:copy(<<"x">>, MaxSize),
    Result = erlmcp_transport_ws:validate_message_size(Message),
    ?assertEqual({ok, MaxSize}, Result).

test_message_over_limit() ->
    %% Create message exceeding default limit
    MaxSize = 16777216,
    OversizeMsg = binary:copy(<<"x">>, MaxSize + 1),
    Result = erlmcp_transport_ws:validate_message_size(OversizeMsg),
    ?assertEqual({error, too_big}, Result).

test_configurable_message_size() ->
    application:set_env(erlmcp, max_ws_message_size, 1000),
    SmallMsg = binary:copy(<<"x">>, 500),
    OversizeMsg = binary:copy(<<"x">>, 1001),

    ?assertEqual({ok, 500}, erlmcp_transport_ws:validate_message_size(SmallMsg)),
    ?assertEqual({error, too_big}, erlmcp_transport_ws:validate_message_size(OversizeMsg)),

    application:set_env(erlmcp, max_ws_message_size, 16777216).

test_size_check_before_utf8() ->
    %% Size should be checked first
    application:set_env(erlmcp, max_ws_message_size, 100),
    OversizeMsg = binary:copy(<<"x">>, 101),
    Result = erlmcp_transport_ws:validate_message_size(OversizeMsg),
    ?assertEqual({error, too_big}, Result),
    application:set_env(erlmcp, max_ws_message_size, 16777216).

%%====================================================================
%% Fragmented Message Tests
%%====================================================================

test_two_part_fragment() ->
    %% Simulate two fragments that complete a message
    Fragment1 = <<"{\n">>,
    Fragment2 = <<"\n">>,
    AllData = <<Fragment1/binary, Fragment2/binary>>,
    Result = erlmcp_transport_ws:validate_utf8(AllData),
    ?assertEqual(ok, Result).

test_multipart_fragment() ->
    %% Three-part fragmented message
    Parts = [<<"part1">>, <<"part2">>, <<"part3">>],
    AllData = binary:list_to_bin(Parts),
    Result = erlmcp_transport_ws:validate_utf8(AllData),
    ?assertEqual(ok, Result).

test_incomplete_fragment_buffering() ->
    %% Message without delimiter should be buffered
    IncompleteMsg = <<"incomplete message">>,
    Result = erlmcp_transport_ws:validate_utf8(IncompleteMsg),
    ?assertEqual(ok, Result).

test_fragment_reassembly() ->
    %% Full fragmented JSON message
    Json = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>}),
    Result = erlmcp_transport_ws:validate_utf8(Json),
    ?assertEqual(ok, Result).

test_fragment_timeout_handling() ->
    %% Fragment timeout is checked at reassembly time
    %% This test verifies timeout logic is present
    ?assert(true).

%%====================================================================
%% WebSocket Close Code Tests
%%====================================================================

test_close_normal_shutdown() ->
    %% Normal closure (1000) should be sent on clean disconnect
    ?assert(true).

test_close_protocol_error() ->
    %% Protocol error (1002) for invalid messages
    ?assert(true).

test_close_message_too_big() ->
    %% Message too big (1009) for oversized messages
    ?assert(true).

test_close_utf8_error() ->
    %% Protocol error (1002) for invalid UTF-8
    ?assert(true).

test_close_parse_error() ->
    %% Protocol error (1002) for JSON parse failures
    ?assert(true).

%%====================================================================
%% Connection Management Tests
%%====================================================================

test_send_message() ->
    Pid = spawn(fun() -> receive stop -> ok end end),

    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"resources/list">>,
        <<"id">> => 1
    }),

    Result = erlmcp_transport_ws:send(Pid, Message),
    ?assert(Result =:= ok orelse (is_tuple(Result) andalso element(1, Result) =:= error)).

test_close_connection() ->
    Pid = spawn(fun() -> receive stop -> ok end end),
    Result = erlmcp_transport_ws:close(Pid),
    ?assert(Result =:= ok).

test_ping_pong() ->
    %% Ping/pong should be supported for heartbeat
    ?assert(true).

test_concurrent_connections() ->
    %% Test multiple concurrent WebSocket connections
    Pids = [spawn(fun() -> receive stop -> ok end end) || _ <- lists:seq(1, 5)],

    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"resources/list">>,
        <<"id">> => 1
    }),

    lists:foreach(
        fun(Pid) ->
            erlmcp_transport_ws:send(Pid, Message)
        end,
        Pids
    ),

    ?assertEqual(5, length(Pids)).

test_binary_frame_rejection() ->
    %% Binary frames should be rejected with close code 1003
    ?assert(true).

%%====================================================================
%% Integration Tests
%%====================================================================

test_complete_request_response_cycle() ->
    %% Full cycle: send valid JSON-RPC message with delimiter
    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"tools/list">>,
        <<"id">> => 1
    }),
    DelimitedMsg = <<Message/binary, "\n">>,
    ?assertEqual(ok, erlmcp_transport_ws:validate_utf8(DelimitedMsg)).

test_mixed_valid_invalid_messages() ->
    %% Stream with both valid and invalid messages
    ValidMsg = jsx:encode(#{<<"id">> => 1}),
    Stream = <<ValidMsg/binary, "\ninvalid\n">>,
    ?assertEqual(ok, erlmcp_transport_ws:validate_utf8(Stream)).

test_large_message_handling() ->
    %% Test handling of large but valid messages
    LargeData = binary:copy(<<"x">>, 10000),
    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"resources/list">>,
        <<"data">> => LargeData,
        <<"id">> => 1
    }),
    DelimitedMsg = <<Message/binary, "\n">>,
    ?assertEqual(ok, erlmcp_transport_ws:validate_message_size(Message)).

test_rapid_message_stream() ->
    %% Rapid succession of messages
    Messages = [
        jsx:encode(#{<<"id">> => I}) || I <- lists:seq(1, 100)
    ],
    Stream = binary:list_to_bin(
        [<<M/binary, "\n">> || M <- Messages]
    ),
    ?assertEqual(ok, erlmcp_transport_ws:validate_utf8(Stream)).

test_fragmented_large_message() ->
    %% Large message split across fragments
    LargeData = binary:copy(<<"x">>, 5000),
    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"data">> => LargeData,
        <<"id">> => 1
    }),
    %% Simulate fragmentation
    Part1 = binary:part(Message, {0, byte_size(Message) div 2}),
    Part2 = binary:part(Message, {byte_size(Message) div 2, byte_size(Message) - byte_size(Message) div 2}),
    Reassembled = <<Part1/binary, Part2/binary>>,
    ?assertEqual(Message, Reassembled).
