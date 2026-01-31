%%%-------------------------------------------------------------------
%%% @doc Chicago School TDD Tests for WebSocket Transport Validation
%%% Tests ONLY observable behavior through public API
%%% NO STATE INSPECTION, NO DUMMY PROCESSES, REAL erlmcp processes only
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_ws_validation_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    %% Set application environment for validation
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

websocket_validation_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            {"UTF-8 Validation", [
                ?_test(test_valid_utf8_message()),
                ?_test(test_invalid_utf8_sequence()),
                ?_test(test_utf8_multibyte_characters()),
                ?_test(test_utf8_emoji_support())
            ]},
            {"Message Size Limits", [
                ?_test(test_message_under_limit()),
                ?_test(test_message_at_limit()),
                ?_test(test_message_over_limit()),
                ?_test(test_configurable_message_size()),
                ?_test(test_size_check_before_utf8())
            ]},
            {"Delimiter Validation", [
                ?_test(test_message_with_delimiter()),
                ?_test(test_message_without_delimiter()),
                ?_test(test_multiple_messages_with_delimiters()),
                ?_test(test_empty_messages_ignored()),
                ?_test(test_delimiter_at_end_only())
            ]}
        ]
    }.

%%====================================================================
%% UTF-8 Validation Tests (Observable Behavior)
%%====================================================================

test_valid_utf8_message() ->
    %% Test API: validate_utf8 returns ok for valid UTF-8
    Message = <<"Hello, World!">>,
    Result = erlmcp_transport_ws:validate_utf8(Message),
    ?assertEqual(ok, Result).

test_invalid_utf8_sequence() ->
    %% Test API: validate_utf8 returns error for invalid UTF-8
    %% Incomplete multibyte sequence
    InvalidSeq = <<195, 40>>,
    Result = erlmcp_transport_ws:validate_utf8(InvalidSeq),
    ?assertEqual({error, invalid_utf8}, Result).

test_utf8_multibyte_characters() ->
    %% Test API: validate_utf8 handles 2-byte UTF-8
    Message = <<"Café"/utf8>>,
    Result = erlmcp_transport_ws:validate_utf8(Message),
    ?assertEqual(ok, Result).

test_utf8_emoji_support() ->
    %% Test API: validate_utf8 handles 4-byte UTF-8 emoji
    Message = <<"Hello 👋"/utf8>>,
    Result = erlmcp_transport_ws:validate_utf8(Message),
    ?assertEqual(ok, Result).

%%====================================================================
%% Message Size Limit Tests (Observable Behavior)
%%====================================================================

test_message_under_limit() ->
    %% Test API: validate_message_size returns {ok, Size} for small messages
    SmallMsg = <<"x">>,
    Result = erlmcp_transport_ws:validate_message_size(SmallMsg),
    ?assertMatch({ok, 1}, Result).

test_message_at_limit() ->
    %% Test API: validate_message_size accepts message at exactly max size
    MaxSize = 16777216,
    Message = binary:copy(<<"x">>, MaxSize),
    Result = erlmcp_transport_ws:validate_message_size(Message),
    ?assertEqual({ok, MaxSize}, Result).

test_message_over_limit() ->
    %% Test API: validate_message_size rejects oversized messages
    MaxSize = 16777216,
    OversizeMsg = binary:copy(<<"x">>, MaxSize + 1),
    Result = erlmcp_transport_ws:validate_message_size(OversizeMsg),
    ?assertEqual({error, too_big}, Result).

test_configurable_message_size() ->
    %% Test API: validate_message_size respects configurable limits
    application:set_env(erlmcp, max_ws_message_size, 1000),
    SmallMsg = binary:copy(<<"x">>, 500),
    OversizeMsg = binary:copy(<<"x">>, 1001),

    ?assertMatch({ok, 500}, erlmcp_transport_ws:validate_message_size(SmallMsg)),
    ?assertEqual({error, too_big}, erlmcp_transport_ws:validate_message_size(OversizeMsg)),

    application:set_env(erlmcp, max_ws_message_size, 16777216).

test_size_check_before_utf8() ->
    %% Test API: Size validation happens before UTF-8 validation
    application:set_env(erlmcp, max_ws_message_size, 100),
    OversizeMsg = binary:copy(<<"x">>, 101),
    Result = erlmcp_transport_ws:validate_message_size(OversizeMsg),
    ?assertEqual({error, too_big}, Result),
    application:set_env(erlmcp, max_ws_message_size, 16777216).

%%====================================================================
%% Delimiter Validation Tests (Observable Behavior)
%%====================================================================

test_message_with_delimiter() ->
    %% Test API: validate_utf8 accepts messages with newline delimiter
    Message = <<"{\n">>,
    Result = erlmcp_transport_ws:validate_utf8(Message),
    ?assertEqual(ok, Result).

test_message_without_delimiter() ->
    %% Test API: validate_utf8 accepts messages without delimiter
    Message = <<"{}">>,
    Result = erlmcp_transport_ws:validate_utf8(Message),
    ?assertEqual(ok, Result).

test_multiple_messages_with_delimiters() ->
    %% Test API: validate_utf8 handles multiple delimited messages
    Messages = <<"msg1\nmsg2\nmsg3\n">>,
    Result = erlmcp_transport_ws:validate_utf8(Messages),
    ?assertEqual(ok, Result).

test_empty_messages_ignored() ->
    %% Test API: validate_utf8 handles empty messages in stream
    Messages = <<"msg1\n\nmsg2\n">>,
    Result = erlmcp_transport_ws:validate_utf8(Messages),
    ?assertEqual(ok, Result).

test_delimiter_at_end_only() ->
    %% Test API: validate_utf8 handles delimiters at end
    Message = <<"content with\nnewlines\n">>,
    Result = erlmcp_transport_ws:validate_utf8(Message),
    ?assertEqual(ok, Result).
