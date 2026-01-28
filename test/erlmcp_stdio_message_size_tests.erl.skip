%%%-------------------------------------------------------------------
%% @doc Test Suite for Stdio Message Size Validation (Gap #45)
%%
%% Tests message size limit validation in the stdio transport:
%% - Message validation before buffering
%% - Default 16MB limit consistency
%% - Configurable limits via sys.config
%% - Error response format
%% - Cleanup after rejected messages
%% - Edge cases and boundary conditions
%%
%% Configuration (sys.config):
%% {erlmcp, [
%%     {message_size_limits, #{
%%         stdio => 16777216  % 16 MB default
%%     }}
%% ]}
%%
%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_stdio_message_size_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Main Test Suite
%%====================================================================

stdio_message_size_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_message_under_limit()),
            ?_test(test_message_at_limit()),
            ?_test(test_message_over_limit()),
            ?_test(test_custom_limit_from_config()),
            ?_test(test_error_response_format()),
            ?_test(test_zero_byte_message()),
            ?_test(test_one_byte_over_limit()),
            ?_test(test_very_large_message()),
            ?_test(test_empty_line_handling()),
            ?_test(test_stdio_limit_consistency()),
            ?_test(test_error_response_includes_size_info()),
            ?_test(test_validation_with_different_encodings())
        ]
    }.

%%====================================================================
%% Individual Tests
%%====================================================================

%% Test: Message well under the limit is accepted
test_message_under_limit() ->
    SmallMessage = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
    Result = erlmcp_message_size:validate_stdio_size(SmallMessage),
    ?assertEqual(ok, Result).

%% Test: Message exactly at the limit is accepted
test_message_at_limit() ->
    Limit = erlmcp_message_size:get_limit(stdio),
    Message = binary:copy(<<"x">>, Limit),
    Result = erlmcp_message_size:validate_stdio_size(Message),
    ?assertEqual(ok, Result).

%% Test: Message over the limit is rejected with error
test_message_over_limit() ->
    Limit = erlmcp_message_size:get_limit(stdio),
    OversizeMessage = binary:copy(<<"x">>, Limit + 1),
    Result = erlmcp_message_size:validate_stdio_size(OversizeMessage),
    ?assertMatch({error, {message_too_large, _}}, Result).

%% Test: Configurable limit from sys.config
test_custom_limit_from_config() ->
    Config = erlmcp_message_size:get_size_limit_config(),
    StdioLimit = maps:get(stdio, Config),

    %% Verify it's configured (should be 16MB by default)
    ?assertEqual(16777216, StdioLimit),

    %% Verify it's same as other transports for consistency
    HttpLimit = maps:get(http_body, Config),
    DefaultLimit = maps:get(default, Config),
    ?assertEqual(HttpLimit, StdioLimit),
    ?assertEqual(DefaultLimit, StdioLimit).

%% Test: Error response is properly formatted JSON-RPC
test_error_response_format() ->
    Limit = erlmcp_message_size:get_limit(stdio),
    OversizeMessage = binary:copy(<<"y">>, Limit + 1),

    Result = erlmcp_message_size:validate_stdio_size(OversizeMessage),
    ?assertMatch({error, {message_too_large, _}}, Result),

    {error, {message_too_large, ErrorResponse}} = Result,

    %% Verify it's valid JSON
    ?assert(is_binary(ErrorResponse)),

    %% Decode and verify structure
    DecodedError = jsx:decode(ErrorResponse, [return_maps]),

    %% Should have JSON-RPC structure
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, DecodedError)),
    ?assert(maps:is_key(<<"error">>, DecodedError)),

    %% Error object should have correct code and message
    Error = maps:get(<<"error">>, DecodedError),
    ?assertEqual(?MCP_ERROR_MESSAGE_TOO_LARGE, maps:get(<<"code">>, Error)),
    ?assertEqual(?MCP_MSG_MESSAGE_TOO_LARGE, maps:get(<<"message">>, Error)).

%% Test: Zero-byte message is allowed (can be empty JSON or just whitespace)
test_zero_byte_message() ->
    EmptyMessage = <<"">>,
    Result = erlmcp_message_size:validate_stdio_size(EmptyMessage),
    ?assertEqual(ok, Result).

%% Test: One byte over limit is rejected
test_one_byte_over_limit() ->
    Limit = erlmcp_message_size:get_limit(stdio),
    Message = binary:copy(<<"a">>, Limit + 1),
    Result = erlmcp_message_size:validate_stdio_size(Message),
    ?assertMatch({error, {message_too_large, _}}, Result).

%% Test: Very large message detection
test_very_large_message() ->
    %% Create a message that's 20MB (exceeds 16MB default)
    VeryLargeMessage = binary:copy(<<"x">>, 20 * 1024 * 1024),
    Result = erlmcp_message_size:validate_stdio_size(VeryLargeMessage),
    ?assertMatch({error, {message_too_large, _}}, Result).

%% Test: Empty lines should be handled gracefully
test_empty_line_handling() ->
    %% Empty message should pass validation
    EmptyLine = <<"">>,
    Result = erlmcp_message_size:validate_stdio_size(EmptyLine),
    ?assertEqual(ok, Result),

    %% Whitespace-only lines should also pass validation
    WhitespaceMessage = <<"   ">>,
    Result2 = erlmcp_message_size:validate_stdio_size(WhitespaceMessage),
    ?assertEqual(ok, Result2).

%% Test: Stdio limit is consistent with default and HTTP limits
test_stdio_limit_consistency() ->
    StdioLimit = erlmcp_message_size:get_limit(stdio),
    HttpLimit = erlmcp_message_size:get_limit(http),
    DefaultLimit = erlmcp_message_size:get_limit(default),

    %% All should be equal by default
    ?assertEqual(StdioLimit, HttpLimit),
    ?assertEqual(StdioLimit, DefaultLimit),

    %% All should be the standard 16MB
    ?assertEqual(?MCP_DEFAULT_MESSAGE_SIZE_LIMIT, StdioLimit).

%% Test: Error response includes size information
test_error_response_includes_size_info() ->
    MaxSize = erlmcp_message_size:get_limit(stdio),
    ErrorResponse = erlmcp_message_size:get_max_size_error(MaxSize),

    DecodedError = jsx:decode(ErrorResponse, [return_maps]),
    Error = maps:get(<<"error">>, DecodedError),
    Data = maps:get(<<"data">>, Error),

    %% Should include maxSize
    ?assertEqual(MaxSize, maps:get(<<"maxSize">>, Data)),

    %% Should include unit
    ?assertEqual(<<"bytes">>, maps:get(<<"unit">>, Data)),

    %% Should include human-readable size
    ReadableSize = maps:get(<<"maxSizeReadable">>, Data),
    ?assert(is_binary(ReadableSize)),
    ?assert(byte_size(ReadableSize) > 0),

    %% Should contain appropriate unit (MB for 16MB)
    ?assert(binary:match(ReadableSize, <<"MB">>) =/= nomatch).

%% Test: Validation works with different message encodings
test_validation_with_different_encodings() ->
    %% UTF-8 JSON message
    JsonMessage = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"params\":{\"text\":\"Hello World\"}}">>,
    Result1 = erlmcp_message_size:validate_stdio_size(JsonMessage),
    ?assertEqual(ok, Result1),

    %% Binary data that's just below limit
    Limit = erlmcp_message_size:get_limit(stdio),
    LargeJsonMessage = binary:copy(<<"x">>, Limit - 100),
    Result2 = erlmcp_message_size:validate_stdio_size(LargeJsonMessage),
    ?assertEqual(ok, Result2),

    %% Exact limit should be allowed
    ExactMessage = binary:copy(<<"y">>, Limit),
    Result3 = erlmcp_message_size:validate_stdio_size(ExactMessage),
    ?assertEqual(ok, Result3),

    %% Just over limit should be rejected
    OverMessage = binary:copy(<<"z">>, Limit + 1),
    Result4 = erlmcp_message_size:validate_stdio_size(OverMessage),
    ?assertMatch({error, {message_too_large, _}}, Result4).

%%====================================================================
%% Integration Tests (with Stdio Transport)
%%====================================================================

stdio_integration_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_transport_initialization()),
            ?_test(test_message_size_limits_configured()),
            ?_test(test_size_limit_is_positive_integer())
        ]
    }.

%% Test: Stdio transport can be initialized
test_transport_initialization() ->
    Owner = self(),
    {ok, TransportPid} = erlmcp_transport_stdio:start_link(Owner),

    ?assert(is_pid(TransportPid)),

    %% Cleanup
    erlmcp_transport_stdio:close(TransportPid),

    %% Verify process is stopped
    timer:sleep(100),
    ?assert(not erlang:is_process_alive(TransportPid)).

%% Test: Message size limits are properly configured in sys.config
test_message_size_limits_configured() ->
    Config = erlmcp_message_size:get_size_limit_config(),

    %% stdio should be in config
    ?assert(maps:is_key(stdio, Config)),

    %% Value should be positive integer
    StdioLimit = maps:get(stdio, Config),
    ?assert(is_integer(StdioLimit)),
    ?assert(StdioLimit > 0).

%% Test: Configured limit is within reasonable bounds
test_size_limit_is_positive_integer() ->
    Config = erlmcp_message_size:get_size_limit_config(),

    maps:foreach(fun(Key, Value) ->
        case Key of
            stdio ->
                %% Stdio limit should be between 1KB and 100MB
                ?assert(Value >= ?MCP_MIN_MESSAGE_SIZE_LIMIT),
                ?assert(Value =< ?MCP_MAX_CONFIGURABLE_SIZE_LIMIT),

                %% Verify it's the expected 16MB
                ?assertEqual(16777216, Value);
            _ ->
                ok
        end
    end, Config).

%%====================================================================
%% Boundary and Edge Case Tests
%%====================================================================

boundary_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_exactly_16mb_is_accepted()),
            ?_test(test_16mb_plus_one_is_rejected()),
            ?_test(test_half_limit_is_accepted()),
            ?_test(test_double_limit_is_rejected()),
            ?_test(test_single_character_message()),
            ?_test(test_newline_handling())
        ]
    }.

%% Test: Exactly 16MB message is accepted
test_exactly_16mb_is_accepted() ->
    Limit = erlmcp_message_size:get_limit(stdio),
    Message = binary:copy(<<"a">>, Limit),
    Result = erlmcp_message_size:validate_stdio_size(Message),
    ?assertEqual(ok, Result).

%% Test: 16MB + 1 byte is rejected
test_16mb_plus_one_is_rejected() ->
    Limit = erlmcp_message_size:get_limit(stdio),
    Message = binary:copy(<<"a">>, Limit + 1),
    Result = erlmcp_message_size:validate_stdio_size(Message),
    ?assertMatch({error, {message_too_large, _}}, Result).

%% Test: Half of limit is accepted
test_half_limit_is_accepted() ->
    Limit = erlmcp_message_size:get_limit(stdio),
    HalfLimit = Limit div 2,
    Message = binary:copy(<<"x">>, HalfLimit),
    Result = erlmcp_message_size:validate_stdio_size(Message),
    ?assertEqual(ok, Result).

%% Test: Double the limit is rejected
test_double_limit_is_rejected() ->
    Limit = erlmcp_message_size:get_limit(stdio),
    DoubleLimit = Limit * 2,
    Message = binary:copy(<<"x">>, DoubleLimit),
    Result = erlmcp_message_size:validate_stdio_size(Message),
    ?assertMatch({error, {message_too_large, _}}, Result).

%% Test: Single character message is accepted
test_single_character_message() ->
    SingleChar = <<"x">>,
    Result = erlmcp_message_size:validate_stdio_size(SingleChar),
    ?assertEqual(ok, Result).

%% Test: Newline handling in messages
test_newline_handling() ->
    %% Message with embedded newline
    MessageWithNewline = <<"line1\nline2">>,
    Result = erlmcp_message_size:validate_stdio_size(MessageWithNewline),
    ?assertEqual(ok, Result),

    %% Message with carriage return
    MessageWithCR = <<"line1\rline2">>,
    Result2 = erlmcp_message_size:validate_stdio_size(MessageWithCR),
    ?assertEqual(ok, Result2),

    %% Message with CRLF
    MessageWithCRLF = <<"line1\r\nline2">>,
    Result3 = erlmcp_message_size:validate_stdio_size(MessageWithCRLF),
    ?assertEqual(ok, Result3).

%%====================================================================
%% Error Message Tests
%%====================================================================

error_message_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_error_message_is_valid_json()),
            ?_test(test_error_includes_all_required_fields()),
            ?_test(test_error_message_is_consistent()),
            ?_test(test_error_code_is_valid())
        ]
    }.

%% Test: Error response is valid JSON
test_error_message_is_valid_json() ->
    Limit = erlmcp_message_size:get_limit(stdio),
    ErrorResponse = erlmcp_message_size:get_max_size_error(Limit),

    ?assert(is_binary(ErrorResponse)),

    %% Should be decodable as JSON
    DecodedError = jsx:decode(ErrorResponse, [return_maps]),
    ?assert(is_map(DecodedError)).

%% Test: Error includes all required fields
test_error_includes_all_required_fields() ->
    Limit = erlmcp_message_size:get_limit(stdio),
    ErrorResponse = erlmcp_message_size:get_max_size_error(Limit),

    DecodedError = jsx:decode(ErrorResponse, [return_maps]),

    %% Top-level fields
    ?assert(maps:is_key(<<"jsonrpc">>, DecodedError)),
    ?assert(maps:is_key(<<"error">>, DecodedError)),

    %% Error object fields
    Error = maps:get(<<"error">>, DecodedError),
    ?assert(maps:is_key(<<"code">>, Error)),
    ?assert(maps:is_key(<<"message">>, Error)),
    ?assert(maps:is_key(<<"data">>, Error)),

    %% Data object fields
    Data = maps:get(<<"data">>, Error),
    ?assert(maps:is_key(<<"maxSize">>, Data)),
    ?assert(maps:is_key(<<"unit">>, Data)),
    ?assert(maps:is_key(<<"maxSizeReadable">>, Data)).

%% Test: Error message is consistent across calls
test_error_message_is_consistent() ->
    Limit = erlmcp_message_size:get_limit(stdio),
    ErrorResponse1 = erlmcp_message_size:get_max_size_error(Limit),
    ErrorResponse2 = erlmcp_message_size:get_max_size_error(Limit),

    %% Same limit should produce same error
    ?assertEqual(ErrorResponse1, ErrorResponse2).

%% Test: Error code matches expected constant
test_error_code_is_valid() ->
    Limit = erlmcp_message_size:get_limit(stdio),
    ErrorResponse = erlmcp_message_size:get_max_size_error(Limit),

    DecodedError = jsx:decode(ErrorResponse, [return_maps]),
    Error = maps:get(<<"error">>, DecodedError),
    ErrorCode = maps:get(<<"code">>, Error),

    %% Should match the constant
    ?assertEqual(?MCP_ERROR_MESSAGE_TOO_LARGE, ErrorCode),

    %% Should be in the list of valid error codes
    ?assert(lists:member(ErrorCode, ?VALID_ERROR_CODES)).
