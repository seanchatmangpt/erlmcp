%%%-------------------------------------------------------------------
%% @doc Test Suite for Message Size Limits (Gap #45)
%%
%% Tests for message size limit validation across all transport types:
%% - HTTP POST body limit
%% - SSE event size limit
%% - WebSocket message size limit
%% - TCP message size limit
%% - Stdio message size limit
%% - Configuration validation
%% - Error responses
%%
%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_message_size_tests).

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
%% Test Suites
%%====================================================================

message_size_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_default_message_size_limit()),
            ?_test(test_message_under_limit()),
            ?_test(test_message_at_limit()),
            ?_test(test_message_over_limit()),
            ?_test(test_http_body_size_validation()),
            ?_test(test_sse_event_size_validation()),
            ?_test(test_websocket_size_validation()),
            ?_test(test_tcp_size_validation()),
            ?_test(test_stdio_size_validation()),
            ?_test(test_get_size_limit_config()),
            ?_test(test_error_response_format()),
            ?_test(test_http_413_error()),
            ?_test(test_size_formatting()),
            ?_test(test_custom_message_size_limit()),
            ?_test(test_large_message_error_details())
        ]
    }.

%%====================================================================
%% Individual Tests
%%====================================================================

%% Test: Default message size limit constant
test_default_message_size_limit() ->
    ?assertEqual(16777216, ?MCP_DEFAULT_MESSAGE_SIZE_LIMIT),
    ?assertEqual(16777216, ?MCP_DEFAULT_HTTP_BODY_SIZE_LIMIT),
    ?assertEqual(16777216, ?MCP_DEFAULT_SSE_EVENT_SIZE_LIMIT),
    ?assertEqual(16777216, ?MCP_DEFAULT_WS_MESSAGE_SIZE_LIMIT).

%% Test: Message under size limit is accepted
test_message_under_limit() ->
    SmallMessage = <<"test">>,
    Result = erlmcp_message_size:validate_message_size(default, SmallMessage),
    ?assertEqual(ok, Result).

%% Test: Message exactly at size limit is accepted
test_message_at_limit() ->
    Limit = erlmcp_message_size:get_limit(default),
    Message = binary:copy(<<"x">>, Limit),
    Result = erlmcp_message_size:validate_message_size(default, Message),
    ?assertEqual(ok, Result).

%% Test: Message over size limit is rejected
test_message_over_limit() ->
    Limit = erlmcp_message_size:get_limit(default),
    OversizeMessage = binary:copy(<<"x">>, Limit + 1),
    Result = erlmcp_message_size:validate_message_size(default, OversizeMessage),
    ?assertMatch({error, {message_too_large, _}}, Result).

%% Test: HTTP body size validation
test_http_body_size_validation() ->
    SmallBody = <<"small payload">>,
    Result1 = erlmcp_message_size:validate_http_body_size(SmallBody),
    ?assertEqual(ok, Result1),

    %% Test oversized body
    HttpLimit = erlmcp_message_size:get_limit(http),
    OversizeBody = binary:copy(<<"y">>, HttpLimit + 1),
    Result2 = erlmcp_message_size:validate_http_body_size(OversizeBody),
    ?assertMatch({error, {message_too_large, _}}, Result2).

%% Test: SSE event size validation
test_sse_event_size_validation() ->
    SmallEvent = <<"data: test">>,
    Result1 = erlmcp_message_size:validate_sse_event_size(SmallEvent),
    ?assertEqual(ok, Result1),

    %% Test oversized event
    SseLimit = erlmcp_message_size:get_limit(sse),
    OversizeEvent = binary:copy(<<"e">>, SseLimit + 1),
    Result2 = erlmcp_message_size:validate_sse_event_size(OversizeEvent),
    ?assertMatch({error, {message_too_large, _}}, Result2).

%% Test: WebSocket message size validation
test_websocket_size_validation() ->
    SmallMessage = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test">>,
        <<"id">> => 1
    }),
    Result1 = erlmcp_message_size:validate_websocket_size(SmallMessage),
    ?assertEqual(ok, Result1),

    %% Test oversized message
    WsLimit = erlmcp_message_size:get_limit(websocket),
    OversizeMessage = binary:copy(<<"w">>, WsLimit + 1),
    Result2 = erlmcp_message_size:validate_websocket_size(OversizeMessage),
    ?assertMatch({error, {message_too_large, _}}, Result2).

%% Test: TCP message size validation
test_tcp_size_validation() ->
    SmallMessage = <<"tcp test">>,
    Result1 = erlmcp_message_size:validate_tcp_size(SmallMessage),
    ?assertEqual(ok, Result1),

    %% Test oversized message
    TcpLimit = erlmcp_message_size:get_limit(tcp),
    OversizeMessage = binary:copy(<<"t">>, TcpLimit + 1),
    Result2 = erlmcp_message_size:validate_tcp_size(OversizeMessage),
    ?assertMatch({error, {message_too_large, _}}, Result2).

%% Test: Stdio message size validation
test_stdio_size_validation() ->
    SmallMessage = <<"stdio test">>,
    Result1 = erlmcp_message_size:validate_stdio_size(SmallMessage),
    ?assertEqual(ok, Result1),

    %% Test oversized message
    StdioLimit = erlmcp_message_size:get_limit(stdio),
    OversizeMessage = binary:copy(<<"s">>, StdioLimit + 1),
    Result2 = erlmcp_message_size:validate_stdio_size(OversizeMessage),
    ?assertMatch({error, {message_too_large, _}}, Result2).

%% Test: Get size limit configuration
test_get_size_limit_config() ->
    Config = erlmcp_message_size:get_size_limit_config(),

    %% Verify all transport types are configured
    ?assert(maps:is_key(default, Config)),
    ?assert(maps:is_key(http_body, Config)),
    ?assert(maps:is_key(sse_event, Config)),
    ?assert(maps:is_key(websocket, Config)),
    ?assert(maps:is_key(tcp, Config)),
    ?assert(maps:is_key(stdio, Config)),

    %% Verify all values are integers
    maps:foreach(fun(_K, V) ->
        ?assert(is_integer(V) andalso V > 0)
    end, Config).

%% Test: Error response format contains required fields
test_error_response_format() ->
    MaxSize = erlmcp_message_size:get_limit(default),
    ErrorResponse = erlmcp_message_size:get_max_size_error(MaxSize),

    %% Should be a JSON-RPC error response
    ?assert(is_binary(ErrorResponse)),

    %% Decode and verify structure
    DecodedError = jsx:decode(ErrorResponse, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, DecodedError)),
    ?assert(maps:is_key(<<"error">>, DecodedError)),

    Error = maps:get(<<"error">>, DecodedError),
    ?assertEqual(?MCP_ERROR_MESSAGE_TOO_LARGE, maps:get(<<"code">>, Error)),
    ?assertEqual(?MCP_MSG_MESSAGE_TOO_LARGE, maps:get(<<"message">>, Error)),

    %% Verify data field contains size information
    Data = maps:get(<<"data">>, Error),
    ?assertEqual(MaxSize, maps:get(<<"maxSize">>, Data)),
    ?assertEqual(<<"bytes">>, maps:get(<<"unit">>, Data)).

%% Test: HTTP 413 error format
test_http_413_error() ->
    {http, ErrorMsg} = erlmcp_message_size:get_http_413_error(),
    ?assertMatch(<<"Payload Too Large", _/binary>>, ErrorMsg),
    ?assert(is_binary(ErrorMsg)).

%% Test: Size formatting (KB, MB, GB)
test_size_formatting() ->
    %% These tests verify the internal format_size function through error messages
    SmallLimit = 1024,  %% 1 KB
    SmallError = erlmcp_message_size:get_max_size_error(SmallLimit),
    ?assert(is_binary(SmallError)),

    %% Large size should format as MB
    LargeLimit = 1024 * 1024 * 10,  %% 10 MB
    LargeError = erlmcp_message_size:get_max_size_error(LargeLimit),
    ?assert(is_binary(LargeError)).

%% Test: Custom message size limit via validate_message_size/3
test_custom_message_size_limit() ->
    CustomLimit = 1000,
    SmallMessage = <<"test">>,
    OversizeMessage = binary:copy(<<"x">>, CustomLimit + 1),

    %% Small message should pass
    Result1 = erlmcp_message_size:validate_message_size(SmallMessage, CustomLimit, http),
    ?assertEqual(ok, Result1),

    %% Oversized message should fail
    Result2 = erlmcp_message_size:validate_message_size(OversizeMessage, CustomLimit, http),
    ?assertMatch({error, {message_too_large, _}}, Result2).

%% Test: Large message error details include readable size
test_large_message_error_details() ->
    MaxSize = erlmcp_message_size:get_limit(default),
    ErrorResponse = erlmcp_message_size:get_max_size_error(MaxSize),

    DecodedError = jsx:decode(ErrorResponse, [return_maps]),
    Error = maps:get(<<"error">>, DecodedError),
    Data = maps:get(<<"data">>, Error),

    %% Should include human-readable size format
    ReadableSize = maps:get(<<"maxSizeReadable">>, Data),
    ?assert(is_binary(ReadableSize)),
    ?assert(byte_size(ReadableSize) > 0),

    %% Should contain unit indicator
    ?assert(binary:match(ReadableSize, [<<"B">>, <<"KB">>, <<"MB">>, <<"GB">>]) =/= nomatch).

%%====================================================================
%% Integration Tests (with JSON-RPC)
%%====================================================================

json_rpc_integration_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_decode_message_under_limit()),
            ?_test(test_decode_message_over_limit()),
            ?_test(test_error_code_in_valid_codes()),
            ?_test(test_error_message_too_large_function())
        ]
    }.

%% Test: Decode message under limit succeeds
test_decode_message_under_limit() ->
    Request = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test">>,
        <<"id">> => 1
    }),

    Result = erlmcp_json_rpc:decode_message(Request, default),
    ?assertMatch({ok, _}, Result).

%% Test: Decode message over limit fails with proper error
test_decode_message_over_limit() ->
    Limit = erlmcp_message_size:get_limit(default),
    OversizeMessage = binary:copy(<<"x">>, Limit + 1),

    Result = erlmcp_json_rpc:decode_message(OversizeMessage, default),
    ?assertMatch({error, {message_too_large, _}}, Result).

%% Test: Message too large error code is in valid error codes list
test_error_code_in_valid_codes() ->
    ?assert(lists:member(?MCP_ERROR_MESSAGE_TOO_LARGE, ?VALID_ERROR_CODES)).

%% Test: error_message_too_large/2 creates proper response
test_error_message_too_large_function() ->
    MaxSize = 16777216,
    Response = erlmcp_json_rpc:error_message_too_large(1, MaxSize),

    ?assert(is_binary(Response)),
    Decoded = jsx:decode(Response, [return_maps]),

    Error = maps:get(<<"error">>, Decoded),
    ?assertEqual(?MCP_ERROR_MESSAGE_TOO_LARGE, maps:get(<<"code">>, Error)),
    ?assertEqual(MaxSize, maps:get(<<"maxSize">>, maps:get(<<"data">>, Error))).

%%====================================================================
%% Configuration Tests
%%====================================================================

configuration_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_get_limit_for_each_transport()),
            ?_test(test_limit_values_are_positive()),
            ?_test(test_limit_values_within_reasonable_bounds())
        ]
    }.

%% Test: Get limit works for all transport types
test_get_limit_for_each_transport() ->
    Transports = [http, sse, websocket, tcp, stdio, default],
    lists:foreach(fun(Transport) ->
        Limit = erlmcp_message_size:get_limit(Transport),
        ?assert(is_integer(Limit)),
        ?assert(Limit > 0)
    end, Transports).

%% Test: All limit values are positive
test_limit_values_are_positive() ->
    Config = erlmcp_message_size:get_size_limit_config(),
    maps:foreach(fun(_K, V) ->
        ?assert(is_integer(V) andalso V > 0)
    end, Config).

%% Test: Limits are within reasonable bounds
test_limit_values_within_reasonable_bounds() ->
    Config = erlmcp_message_size:get_size_limit_config(),
    maps:foreach(fun(_K, V) ->
        %% Should be between 1KB and 100MB by default
        ?assert(V >= ?MCP_MIN_MESSAGE_SIZE_LIMIT),
        ?assert(V =< ?MCP_MAX_CONFIGURABLE_SIZE_LIMIT)
    end, Config).

%%====================================================================
%% Edge Case Tests
%%====================================================================

edge_case_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_zero_byte_message()),
            ?_test(test_single_byte_message()),
            ?_test(test_exactly_limit_boundary()),
            ?_test(test_one_byte_over_limit()),
            ?_test(test_very_large_message()),
            ?_test(test_empty_configuration())
        ]
    }.

%% Test: Zero-byte message is valid
test_zero_byte_message() ->
    EmptyMessage = <<"">>,
    Result = erlmcp_message_size:validate_message_size(default, EmptyMessage),
    ?assertEqual(ok, Result).

%% Test: Single byte message is valid
test_single_byte_message() ->
    SingleByte = <<"x">>,
    Result = erlmcp_message_size:validate_message_size(default, SingleByte),
    ?assertEqual(ok, Result).

%% Test: Message exactly at limit boundary
test_exactly_limit_boundary() ->
    Limit = 1000,
    Message = binary:copy(<<"a">>, Limit),
    Result = erlmcp_message_size:validate_message_size(Message, Limit, http),
    ?assertEqual(ok, Result).

%% Test: One byte over limit is rejected
test_one_byte_over_limit() ->
    Limit = 1000,
    Message = binary:copy(<<"a">>, Limit + 1),
    Result = erlmcp_message_size:validate_message_size(Message, Limit, http),
    ?assertMatch({error, {message_too_large, _}}, Result).

%% Test: Very large message detection
test_very_large_message() ->
    %% Create a large message (but not huge to avoid memory issues)
    VeryLargeMessage = binary:copy(<<"x">>, 20 * 1024 * 1024),  %% 20 MB
    Result = erlmcp_message_size:validate_message_size(default, VeryLargeMessage),
    ?assertMatch({error, {message_too_large, _}}, Result).

%% Test: Missing configuration falls back to defaults
test_empty_configuration() ->
    Config = erlmcp_message_size:get_size_limit_config(),

    %% Should have all required keys even if config is empty
    RequiredKeys = [default, http_body, sse_event, websocket, tcp, stdio],
    lists:foreach(fun(Key) ->
        ?assert(maps:is_key(Key, Config))
    end, RequiredKeys).
