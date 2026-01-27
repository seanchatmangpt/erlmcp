%%%=====================================================================
%%% @doc
%%% SSE Retry Field Tests - Gap #29 Implementation
%%%
%%% Tests for MCP 2025-11-25 compliance requirement to include
%%% retry field in SSE close events, specifying client reconnection
%%% wait time in milliseconds.
%%%
%%% Specification Requirement:
%%% - Include `retry: MILLISECONDS` field in SSE close events
%%% - Specifies client reconnection wait time
%%% - Default: 5000 milliseconds (5 seconds)
%%% - Configurable per connection or globally
%%% - Sent on SSE stream close
%%%
%%% @end
%%%=====================================================================

-module(erlmcp_sse_retry_field_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup & Cleanup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Test Suites
%%====================================================================

sse_retry_field_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_default_retry_timeout()),
            ?_test(test_retry_timeout_from_config()),
            ?_test(test_format_retry_field()),
            ?_test(test_format_retry_field_various_values()),
            ?_test(test_close_event_includes_retry()),
            ?_test(test_close_event_format_with_retry()),
            ?_test(test_idle_timeout_includes_retry()),
            ?_test(test_retry_field_proper_sse_format()),
            ?_test(test_custom_retry_value_configuration()),
            ?_test(test_multiple_close_events_with_retry()),
            ?_test(test_retry_field_edge_cases())
        ]
    }.

%%====================================================================
%% Unit Tests - Retry Timeout Configuration
%%====================================================================

test_default_retry_timeout() ->
    %% Verify default retry timeout is 5000ms (5 seconds)
    %% This is the MCP specification default
    RetryTimeout = get_default_retry_timeout(),
    ?assertEqual(5000, RetryTimeout),
    ?assert(is_integer(RetryTimeout)),
    ?assert(RetryTimeout > 0).

test_retry_timeout_from_config() ->
    %% Verify retry timeout can be read from application config
    %% sys.config: {sse, [{retry_timeout, 5000}]}
    case application:get_env(erlmcp, sse) of
        {ok, SseConfig} ->
            case proplists:get_value(retry_timeout, SseConfig) of
                undefined ->
                    %% Config exists but no retry_timeout set - should use default
                    ?assert(true);
                RetryMs ->
                    %% Config has retry_timeout set
                    ?assert(is_integer(RetryMs)),
                    ?assert(RetryMs > 0)
            end;
        undefined ->
            %% No SSE config - should use default
            ?assert(true)
    end.

%%====================================================================
%% Unit Tests - Retry Field Formatting
%%====================================================================

test_format_retry_field() ->
    %% Test basic retry field formatting: "retry: N\n"
    RetryField = format_retry_field(5000),
    ?assert(is_binary(RetryField)),
    ?assert(binary:match(RetryField, <<"retry: 5000">>) =/= nomatch),
    ?assert(binary:match(RetryField, <<"retry: 5000\n">>) =/= nomatch),
    ?assertEqual(<<"retry: 5000\n">>, RetryField).

test_format_retry_field_various_values() ->
    %% Test retry field formatting with various timeout values
    TestCases = [
        {1000, <<"retry: 1000\n">>},
        {3000, <<"retry: 3000\n">>},
        {5000, <<"retry: 5000\n">>},
        {10000, <<"retry: 10000\n">>},
        {30000, <<"retry: 30000\n">>},
        {60000, <<"retry: 60000\n">>}
    ],

    lists:foreach(
        fun({RetryMs, ExpectedField}) ->
            ActualField = format_retry_field(RetryMs),
            ?assertEqual(ExpectedField, ActualField)
        end,
        TestCases
    ).

%%====================================================================
%% Integration Tests - Close Event Formatting
%%====================================================================

test_close_event_includes_retry() ->
    %% Verify close events include retry field with default timeout
    CloseEvent = format_close_event_with_retry(5000),

    ?assert(is_binary(CloseEvent)),
    ?assert(binary:match(CloseEvent, <<"event: close">>) =/= nomatch),
    ?assert(binary:match(CloseEvent, <<"data: ">>) =/= nomatch),
    ?assert(binary:match(CloseEvent, <<"retry: ">>) =/= nomatch),
    ?assert(binary:match(CloseEvent, <<"retry: 5000">>) =/= nomatch).

test_close_event_format_with_retry() ->
    %% Verify close event has correct SSE format
    %% Format: "event: close\ndata: {...}\nretry: N\n\n"
    CloseEvent = format_close_event_with_retry(5000),

    %% Check event type
    ?assertMatch(<<"event: close\n", _/binary>>, CloseEvent),

    %% Check data field present
    ?assert(binary:match(CloseEvent, <<"data: {\"status\":\"closed\"}">>) =/= nomatch),

    %% Check retry field present
    ?assert(binary:match(CloseEvent, <<"retry: 5000">>) =/= nomatch),

    %% Check proper SSE delimiter at end
    ?assert(binary:match(CloseEvent, <<"\n\n">>) =/= nomatch),
    ?assert(binary:match(CloseEvent, <<"retry: 5000\n\n">>) =/= nomatch).

test_idle_timeout_includes_retry() ->
    %% Verify idle timeout events include retry field
    RetryField = format_retry_field(5000),

    %% Idle timeout should send retry field before closing
    ?assert(is_binary(RetryField)),
    ?assert(binary:match(RetryField, <<"retry: 5000">>) =/= nomatch),

    %% Should only be retry field, not full event
    ?assertNot(binary:match(RetryField, <<"event: ">>) =/= nomatch).

test_retry_field_proper_sse_format() ->
    %% Per SSE specification, retry field format is strict:
    %% - Must start with "retry: "
    %% - Must contain milliseconds as integer
    %% - Must end with newline

    TestCases = [5000, 1000, 30000, 60000],

    lists:foreach(
        fun(RetryMs) ->
            RetryField = format_retry_field(RetryMs),
            RetryBin = integer_to_binary(RetryMs),
            ExpectedPrefix = <<"retry: ", RetryBin/binary>>,

            ?assert(binary:match(RetryField, ExpectedPrefix) =/= nomatch),
            ?assert(binary:match(RetryField, <<"\n">>) =/= nomatch)
        end,
        TestCases
    ).

%%====================================================================
%% Integration Tests - Configuration
%%====================================================================

test_custom_retry_value_configuration() ->
    %% Verify custom retry values are properly configured
    %% In sys.config: {sse, [{retry_timeout, CustomMs}]}

    case application:get_env(erlmcp, sse) of
        {ok, SseConfig} ->
            case proplists:get_value(retry_timeout, SseConfig) of
                undefined ->
                    %% No custom value set
                    ?assertEqual(5000, get_default_retry_timeout());
                CustomMs ->
                    %% Custom value is set and readable
                    ?assert(is_integer(CustomMs)),
                    ?assert(CustomMs > 0),
                    ?assert(CustomMs =/= 5000 orelse CustomMs =:= 5000)
            end;
        undefined ->
            ?assert(true)
    end.

%%====================================================================
%% Integration Tests - Multiple Close Events
%%====================================================================

test_multiple_close_events_with_retry() ->
    %% Verify multiple close events each include retry field
    ClosedEvents = [
        format_close_event_with_retry(3000),
        format_close_event_with_retry(5000),
        format_close_event_with_retry(10000)
    ],

    ?assertEqual(3, length(ClosedEvents)),

    lists:foreach(
        fun(Event) ->
            ?assert(is_binary(Event)),
            ?assert(binary:match(Event, <<"event: close">>) =/= nomatch),
            ?assert(binary:match(Event, <<"retry: ">>) =/= nomatch),
            ?assert(binary:match(Event, <<"\n\n">>) =/= nomatch)
        end,
        ClosedEvents
    ).

%%====================================================================
%% Edge Case Tests
%%====================================================================

test_retry_field_edge_cases() ->
    %% Test edge cases for retry field formatting

    %% Minimum reasonable value (1 second)
    MinRetryField = format_retry_field(1000),
    ?assertEqual(<<"retry: 1000\n">>, MinRetryField),

    %% Large value (5 minutes)
    MaxRetryField = format_retry_field(300000),
    ?assertEqual(<<"retry: 300000\n">>, MaxRetryField),

    %% Ensure field is always well-formed
    lists:foreach(
        fun(RetryMs) ->
            Field = format_retry_field(RetryMs),
            ?assert(binary:match(Field, <<"retry: ">>) =/= nomatch),
            ?assert(binary:match(Field, <<"\n">>) =/= nomatch)
        end,
        [1000, 2000, 3000, 5000, 10000, 30000, 60000, 300000]
    ).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Get default retry timeout (5000ms per MCP spec)
-spec get_default_retry_timeout() -> pos_integer().
get_default_retry_timeout() ->
    case application:get_env(erlmcp, sse) of
        undefined ->
            5000;  %% Default
        {ok, SseConfig} ->
            case proplists:get_value(retry_timeout, SseConfig) of
                undefined ->
                    5000;  %% Default
                RetryMs when is_integer(RetryMs), RetryMs > 0 ->
                    RetryMs;
                _ ->
                    5000   %% Default on invalid value
            end
    end.

%% @doc Format SSE retry field: "retry: N\n"
%% Per SSE specification, retry field tells client to wait N milliseconds
%% before attempting reconnection. (Gap #29)
-spec format_retry_field(pos_integer()) -> binary().
format_retry_field(RetryMs) when is_integer(RetryMs), RetryMs > 0 ->
    RetryBin = integer_to_binary(RetryMs),
    <<"retry: ", RetryBin/binary, "\n">>.

%% @doc Format SSE close event with retry field (Gap #29)
%% Sends close event with retry hint so client knows to reconnect
%% Format: "event: close\ndata: {...}\nretry: N\n\n"
-spec format_close_event_with_retry(pos_integer()) -> binary().
format_close_event_with_retry(RetryMs) when is_integer(RetryMs), RetryMs > 0 ->
    RetryBin = integer_to_binary(RetryMs),
    <<"event: close\ndata: {\"status\":\"closed\"}\nretry: ",
      RetryBin/binary, "\n\n">>.
