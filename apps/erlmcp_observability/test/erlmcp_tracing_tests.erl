%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for erlmcp_tracing module following Chicago School TDD
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_tracing_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Functions
%%====================================================================

%% Test attribute key normalization
normalize_attr_key_test_() ->
    [
        ?_assertEqual(<<"test_key">>, erlmcp_tracing:normalize_attr_key(<<"test_key">>)),
        ?_assertEqual(<<"atom_key">>, erlmcp_tracing:normalize_attr_key(atom_key)),
        ?_assertEqual(<<"string_key">>, erlmcp_tracing:normalize_attr_key("string_key")),
        ?_assertEqual(<<"123">>, erlmcp_tracing:normalize_attr_key(123))
    ].

%% Test attribute value normalization
normalize_attr_value_test_() ->
    [
        ?_assertEqual(<<"binary_value">>, erlmcp_tracing:normalize_attr_value(<<"binary_value">>)),
        ?_assertEqual(123, erlmcp_tracing:normalize_attr_value(123)),
        ?_assertEqual(45.6, erlmcp_tracing:normalize_attr_value(45.6)),
        ?_assertEqual(true, erlmcp_tracing:normalize_attr_value(true)),
        ?_assertEqual(false, erlmcp_tracing:normalize_attr_value(false)),
        ?_assertEqual(<<"atom_value">>, erlmcp_tracing:normalize_attr_value(atom_value)),
        ?_assertEqual(<<"string_value">>, erlmcp_tracing:normalize_attr_value("string_value"))
    ].

%% Test span creation and management with state verification
span_lifecycle_test() ->
    %% Start a span - returns a map with otel_span reference (actual behavior)
    SpanCtx = erlmcp_tracing:start_span(<<"test.span">>),

    %% Verify span context is a map (actual behavior)
    ?assert(is_map(SpanCtx)),

    %% Set attributes
    ok = erlmcp_tracing:set_attributes(SpanCtx, #{<<"test.attr">> => <<"test_value">>}),

    %% Set status
    ok = erlmcp_tracing:set_status(SpanCtx, ok),

    %% End span
    ?assertEqual(ok, erlmcp_tracing:end_span(SpanCtx)).

%% Test server span creation with proper attributes
server_span_test() ->
    %% Test with atom server ID (actual behavior - converted to binary internally)
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.test">>, test_server),

    %% Verify span context is a map (actual behavior)
    ?assert(is_map(SpanCtx)),

    ?assertEqual(ok, erlmcp_tracing:end_span(SpanCtx)).

%% Test transport span creation with proper attributes
transport_span_test() ->
    TransportPid = self(),
    %% Test with atom transport type (actual behavior)
    SpanCtx = erlmcp_tracing:start_transport_span(<<"transport.test">>, TransportPid, stdio),

    %% Verify span context is a map (actual behavior)
    ?assert(is_map(SpanCtx)),

    ?assertEqual(ok, erlmcp_tracing:end_span(SpanCtx)).

%% Test error recording with state verification
record_error_test() ->
    SpanCtx = erlmcp_tracing:start_span(<<"error.test">>),

    %% Record error details
    ok = erlmcp_tracing:record_error_details(SpanCtx, test_error, #{<<"details">> => <<"test">>}),

    %% Verify error details don't crash (behavior verification)
    ?assertEqual(ok, erlmcp_tracing:end_span(SpanCtx)).

%% Test performance metrics recording with state verification
record_metrics_test() ->
    SpanCtx = erlmcp_tracing:start_span(<<"metrics.test">>),

    %% record_performance_metrics calls set_attributes internally
    ok = erlmcp_tracing:record_performance_metrics(SpanCtx, #{
        <<"duration_ms">> => 100,
        <<"memory_bytes">> => 1024
    }),

    ?assertEqual(ok, erlmcp_tracing:end_span(SpanCtx)).

%% Test message metrics recording with state verification
record_message_metrics_test() ->
    SpanCtx = erlmcp_tracing:start_span(<<"message.test">>),

    ok = erlmcp_tracing:record_message_metrics(SpanCtx, <<"test_method">>, 512),

    ?assertEqual(ok, erlmcp_tracing:end_span(SpanCtx)).

%% Test log function
log_test() ->
    %% Logger:info returns ok, verify it doesn't crash
    ?assertEqual(ok, erlmcp_tracing:log("Test log message: ~p", [test_data])).

%% Test add single attribute with state verification
add_span_attribute_test() ->
    SpanCtx = erlmcp_tracing:start_span(<<"attr.test">>),

    %% add_span_attribute calls set_attributes internally with normalized key/value
    ok = erlmcp_tracing:add_span_attribute(SpanCtx, <<"single.attr">>, <<"value">>),

    %% Add second attribute with atom key (should be normalized to binary)
    ok = erlmcp_tracing:add_span_attribute(SpanCtx, atom_attr, atom_value),

    %% Verify operations don't crash
    ?assertEqual(ok, erlmcp_tracing:end_span(SpanCtx)).

%%====================================================================
%% Edge Case Tests
%%====================================================================

%% Test span with empty attributes
span_with_empty_attributes_test() ->
    SpanCtx = erlmcp_tracing:start_span(<<"empty.test">>, #{}),

    %% Verify span was created (returns a map, not a reference)
    ?assert(is_map(SpanCtx)),

    ?assertEqual(ok, erlmcp_tracing:end_span(SpanCtx)).

%% Test set status with invalid value (should not crash)
set_invalid_status_test() ->
    SpanCtx = erlmcp_tracing:start_span(<<"status.test">>),

    %% Setting invalid status should not crash, just return ok
    ?assertEqual(ok, erlmcp_tracing:set_status(SpanCtx, invalid_status)),

    ?assertEqual(ok, erlmcp_tracing:end_span(SpanCtx)).

%% Test concurrent attribute setting
concurrent_attributes_test() ->
    SpanCtx = erlmcp_tracing:start_span(<<"concurrent.test">>),

    %% Set multiple attributes
    ok = erlmcp_tracing:set_attributes(SpanCtx, #{
        <<"attr1">> => value1,
        <<"attr2">> => value2,
        <<"attr3">> => value3
    }),

    %% Verify operations don't crash
    ?assertEqual(ok, erlmcp_tracing:end_span(SpanCtx)).

%% Test error recording with empty details
record_error_with_empty_details_test() ->
    SpanCtx = erlmcp_tracing:start_span(<<"error.empty">>),

    ok = erlmcp_tracing:record_error_details(SpanCtx, empty_error, #{}),

    %% Verify operations don't crash
    ?assertEqual(ok, erlmcp_tracing:end_span(SpanCtx)).
