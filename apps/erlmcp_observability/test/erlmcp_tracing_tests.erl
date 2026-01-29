%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for erlmcp_tracing module
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
        ?_assertEqual(<<"test_key">>, erlmcp_tracing:normalize_attr_key(test_key)),
        ?_assertEqual(<<"test_key">>, erlmcp_tracing:normalize_attr_key("test_key")),
        ?_assertEqual(<<"123">>, erlmcp_tracing:normalize_attr_key(123))
    ].

%% Test attribute value normalization
normalize_attr_value_test_() ->
    [
        ?_assertEqual(<<"binary_value">>, erlmcp_tracing:normalize_attr_value(<<"binary_value">>)),
        ?_assertEqual(<<"atom_value">>, erlmcp_tracing:normalize_attr_value(atom_value)),
        ?_assertEqual(<<"string_value">>, erlmcp_tracing:normalize_attr_value("string_value")),
        ?_assertEqual(123, erlmcp_tracing:normalize_attr_value(123)),
        ?_assertEqual(45.6, erlmcp_tracing:normalize_attr_value(45.6)),
        ?_assertEqual(true, erlmcp_tracing:normalize_attr_value(true)),
        ?_assertEqual(false, erlmcp_tracing:normalize_attr_value(false))
    ].

%% Test span creation and management
span_lifecycle_test() ->
    %% Start a span
    SpanCtx = erlmcp_tracing:start_span(<<"test.span">>),
    ?assertNotEqual(undefined, SpanCtx),

    %% Set attributes
    ok = erlmcp_tracing:set_attributes(SpanCtx, #{<<"test.attr">> => <<"test_value">>}),

    %% Set status
    ok = erlmcp_tracing:set_status(SpanCtx, ok),

    %% End span
    ok = erlmcp_tracing:end_span(SpanCtx),

    %% Test passes if no exceptions
    ?assert(true).

%% Test server span creation
server_span_test() ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.test">>, test_server),
    ?assertNotEqual(undefined, SpanCtx),
    ok = erlmcp_tracing:end_span(SpanCtx),
    ?assert(true).

%% Test transport span creation
transport_span_test() ->
    SpanCtx = erlmcp_tracing:start_transport_span(<<"transport.test">>, self(), stdio),
    ?assertNotEqual(undefined, SpanCtx),
    ok = erlmcp_tracing:end_span(SpanCtx),
    ?assert(true).

%% Test error recording
record_error_test() ->
    SpanCtx = erlmcp_tracing:start_span(<<"error.test">>),
    ok = erlmcp_tracing:record_error_details(SpanCtx, test_error, #{<<"details">> => <<"test">>}),
    ok = erlmcp_tracing:record_exception(SpanCtx, error, test_reason, []),
    ok = erlmcp_tracing:end_span(SpanCtx),
    ?assert(true).

%% Test performance metrics recording
record_metrics_test() ->
    SpanCtx = erlmcp_tracing:start_span(<<"metrics.test">>),
    ok = erlmcp_tracing:record_performance_metrics(SpanCtx, #{
        <<"duration_ms">> => 100,
        <<"memory_bytes">> => 1024
    }),
    ok = erlmcp_tracing:end_span(SpanCtx),
    ?assert(true).

%% Test message metrics recording
record_message_metrics_test() ->
    SpanCtx = erlmcp_tracing:start_span(<<"message.test">>),
    ok = erlmcp_tracing:record_message_metrics(SpanCtx, <<"test_method">>, 512),
    ok = erlmcp_tracing:end_span(SpanCtx),
    ?assert(true).

%% Test log function
log_test() ->
    ok = erlmcp_tracing:log("Test log message: ~p", [test_data]),
    ?assert(true).

%% Test add single attribute
add_span_attribute_test() ->
    SpanCtx = erlmcp_tracing:start_span(<<"attr.test">>),
    ok = erlmcp_tracing:add_span_attribute(SpanCtx, <<"single.attr">>, <<"value">>),
    ok = erlmcp_tracing:add_span_attribute(SpanCtx, atom_attr, atom_value),
    ok = erlmcp_tracing:end_span(SpanCtx),
    ?assert(true).
