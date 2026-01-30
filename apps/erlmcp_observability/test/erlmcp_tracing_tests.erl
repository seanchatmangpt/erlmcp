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

%% Test span creation and management with state verification
span_lifecycle_test() ->
    %% Start a span
    SpanCtx = erlmcp_tracing:start_span(<<"test.span">>),

    %% Verify span context structure (state-based verification)
    ?assertMatch(#{trace_id := _, span_id := _, attributes := _, start_time := _}, SpanCtx),

    %% Set attributes
    ok = erlmcp_tracing:set_attributes(SpanCtx, #{<<"test.attr">> => <<"test_value">>}),

    %% Set status
    ok = erlmcp_tracing:set_status(SpanCtx, ok),

    %% End span
    ?assertEqual(ok, erlmcp_tracing:end_span(SpanCtx)).

%% Test server span creation with proper attributes
server_span_test() ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.test">>, test_server),

    %% Verify span context includes server attributes
    ?assertMatch(#{trace_id := _, span_id := _, attributes := Attrs}, SpanCtx),
    Attrs = maps:get(attributes, SpanCtx, #{}),
    ?assertEqual(<<"server">>, maps:get(<<"span.kind">>, Attrs)),
    ?assertEqual(<<"test_server">>, maps:get(<<"server.id">>, Attrs)),

    ?assertEqual(ok, erlmcp_tracing:end_span(SpanCtx)).

%% Test transport span creation with proper attributes
transport_span_test() ->
    TransportPid = self(),
    SpanCtx = erlmcp_tracing:start_transport_span(<<"transport.test">>, TransportPid, stdio),

    %% Verify span context structure
    ?assertMatch(#{trace_id := _, span_id := _, attributes := Attrs}, SpanCtx),
    Attrs = maps:get(attributes, SpanCtx, #{}),
    ?assertEqual(<<"client">>, maps:get(<<"span.kind">>, Attrs)),
    ?assertEqual(<<"stdio">>, maps:get(<<"transport.type">>, Attrs)),

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

    %% Add first attribute
    ok = erlmcp_tracing:add_span_attribute(SpanCtx, <<"single.attr">>, <<"value">>),

    %% Add second attribute with atom key (should be normalized)
    ok = erlmcp_tracing:add_span_attribute(SpanCtx, atom_attr, atom_value),

    %% Verify operations don't crash
    ?assertEqual(ok, erlmcp_tracing:end_span(SpanCtx)).

%%====================================================================
%% Edge Case Tests
%%====================================================================

%% Test span with empty attributes
span_with_empty_attributes_test() ->
    SpanCtx = erlmcp_tracing:start_span(<<"empty.test">>, #{}),

    %% Verify span was created (it has base attributes added by erlmcp_otel)
    ?assertMatch(#{trace_id := _, span_id := _, attributes := _}, SpanCtx),

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

%% Test normalize_attr_key with various types
normalize_attr_key_comprehensive_test_() ->
    [
        ?_assertEqual(<<"key">>, erlmcp_tracing:normalize_attr_key(<<"key">>)),
        ?_assertEqual(<<"key">>, erlmcp_tracing:normalize_attr_key(key)),
        ?_assertEqual(<<"key">>, erlmcp_tracing:normalize_attr_key("key")),
        ?_assertEqual(<<"123">>, erlmcp_tracing:normalize_attr_key(123))
        %% Note: floats are not supported by normalize_attr_key (function_clause)
    ].

%% Test normalize_attr_value with complex types
normalize_attr_value_complex_test_() ->
    [
        ?_test(begin
            Pid = self(),
            Result = erlmcp_tracing:normalize_attr_value(Pid),
            ?assert(is_binary(Result))
        end),
        ?_test(begin
            Ref = make_ref(),
            NormalizedRef = erlmcp_tracing:normalize_attr_value(Ref),
            ?assert(is_binary(NormalizedRef))
        end),
        ?_test(begin
            Result = erlmcp_tracing:normalize_attr_value(#{a => 1}),
            ?assert(is_binary(Result))
        end),
        ?_test(begin
            %% Integer lists are converted to binary directly
            Result = erlmcp_tracing:normalize_attr_value([1, 2, 3]),
            ?assertEqual(<<1, 2, 3>>, Result)
        end)
    ].

%% Test span with initial attributes
span_with_initial_attributes_test() ->
    InitialAttrs = #{
        <<"initial.attr1">> => <<"value1">>,
        <<"initial.attr2">> => 42
    },
    SpanCtx = erlmcp_tracing:start_span(<<"initial.test">>, InitialAttrs),

    %% Verify initial attributes are present in attributes
    Attrs = maps:get(attributes, SpanCtx, #{}),
    ?assertEqual(<<"value1">>, maps:get(<<"initial.attr1">>, Attrs)),
    ?assertEqual(42, maps:get(<<"initial.attr2">>, Attrs)),

    %% Add more attributes
    ok = erlmcp_tracing:add_span_attribute(SpanCtx, <<"added.attr">>, <<"added_value">>),

    %% Verify operations don't crash
    ?assertEqual(ok, erlmcp_tracing:end_span(SpanCtx)).

%% Test error recording with empty details
record_error_with_empty_details_test() ->
    SpanCtx = erlmcp_tracing:start_span(<<"error.empty">>),

    ok = erlmcp_tracing:record_error_details(SpanCtx, empty_error, #{}),

    %% Verify operations don't crash
    ?assertEqual(ok, erlmcp_tracing:end_span(SpanCtx)).

%% Test exception recording without stacktrace
record_exception_no_stacktrace_test() ->
    SpanCtx = erlmcp_tracing:start_span(<<"exception.no_stack">>),

    ok = erlmcp_tracing:record_exception(SpanCtx, throw, no_reason),

    %% Verify operations don't crash
    ?assertEqual(ok, erlmcp_tracing:end_span(SpanCtx)).
