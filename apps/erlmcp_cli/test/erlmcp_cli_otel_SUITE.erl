%%%-------------------------------------------------------------------
%%% @doc
%%% OTEL Integration Test Suite (Common Test)
%%%
%%% Tests for OpenTelemetry integration in CLI
%%%
%%% Chicago School TDD:
%%% - Real OTEL processes
%%% - Actual telemetry collection
%%% - State-based verification
%%%
%%% Coverage Target: ≥80%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_otel_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Suite Callbacks
%%%====================================================================

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlmcp_cli),
    {ok, _} = application:ensure_all_started(opentelemetry),
    {ok, _} = application:ensure_all_started(opentelemetry_exporter),
    Config.

end_per_suite(Config) ->
    application:stop(opentelemetry_exporter),
    application:stop(opentelemetry),
    application:stop(erlmcp_cli),
    Config.

init_per_testcase(_TestCase, Config) ->
    %% Reset OTEL state before each test
    erlmcp_cli_otel:reset_telemetry(),
    Config.

end_per_testcase(_TestCase, Config) ->
    %% Clean up after each test
    erlmcp_cli_otel:reset_telemetry(),
    Config.

%%%====================================================================
%%% Test Cases
%%%====================================================================

%% @doc Span creation and propagation
span_creation_propagation_test(_Config) ->
    %% Create a span
    SpanName = <<"test.span">>,
    {ok, SpanCtx} = erlmcp_cli_otel:start_span(SpanName,
                                                 #{<<"operation">> => <<"test">>}),

    %% Verify span context
    ?assertNotEqual(undefined, SpanCtx),
    ?assert(is_map(SpanCtx)),
    ?assert(maps:get(<<"trace_id">>, SpanCtx, undefined) =/= undefined),

    %% End span
    ok = erlmcp_cli_otel:end_span(SpanCtx),

    %% Verify span recorded
    {ok, Spans} = erlmcp_cli_otel:get_recorded_spans(),
    ?assert(length(Spans) > 0),

    %% Verify span attributes
    TestSpan = lists:keyfind(<<"name">>, 1, Spans),
    ?assertNotEqual(false, TestSpan),

    ok.

%% @doc Child span creation
child_span_creation_test(_Config) ->
    %% Create parent span
    {ok, ParentCtx} = erlmcp_cli_otel:start_span(<<"parent.span">>, #{}),

    %% Create child span
    {ok, ChildCtx} = erlmcp_cli_otel:start_span(<<"child.span">>,
                                                  #{<<"parent">> => <<"parent">>},
                                                  ParentCtx),

    %% Verify child span has parent trace ID
    ParentTraceId = maps:get(<<"trace_id">>, ParentCtx),
    ChildTraceId = maps:get(<<"trace_id">>, ChildCtx),
    ?assertEqual(ParentTraceId, ChildTraceId),

    %% End both spans
    ok = erlmcp_cli_otel:end_span(ChildCtx),
    ok = erlmcp_cli_otel:end_span(ParentCtx),

    %% Verify both spans recorded
    {ok, Spans} = erlmcp_cli_otel:get_recorded_spans(),
    ?assert(length(Spans) >= 2),

    ok.

%% @doc Span attributes validation
span_attributes_test(_Config) ->
    %% Create span with attributes
    Attributes = #{
        <<"string.attr">> => <<"value">>,
        <<"int.attr">> => 42,
        <<"bool.attr">> => true,
        <<"float.attr">> => 3.14
    },

    {ok, SpanCtx} = erlmcp_cli_otel:start_span(<<"attr.span">>, Attributes),
    ok = erlmcp_cli_otel:end_span(SpanCtx),

    %% Verify attributes recorded
    {ok, Spans} = erlmcp_cli_otel:get_recorded_spans(),
    AttrSpan = lists:keyfind(<<"attr.span">>, 2, Spans),

    ?assertNotEqual(false, AttrSpan),
    {_, Attrs} = lists:keyfind(<<"attributes">>, 1, AttrSpan),

    ?assertEqual(<<"value">>, maps:get(<<"string.attr">>, Attrs)),
    ?assertEqual(42, maps:get(<<"int.attr">>, Attrs)),
    ?assertEqual(true, maps:get(<<"bool.attr">>, Attrs)),
    ?assertEqual(3.14, maps:get(<<"float.attr">>, Attrs)),

    ok.

%% @doc Metric collection and reporting
metric_collection_test(_Config) ->
    %% Record metrics
    CounterName = <<"test.counter">>,
    ok = erlmcp_cli_otel:increment_counter(CounterName, 1, #{}),
    ok = erlmcp_cli_otel:increment_counter(CounterName, 2, #{}),

    %% Record histogram
    HistogramName = <<"test.histogram">>,
    ok = erlmcp_cli_otel:record_histogram(HistogramName, 100.0, #{}),
    ok = erlmcp_cli_otel:record_histogram(HistogramName, 200.0, #{}),

    %% Get metrics
    {ok, Metrics} = erlmcp_cli_otel:get_metrics(),

    %% Verify counter metric
    CounterMetric = lists:keyfind(CounterName, 1, Metrics),
    ?assertNotEqual(false, CounterMetric),
    {_, CounterValue} = lists:keyfind(<<"value">>, 1, CounterMetric),
    ?assertEqual(3, CounterValue),

    %% Verify histogram metric
    HistogramMetric = lists:keyfind(HistogramName, 1, Metrics),
    ?assertNotEqual(false, HistogramMetric),
    {_, HistogramData} = lists:keyfind(<<"data">>, 1, HistogramMetric),
    ?assert(length(HistogramData) >= 2),

    ok.

%% @doc Metric attributes
metric_attributes_test(_Config) ->
    %% Record metric with attributes
    MetricName = <<"attr.metric">>,
    Attributes = #{<<"label">> => <<"test">>, <<"region">> => <<"us-west">>},

    ok = erlmcp_cli_otel:increment_counter(MetricName, 1, Attributes),

    %% Get metrics
    {ok, Metrics} = erlmcp_cli_otel:get_metrics(),

    %% Verify metric with attributes
    Metric = lists:keyfind(MetricName, 1, Metrics),
    ?assertNotEqual(false, Metric),
    {_, MetricAttrs} = lists:keyfind(<<"attributes">>, 1, Metric),

    ?assertEqual(<<"test">>, maps:get(<<"label">>, MetricAttrs)),
    ?assertEqual(<<"us-west">>, maps:get(<<"region">>, MetricAttrs)),

    ok.

%% @doc Trace context management
trace_context_test(_Config) ->
    %% Create trace context
    TraceId = erlmcp_cli_otel:generate_trace_id(),
    SpanId = erlmcp_cli_otel:generate_span_id(),

    Context = #{
        <<"trace_id">> => TraceId,
        <<"span_id">> => SpanId
    },

    %% Set current context
    ok = erlmcp_cli_otel:set_current_context(Context),

    %% Get current context
    {ok, CurrentCtx} = erlmcp_cli_otel:get_current_context(),

    ?assertEqual(TraceId, maps:get(<<"trace_id">>, CurrentCtx)),
    ?assertEqual(SpanId, maps:get(<<"span_id">>, CurrentCtx)),

    ok.

%% @doc Trace context propagation
trace_context_propagation_test(_Config) ->
    %% Create parent span with trace context
    {ok, ParentCtx} = erlmcp_cli_otel:start_span(<<"parent">>, #{}),
    ParentTraceId = maps:get(<<"trace_id">>, ParentCtx),

    %% Propagate context to child (simulate RPC)
    Carrier = erlmcp_cli_otel:inject_context(ParentCtx),

    %% Extract context in child
    {ok, ChildCtx} = erlmcp_cli_otel:extract_context(Carrier),

    ?assertEqual(ParentTraceId, maps:get(<<"trace_id">>, ChildCtx)),

    ok.

%% @doc Span events
span_events_test(_Config) ->
    %% Create span
    {ok, SpanCtx} = erlmcp_cli_otel:start_span(<<"event.span">>, #{}),

    %% Add events
    ok = erlmcp_cli_otel:add_span_event(SpanCtx,
                                          <<"event1">>,
                                          #{<<"key">> => <<"value1">>}),
    ok = erlmcp_cli_otel:add_span_event(SpanCtx,
                                          <<"event2">>,
                                          #{<<"key">> => <<"value2">>}),

    ok = erlmcp_cli_otel:end_span(SpanCtx),

    %% Verify events recorded
    {ok, Spans} = erlmcp_cli_otel:get_recorded_spans(),
    EventSpan = lists:keyfind(<<"event.span">>, 2, Spans),
    {_, Events} = lists:keyfind(<<"events">>, 1, EventSpan),

    ?assert(length(Events) >= 2),

    ok.

%% @doc Span status
span_status_test(_Config) ->
    %% Create span
    {ok, SpanCtx} = erlmcp_cli_otel:start_span(<<"status.span">>, #{}),

    %% Set status
    ok = erlmcp_cli_otel:set_span_status(SpanCtx, ok, <<"Success">>),
    ok = erlmcp_cli_otel:end_span(SpanCtx),

    %% Verify status
    {ok, Spans} = erlmcp_cli_otel:get_recorded_spans(),
    StatusSpan = lists:keyfind(<<"status.span">>, 2, Spans),
    {_, Status} = lists:keyfind(<<"status">>, 1, StatusSpan),

    ?assertEqual(<<"ok">>, Status),

    ok.

%% @doc OTEL export functionality
otel_export_test(_Config) ->
    %% Configure mock exporter
    ok = erlmcp_cli_otel:configure_exporter(#{
        <<"type">> => <<"mock">>,
        <<"endpoint">> => <<"http://localhost:4318">>
    }),

    %% Create and end span
    {ok, SpanCtx} = erlmcp_cli_otel:start_span(<<"export.span">>, #{}),
    ok = erlmcp_cli_otel:end_span(SpanCtx),

    %% Trigger export
    {ok, Exported} = erlmcp_cli_otel:export_spans(),

    ?assert(length(Exported) > 0),

    ok.

%% @doc Performance overhead measurement
performance_overhead_test(_Config) ->
    %% Measure operation without telemetry
    StartNoTel = erlang:monotonic_time(microsecond),
    lists:foreach(fun(_) -> ok end, lists:seq(1, 1000)),
    ElapsedNoTel = erlang:monotonic_time(microsecond) - StartNoTel,

    %% Measure operation with telemetry
    StartWithTel = erlang:monotonic_time(microsecond),
    lists:foreach(fun(I) ->
        SpanName = <<"perf.span.", (integer_to_binary(I))/binary>>,
        {ok, SpanCtx} = erlmcp_cli_otel:start_span(SpanName, #{}),
        ok = erlmcp_cli_otel:end_span(SpanCtx)
    end, lists:seq(1, 1000)),
    ElapsedWithTel = erlang:monotonic_time(microsecond) - StartWithTel,

    %% Calculate overhead
    Overhead = ((ElapsedWithTel - ElapsedNoTel) / ElapsedNoTel) * 100,

    %% Overhead should be reasonable (< 50%)
    ?assert(Overhead < 50.0),

    %% Log overhead
    ct:log("OTEL overhead: ~.2f%", [Overhead]),

    ok.

%% @doc Batch span processing
batch_span_processing_test(_Config) ->
    %% Create many spans
    NumSpans = 100,
    lists:foreach(fun(I) ->
        SpanName = <<"batch.span.", (integer_to_binary(I))/binary>>,
        {ok, SpanCtx} = erlmcp_cli_otel:start_span(SpanName, #{}),
        ok = erlmcp_cli_otel:end_span(SpanCtx)
    end, lists:seq(1, NumSpans)),

    %% Verify all spans recorded
    {ok, Spans} = erlmcp_cli_otel:get_recorded_spans(),
    ?assert(length(Spans) >= NumSpans),

    ok.

%% @doc Metric aggregation
metric_aggregation_test(_Config) ->
    %% Record multiple metric values
    MetricName = <<"agg.metric">>,

    lists:foreach(fun(I) ->
        erlmcp_cli_otel:record_histogram(MetricName, I * 10.0, #{})
    end, lists:seq(1, 100)),

    %% Get aggregated metrics
    {ok, Metrics} = erlmcp_cli_otel:get_metrics(),
    Metric = lists:keyfind(MetricName, 1, Metrics),
    {_, AggData} = lists:keyfind(<<"aggregation">>, 1, Metric),

    %% Verify aggregation
    ?assert(maps:get(<<"count">>, AggData) >= 100),
    ?assert(maps:get(<<"sum">>, AggData) > 0),

    ok.

%% @doc Resource attributes
resource_attributes_test(_Config) ->
    %% Set resource attributes
    ResourceAttrs = #{
        <<"service.name">> => <<"erlmcp-cli">>,
        <<"service.version">> => <<"2.1.0">>,
        <<"deployment.environment">> => <<"test">>
    },

    ok = erlmcp_cli_otel:set_resource_attributes(ResourceAttrs),

    %% Verify resource attributes
    {ok, RetrievedAttrs} = erlmcp_cli_otel:get_resource_attributes(),

    ?assertEqual(<<"erlmcp-cli">>, maps:get(<<"service.name">>, RetrievedAttrs)),
    ?assertEqual(<<"2.1.0">>, maps:get(<<"service.version">>, RetrievedAttrs)),
    ?assertEqual(<<"test">>, maps:get(<<"deployment.environment">>, RetrievedAttrs)),

    ok.

%% @doc Multi-trace correlation
multi_trace_correlation_test(_Config) ->
    %% Create multiple traces with correlation
    CorrelationId = <<"test-correlation-123">>,

    %% Trace 1
    {ok, Span1} = erlmcp_cli_otel:start_span(<<"trace1.span1">>,
                                                #{<<"correlation_id">> => CorrelationId}),
    ok = erlmcp_cli_otel:end_span(Span1),

    %% Trace 2
    {ok, Span2} = erlmcp_cli_otel:start_span(<<"trace2.span1">>,
                                                #{<<"correlation_id">> => CorrelationId}),
    ok = erlmcp_cli_otel:end_span(Span2),

    %% Query traces by correlation ID
    {ok, CorrelatedSpans} = erlmcp_cli_otel:query_spans_by_attribute(
                                <<"correlation_id">>, CorrelationId),

    ?assert(length(CorrelatedSpans) >= 2),

    ok.
