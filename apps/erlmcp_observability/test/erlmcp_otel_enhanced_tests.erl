%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive Tests for Enhanced OpenTelemetry Tracing
%%%
%%% Tests cover:
%%% - End-to-end trace validation
%%% - Baggage propagation
%%% - Multi-node tracing
%%% - Middleware integration
%%% - Exporter functionality
%%% - Sampling strategies
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otel_enhanced_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Include trace_ctx record definition
-record(trace_ctx, {
    trace_id :: binary(),
    span_id :: binary(),
    parent_span_id :: binary() | undefined,
    baggage :: #{atom() => term()}
}).

%% =============================================================================
%% Test Fixtures
%% =============================================================================

otel_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            {"Trace context creation and restoration", fun test_trace_context/0},
            {"Baggage propagation", fun test_baggage_propagation/0},
            {"RPC span injection", fun test_rpc_span_injection/0},
            {"Span linking", fun test_span_linking/0},
            {"Sampling strategies", fun test_sampling_strategies/0},
            {"Middleware transport tracing", fun test_middleware_transport/0},
            {"Middleware handler tracing", fun test_middleware_handler/0},
            {"Request/response annotation", fun test_request_response_annotation/0},
            {"Error recording with stacktrace", fun test_error_recording/0},
            {"Jaeger exporter initialization", fun test_jaeger_exporter/0},
            {"Datadog exporter initialization", fun test_datadog_exporter/0},
            {"Honeycomb exporter initialization", fun test_honeycomb_exporter/0},
            {"End-to-end trace validation", fun test_e2e_trace/0},
            {"Multi-process trace propagation", fun test_multiprocess_trace/0}
        ]
    }.

setup() ->
    %% Initialize OTEL with test configuration
    Config = #{
        service_name => <<"erlmcp-test">>,
        service_version => <<"2.0.0">>,
        exporters => [console],
        sampling => always_on,
        sampling_rate => 1.0
    },
    erlmcp_otel:init(Config),
    ok.

cleanup(_) ->
    erlmcp_otel:shutdown(),
    ok.

%% =============================================================================
%% Trace Context Tests
%% =============================================================================

test_trace_context() ->
    %% Create a span
    SpanCtx = erlmcp_otel:start_span(<<"test.operation">>, #{
        <<"test.attribute">> => <<"value">>
    }),

    %% Verify span context structure
    ?assertMatch(#{
        trace_id := _,
        span_id := _,
        baggage := _
    }, SpanCtx),

    %% Create trace context record
    TraceCtx = erlmcp_otel:create_trace_ctx(SpanCtx),

    %% Verify record structure
    ?assertMatch(#trace_ctx{
        trace_id = _,
        span_id = _
    }, TraceCtx),

    %% Restore from trace context
    RestoredCtx = erlmcp_otel:restore_trace_ctx(TraceCtx),

    %% Verify trace ID matches
    #{trace_id := OriginalTraceId} = SpanCtx,
    #{trace_id := RestoredTraceId} = RestoredCtx,
    ?assertEqual(OriginalTraceId, RestoredTraceId),

    %% Clean up
    erlmcp_otel:end_span(SpanCtx),
    ok.

%% =============================================================================
%% Baggage Propagation Tests
%% =============================================================================

test_baggage_propagation() ->
    %% Set baggage
    ok = erlmcp_otel:propagate_baggage(user_id, <<"user-123">>),
    ok = erlmcp_otel:propagate_baggage(tenant, <<"tenant-456">>),
    ok = erlmcp_otel:propagate_baggage(request_id, <<"req-789">>),

    %% Retrieve baggage
    ?assertEqual(<<"user-123">>, erlmcp_otel:get_baggage(<<"user_id">>)),
    ?assertEqual(<<"tenant-456">>, erlmcp_otel:get_baggage(<<"tenant">>)),
    ?assertEqual(<<"req-789">>, erlmcp_otel:get_baggage(<<"request_id">>)),

    %% Get all baggage
    AllBaggage = erlmcp_otel:get_all_baggage(),
    ?assert(map_size(AllBaggage) >= 3),
    ?assert(maps:is_key(<<"user_id">>, AllBaggage)),

    %% Baggage should propagate to child spans
    ParentSpan = erlmcp_otel:start_span(<<"parent">>, #{}),
    ChildSpan = erlmcp_otel:start_span(<<"child">>, #{}, ParentSpan),

    #{baggage := ChildBaggage} = ChildSpan,
    ?assert(maps:is_key(<<"user_id">>, ChildBaggage)),

    erlmcp_otel:end_span(ChildSpan),
    erlmcp_otel:end_span(ParentSpan),
    ok.

%% =============================================================================
%% RPC Span Injection Tests
%% =============================================================================

test_rpc_span_injection() ->
    %% Inject RPC span
    Method = <<"tools/call">>,
    RequestId = <<"req-001">>,
    Params = #{<<"tool_name">> => <<"calculator">>, <<"args">> => #{<<"x">> => 5}},

    SpanCtx = erlmcp_otel:inject_rpc_span(Method, RequestId, Params),

    %% Verify span attributes
    #{attributes := Attributes} = SpanCtx,
    ?assertEqual(Method, maps:get(<<"rpc.method">>, Attributes)),
    ?assertEqual(RequestId, maps:get(<<"rpc.request_id">>, Attributes)),
    ?assertEqual(<<"jsonrpc">>, maps:get(<<"rpc.system">>, Attributes)),
    ?assertEqual(<<"client">>, maps:get(<<"span.kind">>, Attributes)),

    %% Verify events were added
    #{events := Events} = SpanCtx,
    ?assert(length(Events) >= 1),

    %% Find client.request_sent event
    SentEvent = lists:filter(fun(#{name := Name}) ->
        Name =:= <<"client.request_sent">>
    end, Events),
    ?assert(length(SentEvent) > 0),

    erlmcp_otel:end_span(SpanCtx),
    ok.

%% =============================================================================
%% Span Linking Tests
%% =============================================================================

test_span_linking() ->
    %% Create two separate spans
    Span1 = erlmcp_otel:start_span(<<"operation.1">>, #{}),
    Span2 = erlmcp_otel:start_span(<<"operation.2">>, #{}),

    %% Link Span2 to Span1
    ok = erlmcp_otel:link_span(Span2, Span1),

    %% Verify link attributes were added
    #{attributes := Span2Attrs} = Span2,
    ?assert(maps:is_key(<<"link.trace_id">>, Span2Attrs)),
    ?assert(maps:is_key(<<"link.span_id">>, Span2Attrs)),

    %% Verify link event was added
    #{events := Span2Events} = Span2,
    LinkEvents = lists:filter(fun(#{name := Name}) ->
        Name =:= <<"span.linked">>
    end, Span2Events),
    ?assert(length(LinkEvents) > 0),

    erlmcp_otel:end_span(Span1),
    erlmcp_otel:end_span(Span2),
    ok.

%% =============================================================================
%% Sampling Strategy Tests
%% =============================================================================

test_sampling_strategies() ->
    %% Test always_on
    ?assert(erlmcp_otel:sample_decision(always_on, 0.0)),
    ?assert(erlmcp_otel:sample_decision(always_on, 1.0)),

    %% Test always_off
    ?assertNot(erlmcp_otel:sample_decision(always_off, 0.0)),
    ?assertNot(erlmcp_otel:sample_decision(always_off, 1.0)),

    %% Test trace_id_ratio (probabilistic)
    %% Should sample approximately 10% of the time
    SampleResults = [erlmcp_otel:sample_decision(trace_id_ratio, 0.1) || _ <- lists:seq(1, 100)],
    SampledCount = length([R || R <- SampleResults, R =:= true]),
    %% Allow some variance (5-15%)
    ?assert(SampledCount >= 5 andalso SampledCount =< 15),

    %% Test tail sampling
    SpanCtx = #{
        start_time => erlang:system_time(nanosecond) - 200000000,  % 200ms ago
        status => ok,
        attributes => #{}
    },
    %% High latency should be sampled
    ?assert(erlmcp_otel:tail_sample_decision(SpanCtx)),

    %% Test error sampling
    ErrorSpan = SpanCtx#{status => error},
    ?assert(erlmcp_otel:tail_sample_decision(ErrorSpan)),

    ok.

%% =============================================================================
%% Middleware Tests
%% =============================================================================

test_middleware_transport() ->
    %% Test transport tracing
    Result = erlmcp_otel_middleware:trace_transport(
        tcp,
        <<"send">>,
        fun() -> {ok, sent} end,
        #{socket => test_socket, data_size => 1024}
    ),

    ?assertEqual({ok, sent}, Result),
    ok.

test_middleware_handler() ->
    %% Test handler tracing
    Method = <<"tools/call">>,
    RequestId = <<"req-handler-001">>,

    Result = erlmcp_otel_middleware:trace_handler(
        Method,
        RequestId,
        fun() -> {ok, <<"result">>} end
    ),

    ?assertEqual({ok, <<"result">>}, Result),
    ok.

test_request_response_annotation() ->
    %% Start a span
    SpanCtx = erlmcp_otel:start_span(<<"test.annotate">>, #{}),

    %% Annotate request
    Request = #{
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{<<"tool_name">> => <<"test">>}
    },
    ok = erlmcp_otel_middleware:annotate_request(Request, <<"req-001">>),

    %% Annotate response
    Response = #{
        <<"result">> => #{<<"status">> => <<"ok">>}
    },
    ok = erlmcp_otel_middleware:annotate_response(Response, <<"req-001">>),

    erlmcp_otel:end_span(SpanCtx),
    ok.

%% =============================================================================
%% Error Recording Tests
%% =============================================================================

test_error_recording() ->
    %% Create span and record error
    SpanCtx = erlmcp_otel:start_span(<<"operation.with.error">>, #{}),

    try
        error(test_error)
    catch
        Class:Reason:Stacktrace ->
            ok = erlmcp_otel:record_error(SpanCtx, {Class, Reason, Stacktrace})
    end,

    %% Verify error was recorded
    #{attributes := Attributes, status := Status} = SpanCtx,
    ?assertEqual(error, Status),
    ?assertEqual(true, maps:get(<<"error">>, Attributes)),
    ?assert(maps:is_key(<<"error.type">>, Attributes)),
    ?assert(maps:is_key(<<"error.message">>, Attributes)),

    erlmcp_otel:end_span(SpanCtx),
    ok.

%% =============================================================================
%% Exporter Tests
%% =============================================================================

test_jaeger_exporter() ->
    Config = #{
        endpoint => <<"http://localhost:4318/v1/traces">>,
        protocol => http_protobuf,
        batch_timeout => 5000,
        max_queue_size => 2048,
        headers => [],
        service_name => <<"erlmcp-test">>
    },

    {ok, State} = erlmcp_otel_jaeger:init(Config),
    ?assertMatch(#{config := _, queue := _, timer := _}, State),

    erlmcp_otel_jaeger:shutdown(State),
    ok.

test_datadog_exporter() ->
    Config = #{
        endpoint => <<"http://localhost:4318/v1/traces">>,
        env => <<"test">>,
        service => <<"erlmcp-test">>,
        version => <<"2.0.0">>,
        tags => #{},
        batch_timeout => 5000,
        max_queue_size => 2048
    },

    {ok, State} = erlmcp_otel_datadog:init(Config),
    ?assertMatch(#{config := _, queue := _, timer := _}, State),

    erlmcp_otel_datadog:shutdown(State),
    ok.

test_honeycomb_exporter() ->
    Config = #{
        endpoint => <<"https://api.honeycomb.io">>,
        api_key => <<"test-api-key">>,
        dataset => <<"erlmcp-test">>,
        sample_rate => 10,
        batch_timeout => 5000,
        max_queue_size => 2048
    },

    {ok, State} = erlmcp_otel_honeycomb:init(Config),
    ?assertMatch(#{config := _, queue := _, timer := _}, State),

    erlmcp_otel_honeycomb:shutdown(State),
    ok.

%% =============================================================================
%% End-to-End Tests
%% =============================================================================

test_e2e_trace() ->
    %% Simulate full RPC flow with tracing
    Method = <<"tools/call">>,
    RequestId = <<"req-e2e-001">>,
    Params = #{<<"tool_name">> => <<"calculator">>, <<"operation">> => <<"add">>},

    %% Client side: wrap RPC call
    {ok, #{span_context := ClientSpan, headers := Headers}} =
        erlmcp_otel_middleware:wrap_rpc_call(Method, RequestId, Params),

    %% Verify headers contain trace context
    ?assert(maps:is_key(<<"traceparent">>, Headers)),

    %% Server side: restore context and handle
    ServerCtx = erlmcp_otel:restore_context(Headers),
    ?assertMatch(#{trace_id := _}, ServerCtx),

    %% Execute handler with tracing
    Result = erlmcp_otel_middleware:trace_handler(
        Method,
        RequestId,
        fun() -> {ok, 42} end
    ),

    ?assertEqual({ok, 42}, Result),

    %% Client side: wrap response
    ok = erlmcp_otel_middleware:wrap_rpc_response(
        #{span_context => ClientSpan},
        Result,
        RequestId
    ),

    ok.

%% =============================================================================
%% Multi-Process Trace Tests
%% =============================================================================

test_multiprocess_trace() ->
    %% Create parent span
    ParentSpan = erlmcp_otel:start_span(<<"parent.operation">>, #{}),

    %% Set baggage
    ok = erlmcp_otel:propagate_baggage(correlation_id, <<"corr-123">>),

    %% Create trace context for propagation
    TraceCtx = erlmcp_otel:create_trace_ctx(ParentSpan),

    %% Spawn child process with trace context
    Parent = self(),
    _ChildPid = spawn(fun() ->
        %% Restore context in child process
        ChildCtx = erlmcp_otel:restore_trace_ctx(TraceCtx),
        ChildSpan = erlmcp_otel:start_span(<<"child.operation">>, #{}, ChildCtx),

        %% Verify baggage was propagated
        #{baggage := Baggage} = ChildSpan,
        ?assert(maps:is_key(<<"correlation_id">>, Baggage)),

        erlmcp_otel:end_span(ChildSpan),
        Parent ! {child_done, ChildSpan}
    end),

    %% Wait for child to complete
    receive
        {child_done, ChildSpan} ->
            %% Verify child span has same trace ID
            #{trace_id := ParentTraceId} = ParentSpan,
            #{trace_id := ChildTraceId} = ChildSpan,
            ?assertEqual(ParentTraceId, ChildTraceId),
            ok
    after 5000 ->
        ?assert(false)  % Timeout
    end,

    erlmcp_otel:end_span(ParentSpan),
    ok.
