#!/usr/bin/env escript
%%! -pa apps/erlmcp_core/ebin -pa apps/erlmcp_observability/ebin -pa _build/default/lib/*/ebin

main(_) ->
    io:format("Testing Enhanced OpenTelemetry Features~n"),
    io:format("========================================~n~n"),

    %% Initialize OTEL
    erlmcp_otel:init(#{
        service_name => <<"test-service">>,
        sampling => always_on,
        sampling_rate => 1.0
    }),

    Tests = [
        {"RPC span injection", fun test_rpc_injection/0},
        {"Baggage propagation", fun test_baggage/0},
        {"Trace context creation", fun test_trace_context/0},
        {"Middleware transport", fun test_middleware/0},
        {"Span linking", fun test_span_linking/0},
        {"Sampling strategies", fun test_sampling/0},
        {"Exporter init", fun test_exporters/0}
    ],

    Results = run_tests(Tests),
    Passed = length([R || R <- Results, R =:= pass]),
    Total = length(Results),

    io:format("~n========================================~n"),
    io:format("Results: ~p/~p tests passed~n", [Passed, Total]),

    erlmcp_otel:shutdown(),

    case Passed of
        Total -> halt(0);
        _ -> halt(1)
    end.

run_tests(Tests) ->
    run_tests(Tests, 1, []).

run_tests([], _N, Acc) ->
    lists:reverse(Acc);
run_tests([{Name, Fun} | Rest], N, Acc) ->
    io:format("~p. ~s: ", [N, Name]),
    try
        Fun(),
        io:format("PASS~n"),
        run_tests(Rest, N+1, [pass | Acc])
    catch
        _:Error ->
            io:format("FAIL (~p)~n", [Error]),
            run_tests(Rest, N+1, [fail | Acc])
    end.

test_rpc_injection() ->
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"tools/call">>, <<"req-001">>, #{<<"tool">> => <<"test">>}),
    true = maps:is_key(trace_id, SpanCtx),
    #{attributes := Attrs} = SpanCtx,
    <<"tools/call">> = maps:get(<<"rpc.method">>, Attrs),
    erlmcp_otel:end_span(SpanCtx),
    ok.

test_baggage() ->
    ok = erlmcp_otel:propagate_baggage(user_id, <<"user-123">>),
    <<"user-123">> = erlmcp_otel:get_baggage(<<"user_id">>),
    ok.

test_trace_context() ->
    SpanCtx = erlmcp_otel:start_span(<<"test.operation">>, #{}),
    TraceCtx = erlmcp_otel:create_trace_ctx(SpanCtx),
    trace_ctx = element(1, TraceCtx),
    RestoredCtx = erlmcp_otel:restore_trace_ctx(TraceCtx),
    #{trace_id := TID1} = SpanCtx,
    #{trace_id := TID2} = RestoredCtx,
    true = (TID1 =:= TID2),
    erlmcp_otel:end_span(SpanCtx),
    ok.

test_middleware() ->
    {ok, sent} = erlmcp_otel_middleware:trace_transport(tcp, <<"send">>, fun() -> {ok, sent} end, #{}),
    ok.

test_span_linking() ->
    Span1 = erlmcp_otel:start_span(<<"span1">>, #{}),
    Span2 = erlmcp_otel:start_span(<<"span2">>, #{}),
    ok = erlmcp_otel:link_span(Span2, Span1),
    Span2Updated = erlmcp_otel:get_current_context(),
    #{attributes := Attrs} = Span2Updated,
    true = maps:is_key(<<"link.trace_id">>, Attrs),
    erlmcp_otel:end_span(Span1),
    erlmcp_otel:end_span(Span2),
    ok.

test_sampling() ->
    true = erlmcp_otel:sample_decision(always_on, 0.0),
    false = erlmcp_otel:sample_decision(always_off, 1.0),
    ok.

test_exporters() ->
    {ok, _} = erlmcp_otel_jaeger:init(#{
        endpoint => <<"http://localhost:4318">>,
        protocol => http_protobuf,
        batch_timeout => 5000,
        max_queue_size => 2048,
        headers => [],
        service_name => <<"test">>
    }),
    {ok, _} = erlmcp_otel_datadog:init(#{
        endpoint => <<"http://localhost:4318">>,
        env => <<"test">>,
        service => <<"test">>,
        version => <<"1.0">>,
        tags => #{},
        batch_timeout => 5000,
        max_queue_size => 2048
    }),
    {ok, _} = erlmcp_otel_honeycomb:init(#{
        endpoint => <<"https://api.honeycomb.io">>,
        api_key => <<"test-key">>,
        dataset => <<"test">>,
        batch_timeout => 5000,
        max_queue_size => 2048
    }),
    ok.
