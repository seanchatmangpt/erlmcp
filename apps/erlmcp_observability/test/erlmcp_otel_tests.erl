-module(erlmcp_otel_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests for erlmcp_otel module (OpenTelemetry integration)
%%====================================================================

init_test() ->
    Config = #{
        service_name => <<"test_service">>,
        exporter_endpoint => "http://localhost:4318"
    },
    Result = erlmcp_otel:init(Config),
    ?assertMatch(ok, Result).

start_span_test() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),
    SpanCtx = erlmcp_otel:start_span(<<"test.operation">>, #{}),
    ?assert(is_map(SpanCtx) orelse SpanCtx =:= undefined).

with_span_test() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),
    Fun = fun() ->
        timer:sleep(1),
        ok
    end,
    Result = erlmcp_otel:with_span(<<"test.span">>, #{}, Fun),
    ?assertEqual(ok, Result).

span_lifecycle_test() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),

    SpanCtx = erlmcp_otel:start_span(<<"parent.span">>, #{}),
    ?assert(is_map(SpanCtx) orelse SpanCtx =:= undefined),

    erlmcp_otel:end_span(SpanCtx),
    ok.

nested_spans_test() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),

    OuterFun = fun() ->
        InnerFun = fun() ->
            ok
        end,
        erlmcp_otel:with_span(<<"inner.span">>, #{}, InnerFun)
    end,

    Result = erlmcp_otel:with_span(<<"outer.span">>, #{}, OuterFun),
    ?assertEqual(ok, Result).

error_handling_test() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),

    FunThatFails = fun() ->
        throw(test_error)
    end,

    Result = (catch erlmcp_otel:with_span(<<"error.span">>, #{}, FunThatFails)),
    ?assertEqual(test_error, Result).
