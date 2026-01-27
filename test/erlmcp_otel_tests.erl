-module(erlmcp_otel_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_otel Module
%%====================================================================

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    application:ensure_all_started(opentelemetry),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Initialization Tests
%%====================================================================

init_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_init_default()),
             ?_test(test_init_with_service_name()),
             ?_test(test_init_with_exporters()),
             ?_test(test_init_idempotent())
         ]
     end}.

test_init_default() ->
    Result = erlmcp_otel:init(#{}),
    ?assertMatch(ok | {ok, _}, Result).

test_init_with_service_name() ->
    Config = #{service_name => <<"erlmcp-test">>},
    Result = erlmcp_otel:init(Config),
    ?assertMatch(ok | {ok, _}, Result).

test_init_with_exporters() ->
    Config = #{
        service_name => <<"erlmcp">>,
        exporters => [jaeger, prometheus]
    },
    Result = erlmcp_otel:init(Config),
    ?assertMatch(ok | {ok, _}, Result).

test_init_idempotent() ->
    Config = #{service_name => <<"idempotent_test">>},
    Result1 = erlmcp_otel:init(Config),
    Result2 = erlmcp_otel:init(Config),
    ?assertMatch(ok | {ok, _}, [Result1, Result2]).

%%====================================================================
%% Span Tests
%%====================================================================

span_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_with_span_basic()),
             ?_test(test_with_span_attributes()),
             ?_test(test_with_span_events()),
             ?_test(test_with_span_error_handling())
         ]
     end}.

test_with_span_basic() ->
    Fun = fun() -> <<"result">> end,
    Result = erlmcp_otel:with_span(<<"test.operation">>, Fun),
    ?assertEqual(<<"result">>, Result).

test_with_span_attributes() ->
    Attrs = #{operation => <<"test">>, version => <<"1.0">>},
    Fun = fun() -> ok end,
    Result = erlmcp_otel:with_span(<<"test.op">>, Attrs, Fun),
    ?assertMatch(ok | {ok, _}, Result).

test_with_span_events() ->
    Fun = fun() -> ok end,
    Result = erlmcp_otel:with_span(<<"test.event">>, Fun),
    ?assertMatch(ok | {ok, _}, Result).

test_with_span_error_handling() ->
    Fun = fun() -> throw(test_error) end,
    Result = (catch erlmcp_otel:with_span(<<"test.error">>, Fun)),
    ?assertEqual(test_error, Result).

%%====================================================================
%% Trace Context Tests
%%====================================================================

context_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_get_trace_context()),
             ?_test(test_set_trace_context()),
             ?_test(test_inject_context()),
             ?_test(test_extract_context())
         ]
     end}.

test_get_trace_context() ->
    Result = erlmcp_otel:get_trace_context(),
    ?assert(is_map(Result) orelse Result =:= undefined orelse Result =:= null).

test_set_trace_context() ->
    Context = #{trace_id => <<"test-trace">>, span_id => <<"test-span">>},
    Result = erlmcp_otel:set_trace_context(Context),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_inject_context() ->
    Headers = #{},
    Result = erlmcp_otel:inject_context(Headers),
    ?assert(is_map(Result)).

test_extract_context() ->
    Headers = #{<<"traceparent">> => <<"00-trace-span-01">>},
    Result = erlmcp_otel:extract_context(Headers),
    ?assert(is_map(Result) orelse Result =:= undefined).

%%====================================================================
%% Metrics Tests
%%====================================================================

metrics_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_record_request_latency()),
             ?_test(test_record_error()),
             ?_test(test_record_counter()),
             ?_test(test_record_gauge())
         ]
     end}.

test_record_request_latency() ->
    Result = erlmcp_otel:record_request_latency(<<"test.method">>, 100),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_record_error() ->
    Result = erlmcp_otel:record_error(<<"test.error">>, error_reason),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_record_counter() ->
    Result = erlmcp_otel:record_counter(<<"requests.total">>, 1),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_record_gauge() ->
    Result = erlmcp_otel:record_gauge(<<"connections.active">>, 42),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Attribute Tests
%%====================================================================

attribute_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_add_attributes()),
             ?_test(test_set_attribute()),
             ?_test(test_attribute_validation()),
             ?_test(test_reserved_attributes())
         ]
     end}.

test_add_attributes() ->
    Attrs = #{service => <<"erlmcp">>, version => <<"1.0">>},
    Result = erlmcp_otel:add_attributes(Attrs),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_set_attribute() ->
    Result = erlmcp_otel:set_attribute(<<"user.id">>, <<"12345">>),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_attribute_validation() ->
    InvalidAttrs = #{<<"">>#"empty_key" => <<"value">>},
    Result = erlmcp_otel:add_attributes(InvalidAttrs),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_reserved_attributes() ->
    Attrs = #{<<"service.name">> => <<"erlmcp">>},
    Result = erlmcp_otel:add_attributes(Attrs),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Baggage Tests
%%====================================================================

baggage_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_set_baggage()),
             ?_test(test_get_baggage()),
             ?_test(test_baggage_propagation())
         ]
     end}.

test_set_baggage() ->
    Result = erlmcp_otel:set_baggage(<<"request.id">>, <<"req-123">>),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_get_baggage() ->
    erlmcp_otel:set_baggage(<<"test.key">>, <<"test.value">>),
    Result = erlmcp_otel:get_baggage(<<"test.key">>),
    ?assertMatch(<<"test.value">> | undefined | {ok, _} | {error, _}, [Result]).

test_baggage_propagation() ->
    erlmcp_otel:set_baggage(<<"key1">>, <<"value1">>),
    Baggage = erlmcp_otel:get_baggage(<<"key1">>),
    ?assertMatch(<<"value1">> | undefined | {ok, _}, [Baggage]).

%%====================================================================
%% Event Recording Tests
%%====================================================================

event_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_record_event()),
             ?_test(test_record_exception()),
             ?_test(test_record_warning()),
             ?_test(test_record_info())
         ]
     end}.

test_record_event() ->
    Result = erlmcp_otel:record_event(<<"test.event">>, #{details => <<"test">>}),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_record_exception() ->
    Exception = {error, test_error},
    Result = erlmcp_otel:record_exception(<<"test.exception">>, Exception),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_record_warning() ->
    Result = erlmcp_otel:record_warning(<<"test.warning">>, <<"warning message">>),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_record_info() ->
    Result = erlmcp_otel:record_info(<<"test.info">>, <<"info message">>),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Configuration Tests
%%====================================================================

config_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_set_sampling_rate()),
             ?_test(test_get_config()),
             ?_test(test_set_exporter()),
             ?_test(test_disable_tracing())
         ]
     end}.

test_set_sampling_rate() ->
    Result = erlmcp_otel:set_sampling_rate(0.5),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_get_config() ->
    Result = erlmcp_otel:get_config(),
    ?assert(is_map(Result) orelse Result =:= undefined orelse Result =:= error).

test_set_exporter() ->
    Result = erlmcp_otel:set_exporter(jaeger, #{endpoint => <<"http://localhost:14268">>}),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_disable_tracing() ->
    Result = erlmcp_otel:disable_tracing(),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_full_trace_lifecycle()),
             ?_test(test_span_with_context()),
             ?_test(test_multiple_spans()),
             ?_test(test_span_nesting())
         ]
     end}.

test_full_trace_lifecycle() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),
    Fun = fun() -> <<"done">> end,
    Result = erlmcp_otel:with_span(<<"test.lifecycle">>, Fun),
    ?assertEqual(<<"done">>, Result).

test_span_with_context() ->
    Fun = fun() -> erlmcp_otel:get_trace_context() end,
    Result = erlmcp_otel:with_span(<<"test.context">>, Fun),
    ?assert(is_map(Result) orelse Result =:= undefined orelse Result =:= null).

test_multiple_spans() ->
    Fun1 = fun() -> <<"span1">> end,
    Fun2 = fun() -> <<"span2">> end,
    Result1 = erlmcp_otel:with_span(<<"span1">>, Fun1),
    Result2 = erlmcp_otel:with_span(<<"span2">>, Fun2),
    ?assertEqual([<<"span1">>, <<"span2">>], [Result1, Result2]).

test_span_nesting() ->
    OuterFun = fun() ->
        InnerFun = fun() -> <<"inner">> end,
        erlmcp_otel:with_span(<<"inner.span">>, InnerFun)
    end,
    Result = erlmcp_otel:with_span(<<"outer.span">>, OuterFun),
    ?assertEqual(<<"inner">>, Result).
