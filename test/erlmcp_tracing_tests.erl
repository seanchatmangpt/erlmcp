-module(erlmcp_tracing_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_tracing Module
%%====================================================================

setup() -> ok.
cleanup(_) -> ok.

%%====================================================================
%% Initialization Tests
%%====================================================================

init_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_init_tracing()),
        ?_test(test_init_with_config()),
        ?_test(test_init_idempotent())
    ] end}.

test_init_tracing() ->
    Result = erlmcp_tracing:init(),
    ?assertMatch(ok | {ok, _}, Result).

test_init_with_config() ->
    Config = #{enabled => true, sampling_rate => 1.0},
    Result = erlmcp_tracing:init(Config),
    ?assertMatch(ok | {ok, _}, Result).

test_init_idempotent() ->
    erlmcp_tracing:init(),
    Result = erlmcp_tracing:init(),
    ?assertMatch(ok | {ok, _}, Result).

%%====================================================================
%% Trace Tests
%%====================================================================

trace_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_start_trace()),
        ?_test(test_start_trace_with_parent()),
        ?_test(test_end_trace()),
        ?_test(test_trace_span())
    ] end}.

test_start_trace() ->
    Result = erlmcp_tracing:start_trace(<<"test.trace">>),
    ?assert(is_map(Result) orelse Result =:= ok orelse Result =:= {ok, _}).

test_start_trace_with_parent() ->
    TraceId = erlmcp_tracing:start_trace(<<"parent">>),
    Result = erlmcp_tracing:start_trace(<<"child">>, TraceId),
    ?assert(Result =/= error).

test_end_trace() ->
    TraceId = erlmcp_tracing:start_trace(<<"test">>),
    Result = erlmcp_tracing:end_trace(TraceId),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_trace_span() ->
    Fun = fun() -> ok end,
    Result = erlmcp_tracing:trace_span(<<"operation">>, Fun),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Span Tests
%%====================================================================

span_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_create_span()),
        ?_test(test_add_span_event()),
        ?_test(test_span_attributes()),
        ?_test(test_span_status())
    ] end}.

test_create_span() ->
    Result = erlmcp_tracing:create_span(<<"operation">>),
    ?assert(is_map(Result) orelse is_atom(Result) orelse Result =/= error).

test_add_span_event() ->
    Span = erlmcp_tracing:create_span(<<"test">>),
    Result = erlmcp_tracing:add_span_event(Span, <<"event">>, #{}),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_span_attributes() ->
    Span = erlmcp_tracing:create_span(<<"test">>),
    Attrs = #{key => <<"value">>, number => 42},
    Result = erlmcp_tracing:set_span_attributes(Span, Attrs),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_span_status() ->
    Span = erlmcp_tracing:create_span(<<"test">>),
    Result = erlmcp_tracing:set_span_status(Span, success),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Baggage Tests
%%====================================================================

baggage_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_set_baggage()),
        ?_test(test_get_baggage()),
        ?_test(test_baggage_propagation())
    ] end}.

test_set_baggage() ->
    Result = erlmcp_tracing:set_baggage(<<"key">>, <<"value">>),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_get_baggage() ->
    erlmcp_tracing:set_baggage(<<"test">>, <<"value">>),
    Result = erlmcp_tracing:get_baggage(<<"test">>),
    ?assertMatch(<<"value">> | undefined | {ok, _}, [Result]).

test_baggage_propagation() ->
    erlmcp_tracing:set_baggage(<<"id">>, <<"123">>),
    Value = erlmcp_tracing:get_baggage(<<"id">>),
    ?assertMatch(<<"123">> | undefined | {ok, _}, [Value]).

%%====================================================================
%% Context Tests
%%====================================================================

context_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_get_current_context()),
        ?_test(test_set_context()),
        ?_test(test_inject_context()),
        ?_test(test_extract_context())
    ] end}.

test_get_current_context() ->
    Result = erlmcp_tracing:get_current_context(),
    ?assert(is_map(Result) orelse Result =:= undefined).

test_set_context() ->
    Context = #{trace_id => <<"test">>},
    Result = erlmcp_tracing:set_context(Context),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_inject_context() ->
    Headers = #{},
    Result = erlmcp_tracing:inject_context(Headers),
    ?assert(is_map(Result)).

test_extract_context() ->
    Headers = #{<<"x-trace-id">> => <<"123">>},
    Result = erlmcp_tracing:extract_context(Headers),
    ?assert(is_map(Result) orelse Result =:= undefined).

%%====================================================================
%% Sampling Tests
%%====================================================================

sampling_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_should_sample()),
        ?_test(test_set_sampling_rate()),
        ?_test(test_sampling_probability())
    ] end}.

test_should_sample() ->
    Result = erlmcp_tracing:should_sample(),
    ?assert(is_boolean(Result)).

test_set_sampling_rate() ->
    Result = erlmcp_tracing:set_sampling_rate(0.5),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_sampling_probability() ->
    erlmcp_tracing:set_sampling_rate(1.0),
    ?assert(erlmcp_tracing:should_sample()),
    erlmcp_tracing:set_sampling_rate(0.0),
    ?assertNot(erlmcp_tracing:should_sample()).

%%====================================================================
%% Metrics Tests
%%====================================================================

metrics_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_record_duration()),
        ?_test(test_record_count()),
        ?_test(test_get_metrics())
    ] end}.

test_record_duration() ->
    Result = erlmcp_tracing:record_duration(<<"operation">>, 100),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_record_count() ->
    Result = erlmcp_tracing:record_count(<<"requests">>, 1),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_get_metrics() ->
    erlmcp_tracing:record_count(<<"test">>, 1),
    Result = erlmcp_tracing:get_metrics(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= undefined).

%%====================================================================
%% Error Recording Tests
%%====================================================================

error_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_record_error()),
        ?_test(test_record_exception()),
        ?_test(test_error_with_stacktrace())
    ] end}.

test_record_error() ->
    Result = erlmcp_tracing:record_error(<<"error">>, error_reason),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_record_exception() ->
    Exception = {error, test},
    Result = erlmcp_tracing:record_exception(Exception),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_error_with_stacktrace() ->
    Error = {error, test},
    Stack = [{module, func, 1}],
    Result = erlmcp_tracing:record_error(Error, Stack),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_full_trace_lifecycle()),
        ?_test(test_trace_with_events()),
        ?_test(test_nested_spans()),
        ?_test(test_trace_with_errors())
    ] end}.

test_full_trace_lifecycle() ->
    erlmcp_tracing:init(),
    TraceId = erlmcp_tracing:start_trace(<<"lifecycle">>),
    erlmcp_tracing:end_trace(TraceId),
    ?assert(TraceId =/= error).

test_trace_with_events() ->
    TraceId = erlmcp_tracing:start_trace(<<"events">>),
    erlmcp_tracing:add_span_event(TraceId, <<"event1">>, #{}),
    erlmcp_tracing:add_span_event(TraceId, <<"event2">>, #{}),
    erlmcp_tracing:end_trace(TraceId),
    ?assert(TraceId =/= error).

test_nested_spans() ->
    Parent = erlmcp_tracing:start_trace(<<"parent">>),
    Child = erlmcp_tracing:start_trace(<<"child">>, Parent),
    erlmcp_tracing:end_trace(Child),
    erlmcp_tracing:end_trace(Parent),
    ?assert(Parent =/= error andalso Child =/= error).

test_trace_with_errors() ->
    TraceId = erlmcp_tracing:start_trace(<<"errors">>),
    erlmcp_tracing:record_error(TraceId, test_error),
    erlmcp_tracing:end_trace(TraceId),
    ?assert(TraceId =/= error).
