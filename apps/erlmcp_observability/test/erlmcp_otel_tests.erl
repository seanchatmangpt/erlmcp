%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_otel module following Chicago School TDD
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior through API calls only
%%% - Use REAL erlmcp_otel module (no mocks, no dummy processes)
%%% - NO internal state inspection (test API boundaries only)
%%% - NO record duplication (respect encapsulation)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otel_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Setup/teardown for OTEL initialization
otel_test_() ->
    {setup,
     fun setup_otel/0,
     fun cleanup_otel/1,
     [{"Initialize OpenTelemetry", fun test_init/0},
      {"Start and end span", fun test_span_lifecycle/0},
      {"Execute function within span", fun test_with_span/0},
      {"Nested spans with parent-child relationship", fun test_nested_spans/0},
      {"Record error in span", fun test_record_error/0},
      {"Add attributes to span", fun test_add_attributes/0},
      {"Add events to span", fun test_add_events/0},
      {"Baggage propagation", fun test_baggage/0},
      {"Context propagation", fun test_context_propagation/0},
      {"Restore context from headers", fun test_restore_context/0}]}.

setup_otel() ->
    % Initialize OTEL with test configuration
    Config =
        #{service_name => <<"test_service">>,
          service_version => <<"test_1.0.0">>,
          exporters => [console],
          sampling => always_on},
    ok = erlmcp_otel:init(Config),
    Config.

cleanup_otel(_Config) ->
    % Shutdown OTEL and clean up
    ok = erlmcp_otel:shutdown(),
    % Clean process dictionary
    erase(erlmcp_otel_current_context),
    erase(erlmcp_otel_config).

%%====================================================================
%% Test Cases
%%====================================================================

%% Test that OTEL initializes successfully
test_init() ->
    % Initialize should return ok
    Config = #{service_name => <<"init_test">>, exporters => [console]},
    ?assertEqual(ok, erlmcp_otel:init(Config)),
    % Shutdown to clean up
    ok = erlmcp_otel:shutdown().

%% Test span lifecycle: start, use, end
test_span_lifecycle() ->
    % Start a span with attributes
    SpanCtx = erlmcp_otel:start_span(<<"test.operation">>, #{<<"test.key">> => <<"test.value">>}),

    % Verify span context is a map with required fields (observable behavior)
    ?assert(is_map(SpanCtx)),
    ?assert(maps:is_key(trace_id, SpanCtx)),
    ?assert(maps:is_key(span_id, SpanCtx)),
    ?assert(maps:is_key(start_time, SpanCtx)),

    % End the span
    ?assertEqual(ok, erlmcp_otel:end_span(SpanCtx)).

%% Test executing a function within a span context
test_with_span() ->
    % Execute a function within a span
    Result =
        erlmcp_otel:with_span(<<"test.span">>,
                              #{<<"operation">> => <<"test">>},
                              fun() ->
                                 timer:sleep(1),
                                 {ok, success}
                              end),

    % Verify function result is returned
    ?assertEqual({ok, success}, Result).

%% Test nested spans with parent-child relationships
test_nested_spans() ->
    % Create outer span
    OuterCtx = erlmcp_otel:start_span(<<"outer.span">>, #{}),
    ?assert(is_map(OuterCtx)),
    ?assert(maps:is_key(trace_id, OuterCtx)),
    ?assert(maps:is_key(span_id, OuterCtx)),

    % Create inner span with explicit parent
    InnerCtx = erlmcp_otel:start_span(<<"inner.span">>, #{<<"parent">> => <<"outer">>}, OuterCtx),
    ?assert(is_map(InnerCtx)),

    % Verify trace ID is propagated (same trace in both spans)
    ?assertEqual(maps:get(trace_id, OuterCtx), maps:get(trace_id, InnerCtx)),

    % Verify parent span ID is set
    ?assertEqual(maps:get(span_id, OuterCtx), maps:get(parent_span_id, InnerCtx)),

    % End spans in reverse order
    ?assertEqual(ok, erlmcp_otel:end_span(InnerCtx)),
    ?assertEqual(ok, erlmcp_otel:end_span(OuterCtx)).

%% Test error recording in spans
test_record_error() ->
    SpanCtx = erlmcp_otel:start_span(<<"error.span">>, #{}),

    % Record an error
    Error = {error, test_error, []},
    ?assertEqual(ok, erlmcp_otel:record_error(SpanCtx, Error)),

    % End span
    ?assertEqual(ok, erlmcp_otel:end_span(SpanCtx)).

%% Test adding attributes to spans
test_add_attributes() ->
    SpanCtx = erlmcp_otel:start_span(<<"attr.span">>, #{}),

    % Add attributes
    ?assertEqual(ok,
                 erlmcp_otel:add_attributes(SpanCtx,
                                            #{<<"string.attr">> => <<"value">>,
                                              <<"number.attr">> => 42,
                                              <<"boolean.attr">> => true})),

    % End span
    ?assertEqual(ok, erlmcp_otel:end_span(SpanCtx)).

%% Test adding events to spans
test_add_events() ->
    SpanCtx = erlmcp_otel:start_span(<<"event.span">>, #{}),

    % Add event without attributes
    ?assertEqual(ok, erlmcp_otel:add_event(SpanCtx, <<"test.event">>)),

    % Add event with attributes
    ?assertEqual(ok,
                 erlmcp_otel:add_event(SpanCtx,
                                       <<"test.event2">>,
                                       #{<<"event.key">> => <<"event.value">>})),

    % End span
    ?assertEqual(ok, erlmcp_otel:end_span(SpanCtx)).

%% Test baggage for correlation
test_baggage() ->
    SpanCtx = erlmcp_otel:start_span(<<"baggage.span">>, #{}),

    % Set baggage
    ?assertEqual(ok, erlmcp_otel:set_baggage(<<"correlation.id">>, <<"test-123">>)),
    ?assertEqual(ok, erlmcp_otel:set_baggage(<<"user.id">>, <<"user-456">>)),

    % Get baggage
    ?assertEqual(<<"test-123">>, erlmcp_otel:get_baggage(<<"correlation.id">>)),
    ?assertEqual(<<"user-456">>, erlmcp_otel:get_baggage(<<"user.id">>)),

    % Non-existent baggage returns undefined
    ?assertEqual(undefined, erlmcp_otel:get_baggage(<<"nonexistent">>)),

    % End span
    ?assertEqual(ok, erlmcp_otel:end_span(SpanCtx)).

%% Test context propagation across process boundaries
test_context_propagation() ->
    SpanCtx =
        erlmcp_otel:start_span(<<"propagation.span">>,
                               #{<<"baggage">> => #{<<"request.id">> => <<"req-789">>}}),

    % Propagate context
    Headers = erlmcp_otel:propagate_context(SpanCtx),

    % Verify traceparent header
    ?assert(maps:is_key(<<"traceparent">>, Headers)),
    TraceParent = maps:get(<<"traceparent">>, Headers),
    ?assertMatch(<<"00-", _Rest/binary>>, TraceParent),

    % End span
    ?assertEqual(ok, erlmcp_otel:end_span(SpanCtx)).

%% Test restoring context from headers
test_restore_context() ->
    % Create a span and propagate it
    OriginalCtx = erlmcp_otel:start_span(<<"restore.span">>, #{}),
    Headers = erlmcp_otel:propagate_context(OriginalCtx),

    % Restore context from headers
    RestoredCtx = erlmcp_otel:restore_context(Headers),

    % Verify context is restored
    ?assert(is_map(RestoredCtx)),
    ?assertEqual(maps:get(trace_id, OriginalCtx), maps:get(trace_id, RestoredCtx)),

    % End original span
    ?assertEqual(ok, erlmcp_otel:end_span(OriginalCtx)).

%%====================================================================
%% Edge Case Tests
%%====================================================================

%% Test with_span with error propagation
with_span_error_test() ->
    % Function that throws an error
    ErrorFun = fun() -> throw(test_error) end,

    % Error should be re-raised after being recorded in span
    ?assertThrow(test_error, erlmcp_otel:with_span(<<"error.span">>, #{}, ErrorFun)).

%% Test with_span with multiple errors
with_span_multiple_errors_test() ->
    % Test with different error types
    ?assertThrow({test_error, reason},
                 erlmcp_otel:with_span(<<"error.span">>,
                                       #{},
                                       fun() -> erlang:error({test_error, reason}) end)),

    ?assertExit(test_exit,
                erlmcp_otel:with_span(<<"error.span">>, #{}, fun() -> exit(test_exit) end)).

%% Test span with empty attributes
span_empty_attributes_test() ->
    SpanCtx = erlmcp_otel:start_span(<<"empty.span">>, #{}),
    ?assert(is_map(SpanCtx)),
    ?assertEqual(ok, erlmcp_otel:end_span(SpanCtx)).

%% Test add_attributes with empty map
add_empty_attributes_test() ->
    SpanCtx = erlmcp_otel:start_span(<<"test.span">>, #{}),
    ?assertEqual(ok, erlmcp_otel:add_attributes(SpanCtx, #{})),
    ?assertEqual(ok, erlmcp_otel:end_span(SpanCtx)).

%% Test multiple sequential spans
multiple_sequential_spans_test() ->
    % Create and end multiple spans sequentially
    lists:foreach(fun(N) ->
                     Name = list_to_binary(["span.", integer_to_list(N)]),
                     SpanCtx = erlmcp_otel:start_span(Name, #{}),
                     ?assertEqual(ok, erlmcp_otel:end_span(SpanCtx))
                  end,
                  lists:seq(1, 10)).

%%====================================================================
%% Integration Tests
%%====================================================================

%% Test complete request lifecycle with multiple operations
request_lifecycle_test_() ->
    {setup,
     fun setup_otel/0,
     fun cleanup_otel/1,
     fun(_Config) ->
        [?_test(begin
                    % Simulate a complete request lifecycle
                    RequestCtx =
                        erlmcp_otel:start_span(<<"mcp.request">>,
                                               #{<<"request.id">> => <<"req-001">>}),

                    % Set baggage for correlation
                    ok = erlmcp_otel:set_baggage(<<"request.id">>, <<"req-001">>),

                    % Add processing event
                    ok = erlmcp_otel:add_event(RequestCtx, <<"processing.started">>),

                    % Simulate tool call
                    ToolCtx =
                        erlmcp_otel:start_span(<<"mcp.tool.call">>,
                                               #{<<"tool.name">> => <<"test_tool">>},
                                               RequestCtx),
                    ok = erlmcp_otel:add_event(ToolCtx, <<"tool.executed">>),
                    ok = erlmcp_otel:end_span(ToolCtx),

                    % Add completion event
                    ok = erlmcp_otel:add_event(RequestCtx, <<"request.completed">>),

                    % End request span
                    ok = erlmcp_otel:end_span(RequestCtx)
                end)]
     end}.

%% Test RPC span injection
rpc_span_test_() ->
    {setup,
     fun setup_otel/0,
     fun cleanup_otel/1,
     fun(_Config) ->
        [?_test(begin
                    % Create RPC span
                    RpcCtx =
                        erlmcp_otel:inject_rpc_span(<<"tools.call">>,
                                                    <<"req-002">>,
                                                    #{<<"name">> => <<"echo">>,
                                                      <<"arg">> => <<"hello">>}),

                    % Verify RPC span has correct attributes
                    ?assert(is_map(RpcCtx)),
                    ?assert(maps:is_key(trace_id, RpcCtx)),
                    ?assert(maps:is_key(span_id, RpcCtx)),

                    % End RPC span
                    ok = erlmcp_otel:end_span(RpcCtx)
                end)]
     end}.
