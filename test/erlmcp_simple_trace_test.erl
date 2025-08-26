-module(erlmcp_simple_trace_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Test Suite
%%====================================================================

simple_trace_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"basic tracing workflow", fun test_basic_tracing/0},
         {"nested spans", fun test_nested_spans/0},
         {"error handling", fun test_error_handling/0},
         {"transport operation tracing", fun test_transport_operation/0},
         {"server operation tracing", fun test_server_operation/0},
         {"message processing tracing", fun test_message_processing/0},
         {"json export", fun test_json_export/0},
         {"auto trace creation", fun test_auto_trace_creation/0}
     ]}.

setup() ->
    %% Configure logger to capture log messages for testing
    logger:set_handler_config(default, level, debug),
    ok.

cleanup(_) ->
    %% Clear any remaining trace context
    erlang:erase(),
    ok.

%%====================================================================
%% Basic Tracing Tests
%%====================================================================

test_basic_tracing() ->
    %% Start a trace
    TraceId = erlmcp_simple_trace:start_trace(<<"test_trace">>, #{user => <<"test_user">>}),
    ?assert(is_binary(TraceId)),
    
    %% Verify trace context exists
    Context = erlmcp_simple_trace:current_trace(),
    ?assertMatch(#{trace_id := TraceId}, Context),
    
    %% Add a span
    SpanId1 = erlmcp_simple_trace:add_span(<<"operation1">>, #{component => test}),
    ?assert(is_binary(SpanId1)),
    
    %% Simulate some work
    timer:sleep(10),
    
    %% Add another span
    SpanId2 = erlmcp_simple_trace:add_span(<<"operation2">>, #{component => test, value => 42}),
    ?assert(is_binary(SpanId2)),
    
    %% Simulate more work
    timer:sleep(5),
    
    %% End the trace
    ok = erlmcp_simple_trace:end_trace(TraceId),
    
    %% Verify trace context is cleared
    ?assertEqual(undefined, erlmcp_simple_trace:current_trace()).

test_nested_spans() ->
    TraceId = erlmcp_simple_trace:start_trace(<<"nested_test">>),
    
    %% Add parent span
    ParentSpanId = erlmcp_simple_trace:add_span(<<"parent_operation">>, #{level => 1}),
    
    %% Add child span (should automatically use parent as parent)
    ChildSpanId = erlmcp_simple_trace:add_span(<<"child_operation">>, #{level => 2}),
    
    %% Add grandchild span
    GrandchildSpanId = erlmcp_simple_trace:add_span(<<"grandchild_operation">>, #{level => 3}),
    
    ?assert(ParentSpanId =/= ChildSpanId),
    ?assert(ChildSpanId =/= GrandchildSpanId),
    
    %% End trace
    erlmcp_simple_trace:end_trace(TraceId).

test_error_handling() ->
    TraceId = erlmcp_simple_trace:start_trace(<<"error_test">>),
    
    %% This should not crash even with invalid trace ID
    ok = erlmcp_simple_trace:end_trace(<<"invalid_trace_id">>),
    
    %% Original trace should still be active
    ?assertMatch(#{trace_id := TraceId}, erlmcp_simple_trace:current_trace()),
    
    %% End the real trace
    erlmcp_simple_trace:end_trace(TraceId).

%%====================================================================
%% Convenience Function Tests
%%====================================================================

test_transport_operation() ->
    erlmcp_simple_trace:start_trace(<<"transport_test">>),
    
    Result = erlmcp_simple_trace:trace_transport_operation(
        <<"send_message">>, 
        stdio,
        fun() ->
            timer:sleep(5),
            {ok, <<"message_sent">>}
        end
    ),
    
    ?assertEqual({ok, <<"message_sent">>}, Result),
    
    Context = erlmcp_simple_trace:current_trace(),
    ?assertMatch(#{spans := [_|_]}, Context),
    
    erlmcp_simple_trace:end_trace(maps:get(trace_id, Context)).

test_server_operation() ->
    erlmcp_simple_trace:start_trace(<<"server_test">>),
    
    Result = erlmcp_simple_trace:trace_server_operation(
        <<"handle_request">>, 
        test_server,
        fun() ->
            timer:sleep(3),
            {ok, processed}
        end
    ),
    
    ?assertEqual({ok, processed}, Result),
    
    Context = erlmcp_simple_trace:current_trace(),
    erlmcp_simple_trace:end_trace(maps:get(trace_id, Context)).

test_message_processing() ->
    erlmcp_simple_trace:start_trace(<<"message_test">>),
    
    Result = erlmcp_simple_trace:trace_message_processing(
        <<"tools/list">>,
        <<"msg_123">>,
        1024,
        fun() ->
            timer:sleep(2),
            {ok, [tool1, tool2]}
        end
    ),
    
    ?assertEqual({ok, [tool1, tool2]}, Result),
    
    Context = erlmcp_simple_trace:current_trace(),
    erlmcp_simple_trace:end_trace(maps:get(trace_id, Context)).

%%====================================================================
%% Advanced Feature Tests
%%====================================================================

test_json_export() ->
    TraceId = erlmcp_simple_trace:start_trace(<<"json_test">>, #{version => <<"1.0">>}),
    
    erlmcp_simple_trace:add_span(<<"test_operation">>, #{result => success}),
    timer:sleep(1),
    
    Context = erlmcp_simple_trace:current_trace(),
    JsonBinary = erlmcp_simple_trace:format_trace_json(Context),
    
    ?assert(is_binary(JsonBinary)),
    
    %% Verify it's valid JSON by parsing it
    try
        jsx:decode(JsonBinary)
    of
        JsonMap when is_map(JsonMap) ->
            ok
    catch
        _:_ ->
            ?assert(false, "Generated JSON is not valid")
    end,
    
    erlmcp_simple_trace:end_trace(TraceId).

test_auto_trace_creation() ->
    %% Ensure no trace exists
    ?assertEqual(undefined, erlmcp_simple_trace:current_trace()),
    
    %% Add span without starting trace - should auto-create
    SpanId = erlmcp_simple_trace:add_span(<<"auto_operation">>, #{auto => true}),
    
    %% Should now have a trace context
    Context = erlmcp_simple_trace:current_trace(),
    ?assertMatch(#{trace_id := _}, Context),
    
    erlmcp_simple_trace:end_trace(maps:get(trace_id, Context)).

%%====================================================================
%% Error Scenario Tests
%%====================================================================

transport_operation_error_test() ->
    erlmcp_simple_trace:start_trace(<<"error_transport_test">>),
    
    try
        erlmcp_simple_trace:trace_transport_operation(
            <<"failing_send">>,
            tcp,
            fun() ->
                error(connection_failed)
            end
        )
    catch
        error:connection_failed ->
            ok  %% Expected error
    end,
    
    %% Trace should still be active and contain error information
    Context = erlmcp_simple_trace:current_trace(),
    ?assertMatch(#{spans := [_|_]}, Context),
    
    erlmcp_simple_trace:end_trace(maps:get(trace_id, Context)).

%%====================================================================
%% Performance Tests
%%====================================================================

performance_basic_test() ->
    %% Simple performance test - should be fast
    StartTime = erlang:monotonic_time(microsecond),
    
    TraceId = erlmcp_simple_trace:start_trace(<<"perf_test">>),
    
    %% Add multiple spans
    [erlmcp_simple_trace:add_span(
        iolist_to_binary(io_lib:format("span_~p", [N])),
        #{number => N}
    ) || N <- lists:seq(1, 10)],
    
    erlmcp_simple_trace:end_trace(TraceId),
    
    EndTime = erlang:monotonic_time(microsecond),
    Duration = EndTime - StartTime,
    
    %% Should complete in less than 100ms (100,000 microseconds)
    ?assert(Duration < 100000, 
           io_lib:format("Tracing took ~p microseconds, expected < 100000", [Duration])).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_with_real_work_test() ->
    TraceId = erlmcp_simple_trace:start_trace(<<"integration_test">>, #{
        test_type => integration,
        component => erlmcp
    }),
    
    %% Simulate real work patterns
    erlmcp_simple_trace:trace_transport_operation(
        <<"receive_request">>,
        stdio,
        fun() ->
            %% Simulate nested server operation
            erlmcp_simple_trace:trace_server_operation(
                <<"process_request">>,
                main_server,
                fun() ->
                    %% Simulate registry lookup
                    erlmcp_simple_trace:trace_registry_operation(
                        <<"lookup_tool">>,
                        fun() ->
                            timer:sleep(1),
                            {ok, found}
                        end
                    )
                end
            )
        end
    ),
    
    Context = erlmcp_simple_trace:current_trace(),
    ?assertMatch(#{spans := Spans} when length(Spans) >= 3, Context),
    
    erlmcp_simple_trace:end_trace(TraceId).