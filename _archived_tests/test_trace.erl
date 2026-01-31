#!/usr/bin/env escript

-module(test_trace).

test() ->
    io:format("Testing ErlMCP Simple Trace...~n"),
    
    % Test basic tracing
    TraceId = erlmcp_simple_trace:start_trace(<<"test_trace">>, #{component => <<"test">>}),
    io:format("Started trace: ~p~n", [TraceId]),
    
    % Add a span
    SpanId = erlmcp_simple_trace:add_span(<<"test_operation">>, #{
        operation => <<"test">>,
        component => <<"trace_test">>
    }),
    io:format("Added span: ~p~n", [SpanId]),
    
    % Simulate some work
    timer:sleep(10),
    
    % End the span (note: need to manually calculate duration for this simple test)
    % In real usage, the convenience functions handle this automatically
    
    % Test JSON formatting
    Context = erlmcp_simple_trace:current_trace(),
    case Context of
        undefined ->
            io:format("ERROR: No trace context found~n"),
            halt(1);
        _ ->
            JsonTrace = erlmcp_simple_trace:format_trace_json(Context),
            io:format("JSON trace: ~s~n", [JsonTrace])
    end,
    
    % End the trace
    erlmcp_simple_trace:end_trace(TraceId),
    
    io:format("Trace test completed successfully!~n"),
    init:stop().

main(_) ->
    test().