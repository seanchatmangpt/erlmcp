-module(jsx_simple_trace_integration_test).
-export([run/0]).

run() ->
    %% Start jsx application
    application:start(jsx),
    
    io:format("=== JSX Simple Trace Integration Test ===~n"),
    
    %% Test erlmcp_simple_trace with jsx integration
    try
        %% Start a trace
        TraceId = erlmcp_simple_trace:start_trace(<<"jsx_integration_test">>, #{
            test_type => validation,
            component => jsx,
            environment => production
        }),
        
        io:format("Started trace: ~p~n", [TraceId]),
        
        %% Add some spans with complex data
        SpanId1 = erlmcp_simple_trace:add_span(<<"json_encode_operation">>, #{
            component => jsx,
            input_size => 1024,
            operation_type => encode,
            success => true
        }),
        
        timer:sleep(5),  % Simulate work
        
        SpanId2 = erlmcp_simple_trace:add_span(<<"json_decode_operation">>, #{
            component => jsx,
            input_format => json,
            output_format => erlang_term,
            validation_passed => true
        }),
        
        timer:sleep(3),  % More work
        
        %% Get current trace context
        Context = erlmcp_simple_trace:current_trace(),
        io:format("Current trace context has ~p spans~n", [length(maps:get(spans, Context, []))]),
        
        %% Test JSON formatting with jsx
        TraceJson = erlmcp_simple_trace:format_trace_json(Context),
        io:format("Generated JSON trace data: ~p bytes~n", [byte_size(TraceJson)]),
        
        %% Verify the JSON can be parsed back
        try
            ParsedTrace = jsx:decode(TraceJson),
            io:format("JSON parsing validation: SUCCESS~n"),
            
            %% Verify trace structure
            TraceIdFromJson = maps:get(<<"trace_id">>, ParsedTrace),
            SpansFromJson = maps:get(<<"spans">>, ParsedTrace),
            MetadataFromJson = maps:get(<<"metadata">>, ParsedTrace),
            
            io:format("Trace ID preserved: ~p~n", [TraceIdFromJson == TraceId]),
            io:format("Spans count: ~p~n", [length(SpansFromJson)]),
            io:format("Metadata preserved: ~p~n", [is_map(MetadataFromJson)]),
            
            %% End the trace
            erlmcp_simple_trace:end_trace(TraceId),
            io:format("Trace ended successfully~n"),
            
            io:format("JSX Simple Trace Integration: SUCCESS ✓~n"),
            true
            
        catch
            _:JsonError ->
                io:format("JSON parsing failed: ~p~n", [JsonError]),
                io:format("JSX Simple Trace Integration: FAILED ✗~n"),
                false
        end
        
    catch
        _:Error ->
            io:format("Trace integration failed: ~p~n", [Error]),
            io:format("JSX Simple Trace Integration: FAILED ✗~n"),
            false
    end,
    
    io:format("=== JSX Simple Trace Integration Test: COMPLETED ===~n"),
    ok.