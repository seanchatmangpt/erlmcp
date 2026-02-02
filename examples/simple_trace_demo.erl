#!/usr/bin/env escript
%% -*- erlang -*-

-module(simple_trace_demo).

-export([main/1]).

main(_Args) ->
    io:format("=== ErlMCP Simple Tracing Demo ===~n~n"),
    
    %% Start logger to see trace output
    logger:set_handler_config(default, level, info),
    
    %% Demo 1: Basic tracing workflow
    demo_basic_tracing(),
    
    %% Demo 2: Transport operation tracing
    demo_transport_tracing(),
    
    %% Demo 3: Nested operations
    demo_nested_operations(),
    
    %% Demo 4: Error handling
    demo_error_handling(),
    
    %% Demo 5: JSON export
    demo_json_export(),
    
    io:format("~n=== Demo Complete ===~n").

%%====================================================================
%% Demo Functions
%%====================================================================

demo_basic_tracing() ->
    io:format("1. Basic Tracing Workflow~n"),
    io:format("   Starting trace...~n"),
    
    %% Start a trace with metadata
    TraceId = erlmcp_simple_trace:start_trace(<<"demo_basic">>, #{
        user => <<"demo_user">>,
        version => <<"0.6.0">>,
        environment => development
    }),
    
    io:format("   Trace ID: ~s~n", [TraceId]),
    
    %% Add some spans
    SpanId1 = erlmcp_simple_trace:add_span(<<"initialize">>, #{
        component => demo,
        step => 1
    }),
    
    %% Simulate work
    timer:sleep(10),
    
    SpanId2 = erlmcp_simple_trace:add_span(<<"process_data">>, #{
        component => demo,
        step => 2,
        data_size => 1024
    }),
    
    timer:sleep(5),
    
    SpanId3 = erlmcp_simple_trace:add_span(<<"finalize">>, #{
        component => demo,
        step => 3
    }),
    
    timer:sleep(3),
    
    io:format("   Created spans: ~s, ~s, ~s~n", [SpanId1, SpanId2, SpanId3]),
    
    %% End the trace
    erlmcp_simple_trace:end_trace(TraceId),
    io:format("   Trace completed.~n~n").

demo_transport_tracing() ->
    io:format("2. Transport Operation Tracing~n"),
    
    TraceId = erlmcp_simple_trace:start_trace(<<"demo_transport">>),
    
    %% Simulate a transport operation
    Result = erlmcp_simple_trace:trace_transport_operation(
        <<"send_message">>,
        stdio,
        fun() ->
            io:format("   Simulating message send...~n"),
            timer:sleep(15),
            {ok, message_sent, 256}
        end
    ),
    
    io:format("   Transport result: ~p~n", [Result]),
    
    erlmcp_simple_trace:end_trace(TraceId),
    io:format("   Transport trace completed.~n~n").

demo_nested_operations() ->
    io:format("3. Nested Operations~n"),
    
    TraceId = erlmcp_simple_trace:start_trace(<<"demo_nested">>),
    
    %% Demonstrate nested tracing
    Result = erlmcp_simple_trace:trace_server_operation(
        <<"handle_request">>,
        main_server,
        fun() ->
            io:format("   Server processing request...~n"),
            
            %% Nested registry operation
            RegistryResult = erlmcp_simple_trace:trace_registry_operation(
                <<"lookup_tool">>,
                fun() ->
                    io:format("   Registry looking up tool...~n"),
                    timer:sleep(8),
                    {ok, tool_found, #{name => calculator, version => <<"1.0">>}}
                end
            ),
            
            %% Nested message processing
            MessageResult = erlmcp_simple_trace:trace_message_processing(
                <<"tools/list">>,
                <<"msg_12345">>,
                512,
                fun() ->
                    io:format("   Processing message...~n"),
                    timer:sleep(12),
                    {ok, [calculator, weather_api]}
                end
            ),
            
            {ok, {RegistryResult, MessageResult}}
        end
    ),
    
    io:format("   Nested operations result: ~p~n", [Result]),
    
    erlmcp_simple_trace:end_trace(TraceId),
    io:format("   Nested trace completed.~n~n").

demo_error_handling() ->
    io:format("4. Error Handling~n"),
    
    TraceId = erlmcp_simple_trace:start_trace(<<"demo_error">>),
    
    %% Demonstrate error tracing
    try
        erlmcp_simple_trace:trace_transport_operation(
            <<"failing_operation">>,
            tcp,
            fun() ->
                io:format("   Simulating operation that will fail...~n"),
                timer:sleep(5),
                error(connection_timeout)
            end
        )
    catch
        error:connection_timeout ->
            io:format("   Caught expected error: connection_timeout~n")
    end,
    
    %% Show that trace is still active after error
    Context = erlmcp_simple_trace:current_trace(),
    SpanCount = length(maps:get(spans, Context, [])),
    io:format("   Trace still active with ~p spans~n", [SpanCount]),
    
    erlmcp_simple_trace:end_trace(TraceId),
    io:format("   Error trace completed.~n~n").

demo_json_export() ->
    io:format("5. JSON Export~n"),
    
    TraceId = erlmcp_simple_trace:start_trace(<<"demo_json">>, #{
        export_demo => true,
        timestamp => erlang:system_time(second)
    }),
    
    %% Add a few spans for the JSON export
    erlmcp_simple_trace:add_span(<<"json_operation1">>, #{
        type => demo,
        value => 42
    }),
    
    timer:sleep(5),
    
    erlmcp_simple_trace:add_span(<<"json_operation2">>, #{
        type => demo,
        value => 84,
        nested => #{a => 1, b => 2}
    }),
    
    timer:sleep(3),
    
    %% Export to JSON before ending
    Context = erlmcp_simple_trace:current_trace(),
    JsonBinary = erlmcp_simple_trace:format_trace_json(Context),
    
    io:format("   Generated JSON trace:~n"),
    
    %% Pretty print the JSON
    try
        JsonMap = jsx:decode(JsonBinary),
        PrettyJson = jsx:prettify(jsx:encode(JsonMap)),
        io:format("~s~n", [PrettyJson])
    catch
        _:_ ->
            io:format("   Raw JSON: ~s~n", [JsonBinary])
    end,
    
    erlmcp_simple_trace:end_trace(TraceId),
    io:format("   JSON export demo completed.~n~n").

%%====================================================================
%% Helper Functions
%%====================================================================

format_duration(MicroSeconds) when MicroSeconds < 1000 ->
    io_lib:format("~pμs", [MicroSeconds]);
format_duration(MicroSeconds) when MicroSeconds < 1000000 ->
    io_lib:format("~.1fms", [MicroSeconds / 1000]);
format_duration(MicroSeconds) ->
    io_lib:format("~.1fs", [MicroSeconds / 1000000]).