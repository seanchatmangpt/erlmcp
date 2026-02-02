# MCP Observability Instrumentation Guide

## Quick Reference: Instrumenting MCP Operations

### 1. Adding OTEL Spans

```erlang
%% In erlmcp_server.erl - Instrument handle_call
handle_call({call_tool, Name, Args}, From, State) ->
    Span = erlmcp_otel:start_span(<<"mcp.tools.call">>, #{
        <<"tool.name">> => Name,
        <<"args.count">> => maps:size(Args)
    }),
    try
        Result = execute_tool(Name, Args, State),
        erlmcp_otel:add_attributes(Span, #{<<"success">> => true}),
        erlmcp_otel:end_span(Span),
        {reply, Result, State}
    catch
        Class:Reason:Stack ->
            erlmcp_otel:record_error(Span, {Class, Reason, Stack}),
            erlmcp_otel:end_span(Span),
            {reply, {error, Reason}, State}
    end.
```

### 2. Recording Metrics

```erlang
%% Record MCP-specific metrics
record_mcp_operation(Operation, Latency, Labels) ->
    %% Histogram for latency
    erlmcp_mcp_metrics:record_histogram(
        <<"mcp.", Operation/binary, ".latency_ms">>,
        Latency,
        Labels
    ),
    
    %% Counter for throughput
    erlmcp_mcp_metrics:increment_counter(
        <<"mcp.", Operation/binary, ".total">>,
        Labels
    ).
```

### 3. Health Checks

```erlang
%% Add MCP-specific health check
erlmcp_health_monitor:register_component(
    {mcp_tools, ServerId},
    Pid,
    fun() -> 
        case test_tool_call(ServerId) of
            {ok, Latency} when Latency < 5000 -> healthy;
            {ok, _} -> degraded;
            {error, _} -> unhealthy
        end
    end
).
```

### 4. Chaos Tests

```erlang
%% Run chaos scenario
erlmcp_chaos:run(#{
    experiment => tool_handler_crash,
    target => tools,
    rate => 0.1,
    duration => 60000,
    sla_threshold => #{p95_latency_ms => 100}
}).
```

