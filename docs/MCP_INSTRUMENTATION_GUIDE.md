# MCP Instrumentation Quick Reference
**Version:** 1.0.0
**Date:** 2026-02-02
**Purpose:** Developer guide for adding MCP observability to code

---

## Quick Start

### 1. Add OTEL Span to Function

**Pattern:** Wrap function with span
```erlang
my_function(Arg1, Arg2) ->
    SpanCtx = erlmcp_otel:start_span(<<"mcp.my_operation">>, #{
        <<"arg1">> => Arg1,
        <<"arg2">> => Arg2
    }),
    try
        Result = do_work(Arg1, Arg2),
        erlmcp_otel:end_span(SpanCtx),
        Result
    catch
        Class:Reason:Stacktrace ->
            erlmcp_otel:record_error(SpanCtx, {Class, Reason, Stacktrace}),
            erlmcp_otel:end_span(SpanCtx),
            erlang:raise(Class, Reason, Stacktrace)
    end.
```

**Alternative: with_span/3** (automatic error handling)
```erlang
my_function(Arg1, Arg2) ->
    erlmcp_otel:with_span(
        <<"mcp.my_operation">>,
        #{<<"arg1">> => Arg1, <<"arg2">> => Arg2},
        fun() -> do_work(Arg1, Arg2) end
    ).
```

### 2. Record Metric

**Async (non-blocking):**
```erlang
StartTime = erlang:monotonic_time(microsecond),
Result = do_work(),
Duration = erlang:monotonic_time(microsecond) - StartTime,

erlmcp_mcp_metrics:record_tool_call(
    ServerId,
    ToolName,
    ValidationDuration,
    ExecutionDuration
).
```

### 3. Log Event

```erlang
erlmcp_mcp_logger:log_protocol_event(
    <<"tools/call">>,
    <<"complete">>,
    #{tool_name => ToolName, duration_us => Duration}
).
```

---

## Instrumentation Patterns

### Pattern 1: gen_server:handle_call with OTEL + Metrics

**Full Example:**
```erlang
handle_call({call_tool, Name, Args}, From, State) ->
    %% 1. Start OTEL span
    SpanCtx = erlmcp_otel:start_span(<<"mcp.tools.call">>, #{
        <<"tool.name">> => Name,
        <<"server_id">> => State#state.server_id
    }),

    %% 2. Log start event
    erlmcp_mcp_logger:log_protocol_event(
        <<"tools/call">>,
        <<"start">>,
        #{tool_name => Name}
    ),

    try
        %% 3. Validation timing
        ValidationStart = erlang:monotonic_time(microsecond),
        ok = validate_tool_schema(Name, Args, State),
        ValidationDuration = erlang:monotonic_time(microsecond) - ValidationStart,

        %% 4. Execution timing
        ExecutionStart = erlang:monotonic_time(microsecond),
        Result = execute_tool_handler(Name, Args, State),
        ExecutionDuration = erlang:monotonic_time(microsecond) - ExecutionStart,

        %% 5. Record metrics (async)
        erlmcp_mcp_metrics:record_tool_call(
            State#state.server_id,
            Name,
            ValidationDuration,
            ExecutionDuration
        ),

        %% 6. Add span attributes
        erlmcp_otel:add_attributes(SpanCtx, #{
            <<"validation_time_us">> => ValidationDuration,
            <<"execution_time_us">> => ExecutionDuration,
            <<"success">> => true
        }),

        %% 7. Log complete event
        TotalDuration = ValidationDuration + ExecutionDuration,
        erlmcp_mcp_logger:log_protocol_event(
            <<"tools/call">>,
            <<"complete">>,
            #{tool_name => Name, duration_us => TotalDuration}
        ),

        %% 8. End span
        erlmcp_otel:end_span(SpanCtx),

        {reply, Result, State}
    catch
        Class:Reason:Stacktrace ->
            %% 9. Record error in span
            erlmcp_otel:record_error(SpanCtx, {Class, Reason, Stacktrace}),
            erlmcp_otel:end_span(SpanCtx),

            %% 10. Log error event
            erlmcp_mcp_logger:log_error(
                <<"tool_execution_failed">>,
                #{tool_name => Name, error => Reason},
                Stacktrace
            ),

            {reply, {error, Reason}, State}
    end.
```

### Pattern 2: Nested Spans (Parent-Child)

```erlang
process_request(Request) ->
    %% Parent span
    ParentSpan = erlmcp_otel:start_span(<<"mcp.process_request">>, #{
        <<"request_id">> => maps:get(<<"id">>, Request)
    }),

    try
        %% Child span 1: Decode
        DecodeResult = erlmcp_otel:with_span(
            <<"mcp.jsonrpc.decode">>,
            #{},
            ParentSpan,
            fun() -> erlmcp_json_rpc:decode(Request) end
        ),

        %% Child span 2: Execute
        ExecuteResult = erlmcp_otel:with_span(
            <<"mcp.execute">>,
            #{<<"method">> => maps:get(<<"method">>, DecodeResult)},
            ParentSpan,
            fun() -> execute_method(DecodeResult) end
        ),

        %% Child span 3: Encode
        Response = erlmcp_otel:with_span(
            <<"mcp.jsonrpc.encode">>,
            #{},
            ParentSpan,
            fun() -> erlmcp_json_rpc:encode(ExecuteResult) end
        ),

        erlmcp_otel:end_span(ParentSpan),
        {ok, Response}
    catch
        Class:Reason:Stacktrace ->
            erlmcp_otel:record_error(ParentSpan, {Class, Reason, Stacktrace}),
            erlmcp_otel:end_span(ParentSpan),
            {error, Reason}
    end.
```

### Pattern 3: Context Propagation (Client-Server)

**Client Side (inject context):**
```erlang
call_remote_tool(ToolName, Args) ->
    %% Start client span
    SpanCtx = erlmcp_otel:start_span(<<"mcp.tools.call.client">>, #{
        <<"tool.name">> => ToolName,
        <<"span.kind">> => <<"client">>
    }),

    %% Propagate trace context in request
    Request = #{
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => ToolName,
            <<"arguments">> => Args
        },
        <<"_trace">> => erlmcp_otel:propagate_context(SpanCtx)
    },

    %% Send request
    case send_request(Request) of
        {ok, Response} ->
            erlmcp_otel:end_span(SpanCtx),
            {ok, Response};
        {error, Reason} ->
            erlmcp_otel:record_error(SpanCtx, {error, Reason, []}),
            erlmcp_otel:end_span(SpanCtx),
            {error, Reason}
    end.
```

**Server Side (restore context):**
```erlang
handle_request(Request) ->
    %% Restore parent context from request
    ParentCtx = case maps:get(<<"_trace">>, Request, undefined) of
        undefined -> undefined;
        TraceCtx -> erlmcp_otel:restore_context(TraceCtx)
    end,

    %% Create child span
    SpanCtx = erlmcp_otel:start_span(
        <<"mcp.tools.call.server">>,
        #{<<"span.kind">> => <<"server">>},
        ParentCtx
    ),

    %% Process request
    Result = process_tool_call(Request),

    erlmcp_otel:end_span(SpanCtx),
    Result.
```

### Pattern 4: Conditional Instrumentation (Sampling)

```erlang
maybe_instrument(Operation, Fun) ->
    %% Check sampling decision
    ShouldSample = erlmcp_mcp_sampling:should_sample(Operation),

    case ShouldSample of
        true ->
            %% Full instrumentation
            erlmcp_otel:with_span(
                Operation,
                #{},
                Fun
            );
        false ->
            %% No instrumentation, just execute
            Fun()
    end.
```

### Pattern 5: Metric Recording with Histogram

```erlang
record_latency_metric(Operation, ServerId, Fun) ->
    StartTime = erlang:monotonic_time(microsecond),
    try
        Result = Fun(),
        Duration = erlang:monotonic_time(microsecond) - StartTime,

        %% Record success latency
        erlmcp_mcp_metrics:record_latency(Operation, ServerId, Duration, success),

        Result
    catch
        Class:Reason:Stacktrace ->
            Duration = erlang:monotonic_time(microsecond) - StartTime,

            %% Record error latency
            erlmcp_mcp_metrics:record_latency(Operation, ServerId, Duration, error),

            erlang:raise(Class, Reason, Stacktrace)
    end.
```

---

## Common Instrumentation Points

### erlmcp_server.erl

#### 1. handle_call({call_tool, Name, Args}, From, State)
**What to measure:**
- Total latency (start to end)
- Validation time (schema validation)
- Execution time (handler execution)
- Success/error rate

**Code:**
```erlang
SpanCtx = erlmcp_otel:start_span(<<"mcp.tools.call">>, #{...}),
ValidationStart = erlang:monotonic_time(microsecond),
% ... validate
ValidationDuration = erlang:monotonic_time(microsecond) - ValidationStart,
ExecutionStart = erlang:monotonic_time(microsecond),
% ... execute
ExecutionDuration = erlang:monotonic_time(microsecond) - ExecutionStart,
erlmcp_mcp_metrics:record_tool_call(ServerId, Name, ValidationDuration, ExecutionDuration),
erlmcp_otel:end_span(SpanCtx).
```

#### 2. handle_call({list_resources}, From, State)
**What to measure:**
- Latency (list operation)
- Resource count
- Template matching time (if applicable)

**Code:**
```erlang
StartTime = erlang:monotonic_time(microsecond),
Resources = list_resources_internal(State),
Duration = erlang:monotonic_time(microsecond) - StartTime,
erlmcp_mcp_metrics:record_resource_list(State#state.server_id, Duration).
```

#### 3. handle_call({read_resource, Uri}, From, State)
**What to measure:**
- Latency (read operation)
- Cache hit/miss
- Content size

**Code:**
```erlang
StartTime = erlang:monotonic_time(microsecond),
{Content, CacheHit} = read_resource_internal(Uri, State),
Duration = erlang:monotonic_time(microsecond) - StartTime,
erlmcp_mcp_metrics:record_resource_read(
    State#state.server_id,
    Uri,
    Duration,
    CacheHit
).
```

#### 4. notify_resource_updated(ServerId, Uri, Content)
**What to measure:**
- Notification latency
- Fan-out count (number of subscribers)

**Code:**
```erlang
StartTime = erlang:monotonic_time(microsecond),
SubscriberCount = pg:get_members(?PG_SCOPE, {resource_subscription, Uri}),
% ... notify subscribers
Duration = erlang:monotonic_time(microsecond) - StartTime,
erlmcp_mcp_metrics:record_subscription_update(
    ServerId,
    Uri,
    length(SubscriberCount)
).
```

### erlmcp_client.erl

#### 1. initialize(Client, Capabilities)
**What to measure:**
- Initialize latency
- Handshake success/failure

**Code:**
```erlang
SpanCtx = erlmcp_otel:start_span(<<"mcp.initialize.client">>, #{
    <<"span.kind">> => <<"client">>
}),
StartTime = erlang:monotonic_time(microsecond),
Result = send_initialize_request(Client, Capabilities),
Duration = erlang:monotonic_time(microsecond) - StartTime,
erlmcp_mcp_metrics:record_session_init(get_server_id(Client), Duration),
erlmcp_otel:end_span(SpanCtx).
```

### erlmcp_transport_*.erl

#### 1. send(State, Data)
**What to measure:**
- Send latency
- Message size

**Code:**
```erlang
StartTime = erlang:monotonic_time(microsecond),
ok = do_send(State, Data),
Duration = erlang:monotonic_time(microsecond) - StartTime,
erlmcp_mcp_metrics:record_transport_send(
    get_server_id(State),
    get_transport_type(State),
    Duration
).
```

---

## Decision Logging

### When to Log Decisions

**Always log:**
- Sampling decisions (which requests are traced)
- Routing decisions (which handler to use)
- Caching decisions (cache hit/miss)
- Failover decisions (which backup to use)

### Example: Log Sampling Decision

```erlang
ShouldSample = erlmcp_mcp_sampling:should_sample(Operation),

erlmcp_mcp_logger:log_decision(
    <<"sampling">>,
    case ShouldSample of true -> <<"sample">>; false -> <<"skip">> end,
    get_sampling_rationale(Operation)
).
```

### Example: Log Caching Decision

```erlang
case check_cache(Uri) of
    {ok, Content} ->
        erlmcp_mcp_logger:log_decision(
            <<"caching">>,
            <<"cache_hit">>,
            <<"Resource found in cache">>
        ),
        {ok, Content, true};
    undefined ->
        erlmcp_mcp_logger:log_decision(
            <<"caching">>,
            <<"cache_miss">>,
            <<"Resource not cached, fetching from handler">>
        ),
        fetch_resource(Uri)
end.
```

---

## Error Logging

### When to Log Errors

**Always log:**
- Protocol errors (invalid JSON-RPC, missing fields)
- Handler errors (tool/resource handler crashes)
- Transport errors (connection failures)
- Validation errors (schema validation failures)

### Example: Log Tool Handler Error

```erlang
catch
    Class:Reason:Stacktrace ->
        erlmcp_mcp_logger:log_error(
            <<"tool_execution_failed">>,
            #{
                tool_name => ToolName,
                error => Reason,
                class => Class
            },
            Stacktrace
        ),
        {error, Reason}
end.
```

### Example: Log Validation Error

```erlang
case validate_schema(Args, Schema) of
    ok -> ok;
    {error, ValidationErrors} ->
        erlmcp_mcp_logger:log_error(
            <<"schema_validation_failed">>,
            #{
                tool_name => ToolName,
                validation_errors => ValidationErrors
            },
            []
        ),
        {error, invalid_arguments}
end.
```

---

## Performance Impact

### Overhead Targets

| Operation | Max Overhead | Strategy |
|-----------|--------------|----------|
| Span creation | 0.5ms | Use sampling |
| Metric recording | 0.01ms | Async cast |
| Log event | 0.1ms | Async cast |
| Context propagation | 0.05ms | Minimal serialization |

### Optimization Techniques

**1. Use async recording:**
```erlang
%% Good - non-blocking
erlmcp_mcp_metrics:record_tool_call(ServerId, Name, ValidationDuration, ExecutionDuration).

%% Bad - blocking
gen_server:call(erlmcp_mcp_metrics, {record_tool_call, ...}).
```

**2. Sample high-volume operations:**
```erlang
%% Sample 10% of resource reads
case should_sample(resources_read, 0.1) of
    true -> erlmcp_otel:start_span(...);
    false -> skip
end.
```

**3. Batch metric flushes:**
```erlang
%% Flush metrics every 10 seconds instead of immediately
-define(FLUSH_INTERVAL, 10000).
```

---

## Testing Instrumentation

### Unit Test Pattern

```erlang
instrumentation_test() ->
    %% Setup
    meck:new(erlmcp_otel, [passthrough]),
    meck:new(erlmcp_mcp_metrics, [passthrough]),

    %% Execute instrumented function
    Result = my_instrumented_function(Args),

    %% Verify span created
    ?assert(meck:called(erlmcp_otel, start_span, ['_', '_'])),
    ?assert(meck:called(erlmcp_otel, end_span, ['_'])),

    %% Verify metric recorded
    ?assert(meck:called(erlmcp_mcp_metrics, record_tool_call, ['_', '_', '_', '_'])),

    %% Cleanup
    meck:unload().
```

### Integration Test Pattern

```erlang
end_to_end_instrumentation_test() ->
    %% Start observability stack
    {ok, _} = application:ensure_all_started(erlmcp_observability),

    %% Execute MCP operation
    {ok, Result} = erlmcp_server:call_tool(ServerId, ToolName, Args),

    %% Verify metrics collected
    Metrics = erlmcp_mcp_metrics:get_metrics_snapshot(),
    ?assertMatch(#{<<"erlmcp.mcp.tools.call.latency_us">> := _}, Metrics),

    %% Verify logs generated
    Logs = erlmcp_mcp_logger:get_event_history({last, 10}),
    ?assert(length(Logs) > 0).
```

---

## Checklist for New Code

**Before adding instrumentation:**
- [ ] Identify operation (tools/call, resources/read, etc.)
- [ ] Determine sampling rate (1.0 for critical, 0.1 for high-volume)
- [ ] Choose metric types (histogram for latency, counter for counts)
- [ ] Plan error handling (catch all exceptions)

**During instrumentation:**
- [ ] Add OTEL span (start_span + end_span)
- [ ] Record metrics (async, non-blocking)
- [ ] Log events (protocol_event, decision, error)
- [ ] Propagate context (if crossing process boundaries)
- [ ] Add span attributes (operation-specific metadata)

**After instrumentation:**
- [ ] Write unit tests (verify span/metric calls)
- [ ] Write integration tests (end-to-end verification)
- [ ] Measure overhead (<5% latency increase)
- [ ] Document in code (EDoc comments)
- [ ] Update metrics taxonomy (if new metrics added)

---

## Resources

- **Architecture:** `/home/user/erlmcp/docs/MCP_OBSERVABILITY_ARCHITECTURE.md`
- **Metrics Taxonomy:** `/home/user/erlmcp/docs/MCP_METRICS_TAXONOMY.md`
- **Logging Schema:** `/home/user/erlmcp/docs/MCP_LOGGING_SCHEMA.md`
- **OTEL Module:** `/home/user/erlmcp/apps/erlmcp_observability/src/erlmcp_otel.erl`

---

**Quick Reference Complete!**
