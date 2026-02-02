# Comprehensive MCP Observability Design for erlmcp v2.1.0

**Author:** Erlang Performance Agent + Architecture Team  
**Date:** 2026-02-01  
**Version:** 1.0.0  
**Status:** Design Specification

---

## Executive Summary

This document specifies a comprehensive observability strategy for erlmcp's MCP implementation, building on existing infrastructure (31 observability modules) and addressing MCP-specific requirements. The design integrates OpenTelemetry, custom metrics, profiling, health monitoring, chaos engineering, and compliance tracking.

**Key Objectives:**
1. **100% MCP operation visibility** - Trace every MCP request (resources, tools, prompts, subscriptions)
2. **Performance baseline enforcement** - P50<5ms, P95<20ms, P99<50ms
3. **Chaos resilience verification** - Validate fault tolerance under failure injection
4. **Integration with claude-flow** - Support hybrid observability for SONA requirements
5. **Compliance tracking** - Automated MCP 2025-11-25 specification adherence

---

## 1. OpenTelemetry Integration for MCP Operations

### 1.1 MCP-Specific Span Taxonomy

**Span Naming Convention:** `mcp.<category>.<operation>`

| Span Name | Operation | Attributes |
|-----------|-----------|------------|
| `mcp.initialize` | initialize | `protocol_version`, `client_info`, `server_capabilities` |
| `mcp.resources.list` | resources/list | `cursor`, `count`, `total_resources` |
| `mcp.resources.read` | resources/read | `uri`, `resource_type`, `cache_hit` |
| `mcp.resources.subscribe` | resources/subscribe | `uri`, `subscriber_count` |
| `mcp.resources.unsubscribe` | resources/unsubscribe | `uri`, `subscriber_count` |
| `mcp.resources.notify` | Resource update notifications | `uri`, `fan_out_count`, `notification_latency_ms` |
| `mcp.tools.list` | tools/list | `cursor`, `count`, `total_tools` |
| `mcp.tools.call` | tools/call | `tool_name`, `validation_time_ms`, `execution_time_ms`, `schema_validated` |
| `mcp.prompts.list` | prompts/list | `cursor`, `count`, `total_prompts` |
| `mcp.prompts.get` | prompts/get | `prompt_name`, `argument_count` |
| `mcp.transport.send` | Transport send | `transport_type`, `message_size_bytes`, `encoding_time_ms` |
| `mcp.transport.receive` | Transport receive | `transport_type`, `message_size_bytes`, `decoding_time_ms` |
| `mcp.jsonrpc.encode` | JSON-RPC encoding | `method`, `payload_size_bytes`, `encoder` |
| `mcp.jsonrpc.decode` | JSON-RPC decoding | `method`, `payload_size_bytes`, `decoder` |

### 1.2 Instrumentation Points

**Location: apps/erlmcp_core/src/erlmcp_server.erl**

```erlang
%% Example: Instrument tools/call with nested spans
handle_call({call_tool, Name, Args}, From, State) ->
    %% Start parent span for entire tool call
    ParentSpan = erlmcp_otel:start_span(<<"mcp.tools.call">>, #{
        <<"tool.name">> => Name,
        <<"args.count">> => length(maps:keys(Args))
    }),
    
    try
        %% Nested span: Schema validation
        ValidationResult = erlmcp_otel:with_span(
            <<"mcp.tools.validate_schema">>,
            #{<<"tool.name">> => Name},
            fun() -> validate_tool_schema(Name, Args, State) end
        ),
        
        %% Nested span: Handler execution
        ExecutionResult = erlmcp_otel:with_span(
            <<"mcp.tools.execute_handler">>,
            #{<<"tool.name">> => Name},
            fun() -> execute_tool_handler(Name, Args, State) end
        ),
        
        erlmcp_otel:add_attributes(ParentSpan, #{
            <<"validation_time_ms">> => get_duration(ValidationResult),
            <<"execution_time_ms">> => get_duration(ExecutionResult),
            <<"success">> => true
        }),
        erlmcp_otel:end_span(ParentSpan),
        {reply, ExecutionResult, State}
    catch
        Class:Reason:Stacktrace ->
            erlmcp_otel:record_error(ParentSpan, {Class, Reason, Stacktrace}),
            erlmcp_otel:end_span(ParentSpan),
            {reply, {error, Reason}, State}
    end.
```

### 1.3 Context Propagation

**Across Transport Boundaries:**

```erlang
%% Inject trace context into JSON-RPC message
send_rpc_request(Method, Params, State) ->
    TraceCtx = erlmcp_otel:get_current_context(),
    Message = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method,
        <<"params">> => Params,
        <<"traceContext">> => erlmcp_otel:propagate_context(TraceCtx)
    },
    send_message(Message, State).

%% Restore trace context on receive
handle_rpc_request(Message, State) ->
    case maps:get(<<"traceContext">>, Message, undefined) of
        undefined -> ok;
        TraceCtx -> erlmcp_otel:restore_context(TraceCtx)
    end,
    process_rpc(Message, State).
```

---

## 2. MCP-Specific Metrics

### 2.1 Metric Definitions

**Metric Naming Convention:** `erlmcp.mcp.<category>.<metric_type>`

#### Resource Metrics

| Metric Name | Type | Labels | Description |
|-------------|------|--------|-------------|
| `erlmcp.mcp.resources.list.latency_ms` | Histogram | `server_id` | P50/P95/P99 latency for resources/list |
| `erlmcp.mcp.resources.list.throughput` | Counter | `server_id` | Requests per second |
| `erlmcp.mcp.resources.read.latency_ms` | Histogram | `server_id`, `uri`, `cache_hit` | Read latency by resource |
| `erlmcp.mcp.resources.read.throughput` | Counter | `server_id`, `uri` | Reads per second per resource |
| `erlmcp.mcp.resources.subscribe.count` | Gauge | `server_id`, `uri` | Active subscriptions per resource |
| `erlmcp.mcp.resources.notify.latency_ms` | Histogram | `server_id`, `uri` | Notification delivery latency |
| `erlmcp.mcp.resources.notify.fan_out` | Histogram | `server_id`, `uri` | Subscribers notified per update |
| `erlmcp.mcp.resources.template.match_time_ms` | Histogram | `server_id`, `pattern` | URI template matching time |

#### Tool Metrics

| Metric Name | Type | Labels | Description |
|-------------|------|--------|-------------|
| `erlmcp.mcp.tools.list.latency_ms` | Histogram | `server_id` | tools/list latency |
| `erlmcp.mcp.tools.call.latency_ms` | Histogram | `server_id`, `tool_name` | Total tool call latency |
| `erlmcp.mcp.tools.call.validation_time_ms` | Histogram | `server_id`, `tool_name` | Schema validation time |
| `erlmcp.mcp.tools.call.execution_time_ms` | Histogram | `server_id`, `tool_name` | Handler execution time |
| `erlmcp.mcp.tools.call.throughput` | Counter | `server_id`, `tool_name` | Tool invocations per second |
| `erlmcp.mcp.tools.call.errors` | Counter | `server_id`, `tool_name`, `error_type` | Tool call failures |
| `erlmcp.mcp.tools.schema_cache.hits` | Counter | `server_id` | Schema cache hits |
| `erlmcp.mcp.tools.schema_cache.misses` | Counter | `server_id` | Schema cache misses |

#### Prompt Metrics

| Metric Name | Type | Labels | Description |
|-------------|------|--------|-------------|
| `erlmcp.mcp.prompts.list.latency_ms` | Histogram | `server_id` | prompts/list latency |
| `erlmcp.mcp.prompts.get.latency_ms` | Histogram | `server_id`, `prompt_name` | prompts/get latency |
| `erlmcp.mcp.prompts.get.throughput` | Counter | `server_id`, `prompt_name` | Gets per second |

#### Transport Metrics

| Metric Name | Type | Labels | Description |
|-------------|------|--------|-------------|
| `erlmcp.mcp.transport.send.latency_ms` | Histogram | `server_id`, `transport_type` | Transport send latency |
| `erlmcp.mcp.transport.receive.latency_ms` | Histogram | `server_id`, `transport_type` | Transport receive latency |
| `erlmcp.mcp.transport.message_size_bytes` | Histogram | `server_id`, `transport_type`, `direction` | Message size distribution |
| `erlmcp.mcp.jsonrpc.encode_latency_ms` | Histogram | `server_id`, `method` | JSON encoding time |
| `erlmcp.mcp.jsonrpc.decode_latency_ms` | Histogram | `server_id`, `method` | JSON decoding time |

#### Session Metrics

| Metric Name | Type | Labels | Description |
|-------------|------|--------|-------------|
| `erlmcp.mcp.session.active_count` | Gauge | `server_id` | Active MCP sessions |
| `erlmcp.mcp.session.connection_duration_s` | Histogram | `server_id`, `transport_type` | Session duration |
| `erlmcp.mcp.session.init_latency_ms` | Histogram | `server_id` | Initialize handshake latency |
| `erlmcp.mcp.session.phase` | Gauge | `server_id`, `phase` | Sessions per phase (init/ready) |

### 2.2 Implementation: MCP Metrics Collector

**New Module: apps/erlmcp_observability/src/erlmcp_mcp_metrics.erl**

```erlang
-module(erlmcp_mcp_metrics).
-behaviour(gen_server).

%% API
-export([start_link/0,
         record_resource_list/2,
         record_resource_read/3,
         record_subscription_update/3,
         record_tool_call/4,
         record_tool_validation/3,
         get_mcp_summary/0]).

%% Metrics collection
record_resource_list(ServerId, Latency) ->
    erlmcp_metrics:record_server_operation(
        ServerId, 
        <<"resources_list">>, 
        Latency, 
        #{<<"component">> => <<"mcp">>}
    ),
    %% Also export to OTEL
    otel_histogram:record(
        'erlmcp.mcp.resources.list.latency_ms',
        Latency,
        #{server_id => ServerId}
    ).

record_tool_call(ServerId, ToolName, ValidationTime, ExecutionTime) ->
    TotalTime = ValidationTime + ExecutionTime,
    Labels = #{
        server_id => ServerId,
        tool_name => ToolName
    },
    
    %% Record histograms
    otel_histogram:record('erlmcp.mcp.tools.call.latency_ms', TotalTime, Labels),
    otel_histogram:record('erlmcp.mcp.tools.call.validation_time_ms', ValidationTime, Labels),
    otel_histogram:record('erlmcp.mcp.tools.call.execution_time_ms', ExecutionTime, Labels),
    
    %% Increment counter
    otel_counter:add('erlmcp.mcp.tools.call.throughput', 1, Labels).
```

---

## 3. Performance Profiling and Flame Graphs

### 3.1 Profiling Strategy

**Three-Tier Profiling:**

1. **Continuous Lightweight Profiling** (Always On)
   - Metrics collection (histograms, counters)
   - Sample-based tracing (1% sample rate)
   - Process info snapshots (every 60s)

2. **On-Demand Deep Profiling** (Manual/Triggered)
   - fprof for function-level CPU profiling
   - eprof for time-based profiling
   - recon_trace for live message tracing

3. **Incident Response Profiling** (Auto-Triggered)
   - Latency spike detection (P99 > 100ms)
   - Memory leak detection (heap growth > 20%/min)
   - Message queue buildup (>1000 messages)

### 3.2 Flame Graph Generation

**Integration with erlmcp_profiler:**

```erlang
%% Generate flame graph for MCP tool call path
profile_tool_call_path(ToolName, Duration) ->
    %% Start profiling
    erlmcp_profiler:profile(erlmcp_server, handle_call, 3, #{
        duration => Duration,
        output => "/tmp/mcp_tool_profile.out",
        mode => fprof
    }),
    
    %% Convert fprof output to folded stacks
    erlmcp_profiler:export_folded_stacks(
        "/tmp/mcp_tool_profile.out",
        "/tmp/mcp_tool_stacks.folded"
    ),
    
    %% Generate SVG flame graph (using flamegraph.pl)
    os:cmd("flamegraph.pl /tmp/mcp_tool_stacks.folded > /tmp/mcp_tool_flame.svg"),
    
    {ok, "/tmp/mcp_tool_flame.svg"}.
```

### 3.3 Automated Performance Regression Detection

**Benchmark Suite Integration:**

```erlang
%% New module: apps/erlmcp_observability/src/erlmcp_mcp_benchmarks.erl
-module(erlmcp_mcp_benchmarks).

-export([run_all/0, run_baseline_comparison/1]).

run_all() ->
    Benchmarks = [
        {resources_list_10k, fun bench_resources_list/0},
        {resource_read_simple, fun bench_resource_read/0},
        {tool_call_no_validation, fun bench_tool_call_simple/0},
        {tool_call_with_schema, fun bench_tool_call_complex/0},
        {subscription_fan_out_100, fun bench_subscription_fanout/0}
    ],
    
    Results = [run_benchmark(Name, Fun) || {Name, Fun} <- Benchmarks],
    
    %% Compare against baseline
    Baseline = load_baseline("baseline_v2.1.0.json"),
    Regression = detect_regression(Results, Baseline, 0.10),  % 10% threshold
    
    case Regression of
        [] -> {ok, Results};
        Regressions -> {regression_detected, Regressions, Results}
    end.
```

---

## 4. Health Monitoring and Alerting

### 4.1 MCP-Specific Health Checks

**Extend erlmcp_health_monitor with MCP checks:**

```erlang
%% Register MCP component health checks
register_mcp_health_checks(ServerId) ->
    %% Check 1: Resource list response time
    erlmcp_health_monitor:register_component(
        {mcp_resources, ServerId},
        whereis(ServerId),
        fun() -> check_resource_list_latency(ServerId) end
    ),
    
    %% Check 2: Tool call availability
    erlmcp_health_monitor:register_component(
        {mcp_tools, ServerId},
        whereis(ServerId),
        fun() -> check_tool_availability(ServerId) end
    ),
    
    %% Check 3: Subscription fan-out latency
    erlmcp_health_monitor:register_component(
        {mcp_subscriptions, ServerId},
        whereis(ServerId),
        fun() -> check_subscription_latency(ServerId) end
    ).

check_resource_list_latency(ServerId) ->
    StartTime = erlang:monotonic_time(microsecond),
    case erlmcp_server:list_resources(ServerId) of
        {ok, _Resources} ->
            Latency = erlang:monotonic_time(microsecond) - StartTime,
            if
                Latency < 5000 -> healthy;  % <5ms
                Latency < 20000 -> degraded;  % 5-20ms
                true -> unhealthy  % >20ms
            end;
        {error, _} ->
            unhealthy
    end.
```

### 4.2 Alert Definitions

| Alert Name | Condition | Severity | Action |
|------------|-----------|----------|--------|
| `MCP_ResourceListLatencyHigh` | P95 > 20ms for 5 min | Warning | Log, notify ops |
| `MCP_ResourceListLatencyCritical` | P95 > 50ms for 2 min | Critical | Auto-scale, page oncall |
| `MCP_ToolCallFailureRate` | Error rate > 5% for 5 min | Warning | Log, investigate |
| `MCP_SubscriptionFanOutSlow` | P95 notification latency > 50ms | Warning | Review subscriber count |
| `MCP_SessionInitFailure` | Init timeout rate > 10% | Critical | Check network, auth |
| `MCP_SchemaValidationCacheMiss` | Cache miss rate > 50% | Info | Review cache policy |
| `MCP_TransportDisconnect` | Connection churn > 10/min | Warning | Network investigation |

---

## 5. Integration with claude-flow Observability

### 5.1 Hybrid Observability Architecture

**Problem:** claude-flow requires SONA <0.05ms, erlmcp achieves 1-5ms.

**Solution:** Dual-layer observability:

```
┌─────────────────────────────────────────────────────┐
│           claude-flow (Rust) - SONA Layer           │
│                                                     │
│  ┌───────────────────┐     ┌──────────────────┐   │
│  │ Local Metrics     │     │ Trace Sampling   │   │
│  │ (Prometheus)      │────>│ (1% sample rate) │   │
│  │ - Cache hits      │     │                  │   │
│  │ - Read latency    │     └────────┬─────────┘   │
│  │ (<0.05ms ops)     │              │             │
│  └───────────────────┘              │             │
│                                     │             │
└─────────────────────────────────────┼─────────────┘
                                      │
                            Trace Context Propagation
                                      │
┌─────────────────────────────────────┼─────────────┐
│         erlmcp (Erlang) - MCP Layer │             │
│                                     ▼             │
│  ┌───────────────────┐     ┌──────────────────┐  │
│  │ OTEL Collector    │<────│ Full Traces      │  │
│  │ - Jaeger          │     │ - All MCP ops    │  │
│  │ - Prometheus      │     │ - Subscriptions  │  │
│  │ - Datadog         │     │ - Tool calls     │  │
│  └───────────────────┘     └──────────────────┘  │
└───────────────────────────────────────────────────┘
```

### 5.2 Shared Metrics Export

**Common Prometheus Endpoint:**

```erlang
%% Export erlmcp metrics in Prometheus format
%% Endpoint: http://localhost:9090/metrics
export_prometheus_metrics() ->
    [
        %% MCP metrics
        prometheus_histogram(
            "erlmcp_mcp_resource_list_latency_seconds",
            "Resource list latency",
            [server_id]
        ),
        
        %% Compatibility with claude-flow naming
        prometheus_histogram(
            "mcp_operation_duration_seconds",  % Standard MCP metric name
            "MCP operation duration",
            [operation, server_id]
        )
    ].
```

### 5.3 Trace Context Bridging

**Propagate traces from Rust to Erlang:**

```rust
// claude-flow (Rust) - Send trace context to erlmcp
let trace_ctx = opentelemetry::Context::current();
let mcp_request = McpRequest {
    method: "resources/list",
    params: params,
    trace_context: Some(serialize_trace_context(&trace_ctx)),
};
erlmcp_client.send(mcp_request).await?;
```

```erlang
%% erlmcp (Erlang) - Restore trace context
handle_rpc_request(#{<<"traceContext">> := TraceCtx} = Message, State) ->
    %% Restore parent span from Rust
    ParentSpan = erlmcp_otel:restore_trace_ctx(TraceCtx),
    
    %% Create child span for this operation
    Span = erlmcp_otel:start_span(
        <<"mcp.resources.list">>,
        #{parent => ParentSpan}
    ),
    
    %% Process request...
    erlmcp_otel:end_span(Span).
```

---

## 6. MCP Compliance Tracking Dashboards

### 6.1 Compliance Dashboard Design

**URL:** `http://localhost:9090/mcp-compliance`

**Sections:**

1. **Protocol Compliance**
   - MCP 2025-11-25 specification version
   - Initialize handshake success rate
   - Supported capabilities coverage
   - Method implementation status

2. **Performance Compliance**
   - Latency budgets per operation (P50/P95/P99)
   - Throughput targets (req/s)
   - Memory per connection (<100KB)
   - Connection setup time (<100ms)

3. **Feature Compliance**
   - Resources: Static, templates, subscriptions
   - Tools: Basic, schema validation, deprecation
   - Prompts: Arguments, schema validation
   - Pagination: Cursor support
   - Cancellation: Request cancellation

4. **Real-time Violations**
   - Latency violations (operations exceeding budget)
   - Error rate violations (>5% errors)
   - Schema validation failures
   - Timeout violations

### 6.2 Dashboard Implementation

**WebSocket updates (existing erlmcp_dashboard_server):**

```erlang
%% Send MCP compliance metrics every 1s
broadcast_mcp_compliance() ->
    Metrics = #{
        timestamp => erlang:system_time(millisecond),
        protocol_version => <<"2025-11-25">>,
        compliance => #{
            initialize => #{
                success_rate => calculate_init_success_rate(),
                p95_latency_ms => get_p95_latency(initialize),
                target_latency_ms => 100,
                compliant => true
            },
            resources_list => #{
                p95_latency_ms => get_p95_latency(resources_list),
                target_latency_ms => 50,
                compliant => get_p95_latency(resources_list) < 50
            },
            tools_call => #{
                p95_latency_ms => get_p95_latency(tools_call),
                target_latency_ms => 500,
                error_rate => calculate_error_rate(tools_call),
                compliant => (get_p95_latency(tools_call) < 500) and 
                            (calculate_error_rate(tools_call) < 0.05)
            }
        }
    },
    erlmcp_dashboard_server:broadcast_metrics(Metrics).
```

### 6.3 Compliance Validation Report

**Auto-generated report (weekly):**

```
MCP Compliance Report
=====================
Period: 2026-01-25 to 2026-02-01
MCP Spec: 2025-11-25

Protocol Compliance: ✓ PASS
  - Initialize handshake: 99.8% success rate
  - Capabilities negotiation: 100% compliant
  - Phase transitions: No violations

Performance Compliance: ⚠ WARNING
  - resources/list: P95=4.2ms ✓ (target: <50ms)
  - resources/read: P95=8.1ms ✓ (target: <200ms)
  - tools/call: P95=52ms ⚠ (target: <500ms, 3 violations)
  - Subscription fan-out: P95=18ms ✓ (target: <50ms)

Feature Compliance: ✓ PASS
  - Resources: Static ✓, Templates ✓, Subscriptions ✓
  - Tools: Schema validation ✓, Cancellation ✓
  - Prompts: Arguments ✓, Schema ✓

Recommendations:
  1. Investigate tools/call P95 violations (tool: "complex_analysis")
  2. Consider schema validation caching (75% improvement expected)
  3. Monitor subscription count (approaching fan-out limits)
```

---

## 7. Chaos Engineering Tests

### 7.1 MCP-Specific Chaos Scenarios

**Extend erlmcp_chaos with MCP scenarios:**

```erlang
%% New module: apps/erlmcp_observability/src/erlmcp_mcp_chaos.erl
-module(erlmcp_mcp_chaos).

-export([run_mcp_chaos_suite/0,
         chaos_resource_handler_timeout/1,
         chaos_tool_handler_crash/1,
         chaos_subscription_flood/1,
         chaos_json_corruption/1,
         chaos_transport_jitter/1]).

%% Scenario 1: Resource handler timeout
chaos_resource_handler_timeout(Config) ->
    erlmcp_chaos:run(#{
        experiment => resource_handler_timeout,
        target => resource_handlers,
        duration => 60000,  % 1 minute
        config => #{
            timeout_probability => 0.2,  % 20% of handlers timeout
            timeout_duration => 10000     % 10s timeout
        },
        safety_checks => true,
        sla_threshold => #{
            p95_latency_ms => 100,  % Fail if P95 > 100ms
            error_rate => 0.1        % Fail if error rate > 10%
        }
    }).

%% Scenario 2: Tool handler crash
chaos_tool_handler_crash(Config) ->
    erlmcp_chaos:run(#{
        experiment => kill_random,
        target => tool_handlers,
        rate => 0.1,  % 10% crash rate
        duration => 120000,  % 2 minutes
        auto_rollback => true,
        safety_checks => true,
        expected_outcome => #{
            %% System should recover via supervision
            recovery_time_ms => 1000,
            data_loss => false
        }
    }).

%% Scenario 3: Subscription flood
chaos_subscription_flood(Config) ->
    %% Simulate 1000 subscribers, 10 Hz updates
    erlmcp_chaos:run(#{
        experiment => subscription_flood,
        config => #{
            subscriber_count => 1000,
            update_rate_hz => 10,
            update_duration => 60000  % 1 minute
        },
        expected_outcome => #{
            %% Should handle without latency spike
            p95_notification_latency_ms => 50,
            dropped_notifications => 0
        }
    }).

%% Scenario 4: JSON corruption (transport layer)
chaos_json_corruption(Config) ->
    erlmcp_chaos:run(#{
        experiment => packet_corruption,
        target => json_encoder,
        rate => 0.05,  % 5% corruption rate
        duration => 60000,
        expected_outcome => #{
            %% Should detect and reject corrupted messages
            validation_errors => {gt, 0},
            false_positives => 0
        }
    }).

%% Scenario 5: Transport jitter
chaos_transport_jitter(Config) ->
    erlmcp_chaos:run(#{
        experiment => network_latency,
        target => transports,
        latency => 100,  % Add 100ms jitter
        duration => 120000,
        expected_outcome => #{
            %% Should maintain SLA despite jitter
            p95_e2e_latency_ms => 150,  % 100ms jitter + 50ms processing
            timeout_errors => 0
        }
    }).
```

### 7.2 Chaos Test Matrix

| Scenario | Fault Type | Blast Radius | Expected Behavior | Success Criteria |
|----------|-----------|--------------|-------------------|------------------|
| Resource Handler Timeout | Timeout | 20% handlers | Return timeout error | P95 < 100ms, error rate < 10% |
| Tool Handler Crash | Process crash | 10% tools | Supervisor restart | Recovery < 1s, no data loss |
| Subscription Flood | Load spike | 1000 subscribers | Fan-out without spike | P95 notify < 50ms, 0 drops |
| JSON Corruption | Data corruption | 5% messages | Validation rejection | 100% detection, 0 false positives |
| Transport Jitter | Network latency | 100ms added | Maintain SLA | P95 E2E < 150ms |
| Schema Validation Failure | Invalid input | 10% tool calls | Reject with error | 100% rejection, no crash |
| Memory Leak | Resource leak | Gradual growth | Detect and alert | Alert < 5min, auto-restart |
| Message Queue Buildup | Backpressure | >1000 msgs | Apply backpressure | No OOM, queue drain |

### 7.3 Continuous Chaos (GameDay Automation)

**Scheduled chaos runs:**

```erlang
%% Run chaos suite every Monday at 2 AM UTC
schedule_chaos_gameday() ->
    %% Use erlcron or similar
    erlcron:cron({weekly, monday, {2, 0, 0}}, fun() ->
        Results = erlmcp_mcp_chaos:run_mcp_chaos_suite(),
        Report = generate_chaos_report(Results),
        notify_ops_team(Report),
        store_chaos_results(Results)
    end).
```

---

## 8. Implementation Plan

### Phase 1: Foundation (Week 1-2)

**Tasks:**
1. Create `erlmcp_mcp_metrics.erl` - MCP-specific metrics collector
2. Instrument `erlmcp_server.erl` with OTEL spans
3. Add MCP health checks to `erlmcp_health_monitor.erl`
4. Create MCP compliance dashboard view

**Deliverables:**
- [ ] MCP metrics module (compile + tests)
- [ ] OTEL instrumentation (resources, tools, prompts)
- [ ] Health checks (3 MCP-specific checks)
- [ ] Dashboard view (HTML + WebSocket endpoint)

### Phase 2: Profiling & Benchmarks (Week 3-4)

**Tasks:**
1. Create `erlmcp_mcp_benchmarks.erl` - Automated benchmarks
2. Integrate flame graph generation
3. Add performance regression detection to CI
4. Profile critical paths (tools/call, resources/list)

**Deliverables:**
- [ ] Benchmark suite (5 MCP workloads)
- [ ] Flame graph generation script
- [ ] CI integration (GitHub Actions)
- [ ] Performance baseline (JSON snapshot)

### Phase 3: Chaos Engineering (Week 5-6)

**Tasks:**
1. Create `erlmcp_mcp_chaos.erl` - MCP chaos scenarios
2. Implement 8 chaos test scenarios
3. Add chaos report generation
4. Schedule automated GameDay runs

**Deliverables:**
- [ ] Chaos module (8 scenarios)
- [ ] Chaos test suite (EUnit tests)
- [ ] GameDay automation (erlcron)
- [ ] Chaos reports (weekly)

### Phase 4: Integration & Documentation (Week 7-8)

**Tasks:**
1. claude-flow observability integration
2. Prometheus exporter for shared metrics
3. Trace context bridging (Rust ↔ Erlang)
4. Comprehensive documentation

**Deliverables:**
- [ ] claude-flow integration guide
- [ ] Prometheus endpoint
- [ ] Trace context library
- [ ] Observability runbook

---

## 9. Success Metrics

| Metric | Current | Target | Method |
|--------|---------|--------|--------|
| MCP Operation Coverage | 60% | 100% | OTEL span count |
| Metric Cardinality | 20 | 50+ | Prometheus labels |
| Profiling Overhead | N/A | <5% | CPU benchmarks |
| Chaos Test Coverage | 0 | 8 scenarios | Test suite |
| Alert Response Time | Manual | <5 min | Auto-alerts |
| Compliance Violations | Unknown | 0 | Weekly report |
| Flame Graph Generation | Manual | Auto | CI integration |
| Baseline Regression Detection | Manual | Auto | CI gates |

---

## 10. References

**Existing Modules:**
- `erlmcp_otel.erl` - OpenTelemetry integration
- `erlmcp_metrics.erl` - Metrics collector
- `erlmcp_profiler.erl` - CPU/memory profiling
- `erlmcp_health_monitor.erl` - Health checks
- `erlmcp_chaos.erl` - Chaos engineering
- `erlmcp_dashboard_server.erl` - WebSocket dashboard

**External Dependencies:**
- OpenTelemetry Erlang SDK (1.7.0+)
- Prometheus Erlang Client (4.10.0+)
- FlameGraph (Brendan Gregg's scripts)

**MCP Specification:**
- Version: 2025-11-25
- Performance targets: P50<5ms, P95<20ms, P99<50ms
- Compliance: 100% protocol implementation

---

**Next Steps:**
1. Review and approve this design
2. Spawn agent swarm (erlang-architect, erlang-performance, erlang-otp-developer, etc.)
3. Create detailed implementation tickets
4. Begin Phase 1 implementation

---

**END OF DESIGN DOCUMENT**
