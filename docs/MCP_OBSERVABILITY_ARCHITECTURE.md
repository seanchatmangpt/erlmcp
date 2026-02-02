# MCP Observability Architecture Design
**Version:** 1.0.0
**Date:** 2026-02-02
**Author:** Erlang Architect
**Status:** Design Specification

---

## Executive Summary

This document specifies the **architecture** for comprehensive MCP protocol compliance observability. It defines supervision strategies, behavior selections, module decomposition, and API boundaries without implementation details.

**Key Architectural Decisions:**
1. **Supervision Strategy**: `one_for_one` - isolated failures, no cascading restarts
2. **Behavior Choices**: `gen_server` for stateful collectors, library modules for instrumentation
3. **Module Decomposition**: 4 new modules + instrumentation in existing modules
4. **Performance Impact**: <5% latency overhead, <2% CPU overhead
5. **Integration**: Extends existing erlmcp_observability_sup without breaking changes

---

## 1. Supervision Tree Design

### 1.1 Extended Observability Supervisor

**Current State** (erlmcp_observability_sup):
```
erlmcp_observability_sup (one_for_one)
├── erlmcp_event_manager
├── erlmcp_metrics
├── erlmcp_metrics_server
├── erlmcp_metrics_aggregator
├── erlmcp_dashboard_server
├── erlmcp_health_monitor
├── erlmcp_recovery_manager
├── erlmcp_chaos
├── erlmcp_chaos_worker_sup
├── erlmcp_process_monitor
└── erlmcp_audit_log
```

**Extended Design** (ADD 4 new workers):
```erlang
erlmcp_observability_sup (one_for_one)
├── [Existing 11 workers above]
├── erlmcp_mcp_metrics          [NEW] gen_server - MCP-specific metrics
├── erlmcp_mcp_logger           [NEW] gen_server - Structured logging
├── erlmcp_mcp_alerting         [NEW] gen_server - SLO alerts
└── erlmcp_mcp_compliance       [NEW] gen_server - Compliance tracking
```

### 1.2 Child Specifications

#### erlmcp_mcp_metrics (gen_server)
```erlang
#{id => erlmcp_mcp_metrics,
  start => {erlmcp_mcp_metrics, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_mcp_metrics]}
```

**Rationale:**
- **Behavior**: `gen_server` - stateful, stores metric buffers, supports sync/async recording
- **Restart**: `permanent` - critical for observability, must always be available
- **Shutdown**: 5000ms - flush pending metrics before termination
- **Dependencies**: None (isolated, uses ETS for storage)

#### erlmcp_mcp_logger (gen_server)
```erlang
#{id => erlmcp_mcp_logger,
  start => {erlmcp_mcp_logger, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_mcp_logger]}
```

**Rationale:**
- **Behavior**: `gen_server` - batches log events, formats structured logs
- **Restart**: `permanent` - protocol events must be logged for compliance
- **Shutdown**: 5000ms - flush log buffer to disk/remote
- **Dependencies**: None (uses standard logger backends)

#### erlmcp_mcp_alerting (gen_server)
```erlang
#{id => erlmcp_mcp_alerting,
  start => {erlmcp_mcp_alerting, start_link, []},
  restart => permanent,
  shutdown => 2000,
  type => worker,
  modules => [erlmcp_mcp_alerting]}
```

**Rationale:**
- **Behavior**: `gen_server` - evaluates SLO thresholds, sends alerts
- **Restart**: `permanent` - critical for operational awareness
- **Shutdown**: 2000ms - fast shutdown, alerts are best-effort
- **Dependencies**: erlmcp_mcp_metrics (reads metrics for evaluation)

#### erlmcp_mcp_compliance (gen_server)
```erlang
#{id => erlmcp_mcp_compliance,
  start => {erlmcp_mcp_compliance, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_mcp_compliance]}
```

**Rationale:**
- **Behavior**: `gen_server` - aggregates compliance data, generates reports
- **Restart**: `permanent` - compliance tracking is mandatory
- **Shutdown**: 5000ms - finalize current compliance window
- **Dependencies**: erlmcp_mcp_metrics, erlmcp_mcp_logger (reads data)

### 1.3 Library Modules (No Supervision)

These modules provide instrumentation APIs with no process:
- `erlmcp_mcp_instrumentation` - Instrumentation macros and helpers
- `erlmcp_mcp_sampling` - Sampling decision logic
- `erlmcp_mcp_taxonomy` - Metric naming conventions

---

## 2. Module Architecture

### 2.1 Core Modules

#### erlmcp_mcp_metrics (gen_server)
**Purpose:** MCP-specific metrics collection with feature-level granularity

**State:**
```erlang
-record(state, {
    metric_buffer :: ets:tid(),           % ETS table for metrics
    histogram_buckets :: map(),           % Percentile calculation state
    sampling_config :: map(),             % Per-feature sampling rates
    last_flush :: integer(),              % Last flush timestamp
    flush_interval :: pos_integer()       % Configurable flush interval
}).
```

**API:**
```erlang
-spec record_resource_list(server_id(), latency_us()) -> ok.
-spec record_resource_read(server_id(), uri(), latency_us(), cache_hit()) -> ok.
-spec record_tool_call(server_id(), tool_name(), validation_us(), execution_us()) -> ok.
-spec record_subscription_update(server_id(), uri(), subscriber_count()) -> ok.
-spec get_metrics_snapshot() -> metrics_snapshot().
-spec get_feature_metrics(feature_name()) -> feature_metrics().
```

**Behavior:**
- Async recording via `gen_server:cast` (non-blocking)
- Periodic flush to prometheus/OTEL exporters (configurable interval)
- ETS-based storage survives process restarts
- Histogram approximation using t-digest algorithm

#### erlmcp_mcp_logger (gen_server)
**Purpose:** Structured logging for MCP protocol events

**State:**
```erlang
-record(state, {
    log_buffer :: queue:queue(),         % Batched log events
    buffer_size :: non_neg_integer(),    % Current buffer size
    max_buffer_size :: pos_integer(),    % Flush threshold
    log_level :: logger:level(),         % Minimum log level
    backends :: [logger:handler_id()],   % Configured log backends
    structured_schema :: map()           % JSON schema for logs
}).
```

**API:**
```erlang
-spec log_protocol_event(event_type(), event_data()) -> ok.
-spec log_decision(component(), decision_type(), decision_data()) -> ok.
-spec log_error(error_type(), error_data(), stacktrace()) -> ok.
-spec get_event_history(time_range()) -> [log_event()].
-spec set_log_level(logger:level()) -> ok.
```

**Logging Schema:**
```erlang
#{
    timestamp => integer(),               % Nanosecond precision
    event_type => binary(),               % "protocol_event" | "decision" | "error"
    component => binary(),                % "server" | "client" | "transport"
    operation => binary(),                % "initialize" | "tools/call" | etc.
    trace_id => binary(),                 % OTEL trace ID
    span_id => binary(),                  % OTEL span ID
    server_id => atom(),                  % Server identifier
    metadata => map(),                    % Operation-specific data
    severity => atom()                    % debug | info | warning | error | critical
}
```

#### erlmcp_mcp_alerting (gen_server)
**Purpose:** SLO violation detection and alert generation

**State:**
```erlang
-record(state, {
    slo_definitions :: map(),            % SLO thresholds by feature
    violation_windows :: map(),          % Sliding windows for evaluation
    alert_cooldowns :: map(),            % Prevent alert storms
    alert_backends :: [alert_backend()], % PagerDuty, Slack, email, etc.
    evaluation_interval :: pos_integer() % How often to check SLOs
}).
```

**API:**
```erlang
-spec define_slo(slo_name(), slo_spec()) -> ok.
-spec check_slo_compliance() -> compliance_report().
-spec get_active_violations() -> [violation()].
-spec ack_alert(alert_id()) -> ok.
-spec configure_alert_backend(backend_type(), backend_config()) -> ok.
```

**Alert Definitions:**
```erlang
-record(slo, {
    name :: binary(),
    metric :: metric_name(),
    threshold :: float(),
    duration :: pos_integer(),           % Violation duration before alert
    severity :: warning | critical,
    cooldown :: pos_integer(),           % Minimum time between alerts
    labels :: map()                      % Additional context
}).
```

#### erlmcp_mcp_compliance (gen_server)
**Purpose:** MCP 2025-11-25 specification compliance tracking

**State:**
```erlang
-record(state, {
    spec_version :: binary(),            % MCP spec version
    compliance_checks :: map(),          % Feature -> compliance status
    violations :: [violation()],         % Active compliance violations
    report_schedule :: cron:schedule(),  % When to generate reports
    last_report :: integer(),            % Last report timestamp
    report_storage :: binary()           % Path for report persistence
}).
```

**API:**
```erlang
-spec check_compliance(feature_name()) -> compliance_status().
-spec get_compliance_report() -> compliance_report().
-spec get_compliance_score() -> float().  % 0.0 - 1.0
-spec schedule_compliance_report(schedule()) -> ok.
-spec get_violation_history(time_range()) -> [violation()].
```

---

## 3. Metrics Taxonomy

### 3.1 Metric Naming Convention

**Format:** `erlmcp.mcp.<category>.<operation>.<metric_type>`

**Categories:**
- `resources` - Resource operations
- `tools` - Tool operations
- `prompts` - Prompt operations
- `session` - Session lifecycle
- `transport` - Transport layer
- `jsonrpc` - JSON-RPC encoding/decoding

### 3.2 Metric Definitions

#### Resource Metrics (8 metrics)

| Metric Name | Type | Labels | Unit | Purpose |
|-------------|------|--------|------|---------|
| `erlmcp.mcp.resources.list.latency_us` | Histogram | `server_id` | microseconds | P50/P95/P99 for resources/list |
| `erlmcp.mcp.resources.list.count` | Counter | `server_id` | requests | Total list operations |
| `erlmcp.mcp.resources.read.latency_us` | Histogram | `server_id`, `uri`, `cache_hit` | microseconds | Read latency by resource |
| `erlmcp.mcp.resources.read.count` | Counter | `server_id`, `uri` | requests | Reads per resource |
| `erlmcp.mcp.resources.subscribe.active` | Gauge | `server_id`, `uri` | subscriptions | Active subscriptions |
| `erlmcp.mcp.resources.notify.latency_us` | Histogram | `server_id`, `uri` | microseconds | Notification delivery time |
| `erlmcp.mcp.resources.notify.fan_out` | Histogram | `server_id`, `uri` | subscribers | Subscribers per notification |
| `erlmcp.mcp.resources.template.match_time_us` | Histogram | `server_id`, `pattern` | microseconds | URI template matching |

#### Tool Metrics (8 metrics)

| Metric Name | Type | Labels | Unit | Purpose |
|-------------|------|--------|------|---------|
| `erlmcp.mcp.tools.list.latency_us` | Histogram | `server_id` | microseconds | tools/list latency |
| `erlmcp.mcp.tools.call.latency_us` | Histogram | `server_id`, `tool_name` | microseconds | Total tool call latency |
| `erlmcp.mcp.tools.call.validation_time_us` | Histogram | `server_id`, `tool_name` | microseconds | Schema validation time |
| `erlmcp.mcp.tools.call.execution_time_us` | Histogram | `server_id`, `tool_name` | microseconds | Handler execution time |
| `erlmcp.mcp.tools.call.count` | Counter | `server_id`, `tool_name` | requests | Tool invocations |
| `erlmcp.mcp.tools.call.errors` | Counter | `server_id`, `tool_name`, `error_type` | errors | Tool failures |
| `erlmcp.mcp.tools.schema_cache.hits` | Counter | `server_id` | hits | Schema cache hits |
| `erlmcp.mcp.tools.schema_cache.misses` | Counter | `server_id` | misses | Schema cache misses |

#### Prompt Metrics (3 metrics)

| Metric Name | Type | Labels | Unit | Purpose |
|-------------|------|--------|------|---------|
| `erlmcp.mcp.prompts.list.latency_us` | Histogram | `server_id` | microseconds | prompts/list latency |
| `erlmcp.mcp.prompts.get.latency_us` | Histogram | `server_id`, `prompt_name` | microseconds | prompts/get latency |
| `erlmcp.mcp.prompts.get.count` | Counter | `server_id`, `prompt_name` | requests | Prompt retrievals |

#### Session Metrics (4 metrics)

| Metric Name | Type | Labels | Unit | Purpose |
|-------------|------|--------|------|---------|
| `erlmcp.mcp.session.active` | Gauge | `server_id` | sessions | Active MCP sessions |
| `erlmcp.mcp.session.duration_s` | Histogram | `server_id`, `transport_type` | seconds | Session lifetime |
| `erlmcp.mcp.session.init_latency_us` | Histogram | `server_id` | microseconds | Initialize handshake time |
| `erlmcp.mcp.session.phase` | Gauge | `server_id`, `phase` | sessions | Sessions per phase |

#### Transport Metrics (5 metrics)

| Metric Name | Type | Labels | Unit | Purpose |
|-------------|------|--------|------|---------|
| `erlmcp.mcp.transport.send.latency_us` | Histogram | `server_id`, `transport_type` | microseconds | Transport send time |
| `erlmcp.mcp.transport.receive.latency_us` | Histogram | `server_id`, `transport_type` | microseconds | Transport receive time |
| `erlmcp.mcp.transport.message_size_bytes` | Histogram | `server_id`, `transport_type`, `direction` | bytes | Message size distribution |
| `erlmcp.mcp.jsonrpc.encode_latency_us` | Histogram | `server_id`, `method` | microseconds | JSON encoding time |
| `erlmcp.mcp.jsonrpc.decode_latency_us` | Histogram | `server_id`, `method` | microseconds | JSON decoding time |

**Total:** 28 MCP-specific metrics

### 3.3 Derived Metrics (Calculated by Aggregator)

| Metric Name | Calculation | Purpose |
|-------------|-------------|---------|
| `erlmcp.mcp.tools.call.success_rate` | `(total - errors) / total` | Tool reliability |
| `erlmcp.mcp.resources.cache_hit_ratio` | `cache_hits / total_reads` | Cache effectiveness |
| `erlmcp.mcp.session.churn_rate` | `new_sessions / time_window` | Connection stability |
| `erlmcp.mcp.transport.throughput_ops` | `operations / time_window` | Network load |

---

## 4. Distributed Tracing Architecture

### 4.1 Span Taxonomy

**Naming Convention:** `mcp.<category>.<operation>`

**Span Hierarchy:**
```
mcp.initialize (root)
├── mcp.transport.receive
├── mcp.jsonrpc.decode
├── mcp.session.validate
└── mcp.transport.send

mcp.tools.call (root)
├── mcp.tools.validate_schema (child)
├── mcp.tools.execute_handler (child)
│   ├── [user-defined spans] (grandchild)
│   └── ...
└── mcp.transport.send (sibling)
```

### 4.2 Span Attributes

**Standard Attributes (all spans):**
```erlang
#{
    <<"service.name">> => <<"erlmcp">>,
    <<"service.version">> => <<"2.1.0">>,
    <<"mcp.version">> => <<"2025-11-25">>,
    <<"server_id">> => ServerIdBinary,
    <<"span.kind">> => <<"server">> | <<"client">> | <<"internal">>,
    <<"erlang.node">> => node(),
    <<"erlang.pid">> => pid_to_list(self())
}
```

**Operation-Specific Attributes:**

**tools/call:**
```erlang
#{
    <<"tool.name">> => ToolName,
    <<"tool.args_count">> => length(maps:keys(Args)),
    <<"tool.validation_time_us">> => ValidationTime,
    <<"tool.execution_time_us">> => ExecutionTime,
    <<"tool.schema_validated">> => true | false,
    <<"tool.error">> => ErrorType | undefined
}
```

**resources/read:**
```erlang
#{
    <<"resource.uri">> => Uri,
    <<"resource.cache_hit">> => true | false,
    <<"resource.size_bytes">> => ContentSize,
    <<"resource.mime_type">> => MimeType
}
```

**initialize:**
```erlang
#{
    <<"protocol.version">> => ProtocolVersion,
    <<"client.name">> => ClientName,
    <<"client.version">> => ClientVersion,
    <<"server.capabilities">> => CapabilitiesJson
}
```

### 4.3 Context Propagation

**W3C Trace Context Headers:**
```erlang
%% Inject context into JSON-RPC message
#{
    <<"jsonrpc">> => <<"2.0">>,
    <<"method">> => Method,
    <<"params">> => Params,
    <<"_trace">> => #{
        <<"traceparent">> => <<"00-<trace_id>-<span_id>-01">>,
        <<"tracestate">> => <<"erlmcp=<correlation_id>>>>,
        <<"baggage">> => <<"user_id=<user_id>,session_id=<session_id>">>
    }
}
```

**Restore context on receive:**
```erlang
case maps:get(<<"_trace">>, Message, undefined) of
    undefined ->
        %% Start new trace
        erlmcp_otel:start_span(Name, Attributes);
    TraceCtx ->
        %% Continue existing trace
        ParentCtx = erlmcp_otel:restore_context(TraceCtx),
        erlmcp_otel:start_span(Name, Attributes, ParentCtx)
end
```

### 4.4 Sampling Strategy

**Head-Based Sampling:**
- **Always sample:** `initialize`, `tools/call` (critical path)
- **Sample 10%:** `resources/list`, `prompts/list` (high volume)
- **Sample 1%:** `transport.send`, `jsonrpc.encode` (very high volume)

**Tail-Based Sampling (conditional):**
- Sample 100% if latency > P95 threshold
- Sample 100% if error occurred
- Sample 100% if SLO violation

**Configuration:**
```erlang
#{
    sampling => #{
        default_rate => 0.1,
        per_operation => #{
            <<"mcp.initialize">> => 1.0,
            <<"mcp.tools.call">> => 1.0,
            <<"mcp.resources.list">> => 0.1,
            <<"mcp.transport.send">> => 0.01
        },
        tail_sampling => #{
            enabled => true,
            latency_threshold_us => 100000,  % P95
            error_rate => 1.0
        }
    }
}
```

---

## 5. Logging Schema

### 5.1 Log Event Structure

**Standard Fields (all events):**
```erlang
#{
    <<"timestamp">> => erlang:system_time(nanosecond),
    <<"level">> => debug | info | warning | error | critical,
    <<"event_type">> => protocol_event | decision | error | performance,
    <<"component">> => server | client | transport | registry,
    <<"trace_id">> => TraceId,
    <<"span_id">> => SpanId,
    <<"server_id">> => ServerId,
    <<"node">> => node(),
    <<"pid">> => pid_to_list(self())
}
```

### 5.2 Event Types

#### Protocol Events
```erlang
#{
    <<"event_type">> => <<"protocol_event">>,
    <<"operation">> => <<"initialize">> | <<"tools/call">> | ...,
    <<"phase">> => <<"start">> | <<"complete">> | <<"error">>,
    <<"request_id">> => RequestId,
    <<"duration_us">> => DurationUs,
    <<"metadata">> => #{
        % Operation-specific fields
    }
}
```

**Example - Initialize Event:**
```erlang
#{
    <<"timestamp">> => 1738502400000000000,
    <<"level">> => info,
    <<"event_type">> => <<"protocol_event">>,
    <<"component">> => <<"server">>,
    <<"operation">> => <<"initialize">>,
    <<"phase">> => <<"complete">>,
    <<"server_id">> => <<"server_1">>,
    <<"trace_id">> => <<"abc123...">>,
    <<"span_id">> => <<"def456...">>,
    <<"metadata">> => #{
        <<"protocol_version">> => <<"2025-11-25">>,
        <<"client_name">> => <<"claude-desktop">>,
        <<"capabilities">> => #{...}
    }
}
```

#### Decision Events
```erlang
#{
    <<"event_type">> => <<"decision">>,
    <<"decision_type">> => <<"sampling">> | <<"routing">> | <<"caching">>,
    <<"decision">> => Decision,
    <<"rationale">> => Rationale,
    <<"metadata">> => #{...}
}
```

**Example - Sampling Decision:**
```erlang
#{
    <<"event_type">> => <<"decision">>,
    <<"decision_type">> => <<"sampling">>,
    <<"operation">> => <<"tools/call">>,
    <<"decision">> => <<"sample">>,
    <<"rationale">> => <<"critical_path">>,
    <<"sampling_rate">> => 1.0
}
```

#### Error Events
```erlang
#{
    <<"event_type">> => <<"error">>,
    <<"error_class">> => throw | error | exit,
    <<"error_type">> => ErrorType,
    <<"error_message">> => ErrorMessage,
    <<"stacktrace">> => Stacktrace,
    <<"operation">> => Operation,
    <<"metadata">> => #{...}
}
```

### 5.3 Log Retention Policy

| Level | Retention | Storage |
|-------|-----------|---------|
| debug | 1 day | Local disk |
| info | 7 days | Local disk + S3 |
| warning | 30 days | S3 |
| error | 90 days | S3 + compliance archive |
| critical | 1 year | S3 + compliance archive |

---

## 6. Health Checks Architecture

### 6.1 Component Health Checks

**Check Frequency:**
- Critical components: 10s interval
- Normal components: 30s interval
- Background components: 60s interval

**Health Check Functions:**

#### Transport Health
```erlang
check_transport_health(TransportId) ->
    %% Check 1: Process alive
    case erlmcp_registry:lookup_transport(TransportId) of
        undefined -> unhealthy;
        Pid when is_pid(Pid) ->
            %% Check 2: Message queue size
            {message_queue_len, QueueLen} = process_info(Pid, message_queue_len),
            if QueueLen > 1000 -> degraded;
               QueueLen > 100 -> warning;
               true -> healthy
            end
    end.
```

#### Session Storage Health
```erlang
check_session_storage_health() ->
    Backend = application:get_env(erlmcp_core, session_backend, ets),
    case Backend of
        ets ->
            %% Check ETS table size
            Size = ets:info(erlmcp_sessions, size),
            if Size > 10000 -> warning;
               Size > 50000 -> degraded;
               true -> healthy
            end;
        mnesia ->
            %% Check Mnesia status
            case mnesia:system_info(is_running) of
                yes -> healthy;
                _ -> unhealthy
            end;
        dets ->
            %% Check DETS file integrity
            healthy  % TODO: Implement DETS health check
    end.
```

#### Server Health
```erlang
check_server_health(ServerId) ->
    %% Check 1: Response time
    StartTime = erlang:monotonic_time(microsecond),
    case erlmcp_server:list_resources(ServerId) of
        {ok, _Resources} ->
            Latency = erlang:monotonic_time(microsecond) - StartTime,
            if Latency < 5000 -> healthy;    % <5ms
               Latency < 20000 -> degraded;  % 5-20ms
               true -> unhealthy             % >20ms
            end;
        {error, _} ->
            unhealthy
    end.
```

### 6.2 Health Check Registration

**API:**
```erlang
%% Register MCP component health checks
erlmcp_health_monitor:register_component(
    {mcp_resources, ServerId},
    ServerPid,
    fun() -> check_resource_list_latency(ServerId) end
),

erlmcp_health_monitor:register_component(
    {mcp_tools, ServerId},
    ServerPid,
    fun() -> check_tool_availability(ServerId) end
),

erlmcp_health_monitor:register_component(
    {mcp_subscriptions, ServerId},
    ServerPid,
    fun() -> check_subscription_latency(ServerId) end
).
```

### 6.3 Aggregate Health Score

**Algorithm:**
```erlang
calculate_system_health() ->
    ComponentHealth = erlmcp_health_monitor:get_all_component_health(),

    %% Weight by criticality
    Weights = #{
        transport => 0.3,
        session_storage => 0.2,
        resources => 0.2,
        tools => 0.2,
        subscriptions => 0.1
    },

    Score = maps:fold(fun(Component, Health, Acc) ->
        Weight = maps:get(Component, Weights, 0.1),
        HealthScore = health_to_score(Health),
        Acc + (Weight * HealthScore)
    end, 0.0, ComponentHealth),

    score_to_health(Score).

health_to_score(healthy) -> 1.0;
health_to_score(degraded) -> 0.5;
health_to_score(unhealthy) -> 0.0.

score_to_health(Score) when Score >= 0.8 -> healthy;
score_to_health(Score) when Score >= 0.5 -> degraded;
score_to_health(_) -> unhealthy.
```

---

## 7. Alerting Architecture

### 7.1 SLO Definitions

**Service Level Objectives (SLOs):**

| Feature | SLO | Measurement Window | Threshold |
|---------|-----|-------------------|-----------|
| `resources/list` | P95 < 50ms | 5 min | 95% |
| `resources/read` | P95 < 200ms | 5 min | 95% |
| `tools/call` | P95 < 500ms | 5 min | 90% |
| `initialize` | P95 < 100ms | 5 min | 99% |
| `subscription.notify` | P95 < 50ms | 5 min | 95% |
| `error_rate` | < 5% | 5 min | 95% |

### 7.2 Alert Severity Levels

**Severity Mapping:**

| Severity | Condition | Response Time | Escalation |
|----------|-----------|---------------|------------|
| Info | Minor deviation (<10% over SLO) | None | Log only |
| Warning | Moderate deviation (10-20% over SLO) | 30 min | Notify on-call |
| Critical | Severe deviation (>20% over SLO) | 5 min | Page on-call + escalate |
| Emergency | Complete failure | Immediate | Page all + incident |

### 7.3 Alert Definitions

```erlang
%% Resource list latency alert
#{
    name => <<"mcp_resource_list_latency_high">>,
    metric => <<"erlmcp.mcp.resources.list.latency_us">>,
    threshold => 50000,  % 50ms
    duration => 300000,  % 5 min
    severity => warning,
    cooldown => 600000,  % 10 min
    message => <<"MCP resources/list P95 latency exceeds 50ms">>
}

%% Tool call error rate alert
#{
    name => <<"mcp_tool_call_error_rate_high">>,
    metric => <<"erlmcp.mcp.tools.call.errors">>,
    threshold => 0.05,   % 5%
    duration => 300000,  % 5 min
    severity => critical,
    cooldown => 300000,  % 5 min
    message => <<"MCP tools/call error rate exceeds 5%">>
}

%% Session initialization failure alert
#{
    name => <<"mcp_session_init_failure">>,
    metric => <<"erlmcp.mcp.session.init.errors">>,
    threshold => 0.10,   % 10%
    duration => 120000,  % 2 min
    severity => critical,
    cooldown => 300000,  % 5 min
    message => <<"MCP initialize failure rate exceeds 10%">>
}
```

### 7.4 Alert Backends

**Supported Backends:**
- **PagerDuty** - For critical/emergency alerts
- **Slack** - For warning/info alerts
- **Email** - For daily summaries
- **Webhook** - For custom integrations

**Configuration:**
```erlang
#{
    backends => [
        {pagerduty, #{
            api_key => <<"...">>,
            service_id => <<"...">>,
            severity => [critical, emergency]
        }},
        {slack, #{
            webhook_url => <<"...">>,
            channel => <<"#mcp-alerts">>,
            severity => [warning, critical]
        }},
        {email, #{
            smtp_server => <<"smtp.example.com">>,
            recipients => [<<"ops@example.com">>],
            severity => [info, warning]
        }}
    ]
}
```

---

## 8. Dashboard Architecture

### 8.1 Dashboard Components

**Real-Time Dashboard:** `http://localhost:9090/mcp-compliance`

**Sections:**

#### 1. Protocol Compliance Panel
- MCP spec version: 2025-11-25
- Initialize handshake success rate
- Capabilities coverage percentage
- Phase transition violations
- Method implementation status

#### 2. Performance Compliance Panel
- Latency budgets per operation (P50/P95/P99)
- Throughput targets (req/s)
- Memory per connection (<100KB)
- Connection setup time (<100ms)
- Real-time latency graph (last 1 hour)

#### 3. Feature Compliance Panel
- Resources: Static ✓/✗, Templates ✓/✗, Subscriptions ✓/✗
- Tools: Basic ✓/✗, Schema validation ✓/✗, Deprecation ✓/✗
- Prompts: Arguments ✓/✗, Schema ✓/✗
- Pagination: Cursor support ✓/✗
- Cancellation: Request cancellation ✓/✗

#### 4. Real-time Violations Panel
- Active latency violations (operations exceeding budget)
- Active error rate violations (>5% errors)
- Active schema validation failures
- Active timeout violations
- Violation timeline (last 24 hours)

### 8.2 Dashboard Data Flow

**WebSocket Update Protocol:**
```erlang
%% Server -> Client (every 1s)
#{
    <<"type">> => <<"metrics_update">>,
    <<"timestamp">> => Timestamp,
    <<"data">> => #{
        <<"compliance">> => ComplianceData,
        <<"performance">> => PerformanceData,
        <<"features">> => FeatureData,
        <<"violations">> => ViolationData
    }
}
```

**Dashboard Server (gen_server):**
```erlang
-record(dashboard_state, {
    ws_connections :: [pid()],           % Connected WebSocket clients
    metric_snapshot :: map(),            % Latest metrics
    compliance_status :: map(),          % Latest compliance data
    update_interval :: pos_integer(),    % Broadcast interval (1000ms)
    history_buffer :: queue:queue()      % Last N snapshots
}).
```

### 8.3 Dashboard Mockup (Text-Based)

```
╔════════════════════════════════════════════════════════════════════════╗
║                   MCP Protocol Compliance Dashboard                    ║
║                         Spec Version: 2025-11-25                       ║
╠════════════════════════════════════════════════════════════════════════╣
║ Protocol Compliance                                    Status: ✓ PASS  ║
╟────────────────────────────────────────────────────────────────────────╢
║ Initialize Handshake:      99.8% success rate                 ✓       ║
║ Capabilities Negotiation:  100% compliant                     ✓       ║
║ Phase Transitions:         0 violations                       ✓       ║
║ Method Implementation:     18/18 methods                      ✓       ║
╠════════════════════════════════════════════════════════════════════════╣
║ Performance Compliance                                 Status: ⚠ WARN  ║
╟────────────────────────────────────────────────────────────────────────╢
║ resources/list:    P50=2.1ms  P95=4.2ms  P99=8.5ms     ✓ (<50ms)      ║
║ resources/read:    P50=3.5ms  P95=8.1ms  P99=15.2ms    ✓ (<200ms)     ║
║ tools/call:        P50=12ms   P95=52ms   P99=120ms     ⚠ (>50ms)      ║
║ initialize:        P50=15ms   P95=45ms   P99=95ms      ✓ (<100ms)     ║
║ subscription:      P50=8ms    P95=18ms   P99=35ms      ✓ (<50ms)      ║
╠════════════════════════════════════════════════════════════════════════╣
║ Feature Compliance                                     Status: ✓ PASS  ║
╟────────────────────────────────────────────────────────────────────────╢
║ Resources:      Static ✓  Templates ✓  Subscriptions ✓                ║
║ Tools:          Basic ✓   Schema ✓     Cancellation ✓                 ║
║ Prompts:        Arguments ✓  Schema ✓                                 ║
║ Pagination:     Cursor support ✓                                      ║
╠════════════════════════════════════════════════════════════════════════╣
║ Active Violations (Last 24h)                           Count: 3        ║
╟────────────────────────────────────────────────────────────────────────╢
║ [14:23] tools/call P95 > 500ms (tool: complex_analysis)      ⚠        ║
║ [12:45] tools/call P95 > 500ms (tool: complex_analysis)      ⚠        ║
║ [09:12] tools/call P95 > 500ms (tool: complex_analysis)      ⚠        ║
╠════════════════════════════════════════════════════════════════════════╣
║ Recommendations                                                        ║
╟────────────────────────────────────────────────────────────────────────╢
║ 1. Investigate tools/call P95 violations (tool: complex_analysis)     ║
║ 2. Consider schema validation caching (75% improvement expected)      ║
║ 3. Monitor subscription count (approaching fan-out limits)            ║
╚════════════════════════════════════════════════════════════════════════╝
```

---

## 9. OTEL Integration Architecture

### 9.1 Integration Points

**Existing OTEL Infrastructure:**
- `erlmcp_otel.erl` - Core OTEL library
- `erlmcp_otel_datadog.erl` - Datadog exporter
- `erlmcp_otel_honeycomb.erl` - Honeycomb exporter
- `erlmcp_otel_jaeger.erl` - Jaeger exporter

**New Integration Points:**

#### 9.1.1 Instrumentation in erlmcp_server
```erlang
%% In erlmcp_server:handle_call({call_tool, ...})
handle_call({call_tool, Name, Args}, From, State) ->
    %% Start OTEL span
    SpanCtx = erlmcp_otel:start_span(<<"mcp.tools.call">>, #{
        <<"tool.name">> => Name,
        <<"server_id">> => State#state.server_id
    }),

    try
        %% Validation span
        ValidationStart = erlang:monotonic_time(microsecond),
        ok = validate_tool_schema(Name, Args, State),
        ValidationDuration = erlang:monotonic_time(microsecond) - ValidationStart,

        %% Execution span
        ExecutionStart = erlang:monotonic_time(microsecond),
        Result = execute_tool_handler(Name, Args, State),
        ExecutionDuration = erlang:monotonic_time(microsecond) - ExecutionStart,

        %% Record metrics
        erlmcp_mcp_metrics:record_tool_call(
            State#state.server_id,
            Name,
            ValidationDuration,
            ExecutionDuration
        ),

        %% End span
        erlmcp_otel:add_attributes(SpanCtx, #{
            <<"validation_time_us">> => ValidationDuration,
            <<"execution_time_us">> => ExecutionDuration,
            <<"success">> => true
        }),
        erlmcp_otel:end_span(SpanCtx),

        {reply, Result, State}
    catch
        Class:Reason:Stacktrace ->
            %% Record error
            erlmcp_otel:record_error(SpanCtx, {Class, Reason, Stacktrace}),
            erlmcp_otel:end_span(SpanCtx),

            %% Log error
            erlmcp_mcp_logger:log_error(tool_call_error, #{
                tool_name => Name,
                error => Reason
            }, Stacktrace),

            {reply, {error, Reason}, State}
    end.
```

#### 9.1.2 Instrumentation in erlmcp_client
```erlang
%% In erlmcp_client:call_tool/3
call_tool(Client, ToolName, Args) ->
    %% Start client span
    SpanCtx = erlmcp_otel:start_span(<<"mcp.tools.call.client">>, #{
        <<"tool.name">> => ToolName,
        <<"span.kind">> => <<"client">>
    }),

    RequestId = generate_request_id(),
    Request = #{
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => ToolName,
            <<"arguments">> => Args
        },
        <<"id">> => RequestId,
        <<"_trace">> => erlmcp_otel:propagate_context(SpanCtx)
    },

    %% Send and await response
    case send_request(Client, Request) of
        {ok, Response} ->
            erlmcp_otel:end_span(SpanCtx),
            {ok, Response};
        {error, Reason} ->
            erlmcp_otel:record_error(SpanCtx, {error, Reason, []}),
            erlmcp_otel:end_span(SpanCtx),
            {error, Reason}
    end.
```

### 9.2 Exporter Configuration

**Multi-Exporter Support:**
```erlang
#{
    exporters => [
        {jaeger, #{
            endpoint => "http://localhost:14250",
            service_name => <<"erlmcp">>
        }},
        {datadog, #{
            agent_host => "localhost",
            agent_port => 8126,
            service => <<"erlmcp">>
        }},
        {otlp, #{
            endpoint => "http://localhost:4318",
            protocol => http_protobuf
        }}
    ]
}
```

### 9.3 Cost Optimization

**Sampling Configuration:**
```erlang
#{
    sampling => #{
        default_rate => 0.1,  % 10% for most operations
        per_operation => #{
            <<"mcp.initialize">> => 1.0,       % 100% (critical)
            <<"mcp.tools.call">> => 1.0,       % 100% (critical)
            <<"mcp.resources.list">> => 0.1,   % 10% (high volume)
            <<"mcp.transport.send">> => 0.01   % 1% (very high volume)
        },
        tail_sampling => #{
            enabled => true,
            latency_threshold_us => 100000,  % Sample if >100ms
            error_rate => 1.0                % Sample all errors
        }
    }
}
```

**Estimated Cost Reduction:**
- Without sampling: 1M spans/day = $100/month
- With head-based sampling (10%): 100K spans/day = $10/month
- With tail-based sampling: 120K spans/day = $12/month (includes high-latency + errors)

---

## 10. Instrumentation Checklist

### 10.1 Module Instrumentation

**Priority 1 (Critical Path):**

- [ ] `erlmcp_server.erl`
  - [ ] Instrument `handle_call({call_tool, ...})` - tools/call operation
  - [ ] Instrument `handle_call({list_resources, ...})` - resources/list operation
  - [ ] Instrument `handle_call({read_resource, ...})` - resources/read operation
  - [ ] Instrument `handle_call({subscribe_resource, ...})` - subscription management
  - [ ] Instrument `notify_resource_updated/3` - subscription notifications
  - [ ] Instrument `init/1` - server initialization

- [ ] `erlmcp_client.erl`
  - [ ] Instrument `call_tool/3` - client tool invocation
  - [ ] Instrument `list_resources/1` - client resource listing
  - [ ] Instrument `read_resource/2` - client resource reading
  - [ ] Instrument `initialize/2` - client initialization

**Priority 2 (Transport Layer):**

- [ ] `erlmcp_transport_stdio.erl`
  - [ ] Instrument `send/2` - message sending
  - [ ] Instrument `handle_info({data, ...})` - message receiving

- [ ] `erlmcp_transport_tcp.erl`
  - [ ] Instrument `send/2` - TCP send
  - [ ] Instrument `handle_info({tcp, ...})` - TCP receive

- [ ] `erlmcp_transport_http.erl`
  - [ ] Instrument `send/2` - HTTP request
  - [ ] Instrument `handle_info({gun_response, ...})` - HTTP response

**Priority 3 (Protocol Processing):**

- [ ] `erlmcp_json_rpc.erl`
  - [ ] Instrument `encode/1` - JSON encoding
  - [ ] Instrument `decode/1` - JSON decoding

- [ ] `erlmcp_session_manager.erl`
  - [ ] Instrument `create_session/2` - session creation
  - [ ] Instrument `get_session/1` - session retrieval
  - [ ] Instrument `delete_session/1` - session cleanup

### 10.2 New Module Checklist

- [ ] `erlmcp_mcp_metrics.erl` - Metrics collection
  - [ ] Implement `gen_server` behavior
  - [ ] Create ETS table for metrics storage
  - [ ] Implement histogram approximation (t-digest)
  - [ ] Implement flush mechanism to exporters
  - [ ] Add API functions for recording metrics
  - [ ] Write EUnit tests (>80% coverage)

- [ ] `erlmcp_mcp_logger.erl` - Structured logging
  - [ ] Implement `gen_server` behavior
  - [ ] Create log buffer (queue-based)
  - [ ] Implement log formatting (JSON schema)
  - [ ] Integrate with logger backends
  - [ ] Add log level filtering
  - [ ] Write EUnit tests (>80% coverage)

- [ ] `erlmcp_mcp_alerting.erl` - SLO alerting
  - [ ] Implement `gen_server` behavior
  - [ ] Implement SLO evaluation logic
  - [ ] Implement sliding window calculations
  - [ ] Implement alert cooldown mechanism
  - [ ] Integrate with alert backends (PagerDuty, Slack)
  - [ ] Write EUnit tests (>80% coverage)

- [ ] `erlmcp_mcp_compliance.erl` - Compliance tracking
  - [ ] Implement `gen_server` behavior
  - [ ] Implement compliance check logic
  - [ ] Implement report generation
  - [ ] Schedule periodic reports
  - [ ] Persist reports to disk/S3
  - [ ] Write EUnit tests (>80% coverage)

- [ ] `erlmcp_mcp_instrumentation.erl` - Library module
  - [ ] Create macros for common instrumentation patterns
  - [ ] Implement helper functions for span management
  - [ ] Add timing utilities
  - [ ] Write EUnit tests (>80% coverage)

### 10.3 Configuration Checklist

- [ ] Add MCP observability section to `apps/erlmcp_observability/src/erlmcp_observability.app.src`
- [ ] Add default configuration to `sys.config`
- [ ] Document configuration options in README
- [ ] Add configuration validation in `erlmcp_observability_app:start/2`

### 10.4 Testing Checklist

- [ ] Unit tests for all new modules (>80% coverage)
- [ ] Integration tests for instrumentation
  - [ ] Test span creation and propagation
  - [ ] Test metric recording and aggregation
  - [ ] Test log event generation
  - [ ] Test alert triggering
  - [ ] Test compliance tracking

- [ ] Performance tests
  - [ ] Measure instrumentation overhead (<5% latency)
  - [ ] Measure memory overhead (<10% increase)
  - [ ] Benchmark metric recording throughput (>100K ops/s)
  - [ ] Benchmark log writing throughput (>10K events/s)

- [ ] Common Test suite
  - [ ] End-to-end observability flow
  - [ ] OTEL exporter integration
  - [ ] Dashboard WebSocket updates
  - [ ] Alert delivery verification

### 10.5 Documentation Checklist

- [ ] Architecture documentation (this document)
- [ ] API documentation (EDoc)
- [ ] Metrics reference guide
- [ ] Logging schema reference
- [ ] Dashboard user guide
- [ ] Runbook for operations
- [ ] Migration guide (if breaking changes)

---

## 11. Performance Impact Analysis

### 11.1 Overhead Estimates

| Component | CPU Overhead | Memory Overhead | Latency Impact |
|-----------|--------------|-----------------|----------------|
| OTEL Span Creation | 0.1-0.5ms | 500 bytes/span | 0.1-0.5ms |
| Metric Recording (async) | 0.01-0.05ms | 100 bytes/metric | <0.01ms |
| Log Event (async) | 0.05-0.1ms | 200 bytes/event | <0.01ms |
| Health Check (periodic) | <1% CPU | 10KB | 0ms (async) |
| Dashboard Update | <0.5% CPU | 50KB | 0ms (async) |

**Total Expected Overhead:**
- **Latency**: <5% increase (worst case: 0.5ms added to 10ms operation = 5%)
- **CPU**: <2% increase
- **Memory**: <10MB for 10K active spans + metrics buffer

### 11.2 Mitigation Strategies

**Sampling:**
- Reduce span volume by 90% (10% default sampling)
- Tail-based sampling for critical events

**Batching:**
- Flush metrics every 10s (configurable)
- Batch log events before writing
- Buffer alerts with cooldown

**ETS Storage:**
- Use ETS for high-performance metric storage
- Avoid gen_server bottlenecks for hot paths

**Async Recording:**
- All recording operations use `gen_server:cast` (non-blocking)
- No blocking on exporter failures

### 11.3 Performance Testing

**Load Test Scenarios:**
```erlang
%% Scenario 1: High tool call volume
%% - 1000 tools/call/s
%% - 100% instrumented
%% - Expected: <5% latency increase

%% Scenario 2: High resource read volume
%% - 10,000 resources/read/s
%% - 10% sampled
%% - Expected: <1% latency increase

%% Scenario 3: Subscription fan-out
%% - 1000 subscribers
%% - 10 Hz notification rate
%% - Expected: P95 < 50ms notification latency
```

---

## 12. Rollout Plan

### Phase 1: Foundation (Week 1-2)
**Goal:** Establish core infrastructure

**Tasks:**
1. Create 4 new gen_server modules (skeleton only)
2. Add child specs to erlmcp_observability_sup
3. Implement ETS-based metric storage
4. Create basic API functions

**Deliverables:**
- [ ] Modules compile
- [ ] Supervisor starts successfully
- [ ] Basic tests pass

### Phase 2: Instrumentation (Week 3-4)
**Goal:** Instrument critical paths

**Tasks:**
1. Instrument erlmcp_server (tools/call, resources/list, resources/read)
2. Instrument erlmcp_client (initialize, call_tool)
3. Add OTEL span creation to all operations
4. Implement metric recording

**Deliverables:**
- [ ] Critical path instrumented
- [ ] Metrics collected
- [ ] OTEL spans exported

### Phase 3: Logging & Alerts (Week 5-6)
**Goal:** Add structured logging and alerting

**Tasks:**
1. Implement structured logging
2. Define SLOs and alert rules
3. Integrate with alert backends (PagerDuty, Slack)
4. Test alert delivery

**Deliverables:**
- [ ] Logs generated for all events
- [ ] Alerts fire on SLO violations
- [ ] Alert backends integrated

### Phase 4: Dashboard & Compliance (Week 7-8)
**Goal:** Build compliance dashboard

**Tasks:**
1. Implement compliance tracking
2. Build real-time dashboard
3. Generate weekly compliance reports
4. Document all features

**Deliverables:**
- [ ] Dashboard accessible
- [ ] Compliance reports generated
- [ ] Documentation complete

---

## 13. Success Criteria

| Metric | Current | Target | Measurement |
|--------|---------|--------|-------------|
| MCP Operation Coverage | 60% | 100% | OTEL span count |
| Metric Cardinality | 20 | 28+ | Metric definitions |
| Instrumentation Overhead | N/A | <5% | Latency benchmarks |
| Alert Response Time | Manual | <5 min | Time to notification |
| Compliance Score | Unknown | 100% | Weekly report |
| Dashboard Uptime | N/A | 99.9% | Health checks |
| Log Event Coverage | N/A | 100% | Event taxonomy |

**Gate for Production:**
- [ ] All instrumentation points covered
- [ ] <5% performance overhead verified
- [ ] 100% MCP operation coverage
- [ ] 0 compliance violations for 1 week
- [ ] Dashboard accessible and responsive
- [ ] Alerts delivered within SLA

---

## 14. Risk Analysis

### High Risk
**Risk:** Instrumentation overhead exceeds 5% latency target
**Mitigation:** Aggressive sampling (1-10%), async recording, ETS storage
**Contingency:** Disable instrumentation for specific operations

**Risk:** OTEL exporter failures block protocol operations
**Mitigation:** Isolated supervision, async exports, circuit breakers
**Contingency:** Disable exporter, queue spans for later

### Medium Risk
**Risk:** Alert storm during outages
**Mitigation:** Alert cooldowns, rate limiting, deduplication
**Contingency:** Manual alert suppression

**Risk:** Dashboard becomes a bottleneck
**Mitigation:** WebSocket batching, client-side rendering
**Contingency:** Static snapshots instead of real-time

### Low Risk
**Risk:** Log storage fills disk
**Mitigation:** Retention policies, log rotation, compression
**Contingency:** Increase disk or reduce retention

---

## 15. Architectural Decisions (ADRs)

### ADR-001: Use gen_server for Stateful Collectors

**Decision:** All stateful observability components (metrics, logging, alerting, compliance) use `gen_server` behavior.

**Rationale:**
- State management (buffers, counters, windows)
- Sync/async API support
- Supervisor integration
- Well-understood OTP pattern

**Alternatives Considered:**
- Pure library modules - No state management
- ETS-based stateless - Race conditions, complexity

### ADR-002: ETS for Metric Storage

**Decision:** Store metrics in ETS tables instead of gen_server state.

**Rationale:**
- Survives process restarts
- Concurrent reads without blocking gen_server
- High performance (1M+ ops/s)
- Standard OTP pattern

**Alternatives Considered:**
- gen_server state - Bottleneck, lost on restart
- Mnesia - Overkill for metrics

### ADR-003: Async Metric Recording

**Decision:** All metric recording uses `gen_server:cast` (non-blocking).

**Rationale:**
- Zero blocking on protocol operations
- Performance critical (hot path)
- Best-effort is acceptable for metrics

**Alternatives Considered:**
- Synchronous recording - Blocking, performance impact
- Direct ETS writes - No validation, no aggregation

### ADR-004: Head-Based + Tail-Based Sampling

**Decision:** Use head-based sampling by default (10%) with tail-based for critical events (errors, high latency).

**Rationale:**
- Reduces cost by 90%
- Captures all important events (errors, slow operations)
- Configurable per-operation

**Alternatives Considered:**
- Always-on - Too expensive
- Always-off - No visibility
- Tail-based only - Complex implementation

### ADR-005: Extend Existing Supervisor

**Decision:** Add new workers to `erlmcp_observability_sup` instead of creating new supervisor.

**Rationale:**
- No breaking changes
- Isolated from core protocol
- Consistent with existing architecture

**Alternatives Considered:**
- New supervisor tree - More complexity
- Embed in erlmcp_core_sup - Violates separation

---

## 16. References

**Existing Modules:**
- `/home/user/erlmcp/apps/erlmcp_observability/src/erlmcp_observability_sup.erl`
- `/home/user/erlmcp/apps/erlmcp_observability/src/erlmcp_otel.erl`
- `/home/user/erlmcp/apps/erlmcp_observability/src/erlmcp_metrics.erl`
- `/home/user/erlmcp/apps/erlmcp_observability/src/erlmcp_health_monitor.erl`

**Documentation:**
- `/home/user/erlmcp/docs/MCP_OBSERVABILITY_DESIGN.md`
- `/home/user/erlmcp/docs/architecture.md`
- `/home/user/erlmcp/docs/otp-patterns.md`

**MCP Specification:**
- Version: 2025-11-25
- Performance targets: P50<5ms, P95<20ms, P99<50ms

---

**END OF ARCHITECTURE DOCUMENT**

This design is ready for:
1. **Stakeholder Review** - Erlang OTP Developer, Plan Designer, Code Reviewer
2. **Implementation** - Hand off to Erlang OTP Developer for coding
3. **Testing** - Hand off to Erlang Test Engineer for test strategy
4. **Integration** - Hand off to Build Engineer for CI/CD integration
