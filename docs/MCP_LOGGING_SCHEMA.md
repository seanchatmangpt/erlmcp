# MCP Logging Schema Reference
**Version:** 1.0.0
**Date:** 2026-02-02
**Purpose:** Structured logging schema for MCP protocol events

---

## Standard Log Event Structure

All log events share these standard fields:

```erlang
#{
    <<"timestamp">> => integer(),           % Nanosecond precision
    <<"level">> => atom(),                  % debug | info | warning | error | critical
    <<"event_type">> => binary(),           % Event category
    <<"component">> => binary(),            % server | client | transport | registry
    <<"trace_id">> => binary(),             % OTEL trace ID
    <<"span_id">> => binary(),              % OTEL span ID
    <<"server_id">> => atom(),              % Server identifier
    <<"node">> => atom(),                   % Erlang node
    <<"pid">> => binary()                   % Process ID
}
```

---

## Event Type: protocol_event

**Purpose:** Log MCP protocol operations

### Structure
```erlang
#{
    % Standard fields
    <<"timestamp">> => Timestamp,
    <<"level">> => info,
    <<"event_type">> => <<"protocol_event">>,
    <<"component">> => Component,

    % Protocol-specific fields
    <<"operation">> => binary(),            % MCP method name
    <<"phase">> => binary(),                % start | complete | error
    <<"request_id">> => binary(),           % JSON-RPC request ID
    <<"duration_us">> => integer(),         % Operation duration
    <<"metadata">> => map()                 % Operation-specific data
}
```

### Examples

#### Initialize Event (Complete)
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
    <<"request_id">> => <<"1">>,
    <<"duration_us">> => 45000,
    <<"metadata">> => #{
        <<"protocol_version">> => <<"2025-11-25">>,
        <<"client_name">> => <<"claude-desktop">>,
        <<"client_version">> => <<"1.0.0">>,
        <<"capabilities">> => #{
            <<"resources">> => #{
                <<"subscribe">> => true,
                <<"listChanged">> => true
            },
            <<"tools">> => #{},
            <<"prompts">> => #{}
        }
    }
}
```

#### Tools/Call Event (Start)
```erlang
#{
    <<"timestamp">> => 1738502401000000000,
    <<"level">> => info,
    <<"event_type">> => <<"protocol_event">>,
    <<"component">> => <<"server">>,
    <<"operation">> => <<"tools/call">>,
    <<"phase">> => <<"start">>,
    <<"server_id">> => <<"server_1">>,
    <<"trace_id">> => <<"xyz789...">>,
    <<"span_id">> => <<"uvw012...">>,
    <<"request_id">> => <<"2">>,
    <<"metadata">> => #{
        <<"tool_name">> => <<"analyze_code">>,
        <<"args_count">> => 3
    }
}
```

#### Tools/Call Event (Complete)
```erlang
#{
    <<"timestamp">> => 1738502401250000000,
    <<"level">> => info,
    <<"event_type">> => <<"protocol_event">>,
    <<"component">> => <<"server">>,
    <<"operation">> => <<"tools/call">>,
    <<"phase">> => <<"complete">>,
    <<"server_id">> => <<"server_1">>,
    <<"trace_id">> => <<"xyz789...">>,
    <<"span_id">> => <<"uvw012...">>,
    <<"request_id">> => <<"2">>,
    <<"duration_us">> => 250000,
    <<"metadata">> => #{
        <<"tool_name">> => <<"analyze_code">>,
        <<"validation_time_us">> => 1500,
        <<"execution_time_us">> => 248000,
        <<"result_size_bytes">> => 4096
    }
}
```

#### Resources/Read Event (Cache Hit)
```erlang
#{
    <<"timestamp">> => 1738502402000000000,
    <<"level">> => debug,
    <<"event_type">> => <<"protocol_event">>,
    <<"component">> => <<"server">>,
    <<"operation">> => <<"resources/read">>,
    <<"phase">> => <<"complete">>,
    <<"server_id">> => <<"server_1">>,
    <<"trace_id">> => <<"rst345...">>,
    <<"span_id">> => <<"mno678...">>,
    <<"request_id">> => <<"3">>,
    <<"duration_us">> => 500,
    <<"metadata">> => #{
        <<"uri">> => <<"file:///path/to/resource">>,
        <<"cache_hit">> => true,
        <<"content_size_bytes">> => 2048,
        <<"mime_type">> => <<"text/plain">>
    }
}
```

---

## Event Type: decision

**Purpose:** Log system decisions (routing, caching, sampling, etc.)

### Structure
```erlang
#{
    % Standard fields
    <<"timestamp">> => Timestamp,
    <<"level">> => debug,
    <<"event_type">> => <<"decision">>,
    <<"component">> => Component,

    % Decision-specific fields
    <<"decision_type">> => binary(),        % sampling | routing | caching
    <<"decision">> => term(),               % The decision made
    <<"rationale">> => binary(),            % Why this decision
    <<"metadata">> => map()                 % Additional context
}
```

### Examples

#### Sampling Decision
```erlang
#{
    <<"timestamp">> => 1738502403000000000,
    <<"level">> => debug,
    <<"event_type">> => <<"decision">>,
    <<"component">> => <<"server">>,
    <<"decision_type">> => <<"sampling">>,
    <<"operation">> => <<"tools/call">>,
    <<"decision">> => <<"sample">>,
    <<"rationale">> => <<"critical_path">>,
    <<"metadata">> => #{
        <<"sampling_rate">> => 1.0,
        <<"strategy">> => <<"head_based">>
    }
}
```

#### Caching Decision
```erlang
#{
    <<"timestamp">> => 1738502404000000000,
    <<"level">> => debug,
    <<"event_type">> => <<"decision">>,
    <<"component">> => <<"server">>,
    <<"decision_type">> => <<"caching">>,
    <<"operation">> => <<"resources/read">>,
    <<"decision">> => <<"cache_miss">>,
    <<"rationale">> => <<"resource_not_cached">>,
    <<"metadata">> => #{
        <<"uri">> => <<"file:///new/resource">>,
        <<"cache_policy">> => <<"lru">>,
        <<"cache_size">> => 100
    }
}
```

---

## Event Type: error

**Purpose:** Log errors and exceptions

### Structure
```erlang
#{
    % Standard fields
    <<"timestamp">> => Timestamp,
    <<"level">> => error | critical,
    <<"event_type">> => <<"error">>,
    <<"component">> => Component,

    % Error-specific fields
    <<"error_class">> => atom(),            % throw | error | exit
    <<"error_type">> => binary(),           % Error categorization
    <<"error_message">> => binary(),        % Human-readable message
    <<"stacktrace">> => binary(),           % Formatted stacktrace
    <<"operation">> => binary(),            % Operation that failed
    <<"metadata">> => map()                 % Additional context
}
```

### Examples

#### Tool Handler Error
```erlang
#{
    <<"timestamp">> => 1738502405000000000,
    <<"level">> => error,
    <<"event_type">> => <<"error">>,
    <<"component">> => <<"server">>,
    <<"error_class">> => <<"error">>,
    <<"error_type">> => <<"tool_execution_failed">>,
    <<"error_message">> => <<"Handler function raised exception">>,
    <<"stacktrace">> => <<"[{erlmcp_server,execute_tool_handler,3,...}]">>,
    <<"operation">> => <<"tools/call">>,
    <<"server_id">> => <<"server_1">>,
    <<"trace_id">> => <<"err123...">>,
    <<"span_id">> => <<"err456...">>,
    <<"metadata">> => #{
        <<"tool_name">> => <<"buggy_tool">>,
        <<"args">> => <<"[REDACTED]">>,
        <<"exception">> => <<"badarg">>
    }
}
```

#### Schema Validation Error
```erlang
#{
    <<"timestamp">> => 1738502406000000000,
    <<"level">> => warning,
    <<"event_type">> => <<"error">>,
    <<"component">> => <<"server">>,
    <<"error_class">> => <<"error">>,
    <<"error_type">> => <<"schema_validation_failed">>,
    <<"error_message">> => <<"Tool arguments do not match schema">>,
    <<"stacktrace">> => <<"">>,
    <<"operation">> => <<"tools/call">>,
    <<"server_id">> => <<"server_1">>,
    <<"trace_id">> => <<"val123...">>,
    <<"span_id">> => <<"val456...">>,
    <<"metadata">> => #{
        <<"tool_name">> => <<"strict_tool">>,
        <<"validation_errors">> => [
            <<"Missing required field: 'path'">>,
            <<"Invalid type for 'count': expected integer, got string">>
        ]
    }
}
```

#### Transport Error
```erlang
#{
    <<"timestamp">> => 1738502407000000000,
    <<"level">> => error,
    <<"event_type">> => <<"error">>,
    <<"component">> => <<"transport">>,
    <<"error_class">> => <<"error">>,
    <<"error_type">> => <<"transport_send_failed">>,
    <<"error_message">> => <<"Failed to send message over TCP">>,
    <<"stacktrace">> => <<"">>,
    <<"operation">> => <<"transport.send">>,
    <<"metadata">> => #{
        <<"transport_type">> => <<"tcp">>,
        <<"transport_id">> => <<"tcp_1">>,
        <<"error">> => <<"econnrefused">>
    }
}
```

---

## Event Type: performance

**Purpose:** Log performance-related events (latency spikes, SLO violations)

### Structure
```erlang
#{
    % Standard fields
    <<"timestamp">> => Timestamp,
    <<"level">> => warning | critical,
    <<"event_type">> => <<"performance">>,
    <<"component">> => Component,

    % Performance-specific fields
    <<"performance_type">> => binary(),     % latency_spike | slo_violation | threshold_exceeded
    <<"metric">> => binary(),               % Metric name
    <<"value">> => number(),                % Measured value
    <<"threshold">> => number(),            % Threshold exceeded
    <<"operation">> => binary(),            % Operation affected
    <<"metadata">> => map()                 % Additional context
}
```

### Examples

#### Latency Spike
```erlang
#{
    <<"timestamp">> => 1738502408000000000,
    <<"level">> => warning,
    <<"event_type">> => <<"performance">>,
    <<"component">> => <<"server">>,
    <<"performance_type">> => <<"latency_spike">>,
    <<"metric">> => <<"erlmcp.mcp.tools.call.latency_us">>,
    <<"value">> => 750000,  % 750ms
    <<"threshold">> => 500000,  % 500ms
    <<"operation">> => <<"tools/call">>,
    <<"server_id">> => <<"server_1">>,
    <<"metadata">> => #{
        <<"tool_name">> => <<"slow_tool">>,
        <<"percentile">> => <<"P95">>,
        <<"window">> => <<"5m">>
    }
}
```

#### SLO Violation
```erlang
#{
    <<"timestamp">> => 1738502409000000000,
    <<"level">> => critical,
    <<"event_type">> => <<"performance">>,
    <<"component">> => <<"server">>,
    <<"performance_type">> => <<"slo_violation">>,
    <<"metric">> => <<"erlmcp.mcp.resources.list.latency_us">>,
    <<"value">> => 65000,  % 65ms
    <<"threshold">> => 50000,  % 50ms
    <<"operation">> => <<"resources/list">>,
    <<"server_id">> => <<"server_1">>,
    <<"metadata">> => #{
        <<"slo_name">> => <<"resources_list_p95_latency">>,
        <<"duration_exceeded">> => 300000,  % 5 minutes
        <<"alert_triggered">> => true
    }
}
```

---

## Logging API

```erlang
%% Log protocol event
erlmcp_mcp_logger:log_protocol_event(Operation, Phase, Metadata).

%% Log decision
erlmcp_mcp_logger:log_decision(DecisionType, Decision, Rationale).

%% Log error
erlmcp_mcp_logger:log_error(ErrorType, ErrorData, Stacktrace).

%% Log performance event
erlmcp_mcp_logger:log_performance(PerformanceType, Metric, Value, Threshold).
```

### Example Usage
```erlang
%% In erlmcp_server:handle_call({call_tool, ...})
StartTime = erlang:monotonic_time(microsecond),

%% Log start
erlmcp_mcp_logger:log_protocol_event(
    <<"tools/call">>,
    <<"start">>,
    #{tool_name => ToolName, args_count => length(maps:keys(Args))}
),

%% Execute tool
Result = execute_tool(ToolName, Args),

%% Log complete
Duration = erlang:monotonic_time(microsecond) - StartTime,
erlmcp_mcp_logger:log_protocol_event(
    <<"tools/call">>,
    <<"complete">>,
    #{tool_name => ToolName, duration_us => Duration}
).
```

---

## Log Retention

| Level | Retention | Storage | Format |
|-------|-----------|---------|--------|
| debug | 1 day | Local disk | JSON |
| info | 7 days | Local disk + S3 | JSON |
| warning | 30 days | S3 | JSON compressed |
| error | 90 days | S3 | JSON compressed |
| critical | 1 year | S3 + archive | JSON compressed |

---

## Log Backends

### Configured Backends
```erlang
#{
    backends => [
        {console, #{level => info}},
        {file, #{
            level => debug,
            path => "/var/log/erlmcp/mcp.log",
            rotation => daily,
            max_size => 100 * 1024 * 1024  % 100MB
        }},
        {syslog, #{
            level => warning,
            host => "localhost",
            port => 514
        }},
        {elasticsearch, #{
            level => info,
            url => "http://localhost:9200",
            index => "erlmcp-logs"
        }}
    ]
}
```

---

## Query Examples (Elasticsearch)

### Find all tool errors in last hour
```json
{
  "query": {
    "bool": {
      "must": [
        {"term": {"event_type": "error"}},
        {"term": {"operation": "tools/call"}},
        {"range": {"timestamp": {"gte": "now-1h"}}}
      ]
    }
  }
}
```

### Find high latency events
```json
{
  "query": {
    "bool": {
      "must": [
        {"term": {"event_type": "performance"}},
        {"term": {"performance_type": "latency_spike"}},
        {"range": {"value": {"gt": 100000}}}
      ]
    }
  }
}
```

---

**Total Event Types:** 4 (protocol_event, decision, error, performance)
**Standard Fields:** 9 (consistent across all events)
**Event-Specific Fields:** Variable by type
