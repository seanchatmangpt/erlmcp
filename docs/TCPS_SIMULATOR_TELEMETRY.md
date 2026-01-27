# TCPS MCP Diataxis Simulator - Telemetry and Visualization

## Overview

Production-grade OpenTelemetry integration and visualization system for the TCPS MCP Diataxis simulator. Provides comprehensive observability for learning sessions, work order execution, quality gates, Andon events, and MCP tool interactions.

## Table of Contents

- [Architecture](#architecture)
- [OpenTelemetry Integration](#opentelemetry-integration)
- [Metrics System](#metrics-system)
- [Visualization Framework](#visualization-framework)
- [Installation](#installation)
- [Usage](#usage)
- [Export Formats](#export-formats)
- [Performance](#performance)
- [Best Practices](#best-practices)

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                   TCPS MCP Diataxis Simulator                   │
└──────────────────────┬──────────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────────┐
│              tcps_simulator_telemetry.erl                       │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  OpenTelemetry Spans (8 Types)                           │   │
│  │  - Session, Scenario, Work Order, Quality Gate           │   │
│  │  - Andon Event, Diataxis Navigation, Tutorial Step       │   │
│  │  - MCP Tool Call                                          │   │
│  └──────────────────────────────────────────────────────────┘   │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  Metrics (10+ Types)                                      │   │
│  │  - Gauges: active_sessions, wip_current                  │   │
│  │  - Counters: work_orders, quality_gates, andon_events    │   │
│  │  - Histograms: cycle_time, session_duration, latency     │   │
│  └──────────────────────────────────────────────────────────┘   │
└──────────────────────┬──────────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────────┐
│              tcps_metrics_collector.erl                         │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  ETS-Based High-Performance Storage                      │   │
│  │  - Real-time aggregation                                 │   │
│  │  - Sliding window calculations                           │   │
│  │  - Multi-dimensional metrics                             │   │
│  │  - Anomaly detection                                     │   │
│  └──────────────────────────────────────────────────────────┘   │
└──────────────────────┬──────────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────────┐
│              tcps_visualization_data.erl                        │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  Visualization Data Generation (5 Types)                 │   │
│  │  - Kanban Board Heatmap                                  │   │
│  │  - Quality Gate Funnel                                   │   │
│  │  - Andon Event Timeline                                  │   │
│  │  - Learning Progress Dashboard                           │   │
│  │  - MCP Tool Usage Analytics                              │   │
│  └──────────────────────────────────────────────────────────┘   │
└──────────────────────┬──────────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Export Formats                               │
│  ┌──────────────┬──────────────┬──────────────┬──────────────┐  │
│  │ OTLP/Jaeger  │  Prometheus  │   Chart.js   │     CSV      │  │
│  │  (Tracing)   │  (Metrics)   │    (UI)      │  (Analysis)  │  │
│  └──────────────┴──────────────┴──────────────┴──────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

## OpenTelemetry Integration

### Span Types (8 Total)

#### 1. Session Span (`simulator.session`)

Tracks the full learning session lifecycle.

**Attributes:**
- `session_id`: Unique session identifier
- `user_id`: User identifier
- `quadrant`: Diataxis quadrant (tutorial/how_to/reference/explanation)
- `difficulty_level`: 1-10 difficulty rating
- `start_time`: ISO 8601 timestamp
- `estimated_duration`: Estimated duration in seconds

**Completion Attributes:**
- `work_orders_completed`: Number of work orders completed
- `quality_gates_passed`: Quality gates passed
- `quality_gates_failed`: Quality gates failed
- `andon_events`: Andon events triggered
- `learning_outcomes_achieved`: Achieved learning outcomes
- `user_satisfaction`: 1-5 rating

**Example:**
```erlang
SessionSpan = tcps_simulator_telemetry:start_session(#{
    session_id => <<"session-001">>,
    user_id => <<"user-123">>,
    quadrant => tutorial,
    difficulty_level => 3
}),

%% ... perform session activities ...

tcps_simulator_telemetry:end_session(SessionSpan, #{
    work_orders_completed => 5,
    quality_gates_passed => 4,
    user_satisfaction => 4
}).
```

#### 2. Scenario Span (`simulator.scenario`)

Tracks individual scenario execution within a session.

**Attributes:**
- `scenario_id`: Unique scenario identifier
- `scenario_type`: Type of scenario
- `work_orders_count`: Expected work orders
- `complexity`: Complexity rating
- `expected_outcomes`: Expected learning outcomes

#### 3. Work Order Span (`simulator.work_order`)

Tracks work order creation and execution.

**Attributes:**
- `work_order_id`: Unique work order identifier
- `bucket`: reliability | security | cost | compliance
- `priority`: 1-10 priority rating
- `source`: github | cve | marketplace | internal
- `sla_target`: SLA target in seconds
- `assigned_to`: Assignee identifier
- `dependencies`: List of dependent work order IDs

**Completion Attributes:**
- `status`: completed | cancelled | blocked
- `cycle_time`: Actual cycle time in seconds
- `sla_met`: true | false
- `quality_score`: 0-100 quality score

#### 4. Quality Gate Span (`simulator.quality_gate`)

Tracks quality gate evaluation.

**Attributes:**
- `gate_id`: Unique gate identifier
- `gate_type`: code_coverage | test_pass_rate | security_scan | performance
- `threshold`: Threshold value
- `actual_value`: Actual measured value

**Completion Attributes:**
- `passed`: true | false
- `remediation_required`: true | false
- `remediation_actions`: Required remediation actions

#### 5. Andon Event Span (`simulator.andon_event`)

Tracks Andon event lifecycle.

**Attributes:**
- `event_id`: Unique event identifier
- `severity`: low | medium | high | critical
- `root_cause`: Root cause category
- `impact_radius`: Number of affected work orders

**Completion Attributes:**
- `resolved`: true | false
- `resolution_time`: Resolution time in seconds
- `escalated`: true | false
- `lessons_learned`: Lessons learned

#### 6. Diataxis Navigation Span (`diataxis.navigation`)

Tracks documentation quadrant navigation.

**Attributes:**
- `from_quadrant`: Source quadrant
- `to_quadrant`: Destination quadrant
- `reason`: Navigation reason
- `documentation_accessed`: Documentation pages accessed

#### 7. Tutorial Step Span (`tutorial.step`)

Tracks individual tutorial step execution.

**Attributes:**
- `step_number`: Step number
- `step_type`: instruction | practice | validation | reflection
- `tools_used`: MCP tools used

**Completion Attributes:**
- `completion_status`: completed | skipped | failed
- `time_spent`: Time in seconds
- `hints_used`: Number of hints used
- `errors_made`: Number of errors made

#### 8. MCP Tool Call Span (`mcp.tool_call`)

Tracks MCP tool invocations.

**Attributes:**
- `tool_name`: Tool name
- `tool_version`: Tool version
- `parameters`: Tool parameters (sanitized)
- `caller_context`: Caller context

**Completion Attributes:**
- `result_size`: Result size in bytes
- `latency`: Latency in milliseconds
- `error_code`: Error code if failed
- `cache_hit`: true | false

### Metrics (10+ Types)

#### Gauges

- **`simulator.active_sessions`**: Current active learning sessions
- **`simulator.wip_current`**: Current WIP per bucket (reliability/security/cost/compliance)

#### Counters

- **`simulator.work_orders_created`**: Total work orders created
- **`simulator.quality_gates_passed`**: Quality gates passed
- **`simulator.quality_gates_failed`**: Quality gates failed
- **`simulator.andon_events_total`**: Total Andon events triggered
- **`mcp.tool_calls_total`**: Total MCP tool invocations

#### Histograms

- **`simulator.cycle_time`**: Work order cycle time distribution
- **`simulator.session_duration`**: Session duration distribution
- **`mcp.tool_latency`**: MCP tool latency distribution
- **`simulator.quality_gate_score`**: Quality gate score distribution

## Metrics System

### Real-Time Aggregation

The metrics collector uses ETS-based storage for high-performance, real-time aggregation:

```erlang
%% Start collector
tcps_metrics_collector:start_link(#{
    aggregation_interval => 1000,      % 1 second
    retention_period => 86400000,      % 24 hours
    enable_anomaly_detection => true,
    anomaly_threshold => 2.0           % 2 standard deviations
}).
```

### Aggregation Windows

- **1 minute**: Real-time monitoring
- **5 minutes**: Short-term trends
- **15 minutes**: Medium-term analysis
- **1 hour**: Long-term patterns
- **1 day**: Historical analysis

### Recording Metrics

```erlang
%% Work order metrics
tcps_metrics_collector:record_work_order_created(#{
    bucket => security,
    priority => 9,
    source => cve,
    timestamp => erlang:system_time(millisecond)
}),

tcps_metrics_collector:record_work_order_completed(#{
    bucket => security,
    cycle_time => 3600000,
    sla_met => true,
    quality_score => 95,
    timestamp => erlang:system_time(millisecond)
}),

%% Quality gate metrics
tcps_metrics_collector:record_quality_gate_result(#{
    gate_type => code_coverage,
    passed => true,
    score => 85,
    threshold => 80
}),

%% Learning metrics
tcps_metrics_collector:record_session_complete(#{
    session_id => <<"session-001">>,
    quadrant => tutorial,
    duration => 3600000,
    work_orders_completed => 5,
    user_satisfaction => 4
}).
```

### Retrieving Metrics

```erlang
%% Get work order metrics
WorkOrderMetrics = tcps_metrics_collector:get_work_order_metrics(#{
    window => '5min',
    buckets => [security, reliability],
    include_trends => true
}),

%% Get all metrics
AllMetrics = tcps_metrics_collector:get_metrics(#{
    window => '1hour',
    include_trends => true,
    include_anomalies => true
}),

%% Get summary
Summary = tcps_metrics_collector:get_summary().
```

### Anomaly Detection

```erlang
%% Detect anomalies
Anomalies = tcps_metrics_collector:detect_anomalies(#{
    metric => work_order_creation_rate,
    threshold => 2.0,  % 2 standard deviations
    window => '1hour'
}).
```

## Visualization Framework

### Visualization Types (5 Categories)

#### 1. Kanban Board Heatmap

WIP distribution across buckets and time windows.

**Chart.js Format:**
```erlang
Heatmap = tcps_visualization_data:kanban_heatmap(#{
    window => '1hour',
    buckets => [security, reliability, cost, compliance],
    format => chartjs
}),

%% Returns:
%% #{
%%   labels => [<<"00:00">>, <<"00:05">>, <<"00:10">>, ...],
%%   datasets => [
%%     #{label => <<"Security">>, data => [60, 80, 100, ...],
%%       backgroundColor => <<"#ff6384">>},
%%     #{label => <<"Reliability">>, data => [40, 50, 60, ...],
%%       backgroundColor => <<"#36a2eb">>}
%%   ]
%% }
```

**D3.js Format:**
```erlang
Heatmap = tcps_visualization_data:kanban_heatmap(#{
    window => '1hour',
    buckets => [security, reliability],
    format => d3
}),

%% Returns:
%% [
%%   #{timestamp => 1640000000, data => [{security, 3}, {reliability, 5}]},
%%   #{timestamp => 1640000060, data => [{security, 4}, {reliability, 4}]}
%% ]
```

**Utilization Gauge:**
```erlang
Utilization = tcps_visualization_data:kanban_utilization(#{
    buckets => [security, reliability, cost, compliance],
    format => chartjs
}).
```

#### 2. Quality Gate Funnel

Pass/fail funnel through quality gate pipeline.

```erlang
Funnel = tcps_visualization_data:quality_gate_funnel(#{
    gates => [code_coverage, test_pass_rate, security_scan, performance],
    format => chartjs
}),

%% Funnel stages:
%% 1. Work orders created: 100
%% 2. Code coverage gate: 85 (85% pass)
%% 3. Test pass rate gate: 75 (88% pass)
%% 4. Security scan gate: 70 (93% pass)
%% 5. Performance gate: 68 (97% pass)
```

**Score Distribution:**
```erlang
Scores = tcps_visualization_data:quality_gate_scores(#{
    gates => [code_coverage, test_pass_rate],
    format => chartjs
}).
```

#### 3. Andon Event Timeline

Event occurrence timeline with severity-based coloring.

```erlang
Timeline = tcps_visualization_data:andon_timeline(#{
    window => '1day',
    severity_filter => [high, critical],
    format => d3
}),

%% Returns timeline data suitable for D3 timeline chart
```

**Severity Distribution:**
```erlang
Distribution = tcps_visualization_data:andon_severity_distribution(#{
    window => '1day',
    format => chartjs
}),

%% Returns pie chart data:
%% #{low => 10, medium => 5, high => 2, critical => 1}
```

**Resolution Times:**
```erlang
ResolutionTimes = tcps_visualization_data:andon_resolution_times(#{
    window => '1day',
    format => chartjs
}).
```

#### 4. Learning Progress Dashboard

Comprehensive learning metrics visualization.

```erlang
Dashboard = tcps_visualization_data:learning_dashboard(#{
    session_id => <<"session-001">>,
    include_navigation => true,
    format => chartjs
}),

%% Returns:
%% #{
%%   navigation_flow => ...      % Sankey diagram
%%   tutorial_progress => ...    % Step-by-step progress
%%   learning_outcomes => ...    % Radar chart
%%   session_summary => ...      % Summary metrics
%% }
```

**Quadrant Navigation Flow:**
```erlang
Flow = tcps_visualization_data:quadrant_navigation_flow(#{
    window => '1day',
    format => d3
}),

%% Returns Sankey diagram data:
%% #{
%%   nodes => [
%%     #{id => <<"tutorial">>, name => <<"Tutorial">>},
%%     #{id => <<"howto">>, name => <<"How-To">>},
%%     ...
%%   ],
%%   links => [
%%     #{source => <<"tutorial">>, target => <<"howto">>, value => 10},
%%     ...
%%   ]
%% }
```

#### 5. MCP Tool Usage Analytics

Tool invocation, latency, and error tracking.

```erlang
Analytics = tcps_visualization_data:tool_usage_analytics(#{
    tools => [list_tools, get_prompt, call_tool],
    window => '1hour',
    format => chartjs
}),

%% Returns:
%% #{
%%   invocation_frequency => ...  % Bar chart
%%   latency_heatmap => ...       % Heatmap
%%   error_rates => ...           % Line chart
%%   cache_analytics => ...       % Pie chart
%% }
```

**Latency Heatmap:**
```erlang
Latencies = tcps_visualization_data:tool_latency_heatmap(#{
    window => '1hour',
    tools => [list_tools, call_tool],
    format => chartjs
}).
```

**Cache Analytics:**
```erlang
CacheData = tcps_visualization_data:tool_cache_analytics(#{
    window => '1hour',
    format => chartjs
}),

%% Returns:
%% #{
%%   labels => [<<"Hit Rate">>, <<"Miss Rate">>],
%%   datasets => [#{
%%     data => [75.0, 25.0],
%%     backgroundColor => [<<"#4bc0c0">>, <<"#ff6384">>]
%%   }]
%% }
```

## Export Formats

### OTLP (OpenTelemetry Protocol)

Export traces to Jaeger, Tempo, or other OTLP-compatible backends:

```erlang
%% Configure OTLP endpoint
tcps_simulator_telemetry:configure(#{
    otlp_endpoint => "http://localhost:4318",
    enable_tracing => true,
    sample_rate => 1.0
}).
```

### Prometheus

Export metrics for Prometheus scraping:

```erlang
{ok, PrometheusData} = tcps_simulator_telemetry:export_prometheus(),

%% Returns:
%% tcps_simulator_active_sessions 2
%% tcps_simulator_work_orders_created 150
%% tcps_simulator_wip{bucket="security"} 3
%% tcps_simulator_cycle_time_count 100
%% tcps_simulator_cycle_time_sum 360000
%% tcps_simulator_cycle_time_p50 3200
%% tcps_simulator_cycle_time_p95 5800
```

### JSON

Export all metrics as JSON:

```erlang
{ok, JsonData} = tcps_simulator_telemetry:export_json(),

%% Returns:
%% #{
%%   metrics => #{
%%     active_sessions => 2,
%%     work_orders_created => 150,
%%     ...
%%   },
%%   histograms => #{
%%     cycle_time => #{count => 100, mean => 3600, p50 => 3200, ...}
%%   },
%%   active_spans => 5
%% }
```

### CSV

Export metrics for analysis:

```erlang
tcps_simulator_telemetry:export_csv("/tmp/metrics.csv"),

%% Generates:
%% metric,type,value
%% active_sessions,gauge,2
%% work_orders_created,counter,150
%% cycle_time_count,histogram,100
%% cycle_time_mean,histogram,3600.0
```

## Installation

### Dependencies

Add to `rebar.config`:

```erlang
{deps, [
    %% Existing dependencies...
]}.
```

### Configuration

Add to `config/sys.config`:

```erlang
[
    {erlmcp, [
        {tcps_simulator_telemetry, [
            {otlp_endpoint, "http://localhost:4318"},
            {prometheus_port, 9464},
            {export_interval, 10000},  % 10 seconds
            {enable_tracing, true},
            {enable_metrics, true},
            {sample_rate, 1.0}
        ]},
        {tcps_metrics_collector, [
            {aggregation_interval, 1000},  % 1 second
            {retention_period, 86400000},  % 24 hours
            {enable_anomaly_detection, true},
            {anomaly_threshold, 2.0}
        ]}
    ]}
].
```

## Usage

### Complete Example

```erlang
%% Start telemetry and metrics collector
{ok, _} = tcps_simulator_telemetry:start_link(#{
    otlp_endpoint => "http://localhost:4318",
    prometheus_port => 9464,
    export_interval => 10000
}),
{ok, _} = tcps_metrics_collector:start_link(#{
    aggregation_interval => 1000,
    retention_period => 86400000
}),

%% Start learning session
SessionSpan = tcps_simulator_telemetry:start_session(#{
    session_id => <<"session-001">>,
    user_id => <<"user-123">>,
    quadrant => tutorial,
    difficulty_level => 3
}),

%% Record session start metric
tcps_metrics_collector:record_session_start(#{
    session_id => <<"session-001">>,
    timestamp => erlang:system_time(millisecond)
}),

%% Create work order
WOSpan = tcps_simulator_telemetry:start_work_order(#{
    work_order_id => <<"WO-001">>,
    bucket => security,
    priority => 9,
    source => cve
}, SessionSpan),

%% Record WIP metric
tcps_simulator_telemetry:record_wip(security, 3),

%% Record work order creation
tcps_metrics_collector:record_work_order_created(#{
    bucket => security,
    priority => 9,
    timestamp => erlang:system_time(millisecond)
}),

%% Quality gate evaluation
GateSpan = tcps_simulator_telemetry:start_quality_gate(#{
    gate_id => <<"gate-coverage">>,
    gate_type => code_coverage,
    threshold => 80,
    actual_value => 85
}, WOSpan),

tcps_simulator_telemetry:end_quality_gate(GateSpan, #{
    passed => true,
    remediation_required => false
}),

%% Record quality gate result
tcps_metrics_collector:record_quality_gate_result(#{
    gate_type => code_coverage,
    passed => true,
    score => 85,
    timestamp => erlang:system_time(millisecond)
}),

%% Complete work order
tcps_simulator_telemetry:end_work_order(WOSpan, #{
    status => completed,
    cycle_time => 3600000,
    sla_met => true,
    quality_score => 95
}),

%% Record completion
tcps_metrics_collector:record_work_order_completed(#{
    bucket => security,
    cycle_time => 3600000,
    sla_met => true,
    quality_score => 95,
    timestamp => erlang:system_time(millisecond)
}),

%% End session
tcps_simulator_telemetry:end_session(SessionSpan, #{
    work_orders_completed => 1,
    quality_gates_passed => 1,
    user_satisfaction => 5
}),

%% Record session completion
tcps_metrics_collector:record_session_complete(#{
    session_id => <<"session-001">>,
    duration => 3600000,
    work_orders_completed => 1,
    user_satisfaction => 5,
    timestamp => erlang:system_time(millisecond)
}),

%% Generate visualizations
KanbanHeatmap = tcps_visualization_data:kanban_heatmap(#{
    window => '1hour',
    buckets => [security, reliability, cost, compliance],
    format => chartjs
}),

QualityFunnel = tcps_visualization_data:quality_gate_funnel(#{
    gates => [code_coverage, test_pass_rate, security_scan],
    format => chartjs
}),

Dashboard = tcps_visualization_data:learning_dashboard(#{
    session_id => <<"session-001">>,
    format => chartjs
}),

%% Export metrics
{ok, Prometheus} = tcps_simulator_telemetry:export_prometheus(),
{ok, Json} = tcps_simulator_telemetry:export_json(),
ok = tcps_simulator_telemetry:export_csv("/tmp/metrics.csv").
```

## Performance

### Benchmarks

- **Span Creation**: < 1ms per span
- **Metric Recording**: < 0.1ms per metric
- **Aggregation**: < 100ms for 10,000 metrics
- **Export**: < 500ms for full dataset

### Optimization

**High-Throughput Recording:**
```erlang
%% Batch metric recording
lists:foreach(fun(WO) ->
    tcps_metrics_collector:record_work_order_created(WO)
end, WorkOrders).
```

**Efficient Querying:**
```erlang
%% Use specific windows
Metrics = tcps_metrics_collector:get_work_order_metrics(#{
    window => '5min',  % Smaller window = faster query
    buckets => [security]  % Filter early
}).
```

## Best Practices

### Span Hierarchy

Always maintain proper span hierarchy:

```erlang
%% ✅ CORRECT: Parent-child relationship
SessionSpan = tcps_simulator_telemetry:start_session(...),
ScenarioSpan = tcps_simulator_telemetry:start_scenario(..., SessionSpan),
WOSpan = tcps_simulator_telemetry:start_work_order(..., ScenarioSpan).

%% ❌ WRONG: No parent relationship
SessionSpan = tcps_simulator_telemetry:start_session(...),
WOSpan = tcps_simulator_telemetry:start_work_order(..., undefined).
```

### Metric Naming

Use consistent naming conventions:

```erlang
%% ✅ CORRECT: Consistent bucket naming
tcps_metrics_collector:record_work_order_created(#{bucket => security}),
tcps_simulator_telemetry:record_wip(security, 3).

%% ❌ WRONG: Inconsistent naming
tcps_metrics_collector:record_work_order_created(#{bucket => "security"}),
tcps_simulator_telemetry:record_wip('Security', 3).
```

### Timestamp Management

Always use consistent timestamps:

```erlang
%% ✅ CORRECT: Consistent timestamp format
Now = erlang:system_time(millisecond),
tcps_metrics_collector:record_work_order_created(#{
    bucket => security,
    timestamp => Now
}).

%% ❌ WRONG: Inconsistent timestamp units
tcps_metrics_collector:record_work_order_created(#{
    bucket => security,
    timestamp => erlang:system_time(second)  % Wrong unit!
}).
```

### Resource Cleanup

Always clean up resources:

```erlang
%% ✅ CORRECT: End spans in reverse order
SessionSpan = tcps_simulator_telemetry:start_session(...),
WOSpan = tcps_simulator_telemetry:start_work_order(..., SessionSpan),

%% Do work...

tcps_simulator_telemetry:end_work_order(WOSpan, ...),
tcps_simulator_telemetry:end_session(SessionSpan, ...).
```

### Error Handling

Always handle errors gracefully:

```erlang
%% ✅ CORRECT: Graceful error handling
case tcps_simulator_telemetry:export_csv("/tmp/metrics.csv") of
    ok ->
        logger:info("Metrics exported successfully");
    {error, Reason} ->
        logger:error("Failed to export metrics: ~p", [Reason])
end.
```

## Testing

Run comprehensive tests:

```bash
# Run all telemetry tests
rebar3 eunit --module=tcps_simulator_telemetry_tests

# Run metrics collector tests
rebar3 eunit --module=tcps_metrics_collector_tests

# Run visualization tests
rebar3 eunit --module=tcps_visualization_data_tests

# Run all tests with coverage
rebar3 as test cover
```

## License

Apache 2.0

## Support

For issues and questions, please open an issue on GitHub.
