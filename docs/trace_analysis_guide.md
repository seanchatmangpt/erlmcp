# Trace Analysis Guide

The `erlmcp_trace_analyzer` provides comprehensive trace analysis capabilities for performance monitoring, anomaly detection, and bottleneck identification in distributed tracing systems.

## Overview

The trace analyzer performs multi-dimensional analysis of trace data including:

- **Critical Path Analysis**: Identifies the longest execution path through a trace
- **Anomaly Detection**: Detects unusual patterns, latency spikes, and structural issues
- **Performance Analysis**: Calculates performance metrics and scores
- **Bottleneck Identification**: Finds performance bottlenecks and provides recommendations
- **Relationship Validation**: Validates span parent-child relationships and timing

## Quick Start

```erlang
% Start the trace analyzer
{ok, _Pid} = erlmcp_trace_analyzer:start_link().

% Analyze a complete trace
{ok, Analysis} = erlmcp_trace_analyzer:analyze_trace(TraceId).

% Find critical path for specific spans
{ok, CriticalPath} = erlmcp_trace_analyzer:find_critical_path(Spans).

% Detect anomalies in spans
{ok, Anomalies} = erlmcp_trace_analyzer:detect_anomalies(Spans).
```

## Core Analysis Functions

### Complete Trace Analysis

```erlang
{ok, Analysis} = erlmcp_trace_analyzer:analyze_trace(TraceId).
```

Returns a comprehensive `#trace_analysis{}` record containing:

```erlang
#trace_analysis{
    trace_id,                   % Trace identifier
    total_duration,             % Total trace duration (μs)
    critical_path = [],         % Span IDs on critical path
    critical_path_duration,     % Duration of critical path (μs)
    span_count,                 % Total number of spans
    error_count,                % Number of error spans
    bottlenecks = [],           % Identified bottlenecks
    anomalies = [],             % Detected anomalies
    performance_score,          % Overall performance score (0-100)
    analysis_timestamp          % When analysis was performed
}
```

### Critical Path Analysis

The critical path represents the longest execution chain through your trace:

```erlang
{ok, CriticalPath} = erlmcp_trace_analyzer:find_critical_path(Spans).
% Returns: ["span1", "span3", "span7", "span12"]
```

**Algorithm**: Uses Directed Acyclic Graph (DAG) traversal with Depth-First Search to find the path with maximum cumulative duration.

**Use Cases**:
- Identify the slowest execution path
- Focus optimization efforts on high-impact spans
- Understand request flow bottlenecks

### Anomaly Detection

Detects various types of anomalies in trace data:

```erlang
{ok, Anomalies} = erlmcp_trace_analyzer:detect_anomalies(Spans).
```

**Anomaly Types**:

1. **Latency Anomalies**: Spans with unusually long durations (>3σ from mean)
2. **Missing Parent Spans**: Child spans referencing non-existent parents
3. **Orphaned Spans**: Spans without end times after timeout period
4. **Circular Dependencies**: Spans forming dependency cycles
5. **Error Clusters**: Groups of error spans occurring close in time
6. **Unusual Patterns**: Excessive depth, suspicious durations

**Example Anomaly**:
```erlang
#{
    type => latency_anomaly,
    span_id => "slow_database_query",
    duration => 45000000,        % 45 seconds
    threshold => 5000000,        % 5 second threshold
    deviation => 8.2,            % 8.2 standard deviations
    severity => critical
}
```

### Performance Analysis

```erlang
{ok, Performance} = erlmcp_trace_analyzer:analyze_performance(TraceId).
```

Returns performance metrics:
```erlang
#{
    score => 78,                 % Performance score (0-100)
    avg_duration => 2500000,     % Average span duration (μs)
    error_rate => 0.05,          % Error rate (5%)
    span_count => 24             % Total spans analyzed
}
```

### Bottleneck Identification

Identifies performance bottlenecks and provides optimization recommendations:

```erlang
TraceId = <<"trace_123">>,
{ok, Analysis} = erlmcp_trace_analyzer:analyze_trace(TraceId),
Bottlenecks = Analysis#trace_analysis.bottlenecks.
```

**Bottleneck Structure**:
```erlang
#{
    type => bottleneck,
    span_id => "database_query",
    operation => "SELECT * FROM users",
    total_duration => 8000000,      % 8 seconds
    self_time => 7500000,           % 7.5 seconds self time
    children_time => 500000,        % 0.5 seconds in children
    child_count => 3,
    bottleneck_score => 94,         % 94% of time in self
    recommendations => [
        "Optimize internal processing in SELECT * FROM users",
        "Consider adding database indexes"
    ]
}
```

### Span Relationship Validation

```erlang
{ok, Validation} = erlmcp_trace_analyzer:validate_span_relationships(Spans).
```

Validates:
- Parent-child consistency
- Time ordering (parent starts before children)
- Duration consistency (parent duration >= children duration)
- Span hierarchy integrity

## Report Generation

Generate detailed analysis reports in multiple formats:

```erlang
% JSON report
{ok, JsonReport} = erlmcp_trace_analyzer:generate_report(TraceId, json).

% Text report
{ok, TextReport} = erlmcp_trace_analyzer:generate_report(TraceId, text).
```

**Sample Text Report**:
```
=== Trace Analysis Report ===
Trace ID: "api_request_456"
Total Duration: 12500000 μs
Span Count: 18
Error Count: 2
Performance Score: 73/100

=== Critical Path ===
Duration: 9200000 μs
Spans: ["http_request", "auth_service", "database_query", "response_format"]

=== Bottlenecks (2 found) ===
- database_query: 8000000 μs (score: 94)
- json_serialization: 1200000 μs (score: 87)

=== Anomalies (3 found) ===
- latency_anomaly: Duration 8000000 μs exceeds threshold (critical)
- error_cluster: 2 errors in cluster (medium)
- missing_parent_span: Missing parent span (high)
```

## Configuration

The analyzer uses configurable thresholds for anomaly detection:

```erlang
Thresholds = #{
    latency_sigma => 3.0,           % Standard deviations for latency anomalies
    error_rate_threshold => 0.05,   % 5% error rate threshold
    min_span_duration => 1000,      % Minimum expected duration (μs)
    max_span_duration => 30000000,  % Maximum expected duration (30s)
    max_depth => 50,                % Maximum span hierarchy depth
    orphan_timeout => 5000000       % Orphan detection timeout (5s)
}
```

## Performance Considerations

### Analysis Performance

The trace analyzer is optimized for large traces:

- **Time Complexity**: O(n log n) for most analyses where n = number of spans
- **Memory Usage**: Linear in number of spans with caching
- **Concurrency**: Thread-safe with gen_server architecture

### Caching

Analysis results are automatically cached to improve performance:

```erlang
% Results cached automatically after first analysis
{ok, Analysis1} = erlmcp_trace_analyzer:analyze_trace(TraceId), % Cache miss
{ok, Analysis2} = erlmcp_trace_analyzer:analyze_trace(TraceId). % Cache hit
```

### Batch Analysis

For analyzing multiple traces efficiently:

```erlang
TraceIds = [<<"trace_1">>, <<"trace_2">>, <<"trace_3">>],
Analyses = [erlmcp_trace_analyzer:analyze_trace(TId) || TId <- TraceIds].
```

## Use Cases

### 1. Performance Monitoring Dashboard

```erlang
monitor_trace_performance(TraceId) ->
    {ok, Analysis} = erlmcp_trace_analyzer:analyze_trace(TraceId),

    % Extract key metrics for dashboard
    Metrics = #{
        performance_score => Analysis#trace_analysis.performance_score,
        total_duration => Analysis#trace_analysis.total_duration,
        error_count => Analysis#trace_analysis.error_count,
        anomaly_count => length(Analysis#trace_analysis.anomalies),
        bottleneck_count => length(Analysis#trace_analysis.bottlenecks)
    },

    % Send to monitoring system
    send_to_dashboard(Metrics).
```

### 2. Automated Alerting

```erlang
check_trace_health(TraceId) ->
    {ok, Analysis} = erlmcp_trace_analyzer:analyze_trace(TraceId),

    % Check for critical issues
    CriticalAnomalies = [A || A <- Analysis#trace_analysis.anomalies,
                             maps:get(severity, A) =:= critical],

    case {Analysis#trace_analysis.performance_score, CriticalAnomalies} of
        {Score, []} when Score > 80 ->
            ok; % Healthy trace
        {Score, []} when Score > 50 ->
            {warning, low_performance};
        {_, CriticalAnomalies} when length(CriticalAnomalies) > 0 ->
            {alert, critical_anomalies};
        _ ->
            {alert, poor_performance}
    end.
```

### 3. Performance Regression Detection

```erlang
detect_performance_regression(CurrentTraceId, BaselineTraceId) ->
    {ok, Current} = erlmcp_trace_analyzer:analyze_trace(CurrentTraceId),
    {ok, Baseline} = erlmcp_trace_analyzer:analyze_trace(BaselineTraceId),

    CurrentScore = Current#trace_analysis.performance_score,
    BaselineScore = Baseline#trace_analysis.performance_score,

    ScoreRegression = BaselineScore - CurrentScore,

    case ScoreRegression of
        Regression when Regression > 20 ->
            {regression_detected, critical, ScoreRegression};
        Regression when Regression > 10 ->
            {regression_detected, moderate, ScoreRegression};
        _ ->
            no_regression
    end.
```

## Integration with Tracing System

The analyzer integrates seamlessly with the erlmcp tracing system:

```erlang
% Analysis automatically creates analysis spans for traceability
{ok, Analysis} = erlmcp_trace_analyzer:analyze_trace(TraceId),

% Analysis span is automatically added to trace with tags:
% - component: "erlmcp_trace_analyzer"
% - analysis_version: "1.0"
% - performance_score: Analysis score
```

## Best Practices

### 1. Regular Analysis

Set up periodic analysis of completed traces:

```erlang
schedule_trace_analysis() ->
    % Analyze traces older than 5 minutes
    Cutoff = erlang:system_time(microsecond) - 300000000,
    CompletedTraces = erlmcp_storage:get_completed_traces_before(Cutoff),

    [erlmcp_trace_analyzer:analyze_trace(TId) || TId <- CompletedTraces].
```

### 2. Threshold Tuning

Adjust anomaly detection thresholds based on your application characteristics:

```erlang
% For high-performance applications
HighPerfThresholds = #{
    latency_sigma => 2.0,           % More sensitive
    max_span_duration => 1000000,   % 1 second max
    error_rate_threshold => 0.01    % 1% error threshold
}.

% For batch processing applications
BatchThresholds = #{
    latency_sigma => 4.0,           % Less sensitive
    max_span_duration => 300000000, % 5 minutes max
    max_depth => 100                % Deeper hierarchies OK
}.
```

### 3. Custom Analysis

Extend the analyzer for domain-specific analysis:

```erlang
analyze_database_performance(TraceId) ->
    {ok, Analysis} = erlmcp_trace_analyzer:analyze_trace(TraceId),

    % Filter for database-related bottlenecks
    DbBottlenecks = [B || B <- Analysis#trace_analysis.bottlenecks,
                         is_database_operation(maps:get(operation, B))],

    % Custom database performance metrics
    calculate_db_metrics(DbBottlenecks).

is_database_operation(Operation) ->
    DbKeywords = ["SELECT", "INSERT", "UPDATE", "DELETE", "QUERY"],
    lists:any(fun(Keyword) ->
        string:find(Operation, Keyword) =/= nomatch
    end, DbKeywords).
```

## Troubleshooting

### Common Issues

1. **No Critical Path Found**: Usually indicates disconnected spans or missing parent relationships
2. **False Positive Anomalies**: Adjust sigma thresholds or duration limits
3. **Missing Bottlenecks**: Ensure spans have accurate duration measurements
4. **High Memory Usage**: Enable result caching limits or analyze smaller trace batches

### Debug Mode

Enable detailed logging for troubleshooting:

```erlang
% Set log level to debug
application:set_env(kernel, logger_level, debug).

% Analyze with detailed logging
{ok, Analysis} = erlmcp_trace_analyzer:analyze_trace(TraceId).
```

## API Reference

### Core Functions

- `analyze_trace/1` - Complete trace analysis
- `find_critical_path/1` - Critical path identification
- `detect_anomalies/1` - Anomaly detection
- `analyze_performance/1` - Performance metrics calculation
- `validate_span_relationships/1` - Relationship validation
- `generate_report/2` - Report generation

### Configuration

- Anomaly detection thresholds
- Performance scoring parameters
- Caching settings
- Logging configuration

### Data Structures

- `#trace_analysis{}` - Complete analysis results
- `#span_analysis{}` - Individual span analysis
- Anomaly maps with type, severity, and details
- Bottleneck maps with scores and recommendations

## Future Enhancements

Planned features for future versions:

1. **Machine Learning Integration**: ML-based anomaly detection
2. **Trend Analysis**: Long-term performance trend detection
3. **Comparative Analysis**: Multi-trace comparison and regression detection
4. **Real-time Analysis**: Streaming analysis of active traces
5. **Custom Metrics**: User-defined performance metrics and scoring
6. **Integration APIs**: REST/GraphQL APIs for external tool integration
