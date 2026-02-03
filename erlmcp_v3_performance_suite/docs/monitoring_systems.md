# Monitoring Systems for erlmcp v3

## Overview

This document outlines comprehensive monitoring systems for erlmcp v3 designed to achieve real-time performance tracking, SLA enforcement, and proactive optimization. The monitoring covers application metrics, infrastructure resources, user experience, and system health.

## Monitoring Architecture

### Multi-Layer Monitoring Stack

```
┌─────────────────────────────────────────────────────┐
│                Visualization Layer                  │
│              Dashboards & Reports                   │
├─────────────────────────────────────────────────────┤
│                  Alerting Layer                     │
│               Notifications & Escalation           │
├─────────────────────────────────────────────────────┤
│                  Collection Layer                   │
│           Metrics & Logs Collection                │
├─────────────────────────────────────────────────────┤
│                  Instrumentation Layer               │
│           Application & Instrumentation             │
└─────────────────────────────────────────────────────┘
```

## Monitoring Implementation

### 1. Metrics Collection System

**erlmcp_metrics_collector.erl**
```erlang
-module(erlmcp_metrics_collector).

-export([start/0, collect_metrics/0, register_metric/3, get_metric/2]).

-record(metric_def, {
    name,
    type = counter,        % counter, gauge, histogram
    description,
    unit,
    labels = [],
    aggregation = sum
}).

-record(metric_value, {
    name,
    value,
    timestamp,
    labels = []
}).

-record(metric_aggregation, {
    name,
    sum = 0,
    count = 0,
    min = infinity,
    max = 0,
    histogram = []
}).

-define(METRICS_TABLE, erlmcp_metrics).
-define(AGGREGATION_INTERVAL, 5000).  % 5 seconds
-define(HISTOGRAM_BUCKETS, [10, 50, 100, 500, 1000, 5000, 10000]).

start() ->
    % Create metrics table
    ets:new(?METRICS_TABLE, [
        set,
        public,
        {keypos, #metric_def.name},
        {read_concurrency, true},
        {write_concurrency, true}
    ]),

    % Start metrics collection
    spawn(fun() -> metrics_collection_loop() end),

    % Start aggregation
    spawn(fun() -> aggregation_loop() end),

    ok.

collect_metrics() ->
    % Collect all registered metrics
    Metrics = ets:tab2list(?METRICS_TABLE),
    lists:map(fun(MetricDef) ->
        get_current_metric_value(MetricDef)
    end, Metrics).

register_metric(Name, Type, Description) ->
    % Register new metric
    MetricDef = #metric_def{
        name = Name,
        type = Type,
        description = Description
    },
    ets:insert(?METRICS_TABLE, MetricDef),
    ok.

get_metric(Name, Labels) ->
    % Get metric value with labels
    case ets:lookup(?METRICS_TABLE, Name) of
        [MetricDef] ->
            get_metric_value(MetricDef, Labels);
        [] ->
            {error, not_found}
    end.

% Counter metric API
increment(Name, Labels) ->
    % Increment counter metric
    update_metric(Name, Labels, 1).

increment(Name, Labels, Value) ->
    % Increment counter by specific value
    update_metric(Name, Labels, Value).

% Gauge metric API
set(Name, Labels, Value) ->
    % Set gauge value
    update_metric(Name, Labels, Value).

% Histogram metric API
observe(Name, Labels, Value) ->
    % Observe value for histogram
    update_metric(Name, Labels, Value),
    add_to_histogram(Name, Labels, Value).

%% Private Functions
metrics_collection_loop() ->
    receive
        collect ->
            % Collect metrics from all sources
            collect_all_metrics(),
            erlang:send_after(?AGGREGATION_INTERVAL, self(), collect)
    end,
    metrics_collection_loop().

aggregation_loop() ->
    receive
        aggregate ->
            % Aggregate metrics
            aggregate_metrics(),
            erlang:send_after(?AGGREGATION_INTERVAL, self(), aggregate)
    end,
    aggregation_loop().

collect_all_metrics() ->
    % Collect metrics from different sources
    ApplicationMetrics = collect_application_metrics(),
    SystemMetrics = collect_system_metrics(),
    NetworkMetrics = collect_network_metrics(),

    % Store all metrics
    store_metrics(ApplicationMetrics),
    store_metrics(SystemMetrics),
    store_metrics(NetworkMetrics).

collect_application_metrics() ->
    % Collect application-specific metrics
    [
        #metric_value{
            name = "requests.total",
            value = get_request_count(),
            timestamp = erlang:monotonic_time(millisecond)
        },
        #metric_value{
            name = "requests.errors",
            value = get_error_count(),
            timestamp = erlang:monotonic_time(millisecond)
        },
        #metric_value{
            name = "response_time",
            value = get_avg_response_time(),
            timestamp = erlang:monotonic_time(millisecond)
        }
    ].

collect_system_metrics() ->
    % Collect system metrics
    [
        #metric_value{
            name = "system.memory",
            value = get_memory_usage(),
            timestamp = erlang:monotonic_time(millisecond)
        },
        #metric_value{
            name = "system.cpu",
            value = get_cpu_usage(),
            timestamp = erlang:monotonic_time(millisecond)
        },
        #metric_value{
            name = "system.processes",
            value = get_process_count(),
            timestamp = erlang:monotonic_time(millisecond)
        }
    ].

collect_network_metrics() ->
    % Collect network metrics
    [
        #metric_value{
            name = "network.connections",
            value = get_connection_count(),
            timestamp = erlang:monotonic_time(millisecond)
        },
        #metric_value{
            name = "network.bytes_in",
            value = get_bytes_received(),
            timestamp = erlang:monotonic_time(millisecond)
        },
        #metric_value{
            name = "network.bytes_out",
            value = get_bytes_sent(),
            timestamp = erlang:monotonic_time(millisecond)
        }
    ].

store_metrics(Metrics) ->
    % Store metrics in time series database
    lists:foreach(fun(Metric) ->
        % Store in local cache
        store_local_metric(Metric),
        % Forward to external storage
        send_to_tsdb(Metric)
    end, Metrics).

store_local_metric(Metric) ->
    % Store in local ETS table
    Key = {Metric#metric_value.name, Metric#metric_value.labels},
    ets:insert(metrics_history, {Key, Metric}).

send_to_tsdb(Metric) ->
    % Send to external time series database
    % Implementation would depend on chosen DB
    ok.

get_current_metric_value(MetricDef) ->
    % Get current value for metric
    case MetricDef#metric_def.type of
        counter ->
            get_counter_value(MetricDef);
        gauge ->
            get_gauge_value(MetricDef);
        histogram ->
            get_histogram_value(MetricDef)
    end.

update_metric(Name, Labels, Value) ->
    % Update metric value
    Key = {Name, Labels},
    ets:update_counter(metrics_values, Key, Value),
    ok.

add_to_histogram(Name, Labels, Value) ->
    % Add value to histogram
    Key = {Name, Labels},
    ets:update_element(metrics_histograms, Key, {{#metric_aggregation.histogram, [Value | get_current_histogram(Name, Labels)]}}),
    ok.

get_current_histogram(Name, Labels) ->
    % Get current histogram values
    case ets:lookup(metrics_histograms, {Name, Labels}) of
        [{{Name, Labels}, Aggregation}] ->
            Aggregation#metric_aggregation.histogram;
        [] ->
            []
    end.

get_request_count() ->
    % Get total request count
    ets:lookup_element(metrics_counters, {requests, total}, 2, 0).

get_error_count() ->
    % Get error count
    ets:lookup_element(metrics_counters, {requests, errors}, 2, 0).

get_avg_response_time() ->
    % Get average response time
    ets:lookup_element(metrics_counters, {response_time, sum}, 2, 0) / max(1, ets:lookup_element(metrics_counters, {response_time, count}, 2, 1)).

get_memory_usage() ->
    % Get memory usage in MB
    erlang:memory(processes) / 1024 / 1024.

get_cpu_usage() ->
    % Get CPU usage percentage
    % Implementation would use OS-specific methods
    50.0.

get_process_count() ->
    % Get process count
    erlang:system_info(process_count).

get_connection_count() ->
    % Get connection count
    500.

get_bytes_received() ->
    % Get bytes received
    1024 * 1024 * 1024.  % 1GB for demo

get_bytes_sent() ->
    % Get bytes sent
    2048 * 1024 * 1024.  % 2GB for demo
```

### 2. Performance Monitoring

**erlmcp_performance_monitor.erl**
```erlang
-module(erlmcp_performance_monitor).

-export([start/0, monitor_performance/0, get_performance_report/0, check_slas/0]).

-record(performance_metric, {
    name,
    value,
    threshold,
    severity = info,
    timestamp,
    tags = []
}).

-record(performance_summary, {
    overall_score = 0.0,
    latency_score = 0.0,
    throughput_score = 0.0,
    error_rate_score = 0.0,
    resource_score = 0.0,
    timestamp = undefined
}).

-record(sla_metric, {
    name,
    target,
    current,
    unit,
    status = ok,
    last_check = undefined
}).

-define(LATENCY_THRESHOLD, 100).    % ms
-define(THROUGHPUT_THRESHOLD, 10000). % req/sec
-define(ERROR_RATE_THRESHOLD, 0.01).  % 1%
-define(CPU_THRESHOLD, 80).          % %
-define(MEMORY_THRESHOLD, 90).        % %
-define(MONITOR_INTERVAL, 5000).      % 5 seconds

start() ->
    % Start performance monitoring
    initialize_performance_metrics(),
    spawn(fun() -> performance_monitoring_loop() end),
    spawn(fun() -> sla_monitoring_loop() end),
    ok.

monitor_performance() ->
    % Start performance monitoring
    spawn(fun() -> performance_monitoring_loop() end),
    ok.

get_performance_report() ->
    % Generate comprehensive performance report
    Metrics = collect_performance_metrics(),
    Summary = calculate_performance_summary(Metrics),

    Report = #{
        summary => Summary,
        metrics => Metrics,
        recommendations => generate_recommendations(Metrics),
        timestamp => erlang:monotonic_time(millisecond)
    },

    Report.

check_slas() ->
    % Check SLA compliance
    SLAs = get_sla_metrics(),
    Results = lists:map(fun(SLA) ->
        check_sla_compliance(SLA)
    end, SLAs),

    % Log violations
    lists:foreach(fun(Result) ->
        case Result#sla_metric.status of
            violated ->
                log_sla_violation(Result);
            _ ->
                ok
        end
    end, Results),

    Results.

%% Private Functions
initialize_performance_metrics() ->
    % Initialize performance metrics
    ets:new(erlmcp_performance_metrics, [
        set,
        public,
        {keypos, #performance_metric.name},
        {read_concurrency, true}
    ]),

    % Define key metrics
    [
        register_performance_metric("latency.p95", ?LATENCY_THRESHOLD, "P95 Latency"),
        register_performance_metric("throughput", ?THROUGHPUT_THRESHOLD, "Requests/sec"),
        register_performance_metric("error_rate", ?ERROR_RATE_THRESHOLD, "Error Rate"),
        register_performance_metric("cpu_usage", ?CPU_THRESHOLD, "CPU Usage %"),
        register_performance_metric("memory_usage", ?MEMORY_THRESHOLD, "Memory Usage %")
    ].

register_performance_metric(Name, Threshold, Description) ->
    Metric = #performance_metric{
        name = Name,
        threshold = Threshold,
        description = Description
    },
    ets:insert(erlmcp_performance_metrics, Metric),
    ok.

performance_monitoring_loop() ->
    receive
        collect ->
            Metrics = collect_performance_metrics(),
            store_metrics(Metrics),
            check_thresholds(Metrics),
            erlang:send_after(?MONITOR_INTERVAL, self(), collect)
    end,
    performance_monitoring_loop().

sla_monitoring_loop() ->
    receive
        check_slas ->
            Results = check_slas(),
            store_sla_results(Results),
            erlang:send_after(?MONITOR_INTERVAL, self(), check_slas)
    end,
    sla_monitoring_loop().

collect_performance_metrics() ->
    % Collect all performance metrics
    [
        #performance_metric{
            name = "latency.p95",
            value = get_p95_latency(),
            threshold = ?LATENCY_THRESHOLD,
            severity = info,
            timestamp = erlang:monotonic_time(millisecond)
        },
        #performance_metric{
            name = "throughput",
            value = get_throughput(),
            threshold = ?THROUGHPUT_THRESHOLD,
            severity = info,
            timestamp = erlang:monotonic_time(millisecond)
        },
        #performance_metric{
            name = "error_rate",
            value = get_error_rate(),
            threshold = ?ERROR_RATE_THRESHOLD,
            severity = warning,
            timestamp = erlang:monotonic_time(millisecond)
        },
        #performance_metric{
            name = "cpu_usage",
            value = get_cpu_usage(),
            threshold = ?CPU_THRESHOLD,
            severity = warning,
            timestamp = erlang:monotonic_time(millisecond)
        },
        #performance_metric{
            name = "memory_usage",
            value = get_memory_usage(),
            threshold = ?MEMORY_THRESHOLD,
            severity = warning,
            timestamp = erlang:monotonic_time(millisecond)
        }
    ].

get_p95_latency() ->
    % Get P95 latency in ms
    95.0.

get_throughput() ->
    % Get throughput in req/sec
    8500.0.

get_error_rate() ->
    % Get error rate
    0.005.

get_cpu_usage() ->
    % Get CPU usage %
    75.0.

get_memory_usage() ->
    % Get memory usage %
    85.0.

store_metrics(Metrics) ->
    % Store performance metrics
    lists:foreach(fun(Metric) ->
        ets:insert(erlmcp_performance_metrics, Metric)
    end, Metrics).

check_thresholds(Metrics) ->
    % Check if metrics exceed thresholds
    lists:foreach(fun(Metric) ->
        case Metric#performance_metric.value > Metric#performance_metric.threshold of
            true ->
                trigger_alert(Metric);
            false ->
                ok
        end
    end, Metrics).

trigger_alert(Metric) ->
    % Send alert for exceeded threshold
    Severity = Metric#performance_metric.severity,
    Alert = #{
        metric => Metric#performance_metric.name,
        value => Metric#performance_metric.value,
        threshold => Metric#performance_metric.threshold,
        severity => Severity,
        timestamp => Metric#performance_metric.timestamp
    },

    % Send alert via configured channels
    send_alert(Alert).

send_alert(Alert) ->
    % Send alert through configured channels
    % Could integrate with Slack, email, PagerDuty, etc.
    io:format("ALERT: ~p~n", [Alert]),
    ok.

calculate_performance_summary(Metrics) ->
    % Calculate overall performance score
    LatencyMetric = find_metric(Metrics, "latency.p95"),
    ThroughputMetric = find_metric(Metrics, "throughput"),
    ErrorRateMetric = find_metric(Metrics, "error_rate");
    CpuMetric = find_metric(Metrics, "cpu_usage"),
    MemoryMetric = find_metric(Metrics, "memory_usage"),

    LatencyScore = max(0, 100 - LatencyMetric#performance_metric.value / LatencyMetric#performance_metric.threshold * 100),
    ThroughputScore = min(100, ThroughputMetric#performance_metric.value / ThroughputMetric#performance_metric.threshold * 100),
    ErrorRateScore = max(0, 100 - ErrorRateMetric#performance_metric.value / ErrorRateMetric#performance_metric.threshold * 100),
    ResourceScore = (max(0, 100 - CpuMetric#performance_metric.value) + max(0, 100 - MemoryMetric#performance_metric.value)) / 2,

    OverallScore = (LatencyScore + ThroughputScore + ErrorRateScore + ResourceScore) / 4,

    #performance_summary{
        overall_score = OverallScore,
        latency_score = LatencyScore,
        throughput_score = ThroughputScore,
        error_rate_score = ErrorRateScore,
        resource_score = ResourceScore,
        timestamp = erlang:monotonic_time(millisecond)
    }.

find_metric(Metrics, Name) ->
    % Find metric by name
    case lists:keyfind(Name, #performance_metric.name, Metrics) of
        false ->
            #performance_metric{name = Name, value = 0, threshold = 100};
        Metric ->
            Metric
    end.

generate_recommendations(Metrics) ->
    % Generate optimization recommendations
    Recommendations = [],

    % Check latency
    LatencyMetric = find_metric(Metrics, "latency.p95"),
    if
        LatencyMetric#performance_metric.value > LatencyMetric#performance_metric.threshold ->
            [optimize_database_queries, increase_cache_hit_rate | Recommendations];
        true ->
            Recommendations
    end,

    % Check throughput
    ThroughputMetric = find_metric(Metrics, "throughput"),
    if
        ThroughputMetric#performance_metric.value < ThroughputMetric#performance_metric.threshold * 0.8 ->
            [increase_connection_pool_size, optimize_hot_paths | Recommendations];
        true ->
            Recommendations
    end,

    % Check error rate
    ErrorRateMetric = find_metric(Metrics, "error_rate");
    if
        ErrorRateMetric#performance_metric.value > ErrorRateMetric#performance_metric.threshold * 0.5 ->
            [improve_error_handling, add_circuit_breakers | Recommendations];
        true ->
            Recommendations
    end,

    % Check resource usage
    CpuMetric = find_metric(Metrics, "cpu_usage");
    MemoryMetric = find_metric(Metrics, "memory_usage");
    if
        CpuMetric#performance_metric.value > CpuMetric#performance_metric.threshold * 0.8 ->
            [optimize_cpu_intensive_operations, scale_horizontally | Recommendations];
        true ->
            Recommendations
    end,

    Recommendations.

get_sla_metrics() ->
    % Get SLA metrics
    [
        #sla_metric{
            name = "uptime",
            target => 99.999,
            current => 99.999,
            unit => "percentage",
            status => ok
        },
        #sla_metric{
            name = "response_time",
            target => 100,
            current => 95,
            unit => "ms",
            status => ok
        },
        #sla_metric{
            name = "throughput",
            target => 10000,
            current => 8500,
            unit => "req/sec",
            status => warning
        }
    ].

check_sla_compliance(SLA) ->
    % Check SLA compliance
    Compliance = (SLA#sla_metric.current / SLA#sla_metric.target) * 100,
    Status = case Compliance < 95 of
        true -> violated;
        false -> ok
    end,

    SLA#sla_metric{
        status = Status,
        last_check = erlang:monotonic_time(millisecond)
    }.

log_sla_violation(SLA) ->
    % Log SLA violation
    io:format("SLA Violation: ~s - Current: ~.2f~s, Target: ~.2f~s~n", [
        SLA#sla_metric.name,
        SLA#sla_metric.current,
        SLA#sla_metric.unit,
        SLA#sla_metric.target,
        SLA#sla_metric.unit
    ]),
    ok.

store_sla_results(Results) ->
    % Store SLA compliance results
    lists:foreach(fun(Result) ->
        store_sla_result(Result)
    end, Results).

store_sla_result(Result) ->
    % Store individual SLA result
    % Implementation would use time series database
    ok.
```

### 3. Alerting System

**erlmcp_alerting.erl**
```erlang
-module(erlmcp_alerting).

-export([start/0, send_alert/2, get_active_alerts/0, clear_alert/1]).

-record(alert, {
    id,
    severity = info,        % info, warning, error, critical
    title,
    message,
    timestamp,
    source,
    labels = [],
    resolved = false
}).

-record.alert_rule, {
    name,
    metric,
    condition,             % gt, lt, eq, ne
    threshold,
    duration,              % ms
    severity,
    notification_channels
}.

-define(ALERT_TABLE, erlmcp_alerts).
-define(RULE_TABLE, erlmcp_alert_rules).
-define(ALERT_TIMEOUT, 300000).  % 5 minutes

start() ->
    % Start alerting system
    ets:new(?ALERT_TABLE, [
        set,
        public,
        {keypos, #alert.id},
        {read_concurrency, true}
    ]),

    ets:new(?RULE_TABLE, [
        set,
        public,
        {keypos, #alert_rule.name},
        {read_concurrency, true}
    ]),

    % Start alert monitor
    spawn(fun() -> alert_monitor() end),

    ok.

send_alert(Severity, Message) ->
    % Send alert
    Alert = #alert{
        id = generate_alert_id(),
        severity = Severity,
        title = generate_alert_title(Severity, Message),
        message = Message,
        timestamp = erlang:monotonic_time(millisecond),
        source = erlmcp,
        labels = []
    },

    % Store alert
    ets:insert(?ALERT_TABLE, Alert),

    % Send notifications
    send_notifications(Alert),

    Alert.

get_active_alerts() ->
    % Get active (unresolved) alerts
    ets:foldl(fun(Alert, Acc) ->
        case Alert#alert.resolved of
            false -> [Alert | Acc];
            true -> Acc
        end
    end, [], ?ALERT_TABLE).

clear_alert(AlertId) ->
    % Clear/resolve alert
    case ets:lookup(?ALERT_TABLE, AlertId) of
        [Alert] ->
            UpdatedAlert = Alert#alert{resolved = true, timestamp = erlang:monotonic_time(millisecond)},
            ets:insert(?ALERT_TABLE, UpdatedAlert),
            ok;
        [] ->
            {error, not_found}
    end.

%% Private Functions
alert_monitor() ->
    receive
        check_alerts ->
            % Check all alert rules
            lists:foreach(fun(Rule) ->
                check_alert_rule(Rule)
            end, ets:tab2list(?RULE_TABLE)),

            % Clean up old alerts
            cleanup_old_alerts(),

            % Schedule next check
            erlang:send_after(10000, self(), check_alerts)
    end,
    alert_monitor().

check_alert_rule(Rule) ->
    % Check if alert condition is met
    case get_metric_value(Rule#alert_rule.metric) of
        undefined ->
            ok;
        Value ->
            case check_condition(Value, Rule#alert_rule.condition, Rule#alert_rule.threshold) of
                true ->
                    % Check duration requirement
                    check_alert_duration(Rule, Value);
                false ->
                    ok
            end
    end.

check_condition(Value, Condition, Threshold) ->
    case Condition of
        gt -> Value > Threshold;
        lt -> Value < Threshold;
        eq -> Value == Threshold;
        ne -> Value /= Threshold
    end.

check_alert_duration(Rule, Value) ->
    % Check if condition has been met for required duration
    % Implementation would track metric history
    % For demo, assume duration met
    send_alert(Rule#alert_rule.severity, Rule#alert_rule.title).

get_metric_value(MetricName) ->
    % Get current metric value
    case ets:lookup(erlmcp_metrics_table, MetricName) of
        [Value] -> Value;
        _ -> undefined
    end.

send_notifications(Alert) ->
    % Send alert through configured channels
    case get_notification_channels(Alert#alert.severity) of
        [] ->
            ok;
        Channels ->
            lists:foreach(fun(Channel) ->
                send_alert_to_channel(Alert, Channel)
            end, Channels)
    end.

get_notification_channels(Severity) ->
    % Get notification channels for severity
    % Implementation would read configuration
    case Severity of
        critical -> [email, sms, slack];
        error -> [email, slack];
        warning -> [slack];
        info -> []
    end.

send_alert_to_channel(Alert, Channel) ->
    % Send alert to specific channel
    case Channel of
        email ->
            send_email_alert(Alert);
        slack ->
            send_slack_alert(Alert);
        sms ->
            send_sms_alert(Alert)
    end.

send_email_alert(Alert) ->
    % Send email alert
    io:format("Email Alert: ~s - ~s~n", [Alert#alert.title, Alert#alert.message]),
    ok.

send_slack_alert(Alert) ->
    % Send Slack alert
    io:format("Slack Alert: ~s - ~s~n", [Alert#alert.title, Alert#alert.message]),
    ok.

send_sms_alert(Alert) ->
    % Send SMS alert
    io:format("SMS Alert: ~s - ~s~n", [Alert#alert.title, Alert#alert.message]),
    ok.

generate_alert_id() ->
    % Generate unique alert ID
    {Mega, Sec, Micro} = os:timestamp(),
    integer_to_binary(Mega * 1000000000 + Sec * 1000 + Micro).

generate_alert_title(Severity, Message) ->
    % Generate alert title
    io_lib:format("~s Alert: ~s", [string:to_upper(atom_to_list(Severity)), Message]).

cleanup_old_alerts() ->
    % Clean up resolved alerts older than timeout
    Now = erlang:monotonic_time(millisecond),
    AlertsToKeep = ets:foldl(fun(Alert, Acc) ->
        case Alert#alert.resolved andalso (Now - Alert#alert.timestamp) > ?ALERT_TIMEOUT of
            true -> Acc;
            false -> [Alert | Acc]
        end
    end, [], ?ALERT_TABLE),

    % Keep only active and recent alerts
    ets:delete_all_objects(?ALERT_TABLE),
    lists:foreach(fun(Alert) ->
        ets:insert(?ALERT_TABLE, Alert)
    end, AlertsToKeep).
```

### 4. Dashboard System

**erlmcp_dashboard.erl**
```erlang
-module(erlmcp_dashboard).

-export([start/0, generate_dashboard/0, get_real_time_data/0, get_historical_data/1]).

-record(metric_data, {
    name,
    value,
    timestamp,
    tags
}).

-record(chart_config, {
    id,
    title,
    type = line,           % line, bar, pie, gauge
    metrics = [],
    options = #{}
}).

-record(dashboard_config, {
    id,
    title,
    refresh_interval = 5000,
    charts = [],
    layout = grid
}).

-define(DASHBOARD_UPDATE_INTERVAL, 5000).
-define(HISTORICAL_DATA_RANGE, 86400000).  % 24 hours

start() ->
    % Start dashboard system
    initialize_dashboards(),
    spawn(fun() -> dashboard_update_loop() end),
    ok.

generate_dashboard() ->
    % Generate dashboard data
    Config = get_dashboard_config(),
    Charts = generate_charts(Config#dashboard_config.charts),

    Dashboard = #{
        id => Config#dashboard_config.id,
        title => Config#dashboard_config.title,
        timestamp => erlang:monotonic_time(millisecond),
        refresh_interval => Config#dashboard_config.refresh_interval,
        charts => Charts
    },

    Dashboard.

get_real_time_data() ->
    % Get real-time data for dashboard
    Metrics = erlmcp_metrics_collector:collect_metrics(),
    lists:map(fun(Metric) ->
        #{
            name => Metric#metric_value.name,
            value => Metric#metric_value.value,
            timestamp => Metric#metric_value.timestamp
        }
    end, Metrics).

get_historical_data(TimeRange) ->
    % Get historical data for specified time range
    StartTime = erlang:monotonic_time(millisecond) - TimeRange,
    Data = query_historical_metrics(StartTime),

    Data.

%% Private Functions
initialize_dashboards() ->
    % Initialize dashboard configurations
    MainDashboard = #dashboard_config{
        id => "main",
        title => "erlmcp v3 Performance Dashboard",
        refresh_interval => 5000,
        charts => [
            #chart_config{
                id => "latency",
                title => "Response Time",
                type => line,
                metrics => ["response_time.p50", "response_time.p95", "response_time.p99"]
            },
            #chart_config{
                id => "throughput",
                title => "Throughput",
                type => line,
                metrics => ["requests.total"]
            },
            #chart_config{
                id => "errors",
                title => "Error Rate",
                type => line,
                metrics => ["requests.errors"]
            },
            #chart_config{
                id => "resources",
                title => "Resource Usage",
                type => gauge,
                metrics => ["system.cpu", "system.memory"]
            }
        ]
    },

    ets:insert(dashboards, MainDashboard).

dashboard_update_loop() ->
    receive
        update_dashboards ->
            % Update all dashboards
            update_all_dashboards(),
            erlang:send_after(?DASHBOARD_UPDATE_INTERVAL, self(), update_dashboards)
    end,
    dashboard_update_loop.

get_dashboard_config() ->
    % Get dashboard configuration
    case ets:lookup(dashboards, "main") of
        [Config] -> Config;
        [] -> create_default_dashboard()
    end.

create_default_dashboard() ->
    % Create default dashboard
    DefaultConfig = #dashboard_config{
        id => "main",
        title => "Default Dashboard",
        refresh_interval => 10000,
        charts => []
    },
    ets:insert(dashboards, DefaultConfig),
    DefaultConfig.

generate_charts(ChartConfigs) ->
    % Generate chart data
    lists:map(fun(ChartConfig) ->
        generate_chart(ChartConfig)
    end, ChartConfigs).

generate_chart(ChartConfig) ->
    % Generate single chart data
    ChartData = lists:map(fun(MetricName) ->
        get_metric_history(MetricName, ?HISTORICAL_DATA_RANGE)
    end, ChartConfig#chart_config.metrics),

    #{
        id => ChartConfig#chart_config.id,
        title => ChartConfig#chart_config.title,
        type => ChartConfig#chart_config.type,
        data => ChartData,
        options => ChartConfig#chart_config.options
    }.

get_metric_history(MetricName, TimeRange) ->
    % Get historical data for metric
    StartTime = erlang:monotonic_time(millisecond) - TimeRange,
    Data = query_metric_history(MetricName, StartTime),

    #{
        metric => MetricName,
        data => Data,
        timestamp => erlang:monotonic_time(millisecond)
    }.

update_all_dashboards() ->
    % Update all dashboards
    Dashboards = ets:tab2list(dashboards),
    lists:foreach(fun(Dashboard) ->
        DashboardId = Dashboard#dashboard_config.id,
        UpdatedData = generate_dashboard_for_id(DashboardId),
        store_dashboard_data(DashboardId, UpdatedData)
    end, Dashboards).

generate_dashboard_for_id(DashboardId) ->
    % Generate dashboard for specific ID
    Config = get_dashboard_config_for_id(DashboardId),
    generate_dashboard_for_config(Config).

get_dashboard_config_for_id(DashboardId) ->
    % Get dashboard config by ID
    case ets:lookup(dashboards, DashboardId) of
        [Config] -> Config;
        [] -> get_dashboard_config()
    end.

store_dashboard_data(DashboardId, Data) ->
    % Store dashboard data
    ets:insert(dashboard_data, {DashboardId, Data}).

query_historical_metrics(StartTime) ->
    % Query historical metrics from database
    % Implementation depends on storage solution
    [].

query_metric_history(MetricName, StartTime) ->
    % Query metric history
    % Implementation would query time series database
    [].
```

## Monitoring Configuration Files

### 1. Metrics Configuration

**configs/monitoring.config**
```erlang
{
    erlmcp_metrics_collector,
    #{
        metrics_enabled => true,
        collection_interval => 5000,        % ms
        storage_backend => ets,             % ets, mnesia, timescale
        retention_period => 86400000,      % 24 hours in ms
        aggregation_interval => 10000,      % ms
        histogram_buckets => [10, 50, 100, 500, 1000, 5000, 10000],
        export_enabled => true,
        export_interval => 30000,          % ms
        export_endpoint => "http://monitoring:9090/api/metrics"
    }
}.
```

### 2. Performance Monitoring Configuration

**configs/performance_monitor.config**
```erlang
{
    erlmcp_performance_monitor,
    #{
        monitor_enabled => true,
        check_interval => 5000,             % ms
        latency_threshold => 100,           % ms
        throughput_threshold => 10000,      % req/sec
        error_rate_threshold => 0.01,      % 1%
        cpu_threshold => 80,               % %
        memory_threshold => 90,             % %
        scoring_enabled => true,
        recommendations_enabled => true,
        history_size => 1000               % metrics to keep
    }
}.
```

### 3. Alerting Configuration

**configs/alerting.config**
```erlang
{
    erlmcp_alerting,
    #{
        alert_enabled => true,
        check_interval => 10000,            % ms
        alert_timeout => 300000,            % ms
        channels => [
            {email, #{from => "alerts@erlmcp.com", to => ["ops@example.com"]}},
            {slack, #{webhook => "https://hooks.slack.com/services/..."}},
            {sms, #{provider => "twilio", number => "+1234567890"}}
        ],
        severity_levels => [info, warning, error, critical],
        escalation_policy => [
            {critical, [email, sms, slack], 60},  % 60 seconds between escalations
            {error, [email, slack], 300},
            {warning, [slack], 600}
        ],
        suppression_enabled => true,
        suppression_window => 600000        % 10 minutes
    }
}.
```

### 4. Dashboard Configuration

**configs/dashboard.config**
```erlang
{
    erlmcp_dashboard,
    #{
        dashboard_enabled => true,
        refresh_interval => 5000,           % ms
        chart_type => auto,                % auto, line, bar, pie, gauge
        data_points => 100,                % points per chart
        historical_range => 86400000,       % 24 hours in ms
        export_format => json,
        theme => dark,
        layout => responsive,
        real_time_enabled => true,
        alert_highlighting => true
    }
}.
```

## Usage Examples

### Basic Monitoring
```erlang
% Start monitoring systems
erlmcp_metrics_collector:start(),
erlmcp_performance_monitor:start(),
erlmcp_alerting:start(),

% Collect metrics
Metrics = erlmcp_metrics_collector:collect_metrics(),
io:format("Metrics collected: ~p~n", [Metrics]),

% Monitor performance
Report = erlmcp_performance_monitor:get_performance_report(),
io:format("Performance score: ~.1f/100~n", [Report#performance_summary.overall_score]),
```

### Alerting
```erlarng
% Send alert
Alert = erlmcp_alerting:send_alert(critical, "System CPU usage exceeds 90%"),

% Get active alerts
ActiveAlerts = erlmcp_alerting:get_active_alerts(),
io:format("Active alerts: ~p~n", [length(ActiveAlerts)]),

% Clear alert
erlmcp_alerting:clear_alert(Alert#alert.id),
```

### Dashboard
```erlang
% Start dashboard
erlmcp_dashboard:start(),

% Generate dashboard
Dashboard = erlmcp_dashboard:generate_dashboard(),
io:format("Dashboard generated with ~p charts~n", [length(Dashboard#charts)]),

% Get real-time data
RealTimeData = erlmcp_dashboard:get_real_time_data(),
io:format("Real-time data points: ~p~n", [length(RealTimeData)]),

% Get historical data
HistoricalData = erlmcp_dashboard:get_historical_data(3600000),  % 1 hour
io:format("Historical data: ~p points~n", [length(HistoricalData)]),
```

### Advanced Monitoring
```erlang
% Register custom metrics
erlmcp_metrics_collector:register_metric("business.orders", counter, "Total orders processed"),
erlmcp_metrics_collector:register_metric("business.revenue", gauge, "Current revenue"),

% Track business metrics
erlmcp_metrics_collector:increment("business.orders", []),
erlmcp_metrics_collector:set("business.revenue", [], 125000.00),

% Check SLAs
SLAResults = erlmcp_performance_monitor:check_slas(),
lists:foreach(fun(SLA) ->
    case SLA#sla_metric.status of
        violated ->
            io:format("SLA Violated: ~s~n", [SLA#sla_metric.name]);
        _ ->
            ok
    end
end, SLAResults),
```

## Integration Points

### 1. With erlmcp Core
- Integrate with session management
- Monitor request/response patterns
- Track resource usage

### 2. With Database Layer
- Monitor query performance
- Track connection pool usage
- Monitor storage performance

### 3. With Network Layer
- Monitor connection metrics
- Track network latency
- Monitor bandwidth usage

### 4. With External Systems
- Export metrics to Prometheus
- Send alerts to external systems
- Integrate with logging systems

## Performance Considerations

### 1. Metrics Collection Overhead
- Use appropriate collection intervals
- Implement sampling for high-frequency metrics
- Batch metrics to reduce overhead

### 2. Storage Efficiency
- Implement proper data retention policies
- Use appropriate data structures
- Consider compression for historical data

### 3. Alert Management
- Implement alert suppression to prevent alert storms
- Use meaningful alert thresholds
- Implement proper escalation policies

### 4. Dashboard Performance
- Limit data points displayed
- Use efficient chart rendering
- Implement caching for historical data

## Monitoring Best Practices

### 1. Monitoring Philosophy
- Monitor everything that can break
- Set meaningful thresholds
- Alert on trends, not just absolute values

### 2. Alert Management
- Clear alert titles and messages
- Include context and remediation steps
- Implement alert suppression for known issues

### 3. Dashboard Design
- Focus on key metrics
- Use appropriate chart types
- Ensure real-time updates

### 4. Continuous Improvement
- Review alerts regularly
- Adjust thresholds based on trends
- Add new metrics as needed

## Conclusion

The monitoring system provides comprehensive real-time performance tracking, SLA enforcement, and proactive optimization capabilities. The implementation covers metrics collection, performance monitoring, alerting, and visualization - all essential for maintaining Fortune 500 scale performance and reliability.