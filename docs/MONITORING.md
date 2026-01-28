# ErlMCP Continuous Monitoring System

## Overview

The ErlMCP monitoring system provides comprehensive 24/7 observability with real-time monitoring, health checks, alerting, and SLA tracking. The system is designed to monitor everything continuously and alert on any degradation.

## Features

### 🔍 Real-time Monitoring
- Live span collection with OpenTelemetry integration
- Metric aggregation every 5-10 seconds
- Real-time dashboard updates via WebSocket
- Comprehensive health checking of all components

### 🚨 Alerting System
- Configurable alert rules and thresholds
- Multiple notification channels (console, log, email, Slack, webhook, SMS)
- Alert escalation with cooldown periods
- Alert acknowledgment and resolution tracking

### 📊 Metrics Collection
- System metrics (processes, memory, network)
- Performance metrics (latency, throughput, error rates)
- Application metrics (spans, registry size, transport count)
- Custom metrics support

### 🎯 SLA Monitoring
- Service Level Objective (SLO) tracking
- Availability, latency, reliability, and throughput monitoring
- SLA violation alerts
- Compliance reporting

### 📈 Dashboard
- Real-time web dashboard on port 8080
- Health status visualization
- Metrics history and trends
- Alert management interface
- Multiple export formats (JSON, CSV, Prometheus)

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                 ErlMCP Monitor Supervisor                   │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────────┐  ┌─────────────────┐  ┌──────────────┐ │
│  │  Monitor Core   │  │ Alert Manager   │  │ Metrics      │ │
│  │                 │  │                 │  │ Collector    │ │
│  │ • Health checks │  │ • Notifications │  │              │ │
│  │ • Alert rules   │  │ • Escalation    │  │ • Collection │ │
│  │ • Coordination  │  │ • Channels      │  │ • SLA track  │ │
│  └─────────────────┘  └─────────────────┘  └──────────────┘ │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │                    Dashboard                            │ │
│  │ • Web interface • WebSocket • Export                   │ │
│  └─────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

## Getting Started

### 1. Start Monitoring

```erlang
%% Start monitoring with default configuration
{ok, _Pid} = erlmcp_monitor_sup:start_link(#{}).

%% Or with custom configuration
Config = #{
    check_interval_ms => 5000,
    dashboard_enabled => true,
    alert_handlers => [console, log, email]
},
{ok, _Pid} = erlmcp_monitor_sup:start_link(Config).
```

### 2. Access Dashboard

Open your browser to `http://localhost:8080` to access the monitoring dashboard.

### 3. Configure Alerts

```erlang
%% Add custom alert rule
AlertRule = #{
    id => <<"custom_high_cpu">>,
    name => <<"High CPU Usage">>,
    condition => fun(Metrics) ->
        CpuUsage = maps:get(cpu_usage_percent, Metrics, 0),
        CpuUsage > 80.0
    end,
    threshold => 80.0,
    severity => warning,
    cooldown => 300000,
    enabled => true
},
erlmcp_monitor:add_alert_rule(<<"custom_high_cpu">>, AlertRule).
```

## Health Checks

The monitoring system performs comprehensive health checks every 5 seconds:

### Components Monitored
- **Transports**: Active transport processes and supervisor health
- **Registry**: Capability registry size and responsiveness
- **Memory**: Total memory usage, process memory, system memory
- **Connections**: Active connections and failure rates
- **Processes**: Process count and limits
- **Dependencies**: External service availability

### Health Scoring
Each component receives a health score (0.0-1.0), and an overall health score is calculated using weighted averages:
- Transports: 25%
- Registry: 15%
- Memory: 20%
- Connections: 20%
- Processes: 10%
- Dependencies: 10%

### Health Status Levels
- **Healthy** (score ≥ 0.8): All systems operating normally
- **Degraded** (score 0.5-0.8): Some issues detected, service functional
- **Unhealthy** (score < 0.5): Critical issues, service may be impaired

## Alert Rules

### Default Alert Rules

| Alert | Threshold | Severity | Cooldown |
|-------|-----------|----------|----------|
| High Latency | > 1000ms | Warning | 5 min |
| Critical Latency | > 5000ms | Critical | 3 min |
| High Error Rate | > 1% | Warning | 3 min |
| Critical Error Rate | > 5% | Critical | 2 min |
| High Memory Usage | > 75% | Warning | 10 min |
| Critical Memory Usage | > 90% | Critical | 5 min |
| Connection Failures | > 5% | Warning | 4 min |
| Service Degraded | Health < 70% | Warning | 5 min |
| Service Critical | Health < 50% | Critical | 1 min |
| SLA Violation | Any SLA violated | Critical | 5 min |

### Custom Alert Rules

You can create custom alert rules:

```erlang
CustomRule = #alert_rule{
    id = <<"custom_rule">>,
    name = <<"Custom Alert">>,
    condition = fun(Metrics) ->
        % Your custom logic here
        CustomValue = maps:get(custom_metric, Metrics, 0),
        CustomValue > 100
    end,
    threshold = 100,
    severity = warning,
    cooldown = 300000,
    enabled = true
}.
```

## SLA Objectives

### Default SLA Objectives
- **High Availability**: 99.95% uptime
- **Low Latency**: P95 response time < 500ms
- **Reliability**: Error rate < 0.1%
- **Throughput**: Minimum 100 requests/second

### SLA Status
- **Meeting**: Currently achieving target
- **At Risk**: Below target but above threshold
- **Violated**: Below acceptable threshold

## Notification Channels

### Console
Immediate console output for all alerts.

### Log
Structured logging integration.

### Email
SMTP-based email notifications:
```erlang
email => #{
    type => email,
    enabled => true,
    config => #{
        smtp_server => "smtp.example.com",
        smtp_port => 587,
        username => "alerts@example.com",
        password => "password",
        recipients => ["admin@example.com"]
    },
    rate_limit => #{count => 5, window_ms => 300000}
}
```

### Slack
Webhook-based Slack notifications:
```erlang
slack => #{
    type => slack,
    enabled => true,
    config => #{
        webhook_url => "https://hooks.slack.com/services/...",
        channel => "#alerts",
        username => "ErlMCP Monitor"
    },
    rate_limit => #{count => 10, window_ms => 300000}
}
```

### Webhook
HTTP POST to custom endpoints:
```erlang
webhook => #{
    type => webhook,
    enabled => true,
    config => #{
        url => "https://example.com/webhook/alerts",
        method => post,
        headers => #{
            "Content-Type" => "application/json",
            "Authorization" => "Bearer TOKEN"
        },
        timeout_ms => 5000
    }
}
```

### SMS
SMS notifications for critical alerts:
```erlang
sms => #{
    type => sms,
    enabled => true,
    config => #{
        provider => "twilio",
        account_sid => "YOUR_SID",
        auth_token => "YOUR_TOKEN",
        phone_numbers => ["+1234567890"]
    },
    rate_limit => #{count => 3, window_ms => 900000}
}
```

## Metrics Export

### JSON Export
```erlang
{ok, JSONData} = erlmcp_monitor_dashboard:export_metrics(json).
```

### CSV Export
```erlang
{ok, CSVData} = erlmcp_monitor_dashboard:export_metrics(csv).
```

### Prometheus Export
```erlang
{ok, PrometheusData} = erlmcp_monitor_dashboard:export_metrics(prometheus).
```

## API Reference

### Monitor Control
```erlang
%% Start monitoring
erlmcp_monitor_sup:start_monitoring(Config).

%% Stop monitoring
erlmcp_monitor_sup:stop_monitoring().

%% Get health status
{ok, Health} = erlmcp_monitor:get_health_status().

%% Get metrics
{ok, Metrics} = erlmcp_monitor:get_metrics().
```

### Alert Management
```erlang
%% Add alert rule
erlmcp_monitor:add_alert_rule(RuleId, Rule).

%% Remove alert rule
erlmcp_monitor:remove_alert_rule(RuleId).

%% Acknowledge alert
erlmcp_alert_manager:acknowledge_alert(AlertId).

%% Resolve alert
erlmcp_alert_manager:resolve_alert(AlertId).
```

### Dashboard Operations
```erlang
%% Start dashboard
erlmcp_monitor_dashboard:start_dashboard().

%% Get dashboard data
{ok, Data} = erlmcp_monitor_dashboard:get_dashboard_data().

%% Export metrics
{ok, Export} = erlmcp_monitor_dashboard:export_metrics(json).
```

## Configuration

See `config/monitor.config` for complete configuration options including:
- Check intervals and timeouts
- Alert rules and thresholds
- Notification channels
- SLA objectives
- Dashboard settings
- Custom metrics collectors

## Integration with OpenTelemetry

The monitoring system integrates with OpenTelemetry for:
- Span collection and analysis
- Distributed tracing
- Performance metrics
- Request/response tracking

Example integration:
```erlang
%% In your application code
SpanCtx = otel_tracer:start_span(<<"operation">>),
try
    %% Your operation here
    Result = perform_operation(),
    otel_span:set_attributes(SpanCtx, [{<<"result">>, <<"success">>}]),
    Result
catch
    Error:Reason ->
        otel_span:record_exception(SpanCtx, Error, Reason),
        otel_span:set_status(SpanCtx, opentelemetry:status(error, "Operation failed")),
        {error, Reason}
after
    otel_span:end_span(SpanCtx)
end.
```

## Performance Impact

The monitoring system is designed for minimal performance impact:
- Health checks: ~1ms per check
- Metrics collection: ~2ms per collection
- Memory overhead: ~50MB for full monitoring
- CPU usage: <1% of system resources

## Troubleshooting

### Common Issues

**Dashboard not accessible**: Check that port 8080 is not blocked and `dashboard_enabled => true` in config.

**Alerts not firing**: Verify alert rules are enabled and thresholds are correctly configured.

**High memory usage**: Adjust `max_metric_history` and `max_alert_history` in configuration.

**Missing metrics**: Ensure metrics collection is enabled and components are running.

### Debug Mode

Enable debug logging:
```erlang
Config = #{
    debug => true,
    log_level => debug
}.
```

### Health Check

Verify monitoring system health:
```erlang
Status = erlmcp_monitor_sup:get_component_status(),
[{supervisor, Pid, running}, {erlmcp_monitor, Pid2, running}, ...] = Status.
```

## Testing

Run the comprehensive test suite:
```bash
rebar3 eunit --module=erlmcp_monitor_test
```

The test suite covers:
- Monitor startup and configuration
- Health check functionality
- Metrics collection
- Alert rule management
- Dashboard operations
- SLA tracking
- Performance testing
- Integration scenarios

## Future Enhancements

- **Anomaly Detection**: ML-based anomaly detection
- **Predictive Alerts**: Alert before issues occur
- **Auto-scaling**: Automatic resource scaling based on metrics
- **Custom Dashboards**: User-configurable dashboard layouts
- **Mobile Notifications**: Push notifications to mobile devices
- **Integration APIs**: REST APIs for external integrations
