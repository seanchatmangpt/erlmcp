# TCPS Health Monitoring and Alerting System

## Overview

The TCPS Health Monitoring system provides comprehensive observability for Toyota Production System-based software engineering workflows. It integrates OpenTelemetry for distributed tracing, multi-platform metric export, intelligent alerting, and self-healing capabilities.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    TCPS Health Monitor                          │
│                  (tcps_health gen_server)                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌───────────────┐  ┌───────────────┐  ┌──────────────┐      │
│  │  Health       │  │  Metrics      │  │  Alerting    │      │
│  │  Checks       │  │  Collection   │  │  Engine      │      │
│  └───────────────┘  └───────────────┘  └──────────────┘      │
│                                                                 │
│  ┌───────────────┐  ┌───────────────┐  ┌──────────────┐      │
│  │  OpenTelemetry│  │  SLO/SLI      │  │  Self-       │      │
│  │  Integration  │  │  Tracking     │  │  Healing     │      │
│  └───────────────┘  └───────────────┘  └──────────────┘      │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
                              │
        ┌─────────────────────┼─────────────────────┐
        │                     │                     │
        ▼                     ▼                     ▼
┌──────────────┐      ┌──────────────┐      ┌──────────────┐
│  TCPS        │      │  TCPS        │      │  TCPS        │
│  Components  │      │  Components  │      │  Components  │
├──────────────┤      ├──────────────┤      ├──────────────┤
│ • Kanban     │      │ • Andon      │      │ • TPM        │
│ • WIP Limits │      │ • Stop-Line  │      │ • Maintenance│
│ • Pull System│      │ • Root Cause │      │ • Uptime     │
└──────────────┘      └──────────────┘      └──────────────┘
```

## Features

### 1. Health Checks

Monitor all TCPS components in real-time:

- **Kanban**: WIP limits, queue depth, utilization
- **Andon**: Open events, critical alerts, resolution times
- **TPM**: Maintenance compliance, uptime, MTBF/MTTR
- **Ontology**: SHACL validation status
- **Receipts**: Generation and validation
- **Persistence**: Backup status, data integrity

```erlang
% Perform comprehensive health check
Result = tcps_health:health_check(),

#{
    status => healthy,
    components => #{
        kanban => {healthy, #{wip_current => 5, utilization => 0.6}},
        andon => {healthy, #{open_andons => 0, critical => 0}},
        tpm => {healthy, #{uptime => 0.9995, compliance => 0.95}},
        % ... other components
    },
    metrics => #{...},
    alerts => [],
    timestamp => 1706294400000
}
```

### 2. OpenTelemetry Integration

Full OTLP support for traces, metrics, and logs:

```erlang
% Initialize OpenTelemetry
tcps_health:init_otel().

% Trace production stage execution
Result = tcps_health:trace_production_stage(compilation, #{
    sku_id => <<"SKU-123">>,
    work_order_id => <<"WO-456">>
}, fun() ->
    compile_module(Module)
end).

% Emit metrics
tcps_health:emit_metric(counter, work_orders_completed, 1, #{
    bucket => reliability,
    stage => testing
}).

tcps_health:emit_metric(histogram, lead_time, 3600000, #{
    sku_id => <<"SKU-123">>
}).
```

### 3. Metrics Collection

Comprehensive metrics across all TCPS dimensions:

#### Production Metrics
- Throughput (work orders/minute)
- Lead time (P50, P90, P99)
- Cycle time (average)
- Work orders completed

#### Quality Metrics
- Defect rate
- First pass yield
- Test coverage
- Quality gate pass rate

#### Kanban Metrics
- Current WIP (total and per bucket)
- Queue depth
- Utilization
- Pull signal latency

#### Andon Metrics
- Open count (total and critical)
- Average resolution time
- Triggers per hour
- Escalation count

#### Kaizen Metrics
- Improvements implemented
- Waste reduction percentage
- Automation coverage

#### TPM Metrics
- Uptime percentage
- Last maintenance timestamp
- Maintenance compliance
- MTBF (Mean Time Between Failures)
- MTTR (Mean Time To Recovery)

```erlang
% Collect all metrics
Metrics = tcps_health:collect_metrics(),

#{
    production => #{
        throughput => 10.5,
        lead_time_p50 => 3600000,
        lead_time_p90 => 5400000,
        lead_time_p99 => 7000000
    },
    quality => #{
        defect_rate => 0.02,
        first_pass_yield => 0.98,
        coverage_percent => 0.95
    },
    % ... other categories
}
```

### 4. Multi-Platform Metric Export

Export to multiple observability platforms:

#### Prometheus
```erlang
% Expose metrics for Prometheus scraping
Metrics = tcps_health:export_to_prometheus().

% Example output:
tcps_production_throughput 10.5
tcps_production_lead_time_p50 3600000
tcps_production_lead_time_p90 5400000
tcps_quality_defect_rate 0.02
tcps_kanban_wip_current 5
```

#### OTLP (OpenTelemetry Protocol)
```erlang
% Export in OTLP JSON format
Data = tcps_health:export_metrics(otlp).
```

#### Datadog
```erlang
% Send metrics to Datadog
tcps_health:send_to_datadog(<<"your-api-key">>).
```

#### New Relic
```erlang
% Send metrics to New Relic
tcps_health:send_to_newrelic(<<"your-api-key">>).
```

#### Grafana Cloud
```erlang
% Send metrics to Grafana Cloud
tcps_health:send_to_grafana_cloud(#{
    api_key => <<"your-api-key">>,
    url => <<"https://prometheus-prod-us-central1.grafana.net">>
}).
```

### 5. Intelligent Alerting

Pre-configured alert rules with auto-remediation:

#### Alert Rules

1. **Critical Andon Open >1 Hour**
   - Severity: Critical
   - Auto-remediate: Escalate to on-call
   - Cooldown: 5 minutes

2. **WIP Limit Exceeded**
   - Severity: Warning
   - Auto-remediate: Pause new work orders
   - Cooldown: 10 minutes

3. **SLA Breach Imminent**
   - Severity: Critical
   - Lead time >1.9 hours (approaching 2-hour SLA)
   - Cooldown: 30 minutes

4. **High Quality Gate Failure Rate**
   - Severity: Warning
   - Pass rate <95%
   - Cooldown: 1 hour

5. **Defect Rate Exceeds Threshold**
   - Severity: Critical
   - Rate >5%
   - Cooldown: 30 minutes

6. **Lead Time Anomaly**
   - Severity: Warning
   - P90 >2x P50
   - Cooldown: 1 hour

7. **TPM Maintenance Overdue**
   - Severity: Warning
   - Compliance <90%
   - Auto-remediate: Trigger maintenance
   - Cooldown: 24 hours

#### Notification Channels

- **Slack**: Webhook integration with rich formatting
- **Email**: SMTP with HTML formatting
- **PagerDuty**: Critical alert escalation
- **Generic Webhook**: Custom integrations

```erlang
% Check alert rules
Alerts = tcps_health:check_alert_rules(),

[
    #{
        id => <<"alert-12345">>,
        severity => critical,
        component => andon,
        rule => <<"Critical Andon Open Too Long">>,
        message => <<"Critical Andon has been open for >1 hour">>,
        triggered_at => 1706294400000,
        auto_remediated => false
    }
]

% Send alert manually
tcps_health:send_alert(Alert).
```

### 6. SLO/SLI Tracking

Track Service Level Objectives and calculate error budget:

#### Defined SLOs

1. **Lead Time**: P90 <2 hours (7200000ms)
2. **Quality Gate Pass Rate**: ≥95%
3. **Deployment Success Rate**: ≥99%
4. **Andon Resolution Time**: <4 hours (avg)
5. **System Uptime**: ≥99.9%

```erlang
% Define SLOs
Slos = tcps_health:define_slos().

% Measure SLIs
Slis = tcps_health:measure_slis(),

[
    #{
        metric => lead_time_p90,
        value => 5400000,
        target => 7200000,
        met => true
    },
    #{
        metric => quality_gate_pass_rate,
        value => 0.97,
        target => 0.95,
        met => true
    }
]

% Calculate error budget
Budget = tcps_health:calculate_error_budget(),
% Returns: 0.95 (95% of error budget remaining)

% Get comprehensive SLO status
Status = tcps_health:get_slo_status(),

#{
    slis => [...],
    error_budget => 0.95,
    error_budget_remaining => 0.95,
    timestamp => 1706294400000
}
```

### 7. Self-Healing Auto-Remediation

Automatic remediation for common issues:

```erlang
% Attempt auto-remediation
Alert = #{
    component => kanban,
    rule => <<"WIP Limit Exceeded">>,
    % ...
},

Result = tcps_health:auto_remediate(Alert),
% Returns: {ok, remediated} or {manual, <<"Reason">>}

% View remediation history
History = tcps_health:get_remediation_history(7),

[
    #{
        alert_id => <<"alert-12345">>,
        component => kanban,
        rule => <<"WIP Limit Exceeded">>,
        result => {ok, remediated},
        timestamp => 1706294400000
    }
]
```

#### Auto-Remediation Actions

- **High WIP**: Pause new work orders
- **Critical Andon**: Escalate to on-call
- **TPM Overdue**: Trigger maintenance
- **Ontology Invalid**: Attempt repair
- **Receipt Missing**: Regenerate if possible

### 8. Structured Logging

OTLP-correlated logging with trace context:

```erlang
% Structured log with context
tcps_health:structured_log(info, <<"Work order completed">>, #{
    work_order_id => <<"WO-123">>,
    bucket => reliability,
    duration_ms => 3600000,
    trace_id => <<"trace-xyz">>,
    span_id => <<"span-abc">>
}).

% Log production event
tcps_health:log_production_event(work_order_created, #{
    work_order_id => <<"WO-123">>,
    bucket => reliability,
    priority => 1
}).

% Output:
% [info] 2026-01-26T12:00:00Z: work_order_created ({work_order_id => <<"WO-123">>, ...})
```

### 9. Dashboard API

Real-time dashboard data for visualization:

```erlang
% Get comprehensive dashboard data
Dashboard = tcps_health:get_dashboard_data(),

#{
    health => #{
        status => healthy,
        components => #{...}
    },
    metrics => #{
        production => #{...},
        quality => #{...}
    },
    active_alerts => [...],
    slo_status => #{
        error_budget => 0.95,
        slis => [...]
    },
    timestamp => 1706294400000
}

% Get metrics for specific component
KanbanMetrics = tcps_health:get_component_metrics(kanban),

#{
    wip_current => 5,
    wip_by_bucket => #{
        reliability => 2,
        security => 1,
        cost => 1,
        compliance => 1
    },
    queue_depth => 3,
    utilization => 0.6
}
```

## Configuration

### System Configuration (`config/sys.config`)

```erlang
{tcps_health, [
    % OpenTelemetry OTLP endpoint
    {otel_endpoint, "http://localhost:4318"},

    % Service identification
    {service_name, "tcps-erlmcp"},
    {service_version, "0.5.0"},
    {environment, "production"},

    % Health check intervals
    {check_interval, 30000},         % 30 seconds
    {alert_check_interval, 10000},   % 10 seconds

    % Metrics
    {metrics_port, 9090},
    {metrics_retention_days, 7},

    % Alerting
    {alert_channels, [slack, email]},
    {enable_auto_remediation, true},

    % Slack
    {slack_webhook, "https://hooks.slack.com/services/YOUR/WEBHOOK"},

    % Email
    {email_smtp, "smtp.example.com:587"},
    {email_from, "tcps-alerts@example.com"},
    {email_to, ["ops-team@example.com"]},

    % SLO targets
    {slo_targets, #{
        lead_time_p90 => 7200000,
        quality_gate_pass_rate => 0.95,
        uptime_percent => 0.999
    }},

    % Platform integrations
    {datadog_enabled, true},
    {datadog_api_key, "YOUR-API-KEY"},

    {grafana_cloud_enabled, true},
    {grafana_cloud_url, "https://prometheus-prod-us-central1.grafana.net"},

    % Trace sampling
    {trace_sample_rate, 0.1}  % 10% in production
]}
```

## Usage

### Starting the Health Monitor

```erlang
% Start with default configuration
{ok, Pid} = tcps_health:start_link().

% Start with custom configuration
{ok, Pid} = tcps_health:start_link(#{
    check_interval => 60000,
    alert_channels => [slack],
    slack_webhook => <<"https://hooks.slack.com/...">>
}).

% Stop
tcps_health:stop().
```

### Basic Workflow

```erlang
% 1. Initialize OpenTelemetry
tcps_health:init_otel().

% 2. Perform health check
Health = tcps_health:health_check().

% 3. Collect metrics
Metrics = tcps_health:collect_metrics().

% 4. Check for alerts
Alerts = tcps_health:check_alert_rules().

% 5. Export metrics
PrometheusData = tcps_health:export_to_prometheus().

% 6. Get dashboard data
Dashboard = tcps_health:get_dashboard_data().
```

### Production Tracing

```erlang
% Trace a production stage
Result = tcps_health:trace_production_stage(testing, #{
    sku_id => <<"SKU-123">>,
    work_order_id => <<"WO-456">>,
    bucket => reliability
}, fun() ->
    % Run tests
    run_test_suite(Module)
end).

% Trace with automatic error handling
Result = tcps_health:trace_production_stage(deployment, #{
    sku_id => <<"SKU-789">>
}, fun() ->
    deploy_to_production(Artifact)
end).
```

### Metric Emission

```erlang
% Counter metric
tcps_health:emit_metric(counter, work_orders_completed, 1, #{
    bucket => reliability
}).

% Gauge metric
tcps_health:emit_metric(gauge, wip_current, 5, #{
    bucket => security
}).

% Histogram metric
tcps_health:emit_metric(histogram, lead_time, Duration, #{
    sku_id => <<"SKU-123">>,
    stage => compilation
}).

% Summary metric
tcps_health:emit_metric(summary, defect_rate, 0.02, #{
    time_period => daily
}).
```

## Grafana Dashboard

A comprehensive Grafana dashboard is included at `/priv/grafana/tcps_dashboard.json`.

### Dashboard Panels

1. **System Health Gauge**: Overall health status
2. **Lead Time Distribution**: P50, P90, P99 trends
3. **Error Budget Remaining**: SLO compliance
4. **Current WIP**: Real-time work in progress
5. **WIP by Bucket**: Reliability, Security, Cost, Compliance
6. **Quality Metrics**: Pass rate, yield, coverage
7. **Active Alerts Table**: Real-time alerts
8. **Open Andons**: Critical events
9. **Throughput**: Work orders per minute
10. **System Uptime**: TPM metrics
11. **Defect Rate**: Quality tracking
12. **Component Health Status**: Bar chart
13. **Alerts by Severity**: Pie chart
14. **Production Stage Duration**: Stacked bars

### Importing the Dashboard

1. Open Grafana
2. Go to Dashboards → Import
3. Upload `/priv/grafana/tcps_dashboard.json`
4. Select Prometheus data source
5. Click Import

## Testing

Comprehensive test suite covering all functionality:

```bash
# Run all tests
rebar3 eunit --module=tcps_health_tests

# Run specific test groups
rebar3 eunit --module=tcps_health_tests --group=health_check_test_
rebar3 eunit --module=tcps_health_tests --group=alert_test_
rebar3 eunit --module=tcps_health_tests --group=otel_test_
rebar3 eunit --module=tcps_health_tests --group=slo_sli_test_
```

### Test Coverage

- Health check system (10 tests)
- Metrics collection (8 tests)
- Metric export (3 tests)
- Alerting (8 tests)
- OpenTelemetry (6 tests)
- SLO/SLI tracking (6 tests)
- Dashboard API (3 tests)
- Platform integrations (4 tests)
- Auto-remediation (5 tests)
- Logging (3 tests)
- Integration tests (3 tests)
- Stress tests (3 tests)

**Total: 62 comprehensive tests**

### Simulating Failures

```erlang
% Simulate Kanban WIP limit breach
tcps_health:simulate_failure(kanban, #{
    wip_over_limit => true,
    utilization => 0.95
}).

% Simulate critical Andon
tcps_health:simulate_failure(andon, #{
    critical_open => true,
    open_duration => 3600000
}).

% Simulate SLA breach
tcps_health:simulate_failure(production, #{
    lead_time_p90 => 7500000
}).
```

## Integration with TCPS Components

### Kanban Integration

```erlang
% The health monitor automatically integrates with Kanban
% to track WIP limits and utilization

% Manual WIP check
WipStatus = tcps_kanban:get_wip_status(reliability),
#{
    current => 5,
    limit => 10,
    available => 5,
    utilization => 0.5
}

% Health monitor alerts if utilization >90%
```

### Andon Integration

```erlang
% When Andon events are triggered, health monitor is notified

% Trigger Andon
tcps_andon:trigger_andon(shacl_violation, #{
    sku_id => <<"SKU-123">>,
    stage => validation,
    details => #{violation => <<"Missing required field">>}
}).

% Health monitor checks open Andons
% Alerts if critical Andon open >1 hour
```

### TPM Integration

```erlang
% Track maintenance compliance
% Health monitor checks TPM metrics
% Alerts if compliance <90%
% Auto-remediates by triggering maintenance
```

## Performance Considerations

- **Background Checks**: Run every 30 seconds (configurable)
- **Alert Checks**: Run every 10 seconds (configurable)
- **Metric Retention**: 7 days (configurable)
- **Alert Retention**: 30 days (configurable)
- **Trace Sampling**: 100% in dev, 10% in prod (configurable)
- **ETS Tables**: 5 tables for efficient in-memory storage
- **Cooldown Periods**: Prevent alert storms

### Resource Usage

- **Memory**: ~10-50 MB depending on metric history
- **CPU**: <1% during normal operation
- **I/O**: Minimal, mostly log writes
- **Network**: OTLP export every 10 seconds (if enabled)

## Troubleshooting

### Health Check Failures

```erlang
% Check individual component
Health = tcps_health:component_health(kanban),

% Possible results:
{healthy, Details}
{degraded, Reason, Details}
{unhealthy, Reason, Details}
```

### Alert Not Firing

1. Check alert rule conditions
2. Verify cooldown period hasn't expired
3. Check metric values

```erlang
Rules = tcps_health:define_alert_rules(),
Metrics = tcps_health:collect_metrics(),
Alerts = tcps_health:check_alert_rules().
```

### OTLP Export Issues

1. Verify OTLP endpoint is reachable
2. Check configuration in `sys.config`
3. Review logs for export errors

```erlang
% Test OTLP export
Data = tcps_health:export_metrics(otlp),
io:format("OTLP Data: ~p~n", [Data]).
```

### Platform Integration Issues

```erlang
% Test Datadog integration
Result = tcps_health:send_to_datadog(<<"test-api-key">>),
% Should return: ok or {error, Reason}

% Test Prometheus export
Metrics = tcps_health:export_to_prometheus(),
% Should return binary with Prometheus format
```

## Best Practices

1. **Configure Trace Sampling**: Use 100% in dev, 10% in production
2. **Set Appropriate SLO Targets**: Based on business requirements
3. **Enable Auto-Remediation**: For known failure modes
4. **Monitor Error Budget**: Track SLO compliance
5. **Review Alert History**: Identify recurring issues
6. **Use Structured Logging**: Include trace context
7. **Export to Multiple Platforms**: Redundancy and insights
8. **Regular Grafana Dashboard Review**: Visual monitoring
9. **Test Alert Channels**: Verify notifications work
10. **Tune Cooldown Periods**: Prevent alert fatigue

## Future Enhancements

- [ ] Machine learning-based anomaly detection
- [ ] Predictive alerting (alert before SLA breach)
- [ ] Advanced correlation analysis
- [ ] Custom alert rule DSL
- [ ] Mobile app notifications
- [ ] Incident management integration (Jira, ServiceNow)
- [ ] Cost tracking and optimization
- [ ] Capacity planning recommendations
- [ ] A/B testing framework integration
- [ ] Chaos engineering integration

## References

- [OpenTelemetry Documentation](https://opentelemetry.io/docs/)
- [Prometheus Exposition Format](https://prometheus.io/docs/instrumenting/exposition_formats/)
- [Grafana Dashboard Best Practices](https://grafana.com/docs/grafana/latest/best-practices/)
- [SLO/SLI Guide](https://sre.google/sre-book/service-level-objectives/)
- [Toyota Production System](https://en.wikipedia.org/wiki/Toyota_Production_System)

## License

Same as erlmcp project.

## Support

For issues, questions, or contributions, please see the main erlmcp repository.
