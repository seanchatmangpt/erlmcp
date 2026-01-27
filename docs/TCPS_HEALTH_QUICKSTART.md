# TCPS Health Monitoring - Quick Start Guide

## Installation & Setup

### 1. Configuration

Edit `config/sys.config` to configure the health monitoring system:

```erlang
{tcps_health, [
    % OpenTelemetry endpoint
    {otel_endpoint, "http://localhost:4318"},

    % Service identification
    {service_name, "tcps-erlmcp"},
    {environment, "development"},

    % Alert channels
    {alert_channels, [slack]},
    {slack_webhook, "https://hooks.slack.com/services/YOUR/WEBHOOK/URL"}
]}
```

### 2. Start the System

```erlang
% In Erlang shell
1> application:ensure_all_started(erlmcp).
2> {ok, Pid} = tcps_health:start_link().
```

Or add to your application supervisor:

```erlang
% In your supervisor
{tcps_health, {tcps_health, start_link, []}, permanent, 5000, worker, [tcps_health]}
```

### 3. Initialize OpenTelemetry

```erlang
3> tcps_health:init_otel().
ok
```

## Basic Usage

### Health Check

```erlang
% Get overall health status
4> Health = tcps_health:health_check().
#{status => healthy,
  components => #{kanban => {healthy, #{...}}, ...},
  metrics => #{...},
  alerts => [],
  timestamp => 1706294400000}

% Check specific component
5> tcps_health:component_health(kanban).
{healthy, #{wip_current => 5, utilization => 0.6}}
```

### Collect Metrics

```erlang
% Collect all metrics
6> Metrics = tcps_health:collect_metrics().
#{production => #{throughput => 10.5, ...},
  quality => #{defect_rate => 0.02, ...},
  ...}

% Get specific component metrics
7> tcps_health:get_component_metrics(kanban).
#{wip_current => 5, wip_by_bucket => #{...}, ...}
```

### Trace Production Stages

```erlang
% Trace a production stage with automatic span creation
8> Result = tcps_health:trace_production_stage(testing, #{
    sku_id => <<"SKU-123">>,
    work_order_id => <<"WO-456">>
}, fun() ->
    % Your production code here
    run_tests()
end).
```

### Emit Metrics

```erlang
% Emit counter metric
9> tcps_health:emit_metric(counter, work_orders_completed, 1, #{
    bucket => reliability
}).

% Emit gauge metric
10> tcps_health:emit_metric(gauge, wip_current, 5, #{}).

% Emit histogram metric
11> tcps_health:emit_metric(histogram, lead_time, 3600000, #{
    sku_id => <<"SKU-123">>
}).
```

### Check Alerts

```erlang
% Get active alerts
12> Alerts = tcps_health:check_alert_rules().
[]  % No alerts (healthy system)

% View alert history
13> History = tcps_health:get_alert_history(7).
[#{id => <<"alert-123">>, severity => warning, ...}]
```

### SLO/SLI Tracking

```erlang
% Measure Service Level Indicators
14> Slis = tcps_health:measure_slis().
[#{metric => lead_time_p90, value => 5400000, target => 7200000, met => true}, ...]

% Calculate error budget
15> ErrorBudget = tcps_health:calculate_error_budget().
0.95  % 95% of error budget remaining

% Get comprehensive SLO status
16> tcps_health:get_slo_status().
#{slis => [...], error_budget => 0.95, ...}
```

### Dashboard Data

```erlang
% Get real-time dashboard data
17> Dashboard = tcps_health:get_dashboard_data().
#{health => #{...}, metrics => #{...}, active_alerts => [], ...}
```

### Export Metrics

```erlang
% Export to Prometheus format
18> PrometheusData = tcps_health:export_to_prometheus().
<<"tcps_production_throughput 10.5\ntcps_production_lead_time_p50 3600000\n...">>

% Export to JSON
19> JsonData = tcps_health:export_metrics(json).
<<"{\"production\":{\"throughput\":10.5,...}}">>

% Send to Datadog
20> tcps_health:send_to_datadog(<<"your-api-key">>).
ok

% Send to Grafana Cloud
21> tcps_health:send_to_grafana_cloud(#{
    api_key => <<"your-api-key">>,
    url => <<"https://prometheus-prod-us-central1.grafana.net">>
}).
ok
```

## Running the Examples

```bash
# Compile the example
rebar3 compile

# Run in Erlang shell
rebar3 shell

# In the shell:
1> c("examples/tcps_health_example.erl").
2> tcps_health_example:run_all_examples().
```

## Running Tests

```bash
# Run all health monitoring tests
rebar3 eunit --module=tcps_health_tests

# Run specific test groups
rebar3 eunit --module=tcps_health_tests --group=health_check_test_
rebar3 eunit --module=tcps_health_tests --group=alert_test_
rebar3 eunit --module=tcps_health_tests --group=otel_test_
```

## Setting Up Grafana Dashboard

### Option 1: Import JSON

1. Open Grafana UI
2. Navigate to **Dashboards** → **Import**
3. Click **Upload JSON file**
4. Select `/priv/grafana/tcps_dashboard.json`
5. Choose your Prometheus data source
6. Click **Import**

### Option 2: Manual Configuration

1. Configure Prometheus to scrape metrics:

```yaml
# prometheus.yml
scrape_configs:
  - job_name: 'tcps-erlmcp'
    static_configs:
      - targets: ['localhost:9090']
    scrape_interval: 10s
```

2. Add Prometheus data source in Grafana
3. Import the dashboard JSON

## OpenTelemetry Collector Setup

### Docker Compose

```yaml
version: '3'
services:
  otel-collector:
    image: otel/opentelemetry-collector:latest
    command: ["--config=/etc/otel-collector-config.yaml"]
    volumes:
      - ./otel-collector-config.yaml:/etc/otel-collector-config.yaml
    ports:
      - "4318:4318"   # OTLP HTTP
      - "4317:4317"   # OTLP gRPC
      - "8888:8888"   # Prometheus metrics
      - "8889:8889"   # Prometheus exporter
```

### Collector Configuration

```yaml
# otel-collector-config.yaml
receivers:
  otlp:
    protocols:
      http:
        endpoint: 0.0.0.0:4318
      grpc:
        endpoint: 0.0.0.0:4317

processors:
  batch:

exporters:
  prometheus:
    endpoint: "0.0.0.0:8889"
  logging:
    loglevel: debug

service:
  pipelines:
    traces:
      receivers: [otlp]
      processors: [batch]
      exporters: [logging]
    metrics:
      receivers: [otlp]
      processors: [batch]
      exporters: [prometheus, logging]
```

### Start Collector

```bash
docker-compose up -d
```

## Slack Integration

### 1. Create Webhook

1. Go to https://api.slack.com/apps
2. Create new app
3. Enable "Incoming Webhooks"
4. Create webhook for your channel
5. Copy webhook URL

### 2. Configure

```erlang
% In config/sys.config
{slack_webhook, "https://hooks.slack.com/services/YOUR/WEBHOOK/URL"},
{slack_channel, "#tcps-alerts"}
```

### 3. Test

```erlang
% Trigger a test alert
tcps_health:simulate_failure(kanban, #{wip_over_limit => true}).
tcps_health:check_alert_rules().
```

You should receive a Slack notification!

## Common Patterns

### Pattern 1: Production Pipeline Tracing

```erlang
trace_production_pipeline(SkuId, WorkOrderId) ->
    % Compilation stage
    tcps_health:trace_production_stage(compilation, #{
        sku_id => SkuId,
        work_order_id => WorkOrderId
    }, fun() ->
        compile_module()
    end),

    % Testing stage
    tcps_health:trace_production_stage(testing, #{
        sku_id => SkuId,
        work_order_id => WorkOrderId
    }, fun() ->
        run_tests()
    end),

    % Deployment stage
    tcps_health:trace_production_stage(deployment, #{
        sku_id => SkuId,
        work_order_id => WorkOrderId
    }, fun() ->
        deploy()
    end).
```

### Pattern 2: Custom Metrics Dashboard

```erlang
get_custom_dashboard() ->
    Metrics = tcps_health:collect_metrics(),

    #{
        summary => #{
            wip => maps:get(wip_current, maps:get(kanban, Metrics)),
            defect_rate => maps:get(defect_rate, maps:get(quality, Metrics)),
            throughput => maps:get(throughput, maps:get(production, Metrics))
        },
        trends => #{
            lead_time_p90 => tcps_health:get_metric_history(lead_time_p90, 7),
            defect_rate => tcps_health:get_metric_history(defect_rate, 7)
        },
        health => tcps_health:health_check()
    }.
```

### Pattern 3: Automated Incident Response

```erlang
handle_production_issue() ->
    % Check alerts
    Alerts = tcps_health:check_alert_rules(),

    % Process each alert
    lists:foreach(fun(Alert) ->
        case tcps_health:auto_remediate(Alert) of
            {ok, remediated} ->
                io:format("Auto-remediated: ~p~n", [Alert]);
            {manual, Reason} ->
                io:format("Manual intervention required: ~p~n", [Reason]),
                notify_oncall(Alert)
        end
    end, Alerts).
```

## Troubleshooting

### Issue: Health checks failing

```erlang
% Check individual components
tcps_health:component_health(kanban).
tcps_health:component_health(andon).

% Review logs
tail -f logs/erlmcp.log
```

### Issue: Metrics not exporting

```erlang
% Test Prometheus export
Data = tcps_health:export_to_prometheus(),
io:format("~s~n", [Data]).

% Check OTLP endpoint
curl http://localhost:4318/v1/metrics
```

### Issue: Alerts not firing

```erlang
% Check rule conditions
Rules = tcps_health:define_alert_rules(),
Metrics = tcps_health:collect_metrics(),

% Manually evaluate a rule
[FirstRule | _] = Rules,
Condition = maps:get(condition, FirstRule),
Condition(Metrics).  % Should return true or false
```

### Issue: OpenTelemetry not working

```erlang
% Verify OTLP collector is running
curl http://localhost:4318/v1/traces

% Check configuration
application:get_all_env(tcps_health).

% Re-initialize
tcps_health:init_otel().
```

## Next Steps

1. **Explore the Full Documentation**: See `docs/TCPS_HEALTH_MONITORING.md`
2. **Run All Examples**: Try `tcps_health_example:run_all_examples()`
3. **Set Up Grafana**: Import the dashboard for visualization
4. **Configure Alerts**: Set up Slack/Email notifications
5. **Integrate with Production**: Add to your TCPS pipeline

## Resources

- [Full Documentation](./TCPS_HEALTH_MONITORING.md)
- [OpenTelemetry Erlang](https://github.com/open-telemetry/opentelemetry-erlang)
- [Prometheus](https://prometheus.io/)
- [Grafana](https://grafana.com/)

## Support

For issues or questions, please see the main erlmcp repository.
