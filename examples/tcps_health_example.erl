%%%-----------------------------------------------------------------------------
%%% @doc TCPS Health Monitoring System - Usage Examples
%%%
%%% This module demonstrates how to use the TCPS health monitoring and
%%% alerting system with OpenTelemetry integration.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_health_example).

-export([
    run_all_examples/0,
    example_basic_health_check/0,
    example_otel_tracing/0,
    example_metrics_collection/0,
    example_alerting/0,
    example_slo_tracking/0,
    example_dashboard/0,
    example_platform_export/0
]).

%%%=============================================================================
%%% Main Entry Point
%%%=============================================================================

run_all_examples() ->
    io:format("~n=== TCPS Health Monitoring Examples ===~n~n"),

    % Start the health monitoring system
    {ok, _Pid} = tcps_health:start_link(#{
        check_interval => 60000,
        alert_check_interval => 30000,
        alert_channels => [slack],
        slack_webhook => <<"https://hooks.slack.com/test">>,
        otel_endpoint => "http://localhost:4318"
    }),

    % Run examples
    example_basic_health_check(),
    example_otel_tracing(),
    example_metrics_collection(),
    example_alerting(),
    example_slo_tracking(),
    example_dashboard(),
    example_platform_export(),

    % Stop the system
    tcps_health:stop(),

    io:format("~n=== All Examples Completed ===~n~n"),
    ok.

%%%=============================================================================
%%% Example 1: Basic Health Check
%%%=============================================================================

example_basic_health_check() ->
    io:format("~n--- Example 1: Basic Health Check ---~n"),

    % Perform comprehensive health check
    Result = tcps_health:health_check(),

    io:format("Overall Status: ~p~n", [maps:get(status, Result)]),
    io:format("Components: ~p~n", [maps:keys(maps:get(components, Result))]),
    io:format("Active Alerts: ~p~n", [length(maps:get(alerts, Result))]),

    % Check individual component
    KanbanHealth = tcps_health:component_health(kanban),
    io:format("Kanban Health: ~p~n", [KanbanHealth]),

    ok.

%%%=============================================================================
%%% Example 2: OpenTelemetry Tracing
%%%=============================================================================

example_otel_tracing() ->
    io:format("~n--- Example 2: OpenTelemetry Tracing ---~n"),

    % Initialize OpenTelemetry
    ok = tcps_health:init_otel(),

    % Trace a production stage
    Result = tcps_health:trace_production_stage(testing, #{
        sku_id => <<"SKU-123">>,
        work_order_id => <<"WO-456">>,
        bucket => reliability
    }, fun() ->
        % Simulate testing
        timer:sleep(100),
        io:format("Running tests...~n"),
        {ok, <<"All tests passed">>}
    end),

    io:format("Test Result: ~p~n", [Result]),

    % Trace compilation stage
    CompileResult = tcps_health:trace_production_stage(compilation, fun() ->
        io:format("Compiling module...~n"),
        timer:sleep(50),
        {ok, compiled}
    end),

    io:format("Compile Result: ~p~n", [CompileResult]),

    ok.

%%%=============================================================================
%%% Example 3: Metrics Collection
%%%=============================================================================

example_metrics_collection() ->
    io:format("~n--- Example 3: Metrics Collection ---~n"),

    % Collect all metrics
    Metrics = tcps_health:collect_metrics(),

    % Display production metrics
    Production = maps:get(production, Metrics),
    io:format("Production Metrics:~n"),
    io:format("  Throughput: ~p work orders/min~n", [maps:get(throughput, Production)]),
    io:format("  Lead Time P50: ~p ms~n", [maps:get(lead_time_p50, Production)]),
    io:format("  Lead Time P90: ~p ms~n", [maps:get(lead_time_p90, Production)]),

    % Display quality metrics
    Quality = maps:get(quality, Metrics),
    io:format("Quality Metrics:~n"),
    io:format("  Defect Rate: ~.2f%~n", [maps:get(defect_rate, Quality) * 100]),
    io:format("  Pass Rate: ~.2f%~n", [maps:get(quality_gate_pass_rate, Quality) * 100]),

    % Display Kanban metrics
    Kanban = maps:get(kanban, Metrics),
    io:format("Kanban Metrics:~n"),
    io:format("  Current WIP: ~p~n", [maps:get(wip_current, Kanban)]),
    io:format("  Utilization: ~.2f%~n", [maps:get(utilization, Kanban) * 100]),

    % Emit custom metrics
    tcps_health:emit_metric(counter, work_orders_completed, 1, #{bucket => reliability}),
    tcps_health:emit_metric(gauge, wip_current, 5, #{bucket => security}),
    tcps_health:emit_metric(histogram, lead_time, 3600000, #{sku_id => <<"SKU-123">>}),

    io:format("Custom metrics emitted.~n"),

    ok.

%%%=============================================================================
%%% Example 4: Alerting System
%%%=============================================================================

example_alerting() ->
    io:format("~n--- Example 4: Alerting System ---~n"),

    % Get defined alert rules
    Rules = tcps_health:define_alert_rules(),
    io:format("Total Alert Rules: ~p~n", [length(Rules)]),

    % Display first rule
    [FirstRule | _] = Rules,
    io:format("Example Rule:~n"),
    io:format("  ID: ~p~n", [maps:get(id, FirstRule)]),
    io:format("  Name: ~p~n", [maps:get(name, FirstRule)]),
    io:format("  Severity: ~p~n", [maps:get(severity, FirstRule)]),
    io:format("  Auto-remediate: ~p~n", [maps:get(auto_remediate, FirstRule)]),

    % Check current alerts
    Alerts = tcps_health:check_alert_rules(),
    io:format("Active Alerts: ~p~n", [length(Alerts)]),

    % Simulate a failure to trigger alert
    tcps_health:simulate_failure(kanban, #{wip_over_limit => true}),
    io:format("Simulated WIP limit breach~n"),

    % Get alert history
    History = tcps_health:get_alert_history(7),
    io:format("Alert History (last 7 days): ~p alerts~n", [length(History)]),

    ok.

%%%=============================================================================
%%% Example 5: SLO/SLI Tracking
%%%=============================================================================

example_slo_tracking() ->
    io:format("~n--- Example 5: SLO/SLI Tracking ---~n"),

    % Define SLOs
    Slos = tcps_health:define_slos(),
    io:format("Defined SLOs:~n"),
    lists:foreach(fun(Slo) ->
        io:format("  - ~s (target: ~p)~n", [
            maps:get(name, Slo),
            maps:get(target, Slo)
        ])
    end, Slos),

    % Measure SLIs
    Slis = tcps_health:measure_slis(),
    io:format("~nCurrent SLI Measurements:~n"),
    lists:foreach(fun(Sli) ->
        Met = maps:get(met, Sli),
        Status = case Met of
            true -> "✓ MET";
            false -> "✗ BREACHED"
        end,
        io:format("  - ~p: ~p / ~p [~s]~n", [
            maps:get(metric, Sli),
            maps:get(value, Sli),
            maps:get(target, Sli),
            Status
        ])
    end, Slis),

    % Calculate error budget
    ErrorBudget = tcps_health:calculate_error_budget(),
    io:format("~nError Budget Remaining: ~.2f%~n", [ErrorBudget * 100]),

    % Get SLO status
    Status = tcps_health:get_slo_status(),
    io:format("SLO Status: ~p~n", [maps:keys(Status)]),

    ok.

%%%=============================================================================
%%% Example 6: Dashboard API
%%%=============================================================================

example_dashboard() ->
    io:format("~n--- Example 6: Dashboard API ---~n"),

    % Get comprehensive dashboard data
    Dashboard = tcps_health:get_dashboard_data(),

    io:format("Dashboard Data:~n"),
    io:format("  Health Status: ~p~n", [maps:get(status, maps:get(health, Dashboard))]),
    io:format("  Active Alerts: ~p~n", [length(maps:get(active_alerts, Dashboard))]),
    io:format("  Error Budget: ~.2f%~n", [
        maps:get(error_budget, maps:get(slo_status, Dashboard)) * 100
    ]),

    % Get component-specific metrics
    Components = [kanban, andon, production, quality, kaizen, tpm],
    io:format("~nComponent Metrics:~n"),
    lists:foreach(fun(Component) ->
        Metrics = tcps_health:get_component_metrics(Component),
        io:format("  ~p: ~p metrics~n", [Component, maps:size(Metrics)])
    end, Components),

    ok.

%%%=============================================================================
%%% Example 7: Platform Export
%%%=============================================================================

example_platform_export() ->
    io:format("~n--- Example 7: Platform Export ---~n"),

    % Export to Prometheus format
    PrometheusData = tcps_health:export_to_prometheus(),
    io:format("Prometheus Export (~p bytes):~n~s~n", [
        byte_size(PrometheusData),
        binary:part(PrometheusData, 0, min(200, byte_size(PrometheusData)))
    ]),

    % Export to JSON
    JsonData = tcps_health:export_metrics(json),
    io:format("JSON Export (~p bytes)~n", [byte_size(JsonData)]),

    % Export to OTLP
    OtlpData = tcps_health:export_metrics(otlp),
    io:format("OTLP Export (~p bytes)~n", [byte_size(OtlpData)]),

    % Send to platforms (simulated)
    io:format("~nSending to platforms:~n"),

    Result1 = tcps_health:send_to_datadog(<<"test-api-key">>),
    io:format("  Datadog: ~p~n", [Result1]),

    Result2 = tcps_health:send_to_newrelic(<<"test-api-key">>),
    io:format("  New Relic: ~p~n", [Result2]),

    Result3 = tcps_health:send_to_grafana_cloud(#{
        api_key => <<"test-key">>,
        url => <<"https://prometheus-prod-us-central1.grafana.net">>
    }),
    io:format("  Grafana Cloud: ~p~n", [Result3]),

    ok.

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

% Add any helper functions here if needed
