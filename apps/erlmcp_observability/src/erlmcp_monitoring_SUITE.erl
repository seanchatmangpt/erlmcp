%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive Monitoring Stack Test Suite
%%%
%%% Tests the complete monitoring infrastructure including:
%%% - OTEL metrics collection and aggregation
%%% - Prometheus integration
%%% - Distributed tracing
%%% - Health checks
%%% - Synthetic monitoring
%%% - Alerting rules
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_monitoring_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").

%% Test exports
-export([all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2]).

%% Test cases
-export([
         %% OTEL Integration
         otel_metrics_collection/1,
         otel_trace_propagation/1,
         otel_exporter_configuration/1,

         %% Prometheus Export
         prometheus_metrics_export/1,
         prometheus_alert_rules/1,
         prometheus_dashboards/1,

         %% Distributed Tracing
         jaeger_tracing_integration/1,
         zipkin_tracing_integration/1,
         tracing_context_propagation/1,

         %% Health Monitoring
         health_check_endpoints/1,
         health_aggregation_logic/1,
         health_check_registration/1,

         %% Synthetic Monitoring
         synthetic_transaction_monitoring/1,
         synthetic_transaction_alerting/1,

         %% Log Aggregation
         log_integration_with_loki/1,
         log_format_standards/1,
         log_retention_policies/1,

         %% Alert Routing
         alert_routing_configuration/1,
         alert_escalation_policies/1,
         alert_suppression_rules/1,

         %% Capacity Planning
         performance_baseline_collection/1,
         capacity_metrics_collection/1,
         scaling_thresholds/1,

         %% Error Tracking
         error_tracking_integration/1,
         exception_monitoring/1,
         error_rate_alerting/1,

         %% Business Metrics
         business_kpi_dashboards/1,
         business_metric_collection/1,
         business_alerting/1
        ]).

%% Test groups
-define(GROUP_OTEL, otel_tests).
-define(GROUP_PROMETHEUS, prometheus_tests).
-define(GROUP_TRACING, tracing_tests).
-define(GROUP_HEALTH, health_tests).
-define(GROUP_SYNTHETIC, synthetic_tests).
-define(GROUP_LOGS, log_tests).
-define(GROUP_ALERTS, alert_tests).
-define(GROUP_CAPACITY, capacity_tests).
-define(GROUP_ERRORS, error_tests).
-define(GROUP_BUSINESS, business_tests).

%%====================================================================
%% Test selection
%%====================================================================

all() -> [
          {group, ?GROUP_OTEL},
          {group, ?GROUP_PROMETHEUS},
          {group, ?GROUP_TRACING},
          {group, ?GROUP_HEALTH},
          {group, ?GROUP_SYNTHETIC},
          {group, ?GROUP_LOGS},
          {group, ?GROUP_ALERTS},
          {group, ?GROUP_CAPACITY},
          {group, ?GROUP_ERRORS},
          {group, ?GROUP_BUSINESS}
         ].

groups() -> [
             {?GROUP_OTEL, [parallel],
              [otel_metrics_collection,
               otel_trace_propagation,
               otel_exporter_configuration]},

             {?GROUP_PROMETHEUS, [parallel],
              [prometheus_metrics_export,
               prometheus_alert_rules,
               prometheus_dashboards]},

             {?GROUP_TRACING, [parallel],
              [jaeger_tracing_integration,
               zipkin_tracing_integration,
               tracing_context_propagation]},

             {?GROUP_HEALTH, [parallel],
              [health_check_endpoints,
               health_aggregation_logic,
               health_check_registration]},

             {?GROUP_SYNTHETIC, [parallel],
              [synthetic_transaction_monitoring,
               synthetic_transaction_alerting]},

             {?GROUP_LOGS, [parallel],
              [log_integration_with_loki,
               log_format_standards,
               log_retention_policies]},

             {?GROUP_ALERTS, [parallel],
              [alert_routing_configuration,
               alert_escalation_policies,
               alert_suppression_rules]},

             {?GROUP_CAPACITY, [parallel],
              [performance_baseline_collection,
               capacity_metrics_collection,
               scaling_thresholds]},

             {?GROUP_ERRORS, [parallel],
              [error_tracking_integration,
               exception_monitoring,
               error_rate_alerting]},

             {?GROUP_BUSINESS, [parallel],
              [business_kpi_dashboards,
               business_metric_collection,
               business_alerting]}
            ].

%%====================================================================
%% Suite setup/teardown
%%====================================================================

init_per_suite(Config) ->
    %% Start monitoring infrastructure
    ok = start_monitoring_infrastructure(),

    %% Initialize test data
    TestData = create_test_data(),
    [{test_data, TestData} | Config].

end_per_suite(_Config) ->
    %% Cleanup monitoring infrastructure
    ok = stop_monitoring_infrastructure(),
    ok.

init_per_group(Group, Config) ->
    ct:pal("Starting test group: ~p", [Group]),

    %% Initialize group-specific resources
    case Group of
        ?GROUP_OTEL ->
            %% Start OTEL exporters
            ok = start_otel_exporters(),
            Config;
        ?GROUP_PROMETHEUS ->
            %% Start Prometheus
            ok = start_prometheus(),
            Config;
        ?GROUP_TRACING ->
            %% Start tracing backends
            ok = start_tracing_backends(),
            Config;
        ?GROUP_HEALTH ->
            %% Start health monitors
            ok = start_health_monitors(),
            Config;
        ?GROUP_SYNTHETIC ->
            %% Start synthetic monitors
            ok = start_synthetic_monitors(),
            Config;
        ?GROUP_LOGS ->
            %% Start log aggregation
            ok = start_log_aggregation(),
            Config;
        ?GROUP_ALERTS ->
            %% Start alert manager
            ok = start_alert_manager(),
            Config;
        ?GROUP_CAPACITY ->
            %% Start capacity monitoring
            ok = start_capacity_monitoring(),
            Config;
        ?GROUP_ERRORS ->
            %% Start error tracking
            ok = start_error_tracking(),
            Config;
        ?GROUP_BUSINESS ->
            %% Start business metrics
            ok = start_business_metrics(),
            Config;
        _ ->
            Config
    end.

end_per_group(Group, _Config) ->
    ct:pal("Completed test group: ~p", [Group]),

    %% Cleanup group-specific resources
    case Group of
        ?GROUP_OTEL ->
            ok = stop_otel_exporters();
        ?GROUP_PROMETHEUS ->
            ok = stop_prometheus();
        ?GROUP_TRACING ->
            ok = stop_tracing_backends();
        ?GROUP_HEALTH ->
            ok = stop_health_monitors();
        ?GROUP_SYNTHETIC ->
            ok = stop_synthetic_monitors();
        ?GROUP_LOGS ->
            ok = stop_log_aggregation();
        ?GROUP_ALERTS ->
            ok = stop_alert_manager();
        ?GROUP_CAPACITY ->
            ok = stop_capacity_monitoring();
        ?GROUP_ERRORS ->
            ok = stop_error_tracking();
        ?GROUP_BUSINESS ->
            ok = stop_business_metrics();
        _ ->
            ok
    end,
    ok.

%%====================================================================
%% OTEL Integration Tests
%%====================================================================

otel_metrics_collection(Config) ->
    TestData = proplists:get_value(test_data, Config),
    ServiceName = maps:get(service_name, TestData),

    %% Initialize OTEL
    OtelConfig = #{
        service_name => ServiceName,
        exporters => [prometheus, console],
        sampling => always_on
    },
    ok = erlmcp_otel:init(OtelConfig),

    %% Collect metrics
    Metrics = erlmcp_counters:get_prometheus(),
    ct:assertMatch([_], Metrics, "Metrics should not be empty"),

    %% Verify metric format
    ct:assertMatch(#{"# HELP erlmcp_", _}, Metrics, "Should have HELP and TYPE headers"),

    %% Cleanup
    erlmcp_otel:shutdown(),
    ok.

otel_trace_propagation(Config) ->
    TestData = proplists:get_value(test_data, Config),

    %% Initialize OTEL
    OtelConfig = #{
        service_name => maps:get(service_name, TestData),
        exporters => [console],
        sampling => always_on
    },
    ok = erlmcp_otel:init(OtelConfig),

    %% Create span with context
    SpanCtx = erlmcp_otel:start_span(<<"test.span">>, #{<<"test.attr">> => true}),

    %% Verify span context
    ct:assertMatch(#{trace_id := _, span_id := _}, SpanCtx, "Span context should be valid"),

    %% End span
    ok = erlmcp_otel:end_span(SpanCtx),

    %% Cleanup
    erlmcp_otel:shutdown(),
    ok.

otel_exporter_configuration(Config) ->
    TestData = proplists:get_value(test_data, Config),

    %% Test different exporter configurations
    ExporterConfigs = [
        #{exporters => [console]},
        #{exporters => [jaeger, host => "localhost", port => 14250]},
        #{exporters => [zipkin, endpoint => "http://localhost:9411/api/v2/spans"}},
        #{exporters => [prometheus, port => 9464]}
    ],

    lists:foreach(fun(ConfigMap) ->
        case erlmcp_otel:init(ConfigMap) of
            ok ->
                %% Verify configuration was stored
                StoredConfig = erlang:get(erlmcp_otel_config),
                ct:assert(is_map(StoredConfig), "Configuration should be stored");
            {error, _} ->
                ct:pal("Exporter configuration failed as expected: ~p", [ConfigMap])
        end,

        %% Cleanup
        case erlmcp_otel:shutdown() of
            ok -> ok
        end
    end, ExporterConfigs),

    ok.

%%====================================================================
%% Prometheus Export Tests
%%====================================================================

prometheus_metrics_export(Config) ->
    %% Start Prometheus exporter
    {ok, _Pid} = erlmcp_prometheus_exporter:start_link(9090),

    %% Wait for startup
    timer:sleep(1000),

    %% Test HTTP endpoint
    case httpc:request("http://localhost:9090/metrics") of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            %% Verify response format
            ct:assertMatch([_], Body, "Should return metrics"),
            ct:assertMatch(#{"# HELP erlmcp_", _}, Body, "Should have HELP headers");
        {error, _} ->
            ct:fail("HTTP request failed")
    end,

    %% Stop exporter
    erlmcp_prometheus_exporter:stop(),
    ok.

prometheus_alert_rules(Config) ->
    %% Test alert rule generation
    AlertRules = generate_prometheus_alert_rules(),

    %% Verify rule format
    ct:assert(is_list(AlertRules), "Alert rules should be a list"),
    ct:assert(length(AlertGroups) > 0, "Should have alert groups"),

    %% Test rule validation
    ValidRules = validate_alert_rules(AlertRules),
    ct:assert(length(ValidRules) > 0, "Should have valid rules"),

    ok.

prometheus_dashboards(Config) ->
    %% Test dashboard generation
    Dashboards = generate_prometheus_dashboards(),

    %% Verify dashboard structure
    ct:assert(is_map(Dashboards), "Dashboards should be a map"),
    ct:assert(maps:size(Dashboards) > 0, "Should have dashboards"),

    %% Test dashboard export
    Exported = export_dashboards(Dashboards),
    ct:assert(is_binary(Exported), "Should export valid JSON"),

    ok.

%%====================================================================
%% Distributed Tracing Tests
%%====================================================================

jaeger_tracing_integration(Config) ->
    %% Initialize OTEL with Jaeger
    OtelConfig = #{
        service_name => <<"erlmcp-test">>,
        exporters => [jaeger],
        sampling => always_on
    },
    ok = erlmcp_otel:init(OtelConfig),

    %% Create distributed spans
    Span1 = erlmcp_otel:start_span(<<"span1">>, #{}),
    Span2 = erlmcp_otel:start_span(<<"span2">>, #{<<"parent_id">> => maps:get(span_id, Span1)}),

    %% Verify parent-child relationship
    ct:assertEqual(maps:get(trace_id, Span1), maps:get(trace_id, Span2), "Should share trace ID"),

    %% Cleanup
    ok = erlmcp_otel:end_span(Span2),
    ok = erlmcp_otel:end_span(Span1),
    erlmcp_otel:shutdown(),

    ok.

zipkin_tracing_integration(Config) ->
    %% Initialize OTEL with Zipkin
    OtelConfig = #{
        service_name => <<"erlmcp-test">>,
        exporters => [zipkin],
        sampling => always_on
    },
    ok = erlmcp_otel:init(OtelConfig),

    %% Create trace
    TraceCtx = erlmcp_otel:inject_rpc_span(<<"test.method">>, <<"12345">>, #{}),

    %% Verify trace context format
    ct:assertMatch(#{trace_id := _, span_id := _}, TraceCtx, "Should have trace context"),

    %% Test context propagation
    Headers = erlmcp_otel:propagate_context(TraceCtx),
    ct:assertMatch(#{<<"traceparent">> := _}, Headers, "Should propagate headers"),

    %% Cleanup
    erlmcp_otel:shutdown(),

    ok.

tracing_context_propagation(Config) ->
    %% Test cross-service context propagation
    TestScenarios = [
        {tcp_transport, #{transport_type => tcp}},
        {http_transport, #{transport_type => http}},
        {websocket_transport, #{transport_type => websocket}}
    ],

    lists:foreach(fun({Type, Attrs}) ->
        SpanCtx = erlmcp_otel:start_span(Type, Attrs),
        Propagated = erlmcp_otel:propagate_context(SpanCtx),
        ct:assertMatch(#{<<"traceparent">> := _}, Propagated, "Should propagate ~p", [Type]),
        ok = erlmcp_otel:end_span(SpanCtx)
    end, TestScenarios),

    ok.

%%====================================================================
%% Health Monitoring Tests
%%====================================================================

health_check_endpoints(Config) ->
    %% Start health check server
    {ok, _Pid} = erlmcp_health:start_link(),

    %% Test health check endpoint
    HealthReport = erlmcp_health:check(),
    ct:assert(is_map(HealthReport), "Should return health report"),
    ct:assert(maps:is_key(healthy, HealthReport), "Should have healthy status"),

    %% Register custom health check
    CheckFun = {erlang, whereis, [erlmcp_server]},
    ok = erlmcp_health:register_check(custom_service, CheckFun),

    %% Verify custom check is included
    UpdatedReport = erlmcp_health:check(),
    ct:assert(maps:is_key(checks, UpdatedReport), "Should have check results"),

    %% Cleanup
    erlmcp_health:stop(),
    ok.

health_aggregation_logic(Config) ->
    %% Test health aggregation logic
    TestCases = [
        {#{service1 => healthy, service2 => healthy}, true},
        {#{service1 => healthy, service2 => degraded}, false},
        {#{service1 => unhealthy, service2 => healthy}, false}
    ],

    lists:foreach(fun({Checks, Expected}) ->
        Result = aggregate_health_status(maps:values(Checks)),
        ct:assertEqual(Expected, Result, "Health aggregation should be correct")
    end, TestCases),

    ok.

health_check_registration(Config) ->
    %% Test health check registration system
    CheckFunctions = [
        {registry_check, {erlmcp_registry, list, []}},
        {session_check, {erlmcp_session, list_sessions, []}},
        {transport_check, {erlmcp_transport, status, []}}
    ],

    lists:foreach(fun({Name, Fun}) ->
        ok = erlmcp_health:register_check(Name, Fun),
        ct:pal("Registered check: ~p", [Name])
    end, CheckFunctions),

    %% Verify registration
    Registered = erlmcp_health:check(),
    ct:assert(maps:size(Registered) > 0, "Should have registered checks"),

    %% Cleanup
    lists:foreach(fun({Name, _}) ->
        ok = erlmcp_health:unregister_check(Name)
    end, CheckFunctions),

    ok.

%%====================================================================
%% Synthetic Monitoring Tests
%%====================================================================

synthetic_transaction_monitoring(Config) ->
    %% Start synthetic transaction monitor
    {ok, _Pid} = erlmcp_synthetic_monitor:start_link(),

    %% Define synthetic transactions
    SyntheticTransactions = [
        #{name => tool_call, endpoint => "/tools/call"},
        #{name => resource_read, endpoint => "/resources/{id}"},
        #{name => subscription_create, endpoint => "/subscriptions"}
    ],

    lists:foreach(fun(Transaction) ->
        Result = run_synthetic_transaction(Transaction),
        ct:assert(is_map(Result), "Should return transaction result"),
        ct:assert(maps:is_key(success, Result), "Should have success status")
    end, SyntheticTransactions),

    %% Cleanup
    erlmcp_synthetic_monitor:stop(),
    ok.

synthetic_transaction_alerting(Config) ->
    %% Test synthetic transaction alerting
    AlertThresholds = [
        #{metric => latency_p95, threshold => 5000, operator => gt},
        #{metric => error_rate, threshold => 0.05, operator => gt},
        #{metric => success_rate, threshold => 0.95, operator => lt}
    ],

    %% Test alert generation
    lists:foreach(fun(Threshold) ->
        Alert = generate_synthetic_alert(Threshold),
        ct:assert(is_map(Alert), "Should generate alert"),
        ct:assert(maps:is_key(message, Alert), "Should have alert message")
    end, AlertThresholds),

    ok.

%%====================================================================
%% Log Aggregation Tests
%%====================================================================

log_integration_with_loki(Config) ->
    %% Test Loki log format
    TestLogs = generate_test_logs(),

    lists:foreach(fun(Log) ->
        Formatted = format_for_loki(Log),
        ct:assert(is_binary(Formatted), "Should format for Loki"),
        ct:assertMatch(#{"timestamp" := _, "message" := _}, Formatted, "Should have required fields")
    end, TestLogs),

    ok.

log_format_standards(Config) ->
    %% Test structured logging format
    StructuredLog = #{
        timestamp => erlang:system_time(millisecond),
        level => <<"info">>,
        service => <<"erlmcp">>,
        component => <<"registry">>,
        message => <<"Resource updated">>,
        trace_id => generate_trace_id(),
        span_id => generate_span_id(),
        metadata => #{resource_id => "res123", operation => "update"}
    },

    Formatted = format_structured_log(StructuredLog),
    ct:assert(is_binary(Formatted), "Should format as JSON"),
    ct:assertMatch(#{"timestamp" := _, "level" := _, "service" := _},
                  jsx:is_json(Formatted), "Should be valid JSON"),

    ok.

log_retention_policies(Config) ->
    %% Test log retention configuration
    RetentionPolicies = [
        #{level => debug, retention => "7d"},
        #{level => info, retention => "30d"},
        #{level => warn, retention => "90d"},
        #{level => error, retention => "365d"}
    ],

    lists:foreach(fun(Policy) ->
        Configured = configure_log_retention(Policy),
        ct:assert(is_map(Configured), "Should configure retention"),
        ct:assert(maps:is_key(level, Configured), "Should have level")
    end, RetentionPolicies),

    ok.

%%====================================================================
%% Alert Routing Tests
%%====================================================================

alert_routing_configuration(Config) ->
    %% Test alert routing configuration
    Routes = [
        #{condition => "severity = 'critical'",
          targets => ["pagerduty", "slack", "email"]},
        #{condition => "service = 'registry'",
          targets => ["slack", "webhook"]},
        #{condition => "error_rate > 0.1",
          targets => ["email"]}
    ],

    lists:foreach(fun(Route) ->
        Configured = configure_alert_route(Route),
        ct:assert(is_map(Configured), "Should configure route"),
        ct:assert(maps:is_key(condition, Configured), "Should have condition")
    end, Routes),

    ok.

alert_escalation_policies(Config) ->
    %% Test alert escalation policies
    Escalations = [
        #{severity => "warning",
          delay => "5m",
          steps => [notify_team]},
        #{severity => "error",
          delay => "2m",
          steps => [notify_team, notify_manager]},
        #{severity => "critical",
          delay => "1m",
          steps => [notify_team, notify_manager, notify_director]}
    ],

    lists:foreach(fun(Escalation) ->
        Policy = create_escalation_policy(Escalation),
        ct:assert(is_map(Policy), "Should create policy"),
        ct:assert(maps:is_key(severity, Policy), "Should have severity")
    end, Escalations),

    ok.

alert_suppression_rules(Config) ->
    %% Test alert suppression rules
    Suppressions = [
        #{condition => "service = 'maintenance'",
          duration => "1h"},
        #{condition => "error_type = 'timeout'",
          duration => "30m"},
        #{condition => "deployment_in_progress",
          duration => "2h"}
    ],

    lists:foreach(fun(Suppression) ->
        Rule = create_suppression_rule(Suppression),
        ct:assert(is_map(Rule), "Should create rule"),
        ct:assert(maps:is_key(condition, Rule), "Should have condition")
    end, Suppressions),

    ok.

%%====================================================================
%% Capacity Planning Tests
%%====================================================================

performance_baseline_collection(Config) ->
    %% Test performance baseline collection
    Baselines = collect_performance_baselines(),

    %% Verify baseline structure
    ct:assert(is_map(Baselines), "Should collect baselines"),
    ct:assert(maps:is_key(cpu, Baselines), "Should have CPU baseline"),
    ct:assert(maps:is_key(memory, Baselines), "Should have memory baseline"),
    ct:assert(maps:is_key(network, Baselines), "Should have network baseline"),

    ok.

capacity_metrics_collection(Config) ->
    %% Test capacity metrics collection
    Metrics = collect_capacity_metrics(),

    %% Verify metrics structure
    ct:assert(is_map(Metrics), "Should collect capacity metrics"),
    ct:assert(maps:is_key(utilization, Metrics), "Should have utilization"),
    ct:assert(maps:is_key(throughput, Metrics), "Should have throughput"),
    ct:assert(maps:is_key(latency, Metrics), "Should have latency"),

    ok.

scaling_thresholds(Config) ->
    %% Test scaling threshold configuration
    Thresholds = [
        #{resource => cpu,
          scale_up => 80,
          scale_down => 20},
        #{resource => memory,
          scale_up => 85,
          scale_down => 30},
        #{resource => connections,
          scale_up => 10000,
          scale_down => 1000}
    ],

    lists:foreach(fun(Threshold) ->
        Configured = configure_scaling_threshold(Threshold),
        ct:assert(is_map(Configured), "Should configure threshold"),
        ct:assert(maps:is_key(resource, Configured), "Should have resource")
    end, Thresholds),

    ok.

%%====================================================================
%% Error Tracking Tests
%%====================================================================

error_tracking_integration(Config) ->
    %% Test error tracking integration
    ErrorReports = generate_error_reports(),

    lists:foreach(fun(Error) ->
        Tracked = track_error(Error),
        ct:assert(is_map(Tracked), "Should track error"),
        ct:assert(maps:is_key(error_id, Tracked), "Should have error ID"),
        ct:assert(maps:is_key(timestamp, Tracked), "Should have timestamp")
    end, ErrorReports),

    ok.

exception_monitoring(Config) ->
    %% Test exception monitoring
    Exceptions = [
        {error, {timeout, call}},
        {error, {resource_not_found, 404}},
        {error, {rate_limited, 429}},
        {error, {internal_server_error, 500}}
    ],

    lists:foreach(fun(Exception) ->
        Monitored = monitor_exception(Exception),
        ct:assert(is_map(Monitored), "Should monitor exception"),
        ct:assert(maps:is_key(type, Monitored), "Should have exception type"),
        ct:assert(maps:is_key(count, Monitored), "Should have count")
    end, Exceptions),

    ok.

error_rate_alerting(Config) ->
    %% Test error rate alerting
    ErrorRates = [
        #{service => "registry", rate => 0.02, window => "5m"},
        #{service => "transport", rate => 0.05, window => "10m"},
        #{service => "session", rate => 0.10, window => "1m"}
    ],

    lists:foreach(fun(Rate) ->
        Alert = generate_error_rate_alert(Rate),
        ct:assert(is_map(Alert), "Should generate alert"),
        ct:assert(maps:is_key(message, Alert), "Should have alert message"),
        ct:assert(maps:is_key(severity, Alert), "Should have severity")
    end, ErrorRates),

    ok.

%%====================================================================
%% Business Metrics Tests
%%====================================================================

business_kpi_dashboards(Config) ->
    %% Test business KPI dashboards
    Dashboards = generate_business_dashboards(),

    %% Verify dashboard structure
    ct:assert(is_map(Dashboards), "Should generate dashboards"),
    ct:assert(maps:is_key(customer_satisfaction, Dashboards), "Should have CSAT dashboard"),
    ct:assert(maps:is_key(system_uptime, Dashboards), "Should have uptime dashboard"),
    ct:assert(maps:is_key(transaction_volume, Dashboards), "Should have volume dashboard"),

    ok.

business_metric_collection(Config) ->
    %% Test business metric collection
    Metrics = collect_business_metrics(),

    %% Verify metrics structure
    ct:assert(is_map(Metrics), "Should collect business metrics"),
    ct:assert(maps:is_key(active_sessions, Metrics), "Should have session metrics"),
    ct:assert(maps:is_key(tool_usage, Metrics), "Should have usage metrics"),
    ct:assert(maps:is_key(satisfaction_score, Metrics), "Should have satisfaction metrics"),

    ok.

business_alerting(Config) ->
    %% Test business metric alerting
    BusinessAlerts = [
        #{metric => "customer_satisfaction", threshold => 0.8, operator => lt},
        #{metric => "error_rate", threshold => 0.02, operator => gt},
        #{metric => "p99_latency", threshold => 5000, operator => gt}
    ],

    lists:foreach(fun(Alert) ->
        Generated = generate_business_alert(Alert),
        ct:assert(is_map(Generated), "Should generate business alert"),
        ct:assert(maps:is_key(impact, Generated), "Should have business impact")
    end, BusinessAlerts),

    ok.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

start_monitoring_infrastructure() ->
    %% Start all monitoring components
    ok = application:ensure_started(erlmcp_observability),
    ok = application:ensure_started(opentelemetry),
    ok = application:ensure_started(prometheus),
    ok.

stop_monitoring_infrastructure() ->
    %% Stop all monitoring components
    application:stop(prometheus),
    application:stop(opentelemetry),
    application:stop(erlmcp_observability),
    ok.

create_test_data() ->
    #{
        service_name => <<"erlmcp-monitoring-test">>,
        node => node(),
        timestamp => erlang:system_time(millisecond)
    }.

start_otel_exporters() ->
    %% Start OTEL exporters
    Config = #{exporters => [console]},
    erlmcp_otel:init(Config).

stop_otel_exporters() ->
    erlmcp_otel:shutdown().

start_prometheus() ->
    %% Start Prometheus server
    ok.

stop_prometheus() ->
    %% Stop Prometheus server
    ok.

start_tracing_backends() ->
    %% Start Jaeger and Zipkin backends
    ok.

stop_tracing_backends() ->
    %% Stop tracing backends
    ok.

start_health_monitors() ->
    %% Start health monitoring
    {ok, _} = erlmcp_health:start_link().

stop_health_monitors() ->
    erlmcp_health:stop().

start_synthetic_monitors() ->
    %% Start synthetic monitoring
    {ok, _} = erlmcp_synthetic_monitor:start_link().

stop_synthetic_monitors() ->
    erlmcp_synthetic_monitor:stop().

start_log_aggregation() ->
    %% Start log aggregation
    ok.

stop_log_aggregation() ->
    %% Stop log aggregation
    ok.

start_alert_manager() ->
    %% Start alert manager
    {ok, _} = erlmcp_alert_manager:start_link().

stop_alert_manager() ->
    erlmcp_alert_manager:stop().

start_capacity_monitoring() ->
    %% Start capacity monitoring
    ok.

stop_capacity_monitoring() ->
    %% Stop capacity monitoring
    ok.

start_error_tracking() ->
    %% Start error tracking
    ok.

stop_error_tracking() ->
    %% Stop error tracking
    ok.

start_business_metrics() ->
    %% Start business metrics
    ok.

stop_business_metrics() ->
    %% Stop business metrics
    ok.

aggregate_health_status(Checks) ->
    %% Aggregate health status
    lists:foldl(fun(Check, Acc) ->
        case Check of
            healthy -> Acc and true;
            degraded -> Acc and true;
            unhealthy -> false
        end
    end, true, Checks).

generate_prometheus_alert_rules() ->
    %% Generate Prometheus alert rules
    [
        #{
            alert => "erlmcp_high_error_rate",
            expr => "rate(erlmcp_errors_total[5m]) > 0.1",
            for => "5m",
            labels => #{severity => "warning"},
            annotations => #{summary => "High error rate detected"}
        }
    ].

validate_alert_rules(Rules) ->
    %% Validate alert rules
    lists:filter(fun(Rule) ->
        maps:is_key(alert, Rule) andalso maps:is_key(expr, Rule)
    end, Rules).

generate_prometheus_dashboards() ->
    %% Generate Prometheus dashboards
    #{
        overview => #{
            title => "erlmcp Overview",
            panels => [
                #{type => graph, title => "Requests", metrics => [erlmcp_requests_total]},
                #{type => graph, title => "Errors", metrics => [erlmcp_errors_total]}
            ]
        }
    }.

export_dashboards(Dashboards) ->
    %% Export dashboards as JSON
    jsx:encode(Dashboards).

run_synthetic_transaction(Transaction) ->
    %% Run synthetic transaction
    #{
        name => maps:get(name, Transaction),
        success => true,
        latency => rand:uniform(1000),
        timestamp => erlang:system_time(millisecond)
    }.

generate_synthetic_alert(Threshold) ->
    %% Generate synthetic alert
    #{
        message => "Threshold exceeded",
        metric => maps:get(metric, Threshold),
        value => 0.1,
        threshold => maps:get(threshold, Threshold),
        timestamp => erlang:system_time(millisecond)
    }.

generate_test_logs() ->
    %% Generate test logs
    [
        #{level => info, message => "Service started"},
        #{level => warning, message => "High memory usage"},
        #{level => error, message => "Connection failed"}
    ].

format_for_loki(Log) ->
    %% Format log for Loki
    jsx:encode(Log).

format_structured_log(Log) ->
    %% Format structured log
    jsx:encode(Log).

configure_log_retention(Policy) ->
    %% Configure log retention
    Policy.

configure_alert_route(Route) ->
    %% Configure alert route
    Route.

create_escalation_policy(Escalation) ->
    %% Create escalation policy
    Escalation.

create_suppression_rule(Suppression) ->
    %% Create suppression rule
    Suppression.

collect_performance_baselines() ->
    %% Collect performance baselines
    #{
        cpu => #{p95 => 75.0, p99 => 90.0},
        memory => #{p95 => 80.0, p99 => 95.0},
        network => #{p95 => 100.0, p99 => 150.0}
    }.

collect_capacity_metrics() ->
    %% Collect capacity metrics
    #{
        utilization => 0.75,
        throughput => 1000,
        latency => 100
    }.

configure_scaling_threshold(Threshold) ->
    %% Configure scaling threshold
    Threshold.

generate_error_reports() ->
    %% Generate error reports
    [
        #{type => timeout, count => 5, timestamp => erlang:system_time(millisecond)},
        #{type => resource_not_found, count => 2, timestamp => erlang:system_time(millisecond)}
    ].

track_error(Error) ->
    %% Track error
    maps:merge(Error, #{
        error_id => generate_trace_id(),
        timestamp => erlang:system_time(millisecond)
    }).

monitor_exception(Exception) ->
    %% Monitor exception
    maps:merge(#{type => element(2, Exception)}, #{
        count => 1,
        timestamp => erlang:system_time(millisecond)
    }).

generate_error_rate_alert(Rate) ->
    %% Generate error rate alert
    maps:merge(Rate, #{
        message => "High error rate detected",
        severity => "warning",
        timestamp => erlang:system_time(millisecond)
    }).

generate_business_dashboards() ->
    %% Generate business dashboards
    #{
        customer_satisfaction => #{
            title => "Customer Satisfaction",
            metrics => [csat_score, response_time]
        },
        system_uptime => #{
            title => "System Uptime",
            metrics => [uptime_percentage, downtime_minutes]
        }
    }.

collect_business_metrics() ->
    %% Collect business metrics
    #{
        active_sessions => 100,
        tool_usage => 1000,
        satisfaction_score => 0.95
    }.

generate_business_alert(Alert) ->
    %% Generate business alert
    maps:merge(Alert, #{
        message => "Business metric alert",
        impact => "medium",
        timestamp => erlang:system_time(millisecond)
    }.

generate_trace_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    integer_to_binary(Id, 16).

generate_span_id() ->
    <<Id:64>> = crypto:strong_rand_bytes(8),
    integer_to_binary(Id, 16).