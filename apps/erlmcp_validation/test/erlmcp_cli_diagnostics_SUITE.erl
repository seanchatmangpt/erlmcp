%%%-------------------------------------------------------------------
%%% @doc erlmcp_cli_diagnostics_SUITE - Diagnostics Integration Tests
%%%
%%% Full diagnostic workflow integration tests
%%%
%%% Chicago School TDD - Real system checks, real health monitoring
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_diagnostics_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
     full_health_check_workflow_test,
     system_diagnostics_workflow_test,
     application_health_check_test,
     network_diagnostics_test,
     performance_diagnostics_test,
     continuous_monitoring_test,
     alert_threshold_test,
     diagnostic_report_generation_test
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp),
    ok.

full_health_check_workflow_test(_Config) ->
    %% Run full health check
    {ok, HealthReport} = erlmcp_cli_diagnostics:full_health_check(),
    
    %% Verify report structure
    #{
        overall_status := _Status,
        checks := Checks,
        passed := Passed,
        failed := Failed
    } = HealthReport,
    
    true = is_list(Checks),
    true = is_integer(Passed),
    true = is_integer(Failed),
    
    ok.

system_diagnostics_workflow_test(_Config) ->
    %% Check system resources
    {ok, SysDiag} = erlmcp_cli_diagnostics:system_diagnostics(),
    
    #{
        memory := MemInfo,
        cpu := CpuInfo,
        disk := DiskInfo
    } = SysDiag,
    
    %% Verify all checks returned data
    true = is_map(MemInfo),
    true = is_map(CpuInfo),
    true = is_map(DiskInfo),
    
    ok.

application_health_check_test(_Config) ->
    %% Check erlmcp application health
    {ok, AppHealth} = erlmcp_cli_diagnostics:check_application(erlmcp),
    
    #{
        status := Status,
        running := Running,
        version := _Version
    } = AppHealth,
    
    pass = Status,
    true = Running,
    
    ok.

network_diagnostics_test(_Config) ->
    %% Check network connectivity
    {ok, NetDiag} = erlmcp_cli_diagnostics:network_diagnostics(),
    
    #{
        connectivity := _Connectivity,
        latency := _Latency
    } = NetDiag,
    
    ok.

performance_diagnostics_test(_Config) ->
    %% Get performance metrics
    {ok, PerfMetrics} = erlmcp_cli_diagnostics:performance_metrics(),
    
    #{
        throughput := _Throughput,
        latency := _Latency,
        queue_depth := _QueueDepth
    } = PerfMetrics,
    
    ok.

continuous_monitoring_test(_Config) ->
    %% Start continuous monitoring
    {ok, Monitor} = erlmcp_cli_diagnostics:start_monitoring(#{
        interval => 100,
        checks => [memory, cpu, processes]
    }),
    
    %% Wait for some samples
    timer:sleep(500),
    
    %% Get monitoring report
    {ok, Report} = erlmcp_cli_diagnostics:get_monitoring_report(Monitor),
    #{samples := Samples} = Report,
    true = length(Samples) > 3,
    
    ok = erlmcp_cli_diagnostics:stop_monitoring(Monitor).

alert_threshold_test(_Config) ->
    %% Set alert threshold
    {ok, Monitor} = erlmcp_cli_diagnostics:start_monitoring(#{
        interval => 100,
        checks => [memory],
        thresholds => #{
            memory_usage_percent => 95
        }
    }),
    
    timer:sleep(300),
    
    %% Check for alerts
    {ok, Alerts} = erlmcp_cli_diagnostics:get_alerts(Monitor),
    true = is_list(Alerts),
    
    ok = erlmcp_cli_diagnostics:stop_monitoring(Monitor).

diagnostic_report_generation_test(_Config) ->
    %% Generate diagnostic report
    {ok, Report} = erlmcp_cli_diagnostics:generate_report(#{
        format => html,
        output => "/tmp/diagnostic_report.html"
    }),
    
    %% Verify report generated
    true = filelib:is_file("/tmp/diagnostic_report.html"),
    
    file:delete("/tmp/diagnostic_report.html"),
    ok.
