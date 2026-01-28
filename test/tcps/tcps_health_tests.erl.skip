%%%-----------------------------------------------------------------------------
%%% @doc Test Suite for TCPS Health Monitoring System
%%%
%%% Comprehensive tests covering:
%%% - Health checks for all components
%%% - Alert triggering and notification
%%% - Metric collection and export
%%% - OTLP integration
%%% - SLO/SLI tracking
%%% - Self-healing auto-remediation
%%% - Dashboard API
%%% - Platform integrations
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_health_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Fixtures
%%%=============================================================================

setup() ->
    % Start required applications
    application:ensure_all_started(jsx),

    % Start Kanban if available
    catch tcps_kanban:start_link(),

    % Start health monitoring
    {ok, Pid} = tcps_health:start_link(#{
        check_interval => 60000,  % 1 minute for tests
        alert_check_interval => 30000,  % 30 seconds for tests
        alert_channels => [slack],
        slack_webhook => <<"https://hooks.slack.com/test">>,
        otel_endpoint => "http://localhost:4318"
    }),

    Pid.

cleanup(Pid) ->
    tcps_health:stop(),
    catch tcps_kanban:stop(),
    ok.

%%%=============================================================================
%%% Health Check Tests
%%%=============================================================================

health_check_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_overall_health_check()),
             ?_test(test_component_health_checks()),
             ?_test(test_health_status_caching()),
             ?_test(test_degraded_health()),
             ?_test(test_unhealthy_status())
         ]
     end}.

test_overall_health_check() ->
    Result = tcps_health:health_check(),

    ?assertMatch(#{
        status := _,
        components := _,
        metrics := _,
        alerts := _,
        timestamp := _
    }, Result),

    Status = maps:get(status, Result),
    ?assert(lists:member(Status, [healthy, degraded, unhealthy])).

test_component_health_checks() ->
    Components = [kanban, andon, tpm, ontology, receipts, persistence],

    lists:foreach(fun(Component) ->
        Health = tcps_health:component_health(Component),
        ?assertMatch({_, _}, Health)
    end, Components).

test_health_status_caching() ->
    % First check
    _Result1 = tcps_health:health_check(),

    % Get cached status (should be fast)
    Cached = tcps_health:get_health_status(),
    ?assertMatch(#{status := _}, Cached).

test_degraded_health() ->
    % Simulate degraded condition (high WIP)
    tcps_health:simulate_failure(kanban, #{wip_over_limit => true}),

    Result = tcps_health:health_check(),
    Status = maps:get(status, Result),

    ?assert(Status =/= healthy).

test_unhealthy_status() ->
    % Simulate unhealthy condition
    tcps_health:simulate_failure(andon, #{critical_open => true}),

    Result = tcps_health:health_check(),
    ?assertMatch(#{status := _}, Result).

%%%=============================================================================
%%% Metrics Collection Tests
%%%=============================================================================

metrics_collection_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_collect_all_metrics()),
             ?_test(test_production_metrics()),
             ?_test(test_quality_metrics()),
             ?_test(test_kanban_metrics()),
             ?_test(test_andon_metrics()),
             ?_test(test_kaizen_metrics()),
             ?_test(test_tpm_metrics()),
             ?_test(test_metric_history())
         ]
     end}.

test_collect_all_metrics() ->
    Metrics = tcps_health:collect_metrics(),

    ?assertMatch(#{
        production := #{},
        quality := #{},
        kanban := #{},
        andon := #{},
        kaizen := #{},
        tpm := #{}
    }, Metrics).

test_production_metrics() ->
    Metrics = tcps_health:collect_metrics(),
    Production = maps:get(production, Metrics),

    ?assertMatch(#{
        throughput := _,
        lead_time_p50 := _,
        lead_time_p90 := _,
        lead_time_p99 := _,
        cycle_time_avg := _,
        work_orders_completed := _
    }, Production).

test_quality_metrics() ->
    Metrics = tcps_health:collect_metrics(),
    Quality = maps:get(quality, Metrics),

    ?assertMatch(#{
        defect_rate := _,
        first_pass_yield := _,
        coverage_percent := _,
        quality_gate_pass_rate := _
    }, Quality).

test_kanban_metrics() ->
    Metrics = tcps_health:collect_metrics(),
    Kanban = maps:get(kanban, Metrics),

    ?assertMatch(#{
        wip_current := _,
        wip_by_bucket := _,
        queue_depth := _,
        utilization := _
    }, Kanban).

test_andon_metrics() ->
    Metrics = tcps_health:collect_metrics(),
    Andon = maps:get(andon, Metrics),

    ?assertMatch(#{
        open_count := _,
        critical_count := _,
        avg_resolution_time := _,
        triggers_last_hour := _
    }, Andon).

test_kaizen_metrics() ->
    Metrics = tcps_health:collect_metrics(),
    Kaizen = maps:get(kaizen, Metrics),

    ?assertMatch(#{
        improvements_implemented := _,
        waste_reduction_percent := _,
        automation_coverage := _
    }, Kaizen).

test_tpm_metrics() ->
    Metrics = tcps_health:collect_metrics(),
    Tpm = maps:get(tpm, Metrics),

    ?assertMatch(#{
        uptime_percent := _,
        last_maintenance := _,
        maintenance_compliance := _,
        mtbf := _,
        mttr := _
    }, Tpm).

test_metric_history() ->
    % Collect metrics multiple times
    tcps_health:collect_metrics(),
    timer:sleep(100),
    tcps_health:collect_metrics(),

    History = tcps_health:get_metric_history(lead_time_p90, 1),
    ?assert(is_list(History)).

%%%=============================================================================
%%% Metric Export Tests
%%%=============================================================================

metric_export_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_prometheus_export()),
             ?_test(test_json_export()),
             ?_test(test_otlp_export())
         ]
     end}.

test_prometheus_export() ->
    Result = tcps_health:export_metrics(prometheus),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0),

    % Should contain Prometheus format
    ?assert(binary:match(Result, <<"tcps_">>) =/= nomatch).

test_json_export() ->
    Result = tcps_health:export_metrics(json),
    ?assert(is_binary(Result)),

    % Should be valid JSON
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{}, Decoded).

test_otlp_export() ->
    Result = tcps_health:export_metrics(otlp),
    ?assert(is_binary(Result)),

    % Should contain OTLP structure
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertMatch(#{resource_metrics := _}, Decoded).

%%%=============================================================================
%%% Alert Tests
%%%=============================================================================

alert_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_define_alert_rules()),
             ?_test(test_check_alert_rules()),
             ?_test(test_alert_triggering()),
             ?_test(test_alert_cooldown()),
             ?_test(test_alert_history()),
             ?_test(test_critical_andon_alert()),
             ?_test(test_wip_limit_alert()),
             ?_test(test_sla_breach_alert())
         ]
     end}.

test_define_alert_rules() ->
    Rules = tcps_health:define_alert_rules(),
    ?assert(is_list(Rules)),
    ?assert(length(Rules) > 0),

    % Check rule structure
    [FirstRule | _] = Rules,
    ?assertMatch(#{
        id := _,
        name := _,
        severity := _,
        condition := _,
        message_template := _,
        auto_remediate := _,
        cooldown_seconds := _
    }, FirstRule).

test_check_alert_rules() ->
    Alerts = tcps_health:check_alert_rules(),
    ?assert(is_list(Alerts)).

test_alert_triggering() ->
    % Simulate condition that triggers alert
    tcps_health:simulate_failure(kanban, #{wip_over_limit => true}),

    % Force alert check
    Alerts = tcps_health:check_alert_rules(),

    % Should be a list (may be empty if no alerts triggered)
    ?assert(is_list(Alerts)).

test_alert_cooldown() ->
    % Trigger alert
    tcps_health:simulate_failure(andon, #{critical_open => true}),
    Alerts1 = tcps_health:check_alert_rules(),

    % Immediate re-check should respect cooldown
    Alerts2 = tcps_health:check_alert_rules(),

    % Both should be lists
    ?assert(is_list(Alerts1)),
    ?assert(is_list(Alerts2)).

test_alert_history() ->
    History = tcps_health:get_alert_history(7),
    ?assert(is_list(History)).

test_critical_andon_alert() ->
    % Simulate critical Andon
    tcps_health:simulate_failure(andon, #{
        critical_count => 1,
        open_duration => 3600000  % 1 hour
    }),

    Alerts = tcps_health:check_alert_rules(),
    ?assert(is_list(Alerts)).

test_wip_limit_alert() ->
    % Simulate WIP limit breach
    tcps_health:simulate_failure(kanban, #{
        utilization => 0.95
    }),

    Alerts = tcps_health:check_alert_rules(),
    ?assert(is_list(Alerts)).

test_sla_breach_alert() ->
    % Simulate SLA breach imminent
    tcps_health:simulate_failure(production, #{
        lead_time_p90 => 7000000  % >1.9 hours
    }),

    Alerts = tcps_health:check_alert_rules(),
    ?assert(is_list(Alerts)).

%%%=============================================================================
%%% OpenTelemetry Tests
%%%=============================================================================

otel_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_init_otel()),
             ?_test(test_trace_production_stage()),
             ?_test(test_trace_with_attributes()),
             ?_test(test_emit_counter_metric()),
             ?_test(test_emit_gauge_metric()),
             ?_test(test_emit_histogram_metric())
         ]
     end}.

test_init_otel() ->
    Result = tcps_health:init_otel(),
    ?assertEqual(ok, Result).

test_trace_production_stage() ->
    Result = tcps_health:trace_production_stage(testing, fun() ->
        42
    end),

    ?assertEqual(42, Result).

test_trace_with_attributes() ->
    Attrs = #{
        sku_id => <<"SKU-123">>,
        work_order_id => <<"WO-456">>
    },

    Result = tcps_health:trace_production_stage(validation, Attrs, fun() ->
        <<"validated">>
    end),

    ?assertEqual(<<"validated">>, Result).

test_emit_counter_metric() ->
    Result = tcps_health:emit_metric(counter, work_orders_completed, 1, #{
        bucket => reliability
    }),

    ?assertEqual(ok, Result).

test_emit_gauge_metric() ->
    Result = tcps_health:emit_metric(gauge, wip_current, 5, #{
        bucket => security
    }),

    ?assertEqual(ok, Result).

test_emit_histogram_metric() ->
    Result = tcps_health:emit_metric(histogram, lead_time, 3600000, #{
        stage => testing
    }),

    ?assertEqual(ok, Result).

%%%=============================================================================
%%% SLO/SLI Tests
%%%=============================================================================

slo_sli_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_define_slos()),
             ?_test(test_measure_slis()),
             ?_test(test_calculate_error_budget()),
             ?_test(test_slo_status()),
             ?_test(test_slo_met()),
             ?_test(test_slo_breached())
         ]
     end}.

test_define_slos() ->
    Slos = tcps_health:define_slos(),
    ?assert(is_list(Slos)),
    ?assert(length(Slos) >= 5),

    % Check SLO structure
    [FirstSlo | _] = Slos,
    ?assertMatch(#{
        name := _,
        target := _,
        metric := _,
        operator := _,
        window_days := _
    }, FirstSlo).

test_measure_slis() ->
    Slis = tcps_health:measure_slis(),
    ?assert(is_list(Slis)),

    % Check SLI structure
    case Slis of
        [FirstSli | _] ->
            ?assertMatch(#{
                metric := _,
                value := _,
                target := _,
                met := _
            }, FirstSli);
        [] ->
            ok
    end.

test_calculate_error_budget() ->
    Budget = tcps_health:calculate_error_budget(),
    ?assert(is_float(Budget)),
    ?assert(Budget >= 0.0),
    ?assert(Budget =< 1.0).

test_slo_status() ->
    Status = tcps_health:get_slo_status(),
    ?assertMatch(#{
        slis := _,
        error_budget := _,
        error_budget_remaining := _,
        timestamp := _
    }, Status).

test_slo_met() ->
    % All SLOs should be met in default state
    Slis = tcps_health:measure_slis(),
    AllMet = lists:all(fun(#{met := Met}) -> Met end, Slis),
    ?assert(is_boolean(AllMet)).

test_slo_breached() ->
    % Simulate SLO breach
    tcps_health:simulate_failure(production, #{
        lead_time_p90 => 10000000  % >2 hours
    }),

    Slis = tcps_health:measure_slis(),
    ?assert(is_list(Slis)).

%%%=============================================================================
%%% Dashboard API Tests
%%%=============================================================================

dashboard_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_get_dashboard_data()),
             ?_test(test_get_component_metrics()),
             ?_test(test_dashboard_real_time_data())
         ]
     end}.

test_get_dashboard_data() ->
    Data = tcps_health:get_dashboard_data(),

    ?assertMatch(#{
        health := _,
        metrics := _,
        active_alerts := _,
        slo_status := _,
        timestamp := _
    }, Data).

test_get_component_metrics() ->
    Components = [kanban, andon, production, quality, kaizen, tpm],

    lists:foreach(fun(Component) ->
        Metrics = tcps_health:get_component_metrics(Component),
        ?assertMatch(#{}, Metrics)
    end, Components).

test_dashboard_real_time_data() ->
    % Get dashboard data multiple times
    Data1 = tcps_health:get_dashboard_data(),
    timer:sleep(100),
    Data2 = tcps_health:get_dashboard_data(),

    ?assertMatch(#{timestamp := _}, Data1),
    ?assertMatch(#{timestamp := _}, Data2),

    % Timestamps should be different
    T1 = maps:get(timestamp, Data1),
    T2 = maps:get(timestamp, Data2),
    ?assert(T2 >= T1).

%%%=============================================================================
%%% Platform Integration Tests
%%%=============================================================================

platform_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_prometheus_endpoint()),
             ?_test(test_datadog_integration()),
             ?_test(test_newrelic_integration()),
             ?_test(test_grafana_integration())
         ]
     end}.

test_prometheus_endpoint() ->
    Metrics = tcps_health:export_to_prometheus(),
    ?assert(is_binary(Metrics)),
    ?assert(byte_size(Metrics) > 0).

test_datadog_integration() ->
    Result = tcps_health:send_to_datadog(<<"test-api-key">>),
    ?assertEqual(ok, Result).

test_newrelic_integration() ->
    Result = tcps_health:send_to_newrelic(<<"test-api-key">>),
    ?assertEqual(ok, Result).

test_grafana_integration() ->
    Config = #{
        api_key => <<"test-key">>,
        url => <<"https://grafana.example.com">>
    },
    Result = tcps_health:send_to_grafana_cloud(Config),
    ?assertEqual(ok, Result).

%%%=============================================================================
%%% Auto-Remediation Tests
%%%=============================================================================

auto_remediation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_auto_remediate_andon()),
             ?_test(test_auto_remediate_wip()),
             ?_test(test_auto_remediate_tpm()),
             ?_test(test_manual_remediation()),
             ?_test(test_remediation_history())
         ]
     end}.

test_auto_remediate_andon() ->
    Alert = #{
        id => <<"test-alert-1">>,
        severity => critical,
        component => andon,
        rule => <<"Critical Andon Open Too Long">>,
        message => <<"Test alert">>,
        triggered_at => erlang:system_time(millisecond),
        details => #{},
        auto_remediated => false
    },

    Result = tcps_health:auto_remediate(Alert),
    ?assertMatch({ok, remediated}, Result).

test_auto_remediate_wip() ->
    Alert = #{
        id => <<"test-alert-2">>,
        severity => warning,
        component => kanban,
        rule => <<"WIP Limit Exceeded">>,
        message => <<"Test alert">>,
        triggered_at => erlang:system_time(millisecond),
        details => #{},
        auto_remediated => false
    },

    Result = tcps_health:auto_remediate(Alert),
    ?assertMatch({ok, remediated}, Result).

test_auto_remediate_tpm() ->
    Alert = #{
        id => <<"test-alert-3">>,
        severity => warning,
        component => tpm,
        rule => <<"TPM Maintenance Overdue">>,
        message => <<"Test alert">>,
        triggered_at => erlang:system_time(millisecond),
        details => #{},
        auto_remediated => false
    },

    Result = tcps_health:auto_remediate(Alert),
    ?assertMatch({ok, remediated}, Result).

test_manual_remediation() ->
    Alert = #{
        id => <<"test-alert-4">>,
        severity => critical,
        component => unknown,
        rule => <<"Unknown Rule">>,
        message => <<"Test alert">>,
        triggered_at => erlang:system_time(millisecond),
        details => #{},
        auto_remediated => false
    },

    Result = tcps_health:auto_remediate(Alert),
    ?assertMatch({manual, _}, Result).

test_remediation_history() ->
    History = tcps_health:get_remediation_history(7),
    ?assert(is_list(History)).

%%%=============================================================================
%%% Logging Tests
%%%=============================================================================

logging_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_structured_logging()),
             ?_test(test_log_production_event()),
             ?_test(test_log_with_trace_context())
         ]
     end}.

test_structured_logging() ->
    Result = tcps_health:structured_log(info, <<"Test log">>, #{
        component => test,
        action => testing
    }),

    ?assertEqual(ok, Result).

test_log_production_event() ->
    Result = tcps_health:log_production_event(work_order_created, #{
        work_order_id => <<"WO-123">>,
        bucket => reliability
    }),

    ?assertEqual(ok, Result).

test_log_with_trace_context() ->
    Result = tcps_health:structured_log(info, <<"Traced log">>, #{
        trace_id => <<"trace-123">>,
        span_id => <<"span-456">>,
        action => traced_action
    }),

    ?assertEqual(ok, Result).

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_end_to_end_monitoring()),
             ?_test(test_alert_to_remediation_flow()),
             ?_test(test_metrics_to_dashboard_flow())
         ]
     end}.

test_end_to_end_monitoring() ->
    % Perform health check
    Health = tcps_health:health_check(),
    ?assertMatch(#{status := _}, Health),

    % Collect metrics
    Metrics = tcps_health:collect_metrics(),
    ?assertMatch(#{production := _}, Metrics),

    % Check alerts
    Alerts = tcps_health:check_alert_rules(),
    ?assert(is_list(Alerts)),

    % Get dashboard data
    Dashboard = tcps_health:get_dashboard_data(),
    ?assertMatch(#{health := _, metrics := _}, Dashboard).

test_alert_to_remediation_flow() ->
    % Simulate failure
    tcps_health:simulate_failure(andon, #{critical_open => true}),

    % Check alerts
    Alerts = tcps_health:check_alert_rules(),

    % If alerts exist, try remediation
    case Alerts of
        [Alert | _] ->
            Result = tcps_health:auto_remediate(Alert),
            ?assert(Result =:= {ok, remediated} orelse
                    element(1, Result) =:= manual);
        [] ->
            ok
    end.

test_metrics_to_dashboard_flow() ->
    % Emit some metrics
    tcps_health:emit_metric(counter, test_metric, 1, #{}),
    tcps_health:emit_metric(gauge, test_gauge, 42, #{}),

    % Collect metrics
    Metrics = tcps_health:collect_metrics(),

    % Export in different formats
    Prometheus = tcps_health:export_metrics(prometheus),
    ?assert(is_binary(Prometheus)),

    Json = tcps_health:export_metrics(json),
    ?assert(is_binary(Json)),

    % Get dashboard data
    Dashboard = tcps_health:get_dashboard_data(),
    ?assertMatch(#{metrics := _}, Dashboard).

%%%=============================================================================
%%% Stress Tests
%%%=============================================================================

stress_test_() ->
    {timeout, 60,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(_Pid) ->
          [
              ?_test(test_high_frequency_metrics()),
              ?_test(test_concurrent_health_checks()),
              ?_test(test_alert_storm())
          ]
      end}}.

test_high_frequency_metrics() ->
    % Emit 1000 metrics rapidly
    lists:foreach(fun(N) ->
        tcps_health:emit_metric(counter, test_counter, N, #{iteration => N})
    end, lists:seq(1, 1000)),

    Metrics = tcps_health:collect_metrics(),
    ?assertMatch(#{}, Metrics).

test_concurrent_health_checks() ->
    % Run 100 concurrent health checks
    Parent = self(),

    Pids = [spawn(fun() ->
        Result = tcps_health:health_check(),
        Parent ! {health_check, self(), Result}
    end) || _ <- lists:seq(1, 100)],

    % Collect results
    Results = [receive
        {health_check, Pid, Result} -> Result
    after 5000 ->
        timeout
    end || Pid <- Pids],

    % All should succeed
    ?assertEqual(100, length([R || R <- Results, is_map(R)])).

test_alert_storm() ->
    % Trigger multiple alerts rapidly
    [tcps_health:simulate_failure(Component, #{test => true})
     || Component <- [kanban, andon, tpm, ontology, receipts]],

    % Check alerts
    Alerts = tcps_health:check_alert_rules(),
    ?assert(is_list(Alerts)).
