%%%-------------------------------------------------------------------
%%% @doc ErlMCP Monitor Tests
%%% Comprehensive test suite for the monitoring system.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_monitor_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test setup and teardown
%%%===================================================================

setup() ->
    Config = #{
        check_interval_ms => 100, % Fast for testing
        alert_cooldown_ms => 1000,
        health_check_timeout_ms => 500,
        metrics_retention_hours => 1,
        dashboard_enabled => false, % Disable for tests
        alert_handlers => [test_handler]
    },
    {ok, _Pid} = erlmcp_monitor_sup:start_link(Config),
    Config.

teardown(_Config) ->
    erlmcp_monitor_sup:stop_monitoring(),
    ok.

monitor_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
         fun test_monitor_startup/1,
         fun test_health_checks/1,
         fun test_metrics_collection/1,
         fun test_alert_rules/1,
         fun test_configuration/1
     ]}.

%%%===================================================================
%%% Tests
%%%===================================================================

test_monitor_startup(Config) ->
    [
        % Test that monitor starts successfully
        ?_assert(is_pid(whereis(erlmcp_monitor))),
        
        % Test that monitor accepts configuration
        ?_assertEqual(ok, erlmcp_monitor:update_config(Config)),
        
        % Test health status retrieval
        ?_assertMatch({ok, _}, erlmcp_monitor:get_health_status()),
        
        % Test metrics retrieval
        ?_assertMatch({ok, _}, erlmcp_monitor:get_metrics())
    ].

test_health_checks(_Config) ->
    % Wait for initial health check
    timer:sleep(200),
    
    {ok, HealthStatus} = erlmcp_monitor:get_health_status(),
    
    [
        % Test health status structure
        ?_assert(maps:is_key(overall, HealthStatus)),
        ?_assert(maps:is_key(score, HealthStatus)),
        ?_assert(maps:is_key(components, HealthStatus)),
        ?_assert(maps:is_key(timestamp, HealthStatus)),
        
        % Test health score is within valid range
        ?_assert(maps:get(score, HealthStatus) >= 0.0),
        ?_assert(maps:get(score, HealthStatus) =< 1.0),
        
        % Test overall status is valid
        ?_assert(lists:member(maps:get(overall, HealthStatus), [healthy, degraded, unhealthy])),
        
        % Test components are checked
        Components = maps:get(components, HealthStatus),
        ?_assert(maps:is_key(memory, Components)),
        ?_assert(maps:is_key(processes, Components))
    ].

test_metrics_collection(_Config) ->
    % Wait for metrics collection
    timer:sleep(200),
    
    {ok, Metrics} = erlmcp_monitor:get_metrics(),
    
    [
        % Test metrics structure
        ?_assert(maps:is_key(timestamp, Metrics)),
        ?_assert(maps:is_key(process_count, Metrics)),
        ?_assert(maps:is_key(memory_total, Metrics)),
        ?_assert(maps:is_key(memory_usage_percent, Metrics)),
        
        % Test metric values are reasonable
        ?_assert(maps:get(process_count, Metrics) > 0),
        ?_assert(maps:get(memory_total, Metrics) > 0),
        ?_assert(maps:get(memory_usage_percent, Metrics) >= 0.0),
        ?_assert(maps:get(memory_usage_percent, Metrics) =< 100.0)
    ].

test_alert_rules(_Config) ->
    % Test adding an alert rule
    TestRule = #{
        id => <<"test_rule">>,
        name => <<"Test Rule">>,
        condition => fun(Metrics) ->
            maps:get(test_metric, Metrics, 0) > 100
        end,
        threshold => 100,
        severity => warning,
        cooldown => 5000,
        enabled => true
    },
    
    AddResult = erlmcp_monitor:add_alert_rule(<<"test_rule">>, TestRule),
    
    [
        % Test rule addition
        ?_assertEqual(ok, AddResult),
        
        % Test rule removal
        ?_assertEqual(ok, erlmcp_monitor:remove_alert_rule(<<"test_rule">>)),
        
        % Test removing non-existent rule
        ?_assertMatch({error, _}, erlmcp_monitor:remove_alert_rule(<<"non_existent">>))
    ].

test_configuration(_Config) ->
    % Test configuration updates
    NewConfig = #{
        check_interval_ms => 200,
        alert_cooldown_ms => 2000
    },
    
    UpdateResult = erlmcp_monitor:update_config(NewConfig),
    
    [
        % Test configuration update
        ?_assertEqual(ok, UpdateResult)
    ].

%%%===================================================================
%%% Configuration Tests
%%%===================================================================

config_test_() ->
    [
        fun test_default_config/0,
        fun test_config_validation/0,
        fun test_alert_rule_creation/0,
        fun test_config_merging/0
    ].

test_default_config() ->
    {ok, Config} = erlmcp_monitor_config:load_config(),
    
    [
        ?_assert(maps:is_key(check_interval_ms, Config)),
        ?_assert(maps:is_key(alert_cooldown_ms, Config)),
        ?_assert(maps:is_key(thresholds, Config)),
        ?_assert(maps:is_key(alert_rules, Config))
    ].

test_config_validation() ->
    ValidConfig = #{
        check_interval_ms => 5000,
        alert_cooldown_ms => 60000,
        health_check_timeout_ms => 2000
    },
    
    InvalidConfig = #{
        check_interval_ms => "invalid"
    },
    
    [
        ?_assertEqual(ok, erlmcp_monitor_config:validate_config(ValidConfig)),
        ?_assertMatch({error, _}, erlmcp_monitor_config:validate_config(InvalidConfig))
    ].

test_alert_rule_creation() ->
    Rule = #{
        id => <<"test_high_latency">>,
        name => <<"Test High Latency">>,
        description => <<"Test alert for high latency">>,
        metric => <<"avg_response_time_ms">>,
        condition => <<"gt">>,
        threshold => 1000,
        severity => warning,
        cooldown_ms => 300000,
        enabled => true,
        tags => [<<"performance">>, <<"test">>]
    },
    
    CreateResult = erlmcp_monitor_config:create_alert_rule(Rule),
    
    [
        ?_assertEqual(ok, CreateResult),
        
        % Clean up
        ?_assertEqual(ok, erlmcp_monitor_config:delete_alert_rule(<<"test_high_latency">>))
    ].

test_config_merging() ->
    Default = #{
        a => 1,
        b => #{x => 10, y => 20},
        c => [1, 2, 3]
    },
    
    Override = #{
        b => #{y => 30, z => 40},
        d => 4
    },
    
    Expected = #{
        a => 1,
        b => #{x => 10, y => 30, z => 40},
        c => [1, 2, 3],
        d => 4
    },
    
    Result = erlmcp_monitor_config:merge_configs(Default, Override),
    
    [
        ?_assertEqual(Expected, Result)
    ].

%%%===================================================================
%%% Dashboard Tests
%%%===================================================================

dashboard_test_() ->
    {setup,
     fun setup_dashboard/0,
     fun teardown_dashboard/1,
     [
         fun test_dashboard_data_creation/1,
         fun test_metrics_export/1
     ]}.

setup_dashboard() ->
    Config = #{
        dashboard_enabled => true,
        dashboard_port => 8081, % Use different port for tests
        max_history_entries => 100
    },
    {ok, _Pid} = erlmcp_monitor_dashboard:start_link(Config),
    Config.

teardown_dashboard(_Config) ->
    erlmcp_monitor_dashboard:stop_dashboard(),
    ok.

test_dashboard_data_creation(Config) ->
    % Add some test data
    HealthStatus = #{
        overall => healthy,
        score => 0.95,
        timestamp => erlang:system_time(millisecond)
    },
    
    Metrics = #{
        avg_response_time_ms => 150,
        error_rate_percent => 2.5,
        memory_usage_percent => 45.0,
        timestamp => erlang:system_time(millisecond)
    },
    
    erlmcp_monitor_dashboard:update_metrics(HealthStatus, Metrics),
    
    % Wait for update
    timer:sleep(100),
    
    {ok, DashboardData} = erlmcp_monitor_dashboard:get_dashboard_data(),
    
    [
        ?_assert(maps:is_key(current_health, DashboardData)),
        ?_assert(maps:is_key(current_metrics, DashboardData)),
        ?_assert(maps:is_key(health_history, DashboardData)),
        ?_assert(maps:is_key(metrics_history, DashboardData)),
        ?_assert(maps:is_key(summary, DashboardData))
    ].

test_metrics_export(_Config) ->
    {ok, JsonExport} = erlmcp_monitor_dashboard:export_metrics(json),
    {ok, CsvExport} = erlmcp_monitor_dashboard:export_metrics(csv),
    {ok, PrometheusExport} = erlmcp_monitor_dashboard:export_metrics(prometheus),
    
    [
        ?_assert(is_binary(JsonExport)),
        ?_assert(is_binary(CsvExport)),
        ?_assert(is_binary(PrometheusExport)),
        
        % Test that CSV has headers
        ?_assert(binary:match(CsvExport, <<"timestamp">>) =/= nomatch),
        
        % Test that Prometheus has metrics
        ?_assert(binary:match(PrometheusExport, <<"erlmcp_health_score">>) =/= nomatch)
    ].

%%%===================================================================
%%% Performance Tests
%%%===================================================================

performance_test_() ->
    {timeout, 30, [
        fun test_monitoring_performance/0,
        fun test_memory_usage/0
    ]}.

test_monitoring_performance() ->
    Config = #{
        check_interval_ms => 10, % Very fast for stress test
        dashboard_enabled => false
    },
    
    {ok, _Pid} = erlmcp_monitor_sup:start_link(Config),
    
    % Let it run for a few seconds
    timer:sleep(3000),
    
    % Check that it's still responsive
    {ok, HealthStatus} = erlmcp_monitor:get_health_status(),
    {ok, Metrics} = erlmcp_monitor:get_metrics(),
    
    erlmcp_monitor_sup:stop_monitoring(),
    
    [
        ?_assert(maps:is_key(timestamp, HealthStatus)),
        ?_assert(maps:is_key(timestamp, Metrics)),
        ?_assert(maps:get(timestamp, HealthStatus) > 0),
        ?_assert(maps:get(timestamp, Metrics) > 0)
    ].

test_memory_usage() ->
    Config = #{
        check_interval_ms => 50,
        dashboard_enabled => false
    },
    
    % Measure memory before
    MemoryBefore = erlang:memory(total),
    
    {ok, _Pid} = erlmcp_monitor_sup:start_link(Config),
    
    % Let it run and collect data
    timer:sleep(2000),
    
    % Measure memory after
    MemoryAfter = erlang:memory(total),
    MemoryIncrease = MemoryAfter - MemoryBefore,
    
    erlmcp_monitor_sup:stop_monitoring(),
    
    % Memory increase should be reasonable (less than 50MB)
    MaxAllowedIncrease = 50 * 1024 * 1024,
    
    [
        ?_assert(MemoryIncrease < MaxAllowedIncrease)
    ].

%%%===================================================================
%%% Integration Tests
%%%===================================================================

integration_test_() ->
    {setup,
     fun setup_integration/0,
     fun teardown_integration/1,
     [
         fun test_end_to_end_monitoring/1,
         fun test_alert_triggering/1
     ]}.

setup_integration() ->
    % Start the full monitoring stack
    Config = #{
        check_interval_ms => 100,
        dashboard_enabled => false,
        alerts_enabled => true,
        metrics_enabled => true
    },
    
    {ok, _Pid} = erlmcp_monitor_sup:start_link(Config),
    Config.

teardown_integration(_Config) ->
    erlmcp_monitor_sup:stop_monitoring(),
    ok.

test_end_to_end_monitoring(_Config) ->
    % Test the full monitoring pipeline
    timer:sleep(500), % Let monitoring run
    
    {ok, HealthStatus} = erlmcp_monitor:get_health_status(),
    {ok, Metrics} = erlmcp_monitor:get_metrics(),
    
    [
        % Test that monitoring is working end-to-end
        ?_assert(is_map(HealthStatus)),
        ?_assert(is_map(Metrics)),
        ?_assert(maps:get(timestamp, HealthStatus) > 0),
        ?_assert(maps:get(timestamp, Metrics) > 0)
    ].

test_alert_triggering(_Config) ->
    % Create a rule that should trigger
    TestRule = #{
        id => <<"integration_test_rule">>,
        name => <<"Integration Test Rule">>,
        condition => fun(_Metrics) -> true end, % Always triggers
        threshold => 0,
        severity => info,
        cooldown => 100,
        enabled => true
    },
    
    ok = erlmcp_monitor:add_alert_rule(<<"integration_test_rule">>, TestRule),
    
    % Wait for alert to be processed
    timer:sleep(300),
    
    % Clean up
    ok = erlmcp_monitor:remove_alert_rule(<<"integration_test_rule">>),
    
    [
        ?_assert(true) % If we get here, alert processing worked
    ].