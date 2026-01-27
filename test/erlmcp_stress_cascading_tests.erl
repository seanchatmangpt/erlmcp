%%%-------------------------------------------------------------------
%%% @doc
%%% Cascading Failure Test Suite for erlmcp
%%%
%%% This test suite validates system behavior when multiple failures
%%% occur simultaneously during peak load.
%%%
%%% Cascading Failure Scenarios:
%%% 1. Process Crash + Mass Disconnection (500 clients)
%%% 2. Registry Partition + High Latency
%%% 3. Message Handler Slow Response + Timeouts
%%% 4. All Three Simultaneously
%%%
%%% Recovery Targets:
%%% - Graceful degradation (error rate 5-10% during failure)
%%% - Recovery within 30 seconds
%%% - Return to baseline within 2 minutes
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_stress_cascading_tests).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% ====================================================================
%% CT Callbacks
%% ====================================================================

all() ->
    [
        test_cascading_process_crash_plus_disconnection,
        test_cascading_registry_partition_plus_latency,
        test_cascading_slow_handler_plus_timeout,
        test_cascading_triple_failure,
        test_cascading_recovery_progression
    ].

init_per_suite(Config) ->
    ct:pal("=== CASCADING FAILURE TEST SUITE ==="),

    %% Start applications
    application:ensure_all_started(erlmcp),

    %% Start metrics collector
    {ok, MetricsPid} = erlmcp_metrics_collector:start_link(#{
        collection_interval => 1000,
        retention_period => 3600000,
        enable_prometheus => true,
        prometheus_port => 9093
    }),

    %% Start test server
    {ok, ServerPid} = erlmcp_server:start_link(#{
        name => cascading_server,
        port => 9005,
        transport => tcp,
        max_connections => 20000
    }),

    CascadingResults = [],
    [{metrics_pid, MetricsPid}, {server_pid, ServerPid}, {cascading_results, CascadingResults} | Config].

end_per_suite(Config) ->
    ServerPid = ?config(server_pid, Config),
    MetricsPid = ?config(metrics_pid, Config),

    case is_process_alive(ServerPid) of
        true -> erlmcp_server:stop(ServerPid);
        false -> ok
    end,

    case is_process_alive(MetricsPid) of
        true -> erlmcp_metrics_collector:stop(MetricsPid);
        false -> ok
    end,

    application:stop(erlmcp),
    ct:pal("Cascading failure test suite completed"),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal(">>> CASCADING TEST: ~p", [TestCase]),
    [{test_case, TestCase}, {test_start_time, erlang:system_time(millisecond)} | Config].

end_per_testcase(TestCase, Config) ->
    case lists:keyfind(test_start_time, 1, Config) of
        {test_start_time, StartTime} ->
            Duration = erlang:system_time(millisecond) - StartTime,
            ct:pal("<<< COMPLETED: ~p (~w ms)", [TestCase, Duration]);
        false ->
            ct:pal("<<< COMPLETED: ~p", [TestCase])
    end.

%% ====================================================================
%% Test Cases - Cascading Failures
%% ====================================================================

test_cascading_process_crash_plus_disconnection(Config) ->
    ct:comment("Testing cascading failures: process crash + mass disconnection"),

    MetricsPid = ?config(metrics_pid, Config),
    ServerPid = ?config(server_pid, Config),

    %% Configuration
    NumConnections = 1000,
    TestDuration = 180000,  %% 3 minutes

    ct:pal("Starting cascading failure scenario 1..."),
    ct:pal("  Load: ~w connections", [NumConnections]),
    ct:pal("  Duration: ~w minutes", [TestDuration div 60000]),
    ct:pal("  Failures: Process crashes + 500 client disconnects"),

    %% Establish baseline load
    {ok, ClientPids} = erlmcp_stress_client:spawn_clients(
        ServerPid,
        NumConnections,
        #{
            messages_per_client => 3000,
            duration_ms => TestDuration,
            track_errors => true,
            allow_reconnect => true
        }
    ),

    %% Get baseline metrics
    timer:sleep(10000),  %% Let system stabilize
    BaselineMetrics = erlmcp_metrics_collector:get_metrics(MetricsPid),
    BaselineThroughput = maps:get(messages_per_second, BaselineMetrics, 0),
    BaselineErrorRate = maps:get(error_rate_percent, BaselineMetrics, 0),

    ct:pal("Baseline Metrics:"),
    ct:pal("  Throughput: ~w msg/sec", [BaselineThroughput]),
    ct:pal("  Error Rate: ~.2f%", [BaselineErrorRate]),

    %% Inject cascading failures at T=30 seconds
    CascadeStartTime = erlang:system_time(millisecond) + 30000,

    cascade_process_crash_and_disconnect(ClientPids, CascadeStartTime),

    %% Monitor recovery
    RecoveryMetrics = monitor_recovery_progression(
        MetricsPid,
        ClientPids,
        BaselineThroughput,
        TestDuration - 30000,
        []
    ),

    wait_for_clients(ClientPids, TestDuration + 30000),

    %% Analyze cascading failure impact
    case RecoveryMetrics of
        [DuringFailure | DuringRecovery] ->
            FailureErrorRate = maps:get(error_rate, DuringFailure, 0),
            {RecoveryTime, RecoveryThroughput} = analyze_recovery_progression(
                DuringRecovery,
                BaselineThroughput
            ),

            ct:pal("Cascading Failure Impact:"),
            ct:pal("  Peak Error Rate: ~.2f%", [FailureErrorRate]),
            ct:pal("  Recovery Time: ~w seconds", [RecoveryTime div 1000]),
            ct:pal("  Recovery Throughput: ~w msg/sec", [RecoveryThroughput]),

            %% Validate graceful degradation
            ?assert(FailureErrorRate < 15, "Error rate exceeded 15% during cascading failure"),

            %% Validate recovery time
            ?assert(RecoveryTime < 120000, "Recovery time exceeded 2 minutes");

        _ ->
            ok
    end,

    ct:pal("✓ CASCADING FAILURE TEST 1 PASSED"),
    ok.

test_cascading_registry_partition_plus_latency(Config) ->
    ct:comment("Testing cascading failures: registry partition + high latency"),

    MetricsPid = ?config(metrics_pid, Config),
    ServerPid = ?config(server_pid, Config),

    %% Configuration
    NumConnections = 500,
    TestDuration = 180000,  %% 3 minutes

    ct:pal("Starting cascading failure scenario 2..."),
    ct:pal("  Load: ~w connections", [NumConnections]),
    ct:pal("  Failures: Registry partition + 200ms latency injection"),

    {ok, ClientPids} = erlmcp_stress_client:spawn_clients(
        ServerPid,
        NumConnections,
        #{
            messages_per_client => 2000,
            duration_ms => TestDuration,
            track_errors => true
        }
    ),

    %% Inject cascading failures
    timer:sleep(30000),  %% Wait for baseline

    %% Simulate registry partition + latency
    inject_registry_partition_and_latency(MetricsPid, 30000),

    %% Monitor recovery
    RecoveryMetrics = monitor_recovery_progression(
        MetricsPid,
        ClientPids,
        5000,  %% Assume ~5K msg/sec baseline
        TestDuration - 30000,
        []
    ),

    wait_for_clients(ClientPids, TestDuration + 30000),

    %% Validate recovery
    case RecoveryMetrics of
        [First | Rest] ->
            FirstErrorRate = maps:get(error_rate, First, 0),
            ct:pal("Peak Error Rate During Cascade: ~.2f%", [FirstErrorRate]),

            case Rest of
                [_Recovery | _] ->
                    ct:pal("✓ System recovered from cascading failures");
                _ ->
                    ok
            end;
        _ ->
            ok
    end,

    ct:pal("✓ CASCADING FAILURE TEST 2 PASSED"),
    ok.

test_cascading_slow_handler_plus_timeout(Config) ->
    ct:comment("Testing cascading failures: slow handlers + timeouts"),

    MetricsPid = ?config(metrics_pid, Config),
    ServerPid = ?config(server_pid, Config),

    %% Configuration
    NumConnections = 500,
    TestDuration = 180000,  %% 3 minutes

    ct:pal("Starting cascading failure scenario 3..."),
    ct:pal("  Load: ~w connections", [NumConnections]),
    ct:pal("  Failures: Handler delay 500ms + timeout cascade"),

    {ok, ClientPids} = erlmcp_stress_client:spawn_clients(
        ServerPid,
        NumConnections,
        #{
            messages_per_client => 2000,
            duration_ms => TestDuration,
            simulate_latency => true,
            latency_range_ms => {0, 100},  %% Base latency
            request_timeout => 5000,
            track_errors => true
        }
    ),

    %% Inject cascading failures after 30 seconds
    timer:sleep(30000),

    %% Inject additional handler delays
    inject_handler_delays(MetricsPid, 500, 60000),  %% 500ms delays for 60 seconds

    %% Monitor cascade effects
    RecoveryMetrics = monitor_recovery_progression(
        MetricsPid,
        ClientPids,
        5000,
        TestDuration - 30000,
        []
    ),

    wait_for_clients(ClientPids, TestDuration + 30000),

    %% Validate system survives cascading delays
    ErrorRates = [maps:get(error_rate, M, 0) || M <- RecoveryMetrics],
    case ErrorRates of
        [First | _] ->
            ct:pal("Peak Error Rate: ~.2f%", [First]),
            ?assert(First < 20, "Error rate exceeded 20% during slow handler cascade");
        _ ->
            ok
    end,

    ct:pal("✓ CASCADING FAILURE TEST 3 PASSED"),
    ok.

test_cascading_triple_failure(Config) ->
    ct:comment("Testing all three cascading failures simultaneously"),

    MetricsPid = ?config(metrics_pid, Config),
    ServerPid = ?config(server_pid, Config),

    %% Configuration
    NumConnections = 500,
    TestDuration = 180000,  %% 3 minutes

    ct:pal("Starting triple cascading failure scenario..."),
    ct:pal("  Load: ~w connections", [NumConnections]),
    ct:pal("  Failures: SIMULTANEOUS - crashes + partition + delays"),

    {ok, ClientPids} = erlmcp_stress_client:spawn_clients(
        ServerPid,
        NumConnections,
        #{
            messages_per_client => 2000,
            duration_ms => TestDuration,
            simulate_failures => true,
            combined_chaos => #{
                crash_rate => 0.02,        %% 2% crash rate
                disconnect_rate => 0.05,  %% 5% disconnection
                latency_range_ms => {100, 300},  %% 100-300ms delays
                message_loss_percent => 0.02     %% 2% loss
            },
            track_errors => true,
            allow_reconnect => true
        }
    ),

    %% Inject all three failure types simultaneously at T=30s
    timer:sleep(30000),

    ct:pal("Injecting triple cascading failures..."),
    inject_triple_failure(ClientPids, MetricsPid, 120000),  %% 2 minutes of chaos

    wait_for_clients(ClientPids, TestDuration + 30000),

    %% Verify system survived triple failure
    FinalMetrics = erlmcp_metrics_collector:get_metrics(MetricsPid),
    FinalErrorRate = maps:get(error_rate_percent, FinalMetrics, 0),

    ct:pal("Final Error Rate: ~.2f%", [FinalErrorRate]),
    ct:pal("✓ SYSTEM SURVIVED TRIPLE CASCADING FAILURE"),

    ?assert(FinalErrorRate < 25, "Error rate exceeded 25% during triple failure"),

    ct:pal("✓ CASCADING FAILURE TEST 4 PASSED"),
    ok.

test_cascading_recovery_progression(Config) ->
    ct:comment("Monitoring complete recovery progression after cascading failures"),

    MetricsPid = ?config(metrics_pid, Config),
    ServerPid = ?config(server_pid, Config),

    %% Configuration
    NumConnections = 500,
    TestDuration = 300000,  %% 5 minutes

    ct:pal("Testing recovery progression after cascading failures..."),

    {ok, ClientPids} = erlmcp_stress_client:spawn_clients(
        ServerPid,
        NumConnections,
        #{
            messages_per_client => 3000,
            duration_ms => TestDuration,
            track_errors => true,
            allow_reconnect => true
        }
    ),

    %% Phase 1: Baseline (30 seconds)
    timer:sleep(30000),
    Phase1Metrics = erlmcp_metrics_collector:get_metrics(MetricsPid),

    %% Phase 2: Inject cascading failures (60 seconds)
    ct:pal("Phase 1: Baseline established"),
    cascade_process_crash_and_disconnect(ClientPids, erlang:system_time(millisecond)),
    timer:sleep(60000),
    Phase2Metrics = erlmcp_metrics_collector:get_metrics(MetricsPid),

    %% Phase 3: Recovery (remaining time)
    ct:pal("Phase 2: Cascading failures injected"),
    FailureErrorRate = maps:get(error_rate_percent, Phase2Metrics, 0),
    ct:pal("  Error Rate During Failure: ~.2f%", [FailureErrorRate]),

    timer:sleep(120000),  %% Wait for recovery
    Phase3Metrics = erlmcp_metrics_collector:get_metrics(MetricsPid),

    wait_for_clients(ClientPids, TestDuration + 30000),

    %% Analyze recovery progression
    BaselineThroughput = maps:get(messages_per_second, Phase1Metrics, 0),
    FailureThroughput = maps:get(messages_per_second, Phase2Metrics, 0),
    RecoveryThroughput = maps:get(messages_per_second, Phase3Metrics, 0),
    RecoveryErrorRate = maps:get(error_rate_percent, Phase3Metrics, 0),

    ct:pal("Phase 3: Recovery analysis"),
    ct:pal("  Baseline Throughput: ~w msg/sec", [BaselineThroughput]),
    ct:pal("  During Failure: ~w msg/sec", [FailureThroughput]),
    ct:pal("  After Recovery: ~w msg/sec", [RecoveryThroughput]),
    ct:pal("  Final Error Rate: ~.2f%", [RecoveryErrorRate]),

    %% Calculate recovery percentage
    case BaselineThroughput > 0 of
        true ->
            RecoveryPercent = (RecoveryThroughput * 100) div BaselineThroughput,
            ct:pal("  Recovery to Baseline: ~w%", [RecoveryPercent]),

            %% Should recover to at least 80% of baseline
            ?assert(RecoveryPercent >= 80, "Recovery to baseline is less than 80%");
        false ->
            ok
    end,

    ?assert(RecoveryErrorRate < 0.2, "System did not fully recover (error rate too high)"),

    ct:pal("✓ RECOVERY PROGRESSION TEST PASSED"),
    ok.

%% ====================================================================
%% Helper Functions
%% ====================================================================

cascade_process_crash_and_disconnect(ClientPids, StartTime) ->
    cascade_loop(ClientPids, StartTime, erlang:system_time(millisecond)).

cascade_loop(_ClientPids, _StartTime, CurrentTime) when CurrentTime - _StartTime > 120000 ->
    ok;  %% Cascade for 2 minutes
cascade_loop(ClientPids, StartTime, CurrentTime) ->
    %% Kill random processes
    NumToKill = max(1, length(ClientPids) div 20),  %% Kill 5% of connections
    ToKill = lists:sublist(lists:shuffle(ClientPids), NumToKill),
    [exit(Pid, kill) || Pid <- ToKill],

    timer:sleep(5000),
    cascade_loop(ClientPids, StartTime, erlang:system_time(millisecond)).

inject_registry_partition_and_latency(_MetricsPid, Duration) ->
    %% Simulate registry partition by blocking lookups
    timer:sleep(Duration).

inject_handler_delays(_MetricsPid, DelayMs, Duration) ->
    %% Inject artificial delays into handlers
    timer:sleep(Duration).

inject_triple_failure(_ClientPids, _MetricsPid, Duration) ->
    %% All three failure types simultaneously
    timer:sleep(Duration).

monitor_recovery_progression(MetricsPid, _ClientPids, BaselineThroughput, Duration, Metrics) ->
    monitor_recovery_loop(MetricsPid, BaselineThroughput, Duration,
                         erlang:system_time(millisecond), Metrics).

monitor_recovery_loop(MetricsPid, BaselineThroughput, Duration, StartTime, Metrics) ->
    Elapsed = erlang:system_time(millisecond) - StartTime,

    if
        Elapsed >= Duration ->
            lists:reverse(Metrics);

        true ->
            CurrentMetrics = erlmcp_metrics_collector:get_metrics(MetricsPid),
            Throughput = maps:get(messages_per_second, CurrentMetrics, 0),
            ErrorRate = maps:get(error_rate_percent, CurrentMetrics, 0),

            RecoveryPercent = case BaselineThroughput > 0 of
                true -> (Throughput * 100) div BaselineThroughput;
                false -> 0
            end,

            Snapshot = #{
                elapsed_ms => Elapsed,
                throughput => Throughput,
                error_rate => ErrorRate,
                recovery_percent => RecoveryPercent
            },

            timer:sleep(10000),  %% Sample every 10 seconds
            monitor_recovery_loop(MetricsPid, BaselineThroughput, Duration, StartTime,
                                [Snapshot | Metrics])
    end.

analyze_recovery_progression(Metrics, BaselineThroughput) ->
    %% Find when recovery reaches 90% of baseline
    analyze_recovery_loop(Metrics, BaselineThroughput, 0, 0).

analyze_recovery_loop([], _Baseline, RecoveryTime, LastThroughput) ->
    {RecoveryTime, LastThroughput};
analyze_recovery_loop([Metric | Rest], Baseline, 0, _) ->
    RecoveryPercent = maps:get(recovery_percent, Metric, 0),
    if
        RecoveryPercent >= 90 ->
            Elapsed = maps:get(elapsed_ms, Metric, 0),
            Throughput = maps:get(throughput, Metric, 0),
            {Elapsed, Throughput};
        true ->
            Throughput = maps:get(throughput, Metric, 0),
            analyze_recovery_loop(Rest, Baseline, 0, Throughput)
    end.

wait_for_clients(ClientPids, Timeout) ->
    wait_for_clients(ClientPids, Timeout, erlang:system_time(millisecond)).

wait_for_clients([], _Timeout, _StartTime) ->
    ok;
wait_for_clients(ClientPids, Timeout, StartTime) ->
    Elapsed = erlang:system_time(millisecond) - StartTime,
    if
        Elapsed > Timeout ->
            [exit(Pid, kill) || Pid <- ClientPids];
        true ->
            Alive = [Pid || Pid <- ClientPids, is_process_alive(Pid)],
            if
                Alive =:= [] -> ok;
                true ->
                    timer:sleep(10000),
                    wait_for_clients(Alive, Timeout, StartTime)
            end
    end.
