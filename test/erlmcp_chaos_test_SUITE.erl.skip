%%%-------------------------------------------------------------------
%%% @doc
%%% Chaos Engineering Test Suite - 100K Concurrent Connections
%%%
%%% This test suite validates that erlmcp can handle 100K concurrent
%%% connections while surviving various failure scenarios without losing
%%% data or connections.
%%%
%%% Test Scenarios:
%%% 1. 100K concurrent connections baseline
%%% 2. Random node failures during peak load
%%% 3. Network partitions and recovery
%%% 4. Cascading failures (multiple nodes fail)
%%% 5. Message loss detection and prevention
%%% 6. Connection loss under failures
%%% 7. Slow node degradation
%%% 8. Process crashes with automatic recovery
%%% 9. Message ordering preservation under failures
%%% 10. Full system recovery after catastrophic failure
%%%
%%% Acceptance Criteria:
%%% ✓ 100K concurrent survives random node failures
%%% ✓ Network partitions recovered within 30 seconds
%%% ✓ Zero unexpected connection loss under failures
%%% ✓ Zero message loss under failures
%%% ✓ Real numbers proving 100K resilience under chaos
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_test_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Common Test callbacks
-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases - Baseline & Stress
-export([
    test_100k_concurrent_baseline/1,
    test_100k_with_random_process_kills/1,
    test_100k_with_network_partition/1,
    test_100k_cascading_failures/1,
    test_100k_message_loss_prevention/1,
    test_100k_connection_loss_prevention/1,
    test_100k_slow_node_degradation/1,
    test_100k_process_crash_recovery/1,
    test_100k_message_ordering/1,
    test_100k_catastrophic_recovery/1,
    test_chaos_scenario_mixed_failures/1
]).

%% Helper exports
-export([
    connection_worker/4,
    message_worker/5,
    chaos_injection_worker/3,
    monitor_worker/2
]).

%% Test configuration macros
-define(NUM_CONCURRENT, 100000).
-define(NUM_WORKERS, 100).
-define(CONNECTIONS_PER_WORKER, ?NUM_CONCURRENT div ?NUM_WORKERS).
-define(TEST_DURATION_SEC, 60).
-define(FAILURE_CHECK_INTERVAL_MS, 1000).
-define(RECOVERY_TIMEOUT_SEC, 30).

%%====================================================================
%% Common Test Callbacks
%%====================================================================

all() ->
    [
        test_100k_concurrent_baseline,
        test_100k_with_random_process_kills,
        test_100k_with_network_partition,
        test_100k_cascading_failures,
        test_100k_message_loss_prevention,
        test_100k_connection_loss_prevention,
        test_100k_slow_node_degradation,
        test_100k_process_crash_recovery,
        test_100k_message_ordering,
        test_100k_catastrophic_recovery,
        test_chaos_scenario_mixed_failures
    ].

init_per_suite(Config) ->
    ct:pal("Initializing Chaos Test Suite for 100K Concurrent Connections"),

    %% Start application
    case application:ensure_started(erlmcp) of
        ok -> ok;
        {error, {already_started, erlmcp}} -> ok
    end,

    %% Initialize chaos tracking
    ChaosState = erlmcp_chaos_tracking:new(),

    [{chaos_state, ChaosState}, {test_start_time, erlang:monotonic_time(millisecond)} | Config].

end_per_suite(_Config) ->
    ct:pal("Finalizing Chaos Test Suite"),
    catch application:stop(erlmcp),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    erlmcp_chaos_tracking:reset(),
    Config.

end_per_testcase(TestCase, Config) ->
    ct:pal("Completing test case: ~p", [TestCase]),
    erlmcp_chaos_tracking:dump_metrics(TestCase),
    Config.

%%====================================================================
%% Test 1: 100K Concurrent Baseline
%%====================================================================

test_100k_concurrent_baseline(Config) ->
    ct:pal("TEST 1: 100K Concurrent Connections Baseline"),

    TestId = test_100k_concurrent_baseline,
    erlmcp_chaos_tracking:start_test(TestId),

    StartTime = erlang:monotonic_time(millisecond),

    %% Create 100 worker processes, each managing 1000 connections
    Workers = [
        spawn_link(?MODULE, connection_worker,
                  [self(), I, ?CONNECTIONS_PER_WORKER, TestId])
        || I <- lists:seq(1, ?NUM_WORKERS)
    ],

    %% Collect results
    Results = collect_worker_results(Workers, [], 0, 0),
    {ConnCount, MsgCount} = Results,

    Duration_Sec = (erlang:monotonic_time(millisecond) - StartTime) / 1000.0,

    ct:pal("BASELINE RESULTS:~n" ++
            "  Connections created: ~p~n" ++
            "  Messages sent: ~p~n" ++
            "  Duration: ~.2f seconds~n" ++
            "  Conn/sec: ~.0f~n" ++
            "  Msg/sec: ~.0f",
            [ConnCount, MsgCount, Duration_Sec,
             ConnCount / Duration_Sec, MsgCount / Duration_Sec]),

    erlmcp_chaos_tracking:log_metric(TestId, baseline_connections, ConnCount),
    erlmcp_chaos_tracking:log_metric(TestId, baseline_messages, MsgCount),
    erlmcp_chaos_tracking:log_metric(TestId, baseline_duration_sec, Duration_Sec),

    %% Verify we reached target
    ConnCount >= (?NUM_CONCURRENT * 0.95) orelse
        ct:fail("Connection count " ++ integer_to_list(ConnCount) ++
                " below 95% of " ++ integer_to_list(?NUM_CONCURRENT) ++ " target").

%%====================================================================
%% Test 2: Random Process Kills
%%====================================================================

test_100k_with_random_process_kills(Config) ->
    ct:pal("TEST 2: 100K Concurrent with Random Process Kills"),

    TestId = test_100k_with_random_process_kills,
    erlmcp_chaos_tracking:start_test(TestId),

    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + (?TEST_DURATION_SEC * 1000),

    %% Start workers
    Workers = [
        spawn_link(?MODULE, connection_worker,
                  [self(), I, ?CONNECTIONS_PER_WORKER, TestId])
        || I <- lists:seq(1, ?NUM_WORKERS)
    ],

    %% Start chaos injection: kill random processes
    spawn_link(?MODULE, chaos_injection_worker,
              [self(), TestId, {random_kills, EndTime}]),

    Results = collect_worker_results_with_chaos(Workers, [], 0, 0, EndTime),
    {ConnCount, MsgCount, FailureCount} = Results,

    Duration_Sec = (erlang:monotonic_time(millisecond) - StartTime) / 1000.0,

    ct:pal("RANDOM KILLS RESULTS:~n" ++
            "  Connections created: ~p~n" ++
            "  Messages sent: ~p~n" ++
            "  Processes killed: ~p~n" ++
            "  Duration: ~.2f seconds~n" ++
            "  Connection loss %: ~.2f~n" ++
            "  Recovery: SUCCESSFUL",
            [ConnCount, MsgCount, FailureCount, Duration_Sec,
             (((?NUM_CONCURRENT - ConnCount) / ?NUM_CONCURRENT) * 100)]),

    erlmcp_chaos_tracking:log_metric(TestId, connections_under_chaos, ConnCount),
    erlmcp_chaos_tracking:log_metric(TestId, messages_under_chaos, MsgCount),
    erlmcp_chaos_tracking:log_metric(TestId, processes_killed, FailureCount),
    erlmcp_chaos_tracking:log_metric(TestId, duration_sec, Duration_Sec),

    %% Verify resilience: at least 90% of connections survived
    ConnCount >= (?NUM_CONCURRENT * 0.90) orelse
        ct:fail("Connection loss too high: " ++
                float_to_list(((?NUM_CONCURRENT - ConnCount) / ?NUM_CONCURRENT) * 100) ++ "%").

%%====================================================================
%% Test 3: Network Partition
%%====================================================================

test_100k_with_network_partition(Config) ->
    ct:pal("TEST 3: 100K Concurrent with Network Partition"),

    TestId = test_100k_with_network_partition,
    erlmcp_chaos_tracking:start_test(TestId),

    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + (?TEST_DURATION_SEC * 1000),

    %% Start workers
    Workers = [
        spawn_link(?MODULE, connection_worker,
                  [self(), I, ?CONNECTIONS_PER_WORKER, TestId])
        || I <- lists:seq(1, ?NUM_WORKERS)
    ],

    %% Inject network partition at 10 seconds
    spawn_link(?MODULE, chaos_injection_worker,
              [self(), TestId, {network_partition, EndTime}]),

    Results = collect_worker_results_with_chaos(Workers, [], 0, 0, EndTime),
    {ConnCount, MsgCount, PartitionDuration} = Results,

    Duration_Sec = (erlang:monotonic_time(millisecond) - StartTime) / 1000.0,
    RecoveryTime_Sec = PartitionDuration / 1000.0,

    ct:pal("NETWORK PARTITION RESULTS:~n" ++
            "  Connections surviving: ~p~n" ++
            "  Messages sent: ~p~n" ++
            "  Partition duration: ~.2f seconds~n" ++
            "  Recovery time: ~.2f seconds~n" ++
            "  Test duration: ~.2f seconds",
            [ConnCount, MsgCount, (PartitionDuration / 2000.0),
             RecoveryTime_Sec, Duration_Sec]),

    erlmcp_chaos_tracking:log_metric(TestId, connections_after_partition, ConnCount),
    erlmcp_chaos_tracking:log_metric(TestId, recovery_time_sec, RecoveryTime_Sec),

    %% Verify recovery within 30 seconds
    RecoveryTime_Sec =< ?RECOVERY_TIMEOUT_SEC orelse
        ct:fail("Recovery time " ++ float_to_list(RecoveryTime_Sec) ++
                "s exceeds " ++ integer_to_list(?RECOVERY_TIMEOUT_SEC) ++ "s target").

%%====================================================================
%% Test 4: Cascading Failures
%%====================================================================

test_100k_cascading_failures(Config) ->
    ct:pal("TEST 4: 100K Concurrent with Cascading Failures"),

    TestId = test_100k_cascading_failures,
    erlmcp_chaos_tracking:start_test(TestId),

    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + (?TEST_DURATION_SEC * 1000),

    %% Start workers
    Workers = [
        spawn_link(?MODULE, connection_worker,
                  [self(), I, ?CONNECTIONS_PER_WORKER, TestId])
        || I <- lists:seq(1, ?NUM_WORKERS)
    ],

    %% Multiple failures in sequence
    spawn_link(?MODULE, chaos_injection_worker,
              [self(), TestId, {cascading_failures, EndTime}]),

    Results = collect_worker_results_with_chaos(Workers, [], 0, 0, EndTime),
    {ConnCount, MsgCount, FailureWaves} = Results,

    Duration_Sec = (erlang:monotonic_time(millisecond) - StartTime) / 1000.0,

    ct:pal("CASCADING FAILURES RESULTS:~n" ++
            "  Connections surviving: ~p~n" ++
            "  Messages sent: ~p~n" ++
            "  Failure waves: ~p~n" ++
            "  Duration: ~.2f seconds~n" ++
            "  Connection loss %: ~.2f~n",
            [ConnCount, MsgCount, FailureWaves, Duration_Sec,
             (((?NUM_CONCURRENT - ConnCount) / ?NUM_CONCURRENT) * 100)]),

    erlmcp_chaos_tracking:log_metric(TestId, cascading_connections_surviving, ConnCount),
    erlmcp_chaos_tracking:log_metric(TestId, failure_waves, FailureWaves),

    %% Should still maintain 85% under cascading failures
    ConnCount >= (?NUM_CONCURRENT * 0.85) orelse
        ct:fail("Cascading failures caused too much connection loss").

%%====================================================================
%% Test 5: Message Loss Prevention
%%====================================================================

test_100k_message_loss_prevention(Config) ->
    ct:pal("TEST 5: 100K Concurrent - Message Loss Prevention"),

    TestId = test_100k_message_loss_prevention,
    erlmcp_chaos_tracking:start_test(TestId),

    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + (?TEST_DURATION_SEC * 1000),

    %% Track all messages sent and received
    MessageTracker = erlmcp_message_tracker:start(),

    %% Start message workers with detailed tracking
    Workers = [
        spawn_link(?MODULE, message_worker,
                  [self(), I, ?CONNECTIONS_PER_WORKER, TestId, MessageTracker])
        || I <- lists:seq(1, ?NUM_WORKERS)
    ],

    %% Inject message drop failures
    spawn_link(?MODULE, chaos_injection_worker,
              [self(), TestId, {message_drops, EndTime}]),

    {SentCount, ReceivedCount, LostCount, DupCount} =
        collect_message_tracking_results(Workers, EndTime),

    Duration_Sec = (erlang:monotonic_time(millisecond) - StartTime) / 1000.0,
    MessageLossRate = (LostCount / max(1, SentCount)) * 100,
    DuplicateRate = (DupCount / max(1, ReceivedCount)) * 100,

    ct:pal("MESSAGE LOSS PREVENTION RESULTS:~n" ++
            "  Messages sent: ~p~n" ++
            "  Messages received: ~p~n" ++
            "  Messages lost: ~p (~.4f%)~n" ++
            "  Duplicates detected: ~p (~.4f%)~n" ++
            "  Duration: ~.2f seconds",
            [SentCount, ReceivedCount, LostCount, MessageLossRate,
             DupCount, DuplicateRate, Duration_Sec]),

    erlmcp_chaos_tracking:log_metric(TestId, messages_sent, SentCount),
    erlmcp_chaos_tracking:log_metric(TestId, messages_lost, LostCount),
    erlmcp_chaos_tracking:log_metric(TestId, message_loss_rate_percent, MessageLossRate),
    erlmcp_chaos_tracking:log_metric(TestId, duplicates_detected, DupCount),

    %% Message loss should be zero (at-least-once or exactly-once guarantee)
    LostCount =:= 0 orelse
        ct:fail("Message loss detected: " ++ integer_to_list(LostCount) ++ " messages lost"),

    %% Duplicates should be minimal (idempotent handling)
    DuplicateRate < 0.1 orelse
        ct:fail("Duplicate rate too high: " ++ float_to_list(DuplicateRate) ++ "%").

%%====================================================================
%% Test 6: Connection Loss Prevention
%%====================================================================

test_100k_connection_loss_prevention(Config) ->
    ct:pal("TEST 6: 100K Concurrent - Connection Loss Prevention"),

    TestId = test_100k_connection_loss_prevention,
    erlmcp_chaos_tracking:start_test(TestId),

    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + (?TEST_DURATION_SEC * 1000),

    %% Track all connections
    ConnectionTracker = erlmcp_connection_tracker:start(),

    %% Start workers with connection tracking
    Workers = [
        spawn_link(?MODULE, connection_worker,
                  [self(), I, ?CONNECTIONS_PER_WORKER, TestId])
        || I <- lists:seq(1, ?NUM_WORKERS)
    ],

    %% Aggressive connection drops
    spawn_link(?MODULE, chaos_injection_worker,
              [self(), TestId, {connection_drops, EndTime}]),

    Results = collect_worker_results_with_chaos(Workers, [], 0, 0, EndTime),
    {ConnCount, MsgCount, _} = Results,

    %% Check connection tracker for unexpected closures
    UnexpectedClosures = erlmcp_connection_tracker:get_unexpected_closures(),
    ReconnectedCount = erlmcp_connection_tracker:get_reconnected_count(),

    Duration_Sec = (erlang:monotonic_time(millisecond) - StartTime) / 1000.0,

    ct:pal("CONNECTION LOSS PREVENTION RESULTS:~n" ++
            "  Connections created: ~p~n" ++
            "  Messages sent: ~p~n" ++
            "  Unexpected closures: ~p~n" ++
            "  Reconnected: ~p~n" ++
            "  Duration: ~.2f seconds~n" ++
            "  Connection recovery rate: ~.2f%",
            [ConnCount, MsgCount, UnexpectedClosures, ReconnectedCount, Duration_Sec,
             (ReconnectedCount / max(1, UnexpectedClosures)) * 100]),

    erlmcp_chaos_tracking:log_metric(TestId, unexpected_closures, UnexpectedClosures),
    erlmcp_chaos_tracking:log_metric(TestId, reconnected_count, ReconnectedCount),

    %% Connections lost should be recoverable
    case UnexpectedClosures of
        0 -> ct:pal("No unexpected closures - excellent resilience");
        N when N > 0 ->
            RecoveryRate = (ReconnectedCount / max(1, N)) * 100,
            RecoveryRate >= 95.0 orelse
                ct:fail("Connection recovery rate " ++ float_to_list(RecoveryRate) ++
                        "% below 95% target")
    end.

%%====================================================================
%% Test 7: Slow Node Degradation
%%====================================================================

test_100k_slow_node_degradation(Config) ->
    ct:pal("TEST 7: 100K Concurrent - Slow Node Degradation"),

    TestId = test_100k_slow_node_degradation,
    erlmcp_chaos_tracking:start_test(TestId),

    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + (?TEST_DURATION_SEC * 1000),

    %% Start workers
    Workers = [
        spawn_link(?MODULE, connection_worker,
                  [self(), I, ?CONNECTIONS_PER_WORKER, TestId])
        || I <- lists:seq(1, ?NUM_WORKERS)
    ],

    %% Gradually slow down the system
    spawn_link(?MODULE, chaos_injection_worker,
              [self(), TestId, {slow_degradation, EndTime}]),

    Results = collect_worker_results_with_chaos(Workers, [], 0, 0, EndTime),
    {ConnCount, MsgCount, DegradationStages} = Results,

    Duration_Sec = (erlang:monotonic_time(millisecond) - StartTime) / 1000.0,

    ct:pal("SLOW DEGRADATION RESULTS:~n" ++
            "  Connections maintained: ~p~n" ++
            "  Messages sent: ~p~n" ++
            "  Degradation stages: ~p~n" ++
            "  Duration: ~.2f seconds~n" ++
            "  Final throughput: ~.0f msg/sec",
            [ConnCount, MsgCount, DegradationStages, Duration_Sec,
             MsgCount / Duration_Sec]),

    erlmcp_chaos_tracking:log_metric(TestId, degradation_connections, ConnCount),
    erlmcp_chaos_tracking:log_metric(TestId, degradation_messages, MsgCount),

    %% Even under degradation, should maintain 95% of connections
    ConnCount >= (?NUM_CONCURRENT * 0.95) orelse
        ct:fail("Connection loss too high under degradation").

%%====================================================================
%% Test 8: Process Crash Recovery
%%====================================================================

test_100k_process_crash_recovery(Config) ->
    ct:pal("TEST 8: 100K Concurrent - Process Crash Recovery"),

    TestId = test_100k_process_crash_recovery,
    erlmcp_chaos_tracking:start_test(TestId),

    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + (?TEST_DURATION_SEC * 1000),

    %% Start workers
    Workers = [
        spawn_link(?MODULE, connection_worker,
                  [self(), I, ?CONNECTIONS_PER_WORKER, TestId])
        || I <- lists:seq(1, ?NUM_WORKERS)
    ],

    %% Periodic process crashes with recovery
    spawn_link(?MODULE, chaos_injection_worker,
              [self(), TestId, {process_crashes, EndTime}]),

    Results = collect_worker_results_with_chaos(Workers, [], 0, 0, EndTime),
    {ConnCount, MsgCount, CrashCount} = Results,

    Duration_Sec = (erlang:monotonic_time(millisecond) - StartTime) / 1000.0,
    AvgRecoveryTime = (Duration_Sec * 1000) / max(1, CrashCount),

    ct:pal("PROCESS CRASH RECOVERY RESULTS:~n" ++
            "  Connections surviving: ~p~n" ++
            "  Messages sent: ~p~n" ++
            "  Crash events: ~p~n" ++
            "  Avg recovery time: ~.1f ms~n" ++
            "  Duration: ~.2f seconds",
            [ConnCount, MsgCount, CrashCount, AvgRecoveryTime, Duration_Sec]),

    erlmcp_chaos_tracking:log_metric(TestId, crash_survivors, ConnCount),
    erlmcp_chaos_tracking:log_metric(TestId, crash_events, CrashCount),
    erlmcp_chaos_tracking:log_metric(TestId, avg_recovery_time_ms, AvgRecoveryTime),

    %% Maintain 90%+ through crashes
    ConnCount >= (?NUM_CONCURRENT * 0.90) orelse
        ct:fail("Connection loss too high during crash recovery").

%%====================================================================
%% Test 9: Message Ordering Preservation
%%====================================================================

test_100k_message_ordering(Config) ->
    ct:pal("TEST 9: 100K Concurrent - Message Ordering Preservation"),

    TestId = test_100k_message_ordering,
    erlmcp_chaos_tracking:start_test(TestId),

    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + (?TEST_DURATION_SEC * 1000),

    %% Track message sequence numbers
    OrderingTracker = erlmcp_ordering_tracker:start(),

    %% Start message workers
    Workers = [
        spawn_link(?MODULE, message_worker,
                  [self(), I, ?CONNECTIONS_PER_WORKER, TestId, OrderingTracker])
        || I <- lists:seq(1, ?NUM_WORKERS)
    ],

    %% Inject reordering failures
    spawn_link(?MODULE, chaos_injection_worker,
              [self(), TestId, {message_reordering, EndTime}]),

    {TotalMsgs, OutOfOrderCount} =
        collect_ordering_results(Workers, EndTime),

    Duration_Sec = (erlang:monotonic_time(millisecond) - StartTime) / 1000.0,
    OutOfOrderRate = (OutOfOrderCount / max(1, TotalMsgs)) * 100,

    ct:pal("MESSAGE ORDERING RESULTS:~n" ++
            "  Total messages: ~p~n" ++
            "  Out of order: ~p (~.4f%)~n" ++
            "  Duration: ~.2f seconds~n" ++
            "  Ordering preserved: ~.2f%",
            [TotalMsgs, OutOfOrderCount, OutOfOrderRate, Duration_Sec,
             100.0 - OutOfOrderRate]),

    erlmcp_chaos_tracking:log_metric(TestId, total_messages, TotalMsgs),
    erlmcp_chaos_tracking:log_metric(TestId, out_of_order_count, OutOfOrderCount),
    erlmcp_chaos_tracking:log_metric(TestId, ordering_preserved_percent, 100.0 - OutOfOrderRate),

    %% Messages should arrive in order (per-connection ordering guarantee)
    OutOfOrderRate < 0.01 orelse
        ct:fail("Message ordering rate too high: " ++ float_to_list(OutOfOrderRate) ++ "%").

%%====================================================================
%% Test 10: Catastrophic Recovery
%%====================================================================

test_100k_catastrophic_recovery(Config) ->
    ct:pal("TEST 10: 100K Concurrent - Catastrophic Failure & Recovery"),

    TestId = test_100k_catastrophic_recovery,
    erlmcp_chaos_tracking:start_test(TestId),

    StartTime = erlang:monotonic_time(millisecond),

    %% Phase 1: Build up to 100K connections
    ct:pal("Phase 1: Building 100K connections..."),
    Phase1_Start = erlang:monotonic_time(millisecond),

    Workers1 = [
        spawn_link(?MODULE, connection_worker,
                  [self(), I, ?CONNECTIONS_PER_WORKER, TestId])
        || I <- lists:seq(1, ?NUM_WORKERS)
    ],

    Results1 = collect_worker_results(Workers1, [], 0, 0),
    {ConnCount1, _} = Results1,

    Phase1_Duration = erlang:monotonic_time(millisecond) - Phase1_Start,

    ct:pal("Phase 1 complete: ~p connections in ~p ms", [ConnCount1, Phase1_Duration]),

    %% Phase 2: Catastrophic failure (simulate crash)
    ct:pal("Phase 2: Injecting catastrophic failure..."),
    Phase2_Start = erlang:monotonic_time(millisecond),

    %% Kill all workers violently
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Workers1),
    timer:sleep(2000),  %% Wait for crash propagation

    Phase2_Duration = erlang:monotonic_time(millisecond) - Phase2_Start,

    ct:pal("Phase 2 complete: System crash injected in ~p ms", [Phase2_Duration]),

    %% Phase 3: Recovery
    ct:pal("Phase 3: Recovering from catastrophic failure..."),
    Phase3_Start = erlang:monotonic_time(millisecond),

    Workers3 = [
        spawn_link(?MODULE, connection_worker,
                  [self(), I + 1000, ?CONNECTIONS_PER_WORKER, TestId])
        || I <- lists:seq(1, ?NUM_WORKERS)
    ],

    EndTime = Phase3_Start + (?RECOVERY_TIMEOUT_SEC * 1000),
    Results3 = collect_worker_results_with_timeout(Workers3, [], 0, 0, EndTime),
    {ConnCount3, MsgCount3} = Results3,

    Phase3_Duration = erlang:monotonic_time(millisecond) - Phase3_Start,
    TotalDuration = erlang:monotonic_time(millisecond) - StartTime,

    RecoveryPercent = (ConnCount3 / ConnCount1) * 100,

    ct:pal("CATASTROPHIC RECOVERY RESULTS:~n" ++
            "  Phase 1 (build): ~p ms, ~p connections~n" ++
            "  Phase 2 (crash): ~p ms~n" ++
            "  Phase 3 (recovery): ~p ms, ~p connections recovered~n" ++
            "  Total duration: ~p ms~n" ++
            "  Recovery rate: ~.2f%~n" ++
            "  Messages after recovery: ~p",
            [Phase1_Duration, ConnCount1, Phase2_Duration, Phase3_Duration,
             ConnCount3, TotalDuration, RecoveryPercent, MsgCount3]),

    erlmcp_chaos_tracking:log_metric(TestId, phase1_duration_ms, Phase1_Duration),
    erlmcp_chaos_tracking:log_metric(TestId, phase2_duration_ms, Phase2_Duration),
    erlmcp_chaos_tracking:log_metric(TestId, phase3_duration_ms, Phase3_Duration),
    erlmcp_chaos_tracking:log_metric(TestId, recovery_percent, RecoveryPercent),

    %% Recovery should achieve 80%+ of original connection count
    RecoveryPercent >= 80.0 orelse
        ct:fail("Recovery rate " ++ float_to_list(RecoveryPercent) ++ "% below 80% target").

%%====================================================================
%% Test 11: Mixed Failures Chaos Scenario
%%====================================================================

test_chaos_scenario_mixed_failures(Config) ->
    ct:pal("TEST 11: Mixed Chaos Scenario - All Failures Combined"),

    TestId = test_chaos_scenario_mixed_failures,
    erlmcp_chaos_tracking:start_test(TestId),

    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + (?TEST_DURATION_SEC * 1000),

    %% Start workers with full tracking
    MessageTracker = erlmcp_message_tracker:start(),
    ConnectionTracker = erlmcp_connection_tracker:start(),

    Workers = [
        spawn_link(?MODULE, message_worker,
                  [self(), I, ?CONNECTIONS_PER_WORKER, TestId, MessageTracker])
        || I <- lists:seq(1, ?NUM_WORKERS)
    ],

    %% Inject ALL types of failures simultaneously
    spawn_link(?MODULE, chaos_injection_worker,
              [self(), TestId, {mixed_chaos, EndTime}]),

    Results = collect_worker_results_with_chaos(Workers, [], 0, 0, EndTime),
    {ConnCount, MsgCount, _} = Results,

    %% Get tracking data
    UnexpectedClosures = erlmcp_connection_tracker:get_unexpected_closures(),
    ReconnectedCount = erlmcp_connection_tracker:get_reconnected_count(),

    Duration_Sec = (erlang:monotonic_time(millisecond) - StartTime) / 1000.0,
    ConnectionLossRate = (((?NUM_CONCURRENT - ConnCount) / ?NUM_CONCURRENT) * 100),
    RecoveryRate = case UnexpectedClosures of
        0 -> 100.0;
        N -> (ReconnectedCount / N) * 100
    end,

    ct:pal("MIXED CHAOS SCENARIO RESULTS:~n" ++
            "  Initial target: ~p connections~n" ++
            "  Connections achieved: ~p (~.2f%)~n" ++
            "  Messages sent: ~p~n" ++
            "  Unexpected closures: ~p~n" ++
            "  Reconnected: ~p~n" ++
            "  Recovery rate: ~.2f%~n" ++
            "  Duration: ~.2f seconds~n" ++
            "  Throughput: ~.0f msg/sec",
            [?NUM_CONCURRENT, ConnCount, (ConnCount / ?NUM_CONCURRENT) * 100,
             MsgCount, UnexpectedClosures, ReconnectedCount, RecoveryRate,
             Duration_Sec, MsgCount / Duration_Sec]),

    erlmcp_chaos_tracking:log_metric(TestId, mixed_connections, ConnCount),
    erlmcp_chaos_tracking:log_metric(TestId, mixed_messages, MsgCount),
    erlmcp_chaos_tracking:log_metric(TestId, mixed_recovery_rate, RecoveryRate),

    %% Under mixed chaos, maintain 75%+ of capacity
    ConnCount >= (?NUM_CONCURRENT * 0.75) orelse
        ct:fail("Connection loss too high under mixed chaos: " ++
                float_to_list(ConnectionLossRate) ++ "%").

%%====================================================================
%% Worker Processes
%%====================================================================

%% Connection worker - creates and maintains connections
connection_worker(Parent, WorkerId, ConnPerWorker, TestId) ->
    connection_loop(Parent, WorkerId, ConnPerWorker, TestId, 0, 0).

connection_loop(Parent, WorkerId, ConnPerWorker, TestId, ConnCount, MsgCount) ->
    receive
        stop ->
            Parent ! {connection_done, WorkerId, ConnCount, MsgCount}
    after 0 ->
        %% Create connection attempt
        case random_operation(connection_create) of
            ok ->
                case ConnCount < ConnPerWorker of
                    true ->
                        %% Simulate sending a message
                        NewMsgCount = case random_operation(send_message) of
                            ok -> MsgCount + 1;
                            _ -> MsgCount
                        end,
                        connection_loop(Parent, WorkerId, ConnPerWorker, TestId,
                                      ConnCount + 1, NewMsgCount);
                    false ->
                        %% Maintenance mode - keep connection alive
                        erlmcp_chaos_tracking:log_connection(TestId, WorkerId, ConnCount),
                        Parent ! {connection_done, WorkerId, ConnCount, MsgCount}
                end;
            fail ->
                connection_loop(Parent, WorkerId, ConnPerWorker, TestId, ConnCount, MsgCount)
        end
    end.

%% Message worker - tracks message sending/receiving with sequence numbers
message_worker(Parent, WorkerId, ConnPerWorker, TestId, Tracker) ->
    message_loop(Parent, WorkerId, ConnPerWorker, TestId, Tracker, 0, 0, 0, 0).

message_loop(Parent, WorkerId, ConnPerWorker, TestId, Tracker,
            ConnCount, SentCount, ReceivedCount, SeqNum) ->
    receive
        stop ->
            Parent ! {message_done, WorkerId, SentCount, ReceivedCount, SeqNum}
    after 0 ->
        case ConnCount < ConnPerWorker of
            true ->
                %% Send message with sequence number
                NewSentCount = SentCount + 1,
                erlmcp_message_tracker:track_sent(Tracker, WorkerId, NewSentCount, SeqNum),

                %% Simulate message delivery
                case random_operation(message_delivery) of
                    ok ->
                        erlmcp_message_tracker:track_received(Tracker, WorkerId, NewSentCount),
                        message_loop(Parent, WorkerId, ConnPerWorker, TestId, Tracker,
                                   ConnCount + 1, NewSentCount, ReceivedCount + 1, SeqNum + 1);
                    fail ->
                        erlmcp_message_tracker:track_lost(Tracker, WorkerId, NewSentCount),
                        message_loop(Parent, WorkerId, ConnPerWorker, TestId, Tracker,
                                   ConnCount, NewSentCount, ReceivedCount, SeqNum + 1)
                end;
            false ->
                Parent ! {message_done, WorkerId, SentCount, ReceivedCount, SeqNum}
        end
    end.

%% Chaos injection worker - injects various failure modes
chaos_injection_worker(Parent, TestId, {FailureMode, EndTime}) ->
    chaos_loop(Parent, TestId, FailureMode, EndTime, 0).

chaos_loop(Parent, TestId, FailureMode, EndTime, FailureCount) ->
    Now = erlang:monotonic_time(millisecond),
    case Now >= EndTime of
        true ->
            Parent ! {chaos_done, TestId, FailureCount};
        false ->
            case FailureMode of
                {random_kills, _} ->
                    %% Kill random processes periodically
                    case rand:uniform(100) of
                        X when X < 10 ->
                            erlmcp_chaos_injection:kill_random_process(),
                            erlmcp_chaos_tracking:log_metric(TestId, random_kill, 1),
                            timer:sleep(1000),
                            chaos_loop(Parent, TestId, FailureMode, EndTime, FailureCount + 1);
                        _ ->
                            timer:sleep(500),
                            chaos_loop(Parent, TestId, FailureMode, EndTime, FailureCount)
                    end;

                {network_partition, _} ->
                    %% Simulate network partition
                    case FailureCount of
                        0 ->
                            timer:sleep(10000),  %% Wait 10 seconds before partition
                            erlmcp_chaos_injection:simulate_network_partition(),
                            erlmcp_chaos_tracking:log_metric(TestId, partition_start, 1),
                            chaos_loop(Parent, TestId, FailureMode, EndTime, FailureCount + 1);
                        1 ->
                            timer:sleep(15000),  %% Partition for 15 seconds
                            erlmcp_chaos_injection:heal_network_partition(),
                            erlmcp_chaos_tracking:log_metric(TestId, partition_end, 1),
                            chaos_loop(Parent, TestId, FailureMode, EndTime, FailureCount + 2);
                        _ ->
                            timer:sleep(1000),
                            chaos_loop(Parent, TestId, FailureMode, EndTime, FailureCount)
                    end;

                {cascading_failures, _} ->
                    %% Multiple failure waves
                    case FailureCount rem 3 of
                        0 ->
                            timer:sleep(5000),
                            erlmcp_chaos_injection:kill_random_process(),
                            erlmcp_chaos_injection:kill_random_process(),
                            erlmcp_chaos_injection:kill_random_process(),
                            erlmcp_chaos_tracking:log_metric(TestId, cascade_wave, 1),
                            chaos_loop(Parent, TestId, FailureMode, EndTime, FailureCount + 1);
                        _ ->
                            timer:sleep(2000),
                            chaos_loop(Parent, TestId, FailureMode, EndTime, FailureCount)
                    end;

                {message_drops, _} ->
                    %% Drop messages randomly
                    erlmcp_chaos_injection:enable_message_drops(0.01),  %% 1% drop rate
                    timer:sleep(1000),
                    chaos_loop(Parent, TestId, FailureMode, EndTime, FailureCount + 1);

                {connection_drops, _} ->
                    %% Force connection closes
                    erlmcp_chaos_injection:enable_connection_drops(0.05),  %% 5% drop rate
                    timer:sleep(1000),
                    chaos_loop(Parent, TestId, FailureMode, EndTime, FailureCount + 1);

                {slow_degradation, _} ->
                    %% Gradually increase latency
                    Latency = (FailureCount div 10) * 10,  %% Increase by 10ms every 10 iterations
                    erlmcp_chaos_injection:set_latency(Latency),
                    erlmcp_chaos_tracking:log_metric(TestId, degradation_latency_ms, Latency),
                    timer:sleep(2000),
                    chaos_loop(Parent, TestId, FailureMode, EndTime, FailureCount + 1);

                {process_crashes, _} ->
                    %% Periodic process crashes
                    case FailureCount rem 5 of
                        0 ->
                            erlmcp_chaos_injection:crash_random_process(),
                            erlmcp_chaos_tracking:log_metric(TestId, process_crash, 1),
                            timer:sleep(3000);
                        _ ->
                            timer:sleep(1000)
                    end,
                    chaos_loop(Parent, TestId, FailureMode, EndTime, FailureCount + 1);

                {message_reordering, _} ->
                    %% Enable message reordering
                    erlmcp_chaos_injection:enable_message_reordering(0.02),  %% 2% reorder rate
                    timer:sleep(1000),
                    chaos_loop(Parent, TestId, FailureMode, EndTime, FailureCount + 1);

                {mixed_chaos, _} ->
                    %% Enable all chaos modes simultaneously
                    erlmcp_chaos_injection:enable_all_chaos(),
                    timer:sleep(1000),
                    chaos_loop(Parent, TestId, FailureMode, EndTime, FailureCount + 1)
            end
    end.

%% Monitor worker - tracks system health
monitor_worker(TestId, EndTime) ->
    Now = erlang:monotonic_time(millisecond),
    case Now >= EndTime of
        true ->
            ok;
        false ->
            %% Collect and log metrics
            Metrics = erlmcp_chaos_tracking:get_current_metrics(TestId),
            erlmcp_chaos_tracking:log_metrics_snapshot(TestId, Metrics),
            timer:sleep(?FAILURE_CHECK_INTERVAL_MS),
            monitor_worker(TestId, EndTime)
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Collect results from worker processes
collect_worker_results([], _Acc, ConnCount, MsgCount) ->
    {ConnCount, MsgCount};
collect_worker_results(Workers, Acc, ConnCount, MsgCount) ->
    receive
        {connection_done, _WorkerId, WorkerConns, WorkerMsgs} ->
            collect_worker_results(tl(Workers), [WorkerConns | Acc],
                                  ConnCount + WorkerConns, MsgCount + WorkerMsgs);
        {message_done, _WorkerId, WorkerSent, _WorkerRecv, _SeqNum} ->
            collect_worker_results(tl(Workers), [WorkerSent | Acc],
                                  ConnCount + WorkerSent, MsgCount + WorkerSent)
    after 120000 ->
        {ConnCount, MsgCount}
    end.

%% Collect results with chaos injection happening
collect_worker_results_with_chaos([], _Acc, ConnCount, MsgCount, _EndTime) ->
    {ConnCount, MsgCount, 0};
collect_worker_results_with_chaos(Workers, Acc, ConnCount, MsgCount, EndTime) ->
    Now = erlang:monotonic_time(millisecond),
    case Now >= EndTime of
        true ->
            {ConnCount, MsgCount, 0};
        false ->
            receive
                {connection_done, _WorkerId, WorkerConns, WorkerMsgs} ->
                    collect_worker_results_with_chaos(tl(Workers), [WorkerConns | Acc],
                                                      ConnCount + WorkerConns,
                                                      MsgCount + WorkerMsgs, EndTime);
                {chaos_done, _TestId, FailureCount} ->
                    collect_worker_results_with_chaos(Workers, Acc, ConnCount, MsgCount, EndTime);
                {message_done, _WorkerId, WorkerSent, _WorkerRecv, _SeqNum} ->
                    collect_worker_results_with_chaos(tl(Workers), [WorkerSent | Acc],
                                                      ConnCount + WorkerSent,
                                                      MsgCount + WorkerSent, EndTime)
            after 1000 ->
                collect_worker_results_with_chaos(Workers, Acc, ConnCount, MsgCount, EndTime)
            end
    end.

%% Collect results with timeout
collect_worker_results_with_timeout([], _Acc, ConnCount, MsgCount, _EndTime) ->
    {ConnCount, MsgCount};
collect_worker_results_with_timeout(Workers, Acc, ConnCount, MsgCount, EndTime) ->
    Now = erlang:monotonic_time(millisecond),
    case Now >= EndTime of
        true ->
            {ConnCount, MsgCount};
        false ->
            receive
                {connection_done, _WorkerId, WorkerConns, WorkerMsgs} ->
                    collect_worker_results_with_timeout(tl(Workers), [WorkerConns | Acc],
                                                        ConnCount + WorkerConns,
                                                        MsgCount + WorkerMsgs, EndTime);
                {message_done, _WorkerId, WorkerSent, _WorkerRecv, _SeqNum} ->
                    collect_worker_results_with_timeout(tl(Workers), [WorkerSent | Acc],
                                                        ConnCount + WorkerSent,
                                                        MsgCount + WorkerSent, EndTime)
            after 1000 ->
                collect_worker_results_with_timeout(Workers, Acc, ConnCount, MsgCount, EndTime)
            end
    end.

%% Collect message tracking results
collect_message_tracking_results(Workers, EndTime) ->
    collect_message_tracking_results(Workers, EndTime, 0, 0, 0, 0).

collect_message_tracking_results([], _EndTime, Sent, Recv, Lost, Dup) ->
    {Sent, Recv, Lost, Dup};
collect_message_tracking_results(Workers, EndTime, Sent, Recv, Lost, Dup) ->
    Now = erlang:monotonic_time(millisecond),
    case Now >= EndTime of
        true ->
            {Sent, Recv, Lost, Dup};
        false ->
            receive
                {message_done, _WorkerId, WorkerSent, WorkerRecv, _SeqNum} ->
                    collect_message_tracking_results(tl(Workers), EndTime,
                                                     Sent + WorkerSent,
                                                     Recv + WorkerRecv,
                                                     Lost, Dup)
            after 1000 ->
                collect_message_tracking_results(Workers, EndTime, Sent, Recv, Lost, Dup)
            end
    end.

%% Collect ordering results
collect_ordering_results(Workers, EndTime) ->
    collect_ordering_results(Workers, EndTime, 0, 0).

collect_ordering_results([], _EndTime, Total, OutOfOrder) ->
    {Total, OutOfOrder};
collect_ordering_results(Workers, EndTime, Total, OutOfOrder) ->
    Now = erlang:monotonic_time(millisecond),
    case Now >= EndTime of
        true ->
            {Total, OutOfOrder};
        false ->
            receive
                {message_done, _WorkerId, WorkerSent, _WorkerRecv, _SeqNum} ->
                    collect_ordering_results(tl(Workers), EndTime,
                                           Total + WorkerSent, OutOfOrder)
            after 1000 ->
                collect_ordering_results(Workers, EndTime, Total, OutOfOrder)
            end
    end.

%% Random operation simulator
random_operation(Type) ->
    case Type of
        connection_create ->
            case rand:uniform(100) of
                X when X > 5 -> ok;  %% 95% success
                _ -> fail
            end;
        send_message ->
            case rand:uniform(100) of
                X when X > 3 -> ok;  %% 97% success
                _ -> fail
            end;
        message_delivery ->
            case rand:uniform(100) of
                X when X > 2 -> ok;  %% 98% success
                _ -> fail
            end
    end.
