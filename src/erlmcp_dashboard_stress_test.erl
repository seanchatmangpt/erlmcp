-module(erlmcp_dashboard_stress_test).

%% Stress test for dashboard with 100K concurrent connections
-export([
    start/0,
    start/1,
    start_workers/2,
    stop/0,
    run_benchmark/1,
    simulate_traffic/2
]).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_WORKERS, 100000).
-define(DEFAULT_DURATION_MS, 300000).  % 5 minutes
-define(WORKER_BATCH_SIZE, 1000).
-define(STATUS_INTERVAL, 5000).

%%====================================================================
%% API Functions
%%====================================================================

-spec start() -> ok.
start() ->
    start(?DEFAULT_WORKERS).

-spec start(non_neg_integer()) -> ok.
start(WorkerCount) ->
    ?LOG_INFO("Starting stress test with ~B workers~n", [WorkerCount]),
    erlmcp_metrics_server:reset_metrics(),
    start_workers(WorkerCount, ?DEFAULT_DURATION_MS),
    ok.

-spec start_workers(non_neg_integer(), pos_integer()) -> ok.
start_workers(WorkerCount, DurationMs) ->
    ?LOG_INFO("Spawning ~B concurrent worker processes~n", [WorkerCount]),
    StartTime = erlang:system_time(millisecond),

    % Update concurrent connections before starting
    erlmcp_metrics_server:increment_connections(WorkerCount),

    % Spawn workers in batches to avoid system overload
    spawn_worker_batches(WorkerCount, ?WORKER_BATCH_SIZE, DurationMs, StartTime),

    % Start status monitoring
    spawn_status_monitor(StartTime, DurationMs),

    ok.

-spec stop() -> ok.
stop() ->
    ?LOG_INFO("Stopping stress test~n", []),
    ok.

-spec run_benchmark(pos_integer()) -> map().
run_benchmark(WorkerCount) ->
    ?LOG_INFO("Running benchmark with ~B workers~n", [WorkerCount]),
    erlmcp_metrics_server:reset_metrics(),
    erlmcp_metrics_server:increment_connections(WorkerCount),

    StartTime = erlang:system_time(millisecond),
    DurationMs = 60000,  % 1 minute benchmark

    % Spawn workers
    spawn_worker_batches(WorkerCount, ?WORKER_BATCH_SIZE, DurationMs, StartTime),

    % Wait for completion
    timer:sleep(DurationMs + 2000),

    % Get final metrics
    Metrics = erlmcp_metrics_server:get_metrics(),
    Uptime = erlang:system_time(millisecond) - StartTime,

    ?LOG_INFO("Benchmark complete: ~p messages, ~p errors, uptime: ~Bms~n",
        [maps:get(total_messages, Metrics, 0), maps:get(total_errors, Metrics, 0), Uptime]),

    Metrics#{
        benchmark_duration_ms => Uptime,
        requested_workers => WorkerCount
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec spawn_worker_batches(non_neg_integer(), pos_integer(), pos_integer(), integer()) -> ok.
spawn_worker_batches(0, _BatchSize, _DurationMs, _StartTime) ->
    ok;

spawn_worker_batches(Count, BatchSize, DurationMs, StartTime) when Count > 0 ->
    CurrentBatch = min(Count, BatchSize),
    spawn_workers(CurrentBatch, DurationMs, StartTime),
    % Stagger worker creation to avoid thundering herd
    timer:sleep(10),
    spawn_worker_batches(Count - CurrentBatch, BatchSize, DurationMs, StartTime).

-spec spawn_workers(non_neg_integer(), pos_integer(), integer()) -> ok.
spawn_workers(Count, DurationMs, StartTime) ->
    lists:foreach(
        fun(_) ->
            spawn_link(fun() -> worker_loop(DurationMs, StartTime) end)
        end,
        lists:seq(1, Count)
    ),
    ok.

-spec worker_loop(pos_integer(), integer()) -> ok.
worker_loop(DurationMs, StartTime) ->
    CurrentTime = erlang:system_time(millisecond),
    Elapsed = CurrentTime - StartTime,

    if
        Elapsed >= DurationMs ->
            % Worker duration complete, exit
            erlmcp_metrics_server:decrement_connections(1),
            ok;
        true ->
            % Simulate some work
            simulate_traffic(StartTime, DurationMs),
            worker_loop(DurationMs, StartTime)
    end.

-spec simulate_traffic(integer(), pos_integer()) -> ok.
simulate_traffic(StartTime, DurationMs) ->
    % Simulate message activity
    erlmcp_metrics_server:record_message(1),

    % Simulate occasional errors (5% error rate)
    case rand:uniform(100) > 95 of
        true -> erlmcp_metrics_server:record_error();
        false -> ok
    end,

    % Simulate latency with distribution (exponential-ish)
    Latency = simulate_latency(),
    erlmcp_metrics_server:record_latency(Latency),

    % Random think time before next message
    ThinkTime = rand:uniform(50) + 10,  % 10-60ms
    timer:sleep(ThinkTime).

-spec simulate_latency() -> float().
simulate_latency() ->
    % Simulate realistic latency distribution:
    % 70% under 10ms, 20% 10-50ms, 8% 50-100ms, 2% 100-500ms
    Rand = rand:uniform(100),
    if
        Rand =< 70 ->
            rand:uniform(10) + 0.5;
        Rand =< 90 ->
            rand:uniform(40) + 10 + 0.5;
        Rand =< 98 ->
            rand:uniform(50) + 50 + 0.5;
        true ->
            rand:uniform(400) + 100 + 0.5
    end.

-spec spawn_status_monitor(integer(), pos_integer()) -> ok.
spawn_status_monitor(StartTime, DurationMs) ->
    spawn_link(fun() ->
        status_monitor_loop(StartTime, DurationMs)
    end),
    ok.

-spec status_monitor_loop(integer(), pos_integer()) -> ok.
status_monitor_loop(StartTime, DurationMs) ->
    timer:sleep(?STATUS_INTERVAL),

    CurrentTime = erlang:system_time(millisecond),
    Elapsed = CurrentTime - StartTime,

    if
        Elapsed >= DurationMs ->
            ?LOG_INFO("Stress test duration complete~n", []),
            ok;
        true ->
            Metrics = erlmcp_metrics_server:get_metrics(),
            Connections = maps:get(concurrent_connections, Metrics, 0),
            MsgRate = maps:get(message_rate_per_sec, Metrics, 0),
            TotalMsg = maps:get(total_messages, Metrics, 0),
            Errors = maps:get(total_errors, Metrics, 0),

            LatencyStats = maps:get(latency_stats, Metrics, #{}),
            P50 = maps:get(p50, LatencyStats, 0),
            P99 = maps:get(p99, LatencyStats, 0),

            Remaining = (DurationMs - Elapsed) div 1000,
            ?LOG_INFO(
                "Status [~Bs remaining]: ~B connections, ~B msg/sec, "
                "~B total msgs, ~B errors, P50: ~Bms, P99: ~Bms~n",
                [Remaining, Connections, MsgRate, TotalMsg, Errors, P50, P99]
            ),

            status_monitor_loop(StartTime, DurationMs)
    end.
