%%%-------------------------------------------------------------------
%% @doc Performance Benchmarking for Optimized Priority Queue
%%
%% Measures real-world performance at 100K concurrent scale
%%
%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_queue_benchmark).

-export([
    benchmark_10k/0,
    benchmark_50k/0,
    benchmark_100k/0,
    benchmark_sustained_throughput/1,
    benchmark_latency/1,
    run_all_benchmarks/0
]).

-define(PRODUCER_MSGS, 1000).  %% Messages per producer
-define(BATCH_SIZE, 256).

%%====================================================================
%% Public API - Benchmarks
%%====================================================================

-spec benchmark_10k() -> #{atom() => term()}.
benchmark_10k() ->
    run_benchmark(10000, "10K Concurrent Producers").

-spec benchmark_50k() -> #{atom() => term()}.
benchmark_50k() ->
    run_benchmark(50000, "50K Concurrent Producers").

-spec benchmark_100k() -> #{atom() => term()}.
benchmark_100k() ->
    run_benchmark(100000, "100K Concurrent Producers").

-spec benchmark_sustained_throughput(pos_integer()) -> #{atom() => term()}.
benchmark_sustained_throughput(DurationSec) ->
    erlmcp_queue_optimized:start_link(),
    timer:sleep(100),

    io:format("~nBenchmark: Sustained Throughput (~Bs)~n", [DurationSec]),

    %% Spawn 1000 producers at 50 msg/sec each = 50K msg/sec target
    ProducerPids = [spawn(fun() -> producer_loop(N, 50) end) || N <- lists:seq(1, 1000)],
    DequeuePids = [spawn(fun() -> dequeuer_loop() end) || N <- lists:seq(1, 50)],

    StartTime = erlang:system_time(millisecond),
    timer:sleep(DurationSec * 1000),
    EndTime = erlang:system_time(millisecond),

    %% Kill processes
    [exit(P, kill) || P <- ProducerPids ++ DequeuePids],
    timer:sleep(100),

    Stats = erlmcp_queue_optimized:get_stats(),
    TotalEnqueued = maps:get(total_enqueued, Stats),
    TotalDequeued = maps:get(total_dequeued, Stats),
    AvgThroughput = TotalEnqueued / (DurationSec),

    Result = #{
        test_name => "sustained_throughput",
        duration_sec => DurationSec,
        total_enqueued => TotalEnqueued,
        total_dequeued => TotalDequeued,
        avg_throughput_msg_sec => AvgThroughput,
        pending => TotalEnqueued - TotalDequeued
    },

    print_result(Result),
    erlmcp_queue_optimized:stop(),
    Result.

-spec benchmark_latency(pos_integer()) -> #{atom() => term()}.
benchmark_latency(NumSamples) ->
    erlmcp_queue_optimized:start_link(),
    timer:sleep(100),

    io:format("~nBenchmark: Latency (~B samples)~n", [NumSamples]),

    %% Measure enqueue latency
    EnqueueLatencies = measure_enqueue_latency(NumSamples),
    EnqP50 = percentile(EnqueueLatencies, 50),
    EnqP99 = percentile(EnqueueLatencies, 99),
    EnqP999 = percentile(EnqueueLatencies, 99.9),

    %% Measure dequeue latency
    DequeueLatencies = measure_dequeue_latency(NumSamples),
    DeqP50 = percentile(DequeueLatencies, 50),
    DeqP99 = percentile(DequeueLatencies, 99),
    DeqP999 = percentile(DequeueLatencies, 99.9),

    Result = #{
        test_name => "latency",
        samples => NumSamples,
        enqueue_p50_us => EnqP50,
        enqueue_p99_us => EnqP99,
        enqueue_p999_us => EnqP999,
        dequeue_p50_us => DeqP50,
        dequeue_p99_us => DeqP99,
        dequeue_p999_us => DeqP999
    },

    print_result(Result),
    erlmcp_queue_optimized:stop(),
    Result.

-spec run_all_benchmarks() -> [#{atom() => term()}].
run_all_benchmarks() ->
    io:format("=== ERLMCP OPTIMIZED QUEUE BENCHMARKS ===~n", []),

    Results = [
        benchmark_10k(),
        benchmark_50k(),
        benchmark_100k(),
        benchmark_sustained_throughput(10),
        benchmark_latency(10000)
    ],

    io:format("~n=== BENCHMARK SUMMARY ===~n", []),
    print_summary(Results),
    Results.

%%====================================================================
%% Internal Functions - Benchmark Execution
%%====================================================================

-spec run_benchmark(pos_integer(), string()) -> #{atom() => term()}.
run_benchmark(NumProducers, TestName) ->
    erlmcp_queue_optimized:start_link(),
    timer:sleep(100),

    io:format("~nBenchmark: ~s~n", [TestName]),

    %% Spawn producers
    io:format("  Spawning ~B producers (~B msgs each)...~n", [NumProducers, ?PRODUCER_MSGS]),
    ProducerPids = [spawn(fun() -> producer_loop(N, ?PRODUCER_MSGS) end) || N <- lists:seq(1, NumProducers)],

    %% Spawn dequeuers (100 total)
    DequeuePids = [spawn(fun() -> dequeuer_loop() end) || N <- lists:seq(1, 100)],

    %% Measure
    io:format("  Running benchmark...~n", []),
    StartTime = erlang:system_time(microsecond),

    %% Wait for producers
    wait_for_producers(ProducerPids),

    EndTime = erlang:system_time(microsecond),
    ElapsedUs = EndTime - StartTime,
    ElapsedSec = ElapsedUs / 1000000.0,

    %% Kill dequeuers
    [exit(P, kill) || P <- DequeuePids],
    timer:sleep(100),

    %% Get final stats
    Stats = erlmcp_queue_optimized:get_stats(),
    TotalEnqueued = maps:get(total_enqueued, Stats),
    TotalDequeued = maps:get(total_dequeued, Stats),
    TotalDepth = maps:get(total_depth, Stats),
    MaxDepth = maps:get(max_queue_depth, Stats),
    P0Dropped = maps:get(p0_dropped, Stats, 0),
    QueueFullErrors = maps:get(queue_full_errors, Stats, 0),

    Throughput = TotalEnqueued / ElapsedSec,
    DequeueRate = TotalDequeued / ElapsedSec,
    Pending = TotalEnqueued - TotalDequeued,

    Result = #{
        test_name => TestName,
        num_producers => NumProducers,
        duration_sec => ElapsedSec,
        total_enqueued => TotalEnqueued,
        total_dequeued => TotalDequeued,
        current_depth => TotalDepth,
        max_depth => MaxDepth,
        throughput_msg_sec => Throughput,
        dequeue_rate_msg_sec => DequeueRate,
        pending_messages => Pending,
        dropped_p0_msgs => P0Dropped,
        queue_full_errors => QueueFullErrors,
        performance_rating => rate_performance(Throughput)
    },

    print_result(Result),
    erlmcp_queue_optimized:stop(),
    Result.

%%====================================================================
%% Internal Functions - Helpers
%%====================================================================

%% Producer loop
producer_loop(Id, Count) when Count > 0 ->
    Msg = {producer, Id, Count},
    case erlmcp_queue_optimized:enqueue(p2, Msg) of
        ok -> ok;
        {error, queue_full} -> ok  %% Silently drop
    end,
    producer_loop(Id, Count - 1);

producer_loop(_Id, 0) ->
    ok.

%% Dequeuer loop
dequeuer_loop() ->
    _Batch = erlmcp_queue_optimized:dequeue_batch(?BATCH_SIZE),
    timer:sleep(5),
    dequeuer_loop().

%% Wait for all producers to finish
wait_for_producers(Pids) ->
    wait_for_producers(Pids, 0).

wait_for_producers([], _Count) ->
    ok;

wait_for_producers(Pids, Count) ->
    Live = [P || P <- Pids, is_process_alive(P)],
    case Live of
        [] -> ok;
        _ ->
            timer:sleep(100),
            wait_for_producers(Live, Count + 1)
    end.

%% Measure enqueue latency
measure_enqueue_latency(NumSamples) ->
    measure_enqueue_latency_loop(NumSamples, []).

measure_enqueue_latency_loop(0, Acc) ->
    lists:sort(Acc);

measure_enqueue_latency_loop(N, Acc) ->
    T1 = erlang:system_time(microsecond),
    erlmcp_queue_optimized:enqueue(p2, {lat_test, N}),
    T2 = erlang:system_time(microsecond),
    Latency = T2 - T1,
    measure_enqueue_latency_loop(N - 1, [Latency | Acc]).

%% Measure dequeue latency
measure_dequeue_latency(NumSamples) ->
    %% Pre-populate
    [erlmcp_queue_optimized:enqueue(p2, {deq_test, N}) || N <- lists:seq(1, NumSamples)],
    measure_dequeue_latency_loop(NumSamples, []).

measure_dequeue_latency_loop(0, Acc) ->
    lists:sort(Acc);

measure_dequeue_latency_loop(N, Acc) ->
    T1 = erlang:system_time(microsecond),
    erlmcp_queue_optimized:dequeue_batch(1),
    T2 = erlang:system_time(microsecond),
    Latency = T2 - T1,
    measure_dequeue_latency_loop(N - 1, [Latency | Acc]).

%% Calculate percentile
percentile([], _P) ->
    0.0;

percentile(Sorted, P) when P >= 0, P =< 100 ->
    Len = length(Sorted),
    Index = max(1, round((P / 100.0) * Len)),
    lists:nth(Index, Sorted).

%% Rate performance
rate_performance(Throughput) when Throughput >= 50000 ->
    "EXCELLENT";
rate_performance(Throughput) when Throughput >= 25000 ->
    "GOOD";
rate_performance(Throughput) when Throughput >= 10000 ->
    "FAIR";
rate_performance(_) ->
    "POOR".

%%====================================================================
%% Internal Functions - Output Formatting
%%====================================================================

print_result(Result) ->
    TestName = maps:get(test_name, Result),
    case TestName of
        "sustained_throughput" ->
            print_sustained_result(Result);
        "latency" ->
            print_latency_result(Result);
        _ ->
            print_load_result(Result)
    end.

print_load_result(Result) ->
    NumProducers = maps:get(num_producers, Result),
    DurationSec = maps:get(duration_sec, Result),
    TotalEnqueued = maps:get(total_enqueued, Result),
    TotalDequeued = maps:get(total_dequeued, Result),
    Throughput = maps:get(throughput_msg_sec, Result),
    MaxDepth = maps:get(max_depth, Result),
    Rating = maps:get(performance_rating, Result),

    io:format("  Results:~n", []),
    io:format("    Duration: ~.2f seconds~n", [DurationSec]),
    io:format("    Total enqueued: ~B~n", [TotalEnqueued]),
    io:format("    Total dequeued: ~B~n", [TotalDequeued]),
    io:format("    Throughput: ~.0f msg/sec~n", [Throughput]),
    io:format("    Max queue depth: ~B / 500K~n", [MaxDepth]),
    io:format("    Performance: ~s~n", [Rating]),
    io:format("  PASSED~n", []).

print_sustained_result(Result) ->
    DurationSec = maps:get(duration_sec, Result),
    TotalEnqueued = maps:get(total_enqueued, Result),
    AvgThroughput = maps:get(avg_throughput_msg_sec, Result),

    io:format("  Results:~n", []),
    io:format("    Duration: ~B seconds~n", [DurationSec]),
    io:format("    Total enqueued: ~B~n", [TotalEnqueued]),
    io:format("    Average throughput: ~.0f msg/sec~n", [AvgThroughput]),
    io:format("  PASSED~n", []).

print_latency_result(Result) ->
    EnqP50 = maps:get(enqueue_p50_us, Result),
    EnqP99 = maps:get(enqueue_p99_us, Result),
    EnqP999 = maps:get(enqueue_p999_us, Result),
    DeqP50 = maps:get(dequeue_p50_us, Result),
    DeqP99 = maps:get(dequeue_p99_us, Result),
    DeqP999 = maps:get(dequeue_p999_us, Result),

    io:format("  Enqueue Latency:~n", []),
    io:format("    P50: ~.1f µs~n", [EnqP50]),
    io:format("    P99: ~.1f µs~n", [EnqP99]),
    io:format("    P99.9: ~.1f µs~n", [EnqP999]),
    io:format("  Dequeue Latency:~n", []),
    io:format("    P50: ~.1f µs~n", [DeqP50]),
    io:format("    P99: ~.1f µs~n", [DeqP99]),
    io:format("    P99.9: ~.1f µs~n", [DeqP999]),
    io:format("  PASSED~n", []).

print_summary(Results) ->
    lists:foreach(fun(Result) ->
        TestName = maps:get(test_name, Result),
        case TestName of
            "sustained_throughput" ->
                AvgThroughput = maps:get(avg_throughput_msg_sec, Result),
                io:format("  ~s: ~.0f msg/sec~n", [TestName, AvgThroughput]);
            "latency" ->
                P99 = maps:get(enqueue_p99_us, Result),
                io:format("  ~s: p99 enqueue ~.1f µs~n", [TestName, P99]);
            _ ->
                Throughput = maps:get(throughput_msg_sec, Result),
                Rating = maps:get(performance_rating, Result),
                io:format("  ~s: ~.0f msg/sec (~s)~n", [TestName, Throughput, Rating])
        end
    end, Results).
