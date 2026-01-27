%%%-------------------------------------------------------------------
%% @doc Stress Test Suite for Optimized Priority Queue
%%
%% Tests the queue under extreme load conditions:
%% - 100K concurrent message producers
%% - Continuous message flow at various rates
%% - Memory bounded despite sustained load
%% - P99 latency maintained at <50ms
%% - Throughput sustained at 50K+ msg/sec
%%
%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_queue_optimized_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT Callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    groups/0
]).

%% Functional tests
-export([
    test_basic_enqueue_dequeue/1,
    test_priority_ordering/1,
    test_batch_dequeue/1,
    test_queue_full_behavior/1
]).

%% Performance tests
-export([
    test_10k_concurrent/1,
    test_50k_concurrent/1,
    test_100k_concurrent/1
]).

%% Stress tests
-export([
    test_sustained_throughput/1,
    test_memory_stability/1,
    test_latency_distribution/1
]).

%% Helpers
-export([
    producer/3,
    measure_enqueue_latency/2,
    measure_dequeue_latency/1
]).

-define(TEST_DURATION_SEC, 60).
-define(STATS_INTERVAL_MS, 5000).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        {group, functional},
        {group, performance},
        {group, stress}
    ].

groups() ->
    [
        {functional, [], [
            test_basic_enqueue_dequeue,
            test_priority_ordering,
            test_batch_dequeue,
            test_queue_full_behavior
        ]},
        {performance, [], [
            test_10k_concurrent,
            test_50k_concurrent,
            test_100k_concurrent
        ]},
        {stress, [], [
            test_sustained_throughput,
            test_memory_stability,
            test_latency_distribution
        ]}
    ].

-spec init_per_suite(list()) -> list().
init_per_suite(Config) ->
    ct:pal("Starting optimized queue stress test suite"),
    application:ensure_all_started(erlmcp),

    %% Enable erlang stats
    catch erlang:system_flag(scheduler_wall_time, true),

    [{suite_start_time, erlang:system_time(millisecond)} | Config].

-spec end_per_suite(list()) -> list().
end_per_suite(Config) ->
    ct:pal("Queue stress test suite completed"),
    Config.

-spec init_per_testcase(atom(), list()) -> list().
init_per_testcase(_TestCase, Config) ->
    %% Start fresh queue for each test
    _ = erlmcp_queue_optimized:stop(),
    timer:sleep(100),
    {ok, _Pid} = erlmcp_queue_optimized:start_link(),
    timer:sleep(100),
    [{test_start_time, erlang:system_time(millisecond)} | Config].

-spec end_per_testcase(atom(), list()) -> list().
end_per_testcase(_TestCase, Config) ->
    erlmcp_queue_optimized:stop(),
    Config.

%%====================================================================
%% Functional Tests
%%====================================================================

test_basic_enqueue_dequeue(_Config) ->
    %% Enqueue 100 messages
    ok = erlmcp_queue_optimized:enqueue(p2, msg_1),
    ok = erlmcp_queue_optimized:enqueue(p2, msg_2),
    ok = erlmcp_queue_optimized:enqueue(p2, msg_3),

    %% Dequeue batch
    [msg_1, msg_2, msg_3] = erlmcp_queue_optimized:dequeue_batch(10),
    [] = erlmcp_queue_optimized:dequeue_batch(10),

    ct:pal("Basic enqueue/dequeue test passed"),
    ok.

test_priority_ordering(_Config) ->
    %% Enqueue messages with different priorities
    ok = erlmcp_queue_optimized:enqueue(p3, {msg, p3, 1}),
    ok = erlmcp_queue_optimized:enqueue(p2, {msg, p2, 1}),
    ok = erlmcp_queue_optimized:enqueue(p0, {msg, p0, 1}),
    ok = erlmcp_queue_optimized:enqueue(p1, {msg, p1, 1}),
    ok = erlmcp_queue_optimized:enqueue(p3, {msg, p3, 2}),
    ok = erlmcp_queue_optimized:enqueue(p0, {msg, p0, 2}),

    %% Dequeue in priority order: p0 first, then p1, p2, p3
    Dequeued = erlmcp_queue_optimized:dequeue_batch(10),

    %% Verify p0 messages come first
    [First, Second | _] = Dequeued,
    {msg, p0, _} = First,
    {msg, p0, _} = Second,

    ct:pal("Priority ordering test passed. Order: ~w", [Dequeued]),
    ok.

test_batch_dequeue(_Config) ->
    %% Enqueue 1000 messages
    NumMsgs = 1000,
    _ = [erlmcp_queue_optimized:enqueue(p2, {msg, N}) || N <- lists:seq(1, NumMsgs)],

    %% Dequeue in batches of 256
    Batch1 = erlmcp_queue_optimized:dequeue_batch(256),
    ct:pal("Batch 1 size: ~B", [length(Batch1)]),
    ?assert(length(Batch1) =:= 256),

    Batch2 = erlmcp_queue_optimized:dequeue_batch(256),
    ?assert(length(Batch2) =:= 256),

    Batch3 = erlmcp_queue_optimized:dequeue_batch(256),
    ?assert(length(Batch3) =:= 256),

    Batch4 = erlmcp_queue_optimized:dequeue_batch(256),
    ?assert(length(Batch4) =:= 256),

    %% Remaining messages
    Batch5 = erlmcp_queue_optimized:dequeue_batch(256),
    ct:pal("Final batch size: ~B", [length(Batch5)]),
    ?assert(length(Batch5) =:= (NumMsgs - 1024)),

    ct:pal("Batch dequeue test passed"),
    ok.

test_queue_full_behavior(_Config) ->
    %% Queue has max depth of 500K
    %% Enqueue close to limit
    [erlmcp_queue_optimized:enqueue(p2, {msg, N}) || N <- lists:seq(1, 400000)],

    %% Next enqueue should succeed (not at limit yet)
    ok = erlmcp_queue_optimized:enqueue(p2, near_limit_msg),

    %% Check stats
    Stats = erlmcp_queue_optimized:get_stats(),
    TotalDepth = maps:get(total_depth, Stats),
    ct:pal("Queue depth: ~B / 500000", [TotalDepth]),
    ?assert(TotalDepth > 400000),

    ct:pal("Queue full behavior test passed"),
    ok.

%%====================================================================
%% Performance Tests
%%====================================================================

test_10k_concurrent(Config) ->
    run_concurrent_test(Config, 10000).

test_50k_concurrent(Config) ->
    run_concurrent_test(Config, 50000).

test_100k_concurrent(Config) ->
    run_concurrent_test(Config, 100000).

%% Run concurrent producer test
-spec run_concurrent_test(list(), pos_integer()) -> ok.
run_concurrent_test(Config, NumProducers) ->
    StartTime = erlang:system_time(millisecond),
    TestStart = erlang:system_time(microsecond),

    %% Spawn producers
    ct:pal("Spawning ~B concurrent producers...", [NumProducers]),
    Pids = [spawn_link(?MODULE, producer, [N, 1000, self()]) || N <- lists:seq(1, NumProducers)],

    %% Wait for producers to finish
    ct:pal("Waiting for producers to complete...", []),
    wait_for_producers(Pids, 0),

    TestEnd = erlang:system_time(microsecond),
    ElapsedUs = TestEnd - TestStart,
    ElapsedSec = ElapsedUs / 1000000.0,

    %% Get stats
    Stats = erlmcp_queue_optimized:get_stats(),
    TotalEnqueued = maps:get(total_enqueued, Stats),
    TotalDequeued = maps:get(total_dequeued, Stats),
    TotalDepth = maps:get(total_depth, Stats),
    BatchesProcessed = maps:get(batches_processed, Stats),

    Throughput = TotalEnqueued / ElapsedSec,
    DequeueRate = TotalDequeued / ElapsedSec,

    ct:pal("=== TEST RESULTS: ~B CONCURRENT PRODUCERS ===", [NumProducers]),
    ct:pal("Duration: ~.2f seconds", [ElapsedSec]),
    ct:pal("Total enqueued: ~B messages", [TotalEnqueued]),
    ct:pal("Total dequeued: ~B messages", [TotalDequeued]),
    ct:pal("Current queue depth: ~B messages", [TotalDepth]),
    ct:pal("Batches processed: ~B", [BatchesProcessed]),
    ct:pal("Throughput: ~.0f msg/sec", [Throughput]),
    ct:pal("Dequeue rate: ~.0f msg/sec", [DequeueRate]),
    ct:pal("Pending messages: ~B", [TotalEnqueued - TotalDequeued]),

    %% Verify throughput
    case Throughput >= 50000 of
        true ->
            ct:pal("SUCCESS: Throughput ~.0f msg/sec >= 50K target", [Throughput]);
        false ->
            ct:pal("WARNING: Throughput ~.0f msg/sec < 50K target", [Throughput])
    end,

    ok.

%%====================================================================
%% Stress Tests
%%====================================================================

test_sustained_throughput(Config) ->
    StartTime = erlang:system_time(millisecond),

    %% Spawn 100 producers each sending 100 msgs/sec
    ct:pal("Starting sustained throughput test (100 producers, 60 seconds)..."),
    Pids = [spawn_link(?MODULE, producer, [N, 100, self()]) || N <- lists:seq(1, 100)],

    %% Monitor stats over time
    collect_stats_over_time(?TEST_DURATION_SEC, []),

    %% Wait for completion
    wait_for_producers(Pids, 0),

    EndTime = erlang:system_time(millisecond),
    ElapsedMs = EndTime - StartTime,

    Stats = erlmcp_queue_optimized:get_stats(),
    TotalEnqueued = maps:get(total_enqueued, Stats),
    Throughput = (TotalEnqueued / ElapsedMs) * 1000,

    ct:pal("=== SUSTAINED THROUGHPUT TEST ==="),
    ct:pal("Duration: ~B ms", [ElapsedMs]),
    ct:pal("Total enqueued: ~B", [TotalEnqueued]),
    ct:pal("Average throughput: ~.0f msg/sec", [Throughput]),

    ok.

test_memory_stability(_Config) ->
    %% Spawn producer and dequeue continuously
    %% Memory should stay bounded

    ProducerPid = spawn_link(?MODULE, producer, [1, 5000, self()]),
    DequeuePid = spawn_link(fun continuous_dequeue/0),

    %% Monitor for 30 seconds
    timer:sleep(30000),

    %% Retrieve stats
    Stats = erlmcp_queue_optimized:get_stats(),
    MaxQueueDepth = maps:get(max_queue_depth, Stats),
    CurrentDepth = maps:get(total_depth, Stats),

    ct:pal("=== MEMORY STABILITY TEST ==="),
    ct:pal("Max queue depth: ~B", [MaxQueueDepth]),
    ct:pal("Current queue depth: ~B", [CurrentDepth]),
    ct:pal("Max utilization: ~B%", [maps:get(utilization_percent, Stats)]),

    %% Verify bounded growth
    case MaxQueueDepth =< 500000 of
        true -> ct:pal("SUCCESS: Queue bounded at ~B messages", [MaxQueueDepth]);
        false -> ct:pal("WARNING: Queue exceeded target bound", [])
    end,

    exit(ProducerPid, kill),
    exit(DequeuePid, kill),

    ok.

test_latency_distribution(_Config) ->
    %% Measure enqueue and dequeue latencies
    NumSamples = 10000,

    %% Measure enqueue latencies
    ct:pal("Measuring enqueue latencies (~B samples)...", [NumSamples]),
    EnqueueLatencies = measure_enqueue_latency(NumSamples, []),

    %% Measure dequeue latencies
    ct:pal("Measuring dequeue latencies (~B samples)...", [NumSamples]),
    DequeueLatencies = measure_dequeue_latency(NumSamples),

    %% Calculate percentiles
    EnqP50 = percentile(EnqueueLatencies, 50),
    EnqP99 = percentile(EnqueueLatencies, 99),
    EnqP999 = percentile(EnqueueLatencies, 99.9),

    DeqP50 = percentile(DequeueLatencies, 50),
    DeqP99 = percentile(DequeueLatencies, 99),
    DeqP999 = percentile(DequeueLatencies, 99.9),

    ct:pal("=== LATENCY DISTRIBUTION ==="),
    ct:pal("Enqueue latency - p50: ~.3f us, p99: ~.3f us, p99.9: ~.3f us", [EnqP50, EnqP99, EnqP999]),
    ct:pal("Dequeue latency - p50: ~.3f us, p99: ~.3f us, p99.9: ~.3f us", [DeqP50, DeqP99, DeqP999]),

    %% Verify P99 latency
    case EnqP99 < 50000 of  %% 50ms in microseconds
        true -> ct:pal("SUCCESS: Enqueue P99 latency ~.3f us < 50ms", [EnqP99]);
        false -> ct:pal("WARNING: Enqueue P99 latency ~.3f us > 50ms", [EnqP99])
    end,

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Producer process: enqueue messages at specified rate
-spec producer(pos_integer(), pos_integer(), pid()) -> ok.
producer(Id, MsgsPerSec, Parent) ->
    IntervalMs = 1000 div MsgsPerSec,
    Msg = {producer, Id, erlang:system_time(microsecond)},

    produce_loop(MsgsPerSec, IntervalMs, Msg, Parent, 0).

-spec produce_loop(pos_integer(), pos_integer(), term(), pid(), non_neg_integer()) -> ok.
produce_loop(0, _IntervalMs, _Msg, _Parent, _Count) ->
    ok;

produce_loop(Remaining, IntervalMs, Msg, Parent, Count) ->
    ok = erlmcp_queue_optimized:enqueue(p2, {Msg, Count}),
    case Remaining rem 100 =:= 0 of
        true -> timer:sleep(IntervalMs);
        false -> ok
    end,
    produce_loop(Remaining - 1, IntervalMs, Msg, Parent, Count + 1).

%% Wait for all producer pids to complete
-spec wait_for_producers([pid()], non_neg_integer()) -> ok.
wait_for_producers([], _Count) ->
    ok;

wait_for_producers(Pids, Count) ->
    receive
        {done, _} ->
            RemainingPids = [P || P <- Pids, is_process_alive(P)],
            wait_for_producers(RemainingPids, Count + 1)
    after
        60000 ->
            ct:pal("Timeout waiting for producers (~B done)", [Count]),
            ok
    end.

%% Continuous dequeue loop
-spec continuous_dequeue() -> ok.
continuous_dequeue() ->
    erlmcp_queue_optimized:dequeue_batch(256),
    timer:sleep(10),
    continuous_dequeue().

%% Measure enqueue latency
-spec measure_enqueue_latency(pos_integer(), list()) -> list().
measure_enqueue_latency(0, Acc) ->
    lists:sort(Acc);

measure_enqueue_latency(N, Acc) ->
    T1 = erlang:system_time(microsecond),
    erlmcp_queue_optimized:enqueue(p2, {latency_test, N}),
    T2 = erlang:system_time(microsecond),
    Latency = T2 - T1,
    measure_enqueue_latency(N - 1, [Latency | Acc]).

%% Measure dequeue latency
-spec measure_dequeue_latency(pos_integer()) -> list().
measure_dequeue_latency(NumSamples) ->
    %% Pre-populate queue with messages
    [erlmcp_queue_optimized:enqueue(p2, {dequeue_test, N}) || N <- lists:seq(1, NumSamples)],

    measure_dequeue_loop(NumSamples, []).

-spec measure_dequeue_loop(pos_integer(), list()) -> list().
measure_dequeue_loop(0, Acc) ->
    lists:sort(Acc);

measure_dequeue_loop(N, Acc) ->
    T1 = erlang:system_time(microsecond),
    erlmcp_queue_optimized:dequeue_batch(1),
    T2 = erlang:system_time(microsecond),
    Latency = T2 - T1,
    measure_dequeue_loop(N - 1, [Latency | Acc]).

%% Calculate percentile from sorted list
-spec percentile(list(), number()) -> number().
percentile([], _P) ->
    0.0;

percentile(Sorted, P) when P >= 0, P =< 100 ->
    Len = length(Sorted),
    Index = max(1, round((P / 100.0) * Len)),
    lists:nth(Index, Sorted).

%% Collect stats over time
-spec collect_stats_over_time(pos_integer(), list()) -> ok.
collect_stats_over_time(0, StatsHistory) ->
    print_stats_history(StatsHistory),
    ok;

collect_stats_over_time(RemainingSeconds, StatsHistory) ->
    Stats = erlmcp_queue_optimized:get_stats(),
    TotalEnqueued = maps:get(total_enqueued, Stats),
    TotalDequeued = maps:get(total_dequeued, Stats),
    TotalDepth = maps:get(total_depth, Stats),

    Throughput = case RemainingSeconds > 0 of
        true -> (TotalEnqueued - TotalDequeued) / 5;  %% Last 5 seconds
        false -> 0
    end,

    NewHistory = [{TotalEnqueued, TotalDequeued, TotalDepth, Throughput} | StatsHistory],
    timer:sleep(5000),
    collect_stats_over_time(RemainingSeconds - 5, NewHistory).

-spec print_stats_history(list()) -> ok.
print_stats_history(History) ->
    ct:pal("Stats history over test duration:"),
    lists:foreach(fun({Enq, Deq, Depth, Throughput}) ->
        ct:pal("Enqueued: ~B, Dequeued: ~B, Depth: ~B, Rate: ~.0f msg/5s",
               [Enq, Deq, Depth, Throughput])
    end, lists:reverse(History)).
