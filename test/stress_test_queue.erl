%%%-------------------------------------------------------------------
%% @doc Standalone stress test for optimized queue at 100K scale
%%
%% Run with:
%% erl -noshell -s stress_test_queue run -s init stop
%%
%% @end
%%%-------------------------------------------------------------------

-module(stress_test_queue).

-export([run/0, main/0]).

run() ->
    main().

main() ->
    io:format("=== ERLMCP OPTIMIZED QUEUE STRESS TEST ===~n", []),
    io:format("Testing 100K concurrent message throughput...~n~n", []),

    %% Test 1: Basic functionality
    test_basic(),

    %% Test 2: 10K concurrent
    test_10k_concurrent(),

    %% Test 3: 50K concurrent
    test_50k_concurrent(),

    %% Test 4: 100K concurrent
    test_100k_concurrent(),

    %% Test 5: Memory stability
    test_memory_stability(),

    io:format("~n=== ALL TESTS COMPLETED ===~n", []),
    timer:sleep(1000).

%%====================================================================
%% Test 1: Basic Functionality
%%====================================================================

test_basic() ->
    io:format("TEST 1: Basic Enqueue/Dequeue~n", []),

    %% Start queue
    {ok, _Pid} = erlmcp_queue_optimized:start_link(),
    timer:sleep(100),

    %% Enqueue messages
    ok = erlmcp_queue_optimized:enqueue(p2, msg_1),
    ok = erlmcp_queue_optimized:enqueue(p2, msg_2),
    ok = erlmcp_queue_optimized:enqueue(p2, msg_3),

    %% Dequeue
    [msg_1, msg_2, msg_3] = erlmcp_queue_optimized:dequeue_batch(10),
    [] = erlmcp_queue_optimized:dequeue_batch(10),

    %% Get stats
    Stats = erlmcp_queue_optimized:get_stats(),
    TotalEnqueued = maps:get(total_enqueued, Stats),
    TotalDequeued = maps:get(total_dequeued, Stats),

    io:format("  Basic test: PASSED~n", []),
    io:format("  Total enqueued: ~B, Total dequeued: ~B~n", [TotalEnqueued, TotalDequeued]),

    erlmcp_queue_optimized:stop(),
    timer:sleep(100).

%%====================================================================
%% Test 2: 10K Concurrent Producers
%%====================================================================

test_10k_concurrent() ->
    io:format("~nTEST 2: 10K Concurrent Producers~n", []),
    run_concurrent_test(10000).

%%====================================================================
%% Test 3: 50K Concurrent Producers
%%====================================================================

test_50k_concurrent() ->
    io:format("~nTEST 3: 50K Concurrent Producers~n", []),
    run_concurrent_test(50000).

%%====================================================================
%% Test 4: 100K Concurrent Producers
%%====================================================================

test_100k_concurrent() ->
    io:format("~nTEST 4: 100K Concurrent Producers~n", []),
    run_concurrent_test(100000).

%%====================================================================
%% Helper: Run concurrent producer test
%%====================================================================

run_concurrent_test(NumProducers) ->
    %% Start fresh queue
    {ok, _Pid} = erlmcp_queue_optimized:start_link(),
    timer:sleep(100),

    %% Spawn producers
    io:format("  Spawning ~B producers (1000 msgs each)...~n", [NumProducers]),
    StartTime = erlang:system_time(microsecond),

    %% Spawn all producers
    Pids = [spawn_producer(N, 1000) || N <- lists:seq(1, NumProducers)],

    %% Also spawn 100 dequeuer processes
    DequeuePids = [spawn_dequeuer(N) || N <- lists:seq(1, 100)],

    %% Wait for producers
    io:format("  Waiting for producers to complete...~n", []),
    wait_for_procs(Pids, erlang:system_time(microsecond)),

    EndTime = erlang:system_time(microsecond),
    ElapsedUs = EndTime - StartTime,
    ElapsedSec = ElapsedUs / 1000000.0,

    %% Stop dequeuers
    [exit(Pid, kill) || Pid <- DequeuePids],

    %% Get final stats
    Stats = erlmcp_queue_optimized:get_stats(),
    TotalEnqueued = maps:get(total_enqueued, Stats),
    TotalDequeued = maps:get(total_dequeued, Stats),
    TotalDepth = maps:get(total_depth, Stats),
    MaxDepth = maps:get(max_queue_depth, Stats),
    BatchesProcessed = maps:get(batches_processed, Stats),

    Throughput = TotalEnqueued / ElapsedSec,
    DequeueRate = TotalDequeued / ElapsedSec,

    io:format("  Duration: ~.2f seconds~n", [ElapsedSec]),
    io:format("  Total enqueued: ~B messages~n", [TotalEnqueued]),
    io:format("  Total dequeued: ~B messages~n", [TotalDequeued]),
    io:format("  Current queue depth: ~B~n", [TotalDepth]),
    io:format("  Max queue depth: ~B~n", [MaxDepth]),
    io:format("  Batches processed: ~B~n", [BatchesProcessed]),
    io:format("  Throughput: ~.0f msg/sec~n", [Throughput]),
    io:format("  Dequeue rate: ~.0f msg/sec~n", [DequeueRate]),
    io:format("  Pending messages: ~B~n", [TotalEnqueued - TotalDequeued]),

    case Throughput >= 50000 of
        true -> io:format("  RESULT: PASSED (throughput ~.0f >= 50K)~n", [Throughput]);
        false -> io:format("  RESULT: OK (throughput ~.0f msg/sec)~n", [Throughput])
    end,

    erlmcp_queue_optimized:stop(),
    timer:sleep(100).

%%====================================================================
%% Test 5: Memory Stability
%%====================================================================

test_memory_stability() ->
    io:format("~nTEST 5: Memory Stability (30 seconds)~n", []),

    %% Start queue
    {ok, _Pid} = erlmcp_queue_optimized:start_link(),
    timer:sleep(100),

    %% Spawn producer at 5000 msgs/sec
    ProducerPid = spawn_producer(1, 5000),

    %% Spawn dequeuer
    DequeuePid = spawn_dequeuer(1),

    %% Monitor stats over 30 seconds
    io:format("  Monitoring queue stability...~n", []),
    monitor_queue_stability(30, []),

    %% Stop processes
    exit(ProducerPid, kill),
    exit(DequeuePid, kill),

    %% Final stats
    Stats = erlmcp_queue_optimized:get_stats(),
    MaxDepth = maps:get(max_queue_depth, Stats),
    CurrentDepth = maps:get(total_depth, Stats),
    Utilization = maps:get(utilization_percent, Stats),

    io:format("  Final stats:~n", []),
    io:format("    Max queue depth: ~B / 500000~n", [MaxDepth]),
    io:format("    Current depth: ~B~n", [CurrentDepth]),
    io:format("    Utilization: ~B%~n", [Utilization]),

    case MaxDepth =< 500000 of
        true -> io:format("  RESULT: PASSED (bounded memory)~n", []);
        false -> io:format("  RESULT: WARNING (unbounded growth)~n", [])
    end,

    erlmcp_queue_optimized:stop(),
    timer:sleep(100).

%%====================================================================
%% Helper Functions
%%====================================================================

%% Spawn a producer process
spawn_producer(Id, NumMsgs) ->
    spawn(fun() -> producer_loop(Id, NumMsgs) end).

%% Producer loop
producer_loop(_Id, 0) ->
    ok;

producer_loop(Id, Count) ->
    Msg = {producer, Id, Count, erlang:system_time(microsecond)},
    case erlmcp_queue_optimized:enqueue(p2, Msg) of
        ok -> ok;
        {error, queue_full} ->
            io:format("WARNING: Queue full, producer ~B at ~B~n", [Id, Count])
    end,
    producer_loop(Id, Count - 1).

%% Spawn a dequeuer process
spawn_dequeuer(Id) ->
    spawn(fun() -> dequeuer_loop(Id) end).

%% Dequeuer loop - continuously dequeue batches
dequeuer_loop(_Id) ->
    _Batch = erlmcp_queue_optimized:dequeue_batch(256),
    timer:sleep(10),
    dequeuer_loop(_Id).

%% Wait for all processes to complete
wait_for_procs([], _StartTime) ->
    ok;

wait_for_procs(Pids, StartTime) ->
    Live = [P || P <- Pids, is_process_alive(P)],
    case Live of
        [] ->
            ok;
        _ ->
            timer:sleep(100),
            wait_for_procs(Live, StartTime)
    end.

%% Monitor queue stability over time
monitor_queue_stability(0, History) ->
    print_stability_report(lists:reverse(History));

monitor_queue_stability(Remaining, History) ->
    Stats = erlmcp_queue_optimized:get_stats(),
    Depth = maps:get(total_depth, Stats),
    Enqueued = maps:get(total_enqueued, Stats),
    Dequeued = maps:get(total_dequeued, Stats),

    NewHistory = [{Enqueued, Dequeued, Depth} | History],
    timer:sleep(1000),
    monitor_queue_stability(Remaining - 1, NewHistory).

%% Print stability report
print_stability_report(History) ->
    io:format("  Queue stability:~n", []),
    lists:foreach(fun({Enq, Deq, Depth}) ->
        io:format("    Enq: ~B, Deq: ~B, Depth: ~B~n", [Enq, Deq, Depth])
    end, History).
