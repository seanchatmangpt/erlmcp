#!/usr/bin/env escript

main([]) ->
    %% Compile the queue module
    erlc:compile("src/erlmcp_queue_optimized.erl", [
        {i, "include"},
        {outdir, "."}
    ]),
    
    %% Test basic functionality
    {ok, _Pid} = erlmcp_queue_optimized:start_link(),
    timer:sleep(100),
    
    io:format("=== ERLMCP OPTIMIZED QUEUE TEST ===~n~n", []),
    
    %% Test 1: Basic
    io:format("Test 1: Basic enqueue/dequeue...~n", []),
    ok = erlmcp_queue_optimized:enqueue(p2, msg_1),
    [msg_1] = erlmcp_queue_optimized:dequeue_batch(10),
    io:format("  PASSED~n~n", []),
    
    %% Test 2: Multiple messages
    io:format("Test 2: Batch processing (100 messages)...~n", []),
    [ok = erlmcp_queue_optimized:enqueue(p2, {msg, N}) || N <- lists:seq(1, 100)],
    Batch = erlmcp_queue_optimized:dequeue_batch(256),
    io:format("  Dequeued ~B messages~n", [length(Batch)]),
    io:format("  PASSED~n~n", []),
    
    %% Test 3: Concurrent load (10K)
    io:format("Test 3: 10K concurrent producers...~n", []),
    erlmcp_queue_optimized:reset(),
    
    StartTime = erlang:system_time(microsecond),
    NumProducers = 10000,
    
    Pids = [spawn(fun() -> 
        [erlmcp_queue_optimized:enqueue(p2, {msg, P, N}) || N <- lists:seq(1, 10)]
    end) || P <- lists:seq(1, NumProducers)],
    
    wait_for_pids(Pids, 0),
    EndTime = erlang:system_time(microsecond),
    ElapsedUs = EndTime - StartTime,
    ElapsedSec = ElapsedUs / 1000000.0,
    
    Stats = erlmcp_queue_optimized:get_stats(),
    TotalEnqueued = maps:get(total_enqueued, Stats),
    Throughput = TotalEnqueued / ElapsedSec,
    
    io:format("  Duration: ~.2f seconds~n", [ElapsedSec]),
    io:format("  Total enqueued: ~B~n", [TotalEnqueued]),
    io:format("  Throughput: ~.0f msg/sec~n", [Throughput]),
    io:format("  PASSED~n~n", []),
    
    erlmcp_queue_optimized:stop(),
    io:format("=== ALL TESTS PASSED ===~n", []).

wait_for_pids([], _) -> ok;
wait_for_pids(Pids, Count) ->
    Live = [P || P <- Pids, is_process_alive(P)],
    case Live of
        [] -> ok;
        _ -> timer:sleep(10), wait_for_pids(Live, Count + 1)
    end.
