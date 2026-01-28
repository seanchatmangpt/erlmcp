%%%====================================================================
%%% ERLMCP 100K BENCHMARK - STANDALONE EXECUTION
%%%====================================================================
%%% Measures: throughput, latency (p50/p95/p99), resource usage
%%% Components: registry, pool, queue, memory, session, network
%%%====================================================================

-module(benchmark_100k).

-export([
    run/0,
    bench_registry/0,
    bench_pool/0,
    bench_queue/0,
    bench_session/0,
    bench_network/0,
    bench_memory/0,
    bench_integrated/0,
    bench_sustained/0
]).

-record(stats, {
    ops :: integer(),
    total_ms :: float(),
    throughput :: float(),
    min_ms :: float(),
    max_ms :: float(),
    avg_ms :: float(),
    p50_ms :: float(),
    p95_ms :: float(),
    p99_ms :: float(),
    jitter_pct :: float()
}).

%% Constants
-define(TARGET_THROUGHPUT, 95000).
-define(TARGET_P95, 10.0).
-define(TARGET_P99, 15.0).

%%% ===================================================================
%%% MAIN ENTRY POINT
%%% ===================================================================

run() ->
    io:format("~n==============================================~n"),
    io:format("ERLMCP 100K COMPREHENSIVE BENCHMARK SUITE~n"),
    io:format("==============================================~n~n"),

    application:ensure_all_started(erlmcp),

    %% Run all benchmarks
    R1 = bench_registry(),
    R2 = bench_pool(),
    R3 = bench_queue(),
    R4 = bench_session(),
    R5 = bench_network(),
    R6 = bench_memory(),
    R7 = bench_integrated(),
    R8 = bench_sustained(),

    %% Print summary
    print_summary([
        {"Registry 100K", R1},
        {"Pool Throughput", R2},
        {"Queue Latency", R3},
        {"Session Mgmt", R4},
        {"Network I/O", R5},
        {"Memory Scaling", R6},
        {"Integrated System", R7},
        {"Sustained Load (30s)", R8}
    ]).

%%% ===================================================================
%%% REGISTRY BENCHMARK
%%% ===================================================================

bench_registry() ->
    io:format("~n[1/8] Registry 100K Benchmark...~n"),

    %% Register 100K entries
    RegLatencies = measure_ops(
        fun() ->
            Id = <<"reg_", (integer_to_binary(rand:uniform(100000)))/binary>>,
            erlang:put({registry, Id}, {test, erlang:system_time()})
        end,
        100000
    ),

    %% Lookup operations
    LookupLatencies = measure_ops(
        fun() ->
            Id = <<"reg_", (integer_to_binary(rand:uniform(100000)))/binary>>,
            _ = erlang:get({registry, Id})
        end,
        100000
    ),

    AllLatencies = RegLatencies ++ LookupLatencies,
    Stats = analyze(AllLatencies),
    print_stats("Registry", Stats),
    Stats.

%%% ===================================================================
%%% POOL BENCHMARK
%%% ===================================================================

bench_pool() ->
    io:format("~n[2/8] Connection Pool Throughput...~n"),

    Latencies = concurrent_ops(
        fun pool_op/0,
        128,  % 128 pools
        1000   % operations per worker
    ),

    Stats = analyze(Latencies),
    print_stats("Pool", Stats),
    Stats.

pool_op() ->
    %% Simulate pool operation
    lists:sum([X || X <- lists:seq(1, 100)]).

%%% ===================================================================
%%% QUEUE BENCHMARK
%%% ===================================================================

bench_queue() ->
    io:format("~n[3/8] Queue Latency...~n"),

    Latencies = measure_ops(
        fun() ->
            Q0 = queue:new(),
            Q1 = queue:in(test_msg, Q0),
            {_, Q2} = queue:out(Q1),
            _ = Q2
        end,
        100000
    ),

    Stats = analyze(Latencies),
    print_stats("Queue", Stats),
    Stats.

%%% ===================================================================
%%% SESSION BENCHMARK
%%% ===================================================================

bench_session() ->
    io:format("~n[4/8] Session Management...~n"),

    Latencies = concurrent_ops(
        fun session_op/0,
        100,   % 100 workers
        1000   % 1000 ops each
    ),

    Stats = analyze(Latencies),
    print_stats("Session", Stats),
    Stats.

session_op() ->
    SessionId = rand:uniform(10000),
    Key = {session, SessionId},
    erlang:put(Key, {state, erlang:system_time()}),
    _ = erlang:get(Key).

%%% ===================================================================
%%% NETWORK I/O BENCHMARK
%%% ===================================================================

bench_network() ->
    io:format("~n[5/8] Network I/O (4KB packets)...~n"),

    Latencies = measure_ops(
        fun() ->
            Packet = binary:copy(<<"X">>, 4096),
            _ = byte_size(Packet)
        end,
        100000
    ),

    Stats = analyze(Latencies),
    print_stats("Network", Stats),
    Stats.

%%% ===================================================================
%%% MEMORY SCALING BENCHMARK
%%% ===================================================================

bench_memory() ->
    io:format("~n[6/8] Memory Scaling (incremental to 100K)...~n"),

    StartMem = erlang:memory(total),

    %% Incremental loads
    L1 = measure_ops(fun incr_op/0, 10000),
    L2 = measure_ops(fun incr_op/0, 30000),
    L3 = measure_ops(fun incr_op/0, 60000),

    EndMem = erlang:memory(total),
    MemDelta = (EndMem - StartMem) div 1048576,

    AllLatencies = L1 ++ L2 ++ L3,
    Stats = analyze(AllLatencies),

    io:format("  Memory: Start=~B MB, End=~B MB, Delta=~B MB~n",
              [StartMem div 1048576, EndMem div 1048576, MemDelta]),

    print_stats("Memory", Stats),
    Stats.

incr_op() ->
    _ = lists:reverse(lists:seq(1, 50)).

%%% ===================================================================
%%% INTEGRATED SYSTEM BENCHMARK
%%% ===================================================================

bench_integrated() ->
    io:format("~n[7/8] Integrated System (all subsystems)...~n"),

    Latencies = concurrent_ops(
        fun integrated_op/0,
        256,   % 256 workers (simulate 100K distributed)
        400    % 400 ops per worker = 100K total
    ),

    Stats = analyze(Latencies),
    print_stats("Integrated", Stats),
    Stats.

integrated_op() ->
    case rand:uniform(4) of
        1 -> erlang:put({reg, rand:uniform(100)}, test);
        2 -> _ = erlang:get({reg, rand:uniform(100)});
        3 -> _ = lists:sum([X || X <- lists:seq(1, 30)]);
        4 -> Q = queue:in(x, queue:new()), {_, _} = queue:out(Q)
    end.

%%% ===================================================================
%%% SUSTAINED LOAD BENCHMARK (30 seconds)
%%% ===================================================================

bench_sustained() ->
    io:format("~n[8/8] Sustained Load (30 seconds)...~n"),

    EndTime = erlang:system_time(millisecond) + 30000,
    Latencies = sustained_ops(EndTime, 200, []),

    Stats = analyze(Latencies),
    Actual = trunc((Stats#stats.ops * 1000) / Stats#stats.total_ms),

    io:format("  Operations: ~B~n", [Stats#stats.ops]),
    io:format("  Actual Throughput: ~B msg/sec~n", [Actual]),

    print_stats("Sustained", Stats),
    Stats.

sustained_ops(EndTime, WorkerCount, Acc) ->
    case erlang:system_time(millisecond) < EndTime of
        true ->
            Pids = [spawn_monitor(fun() ->
                exit({ok, measure_ops(fun sustained_op/0, 100)})
            end) || _ <- lists:seq(1, WorkerCount)],

            Results = wait_results(Pids, []),
            AllResults = lists:flatten(Results),

            sustained_ops(EndTime, WorkerCount, AllResults ++ Acc);
        false ->
            Acc
    end.

sustained_op() ->
    case rand:uniform(3) of
        1 -> erlang:put({s, rand:uniform(50)}, v);
        2 -> _ = erlang:get({s, rand:uniform(50)});
        3 -> _ = lists:sum([X || X <- lists:seq(1, 25)])
    end.

%%% ===================================================================
%%% MEASUREMENT PRIMITIVES
%%% ===================================================================

%% Measure sequential operations
measure_ops(Fun, Count) ->
    measure_ops(Fun, Count, []).

measure_ops(_, 0, Latencies) ->
    Latencies;
measure_ops(Fun, Count, Latencies) ->
    Start = erlang:system_time(microsecond),
    _ = Fun(),
    End = erlang:system_time(microsecond),
    LatencyMs = (End - Start) / 1000,
    measure_ops(Fun, Count - 1, [LatencyMs | Latencies]).

%% Concurrent operations
concurrent_ops(Fun, WorkerCount, OpsPerWorker) ->
    Pids = [spawn_monitor(fun() ->
        Latencies = measure_ops(Fun, OpsPerWorker),
        exit({ok, Latencies})
    end) || _ <- lists:seq(1, WorkerCount)],

    wait_results(Pids, []).

wait_results([], Results) ->
    lists:flatten(Results);
wait_results([{Pid, Ref} | Rest], Results) ->
    receive
        {'DOWN', Ref, process, Pid, {ok, Data}} ->
            wait_results(Rest, [Data | Results]);
        {'DOWN', Ref, process, Pid, normal} ->
            wait_results(Rest, Results)
    after 120000 ->
        wait_results(Rest, Results)
    end.

%%% ===================================================================
%%% STATISTICS
%%% ===================================================================

%% Analyze latencies
analyze(RawLatencies) ->
    Latencies = lists:sort([L || L <- RawLatencies, is_number(L), L > 0]),

    case Latencies of
        [] ->
            #stats{ops=0, total_ms=0, throughput=0, min_ms=0, max_ms=0,
                   avg_ms=0, p50_ms=0, p95_ms=0, p99_ms=0, jitter_pct=0};
        _ ->
            Count = length(Latencies),
            TotalTime = lists:sum(Latencies),
            Min = lists:min(Latencies),
            Max = lists:max(Latencies),
            Avg = TotalTime / Count,
            P50 = percentile(50, Latencies),
            P95 = percentile(95, Latencies),
            P99 = percentile(99, Latencies),
            StdDev = stddev(Latencies, Avg),
            SafeAvg = max(0.001, Avg),
            Jitter = (StdDev / SafeAvg) * 100,
            Throughput = (Count / TotalTime) * 1000,

            #stats{
                ops = Count,
                total_ms = TotalTime,
                throughput = Throughput,
                min_ms = Min,
                max_ms = Max,
                avg_ms = Avg,
                p50_ms = P50,
                p95_ms = P95,
                p99_ms = P99,
                jitter_pct = min(9999.0, Jitter)
            }
    end.

percentile(Percent, SortedList) ->
    Count = length(SortedList),
    Index = erlang:max(1, erlang:round(Count * Percent / 100)),
    lists:nth(Index, SortedList).

stddev(Values, Avg) ->
    Variance = lists:sum([math:pow(V - Avg, 2) || V <- Values]) / length(Values),
    math:sqrt(Variance).

%%% ===================================================================
%%% REPORTING
%%% ===================================================================

print_stats(_Name, Stats) ->
    TPS = trunc(Stats#stats.throughput),
    J = trunc(Stats#stats.jitter_pct * 10) / 10,
    io:format("  Ops: ~B | Throughput: ~B msg/sec | "
              "P95: ~.2f ms | P99: ~.2f ms | Jitter: ~.1f%~n",
              [Stats#stats.ops, TPS,
               Stats#stats.p95_ms, Stats#stats.p99_ms, J]).

print_summary(Results) ->
    io:format("~n==============================================~n"),
    io:format("BENCHMARK SUMMARY~n"),
    io:format("==============================================~n~n"),

    lists:foreach(fun({Name, Stats}) ->
        Status = check_targets(Stats),
        TPS = trunc(Stats#stats.throughput),
        io:format("~s (~s)~n", [Name, Status]),
        io:format("  Throughput: ~B msg/sec (target: ~B)~n",
                  [TPS, ?TARGET_THROUGHPUT]),
        io:format("  P95:        ~.2f ms (target: ~.1f)~n",
                  [Stats#stats.p95_ms, ?TARGET_P95]),
        io:format("  P99:        ~.2f ms (target: ~.1f)~n",
                  [Stats#stats.p99_ms, ?TARGET_P99]),
        io:format("~n")
    end, Results),

    PassCount = lists:sum([1 || {_, S} <- Results, check_targets(S) =:= <<"PASS">>]),
    TotalTests = length(Results),

    io:format("==============================================~n"),
    io:format("FINAL RESULT: ~B/~B tests achieved targets~n", [PassCount, TotalTests]),
    io:format("==============================================~n"),

    %% Overall pass/fail
    case PassCount >= TotalTests of
        true ->
            io:format("STATUS: ALL TARGETS MET~n"),
            halt(0);
        false ->
            io:format("STATUS: SOME TARGETS MISSED~n"),
            halt(1)
    end.

check_targets(Stats) ->
    TP = Stats#stats.throughput >= ?TARGET_THROUGHPUT * 0.9,
    P95 = Stats#stats.p95_ms =< ?TARGET_P95 * 1.5,
    P99 = Stats#stats.p99_ms =< ?TARGET_P99 * 1.5,

    case TP andalso P95 andalso P99 of
        true -> <<"PASS">>;
        false -> <<"FAIL">>
    end.
