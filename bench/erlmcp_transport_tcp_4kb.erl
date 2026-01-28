%%%===================================================================
%%% erlmcp_transport_tcp_4kb.erl - TCP Transport 4KB Payload Benchmark
%%%===================================================================
%%%
%%% Measures transport ceiling for 4KB payloads
%%% Target: >= 95K msg/sec (2.2x improvement from 42.6K baseline)
%%%
%%% Execution:
%%%   erl -pa _build/default/lib/*/ebin -s erlmcp_transport_tcp_4kb run
%%%   OR
%%%   rebar3 run erlmcp_transport_tcp_4kb
%%%
%%% Output: JSON metrics + CSV for graphing
%%%

-module(erlmcp_transport_tcp_4kb).

-export([run/0, benchmark/0, run_test/1, run_test/2]).

-record(stats, {
    ops = 0 :: non_neg_integer(),
    total_time_ms = 0.0 :: float(),
    throughput = 0.0 :: float(),
    min_us = 999999.0 :: float(),
    max_us = 0.0 :: float(),
    avg_us = 0.0 :: float(),
    p50_us = 0.0 :: float(),
    p95_us = 0.0 :: float(),
    p99_us = 0.0 :: float(),
    gc_time_ms = 0.0 :: float(),
    gc_count = 0 :: non_neg_integer()
}).

-define(PAYLOAD_SIZE, 4096).
-define(DURATION_MS, 30000).
-define(TARGET_THROUGHPUT, 95000).
-define(BASELINE_THROUGHPUT, 42600).

%%====================================================================
%% Main Entry Point
%%====================================================================

run() ->
    main([]).

main(_Args) ->
    benchmark(),
    halt(0).

run_test(Label) ->
    run_test(Label, ?DURATION_MS).

run_test(Label, DurationMs) ->
    io:format("~n=== TCP 4KB Transport Benchmark: ~s ===~n", [Label]),
    benchmark_tcp_transport(DurationMs).

%%====================================================================
%% Benchmark Main
%%====================================================================

benchmark() ->
    application:ensure_all_started(erlmcp),

    io:format("~n~n", []),
    io:format("========================================================~n"),
    io:format("ERLMCP TCP Transport 4KB Payload Benchmark v1.3.0~n"),
    io:format("========================================================~n"),
    io:format("Baseline:  ~w msg/sec (v1.2.0)~n", [?BASELINE_THROUGHPUT]),
    io:format("Target:    ~w msg/sec (2.2x improvement)~n", [?TARGET_THROUGHPUT]),
    io:format("Payload:   ~w bytes~n", [?PAYLOAD_SIZE]),
    io:format("Duration:  ~w ms per test~n", [?DURATION_MS]),
    io:format("~n", []),

    %% Run 5 iterations for statistical significance
    Results = lists:map(fun(Iteration) ->
        io:format("[~w/5] Running benchmark iteration...~n", [Iteration]),
        benchmark_tcp_transport(?DURATION_MS)
    end, lists:seq(1, 5)),

    %% Analyze results
    print_results(Results),

    %% Export data for graphing
    export_csv(Results),
    export_json(Results),

    ok.

%%====================================================================
%% TCP Transport Benchmark
%%====================================================================

benchmark_tcp_transport(DurationMs) ->
    %% Start erlmcp application
    application:ensure_all_started(erlmcp),

    %% Create server
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => benchmark_server
    }),

    %% Get actual port
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    %% Create N worker connections
    NumWorkers = 32,
    WorkerPids = lists:map(fun(_I) ->
        start_worker(localhost, Port)
    end, lists:seq(1, NumWorkers)),

    %% Give workers time to connect
    timer:sleep(500),

    %% Measure GC before test
    erlang:garbage_collect(),
    GCBefore = erlang:statistics(garbage_collection),

    %% Run benchmark
    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + DurationMs,

    {TotalOps, Latencies} = run_benchmark_workers(WorkerPids, EndTime, 0, []),

    %% Measure GC after test
    GCAfter = erlang:statistics(garbage_collection),
    {GCCount, _Words, GCTime} = GCAfter,
    {GCCountBefore, _WordsBefore, GCTimeBefore} = GCBefore,

    ActualDurationMs = erlang:monotonic_time(millisecond) - StartTime,
    ActualDurationSec = ActualDurationMs / 1000.0,

    %% Calculate statistics
    Throughput = TotalOps / ActualDurationSec,

    %% Sort latencies for percentile calculation
    SortedLatencies = lists:sort(Latencies),
    Stats = #stats{
        ops = TotalOps,
        total_time_ms = ActualDurationMs,
        throughput = Throughput,
        min_us = lists:min(SortedLatencies),
        max_us = lists:max(SortedLatencies),
        avg_us = lists:sum(SortedLatencies) / length(SortedLatencies),
        p50_us = percentile(SortedLatencies, 0.50),
        p95_us = percentile(SortedLatencies, 0.95),
        p99_us = percentile(SortedLatencies, 0.99),
        gc_time_ms = (GCTime - GCTimeBefore) / 1000.0,
        gc_count = GCCount - GCCountBefore
    },

    %% Cleanup
    lists:foreach(fun(Pid) ->
        try erlmcp_transport_tcp:close(Pid) catch _:_ -> ok end
    end, WorkerPids),
    erlmcp_transport_tcp:close(ServerPid),

    Stats.

%%====================================================================
%% Worker Process
%%====================================================================

start_worker(Host, Port) ->
    {ok, Pid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => Host,
        port => Port,
        connect_timeout => 5000
    }),
    Pid.

run_benchmark_workers([], _EndTime, Ops, Latencies) ->
    {Ops, Latencies};
run_benchmark_workers(WorkerPids, EndTime, Ops, Latencies) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    case CurrentTime >= EndTime of
        true ->
            {Ops, Latencies};
        false ->
            %% Send messages from all workers
            NewLatencies = lists:foldl(fun(Pid, Acc) ->
                Payload = binary:copy(<<0:?PAYLOAD_SIZE/unit:8>>),
                MsgStart = erlang:monotonic_time(microsecond),
                erlmcp_transport_tcp:send(Pid, Payload),
                MsgEnd = erlang:monotonic_time(microsecond),
                LatencyUs = MsgEnd - MsgStart,
                [LatencyUs | Acc]
            end, Latencies, WorkerPids),

            run_benchmark_workers(WorkerPids, EndTime, Ops + length(WorkerPids), NewLatencies)
    end.

%%====================================================================
%% Statistics Helpers
%%====================================================================

percentile([], _P) -> 0.0;
percentile(Data, P) when P >= 0, P =< 1 ->
    N = length(Data),
    Index = max(1, round(N * P)),
    lists:nth(Index, Data).

%%====================================================================
%% Reporting
%%====================================================================

print_results(Results) ->
    io:format("~n", []),
    io:format("========================================================~n"),
    io:format("BENCHMARK RESULTS (5 iterations)~n"),
    io:format("========================================================~n"),

    %% Print per-iteration results
    lists:foreach(fun({I, Stats}) ->
        io:format("~nIteration ~w:~n", [I]),
        io:format("  Throughput:  ~.2f msg/sec~n", [Stats#stats.throughput]),
        io:format("  Operations:  ~w~n", [Stats#stats.ops]),
        io:format("  Duration:    ~.2f ms~n", [Stats#stats.total_time_ms]),
        io:format("  Latency (µs):~n"),
        io:format("    Min:       ~.2f~n", [Stats#stats.min_us]),
        io:format("    P50:       ~.2f~n", [Stats#stats.p50_us]),
        io:format("    P95:       ~.2f~n", [Stats#stats.p95_us]),
        io:format("    P99:       ~.2f~n", [Stats#stats.p99_us]),
        io:format("    Max:       ~.2f~n", [Stats#stats.max_us]),
        io:format("    Avg:       ~.2f~n", [Stats#stats.avg_us]),
        io:format("  GC Impact:   ~.2f ms (~w collections)~n", [Stats#stats.gc_time_ms, Stats#stats.gc_count])
    end, lists:enumerate(1, Results)),

    %% Calculate aggregate statistics
    Throughputs = [S#stats.throughput || S <- Results],
    AvgThroughput = lists:sum(Throughputs) / length(Throughputs),
    MaxThroughput = lists:max(Throughputs),
    MinThroughput = lists:min(Throughputs),
    StdDev = stddev(Throughputs),

    Improvement = ((AvgThroughput - ?BASELINE_THROUGHPUT) / ?BASELINE_THROUGHPUT) * 100,
    Target = ?TARGET_THROUGHPUT,

    io:format("~n", []),
    io:format("========================================================~n"),
    io:format("AGGREGATE STATISTICS~n"),
    io:format("========================================================~n"),
    io:format("Average Throughput: ~.2f msg/sec~n", [AvgThroughput]),
    io:format("Min/Max Throughput: ~.2f / ~.2f msg/sec~n", [MinThroughput, MaxThroughput]),
    io:format("Std Deviation:      ~.2f msg/sec~n", [StdDev]),
    io:format("~nComparison:~n"),
    io:format("  Baseline (v1.2.0):  ~w msg/sec~n", [?BASELINE_THROUGHPUT]),
    io:format("  Target (v1.3.0):    ~w msg/sec~n", [Target]),
    io:format("  Current:            ~.2f msg/sec~n", [AvgThroughput]),
    io:format("  Improvement:        ~+.2f%~n", [Improvement]),

    case AvgThroughput >= Target of
        true ->
            io:format("~n  ✓ TARGET ACHIEVED~n");
        false ->
            Gap = Target - AvgThroughput,
            io:format("~n  ✗ Gap to target: ~.2f msg/sec (~.2f%)~n",
                     [Gap, (Gap / Target) * 100])
    end,

    io:format("~n", []),
    io:format("========================================================~n").

stddev(Values) when length(Values) < 2 ->
    0.0;
stddev(Values) ->
    Mean = lists:sum(Values) / length(Values),
    Variance = lists:sum([math:pow(V - Mean, 2) || V <- Values]) / (length(Values) - 1),
    math:sqrt(Variance).

%%====================================================================
%% Data Export
%%====================================================================

export_csv(Results) ->
    Filename = "transport_tcp_4kb_results.csv",
    Header = "iteration,throughput_msg_sec,operations,duration_ms,min_us,p50_us,p95_us,p99_us,max_us,avg_us,gc_time_ms,gc_count\n",
    Lines = lists:map(fun({I, S}) ->
        io_lib:format("~w,~.2f,~w,~.2f,~.2f,~.2f,~.2f,~.2f,~.2f,~.2f,~.2f,~w~n",
                     [I,
                      S#stats.throughput,
                      S#stats.ops,
                      S#stats.total_time_ms,
                      S#stats.min_us,
                      S#stats.p50_us,
                      S#stats.p95_us,
                      S#stats.p99_us,
                      S#stats.max_us,
                      S#stats.avg_us,
                      S#stats.gc_time_ms,
                      S#stats.gc_count])
    end, lists:enumerate(1, Results)),

    Content = [Header | Lines],
    file:write_file(Filename, Content),
    io:format("Exported: ~s~n", [Filename]).

export_json(Results) ->
    Filename = "transport_tcp_4kb_results.json",
    JsonResults = lists:map(fun({I, S}) ->
        #{
            iteration => I,
            throughput_msg_sec => round(S#stats.throughput * 100) / 100,
            operations => S#stats.ops,
            duration_ms => round(S#stats.total_time_ms * 100) / 100,
            latency => #{
                min_us => round(S#stats.min_us * 100) / 100,
                p50_us => round(S#stats.p50_us * 100) / 100,
                p95_us => round(S#stats.p95_us * 100) / 100,
                p99_us => round(S#stats.p99_us * 100) / 100,
                max_us => round(S#stats.max_us * 100) / 100,
                avg_us => round(S#stats.avg_us * 100) / 100
            },
            gc => #{
                time_ms => round(S#stats.gc_time_ms * 100) / 100,
                collections => S#stats.gc_count
            }
        }
    end, lists:enumerate(1, Results)),

    Json = jsx:encode(JsonResults),
    file:write_file(Filename, Json),
    io:format("Exported: ~s~n", [Filename]).
