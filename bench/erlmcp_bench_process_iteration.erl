%%%====================================================================
%%% ERLMCP PROCESS ITERATION BENCHMARK - OTP 28 ITERATOR VS LIST
%%%====================================================================
%%% Module: erlmcp_bench_process_iteration
%%% Purpose: Compare process iteration scalability on OTP 28
%%% Target: O(1) memory on OTP 28 vs O(N) on OTP 27
%%% Workloads: 1K, 10K, 100K, 1M processes
%%% Measures: Memory allocation patterns, iteration throughput
%%%====================================================================

-module(erlmcp_bench_process_iteration).

-export([
    run/0,
    run/1,
    run_all/0,
    workloads/0
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Workload Definitions
%%====================================================================

-spec workloads() -> [map()].
workloads() ->
    [
        #{id => <<"proc_iter_1k">>, processes => 1000},
        #{id => <<"proc_iter_10k">>, processes => 10000},
        #{id => <<"proc_iter_100k">>, processes => 100000},
        #{id => <<"proc_iter_1m">>, processes => 1000000}
    ].

%%====================================================================
%% Main Entry Points
%%====================================================================

-spec run() -> ok.
run() ->
    run_all().

-spec run(binary()) -> ok | {error, term()}.
run(WorkloadId) when is_binary(WorkloadId) ->
    Workloads = workloads(),
    case lists:filter(fun(#{id := Id}) -> Id =:= WorkloadId end, Workloads) of
        [] ->
            io:format("ERROR: Unknown workload: ~s~n", [WorkloadId]),
            {error, {unknown_workload, WorkloadId}};
        [Workload] ->
            run_workload(Workload)
    end;
run(WorkloadId) when is_list(WorkloadId) ->
    run(list_to_binary(WorkloadId)).

-spec run_all() -> ok.
run_all() ->
    io:format("~n==============================================~n"),
    io:format("ERLMCP PROCESS ITERATION BENCHMARK~n"),
    io:format("OTP 28 Iterator vs OTP 27 List~n"),
    io:format("==============================================~n~n"),

    OtpVersion = erlang:system_info(otp_release),
    io:format("OTP Version: ~s~n", [OtpVersion]),
    io:format("Iterator available: ~p~n~n", [has_process_iterator()]),

    lists:foreach(fun(Workload) ->
        run_workload(Workload)
    end, workloads()),

    io:format("~n==============================================~n"),
    io:format("Process iteration benchmarks complete.~n"),
    io:format("Results in bench/results/~n"),
    io:format("==============================================~n~n"),
    ok.

%%====================================================================
%% Workload Execution
%%====================================================================

-spec run_workload(map()) -> ok | {error, term()}.
run_workload(#{id := WorkloadId, processes := NumProcs} = _Workload) ->
    io:format("~n--- Workload: ~s (~p processes) ---~n", [WorkloadId, NumProcs]),

    %% Capture environment
    Env = capture_environment(),

    %% Spawn test processes
    io:format("Spawning ~p processes...~n", [NumProcs]),
    ProcessPids = spawn_test_processes(NumProcs),
    
    io:format("Spawned ~p processes, measuring iteration...~n", [length(ProcessPids)]),

    %% Benchmark traditional processes/0 (OTP 27 and earlier)
    io:format("~nBenchmarking processes/0 (traditional)...~n"),
    ProcessesResults = benchmark_processes_list(NumProcs),

    %% Benchmark processes_iterator/0 (OTP 28+)
    IteratorResults = case has_process_iterator() of
        true ->
            io:format("Benchmarking processes_iterator/0 (OTP 28)...~n"),
            benchmark_processes_iterator(NumProcs);
        false ->
            io:format("processes_iterator/0 not available (OTP < 28)~n"),
            undefined
    end,

    %% Cleanup test processes
    cleanup_test_processes(ProcessPids),

    %% Build report
    Report = build_report(WorkloadId, Env, NumProcs, ProcessesResults, IteratorResults),

    %% Validate and write
    case validate_report(Report) of
        ok ->
            Timestamp = erlang:system_time(second),
            Filename = io_lib:format("bench/results/proc_iter_~s_~p.json", [WorkloadId, Timestamp]),
            write_report(Filename, Report),
            io:format("✓ Report written: ~s~n", [Filename]),
            
            %% Display comparison
            display_comparison(NumProcs, ProcessesResults, IteratorResults),
            ok;
        {error, ValidationError} ->
            io:format("✗ Validation failed: ~p~n", [ValidationError]),
            {error, {validation_failed, ValidationError}}
    end.

%%====================================================================
%% Process Management
%%====================================================================

%% Spawn test processes that stay alive for the benchmark
-spec spawn_test_processes(pos_integer()) -> [pid()].
spawn_test_processes(NumProcs) ->
    lists:map(fun(_) ->
        spawn(fun() ->
            %% Simple process that waits for termination
            receive
                stop -> ok
            end
        end)
    end, lists:seq(1, NumProcs)).

%% Cleanup test processes
-spec cleanup_test_processes([pid()]) -> ok.
cleanup_test_processes(Pids) ->
    lists:foreach(fun(Pid) ->
        catch Pid ! stop
    end, Pids),
    %% Give processes time to exit
    timer:sleep(100),
    ok.

%%====================================================================
%% Benchmark: processes/0 (Traditional)
%%====================================================================

-spec benchmark_processes_list(pos_integer()) -> map().
benchmark_processes_list(_NumProcs) ->
    %% Measure memory before
    MemoryBefore = erlang:memory(total),
    ProcessMemBefore = erlang:memory(processes),

    %% Measure iteration
    Iterations = 10,
    Latencies = lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        
        %% Get all processes (O(N) memory allocation)
        AllProcs = erlang:processes(),
        Count = length(AllProcs),
        
        End = erlang:monotonic_time(microsecond),
        {End - Start, Count}
    end, lists:seq(1, Iterations)),

    %% Measure memory after
    MemoryAfter = erlang:memory(total),
    ProcessMemAfter = erlang:memory(processes),

    %% Extract timing data
    TimingsUs = [T || {T, _} <- Latencies],
    {_, ProcessCount} = hd(Latencies),

    %% Calculate memory delta
    MemoryDelta = MemoryAfter - MemoryBefore,
    ProcessMemDelta = ProcessMemAfter - ProcessMemBefore,

    %% Estimate memory per process (allocation overhead)
    MemoryPerProc = MemoryDelta / ProcessCount,

    #{
        method => processes_list,
        iterations => Iterations,
        process_count => ProcessCount,
        latency_metrics => calculate_metrics(TimingsUs),
        memory_total_delta_mib => round_float(MemoryDelta / (1024 * 1024), 2),
        memory_processes_delta_mib => round_float(ProcessMemDelta / (1024 * 1024), 2),
        memory_per_process_bytes => round_float(MemoryPerProc, 2),
        complexity => <<"O(N)">>,
        description => <<"Allocates full process list in memory">>
    }.

%%====================================================================
%% Benchmark: processes_iterator/0 (OTP 28)
%%====================================================================

-spec benchmark_processes_iterator(pos_integer()) -> map().
benchmark_processes_iterator(_NumProcs) ->
    case has_process_iterator() of
        false ->
            #{method => processes_iterator, available => false};
        true ->
            %% Measure memory before
            MemoryBefore = erlang:memory(total),
            ProcessMemBefore = erlang:memory(processes),

            %% Measure iteration
            Iterations = 10,
            Latencies = lists:map(fun(_) ->
                Start = erlang:monotonic_time(microsecond),
                
                %% Iterate using iterator (O(1) memory)
                Iterator = erlang:processes_iterator(),
                Count = count_processes_iterator(Iterator, 0),
                
                End = erlang:monotonic_time(microsecond),
                {End - Start, Count}
            end, lists:seq(1, Iterations)),

            %% Measure memory after
            MemoryAfter = erlang:memory(total),
            ProcessMemAfter = erlang:memory(processes),

            %% Extract timing data
            TimingsUs = [T || {T, _} <- Latencies],
            {_, ProcessCount} = hd(Latencies),

            %% Calculate memory delta (should be minimal)
            MemoryDelta = MemoryAfter - MemoryBefore,
            ProcessMemDelta = ProcessMemAfter - ProcessMemBefore,

            %% Estimate memory per process (should be near zero for iterator)
            MemoryPerProc = MemoryDelta / max(1, ProcessCount),

            #{
                method => processes_iterator,
                iterations => Iterations,
                process_count => ProcessCount,
                latency_metrics => calculate_metrics(TimingsUs),
                memory_total_delta_mib => round_float(MemoryDelta / (1024 * 1024), 2),
                memory_processes_delta_mib => round_float(ProcessMemDelta / (1024 * 1024), 2),
                memory_per_process_bytes => round_float(MemoryPerProc, 2),
                complexity => <<"O(1)">>,
                description => <<"Iterator with constant memory usage">>
            }
    end.

%% Count processes using iterator
-spec count_processes_iterator(term(), non_neg_integer()) -> non_neg_integer().
count_processes_iterator(Iterator, Acc) ->
    case erlang:processes_iterator_next(Iterator) of
        {_Pid, NextIterator} ->
            count_processes_iterator(NextIterator, Acc + 1);
        done ->
            Acc
    end.

%%====================================================================
%% Metrics Calculation
%%====================================================================

-spec calculate_metrics([non_neg_integer()]) -> map().
calculate_metrics([]) ->
    #{p50_us => 0.0, p95_us => 0.0, p99_us => 0.0, avg_us => 0.0, min_us => 0.0, max_us => 0.0};
calculate_metrics(Latencies) ->
    Sorted = lists:sort(Latencies),
    Len = length(Sorted),
    
    P50 = percentile(Sorted, 0.50),
    P95 = percentile(Sorted, 0.95),
    P99 = percentile(Sorted, 0.99),
    Avg = lists:sum(Sorted) / Len,
    Min = lists:min(Sorted),
    Max = lists:max(Sorted),
    
    #{
        p50_us => round_float(P50, 1),
        p95_us => round_float(P95, 1),
        p99_us => round_float(P99, 1),
        avg_us => round_float(Avg, 1),
        min_us => round_float(Min, 1),
        max_us => round_float(Max, 1)
    }.

-spec percentile([number()], float()) -> float().
percentile(SortedList, Percentile) ->
    Len = length(SortedList),
    Index = max(1, min(Len, round(Len * Percentile))),
    lists:nth(Index, SortedList).

%%====================================================================
%% Report Building
%%====================================================================

-spec build_report(binary(), map(), pos_integer(), map(), map() | undefined) -> map().
build_report(WorkloadId, Env, NumProcs, ProcessesResults, IteratorResults) ->
    BaseReport = #{
        workload_id => WorkloadId,
        benchmark => <<"process_iteration">>,
        timestamp => erlang:system_time(second),
        environment => Env,
        process_count => NumProcs,
        precision => <<"microsecond">>,
        scope => <<"per_node">>,
        processes_list_results => ProcessesResults
    },
    
    case IteratorResults of
        undefined ->
            BaseReport;
        _ ->
            %% Calculate memory improvement
            ListMemory = maps:get(memory_per_process_bytes, ProcessesResults),
            IteratorMemory = maps:get(memory_per_process_bytes, IteratorResults),
            
            MemoryImprovement = if
                ListMemory > 0 -> ((ListMemory - IteratorMemory) / ListMemory) * 100;
                true -> 0.0
            end,
            
            BaseReport#{
                processes_iterator_results => IteratorResults,
                memory_improvement_percent => round_float(MemoryImprovement, 1),
                target_complexity => <<"O(1) vs O(N)">>
            }
    end.

%%====================================================================
%% Display & Output
%%====================================================================

-spec display_comparison(pos_integer(), map(), map() | undefined) -> ok.
display_comparison(_NumProcs, ProcessesResults, undefined) ->
    io:format("~n--- Results (processes/0 only) ---~n"),
    display_method_results(processes_list, ProcessesResults),
    ok;
display_comparison(NumProcs, ProcessesResults, IteratorResults) ->
    io:format("~n--- Performance Comparison (~p processes) ---~n~n", [NumProcs]),
    
    io:format("processes/0 (Traditional O(N)):~n"),
    display_method_results(processes_list, ProcessesResults),
    
    io:format("~nprocesses_iterator/0 (OTP 28 O(1)):~n"),
    display_method_results(processes_iterator, IteratorResults),
    
    %% Calculate improvements
    ListMetrics = maps:get(latency_metrics, ProcessesResults),
    IteratorMetrics = maps:get(latency_metrics, IteratorResults),
    
    ListLatency = maps:get(avg_us, ListMetrics),
    IteratorLatency = maps:get(avg_us, IteratorMetrics),
    
    ListMemory = maps:get(memory_per_process_bytes, ProcessesResults),
    IteratorMemory = maps:get(memory_per_process_bytes, IteratorResults),
    
    LatencyRatio = if
        IteratorLatency > 0 -> ListLatency / IteratorLatency;
        true -> 0.0
    end,
    
    MemoryRatio = if
        IteratorMemory > 0 -> ListMemory / IteratorMemory;
        true -> infinity
    end,
    
    io:format("~n--- Memory Efficiency ---~n"),
    io:format("List memory:     ~.2f bytes/process (O(N) allocation)~n", [ListMemory]),
    io:format("Iterator memory: ~.2f bytes/process (O(1) allocation)~n", [IteratorMemory]),
    io:format("Memory ratio:    ~.1fx reduction~n", [MemoryRatio]),
    
    io:format("~n--- Latency Comparison ---~n"),
    io:format("List latency:     ~.1f us (avg)~n", [ListLatency]),
    io:format("Iterator latency: ~.1f us (avg)~n", [IteratorLatency]),
    io:format("Latency ratio:    ~.2fx~n", [LatencyRatio]),
    
    Status = if
        MemoryRatio > 100.0 -> "✓ EXCELLENT O(1) MEMORY";
        MemoryRatio > 10.0 -> "✓ GOOD MEMORY IMPROVEMENT";
        MemoryRatio > 1.0 -> "⚠ MODERATE IMPROVEMENT";
        true -> "✗ NO IMPROVEMENT"
    end,
    io:format("~nStatus: ~s~n", [Status]),
    
    ok.

-spec display_method_results(atom(), map()) -> ok.
display_method_results(_Method, Results) ->
    Metrics = maps:get(latency_metrics, Results),
    Memory = maps:get(memory_total_delta_mib, Results),
    MemoryPerProc = maps:get(memory_per_process_bytes, Results),
    Complexity = maps:get(complexity, Results),
    
    io:format("  Latency:    ~.1f us (p50), ~.1f us (p95), ~.1f us (p99)~n",
              [maps:get(p50_us, Metrics), maps:get(p95_us, Metrics), maps:get(p99_us, Metrics)]),
    io:format("  Memory:     ~.2f MiB total, ~.2f bytes/process~n", [Memory, MemoryPerProc]),
    io:format("  Complexity: ~s~n", [Complexity]),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

-spec has_process_iterator() -> boolean().
has_process_iterator() ->
    %% Check if processes_iterator/0 is available (OTP 28+)
    erlang:function_exported(erlang, processes_iterator, 0).

-spec capture_environment() -> map().
capture_environment() ->
    {ok, Hostname} = inet:gethostname(),
    OtpRelease = erlang:system_info(otp_release),
    
    #{
        hostname => list_to_binary(Hostname),
        erlang_version => list_to_binary("OTP-" ++ OtpRelease),
        processes_iterator_available => has_process_iterator()
    }.

-spec validate_report(map()) -> ok | {error, term()}.
validate_report(Report) ->
    RequiredFields = [workload_id, benchmark, timestamp, environment, processes_list_results],
    case lists:all(fun(F) -> maps:is_key(F, Report) end, RequiredFields) of
        true -> ok;
        false -> {error, missing_required_fields}
    end.

-spec write_report(string(), map()) -> ok | {error, term()}.
write_report(Filename, Report) ->
    filelib:ensure_dir(Filename),
    Json = jsx:encode(Report, [{space, 2}, {indent, 2}]),
    file:write_file(Filename, Json).

-spec round_float(number(), integer()) -> float().
round_float(Value, DecimalPlaces) ->
    Multiplier = math:pow(10, DecimalPlaces),
    round(Value * Multiplier) / Multiplier.
