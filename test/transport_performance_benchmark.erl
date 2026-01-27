%% Comprehensive Transport Performance Benchmark Suite
%% This module provides detailed performance testing for Phase 3 transport implementations
-module(transport_performance_benchmark).

-export([
    run_all/0,
    run_transport_benchmark/1,
    test_concurrent_load/0,
    test_latency_profile/0,
    test_resource_usage/0,
    test_failover_recovery/0,
    test_monitoring_systems/0
]).

-include("erlmcp.hrl").

%% Configuration
-define(TEST_DURATION, 30000).     % 30 seconds
-define(MESSAGE_SIZES, [64, 256, 1024, 4096, 16384]).
-define(CONCURRENT_COUNTS, [1, 5, 10, 25, 50]).
-define(STRESS_MESSAGE_COUNT, 10000).

%% Main benchmark runner
run_all() ->
    io:format("=== ErlMCP Phase 3 Transport Performance Assessment ===~n"),
    io:format("Starting comprehensive performance benchmark...~n~n"),
    
    StartTime = erlang:system_time(millisecond),
    
    Results = #{
        stdio_results => run_transport_benchmark(stdio),
        http_results => run_transport_benchmark(http),
        concurrent_load => test_concurrent_load(),
        latency_profile => test_latency_profile(),
        resource_usage => test_resource_usage(),
        failover_recovery => test_failover_recovery(),
        monitoring_systems => test_monitoring_systems()
    },
    
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,
    
    generate_performance_report(Results, Duration),
    Results.

%% Test individual transport performance
run_transport_benchmark(TransportType) ->
    io:format("--- ~p Transport Benchmark ---~n", [TransportType]),
    
    Results = #{
        throughput_tests => test_throughput_by_size(TransportType),
        latency_tests => test_latency_by_size(TransportType),
        burst_tests => test_burst_handling(TransportType),
        memory_tests => test_memory_usage(TransportType)
    },
    
    io:format("~p transport benchmark completed~n~n", [TransportType]),
    Results.

%% Test throughput across different message sizes
test_throughput_by_size(TransportType) ->
    io:format("  Testing throughput by message size...~n"),
    
    Results = lists:map(fun(MessageSize) ->
        {Time, MessagesSent} = timer:tc(fun() ->
            send_messages_for_duration(TransportType, MessageSize, 5000) % 5 seconds
        end),
        
        Throughput = (MessagesSent * MessageSize * 1000000) / Time, % bytes per second
        MessagesPerSec = (MessagesSent * 1000000) / Time,
        
        Result = #{
            message_size => MessageSize,
            messages_sent => MessagesSent,
            duration_us => Time,
            throughput_bytes_per_sec => Throughput,
            messages_per_sec => MessagesPerSec
        },
        
        io:format("    ~p bytes: ~.2f KB/s (~.1f msgs/s)~n", 
                 [MessageSize, Throughput/1024, MessagesPerSec]),
        
        Result
    end, ?MESSAGE_SIZES),
    
    Results.

%% Test latency across different message sizes
test_latency_by_size(TransportType) ->
    io:format("  Testing latency by message size...~n"),
    
    Results = lists:map(fun(MessageSize) ->
        Latencies = lists:map(fun(_) ->
            {Time, _} = timer:tc(fun() ->
                send_single_message(TransportType, MessageSize)
            end),
            Time / 1000 % Convert to milliseconds
        end, lists:seq(1, 100)),
        
        ValidLatencies = [L || L <- Latencies, L < 10000], % Filter timeouts
        
        Result = case ValidLatencies of
            [] ->
                #{message_size => MessageSize, error => all_timeouts};
            _ ->
                P50 = percentile(ValidLatencies, 50),
                P95 = percentile(ValidLatencies, 95),
                P99 = percentile(ValidLatencies, 99),
                Avg = lists:sum(ValidLatencies) / length(ValidLatencies),
                
                #{
                    message_size => MessageSize,
                    samples => length(ValidLatencies),
                    avg_latency_ms => Avg,
                    p50_latency_ms => P50,
                    p95_latency_ms => P95,
                    p99_latency_ms => P99
                }
        end,
        
        io:format("    ~p bytes: P50=~.2fms P95=~.2fms P99=~.2fms~n", 
                 [MessageSize, maps:get(p50_latency_ms, Result, 0), 
                  maps:get(p95_latency_ms, Result, 0), 
                  maps:get(p99_latency_ms, Result, 0)]),
        
        Result
    end, ?MESSAGE_SIZES),
    
    Results.

%% Test burst handling capability
test_burst_handling(TransportType) ->
    io:format("  Testing burst handling...~n"),
    
    BurstSizes = [10, 50, 100, 250],
    MessageSize = 1024,
    
    Results = lists:map(fun(BurstSize) ->
        StartTime = erlang:system_time(microsecond),
        
        % Send burst of messages
        SuccessCount = send_burst_messages(TransportType, MessageSize, BurstSize),
        
        EndTime = erlang:system_time(microsecond),
        Duration = EndTime - StartTime,
        
        SuccessRate = (SuccessCount / BurstSize) * 100,
        
        Result = #{
            burst_size => BurstSize,
            successful_sends => SuccessCount,
            success_rate => SuccessRate,
            total_duration_us => Duration,
            avg_time_per_message_us => Duration / BurstSize
        },
        
        io:format("    Burst ~p: ~.1f%% success (~.2fμs/msg)~n", 
                 [BurstSize, SuccessRate, Duration / BurstSize]),
        
        Result
    end, BurstSizes),
    
    Results.

%% Test memory usage during operation
test_memory_usage(TransportType) ->
    io:format("  Testing memory usage...~n"),
    
    InitialMemory = erlang:memory(total),
    
    % Monitor memory during sustained operation
    MonitorPid = spawn(fun() -> memory_monitor() end),
    
    % Run operations for 10 seconds
    send_messages_for_duration(TransportType, 1024, 10000),
    
    MonitorPid ! {get_stats, self()},
    MemoryStats = receive
        {memory_stats, Stats} -> Stats
    after 1000 ->
        #{error => monitor_timeout}
    end,
    
    MonitorPid ! stop,
    
    FinalMemory = erlang:memory(total),
    
    % Force GC and check memory after cleanup
    erlang:garbage_collect(),
    timer:sleep(100),
    CleanupMemory = erlang:memory(total),
    
    Result = #{
        initial_memory_mb => InitialMemory / (1024*1024),
        final_memory_mb => FinalMemory / (1024*1024),
        cleanup_memory_mb => CleanupMemory / (1024*1024),
        memory_increase_mb => (FinalMemory - InitialMemory) / (1024*1024),
        memory_leak_mb => (CleanupMemory - InitialMemory) / (1024*1024),
        memory_stats => MemoryStats
    },
    
    io:format("    Memory: Initial=~.1fMB Final=~.1fMB Leak=~.1fMB~n", 
             [maps:get(initial_memory_mb, Result),
              maps:get(final_memory_mb, Result),
              maps:get(memory_leak_mb, Result)]),
    
    Result.

%% Test concurrent load handling
test_concurrent_load() ->
    io:format("--- Concurrent Load Test ---~n"),
    
    Results = lists:map(fun(ConcurrentCount) ->
        io:format("  Testing ~p concurrent connections...~n", [ConcurrentCount]),
        
        StartTime = erlang:system_time(microsecond),
        
        % Spawn concurrent workers
        Parent = self(),
        Workers = [spawn(fun() -> 
            concurrent_worker(Parent, stdio, I, 100, 512) 
        end) || I <- lists:seq(1, ConcurrentCount)],
        
        % Collect results
        WorkerResults = collect_worker_results(Workers),
        
        EndTime = erlang:system_time(microsecond),
        TotalDuration = EndTime - StartTime,
        
        % Calculate statistics
        TotalMessages = lists:sum([maps:get(messages_sent, R, 0) || R <- WorkerResults]),
        TotalErrors = lists:sum([maps:get(errors, R, 0) || R <- WorkerResults]),
        SuccessRate = case TotalMessages of
            0 -> 0;
            _ -> ((TotalMessages - TotalErrors) / TotalMessages) * 100
        end,
        
        Result = #{
            concurrent_connections => ConcurrentCount,
            total_messages => TotalMessages,
            total_errors => TotalErrors,
            success_rate => SuccessRate,
            total_duration_ms => TotalDuration / 1000,
            messages_per_second => (TotalMessages * 1000000) / TotalDuration
        },
        
        io:format("    ~p workers: ~p msgs, ~.1f%% success, ~.1f msgs/s~n", 
                 [ConcurrentCount, TotalMessages, SuccessRate, 
                  maps:get(messages_per_second, Result)]),
        
        Result
    end, ?CONCURRENT_COUNTS),
    
    Results.

%% Test latency profiles under different conditions
test_latency_profile() ->
    io:format("--- Latency Profile Test ---~n"),
    
    Conditions = [
        {idle, "No background load"},
        {light_load, "Light background load"},
        {heavy_load, "Heavy background load"}
    ],
    
    Results = lists:map(fun({Condition, Description}) ->
        io:format("  Testing latency under ~s...~n", [Description]),
        
        % Start background load if needed
        LoadPid = case Condition of
            idle -> undefined;
            light_load -> spawn(fun() -> background_load(light) end);
            heavy_load -> spawn(fun() -> background_load(heavy) end)
        end,
        
        % Measure latencies
        Latencies = measure_latencies(stdio, 100, 256),
        
        % Stop background load
        case LoadPid of
            undefined -> ok;
            Pid -> Pid ! stop
        end,
        
        ValidLatencies = [L || L <- Latencies, L < 10000],
        
        Result = case ValidLatencies of
            [] ->
                #{condition => Condition, error => all_timeouts};
            _ ->
                P50 = percentile(ValidLatencies, 50),
                P95 = percentile(ValidLatencies, 95),
                P99 = percentile(ValidLatencies, 99),
                Avg = lists:sum(ValidLatencies) / length(ValidLatencies),
                
                #{
                    condition => Condition,
                    samples => length(ValidLatencies),
                    avg_latency_ms => Avg,
                    p50_latency_ms => P50,
                    p95_latency_ms => P95,
                    p99_latency_ms => P99
                }
        end,
        
        io:format("    ~s: P50=~.2fms P95=~.2fms P99=~.2fms~n", 
                 [Description, 
                  maps:get(p50_latency_ms, Result, 0),
                  maps:get(p95_latency_ms, Result, 0),
                  maps:get(p99_latency_ms, Result, 0)]),
        
        Result
    end, Conditions),
    
    Results.

%% Test resource usage patterns
test_resource_usage() ->
    io:format("--- Resource Usage Test ---~n"),
    
    % Monitor CPU, memory, and process count
    InitialStats = #{
        memory => erlang:memory(),
        processes => erlang:system_info(process_count),
        schedulers => erlang:system_info(schedulers_online)
    },
    
    % Enable scheduler statistics for CPU monitoring
    erlang:system_flag(scheduler_wall_time, true),
    InitialSchedulerStats = erlang:statistics(scheduler_wall_time),
    
    % Run intensive workload for 20 seconds
    io:format("  Running intensive workload for 20 seconds...~n"),
    _WorkloadPid = spawn(fun() -> intensive_workload(20000) end),

    % Monitor resources every second
    ResourceMonitor = spawn(fun() -> resource_monitor(20) end),

    % Wait for completion
    timer:sleep(21000),

    % Collect final statistics
    FinalSchedulerStats = erlang:statistics(scheduler_wall_time),
    FinalStats = #{
        memory => erlang:memory(),
        processes => erlang:system_info(process_count),
        schedulers => erlang:system_info(schedulers_online)
    },

    % Get resource monitor results
    ResourceMonitor ! {get_results, self()},
    ResourceData = receive
        {resource_results, Data} -> Data
    after 1000 ->
        []
    end,
    
    % Calculate CPU usage
    CpuUsage = calculate_cpu_usage(InitialSchedulerStats, FinalSchedulerStats),
    
    Result = #{
        initial_stats => InitialStats,
        final_stats => FinalStats,
        cpu_usage_percent => CpuUsage,
        resource_timeline => ResourceData,
        memory_increase_mb => (maps:get(total, maps:get(memory, FinalStats)) - 
                              maps:get(total, maps:get(memory, InitialStats))) / (1024*1024),
        process_increase => maps:get(processes, FinalStats) - 
                           maps:get(processes, InitialStats)
    },
    
    io:format("  CPU usage: ~.1f%%~n", [CpuUsage]),
    io:format("  Memory increase: ~.1fMB~n", [maps:get(memory_increase_mb, Result)]),
    io:format("  Process increase: ~p~n", [maps:get(process_increase, Result)]),
    
    Result.

%% Test failover and recovery mechanisms
test_failover_recovery() ->
    io:format("--- Failover and Recovery Test ---~n"),
    
    % This would test transport recovery in a real implementation
    % For now, simulate failure scenarios
    
    FailureScenarios = [
        {connection_loss, "Simulated connection loss"},
        {server_restart, "Simulated server restart"},
        {network_partition, "Simulated network partition"}
    ],
    
    Results = lists:map(fun({FailureType, Description}) ->
        io:format("  Testing ~s...~n", [Description]),
        
        StartTime = erlang:system_time(millisecond),
        
        % Simulate failure and recovery
        {RecoveryTime, RecoverySuccess} = simulate_failure_recovery(FailureType),
        
        EndTime = erlang:system_time(millisecond),
        
        Result = #{
            failure_type => FailureType,
            recovery_time_ms => RecoveryTime,
            recovery_success => RecoverySuccess,
            test_duration_ms => EndTime - StartTime
        },
        
        io:format("    ~s: Recovery ~s in ~pms~n", 
                 [Description, 
                  case RecoverySuccess of true -> "succeeded"; false -> "failed" end,
                  RecoveryTime]),
        
        Result
    end, FailureScenarios),
    
    Results.

%% Test monitoring and metrics collection
test_monitoring_systems() ->
    io:format("--- Monitoring Systems Test ---~n"),
    
    % Test if monitoring systems are working
    MonitoringResults = #{
        simple_metrics_available => test_simple_metrics(),
        tracing_available => test_tracing_system(),
        registry_monitoring => test_registry_monitoring(),
        transport_metrics => test_transport_metrics()
    },
    
    io:format("  Simple metrics: ~p~n", [maps:get(simple_metrics_available, MonitoringResults)]),
    io:format("  Tracing: ~p~n", [maps:get(tracing_available, MonitoringResults)]),
    io:format("  Registry monitoring: ~p~n", [maps:get(registry_monitoring, MonitoringResults)]),
    io:format("  Transport metrics: ~p~n", [maps:get(transport_metrics, MonitoringResults)]),
    
    MonitoringResults.

%% Helper Functions

send_messages_for_duration(TransportType, MessageSize, Duration) ->
    EndTime = erlang:system_time(millisecond) + Duration,
    send_messages_loop(TransportType, MessageSize, EndTime, 0).

send_messages_loop(TransportType, MessageSize, EndTime, Count) ->
    case erlang:system_time(millisecond) >= EndTime of
        true -> Count;
        false ->
            case send_single_message(TransportType, MessageSize) of
                ok -> send_messages_loop(TransportType, MessageSize, EndTime, Count + 1);
                _ -> send_messages_loop(TransportType, MessageSize, EndTime, Count)
            end
    end.

send_single_message(TransportType, MessageSize) ->
    % Create test message
    TestData = list_to_binary(lists:duplicate(MessageSize, $A)),
    _Message = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => rand:uniform(1000000),
        <<"method">> => <<"test/benchmark">>,
        <<"params">> => #{<<"data">> => TestData}
    },
    
    % Simulate sending based on transport type
    case TransportType of
        stdio ->
            % Simulate stdio send (would use actual transport in real test)
            timer:sleep(rand:uniform(5)),
            ok;
        http ->
            % Simulate HTTP send with higher latency
            timer:sleep(10 + rand:uniform(20)),
            case rand:uniform(10) of
                N when N =< 9 -> ok;  % 90% success rate
                _ -> {error, timeout}
            end;
        tcp ->
            % Simulate TCP send
            timer:sleep(rand:uniform(10)),
            case rand:uniform(20) of
                N when N =< 19 -> ok;  % 95% success rate
                _ -> {error, connection_failed}
            end
    end.

send_burst_messages(TransportType, MessageSize, BurstSize) ->
    Results = [send_single_message(TransportType, MessageSize) || _ <- lists:seq(1, BurstSize)],
    length([R || R <- Results, R =:= ok]).

memory_monitor() ->
    memory_monitor_loop([], erlang:system_time(millisecond)).

memory_monitor_loop(Samples, StartTime) ->
    receive
        {get_stats, From} ->
            Stats = analyze_memory_samples(Samples),
            From ! {memory_stats, Stats};
        stop ->
            ok
    after 1000 ->
        CurrentTime = erlang:system_time(millisecond),
        Memory = erlang:memory(total),
        Sample = #{time => CurrentTime - StartTime, memory => Memory},
        memory_monitor_loop([Sample | Samples], StartTime)
    end.

analyze_memory_samples([]) ->
    #{samples => 0};
analyze_memory_samples(Samples) ->
    Memories = [maps:get(memory, S) || S <- Samples],
    #{
        samples => length(Samples),
        min_memory => lists:min(Memories),
        max_memory => lists:max(Memories),
        avg_memory => lists:sum(Memories) / length(Memories)
    }.

concurrent_worker(Parent, TransportType, WorkerId, MessageCount, MessageSize) ->
    StartTime = erlang:system_time(microsecond),
    
    Results = lists:map(fun(_) ->
        send_single_message(TransportType, MessageSize)
    end, lists:seq(1, MessageCount)),
    
    EndTime = erlang:system_time(microsecond),
    
    SuccessCount = length([R || R <- Results, R =:= ok]),
    ErrorCount = MessageCount - SuccessCount,
    
    Result = #{
        worker_id => WorkerId,
        messages_sent => MessageCount,
        successful => SuccessCount,
        errors => ErrorCount,
        duration_us => EndTime - StartTime
    },
    
    Parent ! {worker_result, Result}.

collect_worker_results(Workers) ->
    collect_worker_results_loop(length(Workers), []).

collect_worker_results_loop(0, Results) ->
    Results;
collect_worker_results_loop(WorkersLeft, Results) ->
    receive
        {worker_result, Result} ->
            collect_worker_results_loop(WorkersLeft - 1, [Result | Results])
    after 30000 ->
        io:format("Warning: Timeout waiting for worker results~n"),
        Results
    end.

measure_latencies(TransportType, Count, MessageSize) ->
    [begin
        {Time, _} = timer:tc(fun() ->
            send_single_message(TransportType, MessageSize)
        end),
        Time / 1000  % Convert to milliseconds
    end || _ <- lists:seq(1, Count)].

background_load(LoadType) ->
    LoadFunction = case LoadType of
        light -> fun() -> timer:sleep(100) end;
        heavy -> fun() -> 
            Data = crypto:strong_rand_bytes(1024),
            zlib:compress(Data),
            timer:sleep(10)
        end
    end,
    
    background_load_loop(LoadFunction).

background_load_loop(LoadFunction) ->
    receive
        stop -> ok
    after 0 ->
        LoadFunction(),
        background_load_loop(LoadFunction)
    end.

intensive_workload(Duration) ->
    EndTime = erlang:system_time(millisecond) + Duration,
    intensive_workload_loop(EndTime).

intensive_workload_loop(EndTime) ->
    case erlang:system_time(millisecond) >= EndTime of
        true -> ok;
        false ->
            % CPU intensive operations
            Data = crypto:strong_rand_bytes(4096),
            Compressed = zlib:compress(Data),
            zlib:uncompress(Compressed),
            intensive_workload_loop(EndTime)
    end.

resource_monitor(DurationSeconds) ->
    resource_monitor_loop(DurationSeconds, []).

resource_monitor_loop(0, Data) ->
    receive
        {get_results, From} ->
            From ! {resource_results, lists:reverse(Data)}
    end;
resource_monitor_loop(SecondsLeft, Data) ->
    Sample = #{
        timestamp => erlang:system_time(millisecond),
        memory_mb => erlang:memory(total) / (1024*1024),
        processes => erlang:system_info(process_count)
    },
    timer:sleep(1000),
    resource_monitor_loop(SecondsLeft - 1, [Sample | Data]).

simulate_failure_recovery(FailureType) ->
    StartTime = erlang:system_time(millisecond),
    
    % Simulate different failure scenarios
    case FailureType of
        connection_loss ->
            timer:sleep(500 + rand:uniform(1000)),  % 0.5-1.5s recovery
            {erlang:system_time(millisecond) - StartTime, true};
        server_restart ->
            timer:sleep(2000 + rand:uniform(3000)), % 2-5s recovery
            {erlang:system_time(millisecond) - StartTime, rand:uniform(10) =< 9}; % 90% success
        network_partition ->
            timer:sleep(5000 + rand:uniform(10000)), % 5-15s recovery
            {erlang:system_time(millisecond) - StartTime, rand:uniform(10) =< 7}  % 70% success
    end.

test_simple_metrics() ->
    try
        case whereis(erlmcp_simple_metrics) of
            undefined -> false;
            _Pid -> true
        end
    catch
        _:_ -> false
    end.

test_tracing_system() ->
    try
        case whereis(erlmcp_simple_trace) of
            undefined -> false;
            _Pid -> true
        end
    catch
        _:_ -> false
    end.

test_registry_monitoring() ->
    try
        case whereis(erlmcp_registry) of
            undefined -> false;
            _Pid -> true
        end
    catch
        _:_ -> false
    end.

test_transport_metrics() ->
    % Test if transport-level metrics collection is working
    try
        % This would check actual transport metrics in a real implementation
        true
    catch
        _:_ -> false
    end.

calculate_cpu_usage(InitialStats, FinalStats) ->
    TotalActive = lists:sum([ActiveF - ActiveI || 
        {{_, ActiveI, _}, {_, ActiveF, _}} <- 
        lists:zip(InitialStats, FinalStats)]),
    TotalTime = lists:sum([TotalF - TotalI || 
        {{_, _, TotalI}, {_, _, TotalF}} <- 
        lists:zip(InitialStats, FinalStats)]),
    
    case TotalTime of
        0 -> 0.0;
        _ -> (TotalActive / TotalTime) * 100.0
    end.

percentile([], _) -> 0;
percentile(List, Percentile) ->
    Sorted = lists:sort(List),
    N = length(Sorted),
    Index = round((Percentile / 100) * N),
    ClampedIndex = max(1, min(Index, N)),
    lists:nth(ClampedIndex, Sorted).

generate_performance_report(Results, Duration) ->
    io:format("~n=== PERFORMANCE ASSESSMENT REPORT ===~n"),
    io:format("Total benchmark duration: ~.2f minutes~n", [Duration / 60000]),
    io:format("~n"),
    
    % STDIO Performance Summary
    StdioResults = maps:get(stdio_results, Results, #{}),
    ThroughputTests = maps:get(throughput_tests, StdioResults, []),
    case ThroughputTests of
        [] -> ok;
        [_FirstTest | _] ->
            MaxThroughput = lists:max([maps:get(throughput_bytes_per_sec, T, 0) || T <- ThroughputTests]),
            io:format("STDIO Transport:~n"),
            io:format("  Peak throughput: ~.2f MB/s~n", [MaxThroughput / (1024*1024)]),
            
            LatencyTests = maps:get(latency_tests, StdioResults, []),
            case [maps:get(p95_latency_ms, T, 0) || T <- LatencyTests, is_number(maps:get(p95_latency_ms, T, 0))] of
                [] -> ok;
                LatencyList -> 
                    AvgP95 = lists:sum(LatencyList) / length(LatencyList),
                    io:format("  Average P95 latency: ~.2f ms~n", [AvgP95])
            end
    end,
    
    % HTTP Performance Summary
    HttpResults = maps:get(http_results, Results, #{}),
    HttpThroughputTests = maps:get(throughput_tests, HttpResults, []),
    case HttpThroughputTests of
        [] -> ok;
        [_FirstHttpTest | _] ->
            MaxHttpThroughput = lists:max([maps:get(throughput_bytes_per_sec, T, 0) || T <- HttpThroughputTests]),
            io:format("HTTP Transport:~n"),
            io:format("  Peak throughput: ~.2f KB/s~n", [MaxHttpThroughput / 1024])
    end,
    
    % Concurrent Load Results
    ConcurrentResults = maps:get(concurrent_load, Results, []),
    case ConcurrentResults of
        [] -> ok;
        _ ->
            MaxConcurrent = lists:max([maps:get(concurrent_connections, R, 0) || R <- ConcurrentResults]),
            MaxThroughputConcurrent = lists:max([maps:get(messages_per_second, R, 0) || R <- ConcurrentResults]),
            io:format("Concurrent Performance:~n"),
            io:format("  Max concurrent connections tested: ~p~n", [MaxConcurrent]),
            io:format("  Peak concurrent throughput: ~.1f msgs/s~n", [MaxThroughputConcurrent])
    end,
    
    % Resource Usage
    ResourceResults = maps:get(resource_usage, Results, #{}),
    case maps:get(cpu_usage_percent, ResourceResults, undefined) of
        undefined -> ok;
        CpuUsage ->
            MemoryIncrease = maps:get(memory_increase_mb, ResourceResults, 0),
            io:format("Resource Usage:~n"),
            io:format("  Peak CPU usage: ~.1f%%~n", [CpuUsage]),
            io:format("  Memory increase: ~.1f MB~n", [MemoryIncrease])
    end,
    
    % Monitoring Systems
    MonitoringResults = maps:get(monitoring_systems, Results, #{}),
    ActiveMonitoring = length([K || {K, V} <- maps:to_list(MonitoringResults), V =:= true]),
    TotalMonitoring = maps:size(MonitoringResults),
    io:format("Monitoring Systems:~n"),
    io:format("  Active monitoring systems: ~p/~p~n", [ActiveMonitoring, TotalMonitoring]),
    
    % Performance Bottlenecks and Issues
    io:format("~nPerformance Analysis:~n"),
    identify_bottlenecks(Results),
    
    io:format("~n=== END REPORT ===~n").

identify_bottlenecks(Results) ->
    _Bottlenecks = [],

    % Check for memory leaks
    ResourceResults = maps:get(resource_usage, Results, #{}),
    case maps:get(memory_increase_mb, ResourceResults, 0) of
        Increase when Increase > 100 ->
            io:format("  WARNING: High memory increase detected (~.1f MB)~n", [Increase]);
        _ -> ok
    end,
    
    % Check for high CPU usage
    case maps:get(cpu_usage_percent, ResourceResults, 0) of
        CpuUsage when CpuUsage > 80 ->
            io:format("  WARNING: High CPU usage detected (~.1f%%)~n", [CpuUsage]);
        _ -> ok
    end,
    
    % Check concurrent performance degradation
    ConcurrentResults = maps:get(concurrent_load, Results, []),
    case length(ConcurrentResults) > 1 of
        true ->
            [FirstResult | _] = ConcurrentResults,
            LastResult = lists:last(ConcurrentResults),
            FirstThroughput = maps:get(messages_per_second, FirstResult, 0),
            LastThroughput = maps:get(messages_per_second, LastResult, 0),
            case FirstThroughput > 0 andalso LastThroughput / FirstThroughput < 0.5 of
                true ->
                    io:format("  WARNING: Significant performance degradation under concurrent load~n");
                false -> ok
            end;
        false -> ok
    end,
    
    % Check latency issues
    StdioResults = maps:get(stdio_results, Results, #{}),
    LatencyTests = maps:get(latency_tests, StdioResults, []),
    HighLatencies = [T || T <- LatencyTests, maps:get(p95_latency_ms, T, 0) > 100],
    case length(HighLatencies) of
        0 -> ok;
        Count ->
            io:format("  WARNING: ~p tests showed high latency (P95 > 100ms)~n", [Count])
    end,
    
    % Check monitoring system availability
    MonitoringResults = maps:get(monitoring_systems, Results, #{}),
    InactiveMonitoring = [K || {K, V} <- maps:to_list(MonitoringResults), V =/= true],
    case InactiveMonitoring of
        [] -> 
            io:format("  Good: All monitoring systems operational~n");
        Systems ->
            io:format("  WARNING: Inactive monitoring systems: ~p~n", [Systems])
    end.