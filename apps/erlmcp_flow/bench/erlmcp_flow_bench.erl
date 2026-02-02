%%%-------------------------------------------------------------------
%%% @doc erlmcp-flow Performance Benchmark Suite
%%% @end
%%% Target: 500K msg/s, <50ms p99 latency, Zero task loss
%%%-------------------------------------------------------------------
-module(erlmcp_flow_bench).

-export([
    run_all/0,
    run_throughput/0,
    run_latency/0,
    run_memory/0,
    run_reliability/0,
    generate_report/1
]).

-include_lib("eunit/include/eunit.hrl").

-define(NUM_AGENTS, 60).
-define(NUM_MESSAGES, 100000).
-define(NUM_OPERATIONS, 10000).
-define(TARGET_THROUGHPUT, 500000).  % msg/s
-define(TARGET_LATENCY_P99, 50).     % ms
-define(TARGET_MEMORY_MB, 512).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Run all benchmarks and generate comprehensive report
run_all() ->
    io:format("~n=== erlmcp-flow Performance Benchmark Suite ===~n~n"),
    
    Results = #{
        throughput => run_throughput(),
        latency => run_latency(),
        memory => run_memory(),
        reliability => run_reliability()
    },
    
    Report = generate_report(Results),
    write_results(Report),
    
    io:format("~n=== Benchmark Complete ===~n"),
    io:format("Report: bench/results/flow_benchmark_~s.json~n", [timestamp()]),
    
    Results.

%%%===================================================================
%%% Throughput Benchmarks
%%%===================================================================

run_throughput() ->
    io:format("~n--- Throughput Benchmarks ---~n"),
    
    #{
        message_passing => bench_message_passing_throughput(),
        consensus => bench_consensus_throughput(),
        agent_scheduling => bench_agent_scheduling_throughput(),
        pattern_search => bench_pattern_search_throughput()
    }.

bench_message_passing_throughput() ->
    io:format("Message Passing Throughput: "),
    
    % Warmup
    warmup_agents(?NUM_AGENTS),
    
    % Benchmark
    Start = erlang:monotonic_time(microsecond),
    
    Agents = spawn_test_agents(?NUM_AGENTS),
    send_messages_batched(Agents, ?NUM_MESSAGES),
    wait_for_completion(Agents),
    
    Duration = erlang:monotonic_time(microsecond) - Start,
    Throughput = ?NUM_MESSAGES / (Duration / 1000000),
    
    cleanup_agents(Agents),
    
    Result = #{
        throughput_msg_per_s => round(Throughput),
        duration_ms => Duration / 1000,
        num_messages => ?NUM_MESSAGES,
        num_agents => ?NUM_AGENTS,
        target_met => Throughput >= ?TARGET_THROUGHPUT
    },
    
    io:format("~.0f msg/s (~s)~n", [
        Throughput,
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end
    ]),
    
    Result.

bench_consensus_throughput() ->
    io:format("Consensus Throughput: "),
    
    % Start Raft cluster (5 nodes)
    ClusterSize = 5,
    Cluster = spawn_raft_cluster(ClusterSize),
    
    Start = erlang:monotonic_time(microsecond),
    
    % Submit operations
    Operations = [{set, I, test_value} || I <- lists:seq(1, ?NUM_OPERATIONS)],
    submit_operations(Cluster, Operations),
    wait_for_commits(Cluster, length(Operations)),
    
    Duration = erlang:monotonic_time(microsecond) - Start,
    Throughput = length(Operations) / (Duration / 1000000),
    
    cleanup_cluster(Cluster),
    
    Result = #{
        throughput_ops_per_s => round(Throughput),
        duration_ms => Duration / 1000,
        num_operations => length(Operations),
        cluster_size => ClusterSize,
        target_met => Throughput >= 100000  % 100K ops/s target
    },
    
    io:format("~.0f ops/s (~s)~n", [
        Throughput,
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end
    ]),
    
    Result.

bench_agent_scheduling_throughput() ->
    io:format("Agent Scheduling Throughput: "),
    
    NumTasks = 50000,
    
    % Start pool
    Pool = spawn_agent_pool(#{min_size => 10, max_size => ?NUM_AGENTS}),
    
    Start = erlang:monotonic_time(microsecond),
    
    % Submit tasks
    Tasks = [create_task(I) || I <- lists:seq(1, NumTasks)],
    assign_tasks(Pool, Tasks),
    wait_for_task_completion(Pool, NumTasks),
    
    Duration = erlang:monotonic_time(microsecond) - Start,
    Throughput = NumTasks / (Duration / 1000000),
    
    cleanup_pool(Pool),
    
    Result = #{
        throughput_tasks_per_s => round(Throughput),
        duration_ms => Duration / 1000,
        num_tasks => NumTasks,
        pool_size => ?NUM_AGENTS,
        target_met => Throughput >= 100000  % 100K tasks/s target
    },
    
    io:format("~.0f tasks/s (~s)~n", [
        Throughput,
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end
    ]),
    
    Result.

bench_pattern_search_throughput() ->
    io:format("Pattern Search Throughput: "),
    
    % Build HNSW index
    NumVectors = 100000,
    VectorDim = 384,
    
    Index = build_hnsw_index(NumVectors, VectorDim),
    
    NumSearches = 1000,
    Start = erlang:monotonic_time(microsecond),
    
    % Execute searches
    [search_pattern(Index, random_vector(VectorDim), 10) || _ <- lists:seq(1, NumSearches)],
    
    Duration = erlang:monotonic_time(microsecond) - Start,
    Throughput = NumSearches / (Duration / 1000000),
    AvgLatency = Duration / NumSearches,
    
    Result = #{
        throughput_searches_per_s => round(Throughput),
        avg_latency_us => AvgLatency,
        num_searches => NumSearches,
        index_size => NumVectors,
        target_met => AvgLatency =< 10000  % <10ms avg
    },
    
    io:format("~.0f searches/s, ~.2fms avg (~s)~n", [
        Throughput,
        AvgLatency / 1000,
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end
    ]),
    
    Result.

%%%===================================================================
%%% Latency Benchmarks
%%%===================================================================

run_latency() ->
    io:format("~n--- Latency Benchmarks ---~n"),
    
    #{
        message_passing => bench_message_passing_latency(),
        consensus_finality => bench_consensus_latency(),
        agent_assignment => bench_agent_assignment_latency(),
        pattern_search => bench_pattern_search_latency()
    }.

bench_message_passing_latency() ->
    io:format("Message Passing Latency: "),
    
    NumSamples = 10000,
    Agents = spawn_test_agents(?NUM_AGENTS),
    
    % Warmup
    [send_and_measure(Agents) || _ <- lists:seq(1, 100)],
    
    % Measure
    Latencies = [send_and_measure(Agents) || _ <- lists:seq(1, NumSamples)],
    
    cleanup_agents(Agents),
    
    Result = #{
        p50_ms => percentile(Latencies, 0.50) / 1000,
        p95_ms => percentile(Latencies, 0.95) / 1000,
        p99_ms => percentile(Latencies, 0.99) / 1000,
        avg_ms => lists:sum(Latencies) / length(Latencies) / 1000,
        num_samples => NumSamples,
        target_met => percentile(Latencies, 0.99) / 1000 =< ?TARGET_LATENCY_P99
    },
    
    io:format("p50=~.2fms, p95=~.2fms, p99=~.2fms (~s)~n", [
        maps:get(p50_ms, Result),
        maps:get(p95_ms, Result),
        maps:get(p99_ms, Result),
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end
    ]),
    
    Result.

bench_consensus_latency() ->
    io:format("Consensus Finality Latency: "),
    
    Cluster = spawn_raft_cluster(5),
    NumSamples = 1000,
    
    % Warmup
    [submit_and_measure(Cluster) || _ <- lists:seq(1, 10)],
    
    % Measure
    Latencies = [submit_and_measure(Cluster) || _ <- lists:seq(1, NumSamples)],
    
    cleanup_cluster(Cluster),
    
    Result = #{
        p50_ms => percentile(Latencies, 0.50) / 1000,
        p95_ms => percentile(Latencies, 0.95) / 1000,
        p99_ms => percentile(Latencies, 0.99) / 1000,
        avg_ms => lists:sum(Latencies) / length(Latencies) / 1000,
        num_samples => NumSamples,
        target_met => percentile(Latencies, 0.99) / 1000 =< 100  % <100ms p99
    },
    
    io:format("p50=~.2fms, p95=~.2fms, p99=~.2fms (~s)~n", [
        maps:get(p50_ms, Result),
        maps:get(p95_ms, Result),
        maps:get(p99_ms, Result),
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end
    ]),
    
    Result.

bench_agent_assignment_latency() ->
    io:format("Agent Assignment Latency: "),
    
    Pool = spawn_agent_pool(#{min_size => 10, max_size => ?NUM_AGENTS}),
    NumSamples = 10000,
    
    % Warmup
    [assign_and_measure(Pool) || _ <- lists:seq(1, 100)],
    
    % Measure
    Latencies = [assign_and_measure(Pool) || _ <- lists:seq(1, NumSamples)],
    
    cleanup_pool(Pool),
    
    Result = #{
        p50_ms => percentile(Latencies, 0.50) / 1000,
        p95_ms => percentile(Latencies, 0.95) / 1000,
        p99_ms => percentile(Latencies, 0.99) / 1000,
        avg_ms => lists:sum(Latencies) / length(Latencies) / 1000,
        num_samples => NumSamples,
        target_met => percentile(Latencies, 0.99) / 1000 =< 1  % <1ms p99
    },
    
    io:format("p50=~.2fms, p95=~.2fms, p99=~.2fms (~s)~n", [
        maps:get(p50_ms, Result),
        maps:get(p95_ms, Result),
        maps:get(p99_ms, Result),
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end
    ]),
    
    Result.

bench_pattern_search_latency() ->
    io:format("Pattern Search Latency: "),
    
    Index = build_hnsw_index(100000, 384),
    NumSamples = 1000,
    
    % Warmup
    [search_and_measure(Index, 384) || _ <- lists:seq(1, 10)],
    
    % Measure
    Latencies = [search_and_measure(Index, 384) || _ <- lists:seq(1, NumSamples)],
    
    Result = #{
        p50_ms => percentile(Latencies, 0.50) / 1000,
        p95_ms => percentile(Latencies, 0.95) / 1000,
        p99_ms => percentile(Latencies, 0.99) / 1000,
        avg_ms => lists:sum(Latencies) / length(Latencies) / 1000,
        num_samples => NumSamples,
        target_met => percentile(Latencies, 0.99) / 1000 =< 100  % <100ms p99
    },
    
    io:format("p50=~.2fms, p95=~.2fms, p99=~.2fms (~s)~n", [
        maps:get(p50_ms, Result),
        maps:get(p95_ms, Result),
        maps:get(p99_ms, Result),
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end
    ]),
    
    Result.

%%%===================================================================
%%% Memory Benchmarks
%%%===================================================================

run_memory() ->
    io:format("~n--- Memory Benchmarks ---~n"),
    
    #{
        hnsw_index => bench_hnsw_memory(),
        agent_pool => bench_agent_pool_memory(),
        message_queue => bench_message_queue_memory(),
        total => bench_total_memory()
    }.

bench_hnsw_memory() ->
    io:format("HNSW Index Memory: "),
    
    erlang:garbage_collect(),
    MemBefore = erlang:memory(total),
    
    % Build index
    NumVectors = 100000,
    Index = build_hnsw_index(NumVectors, 384),
    
    erlang:garbage_collect(),
    MemAfter = erlang:memory(total),
    
    UsedMB = (MemAfter - MemBefore) / (1024 * 1024),
    BytesPerVector = (MemAfter - MemBefore) / NumVectors,
    
    Result = #{
        memory_mb => UsedMB,
        num_vectors => NumVectors,
        bytes_per_vector => BytesPerVector,
        target_met => UsedMB =< 100  % <100MB for 100K vectors
    },
    
    io:format("~.2f MB (~.0f bytes/vector) (~s)~n", [
        UsedMB,
        BytesPerVector,
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end
    ]),
    
    Result.

bench_agent_pool_memory() ->
    io:format("Agent Pool Memory: "),
    
    erlang:garbage_collect(),
    MemBefore = erlang:memory(total),
    
    % Start pool
    Pool = spawn_agent_pool(#{min_size => ?NUM_AGENTS, max_size => ?NUM_AGENTS}),
    
    erlang:garbage_collect(),
    MemAfter = erlang:memory(total),
    
    UsedMB = (MemAfter - MemBefore) / (1024 * 1024),
    MBPerAgent = UsedMB / ?NUM_AGENTS,
    
    cleanup_pool(Pool),
    
    Result = #{
        memory_mb => UsedMB,
        num_agents => ?NUM_AGENTS,
        mb_per_agent => MBPerAgent,
        target_met => UsedMB =< 300  % <300MB for 60 agents
    },
    
    io:format("~.2f MB (~.2f MB/agent) (~s)~n", [
        UsedMB,
        MBPerAgent,
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end
    ]),
    
    Result.

bench_message_queue_memory() ->
    io:format("Message Queue Memory: "),
    
    erlang:garbage_collect(),
    MemBefore = erlang:memory(total),
    
    % Create queue with 10K messages
    NumMessages = 10000,
    Queue = create_message_queue(NumMessages),
    
    erlang:garbage_collect(),
    MemAfter = erlang:memory(total),
    
    UsedMB = (MemAfter - MemBefore) / (1024 * 1024),
    BytesPerMessage = (MemAfter - MemBefore) / NumMessages,
    
    Result = #{
        memory_mb => UsedMB,
        num_messages => NumMessages,
        bytes_per_message => BytesPerMessage,
        target_met => UsedMB =< 50  % <50MB for 10K messages
    },
    
    io:format("~.2f MB (~.0f bytes/message) (~s)~n", [
        UsedMB,
        BytesPerMessage,
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end
    ]),
    
    Result.

bench_total_memory() ->
    io:format("Total System Memory: "),
    
    erlang:garbage_collect(),
    MemBefore = erlang:memory(total),
    
    % Full system: 60 agents + HNSW index + message queue
    Pool = spawn_agent_pool(#{min_size => ?NUM_AGENTS, max_size => ?NUM_AGENTS}),
    Index = build_hnsw_index(100000, 384),
    Queue = create_message_queue(10000),
    
    erlang:garbage_collect(),
    MemAfter = erlang:memory(total),
    
    UsedMB = (MemAfter - MemBefore) / (1024 * 1024),
    
    cleanup_pool(Pool),
    
    Result = #{
        memory_mb => UsedMB,
        target_met => UsedMB =< ?TARGET_MEMORY_MB
    },
    
    io:format("~.2f MB (~s)~n", [
        UsedMB,
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end
    ]),
    
    Result.

%%%===================================================================
%%% Reliability Benchmarks
%%%===================================================================

run_reliability() ->
    io:format("~n--- Reliability Benchmarks ---~n"),
    
    #{
        zero_task_loss => bench_zero_task_loss(),
        consensus_safety => bench_consensus_safety(),
        agent_failover => bench_agent_failover()
    }.

bench_zero_task_loss() ->
    io:format("Zero Task Loss: "),
    
    NumTasks = 10000,
    Pool = spawn_agent_pool(#{min_size => 10, max_size => ?NUM_AGENTS}),
    
    % Submit tasks
    TaskIds = [assign_task(Pool, create_task(I)) || I <- lists:seq(1, NumTasks)],
    
    % Randomly kill agents during execution
    spawn(fun() ->
        timer:sleep(100),
        [begin
            kill_random_agent(Pool),
            timer:sleep(10)
        end || _ <- lists:seq(1, 20)]
    end),
    
    % Wait for completion
    Completed = wait_for_all_tasks(TaskIds, 60000),
    
    cleanup_pool(Pool),
    
    LossRate = ((NumTasks - length(Completed)) / NumTasks) * 100,
    
    Result = #{
        total_tasks => NumTasks,
        completed_tasks => length(Completed),
        lost_tasks => NumTasks - length(Completed),
        loss_rate_pct => LossRate,
        target_met => length(Completed) == NumTasks
    },
    
    io:format("~p/~p completed, ~.2f% loss (~s)~n", [
        length(Completed),
        NumTasks,
        LossRate,
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end
    ]),
    
    Result.

bench_consensus_safety() ->
    io:format("Consensus Safety: "),
    
    Cluster = spawn_raft_cluster(5),
    NumOperations = 1000,
    
    % Submit operations
    Operations = [{set, I, value} || I <- lists:seq(1, NumOperations)],
    Indices = submit_operations(Cluster, Operations),
    
    % Kill random nodes
    spawn(fun() ->
        timer:sleep(50),
        [begin
            kill_random_node(Cluster),
            timer:sleep(20)
        end || _ <- lists:seq(1, 2)]
    end),
    
    % Verify all committed
    Committed = wait_for_commits(Cluster, length(Indices)),
    
    cleanup_cluster(Cluster),
    
    Result = #{
        total_operations => NumOperations,
        committed_operations => Committed,
        safety_violations => NumOperations - Committed,
        target_met => Committed == NumOperations
    },
    
    io:format("~p/~p committed (~s)~n", [
        Committed,
        NumOperations,
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end
    ]),
    
    Result.

bench_agent_failover() ->
    io:format("Agent Failover: "),
    
    Pool = spawn_agent_pool(#{min_size => 10, max_size => ?NUM_AGENTS}),
    NumFailovers = 20,
    
    % Measure failover time
    FailoverTimes = [measure_failover(Pool) || _ <- lists:seq(1, NumFailovers)],
    
    cleanup_pool(Pool),
    
    Result = #{
        p50_ms => percentile(FailoverTimes, 0.50) / 1000,
        p95_ms => percentile(FailoverTimes, 0.95) / 1000,
        p99_ms => percentile(FailoverTimes, 0.99) / 1000,
        avg_ms => lists:sum(FailoverTimes) / length(FailoverTimes) / 1000,
        num_failovers => NumFailovers,
        target_met => percentile(FailoverTimes, 0.99) / 1000 =< 100  % <100ms p99
    },
    
    io:format("p50=~.2fms, p95=~.2fms, p99=~.2fms (~s)~n", [
        maps:get(p50_ms, Result),
        maps:get(p95_ms, Result),
        maps:get(p99_ms, Result),
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end
    ]),
    
    Result.

%%%===================================================================
%%% Report Generation
%%%===================================================================

generate_report(Results) ->
    #{
        metadata => #{
            timestamp => erlang:system_time(millisecond),
            otp_version => erlang:system_info(otp_release),
            erts_version => erlang:system_info(version),
            system_architecture => erlang:system_info(system_architecture),
            schedulers => erlang:system_info(schedulers),
            schedulers_online => erlang:system_info(schedulers_online)
        },
        targets => #{
            throughput_msg_per_s => ?TARGET_THROUGHPUT,
            latency_p99_ms => ?TARGET_LATENCY_P99,
            memory_mb => ?TARGET_MEMORY_MB,
            task_loss_pct => 0.0
        },
        results => Results,
        summary => generate_summary(Results)
    }.

generate_summary(Results) ->
    % Extract key metrics
    ThroughputResult = maps:get(throughput, Results),
    LatencyResult = maps:get(latency, Results),
    MemoryResult = maps:get(memory, Results),
    ReliabilityResult = maps:get(reliability, Results),
    
    MessageThroughput = maps:get(throughput_msg_per_s, maps:get(message_passing, ThroughputResult)),
    MessageLatencyP99 = maps:get(p99_ms, maps:get(message_passing, LatencyResult)),
    TotalMemory = maps:get(memory_mb, maps:get(total, MemoryResult)),
    TaskLoss = maps:get(loss_rate_pct, maps:get(zero_task_loss, ReliabilityResult)),
    
    #{
        throughput_met => MessageThroughput >= ?TARGET_THROUGHPUT,
        latency_met => MessageLatencyP99 =< ?TARGET_LATENCY_P99,
        memory_met => TotalMemory =< ?TARGET_MEMORY_MB,
        reliability_met => TaskLoss == 0.0,
        
        overall_pass => (MessageThroughput >= ?TARGET_THROUGHPUT) and
                        (MessageLatencyP99 =< ?TARGET_LATENCY_P99) and
                        (TotalMemory =< ?TARGET_MEMORY_MB) and
                        (TaskLoss == 0.0),
        
        key_metrics => #{
            throughput_msg_per_s => MessageThroughput,
            latency_p99_ms => MessageLatencyP99,
            memory_mb => TotalMemory,
            task_loss_pct => TaskLoss
        }
    }.

write_results(Report) ->
    Filename = io_lib:format("bench/results/flow_benchmark_~s.json", [timestamp()]),
    file:write_file(Filename, jsx:encode(Report)),
    ok.

%%%===================================================================
%%% Helper Functions (Stubs - to be implemented)
%%%===================================================================

timestamp() ->
    {{Y, M, D}, {H, Min, S}} = calendar:universal_time(),
    io_lib:format("~4..0B~2..0B~2..0B_~2..0B~2..0B~2..0B", [Y, M, D, H, Min, S]).

percentile(List, P) ->
    Sorted = lists:sort(List),
    Index = max(1, min(length(Sorted), round(P * length(Sorted)))),
    lists:nth(Index, Sorted).

%% Stubs for implementation
spawn_test_agents(_N) -> [].
warmup_agents(_N) -> ok.
send_messages_batched(_Agents, _N) -> ok.
wait_for_completion(_Agents) -> ok.
cleanup_agents(_Agents) -> ok.

spawn_raft_cluster(_Size) -> undefined.
submit_operations(_Cluster, _Ops) -> [].
wait_for_commits(_Cluster, _N) -> 0.
cleanup_cluster(_Cluster) -> ok.

spawn_agent_pool(_Opts) -> undefined.
assign_tasks(_Pool, _Tasks) -> ok.
wait_for_task_completion(_Pool, _N) -> ok.
cleanup_pool(_Pool) -> ok.

build_hnsw_index(_N, _Dim) -> undefined.
search_pattern(_Index, _Vector, _K) -> [].

send_and_measure(_Agents) -> 0.
submit_and_measure(_Cluster) -> 0.
assign_and_measure(_Pool) -> 0.
search_and_measure(_Index, _Dim) -> 0.

create_message_queue(_N) -> undefined.
create_task(_I) -> #{}.
assign_task(_Pool, _Task) -> 0.
wait_for_all_tasks(_TaskIds, _Timeout) -> [].
kill_random_agent(_Pool) -> ok.
kill_random_node(_Cluster) -> ok.
measure_failover(_Pool) -> 0.
random_vector(_Dim) -> [].
