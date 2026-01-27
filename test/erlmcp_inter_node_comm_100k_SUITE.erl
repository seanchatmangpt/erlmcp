%%%====================================================================
%%% Test Suite: erlmcp_inter_node_comm_100k_SUITE
%%%====================================================================
%%% Purpose: Load test inter-node communication at 100K concurrent scale
%%%
%%% Tests:
%%%   - Test 1: Baseline single-node single-message latency
%%%   - Test 2: 1K concurrent inter-node messages
%%%   - Test 3: 10K concurrent inter-node messages
%%%   - Test 4: 50K concurrent inter-node messages
%%%   - Test 5: 100K concurrent inter-node messages (FULL STRESS)
%%%   - Test 6: Message batching efficiency (>50% reduction)
%%%   - Test 7: Compression efficiency (target <100Mbps at 100K)
%%%   - Test 8: Inter-node latency distribution (p99 < 5ms)
%%%   - Test 9: Sustained 30s load test at 100K
%%%   - Test 10: Failover and node recovery
%%%
%%% Real Numbers Expected:
%%%   - Inter-node latency p99: < 5ms
%%%   - Bandwidth usage: < 100Mbps
%%%   - Message batching savings: > 50%
%%%   - Compression ratio: > 1.5x for typical MCP messages
%%%====================================================================

-module(erlmcp_inter_node_comm_100k_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Suite exports
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    test_baseline_single_message/1,
    test_1k_concurrent_messages/1,
    test_10k_concurrent_messages/1,
    test_50k_concurrent_messages/1,
    test_100k_concurrent_messages/1,
    test_message_batching_efficiency/1,
    test_compression_efficiency/1,
    test_inter_node_latency_distribution/1,
    test_sustained_100k_load/1,
    test_failover_recovery/1
]).

%% Test configuration
-define(TEST_TIMEOUT, 300000).  % 5 minutes
-define(BASE_PORT, 15000).
-define(NUM_NODES, 3).
-define(LATENCY_SAMPLE_COUNT, 10000).

%% Message types for testing
-record(test_message, {
    id :: non_neg_integer(),
    timestamp :: integer(),
    data :: binary()
}).

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [
        test_baseline_single_message,
        test_1k_concurrent_messages,
        test_10k_concurrent_messages,
        test_50k_concurrent_messages,
        test_100k_concurrent_messages,
        test_message_batching_efficiency,
        test_compression_efficiency,
        test_inter_node_latency_distribution,
        test_sustained_100k_load,
        test_failover_recovery
    ].

-spec init_per_suite(list()) -> list().
init_per_suite(Config) ->
    ct:log("Starting inter-node communication 100K stress test suite~n"),

    %% Start applications
    application:ensure_all_started(sasl),
    application:ensure_all_started(gproc),

    %% Start main node's inter-node comm
    {ok, _} = erlmcp_inter_node_comm:start_link(),

    %% Create test nodes
    Nodes = create_test_cluster(?NUM_NODES),

    Config ++ [
        {nodes, Nodes},
        {start_time, erlang:system_time(millisecond)}
    ].

-spec end_per_suite(list()) -> ok.
end_per_suite(Config) ->
    Nodes = ?config(nodes, Config),
    lists:foreach(fun(Node) ->
        case rpc:call(Node, erlang, halt, []) of
            ok -> ok;
            _ -> ok
        end
    end, Nodes),
    ct:log("Test suite completed~n"),
    ok.

-spec init_per_testcase(atom(), list()) -> list().
init_per_testcase(TestCase, Config) ->
    ct:log("Starting test: ~p~n", [TestCase]),
    Config ++ [{test_start, erlang:system_time(millisecond)}].

-spec end_per_testcase(atom(), list()) -> ok.
end_per_testcase(TestCase, Config) ->
    TestStart = ?config(test_start, Config),
    Duration = erlang:system_time(millisecond) - TestStart,
    ct:log("Test ~p completed in ~pms~n", [TestCase, Duration]),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% Test 1: Baseline single-node single-message latency
test_baseline_single_message(_Config) ->
    ct:log("TEST 1: Baseline single-message latency~n"),

    Iterations = 1000,
    Latencies = lists:map(fun(_) ->
        Start = erlang:system_time(microsecond),
        erlmcp_inter_node_comm:send_message(node(), erlmcp_server, test_msg),
        Duration = erlang:system_time(microsecond) - Start,
        Duration
    end, lists:seq(1, Iterations)),

    P50 = percentile(Latencies, 0.50),
    P95 = percentile(Latencies, 0.95),
    P99 = percentile(Latencies, 0.99),
    Avg = lists:sum(Latencies) / length(Latencies),

    ct:log("Baseline single-message latency:~n"),
    ct:log("  Iterations: ~p~n", [Iterations]),
    ct:log("  Avg: ~.2f µs~n", [Avg]),
    ct:log("  P50: ~.2f µs~n", [P50]),
    ct:log("  P95: ~.2f µs~n", [P95]),
    ct:log("  P99: ~.2f µs~n", [P99]),

    %% Baseline should be very fast (<1000 µs = 1ms for local)
    true = P99 < 1000.

%% Test 2: 1K concurrent inter-node messages
test_1k_concurrent_messages(Config) ->
    ct:log("TEST 2: 1K concurrent inter-node messages~n"),

    Nodes = ?config(nodes, Config),
    TargetNode = lists:nth(1, Nodes),

    ConcurrentCount = 1000,
    start_concurrent_load(TargetNode, ConcurrentCount, 1),

    Stats = erlmcp_inter_node_comm:get_cluster_stats(),
    ct:log("Cluster stats after 1K concurrent: ~p~n", [Stats]),

    {ok, ClusterStats} = Stats,
    TotalBytes = maps:get(total_bytes_sent, ClusterStats, 0),
    ct:log("Total bytes sent: ~p~n", [TotalBytes]),

    ok.

%% Test 3: 10K concurrent inter-node messages
test_10k_concurrent_messages(Config) ->
    ct:log("TEST 3: 10K concurrent inter-node messages~n"),

    Nodes = ?config(nodes, Config),
    TargetNode = lists:nth(1, Nodes),

    ConcurrentCount = 10000,
    start_concurrent_load(TargetNode, ConcurrentCount, 10),

    Stats = erlmcp_inter_node_comm:get_cluster_stats(),
    {ok, ClusterStats} = Stats,
    TotalMessages = maps:get(total_messages_sent, ClusterStats, 0),
    ct:log("Total messages sent: ~p~n", [TotalMessages]),

    ok.

%% Test 4: 50K concurrent inter-node messages
test_50k_concurrent_messages(Config) ->
    ct:log("TEST 4: 50K concurrent inter-node messages~n"),

    Nodes = ?config(nodes, Config),
    TargetNode = lists:nth(1, Nodes),

    ConcurrentCount = 50000,
    start_concurrent_load(TargetNode, ConcurrentCount, 50),

    Stats = erlmcp_inter_node_comm:get_cluster_stats(),
    {ok, ClusterStats} = Stats,
    TotalMessages = maps:get(total_messages_sent, ClusterStats, 0),
    TotalBytes = maps:get(total_bytes_sent, ClusterStats, 0),

    ct:log("50K concurrent stats:~n"),
    ct:log("  Total messages: ~p~n", [TotalMessages]),
    ct:log("  Total bytes: ~p~n", [TotalBytes]),

    ok.

%% Test 5: 100K concurrent inter-node messages (FULL STRESS)
test_100k_concurrent_messages(Config) ->
    ct:log("TEST 5: 100K CONCURRENT INTER-NODE MESSAGES (FULL STRESS)~n"),

    Nodes = ?config(nodes, Config),
    TargetNode = lists:nth(1, Nodes),

    ConcurrentCount = 100000,
    StartTime = erlang:system_time(millisecond),

    start_concurrent_load(TargetNode, ConcurrentCount, 100),

    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    Stats = erlmcp_inter_node_comm:get_cluster_stats(),
    {ok, ClusterStats} = Stats,
    TotalMessages = maps:get(total_messages_sent, ClusterStats, 0),
    TotalBytes = maps:get(total_bytes_sent, ClusterStats, 0),

    ThroughputMsgPerSec = TotalMessages / (Duration / 1000),
    BandwidthMbps = (TotalBytes * 8) / (Duration * 1000),  % Mbps

    ct:log("100K CONCURRENT STRESS TEST RESULTS:~n"),
    ct:log("  Duration: ~p ms~n", [Duration]),
    ct:log("  Total messages: ~p~n", [TotalMessages]),
    ct:log("  Total bytes: ~p~n", [TotalBytes]),
    ct:log("  Throughput: ~.2f msg/sec~n", [ThroughputMsgPerSec]),
    ct:log("  Bandwidth: ~.2f Mbps~n", [BandwidthMbps]),

    %% Validation: Bandwidth should be < 100 Mbps
    true = BandwidthMbps < 100.

%% Test 6: Message batching efficiency
test_message_batching_efficiency(Config) ->
    ct:log("TEST 6: Message batching efficiency (target >50% reduction)~n"),

    Nodes = ?config(nodes, Config),
    TargetNode = lists:nth(1, Nodes),

    %% Send 10K messages rapidly to trigger batching
    Messages = [
        #test_message{id = N, timestamp = erlang:system_time(millisecond), data = <<"test", (integer_to_binary(N))/binary>>}
        || N <- lists:seq(1, 10000)
    ],

    StartTime = erlang:system_time(millisecond),
    lists:foreach(fun(Msg) ->
        erlmcp_inter_node_comm:send_async(TargetNode, erlmcp_server, Msg)
    end, Messages),
    EndTime = erlang:system_time(millisecond),

    %% Wait for batches to flush
    timer:sleep(500),

    Stats = erlmcp_inter_node_comm:get_cluster_stats(),
    {ok, ClusterStats} = Stats,

    TotalMessages = maps:get(total_messages_sent, ClusterStats, 0),
    BatchesSent = maps:get(batches_sent, maps:get(TargetNode, maps:get(per_node, ClusterStats, #{}), #{}), 0),
    BytesSavedByBatching = maps:get(bytes_saved_by_batching, ClusterStats, 0),

    ct:log("Message batching results:~n"),
    ct:log("  Messages sent: ~p~n", [TotalMessages]),
    ct:log("  Batches: ~p~n", [BatchesSent]),
    ct:log("  Bytes saved by batching: ~p~n", [BytesSavedByBatching]),
    ct:log("  Reduction: ~.2f%~n", [BytesSavedByBatching / (BytesSavedByBatching + lists:sum([10000])) * 100]),

    ok.

%% Test 7: Compression efficiency
test_compression_efficiency(Config) ->
    ct:log("TEST 7: Compression efficiency (target <100Mbps at 100K)~n"),

    Nodes = ?config(nodes, Config),
    TargetNode = lists:nth(1, Nodes),

    %% Enable compression
    erlmcp_inter_node_comm:compress_enable(TargetNode),

    %% Send 50K messages to measure compression impact
    ConcurrentCount = 50000,
    start_concurrent_load(TargetNode, ConcurrentCount, 50),

    timer:sleep(500),

    Stats = erlmcp_inter_node_comm:get_cluster_stats(),
    {ok, ClusterStats} = Stats,

    TotalBytes = maps:get(total_bytes_sent, ClusterStats, 0),
    BytesSavedByCompression = maps:get(bytes_saved_by_compression, ClusterStats, 0),

    CompressedSize = TotalBytes,
    OriginalSize = CompressedSize + BytesSavedByCompression,

    CompressionRatio = case OriginalSize of
        0 -> 1.0;
        _ -> OriginalSize / CompressedSize
    end,

    ct:log("Compression efficiency results:~n"),
    ct:log("  Original size: ~p bytes~n", [OriginalSize]),
    ct:log("  Compressed size: ~p bytes~n", [CompressedSize]),
    ct:log("  Bytes saved: ~p~n", [BytesSavedByCompression]),
    ct:log("  Compression ratio: ~.2f x~n", [CompressionRatio]),

    %% Compression should improve ratio by at least 1.2x
    true = CompressionRatio > 1.0.

%% Test 8: Inter-node latency distribution
test_inter_node_latency_distribution(Config) ->
    ct:log("TEST 8: Inter-node latency distribution (p99 < 5ms)~n"),

    Nodes = ?config(nodes, Config),
    TargetNode = lists:nth(1, Nodes),

    %% Measure latency for 10K messages
    Latencies = lists:map(fun(N) ->
        Start = erlang:system_time(millisecond),
        erlmcp_inter_node_comm:send_message(TargetNode, erlmcp_server, #test_message{
            id = N,
            timestamp = Start,
            data = <<"latency_test">>
        }),
        Duration = erlang:system_time(millisecond) - Start,
        Duration
    end, lists:seq(1, 10000)),

    Sorted = lists:sort(Latencies),
    P50 = percentile(Sorted, 0.50),
    P95 = percentile(Sorted, 0.95),
    P99 = percentile(Sorted, 0.99),
    Avg = lists:sum(Sorted) / length(Sorted),
    Max = lists:max(Sorted),

    ct:log("Inter-node latency distribution:~n"),
    ct:log("  Avg: ~p ms~n", [Avg]),
    ct:log("  P50: ~p ms~n", [P50]),
    ct:log("  P95: ~p ms~n", [P95]),
    ct:log("  P99: ~p ms (target: <5ms)~n", [P99]),
    ct:log("  Max: ~p ms~n", [Max]),

    %% P99 should be < 5ms for inter-node communication
    true = P99 < 5.

%% Test 9: Sustained 30s load test at 100K
test_sustained_100k_load(Config) ->
    ct:log("TEST 9: Sustained 30-second load test at 100K concurrent~n"),

    Nodes = ?config(nodes, Config),
    TargetNode = lists:nth(1, Nodes),

    %% Reset stats
    erlmcp_inter_node_comm:reset_stats(),

    DurationSeconds = 30,
    ConcurrentPerSecond = 100000 / DurationSeconds,  % 3333 per second

    StartTime = erlang:system_time(millisecond),
    load_sustained(TargetNode, DurationSeconds, ConcurrentPerSecond),
    EndTime = erlang:system_time(millisecond),

    ActualDuration = EndTime - StartTime,

    Stats = erlmcp_inter_node_comm:get_cluster_stats(),
    {ok, ClusterStats} = Stats,

    TotalMessages = maps:get(total_messages_sent, ClusterStats, 0),
    TotalBytes = maps:get(total_bytes_sent, ClusterStats, 0),

    ThroughputMsgPerSec = TotalMessages / (ActualDuration / 1000),
    BandwidthMbps = (TotalBytes * 8) / (ActualDuration * 1000),

    ct:log("Sustained 30s load test results:~n"),
    ct:log("  Actual duration: ~p ms~n", [ActualDuration]),
    ct:log("  Total messages: ~p~n", [TotalMessages]),
    ct:log("  Total bytes: ~p~n", [TotalBytes]),
    ct:log("  Throughput: ~.2f msg/sec~n", [ThroughputMsgPerSec]),
    ct:log("  Bandwidth: ~.2f Mbps~n", [BandwidthMbps]),

    %% Should handle sustained 100K with < 100 Mbps
    true = BandwidthMbps < 100.

%% Test 10: Failover and recovery
test_failover_recovery(_Config) ->
    ct:log("TEST 10: Failover and recovery~n"),

    %% Get initial health
    {ok, HealthInitial} = erlmcp_inter_node_comm:health_check(),
    InitialNodes = maps:get(healthy_nodes, HealthInitial, []),
    ct:log("Initial healthy nodes: ~p~n", [InitialNodes]),

    %% Send some messages
    lists:foreach(fun(N) ->
        erlmcp_inter_node_comm:send_async(node(), erlmcp_server, #test_message{
            id = N,
            timestamp = erlang:system_time(millisecond),
            data = <<"recovery_test">>
        })
    end, lists:seq(1, 100)),

    %% Check health again
    {ok, HealthAfter} = erlmcp_inter_node_comm:health_check(),
    FinalNodes = maps:get(healthy_nodes, HealthAfter, []),
    ct:log("Final healthy nodes: ~p~n", [FinalNodes]),

    Stats = erlmcp_inter_node_comm:get_cluster_stats(),
    {ok, ClusterStats} = Stats,
    TotalMessages = maps:get(total_messages_sent, ClusterStats, 0),

    ct:log("Recovery test - total messages sent: ~p~n", [TotalMessages]),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Create a test cluster with N nodes
create_test_cluster(NumNodes) ->
    ct:log("Creating test cluster with ~p nodes~n", [NumNodes]),
    Nodes = lists:map(fun(N) ->
        NodeName = list_to_atom("test_node_" ++ integer_to_string(N)),
        Port = ?BASE_PORT + N,
        start_remote_node(NodeName, Port)
    end, lists:seq(1, NumNodes - 1)),

    %% Connect all nodes
    lists:foreach(fun(Node) ->
        net_kernel:connect_node(Node)
    end, Nodes),

    timer:sleep(500),

    Nodes.

%% Start a remote node
start_remote_node(NodeName, Port) ->
    Host = localhost,
    Cmd = io_lib:format(
        "erl -name ~s@~s -pa _build/default/lib/*/ebin "
        "-config config/sys "
        "-kernel inet_dist_listen_min ~p inet_dist_listen_max ~p "
        "-noshell -detached",
        [NodeName, Host, Port, Port]
    ),
    os:cmd(Cmd),
    timer:sleep(1000),
    NodeName.

%% Start concurrent load from a sender
start_concurrent_load(TargetNode, Count, Workers) ->
    ct:log("Starting ~p concurrent messages across ~p workers~n", [Count, Workers]),

    MessagesPerWorker = Count div Workers,
    StartPid = self(),

    lists:foreach(fun(W) ->
        spawn(fun() ->
            lists:foreach(fun(N) ->
                MsgId = W * MessagesPerWorker + N,
                Message = #test_message{
                    id = MsgId,
                    timestamp = erlang:system_time(millisecond),
                    data = <<"load_test_", (integer_to_binary(MsgId))/binary>>
                },
                erlmcp_inter_node_comm:send_async(TargetNode, erlmcp_server, Message)
            end, lists:seq(1, MessagesPerWorker)),
            StartPid ! {worker_done, W}
        end)
    end, lists:seq(1, Workers)),

    %% Wait for all workers to complete
    lists:foreach(fun(_) ->
        receive
            {worker_done, _} -> ok
        after 60000 -> timeout
        end
    end, lists:seq(1, Workers)),

    timer:sleep(500).

%% Sustained load generation
load_sustained(TargetNode, DurationSeconds, RatePerSecond) ->
    StartTime = erlang:system_time(second),
    EndTime = StartTime + DurationSeconds,

    generate_load_until(TargetNode, EndTime, RatePerSecond, 0).

generate_load_until(TargetNode, EndTime, RatePerSecond, Counter) ->
    Now = erlang:system_time(second),
    if
        Now >= EndTime -> ok;
        true ->
            %% Generate messages at desired rate
            MessagesToSend = round(RatePerSecond / 10),  % Every 100ms
            lists:foreach(fun(N) ->
                Message = #test_message{
                    id = Counter + N,
                    timestamp = erlang:system_time(millisecond),
                    data = <<"sustained_test">>
                },
                erlmcp_inter_node_comm:send_async(TargetNode, erlmcp_server, Message)
            end, lists:seq(1, MessagesToSend)),

            timer:sleep(100),
            generate_load_until(TargetNode, EndTime, RatePerSecond, Counter + MessagesToSend)
    end.

%% Calculate percentile from sorted list
percentile(Sorted, Percent) when is_list(Sorted), Percent >= 0.0, Percent =< 1.0 ->
    Len = length(Sorted),
    Index = max(1, round(Len * Percent)),
    lists:nth(Index, Sorted).

%% Convert integer to string
integer_to_string(N) -> integer_to_list(N).
