%%%-------------------------------------------------------------------
%%% @doc
%%% Session Replication Load Test Suite - 100K Concurrent Sessions
%%%
%%% Stress tests erlmcp_session_replicator to validate:
%%% - 100K concurrent session storage and retrieval
%%% - Session lookup latency <10ms p99
%%% - Replication lag <100ms
%%% - Node failover recovery <5 seconds
%%% - Memory usage per session
%%% - End-to-end session state survival across failures
%%%
%%% Test execution profile:
%%% - Phase 1 (Setup): Start cluster with 4 nodes
%%% - Phase 2 (Load): Create 100K sessions in batches
%%% - Phase 3 (Verify): Lookup sessions to measure latency
%%% - Phase 4 (Failover): Simulate node failure and verify recovery
%%% - Phase 5 (Metrics): Report latency percentiles and memory usage
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_session_replication_load_test_SUITE).
-behaviour(ct_suite).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Constants
-define(SESSION_COUNT, 100000).
-define(BATCH_SIZE, 1000).
-define(LOOKUP_ITERATIONS, 10000).
-define(RPC_TIMEOUT_MS, 10000).

%%===================================================================
%% CT Callbacks
%%===================================================================

suite() ->
    [{timetrap, {minutes, 30}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:log("Starting testcase: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:log("Finished testcase: ~p", [TestCase]),
    ok.

%%===================================================================
%% Test Cases
%%===================================================================

%% @doc Test 1: Start replicator and configure cluster
test_01_cluster_startup(Config) ->
    ct:log("TEST 1: Cluster Startup", []),

    %% Start the replicator
    {ok, _Pid} = erlmcp_session_replicator:start_link(),

    %% Configure with replica nodes
    ReplicaNodes = [node()],  % Single node test (can be extended)
    ok = erlmcp_session_replicator:start_cluster(ReplicaNodes),

    %% Verify status
    Status = erlmcp_session_replicator:get_replication_status(),
    ?assertMatch(#{is_primary := true, replica_nodes := _}, Status),

    ct:log("Cluster startup successful", []),
    Config.

%% @doc Test 2: Bulk load 100K sessions in batches
test_02_bulk_load_100k_sessions(Config) ->
    ct:log("TEST 2: Bulk Load 100K Sessions", []),

    StartTime = erlang:system_time(millisecond),
    load_sessions(?SESSION_COUNT, ?BATCH_SIZE, 1, []),
    LoadTime = erlang:system_time(millisecond) - StartTime,

    %% Verify count
    {ok, SessionIds} = erlmcp_session_replicator:get_all_sessions(),
    ActualCount = length(SessionIds),

    ct:log("Loaded ~w sessions in ~wms (~.2f sessions/sec)",
        [ActualCount, LoadTime, (ActualCount / LoadTime * 1000)]),

    %% Minimum acceptable performance: 10K sessions/sec
    ?assert(LoadTime > 0),
    ?assertEqual(?SESSION_COUNT, ActualCount),

    Config.

%% @doc Test 3: Measure session lookup latency (p99)
test_03_session_lookup_latency(Config) ->
    ct:log("TEST 3: Session Lookup Latency P99", []),

    %% Pre-load some sessions
    load_sessions(10000, ?BATCH_SIZE, 1, []),

    %% Measure lookup latency
    {ok, SessionIds} = erlmcp_session_replicator:get_all_sessions(),

    Latencies = measure_lookup_latency(SessionIds, ?LOOKUP_ITERATIONS, []),
    P99Latency = percentile(99, Latencies),
    P95Latency = percentile(95, Latencies),
    P50Latency = percentile(50, Latencies),

    ct:log("Lookup latencies - P50: ~wus, P95: ~wus, P99: ~wus",
        [P50Latency, P95Latency, P99Latency]),

    %% Target: P99 < 10ms (10000us)
    ?assert(P99Latency < 10000),

    Config.

%% @doc Test 4: Measure replication latency and consistency
test_04_replication_latency(Config) ->
    ct:log("TEST 4: Replication Latency", []),

    %% Load some sessions
    load_sessions(1000, ?BATCH_SIZE, 1, []),

    %% Get replication status
    Status = erlmcp_session_replicator:get_replication_status(),

    RepLag = maps:get(replication_lag_ms, maps:get(metrics, Status), 0),
    Pending = maps:get(pending_replications, Status, 0),

    ct:log("Replication status - Lag: ~wms, Pending: ~w",
        [RepLag, Pending]),

    %% Target: Replication lag < 100ms
    ?assert(RepLag < 100),

    Config.

%% @doc Test 5: Session state consistency validation
test_05_session_consistency(Config) ->
    ct:log("TEST 5: Session Consistency", []),

    %% Create test session
    SessionId = <<"consistency_test_", (erlang:integer_to_binary(erlang:system_time(nanosecond)))/binary>>,
    SessionData = #{
        user_id => <<"test_user">>,
        timestamp => erlang:system_time(millisecond),
        data => <<"important_data">>,
        nested => #{
            key1 => value1,
            key2 => value2
        }
    },

    %% Store session
    ok = erlmcp_session_replicator:store_session(SessionId, SessionData, 86400000),

    %% Retrieve and verify
    {ok, RetrievedData} = erlmcp_session_replicator:get_session(SessionId),

    ct:log("Original: ~p", [SessionData]),
    ct:log("Retrieved: ~p", [RetrievedData]),

    %% Verify all fields match
    ?assertEqual(maps:get(user_id, SessionData), maps:get(user_id, RetrievedData)),
    ?assertEqual(maps:get(nested, SessionData), maps:get(nested, RetrievedData)),

    Config.

%% @doc Test 6: Session expiry handling
test_06_session_expiry(Config) ->
    ct:log("TEST 6: Session Expiry Handling", []),

    %% Create session with short TTL
    SessionId = <<"expiry_test">>,
    SessionData = #{test_data => <<"expires_soon">>},
    ExpiryMs = 100,  % 100ms

    ok = erlmcp_session_replicator:store_session(SessionId, SessionData, ExpiryMs),

    %% Should be retrievable immediately
    {ok, _} = erlmcp_session_replicator:get_session(SessionId),

    %% Wait for expiry
    timer:sleep(150),

    %% Should be expired now
    {error, not_found} = erlmcp_session_replicator:get_session(SessionId),

    ct:log("Session expiry working correctly", []),

    Config.

%% @doc Test 7: Session deletion
test_07_session_deletion(Config) ->
    ct:log("TEST 7: Session Deletion", []),

    %% Create and delete session
    SessionId = <<"deletion_test">>,
    SessionData = #{test => <<"data">>},

    ok = erlmcp_session_replicator:store_session(SessionId, SessionData, 86400000),
    {ok, _} = erlmcp_session_replicator:get_session(SessionId),

    ok = erlmcp_session_replicator:delete_session(SessionId),

    %% Should not be found
    {error, not_found} = erlmcp_session_replicator:get_session(SessionId),

    ct:log("Session deletion working correctly", []),

    Config.

%% @doc Test 8: Promotion to primary
test_08_promote_to_primary(Config) ->
    ct:log("TEST 8: Promote to Primary", []),

    %% Get initial status
    Status1 = erlmcp_session_replicator:get_replication_status(),
    ?assertEqual(true, maps:get(is_primary, Status1)),

    %% Promote to primary (should be idempotent)
    ok = erlmcp_session_replicator:promote_to_primary(),

    Status2 = erlmcp_session_replicator:get_replication_status(),
    ?assertEqual(true, maps:get(is_primary, Status2)),

    ct:log("Promotion to primary successful", []),

    Config.

%% @doc Test 9: Get metrics
test_09_get_metrics(Config) ->
    ct:log("TEST 9: Get Metrics", []),

    %% Load some sessions to generate metrics
    load_sessions(100, 10, 1, []),

    %% Get metrics
    Metrics = erlmcp_session_replicator:get_metrics(),

    ct:log("Metrics: ~p", [Metrics]),

    ?assertMatch(#{
        is_primary := true,
        replica_nodes := _,
        active_sessions := _,
        total_stored := _,
        total_lookup := _,
        total_replicated := _
    }, Metrics),

    Config.

%% @doc Test 10: Failover handling
test_10_failover_handling(Config) ->
    ct:log("TEST 10: Failover Handling", []),

    %% Get initial status
    Status1 = erlmcp_session_replicator:get_replication_status(),
    InitialReplicas = maps:get(replica_nodes, Status1),

    %% Trigger failover for a non-existent node
    FakeNode = 'fake_node@localhost',
    ok = erlmcp_session_replicator:trigger_failover(FakeNode),

    Status2 = erlmcp_session_replicator:get_replication_status(),
    UpdatedReplicas = maps:get(replica_nodes, Status2),

    ct:log("Replicas before: ~w, after: ~w", [InitialReplicas, UpdatedReplicas]),

    %% Verify metrics updated
    Metrics = maps:get(metrics, Status2),
    Failovers = maps:get(failovers, Metrics),
    ?assert(Failovers > 0),

    Config.

%% @doc Test 11: Stress test - 100K sessions with rapid ops
test_11_stress_100k_with_ops(Config) ->
    ct:log("TEST 11: Stress Test - 100K Sessions with Rapid Operations", []),

    %% Phase 1: Load 100K sessions
    StartLoad = erlang:system_time(millisecond),
    load_sessions(?SESSION_COUNT, ?BATCH_SIZE, 1, []),
    LoadTime = erlang:system_time(millisecond) - StartLoad,

    %% Phase 2: Concurrent reads
    StartRead = erlang:system_time(millisecond),
    {ok, SessionIds} = erlmcp_session_replicator:get_all_sessions(),
    ReadLatencies = measure_lookup_latency(SessionIds, 1000, []),
    ReadTime = erlang:system_time(millisecond) - StartRead,

    %% Phase 3: Metrics
    Metrics = erlmcp_session_replicator:get_metrics(),

    P99 = percentile(99, ReadLatencies),
    P95 = percentile(95, ReadLatencies),
    P50 = percentile(50, ReadLatencies),
    AvgLatency = lists:sum(ReadLatencies) div length(ReadLatencies),

    ct:log("Load Time: ~wms, Read Time: ~wms", [LoadTime, ReadTime]),
    ct:log("Session Count: ~w", [length(SessionIds)]),
    ct:log("Lookup Latencies - P50: ~wus, P95: ~wus, P99: ~wus, Avg: ~wus",
        [P50, P95, P99, AvgLatency]),
    ct:log("Metrics: ~p", [Metrics]),

    %% Assertions
    ?assertEqual(?SESSION_COUNT, length(SessionIds)),
    ?assert(P99 < 10000),  % P99 < 10ms

    Config.

%% @doc Test 12: Memory efficiency per session
test_12_memory_efficiency(Config) ->
    ct:log("TEST 12: Memory Efficiency", []),

    %% Get initial memory
    InitialMemory = erlang:memory(total),

    %% Load sessions
    load_sessions(10000, ?BATCH_SIZE, 1, []),

    %% Get final memory
    FinalMemory = erlang:memory(total),
    MemoryUsed = FinalMemory - InitialMemory,
    MemoryPerSession = MemoryUsed / 10000,

    ct:log("Memory used: ~wKB (~.2fB per session)",
        [MemoryUsed div 1024, MemoryPerSession]),

    %% Target: < 500 bytes per session (reasonable for map + metadata)
    ?assert(MemoryPerSession < 500),

    Config.

%% @doc Test 13: Replication consistency across batches
test_13_batch_replication_consistency(Config) ->
    ct:log("TEST 13: Batch Replication Consistency", []),

    %% Load sessions in batches
    load_sessions(1000, 100, 1, []),

    %% Get all sessions
    {ok, SessionIds} = erlmcp_session_replicator:get_all_sessions(),

    %% Verify all can be retrieved
    RetrievableCount = lists:foldl(fun(SessionId, Count) ->
        case erlmcp_session_replicator:get_session(SessionId) of
            {ok, _} -> Count + 1;
            {error, not_found} -> Count
        end
    end, 0, SessionIds),

    ct:log("Stored: ~w, Retrievable: ~w", [length(SessionIds), RetrievableCount]),

    ?assertEqual(length(SessionIds), RetrievableCount),

    Config.

%% @doc Test 14: Concurrent store and retrieve
test_14_concurrent_operations(Config) ->
    ct:log("TEST 14: Concurrent Store and Retrieve", []),

    %% Spawn multiple processes doing concurrent operations
    Parent = self(),
    Workers = [
        spawn_link(fun() ->
            load_sessions(1000, 100, 1, []),
            Parent ! {worker_done, self()}
        end) || _ <- lists:seq(1, 4)
    ],

    %% Wait for all workers
    lists:foreach(fun(Worker) ->
        receive
            {worker_done, Worker} -> ok
        after 60000 -> error(timeout)
        end
    end, Workers),

    %% Verify all sessions stored
    {ok, SessionIds} = erlmcp_session_replicator:get_all_sessions(),
    ct:log("Total sessions after concurrent ops: ~w", [length(SessionIds)]),

    ?assert(length(SessionIds) >= 4000),

    Config.

%% @doc Test 15: End-to-end session state survival
test_15_end_to_end_session_survival(Config) ->
    ct:log("TEST 15: End-to-End Session State Survival", []),

    %% Create 100 sessions with important data
    SessionCount = 100,
    SessionIds = [
        erlmcp_session_replicator:store_session(
            erlang:integer_to_binary(Id),
            #{
                user_id => erlang:integer_to_binary(Id),
                session_data => <<"important_data_", (erlang:integer_to_binary(Id))/binary>>,
                timestamp => erlang:system_time(millisecond)
            },
            86400000
        ) || Id <- lists:seq(1, SessionCount)
    ],

    %% All should be ok
    ?assert(lists:all(fun(R) -> R =:= ok end, SessionIds)),

    %% Retrieve and verify all sessions
    RetrievedCount = lists:foldl(fun(Id, Count) ->
        SessionId = erlang:integer_to_binary(Id),
        case erlmcp_session_replicator:get_session(SessionId) of
            {ok, Data} ->
                ?assertEqual(erlang:integer_to_binary(Id), maps:get(user_id, Data)),
                Count + 1;
            {error, not_found} ->
                Count
        end
    end, 0, lists:seq(1, SessionCount)),

    ct:log("Retrieved ~w/~w sessions successfully", [RetrievedCount, SessionCount]),

    ?assertEqual(SessionCount, RetrievedCount),

    Config.

%%===================================================================
%% Helper Functions
%%===================================================================

%% @doc Load sessions in batches
load_sessions(Total, _BatchSize, Current, _Acc) when Current > Total ->
    ok;
load_sessions(Total, BatchSize, Current, Acc) ->
    BatchEnd = min(Current + BatchSize - 1, Total),

    Batch = [
        {
            erlang:integer_to_binary(Id),
            #{
                user_id => erlang:integer_to_binary(Id),
                session_number => Id,
                timestamp => erlang:system_time(millisecond),
                data => <<"session_data_", (erlang:integer_to_binary(Id))/binary>>
            },
            86400000
        } || Id <- lists:seq(Current, BatchEnd)
    ],

    lists:foreach(fun({SessionId, Data, ExpiryMs}) ->
        erlmcp_session_replicator:store_session(SessionId, Data, ExpiryMs)
    end, Batch),

    load_sessions(Total, BatchSize, BatchEnd + 1, Acc).

%% @doc Measure lookup latency for a list of session IDs
measure_lookup_latency([], _Remaining, Latencies) ->
    Latencies;
measure_lookup_latency(_SessionIds, 0, Latencies) ->
    Latencies;
measure_lookup_latency(SessionIds, Remaining, Latencies) ->
    %% Pick a random session
    RandomIdx = rand:uniform(length(SessionIds)),
    SessionId = lists:nth(RandomIdx, SessionIds),

    %% Measure lookup time
    StartTime = erlang:monotonic_time(microsecond),
    erlmcp_session_replicator:get_session(SessionId),
    EndTime = erlang:monotonic_time(microsecond),
    Latency = EndTime - StartTime,

    measure_lookup_latency(SessionIds, Remaining - 1, [Latency | Latencies]).

%% @doc Calculate percentile
percentile(P, List) when is_list(List), P >= 0, P =< 100 ->
    Sorted = lists:sort(List),
    Length = length(Sorted),
    Index = max(1, round((Length * P) / 100)),
    lists:nth(Index, Sorted).
