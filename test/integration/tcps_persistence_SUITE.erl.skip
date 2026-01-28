%%%-------------------------------------------------------------------
%%% @doc TCPS Persistence Test Suite
%%%
%%% Comprehensive testing of persistence layer including:
%%% - Receipt storage and retrieval
%%% - RDF ontology persistence
%%% - Backup and restore
%%% - Data integrity
%%% - Cross-session persistence
%%% - Corruption recovery
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_persistence_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

%% CT callbacks
-export([all/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Test cases
-export([
    test_receipt_roundtrip/1,
    test_rdf_persistence/1,
    test_backup_restore/1,
    test_work_order_persistence/1,
    test_andon_persistence/1,
    test_metrics_persistence/1,
    test_corruption_recovery/1,
    test_concurrent_writes/1,
    test_large_dataset/1,
    test_query_performance/1,
    test_incremental_backup/1,
    test_cross_session_persistence/1,
    test_ontology_versioning/1,
    test_receipt_chain_integrity/1,
    test_disaster_recovery/1
]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

suite() ->
    [{timetrap, {minutes, 10}}].

all() ->
    [
        test_receipt_roundtrip,
        test_rdf_persistence,
        test_backup_restore,
        test_work_order_persistence,
        test_andon_persistence,
        test_metrics_persistence,
        test_corruption_recovery,
        test_concurrent_writes,
        test_large_dataset,
        test_query_performance,
        test_incremental_backup,
        test_cross_session_persistence,
        test_ontology_versioning,
        test_receipt_chain_integrity,
        test_disaster_recovery
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlmcp),
    ok = tcps_test_utils:init_test_env(),
    Config.

end_per_suite(_Config) ->
    ok = tcps_test_utils:cleanup_test_env(),
    ok = application:stop(tcps),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("~n=== Starting test case: ~p ===~n", [TestCase]),
    ok = tcps_test_utils:clear_all_data(),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("~n=== Completed test case: ~p ===~n", [TestCase]),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test receipt roundtrip (store and load)
%% @end
%%--------------------------------------------------------------------
test_receipt_roundtrip(_Config) ->
    ct:pal("~n=== Testing Receipt Roundtrip ===~n"),

    %% Create receipt
    Receipt = #{
        id => <<"receipt-123">>,
        work_order_id => <<"wo-456">>,
        stage => test,
        result => pass,
        metadata => #{
            tests_run => 150,
            coverage => 85.5,
            duration_ms => 5000
        },
        timestamp => erlang:system_time(millisecond),
        signature => <<"sig-abc123">>
    },

    ct:pal("Created receipt: ~p~n", [Receipt]),

    %% Store as JSON
    {ok, JsonPath} = tcps_persistence:store_receipt_json(Receipt),
    ?assert(filelib:is_file(JsonPath)),
    ct:pal("Stored JSON: ~s~n", [JsonPath]),

    %% Persist to RDF ontology
    ok = tcps_persistence:persist_to_ontology(Receipt),
    ct:pal("Persisted to ontology~n"),

    %% Load from JSON
    {ok, LoadedFromJson} = tcps_persistence:load_receipt_json(JsonPath),
    ?assertEqual(Receipt, LoadedFromJson),
    ct:pal("Loaded from JSON successfully~n"),

    %% Query from ontology
    [QueriedReceipt] = tcps_persistence:query_receipts_by_id(
        maps:get(id, Receipt)
    ),
    ?assertEqual(Receipt, QueriedReceipt),
    ct:pal("Queried from ontology successfully~n"),

    ct:pal("~n=== Receipt Roundtrip: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test RDF ontology persistence
%% @end
%%--------------------------------------------------------------------
test_rdf_persistence(_Config) ->
    ct:pal("~n=== Testing RDF Persistence ===~n"),

    %% Create work order
    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),

    %% Process pipeline (generates receipts)
    ok = tcps_test_utils:process_work_order_full(WorkOrderId),

    %% Query ontology
    {ok, WOData} = tcps_persistence:query_ontology(#{
        subject => WorkOrderId,
        predicate => <<"rdf:type">>,
        object => <<"tcps:WorkOrder">>
    }),

    ?assert(maps:is_key(status, WOData)),
    ?assert(maps:is_key(bucket, WOData)),
    ct:pal("Work order persisted in ontology~n"),

    %% Query receipts
    Receipts = tcps_persistence:query_receipts_by_work_order(WorkOrderId),
    ?assert(length(Receipts) >= 7),
    ct:pal("Retrieved ~p receipts from ontology~n", [length(Receipts)]),

    %% Verify relationships
    lists:foreach(fun(Receipt) ->
        ?assertEqual(WorkOrderId, maps:get(work_order_id, Receipt))
    end, Receipts),

    ct:pal("~n=== RDF Persistence: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test backup and restore
%% @end
%%--------------------------------------------------------------------
test_backup_restore(_Config) ->
    ct:pal("~n=== Testing Backup and Restore ===~n"),

    %% Generate test data
    WorkOrders = lists:map(fun(_) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        ok = tcps_test_utils:process_work_order_full(WO),
        WO
    end, lists:seq(1, 10)),

    ct:pal("Created 10 work orders with receipts~n"),

    %% Backup
    BackupFile = "/tmp/tcps_backup_test.tar.gz",
    ok = tcps_persistence:backup_all(BackupFile),
    ?assert(filelib:is_file(BackupFile)),

    {ok, FileInfo} = file:read_file_info(BackupFile),
    BackupSize = FileInfo#file_info.size,
    ct:pal("Backup created: ~s (~p bytes)~n", [BackupFile, BackupSize]),

    %% Clear all data
    ok = tcps_test_utils:clear_all_data(),
    ?assertEqual([], tcps_work_order:list_all()),
    ct:pal("All data cleared~n"),

    %% Restore
    ok = tcps_persistence:restore_from_backup(BackupFile),
    ct:pal("Restored from backup~n"),

    %% Verify data restored
    RestoredWOs = tcps_work_order:list_all(),
    ?assertEqual(10, length(RestoredWOs)),
    ct:pal("Restored 10 work orders~n"),

    %% Verify receipts restored
    lists:foreach(fun(WO) ->
        {ok, Receipts} = tcps_persistence:get_receipts_by_work_order(WO),
        ?assert(length(Receipts) >= 7)
    end, WorkOrders),

    ct:pal("All receipts restored~n"),

    %% Cleanup
    file:delete(BackupFile),

    ct:pal("~n=== Backup and Restore: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test work order persistence
%% @end
%%--------------------------------------------------------------------
test_work_order_persistence(_Config) ->
    ct:pal("~n=== Testing Work Order Persistence ===~n"),

    %% Create work order with complex data
    {ok, WorkOrderId} = tcps_work_order:create(#{
        title => <<"Test feature implementation">>,
        bucket => reliability,
        priority => high,
        estimated_effort => 8,
        labels => [<<"feature">>, <<"high-priority">>],
        metadata => #{
            github_issue => 123,
            assignee => <<"developer@example.com">>,
            sprint => <<"2024-Q1-Sprint-1">>
        }
    }),

    ct:pal("Created work order with metadata~n"),

    %% Update status
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Reload
    {ok, Reloaded} = tcps_work_order:get(WorkOrderId),

    %% Verify all fields
    ?assertEqual(<<"Test feature implementation">>, maps:get(title, Reloaded)),
    ?assertEqual(reliability, maps:get(bucket, Reloaded)),
    ?assertEqual(high, maps:get(priority, Reloaded)),
    ?assertEqual(in_progress, maps:get(status, Reloaded)),

    Metadata = maps:get(metadata, Reloaded),
    ?assertEqual(123, maps:get(github_issue, Metadata)),

    ct:pal("All work order fields persisted correctly~n"),

    ct:pal("~n=== Work Order Persistence: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test Andon persistence
%% @end
%%--------------------------------------------------------------------
test_andon_persistence(_Config) ->
    ct:pal("~n=== Testing Andon Persistence ===~n"),

    %% Create and trigger Andon
    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{}),
    {ok, AndonId} = tcps_test_utils:wait_for_andon(WorkOrderId),

    %% Add 5 Whys analysis
    {ok, _AnalysisId} = tcps_andon:add_5_whys_analysis(AndonId, #{
        root_cause => <<"Authentication logic error">>,
        corrective_action => <<"Fix token validation">>,
        preventive_action => <<"Add integration tests">>
    }),

    ct:pal("Andon with analysis created~n"),

    %% Reload Andon
    {ok, ReloadedAndon} = tcps_andon:get(AndonId),

    %% Verify all fields
    ?assertEqual(WorkOrderId, maps:get(work_order_id, ReloadedAndon)),
    ?assertEqual(test_failure, maps:get(type, ReloadedAndon)),
    ?assert(maps:is_key(analysis, ReloadedAndon)),

    Analysis = maps:get(analysis, ReloadedAndon),
    ?assertEqual(<<"Authentication logic error">>, maps:get(root_cause, Analysis)),

    ct:pal("Andon persisted with analysis~n"),

    %% Resolve
    ok = tcps_andon:resolve(AndonId, #{resolved_by => <<"dev@example.com">>}),

    %% Reload
    {ok, ResolvedAndon} = tcps_andon:get(AndonId),
    ?assertEqual(resolved, maps:get(status, ResolvedAndon)),

    ct:pal("Andon resolution persisted~n"),

    ct:pal("~n=== Andon Persistence: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test metrics persistence
%% @end
%%--------------------------------------------------------------------
test_metrics_persistence(_Config) ->
    ct:pal("~n=== Testing Metrics Persistence ===~n"),

    %% Generate metrics
    lists:foreach(fun(_) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        ok = tcps_test_utils:process_work_order_full(WO)
    end, lists:seq(1, 10)),

    %% Update Kaizen metrics
    ok = tcps_kaizen:update_metrics(),

    %% Get metrics
    Metrics1 = tcps_kaizen:get_metrics(),
    ?assertEqual(10, maps:get(work_orders_completed, Metrics1)),
    ct:pal("Metrics generated: ~p completed~n", [10]),

    %% Persist metrics
    ok = tcps_persistence:persist_metrics(Metrics1),

    %% Clear in-memory metrics
    ok = tcps_kaizen:reset_metrics(),

    %% Reload metrics
    {ok, Metrics2} = tcps_persistence:load_metrics(),
    ?assertEqual(10, maps:get(work_orders_completed, Metrics2)),

    ct:pal("Metrics persisted and restored~n"),

    ct:pal("~n=== Metrics Persistence: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test corruption recovery
%% @end
%%--------------------------------------------------------------------
test_corruption_recovery(_Config) ->
    ct:pal("~n=== Testing Corruption Recovery ===~n"),

    %% Create valid data
    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_test_utils:process_work_order_full(WorkOrderId),

    %% Get receipt file path
    {ok, Receipts} = tcps_persistence:get_receipts_by_work_order(WorkOrderId),
    [FirstReceipt | _] = Receipts,
    ReceiptPath = tcps_persistence:get_receipt_path(maps:get(id, FirstReceipt)),

    ct:pal("Receipt path: ~s~n", [ReceiptPath]),

    %% Corrupt file
    ok = file:write_file(ReceiptPath, <<"corrupted data">>),
    ct:pal("Corrupted receipt file~n"),

    %% Try to load (should detect corruption)
    {error, corruption_detected} =
        tcps_persistence:load_receipt_json(ReceiptPath),

    ct:pal("Corruption detected~n"),

    %% Attempt recovery
    {ok, recovered} = tcps_persistence:recover_from_corruption(ReceiptPath),

    %% Verify recovered
    {ok, RecoveredReceipt} = tcps_persistence:load_receipt_json(ReceiptPath),
    ?assertEqual(FirstReceipt, RecoveredReceipt),

    ct:pal("Successfully recovered from corruption~n"),

    ct:pal("~n=== Corruption Recovery: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test concurrent writes
%% @end
%%--------------------------------------------------------------------
test_concurrent_writes(_Config) ->
    ct:pal("~n=== Testing Concurrent Writes ===~n"),

    %% Create 50 receipts concurrently
    Parent = self(),
    lists:foreach(fun(N) ->
        spawn_link(fun() ->
            Receipt = #{
                id => iolist_to_binary(["receipt-", integer_to_list(N)]),
                work_order_id => <<"wo-test">>,
                stage => test,
                result => pass,
                metadata => #{seq => N},
                timestamp => erlang:system_time(millisecond),
                signature => <<"sig">>
            },
            {ok, _Path} = tcps_persistence:store_receipt_json(Receipt),
            ok = tcps_persistence:persist_to_ontology(Receipt),
            Parent ! {done, N}
        end)
    end, lists:seq(1, 50)),

    %% Wait for all
    lists:foreach(fun(N) ->
        receive
            {done, N} -> ok
        after 10000 ->
            error({timeout, N})
        end
    end, lists:seq(1, 50)),

    ct:pal("All 50 receipts written concurrently~n"),

    %% Verify all persisted
    AllReceipts = tcps_persistence:query_receipts_by_work_order(<<"wo-test">>),
    ?assertEqual(50, length(AllReceipts)),

    %% Verify no corruption
    lists:foreach(fun(Receipt) ->
        ?assert(tcps_receipt_verifier:verify_signature(Receipt))
    end, AllReceipts),

    ct:pal("All receipts valid~n"),

    ct:pal("~n=== Concurrent Writes: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test large dataset handling
%% @end
%%--------------------------------------------------------------------
test_large_dataset(_Config) ->
    ct:pal("~n=== Testing Large Dataset ===~n"),

    %% Create 100 work orders
    StartTime = erlang:monotonic_time(millisecond),

    WorkOrders = lists:map(fun(_) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        ok = tcps_test_utils:process_work_order_full(WO),
        WO
    end, lists:seq(1, 100)),

    CreateTime = erlang:monotonic_time(millisecond) - StartTime,
    ct:pal("Created 100 work orders in ~p ms~n", [CreateTime]),

    %% Count total receipts
    TotalReceipts = lists:sum(lists:map(fun(WO) ->
        {ok, Receipts} = tcps_persistence:get_receipts_by_work_order(WO),
        length(Receipts)
    end, WorkOrders)),

    ?assert(TotalReceipts >= 700), % At least 7 per work order
    ct:pal("Total receipts: ~p~n", [TotalReceipts]),

    %% Test query performance
    QueryStart = erlang:monotonic_time(millisecond),
    AllWOs = tcps_work_order:list_all(),
    QueryEnd = erlang:monotonic_time(millisecond),
    QueryTime = QueryEnd - QueryStart,

    ?assertEqual(100, length(AllWOs)),
    ?assert(QueryTime < 1000), % Should be under 1 second
    ct:pal("Query time for 100 work orders: ~p ms~n", [QueryTime]),

    ct:pal("~n=== Large Dataset: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test query performance
%% @end
%%--------------------------------------------------------------------
test_query_performance(_Config) ->
    ct:pal("~n=== Testing Query Performance ===~n"),

    %% Create test data
    lists:foreach(fun(_) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        ok = tcps_test_utils:process_work_order_full(WO)
    end, lists:seq(1, 50)),

    ct:pal("Created 50 work orders~n"),

    %% Test various queries
    Queries = [
        {list_all_work_orders, fun() -> tcps_work_order:list_all() end},
        {list_by_bucket, fun() ->
            tcps_work_order:list_by_bucket(security)
        end},
        {list_by_status, fun() ->
            tcps_work_order:list_by_status(completed)
        end},
        {query_receipts, fun() ->
            tcps_persistence:get_all_receipts()
        end}
    ],

    Results = lists:map(fun({Name, QueryFun}) ->
        StartTime = erlang:monotonic_time(microsecond),
        _Result = QueryFun(),
        EndTime = erlang:monotonic_time(microsecond),
        Duration = EndTime - StartTime,
        ct:pal("~p: ~p μs~n", [Name, Duration]),
        {Name, Duration}
    end, Queries),

    %% All queries should be fast
    lists:foreach(fun({Name, Duration}) ->
        case Duration > 100000 of % 100ms
            true ->
                ct:pal("WARNING: ~p took ~p μs~n", [Name, Duration]);
            false ->
                ok
        end
    end, Results),

    ct:pal("~n=== Query Performance: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test incremental backup
%% @end
%%--------------------------------------------------------------------
test_incremental_backup(_Config) ->
    ct:pal("~n=== Testing Incremental Backup ===~n"),

    %% Create initial data
    lists:foreach(fun(_) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        ok = tcps_test_utils:process_work_order_full(WO)
    end, lists:seq(1, 10)),

    %% Full backup
    FullBackup = "/tmp/tcps_full_backup.tar.gz",
    ok = tcps_persistence:backup_all(FullBackup),
    {ok, FullInfo} = file:read_file_info(FullBackup),
    FullSize = FullInfo#file_info.size,
    ct:pal("Full backup: ~p bytes~n", [FullSize]),

    %% Add more data
    lists:foreach(fun(_) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        ok = tcps_test_utils:process_work_order_full(WO)
    end, lists:seq(1, 5)),

    %% Incremental backup
    IncrBackup = "/tmp/tcps_incr_backup.tar.gz",
    ok = tcps_persistence:backup_incremental(IncrBackup, FullBackup),
    {ok, IncrInfo} = file:read_file_info(IncrBackup),
    IncrSize = IncrInfo#file_info.size,
    ct:pal("Incremental backup: ~p bytes~n", [IncrSize]),

    %% Incremental should be smaller
    ?assert(IncrSize < FullSize),
    ct:pal("Incremental is ~.2f% of full size~n",
           [(IncrSize / FullSize) * 100]),

    %% Cleanup
    file:delete(FullBackup),
    file:delete(IncrBackup),

    ct:pal("~n=== Incremental Backup: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test cross-session persistence
%% @end
%%--------------------------------------------------------------------
test_cross_session_persistence(_Config) ->
    ct:pal("~n=== Testing Cross-Session Persistence ===~n"),

    %% Session 1: Create data
    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_test_utils:process_work_order_full(WorkOrderId),
    ct:pal("Session 1: Created work order ~s~n", [WorkOrderId]),

    %% Simulate session end
    ok = tcps_persistence:flush_all(),
    ct:pal("Session ended, all data flushed~n"),

    %% Simulate new session (restart app)
    ok = application:stop(tcps),
    {ok, _} = application:start(tcps),
    ct:pal("Session 2: Application restarted~n"),

    %% Verify data available
    {ok, WOData} = tcps_work_order:get(WorkOrderId),
    ?assertEqual(completed, maps:get(status, WOData)),
    ct:pal("Session 2: Work order data retrieved~n"),

    %% Verify receipts available
    {ok, Receipts} = tcps_persistence:get_receipts_by_work_order(WorkOrderId),
    ?assert(length(Receipts) >= 7),
    ct:pal("Session 2: Receipts retrieved (~p)~n", [length(Receipts)]),

    ct:pal("~n=== Cross-Session Persistence: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test ontology versioning
%% @end
%%--------------------------------------------------------------------
test_ontology_versioning(_Config) ->
    ct:pal("~n=== Testing Ontology Versioning ===~n"),

    %% Get current ontology version
    {ok, Version1} = tcps_persistence:get_ontology_version(),
    ct:pal("Current ontology version: ~s~n", [Version1]),

    %% Create data with version
    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_test_utils:process_work_order_full(WorkOrderId),

    %% Verify version stored with data
    {ok, WOData} = tcps_persistence:query_ontology(#{
        subject => WorkOrderId
    }),
    ?assertEqual(Version1, maps:get(ontology_version, WOData)),

    ct:pal("Data tagged with ontology version~n"),

    %% Simulate version upgrade
    ok = tcps_persistence:set_ontology_version(<<"2.0.0">>),
    {ok, Version2} = tcps_persistence:get_ontology_version(),
    ?assertEqual(<<"2.0.0">>, Version2),
    ct:pal("Upgraded to ontology version: ~s~n", [Version2]),

    %% Verify old data still readable
    {ok, _WOData2} = tcps_work_order:get(WorkOrderId),
    ct:pal("Old data still readable after upgrade~n"),

    ct:pal("~n=== Ontology Versioning: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test receipt chain integrity
%% @end
%%--------------------------------------------------------------------
test_receipt_chain_integrity(_Config) ->
    ct:pal("~n=== Testing Receipt Chain Integrity ===~n"),

    %% Create work order with receipts
    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_test_utils:process_work_order_full(WorkOrderId),

    %% Get receipts
    {ok, Receipts} = tcps_persistence:get_receipts_by_work_order(WorkOrderId),
    ct:pal("Retrieved ~p receipts~n", [length(Receipts)]),

    %% Verify chain integrity
    {ok, complete} = tcps_receipt_verifier:verify_chain(WorkOrderId),
    ct:pal("Chain integrity verified~n"),

    %% Tamper with middle receipt
    [_, SecondReceipt | _] = Receipts,
    TamperedReceipt = SecondReceipt#{result => tampered},
    ok = tcps_persistence:store_receipt_json(TamperedReceipt),

    %% Verify chain now broken
    {error, chain_broken} = tcps_receipt_verifier:verify_chain(WorkOrderId),
    ct:pal("Tampered chain detected~n"),

    %% Repair chain
    ok = tcps_persistence:repair_receipt_chain(WorkOrderId),

    %% Verify repaired
    {ok, complete} = tcps_receipt_verifier:verify_chain(WorkOrderId),
    ct:pal("Chain repaired~n"),

    ct:pal("~n=== Receipt Chain Integrity: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test disaster recovery
%% @end
%%--------------------------------------------------------------------
test_disaster_recovery(_Config) ->
    ct:pal("~n=== Testing Disaster Recovery ===~n"),

    %% Create significant dataset
    lists:foreach(fun(_) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        ok = tcps_test_utils:process_work_order_full(WO)
    end, lists:seq(1, 20)),

    ct:pal("Created 20 work orders~n"),

    %% Create backup
    BackupFile = "/tmp/tcps_disaster_backup.tar.gz",
    ok = tcps_persistence:backup_all(BackupFile),
    ct:pal("Backup created~n"),

    %% Simulate disaster (corrupt all data)
    ok = tcps_test_utils:corrupt_all_data(),
    ct:pal("All data corrupted (simulated disaster)~n"),

    %% Verify data inaccessible
    {error, _} = tcps_work_order:list_all(),

    %% Perform disaster recovery
    ok = tcps_persistence:disaster_recovery(BackupFile),
    ct:pal("Disaster recovery completed~n"),

    %% Verify data restored
    RestoredWOs = tcps_work_order:list_all(),
    ?assertEqual(20, length(RestoredWOs)),

    %% Verify receipts intact
    lists:foreach(fun(WO) ->
        {ok, complete} = tcps_receipt_verifier:verify_chain(WO)
    end, RestoredWOs),

    ct:pal("All data and receipts restored~n"),

    %% Cleanup
    file:delete(BackupFile),

    ct:pal("~n=== Disaster Recovery: SUCCESS ===~n"),
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================
