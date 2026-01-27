%%%-------------------------------------------------------------------
%%% @doc TCPS Persistence Layer Tests
%%%
%%% Comprehensive tests for dual storage (JSON + RDF) persistence.
%%%
%%% Tests:
%%% - Receipt storage and retrieval
%%% - Work order storage and retrieval
%%% - Andon event storage and retrieval
%%% - Ontology integration
%%% - Backup and restore
%%% - Integrity verification
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_persistence_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Setup and Teardown
%%%===================================================================

setup() ->
    % Ensure directories exist
    tcps_persistence:ensure_dirs(),

    % Start required services
    {ok, _} = tcps_ontology_index:start_link(),
    {ok, _} = tcps_query_cache:start_link(),

    % Create indexes
    ok = tcps_ontology_index:create_indexes(),

    ok.

cleanup(_) ->
    % Stop services
    tcps_ontology_index:stop(),
    tcps_query_cache:stop(),

    % Clean up test data
    file:del_dir_r("priv/tcps/receipts"),
    file:del_dir_r("priv/tcps/work_orders"),
    file:del_dir_r("priv/tcps/andon_events"),
    file:del_dir_r("priv/tcps/ontology"),
    file:del_dir_r("priv/tcps/backups"),

    ok.

%%%===================================================================
%%% Test Generators
%%%===================================================================

persistence_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"Store and load receipt", fun test_store_load_receipt/0},
         {"Store and load work order", fun test_store_load_work_order/0},
         {"Store and load Andon event", fun test_store_load_andon_event/0},
         {"List receipts by SKU", fun test_list_receipts_by_sku/0},
         {"List receipts by stage", fun test_list_receipts_by_stage/0},
         {"List work orders by status", fun test_list_work_orders_by_status/0},
         {"List work orders by bucket", fun test_list_work_orders_by_bucket/0},
         {"Update work order", fun test_update_work_order/0},
         {"Delete work order", fun test_delete_work_order/0},
         {"Verify receipt checksum", fun test_verify_receipt/0},
         {"Full backup and restore", fun test_full_backup_restore/0},
         {"Incremental backup", fun test_incremental_backup/0},
         {"Integrity verification", fun test_integrity_verification/0},
         {"Receipt chain verification", fun test_receipt_chain_verification/0},
         {"Ontology rebuild", fun test_ontology_rebuild/0}
     ]}.

%%%===================================================================
%%% Receipt Storage Tests
%%%===================================================================

test_store_load_receipt() ->
    % Create test receipt
    Receipt = #{
        sku_id => <<"SKU-001">>,
        stage => shacl,
        timestamp => <<"2026-01-26T10:00:00Z">>,
        status => pass,
        evidence => <<"SHACL validation passed">>
    },

    % Store receipt
    {ok, Path} = tcps_persistence:store_receipt(Receipt),
    ?assert(is_binary(Path)),

    % Load receipt
    {ok, LoadedReceipt} = tcps_persistence:load_receipt(Path),
    ?assertEqual(<<"SKU-001">>, maps:get(sku_id, LoadedReceipt)),
    ?assertEqual(shacl, maps:get(stage, LoadedReceipt)),
    ?assertEqual(pass, maps:get(status, LoadedReceipt)),

    % Verify checksum was added
    ?assert(maps:is_key(checksum, LoadedReceipt)),

    ok.

test_verify_receipt() ->
    % Create receipt
    Receipt = #{
        sku_id => <<"SKU-002">>,
        stage => compile,
        timestamp => <<"2026-01-26T11:00:00Z">>,
        status => pass,
        evidence => <<"Compilation successful">>
    },

    % Store receipt
    {ok, Path} = tcps_persistence:store_receipt(Receipt),

    % Load and verify
    {ok, LoadedReceipt} = tcps_persistence:load_receipt(Path),
    ?assertEqual(ok, tcps_persistence:verify_receipt(LoadedReceipt)),

    % Tamper with receipt
    TamperedReceipt = LoadedReceipt#{status => fail},
    ?assertEqual({error, checksum_mismatch},
                 tcps_persistence:verify_receipt(TamperedReceipt)),

    ok.

test_list_receipts_by_sku() ->
    % Create multiple receipts for same SKU
    SkuId = <<"SKU-003">>,

    Receipts = [
        #{sku_id => SkuId, stage => shacl, timestamp => <<"2026-01-26T10:00:00Z">>,
          status => pass, evidence => <<"Stage 1">>},
        #{sku_id => SkuId, stage => compile, timestamp => <<"2026-01-26T11:00:00Z">>,
          status => pass, evidence => <<"Stage 2">>},
        #{sku_id => SkuId, stage => test, timestamp => <<"2026-01-26T12:00:00Z">>,
          status => pass, evidence => <<"Stage 3">>}
    ],

    % Store all receipts
    lists:foreach(fun(R) ->
        {ok, _} = tcps_persistence:store_receipt(R)
    end, Receipts),

    % List receipts by SKU
    FoundReceipts = tcps_persistence:list_receipts_by_sku(SkuId),
    ?assertEqual(3, length(FoundReceipts)),

    ok.

test_list_receipts_by_stage() ->
    % Create receipts at different stages
    Receipts = [
        #{sku_id => <<"SKU-004">>, stage => test, timestamp => <<"2026-01-26T10:00:00Z">>,
          status => pass, evidence => <<"Test 1">>},
        #{sku_id => <<"SKU-005">>, stage => test, timestamp => <<"2026-01-26T11:00:00Z">>,
          status => pass, evidence => <<"Test 2">>},
        #{sku_id => <<"SKU-006">>, stage => compile, timestamp => <<"2026-01-26T12:00:00Z">>,
          status => pass, evidence => <<"Compile">>}
    ],

    % Store all receipts
    lists:foreach(fun(R) ->
        {ok, _} = tcps_persistence:store_receipt(R)
    end, Receipts),

    % List receipts by stage
    TestReceipts = tcps_persistence:list_receipts_by_stage(test),
    ?assertEqual(2, length(TestReceipts)),

    ok.

%%%===================================================================
%%% Work Order Storage Tests
%%%===================================================================

test_store_load_work_order() ->
    % Create test work order
    WorkOrder = #{
        id => <<"WO-001">>,
        bucket => security,
        priority => 10,
        status => queued,
        description => <<"Fix CVE-2026-1234">>,
        created_at => erlang:timestamp(),
        sla_deadline => erlang:timestamp()
    },

    % Store work order
    {ok, Path} = tcps_persistence:store_work_order(WorkOrder),
    ?assert(is_binary(Path)),

    % Load work order
    {ok, LoadedWorkOrder} = tcps_persistence:get_work_order(<<"WO-001">>),
    ?assertEqual(<<"WO-001">>, maps:get(id, LoadedWorkOrder)),
    ?assertEqual(security, maps:get(bucket, LoadedWorkOrder)),
    ?assertEqual(10, maps:get(priority, LoadedWorkOrder)),

    ok.

test_list_work_orders_by_status() ->
    % Create work orders with different statuses
    WorkOrders = [
        #{id => <<"WO-002">>, bucket => reliability, priority => 7,
          status => queued, description => <<"Fix bug">>,
          created_at => erlang:timestamp(), sla_deadline => erlang:timestamp()},
        #{id => <<"WO-003">>, bucket => features, priority => 5,
          status => in_progress, description => <<"Add feature">>,
          created_at => erlang:timestamp(), sla_deadline => erlang:timestamp()},
        #{id => <<"WO-004">>, bucket => security, priority => 9,
          status => queued, description => <<"Security fix">>,
          created_at => erlang:timestamp(), sla_deadline => erlang:timestamp()}
    ],

    % Store all work orders
    lists:foreach(fun(WO) ->
        {ok, _} = tcps_persistence:store_work_order(WO)
    end, WorkOrders),

    % List by status
    QueuedOrders = tcps_persistence:list_work_orders_by_status(queued),
    ?assertEqual(2, length(QueuedOrders)),

    InProgressOrders = tcps_persistence:list_work_orders_by_status(in_progress),
    ?assertEqual(1, length(InProgressOrders)),

    ok.

test_list_work_orders_by_bucket() ->
    % Create work orders in different buckets
    WorkOrders = [
        #{id => <<"WO-005">>, bucket => security, priority => 10,
          status => queued, description => <<"CVE fix">>,
          created_at => erlang:timestamp(), sla_deadline => erlang:timestamp()},
        #{id => <<"WO-006">>, bucket => security, priority => 9,
          status => queued, description => <<"Security audit">>,
          created_at => erlang:timestamp(), sla_deadline => erlang:timestamp()},
        #{id => <<"WO-007">>, bucket => features, priority => 5,
          status => queued, description => <<"New feature">>,
          created_at => erlang:timestamp(), sla_deadline => erlang:timestamp()}
    ],

    % Store all work orders
    lists:foreach(fun(WO) ->
        {ok, _} = tcps_persistence:store_work_order(WO)
    end, WorkOrders),

    % List by bucket
    SecurityOrders = tcps_persistence:list_work_orders_by_bucket(security),
    ?assertEqual(2, length(SecurityOrders)),

    ok.

test_update_work_order() ->
    % Create work order
    WorkOrder = #{
        id => <<"WO-008">>,
        bucket => reliability,
        priority => 6,
        status => queued,
        description => <<"Bug fix">>,
        created_at => erlang:timestamp(),
        sla_deadline => erlang:timestamp()
    },

    % Store work order
    {ok, _} = tcps_persistence:store_work_order(WorkOrder),

    % Update status
    UpdatedWorkOrder = WorkOrder#{status => in_progress},
    ok = tcps_persistence:update_work_order(UpdatedWorkOrder),

    % Verify update
    {ok, LoadedWorkOrder} = tcps_persistence:get_work_order(<<"WO-008">>),
    ?assertEqual(in_progress, maps:get(status, LoadedWorkOrder)),

    ok.

test_delete_work_order() ->
    % Create work order
    WorkOrder = #{
        id => <<"WO-009">>,
        bucket => cost,
        priority => 4,
        status => queued,
        description => <<"Performance optimization">>,
        created_at => erlang:timestamp(),
        sla_deadline => erlang:timestamp()
    },

    % Store work order
    {ok, _} = tcps_persistence:store_work_order(WorkOrder),

    % Delete work order
    ok = tcps_persistence:delete_work_order(<<"WO-009">>),

    % Verify deletion
    ?assertEqual({error, not_found},
                 tcps_persistence:get_work_order(<<"WO-009">>)),

    ok.

%%%===================================================================
%%% Andon Event Storage Tests
%%%===================================================================

test_store_load_andon_event() ->
    % Create test Andon event
    Andon = #{
        id => <<"ANDON-001">>,
        sku_id => <<"SKU-007">>,
        severity => critical,
        status => open,
        root_cause => #{
            problem => <<"Non-deterministic build">>,
            root_cause => <<"Timestamp in artifact">>,
            countermeasure => <<"Remove timestamp">>
        },
        created_at => <<"2026-01-26T13:00:00Z">>
    },

    % Store Andon event
    {ok, Path} = tcps_persistence:store_andon_event(Andon),
    ?assert(is_binary(Path)),

    % Load Andon event
    {ok, LoadedAndon} = tcps_persistence:get_andon_event(<<"ANDON-001">>),
    ?assertEqual(<<"ANDON-001">>, maps:get(id, LoadedAndon)),
    ?assertEqual(critical, maps:get(severity, LoadedAndon)),
    ?assertEqual(open, maps:get(status, LoadedAndon)),

    ok.

%%%===================================================================
%%% Backup and Restore Tests
%%%===================================================================

test_full_backup_restore() ->
    % Create test data
    Receipt = #{
        sku_id => <<"SKU-BACKUP">>,
        stage => compile,
        timestamp => <<"2026-01-26T14:00:00Z">>,
        status => pass,
        evidence => <<"Backup test">>
    },
    {ok, _} = tcps_persistence:store_receipt(Receipt),

    % Create full backup
    {ok, BackupPath} = tcps_persistence:backup(full),
    ?assert(filelib:is_regular(binary_to_list(BackupPath))),

    % Verify checksum file exists
    ChecksumPath = binary_to_list(BackupPath) ++ ".sha256",
    ?assert(filelib:is_regular(ChecksumPath)),

    % Clear data
    file:del_dir_r("priv/tcps/receipts"),

    % Restore from backup
    ok = tcps_persistence:restore(BackupPath),

    % Verify data restored
    RestoredReceipts = tcps_persistence:list_receipts_by_sku(<<"SKU-BACKUP">>),
    ?assertEqual(1, length(RestoredReceipts)),

    ok.

test_incremental_backup() ->
    % Create initial data and full backup
    Receipt1 = #{
        sku_id => <<"SKU-INCR-1">>,
        stage => shacl,
        timestamp => <<"2026-01-26T15:00:00Z">>,
        status => pass,
        evidence => <<"Initial">>
    },
    {ok, _} = tcps_persistence:store_receipt(Receipt1),

    {ok, _FullBackup} = tcps_persistence:backup(full),

    % Add more data
    timer:sleep(1000),  % Ensure different mtime
    Receipt2 = #{
        sku_id => <<"SKU-INCR-2">>,
        stage => compile,
        timestamp => <<"2026-01-26T16:00:00Z">>,
        status => pass,
        evidence => <<"Incremental">>
    },
    {ok, _} = tcps_persistence:store_receipt(Receipt2),

    % Create incremental backup
    {ok, IncrBackup} = tcps_persistence:backup(incremental),
    ?assert(is_binary(IncrBackup)),
    ?assert(byte_size(IncrBackup) > 0),

    ok.

%%%===================================================================
%%% Integrity Verification Tests
%%%===================================================================

test_integrity_verification() ->
    % Create test data
    Receipt = #{
        sku_id => <<"SKU-INTEGRITY">>,
        stage => test,
        timestamp => <<"2026-01-26T17:00:00Z">>,
        status => pass,
        evidence => <<"Integrity test">>
    },
    {ok, _} = tcps_persistence:store_receipt(Receipt),

    % Run integrity verification
    {ok, Report} = tcps_persistence:verify_integrity(),

    % Verify report structure
    ?assert(maps:is_key(json_files, Report)),
    ?assert(maps:is_key(rdf_ontology, Report)),
    ?assert(maps:is_key(indexes, Report)),
    ?assert(maps:is_key(receipt_chains, Report)),

    ok.

%%%===================================================================
%%% Receipt Chain Verification Tests
%%%===================================================================

test_receipt_chain_verification() ->
    % Create complete receipt chain
    SkuId = <<"SKU-CHAIN">>,
    Stages = [shacl, compile, test, security, deterministic,
              quality, release, smoke, validate, deploy],

    lists:foreach(fun(Stage) ->
        Receipt = #{
            sku_id => SkuId,
            stage => Stage,
            timestamp => iolist_to_binary(
                io_lib:format("2026-01-26T~2..0B:00:00Z",
                             [10 + length(lists:takewhile(fun(S) -> S =/= Stage end, Stages))])
            ),
            status => pass,
            evidence => <<"Chain test">>
        },
        {ok, _} = tcps_persistence:store_receipt(Receipt)
    end, Stages),

    % Verify chain
    {ok, complete} = tcps_receipt:verify_chain(SkuId),

    ok.

%%%===================================================================
%%% Ontology Rebuild Tests
%%%===================================================================

test_ontology_rebuild() ->
    % Create test data
    Receipt = #{
        sku_id => <<"SKU-REBUILD">>,
        stage => compile,
        timestamp => <<"2026-01-26T18:00:00Z">>,
        status => pass,
        evidence => <<"Rebuild test">>
    },
    {ok, _} = tcps_persistence:store_receipt(Receipt),

    % Rebuild ontology
    ok = tcps_persistence:rebuild_ontology(),

    % Verify data still accessible
    Receipts = tcps_persistence:list_receipts_by_sku(<<"SKU-REBUILD">>),
    ?assertEqual(1, length(Receipts)),

    ok.
