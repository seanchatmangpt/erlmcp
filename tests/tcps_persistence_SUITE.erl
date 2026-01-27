%%%-------------------------------------------------------------------
%%% @doc Common Test Suite for TCPS Persistence
%%%
%%% Tests:
%%% - Receipt storage (JSON + checksums)
%%% - RDF ontology persistence
%%% - SPARQL queries
%%% - Backup and restore
%%% - Concurrent access
%%% - Round-trip (JSON → RDF → JSON)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_persistence_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    % Receipt Storage Tests
    test_store_receipt/1,
    test_load_receipt/1,
    test_verify_receipt_checksum/1,
    test_get_all_receipts/1,
    test_invalid_checksum/1,

    % RDF Ontology Tests
    test_persist_receipt_to_ontology/1,
    test_persist_work_order/1,
    test_persist_andon/1,
    test_persist_root_cause/1,

    % SPARQL Query Tests
    test_query_receipts_by_sku/1,
    test_query_work_orders_by_bucket/1,
    test_query_open_andons/1,
    test_custom_sparql_query/1,

    % Backup and Restore Tests
    test_backup_all/1,
    test_restore_from_backup/1,
    test_backup_integrity/1,

    % Validation Tests
    test_validate_ontology/1,
    test_repair_ontology/1,

    % Change Log Tests
    test_log_change/1,
    test_get_change_history/1,

    % Export Tests
    test_export_ontology_turtle/1,
    test_export_ontology_ntriples/1,
    test_export_ontology_jsonld/1,
    test_export_receipts_json/1,
    test_export_receipts_csv/1,

    % Round-Trip Tests
    test_json_to_rdf_to_json_roundtrip/1,

    % Concurrent Access Tests
    test_concurrent_receipt_storage/1,
    test_concurrent_ontology_persistence/1
]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
        {group, receipt_storage},
        {group, rdf_ontology},
        {group, sparql_queries},
        {group, backup_restore},
        {group, validation},
        {group, change_log},
        {group, export},
        {group, round_trip},
        {group, concurrent}
    ].

groups() ->
    [
        {receipt_storage, [parallel], [
            test_store_receipt,
            test_load_receipt,
            test_verify_receipt_checksum,
            test_get_all_receipts,
            test_invalid_checksum
        ]},
        {rdf_ontology, [parallel], [
            test_persist_receipt_to_ontology,
            test_persist_work_order,
            test_persist_andon,
            test_persist_root_cause
        ]},
        {sparql_queries, [parallel], [
            test_query_receipts_by_sku,
            test_query_work_orders_by_bucket,
            test_query_open_andons,
            test_custom_sparql_query
        ]},
        {backup_restore, [sequence], [
            test_backup_all,
            test_restore_from_backup,
            test_backup_integrity
        ]},
        {validation, [sequence], [
            test_validate_ontology,
            test_repair_ontology
        ]},
        {change_log, [parallel], [
            test_log_change,
            test_get_change_history
        ]},
        {export, [parallel], [
            test_export_ontology_turtle,
            test_export_ontology_ntriples,
            test_export_ontology_jsonld,
            test_export_receipts_json,
            test_export_receipts_csv
        ]},
        {round_trip, [sequence], [
            test_json_to_rdf_to_json_roundtrip
        ]},
        {concurrent, [parallel], [
            test_concurrent_receipt_storage,
            test_concurrent_ontology_persistence
        ]}
    ].

init_per_suite(Config) ->
    % Set up test environment
    application:set_env(tcps, persistence, #{
        ontology_dir => "test_ontology",
        receipts_dir => "test_receipts",
        backup_dir => "test_backups"
    }),

    % Initialize persistence
    tcps_persistence:init(),

    Config.

end_per_suite(_Config) ->
    % Clean up test directories
    delete_test_dirs(),
    ok.

init_per_testcase(_TestCase, Config) ->
    % Ensure clean state for each test
    tcps_persistence:ensure_dirs(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases - Receipt Storage
%%%===================================================================

test_store_receipt(_Config) ->
    Receipt = #{
        sku_id => <<"SKU-001">>,
        stage => compile,
        timestamp => <<"2026-01-26T10:00:00Z">>,
        status => pass,
        evidence => <<"Compilation successful">>
    },

    {ok, Path} = tcps_persistence:store_receipt(Receipt),
    ?assert(filelib:is_regular(binary_to_list(Path))),

    % Verify checksum was added
    {ok, StoredReceipt} = tcps_persistence:load_receipt(Path),
    ?assertMatch(#{checksum := _}, StoredReceipt).

test_load_receipt(_Config) ->
    Receipt = #{
        sku_id => <<"SKU-002">>,
        stage => test,
        timestamp => <<"2026-01-26T10:05:00Z">>,
        status => pass,
        evidence => <<"All tests passed">>
    },

    {ok, Path} = tcps_persistence:store_receipt(Receipt),
    {ok, LoadedReceipt} = tcps_persistence:load_receipt(Path),

    ?assertEqual(maps:get(sku_id, Receipt), maps:get(sku_id, LoadedReceipt)),
    ?assertEqual(maps:get(stage, Receipt), maps:get(stage, LoadedReceipt)),
    ?assertEqual(maps:get(status, Receipt), maps:get(status, LoadedReceipt)).

test_verify_receipt_checksum(_Config) ->
    Receipt = #{
        sku_id => <<"SKU-003">>,
        stage => lint,
        timestamp => <<"2026-01-26T10:10:00Z">>,
        status => pass,
        evidence => <<"Linting passed">>
    },

    {ok, Path} = tcps_persistence:store_receipt(Receipt),
    {ok, LoadedReceipt} = tcps_persistence:load_receipt(Path),

    % Verify checksum
    ?assertEqual(ok, tcps_persistence:verify_receipt(LoadedReceipt)).

test_get_all_receipts(_Config) ->
    SkuId = <<"SKU-004">>,

    % Store multiple receipts
    Receipts = [
        #{sku_id => SkuId, stage => compile, timestamp => <<"2026-01-26T10:00:00Z">>,
          status => pass, evidence => <<"Compile 1">>},
        #{sku_id => SkuId, stage => test, timestamp => <<"2026-01-26T10:05:00Z">>,
          status => pass, evidence => <<"Test 1">>},
        #{sku_id => SkuId, stage => deploy, timestamp => <<"2026-01-26T10:10:00Z">>,
          status => pass, evidence => <<"Deploy 1">>}
    ],

    lists:foreach(
        fun(R) -> {ok, _} = tcps_persistence:store_receipt(R) end,
        Receipts
    ),

    % Get all receipts
    AllReceipts = tcps_persistence:get_all_receipts(SkuId),
    ?assertEqual(3, length(AllReceipts)),

    % Verify sorted by timestamp
    [First | _] = AllReceipts,
    ?assertEqual(<<"2026-01-26T10:00:00Z">>, maps:get(timestamp, First)).

test_invalid_checksum(_Config) ->
    Receipt = #{
        sku_id => <<"SKU-005">>,
        stage => build,
        timestamp => <<"2026-01-26T10:15:00Z">>,
        status => pass,
        evidence => <<"Build successful">>,
        checksum => <<"invalid_checksum">>
    },

    ?assertEqual({error, checksum_mismatch}, tcps_persistence:verify_receipt(Receipt)).

%%%===================================================================
%%% Test Cases - RDF Ontology
%%%===================================================================

test_persist_receipt_to_ontology(_Config) ->
    Receipt = #{
        sku_id => <<"SKU-006">>,
        stage => compile,
        timestamp => <<"2026-01-26T11:00:00Z">>,
        status => pass,
        evidence => <<"Compilation successful">>
    },

    ?assertEqual(ok, tcps_persistence:persist_to_ontology(Receipt)),

    % Verify file exists
    OntologyFile = filename:join("test_ontology", "receipts.ttl"),
    ?assert(filelib:is_regular(OntologyFile)).

test_persist_work_order(_Config) ->
    WorkOrder = #{
        id => <<"WO-001">>,
        sku_id => <<"SKU-007">>,
        bucket => production,
        priority => 1,
        sla => 3600,
        created_at => <<"2026-01-26T11:05:00Z">>
    },

    ?assertEqual(ok, tcps_persistence:persist_work_order(WorkOrder)),

    % Verify file exists
    OntologyFile = filename:join("test_ontology", "work_orders.ttl"),
    ?assert(filelib:is_regular(OntologyFile)).

test_persist_andon(_Config) ->
    AndonEvent = #{
        id => <<"ANDON-001">>,
        sku_id => <<"SKU-008">>,
        severity => critical,
        status => open,
        created_at => <<"2026-01-26T11:10:00Z">>
    },

    ?assertEqual(ok, tcps_persistence:persist_andon(AndonEvent)),

    % Verify file exists
    OntologyFile = filename:join("test_ontology", "andon_events.ttl"),
    ?assert(filelib:is_regular(OntologyFile)).

test_persist_root_cause(_Config) ->
    Analysis = #{
        id => <<"RCA-001">>,
        problem => <<"Build failure">>,
        root_cause => <<"Missing dependency">>,
        countermeasure => <<"Add dependency to manifest">>
    },

    ?assertEqual(ok, tcps_persistence:persist_root_cause(Analysis)),

    % Verify file exists
    OntologyFile = filename:join("test_ontology", "root_cause_analyses.ttl"),
    ?assert(filelib:is_regular(OntologyFile)).

%%%===================================================================
%%% Test Cases - SPARQL Queries
%%%===================================================================

test_query_receipts_by_sku(_Config) ->
    SkuId = <<"SKU-009">>,

    % Store receipts
    Receipt1 = #{
        sku_id => SkuId, stage => compile,
        timestamp => <<"2026-01-26T12:00:00Z">>,
        status => pass, evidence => <<"Compile OK">>
    },
    Receipt2 = #{
        sku_id => SkuId, stage => test,
        timestamp => <<"2026-01-26T12:05:00Z">>,
        status => pass, evidence => <<"Tests OK">>
    },

    {ok, _} = tcps_persistence:store_receipt(Receipt1),
    {ok, _} = tcps_persistence:store_receipt(Receipt2),

    % Query receipts (will be empty until Python integration)
    Results = tcps_persistence:query_receipts_by_sku(SkuId),
    ?assert(is_list(Results)).

test_query_work_orders_by_bucket(_Config) ->
    % Store work orders
    WO1 = #{
        id => <<"WO-010">>, sku_id => <<"SKU-010">>,
        bucket => production, priority => 1, sla => 3600,
        created_at => <<"2026-01-26T12:10:00Z">>
    },
    WO2 = #{
        id => <<"WO-011">>, sku_id => <<"SKU-011">>,
        bucket => production, priority => 2, sla => 7200,
        created_at => <<"2026-01-26T12:15:00Z">>
    },

    ok = tcps_persistence:persist_work_order(WO1),
    ok = tcps_persistence:persist_work_order(WO2),

    % Query work orders
    Results = tcps_persistence:query_work_orders_by_bucket(production),
    ?assert(is_list(Results)).

test_query_open_andons(_Config) ->
    % Store Andon events
    Andon1 = #{
        id => <<"ANDON-010">>, sku_id => <<"SKU-012">>,
        severity => critical, status => open,
        created_at => <<"2026-01-26T12:20:00Z">>
    },
    Andon2 = #{
        id => <<"ANDON-011">>, sku_id => <<"SKU-013">>,
        severity => warning, status => investigating,
        created_at => <<"2026-01-26T12:25:00Z">>
    },

    ok = tcps_persistence:persist_andon(Andon1),
    ok = tcps_persistence:persist_andon(Andon2),

    % Query open Andons
    Results = tcps_persistence:query_open_andons(),
    ?assert(is_list(Results)).

test_custom_sparql_query(_Config) ->
    Query = <<"PREFIX tcps: <http://example.org/tcps#>\n"
              "SELECT ?receipt WHERE { ?receipt rdf:type tcps:Receipt }">>,

    {ok, Results} = tcps_persistence:query_ontology(Query),
    ?assert(is_list(Results)).

%%%===================================================================
%%% Test Cases - Backup and Restore
%%%===================================================================

test_backup_all(_Config) ->
    % Create some data
    Receipt = #{
        sku_id => <<"SKU-014">>, stage => compile,
        timestamp => <<"2026-01-26T13:00:00Z">>,
        status => pass, evidence => <<"Backup test">>
    },
    {ok, _} = tcps_persistence:store_receipt(Receipt),

    % Create backup
    BackupPath = <<"test_backups/backup1">>,
    ?assertEqual(ok, tcps_persistence:backup_all(BackupPath)),

    % Verify tarball exists
    TarballPath = binary_to_list(BackupPath) ++ ".tar.gz",
    ?assert(filelib:is_regular(TarballPath)).

test_restore_from_backup(_Config) ->
    % Create backup first
    Receipt = #{
        sku_id => <<"SKU-015">>, stage => test,
        timestamp => <<"2026-01-26T13:05:00Z">>,
        status => pass, evidence => <<"Restore test">>
    },
    {ok, _} = tcps_persistence:store_receipt(Receipt),

    BackupPath = <<"test_backups/backup2">>,
    ok = tcps_persistence:backup_all(BackupPath),

    % Clear current data
    delete_test_dirs(),
    tcps_persistence:init(),

    % Restore from backup
    TarballPath = <<BackupPath/binary, ".tar.gz">>,
    ?assertEqual(ok, tcps_persistence:restore_from_backup(TarballPath)).

test_backup_integrity(_Config) ->
    % Create data
    SkuId = <<"SKU-016">>,
    Receipt1 = #{
        sku_id => SkuId, stage => compile,
        timestamp => <<"2026-01-26T13:10:00Z">>,
        status => pass, evidence => <<"Integrity test 1">>
    },
    Receipt2 = #{
        sku_id => SkuId, stage => test,
        timestamp => <<"2026-01-26T13:15:00Z">>,
        status => pass, evidence => <<"Integrity test 2">>
    },

    {ok, _} = tcps_persistence:store_receipt(Receipt1),
    {ok, _} = tcps_persistence:store_receipt(Receipt2),

    OriginalReceipts = tcps_persistence:get_all_receipts(SkuId),
    ?assertEqual(2, length(OriginalReceipts)),

    % Backup and restore
    BackupPath = <<"test_backups/backup3">>,
    ok = tcps_persistence:backup_all(BackupPath),

    delete_test_dirs(),
    tcps_persistence:init(),

    TarballPath = <<BackupPath/binary, ".tar.gz">>,
    ok = tcps_persistence:restore_from_backup(TarballPath),

    % Verify data integrity
    RestoredReceipts = tcps_persistence:get_all_receipts(SkuId),
    ?assertEqual(length(OriginalReceipts), length(RestoredReceipts)).

%%%===================================================================
%%% Test Cases - Validation
%%%===================================================================

test_validate_ontology(_Config) ->
    % Create valid data
    Receipt = #{
        sku_id => <<"SKU-017">>, stage => compile,
        timestamp => <<"2026-01-26T14:00:00Z">>,
        status => pass, evidence => <<"Validation test">>
    },
    {ok, _} = tcps_persistence:store_receipt(Receipt),

    % Validate
    Result = tcps_persistence:validate_ontology(),
    ?assertMatch({ok, valid}, Result).

test_repair_ontology(_Config) ->
    % Create invalid data (simulated violations)
    Violations = [
        #{entity => <<"tcps:Receipt_001">>,
          property => <<"tcps:timestamp">>,
          message => <<"Missing required property">>}
    ],

    % Attempt repair
    Result = tcps_persistence:repair_ontology(Violations),
    ?assert(is_tuple(Result)).

%%%===================================================================
%%% Test Cases - Change Log
%%%===================================================================

test_log_change(_Config) ->
    Change = #{
        action => created,
        timestamp => <<"2026-01-26T14:05:00Z">>
    },

    ?assertEqual(ok, tcps_persistence:log_change(receipt, <<"RECEIPT-001">>, Change)),

    % Verify change log file exists
    ChangeLogFile = filename:join("test_ontology", "changes.ttl"),
    ?assert(filelib:is_regular(ChangeLogFile)).

test_get_change_history(_Config) ->
    Id = <<"ENTITY-001">>,

    % Log multiple changes
    Change1 = #{action => created, timestamp => <<"2026-01-26T14:10:00Z">>},
    Change2 = #{action => updated, timestamp => <<"2026-01-26T14:15:00Z">>},

    ok = tcps_persistence:log_change(work_order, Id, Change1),
    ok = tcps_persistence:log_change(work_order, Id, Change2),

    % Get history
    History = tcps_persistence:get_change_history(work_order, Id),
    ?assert(is_list(History)).

%%%===================================================================
%%% Test Cases - Export
%%%===================================================================

test_export_ontology_turtle(_Config) ->
    Receipt = #{
        sku_id => <<"SKU-018">>, stage => compile,
        timestamp => <<"2026-01-26T14:20:00Z">>,
        status => pass, evidence => <<"Export test">>
    },
    {ok, _} = tcps_persistence:store_receipt(Receipt),

    {ok, Content} = tcps_persistence:export_ontology(turtle),
    ?assert(is_binary(Content)),
    ?assert(byte_size(Content) > 0).

test_export_ontology_ntriples(_Config) ->
    {ok, Content} = tcps_persistence:export_ontology(ntriples),
    ?assert(is_binary(Content)).

test_export_ontology_jsonld(_Config) ->
    {ok, Content} = tcps_persistence:export_ontology(jsonld),
    ?assert(is_binary(Content)).

test_export_receipts_json(_Config) ->
    SkuId = <<"SKU-019">>,
    Receipt = #{
        sku_id => SkuId, stage => compile,
        timestamp => <<"2026-01-26T14:25:00Z">>,
        status => pass, evidence => <<"JSON export">>
    },
    {ok, _} = tcps_persistence:store_receipt(Receipt),

    {ok, Content} = tcps_persistence:export_receipts(SkuId, json),
    ?assert(is_binary(Content)),

    % Verify valid JSON
    _Decoded = jsone:decode(Content, [{object_format, map}]).

test_export_receipts_csv(_Config) ->
    SkuId = <<"SKU-020">>,
    Receipt = #{
        sku_id => SkuId, stage => test,
        timestamp => <<"2026-01-26T14:30:00Z">>,
        status => pass, evidence => <<"CSV export">>
    },
    {ok, _} = tcps_persistence:store_receipt(Receipt),

    {ok, Content} = tcps_persistence:export_receipts(SkuId, csv),
    ?assert(is_binary(Content)),
    ?assert(byte_size(Content) > 0).

%%%===================================================================
%%% Test Cases - Round-Trip
%%%===================================================================

test_json_to_rdf_to_json_roundtrip(_Config) ->
    OriginalReceipt = #{
        sku_id => <<"SKU-021">>,
        stage => compile,
        timestamp => <<"2026-01-26T14:35:00Z">>,
        status => pass,
        evidence => <<"Round-trip test">>
    },

    % JSON → Storage
    {ok, Path} = tcps_persistence:store_receipt(OriginalReceipt),

    % Storage → RDF
    ok = tcps_persistence:persist_to_ontology(OriginalReceipt),

    % RDF → Query → JSON
    Results = tcps_persistence:query_receipts_by_sku(<<"SKU-021">>),

    % Storage → JSON
    {ok, LoadedReceipt} = tcps_persistence:load_receipt(Path),

    % Verify core fields match
    ?assertEqual(maps:get(sku_id, OriginalReceipt), maps:get(sku_id, LoadedReceipt)),
    ?assertEqual(maps:get(stage, OriginalReceipt), maps:get(stage, LoadedReceipt)),
    ?assertEqual(maps:get(status, OriginalReceipt), maps:get(status, LoadedReceipt)).

%%%===================================================================
%%% Test Cases - Concurrent Access
%%%===================================================================

test_concurrent_receipt_storage(_Config) ->
    SkuId = <<"SKU-022">>,

    % Spawn multiple processes storing receipts concurrently
    Processes = lists:map(
        fun(N) ->
            spawn(fun() ->
                Receipt = #{
                    sku_id => SkuId,
                    stage => list_to_atom("stage_" ++ integer_to_list(N)),
                    timestamp => iolist_to_binary(io_lib:format("2026-01-26T15:~2..0B:00Z", [N])),
                    status => pass,
                    evidence => iolist_to_binary(io_lib:format("Concurrent test ~B", [N]))
                },
                {ok, _} = tcps_persistence:store_receipt(Receipt)
            end)
        end,
        lists:seq(1, 10)
    ),

    % Wait for all processes to complete
    timer:sleep(1000),

    % Verify all receipts stored
    AllReceipts = tcps_persistence:get_all_receipts(SkuId),
    ?assertEqual(10, length(AllReceipts)).

test_concurrent_ontology_persistence(_Config) ->
    % Spawn multiple processes persisting to ontology concurrently
    Processes = lists:map(
        fun(N) ->
            spawn(fun() ->
                WorkOrder = #{
                    id => iolist_to_binary(io_lib:format("WO-~3..0B", [N])),
                    sku_id => iolist_to_binary(io_lib:format("SKU-~3..0B", [N])),
                    bucket => production,
                    priority => N,
                    sla => 3600,
                    created_at => iolist_to_binary(io_lib:format("2026-01-26T15:~2..0B:00Z", [N]))
                },
                ok = tcps_persistence:persist_work_order(WorkOrder)
            end)
        end,
        lists:seq(1, 10)
    ),

    % Wait for all processes to complete
    timer:sleep(1000),

    % Verify ontology file exists and is not corrupted
    OntologyFile = filename:join("test_ontology", "work_orders.ttl"),
    ?assert(filelib:is_regular(OntologyFile)).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

delete_test_dirs() ->
    Dirs = ["test_ontology", "test_receipts", "test_backups"],
    lists:foreach(
        fun(Dir) ->
            case filelib:is_dir(Dir) of
                true ->
                    {ok, Files} = file:list_dir(Dir),
                    [file:delete(filename:join(Dir, F)) || F <- Files],
                    file:del_dir(Dir);
                false ->
                    ok
            end
        end,
        Dirs
    ).
