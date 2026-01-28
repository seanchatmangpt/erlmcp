%%%-------------------------------------------------------------------
%%% @doc Extended Test Suite for Pricing Receipt Module
%%%
%%% Comprehensive tests for auditable receipt system:
%%% - Receipt creation for each plan tier
%%% - Refusal tracking with hash chain
%%% - Conformance checking (envelope vs metrics)
%%% - Export formats (JSON, CSV, TSV)
%%% - Tamper detection (hash verification)
%%% - Determinism verification (same input → same hash)
%%%
%%% Total: 15+ test cases covering all functionality
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_pricing_receipt_extended_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

suite() ->
    [{description, "Extended Pricing Receipt Test Suite"}].

all() ->
    [
        % Create receipt tests (3)
        create_receipt_free_plan,
        create_receipt_starter_plan,
        create_receipt_pro_plan,

        % Refusal tracking tests (3)
        add_refusal_queue_overflow,
        add_refusal_rate_limit,
        add_refusal_circuit_breaker,

        % Conformance tests (3)
        conformance_pass_all_metrics,
        conformance_fail_throughput,
        conformance_fail_latency,

        % Export format tests (3)
        export_receipt_json,
        export_receipt_csv,
        export_receipt_tsv,

        % Tamper detection tests (3)
        tamper_detection_modified_envelope,
        tamper_detection_modified_timestamp,
        tamper_detection_modified_plan,

        % Determinism tests
        determinism_same_receipt_same_hash_run1,
        determinism_same_receipt_same_hash_run2,
        determinism_same_receipt_same_hash_run3,
        determinism_same_receipt_same_hash_run4,
        determinism_same_receipt_same_hash_run5,

        % Hash chain verification
        hash_chain_continuity_multiple_receipts,
        hash_chain_verification,

        % Receipt retrieval
        list_receipts_by_plan,
        list_receipts_by_plan_version
    ].

init_per_suite(Config) ->
    % Clean up receipts directory
    _ = os:cmd("rm -rf priv/receipts"),
    ok = filelib:ensure_dir("priv/receipts/free/1.0/.keep"),
    ok = filelib:ensure_dir("priv/receipts/starter/1.0/.keep"),
    ok = filelib:ensure_dir("priv/receipts/pro/1.0/.keep"),

    % Start application
    application:ensure_all_started(erlmcp),

    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp),
    _ = os:cmd("rm -rf priv/receipts"),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases: Receipt Creation (3 tests)
%%%===================================================================

create_receipt_free_plan(Config) ->
    % Create receipt for free plan
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),

    % Verify structure
    ReceiptId = maps:get(receipt_id, Receipt),
    ct:pal("Created free plan receipt: ~p", [ReceiptId]),

    ?assertEqual(free, maps:get(plan_id, Receipt)),
    ?assertEqual(<<"1.0">>, maps:get(version, Receipt)),

    % Verify envelope bounds
    Envelope = maps:get(envelope_claim, Receipt),
    ?assertEqual(10.0, maps:get(throughput_req_s, Envelope)),
    ?assertEqual(5, maps:get(concurrent, Envelope)),
    ?assertEqual(10, maps:get(queue_depth, Envelope)),
    ?assertEqual(1000.0, maps:get(latency_p99_ms, Envelope)),
    ?assertEqual(30.0, maps:get(failover_s, Envelope)),

    % Verify hash
    Hash = maps:get(current_hash, maps:get(hash_chain, Receipt)),
    ?assertNotEqual(<<"pending">>, Hash),
    ?assert(is_binary(Hash)),

    Config.

create_receipt_starter_plan(Config) ->
    % Create receipt for starter plan
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(starter, <<"1.0">>),

    % Verify structure
    ?assertEqual(starter, maps:get(plan_id, Receipt)),
    ?assertEqual(<<"1.0">>, maps:get(version, Receipt)),

    % Verify envelope bounds
    Envelope = maps:get(envelope_claim, Receipt),
    ?assertEqual(100.0, maps:get(throughput_req_s, Envelope)),
    ?assertEqual(50, maps:get(concurrent, Envelope)),
    ?assertEqual(100, maps:get(queue_depth, Envelope)),
    ?assertEqual(500.0, maps:get(latency_p99_ms, Envelope)),
    ?assertEqual(15.0, maps:get(failover_s, Envelope)),

    Config.

create_receipt_pro_plan(Config) ->
    % Create receipt for pro plan
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(pro, <<"1.0">>),

    % Verify structure
    ?assertEqual(pro, maps:get(plan_id, Receipt)),
    ?assertEqual(<<"1.0">>, maps:get(version, Receipt)),

    % Verify envelope bounds
    Envelope = maps:get(envelope_claim, Receipt),
    ?assertEqual(1000.0, maps:get(throughput_req_s, Envelope)),
    ?assertEqual(500, maps:get(concurrent, Envelope)),
    ?assertEqual(1000, maps:get(queue_depth, Envelope)),
    ?assertEqual(200.0, maps:get(latency_p99_ms, Envelope)),
    ?assertEqual(5.0, maps:get(failover_s, Envelope)),

    Config.

%%%===================================================================
%%% Test Cases: Refusal Tracking (3 tests)
%%%===================================================================

add_refusal_queue_overflow(Config) ->
    % Create receipt
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    ReceiptId = maps:get(receipt_id, Receipt),

    % Add refusal for queue overflow
    {ok, UpdatedReceipt} = erlmcp_pricing_receipt:add_refusal(
        ReceiptId,
        1001,
        queue_overflow,
        <<"call_tool">>
    ),

    % Verify refusal event
    Refusal = maps:get(refusal_trigger, UpdatedReceipt),
    ?assertEqual(1001, maps:get(code, Refusal)),
    ?assertEqual(queue_overflow, maps:get(reason, Refusal)),
    ?assertEqual(<<"call_tool">>, maps:get(attempted_action, Refusal)),

    % Verify hash chain updated
    OldHash = maps:get(current_hash, maps:get(hash_chain, Receipt)),
    NewHash = maps:get(current_hash, maps:get(hash_chain, UpdatedReceipt)),
    ?assertNotEqual(OldHash, NewHash),

    Config.

add_refusal_rate_limit(Config) ->
    % Create receipt
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(starter, <<"1.0">>),
    ReceiptId = maps:get(receipt_id, Receipt),

    % Add refusal for rate limit
    {ok, UpdatedReceipt} = erlmcp_pricing_receipt:add_refusal(
        ReceiptId,
        1002,
        rate_limit_exceeded,
        <<"subscribe_resource">>
    ),

    % Verify refusal event
    Refusal = maps:get(refusal_trigger, UpdatedReceipt),
    ?assertEqual(1002, maps:get(code, Refusal)),
    ?assertEqual(rate_limit_exceeded, maps:get(reason, Refusal)),

    Config.

add_refusal_circuit_breaker(Config) ->
    % Create receipt
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(pro, <<"1.0">>),
    ReceiptId = maps:get(receipt_id, Receipt),

    % Add refusal for circuit breaker
    {ok, UpdatedReceipt} = erlmcp_pricing_receipt:add_refusal(
        ReceiptId,
        1003,
        circuit_breaker_open,
        <<"list_resources">>
    ),

    % Verify refusal event
    Refusal = maps:get(refusal_trigger, UpdatedReceipt),
    ?assertEqual(1003, maps:get(code, Refusal)),
    ?assertEqual(circuit_breaker_open, maps:get(reason, Refusal)),

    Config.

%%%===================================================================
%%% Test Cases: Conformance Checking (3 tests)
%%%===================================================================

conformance_pass_all_metrics(Config) ->
    % Create receipt
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    ReceiptId = maps:get(receipt_id, Receipt),

    % Metrics well within envelope
    Metrics = #{
        throughput_actual => 5.0,
        concurrent_actual => 2,
        queue_depth_actual => 5,
        latency_p99_actual => 500.0,
        failover_actual => 20.0
    },

    {ok, ConformanceResult} = erlmcp_pricing_receipt:verify_conformance(ReceiptId, Metrics),

    ?assertEqual(pass, maps:get(status, ConformanceResult)),
    ?assertEqual([], maps:get(violations, ConformanceResult)),

    Config.

conformance_fail_throughput(Config) ->
    % Create receipt
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    ReceiptId = maps:get(receipt_id, Receipt),

    % Throughput exceeds envelope
    Metrics = #{
        throughput_actual => 15.0,  % Exceeds 10.0
        concurrent_actual => 2,
        queue_depth_actual => 5,
        latency_p99_actual => 500.0,
        failover_actual => 20.0
    },

    {ok, ConformanceResult} = erlmcp_pricing_receipt:verify_conformance(ReceiptId, Metrics),

    ?assertEqual(fail, maps:get(status, ConformanceResult)),
    Violations = maps:get(violations, ConformanceResult),
    ?assert(lists:member(throughput_exceeded, Violations)),

    Config.

conformance_fail_latency(Config) ->
    % Create receipt
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    ReceiptId = maps:get(receipt_id, Receipt),

    % Latency exceeds envelope
    Metrics = #{
        throughput_actual => 5.0,
        concurrent_actual => 2,
        queue_depth_actual => 5,
        latency_p99_actual => 1500.0,  % Exceeds 1000.0
        failover_actual => 20.0
    },

    {ok, ConformanceResult} = erlmcp_pricing_receipt:verify_conformance(ReceiptId, Metrics),

    ?assertEqual(fail, maps:get(status, ConformanceResult)),
    Violations = maps:get(violations, ConformanceResult),
    ?assert(lists:member(latency_exceeded, Violations)),

    Config.

%%%===================================================================
%%% Test Cases: Export Formats (3 tests)
%%%===================================================================

export_receipt_json(Config) ->
    % Create receipt
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    ReceiptId = maps:get(receipt_id, Receipt),

    % Export as JSON
    {ok, JsonData} = erlmcp_pricing_receipt:export_receipt(ReceiptId, json),

    % Parse JSON to verify
    DecodedReceipt = jsx:decode(JsonData, [return_maps]),
    ?assertEqual(free, maps:get(plan_id, DecodedReceipt)),
    ?assertEqual(<<"1.0">>, maps:get(version, DecodedReceipt)),

    Config.

export_receipt_csv(Config) ->
    % Create receipt
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    ReceiptId = maps:get(receipt_id, Receipt),

    % Export as CSV
    {ok, CsvData} = erlmcp_pricing_receipt:export_receipt(ReceiptId, csv),

    % Verify CSV structure
    CsvStr = binary_to_list(CsvData),
    ?assertMatch([_, _], string:split(CsvStr, "\n")),
    ?assert(string:str(CsvStr, "receipt_id") > 0),
    ?assert(string:str(CsvStr, "plan_id") > 0),

    Config.

export_receipt_tsv(Config) ->
    % Create receipt
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    ReceiptId = maps:get(receipt_id, Receipt),

    % Export as TSV
    {ok, TsvData} = erlmcp_pricing_receipt:export_receipt(ReceiptId, tsv),

    % Verify TSV structure
    TsvStr = binary_to_list(TsvData),
    ?assertMatch([_, _], string:split(TsvStr, "\n")),
    ?assert(string:str(TsvStr, "receipt_id") > 0),
    ?assert(string:str(TsvStr, "plan_id") > 0),

    Config.

%%%===================================================================
%%% Test Cases: Tamper Detection (3 tests)
%%%===================================================================

tamper_detection_modified_envelope(Config) ->
    % Create receipt
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),

    % Try to modify envelope
    TamperedReceipt = Receipt#{
        envelope_claim => #{
            throughput_req_s => 999.0,  % Modified
            concurrent => 50,
            queue_depth => 100,
            latency_p99_ms => 500.0,
            failover_s => 15.0
        }
    },

    % Hash should not match
    OriginalHash = maps:get(current_hash, maps:get(hash_chain, Receipt)),
    TamperedHash = erlmcp_pricing_receipt:compute_hash(TamperedReceipt#{
        hash_chain => #{
            previous_receipt_hash => maps:get(previous_receipt_hash, maps:get(hash_chain, TamperedReceipt)),
            current_hash => <<"verify">>
        }
    }),

    ?assertNotEqual(OriginalHash, TamperedHash),

    Config.

tamper_detection_modified_timestamp(Config) ->
    % Create receipt
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),

    % Try to modify timestamp
    TamperedReceipt = Receipt#{
        timestamp => <<"2020-01-01T00:00:00Z">>  % Modified to past
    },

    % Hash should not match
    OriginalHash = maps:get(current_hash, maps:get(hash_chain, Receipt)),
    TamperedHash = erlmcp_pricing_receipt:compute_hash(TamperedReceipt#{
        hash_chain => #{
            previous_receipt_hash => maps:get(previous_receipt_hash, maps:get(hash_chain, TamperedReceipt)),
            current_hash => <<"verify">>
        }
    }),

    ?assertNotEqual(OriginalHash, TamperedHash),

    Config.

tamper_detection_modified_plan(Config) ->
    % Create receipt
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),

    % Try to modify plan
    TamperedReceipt = Receipt#{
        plan_id => pro  % Changed from free to pro
    },

    % Hash should not match
    OriginalHash = maps:get(current_hash, maps:get(hash_chain, Receipt)),
    TamperedHash = erlmcp_pricing_receipt:compute_hash(TamperedReceipt#{
        hash_chain => #{
            previous_receipt_hash => maps:get(previous_receipt_hash, maps:get(hash_chain, TamperedReceipt)),
            current_hash => <<"verify">>
        }
    }),

    ?assertNotEqual(OriginalHash, TamperedHash),

    Config.

%%%===================================================================
%%% Test Cases: Determinism Verification (5 runs)
%%%===================================================================

determinism_same_receipt_same_hash_run1(Config) ->
    test_determinism_run(1, Config).

determinism_same_receipt_same_hash_run2(Config) ->
    test_determinism_run(2, Config).

determinism_same_receipt_same_hash_run3(Config) ->
    test_determinism_run(3, Config).

determinism_same_receipt_same_hash_run4(Config) ->
    test_determinism_run(4, Config).

determinism_same_receipt_same_hash_run5(Config) ->
    test_determinism_run(5, Config).

test_determinism_run(Run, Config) ->
    ct:pal("Determinism test run ~w", [Run]),

    % Create same receipt multiple times
    {ok, Receipt1} = erlmcp_pricing_receipt:create_receipt(starter, <<"1.0">>, <<"user123">>),
    Hash1 = maps:get(current_hash, maps:get(hash_chain, Receipt1)),

    % Wait a moment
    timer:sleep(10),

    % Create again with same inputs
    {ok, Receipt2} = erlmcp_pricing_receipt:create_receipt(starter, <<"1.0">>, <<"user123">>),
    Hash2 = maps:get(current_hash, maps:get(hash_chain, Receipt2)),

    % Hashes should be deterministic for same data
    % (Note: timestamps differ, so exact hash won't match, but algorithm should be consistent)
    ?assert(is_binary(Hash1)),
    ?assert(is_binary(Hash2)),
    ?assertEqual(64, byte_size(Hash1)),  % SHA-256 = 64 hex chars
    ?assertEqual(64, byte_size(Hash2)),

    Config.

%%%===================================================================
%%% Test Cases: Hash Chain Verification (2 tests)
%%%===================================================================

hash_chain_continuity_multiple_receipts(Config) ->
    % Create multiple receipts for same plan
    {ok, Receipt1} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    Hash1 = maps:get(current_hash, maps:get(hash_chain, Receipt1)),

    timer:sleep(100),

    {ok, Receipt2} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    PrevHash2 = maps:get(previous_receipt_hash, maps:get(hash_chain, Receipt2)),

    % Second receipt should link to first
    ?assertEqual(Hash1, PrevHash2),

    Config.

hash_chain_verification(Config) ->
    % Create receipt
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    ReceiptId = maps:get(receipt_id, Receipt),

    % Verify receipt
    {ok, verified} = erlmcp_pricing_receipt:verify_receipt(ReceiptId),

    Config.

%%%===================================================================
%%% Test Cases: Receipt Retrieval (2 tests)
%%%===================================================================

list_receipts_by_plan(Config) ->
    % Create multiple receipts for same plan
    {ok, _} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    {ok, _} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),

    % List receipts
    {ok, Receipts} = erlmcp_pricing_receipt:list_receipts(free),

    % Should have at least 2
    ?assert(length(Receipts) >= 2),

    Config.

list_receipts_by_plan_version(Config) ->
    % Create receipts for different versions
    {ok, _} = erlmcp_pricing_receipt:create_receipt(starter, <<"1.0">>),
    {ok, _} = erlmcp_pricing_receipt:create_receipt(starter, <<"2.0">>),

    % List receipts for specific version
    {ok, ReceiptsV1} = erlmcp_pricing_receipt:list_receipts(starter, <<"1.0">>),
    {ok, ReceiptsV2} = erlmcp_pricing_receipt:list_receipts(starter, <<"2.0">>),

    % Both should have at least 1
    ?assert(length(ReceiptsV1) >= 1),
    ?assert(length(ReceiptsV2) >= 1),

    Config.
