%%%-------------------------------------------------------------------
%%% @doc Basic unit tests for Pricing Receipt Module
%%%
%%% Tests core functionality of auditable receipt system with:
%%% - Receipt creation and storage
%%% - Hash chain computation
%%% - Conformance verification
%%% - Export formats
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_pricing_receipt_basic_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Generator
%%%===================================================================

pricing_receipt_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Create free plan receipt", fun test_create_free_plan_receipt/0},
          {"Create starter plan receipt", fun test_create_starter_plan_receipt/0},
          {"Create pro plan receipt", fun test_create_pro_plan_receipt/0},
          {"Add refusal for queue overflow", fun test_add_refusal_queue_overflow/0},
          {"Conformance pass", fun test_conformance_pass/0},
          {"Conformance fail throughput", fun test_conformance_fail_throughput/0},
          {"Export receipt as JSON", fun test_export_receipt_json/0},
          {"Export receipt as CSV", fun test_export_receipt_csv/0},
          {"Export receipt as TSV", fun test_export_receipt_tsv/0},
          {"Verify receipt", fun test_verify_receipt/0},
          {"Tamper detection envelope", fun test_tamper_detection_envelope/0},
          {"Tamper detection timestamp", fun test_tamper_detection_timestamp/0},
          {"Hash chain continuity", fun test_hash_chain_continuity/0},
          {"List receipts", fun test_list_receipts/0},
          {"List receipts by version", fun test_list_receipts_by_version/0}
         ]
     end}.

%%%===================================================================
%%% Fixtures
%%%===================================================================

setup() ->
    % Ensure receipts directory exists
    ok = filelib:ensure_dir("priv/receipts/free/1.0/.keep"),
    ok = filelib:ensure_dir("priv/receipts/starter/1.0/.keep"),
    ok = filelib:ensure_dir("priv/receipts/pro/1.0/.keep"),
    application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    application:stop(erlmcp),
    _ = os:cmd("rm -rf priv/receipts"),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_create_free_plan_receipt() ->
    % Create receipt for free plan
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),

    % Verify structure
    ?assertEqual(free, maps:get(plan_id, Receipt)),
    ?assertEqual(<<"1.0">>, maps:get(version, Receipt)),

    % Verify envelope bounds
    Envelope = maps:get(envelope_claim, Receipt),
    ?assertEqual(10.0, maps:get(throughput_req_s, Envelope)),
    ?assertEqual(5, maps:get(concurrent, Envelope)),
    ?assertEqual(10, maps:get(queue_depth, Envelope)),
    ?assertEqual(1000.0, maps:get(latency_p99_ms, Envelope)),
    ?assertEqual(30.0, maps:get(failover_s, Envelope)),

    % Verify hash present
    Hash = maps:get(current_hash, maps:get(hash_chain, Receipt)),
    ?assertNotEqual(<<"pending">>, Hash),
    ?assert(is_binary(Hash)),
    ?assertEqual(64, byte_size(Hash)).  % SHA-256 = 64 hex chars

test_create_starter_plan_receipt() ->
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
    ?assertEqual(15.0, maps:get(failover_s, Envelope)).

test_create_pro_plan_receipt() ->
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
    ?assertEqual(5.0, maps:get(failover_s, Envelope)).

test_add_refusal_queue_overflow() ->
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
    ?assertNotEqual(OldHash, NewHash).

test_conformance_pass() ->
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
    ?assertEqual([], maps:get(violations, ConformanceResult)).

test_conformance_fail_throughput() ->
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
    ?assert(lists:member(throughput_exceeded, Violations)).

test_export_receipt_json() ->
    % Create receipt
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    ReceiptId = maps:get(receipt_id, Receipt),

    % Export as JSON
    {ok, JsonData} = erlmcp_pricing_receipt:export_receipt(ReceiptId, json),

    % Parse JSON to verify
    DecodedReceipt = jsx:decode(JsonData, [return_maps]),
    ?assertEqual(free, maps:get(plan_id, DecodedReceipt)),
    ?assertEqual(<<"1.0">>, maps:get(version, DecodedReceipt)).

test_export_receipt_csv() ->
    % Create receipt
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    ReceiptId = maps:get(receipt_id, Receipt),

    % Export as CSV
    {ok, CsvData} = erlmcp_pricing_receipt:export_receipt(ReceiptId, csv),

    % Verify CSV structure
    CsvStr = iolist_to_binary(CsvData),
    ?assert(string:find(CsvStr, <<"receipt_id">>) =/= nomatch),
    ?assert(string:find(CsvStr, <<"plan_id">>) =/= nomatch).

test_export_receipt_tsv() ->
    % Create receipt
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    ReceiptId = maps:get(receipt_id, Receipt),

    % Export as TSV
    {ok, TsvData} = erlmcp_pricing_receipt:export_receipt(ReceiptId, tsv),

    % Verify TSV structure
    TsvStr = iolist_to_binary(TsvData),
    ?assert(string:find(TsvStr, <<"receipt_id">>) =/= nomatch),
    ?assert(string:find(TsvStr, <<"plan_id">>) =/= nomatch).

test_verify_receipt() ->
    % Create receipt
    {ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    ReceiptId = maps:get(receipt_id, Receipt),

    % Verify receipt
    {ok, verified} = erlmcp_pricing_receipt:verify_receipt(ReceiptId).

test_tamper_detection_envelope() ->
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

    ?assertNotEqual(OriginalHash, TamperedHash).

test_tamper_detection_timestamp() ->
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

    ?assertNotEqual(OriginalHash, TamperedHash).

test_hash_chain_continuity() ->
    % Create multiple receipts for same plan
    {ok, Receipt1} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    Hash1 = maps:get(current_hash, maps:get(hash_chain, Receipt1)),

    timer:sleep(100),

    {ok, Receipt2} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    PrevHash2 = maps:get(previous_receipt_hash, maps:get(hash_chain, Receipt2)),

    % Second receipt should link to first
    ?assertEqual(Hash1, PrevHash2).

test_list_receipts() ->
    % Create multiple receipts for same plan
    {ok, _} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
    timer:sleep(50),
    {ok, _} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),

    % List receipts
    {ok, Receipts} = erlmcp_pricing_receipt:list_receipts(free),

    % Should have at least 2
    ?assert(length(Receipts) >= 2).

test_list_receipts_by_version() ->
    % Create receipts for different versions
    {ok, _} = erlmcp_pricing_receipt:create_receipt(starter, <<"1.0">>),
    timer:sleep(50),
    {ok, _} = erlmcp_pricing_receipt:create_receipt(starter, <<"2.0">>),

    % List receipts for specific version
    {ok, ReceiptsV1} = erlmcp_pricing_receipt:list_receipts(starter, <<"1.0">>),
    {ok, ReceiptsV2} = erlmcp_pricing_receipt:list_receipts(starter, <<"2.0">>),

    % Both should have at least 1
    ?assert(length(ReceiptsV1) >= 1),
    ?assert(length(ReceiptsV2) >= 1).
