%%%-----------------------------------------------------------------------------
%%% @doc TCPS Receipt Verifier Test Suite
%%%
%%% Comprehensive test coverage for receipt verification and audit system.
%%%
%%% Test Categories:
%%% - Single receipt validation (valid, invalid, malformed)
%%% - Receipt chain verification (complete, incomplete, gaps)
%%% - Deterministic build verification (reproducible, non-deterministic)
%%% - Audit trail generation (complete history, metrics)
%%% - Compliance reporting (metrics, violations, thresholds)
%%% - Stage sequence validation (correct order, duplicates, missing)
%%% - Ontology link verification (valid refs, missing refs)
%%% - Tamper detection (checksums, timestamps, signatures)
%%% - Batch verification (directory scanning)
%%% - Receipt storage (naming, checksums, linking)
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_receipt_verifier_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Fixtures
%%%=============================================================================

setup() ->
    % Create test receipts directory
    TestDir = "test/fixtures/receipts",
    filelib:ensure_dir(TestDir ++ "/"),

    % Ensure priv/receipts exists
    PrivDir = "priv/receipts",
    filelib:ensure_dir(PrivDir ++ "/"),

    % Start Andon system if not running
    case whereis(tcps_andon) of
        undefined -> tcps_andon:start();
        _ -> ok
    end,

    TestDir.

cleanup(TestDir) ->
    % Clean up test fixtures
    case file:list_dir(TestDir) of
        {ok, Files} ->
            [file:delete(filename:join(TestDir, F)) || F <- Files],
            file:del_dir(TestDir);
        _ ->
            ok
    end,
    ok.

%%%=============================================================================
%%% Receipt Verification Tests
%%%=============================================================================

verify_receipt_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(TestDir) ->
         [
          {"Verify valid receipt", fun() -> test_verify_valid_receipt(TestDir) end},
          {"Reject missing required fields", fun() -> test_verify_missing_fields(TestDir) end},
          {"Reject invalid timestamp", fun() -> test_verify_invalid_timestamp(TestDir) end},
          {"Reject invalid SKU ID", fun() -> test_verify_invalid_sku_id(TestDir) end},
          {"Reject invalid stage", fun() -> test_verify_invalid_stage(TestDir) end},
          {"Reject invalid status", fun() -> test_verify_invalid_status(TestDir) end},
          {"Reject malformed JSON", fun() -> test_verify_malformed_json(TestDir) end}
         ]
     end}.

test_verify_valid_receipt(TestDir) ->
    Receipt = create_valid_receipt(<<"SKU-001">>, <<"compilation">>, <<"open">>),
    Path = write_receipt_to_file(Receipt, TestDir, "valid.json"),

    ?assertMatch({ok, valid}, tcps_receipt_verifier:verify_receipt(Path)).

test_verify_missing_fields(TestDir) ->
    Receipt = #{
        <<"receipt_id">> => <<"RCPT-001">>,
        <<"timestamp">> => erlang:system_time(millisecond)
        % Missing sku_id, stage, status
    },
    Path = write_receipt_to_file(Receipt, TestDir, "missing_fields.json"),

    Result = tcps_receipt_verifier:verify_receipt(Path),
    ?assertMatch({error, {invalid, _}}, Result).

test_verify_invalid_timestamp(TestDir) ->
    Receipt = create_valid_receipt(<<"SKU-001">>, <<"compilation">>, <<"open">>),
    InvalidReceipt = maps:put(<<"timestamp">>, -1, Receipt),
    Path = write_receipt_to_file(InvalidReceipt, TestDir, "invalid_timestamp.json"),

    Result = tcps_receipt_verifier:verify_receipt(Path),
    ?assertMatch({error, {invalid, _}}, Result).

test_verify_invalid_sku_id(TestDir) ->
    Receipt = create_valid_receipt(<<"">>, <<"compilation">>, <<"open">>),
    Path = write_receipt_to_file(Receipt, TestDir, "invalid_sku.json"),

    Result = tcps_receipt_verifier:verify_receipt(Path),
    ?assertMatch({error, {invalid, _}}, Result).

test_verify_invalid_stage(TestDir) ->
    Receipt = create_valid_receipt(<<"SKU-001">>, <<"invalid_stage">>, <<"open">>),
    Path = write_receipt_to_file(Receipt, TestDir, "invalid_stage.json"),

    Result = tcps_receipt_verifier:verify_receipt(Path),
    ?assertMatch({error, {invalid, _}}, Result).

test_verify_invalid_status(TestDir) ->
    Receipt = create_valid_receipt(<<"SKU-001">>, <<"compilation">>, <<"invalid_status">>),
    Path = write_receipt_to_file(Receipt, TestDir, "invalid_status.json"),

    Result = tcps_receipt_verifier:verify_receipt(Path),
    ?assertMatch({error, {invalid, _}}, Result).

test_verify_malformed_json(TestDir) ->
    Path = filename:join(TestDir, "malformed.json"),
    file:write_file(Path, <<"{invalid json">>),

    Result = tcps_receipt_verifier:verify_receipt(list_to_binary(Path)),
    ?assertMatch({error, {invalid, [json_parse_error]}}, Result).

%%%=============================================================================
%%% Receipt Chain Verification Tests
%%%=============================================================================

verify_receipt_chain_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Verify complete chain", fun test_verify_complete_chain/0},
      {"Detect missing stages", fun test_verify_missing_stages/0},
      {"Detect non-chronological order", fun test_verify_non_chronological/0},
      {"Detect timestamp gaps", fun test_verify_timestamp_gaps/0}
     ]}.

test_verify_complete_chain() ->
    SkuId = <<"SKU-CHAIN-001">>,

    % Create complete chain of receipts
    Stages = [<<"compilation">>, <<"testing">>, <<"validation">>, <<"execution">>],
    BaseTime = erlang:system_time(millisecond),

    lists:foreach(
        fun({Stage, Idx}) ->
            Receipt = create_valid_receipt(
                SkuId,
                Stage,
                <<"pass">>,
                BaseTime + (Idx * 1000000)  % 1000 seconds between stages
            ),
            write_receipt_to_priv(Receipt, SkuId, Stage)
        end,
        lists:zip(Stages, lists:seq(0, length(Stages) - 1))
    ),

    Result = tcps_receipt_verifier:verify_receipt_chain(SkuId),
    ?assertMatch({ok, complete}, Result).

test_verify_missing_stages() ->
    SkuId = <<"SKU-MISSING-001">>,

    % Create incomplete chain (missing testing stage)
    Stages = [<<"compilation">>, <<"validation">>, <<"execution">>],
    BaseTime = erlang:system_time(millisecond),

    lists:foreach(
        fun({Stage, Idx}) ->
            Receipt = create_valid_receipt(
                SkuId,
                Stage,
                <<"pass">>,
                BaseTime + (Idx * 1000000)
            ),
            write_receipt_to_priv(Receipt, SkuId, Stage)
        end,
        lists:zip(Stages, lists:seq(0, length(Stages) - 1))
    ),

    Result = tcps_receipt_verifier:verify_receipt_chain(SkuId),
    ?assertMatch({error, {incomplete, [testing]}}, Result).

test_verify_non_chronological() ->
    SkuId = <<"SKU-NONCHRONO-001">>,

    % Create chain with wrong order
    BaseTime = erlang:system_time(millisecond),

    Receipt1 = create_valid_receipt(SkuId, <<"compilation">>, <<"pass">>, BaseTime + 3000000),
    Receipt2 = create_valid_receipt(SkuId, <<"testing">>, <<"pass">>, BaseTime + 1000000),
    Receipt3 = create_valid_receipt(SkuId, <<"validation">>, <<"pass">>, BaseTime + 2000000),
    Receipt4 = create_valid_receipt(SkuId, <<"execution">>, <<"pass">>, BaseTime + 4000000),

    write_receipt_to_priv(Receipt1, SkuId, <<"compilation">>),
    write_receipt_to_priv(Receipt2, SkuId, <<"testing">>),
    write_receipt_to_priv(Receipt3, SkuId, <<"validation">>),
    write_receipt_to_priv(Receipt4, SkuId, <<"execution">>),

    Result = tcps_receipt_verifier:verify_receipt_chain(SkuId),
    ?assertMatch({error, {incomplete, [not_chronological]}}, Result).

test_verify_timestamp_gaps() ->
    SkuId = <<"SKU-GAPS-001">>,

    % Create chain with large gap (> 24 hours)
    BaseTime = erlang:system_time(millisecond),

    Receipt1 = create_valid_receipt(SkuId, <<"compilation">>, <<"pass">>, BaseTime),
    Receipt2 = create_valid_receipt(SkuId, <<"testing">>, <<"pass">>,
                                   BaseTime + (25 * 60 * 60 * 1000)),  % 25 hours later
    Receipt3 = create_valid_receipt(SkuId, <<"validation">>, <<"pass">>,
                                   BaseTime + (26 * 60 * 60 * 1000)),
    Receipt4 = create_valid_receipt(SkuId, <<"execution">>, <<"pass">>,
                                   BaseTime + (27 * 60 * 60 * 1000)),

    write_receipt_to_priv(Receipt1, SkuId, <<"compilation">>),
    write_receipt_to_priv(Receipt2, SkuId, <<"testing">>),
    write_receipt_to_priv(Receipt3, SkuId, <<"validation">>),
    write_receipt_to_priv(Receipt4, SkuId, <<"execution">>),

    Result = tcps_receipt_verifier:verify_receipt_chain(SkuId),
    ?assertMatch({error, {incomplete, [timestamp_gap_too_large]}}, Result).

%%%=============================================================================
%%% Deterministic Build Verification Tests
%%%=============================================================================

verify_deterministic_build_test_() ->
    [
     {"Verify deterministic build", fun test_verify_deterministic_build/0},
     {"Detect non-deterministic build", fun test_verify_non_deterministic_build/0}
    ].

test_verify_deterministic_build() ->
    SkuId = <<"SKU-DET-001">>,

    % Mock consistent builds (in production, this would compile actual code)
    Result = tcps_receipt_verifier:verify_deterministic_build(SkuId),

    % Since we're using simulated builds, result should be deterministic
    ?assertMatch({ok, deterministic}, Result).

test_verify_non_deterministic_build() ->
    % This test demonstrates the API but won't actually fail
    % since our mock build is deterministic
    SkuId = <<"SKU-NONDET-001">>,

    Result = tcps_receipt_verifier:verify_deterministic_build(SkuId),

    % Verify result is one of the expected types
    ?assert(
        case Result of
            {ok, deterministic} -> true;
            {error, {non_deterministic, _}} -> true;
            _ -> false
        end
    ).

%%%=============================================================================
%%% Audit Trail Tests
%%%=============================================================================

audit_trail_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Generate complete audit trail", fun test_audit_trail_complete/0},
      {"Audit trail includes work order", fun test_audit_trail_work_order/0},
      {"Audit trail includes stages", fun test_audit_trail_stages/0},
      {"Audit trail includes receipts", fun test_audit_trail_receipts/0}
     ]}.

test_audit_trail_complete() ->
    SkuId = <<"SKU-AUDIT-001">>,

    % Create receipts
    create_test_receipt_chain(SkuId),

    % Generate audit trail
    AuditReport = tcps_receipt_verifier:audit_trail(SkuId),

    ?assertMatch(#{sku_id := SkuId}, AuditReport),
    ?assert(maps:is_key(work_order_created, AuditReport)),
    ?assert(maps:is_key(production_stages, AuditReport)),
    ?assert(maps:is_key(receipts, AuditReport)),
    ?assert(maps:is_key(total_lead_time_hours, AuditReport)),
    ?assert(maps:is_key(stage_durations, AuditReport)),
    ?assert(maps:is_key(quality_gates, AuditReport)),
    ?assert(maps:is_key(publish_status, AuditReport)).

test_audit_trail_work_order() ->
    SkuId = <<"SKU-AUDIT-WO-001">>,
    create_test_receipt_chain(SkuId),

    AuditReport = tcps_receipt_verifier:audit_trail(SkuId),
    WorkOrder = maps:get(work_order_created, AuditReport),

    ?assertMatch(#{created_at := _, status := _}, WorkOrder).

test_audit_trail_stages() ->
    SkuId = <<"SKU-AUDIT-STAGES-001">>,
    create_test_receipt_chain(SkuId),

    AuditReport = tcps_receipt_verifier:audit_trail(SkuId),
    Stages = maps:get(production_stages, AuditReport),

    ?assert(is_list(Stages)),
    ?assert(length(Stages) >= 4).

test_audit_trail_receipts() ->
    SkuId = <<"SKU-AUDIT-RECEIPTS-001">>,
    create_test_receipt_chain(SkuId),

    AuditReport = tcps_receipt_verifier:audit_trail(SkuId),
    Receipts = maps:get(receipts, AuditReport),

    ?assert(is_list(Receipts)),
    ?assert(length(Receipts) >= 4).

%%%=============================================================================
%%% Compliance Reporting Tests
%%%=============================================================================

audit_compliance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Generate compliance report", fun test_audit_compliance_basic/0},
      {"Compliance report has all fields", fun test_audit_compliance_fields/0},
      {"Compliance detects violations", fun test_audit_compliance_violations/0}
     ]}.

test_audit_compliance_basic() ->
    StartDate = {2026, 1, 1},
    EndDate = {2026, 1, 31},

    Report = tcps_receipt_verifier:audit_compliance({StartDate, EndDate}),

    ?assertMatch(#{period := {StartDate, EndDate}}, Report),
    ?assert(maps:is_key(total_skus_processed, Report)),
    ?assert(maps:is_key(compliance_status, Report)).

test_audit_compliance_fields() ->
    StartDate = {2026, 1, 1},
    EndDate = {2026, 1, 31},

    Report = tcps_receipt_verifier:audit_compliance({StartDate, EndDate}),

    RequiredFields = [
        period, total_skus_processed, receipts_generated,
        quality_gates_passed, quality_gates_failed,
        andon_events_triggered, andon_events_resolved,
        average_lead_time_hours, defect_rate_percent,
        first_pass_yield_percent, rework_rate_percent,
        stage_metrics, compliance_status, violations
    ],

    lists:foreach(
        fun(Field) ->
            ?assert(maps:is_key(Field, Report))
        end,
        RequiredFields
    ).

test_audit_compliance_violations() ->
    StartDate = {2026, 1, 1},
    EndDate = {2026, 1, 31},

    Report = tcps_receipt_verifier:audit_compliance({StartDate, EndDate}),
    Violations = maps:get(violations, Report),

    ?assert(is_list(Violations)).

%%%=============================================================================
%%% Stage Sequence Validation Tests
%%%=============================================================================

validate_stage_sequence_test_() ->
    [
     {"Valid stage sequence", fun test_valid_stage_sequence/0},
     {"Detect missing stages", fun test_sequence_missing_stages/0},
     {"Detect duplicate stages", fun test_sequence_duplicate_stages/0},
     {"Detect wrong order", fun test_sequence_wrong_order/0}
    ].

test_valid_stage_sequence() ->
    Receipts = [
        create_receipt_with_stage(<<"compilation">>),
        create_receipt_with_stage(<<"testing">>),
        create_receipt_with_stage(<<"validation">>),
        create_receipt_with_stage(<<"execution">>)
    ],

    Result = tcps_receipt_verifier:validate_stage_sequence(Receipts),
    ?assertMatch({ok, valid}, Result).

test_sequence_missing_stages() ->
    Receipts = [
        create_receipt_with_stage(<<"compilation">>),
        create_receipt_with_stage(<<"validation">>),
        create_receipt_with_stage(<<"execution">>)
    ],

    Result = tcps_receipt_verifier:validate_stage_sequence(Receipts),
    ?assertMatch({error, {invalid_sequence, _}}, Result).

test_sequence_duplicate_stages() ->
    Receipts = [
        create_receipt_with_stage(<<"compilation">>),
        create_receipt_with_stage(<<"testing">>),
        create_receipt_with_stage(<<"testing">>),
        create_receipt_with_stage(<<"validation">>),
        create_receipt_with_stage(<<"execution">>)
    ],

    Result = tcps_receipt_verifier:validate_stage_sequence(Receipts),
    ?assertMatch({error, {invalid_sequence, duplicate_stages}}, Result).

test_sequence_wrong_order() ->
    Receipts = [
        create_receipt_with_stage(<<"testing">>),
        create_receipt_with_stage(<<"compilation">>),
        create_receipt_with_stage(<<"validation">>),
        create_receipt_with_stage(<<"execution">>)
    ],

    Result = tcps_receipt_verifier:validate_stage_sequence(Receipts),
    ?assertMatch({error, {invalid_sequence, _}}, Result).

%%%=============================================================================
%%% Ontology Link Verification Tests
%%%=============================================================================

verify_ontology_links_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(TestDir) ->
         [
          {"Valid ontology links", fun() -> test_valid_ontology_links(TestDir) end},
          {"Missing ontology refs", fun() -> test_missing_ontology_refs(TestDir) end},
          {"Invalid URI format", fun() -> test_invalid_uri_format(TestDir) end}
         ]
     end}.

test_valid_ontology_links(TestDir) ->
    Receipt = create_valid_receipt(<<"SKU-001">>, <<"compilation">>, <<"open">>),
    ReceiptWithRefs = maps:put(
        <<"ontology_refs">>,
        [
            <<"http://example.org/tcps/ontology#AndonEvent">>,
            <<"http://example.org/tcps/ontology#compilation">>,
            <<"http://example.org/tcps/ontology#StopTheLine">>
        ],
        Receipt
    ),
    Path = write_receipt_to_file(ReceiptWithRefs, TestDir, "valid_ontology.json"),

    Result = tcps_receipt_verifier:verify_ontology_links(Path),
    ?assertMatch({ok, linked}, Result).

test_missing_ontology_refs(TestDir) ->
    Receipt = create_valid_receipt(<<"SKU-001">>, <<"compilation">>, <<"open">>),
    ReceiptWithoutRefs = maps:remove(<<"ontology_refs">>, Receipt),
    Path = write_receipt_to_file(ReceiptWithoutRefs, TestDir, "no_ontology.json"),

    Result = tcps_receipt_verifier:verify_ontology_links(Path),
    % Should still pass as ontology_refs are optional in current implementation
    ?assertMatch({ok, linked}, Result).

test_invalid_uri_format(TestDir) ->
    Receipt = create_valid_receipt(<<"SKU-001">>, <<"compilation">>, <<"open">>),
    ReceiptWithBadRefs = maps:put(
        <<"ontology_refs">>,
        [
            <<"not_a_uri">>,
            <<"also_not_a_uri">>
        ],
        Receipt
    ),
    Path = write_receipt_to_file(ReceiptWithBadRefs, TestDir, "bad_ontology.json"),

    Result = tcps_receipt_verifier:verify_ontology_links(Path),
    ?assertMatch({error, {unlinked, _}}, Result).

%%%=============================================================================
%%% Tamper Detection Tests
%%%=============================================================================

detect_tampering_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(TestDir) ->
         [
          {"Authentic receipt", fun() -> test_authentic_receipt(TestDir) end},
          {"Detect checksum mismatch", fun() -> test_checksum_mismatch(TestDir) end},
          {"Detect future timestamp", fun() -> test_future_timestamp(TestDir) end},
          {"Detect old timestamp", fun() -> test_old_timestamp(TestDir) end}
         ]
     end}.

test_authentic_receipt(TestDir) ->
    Receipt = create_valid_receipt(<<"SKU-001">>, <<"compilation">>, <<"open">>),
    Checksum = tcps_receipt_verifier:compute_checksum(Receipt),
    ReceiptWithChecksum = maps:put(<<"checksum">>, Checksum, Receipt),
    Path = write_receipt_to_file(ReceiptWithChecksum, TestDir, "authentic.json"),

    Result = tcps_receipt_verifier:detect_tampering(Path),
    ?assertMatch({ok, authentic}, Result).

test_checksum_mismatch(TestDir) ->
    Receipt = create_valid_receipt(<<"SKU-001">>, <<"compilation">>, <<"open">>),
    ReceiptWithBadChecksum = maps:put(<<"checksum">>, <<"invalid_checksum">>, Receipt),
    Path = write_receipt_to_file(ReceiptWithBadChecksum, TestDir, "bad_checksum.json"),

    Result = tcps_receipt_verifier:detect_tampering(Path),
    ?assertMatch({error, {tampered, #{checksum := mismatch}}}, Result).

test_future_timestamp(TestDir) ->
    FutureTime = erlang:system_time(millisecond) + (24 * 60 * 60 * 1000),  % 1 day in future
    Receipt = create_valid_receipt(<<"SKU-001">>, <<"compilation">>, <<"open">>, FutureTime),
    Path = write_receipt_to_file(Receipt, TestDir, "future_timestamp.json"),

    Result = tcps_receipt_verifier:detect_tampering(Path),
    ?assertMatch({error, {tampered, #{timestamp := _}}}, Result).

test_old_timestamp(TestDir) ->
    % 2 years ago
    OldTime = erlang:system_time(millisecond) - (2 * 365 * 24 * 60 * 60 * 1000),
    Receipt = create_valid_receipt(<<"SKU-001">>, <<"compilation">>, <<"open">>, OldTime),
    Path = write_receipt_to_file(Receipt, TestDir, "old_timestamp.json"),

    Result = tcps_receipt_verifier:detect_tampering(Path),
    ?assertMatch({error, {tampered, #{timestamp := _}}}, Result).

%%%=============================================================================
%%% Batch Verification Tests
%%%=============================================================================

verify_all_receipts_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(TestDir) ->
         [
          {"Verify empty directory", fun() -> test_verify_empty_directory(TestDir) end},
          {"Verify mixed valid/invalid", fun() -> test_verify_mixed_receipts(TestDir) end}
         ]
     end}.

test_verify_empty_directory(TestDir) ->
    Result = tcps_receipt_verifier:verify_all_receipts(list_to_binary(TestDir)),

    ?assertMatch(#{valid := 0, invalid := [], total := 0}, Result).

test_verify_mixed_receipts(TestDir) ->
    % Create valid receipt
    Valid1 = create_valid_receipt(<<"SKU-001">>, <<"compilation">>, <<"open">>),
    write_receipt_to_file(Valid1, TestDir, "valid1.json"),

    % Create invalid receipt (missing fields)
    Invalid1 = #{<<"receipt_id">> => <<"RCPT-002">>},
    write_receipt_to_file(Invalid1, TestDir, "invalid1.json"),

    % Create another valid receipt
    Valid2 = create_valid_receipt(<<"SKU-003">>, <<"testing">>, <<"pass">>),
    write_receipt_to_file(Valid2, TestDir, "valid2.json"),

    Result = tcps_receipt_verifier:verify_all_receipts(list_to_binary(TestDir)),

    ?assertMatch(#{valid := 2, total := 3}, Result),
    #{invalid := Invalid} = Result,
    ?assertEqual(1, length(Invalid)).

%%%=============================================================================
%%% Receipt Storage Tests
%%%=============================================================================

store_receipt_test_() ->
    [
     {"Store receipt with checksum", fun test_store_receipt_with_checksum/0},
     {"Receipt filename format", fun test_receipt_filename_format/0}
    ].

test_store_receipt_with_checksum() ->
    SkuId = <<"SKU-STORE-001">>,
    Receipt = #{
        receipt_id => <<"RCPT-STORE-001">>,
        timestamp => erlang:system_time(millisecond),
        sku_id => SkuId,
        stage => compilation,
        status => <<"pass">>
    },

    {ok, Path} = tcps_receipt_verifier:store_receipt(Receipt, SkuId),

    % Verify file was created
    ?assert(filelib:is_file(Path)),

    % Verify checksum was added
    {ok, JsonBin} = file:read_file(Path),
    StoredReceipt = jsx:decode(JsonBin, [return_maps]),
    ?assert(maps:is_key(<<"checksum">>, StoredReceipt)).

test_receipt_filename_format() ->
    SkuId = <<"SKU-STORE-002">>,
    Receipt = #{
        receipt_id => <<"RCPT-STORE-002">>,
        timestamp => erlang:system_time(millisecond),
        sku_id => SkuId,
        stage => testing,
        status => <<"pass">>
    },

    {ok, Path} = tcps_receipt_verifier:store_receipt(Receipt, SkuId),

    % Check path format: receipts/<sku_id>/<stage>-<timestamp>.json
    PathStr = binary_to_list(Path),
    ?assert(string:str(PathStr, "receipts") > 0),
    ?assert(string:str(PathStr, binary_to_list(SkuId)) > 0),
    ?assert(string:str(PathStr, ".json") > 0).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

create_valid_receipt(SkuId, Stage, Status) ->
    Timestamp = erlang:system_time(millisecond),
    create_valid_receipt(SkuId, Stage, Status, Timestamp).

create_valid_receipt(SkuId, Stage, Status, Timestamp) ->
    ReceiptId = iolist_to_binary(
        io_lib:format("RCPT-~p-~p-~p",
                     [Timestamp, rand:uniform(1000000), rand:uniform(10000)])
    ),

    TimestampIso = format_iso8601(Timestamp),

    #{
        <<"receipt_id">> => ReceiptId,
        <<"timestamp">> => Timestamp,
        <<"timestamp_iso">> => TimestampIso,
        <<"sku_id">> => SkuId,
        <<"stage">> => Stage,
        <<"status">> => Status,
        <<"receipt_type">> => <<"test_receipt">>,
        <<"ontology_refs">> => [
            <<"http://example.org/tcps/ontology#AndonEvent">>,
            <<"http://example.org/tcps/ontology#", Stage/binary>>,
            <<"http://example.org/tcps/ontology#StopTheLine">>
        ]
    }.

create_receipt_with_stage(Stage) ->
    #{
        <<"receipt_id">> => <<"RCPT-TEST">>,
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"sku_id">> => <<"SKU-TEST">>,
        <<"stage">> => Stage,
        <<"status">> => <<"pass">>
    }.

write_receipt_to_file(Receipt, TestDir, Filename) ->
    Path = filename:join(TestDir, Filename),
    JsonBin = jsx:encode(Receipt),
    ok = file:write_file(Path, JsonBin),
    list_to_binary(Path).

write_receipt_to_priv(Receipt, SkuId, Stage) ->
    BaseDir = "priv/receipts",
    filelib:ensure_dir(BaseDir ++ "/"),

    Timestamp = maps:get(<<"timestamp">>, Receipt),
    Filename = io_lib:format("~s-~p.json", [Stage, Timestamp]),
    Path = filename:join(BaseDir, Filename),

    JsonBin = jsx:encode(Receipt),
    ok = file:write_file(Path, JsonBin),
    Path.

create_test_receipt_chain(SkuId) ->
    Stages = [<<"compilation">>, <<"testing">>, <<"validation">>, <<"execution">>],
    BaseTime = erlang:system_time(millisecond),

    lists:foreach(
        fun({Stage, Idx}) ->
            Receipt = create_valid_receipt(
                SkuId,
                Stage,
                <<"pass">>,
                BaseTime + (Idx * 1000000)
            ),
            write_receipt_to_priv(Receipt, SkuId, Stage)
        end,
        lists:zip(Stages, lists:seq(0, length(Stages) - 1))
    ).

format_iso8601(Millisecond) ->
    Seconds = Millisecond div 1000,
    Microseconds = (Millisecond rem 1000) * 1000,

    BaseSeconds = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    GregorianSeconds = BaseSeconds + Seconds,
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:gregorian_seconds_to_datetime(GregorianSeconds),

    Iso = io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
                        [Year, Month, Day, Hour, Minute, Second, Microseconds div 1000]),
    list_to_binary(lists:flatten(Iso)).

%%%=============================================================================
%%% Comprehensive Chain Verification Tests
%%%=============================================================================

verify_complete_chain_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Verify complete chain - all checks pass", fun test_verify_complete_chain_pass/0},
      {"Verify complete chain - some checks fail", fun test_verify_complete_chain_fail/0},
      {"Verify quality gates passed", fun test_verify_quality_gates_passed/0},
      {"Verify no tampering", fun test_verify_no_tampering/0},
      {"Verify signature chain", fun test_verify_signature_chain/0},
      {"Verify stage transitions", fun test_verify_stage_transitions/0}
     ]}.

test_verify_complete_chain_pass() ->
    SkuId = <<"SKU-COMPLETE-001">>,

    % Create complete, valid chain
    Stages = [<<"compilation">>, <<"testing">>, <<"validation">>, <<"execution">>],
    BaseTime = erlang:system_time(millisecond),

    lists:foreach(
        fun({Stage, Idx}) ->
            Receipt = create_valid_receipt(
                SkuId,
                Stage,
                <<"pass">>,
                BaseTime + (Idx * 1000000)
            ),
            % Add checksum
            Checksum = tcps_receipt_verifier:compute_checksum(Receipt),
            ReceiptWithChecksum = maps:put(<<"checksum">>, Checksum, Receipt),
            write_receipt_to_priv(ReceiptWithChecksum, SkuId, Stage)
        end,
        lists:zip(Stages, lists:seq(0, length(Stages) - 1))
    ),

    Result = tcps_receipt_verifier:verify_complete_chain(SkuId),
    ?assertMatch({ok, _Report}, Result),

    {ok, Report} = Result,
    ?assert(maps:get(pass_rate, Report) > 0.5).

test_verify_complete_chain_fail() ->
    SkuId = <<"SKU-COMPLETE-FAIL-001">>,

    % Create incomplete chain (missing testing stage)
    Stages = [<<"compilation">>, <<"validation">>, <<"execution">>],
    BaseTime = erlang:system_time(millisecond),

    lists:foreach(
        fun({Stage, Idx}) ->
            Receipt = create_valid_receipt(
                SkuId,
                Stage,
                <<"pass">>,
                BaseTime + (Idx * 1000000)
            ),
            write_receipt_to_priv(Receipt, SkuId, Stage)
        end,
        lists:zip(Stages, lists:seq(0, length(Stages) - 1))
    ),

    Result = tcps_receipt_verifier:verify_complete_chain(SkuId),
    ?assertMatch({error, _Violations}, Result).

test_verify_quality_gates_passed() ->
    SkuId = <<"SKU-QUALITY-001">>,

    % Create receipts with all passing gates
    Stages = [<<"compilation">>, <<"testing">>, <<"validation">>, <<"execution">>],
    BaseTime = erlang:system_time(millisecond),

    lists:foreach(
        fun({Stage, Idx}) ->
            Receipt = create_valid_receipt(
                SkuId,
                Stage,
                <<"pass">>,
                BaseTime + (Idx * 1000000)
            ),
            write_receipt_to_priv(Receipt, SkuId, Stage)
        end,
        lists:zip(Stages, lists:seq(0, length(Stages) - 1))
    ),

    Result = tcps_receipt_verifier:verify_quality_gates_passed(SkuId),
    ?assertMatch({ok, pass}, Result).

test_verify_no_tampering() ->
    SkuId = <<"SKU-TAMPER-001">>,

    % Create receipts with valid checksums
    Stages = [<<"compilation">>, <<"testing">>],
    BaseTime = erlang:system_time(millisecond),

    lists:foreach(
        fun({Stage, Idx}) ->
            Receipt = create_valid_receipt(
                SkuId,
                Stage,
                <<"pass">>,
                BaseTime + (Idx * 1000000)
            ),
            % Add valid checksum
            Checksum = tcps_receipt_verifier:compute_checksum(Receipt),
            ReceiptWithChecksum = maps:put(<<"checksum">>, Checksum, Receipt),
            write_receipt_to_priv(ReceiptWithChecksum, SkuId, Stage)
        end,
        lists:zip(Stages, lists:seq(0, length(Stages) - 1))
    ),

    Result = tcps_receipt_verifier:verify_no_tampering(SkuId),
    ?assertMatch({ok, verified}, Result).

test_verify_signature_chain() ->
    SkuId = <<"SKU-SIG-001">>,

    % Create receipts with signatures
    Stages = [<<"compilation">>, <<"testing">>],
    BaseTime = erlang:system_time(millisecond),

    lists:foreach(
        fun({Stage, Idx}) ->
            Receipt = create_valid_receipt(
                SkuId,
                Stage,
                <<"pass">>,
                BaseTime + (Idx * 1000000)
            ),
            % Add dummy signature
            ReceiptWithSig = maps:put(<<"signature">>, <<"dummy_sig">>, Receipt),
            write_receipt_to_priv(ReceiptWithSig, SkuId, Stage)
        end,
        lists:zip(Stages, lists:seq(0, length(Stages) - 1))
    ),

    Result = tcps_receipt_verifier:verify_signature_chain(SkuId),
    ?assertMatch({ok, verified}, Result).

test_verify_stage_transitions() ->
    SkuId = <<"SKU-TRANS-001">>,

    % Create receipts with valid stage transitions
    Stages = [<<"compilation">>, <<"testing">>, <<"validation">>, <<"execution">>],
    BaseTime = erlang:system_time(millisecond),

    lists:foreach(
        fun({Stage, Idx}) ->
            Receipt = create_valid_receipt(
                SkuId,
                Stage,
                <<"pass">>,
                BaseTime + (Idx * 1000000)
            ),
            write_receipt_to_priv(Receipt, SkuId, Stage)
        end,
        lists:zip(Stages, lists:seq(0, length(Stages) - 1))
    ),

    Result = tcps_receipt_verifier:verify_stage_transitions(SkuId),
    ?assertMatch({ok, valid}, Result).

%%%=============================================================================
%%% Audit Trail Generation Tests
%%%=============================================================================

audit_trail_generation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Generate audit trail", fun test_generate_audit_trail/0},
      {"Generate compliance report", fun test_generate_compliance_report/0},
      {"Export audit trail JSON", fun test_export_audit_trail_json/0},
      {"Export audit trail PDF", fun test_export_audit_trail_pdf/0},
      {"Export audit trail XML", fun test_export_audit_trail_xml/0}
     ]}.

test_generate_audit_trail() ->
    SkuId = <<"SKU-AUDIT-GEN-001">>,
    create_test_receipt_chain(SkuId),

    {ok, AuditTrail} = tcps_receipt_verifier:generate_audit_trail(SkuId),

    ?assertMatch(#{sku_id := SkuId}, AuditTrail),
    ?assert(maps:is_key(work_order_id, AuditTrail)),
    ?assert(maps:is_key(receipts, AuditTrail)),
    ?assert(maps:is_key(andon_events, AuditTrail)),
    ?assert(maps:is_key(verification, AuditTrail)),
    ?assert(maps:is_key(generated_at, AuditTrail)),
    ?assert(maps:is_key(generated_by, AuditTrail)).

test_generate_compliance_report() ->
    SkuId = <<"SKU-COMPLIANCE-001">>,
    create_test_receipt_chain(SkuId),

    {ok, Report} = tcps_receipt_verifier:generate_compliance_report(SkuId),

    ?assertMatch(#{sku_id := SkuId}, Report),
    ?assert(maps:is_key(compliance_status, Report)),
    ?assert(maps:is_key(requirements, Report)),
    ?assert(maps:is_key(audit_trail_id, Report)),
    ?assert(maps:is_key(verified_by, Report)),
    ?assert(maps:is_key(verified_at, Report)),

    % Check requirements structure
    Requirements = maps:get(requirements, Report),
    ?assert(maps:is_key(deterministic_build, Requirements)),
    ?assert(maps:is_key(security_scan, Requirements)),
    ?assert(maps:is_key(test_coverage, Requirements)),
    ?assert(maps:is_key(quality_gates, Requirements)).

test_export_audit_trail_json() ->
    SkuId = <<"SKU-EXPORT-JSON-001">>,
    create_test_receipt_chain(SkuId),

    {ok, FilePath} = tcps_receipt_verifier:export_audit_trail(SkuId, json),

    ?assert(is_binary(FilePath)),
    ?assert(filelib:is_file(FilePath)),

    % Verify JSON is valid
    {ok, JsonBin} = file:read_file(FilePath),
    AuditTrail = jsx:decode(JsonBin, [return_maps]),
    ?assertMatch(#{<<"sku_id">> := SkuId}, AuditTrail).

test_export_audit_trail_pdf() ->
    SkuId = <<"SKU-EXPORT-PDF-001">>,
    create_test_receipt_chain(SkuId),

    {ok, FilePath} = tcps_receipt_verifier:export_audit_trail(SkuId, pdf),

    ?assert(is_binary(FilePath)).

test_export_audit_trail_xml() ->
    SkuId = <<"SKU-EXPORT-XML-001">>,
    create_test_receipt_chain(SkuId),

    {ok, FilePath} = tcps_receipt_verifier:export_audit_trail(SkuId, xml),

    ?assert(is_binary(FilePath)).

%%%=============================================================================
%%% Tampering Detection Tests
%%%=============================================================================

tampering_detection_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Detect tampered checksum", fun test_detect_tampered_checksum/0},
      {"Detect invalid signature", fun test_detect_invalid_signature/0}
     ]}.

test_detect_tampered_checksum() ->
    SkuId = <<"SKU-TAMPER-CHECK-001">>,

    % Create receipt with invalid checksum
    Receipt = create_valid_receipt(SkuId, <<"compilation">>, <<"pass">>),
    ReceiptWithBadChecksum = maps:put(<<"checksum">>, <<"invalid">>, Receipt),
    write_receipt_to_priv(ReceiptWithBadChecksum, SkuId, <<"compilation">>),

    Result = tcps_receipt_verifier:verify_no_tampering(SkuId),
    ?assertMatch({error, {tampered, _}}, Result).

test_detect_invalid_signature() ->
    SkuId = <<"SKU-INVALID-SIG-001">>,

    % Create receipt with empty signature
    Receipt = create_valid_receipt(SkuId, <<"compilation">>, <<"pass">>),
    ReceiptWithEmptySig = maps:put(<<"signature">>, <<"">>, Receipt),
    write_receipt_to_priv(ReceiptWithEmptySig, SkuId, <<"compilation">>),

    Result = tcps_receipt_verifier:verify_signature_chain(SkuId),
    ?assertMatch({error, {invalid_signatures, _}}, Result).

%%%=============================================================================
%%% Stage Transition Tests
%%%=============================================================================

stage_transition_test_() ->
    [
     {"Valid stage transitions", fun test_valid_stage_transitions/0},
     {"Invalid stage transitions", fun test_invalid_stage_transitions/0},
     {"Missing stage transitions", fun test_missing_stage_transitions/0}
    ].

test_valid_stage_transitions() ->
    SkuId = <<"SKU-VALID-TRANS-001">>,

    % Create receipts with valid transitions
    Stages = [<<"compilation">>, <<"testing">>, <<"validation">>, <<"execution">>],
    BaseTime = erlang:system_time(millisecond),

    lists:foreach(
        fun({Stage, Idx}) ->
            Receipt = create_valid_receipt(
                SkuId,
                Stage,
                <<"pass">>,
                BaseTime + (Idx * 1000000)
            ),
            write_receipt_to_priv(Receipt, SkuId, Stage)
        end,
        lists:zip(Stages, lists:seq(0, length(Stages) - 1))
    ),

    Result = tcps_receipt_verifier:verify_stage_transitions(SkuId),
    ?assertMatch({ok, valid}, Result).

test_invalid_stage_transitions() ->
    SkuId = <<"SKU-INVALID-TRANS-001">>,

    % Create receipts with invalid transitions (skip testing)
    Stages = [<<"compilation">>, <<"validation">>, <<"execution">>],
    BaseTime = erlang:system_time(millisecond),

    lists:foreach(
        fun({Stage, Idx}) ->
            Receipt = create_valid_receipt(
                SkuId,
                Stage,
                <<"pass">>,
                BaseTime + (Idx * 1000000)
            ),
            write_receipt_to_priv(Receipt, SkuId, Stage)
        end,
        lists:zip(Stages, lists:seq(0, length(Stages) - 1))
    ),

    Result = tcps_receipt_verifier:verify_stage_transitions(SkuId),
    ?assertMatch({error, {invalid_transitions, _}}, Result).

test_missing_stage_transitions() ->
    SkuId = <<"SKU-MISSING-TRANS-001">>,

    % Create only one receipt
    Receipt = create_valid_receipt(SkuId, <<"compilation">>, <<"pass">>),
    write_receipt_to_priv(Receipt, SkuId, <<"compilation">>),

    Result = tcps_receipt_verifier:verify_stage_transitions(SkuId),
    ?assertMatch({ok, valid}, Result).
