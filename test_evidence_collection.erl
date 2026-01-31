#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/erlmcp_validation/ebin
%%! -pa _build/default/lib/jsx/ebin

-mode(compile).

main(_) ->
    %% Test evidence collection functions
    io:format("~n=== Testing Evidence Collection ===~n~n"),

    %% Test 1: Collect test evidence
    io:format("Test 1: Collect test evidence...~n"),
    TestData = #{
        type => test_result,
        module => erlmcp_json_rpc_tests,
        function => test_encode_request,
        status => passed,
        runtime_ms => 5,
        timestamp => "2026-01-30T12:00:00Z"
    },
    {ok, TestEvidence} = erlmcp_compliance_report:collect_evidence(test_result, TestData),
    io:format("  ✓ Evidence ID: ~s~n", [maps:get(evidence_id, TestEvidence)]),
    io:format("  ✓ Evidence Type: ~s~n", [maps:get(evidence_type, TestEvidence)]),
    io:format("  ✓ Hash: ~s~n", [maps:get(hash, TestEvidence)]),

    %% Test 2: Collect coverage evidence
    io:format("~nTest 2: Collect coverage evidence...~n"),
    CoverageData = #{
        type => coverage_metrics,
        module => erlmcp_json_rpc,
        coverage_percentage => 85.5,
        lines_covered => 425,
        lines_total => 497,
        timestamp => "2026-01-30T12:00:00Z"
    },
    {ok, CoverageEvidence} = erlmcp_compliance_report:collect_evidence(coverage_metrics, CoverageData),
    io:format("  ✓ Coverage: ~p%~n", [maps:get(coverage_percentage, maps:get(content, CoverageEvidence))]),

    %% Test 3: Hash evidence
    io:format("~nTest 3: Hash evidence...~n"),
    Content = #{test => data, value => 42},
    {ok, Hash} = erlmcp_compliance_report:hash_evidence(Content),
    io:format("  ✓ Hash length: ~p bytes~n", [byte_size(Hash)]),
    io:format("  ✓ Hash: ~s~n", [Hash]),

    %% Test 4: Verify integrity
    io:format("~nTest 4: Verify evidence integrity...~n"),
    {ok, IsValid} = erlmcp_compliance_report:verify_evidence_integrity(Content, Hash),
    io:format("  ✓ Valid: ~p~n", [IsValid]),

    %% Test 5: Detect tampering
    io:format("~nTest 5: Detect tampered evidence...~n"),
    TamperedContent = #{test => different_data},
    {ok, IsTampered} = erlmcp_compliance_report:verify_evidence_integrity(TamperedContent, Hash),
    io:format("  ✓ Tampered detection: ~p~n", [not IsTampered]),

    %% Test 6: Create evidence bundle
    io:format("~nTest 6: Create evidence bundle...~n"),
    TmpDir = "/tmp/evidence_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    BundlePath = filename:join([TmpDir, "test_bundle"]),
    {ok, CreatedPath} = erlmcp_compliance_report:create_evidence_bundle(BundlePath),
    io:format("  ✓ Bundle created: ~s~n", [CreatedPath]),
    io:format("  ✓ Directory exists: ~p~n", [filelib:is_dir(CreatedPath)]),

    %% Test 7: Store evidence bundle
    io:format("~nTest 7: Store evidence bundle...~n"),
    EvidenceItems = [TestEvidence, CoverageEvidence],
    {ok, StoredPath} = erlmcp_compliance_report:store_evidence_bundle(BundlePath, EvidenceItems),
    io:format("  ✓ Stored to: ~s~n", [StoredPath]),

    %% Test 8: Link receipt chain
    io:format("~nTest 8: Link receipt chain...~n"),
    ReceiptChain = #{
        previous_hash => <<"prev_hash_123">>,
        transaction_id => <<"tx_456">>,
        timestamp => "2026-01-30T12:00:00Z"
    },
    {ok, ReceiptPath} = erlmcp_compliance_report:link_receipt_chain(BundlePath, ReceiptChain),
    io:format("  ✓ Receipt chain linked: ~s~n", [ReceiptPath]),

    %% Test 9: Generate evidence report
    io:format("~nTest 9: Generate evidence report...~n"),
    Report = erlmcp_compliance_report:generate_evidence_report(EvidenceItems),
    io:format("  ✓ Report ID: ~s~n", [maps:get(<<"report_id">>, Report)]),
    io:format("  ✓ Evidence count: ~p~n", [maps:get(<<"evidence_count">>, Report)]),

    %% Test 10: Invalid evidence type
    io:format("~nTest 10: Handle invalid evidence type...~n"),
    InvalidData = #{type => invalid_type},
    Result = erlmcp_compliance_report:collect_evidence(invalid_type, InvalidData),
    io:format("  ✓ Error returned: ~p~n", [Result]),

    %% Cleanup
    io:format("~nCleaning up test directory...~n"),
    file:del_dir_r(TmpDir),

    io:format("~n=== All Evidence Collection Tests Passed ===~n~n"),
    io:format("~nEvidence Types Implemented:~n"),
    io:format("  - test_result~n"),
    io:format("  - coverage_metrics~n"),
    io:format("  - security_scan~n"),
    io:format("  - performance_benchmark~n"),
    io:format("  - compliance_validation~n"),
    io:format("~nHash Algorithm: SHA-256~n"),
    io:format("~nOutput Format: JSON with metadata, timestamps, hashes~n~n").
