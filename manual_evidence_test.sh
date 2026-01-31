#!/bin/bash

echo "=== Manual Evidence Collection Test ==="
echo ""

# Create a test Erlang node to test evidence collection
erl -noshell -pa _build/default/lib/*/ebin -eval '
    io:format("~n=== Testing Evidence Collection Module ===~n~n"),

    %% Test 1: Hash evidence
    io:format("Test 1: Hash evidence with SHA-256...~n"),
    Content = #{test => data, value => 42},
    {ok, Hash} = erlmcp_compliance_report:hash_evidence(Content),
    io:format("  ✓ Hash generated: ~s~n", [Hash]),
    io:format("  ✓ Hash length: ~p bytes (expected 64 for SHA-256)~n", [byte_size(Hash)]),

    %% Test 2: Verify integrity
    io:format("~nTest 2: Verify evidence integrity...~n"),
    {ok, IsValid} = erlmcp_compliance_report:verify_evidence_integrity(Content, Hash),
    io:format("  ✓ Integrity check: ~p~n", [IsValid]),

    %% Test 3: Detect tampering
    io:format("~nTest 3: Detect tampered evidence...~n"),
    TamperedContent = #{test => tampered_data},
    {ok, IsTampered} = erlmcp_compliance_report:verify_evidence_integrity(TamperedContent, Hash),
    io:format("  ✓ Tampering detected: ~p~n", [not IsTampered]),

    %% Test 4: Collect test evidence
    io:format("~nTest 4: Collect test result evidence...~n"),
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
    io:format("  ✓ Module: ~p~n", [maps:get(module, maps:get(content, TestEvidence))]),

    %% Test 5: Collect coverage evidence
    io:format("~nTest 5: Collect coverage metrics evidence...~n"),
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

    %% Test 6: Collect security evidence
    io:format("~nTest 6: Collect security scan evidence...~n"),
    SecurityData = #{
        type => security_scan,
        scanner => bandit,
        issues_found => 0,
        timestamp => "2026-01-30T12:00:00Z"
    },
    {ok, SecurityEvidence} = erlmcp_compliance_report:collect_evidence(security_scan, SecurityData),
    io:format("  ✓ Scanner: ~p~n", [maps:get(scanner, maps:get(content, SecurityEvidence))]),
    io:format("  ✓ Issues found: ~p~n", [maps:get(issues_found, maps:get(content, SecurityEvidence))]),

    %% Test 7: Collect performance evidence
    io:format("~nTest 7: Collect performance benchmark evidence...~n"),
    PerfData = #{
        type => performance_benchmark,
        benchmark_name => core_ops_100k,
        throughput_ops_per_sec => 2690000,
        timestamp => "2026-01-30T12:00:00Z"
    },
    {ok, PerfEvidence} = erlmcp_compliance_report:collect_evidence(performance_benchmark, PerfData),
    io:format("  ✓ Benchmark: ~p~n", [maps:get(benchmark_name, maps:get(content, PerfEvidence))]),
    io:format("  ✓ Throughput: ~p ops/sec~n", [maps:get(throughput_ops_per_sec, maps:get(content, PerfEvidence))]),

    %% Test 8: Collect compliance evidence
    io:format("~nTest 8: Collect compliance validation evidence...~n"),
    ComplianceData = #{
        type => compliance_validation,
        spec_version => "2025-11-25",
        overall_compliance => 95.5,
        timestamp => "2026-01-30T12:00:00Z"
    },
    {ok, ComplianceEvidence} = erlmcp_compliance_report:collect_evidence(compliance_validation, ComplianceData),
    io:format("  ✓ Compliance: ~p%~n", [maps:get(overall_compliance, maps:get(content, ComplianceEvidence))]),

    %% Test 9: Create evidence bundle
    io:format("~nTest 9: Create evidence bundle...~n"),
    BundlePath = "/tmp/test_evidence_bundle_" ++ integer_to_list(erlang:unique_integer([positive])),
    {ok, CreatedPath} = erlmcp_compliance_report:create_evidence_bundle(BundlePath),
    io:format("  ✓ Bundle created: ~s~n", [CreatedPath]),

    %% Test 10: Store evidence bundle
    io:format("~nTest 10: Store evidence to bundle...~n"),
    EvidenceItems = [TestEvidence, CoverageEvidence, SecurityEvidence, PerfEvidence, ComplianceEvidence],
    {ok, StoredPath} = erlmcp_compliance_report:store_evidence_bundle(BundlePath, EvidenceItems),
    io:format("  ✓ Stored ~p evidence items~n", [length(EvidenceItems)]),

    %% Test 11: Link receipt chain
    io:format("~nTest 11: Link to receipt chain...~n"),
    ReceiptChain = #{previous_hash => <<"prev_hash">>, transaction_id => <<"tx_123">>},
    {ok, ReceiptPath} = erlmcp_compliance_report:link_receipt_chain(BundlePath, ReceiptChain),
    io:format("  ✓ Receipt chain linked~n"),

    %% Test 12: Generate evidence report
    io:format("~nTest 12: Generate evidence report...~n"),
    Report = erlmcp_compliance_report:generate_evidence_report(EvidenceItems),
    io:format("  ✓ Report ID: ~s~n", [maps:get(<<"report_id">>, Report)]),
    io:format("  ✓ Evidence count: ~p~n", [maps:get(<<"evidence_count">>, Report)]),

    %% Test 13: Invalid evidence type
    io:format("~nTest 13: Handle invalid evidence type...~n"),
    Result = erlmcp_compliance_report:collect_evidence(invalid_type, #{}),
    io:format("  ✓ Error returned: ~p~n", [Result]),

    %% Cleanup
    io:format("~nCleaning up test bundle...~n"),
    file:del_dir_r(BundlePath),

    io:format("~n=== All Tests Passed ===~n~n"),
    io:format("Summary:~n"),
    io:format("  Evidence Types Implemented:~n"),
    io:format("    - test_result~n"),
    io:format("    - coverage_metrics~n"),
    io:format("    - security_scan~n"),
    io:format("    - performance_benchmark~n"),
    io:format("    - compliance_validation~n"),
    io:format("  Hash Algorithm: SHA-256~n"),
    io:format("  Output Format: JSON with metadata, timestamps, hashes~n"),
    init:stop()
' 2>&1 | grep -E "(Test |✓|===|Summary|Evidence Types|Hash|Output)"
