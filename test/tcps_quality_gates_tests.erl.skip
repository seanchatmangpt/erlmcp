%%%-----------------------------------------------------------------------------
%%% @doc TCPS Quality Gates Unit Tests
%%%
%%% Comprehensive unit testing for quality gates enforcement system.
%%%
%%% Test Coverage:
%%% - Individual gate testing (8 gates)
%%% - Sequential gate execution
%%% - Gate caching and status retrieval
%%% - Andon triggering on failures
%%% - Receipt generation and storage
%%% - Stage transition validation
%%% - Quality metrics calculation
%%% - Edge cases and error handling
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_quality_gates_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Fixtures
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Setup and teardown
%%------------------------------------------------------------------------------
quality_gates_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
             {"SHACL validation gate tests", fun test_shacl_validation_gate/0},
             {"Compilation gate tests", fun test_compilation_gate/0},
             {"Test execution gate tests", fun test_test_execution_gate/0},
             {"Security scan gate tests", fun test_security_scan_gate/0},
             {"Deterministic build gate tests", fun test_deterministic_build_gate/0},
             {"Quality metrics gate tests", fun test_quality_metrics_gate/0},
             {"Release verification gate tests", fun test_release_verification_gate/0},
             {"Smoke test gate tests", fun test_smoke_test_gate/0},
             {"Sequential gate execution tests", fun test_check_all_gates/0},
             {"Gate status caching tests", fun test_gate_status_caching/0},
             {"Stage transition validation tests", fun test_stage_transition_validation/0},
             {"Quality metrics calculation tests", fun test_quality_metrics_calculation/0},
             {"Receipt generation tests", fun test_receipt_generation/0},
             {"Andon integration tests", fun test_andon_integration/0},
             {"Stage to gate mapping tests", fun test_stage_gate_mapping/0},
             {"Unknown gate error handling tests", fun test_unknown_gate/0},
             {"Can proceed validation tests", fun test_can_proceed/0}
         ]
     end}.

setup() ->
    % Start dependencies
    application:ensure_all_started(jsx),

    % Start Andon system
    tcps_andon:start(),

    % Start quality gates gen_server
    {ok, Pid} = tcps_quality_gates:start_link(),

    % Ensure receipts directory exists
    filelib:ensure_dir("priv/receipts/"),

    Pid.

teardown(Pid) ->
    % Stop quality gates gen_server
    case is_process_alive(Pid) of
        true -> exit(Pid, shutdown);
        false -> ok
    end,

    % Stop Andon system
    tcps_andon:stop(),

    % Clean up test receipts
    case file:list_dir("priv/receipts") of
        {ok, Files} ->
            [file:delete(filename:join("priv/receipts", F)) || F <- Files];
        _ ->
            ok
    end,

    ok.

%%%=============================================================================
%%% Individual Gate Tests
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Test SHACL validation gate
%%------------------------------------------------------------------------------
test_shacl_validation_gate() ->
    SkuId = <<"test-sku-shacl-001">>,

    % Check SHACL validation gate
    Result = tcps_quality_gates:check_gate(shacl_validation, SkuId),

    % Should pass (skipped if module not available)
    ?assertMatch({pass, _Receipt}, Result),

    {pass, Receipt} = Result,

    % Verify receipt structure
    ?assertEqual(quality_gate, maps:get(receipt_type, Receipt)),
    ?assertEqual(shacl_validation, maps:get(gate, Receipt)),
    ?assertEqual(SkuId, maps:get(sku_id, Receipt)),
    ?assertEqual(pass, maps:get(status, Receipt)),
    ?assert(maps:is_key(timestamp, Receipt)),
    ?assert(maps:is_key(details, Receipt)),

    % Verify ontology references
    OntologyRefs = maps:get(ontology_refs, Receipt),
    ?assert(lists:member(<<"tcps:QualityGate">>, OntologyRefs)),
    ?assert(lists:member(<<"tcps:shacl_validation">>, OntologyRefs)),

    ok.

%%------------------------------------------------------------------------------
%% Test compilation gate
%%------------------------------------------------------------------------------
test_compilation_gate() ->
    SkuId = <<"test-sku-compile-001">>,

    % Check compilation gate (should pass if project compiles)
    Result = tcps_quality_gates:check_gate(compilation, SkuId),

    % Verify result is either pass or fail with proper structure
    case Result of
        {pass, Receipt} ->
            ?assertEqual(quality_gate, maps:get(receipt_type, Receipt)),
            ?assertEqual(compilation, maps:get(gate, Receipt)),
            ?assertEqual(pass, maps:get(status, Receipt)),

            Details = maps:get(details, Receipt),
            ?assertEqual(0, maps:get(error_count, Details));

        {fail, Violations} ->
            ?assert(is_list(Violations)),
            ?assert(length(Violations) > 0),

            [FirstViolation | _] = Violations,
            ?assertEqual(compilation, maps:get(gate, FirstViolation)),
            ?assert(maps:get(error_count, FirstViolation) > 0)
    end,

    ok.

%%------------------------------------------------------------------------------
%% Test test execution gate
%%------------------------------------------------------------------------------
test_test_execution_gate() ->
    SkuId = <<"test-sku-test-001">>,

    % Check test execution gate
    Result = tcps_quality_gates:check_gate(test_execution, SkuId),

    % Verify structure regardless of pass/fail
    case Result of
        {pass, Receipt} ->
            ?assertEqual(quality_gate, maps:get(receipt_type, Receipt)),
            ?assertEqual(test_execution, maps:get(gate, Receipt)),

            Details = maps:get(details, Receipt),
            ?assert(maps:is_key(total_tests, Details)),
            ?assert(maps:is_key(passed_tests, Details)),
            ?assert(maps:is_key(pass_rate, Details)),
            ?assert(maps:is_key(coverage, Details));

        {fail, Violations} ->
            ?assert(is_list(Violations)),
            ?assert(length(Violations) > 0)
    end,

    ok.

%%------------------------------------------------------------------------------
%% Test security scan gate
%%------------------------------------------------------------------------------
test_security_scan_gate() ->
    SkuId = <<"test-sku-security-001">>,

    % Check security scan gate
    Result = tcps_quality_gates:check_gate(security_scan, SkuId),

    % Should pass (no vulnerabilities in test environment)
    ?assertMatch({pass, _Receipt}, Result),

    {pass, Receipt} = Result,

    % Verify receipt
    ?assertEqual(security_scan, maps:get(gate, Receipt)),
    ?assertEqual(pass, maps:get(status, Receipt)),

    Details = maps:get(details, Receipt),
    ?assertEqual(0, maps:get(total_issues, Details)),

    ok.

%%------------------------------------------------------------------------------
%% Test deterministic build gate
%%------------------------------------------------------------------------------
test_deterministic_build_gate() ->
    SkuId = <<"test-sku-deterministic-001">>,

    % Check deterministic build gate
    Result = tcps_quality_gates:check_gate(deterministic_build, SkuId),

    % Verify structure
    case Result of
        {pass, Receipt} ->
            ?assertEqual(deterministic_build, maps:get(gate, Receipt)),
            Details = maps:get(details, Receipt),
            ?assert(maps:get(deterministic, Details) =:= true);

        {fail, Violations} ->
            ?assert(is_list(Violations)),
            [FirstViolation | _] = Violations,
            ?assertEqual(deterministic_build, maps:get(gate, FirstViolation))
    end,

    ok.

%%------------------------------------------------------------------------------
%% Test quality metrics gate
%%------------------------------------------------------------------------------
test_quality_metrics_gate() ->
    SkuId = <<"test-sku-metrics-001">>,

    % Check quality metrics gate
    Result = tcps_quality_gates:check_gate(quality_metrics, SkuId),

    % Verify structure
    case Result of
        {pass, Receipt} ->
            ?assertEqual(quality_metrics, maps:get(gate, Receipt)),
            Details = maps:get(details, Receipt),
            ?assert(maps:is_key(test_pass_rate, Details)),
            ?assert(maps:is_key(test_coverage, Details));

        {fail, Violations} ->
            ?assert(is_list(Violations))
    end,

    ok.

%%------------------------------------------------------------------------------
%% Test release verification gate
%%------------------------------------------------------------------------------
test_release_verification_gate() ->
    SkuId = <<"test-sku-release-001">>,

    % Check release verification gate
    Result = tcps_quality_gates:check_gate(release_verification, SkuId),

    % Verify structure
    case Result of
        {pass, Receipt} ->
            ?assertEqual(release_verification, maps:get(gate, Receipt)),
            Details = maps:get(details, Receipt),
            ?assertEqual(true, maps:get(all_passed, Details));

        {fail, Violations} ->
            ?assert(is_list(Violations)),
            ?assert(length(Violations) > 0)
    end,

    ok.

%%------------------------------------------------------------------------------
%% Test smoke test gate
%%------------------------------------------------------------------------------
test_smoke_test_gate() ->
    SkuId = <<"test-sku-smoke-001">>,

    % Check smoke test gate
    Result = tcps_quality_gates:check_gate(smoke_test, SkuId),

    % Should pass (basic smoke tests)
    ?assertMatch({pass, _Receipt}, Result),

    {pass, Receipt} = Result,
    ?assertEqual(smoke_test, maps:get(gate, Receipt)),

    ok.

%%%=============================================================================
%%% Sequential Gate Execution Tests
%%%=============================================================================

test_check_all_gates() ->
    SkuId = <<"test-sku-all-gates-001">>,

    % Check all gates sequentially
    Result = tcps_quality_gates:check_all_gates(SkuId),

    % Verify result is either all passed or failed_at
    case Result of
        {ok, Receipts} ->
            % All gates passed
            ?assert(is_list(Receipts)),
            ?assert(length(Receipts) > 0),

            % Verify each receipt
            lists:foreach(fun(Receipt) ->
                ?assertEqual(quality_gate, maps:get(receipt_type, Receipt)),
                ?assertEqual(pass, maps:get(status, Receipt)),
                ?assertEqual(SkuId, maps:get(sku_id, Receipt))
            end, Receipts),

            % Verify gates are in order
            Gates = [maps:get(gate, R) || R <- Receipts],
            ExpectedOrder = [
                shacl_validation,
                compilation,
                test_execution,
                security_scan,
                deterministic_build,
                quality_metrics,
                release_verification,
                smoke_test
            ],
            ?assertEqual(ExpectedOrder, Gates);

        {failed_at, Gate, Violations} ->
            % Some gate failed
            ?assert(is_atom(Gate)),
            ?assert(is_list(Violations)),
            ?assert(length(Violations) > 0)
    end,

    ok.

%%%=============================================================================
%%% Gate Status Caching Tests
%%%=============================================================================

test_gate_status_caching() ->
    SkuId = <<"test-sku-cache-001">>,

    % Initially, gate should be not_run
    ?assertEqual(not_run, tcps_quality_gates:get_gate_status(compilation, SkuId)),

    % Run gate
    {Status, Data} = tcps_quality_gates:check_gate(compilation, SkuId),

    % Now gate status should be cached
    CachedStatus = tcps_quality_gates:get_gate_status(compilation, SkuId),

    case Status of
        pass ->
            ?assertMatch({passed, _Receipt}, CachedStatus),
            {passed, Receipt} = CachedStatus,
            ?assertEqual(Data, Receipt);

        fail ->
            ?assertMatch({failed, _Violations}, CachedStatus),
            {failed, Violations} = CachedStatus,
            ?assertEqual(Data, Violations)
    end,

    ok.

%%%=============================================================================
%%% Stage Transition Validation Tests
%%%=============================================================================

test_stage_transition_validation() ->
    SkuId = <<"test-sku-transition-001">>,

    % Test transition to testing stage (requires compilation gate)
    % First check compilation gate
    _CompileResult = tcps_quality_gates:check_gate(compilation, SkuId),

    % Now validate transition to testing
    Result = tcps_quality_gates:validate_stage_transition(
        SkuId, compilation, testing
    ),

    % Should succeed if compilation passed
    case Result of
        ok ->
            ?assert(true);
        {error, {blocked, Reason}} ->
            % If blocked, verify reason is valid
            ?assert(is_tuple(Reason) orelse is_list(Reason))
    end,

    ok.

test_stage_gate_mapping() ->
    % Test stage to gate mapping
    ?assertEqual(compilation, tcps_quality_gates:stage_to_gate(compilation)),
    ?assertEqual(test_execution, tcps_quality_gates:stage_to_gate(testing)),
    ?assertEqual(shacl_validation, tcps_quality_gates:stage_to_gate(validation)),
    ?assertEqual(smoke_test, tcps_quality_gates:stage_to_gate(execution)),
    ?assertEqual(release_verification, tcps_quality_gates:stage_to_gate(integration)),
    ?assertEqual(release_verification, tcps_quality_gates:stage_to_gate(deployment)),

    % Test gate to stage mapping
    ?assertEqual(validation, tcps_quality_gates:gate_to_stage(shacl_validation)),
    ?assertEqual(compilation, tcps_quality_gates:gate_to_stage(compilation)),
    ?assertEqual(testing, tcps_quality_gates:gate_to_stage(test_execution)),
    ?assertEqual(testing, tcps_quality_gates:gate_to_stage(security_scan)),
    ?assertEqual(compilation, tcps_quality_gates:gate_to_stage(deterministic_build)),
    ?assertEqual(testing, tcps_quality_gates:gate_to_stage(quality_metrics)),
    ?assertEqual(integration, tcps_quality_gates:gate_to_stage(release_verification)),
    ?assertEqual(execution, tcps_quality_gates:gate_to_stage(smoke_test)),

    ok.

%%%=============================================================================
%%% Quality Metrics Tests
%%%=============================================================================

test_quality_metrics_calculation() ->
    % Get quality metrics
    Metrics = tcps_quality_gates:get_quality_metrics(),

    % Verify structure
    ?assert(maps:is_key(test_pass_rate, Metrics)),
    ?assert(maps:is_key(test_coverage, Metrics)),
    ?assert(maps:is_key(defect_rate, Metrics)),
    ?assert(maps:is_key(first_pass_yield, Metrics)),
    ?assert(maps:is_key(gate_pass_rates, Metrics)),

    % Verify values are in valid range
    TestPassRate = maps:get(test_pass_rate, Metrics),
    ?assert(TestPassRate >= 0.0 andalso TestPassRate =< 1.0),

    TestCoverage = maps:get(test_coverage, Metrics),
    ?assert(TestCoverage >= 0.0 andalso TestCoverage =< 1.0),

    DefectRate = maps:get(defect_rate, Metrics),
    ?assert(DefectRate >= 0.0),

    FirstPassYield = maps:get(first_pass_yield, Metrics),
    ?assert(FirstPassYield >= 0.0 andalso FirstPassYield =< 1.0),

    % Verify gate pass rates
    GatePassRates = maps:get(gate_pass_rates, Metrics),
    ?assert(maps:is_key(shacl_validation, GatePassRates)),
    ?assert(maps:is_key(compilation, GatePassRates)),
    ?assert(maps:is_key(test_execution, GatePassRates)),

    ok.

%%%=============================================================================
%%% Receipt Generation Tests
%%%=============================================================================

test_receipt_generation() ->
    SkuId = <<"test-sku-receipt-001">>,

    % Run gate to generate receipt
    {Status, Receipt} = tcps_quality_gates:check_gate(security_scan, SkuId),

    ?assertEqual(pass, Status),

    % Verify receipt structure
    ?assert(maps:is_key(receipt_id, Receipt)),
    ?assert(maps:is_key(receipt_type, Receipt)),
    ?assert(maps:is_key(gate, Receipt)),
    ?assert(maps:is_key(sku_id, Receipt)),
    ?assert(maps:is_key(status, Receipt)),
    ?assert(maps:is_key(timestamp, Receipt)),
    ?assert(maps:is_key(timestamp_iso, Receipt)),
    ?assert(maps:is_key(details, Receipt)),
    ?assert(maps:is_key(ontology_refs, Receipt)),

    % Verify timestamp_iso format (ISO 8601)
    TimestampIso = maps:get(timestamp_iso, Receipt),
    ?assert(is_binary(TimestampIso)),
    % Should match YYYY-MM-DDTHH:MM:SSZ
    ?assertMatch({match, _}, re:run(TimestampIso,
        <<"^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$">>)),

    % Verify receipt file was created
    ReceiptId = maps:get(receipt_id, Receipt),
    ReceiptPath = filename:join("priv/receipts",
                                binary_to_list(ReceiptId) ++ ".json"),
    ?assertEqual(true, filelib:is_file(ReceiptPath)),

    % Verify receipt content
    {ok, JsonBin} = file:read_file(ReceiptPath),
    StoredReceipt = jsx:decode(JsonBin, [return_maps]),
    ?assertEqual(ReceiptId, maps:get(<<"receipt_id">>, StoredReceipt)),

    ok.

%%%=============================================================================
%%% Andon Integration Tests
%%%=============================================================================

test_andon_integration() ->
    SkuId = <<"test-sku-andon-001">>,

    % Clear any existing Andons
    tcps_andon:stop(),
    tcps_andon:start(),

    % Run a gate that will fail (if compilation has errors)
    % We'll test with test_execution which has configurable thresholds
    _Result = tcps_quality_gates:check_gate(test_execution, SkuId),

    % If gate failed, verify Andon was triggered
    % (In test environment, gate may pass, so we check conditionally)
    case _Result of
        {fail, _Violations} ->
            % Verify Andon exists
            History = tcps_andon:get_andon_history(SkuId),
            ?assert(length(History) > 0),

            % Verify Andon details
            [Andon | _] = History,
            ?assertEqual(SkuId, maps:get(sku_id, Andon)),
            ?assert(maps:is_key(failure_type, Andon)),
            ?assertEqual(open, maps:get(status, Andon));

        {pass, _Receipt} ->
            % Gate passed - no Andon should be triggered
            History = tcps_andon:get_andon_history(SkuId),
            ?assertEqual([], History)
    end,

    ok.

%%%=============================================================================
%%% Edge Cases and Error Handling Tests
%%%=============================================================================

test_unknown_gate() ->
    SkuId = <<"test-sku-unknown-001">>,

    % Try to check unknown gate
    Result = tcps_quality_gates:check_gate(unknown_gate, SkuId),

    % Should fail with error
    ?assertMatch({fail, _Violations}, Result),

    {fail, Violations} = Result,
    ?assert(length(Violations) > 0),

    [FirstViolation | _] = Violations,
    ?assertEqual(unknown_gate, maps:get(gate, FirstViolation)),
    ?assertEqual(<<"Unknown quality gate">>, maps:get(error, FirstViolation)),

    ok.

test_can_proceed() ->
    SkuId = <<"test-sku-proceed-001">>,

    % Run compilation gate
    _CompileResult = tcps_quality_gates:check_gate(compilation, SkuId),

    % Check if can proceed to testing
    Result = tcps_quality_gates:can_proceed(SkuId, testing),

    % Verify result format
    case Result of
        {ok, proceed} ->
            ?assert(true);
        {blocked, Reasons} ->
            ?assert(is_list(Reasons)),
            ?assert(length(Reasons) > 0)
    end,

    ok.
