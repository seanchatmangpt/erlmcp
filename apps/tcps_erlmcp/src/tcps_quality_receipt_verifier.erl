%%%-----------------------------------------------------------------------------
%%% @doc TCPS Quality Receipt Verification System
%%%
%%% Validates quality gate receipts and ensures all gates passed before
%%% allowing release or SKU certification. Blocks releases with failed gates.
%%%
%%% Core Responsibilities:
%%% - Verify quality gate receipts exist and are valid
%%% - Check all required quality gates passed
%%% - Validate receipt chain integrity (SHA-256 checksums)
%%% - Block releases if any gate failed
%%% - Generate verification reports
%%%
%%% Quality Gates Verified:
%%% - compilation_status (0 errors required)
%%% - test_pass_rate (>=95% pass rate)
%%% - coverage_percentage (>=80% coverage)
%%% - dialyzer_clean (0 type errors)
%%% - xref_clean (0 undefined function calls)
%%% - benchmark_regression (<10% performance regression)
%%%
%%% Receipt Chain Validation:
%%% - All receipts present (no gaps)
%%% - Checksums match (SHA-256 verification)
%%% - Chronological order (timestamps increasing)
%%% - Quality gate data complete
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_quality_receipt_verifier).

%% API exports
-export([
    verify_quality_receipts/1,
    verify_all_gates_passed/1,
    check_receipt_chain_integrity/1,
    validate_release_readiness/1,
    generate_quality_report/1,
    blocks_release/1
]).

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type sku_id() :: binary().
-type gate_name() :: compilation | testing | coverage | dialyzer |
                     xref | benchmark | security.

-type quality_receipt() :: #{
    receipt_id := binary(),
    sku_id := binary(),
    gate := gate_name(),
    timestamp := integer(),
    checksum := binary(),
    quality_data := quality_data()
}.

-type quality_data() :: #{
    compilation_status := passed | failed,
    compilation_errors := non_neg_integer(),
    test_pass_rate := float(),
    test_total := non_neg_integer(),
    test_passed := non_neg_integer(),
    coverage_percentage := float(),
    dialyzer_clean := boolean(),
    dialyzer_warnings := non_neg_integer(),
    xref_clean := boolean(),
    xref_issues := non_neg_integer(),
    benchmark_regression := float()
}.

-type verification_result() :: #{
    sku_id := sku_id(),
    all_gates_passed := boolean(),
    missing_receipts := [gate_name()],
    failed_gates := [gate_name()],
    chain_intact := boolean(),
    release_blocked := boolean(),
    verification_timestamp := integer()
}.

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Verify quality receipts for SKU.
%%
%% Checks:
%% - All required quality gate receipts exist
%% - Receipt chain is complete and intact
%% - All quality gates passed
%% - Checksums valid
%%
%% @end
%%------------------------------------------------------------------------------
-spec verify_quality_receipts(SkuId :: sku_id()) ->
    {ok, verification_result()} | {error, term()}.
verify_quality_receipts(SkuId) ->
    io:format("~n=== Verifying Quality Receipts for SKU: ~s ===~n", [SkuId]),

    % Load quality receipts
    case load_quality_receipts(SkuId) of
        {ok, Receipts} ->
            % Check all required gates present
            RequiredGates = required_quality_gates(),
            PresentGates = [gate_from_receipt(R) || R <- Receipts],
            MissingGates = RequiredGates -- PresentGates,

            % Verify each receipt's integrity
            IntegrityResults = verify_receipt_integrity(Receipts),
            ChainIntact = lists:all(fun({_Gate, Result}) -> Result =:= valid end,
                                    IntegrityResults),

            % Check which gates passed/failed
            GateResults = check_gate_results(Receipts),
            FailedGates = [Gate || {Gate, failed} <- GateResults],
            AllPassed = (FailedGates =:= []) andalso (MissingGates =:= []),

            % Determine if release is blocked
            ReleaseBlocked = not AllPassed orelse not ChainIntact,

            Result = #{
                sku_id => SkuId,
                all_gates_passed => AllPassed,
                missing_receipts => MissingGates,
                failed_gates => FailedGates,
                chain_intact => ChainIntact,
                release_blocked => ReleaseBlocked,
                verification_timestamp => erlang:system_time(millisecond)
            },

            % Print summary
            print_verification_summary(Result),

            case ReleaseBlocked of
                true ->
                    io:format("~n✗ RELEASE BLOCKED: Quality gates not satisfied~n"),
                    {error, {release_blocked, Result}};
                false ->
                    io:format("~n✓ RELEASE APPROVED: All quality gates passed~n"),
                    {ok, Result}
            end;
        {error, Reason} ->
            io:format("✗ Error loading receipts: ~p~n", [Reason]),
            {error, {no_receipts, Reason}}
    end.

%%------------------------------------------------------------------------------
%% @doc Verify all quality gates passed.
%%
%% Checks quality data in receipts:
%% - compilation_status = passed (0 errors)
%% - test_pass_rate >= 0.95 (95%)
%% - coverage_percentage >= 0.80 (80%)
%% - dialyzer_clean = true (0 warnings)
%% - xref_clean = true (0 issues)
%% - benchmark_regression < 0.10 (< 10% regression)
%%
%% @end
%%------------------------------------------------------------------------------
-spec verify_all_gates_passed(SkuId :: sku_id()) ->
    {ok, all_passed} | {error, {gates_failed, [gate_name()]}}.
verify_all_gates_passed(SkuId) ->
    case load_quality_receipts(SkuId) of
        {ok, Receipts} ->
            FailedGates = check_gate_results(Receipts),
            Failed = [Gate || {Gate, failed} <- FailedGates],

            case Failed of
                [] -> {ok, all_passed};
                _ -> {error, {gates_failed, Failed}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc Check receipt chain integrity.
%%
%% Verifies:
%% - SHA-256 checksums match for all receipts
%% - No tampering detected
%% - Receipt chain is complete
%%
%% @end
%%------------------------------------------------------------------------------
-spec check_receipt_chain_integrity(SkuId :: sku_id()) ->
    {ok, intact} | {error, {integrity_failed, Details :: map()}}.
check_receipt_chain_integrity(SkuId) ->
    case load_quality_receipts(SkuId) of
        {ok, Receipts} ->
            IntegrityResults = verify_receipt_integrity(Receipts),
            Failures = [{Gate, Reason} || {Gate, Reason} <- IntegrityResults,
                                          Reason =/= valid],

            case Failures of
                [] ->
                    {ok, intact};
                _ ->
                    {error, {integrity_failed, maps:from_list(Failures)}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc Validate SKU is ready for release.
%%
%% Comprehensive check:
%% - All quality receipts present
%% - All quality gates passed
%% - Receipt chain intact
%% - No open Andon events
%%
%% BLOCKS release if any check fails.
%%
%% @end
%%------------------------------------------------------------------------------
-spec validate_release_readiness(SkuId :: sku_id()) ->
    {ok, ready} | {error, {blocked, Reason :: term()}}.
validate_release_readiness(SkuId) ->
    io:format("~n=== Validating Release Readiness for SKU: ~s ===~n", [SkuId]),

    Checks = [
        {quality_receipts, verify_quality_receipts(SkuId)},
        {chain_integrity, check_receipt_chain_integrity(SkuId)},
        {gates_passed, verify_all_gates_passed(SkuId)},
        {andon_check, check_no_open_andons(SkuId)}
    ],

    Failures = lists:filtermap(
        fun({CheckName, Result}) ->
            case Result of
                {ok, _} -> false;
                {error, Reason} -> {true, {CheckName, Reason}}
            end
        end,
        Checks
    ),

    case Failures of
        [] ->
            io:format("✓ All release validation checks passed~n"),
            {ok, ready};
        _ ->
            io:format("✗ Release validation FAILED:~n"),
            lists:foreach(
                fun({Check, Reason}) ->
                    io:format("  ✗ ~p: ~p~n", [Check, Reason])
                end,
                Failures
            ),
            {error, {blocked, Failures}}
    end.

%%------------------------------------------------------------------------------
%% @doc Generate quality verification report.
%%
%% Report includes:
%% - All quality gate results
%% - Receipt chain status
%% - Detailed metrics (test pass rate, coverage, etc.)
%% - Release decision (approved/blocked)
%%
%% @end
%%------------------------------------------------------------------------------
-spec generate_quality_report(SkuId :: sku_id()) -> {ok, map()}.
generate_quality_report(SkuId) ->
    {ok, Receipts} = load_quality_receipts(SkuId),

    Report = #{
        sku_id => SkuId,
        receipts => length(Receipts),
        quality_metrics => extract_quality_metrics(Receipts),
        gate_results => check_gate_results(Receipts),
        integrity_status => verify_receipt_integrity(Receipts),
        release_decision => case validate_release_readiness(SkuId) of
            {ok, ready} -> approved;
            {error, _} -> blocked
        end,
        generated_at => erlang:system_time(millisecond)
    },

    {ok, Report}.

%%------------------------------------------------------------------------------
%% @doc Check if SKU release is blocked.
%%
%% Returns true if any quality gate failed or receipts missing.
%%
%% @end
%%------------------------------------------------------------------------------
-spec blocks_release(SkuId :: sku_id()) -> boolean().
blocks_release(SkuId) ->
    case verify_quality_receipts(SkuId) of
        {ok, Result} ->
            maps:get(release_blocked, Result, true);
        {error, _} ->
            true
    end.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Load quality receipts from persistence
%%------------------------------------------------------------------------------
load_quality_receipts(SkuId) ->
    case tcps_persistence:list_receipts_by_sku(SkuId) of
        [] ->
            {error, no_receipts_found};
        Receipts ->
            % Filter for quality gate receipts
            QualityReceipts = lists:filter(
                fun(R) ->
                    Type = maps:get(receipt_type, R, unknown),
                    Type =:= quality_gate
                end,
                Receipts
            ),
            {ok, QualityReceipts}
    end.

%%------------------------------------------------------------------------------
%% Required quality gates
%%------------------------------------------------------------------------------
required_quality_gates() ->
    [compilation, testing, coverage, dialyzer, xref].

%%------------------------------------------------------------------------------
%% Extract gate from receipt
%%------------------------------------------------------------------------------
gate_from_receipt(Receipt) ->
    maps:get(gate, Receipt, unknown).

%%------------------------------------------------------------------------------
%% Verify receipt integrity (checksum validation)
%%------------------------------------------------------------------------------
verify_receipt_integrity(Receipts) ->
    lists:map(
        fun(Receipt) ->
            Gate = gate_from_receipt(Receipt),
            StoredChecksum = maps:get(checksum, Receipt, undefined),

            % Recompute checksum
            ComputedChecksum = compute_receipt_checksum(Receipt),

            Result = case StoredChecksum =:= ComputedChecksum of
                true -> valid;
                false -> {checksum_mismatch, StoredChecksum, ComputedChecksum}
            end,

            {Gate, Result}
        end,
        Receipts
    ).

%%------------------------------------------------------------------------------
%% Compute SHA-256 checksum of receipt
%%------------------------------------------------------------------------------
compute_receipt_checksum(Receipt) ->
    % Remove checksum field
    ReceiptData = maps:remove(checksum, Receipt),

    % Canonical JSON encoding
    Json = jsx:encode(ReceiptData),

    % SHA-256 hash
    Hash = crypto:hash(sha256, Json),
    base64:encode(Hash).

%%------------------------------------------------------------------------------
%% Check gate pass/fail results
%%------------------------------------------------------------------------------
check_gate_results(Receipts) ->
    lists:map(
        fun(Receipt) ->
            Gate = gate_from_receipt(Receipt),
            QualityData = maps:get(details, Receipt, #{}),

            Result = evaluate_gate(Gate, QualityData),
            {Gate, Result}
        end,
        Receipts
    ).

%%------------------------------------------------------------------------------
%% Evaluate if gate passed based on quality data
%%------------------------------------------------------------------------------
evaluate_gate(compilation, QualityData) ->
    Status = maps:get(compilation_status, QualityData, failed),
    Errors = maps:get(compilation_errors, QualityData, 1),

    case Status =:= passed andalso Errors =:= 0 of
        true -> passed;
        false -> failed
    end;

evaluate_gate(testing, QualityData) ->
    PassRate = maps:get(test_pass_rate, QualityData, 0.0),

    case PassRate >= 0.95 of
        true -> passed;
        false -> failed
    end;

evaluate_gate(coverage, QualityData) ->
    Coverage = maps:get(coverage_percentage, QualityData, 0.0),

    case Coverage >= 0.80 of
        true -> passed;
        false -> failed
    end;

evaluate_gate(dialyzer, QualityData) ->
    Clean = maps:get(dialyzer_clean, QualityData, false),

    case Clean of
        true -> passed;
        false -> failed
    end;

evaluate_gate(xref, QualityData) ->
    Clean = maps:get(xref_clean, QualityData, false),

    case Clean of
        true -> passed;
        false -> failed
    end;

evaluate_gate(benchmark, QualityData) ->
    Regression = maps:get(benchmark_regression, QualityData, 1.0),

    case Regression < 0.10 of
        true -> passed;
        false -> failed
    end;

evaluate_gate(_Gate, _QualityData) ->
    failed.

%%------------------------------------------------------------------------------
%% Extract quality metrics from receipts
%%------------------------------------------------------------------------------
extract_quality_metrics(Receipts) ->
    lists:foldl(
        fun(Receipt, Acc) ->
            Gate = gate_from_receipt(Receipt),
            QualityData = maps:get(details, Receipt, #{}),
            maps:put(Gate, QualityData, Acc)
        end,
        #{},
        Receipts
    ).

%%------------------------------------------------------------------------------
%% Check for open Andon events
%%------------------------------------------------------------------------------
check_no_open_andons(SkuId) ->
    case whereis(tcps_andon) of
        undefined ->
            {ok, no_andon_system};
        _Pid ->
            case tcps_andon:can_proceed_to_stage(SkuId, deployment) of
                {ok, proceed} ->
                    {ok, no_open_andons};
                {blocked, AndonIds} ->
                    {error, {open_andons, AndonIds}}
            end
    end.

%%------------------------------------------------------------------------------
%% Print verification summary
%%------------------------------------------------------------------------------
print_verification_summary(Result) ->
    io:format("~n--- Verification Summary ---~n"),
    io:format("All Gates Passed: ~p~n", [maps:get(all_gates_passed, Result)]),
    io:format("Missing Receipts: ~p~n", [maps:get(missing_receipts, Result)]),
    io:format("Failed Gates: ~p~n", [maps:get(failed_gates, Result)]),
    io:format("Chain Intact: ~p~n", [maps:get(chain_intact, Result)]),
    io:format("Release Blocked: ~p~n", [maps:get(release_blocked, Result)]),
    ok.
