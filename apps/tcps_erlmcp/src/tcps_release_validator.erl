%%%-----------------------------------------------------------------------------
%%% @doc TCPS Release Validation System
%%%
%%% Validates release readiness and blocks releases if validation fails.
%%% Ensures complete receipt chain and all quality gates passed.
%%%
%%% Core Responsibilities:
%%% - Validate release readiness before SKU certification
%%% - Check receipt chain completeness
%%% - Ensure all quality gates passed
%%% - Block release if validation fails
%%% - Generate release validation reports
%%%
%%% Validation Checks:
%%% 1. Receipt chain complete (all stages present)
%%% 2. Quality gate receipts present and valid
%%% 3. All quality gates passed (compilation, tests, coverage, etc.)
%%% 4. No open Andon events
%%% 5. Receipt chain integrity (SHA-256 checksums)
%%% 6. Chronological ordering (no timestamp gaps)
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_release_validator).

%% API exports
-export([
    validate_release/1,
    check_receipt_chain_complete/1,
    check_all_quality_gates_passed/1,
    blocks_release_certification/1,
    generate_release_report/1
]).

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type sku_id() :: binary().

-type validation_result() :: #{
    sku_id := sku_id(),
    release_ready := boolean(),
    checks := #{atom() => passed | failed},
    blocking_issues := [term()],
    validated_at := integer()
}.

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Validate SKU for release.
%%
%% Comprehensive validation:
%% - Receipt chain complete
%% - All quality gates passed
%% - No open Andon events
%% - Chain integrity verified
%%
%% BLOCKS release if any check fails.
%%
%% @end
%%------------------------------------------------------------------------------
-spec validate_release(SkuId :: sku_id()) ->
    {ok, validation_result()} | {error, {blocked, validation_result()}}.
validate_release(SkuId) ->
    io:format("~n=== TCPS Release Validation for SKU: ~s ===~n", [SkuId]),

    % Run all validation checks
    Checks = #{
        receipt_chain_complete => check_receipt_chain_complete(SkuId),
        quality_gates_passed => check_all_quality_gates_passed(SkuId),
        no_open_andons => check_no_open_andons(SkuId),
        chain_integrity => check_chain_integrity(SkuId),
        chronological_order => check_chronological_order(SkuId)
    },

    % Determine which checks passed/failed
    CheckResults = maps:map(
        fun(_Name, Result) ->
            case Result of
                {ok, _} -> passed;
                ok -> passed;
                _ -> failed
            end
        end,
        Checks
    ),

    % Find blocking issues
    BlockingIssues = lists:filtermap(
        fun({CheckName, CheckResult}) ->
            case CheckResult of
                {ok, _} -> false;
                ok -> false;
                {error, Reason} -> {true, {CheckName, Reason}}
            end
        end,
        maps:to_list(Checks)
    ),

    % Determine if release is ready
    ReleaseReady = BlockingIssues =:= [],

    Result = #{
        sku_id => SkuId,
        release_ready => ReleaseReady,
        checks => CheckResults,
        blocking_issues => BlockingIssues,
        validated_at => erlang:system_time(millisecond)
    },

    % Print results
    print_validation_results(Result),

    case ReleaseReady of
        true ->
            io:format("~n✓ RELEASE VALIDATION PASSED~n"),
            {ok, Result};
        false ->
            io:format("~n✗ RELEASE VALIDATION FAILED - RELEASE BLOCKED~n"),
            {error, {blocked, Result}}
    end.

%%------------------------------------------------------------------------------
%% @doc Check receipt chain is complete.
%%
%% Verifies all required receipts present:
%% - shacl_validation
%% - compilation
%% - testing
%% - security
%% - deterministic_build
%% - quality (quality gate receipts)
%% - release_verification
%%
%% @end
%%------------------------------------------------------------------------------
-spec check_receipt_chain_complete(SkuId :: sku_id()) ->
    {ok, complete} | {error, {incomplete, Missing :: [atom()]}}.
check_receipt_chain_complete(SkuId) ->
    case tcps_receipt:verify_chain(SkuId) of
        {ok, complete} ->
            {ok, complete};
        {error, {incomplete, Missing}} ->
            {error, {incomplete, Missing}}
    end.

%%------------------------------------------------------------------------------
%% @doc Check all quality gates passed.
%%
%% Delegates to tcps_quality_receipt_verifier for comprehensive check.
%%
%% @end
%%------------------------------------------------------------------------------
-spec check_all_quality_gates_passed(SkuId :: sku_id()) ->
    {ok, all_passed} | {error, term()}.
check_all_quality_gates_passed(SkuId) ->
    tcps_quality_receipt_verifier:verify_all_gates_passed(SkuId).

%%------------------------------------------------------------------------------
%% @doc Check if release certification should be blocked.
%%
%% Returns true if any validation check fails.
%%
%% @end
%%------------------------------------------------------------------------------
-spec blocks_release_certification(SkuId :: sku_id()) -> boolean().
blocks_release_certification(SkuId) ->
    case validate_release(SkuId) of
        {ok, _Result} ->
            false;
        {error, {blocked, _Result}} ->
            true
    end.

%%------------------------------------------------------------------------------
%% @doc Generate release validation report.
%%
%% Detailed report including:
%% - All validation check results
%% - Quality gate summary
%% - Receipt chain status
%% - Blocking issues (if any)
%% - Release decision
%%
%% @end
%%------------------------------------------------------------------------------
-spec generate_release_report(SkuId :: sku_id()) -> {ok, map()}.
generate_release_report(SkuId) ->
    % Run validation
    ValidationResult = case validate_release(SkuId) of
        {ok, Result} -> Result;
        {error, {blocked, Result}} -> Result
    end,

    % Get quality report
    {ok, QualityReport} = tcps_quality_receipt_verifier:generate_quality_report(SkuId),

    % Get receipt chain audit
    AuditTrail = tcps_receipt:generate_audit_trail(SkuId),

    Report = #{
        sku_id => SkuId,
        validation_result => ValidationResult,
        quality_report => QualityReport,
        audit_trail => AuditTrail,
        release_decision => case maps:get(release_ready, ValidationResult) of
            true -> approved;
            false -> blocked
        end,
        generated_at => erlang:system_time(millisecond)
    },

    {ok, Report}.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

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
%% Check receipt chain integrity
%%------------------------------------------------------------------------------
check_chain_integrity(SkuId) ->
    tcps_quality_receipt_verifier:check_receipt_chain_integrity(SkuId).

%%------------------------------------------------------------------------------
%% Check chronological order
%%------------------------------------------------------------------------------
check_chronological_order(SkuId) ->
    tcps_receipt:verify_chronological(SkuId).

%%------------------------------------------------------------------------------
%% Print validation results
%%------------------------------------------------------------------------------
print_validation_results(Result) ->
    io:format("~n--- Release Validation Results ---~n"),
    io:format("Release Ready: ~p~n", [maps:get(release_ready, Result)]),

    io:format("~nCheck Results:~n"),
    Checks = maps:get(checks, Result),
    maps:foreach(
        fun(CheckName, CheckResult) ->
            Symbol = case CheckResult of
                passed -> "✓";
                failed -> "✗"
            end,
            io:format("  ~s ~p~n", [Symbol, CheckName])
        end,
        Checks
    ),

    BlockingIssues = maps:get(blocking_issues, Result),
    case BlockingIssues of
        [] ->
            io:format("~nNo blocking issues~n");
        _ ->
            io:format("~nBlocking Issues:~n"),
            lists:foreach(
                fun({Issue, Reason}) ->
                    io:format("  ✗ ~p: ~p~n", [Issue, Reason])
                end,
                BlockingIssues
            )
    end,
    ok.
