%%%-------------------------------------------------------------------
%%% @doc TCPS Quality Gates Test Suite
%%%
%%% Comprehensive testing of quality gate enforcement including:
%%% - Coverage threshold enforcement (80%+)
%%% - Compilation error detection
%%% - Test failure blocking
%%% - Security vulnerability detection
%%% - SHACL validation enforcement
%%% - Deterministic build verification
%%% - Multi-gate failures
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_quality_gates_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT callbacks
-export([all/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Test cases
-export([
    test_coverage_below_80_triggers_andon/1,
    test_coverage_at_80_passes/1,
    test_coverage_above_80_passes/1,
    test_compilation_errors_trigger_andon/1,
    test_compilation_warnings_logged/1,
    test_test_failures_trigger_andon/1,
    test_security_vulnerabilities_trigger_andon/1,
    test_shacl_violations_trigger_andon/1,
    test_non_deterministic_build_fails/1,
    test_multi_gate_failures/1,
    test_quality_gate_order/1,
    test_quality_metrics_tracking/1,
    test_gate_bypass_prevention/1,
    test_incremental_quality_improvement/1,
    test_zero_tolerance_security/1
]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

suite() ->
    [{timetrap, {minutes, 5}}].

all() ->
    [
        test_coverage_below_80_triggers_andon,
        test_coverage_at_80_passes,
        test_coverage_above_80_passes,
        test_compilation_errors_trigger_andon,
        test_compilation_warnings_logged,
        test_test_failures_trigger_andon,
        test_security_vulnerabilities_trigger_andon,
        test_shacl_violations_trigger_andon,
        test_non_deterministic_build_fails,
        test_multi_gate_failures,
        test_quality_gate_order,
        test_quality_metrics_tracking,
        test_gate_bypass_prevention,
        test_incremental_quality_improvement,
        test_zero_tolerance_security
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlmcp),
    ok = tcps_test_utils:init_test_env(),
    Config.

end_per_suite(_Config) ->
    ok = tcps_test_utils:cleanup_test_env(),
    ok = application:stop(tcps),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("~n=== Starting test case: ~p ===~n", [TestCase]),
    ok = tcps_test_utils:clear_all_data(),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("~n=== Completed test case: ~p ===~n", [TestCase]),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test that coverage below 80% triggers Andon
%% @end
%%--------------------------------------------------------------------
test_coverage_below_80_triggers_andon(_Config) ->
    ct:pal("~n=== Testing Coverage Below 80% Triggers Andon ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Run tests with 75% coverage (below threshold)
    ok = tcps_test_utils:inject_low_coverage(WorkOrderId, 75.0),
    {ok, _TestResults} = tcps_test_utils:run_tests(WorkOrderId),

    %% Run quality gates
    {error, quality_gate_failed} = tcps_quality:check_gates(WorkOrderId),
    ct:pal("Quality gate failed as expected~n"),

    %% Verify Andon triggered
    {ok, AndonId} = tcps_test_utils:wait_for_andon_type(low_coverage),
    {ok, Andon} = tcps_andon:get(AndonId),

    ?assertEqual(low_coverage, maps:get(type, Andon)),
    ?assertEqual(WorkOrderId, maps:get(work_order_id, Andon)),

    Details = maps:get(details, Andon),
    ?assertEqual(75.0, maps:get(actual_coverage, Details)),
    ?assertEqual(80.0, maps:get(required_coverage, Details)),

    ct:pal("Low coverage Andon triggered: 75.0% < 80.0%~n"),

    %% Verify pipeline blocked
    ?assertMatch({blocked, _}, tcps_andon:can_proceed(WorkOrderId, release)),

    ct:pal("~n=== Coverage Below 80% Triggers Andon: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that coverage at exactly 80% passes
%% @end
%%--------------------------------------------------------------------
test_coverage_at_80_passes(_Config) ->
    ct:pal("~n=== Testing Coverage At 80% Passes ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Run tests with exactly 80% coverage
    ok = tcps_test_utils:inject_coverage(WorkOrderId, 80.0),
    {ok, _TestResults} = tcps_test_utils:run_tests(WorkOrderId),

    %% Run quality gates
    {ok, pass} = tcps_quality:check_gates(WorkOrderId),
    ct:pal("Quality gate passed with 80.0% coverage~n"),

    %% Verify no Andon triggered
    {error, no_andon} = tcps_test_utils:check_for_andon(WorkOrderId),

    %% Verify pipeline not blocked
    ?assertEqual({ok, proceed}, tcps_andon:can_proceed(WorkOrderId, release)),

    ct:pal("~n=== Coverage At 80% Passes: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that coverage above 80% passes
%% @end
%%--------------------------------------------------------------------
test_coverage_above_80_passes(_Config) ->
    ct:pal("~n=== Testing Coverage Above 80% Passes ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Run tests with 95% coverage
    ok = tcps_test_utils:inject_coverage(WorkOrderId, 95.0),
    {ok, TestResults} = tcps_test_utils:run_tests(WorkOrderId),

    ?assertEqual(95.0, maps:get(coverage, TestResults)),

    %% Run quality gates
    {ok, pass} = tcps_quality:check_gates(WorkOrderId),
    ct:pal("Quality gate passed with 95.0% coverage~n"),

    %% Verify no Andon
    {error, no_andon} = tcps_test_utils:check_for_andon(WorkOrderId),

    ct:pal("~n=== Coverage Above 80% Passes: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that compilation errors trigger Andon
%% @end
%%--------------------------------------------------------------------
test_compilation_errors_trigger_andon(_Config) ->
    ct:pal("~n=== Testing Compilation Errors Trigger Andon ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Inject compilation errors
    ok = tcps_test_utils:inject_compilation_error(WorkOrderId, #{
        file => "src/tcps_core.erl",
        line => 42,
        error => "syntax error before: ')'",
        type => syntax_error
    }),

    ok = tcps_test_utils:inject_compilation_error(WorkOrderId, #{
        file => "src/tcps_kanban.erl",
        line => 108,
        error => "undefined function calculate/2",
        type => undefined_function
    }),

    %% Run compilation
    {error, compilation_failed} = tcps_test_utils:run_compilation(WorkOrderId),

    %% Verify Andon triggered
    {ok, AndonId} = tcps_test_utils:wait_for_andon_type(compilation_error),
    {ok, Andon} = tcps_andon:get(AndonId),

    ?assertEqual(compilation_error, maps:get(type, Andon)),

    Details = maps:get(details, Andon),
    ?assertEqual(2, maps:get(error_count, Details)),
    ?assert(maps:is_key(errors, Details)),

    ct:pal("Compilation error Andon triggered with 2 errors~n"),

    %% Verify quality gate blocked
    {error, quality_gate_failed} = tcps_quality:check_gates(WorkOrderId),

    ct:pal("~n=== Compilation Errors Trigger Andon: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that compilation warnings are logged but don't block
%% @end
%%--------------------------------------------------------------------
test_compilation_warnings_logged(_Config) ->
    ct:pal("~n=== Testing Compilation Warnings Logged ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Inject compilation warnings
    ok = tcps_test_utils:inject_compilation_warning(WorkOrderId, #{
        file => "src/helper.erl",
        line => 23,
        warning => "unused variable 'Temp'"
    }),

    %% Run compilation (should succeed with warnings)
    {ok, CompileResult} = tcps_test_utils:run_compilation(WorkOrderId),
    ?assertEqual(success, maps:get(result, CompileResult)),
    ?assertEqual(1, maps:get(warning_count, CompileResult)),

    %% Quality gates should pass
    {ok, pass} = tcps_quality:check_gates(WorkOrderId),
    ct:pal("Quality gate passed with warnings~n"),

    %% Verify no Andon
    {error, no_andon} = tcps_test_utils:check_for_andon(WorkOrderId),

    %% Verify warnings recorded
    {ok, Receipts} = tcps_persistence:get_receipts_by_work_order(WorkOrderId),
    CompileReceipts = [R || R <- Receipts, maps:get(stage, R) =:= compile],
    [CompileReceipt] = CompileReceipts,

    Metadata = maps:get(metadata, CompileReceipt),
    ?assertEqual(1, maps:get(warnings, Metadata)),

    ct:pal("Warnings logged in receipt~n"),
    ct:pal("~n=== Compilation Warnings Logged: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that test failures trigger Andon
%% @end
%%--------------------------------------------------------------------
test_test_failures_trigger_andon(_Config) ->
    ct:pal("~n=== Testing Test Failures Trigger Andon ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Inject test failures
    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{
        test_name => "test_authentication",
        error => "Expected 200, got 401",
        module => "auth_SUITE",
        line => 45
    }),

    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{
        test_name => "test_database_connection",
        error => "Connection timeout",
        module => "db_SUITE",
        line => 78
    }),

    %% Run tests
    {error, test_failure} = tcps_test_utils:run_tests(WorkOrderId),

    %% Verify Andon triggered
    {ok, AndonId} = tcps_test_utils:wait_for_andon_type(test_failure),
    {ok, Andon} = tcps_andon:get(AndonId),

    Details = maps:get(details, Andon),
    ?assertEqual(2, maps:get(failed_tests, Details)),

    ct:pal("Test failure Andon triggered with 2 failures~n"),

    %% Verify quality gate blocked
    {error, quality_gate_failed} = tcps_quality:check_gates(WorkOrderId),

    ct:pal("~n=== Test Failures Trigger Andon: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that security vulnerabilities trigger Andon
%% @end
%%--------------------------------------------------------------------
test_security_vulnerabilities_trigger_andon(_Config) ->
    ct:pal("~n=== Testing Security Vulnerabilities Trigger Andon ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Inject security vulnerabilities
    ok = tcps_test_utils:inject_security_vulnerability(WorkOrderId, #{
        type => sql_injection,
        severity => high,
        file => "src/database.erl",
        line => 156,
        description => "Unsanitized user input in SQL query",
        cve => "CVE-2024-1234"
    }),

    ok = tcps_test_utils:inject_security_vulnerability(WorkOrderId, #{
        type => xss,
        severity => medium,
        file => "src/web_handler.erl",
        line => 89,
        description => "Unescaped user input in HTML output"
    }),

    %% Run security scan
    {error, security_issues_found} = tcps_test_utils:run_security_scan(WorkOrderId),

    %% Verify Andon triggered
    {ok, AndonId} = tcps_test_utils:wait_for_andon_type(security_vulnerability),
    {ok, Andon} = tcps_andon:get(AndonId),

    Details = maps:get(details, Andon),
    ?assertEqual(2, maps:get(vulnerability_count, Details)),

    %% Verify high severity flagged
    ?assert(lists:member(high, maps:get(severities, Details))),

    ct:pal("Security vulnerability Andon triggered~n"),

    %% Verify quality gate blocked
    {error, quality_gate_failed} = tcps_quality:check_gates(WorkOrderId),

    ct:pal("~n=== Security Vulnerabilities Trigger Andon: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that SHACL violations trigger Andon
%% @end
%%--------------------------------------------------------------------
test_shacl_violations_trigger_andon(_Config) ->
    ct:pal("~n=== Testing SHACL Violations Trigger Andon ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Inject SHACL violations
    ok = tcps_test_utils:inject_shacl_violation(WorkOrderId, #{
        shape => "tcps:WorkOrderShape",
        property => "tcps:estimatedEffort",
        constraint => "minInclusive",
        message => "Estimated effort must be >= 1"
    }),

    %% Run SHACL validation
    {error, validation_failed} = tcps_test_utils:run_shacl_validation(WorkOrderId),

    %% Verify Andon triggered
    {ok, AndonId} = tcps_test_utils:wait_for_andon_type(shacl_violation),
    {ok, Andon} = tcps_andon:get(AndonId),

    Details = maps:get(details, Andon),
    ?assertEqual(1, maps:get(violation_count, Details)),

    ct:pal("SHACL violation Andon triggered~n"),

    %% Verify quality gate blocked
    {error, quality_gate_failed} = tcps_quality:check_gates(WorkOrderId),

    ct:pal("~n=== SHACL Violations Trigger Andon: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that non-deterministic builds fail quality gate
%% @end
%%--------------------------------------------------------------------
test_non_deterministic_build_fails(_Config) ->
    ct:pal("~n=== Testing Non-Deterministic Build Fails ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),
    ok = tcps_test_utils:process_pipeline_stages(WorkOrderId),

    %% Inject non-determinism (e.g., timestamp in build)
    ok = tcps_test_utils:inject_timestamp_in_build(WorkOrderId),

    %% Verify build
    {ok, BuildResult} = tcps_deterministic:verify_build(WorkOrderId),
    ?assertEqual(non_deterministic, maps:get(result, BuildResult)),

    ct:pal("Non-deterministic build detected~n"),

    %% Verify Andon triggered
    {ok, AndonId} = tcps_test_utils:wait_for_andon_type(non_deterministic_build),
    {ok, Andon} = tcps_andon:get(AndonId),

    Details = maps:get(details, Andon),
    ?assert(maps:is_key(hash_mismatch, Details)),

    ct:pal("Non-deterministic build Andon triggered~n"),

    %% Verify quality gate blocked
    {error, quality_gate_failed} = tcps_quality:check_gates(WorkOrderId),

    ct:pal("~n=== Non-Deterministic Build Fails: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test multiple gate failures at once
%% @end
%%--------------------------------------------------------------------
test_multi_gate_failures(_Config) ->
    ct:pal("~n=== Testing Multi-Gate Failures ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Inject multiple failures
    ok = tcps_test_utils:inject_low_coverage(WorkOrderId, 70.0),
    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{}),
    ok = tcps_test_utils:inject_security_vulnerability(WorkOrderId, #{}),

    %% Run quality gates
    {error, quality_gate_failed} = tcps_quality:check_gates(WorkOrderId),

    %% Verify multiple Andons triggered
    timer:sleep(1000), % Wait for all Andons to trigger
    OpenAndons = tcps_andon:list_open(),
    WorkOrderAndons = [A || A <- OpenAndons,
                            {ok, AndonData} <- [tcps_andon:get(A)],
                            maps:get(work_order_id, AndonData) =:= WorkOrderId],

    ?assert(length(WorkOrderAndons) >= 2),
    ct:pal("Multiple Andons triggered: ~p~n", [length(WorkOrderAndons)]),

    %% Verify types
    AndonTypes = lists:map(fun(AndonId) ->
        {ok, Andon} = tcps_andon:get(AndonId),
        maps:get(type, Andon)
    end, WorkOrderAndons),

    ?assert(lists:member(low_coverage, AndonTypes) orelse
            lists:member(test_failure, AndonTypes)),
    ?assert(lists:member(security_vulnerability, AndonTypes)),

    ct:pal("Andon types: ~p~n", [AndonTypes]),

    ct:pal("~n=== Multi-Gate Failures: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test quality gate execution order
%% @end
%%--------------------------------------------------------------------
test_quality_gate_order(_Config) ->
    ct:pal("~n=== Testing Quality Gate Order ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Quality gates should run in order:
    %% 1. SHACL validation
    %% 2. Compilation
    %% 3. Tests
    %% 4. Coverage
    %% 5. Security
    %% 6. Deterministic build

    %% Process with tracking
    ok = tcps_test_utils:enable_gate_tracking(WorkOrderId),
    ok = tcps_test_utils:process_pipeline_stages(WorkOrderId),
    {ok, pass} = tcps_quality:check_gates(WorkOrderId),

    %% Verify execution order
    {ok, GateOrder} = tcps_test_utils:get_gate_execution_order(WorkOrderId),

    ExpectedOrder = [shacl, compile, test, coverage, security, deterministic],
    ?assertEqual(ExpectedOrder, GateOrder),

    ct:pal("Quality gates executed in correct order~n"),

    ct:pal("~n=== Quality Gate Order: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test quality metrics tracking
%% @end
%%--------------------------------------------------------------------
test_quality_metrics_tracking(_Config) ->
    ct:pal("~n=== Testing Quality Metrics Tracking ===~n"),

    %% Reset metrics
    ok = tcps_quality:reset_metrics(),

    %% Process work orders with various quality levels
    %% High quality
    {ok, WO1} = tcps_test_utils:create_test_work_order(),
    ok = tcps_test_utils:inject_coverage(WO1, 95.0),
    ok = tcps_test_utils:process_work_order_full(WO1),

    %% Medium quality (low coverage, but passes)
    {ok, WO2} = tcps_test_utils:create_test_work_order(),
    ok = tcps_test_utils:inject_coverage(WO2, 82.0),
    ok = tcps_test_utils:process_work_order_full(WO2),

    %% Failed quality (below threshold)
    {ok, WO3} = tcps_test_utils:create_test_work_order(),
    ok = tcps_test_utils:inject_low_coverage(WO3, 70.0),
    {error, quality_gate_failed} = tcps_quality:check_gates(WO3),

    %% Get metrics
    Metrics = tcps_quality:get_metrics(),

    ?assertEqual(2, maps:get(passed_gates, Metrics)),
    ?assertEqual(1, maps:get(failed_gates, Metrics)),

    AvgCoverage = maps:get(avg_coverage, Metrics),
    ?assert(AvgCoverage >= 82.0), % Average of successful ones

    ct:pal("Quality metrics: ~p~n", [Metrics]),

    ct:pal("~n=== Quality Metrics Tracking: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that quality gates cannot be bypassed
%% @end
%%--------------------------------------------------------------------
test_gate_bypass_prevention(_Config) ->
    ct:pal("~n=== Testing Gate Bypass Prevention ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Inject low coverage
    ok = tcps_test_utils:inject_low_coverage(WorkOrderId, 60.0),

    %% Try to skip directly to release
    {error, quality_gate_not_passed} =
        tcps_test_utils:attempt_release(WorkOrderId),

    ct:pal("Cannot release without passing quality gates~n"),

    %% Try to manually mark as passed
    {error, unauthorized} =
        tcps_test_utils:attempt_manual_override(WorkOrderId),

    ct:pal("Cannot manually override quality gates~n"),

    %% Verify work order still blocked
    {ok, WO} = tcps_work_order:get(WorkOrderId),
    ?assertNotEqual(completed, maps:get(status, WO)),

    ct:pal("~n=== Gate Bypass Prevention: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test incremental quality improvement
%% @end
%%--------------------------------------------------------------------
test_incremental_quality_improvement(_Config) ->
    ct:pal("~n=== Testing Incremental Quality Improvement ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% First attempt: 70% coverage (fails)
    ok = tcps_test_utils:inject_coverage(WorkOrderId, 70.0),
    {error, quality_gate_failed} = tcps_quality:check_gates(WorkOrderId),
    ct:pal("First attempt failed: 70.0%~n"),

    %% Second attempt: 75% coverage (still fails)
    ok = tcps_test_utils:inject_coverage(WorkOrderId, 75.0),
    {error, quality_gate_failed} = tcps_quality:check_gates(WorkOrderId),
    ct:pal("Second attempt failed: 75.0%~n"),

    %% Third attempt: 82% coverage (passes)
    ok = tcps_test_utils:inject_coverage(WorkOrderId, 82.0),
    {ok, pass} = tcps_quality:check_gates(WorkOrderId),
    ct:pal("Third attempt passed: 82.0%~n"),

    %% Verify improvement tracked
    {ok, QualityHistory} = tcps_quality:get_history(WorkOrderId),
    ?assertEqual(3, length(QualityHistory)),

    Coverages = [maps:get(coverage, H) || H <- QualityHistory],
    ?assertEqual([70.0, 75.0, 82.0], Coverages),

    ct:pal("Quality improvement tracked: 70%% -> 75%% -> 82%%~n"),

    ct:pal("~n=== Incremental Quality Improvement: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test zero tolerance for security issues
%% @end
%%--------------------------------------------------------------------
test_zero_tolerance_security(_Config) ->
    ct:pal("~n=== Testing Zero Tolerance Security ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Perfect quality except one low-severity security issue
    ok = tcps_test_utils:inject_coverage(WorkOrderId, 95.0),
    ok = tcps_test_utils:inject_security_vulnerability(WorkOrderId, #{
        type => information_disclosure,
        severity => low,
        file => "src/logger.erl",
        line => 45,
        description => "Logs may contain sensitive data"
    }),

    %% Should still fail quality gate (zero tolerance)
    {error, quality_gate_failed} = tcps_quality:check_gates(WorkOrderId),
    ct:pal("Quality gate failed despite high coverage~n"),

    %% Verify Andon triggered even for low severity
    {ok, AndonId} = tcps_test_utils:wait_for_andon_type(security_vulnerability),
    {ok, Andon} = tcps_andon:get(AndonId),

    Details = maps:get(details, Andon),
    ?assertEqual(low, maps:get(max_severity, Details)),

    ct:pal("Even low-severity security issues trigger Andon~n"),

    ct:pal("~n=== Zero Tolerance Security: SUCCESS ===~n"),
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================
