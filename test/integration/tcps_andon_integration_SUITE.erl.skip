%%%-------------------------------------------------------------------
%%% @doc TCPS Andon Integration Test Suite
%%%
%%% Comprehensive testing of Andon cord (stop-the-line) functionality
%%% including:
%%% - Automatic triggering on quality issues
%%% - Pipeline blocking and resumption
%%% - 5 Whys root cause analysis
%%% - Concurrent Andon handling
%%% - Escalation workflows
%%% - Metrics and tracking
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_andon_integration_SUITE).

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
    test_andon_stops_pipeline/1,
    test_test_failure_triggers_andon/1,
    test_compilation_error_triggers_andon/1,
    test_low_coverage_triggers_andon/1,
    test_security_vulnerability_triggers_andon/1,
    test_5_whys_analysis/1,
    test_andon_blocking/1,
    test_andon_resolution/1,
    test_concurrent_andons/1,
    test_andon_escalation/1,
    test_andon_metrics/1,
    test_andon_notification/1,
    test_andon_history/1,
    test_multiple_failures_single_andon/1,
    test_andon_timeout/1
]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

suite() ->
    [{timetrap, {minutes, 5}}].

all() ->
    [
        test_andon_stops_pipeline,
        test_test_failure_triggers_andon,
        test_compilation_error_triggers_andon,
        test_low_coverage_triggers_andon,
        test_security_vulnerability_triggers_andon,
        test_5_whys_analysis,
        test_andon_blocking,
        test_andon_resolution,
        test_concurrent_andons,
        test_andon_escalation,
        test_andon_metrics,
        test_andon_notification,
        test_andon_history,
        test_multiple_failures_single_andon,
        test_andon_timeout
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
    ok = tcps_andon:reset_metrics(),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("~n=== Completed test case: ~p ===~n", [TestCase]),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test that Andon stops the pipeline
%% @end
%%--------------------------------------------------------------------
test_andon_stops_pipeline(_Config) ->
    ct:pal("~n=== Testing Andon Stops Pipeline ===~n"),

    %% Create and start work order
    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),
    ct:pal("Work order started: ~p~n", [WorkOrderId]),

    %% Inject test failure
    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{
        test_name => "test_authentication",
        error => "Expected 200, got 401",
        file => "test/auth_test.erl",
        line => 42
    }),
    ct:pal("Test failure injected~n"),

    %% Run tests (should trigger Andon)
    {error, test_failure} = tcps_test_utils:run_tests(WorkOrderId),
    ct:pal("Tests failed as expected~n"),

    %% Verify Andon triggered
    {ok, AndonId} = tcps_test_utils:wait_for_andon(WorkOrderId),
    {ok, Andon} = tcps_andon:get(AndonId),
    ct:pal("Andon triggered: ~p~n", [AndonId]),

    %% Verify Andon details
    ?assertEqual(WorkOrderId, maps:get(work_order_id, Andon)),
    ?assertEqual(test_failure, maps:get(type, Andon)),
    ?assertEqual(open, maps:get(status, Andon)),
    ?assert(maps:is_key(timestamp, Andon)),

    %% Verify pipeline blocked
    BlockResult = tcps_andon:can_proceed(WorkOrderId, release),
    ?assertMatch({blocked, _}, BlockResult),
    {blocked, BlockedStages} = BlockResult,
    ?assert(length(BlockedStages) > 0),
    ct:pal("Pipeline blocked at stages: ~p~n", [BlockedStages]),

    %% Verify all downstream stages blocked
    ?assertMatch({blocked, _}, tcps_andon:can_proceed(WorkOrderId, test)),
    ?assertMatch({blocked, _}, tcps_andon:can_proceed(WorkOrderId, quality)),
    ?assertMatch({blocked, _}, tcps_andon:can_proceed(WorkOrderId, release)),
    ?assertMatch({blocked, _}, tcps_andon:can_proceed(WorkOrderId, publish)),
    ct:pal("All downstream stages blocked~n"),

    ct:pal("~n=== Andon Stops Pipeline: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that test failures trigger Andon
%% @end
%%--------------------------------------------------------------------
test_test_failure_triggers_andon(_Config) ->
    ct:pal("~n=== Testing Test Failure Triggers Andon ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Inject multiple test failures
    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{
        test_name => "test_api_endpoint",
        error => "Connection refused"
    }),
    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{
        test_name => "test_database_query",
        error => "Timeout after 5000ms"
    }),

    %% Run tests
    {error, test_failure} = tcps_test_utils:run_tests(WorkOrderId),

    %% Verify Andon triggered with details
    {ok, AndonId} = tcps_test_utils:wait_for_andon(WorkOrderId),
    {ok, Andon} = tcps_andon:get(AndonId),

    ?assertEqual(test_failure, maps:get(type, Andon)),

    Details = maps:get(details, Andon),
    ?assertEqual(2, maps:get(failed_tests, Details)),
    ?assert(maps:is_key(error_messages, Details)),

    ct:pal("Test failures properly captured in Andon~n"),
    ct:pal("~n=== Test Failure Triggers Andon: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that compilation errors trigger Andon
%% @end
%%--------------------------------------------------------------------
test_compilation_error_triggers_andon(_Config) ->
    ct:pal("~n=== Testing Compilation Error Triggers Andon ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Inject compilation error
    ok = tcps_test_utils:inject_compilation_error(WorkOrderId, #{
        file => "src/auth.erl",
        line => 123,
        error => "syntax error before: ')'",
        type => syntax_error
    }),

    %% Run compilation
    {error, compilation_failed} = tcps_test_utils:run_compilation(WorkOrderId),

    %% Verify Andon triggered
    {ok, AndonId} = tcps_test_utils:wait_for_andon_type(compilation_error),
    {ok, Andon} = tcps_andon:get(AndonId),

    ?assertEqual(compilation_error, maps:get(type, Andon)),

    Details = maps:get(details, Andon),
    ?assertEqual("src/auth.erl", maps:get(file, Details)),
    ?assertEqual(123, maps:get(line, Details)),

    ct:pal("Compilation error properly captured~n"),
    ct:pal("~n=== Compilation Error Triggers Andon: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that low coverage triggers Andon
%% @end
%%--------------------------------------------------------------------
test_low_coverage_triggers_andon(_Config) ->
    ct:pal("~n=== Testing Low Coverage Triggers Andon ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Inject low coverage (75% < 80% threshold)
    ok = tcps_test_utils:inject_low_coverage(WorkOrderId, 75.0),

    %% Run quality gates
    {error, quality_gate_failed} = tcps_quality:check_gates(WorkOrderId),

    %% Verify Andon triggered
    {ok, AndonId} = tcps_test_utils:wait_for_andon_type(low_coverage),
    {ok, Andon} = tcps_andon:get(AndonId),

    ?assertEqual(low_coverage, maps:get(type, Andon)),

    Details = maps:get(details, Andon),
    ?assertEqual(75.0, maps:get(actual_coverage, Details)),
    ?assertEqual(80.0, maps:get(required_coverage, Details)),

    ct:pal("Low coverage properly detected: 75.0% < 80.0%~n"),
    ct:pal("~n=== Low Coverage Triggers Andon: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that security vulnerabilities trigger Andon
%% @end
%%--------------------------------------------------------------------
test_security_vulnerability_triggers_andon(_Config) ->
    ct:pal("~n=== Testing Security Vulnerability Triggers Andon ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Inject security vulnerability
    ok = tcps_test_utils:inject_security_vulnerability(WorkOrderId, #{
        type => sql_injection,
        severity => high,
        file => "src/database.erl",
        line => 89,
        description => "Unsanitized user input in SQL query"
    }),

    %% Run security scan
    {error, security_issues_found} = tcps_test_utils:run_security_scan(WorkOrderId),

    %% Verify Andon triggered
    {ok, AndonId} = tcps_test_utils:wait_for_andon_type(security_vulnerability),
    {ok, Andon} = tcps_andon:get(AndonId),

    ?assertEqual(security_vulnerability, maps:get(type, Andon)),

    Details = maps:get(details, Andon),
    ?assertEqual(sql_injection, maps:get(vulnerability_type, Details)),
    ?assertEqual(high, maps:get(severity, Details)),

    ct:pal("Security vulnerability properly detected~n"),
    ct:pal("~n=== Security Vulnerability Triggers Andon: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test 5 Whys root cause analysis
%% @end
%%--------------------------------------------------------------------
test_5_whys_analysis(_Config) ->
    ct:pal("~n=== Testing 5 Whys Analysis ===~n"),

    %% Create Andon
    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{}),
    {ok, AndonId} = tcps_test_utils:wait_for_andon(WorkOrderId),

    %% Perform 5 Whys analysis
    Analysis = [
        #{
            question => "Why did the test fail?",
            answer => "Because the API returned 500 instead of 200"
        },
        #{
            question => "Why did the API return 500?",
            answer => "Because the database query timed out"
        },
        #{
            question => "Why did the database query timeout?",
            answer => "Because there was no index on the queried column"
        },
        #{
            question => "Why was there no index?",
            answer => "Because the migration script was incomplete"
        },
        #{
            question => "Why was the migration script incomplete?",
            answer => "Because the database schema review was skipped"
        }
    ],

    {ok, AnalysisId} = tcps_andon:add_5_whys_analysis(AndonId, #{
        questions_and_answers => Analysis,
        root_cause => "Database schema review process was skipped",
        corrective_action => "Implement mandatory schema review checklist",
        preventive_action => "Add automated index recommendation tool"
    }),

    ct:pal("5 Whys analysis added: ~p~n", [AnalysisId]),

    %% Verify analysis stored
    {ok, Andon} = tcps_andon:get(AndonId),
    ?assert(maps:is_key(analysis, Andon)),

    AnalysisData = maps:get(analysis, Andon),
    ?assertEqual(5, length(maps:get(questions_and_answers, AnalysisData))),
    ?assert(maps:is_key(root_cause, AnalysisData)),
    ?assert(maps:is_key(corrective_action, AnalysisData)),

    ct:pal("5 Whys analysis properly stored~n"),
    ct:pal("~n=== 5 Whys Analysis: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test Andon blocking behavior
%% @end
%%--------------------------------------------------------------------
test_andon_blocking(_Config) ->
    ct:pal("~n=== Testing Andon Blocking ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Trigger Andon at test stage
    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{}),
    {error, test_failure} = tcps_test_utils:run_tests(WorkOrderId),
    {ok, _AndonId} = tcps_test_utils:wait_for_andon(WorkOrderId),

    %% Verify stages blocked
    ?assertMatch({blocked, _}, tcps_andon:can_proceed(WorkOrderId, quality)),
    ?assertMatch({blocked, _}, tcps_andon:can_proceed(WorkOrderId, deterministic)),
    ?assertMatch({blocked, _}, tcps_andon:can_proceed(WorkOrderId, release)),
    ?assertMatch({blocked, _}, tcps_andon:can_proceed(WorkOrderId, publish)),

    %% Verify upstream stages not blocked
    ?assertEqual({ok, proceed}, tcps_andon:can_proceed(WorkOrderId, shacl)),
    ?assertEqual({ok, proceed}, tcps_andon:can_proceed(WorkOrderId, compile)),

    ct:pal("Downstream stages blocked, upstream stages allowed~n"),
    ct:pal("~n=== Andon Blocking: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test Andon resolution
%% @end
%%--------------------------------------------------------------------
test_andon_resolution(_Config) ->
    ct:pal("~n=== Testing Andon Resolution ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Trigger Andon
    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{}),
    {ok, AndonId} = tcps_test_utils:wait_for_andon(WorkOrderId),

    %% Add analysis
    {ok, _AnalysisId} = tcps_andon:add_5_whys_analysis(AndonId, #{
        root_cause => "Test issue",
        corrective_action => "Fixed test"
    }),

    %% Resolve Andon
    ok = tcps_andon:resolve(AndonId, #{
        resolved_by => "developer@example.com",
        resolution_notes => "Fixed authentication logic",
        time_to_resolve_seconds => 300
    }),

    %% Verify resolved
    {ok, Andon} = tcps_andon:get(AndonId),
    ?assertEqual(resolved, maps:get(status, Andon)),
    ?assert(maps:is_key(resolution_timestamp, Andon)),
    ?assert(maps:is_key(resolved_by, Andon)),

    %% Verify pipeline unblocked
    ?assertEqual({ok, proceed}, tcps_andon:can_proceed(WorkOrderId, release)),
    ?assertEqual({ok, proceed}, tcps_andon:can_proceed(WorkOrderId, publish)),

    ct:pal("Andon resolved and pipeline unblocked~n"),
    ct:pal("~n=== Andon Resolution: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test concurrent Andon handling
%% @end
%%--------------------------------------------------------------------
test_concurrent_andons(_Config) ->
    ct:pal("~n=== Testing Concurrent Andons ===~n"),

    %% Create 50 work orders
    WorkOrders = lists:map(fun(_) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        ok = tcps_kanban:start_work_order(WO),
        WO
    end, lists:seq(1, 50)),

    ct:pal("Created 50 work orders~n"),

    %% Trigger Andons concurrently
    Parent = self(),
    lists:foreach(fun(WO) ->
        spawn_link(fun() ->
            ok = tcps_test_utils:inject_test_failure(WO, #{}),
            {ok, AndonId} = tcps_test_utils:wait_for_andon(WO),
            Parent ! {andon, WO, AndonId}
        end)
    end, WorkOrders),

    %% Collect Andon IDs
    AndonIds = lists:map(fun(WO) ->
        receive
            {andon, WO, AndonId} -> AndonId
        after 5000 ->
            error({timeout, WO})
        end
    end, WorkOrders),

    ?assertEqual(50, length(AndonIds)),
    ct:pal("All 50 Andons triggered~n"),

    %% Verify all recorded
    OpenAndons = tcps_andon:list_open(),
    ?assertEqual(50, length(OpenAndons)),

    %% Resolve concurrently
    lists:foreach(fun(AndonId) ->
        spawn_link(fun() ->
            ok = tcps_andon:resolve(AndonId, #{root_cause => "Test"})
        end)
    end, AndonIds),

    %% Wait for all resolved
    tcps_test_utils:wait_until(fun() ->
        length(tcps_andon:list_open()) =:= 0
    end, 10000),

    ?assertEqual([], tcps_andon:list_open()),
    ct:pal("All 50 Andons resolved~n"),

    ct:pal("~n=== Concurrent Andons: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test Andon escalation
%% @end
%%--------------------------------------------------------------------
test_andon_escalation(_Config) ->
    ct:pal("~n=== Testing Andon Escalation ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{}),
    {ok, AndonId} = tcps_test_utils:wait_for_andon(WorkOrderId),

    %% Wait for escalation timeout (simulate)
    ok = tcps_test_utils:advance_time(3600), % 1 hour

    %% Verify escalated
    {ok, Andon} = tcps_andon:get(AndonId),
    ?assertEqual(escalated, maps:get(escalation_level, Andon)),
    ?assert(maps:is_key(escalated_to, Andon)),
    ?assert(maps:is_key(escalation_timestamp, Andon)),

    ct:pal("Andon escalated after timeout~n"),
    ct:pal("~n=== Andon Escalation: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test Andon metrics tracking
%% @end
%%--------------------------------------------------------------------
test_andon_metrics(_Config) ->
    ct:pal("~n=== Testing Andon Metrics ===~n"),

    %% Reset metrics
    ok = tcps_andon:reset_metrics(),

    %% Trigger and resolve 10 Andons
    lists:foreach(fun(_) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        ok = tcps_test_utils:inject_test_failure(WO, #{}),
        {ok, AndonId} = tcps_test_utils:wait_for_andon(WO),
        ok = tcps_andon:resolve(AndonId, #{root_cause => "Test"})
    end, lists:seq(1, 10)),

    %% Get metrics
    Metrics = tcps_andon:get_metrics(),

    ?assertEqual(10, maps:get(total_andons, Metrics)),
    ?assertEqual(10, maps:get(resolved_andons, Metrics)),
    ?assertEqual(0, maps:get(open_andons, Metrics)),
    ?assert(maps:get(avg_resolution_time_seconds, Metrics) > 0),

    %% Verify metrics by type
    ByType = maps:get(andons_by_type, Metrics),
    ?assertEqual(10, maps:get(test_failure, ByType)),

    ct:pal("Metrics: ~p~n", [Metrics]),
    ct:pal("~n=== Andon Metrics: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test Andon notification system
%% @end
%%--------------------------------------------------------------------
test_andon_notification(_Config) ->
    ct:pal("~n=== Testing Andon Notification ===~n"),

    %% Subscribe to notifications
    ok = tcps_andon:subscribe_notifications(self()),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{}),
    {ok, AndonId} = tcps_test_utils:wait_for_andon(WorkOrderId),

    %% Verify notification received
    receive
        {andon_triggered, AndonId, Details} ->
            ?assertEqual(WorkOrderId, maps:get(work_order_id, Details)),
            ct:pal("Andon triggered notification received~n")
    after 5000 ->
        error(notification_timeout)
    end,

    %% Resolve
    ok = tcps_andon:resolve(AndonId, #{root_cause => "Test"}),

    %% Verify resolution notification
    receive
        {andon_resolved, AndonId, _Details} ->
            ct:pal("Andon resolved notification received~n")
    after 5000 ->
        error(notification_timeout)
    end,

    ct:pal("~n=== Andon Notification: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test Andon history tracking
%% @end
%%--------------------------------------------------------------------
test_andon_history(_Config) ->
    ct:pal("~n=== Testing Andon History ===~n"),

    %% Create and resolve multiple Andons
    WorkOrders = lists:map(fun(N) ->
        {ok, WO} = tcps_test_utils:create_test_work_order(),
        ok = tcps_test_utils:inject_test_failure(WO, #{seq => N}),
        {ok, AndonId} = tcps_test_utils:wait_for_andon(WO),
        ok = tcps_andon:resolve(AndonId, #{root_cause => "Test"}),
        {WO, AndonId}
    end, lists:seq(1, 5)),

    [FirstWO | _] = [WO || {WO, _} <- WorkOrders],

    %% Get history for work order
    {ok, History} = tcps_andon:get_history(FirstWO),
    ?assertEqual(1, length(History)),

    [Andon] = History,
    ?assertEqual(resolved, maps:get(status, Andon)),
    ?assert(maps:is_key(resolution_timestamp, Andon)),

    %% Get all history
    {ok, AllHistory} = tcps_andon:get_all_history(),
    ?assertEqual(5, length(AllHistory)),

    ct:pal("Andon history tracked: ~p records~n", [length(AllHistory)]),
    ct:pal("~n=== Andon History: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test multiple failures creating single Andon
%% @end
%%--------------------------------------------------------------------
test_multiple_failures_single_andon(_Config) ->
    ct:pal("~n=== Testing Multiple Failures Single Andon ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),

    %% Inject multiple failures at once
    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{
        test => "test_1",
        error => "Error 1"
    }),
    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{
        test => "test_2",
        error => "Error 2"
    }),
    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{
        test => "test_3",
        error => "Error 3"
    }),

    %% Verify single Andon with all failures
    {ok, AndonId} = tcps_test_utils:wait_for_andon(WorkOrderId),
    {ok, Andon} = tcps_andon:get(AndonId),

    Details = maps:get(details, Andon),
    ?assertEqual(3, maps:get(failed_tests, Details)),

    %% Verify no duplicate Andons
    AllOpen = tcps_andon:list_open(),
    WorkOrderAndons = [A || A <- AllOpen,
                            {ok, AndonData} <- [tcps_andon:get(A)],
                            maps:get(work_order_id, AndonData) =:= WorkOrderId],
    ?assertEqual(1, length(WorkOrderAndons)),

    ct:pal("Single Andon created for multiple failures~n"),
    ct:pal("~n=== Multiple Failures Single Andon: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test Andon timeout handling
%% @end
%%--------------------------------------------------------------------
test_andon_timeout(_Config) ->
    ct:pal("~n=== Testing Andon Timeout ===~n"),

    {ok, WorkOrderId} = tcps_test_utils:create_test_work_order(),
    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{}),
    {ok, AndonId} = tcps_test_utils:wait_for_andon(WorkOrderId),

    %% Simulate timeout
    ok = tcps_test_utils:advance_time(7200), % 2 hours

    %% Verify timeout status
    {ok, Andon} = tcps_andon:get(AndonId),
    ?assertEqual(timeout, maps:get(timeout_status, Andon)),
    ?assertEqual(escalated, maps:get(escalation_level, Andon)),

    %% Verify notification sent
    {ok, Notifications} = tcps_andon:get_notifications(AndonId),
    ?assert(length(Notifications) > 0),

    TimeoutNotifications = [N || N <- Notifications,
                                 maps:get(type, N) =:= timeout],
    ?assert(length(TimeoutNotifications) > 0),

    ct:pal("Timeout handling working correctly~n"),
    ct:pal("~n=== Andon Timeout: SUCCESS ===~n"),
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================
