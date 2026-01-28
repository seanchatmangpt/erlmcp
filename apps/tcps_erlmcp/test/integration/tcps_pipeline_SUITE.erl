%%%-------------------------------------------------------------------
%%% @doc TCPS Full Pipeline Integration Test Suite
%%%
%%% Comprehensive end-to-end testing of the complete TCPS production
%%% pipeline from work order creation through SKU publication.
%%%
%%% Tests cover:
%%% - Complete production pipeline (10-stage workflow)
%%% - Security patch workflow
%%% - Feature development workflow
%%% - Andon stop and resume
%%% - Quality gate enforcement
%%% - Heijunka leveling
%%% - Concurrent SKU processing
%%% - Receipt chain verification
%%% - Deterministic build verification
%%% - Kaizen improvement cycle
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_pipeline_SUITE).

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
    test_complete_production_pipeline/1,
    test_security_patch_workflow/1,
    test_feature_development_workflow/1,
    test_andon_stop_and_resume/1,
    test_quality_gate_enforcement/1,
    test_heijunka_leveling/1,
    test_concurrent_skus/1,
    test_receipt_chain_verification/1,
    test_deterministic_build/1,
    test_kaizen_improvement_cycle/1,
    test_multi_stage_failure_recovery/1,
    test_wip_limit_enforcement/1,
    test_bucket_balancing/1,
    test_release_rollback/1,
    test_marketplace_publishing/1
]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

suite() ->
    [{timetrap, {minutes, 10}}].

all() ->
    [
        test_complete_production_pipeline,
        test_security_patch_workflow,
        test_feature_development_workflow,
        test_andon_stop_and_resume,
        test_quality_gate_enforcement,
        test_heijunka_leveling,
        test_concurrent_skus,
        test_receipt_chain_verification,
        test_deterministic_build,
        test_kaizen_improvement_cycle,
        test_multi_stage_failure_recovery,
        test_wip_limit_enforcement,
        test_bucket_balancing,
        test_release_rollback,
        test_marketplace_publishing
    ].

init_per_suite(Config) ->
    %% Start required applications
    {ok, _} = application:ensure_all_started(erlmcp),

    %% Initialize test environment
    ok = tcps_test_utils:init_test_env(),

    %% Start mock services
    {ok, GitHubMock} = tcps_test_utils:start_github_mock(),
    {ok, MarketplaceMock} = tcps_test_utils:start_marketplace_mock(),
    {ok, OtelMock} = tcps_test_utils:start_otel_mock(),

    %% Store in config
    [
        {github_mock, GitHubMock},
        {marketplace_mock, MarketplaceMock},
        {otel_mock, OtelMock}
        | Config
    ].

end_per_suite(Config) ->
    %% Stop mock services
    ok = tcps_test_utils:stop_github_mock(?config(github_mock, Config)),
    ok = tcps_test_utils:stop_marketplace_mock(?config(marketplace_mock, Config)),
    ok = tcps_test_utils:stop_otel_mock(?config(otel_mock, Config)),

    %% Cleanup test environment
    ok = tcps_test_utils:cleanup_test_env(),

    %% Stop applications
    ok = application:stop(tcps),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("~n=== Starting test case: ~p ===~n", [TestCase]),

    %% Clear all state
    ok = tcps_test_utils:clear_all_data(),

    %% Reset metrics
    ok = tcps_kaizen:reset_metrics(),

    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("~n=== Completed test case: ~p ===~n", [TestCase]),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test complete production pipeline (10-stage workflow)
%%
%% Stages:
%% 1. Work order creation from GitHub issue
%% 2. Heijunka scheduling
%% 3. SHACL validation
%% 4. Compilation
%% 5. Testing (80%+ coverage)
%% 6. Quality gates
%% 7. Deterministic build verification
%% 8. Release creation
%% 9. Marketplace publication
%% 10. Kaizen metrics update
%% @end
%%--------------------------------------------------------------------
test_complete_production_pipeline(_Config) ->
    ct:pal("~n=== Testing Complete Production Pipeline ===~n"),

    %% Stage 1: Create work order from GitHub issue
    MockIssue = #{
        number => 123,
        title => <<"Implement new authentication module">>,
        labels => [<<"feature">>, <<"high-priority">>],
        bucket => reliability,
        estimated_effort => 8
    },

    {ok, WorkOrderId} = tcps_work_order:create_from_github(MockIssue),
    ct:pal("Created work order: ~p~n", [WorkOrderId]),
    ?assertMatch({ok, _}, tcps_work_order:get(WorkOrderId)),

    %% Stage 2: Heijunka scheduling
    Schedule = tcps_kanban:heijunka_schedule(),
    ?assert(lists:member(WorkOrderId, Schedule)),
    ct:pal("Work order scheduled in position: ~p~n",
           [find_position(WorkOrderId, Schedule)]),

    %% Stage 3: Start work order
    ok = tcps_kanban:start_work_order(WorkOrderId),
    {ok, WO1} = tcps_work_order:get(WorkOrderId),
    ?assertEqual(in_progress, maps:get(status, WO1)),
    ct:pal("Work order started~n"),

    %% Stage 4: SHACL validation
    {ok, ShaclResult} = run_shacl_validation(WorkOrderId),
    ?assertEqual(valid, ShaclResult),
    Receipt1 = generate_receipt(WorkOrderId, shacl, pass, #{
        rules_checked => 150,
        violations => 0
    }),
    ok = tcps_persistence:store_receipt(Receipt1),
    ct:pal("SHACL validation passed: ~p rules checked~n", [150]),

    %% Stage 5: Compilation
    {ok, CompileResult} = run_compilation(WorkOrderId),
    ?assertEqual(success, CompileResult),
    Receipt2 = generate_receipt(WorkOrderId, compile, pass, #{
        modules_compiled => 25,
        warnings => 0,
        errors => 0
    }),
    ok = tcps_persistence:store_receipt(Receipt2),
    ct:pal("Compilation succeeded: ~p modules~n", [25]),

    %% Stage 6: Testing with 80%+ coverage
    {ok, TestResults} = run_tests(WorkOrderId),
    Coverage = maps:get(coverage, TestResults),
    ?assert(Coverage >= 80.0),
    ?assertEqual(0, maps:get(failures, TestResults)),
    Receipt3 = generate_receipt(WorkOrderId, test, pass, #{
        tests_run => 150,
        tests_passed => 150,
        coverage => Coverage,
        duration_ms => 5000
    }),
    ok = tcps_persistence:store_receipt(Receipt3),
    ct:pal("Tests passed: ~p tests, ~.2f% coverage~n", [150, Coverage]),

    %% Stage 7: Quality gates
    {ok, QualityResult} = tcps_quality:check_gates(WorkOrderId),
    ?assertEqual(pass, QualityResult),
    Receipt4 = generate_receipt(WorkOrderId, quality, pass, #{
        gates_checked => [coverage, compilation, tests, security],
        gates_passed => [coverage, compilation, tests, security]
    }),
    ok = tcps_persistence:store_receipt(Receipt4),
    ct:pal("Quality gates passed: all 4 gates~n"),

    %% Stage 8: Deterministic build verification
    {ok, BuildResult} = tcps_deterministic:verify_build(WorkOrderId),
    ?assertEqual(deterministic, maps:get(result, BuildResult)),
    BuildHash = maps:get(hash, BuildResult),
    Receipt5 = generate_receipt(WorkOrderId, deterministic, pass, #{
        hash => BuildHash,
        verified => true
    }),
    ok = tcps_persistence:store_receipt(Receipt5),
    ct:pal("Deterministic build verified: ~s~n", [BuildHash]),

    %% Stage 9: Create release
    {ok, SkuId} = create_release(WorkOrderId),
    ?assertMatch(<<"sku-", _/binary>>, SkuId),
    Receipt6 = generate_receipt(WorkOrderId, release, pass, #{
        sku_id => SkuId,
        version => <<"1.0.0">>,
        artifact_size => 1024000
    }),
    ok = tcps_persistence:store_receipt(Receipt6),
    ct:pal("Release created: ~s~n", [SkuId]),

    %% Stage 10: Publish to marketplace
    ok = publish_to_marketplace(SkuId),
    Receipt7 = generate_receipt(WorkOrderId, publish, pass, #{
        sku_id => SkuId,
        marketplace_url => <<"https://marketplace.example.com/", SkuId/binary>>
    }),
    ok = tcps_persistence:store_receipt(Receipt7),
    ct:pal("Published to marketplace~n"),

    %% Stage 11: Verify receipt chain
    {ok, ChainResult} = tcps_receipt_verifier:verify_chain(WorkOrderId),
    ?assertEqual(complete, ChainResult),
    ct:pal("Receipt chain verified: complete~n"),

    %% Stage 12: Complete work order
    ok = tcps_work_order:complete(WorkOrderId, SkuId),
    {ok, WO2} = tcps_work_order:get(WorkOrderId),
    ?assertEqual(completed, maps:get(status, WO2)),
    ?assertEqual(SkuId, maps:get(sku_id, WO2)),
    ct:pal("Work order completed~n"),

    %% Stage 13: Update Kaizen metrics
    ok = tcps_kaizen:update_metrics(),
    Metrics = tcps_kaizen:get_metrics(),
    ?assert(maps:get(work_orders_completed, Metrics) > 0),
    ct:pal("Kaizen metrics updated~n"),

    %% Verify SKU published
    {ok, SkuData} = query_published_sku(SkuId),
    ?assertEqual(WorkOrderId, maps:get(work_order_id, SkuData)),
    ?assertEqual(<<"1.0.0">>, maps:get(version, SkuData)),

    ct:pal("~n=== Complete Production Pipeline: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test security patch workflow (expedited processing)
%% @end
%%--------------------------------------------------------------------
test_security_patch_workflow(_Config) ->
    ct:pal("~n=== Testing Security Patch Workflow ===~n"),

    %% Create high-priority security work order
    MockIssue = #{
        number => 456,
        title => <<"CRITICAL: Fix SQL injection vulnerability">>,
        labels => [<<"security">>, <<"critical">>],
        bucket => security,
        estimated_effort => 4
    },

    {ok, WorkOrderId} = tcps_work_order:create_from_github(MockIssue),

    %% Verify expedited scheduling (should be first)
    Schedule = tcps_kanban:heijunka_schedule(),
    ?assertEqual(WorkOrderId, hd(Schedule)),
    ct:pal("Security work order scheduled first~n"),

    %% Process through pipeline
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Run security-focused validation
    {ok, SecurityScan} = run_security_scan(WorkOrderId),
    ?assertEqual(0, maps:get(vulnerabilities, SecurityScan)),

    %% Fast-track through stages
    ok = process_pipeline_stages(WorkOrderId),

    %% Verify completed
    {ok, WO} = tcps_work_order:get(WorkOrderId),
    ?assertEqual(completed, maps:get(status, WO)),

    %% Verify security metrics tracked
    Metrics = tcps_kaizen:get_security_metrics(),
    ?assert(maps:get(security_patches_deployed, Metrics) > 0),

    ct:pal("~n=== Security Patch Workflow: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test feature development workflow (standard processing)
%% @end
%%--------------------------------------------------------------------
test_feature_development_workflow(_Config) ->
    ct:pal("~n=== Testing Feature Development Workflow ===~n"),

    %% Create feature work order
    MockIssue = #{
        number => 789,
        title => <<"Add export to CSV feature">>,
        labels => [<<"feature">>, <<"enhancement">>],
        bucket => cost,
        estimated_effort => 5
    },

    {ok, WorkOrderId} = tcps_work_order:create_from_github(MockIssue),

    %% Process through pipeline
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Run comprehensive testing
    {ok, TestResults} = run_comprehensive_tests(WorkOrderId),
    ?assertEqual(0, maps:get(failures, TestResults)),
    ?assert(maps:get(coverage, TestResults) >= 80.0),

    %% Verify feature flags
    {ok, WO} = tcps_work_order:get(WorkOrderId),
    ?assertEqual(true, maps:get(requires_feature_flag, WO)),

    %% Complete pipeline
    ok = process_pipeline_stages(WorkOrderId),

    %% Verify feature tracked
    Metrics = tcps_kaizen:get_feature_metrics(),
    ?assert(maps:get(features_delivered, Metrics) > 0),

    ct:pal("~n=== Feature Development Workflow: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test Andon stop and resume
%% @end
%%--------------------------------------------------------------------
test_andon_stop_and_resume(_Config) ->
    ct:pal("~n=== Testing Andon Stop and Resume ===~n"),

    %% Start work order
    {ok, WorkOrderId} = create_test_work_order(),
    ok = tcps_kanban:start_work_order(WorkOrderId),

    %% Inject test failure
    ok = tcps_test_utils:inject_test_failure(WorkOrderId, #{
        test_name => "test_authentication",
        error => "Expected 200, got 401"
    }),

    %% Run tests (should trigger Andon)
    {error, test_failure} = run_tests(WorkOrderId),

    %% Verify Andon triggered
    {ok, AndonId} = tcps_test_utils:wait_for_andon(WorkOrderId),
    ?assertMatch({ok, _}, tcps_andon:get(AndonId)),
    ct:pal("Andon triggered: ~p~n", [AndonId]),

    %% Verify pipeline blocked
    {blocked, BlockedStages} = tcps_andon:can_proceed(WorkOrderId, release),
    ?assert(length(BlockedStages) > 0),
    ct:pal("Pipeline blocked at stages: ~p~n", [BlockedStages]),

    %% Perform 5 Whys analysis
    {ok, AnalysisId} = perform_5_whys(AndonId, [
        "Why did authentication fail?",
        "Because token was expired",
        "Why was token expired?",
        "Because refresh logic was not implemented",
        "Why was refresh not implemented?",
        "Because it was not in the original specification",
        "Why was it not specified?",
        "Because the requirement was overlooked",
        "Root cause: Incomplete requirements analysis"
    ]),
    ct:pal("5 Whys analysis completed: ~p~n", [AnalysisId]),

    %% Resolve Andon
    ok = tcps_andon:resolve(AndonId, AnalysisId),
    ct:pal("Andon resolved~n"),

    %% Verify pipeline unblocked
    {ok, proceed} = tcps_andon:can_proceed(WorkOrderId, release),
    ct:pal("Pipeline unblocked~n"),

    %% Resume from test stage
    ok = resume_from_stage(WorkOrderId, test),

    %% Complete successfully
    ok = process_pipeline_stages(WorkOrderId),
    {ok, WO} = tcps_work_order:get(WorkOrderId),
    ?assertEqual(completed, maps:get(status, WO)),

    ct:pal("~n=== Andon Stop and Resume: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test quality gate enforcement
%% @end
%%--------------------------------------------------------------------
test_quality_gate_enforcement(_Config) ->
    ct:pal("~n=== Testing Quality Gate Enforcement ===~n"),

    %% Test coverage gate
    {ok, WO1} = create_test_work_order(),
    ok = tcps_kanban:start_work_order(WO1),
    ok = tcps_test_utils:inject_low_coverage(WO1, 75.0),
    {error, quality_gate_failed} = tcps_quality:check_gates(WO1),
    ct:pal("Coverage gate enforced (75% < 80%)~n"),

    %% Test compilation gate
    {ok, WO2} = create_test_work_order(),
    ok = tcps_kanban:start_work_order(WO2),
    ok = tcps_test_utils:inject_compilation_error(WO2),
    {error, quality_gate_failed} = tcps_quality:check_gates(WO2),
    ct:pal("Compilation gate enforced~n"),

    %% Test security gate
    {ok, WO3} = create_test_work_order(),
    ok = tcps_kanban:start_work_order(WO3),
    ok = tcps_test_utils:inject_security_vulnerability(WO3),
    {error, quality_gate_failed} = tcps_quality:check_gates(WO3),
    ct:pal("Security gate enforced~n"),

    %% Test passing all gates
    {ok, WO4} = create_test_work_order(),
    ok = tcps_kanban:start_work_order(WO4),
    ok = process_pipeline_stages(WO4),
    {ok, pass} = tcps_quality:check_gates(WO4),
    ct:pal("All gates passed~n"),

    ct:pal("~n=== Quality Gate Enforcement: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test Heijunka leveling
%% @end
%%--------------------------------------------------------------------
test_heijunka_leveling(_Config) ->
    ct:pal("~n=== Testing Heijunka Leveling ===~n"),

    %% Create work orders across all buckets
    WO_Reliability = create_work_orders(reliability, 5),
    WO_Security = create_work_orders(security, 5),
    WO_Cost = create_work_orders(cost, 5),
    WO_Compliance = create_work_orders(compliance, 5),

    AllWorkOrders = WO_Reliability ++ WO_Security ++ WO_Cost ++ WO_Compliance,
    ct:pal("Created 20 work orders across 4 buckets~n"),

    %% Get Heijunka schedule
    Schedule = tcps_kanban:heijunka_schedule(),
    ?assertEqual(20, length(Schedule)),

    %% Verify no batching (max 2 consecutive from same bucket)
    ok = verify_no_batching(Schedule, 2),
    ct:pal("No batching detected~n"),

    %% Verify bucket distribution
    BucketCounts = count_by_bucket(Schedule),
    ?assertEqual(5, maps:get(reliability, BucketCounts)),
    ?assertEqual(5, maps:get(security, BucketCounts)),
    ?assertEqual(5, maps:get(cost, BucketCounts)),
    ?assertEqual(5, maps:get(compliance, BucketCounts)),
    ct:pal("Bucket distribution verified: ~p~n", [BucketCounts]),

    %% Verify leveling score
    LevelingScore = tcps_kanban:calculate_leveling_score(Schedule),
    ?assert(LevelingScore >= 0.8),
    ct:pal("Leveling score: ~.2f~n", [LevelingScore]),

    ct:pal("~n=== Heijunka Leveling: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test concurrent SKU processing
%% @end
%%--------------------------------------------------------------------
test_concurrent_skus(_Config) ->
    ct:pal("~n=== Testing Concurrent SKU Processing ===~n"),

    %% Create 10 work orders
    WorkOrders = create_work_orders(mixed, 10),
    ct:pal("Created 10 work orders~n"),

    %% Process concurrently
    Parent = self(),
    Results = lists:map(fun(WO) ->
        spawn_link(fun() ->
            Result = process_work_order_full(WO),
            Parent ! {result, WO, Result}
        end),
        WO
    end, WorkOrders),

    %% Collect results
    Collected = collect_results(Results, []),
    ?assertEqual(10, length(Collected)),

    %% Verify all completed successfully
    AllOk = lists:all(fun({_, ok}) -> true; (_) -> false end, Collected),
    ?assert(AllOk),
    ct:pal("All 10 work orders completed successfully~n"),

    %% Verify no race conditions
    ok = verify_no_duplicate_receipts(),
    ok = verify_all_receipts_valid(),
    ct:pal("No race conditions detected~n"),

    ct:pal("~n=== Concurrent SKU Processing: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test receipt chain verification
%% @end
%%--------------------------------------------------------------------
test_receipt_chain_verification(_Config) ->
    ct:pal("~n=== Testing Receipt Chain Verification ===~n"),

    %% Process work order (generates receipt chain)
    {ok, WorkOrderId} = create_test_work_order(),
    ok = process_pipeline_stages(WorkOrderId),

    %% Verify receipt chain
    {ok, ChainResult} = tcps_receipt_verifier:verify_chain(WorkOrderId),
    ?assertEqual(complete, ChainResult),
    ct:pal("Receipt chain verified: complete~n"),

    %% Get receipt chain
    {ok, Receipts} = tcps_persistence:get_receipts_by_work_order(WorkOrderId),
    ?assert(length(Receipts) >= 7),
    ct:pal("Receipt chain contains ~p receipts~n", [length(Receipts)]),

    %% Verify cryptographic signatures
    AllValid = lists:all(fun(Receipt) ->
        tcps_receipt_verifier:verify_signature(Receipt)
    end, Receipts),
    ?assert(AllValid),
    ct:pal("All receipt signatures valid~n"),

    %% Verify temporal ordering
    ok = verify_temporal_ordering(Receipts),
    ct:pal("Temporal ordering verified~n"),

    %% Test tampered receipt detection
    TamperedReceipt = tamper_receipt(hd(Receipts)),
    false = tcps_receipt_verifier:verify_signature(TamperedReceipt),
    ct:pal("Tampered receipt detected~n"),

    ct:pal("~n=== Receipt Chain Verification: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test deterministic build verification
%% @end
%%--------------------------------------------------------------------
test_deterministic_build(_Config) ->
    ct:pal("~n=== Testing Deterministic Build Verification ===~n"),

    %% Process work order
    {ok, WorkOrderId} = create_test_work_order(),
    ok = process_pipeline_stages(WorkOrderId),

    %% Verify deterministic build
    {ok, BuildResult1} = tcps_deterministic:verify_build(WorkOrderId),
    ?assertEqual(deterministic, maps:get(result, BuildResult1)),
    Hash1 = maps:get(hash, BuildResult1),
    ct:pal("First build hash: ~s~n", [Hash1]),

    %% Rebuild
    {ok, BuildResult2} = tcps_deterministic:rebuild(WorkOrderId),
    Hash2 = maps:get(hash, BuildResult2),
    ct:pal("Second build hash: ~s~n", [Hash2]),

    %% Verify hashes match
    ?assertEqual(Hash1, Hash2),
    ct:pal("Build is deterministic~n"),

    %% Test non-deterministic detection
    ok = tcps_test_utils:inject_timestamp_in_build(WorkOrderId),
    {ok, BuildResult3} = tcps_deterministic:rebuild(WorkOrderId),
    ?assertEqual(non_deterministic, maps:get(result, BuildResult3)),
    ct:pal("Non-deterministic build detected~n"),

    ct:pal("~n=== Deterministic Build Verification: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test Kaizen improvement cycle
%% @end
%%--------------------------------------------------------------------
test_kaizen_improvement_cycle(_Config) ->
    ct:pal("~n=== Testing Kaizen Improvement Cycle ===~n"),

    %% Reset metrics
    ok = tcps_kaizen:reset_metrics(),

    %% Process 10 work orders
    WorkOrders = create_work_orders(mixed, 10),
    lists:foreach(fun(WO) ->
        ok = process_work_order_full(WO)
    end, WorkOrders),
    ct:pal("Processed 10 work orders~n"),

    %% Get metrics
    Metrics = tcps_kaizen:get_metrics(),
    ?assertEqual(10, maps:get(work_orders_completed, Metrics)),

    %% Verify cycle time tracking
    AvgCycleTime = maps:get(avg_cycle_time_seconds, Metrics),
    ?assert(AvgCycleTime > 0),
    ct:pal("Average cycle time: ~.2f seconds~n", [AvgCycleTime]),

    %% Verify quality metrics
    QualityScore = maps:get(quality_score, Metrics),
    ?assert(QualityScore >= 0.8),
    ct:pal("Quality score: ~.2f~n", [QualityScore]),

    %% Trigger improvement suggestion
    {ok, Suggestions} = tcps_kaizen:generate_suggestions(),
    ?assert(length(Suggestions) > 0),
    ct:pal("Generated ~p improvement suggestions~n", [length(Suggestions)]),

    %% Apply improvement
    [FirstSuggestion | _] = Suggestions,
    ok = tcps_kaizen:apply_improvement(FirstSuggestion),
    ct:pal("Applied improvement: ~s~n", [maps:get(description, FirstSuggestion)]),

    ct:pal("~n=== Kaizen Improvement Cycle: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test multi-stage failure recovery
%% @end
%%--------------------------------------------------------------------
test_multi_stage_failure_recovery(_Config) ->
    ct:pal("~n=== Testing Multi-Stage Failure Recovery ===~n"),

    %% Test recovery from each stage
    Stages = [shacl, compile, test, quality, deterministic, release, publish],

    lists:foreach(fun(Stage) ->
        ct:pal("Testing recovery from ~p stage~n", [Stage]),

        %% Create work order
        {ok, WO} = create_test_work_order(),
        ok = tcps_kanban:start_work_order(WO),

        %% Inject failure at stage
        ok = tcps_test_utils:inject_failure_at_stage(WO, Stage),

        %% Verify Andon triggered
        {ok, AndonId} = tcps_test_utils:wait_for_andon(WO),

        %% Resolve and resume
        ok = tcps_andon:resolve(AndonId, #{root_cause => "Test injection"}),
        ok = resume_from_stage(WO, Stage),

        %% Verify completion
        ok = process_pipeline_stages(WO),
        {ok, WOData} = tcps_work_order:get(WO),
        ?assertEqual(completed, maps:get(status, WOData))
    end, Stages),

    ct:pal("~n=== Multi-Stage Failure Recovery: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test WIP limit enforcement
%% @end
%%--------------------------------------------------------------------
test_wip_limit_enforcement(_Config) ->
    ct:pal("~n=== Testing WIP Limit Enforcement ===~n"),

    %% Set WIP limit to 5 for reliability bucket
    ok = tcps_kanban:set_wip_limit(reliability, 5),

    %% Try to start 10 reliability work orders
    WorkOrders = create_work_orders(reliability, 10),

    Results = lists:map(fun(WO) ->
        tcps_kanban:start_work_order(WO)
    end, WorkOrders),

    %% Verify only 5 started, 5 rejected
    Started = [ok || ok <- Results],
    Rejected = [{error, wip_limit} || {error, wip_limit} <- Results],

    ?assertEqual(5, length(Started)),
    ?assertEqual(5, length(Rejected)),
    ct:pal("WIP limit enforced: 5 started, 5 rejected~n"),

    %% Complete one work order
    [FirstWO | _] = [WO || {WO, ok} <- lists:zip(WorkOrders, Results), ok =:= ok],
    ok = tcps_work_order:complete(FirstWO, <<"sku-test">>),
    ct:pal("Completed one work order~n"),

    %% Try to start another
    {ok, NewWO} = create_test_work_order(reliability),
    ok = tcps_kanban:start_work_order(NewWO),
    ct:pal("Started new work order after completion~n"),

    ct:pal("~n=== WIP Limit Enforcement: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test bucket balancing
%% @end
%%--------------------------------------------------------------------
test_bucket_balancing(_Config) ->
    ct:pal("~n=== Testing Bucket Balancing ===~n"),

    %% Create imbalanced load (20 security, 5 each of others)
    WO_Security = create_work_orders(security, 20),
    WO_Reliability = create_work_orders(reliability, 5),
    WO_Cost = create_work_orders(cost, 5),
    WO_Compliance = create_work_orders(compliance, 5),

    ct:pal("Created imbalanced load: 20 security, 5 others~n"),

    %% Get Heijunka schedule
    Schedule = tcps_kanban:heijunka_schedule(),

    %% Verify security work is distributed
    FirstHalf = lists:sublist(Schedule, 1, 17),
    SecondHalf = lists:sublist(Schedule, 18, 18),

    SecurityInFirstHalf = count_bucket_in_list(security, FirstHalf),
    SecurityInSecondHalf = count_bucket_in_list(security, SecondHalf),

    %% Should be roughly balanced (not all security at start or end)
    ?assert(SecurityInFirstHalf >= 8),
    ?assert(SecurityInSecondHalf >= 8),
    ct:pal("Security work distributed: ~p in first half, ~p in second half~n",
           [SecurityInFirstHalf, SecurityInSecondHalf]),

    ct:pal("~n=== Bucket Balancing: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test release rollback
%% @end
%%--------------------------------------------------------------------
test_release_rollback(_Config) ->
    ct:pal("~n=== Testing Release Rollback ===~n"),

    %% Create and complete work order
    {ok, WorkOrderId} = create_test_work_order(),
    ok = process_pipeline_stages(WorkOrderId),

    {ok, WO} = tcps_work_order:get(WorkOrderId),
    SkuId = maps:get(sku_id, WO),
    ct:pal("Released SKU: ~s~n", [SkuId]),

    %% Simulate production issue
    ok = tcps_test_utils:simulate_production_issue(SkuId),

    %% Rollback
    {ok, RollbackId} = tcps_release:rollback(SkuId),
    ct:pal("Rollback initiated: ~s~n", [RollbackId]),

    %% Verify SKU unpublished
    {error, not_found} = query_published_sku(SkuId),
    ct:pal("SKU unpublished from marketplace~n"),

    %% Verify rollback receipt created
    {ok, RollbackReceipt} = tcps_persistence:get_rollback_receipt(RollbackId),
    ?assertEqual(SkuId, maps:get(sku_id, RollbackReceipt)),

    ct:pal("~n=== Release Rollback: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test marketplace publishing
%% @end
%%--------------------------------------------------------------------
test_marketplace_publishing(_Config) ->
    ct:pal("~n=== Testing Marketplace Publishing ===~n"),

    %% Create work order
    {ok, WorkOrderId} = create_test_work_order(),
    ok = process_pipeline_stages(WorkOrderId),

    {ok, WO} = tcps_work_order:get(WorkOrderId),
    SkuId = maps:get(sku_id, WO),

    %% Verify published
    {ok, SkuData} = query_published_sku(SkuId),
    ?assertEqual(WorkOrderId, maps:get(work_order_id, SkuData)),

    %% Verify metadata
    Metadata = maps:get(metadata, SkuData),
    ?assertEqual(<<"1.0.0">>, maps:get(version, Metadata)),
    ?assert(maps:is_key(description, Metadata)),
    ?assert(maps:is_key(license, Metadata)),

    ct:pal("SKU published with complete metadata~n"),

    %% Verify searchable
    {ok, SearchResults} = tcps_marketplace:search(#{query => "test"}),
    ?assert(lists:member(SkuId, SearchResults)),
    ct:pal("SKU is searchable in marketplace~n"),

    ct:pal("~n=== Marketplace Publishing: SUCCESS ===~n"),
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @private
create_test_work_order() ->
    create_test_work_order(reliability).

%% @private
create_test_work_order(Bucket) ->
    tcps_work_order:create_from_github(#{
        number => rand:uniform(10000),
        title => <<"Test work order">>,
        labels => [<<"test">>],
        bucket => Bucket,
        estimated_effort => 5
    }).

%% @private
create_work_orders(Bucket, Count) when Bucket =/= mixed ->
    lists:map(fun(_) ->
        {ok, WO} = create_test_work_order(Bucket),
        WO
    end, lists:seq(1, Count));
create_work_orders(mixed, Count) ->
    Buckets = [reliability, security, cost, compliance],
    lists:map(fun(N) ->
        Bucket = lists:nth((N rem 4) + 1, Buckets),
        {ok, WO} = create_test_work_order(Bucket),
        WO
    end, lists:seq(1, Count)).

%% @private
generate_receipt(WorkOrderId, Stage, Result, Metadata) ->
    #{
        id => tcps_test_utils:generate_id(),
        work_order_id => WorkOrderId,
        stage => Stage,
        result => Result,
        metadata => Metadata,
        timestamp => erlang:system_time(millisecond),
        signature => tcps_test_utils:generate_signature()
    }.

%% @private
run_shacl_validation(WorkOrderId) ->
    tcps_test_utils:mock_shacl_validation(WorkOrderId, valid).

%% @private
run_compilation(WorkOrderId) ->
    tcps_test_utils:mock_compilation(WorkOrderId, success).

%% @private
run_tests(WorkOrderId) ->
    tcps_test_utils:mock_tests(WorkOrderId, #{
        tests_run => 150,
        tests_passed => 150,
        failures => 0,
        coverage => 85.5,
        duration_ms => 5000
    }).

%% @private
run_comprehensive_tests(WorkOrderId) ->
    run_tests(WorkOrderId).

%% @private
run_security_scan(WorkOrderId) ->
    tcps_test_utils:mock_security_scan(WorkOrderId, #{
        vulnerabilities => 0,
        warnings => 0
    }).

%% @private
create_release(WorkOrderId) ->
    SkuId = iolist_to_binary([<<"sku-">>, integer_to_binary(erlang:unique_integer([positive]))]),
    ok = tcps_work_order:set_sku_id(WorkOrderId, SkuId),
    {ok, SkuId}.

%% @private
publish_to_marketplace(SkuId) ->
    tcps_test_utils:mock_marketplace_publish(SkuId).

%% @private
query_published_sku(SkuId) ->
    tcps_test_utils:mock_marketplace_query(SkuId).

%% @private
perform_5_whys(AndonId, Analysis) ->
    AnalysisId = tcps_test_utils:generate_id(),
    ok = tcps_andon:add_analysis(AndonId, #{
        id => AnalysisId,
        type => five_whys,
        questions_and_answers => Analysis
    }),
    {ok, AnalysisId}.

%% @private
resume_from_stage(WorkOrderId, Stage) ->
    tcps_kanban:resume_work_order(WorkOrderId, Stage).

%% @private
process_pipeline_stages(WorkOrderId) ->
    ok = run_shacl_validation(WorkOrderId),
    ok = run_compilation(WorkOrderId),
    ok = run_tests(WorkOrderId),
    ok = tcps_quality:check_gates(WorkOrderId),
    ok = tcps_deterministic:verify_build(WorkOrderId),
    {ok, _} = create_release(WorkOrderId),
    ok.

%% @private
process_work_order_full(WorkOrderId) ->
    ok = tcps_kanban:start_work_order(WorkOrderId),
    ok = process_pipeline_stages(WorkOrderId),
    {ok, WO} = tcps_work_order:get(WorkOrderId),
    SkuId = maps:get(sku_id, WO),
    ok = publish_to_marketplace(SkuId),
    ok = tcps_work_order:complete(WorkOrderId, SkuId),
    ok.

%% @private
collect_results([], Acc) -> Acc;
collect_results([WO | Rest], Acc) ->
    receive
        {result, WO, Result} ->
            collect_results(Rest, [{WO, Result} | Acc])
    after 30000 ->
            collect_results(Rest, [{WO, timeout} | Acc])
    end.

%% @private
verify_no_duplicate_receipts() ->
    AllReceipts = tcps_persistence:get_all_receipts(),
    ReceiptIds = [maps:get(id, R) || R <- AllReceipts],
    UniqueIds = lists:usort(ReceiptIds),
    case length(ReceiptIds) =:= length(UniqueIds) of
        true -> ok;
        false -> {error, duplicate_receipts}
    end.

%% @private
verify_all_receipts_valid() ->
    AllReceipts = tcps_persistence:get_all_receipts(),
    AllValid = lists:all(fun(R) ->
        tcps_receipt_verifier:verify_signature(R)
    end, AllReceipts),
    case AllValid of
        true -> ok;
        false -> {error, invalid_receipts}
    end.

%% @private
verify_temporal_ordering(Receipts) ->
    Timestamps = [maps:get(timestamp, R) || R <- Receipts],
    Sorted = lists:sort(Timestamps),
    case Timestamps =:= Sorted of
        true -> ok;
        false -> {error, temporal_ordering_violated}
    end.

%% @private
tamper_receipt(Receipt) ->
    Receipt#{result => tampered}.

%% @private
find_position(Element, List) ->
    find_position(Element, List, 1).

find_position(Element, [Element | _], Pos) -> Pos;
find_position(Element, [_ | Rest], Pos) -> find_position(Element, Rest, Pos + 1);
find_position(_, [], _) -> not_found.

%% @private
verify_no_batching(Schedule, MaxConsecutive) ->
    verify_no_batching(Schedule, MaxConsecutive, undefined, 0).

verify_no_batching([], _, _, _) ->
    ok;
verify_no_batching([WO | Rest], MaxConsecutive, PrevBucket, Count) ->
    {ok, WOData} = tcps_work_order:get(WO),
    Bucket = maps:get(bucket, WOData),

    NewCount = case Bucket =:= PrevBucket of
        true -> Count + 1;
        false -> 1
    end,

    case NewCount > MaxConsecutive of
        true -> {error, batching_detected};
        false -> verify_no_batching(Rest, MaxConsecutive, Bucket, NewCount)
    end.

%% @private
count_by_bucket(Schedule) ->
    lists:foldl(fun(WO, Acc) ->
        {ok, WOData} = tcps_work_order:get(WO),
        Bucket = maps:get(bucket, WOData),
        maps:update_with(Bucket, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, Schedule).

%% @private
count_bucket_in_list(Bucket, List) ->
    length([WO || WO <- List,
                  {ok, WOData} <- [tcps_work_order:get(WO)],
                  maps:get(bucket, WOData) =:= Bucket]).
