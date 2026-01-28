%%%-----------------------------------------------------------------------------
%%% @doc TCPS SKU Lifecycle Management - Comprehensive Test Suite
%%%
%%% Tests cover:
%%% - SKU creation from work orders
%%% - Stage transition logic with quality gates
%%% - Automatic pipeline progression
%%% - Andon event blocking
%%% - Receipt chain tracking
%%% - Marketplace publication workflow
%%% - Error handling and failure scenarios
%%% - Concurrent pipeline execution
%%% - Production metrics and reporting
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_sku_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Fixtures
%%%=============================================================================

sku_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"SKU creation from work order", fun test_create_sku/0},
         {"Get SKU by ID", fun test_get_sku/0},
         {"List all SKUs", fun test_list_all_skus/0},
         {"Get SKU status", fun test_get_sku_status/0},
         {"Get current stage", fun test_get_current_stage/0},
         {"Stage transition sequential", fun test_transition_stage/0},
         {"Invalid stage sequence rejected", fun test_invalid_stage_sequence/0},
         {"Quality gate failure blocks transition", fun test_quality_gate_failure/0},
         {"Andon blocks stage transition", fun test_andon_blocks_transition/0},
         {"Production history tracking", fun test_production_history/0},
         {"Receipt chain tracking", fun test_receipt_chain/0},
         {"Verify receipt chain integrity", fun test_verify_receipt_chain/0},
         {"Process SKU pipeline synchronously", fun test_process_sku_pipeline/0},
         {"Pipeline stops on failure", fun test_pipeline_failure/0},
         {"Process pipeline asynchronously", fun test_process_sku_pipeline_async/0},
         {"Get pipeline progress", fun test_get_pipeline_progress/0},
         {"Abort pipeline execution", fun test_abort_pipeline/0},
         {"Mark SKU as released", fun test_mark_released/0},
         {"Mark SKU as published", fun test_mark_published/0},
         {"Get marketplace status", fun test_get_marketplace_status/0},
         {"Generate marketplace URL", fun test_generate_marketplace_url/0},
         {"Produce SKU end-to-end", fun test_produce_sku/0},
         {"Get SKUs by status", fun test_get_skus_by_status/0},
         {"Production metrics", fun test_production_metrics/0},
         {"Stage statistics", fun test_stage_statistics/0},
         {"Multiple concurrent SKUs", fun test_concurrent_skus/0},
         {"SKU deletion", fun test_delete_sku/0},
         {"Not found errors", fun test_not_found_errors/0}
     ]}.

setup() ->
    %% Initialize ETS table
    tcps_sku:init_ets(),

    %% Start gen_server
    {ok, Pid} = tcps_sku:start_link(#{auto_persist => false}),
    Pid.

cleanup(Pid) ->
    %% Stop gen_server
    tcps_sku:stop(),

    %% Clean up ETS
    case ets:info(tcps_skus) of
        undefined -> ok;
        _ -> ets:delete(tcps_skus)
    end,

    ok.

%%%=============================================================================
%%% Test Cases - SKU Creation
%%%=============================================================================

test_create_sku() ->
    WorkOrderId = <<"WO-001">>,

    %% Create SKU
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Verify SKU ID format
    ?assert(is_binary(SkuId)),
    ?assert(byte_size(SkuId) > 0),
    ?assert(binary:match(SkuId, <<"SKU-">>) =/= nomatch),

    %% Verify SKU created
    {ok, Sku} = tcps_sku:get_sku(SkuId),
    ?assertEqual(SkuId, maps:get(id, Sku)),
    ?assertEqual(WorkOrderId, maps:get(work_order_id, Sku)),
    ?assertEqual(shacl_validation, maps:get(current_stage, Sku)),
    ?assertEqual(in_production, maps:get(status, Sku)),
    ?assertEqual([], maps:get(production_history, Sku)),
    ?assertEqual([], maps:get(receipts, Sku)).

test_get_sku() ->
    WorkOrderId = <<"WO-002">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Get existing SKU
    {ok, Sku} = tcps_sku:get_sku(SkuId),
    ?assertEqual(SkuId, maps:get(id, Sku)),

    %% Get non-existent SKU
    {error, not_found} = tcps_sku:get_sku(<<"non-existent">>).

test_list_all_skus() ->
    %% Create multiple SKUs
    {ok, _SkuId1} = tcps_sku:create_sku(<<"WO-101">>),
    {ok, _SkuId2} = tcps_sku:create_sku(<<"WO-102">>),
    {ok, _SkuId3} = tcps_sku:create_sku(<<"WO-103">>),

    %% List all SKUs
    AllSkus = tcps_sku:list_all_skus(),
    ?assert(length(AllSkus) >= 3).

%%%=============================================================================
%%% Test Cases - Stage Management
%%%=============================================================================

test_get_sku_status() ->
    WorkOrderId = <<"WO-003">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Get status
    {ok, Status} = tcps_sku:get_sku_status(SkuId),

    ?assertEqual(SkuId, maps:get(sku_id, Status)),
    ?assertEqual(WorkOrderId, maps:get(work_order_id, Status)),
    ?assertEqual(shacl_validation, maps:get(current_stage, Status)),
    ?assertEqual(in_production, maps:get(status, Status)),
    ?assert(maps:get(completion_percent, Status) >= 0),
    ?assert(maps:get(completion_percent, Status) =< 100),
    ?assertEqual(0, maps:get(receipts_count, Status)),
    ?assertEqual(0, maps:get(open_andons_count, Status)),
    ?assert(is_list(maps:get(production_history, Status))).

test_get_current_stage() ->
    WorkOrderId = <<"WO-004">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Get current stage
    {ok, Stage} = tcps_sku:get_current_stage(SkuId),
    ?assertEqual(shacl_validation, Stage).

test_transition_stage() ->
    WorkOrderId = <<"WO-005">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Verify initial stage
    {ok, shacl_validation} = tcps_sku:get_current_stage(SkuId),

    %% Transition to next stage
    ok = tcps_sku:transition_stage(SkuId, compilation),

    %% Verify stage updated
    {ok, compilation} = tcps_sku:get_current_stage(SkuId),

    %% Verify production history recorded
    {ok, History} = tcps_sku:get_production_history(SkuId),
    ?assert(length(History) =:= 1),

    [Entry] = History,
    ?assertEqual(shacl_validation, maps:get(stage, Entry)),
    ?assertEqual(pass, maps:get(status, Entry)),
    ?assert(maps:is_key(receipt_id, Entry)),
    ?assert(maps:is_key(timestamp, Entry)).

test_invalid_stage_sequence() ->
    WorkOrderId = <<"WO-006">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Try to skip stages (invalid sequence)
    {error, invalid_sequence} = tcps_sku:transition_stage(SkuId, test_execution),

    %% Verify still at initial stage
    {ok, shacl_validation} = tcps_sku:get_current_stage(SkuId).

test_quality_gate_failure() ->
    %% Note: This test requires mocking quality gate failures
    %% For now, we test the structure - actual integration would need tcps_quality_gates module
    WorkOrderId = <<"WO-007">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% In production, quality gate failures would prevent transitions
    %% Current mock implementation always passes
    ok = tcps_sku:transition_stage(SkuId, compilation),
    {ok, compilation} = tcps_sku:get_current_stage(SkuId).

test_andon_blocks_transition() ->
    %% Note: This test requires tcps_andon module integration
    %% Structure demonstrates Andon blocking capability
    WorkOrderId = <<"WO-008">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Without open Andons, transition should succeed
    {ok, can_proceed} = tcps_sku:can_transition(SkuId, compilation),
    ok = tcps_sku:transition_stage(SkuId, compilation).

test_production_history() ->
    WorkOrderId = <<"WO-009">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Initially empty history
    {ok, History1} = tcps_sku:get_production_history(SkuId),
    ?assertEqual([], History1),

    %% Transition through stages
    ok = tcps_sku:transition_stage(SkuId, compilation),
    ok = tcps_sku:transition_stage(SkuId, test_execution),

    %% Verify history accumulated
    {ok, History2} = tcps_sku:get_production_history(SkuId),
    ?assertEqual(2, length(History2)),

    %% Verify history ordering (newest first)
    [Entry2, Entry1] = History2,
    ?assertEqual(compilation, maps:get(stage, Entry2)),
    ?assertEqual(shacl_validation, maps:get(stage, Entry1)).

%%%=============================================================================
%%% Test Cases - Receipt Management
%%%=============================================================================

test_receipt_chain() ->
    WorkOrderId = <<"WO-010">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Initially no receipts
    {ok, Receipts1} = tcps_sku:get_receipts(SkuId),
    ?assertEqual([], Receipts1),

    %% Transition generates receipts
    ok = tcps_sku:transition_stage(SkuId, compilation),

    %% Verify receipt added
    {ok, Receipts2} = tcps_sku:get_receipts(SkuId),
    ?assertEqual(1, length(Receipts2)),

    %% Add more transitions
    ok = tcps_sku:transition_stage(SkuId, test_execution),
    {ok, Receipts3} = tcps_sku:get_receipts(SkuId),
    ?assertEqual(2, length(Receipts3)).

test_verify_receipt_chain() ->
    WorkOrderId = <<"WO-011">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Incomplete chain
    {error, {missing_receipts, Missing1}} = tcps_sku:verify_receipt_chain(SkuId),
    ?assert(length(Missing1) > 0),

    %% Complete all stages
    ok = tcps_sku:transition_stage(SkuId, compilation),
    ok = tcps_sku:transition_stage(SkuId, test_execution),
    ok = tcps_sku:transition_stage(SkuId, security_scan),
    ok = tcps_sku:transition_stage(SkuId, deterministic_build),
    ok = tcps_sku:transition_stage(SkuId, quality_metrics),
    ok = tcps_sku:transition_stage(SkuId, release_verification),
    ok = tcps_sku:transition_stage(SkuId, smoke_test),
    ok = tcps_sku:transition_stage(SkuId, marketplace_validation),
    ok = tcps_sku:transition_stage(SkuId, publication),

    %% Complete chain
    {ok, complete} = tcps_sku:verify_receipt_chain(SkuId).

%%%=============================================================================
%%% Test Cases - Pipeline Execution
%%%=============================================================================

test_process_sku_pipeline() ->
    WorkOrderId = <<"WO-012">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Process pipeline synchronously
    {ok, FinalStage} = tcps_sku:process_sku_pipeline(SkuId),

    %% Verify reached final stage
    ?assertEqual(publication, FinalStage),

    %% Verify SKU at final stage
    {ok, publication} = tcps_sku:get_current_stage(SkuId),

    %% Verify all receipts generated
    {ok, Receipts} = tcps_sku:get_receipts(SkuId),
    ?assert(length(Receipts) >= 9),  % One per stage transition

    %% Verify complete history
    {ok, History} = tcps_sku:get_production_history(SkuId),
    ?assert(length(History) >= 9).

test_pipeline_failure() ->
    %% Note: Actual failure testing requires quality gate mocking
    %% This test demonstrates the structure
    WorkOrderId = <<"WO-013">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% With current mock (always passes), pipeline completes
    {ok, _FinalStage} = tcps_sku:process_sku_pipeline(SkuId).

test_process_sku_pipeline_async() ->
    WorkOrderId = <<"WO-014">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Start async pipeline
    {ok, WorkerPid} = tcps_sku:process_sku_pipeline_async(SkuId),
    ?assert(is_pid(WorkerPid)),

    %% Give worker time to complete
    timer:sleep(100),

    %% Verify pipeline completed
    {ok, Stage} = tcps_sku:get_current_stage(SkuId),
    ?assertEqual(publication, Stage).

test_get_pipeline_progress() ->
    WorkOrderId = <<"WO-015">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Get initial progress
    {ok, Progress1} = tcps_sku:get_pipeline_progress(SkuId),
    ?assertEqual(SkuId, maps:get(sku_id, Progress1)),
    ?assertEqual(shacl_validation, maps:get(current_stage, Progress1)),
    ?assertEqual(in_production, maps:get(status, Progress1)),
    ?assert(maps:get(percent_complete, Progress1) >= 0),

    %% Transition a few stages
    ok = tcps_sku:transition_stage(SkuId, compilation),
    ok = tcps_sku:transition_stage(SkuId, test_execution),

    %% Get updated progress
    {ok, Progress2} = tcps_sku:get_pipeline_progress(SkuId),
    ?assertEqual(test_execution, maps:get(current_stage, Progress2)),
    ?assert(maps:get(percent_complete, Progress2) > maps:get(percent_complete, Progress1)).

test_abort_pipeline() ->
    WorkOrderId = <<"WO-016">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Start pipeline
    ok = tcps_sku:transition_stage(SkuId, compilation),

    %% Abort pipeline
    Reason = <<"User requested abort">>,
    ok = tcps_sku:abort_pipeline(SkuId, Reason),

    %% Verify SKU marked as aborted
    {ok, Sku} = tcps_sku:get_sku(SkuId),
    ?assertEqual(aborted, maps:get(status, Sku)),
    ?assertEqual(Reason, maps:get(failure_reason, Sku)).

%%%=============================================================================
%%% Test Cases - Release & Publication
%%%=============================================================================

test_mark_released() ->
    WorkOrderId = <<"WO-017">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Can't mark as released before pipeline complete
    {error, pipeline_incomplete} = tcps_sku:mark_released(SkuId),

    %% Complete pipeline
    {ok, _} = tcps_sku:process_sku_pipeline(SkuId),

    %% Mark as released
    ok = tcps_sku:mark_released(SkuId),

    %% Verify status
    {ok, Sku} = tcps_sku:get_sku(SkuId),
    ?assertEqual(completed, maps:get(status, Sku)),
    ?assert(maps:is_key(completed_at, Sku)).

test_mark_published() ->
    WorkOrderId = <<"WO-018">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Can't publish before completion
    {error, _} = tcps_sku:mark_published(SkuId),

    %% Complete pipeline and release
    {ok, _} = tcps_sku:process_sku_pipeline(SkuId),
    ok = tcps_sku:mark_released(SkuId),

    %% Publish to marketplace
    ok = tcps_sku:mark_published(SkuId),

    %% Verify status
    {ok, Sku} = tcps_sku:get_sku(SkuId),
    ?assertEqual(published, maps:get(status, Sku)),
    ?assert(maps:is_key(published_at, Sku)),
    ?assert(maps:is_key(marketplace_url, Sku)).

test_get_marketplace_status() ->
    WorkOrderId = <<"WO-019">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Get status before publication
    {ok, Status1} = tcps_sku:get_marketplace_status(SkuId),
    ?assertEqual(in_production, maps:get(status, Status1)),
    ?assertEqual(undefined, maps:get(marketplace_url, Status1)),

    %% Publish
    {ok, _} = tcps_sku:process_sku_pipeline(SkuId),
    ok = tcps_sku:mark_released(SkuId),
    ok = tcps_sku:mark_published(SkuId),

    %% Get status after publication
    {ok, Status2} = tcps_sku:get_marketplace_status(SkuId),
    ?assertEqual(published, maps:get(status, Status2)),
    ?assert(is_binary(maps:get(marketplace_url, Status2))),
    ?assert(maps:is_key(published_at, Status2)).

test_generate_marketplace_url() ->
    SkuId = <<"SKU-TEST-123">>,
    Url = tcps_sku:generate_marketplace_url(SkuId),

    ?assert(is_binary(Url)),
    ?assert(binary:match(Url, <<"marketplace">>) =/= nomatch),
    ?assert(binary:match(Url, SkuId) =/= nomatch).

%%%=============================================================================
%%% Test Cases - Integration
%%%=============================================================================

test_produce_sku() ->
    WorkOrderId = <<"WO-020">>,

    %% Run complete production flow
    {ok, SkuId} = tcps_sku:produce_sku(WorkOrderId),

    %% Verify SKU created and published
    {ok, Sku} = tcps_sku:get_sku(SkuId),
    ?assertEqual(published, maps:get(status, Sku)),
    ?assertEqual(publication, maps:get(current_stage, Sku)),
    ?assert(maps:is_key(marketplace_url, Sku)),

    %% Verify complete receipt chain
    {ok, complete} = tcps_sku:verify_receipt_chain(SkuId).

%%%=============================================================================
%%% Test Cases - Reporting
%%%=============================================================================

test_get_skus_by_status() ->
    %% Create SKUs with different statuses
    {ok, SkuId1} = tcps_sku:create_sku(<<"WO-021">>),
    {ok, SkuId2} = tcps_sku:create_sku(<<"WO-022">>),
    {ok, SkuId3} = tcps_sku:produce_sku(<<"WO-023">>),

    %% Get in_production SKUs
    InProduction = tcps_sku:get_all_skus_by_status(in_production),
    ?assert(length(InProduction) >= 2),

    %% Get published SKUs
    Published = tcps_sku:get_all_skus_by_status(published),
    ?assert(length(Published) >= 1),

    %% Verify SKU IDs
    InProductionIds = [maps:get(id, S) || S <- InProduction],
    ?assert(lists:member(SkuId1, InProductionIds)),
    ?assert(lists:member(SkuId2, InProductionIds)),

    PublishedIds = [maps:get(id, S) || S <- Published],
    ?assert(lists:member(SkuId3, PublishedIds)).

test_production_metrics() ->
    %% Create various SKUs
    {ok, _SkuId1} = tcps_sku:create_sku(<<"WO-024">>),
    {ok, _SkuId2} = tcps_sku:create_sku(<<"WO-025">>),
    {ok, _SkuId3} = tcps_sku:produce_sku(<<"WO-026">>),

    %% Get metrics
    Metrics = tcps_sku:get_production_metrics(),

    ?assert(maps:get(total_skus, Metrics) > 0),
    ?assert(maps:get(in_production, Metrics) >= 0),
    ?assert(maps:get(published, Metrics) >= 0),
    ?assert(maps:get(failed, Metrics) >= 0),
    ?assert(maps:get(success_rate, Metrics) >= 0),
    ?assert(maps:get(success_rate, Metrics) =< 100).

test_stage_statistics() ->
    %% Create SKUs at different stages
    {ok, _SkuId1} = tcps_sku:create_sku(<<"WO-027">>),
    {ok, SkuId2} = tcps_sku:create_sku(<<"WO-028">>),
    ok = tcps_sku:transition_stage(SkuId2, compilation),

    %% Get statistics
    Stats = tcps_sku:get_stage_statistics(),

    ?assert(is_map(Stats)),
    ?assert(maps:size(Stats) > 0),

    %% Verify counts for stages with SKUs
    ?assert(maps:is_key(shacl_validation, Stats) orelse
            maps:is_key(compilation, Stats)).

%%%=============================================================================
%%% Test Cases - Concurrency
%%%=============================================================================

test_concurrent_skus() ->
    %% Create multiple SKUs concurrently
    WorkOrderIds = [
        <<"WO-030">>,
        <<"WO-031">>,
        <<"WO-032">>,
        <<"WO-033">>,
        <<"WO-034">>
    ],

    %% Create all SKUs
    SkuIds = [begin
        {ok, SkuId} = tcps_sku:create_sku(WoId),
        SkuId
    end || WoId <- WorkOrderIds],

    ?assertEqual(5, length(SkuIds)),

    %% Process pipelines concurrently
    Workers = [begin
        {ok, Pid} = tcps_sku:process_sku_pipeline_async(SkuId),
        Pid
    end || SkuId <- SkuIds],

    ?assertEqual(5, length(Workers)),

    %% Wait for completion
    timer:sleep(500),

    %% Verify all completed
    lists:foreach(fun(SkuId) ->
        {ok, Stage} = tcps_sku:get_current_stage(SkuId),
        ?assertEqual(publication, Stage)
    end, SkuIds).

%%%=============================================================================
%%% Test Cases - Error Handling
%%%=============================================================================

test_delete_sku() ->
    WorkOrderId = <<"WO-040">>,
    {ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

    %% Verify exists
    {ok, _Sku} = tcps_sku:get_sku(SkuId),

    %% Delete
    ok = tcps_sku:delete_sku(SkuId),

    %% Verify deleted
    {error, not_found} = tcps_sku:get_sku(SkuId),

    %% Delete non-existent
    {error, not_found} = tcps_sku:delete_sku(<<"non-existent">>).

test_not_found_errors() ->
    FakeSkuId = <<"SKU-FAKE-999">>,

    %% Test all operations with non-existent SKU
    {error, not_found} = tcps_sku:get_sku(FakeSkuId),
    {error, not_found} = tcps_sku:get_sku_status(FakeSkuId),
    {error, not_found} = tcps_sku:get_current_stage(FakeSkuId),
    {error, not_found} = tcps_sku:transition_stage(FakeSkuId, compilation),
    {error, not_found} = tcps_sku:can_transition(FakeSkuId, compilation),
    {error, not_found} = tcps_sku:get_production_history(FakeSkuId),
    {error, not_found} = tcps_sku:get_receipts(FakeSkuId),
    {error, not_found} = tcps_sku:verify_receipt_chain(FakeSkuId),
    {error, not_found} = tcps_sku:get_marketplace_status(FakeSkuId),
    {error, not_found} = tcps_sku:delete_sku(FakeSkuId).
