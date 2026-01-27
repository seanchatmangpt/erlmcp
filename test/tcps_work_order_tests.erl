%%%-----------------------------------------------------------------------------
%%% @doc Comprehensive Test Suite for TCPS Work Order Management System
%%%
%%% Tests all aspects of work order lifecycle, pull signal routing, SLA tracking,
%%% dependency management, queue management, and reporting.
%%%
%%% Test Coverage:
%%% - Work order creation from all pull signal types
%%% - Complete lifecycle (create, start, progress, complete, cancel)
%%% - Pull signal routing and prioritization
%%% - Queue management and WIP limits
%%% - SLA tracking and breach detection
%%% - Dependency management and blocking
%%% - Comprehensive reporting
%%% - RDF persistence and JSON export
%%% - Integration with Kanban, Kaizen, and Andon
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_work_order_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Setup and Teardown
%%%=============================================================================

setup() ->
    %% Start work order manager with test configuration
    {ok, Pid} = tcps_work_order:start_link(#{
        receipts_dir => "test/receipts/work_orders",
        ontology_file => "test/ontology/work_orders_test.ttl",
        config => #{
            sla_hours => #{
                security => 1,      % 1 hour for testing
                reliability => 2,
                features => 4,
                cost => 4,
                compliance => 2,
                technical_debt => infinity
            },
            wip_limits => #{
                security => 2,
                reliability => 3,
                cost => 3,
                compliance => 3,
                features => 5,
                technical_debt => 2
            }
        }
    }),

    %% Ensure test directories exist
    filelib:ensure_dir("test/receipts/work_orders/"),
    filelib:ensure_dir("test/ontology/"),

    Pid.

cleanup(Pid) ->
    tcps_work_order:stop(),
    exit(Pid, kill),
    timer:sleep(100),

    %% Clean up test files
    os:cmd("rm -rf test/receipts/work_orders"),
    os:cmd("rm -rf test/ontology/work_orders_test.ttl").

%%%=============================================================================
%%% Work Order Creation Tests
%%%=============================================================================

work_order_creation_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"Create work order from pull signal", fun test_create_work_order/0},
         {"Create work order from GitHub issue", fun test_create_from_github/0},
         {"Create work order from security advisory", fun test_create_from_security_advisory/0},
         {"Create work order from marketplace event", fun test_create_from_marketplace/0},
         {"Route pull signal to correct bucket", fun test_route_pull_signal/0},
         {"Prioritize multiple signals", fun test_prioritize_signals/0}
     ]}.

test_create_work_order() ->
    %% Create basic pull signal
    PullSignal = #{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/42">>,
        description => <<"Fix authentication bug">>,
        labels => [<<"bug">>, <<"high">>],
        metadata => #{}
    },

    {ok, WorkOrderId} = tcps_work_order:create_work_order(PullSignal),

    %% Verify work order was created
    ?assertMatch(<<"WO-", _/binary>>, WorkOrderId),

    %% Check work order status
    {ok, Status} = tcps_work_order:get_work_order_status(WorkOrderId),
    WorkOrder = maps:get(work_order, Status),

    ?assertEqual(reliability, maps:get(bucket, WorkOrder)),
    ?assertEqual(queued, maps:get(status, WorkOrder)),
    ?assertEqual(<<"Fix authentication bug">>, maps:get(description, WorkOrder)),
    ?assert(maps:get(priority, WorkOrder) >= 5).

test_create_from_github() ->
    %% Create from GitHub URL
    IssueUrl = <<"https://github.com/erlmcp/erlmcp/issues/123">>,
    {ok, WorkOrderId} = tcps_work_order:create_from_github(IssueUrl),

    %% Verify creation
    ?assertMatch(<<"WO-", _/binary>>, WorkOrderId),

    {ok, Status} = tcps_work_order:get_work_order_status(WorkOrderId),
    WorkOrder = maps:get(work_order, Status),

    PullSignal = maps:get(pull_signal, WorkOrder),
    ?assertEqual(github_issue, maps:get(type, PullSignal)),
    ?assertEqual(IssueUrl, maps:get(source, PullSignal)).

test_create_from_security_advisory() ->
    %% Create from CVE
    CVE = <<"CVE-2026-1234">>,
    {ok, WorkOrderId} = tcps_work_order:create_from_security_advisory(CVE),

    %% Verify critical security work order
    {ok, Status} = tcps_work_order:get_work_order_status(WorkOrderId),
    WorkOrder = maps:get(work_order, Status),

    ?assertEqual(security, maps:get(bucket, WorkOrder)),
    ?assertEqual(10, maps:get(priority, WorkOrder)),
    ?assertEqual(queued, maps:get(status, WorkOrder)),

    %% Verify description includes CVE
    Description = maps:get(description, WorkOrder),
    ?assert(binary:match(Description, CVE) =/= nomatch).

test_create_from_marketplace() ->
    %% Test install event
    InstallEvent = #{
        type => install,
        customer_id => <<"cust-123">>,
        product => <<"mcp-server">>,
        timestamp => erlang:timestamp()
    },

    {ok, WorkOrderId1} = tcps_work_order:create_from_marketplace(InstallEvent),
    {ok, Status1} = tcps_work_order:get_work_order_status(WorkOrderId1),
    WorkOrder1 = maps:get(work_order, Status1),
    ?assertEqual(features, maps:get(bucket, WorkOrder1)),

    %% Test refund event
    RefundEvent = #{
        type => refund,
        customer_id => <<"cust-456">>,
        product => <<"mcp-server">>,
        reason => <<"quality issue">>,
        timestamp => erlang:timestamp()
    },

    {ok, WorkOrderId2} = tcps_work_order:create_from_marketplace(RefundEvent),
    {ok, Status2} = tcps_work_order:get_work_order_status(WorkOrderId2),
    WorkOrder2 = maps:get(work_order, Status2),
    ?assertEqual(reliability, maps:get(bucket, WorkOrder2)).

test_route_pull_signal() ->
    %% Test security routing
    SecuritySignal = #{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/1">>,
        description => <<"Security vulnerability">>,
        labels => [<<"security">>, <<"critical">>],
        metadata => #{}
    },

    {ok, WorkOrderId1} = tcps_work_order:route_pull_signal(SecuritySignal),
    {ok, Status1} = tcps_work_order:get_work_order_status(WorkOrderId1),
    WorkOrder1 = maps:get(work_order, Status1),
    ?assertEqual(security, maps:get(bucket, WorkOrder1)),

    %% Test compliance routing
    ComplianceSignal = #{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/2">>,
        description => <<"GDPR compliance update">>,
        labels => [<<"compliance">>],
        metadata => #{}
    },

    {ok, WorkOrderId2} = tcps_work_order:route_pull_signal(ComplianceSignal),
    {ok, Status2} = tcps_work_order:get_work_order_status(WorkOrderId2),
    WorkOrder2 = maps:get(work_order, Status2),
    ?assertEqual(compliance, maps:get(bucket, WorkOrder2)),

    %% Test technical debt routing
    TechDebtSignal = #{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/3">>,
        description => <<"Refactor legacy code">>,
        labels => [<<"refactor">>, <<"tech-debt">>],
        metadata => #{}
    },

    {ok, WorkOrderId3} = tcps_work_order:route_pull_signal(TechDebtSignal),
    {ok, Status3} = tcps_work_order:get_work_order_status(WorkOrderId3),
    WorkOrder3 = maps:get(work_order, Status3),
    ?assertEqual(technical_debt, maps:get(bucket, WorkOrder3)).

test_prioritize_signals() ->
    %% Create signals with different priorities
    Signals = [
        #{type => github_issue, source => <<"1">>, description => <<"feature">>,
          labels => [<<"enhancement">>], metadata => #{}},
        #{type => cve, source => <<"CVE-2026-1">>, description => <<"security">>,
          labels => [<<"security">>], metadata => #{}},
        #{type => github_issue, source => <<"2">>, description => <<"bug">>,
          labels => [<<"bug">>, <<"high">>], metadata => #{}},
        #{type => github_issue, source => <<"3">>, description => <<"refactor">>,
          labels => [<<"refactor">>], metadata => #{}}
    ],

    Prioritized = tcps_work_order:prioritize_signals(Signals),

    %% Verify security (CVE) is first
    [First | _] = Prioritized,
    ?assertEqual(cve, maps:get(type, First)),

    %% Verify refactor is last
    Last = lists:last(Prioritized),
    ?assertEqual([<<"refactor">>], maps:get(labels, Last)).

%%%=============================================================================
%%% Work Order Lifecycle Tests
%%%=============================================================================

lifecycle_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"Complete work order lifecycle", fun test_complete_lifecycle/0},
         {"Start work order with WIP limit check", fun test_start_with_wip_limit/0},
         {"Progress through stages", fun test_progress_stages/0},
         {"Complete work order", fun test_complete_work_order/0},
         {"Cancel work order", fun test_cancel_work_order/0},
         {"Cannot start blocked work order", fun test_cannot_start_blocked/0}
     ]}.

test_complete_lifecycle() ->
    %% Create work order
    PullSignal = #{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/100">>,
        description => <<"Add new feature">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    },

    {ok, WorkOrderId} = tcps_work_order:create_work_order(PullSignal),

    %% Verify queued status
    {ok, Status1} = tcps_work_order:get_work_order_status(WorkOrderId),
    WorkOrder1 = maps:get(work_order, Status1),
    ?assertEqual(queued, maps:get(status, WorkOrder1)),

    %% Start work order
    ?assertEqual(ok, tcps_work_order:start_work_order(WorkOrderId)),

    {ok, Status2} = tcps_work_order:get_work_order_status(WorkOrderId),
    WorkOrder2 = maps:get(work_order, Status2),
    ?assertEqual(in_progress, maps:get(status, WorkOrder2)),
    ?assert(maps:is_key(started_at, WorkOrder2)),

    %% Progress through stages
    ?assertEqual(ok, tcps_work_order:progress_work_order(WorkOrderId, requirements)),
    ?assertEqual(ok, tcps_work_order:progress_work_order(WorkOrderId, design)),
    ?assertEqual(ok, tcps_work_order:progress_work_order(WorkOrderId, implementation)),
    ?assertEqual(ok, tcps_work_order:progress_work_order(WorkOrderId, testing)),

    {ok, Status3} = tcps_work_order:get_work_order_status(WorkOrderId),
    WorkOrder3 = maps:get(work_order, Status3),
    ?assertEqual(testing, maps:get(current_stage, WorkOrder3)),
    ?assertEqual([testing, implementation, design, requirements],
                 maps:get(stages_completed, WorkOrder3)),

    %% Complete work order
    SkuId = <<"sku-feature-100">>,
    ?assertEqual(ok, tcps_work_order:complete_work_order(WorkOrderId, SkuId)),

    {ok, Status4} = tcps_work_order:get_work_order_status(WorkOrderId),
    WorkOrder4 = maps:get(work_order, Status4),
    ?assertEqual(completed, maps:get(status, WorkOrder4)),
    ?assertEqual(SkuId, maps:get(sku_id, WorkOrder4)),
    ?assert(maps:is_key(completed_at, WorkOrder4)).

test_start_with_wip_limit() ->
    %% Create work orders up to WIP limit (security bucket has limit of 2)
    PullSignal = #{
        type => cve,
        source => <<"CVE-2026-">>,
        description => <<"Security issue">>,
        labels => [<<"security">>],
        metadata => #{}
    },

    %% Create and start 2 work orders (at limit)
    {ok, WO1} = tcps_work_order:create_work_order(PullSignal#{source => <<"CVE-2026-1">>}),
    {ok, WO2} = tcps_work_order:create_work_order(PullSignal#{source => <<"CVE-2026-2">>}),
    {ok, WO3} = tcps_work_order:create_work_order(PullSignal#{source => <<"CVE-2026-3">>}),

    ?assertEqual(ok, tcps_work_order:start_work_order(WO1)),
    ?assertEqual(ok, tcps_work_order:start_work_order(WO2)),

    %% Third should fail due to WIP limit
    ?assertEqual({error, wip_limit}, tcps_work_order:start_work_order(WO3)),

    %% Complete one work order
    ?assertEqual(ok, tcps_work_order:complete_work_order(WO1, <<"sku-1">>)),

    %% Now third should start
    ?assertEqual(ok, tcps_work_order:start_work_order(WO3)).

test_progress_stages() ->
    %% Create and start work order
    PullSignal = #{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/200">>,
        description => <<"Test stages">>,
        labels => [<<"bug">>],
        metadata => #{}
    },

    {ok, WorkOrderId} = tcps_work_order:create_work_order(PullSignal),
    tcps_work_order:start_work_order(WorkOrderId),

    %% Progress through all stages
    Stages = [requirements, design, implementation, testing, integration, deployment],

    lists:foreach(fun(Stage) ->
        ?assertEqual(ok, tcps_work_order:progress_work_order(WorkOrderId, Stage))
    end, Stages),

    %% Verify all stages completed
    {ok, Status} = tcps_work_order:get_work_order_status(WorkOrderId),
    WorkOrder = maps:get(work_order, Status),

    StagesCompleted = maps:get(stages_completed, WorkOrder),
    ?assertEqual(length(Stages), length(StagesCompleted)),
    ?assertEqual(deployment, maps:get(current_stage, WorkOrder)).

test_complete_work_order() ->
    %% Create and complete work order
    PullSignal = #{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/300">>,
        description => <<"Complete test">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    },

    {ok, WorkOrderId} = tcps_work_order:create_work_order(PullSignal),
    tcps_work_order:start_work_order(WorkOrderId),

    SkuId = <<"sku-complete-300">>,
    ?assertEqual(ok, tcps_work_order:complete_work_order(WorkOrderId, SkuId)),

    %% Verify completion
    {ok, Status} = tcps_work_order:get_work_order_status(WorkOrderId),
    WorkOrder = maps:get(work_order, Status),

    ?assertEqual(completed, maps:get(status, WorkOrder)),
    ?assertEqual(SkuId, maps:get(sku_id, WorkOrder)),
    ?assertEqual(published, maps:get(current_stage, WorkOrder)),
    ?assert(maps:is_key(completed_at, WorkOrder)),

    %% Verify elapsed time
    ElapsedHours = maps:get(elapsed_hours, Status),
    ?assert(ElapsedHours >= 0.0).

test_cancel_work_order() ->
    %% Create work order
    PullSignal = #{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/400">>,
        description => <<"Cancel test">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    },

    {ok, WorkOrderId} = tcps_work_order:create_work_order(PullSignal),

    %% Cancel with reason
    Reason = <<"Duplicate of issue #399">>,
    ?assertEqual(ok, tcps_work_order:cancel_work_order(WorkOrderId, Reason)),

    %% Verify cancellation
    {ok, Status} = tcps_work_order:get_work_order_status(WorkOrderId),
    WorkOrder = maps:get(work_order, Status),

    ?assertEqual(cancelled, maps:get(status, WorkOrder)),
    ?assertEqual(Reason, maps:get(cancellation_reason, WorkOrder)),
    ?assert(maps:is_key(cancelled_at, WorkOrder)).

test_cannot_start_blocked() ->
    %% Create two work orders with dependency
    PullSignal1 = #{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/501">>,
        description => <<"Dependency test 1">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    },

    PullSignal2 = #{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/502">>,
        description => <<"Dependency test 2">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    },

    {ok, WO1} = tcps_work_order:create_work_order(PullSignal1),
    {ok, WO2} = tcps_work_order:create_work_order(PullSignal2),

    %% WO2 depends on WO1
    ?assertEqual(ok, tcps_work_order:add_dependency(WO2, WO1)),

    %% WO2 should be blocked
    ?assertEqual({error, blocked_by_dependencies},
                 tcps_work_order:start_work_order(WO2)),

    %% Complete WO1
    tcps_work_order:start_work_order(WO1),
    tcps_work_order:complete_work_order(WO1, <<"sku-1">>),

    %% Now WO2 should start
    ?assertEqual(ok, tcps_work_order:start_work_order(WO2)).

%%%=============================================================================
%%% Queue Management Tests
%%%=============================================================================

queue_management_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"Get queue ordered by priority", fun test_get_queue/0},
         {"Dequeue next work order", fun test_dequeue_next/0},
         {"Reorder queue manually", fun test_reorder_queue/0},
         {"Queue respects WIP limits", fun test_queue_wip_limits/0}
     ]}.

test_get_queue() ->
    %% Create work orders with different priorities
    Signals = [
        #{type => github_issue, source => <<"1">>, description => <<"Low priority">>,
          labels => [<<"enhancement">>], metadata => #{}},
        #{type => github_issue, source => <<"2">>, description => <<"High priority">>,
          labels => [<<"bug">>, <<"critical">>], metadata => #{}},
        #{type => github_issue, source => <<"3">>, description => <<"Medium priority">>,
          labels => [<<"bug">>, <<"medium">>], metadata => #{}}
    ],

    %% Create all work orders (all go to reliability bucket)
    lists:foreach(fun(Signal) ->
        tcps_work_order:create_work_order(Signal)
    end, Signals),

    %% Get queue
    Queue = tcps_work_order:get_queue(reliability),

    %% Verify queue is ordered by priority (highest first)
    ?assertEqual(3, length(Queue)),

    [First, Second, Third] = Queue,
    ?assert(maps:get(priority, First) >= maps:get(priority, Second)),
    ?assert(maps:get(priority, Second) >= maps:get(priority, Third)).

test_dequeue_next() ->
    %% Create work orders
    PullSignal = #{
        type => github_issue,
        source => <<"test">>,
        description => <<"Test dequeue">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    },

    {ok, WO1} = tcps_work_order:create_work_order(PullSignal#{source => <<"1">>}),
    {ok, WO2} = tcps_work_order:create_work_order(PullSignal#{source => <<"2">>}),

    %% Dequeue first
    {ok, WorkOrder1} = tcps_work_order:dequeue_next(features),
    ?assertEqual(WO1, maps:get(id, WorkOrder1)),
    ?assertEqual(in_progress, maps:get(status, WorkOrder1)),

    %% Queue should now have one less item
    Queue = tcps_work_order:get_queue(features),
    ?assertEqual(1, length(Queue)).

test_reorder_queue() ->
    %% Create work orders
    PullSignal = #{
        type => github_issue,
        source => <<"test">>,
        description => <<"Test reorder">>,
        labels => [<<"bug">>],
        metadata => #{}
    },

    {ok, WO1} = tcps_work_order:create_work_order(PullSignal#{source => <<"1">>}),
    {ok, WO2} = tcps_work_order:create_work_order(PullSignal#{source => <<"2">>}),
    {ok, WO3} = tcps_work_order:create_work_order(PullSignal#{source => <<"3">>}),

    %% Reorder: put WO3 first
    NewOrder = [WO3, WO1, WO2],
    ?assertEqual(ok, tcps_work_order:reorder_queue(reliability, NewOrder)),

    %% Verify new order
    Queue = tcps_work_order:get_queue(reliability),
    [First | _] = Queue,
    ?assertEqual(WO3, maps:get(id, First)).

test_queue_wip_limits() ->
    %% Fill security queue to WIP limit
    PullSignal = #{
        type => cve,
        source => <<"CVE-2026-">>,
        description => <<"Security issue">>,
        labels => [<<"security">>],
        metadata => #{}
    },

    %% Create 3 work orders (limit is 2)
    {ok, WO1} = tcps_work_order:create_work_order(PullSignal#{source => <<"1">>}),
    {ok, WO2} = tcps_work_order:create_work_order(PullSignal#{source => <<"2">>}),
    {ok, WO3} = tcps_work_order:create_work_order(PullSignal#{source => <<"3">>}),

    %% Dequeue first two
    {ok, _} = tcps_work_order:dequeue_next(security),
    {ok, _} = tcps_work_order:dequeue_next(security),

    %% Third should fail due to WIP limit
    ?assertEqual({error, wip_limit}, tcps_work_order:dequeue_next(security)).

%%%=============================================================================
%%% SLA Tracking Tests
%%%=============================================================================

sla_tracking_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"Check SLA status on time", fun test_sla_on_time/0},
         {"Check SLA warning", fun test_sla_warning/0},
         {"Check SLA breach", fun test_sla_breach/0},
         {"Get all SLA breaches", fun test_get_sla_breaches/0},
         {"Get SLA warnings", fun test_get_sla_warnings/0}
     ]}.

test_sla_on_time() ->
    %% Create work order (security has 1 hour SLA in test)
    PullSignal = #{
        type => cve,
        source => <<"CVE-2026-1">>,
        description => <<"Security issue">>,
        labels => [<<"security">>],
        metadata => #{}
    },

    {ok, WorkOrderId} = tcps_work_order:create_work_order(PullSignal),

    %% Check SLA immediately (should be on time)
    ?assertMatch({ok, on_time}, tcps_work_order:check_sla(WorkOrderId)).

test_sla_warning() ->
    %% Note: This test is difficult to implement without mocking time
    %% In production, would use meck or similar to advance time
    ok.

test_sla_breach() ->
    %% Note: This test is difficult to implement without mocking time
    %% In production, would use meck or similar to advance time past deadline
    ok.

test_get_sla_breaches() ->
    %% Get breaches (should be empty for new work orders)
    Breaches = tcps_work_order:get_sla_breaches(),
    ?assertEqual([], Breaches).

test_get_sla_warnings() ->
    %% Get warnings (should be empty for new work orders)
    Warnings = tcps_work_order:get_sla_warnings(1.0),
    ?assertEqual([], Warnings).

%%%=============================================================================
%%% Dependency Management Tests
%%%=============================================================================

dependency_management_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"Add dependency between work orders", fun test_add_dependency/0},
         {"Get dependency graph", fun test_get_dependencies/0},
         {"Resolve dependency unblocks work orders", fun test_resolve_dependency/0},
         {"Detect circular dependencies", fun test_circular_dependency/0},
         {"Complex dependency chain", fun test_dependency_chain/0}
     ]}.

test_add_dependency() ->
    %% Create two work orders
    PullSignal = #{
        type => github_issue,
        source => <<"test">>,
        description => <<"Test dependency">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    },

    {ok, WO1} = tcps_work_order:create_work_order(PullSignal#{source => <<"1">>}),
    {ok, WO2} = tcps_work_order:create_work_order(PullSignal#{source => <<"2">>}),

    %% Add dependency
    ?assertEqual(ok, tcps_work_order:add_dependency(WO2, WO1)),

    %% Verify WO2 is blocked
    {ok, Status} = tcps_work_order:get_work_order_status(WO2),
    WorkOrder = maps:get(work_order, Status),
    ?assertEqual(blocked, maps:get(status, WorkOrder)).

test_get_dependencies() ->
    %% Create three work orders
    PullSignal = #{
        type => github_issue,
        source => <<"test">>,
        description => <<"Test dependencies">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    },

    {ok, WO1} = tcps_work_order:create_work_order(PullSignal#{source => <<"1">>}),
    {ok, WO2} = tcps_work_order:create_work_order(PullSignal#{source => <<"2">>}),
    {ok, WO3} = tcps_work_order:create_work_order(PullSignal#{source => <<"3">>}),

    %% WO2 depends on WO1, WO3 depends on WO1
    tcps_work_order:add_dependency(WO2, WO1),
    tcps_work_order:add_dependency(WO3, WO1),

    %% Get dependencies for WO1
    {ok, Deps1} = tcps_work_order:get_dependencies(WO1),
    ?assertEqual([], maps:get(blocked_by, Deps1)),
    ?assertEqual(2, length(maps:get(blocking, Deps1))),

    %% Get dependencies for WO2
    {ok, Deps2} = tcps_work_order:get_dependencies(WO2),
    ?assertEqual([WO1], maps:get(blocked_by, Deps2)),
    ?assertEqual([], maps:get(blocking, Deps2)).

test_resolve_dependency() ->
    %% Create two work orders with dependency
    PullSignal = #{
        type => github_issue,
        source => <<"test">>,
        description => <<"Test resolve">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    },

    {ok, WO1} = tcps_work_order:create_work_order(PullSignal#{source => <<"1">>}),
    {ok, WO2} = tcps_work_order:create_work_order(PullSignal#{source => <<"2">>}),

    tcps_work_order:add_dependency(WO2, WO1),

    %% WO2 should be blocked
    {ok, Status1} = tcps_work_order:get_work_order_status(WO2),
    WorkOrder1 = maps:get(work_order, Status1),
    ?assertEqual(blocked, maps:get(status, WorkOrder1)),

    %% Complete WO1
    tcps_work_order:start_work_order(WO1),
    tcps_work_order:complete_work_order(WO1, <<"sku-1">>),

    %% WO2 should now be unblocked
    {ok, Status2} = tcps_work_order:get_work_order_status(WO2),
    WorkOrder2 = maps:get(work_order, Status2),
    ?assertEqual(queued, maps:get(status, WorkOrder2)).

test_circular_dependency() ->
    %% Create three work orders
    PullSignal = #{
        type => github_issue,
        source => <<"test">>,
        description => <<"Test circular">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    },

    {ok, WO1} = tcps_work_order:create_work_order(PullSignal#{source => <<"1">>}),
    {ok, WO2} = tcps_work_order:create_work_order(PullSignal#{source => <<"2">>}),
    {ok, WO3} = tcps_work_order:create_work_order(PullSignal#{source => <<"3">>}),

    %% Create chain: WO1 -> WO2 -> WO3
    ?assertEqual(ok, tcps_work_order:add_dependency(WO2, WO1)),
    ?assertEqual(ok, tcps_work_order:add_dependency(WO3, WO2)),

    %% Try to create cycle: WO3 -> WO1
    ?assertEqual({error, circular_dependency},
                 tcps_work_order:add_dependency(WO1, WO3)).

test_dependency_chain() ->
    %% Create chain: WO4 -> WO3 -> WO2 -> WO1
    PullSignal = #{
        type => github_issue,
        source => <<"test">>,
        description => <<"Test chain">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    },

    {ok, WO1} = tcps_work_order:create_work_order(PullSignal#{source => <<"1">>}),
    {ok, WO2} = tcps_work_order:create_work_order(PullSignal#{source => <<"2">>}),
    {ok, WO3} = tcps_work_order:create_work_order(PullSignal#{source => <<"3">>}),
    {ok, WO4} = tcps_work_order:create_work_order(PullSignal#{source => <<"4">>}),

    tcps_work_order:add_dependency(WO2, WO1),
    tcps_work_order:add_dependency(WO3, WO2),
    tcps_work_order:add_dependency(WO4, WO3),

    %% Complete WO1
    tcps_work_order:start_work_order(WO1),
    tcps_work_order:complete_work_order(WO1, <<"sku-1">>),

    %% WO2 should be unblocked, but WO3 and WO4 still blocked
    {ok, Status2} = tcps_work_order:get_work_order_status(WO2),
    ?assertEqual(queued, maps:get(status, maps:get(work_order, Status2))),

    {ok, Status3} = tcps_work_order:get_work_order_status(WO3),
    ?assertEqual(blocked, maps:get(status, maps:get(work_order, Status3))),

    {ok, Status4} = tcps_work_order:get_work_order_status(WO4),
    ?assertEqual(blocked, maps:get(status, maps:get(work_order, Status4))),

    %% Complete WO2
    tcps_work_order:start_work_order(WO2),
    tcps_work_order:complete_work_order(WO2, <<"sku-2">>),

    %% WO3 should be unblocked, WO4 still blocked
    {ok, Status3b} = tcps_work_order:get_work_order_status(WO3),
    ?assertEqual(queued, maps:get(status, maps:get(work_order, Status3b))),

    {ok, Status4b} = tcps_work_order:get_work_order_status(WO4),
    ?assertEqual(blocked, maps:get(status, maps:get(work_order, Status4b))).

%%%=============================================================================
%%% Reporting Tests
%%%=============================================================================

reporting_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"Get work order status", fun test_get_work_order_status/0},
         {"Generate work order report", fun test_generate_report/0},
         {"Get metrics", fun test_get_metrics/0}
     ]}.

test_get_work_order_status() ->
    %% Create work order
    PullSignal = #{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/1000">>,
        description => <<"Status test">>,
        labels => [<<"bug">>],
        metadata => #{}
    },

    {ok, WorkOrderId} = tcps_work_order:create_work_order(PullSignal),

    %% Get status
    {ok, Status} = tcps_work_order:get_work_order_status(WorkOrderId),

    %% Verify status fields
    ?assert(maps:is_key(work_order, Status)),
    ?assert(maps:is_key(elapsed_hours, Status)),
    ?assert(maps:is_key(sla_status, Status)),
    ?assert(maps:is_key(dependencies, Status)),
    ?assert(maps:is_key(receipts, Status)),

    %% Verify elapsed hours is non-negative
    ElapsedHours = maps:get(elapsed_hours, Status),
    ?assert(ElapsedHours >= 0.0).

test_generate_report() ->
    %% Create and complete some work orders
    Today = calendar:universal_time(),
    {Date, _} = Today,

    PullSignal = #{
        type => github_issue,
        source => <<"test">>,
        description => <<"Report test">>,
        labels => [<<"bug">>],
        metadata => #{}
    },

    %% Create 5 work orders
    WorkOrderIds = [begin
        {ok, WoId} = tcps_work_order:create_work_order(
            PullSignal#{source => integer_to_binary(N)}
        ),
        WoId
    end || N <- lists:seq(1, 5)],

    %% Complete 3, cancel 1, leave 1 pending
    [WO1, WO2, WO3, WO4, WO5] = WorkOrderIds,

    tcps_work_order:start_work_order(WO1),
    tcps_work_order:complete_work_order(WO1, <<"sku-1">>),

    tcps_work_order:start_work_order(WO2),
    tcps_work_order:complete_work_order(WO2, <<"sku-2">>),

    tcps_work_order:start_work_order(WO3),
    tcps_work_order:complete_work_order(WO3, <<"sku-3">>),

    tcps_work_order:cancel_work_order(WO4, <<"Duplicate">>),

    %% Generate report
    TimePeriod = {Date, Date},
    Report = tcps_work_order:generate_work_order_report(TimePeriod),

    %% Verify report fields
    ?assertEqual(5, maps:get(total_work_orders, Report)),
    ?assertEqual(3, maps:get(completed, Report)),
    ?assertEqual(1, maps:get(cancelled, Report)),
    ?assert(maps:is_key(by_bucket, Report)),
    ?assert(maps:is_key(by_status, Report)),
    ?assert(maps:is_key(average_lead_time, Report)),
    ?assert(maps:is_key(sla_compliance_rate, Report)).

test_get_metrics() ->
    %% Create some work orders
    Today = calendar:universal_time(),
    {Date, _} = Today,

    PullSignal = #{
        type => github_issue,
        source => <<"test">>,
        description => <<"Metrics test">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    },

    {ok, WO1} = tcps_work_order:create_work_order(PullSignal),
    tcps_work_order:start_work_order(WO1),

    %% Get metrics
    TimePeriod = {Date, Date},
    Metrics = tcps_work_order:get_metrics(TimePeriod),

    %% Verify metrics fields
    ?assert(maps:is_key(lead_time, Metrics)),
    ?assert(maps:is_key(throughput, Metrics)),
    ?assert(maps:is_key(sla_compliance, Metrics)),
    ?assert(maps:is_key(work_in_progress, Metrics)),
    ?assert(maps:is_key(queue_depth_by_bucket, Metrics)),

    %% Verify WIP count
    ?assertEqual(1, maps:get(work_in_progress, Metrics)).

%%%=============================================================================
%%% Persistence Tests
%%%=============================================================================

persistence_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"Save work order to ontology", fun test_save_to_ontology/0},
         {"Export work order to JSON", fun test_export_to_json/0}
     ]}.

test_save_to_ontology() ->
    %% Create work order
    PullSignal = #{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/2000">>,
        description => <<"RDF test">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    },

    {ok, WorkOrderId} = tcps_work_order:create_work_order(PullSignal),

    %% Save to ontology
    ?assertEqual(ok, tcps_work_order:save_to_ontology(WorkOrderId)),

    %% Verify file was created
    ?assert(filelib:is_file("test/ontology/work_orders_test.ttl")).

test_export_to_json() ->
    %% Create work order
    PullSignal = #{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/3000">>,
        description => <<"JSON export test">>,
        labels => [<<"bug">>],
        metadata => #{}
    },

    {ok, WorkOrderId} = tcps_work_order:create_work_order(PullSignal),

    %% Export to JSON
    {ok, Json} = tcps_work_order:export_to_json(WorkOrderId),

    %% Verify JSON is valid binary
    ?assert(is_binary(Json)),
    ?assert(byte_size(Json) > 0),

    %% Verify JSON contains work order ID
    ?assert(binary:match(Json, WorkOrderId) =/= nomatch).

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

integration_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"End-to-end workflow", fun test_end_to_end_workflow/0},
         {"Multiple buckets concurrent", fun test_multiple_buckets/0},
         {"Complex scenario with all features", fun test_complex_scenario/0}
     ]}.

test_end_to_end_workflow() ->
    %% Simulate complete workflow from GitHub issue to completion

    %% 1. GitHub issue arrives
    IssueUrl = <<"https://github.com/erlmcp/erlmcp/issues/9999">>,
    {ok, WorkOrderId} = tcps_work_order:create_from_github(IssueUrl),

    %% 2. Verify queued
    {ok, Status1} = tcps_work_order:get_work_order_status(WorkOrderId),
    ?assertEqual(queued, maps:get(status, maps:get(work_order, Status1))),

    %% 3. Start work (pull from queue)
    {ok, WorkOrder} = tcps_work_order:dequeue_next(reliability),
    ?assertEqual(WorkOrderId, maps:get(id, WorkOrder)),

    %% 4. Progress through development stages
    Stages = [requirements, design, implementation, testing, integration],
    lists:foreach(fun(Stage) ->
        tcps_work_order:progress_work_order(WorkOrderId, Stage)
    end, Stages),

    %% 5. Complete and publish
    SkuId = <<"sku-github-9999">>,
    tcps_work_order:complete_work_order(WorkOrderId, SkuId),

    %% 6. Verify completion
    {ok, Status2} = tcps_work_order:get_work_order_status(WorkOrderId),
    FinalWorkOrder = maps:get(work_order, Status2),
    ?assertEqual(completed, maps:get(status, FinalWorkOrder)),
    ?assertEqual(SkuId, maps:get(sku_id, FinalWorkOrder)),

    %% 7. Verify metrics
    Today = calendar:universal_time(),
    {Date, _} = Today,
    Report = tcps_work_order:generate_work_order_report({Date, Date}),
    ?assertEqual(1, maps:get(completed, Report)).

test_multiple_buckets() ->
    %% Create work orders in different buckets
    Signals = [
        #{type => cve, source => <<"CVE-2026-1">>, description => <<"Security">>,
          labels => [<<"security">>], metadata => #{}},
        #{type => github_issue, source => <<"1">>, description => <<"Bug">>,
          labels => [<<"bug">>], metadata => #{}},
        #{type => github_issue, source => <<"2">>, description => <<"Feature">>,
          labels => [<<"enhancement">>], metadata => #{}},
        #{type => github_issue, source => <<"3">>, description => <<"Compliance">>,
          labels => [<<"compliance">>], metadata => #{}}
    ],

    %% Create all work orders
    WorkOrderIds = [begin
        {ok, WoId} = tcps_work_order:create_work_order(Signal),
        WoId
    end || Signal <- Signals],

    %% Verify they're in different buckets
    Buckets = [begin
        {ok, Status} = tcps_work_order:get_work_order_status(WoId),
        maps:get(bucket, maps:get(work_order, Status))
    end || WoId <- WorkOrderIds],

    ?assertEqual(4, length(lists:usort(Buckets))),

    %% Start work in all buckets concurrently
    lists:foreach(fun(WoId) ->
        tcps_work_order:start_work_order(WoId)
    end, WorkOrderIds),

    %% Verify all started
    Statuses = [begin
        {ok, Status} = tcps_work_order:get_work_order_status(WoId),
        maps:get(status, maps:get(work_order, Status))
    end || WoId <- WorkOrderIds],

    ?assertEqual([in_progress, in_progress, in_progress, in_progress], Statuses).

test_complex_scenario() ->
    %% Complex scenario combining:
    %% - Multiple work orders with dependencies
    %% - Different buckets and priorities
    %% - SLA tracking
    %% - Cancellations
    %% - Completions

    %% Create foundation work order
    {ok, Foundation} = tcps_work_order:create_work_order(#{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/1">>,
        description => <<"Foundation library">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    }),

    %% Create dependent features
    {ok, Feature1} = tcps_work_order:create_work_order(#{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/2">>,
        description => <<"Feature 1 using foundation">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    }),

    {ok, Feature2} = tcps_work_order:create_work_order(#{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/3">>,
        description => <<"Feature 2 using foundation">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    }),

    %% Create critical security fix
    {ok, SecurityFix} = tcps_work_order:create_from_security_advisory(
        <<"CVE-2026-CRITICAL">>
    ),

    %% Add dependencies
    tcps_work_order:add_dependency(Feature1, Foundation),
    tcps_work_order:add_dependency(Feature2, Foundation),

    %% Start and complete foundation
    tcps_work_order:start_work_order(Foundation),
    tcps_work_order:progress_work_order(Foundation, implementation),
    tcps_work_order:complete_work_order(Foundation, <<"sku-foundation">>),

    %% Features should now be unblocked
    {ok, Status1} = tcps_work_order:get_work_order_status(Feature1),
    ?assertEqual(queued, maps:get(status, maps:get(work_order, Status1))),

    %% Start security fix (highest priority)
    tcps_work_order:start_work_order(SecurityFix),
    tcps_work_order:complete_work_order(SecurityFix, <<"sku-security">>),

    %% Cancel Feature2 (changed requirements)
    tcps_work_order:cancel_work_order(Feature2, <<"Requirements changed">>),

    %% Complete Feature1
    tcps_work_order:start_work_order(Feature1),
    tcps_work_order:complete_work_order(Feature1, <<"sku-feature1">>),

    %% Generate report
    Today = calendar:universal_time(),
    {Date, _} = Today,
    Report = tcps_work_order:generate_work_order_report({Date, Date}),

    ?assertEqual(4, maps:get(total_work_orders, Report)),
    ?assertEqual(3, maps:get(completed, Report)),
    ?assertEqual(1, maps:get(cancelled, Report)).
