#!/usr/bin/env escript
%%%-----------------------------------------------------------------------------
%%% @doc TCPS Work Order Management System - Demo Script
%%%
%%% Demonstrates all major features of the work order system:
%%% - Pull signal creation from multiple sources
%%% - Work order lifecycle management
%%% - Queue management with priorities
%%% - SLA tracking
%%% - Dependency management
%%% - Reporting and metrics
%%%
%%% Usage:
%%%   escript examples/work_order_demo.erl
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-mode(compile).

main(_Args) ->
    io:format("~n"),
    io:format("╔══════════════════════════════════════════════════════════════╗~n"),
    io:format("║  TCPS Work Order Management System - Demonstration          ║~n"),
    io:format("╚══════════════════════════════════════════════════════════════╝~n"),
    io:format("~n"),

    %% Start the work order manager
    io:format("Starting TCPS Work Order Manager...~n"),
    {ok, _Pid} = tcps_work_order:start_link(),
    io:format("✓ Work Order Manager started~n~n"),

    %% Demo 1: Create work orders from different sources
    demo_pull_signals(),

    %% Demo 2: Work order lifecycle
    demo_lifecycle(),

    %% Demo 3: Queue management
    demo_queue_management(),

    %% Demo 4: Dependency management
    demo_dependencies(),

    %% Demo 5: SLA tracking
    demo_sla_tracking(),

    %% Demo 6: Reporting
    demo_reporting(),

    %% Cleanup
    io:format("~nStopping Work Order Manager...~n"),
    tcps_work_order:stop(),
    io:format("✓ Demo complete!~n~n"),
    ok.

%%%=============================================================================
%%% Demo Scenarios
%%%=============================================================================

demo_pull_signals() ->
    io:format("═══ Demo 1: Pull Signal Processing ═══~n~n"),

    %% GitHub Issue
    io:format("1. Creating work order from GitHub issue...~n"),
    IssueUrl = <<"https://github.com/erlmcp/erlmcp/issues/42">>,
    {ok, WO1} = tcps_work_order:create_from_github(IssueUrl),
    io:format("   ✓ Created: ~s~n", [WO1]),

    {ok, Status1} = tcps_work_order:get_work_order_status(WO1),
    WorkOrder1 = maps:get(work_order, Status1),
    io:format("   - Bucket: ~p~n", [maps:get(bucket, WorkOrder1)]),
    io:format("   - Priority: ~p~n", [maps:get(priority, WorkOrder1)]),
    io:format("   - Status: ~p~n", [maps:get(status, WorkOrder1)]),

    %% Security Advisory
    io:format("~n2. Creating critical security work order (CVE)...~n"),
    CVE = <<"CVE-2026-1234">>,
    {ok, WO2} = tcps_work_order:create_from_security_advisory(CVE),
    io:format("   ✓ Created: ~s~n", [WO2]),

    {ok, Status2} = tcps_work_order:get_work_order_status(WO2),
    WorkOrder2 = maps:get(work_order, Status2),
    io:format("   - Bucket: ~p (always security)~n", [maps:get(bucket, WorkOrder2)]),
    io:format("   - Priority: ~p (always critical)~n", [maps:get(priority, WorkOrder2)]),
    io:format("   - SLA: 24 hours~n"),

    %% Marketplace Event
    io:format("~n3. Creating work order from marketplace refund...~n"),
    RefundEvent = #{
        type => refund,
        customer_id => <<"cust-123">>,
        product => <<"mcp-server">>,
        reason => <<"quality issue">>,
        timestamp => erlang:timestamp()
    },
    {ok, WO3} = tcps_work_order:create_from_marketplace(RefundEvent),
    io:format("   ✓ Created: ~s~n", [WO3]),

    {ok, Status3} = tcps_work_order:get_work_order_status(WO3),
    WorkOrder3 = maps:get(work_order, Status3),
    io:format("   - Bucket: ~p (refunds go to reliability)~n",
              [maps:get(bucket, WorkOrder3)]),

    io:format("~n✓ Created 3 work orders from different pull signals~n~n"),
    timer:sleep(1000).

demo_lifecycle() ->
    io:format("═══ Demo 2: Work Order Lifecycle ═══~n~n"),

    %% Create work order
    io:format("1. Creating feature work order...~n"),
    PullSignal = #{
        type => github_issue,
        source => <<"https://github.com/erlmcp/erlmcp/issues/100">>,
        description => <<"Add dark mode support">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    },
    {ok, WorkOrderId} = tcps_work_order:create_work_order(PullSignal),
    io:format("   ✓ Created: ~s (status: queued)~n", [WorkOrderId]),

    %% Start work order
    io:format("~n2. Starting work order...~n"),
    ok = tcps_work_order:start_work_order(WorkOrderId),
    {ok, Status1} = tcps_work_order:get_work_order_status(WorkOrderId),
    io:format("   ✓ Status: ~p~n",
              [maps:get(status, maps:get(work_order, Status1))]),

    %% Progress through stages
    io:format("~n3. Progressing through development stages...~n"),
    Stages = [requirements, design, implementation, testing, integration],
    lists:foreach(fun(Stage) ->
        ok = tcps_work_order:progress_work_order(WorkOrderId, Stage),
        io:format("   ✓ Completed stage: ~p~n", [Stage]),
        timer:sleep(200)
    end, Stages),

    %% Complete
    io:format("~n4. Completing work order...~n"),
    SkuId = <<"sku-dark-mode">>,
    ok = tcps_work_order:complete_work_order(WorkOrderId, SkuId),
    {ok, Status2} = tcps_work_order:get_work_order_status(WorkOrderId),
    WorkOrder = maps:get(work_order, Status2),
    io:format("   ✓ Status: ~p~n", [maps:get(status, WorkOrder)]),
    io:format("   ✓ SKU: ~s~n", [maps:get(sku_id, WorkOrder)]),
    io:format("   ✓ Lead time: ~.2f hours~n",
              [maps:get(elapsed_hours, Status2)]),

    io:format("~n✓ Complete lifecycle demonstrated~n~n"),
    timer:sleep(1000).

demo_queue_management() ->
    io:format("═══ Demo 3: Queue Management ═══~n~n"),

    %% Create multiple work orders with different priorities
    io:format("1. Creating work orders with different priorities...~n"),

    Signals = [
        {<<"Low priority">>, [<<"enhancement">>]},
        {<<"High priority">>, [<<"bug">>, <<"critical">>]},
        {<<"Medium priority">>, [<<"bug">>, <<"medium">>]}
    ],

    lists:foreach(fun({Desc, Labels}) ->
        Signal = #{
            type => github_issue,
            source => <<"test">>,
            description => Desc,
            labels => Labels,
            metadata => #{}
        },
        {ok, WoId} = tcps_work_order:create_work_order(Signal),
        {ok, Status} = tcps_work_order:get_work_order_status(WoId),
        Priority = maps:get(priority, maps:get(work_order, Status)),
        io:format("   ✓ ~s: priority ~p~n", [Desc, Priority])
    end, Signals),

    %% Show queue order
    io:format("~n2. Queue is automatically ordered by priority:~n"),
    Queue = tcps_work_order:get_queue(reliability),
    lists:foreach(fun(WorkOrder) ->
        Desc = maps:get(description, WorkOrder),
        Priority = maps:get(priority, WorkOrder),
        io:format("   - ~s (priority ~p)~n", [Desc, Priority])
    end, Queue),

    %% Dequeue highest priority
    io:format("~n3. Dequeuing highest priority work order...~n"),
    {ok, DequeuedWorkOrder} = tcps_work_order:dequeue_next(reliability),
    io:format("   ✓ Started: ~s~n", [maps:get(description, DequeuedWorkOrder)]),
    io:format("   ✓ Status: ~p~n", [maps:get(status, DequeuedWorkOrder)]),

    io:format("~n✓ Queue management demonstrated~n~n"),
    timer:sleep(1000).

demo_dependencies() ->
    io:format("═══ Demo 4: Dependency Management ═══~n~n"),

    %% Create foundation work order
    io:format("1. Creating foundation library work order...~n"),
    FoundationSignal = #{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/200">>,
        description => <<"Build authentication library">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    },
    {ok, Foundation} = tcps_work_order:create_work_order(FoundationSignal),
    io:format("   ✓ Created: ~s~n", [Foundation]),

    %% Create dependent feature
    io:format("~n2. Creating feature that depends on foundation...~n"),
    FeatureSignal = #{
        type => github_issue,
        source => <<"https://github.com/org/repo/issues/201">>,
        description => <<"Add OAuth support">>,
        labels => [<<"enhancement">>],
        metadata => #{}
    },
    {ok, Feature} = tcps_work_order:create_work_order(FeatureSignal),
    io:format("   ✓ Created: ~s~n", [Feature]),

    %% Add dependency
    io:format("~n3. Adding dependency (feature depends on foundation)...~n"),
    ok = tcps_work_order:add_dependency(Feature, Foundation),
    {ok, Deps} = tcps_work_order:get_dependencies(Feature),
    io:format("   ✓ Blocked by: ~p~n", [maps:get(blocked_by, Deps)]),

    %% Try to start (will fail - blocked)
    io:format("~n4. Attempting to start blocked feature...~n"),
    {error, blocked_by_dependencies} = tcps_work_order:start_work_order(Feature),
    io:format("   ✗ Cannot start - blocked by dependencies~n"),

    %% Complete foundation
    io:format("~n5. Completing foundation work order...~n"),
    ok = tcps_work_order:start_work_order(Foundation),
    ok = tcps_work_order:complete_work_order(Foundation, <<"sku-auth-lib">>),
    io:format("   ✓ Foundation completed~n"),

    %% Feature now unblocked
    io:format("~n6. Feature automatically unblocked...~n"),
    {ok, FeatureStatus} = tcps_work_order:get_work_order_status(Feature),
    FeatureWO = maps:get(work_order, FeatureStatus),
    io:format("   ✓ Status: ~p (was blocked, now queued)~n",
              [maps:get(status, FeatureWO)]),

    io:format("~n✓ Dependency management demonstrated~n~n"),
    timer:sleep(1000).

demo_sla_tracking() ->
    io:format("═══ Demo 5: SLA Tracking ═══~n~n"),

    io:format("1. SLA deadlines by bucket:~n"),
    io:format("   - Security: 24 hours~n"),
    io:format("   - Reliability: 7 days~n"),
    io:format("   - Compliance: 7 days~n"),
    io:format("   - Cost: 30 days~n"),
    io:format("   - Features: 30 days~n"),
    io:format("   - Technical Debt: best effort~n"),

    %% Create work order and check SLA
    io:format("~n2. Creating work order and checking SLA status...~n"),
    PullSignal = #{
        type => cve,
        source => <<"CVE-2026-5678">>,
        description => <<"Security vulnerability">>,
        labels => [<<"security">>],
        metadata => #{}
    },
    {ok, WorkOrderId} = tcps_work_order:create_work_order(PullSignal),
    io:format("   ✓ Created security work order~n"),

    {ok, SlaStatus} = tcps_work_order:check_sla(WorkOrderId),
    io:format("   ✓ SLA Status: ~p~n", [SlaStatus]),

    %% Check for any breaches
    io:format("~n3. Checking for SLA breaches...~n"),
    Breaches = tcps_work_order:get_sla_breaches(),
    case Breaches of
        [] ->
            io:format("   ✓ No SLA breaches~n");
        _ ->
            io:format("   ✗ ~p work orders breached SLA~n", [length(Breaches)])
    end,

    io:format("~n✓ SLA tracking demonstrated~n~n"),
    timer:sleep(1000).

demo_reporting() ->
    io:format("═══ Demo 6: Reporting and Metrics ═══~n~n"),

    %% Generate report for today
    io:format("1. Generating daily work order report...~n"),
    Today = calendar:universal_time(),
    {Date, _} = Today,
    Report = tcps_work_order:generate_work_order_report({Date, Date}),

    io:format("~n   Report for ~p:~n", [Date]),
    io:format("   ════════════════════════════════════════~n"),
    io:format("   Total Work Orders: ~p~n",
              [maps:get(total_work_orders, Report)]),
    io:format("   Completed: ~p~n", [maps:get(completed, Report)]),
    io:format("   Cancelled: ~p~n", [maps:get(cancelled, Report)]),

    io:format("~n   By Bucket:~n"),
    ByBucket = maps:get(by_bucket, Report),
    lists:foreach(fun({Bucket, Count}) ->
        io:format("     - ~p: ~p~n", [Bucket, Count])
    end, maps:to_list(ByBucket)),

    io:format("~n   By Status:~n"),
    ByStatus = maps:get(by_status, Report),
    lists:foreach(fun({Status, Count}) ->
        io:format("     - ~p: ~p~n", [Status, Count])
    end, maps:to_list(ByStatus)),

    case maps:get(completed, Report) of
        0 ->
            ok;
        _ ->
            io:format("~n   Average Lead Time: ~.2f hours~n",
                      [maps:get(average_lead_time, Report)]),
            io:format("   SLA Compliance: ~.1f%~n",
                      [maps:get(sla_compliance_rate, Report)])
    end,

    %% Get metrics
    io:format("~n2. Current system metrics:~n"),
    Metrics = tcps_work_order:get_metrics({Date, Date}),
    io:format("   - Work in Progress: ~p~n",
              [maps:get(work_in_progress, Metrics)]),

    QueueDepths = maps:get(queue_depth_by_bucket, Metrics),
    io:format("   - Queue Depths:~n"),
    lists:foreach(fun({Bucket, Depth}) ->
        io:format("     · ~p: ~p~n", [Bucket, Depth])
    end, maps:to_list(QueueDepths)),

    io:format("~n✓ Reporting demonstrated~n~n"),
    timer:sleep(1000).
