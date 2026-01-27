# TCPS Work Order Management System

**Version:** 1.0.0
**Module:** `tcps_work_order`
**Last Updated:** 2026-01-26

## Overview

The TCPS Work Order Management System implements Toyota Production System principles for demand-driven software development. It provides comprehensive work order lifecycle management with pull signal processing, queue management, SLA tracking, dependency resolution, and integration with Kanban WIP limits and Kaizen continuous improvement.

## Table of Contents

1. [Architecture](#architecture)
2. [Core Concepts](#core-concepts)
3. [Pull Signal Sources](#pull-signal-sources)
4. [Bucket Classification](#bucket-classification)
5. [Work Order Lifecycle](#work-order-lifecycle)
6. [API Reference](#api-reference)
7. [SLA Management](#sla-management)
8. [Dependency Management](#dependency-management)
9. [Queue Management](#queue-management)
10. [Reporting and Metrics](#reporting-and-metrics)
11. [Integration](#integration)
12. [Examples](#examples)
13. [Testing](#testing)

## Architecture

### System Design

```
Pull Signals (Demand)
       ↓
  ┌─────────────────────────────────┐
  │   Work Order Creation Layer     │
  │  - GitHub Issues/PRs            │
  │  - Security Advisories (CVE)    │
  │  - Marketplace Events           │
  │  - Internal Requests            │
  └─────────────────────────────────┘
       ↓
  ┌─────────────────────────────────┐
  │   Routing & Prioritization      │
  │  - Bucket assignment            │
  │  - Priority calculation         │
  │  - SLA deadline setting         │
  └─────────────────────────────────┘
       ↓
  ┌─────────────────────────────────┐
  │   Queue Management              │
  │  - Priority-ordered queues      │
  │  - WIP limit enforcement        │
  │  - Dependency blocking          │
  └─────────────────────────────────┘
       ↓
  ┌─────────────────────────────────┐
  │   Work Order Lifecycle          │
  │  - Start (check WIP/deps)       │
  │  - Progress through stages      │
  │  - Complete/Cancel              │
  │  - Receipt generation           │
  └─────────────────────────────────┘
       ↓
  ┌─────────────────────────────────┐
  │   Integration Layer             │
  │  - Kanban (WIP management)      │
  │  - Kaizen (metrics)             │
  │  - Andon (quality alerts)       │
  │  - RDF Ontology (persistence)   │
  └─────────────────────────────────┘
```

### Data Flow

1. **Pull Signal Arrival**: External event triggers work order creation
2. **Routing**: Signal analyzed and routed to appropriate bucket
3. **Prioritization**: Priority calculated based on type, bucket, labels
4. **Queueing**: Work order added to bucket queue in priority order
5. **Execution**: Work order dequeued when WIP capacity available
6. **Progress Tracking**: Stages completed, receipts generated
7. **Completion**: SKU linked, metrics recorded, dependencies resolved
8. **Persistence**: Saved to RDF ontology and JSON receipts

## Core Concepts

### Work Order

A work order represents customer demand for production. It is a pull signal that triggers the creation and delivery of a SKU through the TCPS pipeline.

**Key Properties:**
- **ID**: Unique identifier (format: `WO-timestamp-random`)
- **Bucket**: Classification (reliability, security, cost, compliance, features, technical_debt)
- **Priority**: 1-10 (10 = critical)
- **Status**: Lifecycle state (queued, in_progress, blocked, completed, cancelled)
- **Pull Signal**: Original demand source
- **SLA Deadline**: Required completion time
- **Dependencies**: Blocking/blocked relationships

### Pull Signal

A pull signal is an event that creates demand for production. Sources include:
- GitHub issues and pull requests
- Security advisories (CVE)
- Marketplace install/refund events
- Internal team requests

Each signal type has routing rules that determine bucket assignment and priority.

### Bucket

Work orders are classified into buckets based on their purpose:

| Bucket | Purpose | SLA | Priority Range | Example |
|--------|---------|-----|----------------|---------|
| **Security** | Security vulnerabilities | 24 hours | 8-10 | CVE fixes |
| **Reliability** | Bugs, production issues | 7 days | 5-9 | Crash fixes |
| **Compliance** | Legal, regulatory | 7 days | 4-8 | GDPR updates |
| **Cost** | Performance optimization | 30 days | 3-7 | Query tuning |
| **Features** | New functionality | 30 days | 2-6 | Dark mode |
| **Technical Debt** | Refactoring | Best effort | 1-4 | Code cleanup |

## Pull Signal Sources

### GitHub Issues

**Pull Signal Type**: `github_issue`

**Routing Logic**:
- Labels determine bucket:
  - `security` → Security bucket
  - `bug` → Reliability bucket
  - `compliance` → Compliance bucket
  - `refactor`, `tech-debt` → Technical Debt bucket
  - `performance` → Cost bucket
  - `enhancement` → Features bucket

**Priority Calculation**:
- `critical` label → Priority 9
- `high` label → Priority 7
- `medium` label → Priority 5
- `low` or no label → Priority 3

**Example**:
```erlang
IssueUrl = <<"https://github.com/erlmcp/erlmcp/issues/123">>,
{ok, WorkOrderId} = tcps_work_order:create_from_github(IssueUrl).
```

### Security Advisories (CVE)

**Pull Signal Type**: `cve`

**Routing Logic**:
- Always routed to Security bucket
- Always priority 10 (critical)
- SLA: 24 hours
- Triggers Andon alert

**Example**:
```erlang
CVE = <<"CVE-2026-1234">>,
{ok, WorkOrderId} = tcps_work_order:create_from_security_advisory(CVE).
```

### Marketplace Events

**Pull Signal Types**: `marketplace_install`, `marketplace_refund`

**Routing Logic**:
- **Install events** → Features bucket (demand for new features)
- **Refund events** → Reliability bucket (quality issue to investigate)

**Example**:
```erlang
Event = #{
    type => refund,
    customer_id => <<"cust-123">>,
    product => <<"mcp-server">>,
    reason => <<"quality issue">>,
    timestamp => erlang:timestamp()
},
{ok, WorkOrderId} = tcps_work_order:create_from_marketplace(Event).
```

### Internal Requests

**Pull Signal Type**: `internal_request`

**Routing Logic**:
- Bucket specified in metadata
- Priority based on internal urgency

**Example**:
```erlang
PullSignal = #{
    type => internal_request,
    source => <<"tech-debt-001">>,
    description => <<"Refactor authentication module">>,
    labels => [<<"refactor">>],
    metadata => #{bucket => technical_debt, urgency => low}
},
{ok, WorkOrderId} = tcps_work_order:create_work_order(PullSignal).
```

## Bucket Classification

### Security Bucket

**Purpose**: Critical security vulnerabilities and compliance issues

**SLA**: 24 hours

**Priority Range**: 8-10

**WIP Limit**: 5 (configurable)

**Quality Gates**:
- Security scanning (Bandit)
- Penetration testing
- Vulnerability assessment
- Compliance verification

**Triggers Andon**: Yes, for SLA breaches and critical CVEs

### Reliability Bucket

**Purpose**: Production bugs, crashes, data corruption

**SLA**: 7 days

**Priority Range**: 5-9

**WIP Limit**: 5 (configurable)

**Quality Gates**:
- Test coverage ≥ 80%
- Integration tests passing
- Performance benchmarks
- Load testing

### Compliance Bucket

**Purpose**: Regulatory requirements, legal obligations

**SLA**: 7 days

**Priority Range**: 4-8

**WIP Limit**: 5 (configurable)

**Quality Gates**:
- Compliance verification
- Audit trail completeness
- Documentation requirements
- Legal review

### Cost Bucket

**Purpose**: Performance optimization, resource efficiency

**SLA**: 30 days

**Priority Range**: 3-7

**WIP Limit**: 5 (configurable)

**Quality Gates**:
- Benchmark improvements
- Resource usage metrics
- Cost analysis
- Profiling results

### Features Bucket

**Purpose**: New functionality, enhancements

**SLA**: 30 days

**Priority Range**: 2-6

**WIP Limit**: 10 (higher capacity for features)

**Quality Gates**:
- User acceptance testing
- Integration tests
- Documentation
- Example code

### Technical Debt Bucket

**Purpose**: Code quality, refactoring, maintainability

**SLA**: Best effort (no strict deadline)

**Priority Range**: 1-4

**WIP Limit**: 5 (configurable)

**Quality Gates**:
- Code quality metrics
- Maintainability index
- Technical debt reduction
- Test improvements

## Work Order Lifecycle

### States

```
┌─────────┐
│ Pending │ (Created, awaiting validation)
└────┬────┘
     ↓
┌────────┐
│ Queued │ (In priority queue, waiting for WIP capacity)
└────┬───┘
     ↓
┌──────────────┐
│ In Progress  │ (Active work, consuming WIP slot)
└──────┬───────┘
       ↓
   ┌───────┐
   │Blocked│ (Waiting for dependency resolution)
   └───┬───┘
       ↓
┌──────────┐     ┌───────────┐
│Completed │ or  │ Cancelled │
└──────────┘     └───────────┘
```

### Lifecycle Functions

#### 1. Create

```erlang
PullSignal = #{
    type => github_issue,
    source => <<"https://github.com/org/repo/issues/42">>,
    description => <<"Fix authentication bug">>,
    labels => [<<"bug">>, <<"high">>],
    metadata => #{}
},
{ok, WorkOrderId} = tcps_work_order:create_work_order(PullSignal).
```

**Actions**:
- Generate unique ID
- Determine bucket and priority
- Calculate SLA deadline
- Add to queue (priority-ordered)
- Generate creation receipt
- Save to ontology

#### 2. Start

```erlang
ok = tcps_work_order:start_work_order(WorkOrderId).
```

**Checks**:
- WIP limit not exceeded
- Dependencies resolved
- Work order status is `queued`

**Actions**:
- Mark as `in_progress`
- Remove from queue
- Add to active work orders
- Increment WIP counter
- Generate start receipt
- Notify Kanban

**Possible Results**:
- `ok` - Successfully started
- `{error, wip_limit}` - WIP capacity full
- `{error, blocked_by_dependencies}` - Dependencies not resolved
- `{error, not_found}` - Work order doesn't exist

#### 3. Progress

```erlang
ok = tcps_work_order:progress_work_order(WorkOrderId, implementation).
```

**Actions**:
- Update `current_stage`
- Add stage to `stages_completed`
- Generate stage receipt
- Check quality gates (optional)

**Stages**:
1. `requirements` - Requirements analysis
2. `design` - Architecture and design
3. `implementation` - Code development
4. `testing` - Testing and validation
5. `integration` - System integration
6. `deployment` - Production deployment
7. `published` - Available to customers

#### 4. Complete

```erlang
SkuId = <<"sku-feature-42">>,
ok = tcps_work_order:complete_work_order(WorkOrderId, SkuId).
```

**Actions**:
- Mark as `completed`
- Link to SKU
- Calculate lead time
- Generate completion receipt
- Free WIP slot
- Resolve dependencies (unblock dependents)
- Update Kaizen metrics
- Save to ontology

#### 5. Cancel

```erlang
Reason = <<"Duplicate of issue #41">>,
ok = tcps_work_order:cancel_work_order(WorkOrderId, Reason).
```

**Actions**:
- Mark as `cancelled`
- Record cancellation reason
- Generate cancellation receipt
- Free WIP slot (if in progress)
- Remove from queue (if queued)

## API Reference

### Work Order Creation

#### `create_work_order/1`

Create work order from pull signal.

```erlang
-spec create_work_order(PullSignal :: pull_signal()) ->
    {ok, work_order_id()} | {error, term()}.
```

**Example**:
```erlang
PullSignal = #{
    type => github_issue,
    source => <<"https://github.com/org/repo/issues/1">>,
    description => <<"Add feature X">>,
    labels => [<<"enhancement">>],
    metadata => #{}
},
{ok, WorkOrderId} = tcps_work_order:create_work_order(PullSignal).
```

#### `create_from_github/1`

Create work order from GitHub issue URL.

```erlang
-spec create_from_github(IssueUrl :: binary()) ->
    {ok, work_order_id()} | {error, term()}.
```

**Example**:
```erlang
{ok, WorkOrderId} = tcps_work_order:create_from_github(
    <<"https://github.com/erlmcp/erlmcp/issues/123">>
).
```

#### `create_from_security_advisory/1`

Create urgent security work order from CVE.

```erlang
-spec create_from_security_advisory(CVE :: binary()) ->
    {ok, work_order_id()}.
```

**Example**:
```erlang
{ok, WorkOrderId} = tcps_work_order:create_from_security_advisory(
    <<"CVE-2026-1234">>
).
```

#### `create_from_marketplace/1`

Create work order from marketplace event.

```erlang
-spec create_from_marketplace(Event :: map()) ->
    {ok, work_order_id()} | {error, term()}.
```

### Work Order Lifecycle

#### `start_work_order/1`

Start work order execution (check WIP limits and dependencies).

```erlang
-spec start_work_order(WorkOrderId :: work_order_id()) ->
    ok | {error, wip_limit | not_found | already_started}.
```

#### `progress_work_order/2`

Update work order to new stage.

```erlang
-spec progress_work_order(WorkOrderId :: work_order_id(), Stage :: stage()) ->
    ok | {error, term()}.
```

#### `complete_work_order/2`

Complete work order and link to SKU.

```erlang
-spec complete_work_order(WorkOrderId :: work_order_id(), SkuId :: sku_id()) ->
    ok | {error, term()}.
```

#### `cancel_work_order/2`

Cancel work order with reason.

```erlang
-spec cancel_work_order(WorkOrderId :: work_order_id(), Reason :: binary()) ->
    ok | {error, term()}.
```

### Queue Management

#### `get_queue/1`

Get pending work orders for bucket (priority-ordered).

```erlang
-spec get_queue(Bucket :: bucket()) -> [work_order()].
```

**Example**:
```erlang
Queue = tcps_work_order:get_queue(security),
io:format("Security queue has ~p work orders~n", [length(Queue)]).
```

#### `dequeue_next/1`

Dequeue and start highest priority work order.

```erlang
-spec dequeue_next(Bucket :: bucket()) ->
    {ok, work_order()} | {empty} | {error, wip_limit}.
```

**Example**:
```erlang
case tcps_work_order:dequeue_next(reliability) of
    {ok, WorkOrder} ->
        io:format("Started work order: ~p~n", [maps:get(id, WorkOrder)]);
    {error, wip_limit} ->
        io:format("WIP limit reached, work order queued~n");
    empty ->
        io:format("No work orders in queue~n")
end.
```

#### `reorder_queue/2`

Manually reorder queue (emergency reprioritization).

```erlang
-spec reorder_queue(Bucket :: bucket(), NewOrder :: [work_order_id()]) ->
    ok | {error, term()}.
```

### SLA Tracking

#### `check_sla/1`

Check SLA status for work order.

```erlang
-spec check_sla(WorkOrderId :: work_order_id()) ->
    {ok, on_time} | {warning, float()} | {breached, float()} | {error, not_found}.
```

**Returns**:
- `{ok, on_time}` - Within SLA
- `{warning, Hours}` - < 25% time remaining
- `{breached, Hours}` - Past deadline (Hours overdue)

#### `get_sla_breaches/0`

Get all work orders past SLA deadline.

```erlang
-spec get_sla_breaches() -> [sla_check()].
```

**Example**:
```erlang
Breaches = tcps_work_order:get_sla_breaches(),
lists:foreach(fun(Breach) ->
    #{work_order_id := WoId, overdue_hours := Hours} = Breach,
    io:format("ALERT: ~s is ~.1f hours overdue!~n", [WoId, Hours])
end, Breaches).
```

#### `get_sla_warnings/1`

Get work orders approaching SLA deadline.

```erlang
-spec get_sla_warnings(WarningHours :: float()) -> [sla_check()].
```

### Dependency Management

#### `add_dependency/2`

Add dependency between work orders.

```erlang
-spec add_dependency(WorkOrderId :: work_order_id(),
                     DependsOn :: work_order_id()) -> ok | {error, term()}.
```

**Example**:
```erlang
% WO-002 depends on WO-001 completing first
ok = tcps_work_order:add_dependency(
    <<"WO-002">>,
    <<"WO-001">>
).
```

**Prevents**:
- Circular dependencies
- Invalid work order IDs
- Dependency cycles

#### `get_dependencies/1`

Get dependency graph for work order.

```erlang
-spec get_dependencies(WorkOrderId :: work_order_id()) ->
    {ok, dependency_graph()} | {error, not_found}.
```

**Example**:
```erlang
{ok, Deps} = tcps_work_order:get_dependencies(WorkOrderId),
#{blocking := Blocking, blocked_by := BlockedBy} = Deps,
io:format("This work order blocks: ~p~n", [Blocking]),
io:format("This work order is blocked by: ~p~n", [BlockedBy]).
```

### Reporting

#### `get_work_order_status/1`

Get complete status for work order.

```erlang
-spec get_work_order_status(WorkOrderId :: work_order_id()) ->
    {ok, map()} | {error, not_found}.
```

**Returns**:
```erlang
#{
    work_order => WorkOrder,
    elapsed_hours => 12.5,
    sla_status => {warning, 6.0},
    dependencies => #{blocking => [], blocked_by => [<<"WO-001">>]},
    receipts => [<<"receipt-1">>, <<"receipt-2">>]
}
```

#### `generate_work_order_report/1`

Generate comprehensive report for time period.

```erlang
-spec generate_work_order_report(TimePeriod :: {calendar:date(), calendar:date()}) ->
    work_order_report().
```

**Example**:
```erlang
Today = calendar:universal_time(),
{Date, _} = Today,
Report = tcps_work_order:generate_work_order_report({Date, Date}),

#{
    total_work_orders := Total,
    by_bucket := ByBucket,
    completed := Completed,
    average_lead_time := AvgLeadTime,
    sla_compliance_rate := SlaRate
} = Report,

io:format("Report for ~p:~n", [Date]),
io:format("  Total: ~p work orders~n", [Total]),
io:format("  Completed: ~p (~.1f%)~n", [Completed, Completed/Total*100]),
io:format("  Avg Lead Time: ~.1f hours~n", [AvgLeadTime]),
io:format("  SLA Compliance: ~.1f%~n", [SlaRate]).
```

## SLA Management

### SLA Configuration

Default SLA hours by bucket:

```erlang
#{
    security => 24,          % 24 hours
    reliability => 168,      % 7 days
    compliance => 168,       % 7 days
    cost => 720,            % 30 days
    features => 720,        % 30 days
    technical_debt => infinity  % No strict deadline
}
```

### SLA Calculation

SLA deadline is calculated at work order creation:

```erlang
SlaDeadline = CreatedAt + SlaHours
```

### SLA Status

Three states:

1. **On Time**: Current time < SLA deadline
2. **Warning**: Time remaining < 25% of total SLA
3. **Breached**: Current time > SLA deadline

### Andon Integration

SLA breaches trigger Andon alerts for:
- **Security bucket**: All breaches (critical)
- **Reliability bucket**: Breaches > 24 hours overdue
- **Other buckets**: Breaches > 48 hours overdue

## Dependency Management

### Dependency Rules

1. **No Circular Dependencies**: A → B → C → A is rejected
2. **Transitive Blocking**: If A blocks B, and B blocks C, then A indirectly blocks C
3. **Automatic Unblocking**: When dependency completes, blocked work orders transition from `blocked` to `queued`

### Dependency Scenarios

#### Simple Dependency Chain

```erlang
% Create foundation library
{ok, Foundation} = tcps_work_order:create_work_order(FoundationSignal),

% Create feature that depends on foundation
{ok, Feature1} = tcps_work_order:create_work_order(Feature1Signal),
ok = tcps_work_order:add_dependency(Feature1, Foundation),

% Feature1 is now blocked
{ok, Status} = tcps_work_order:get_work_order_status(Feature1),
blocked = maps:get(status, maps:get(work_order, Status)),

% Complete foundation
tcps_work_order:start_work_order(Foundation),
tcps_work_order:complete_work_order(Foundation, <<"sku-foundation">>),

% Feature1 is automatically unblocked
{ok, Status2} = tcps_work_order:get_work_order_status(Feature1),
queued = maps:get(status, maps:get(work_order, Status2)).
```

#### Multiple Dependencies

```erlang
% Feature depends on both Auth and DB modules
ok = tcps_work_order:add_dependency(Feature, AuthModule),
ok = tcps_work_order:add_dependency(Feature, DbModule),

% Feature remains blocked until BOTH complete
tcps_work_order:complete_work_order(AuthModule, <<"sku-auth">>),
% Feature still blocked

tcps_work_order:complete_work_order(DbModule, <<"sku-db">>),
% Feature now unblocked
```

## Queue Management

### Queue Structure

Each bucket maintains a priority-ordered queue:

```erlang
#{
    security => [WO-003, WO-001, WO-002],      % Priority: 10, 9, 8
    reliability => [WO-005, WO-004],           % Priority: 8, 7
    features => [WO-010, WO-009, WO-008, ...]  % Priority: 6, 5, 4
}
```

### Queue Operations

#### Add to Queue

Work orders are inserted in priority order:

```erlang
% Higher priority work orders go to front of queue
insert_by_priority(WorkOrderId, Priority, Queue, WorkOrders).
```

#### Dequeue

Highest priority work order is dequeued first:

```erlang
{ok, WorkOrder} = tcps_work_order:dequeue_next(Bucket),
% Work order is automatically started if WIP capacity available
```

#### Manual Reordering

Emergency reprioritization:

```erlang
% Move critical work order to front
CurrentQueue = tcps_work_order:get_queue(security),
[WO1, WO2, WO3 | Rest] = [maps:get(id, WO) || WO <- CurrentQueue],

% Reorder: put WO3 first (emergency)
NewOrder = [WO3, WO1, WO2 | Rest],
ok = tcps_work_order:reorder_queue(security, NewOrder).
```

## Reporting and Metrics

### Work Order Report

Comprehensive report for time period:

```erlang
Report = #{
    time_period => {{2026, 1, 1}, {2026, 1, 31}},
    total_work_orders => 142,
    by_bucket => #{
        security => 15,
        reliability => 45,
        features => 60,
        cost => 12,
        compliance => 8,
        technical_debt => 2
    },
    by_status => #{
        completed => 98,
        in_progress => 25,
        queued => 15,
        cancelled => 4
    },
    completed => 98,
    cancelled => 4,
    average_lead_time => 48.5,  % hours
    sla_compliance_rate => 96.8,  % percent
    top_priorities => [...]  % Highest priority active work orders
}
```

### Metrics

Key metrics tracked:

1. **Lead Time**: Creation to completion (hours)
2. **Throughput**: Work orders completed per day
3. **SLA Compliance**: Percentage completed within SLA
4. **Work in Progress**: Current active work orders
5. **Queue Depth**: Work orders waiting per bucket

### Kaizen Integration

Work order completion metrics feed into Kaizen:

```erlang
Receipt = #{
    type => work_order,
    work_order_id => WorkOrderId,
    timestamp => CompletedAt,
    duration => LeadTimeSeconds,
    success => true
},
tcps_kaizen:record_receipt(Receipt).
```

## Integration

### Kanban Integration

Work orders integrate with `tcps_kanban` for WIP management:

```erlang
% On work order start
notify_kanban_start(Bucket, WorkOrderId),

% On work order complete
notify_kanban_complete(Bucket, WorkOrderId).
```

### Kaizen Integration

Completion metrics sent to `tcps_kaizen`:

```erlang
notify_kaizen_completion(WorkOrder).
```

### Andon Integration

Quality alerts triggered for:
- Critical security work orders (CVE)
- SLA breaches
- Quality gate failures

```erlang
trigger_security_andon(WorkOrderId, CVE),
trigger_sla_andon(WorkOrderId, Breach).
```

### RDF Ontology

Work orders persisted to RDF:

```erlang
% Save to ontology
ok = tcps_work_order:save_to_ontology(WorkOrderId),

% Load from ontology
{ok, WorkOrder} = tcps_work_order:load_from_ontology(WorkOrderId).
```

## Examples

### Example 1: GitHub Bug Fix Workflow

```erlang
%% 1. GitHub issue arrives
IssueUrl = <<"https://github.com/erlmcp/erlmcp/issues/42">>,
{ok, WorkOrderId} = tcps_work_order:create_from_github(IssueUrl),

%% 2. Check status
{ok, Status1} = tcps_work_order:get_work_order_status(WorkOrderId),
io:format("Work order created: ~p~n", [Status1]),

%% 3. Dequeue when WIP available
{ok, WorkOrder} = tcps_work_order:dequeue_next(reliability),

%% 4. Progress through development
tcps_work_order:progress_work_order(WorkOrderId, requirements),
tcps_work_order:progress_work_order(WorkOrderId, design),
tcps_work_order:progress_work_order(WorkOrderId, implementation),
tcps_work_order:progress_work_order(WorkOrderId, testing),

%% 5. Complete and publish
SkuId = <<"sku-bugfix-42">>,
tcps_work_order:complete_work_order(WorkOrderId, SkuId),

%% 6. Generate report
{Date, _} = calendar:universal_time(),
Report = tcps_work_order:generate_work_order_report({Date, Date}),
io:format("Daily report: ~p~n", [Report]).
```

### Example 2: Security Advisory Response

```erlang
%% 1. CVE advisory received
CVE = <<"CVE-2026-1234">>,
{ok, WorkOrderId} = tcps_work_order:create_from_security_advisory(CVE),
%% → Triggers Andon alert
%% → Bucket: security
%% → Priority: 10
%% → SLA: 24 hours

%% 2. Immediately start (high priority)
ok = tcps_work_order:start_work_order(WorkOrderId),

%% 3. Check SLA status
{ok, SlaStatus} = tcps_work_order:check_sla(WorkOrderId),
%% → {ok, on_time} initially

%% 4. Complete fix
tcps_work_order:progress_work_order(WorkOrderId, implementation),
tcps_work_order:progress_work_order(WorkOrderId, testing),
tcps_work_order:complete_work_order(WorkOrderId, <<"sku-security-fix">>),

%% 5. Verify SLA compliance
Report = tcps_work_order:generate_work_order_report({Date, Date}),
SlaCompliance = maps:get(sla_compliance_rate, Report),
io:format("SLA compliance: ~.1f%~n", [SlaCompliance]).
```

### Example 3: Dependency Chain

```erlang
%% 1. Create foundation library work order
FoundationSignal = #{
    type => github_issue,
    source => <<"https://github.com/org/repo/issues/100">>,
    description => <<"Build authentication library">>,
    labels => [<<"enhancement">>],
    metadata => #{}
},
{ok, Foundation} = tcps_work_order:create_work_order(FoundationSignal),

%% 2. Create dependent features
Feature1Signal = #{
    type => github_issue,
    source => <<"https://github.com/org/repo/issues/101">>,
    description => <<"Add OAuth support">>,
    labels => [<<"enhancement">>],
    metadata => #{}
},
{ok, Feature1} = tcps_work_order:create_work_order(Feature1Signal),

Feature2Signal = #{
    type => github_issue,
    source => <<"https://github.com/org/repo/issues/102">>,
    description => <<"Add JWT tokens">>,
    labels => [<<"enhancement">>],
    metadata => #{}
},
{ok, Feature2} = tcps_work_order:create_work_order(Feature2Signal),

%% 3. Add dependencies
ok = tcps_work_order:add_dependency(Feature1, Foundation),
ok = tcps_work_order:add_dependency(Feature2, Foundation),

%% 4. Try to start features (will fail - blocked)
{error, blocked_by_dependencies} = tcps_work_order:start_work_order(Feature1),

%% 5. Complete foundation
tcps_work_order:start_work_order(Foundation),
tcps_work_order:complete_work_order(Foundation, <<"sku-auth-lib">>),

%% 6. Features automatically unblocked
ok = tcps_work_order:start_work_order(Feature1),
ok = tcps_work_order:start_work_order(Feature2).
```

## Testing

### Test Suite

Location: `test/tcps_work_order_tests.erl`

### Running Tests

```bash
# Compile and run tests
rebar3 eunit --module=tcps_work_order_tests

# Run specific test group
rebar3 eunit --module=tcps_work_order_tests --generator="work_order_creation_test_"
```

### Test Coverage

- **Work Order Creation**: All pull signal types, routing, prioritization
- **Lifecycle**: Complete workflows from creation to completion/cancellation
- **Queue Management**: Priority ordering, WIP limits, manual reordering
- **SLA Tracking**: On-time, warning, breached scenarios
- **Dependencies**: Simple chains, complex graphs, circular detection
- **Reporting**: Status, metrics, comprehensive reports
- **Integration**: Kanban, Kaizen, Andon coordination

### Test Scenarios

1. **Basic Workflow**: Create → Start → Progress → Complete
2. **WIP Limits**: Queue work when limit reached
3. **Dependencies**: Block until dependency completes
4. **Circular Dependencies**: Reject cycles
5. **Multiple Buckets**: Concurrent work across buckets
6. **SLA Tracking**: Warning and breach detection
7. **Cancellation**: Cancel with reason
8. **Persistence**: RDF and JSON export
9. **Complex Scenarios**: Combined features

## Performance Characteristics

### Time Complexity

- **Create Work Order**: O(n) where n = queue size (insertion sort by priority)
- **Get Queue**: O(m) where m = queue size
- **Check SLA**: O(1)
- **Get Dependencies**: O(1)
- **Generate Report**: O(w) where w = total work orders in time period

### Space Complexity

- **Work Orders**: O(w) where w = total work orders
- **Dependencies**: O(d) where d = total dependency relationships
- **Queues**: O(q) where q = queued work orders

### Scalability

- **gen_server**: Single process bottleneck for high concurrency
- **ETS**: Read concurrency enabled for fast lookups
- **Future Optimization**: Consider pg2 for distributed work order management

## Future Enhancements

1. **Distributed Work Orders**: Multi-node work order coordination
2. **Advanced Routing**: Machine learning for bucket/priority assignment
3. **Predictive SLA**: Forecast SLA breaches based on velocity
4. **Auto-Scaling**: Dynamic WIP limits based on team capacity
5. **GitHub Integration**: Real-time webhook processing
6. **Marketplace Integration**: Live event streaming
7. **SPARQL Queries**: Advanced ontology querying
8. **Metrics Dashboard**: Real-time visualization
9. **Workflow Templates**: Pre-defined work order patterns
10. **AI-Assisted Routing**: LLM-based signal classification

## References

- **TCPS Core Ontology**: `ontology/tcps_core.ttl`
- **Work Order Ontology**: `ontology/work_orders.ttl`
- **Kanban Module**: `src/tcps_kanban.erl`
- **Kaizen Module**: `src/tcps_kaizen.erl`
- **Andon Module**: `src/tcps/tcps_andon.erl`

## License

Copyright (c) 2026 TCPS Development Team

See LICENSE file for details.
