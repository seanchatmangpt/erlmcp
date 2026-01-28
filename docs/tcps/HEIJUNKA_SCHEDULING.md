# TCPS Heijunka Scheduling Guide

**Version**: 1.0.0
**Last Updated**: 2026-01-26
**Purpose**: Guide for production leveling and work scheduling

## Table of Contents

1. [Overview](#overview)
2. [Bucket Definitions](#bucket-definitions)
3. [Leveling Algorithm](#leveling-algorithm)
4. [WIP Limit Configuration](#wip-limit-configuration)
5. [Pull Signal Processing](#pull-signal-processing)
6. [Schedule Optimization](#schedule-optimization)

---

## Overview

**Heijunka** (平準化) means "leveling" - smoothing production to avoid peaks and valleys.

### Core Principle

> **"Level the type and quantity of production over time to maintain stable throughput."**

### Why Heijunka?

**Without Heijunka** (Batching):
```
Week 1: FEATURE FEATURE FEATURE FEATURE FEATURE
Week 2: FEATURE FEATURE FEATURE FEATURE FEATURE
Week 3: BUG BUG BUG BUG BUG BUG BUG BUG
Week 4: SECURITY SECURITY SECURITY SECURITY
        ↑ Security debt accumulates for 3 weeks, then CRISIS
```

**With Heijunka** (Leveling):
```
Week 1: SEC REL FEAT DEBT COMP SEC REL
Week 2: FEAT SEC REL COMP DEBT FEAT SEC
Week 3: REL DEBT SEC FEAT COMP REL SEC
Week 4: FEAT COMP SEC DEBT REL FEAT SEC
        ↑ Balanced, predictable, no crises
```

---

## Bucket Definitions

### The 5 Standard Buckets

#### 1. Reliability (REL)

**Definition**: Work that improves system stability and availability

**Examples**:
- Bug fixes
- Performance optimization
- Memory leak fixes
- Error handling improvements
- Crash prevention

**Priority**: HIGH (affects users immediately)

**Daily Allocation**: 2 slots

**SLO**: < 48 hours from report to fix

**Example Work Orders**:
```turtle
:wo-rel-001 a taiea:WorkOrder ;
    taiea:bucket "reliability" ;
    taiea:requestedSKU "erlmcp-tcp-connection-leak-fix" ;
    taiea:priority "high" ;
    taiea:demandSource :github-issue-1234 .
```

---

#### 2. Security (SEC)

**Definition**: Work that addresses vulnerabilities and hardens the system

**Examples**:
- CVE patches
- Dependency updates for security
- Authentication improvements
- Authorization fixes
- Input validation

**Priority**: CRITICAL (zero tolerance for vulnerabilities)

**Daily Allocation**: 2 slots

**SLO**:
- Critical CVE: < 24 hours
- High CVE: < 72 hours

**Example Work Orders**:
```turtle
:wo-sec-001 a taiea:WorkOrder ;
    taiea:bucket "security" ;
    taiea:requestedSKU "erlmcp-cve-2026-1234-patch" ;
    taiea:priority "critical" ;
    taiea:demandSource :cve-2026-1234 .
```

---

#### 3. Features (FEAT)

**Definition**: New capabilities requested by users

**Examples**:
- New protocol support
- New transport type
- API enhancements
- Tool additions

**Priority**: MEDIUM (important but not urgent)

**Daily Allocation**: 1 slot

**SLO**: < 2 weeks from approval to release

**Example Work Orders**:
```turtle
:wo-feat-001 a taiea:WorkOrder ;
    taiea:bucket "features" ;
    taiea:requestedSKU "erlmcp-websocket-transport" ;
    taiea:priority "medium" ;
    taiea:demandSource :marketplace-install-spike .
```

---

#### 4. Compliance (COMP)

**Definition**: Regulatory, legal, or audit requirements

**Examples**:
- GDPR requirements
- SOC2 controls
- Audit logging
- License compliance
- Data retention policies

**Priority**: HIGH (legal/contractual obligation)

**Daily Allocation**: 1 slot

**SLO**: Per regulatory deadline

**Example Work Orders**:
```turtle
:wo-comp-001 a taiea:WorkOrder ;
    taiea:bucket "compliance" ;
    taiea:requestedSKU "erlmcp-gdpr-data-deletion" ;
    taiea:priority "high" ;
    taiea:dueDate "2026-02-15"^^xsd:date ;
    taiea:demandSource :legal-requirement .
```

---

#### 5. Technical Debt (DEBT)

**Definition**: Refactoring and cleanup to improve maintainability

**Examples**:
- Code refactoring
- Test improvement
- Documentation updates
- Dependency upgrades (non-security)
- Architecture improvements

**Priority**: LOW (but essential long-term)

**Daily Allocation**: 1 slot

**SLO**: < 1 month from identification

**Example Work Orders**:
```turtle
:wo-debt-001 a taiea:WorkOrder ;
    taiea:bucket "debt" ;
    taiea:requestedSKU "erlmcp-refactor-registry" ;
    taiea:priority "low" ;
    taiea:demandSource :code-review-comment .
```

---

## Leveling Algorithm

### Daily Schedule Template

```
Monday:    SEC REL FEAT DEBT COMP SEC REL
Tuesday:   REL SEC COMP FEAT DEBT REL SEC
Wednesday: FEAT DEBT SEC REL COMP FEAT SEC
Thursday:  SEC COMP REL DEBT FEAT SEC REL
Friday:    REL FEAT SEC COMP DEBT REL SEC
```

**Total per week**:
- Reliability: 10 slots (28%)
- Security: 10 slots (28%)
- Features: 5 slots (14%)
- Compliance: 5 slots (14%)
- Technical Debt: 5 slots (14%)

**Total throughput**: 35 work orders per week (assuming 1 work order per slot)

---

### Algorithm: Leveling by Bucket

```erlang
% heijunka.erl - Production leveling algorithm

-module(heijunka).
-export([create_schedule/2]).

-define(DAILY_ALLOCATION, #{
    reliability => 2,
    security => 2,
    features => 1,
    compliance => 1,
    debt => 1
}).

-define(MAX_WIP, 3).

%% Create leveled schedule for a period
-spec create_schedule(WorkOrders :: [work_order()], Days :: integer()) ->
    {ok, schedule()} | {error, term()}.
create_schedule(WorkOrders, Days) ->
    % Step 1: Group work orders by bucket
    Buckets = group_by_bucket(WorkOrders),

    % Step 2: Check WIP limits
    CurrentWIP = count_active_wip(),
    case CurrentWIP >= ?MAX_WIP of
        true ->
            {error, wip_limit_exceeded};
        false ->
            % Step 3: Create leveled schedule
            Schedule = level_schedule(Buckets, Days),
            {ok, Schedule}
    end.

%% Group work orders by bucket
group_by_bucket(WorkOrders) ->
    lists:foldl(fun(WO, Acc) ->
        Bucket = maps:get(bucket, WO),
        maps:update_with(Bucket, fun(WOs) -> [WO | WOs] end, [WO], Acc)
    end, #{}, WorkOrders).

%% Create leveled schedule
level_schedule(Buckets, Days) ->
    % For each day
    lists:flatmap(fun(Day) ->
        % For each bucket
        maps:fold(fun(Bucket, WOs, DaySchedule) ->
            % Get daily allocation for this bucket
            SlotsForBucket = maps:get(Bucket, ?DAILY_ALLOCATION, 0),

            % Select highest priority work orders
            SelectedWOs = select_work_orders(WOs, SlotsForBucket),

            % Create schedule entries
            Entries = lists:map(fun(WO) ->
                #{
                    day => Day,
                    bucket => Bucket,
                    work_order => WO,
                    start_time => calculate_start_time(Day, DaySchedule)
                }
            end, SelectedWOs),

            DaySchedule ++ Entries
        end, [], Buckets)
    end, lists:seq(1, Days)).

%% Select work orders based on priority
select_work_orders(WorkOrders, Count) ->
    % Sort by priority (critical > high > medium > low)
    Sorted = lists:sort(fun(A, B) ->
        priority_value(maps:get(priority, A)) >
        priority_value(maps:get(priority, B))
    end, WorkOrders),

    % Take top N
    lists:sublist(Sorted, Count).

priority_value(critical) -> 4;
priority_value(high) -> 3;
priority_value(medium) -> 2;
priority_value(low) -> 1.
```

---

### Example Schedule Generation

```bash
# Create schedule for next 5 days
./tools/heijunka schedule \
    --input ontology/work_orders.ttl \
    --days 5 \
    --output receipts/plan/schedule-20260126-20260130.json

# Output: receipts/plan/schedule-20260126-20260130.json
{
  "schedule_id": "sched-20260126-20260130",
  "start_date": "2026-01-26",
  "end_date": "2026-01-30",
  "total_days": 5,
  "total_slots": 35,

  "daily_schedule": [
    {
      "day": 1,
      "date": "2026-01-26",
      "slots": [
        {
          "slot": 1,
          "bucket": "security",
          "work_order": "wo-sec-001",
          "sku": "erlmcp-cve-patch",
          "priority": "critical",
          "start_time": "09:00"
        },
        {
          "slot": 2,
          "bucket": "reliability",
          "work_order": "wo-rel-003",
          "sku": "erlmcp-connection-leak-fix",
          "priority": "high",
          "start_time": "10:30"
        },
        {
          "slot": 3,
          "bucket": "features",
          "work_order": "wo-feat-002",
          "sku": "erlmcp-websocket-transport",
          "priority": "medium",
          "start_time": "13:00"
        },
        {
          "slot": 4,
          "bucket": "debt",
          "work_order": "wo-debt-005",
          "sku": "erlmcp-refactor-registry",
          "priority": "low",
          "start_time": "14:30"
        },
        {
          "slot": 5,
          "bucket": "compliance",
          "work_order": "wo-comp-001",
          "sku": "erlmcp-audit-logging",
          "priority": "high",
          "start_time": "16:00"
        }
      ],
      "bucket_distribution": {
        "security": 1,
        "reliability": 1,
        "features": 1,
        "debt": 1,
        "compliance": 1
      }
    }
  ],

  "summary": {
    "total_work_orders": 35,
    "bucket_distribution": {
      "reliability": 10,
      "security": 10,
      "features": 5,
      "compliance": 5,
      "debt": 5
    },
    "wip_limit": 3,
    "wip_current": 2,
    "capacity_available": true
  }
}
```

---

## WIP Limit Configuration

### Why Limit WIP?

**Too much WIP**:
- Hides bottlenecks
- Increases context switching
- Delays feedback
- Inflates lead time

**Optimal WIP**:
- Surfaces bottlenecks immediately
- Maintains focus
- Fast feedback
- Minimal lead time

### Calculating Optimal WIP Limit

**Little's Law**:
```
WIP = Throughput × Lead Time

Example:
- Throughput: 7 work orders per day
- Target Lead Time: 6 hours (0.25 days)
- Optimal WIP: 7 × 0.25 = 1.75 ≈ 2

Set WIP limit: 2-3
```

### WIP Limit Rules

```erlang
% config/wip_limits.erl

-define(WIP_LIMITS, #{
    % Per stage
    pull => 10,        % Max work orders in backlog
    plan => 5,         % Max work orders in planning
    generate => 3,     % Max work orders being generated
    build => 3,        % Max work orders being built
    test => 2,         % Max work orders in testing
    release => 1,      % Max work orders in release
    publish => 1,      % Max work orders being published
    verify => 1,       % Max work orders in verification

    % Overall
    total => 3         % Max active work orders (in_progress status)
}).
```

### Enforcing WIP Limits

```bash
# Check WIP before accepting new work
./tools/wip check

# Output:
Current WIP: 2
WIP Limit: 3
Status: ACCEPT (1 slot available)

Active Work Orders:
1. wo-sec-001 (Stage: test, Age: 2.3 hours)
2. wo-rel-003 (Stage: build, Age: 1.1 hours)

# If limit exceeded:
Current WIP: 3
WIP Limit: 3
Status: REJECT (at capacity)

Action Required: Wait for work order to complete
Estimated slot available: 1.2 hours
```

### Dynamic WIP Adjustment

```erlang
% Adjust WIP based on performance

-spec adjust_wip_limit(Metrics :: map()) -> integer().
adjust_wip_limit(#{lead_time := LeadTime, throughput := Throughput}) ->
    % If lead time increasing, reduce WIP
    case LeadTime > target_lead_time() of
        true ->
            current_wip_limit() - 1;
        false ->
            % If throughput can handle more, increase WIP
            case Throughput > target_throughput() of
                true ->
                    current_wip_limit() + 1;
                false ->
                    current_wip_limit()
            end
    end.
```

---

## Pull Signal Processing

### Demand Signal Types

#### 1. Marketplace Signals

**Source**: Marketplace analytics

**Signals**:
- Install spike (demand increasing)
- Refund spike (quality issue)
- Feature request concentration

**Processing**:
```bash
# Collect marketplace signals
./tools/demand/marketplace analyze --period last-7-days

# Output:
Install Spike Detected:
- Feature: WebSocket Transport
- Install count: 147 (baseline: 45)
- Trend: +227% increase
- Recommendation: Create feature work order

Refund Spike Detected:
- Version: 0.5.2
- Refund count: 12 (baseline: 2)
- Common reason: "Connection timeout"
- Recommendation: Create reliability work order
```

**Work Order Creation**:
```bash
# Auto-create work order from signal
./tools/work-order create-from-signal \
    --signal marketplace-install-spike-websocket \
    --bucket features \
    --priority high

# Creates: wo-feat-007 (WebSocket transport)
```

---

#### 2. Security Signals

**Source**: CVE databases, security scanners

**Signals**:
- New CVE affecting dependencies
- Security scan findings
- Penetration test results

**Processing**:
```bash
# Monitor security feeds
./tools/demand/security monitor

# Output:
New CVE Detected:
- CVE: CVE-2026-1234
- Package: gun
- Current Version: 2.0.1
- Patched Version: 2.0.2
- Severity: HIGH
- CVSS: 7.8
- Recommendation: Create security work order (URGENT)
```

**Work Order Creation**:
```bash
# Auto-create security work order
./tools/work-order create-from-cve \
    --cve CVE-2026-1234 \
    --bucket security \
    --priority critical \
    --slo 24h

# Creates: wo-sec-008 (CVE-2026-1234 patch)
```

---

#### 3. GitHub Signals

**Source**: GitHub issues, PR comments

**Signals**:
- High-impact bug reports
- Feature requests with many 👍
- Performance complaints

**Processing**:
```bash
# Analyze GitHub activity
./tools/demand/github analyze --repo erlmcp

# Output:
High-Priority Issues:
1. Issue #234: Connection leak (15 👍, 8 comments)
   Bucket: reliability
   Priority: high

2. Issue #156: Add gRPC transport (23 👍, 12 comments)
   Bucket: features
   Priority: medium
```

---

#### 4. Internal Signals

**Source**: Code reviews, tech debt tracking

**Signals**:
- Repeated code review comments
- Flaky tests
- High complexity modules

**Processing**:
```bash
# Detect tech debt
./tools/demand/tech-debt detect

# Output:
Tech Debt Identified:
1. Module: erlmcp_registry
   Cyclomatic Complexity: 42 (threshold: 20)
   Recommendation: Refactor

2. Test: erlmcp_transport_http_gun_SUITE
   Flakiness: 12% (threshold: 5%)
   Recommendation: Fix flaky test
```

---

## Schedule Optimization

### Optimization Goals

1. **Minimize Lead Time**: Get work done fast
2. **Maximize Throughput**: Complete more work
3. **Balance Buckets**: Avoid neglecting any category
4. **Respect SLOs**: Meet deadlines
5. **Respect WIP Limits**: Don't overload

### Optimization Algorithm

```erlang
% heijunka_optimizer.erl

-module(heijunka_optimizer).
-export([optimize_schedule/1]).

optimize_schedule(Schedule) ->
    % Multi-objective optimization
    Optimized = lists:foldl(fun(Optimizer, Sched) ->
        Optimizer(Sched)
    end, Schedule, [
        fun minimize_lead_time/1,
        fun balance_buckets/1,
        fun respect_slos/1,
        fun smooth_resource_usage/1
    ]),

    Optimized.

%% Minimize lead time by prioritizing short tasks
minimize_lead_time(Schedule) ->
    % Sort work orders by estimated duration
    lists:sort(fun(A, B) ->
        estimated_duration(A) =< estimated_duration(B)
    end, Schedule).

%% Balance buckets to avoid accumulation
balance_buckets(Schedule) ->
    % Calculate current distribution
    Distribution = calculate_distribution(Schedule),

    % Check if any bucket is over/under allocated
    Target = ?DAILY_ALLOCATION,

    % Rebalance if needed
    case is_balanced(Distribution, Target) of
        true -> Schedule;
        false -> rebalance(Schedule, Distribution, Target)
    end.

%% Respect SLOs by prioritizing near-deadline work
respect_slos(Schedule) ->
    % Calculate time until deadline
    WithDeadlines = lists:map(fun(WO) ->
        DaysUntilDue = days_until_due(WO),
        {DaysUntilDue, WO}
    end, Schedule),

    % Sort by urgency
    Sorted = lists:sort(WithDeadlines),

    % Extract work orders
    [WO || {_, WO} <- Sorted].

%% Smooth resource usage to avoid bottlenecks
smooth_resource_usage(Schedule) ->
    % Distribute compute-intensive tasks
    % Avoid scheduling all heavy builds at once
    distribute_by_resource_usage(Schedule).
```

---

### Example: Schedule Optimization

**Before Optimization**:
```
Day 1: FEAT FEAT FEAT FEAT FEAT (all features)
Day 2: SEC SEC SEC SEC SEC (all security)
Day 3: REL REL REL REL REL (all reliability)

Problems:
- Security debt accumulates for 2 days
- Resource spikes (all builds on same day)
- Unbalanced flow
```

**After Optimization**:
```
Day 1: SEC REL FEAT DEBT COMP (balanced)
Day 2: REL SEC COMP FEAT DEBT (balanced)
Day 3: FEAT DEBT SEC REL COMP (balanced)

Benefits:
- Continuous security attention
- Smooth resource usage
- Balanced throughput
```

---

## Heijunka Box Visualization

```
┌─────────────┬─────────────┬─────────────┬─────────────┬─────────────┐
│ Reliability │  Security   │  Features   │ Compliance  │ Tech Debt   │
│   (2/day)   │   (2/day)   │   (1/day)   │   (1/day)   │   (1/day)   │
├─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤
│ ┌─────────┐ │ ┌─────────┐ │ ┌─────────┐ │ ┌─────────┐ │ ┌─────────┐ │
│ │ wo-rel- │ │ │ wo-sec- │ │ │ wo-feat-│ │ │ wo-comp-│ │ │ wo-debt-│ │
│ │ 001     │ │ │ 001     │ │ │ 001     │ │ │ 001     │ │ │ 001     │ │
│ └─────────┘ │ └─────────┘ │ └─────────┘ │ └─────────┘ │ └─────────┘ │
│ ┌─────────┐ │ ┌─────────┐ │ ┌─────────┐ │ ┌─────────┐ │ ┌─────────┐ │
│ │ wo-rel- │ │ │ wo-sec- │ │ │ wo-feat-│ │ │ wo-comp-│ │ │ wo-debt-│ │
│ │ 002     │ │ │ 002     │ │ │ 002     │ │ │ 002     │ │ │ 002     │ │
│ └─────────┘ │ └─────────┘ │ └─────────┘ │ └─────────┘ │ └─────────┘ │
└─────────────┴─────────────┴─────────────┴─────────────┴─────────────┘
                  ↓ Pull one from each bucket per day ↓
```

---

## Conclusion

Heijunka leveling ensures:
- Balanced production (no crisis mode)
- Predictable throughput
- Smooth resource usage
- Continuous attention to all priorities
- Sustainable pace

**Level the load. Maintain the flow.**

---

**See Also**:
- [TCPS.md](TCPS.md) - Complete TCPS guide
- [STANDARD_WORK.md](STANDARD_WORK.md) - Stage procedures
- [KAIZEN_GUIDE.md](KAIZEN_GUIDE.md) - Continuous improvement
- [RECEIPTS_SPEC.md](RECEIPTS_SPEC.md) - Receipt schemas
