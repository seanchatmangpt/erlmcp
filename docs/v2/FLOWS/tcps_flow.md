# TCPS Pipeline Flow Documentation

**Focus:** Work Order → Quality Gates → Andon → Receipt Chain → SKU Release

## 1. Demand-Driven Pull Signal Flow

### 1.1 Pull Signal Sources

```
GitHub Issues/PRs
  │
  ├─ type: github_issue | github_pr
  ├─ labels: [bug, critical, security, ...]
  ├─ milestone: deadline
  └─ description: ← tcps_work_order

Marketplace Events
  │
  ├─ type: marketplace_install | marketplace_refund
  ├─ customer_id: X
  └─ reason: quality issue or upsell opportunity

CVE Advisories
  │
  ├─ type: cve
  ├─ severity: critical
  ├─ cvss_score: 9.8
  └─ patches_available: true
```

**File:** `/Users/sac/erlmcp/src/tcps_work_order.erl:1945-1992`

### 1.2 Bucket Assignment Logic

```
Signal Analysis:
  ├─ CVE prefix "CVE-" → bucket: security, priority: 10
  ├─ Label "bug" → bucket: reliability, priority: 7-9
  ├─ Label "enhancement" → bucket: features, priority: 3-5
  ├─ Label "compliance" → bucket: compliance, priority: 6-8
  ├─ Label "refactor" → bucket: technical_debt, priority: 2-4
  ├─ Label "performance" → bucket: cost, priority: 3-7
  └─ marketplace_refund → bucket: reliability, priority: 8

Function: tcps_work_order:determine_bucket/1
File: `/Users/sac/erlmcp/src/tcps_work_order.erl:1133-1158`
```

### 1.3 Priority Calculation

```erlang
% Canonical priority mapping (1=lowest, 10=critical)
calculate_priority(Signal, Bucket) →

% Security bucket (highest priority window)
cve + security → 10 (24-hour critical window)

% Reliability bucket (urgent)
marketplace_refund → 8 (customer dissatisfaction)
github_issue + "critical" label → 9 (system down)
github_issue + "high" label → 7

% Features bucket (medium)
marketplace_install → 4 (demand signal)
github_issue + no label → 3

% Technical debt (lowest priority)
refactor label → 3 (optional optimization)

Function: tcps_work_order:calculate_priority/2
File: `/Users/sac/erlmcp/src/tcps_work_order.erl:1160-1181`
```

### 1.4 SLA Deadline Calculation

```
SLA Hours per Bucket (configured):
├─ security: 24h       → deadline: now + 86,400 seconds
├─ reliability: 168h   → deadline: now + 604,800 seconds (7 days)
├─ compliance: 168h    → deadline: now + 604,800 seconds
├─ cost: 720h          → deadline: now + 2,592,000 seconds (30 days)
├─ features: 720h      → deadline: now + 2,592,000 seconds
└─ technical_debt: ∞   → deadline: now + 100 years (soft deadline)

Function: tcps_work_order:calculate_deadline/1
File: `/Users/sac/erlmcp/src/tcps_work_order.erl:1686-1704`
```

## 2. Kanban Flow Control

### 2.1 WIP Limit Check

```
Work Order Creation
  │
  └─→ tcps_kanban:check_wip_limit(bucket) {Line: tcps_kanban.erl:197-210}
      │
      ├─ Current WIP for bucket < Limit?
      │  │
      │  ├─ YES → OK, proceed to Heijunka
      │  │  status: pending → queued
      │  │
      │  └─ NO → Queue work order, wait for slot
      │     status: pending (remains queued)
      │
      └─ Limits per bucket (default):
         ├─ security: 5
         ├─ reliability: 5
         ├─ cost: 5
         └─ compliance: 5
```

**File:** `/Users/sac/erlmcp/src/tcps_kanban.erl:130-157`

### 2.2 Work Order Queueing

```erlang
% State.queues = #{
%   security => [wo3, wo1, wo5],        % [highest priority, ..., lowest]
%   reliability => [wo7, wo2],
%   cost => [],
%   compliance => [wo4]
% }

% Ordering: insert_by_priority/4
% Higher priority numbers come first in queue
% So dequeue_next/1 always takes highest priority work

Function: tcps_work_order:insert_by_priority/4
File: `/Users/sac/erlmcp/src/tcps_work_order.erl:1239-1254`
```

## 3. Heijunka Production Leveling

### 3.1 Round-Robin Interleaving

```
Input Work Orders (all pending/queued):
├─ security: [wo10:p9, wo11:p8, wo12:p7]   (sorted by priority)
├─ reliability: [wo5:p9, wo6:p7]
├─ cost: [wo3:p6]
└─ features: [wo1:p8, wo2:p5]

Round-Robin Algorithm:
├─ Pass 1: security[wo10], reliability[wo5], cost[wo3], features[wo1]
│          (one from each bucket, take highest priority within bucket)
│
├─ Pass 2: security[wo11], reliability[wo6], features[wo2]
│          (cost bucket exhausted)
│
├─ Pass 3: security[wo12]
│          (all but security exhausted)
│
└─ Output: [wo10, wo5, wo3, wo1, wo11, wo6, wo2, wo12]
           (no more than 2 consecutive from same bucket enforced)

Function: tcps_heijunka:level_work_orders/1
File: `/Users/sac/erlmcp/src/tcps_heijunka.erl:123-136`
```

### 3.2 Leveling Metrics

```erlang
% Measures quality of schedule:
leveling_score() = #{
  batching_metric => 0.87,           % 0-1: How well it prevents batching
  distribution_uniformity => 0.91,   % 0-1: Even distribution across buckets
  priority_preservation => 0.95,     % 0-1: High-priority items preserved
  overall_score => 0.90              % Weighted average
}

% Calculation: tcps_heijunka:get_leveling_score/1
% File: `/Users/sac/erlmcp/src/tcps_heijunka.erl:183-199`
```

## 4. Quality Gates Pipeline (Jidoka)

### 4.1 Eight-Stage Sequential Pipeline

```
Work Order (status: in_progress)
  │
  ├─→ [Gate 1/8] SHACL Validation
  │   │  Module: tcps_quality_gates:check_shacl_validation/2
  │   │  File: `/Users/sac/erlmcp/src/tcps_quality_gates.erl:380-420`
  │   │  Checks: Ontology conformance (JSON-LD schema validation)
  │   │  Status: pass / {fail, violations}
  │   │  Triggers Andon: {error, {violations, [...]}
  │   │
  │   ├─ PASS? → Continue to Gate 2
  │   └─ FAIL? → Trigger Andon, BLOCK work order
  │
  ├─→ [Gate 2/8] Compilation
  │   │  Module: tcps_quality_gates:check_compilation/2
  │   │  File: `/Users/sac/erlmcp/src/tcps_quality_gates.erl:428-479`
  │   │  Command: rebar3 compile
  │   │  Checks: Zero compilation errors (warnings tolerated)
  │   │  Status: {error_count, warning_count}
  │   │  Triggers Andon: if error_count > 0
  │   │
  │   ├─ PASS? → Continue to Gate 3
  │   └─ FAIL? → Trigger Andon, BLOCK work order
  │
  ├─→ [Gate 3/8] Test Execution
  │   │  Module: tcps_quality_gates:check_test_execution/2
  │   │  File: `/Users/sac/erlmcp/src/tcps_quality_gates.erl:488-549`
  │   │  Command: rebar3 do eunit --cover, cover --verbose
  │   │  Thresholds:
  │   │    ├─ Pass rate: >= 95% (critical)
  │   │    ├─ Coverage: >= 80% (critical)
  │   │    └─ Total tests: > 0
  │   │  Triggers Andon: if pass_rate < 95% OR coverage < 80%
  │   │
  │   ├─ PASS? → Continue to Gate 4
  │   └─ FAIL? → Trigger Andon, BLOCK work order
  │
  ├─→ [Gate 4/8] Security Scan
  │   │  Module: tcps_quality_gates:check_security_scan/2
  │   │  File: `/Users/sac/erlmcp/src/tcps_quality_gates.erl:559-601`
  │   │  Scans:
  │   │    ├─ Dependency vulnerabilities (CVE databases)
  │   │    ├─ Hardcoded secrets (truffleHog)
  │   │    └─ Insecure patterns (static analysis)
  │   │  Triggers Andon: if total_issues > 0
  │   │
  │   ├─ PASS? → Continue to Gate 5
  │   └─ FAIL? → Trigger Andon, BLOCK work order
  │
  ├─→ [Gate 5/8] Deterministic Build
  │   │  Module: tcps_quality_gates:check_deterministic_build/2
  │   │  File: `/Users/sac/erlmcp/src/tcps_quality_gates.erl:611-652`
  │   │  Verification: Build twice, compare SHA-256 hashes
  │   │  Module: tcps_deterministic:verify_deterministic_build/1
  │   │  File: `/Users/sac/erlmcp/src/tcps/tcps_deterministic.erl`
  │   │  Purpose: Ensure reproducible builds (supply chain security)
  │   │  Triggers Andon: if hash1 != hash2
  │   │
  │   ├─ PASS? → Continue to Gate 6
  │   └─ FAIL? → Trigger Andon, BLOCK work order
  │
  ├─→ [Gate 6/8] Quality Metrics
  │   │  Module: tcps_quality_gates:check_quality_metrics/2
  │   │  File: `/Users/sac/erlmcp/src/tcps_quality_gates.erl:659-680`
  │   │  Aggregate metrics:
  │   │    ├─ Average test pass rate (last 30 days)
  │   │    ├─ Average code coverage
  │   │    ├─ Defect rate (Andon events / work orders)
  │   │    └─ First pass yield (gates passed on first try / total)
  │   │  Triggers Andon: if any metric < threshold
  │   │
  │   ├─ PASS? → Continue to Gate 7
  │   └─ FAIL? → Trigger Andon, BLOCK work order
  │
  ├─→ [Gate 7/8] Release Verification
  │   │  Module: tcps_quality_gates:check_release_verification/2
  │   │  File: `/Users/sac/erlmcp/src/tcps_quality_gates.erl:692-727`
  │   │  Checks:
  │   │    ├─ Artifact exists (_build/default/rel)
  │   │    ├─ SBOM generated (supply chain metadata)
  │   │    ├─ Licenses compliant (approved OSS licenses)
  │   │    ├─ Dependencies pinned (rebar.lock present)
  │   │    └─ Receipt chain complete (all prior receipts)
  │   │  Triggers Andon: if any check fails
  │   │
  │   ├─ PASS? → Continue to Gate 8
  │   └─ FAIL? → Trigger Andon, BLOCK work order
  │
  └─→ [Gate 8/8] Smoke Tests (NON-BLOCKING)
      │  Module: tcps_quality_gates:check_smoke_test/2
      │  File: `/Users/sac/erlmcp/src/tcps_quality_gates.erl:738-760`
      │  Tests: Basic functionality (start, health, stop)
      │  Severity: medium (warning only, does not trigger blocking Andon)
      │
      ├─ PASS? → Ready for release ✓
      └─ FAIL? → Log warning but allow continuation

All gates passed?
  │
  ├─ YES → Work order eligible for completion
  └─ NO → Block at first failing gate (fail-fast architecture)
```

**File:** `/Users/sac/erlmcp/src/tcps_quality_gates.erl:97-110` (gate ordering)

### 4.2 Cache & Receipt Generation

```erlang
% Gate results cached for 5 minutes to avoid re-execution
cache_gate_result(Gate, SkuId, passed, Receipt, State)
  │
  └─→ ETS insert: {Gate, SkuId} → {Gate, SkuId, passed, Receipt}

% Receipt structure:
Receipt = #{
  receipt_id => <<"RCPT-gate_name-timestamp-random">>,
  receipt_type => quality_gate,
  gate => Gate,
  sku_id => SkuId,
  status => pass | fail,
  timestamp => Milliseconds,
  timestamp_iso => <<"2026-01-27T12:34:56Z">>,
  details => #{...gate-specific details...},
  ontology_refs => [<<"tcps:QualityGate">>, <<"tcps:Gate_Name">>]
}

Function: tcps_quality_gates:generate_gate_receipt/4
File: `/Users/sac/erlmcp/src/tcps_quality_gates.erl:1135-1152`
```

## 5. Andon Stop-the-Line System

### 5.1 Trigger Points

```
Quality Gate Failure
  │
  └─→ trigger_andon(failure_type, context)
      │
      ├─ failure_type: shacl_violation
      ├─ failure_type: compilation_failure
      ├─ failure_type: test_failure
      ├─ failure_type: non_determinism
      └─ failure_type: missing_receipt
      │
      context: #{
        sku_id => <<"sku-123">>,
        stage => compilation,    % Which stage detected failure
        details => #{            % Gate-specific information
          gate => shacl_validation,
          violations => [...]
        },
        metadata => #{
          triggered_by => <<"tcps_quality_gates">>,
          timestamp => Milliseconds
        }
      }

File: `/Users/sac/erlmcp/src/tcps_andon.erl:160-202`
```

### 5.2 Andon Event Lifecycle

```
1. TRIGGER (event_id created, status=open)
   │
   ├─ Create: ANDON-timestamp-random-unique
   ├─ Store in ETS: {AndonId, Event}
   ├─ Generate Andon receipt: RCPT-andon-timestamp-random
   ├─ Store receipt: priv/receipts/andons/RCPT-*.json
   ├─ Broadcast to dashboard (SSE)
   ├─ Log to error_logger
   └─ Status: open (BLOCKING work order progression)

2. OPEN (blocking phase)
   │
   ├─ Work order cannot proceed to next stage
   ├─ Block all dependent work orders
   ├─ Notify team via dashboard (real-time)
   ├─ Escalate to on-call if critical
   └─ Await root cause analysis

3. RESOLUTION (analysis phase)
   │
   ├─ Analyst performs 5-Whys analysis
   ├─ Records: root_cause (why it happened)
   ├─ Records: fix_applied (what was fixed)
   ├─ Records: prevention_added (how to prevent recurrence)
   ├─ Time to resolution tracked
   └─ Generate resolution receipt: RCPT-resolution-timestamp-random

4. RESOLVED (closure)
   │
   ├─ Update status: open → resolved
   ├─ Store resolution in ETS
   ├─ Store resolution receipt
   ├─ Broadcast resolution to dashboard
   ├─ Unblock dependent work orders
   └─ Record in Kaizen metrics

File: `/Users/sac/erlmcp/src/tcps_andon.erl:160-304`
```

### 5.3 Stop-the-Line Enforcement

```erlang
% Before work order can proceed to next stage:
can_proceed_to_stage(SkuId, Stage)
  │
  └─→ Query Andon events for this SKU
      │
      ├─ Find all open Andon events
      │  (status = open)
      │
      ├─ If none found:
      │  └─ {ok, proceed} ✓
      │
      └─ If open events exist:
         └─ {blocked, [AndonId1, AndonId2, ...]} ✗

File: `/Users/sac/erlmcp/src/tcps_andon.erl:241-257`
```

### 5.4 Dashboard Real-Time Updates

```
Andon Triggered
  │
  └─→ broadcast_andon_event(andon_triggered, AndonEvent)
      │
      ├─ Query tcps_sse_manager (Server-Sent Events)
      │  └─ Send: {type: andon_triggered, data: {...}, timestamp}
      │
      ├─ Query tcps_dashboard (legacy path)
      │  └─ Notify: notify_event(andon_triggered, formatted_event)
      │
      └─ All connected SSE clients see:
         └─ EventSource message (real-time update)

File: `/Users/sac/erlmcp/src/tcps_andon.erl:587-608`
```

## 6. Receipt Chain Verification

### 6.1 Chain Completeness

```
Completed Work Order
  │
  └─→ tcps_receipt:verify_chain(SkuId)
      │
      ├─ Expected stages (mandatory receipt types):
      │  ├─ shacl (SHACL validation receipt)
      │  ├─ compile (Compilation receipt)
      │  ├─ test (Test execution receipt)
      │  ├─ security (Security scan receipt)
      │  ├─ deterministic (Deterministic build receipt)
      │  ├─ quality (Quality metrics receipt)
      │  ├─ release (Release verification receipt)
      │  ├─ smoke (Smoke tests receipt)
      │  ├─ validate (Ontology validation receipt)
      │  └─ deploy (Deployment receipt)
      │
      ├─ Missing? → {error, {incomplete, Missing}}
      │  (e.g., no test receipt found)
      │
      └─ All present? → verify_stage_chronological_order/1
         │
         └─ Check timestamps increasing: T(shacl) < T(compile) < T(test) < ...
            │
            ├─ NOT increasing? → {error, not_chronological}
            │
            └─ Increasing? → verify_no_timestamp_gaps/1
               │
               ├─ Any gap > 24 hours? → {error, timestamp_gap_too_large}
               │
               └─ All gaps < 24h? → {ok, complete} ✓

File: `/Users/sac/erlmcp/src/tcps_receipt.erl:48-81`
```

### 6.2 Deterministic Verification

```
Deterministic Build Receipt
  │
  └─→ tcps_receipt:verify_deterministic(SkuId)
      │
      └─ Lookup deterministic receipt
         │
         ├─ status: pass → {ok, deterministic} ✓
         │  (build hashes matched)
         │
         ├─ status: fail → {error, {non_deterministic, Evidence}}
         │  Evidence: binary diff of artifacts or detailed evidence
         │
         ├─ Not found → {error, {incomplete, [deterministic]}}
         │
         └─ Duplicate receipts → {error, {duplicate_receipts, deterministic}}
            (impossible in normal operation)

File: `/Users/sac/erlmcp/src/tcps_receipt.erl:83-105`
```

### 6.3 Audit Trail Generation

```erlang
% Complete timeline for SKU:
audit_trail() = #{
  sku_id => <<"sku-123">>,
  timeline => [
    #{
      stage => shacl,
      timestamp => 1674806096000,
      status => pass,
      evidence => <<"Gateway 1 of 8">>
    },
    #{
      stage => compile,
      timestamp => 1674806097123,
      status => pass,
      evidence => <<"0 errors, 0 warnings">>
    },
    % ... 8 more stages ...
    #{
      stage => deploy,
      timestamp => 1674806125456,
      status => pass,
      evidence => <<"Released to production">>
    }
  ],
  total_receipts => 10,
  lead_time_hours => 0.82,    % Time from first to last receipt
  chain_status => complete,   % All stages present + chronological
  generated_at => 1674806200000
}

Function: tcps_receipt:generate_audit_trail/1
File: `/Users/sac/erlmcp/src/tcps_receipt.erl:116-154`
```

## 7. SKU Release & Completion

### 7.1 Stage Transitions

```
Work Order Stages (linear progression):
requirements → design → implementation → testing → integration → deployment → published

Each transition:
├─ Check no open Andon events (stop-the-line check)
├─ Check required quality gate passed for target stage
├─ Update work_order.current_stage = new_stage
├─ Append new_stage to work_order.stages_completed
├─ Generate stage receipt
├─ Store receipt to priv/receipts/work_orders/
└─ Save to RDF ontology

Module: tcps_work_order:progress_work_order/2
File: `/Users/sac/erlmcp/src/tcps_work_order.erl:357-360` (API)
File: `/Users/sac/erlmcp/src/tcps_work_order.erl:992-1014` (implementation)
```

### 7.2 Work Order Completion

```
Complete Work Order
  │
  └─→ tcps_work_order:complete_work_order(WorkOrderId, SkuId)
      │
      ├─ Mark status: in_progress → completed
      ├─ Link to SKU: sku_id = SkuId
      ├─ Set final stage: current_stage = published
      ├─ Set completion timestamp
      │
      ├─ Free Kanban WIP slot
      │  └─ tcps_kanban:complete_work_order(Bucket, WorkOrderId)
      │
      ├─ Resolve dependencies (unblock dependent work orders)
      │  └─ For each blocked_by WO: change status pending → queued
      │
      ├─ Update Kaizen metrics
      │  └─ tcps_kaizen:record_receipt(#{type: work_order, ...})
      │     • Lead time: completed_at - created_at
      │     • First-pass yield (gates passed on first attempt)
      │     • SLA compliance (completed_at <= sla_deadline)
      │
      ├─ Save to RDF ontology
      │  └─ tcps_work_order:save_to_ontology(WorkOrderId)
      │     Creates RDF triple:
      │     tcps:WorkOrderId a tcps:WorkOrder ;
      │       tcps:skuId "SkuId" ;
      │       tcps:bucket "reliability" ;
      │       tcps:priority 7 ;
      │       tcps:status "completed" ;
      │       tcps:createdAt "2026-01-27T12:00:00Z" ;
      │       tcps:completedAt "2026-01-27T12:30:00Z" .
      │
      └─ Generate completion receipt
         Receipt: #{
           receipt_id => <<"RCPT-completion-timestamp-random">>,
           type => work_order_completed,
           work_order_id => WorkOrderId,
           sku_id => SkuId,
           lead_time_hours => 0.82,
           stages_completed => [requirements, design, implementation, ...],
           timestamp => CompletedAt
         }

File: `/Users/sac/erlmcp/src/tcps_work_order.erl:375-378` (API)
File: `/Users/sac/erlmcp/src/tcps_work_order.erl:1016-1066` (implementation)
```

## 8. Integration Points

### 8.1 MCP Tool Registration

```erlang
% Tools automatically registered with erlmcp_server:
% (via tcps_mcp_server:list_tools/0)

Tool: tcps-create-work-order
  Input: #{
    bucket: reliability | security | cost | compliance | features | technical_debt,
    priority: 1..10,
    description: <<"Issue description">>
  }
  Output: {ok, work_order_id}
  Implementation: tcps_work_order:create_work_order/1

Tool: tcps-check-quality-gates
  Input: #{sku_id: <<"sku-123">>}
  Output: {ok, [receipts]} | {failed_at, gate, violations}
  Implementation: tcps_quality_gates:check_all_gates/1

Tool: tcps-resolve-andon
  Input: #{
    andon_id: <<"ANDON-...">>,
    root_cause: <<"Root cause analysis">>,
    fix_applied: <<"Fix description">>,
    prevention_added: <<"Prevention measure">>
  }
  Output: {ok, resolution_timestamp}
  Implementation: tcps_andon:resolve_andon/2

File: `/Users/sac/erlmcp/src/tcps_mcp_diataxis/tcps_mcp_tools.erl`
```

### 8.2 JSON-RPC Message Flow

```
AI Client (e.g., Claude)
  │
  └─→ JSON-RPC call_tool("tcps-create-work-order", {...})
      │
      └─→ erlmcp_server (routing)
          │
          └─→ tcps_work_order:create_work_order/1
              │
              ├─ Determine bucket, priority, SLA
              ├─ Check Kanban WIP limit
              ├─ Insert into ETS
              ├─ Generate creation receipt
              └─ Return {ok, work_order_id}
              │
              └─ JSON-RPC response → AI Client
```

## 9. Error Handling & Failure Modes

### 9.1 Cascading Failures

```
Scenario: Security gate failure
  │
  ├─ Gate 4 (security_scan) finds vulnerability
  ├─ Trigger Andon(security_scan, {sku_id, details})
  ├─ Work order status: in_progress → BLOCKED
  ├─ Dependent work orders remain queued
  ├─ Dashboard shows red alert
  ├─ Team notified (email/Slack hook if enabled)
  │
  └─ Analyst resolves:
     ├─ Performs 5-Whys → root cause
     ├─ Applies fix (patch vulnerability)
     ├─ Adds prevention (dependency audit in CI)
     ├─ Resolve Andon
     ├─ Work order auto-retries all gates
     └─ If pass, work order continues from where blocked
```

### 9.2 Partial Failures

```
Scenario: Test execution fails after deterministic build passes
  │
  ├─ Gate 5 (deterministic_build) → PASS ✓
  ├─ Gate 6 (quality_metrics) → PASS ✓
  ├─ Gate 7 (release_verification) → FAIL ✗
  │  (SBOM not generated)
  │
  ├─ Work order status: in_progress → BLOCKED at Gate 7
  ├─ Gates 1-6 receipts cached (5-minute TTL)
  ├─ Analyst fixes SBOM generation
  ├─ Trigger retry from Gate 7 (skip 1-6 due to cache)
  └─ If pass, work order continues
```

## 10. Performance & Scalability

### 10.1 Latency Breakdown

```
Work Order Creation: <5ms
├─ Bucket determination: <1ms
├─ Priority calculation: <1ms
├─ SLA deadline: <1ms
└─ ETS insert: <1ms

Kanban Check: <1ms
├─ ETS lookup (current WIP): <1ms
└─ Comparison: <1ms

Heijunka Scheduling (100 work orders): ~5ms
├─ Group by bucket: ~1ms
├─ Sort by priority: ~2ms
├─ Round-robin interleave: ~2ms

Quality Gate (compilation): 100-500ms
├─ Rebar3 subprocess: ~80-450ms
├─ Parse output: ~10ms
├─ Generate receipt: <5ms

Andon Trigger: <10ms
├─ Event creation: <1ms
├─ ETS insert: <1ms
├─ Receipt generation: <5ms
├─ SSE broadcast: <3ms

Receipt Verification: 10-100ms
├─ RDF query: ~5-50ms
├─ Chronological check: ~2-10ms
├─ Checksum verification: ~3-40ms
```

### 10.2 Throughput

| Operation | Throughput |
|-----------|------------|
| Create work order | 500K/sec (ETS-bound) |
| Check WIP limit | 500K/sec (ETS-bound) |
| Heijunka schedule | 100K/sec (CPU-bound) |
| Quality gate check | 1-10/sec (subprocess-bound) |
| Andon trigger | 100K/sec (ETS-bound) |
| Receipt verification | 10-100/sec (RDF-bound) |

## 11. Monitoring & Observability

### 11.1 Telemetry Events

```erlang
% Published to OTEL/Prometheus:

work_order.created
  ├─ bucket: reliability
  ├─ priority: 7
  └─ lead_time_hours: (on completion)

quality_gate.pass
  ├─ gate: test_execution
  ├─ sku_id: sku-123
  └─ duration_ms: 450

quality_gate.fail
  ├─ gate: security_scan
  ├─ sku_id: sku-123
  ├─ duration_ms: 120
  └─ failure_count: 2 (vulns)

andon.triggered
  ├─ failure_type: test_failure
  ├─ sku_id: sku-123
  ├─ stage: testing
  └─ severity: warning

andon.resolved
  ├─ andon_id: ANDON-...
  ├─ resolution_time_minutes: 15
  └─ root_cause: detected
```

## 12. Testing Strategies

### 12.1 Unit Tests

**File:** `/Users/sac/erlmcp/test/tcps_*_tests.erl`

All modules include EUnit tests covering:
- Happy path (normal operation)
- Error cases (invalid inputs, missing resources)
- Edge cases (boundary values, empty lists)
- State transitions (pending → queued → in_progress)

### 12.2 Integration Tests

**File:** `/Users/sac/erlmcp/test/integration/tcps_*_SUITE.erl`

Common Test suites covering:
- End-to-end workflows (signal → receipt)
- Multi-module interactions (Kanban + Heijunka + Gates)
- Andon stop-the-line enforcement
- Receipt chain verification
- Performance benchmarks

### 12.3 Simulation Tests

**File:** `/Users/sac/erlmcp/src/tcps_mcp_diataxis/simulator/`

TCPS simulator allows:
- Replay real scenarios
- Test custom policies
- Stress test with 1000s of work orders
- Visualize Heijunka leveling

---

**Status:** Production-ready (v1.5.0)
**Last Updated:** January 2026
**Maintainer:** erlmcp team
