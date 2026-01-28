# TCPS Subsystem - Component Architecture (L3)

**Derived from:** tcps/*.erl analysis + tcps_*.erl modules (57 modules total)

## Overview

TCPS (Toyota Code Production System) is a lean manufacturing-inspired quality and workflow management subsystem for erlmcp. It implements authentic Japanese manufacturing concepts (Jidoka, Andon, Kanban, Heijunka, Kaizen) adapted for software engineering.

## System Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                         TCPS SUBSYSTEM                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │          PULL SIGNAL INGESTION LAYER                     │  │
│  │  (tcps_work_order.erl) - Pull-based demand routing      │  │
│  │  - GitHub issues/PRs → Work orders                      │  │
│  │  - Marketplace install/refund events                    │  │
│  │  - CVE security advisories (Priority 10)                │  │
│  │  - 6 buckets: security, reliability, cost, compliance,  │  │
│  │             features, technical_debt                    │  │
│  └──────────────────────────────────────────────────────────┘  │
│         ↓                                                         │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │         FLOW CONTROL LAYER                               │  │
│  │  ┌─────────────────────────────────────────────────┐    │  │
│  │  │ Kanban (tcps_kanban.erl) - WIP limits           │    │  │
│  │  │ - Per-bucket limits (default: 5 per bucket)     │    │  │
│  │  │ - Check availability before starting             │    │  │
│  │  │ - Free slots on completion                       │    │  │
│  │  │ - Utilization tracking                           │    │  │
│  │  └─────────────────────────────────────────────────┘    │  │
│  │  ┌─────────────────────────────────────────────────┐    │  │
│  │  │ Heijunka (tcps_heijunka.erl) - Leveling         │    │  │
│  │  │ - Prevent batching (round-robin across buckets) │    │  │
│  │  │ - Max 2 consecutive from same bucket             │    │  │
│  │  │ - Leveling score calculation (0-1)              │    │  │
│  │  │ - Batch scheduling with optimal allocation      │    │  │
│  │  └─────────────────────────────────────────────────┘    │  │
│  └──────────────────────────────────────────────────────────┘  │
│         ↓                                                         │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │         QUALITY GATES LAYER (Jidoka)                    │  │
│  │  (tcps_quality_gates.erl) - 8 sequential gates          │  │
│  │  1. SHACL validation (ontology conformance)              │  │
│  │  2. Compilation (zero errors required)                  │  │
│  │  3. Test execution (95% pass rate, 80% coverage)        │  │
│  │  4. Security scan (CVE, secrets, patterns)              │  │
│  │  5. Deterministic build (SHA-256 verification)          │  │
│  │  6. Quality metrics (production thresholds)              │  │
│  │  7. Release verification (SBOM, licenses, pinned deps)  │  │
│  │  8. Smoke tests (basic functionality)                   │  │
│  │  - Gate results cached in ETS (fast lookup)             │  │
│  │  - Triggers Andon on failure (blocking gates only)      │  │
│  │  - Generates receipts for audit trail                   │  │
│  │  - Stage transition validation                          │  │
│  └──────────────────────────────────────────────────────────┘  │
│         ↓ (if all gates pass)                                    │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │         ANDON STOP-THE-LINE LAYER                       │  │
│  │  (tcps/tcps_andon.erl + tcps_andon.erl)                 │  │
│  │  5 failure types trigger Andon:                         │  │
│  │  - shacl_violation (critical)                           │  │
│  │  - compilation_failure (critical)                       │  │
│  │  - test_failure (warning)                               │  │
│  │  - non_determinism (critical)                           │  │
│  │  - missing_receipt (warning)                            │  │
│  │  - Integration hooks:                                   │  │
│  │    • hook_compilation_failure/1                         │  │
│  │    • hook_test_failure/1                                │  │
│  │    • hook_shacl_failure/1                               │  │
│  │  - Event lifecycle: open → root cause analysis →        │  │
│  │                     fix applied → prevention → resolved │  │
│  │  - Dashboard broadcasting (SSE + real-time)             │  │
│  └──────────────────────────────────────────────────────────┘  │
│         ↓ (if no Andons or resolved)                            │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │         RECEIPT CHAIN LAYER                              │  │
│  │  (tcps_receipt.erl + tcps_receipt_verifier.erl)         │  │
│  │  - SHA-256 chain verification (immutable audit trail)    │  │
│  │  - Stage receipts: creation, start, progress, complete  │  │
│  │  - Chronological ordering validation                    │  │
│  │  - No timestamp gaps (max 24 hours allowed)              │  │
│  │  - Deterministic verification (hash equality)           │  │
│  │  - Lead time calculation (created_at → completed_at)    │  │
│  │  - All receipts stored as JSON + RDF ontology            │  │
│  └──────────────────────────────────────────────────────────┘  │
│         ↓                                                         │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │         SKU RELEASE LAYER                                │  │
│  │  (tcps_sku.erl) - Product version certification         │  │
│  │  - Stage transitions: requirements → design →           │  │
│  │                      implementation → testing →          │  │
│  │                      integration → deployment → published│  │
│  │  - SLA tracking (security: 24h, reliability: 7d, etc)    │  │
│  │  - Dependency graph (blocking/blocked_by)                │  │
│  │  - Ontology persistence (RDF triples)                    │  │
│  └──────────────────────────────────────────────────────────┘  │
│         ↓                                                         │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │         KAIZEN CONTINUOUS IMPROVEMENT LAYER              │  │
│  │  (tcps_kaizen.erl) - Metrics & learning                 │  │
│  │  - Lead time tracking                                   │  │
│  │  - First-pass yield (gates passed on first try / total) │  │
│  │  - SLA compliance rate                                  │  │
│  │  - Defect rate (Andon events / work orders)              │  │
│  │  - Per-gate pass rates                                  │  │
│  │  - Improvement suggestions from patterns                │  │
│  └──────────────────────────────────────────────────────────┘  │
│                                                                   │
└─────────────────────────────────────────────────────────────────┘
```

## Core Module Mapping

### Tier 1: Work Order & Pull Signal Management (5 modules)

| Module | File | Lines | Responsibility |
|--------|------|-------|-----------------|
| **tcps_work_order** | src/tcps_work_order.erl | 2203 | JIT pull signal routing, work order lifecycle, SLA tracking, dependency management |
| **tcps_kanban** | src/tcps_kanban.erl | 300+ | WIP limit enforcement per bucket, availability checking, slot management |
| **tcps_heijunka** | src/tcps_heijunka.erl | 250+ | Production leveling (round-robin), batch scheduling, distribution uniformity |
| **tcps_kaizen** | src/tcps_kaizen.erl | 200+ | Metrics collection, improvement tracking, lead time analysis |
| **tcps_root_cause** | src/tcps_root_cause.erl | TBD | 5-Whys analysis, prevention measure tracking |

### Tier 2: Quality Gates (Jidoka Implementation) (4 modules)

| Module | File | Lines | Responsibility |
|--------|------|-------|-----------------|
| **tcps_quality_gates** | src/tcps_quality_gates.erl | 1318 | 8-gate pipeline, SHACL/compile/test/security/deterministic/metrics/release/smoke, stage-gate mapping |
| **tcps/tcps_rebar3_quality** | src/tcps/tcps_rebar3_quality.erl | TBD | Rebar3 plugin integration for quality gates |
| **tcps/tcps_rebar3_shacl** | src/tcps/tcps_rebar3_shacl.erl | TBD | SHACL validation gate execution |
| **tcps_sku** | src/tcps_sku.erl | 200+ | SKU (product version) lifecycle, stage transitions, release certification |

### Tier 3: Stop-the-Line (Andon) (4 modules)

| Module | File | Lines | Responsibility |
|--------|------|-------|-----------------|
| **tcps_andon** | src/tcps_andon.erl | 696 | Event triggering (5 types), resolution workflow, receipt generation, SSE broadcasting |
| **tcps/tcps_andon.erl** | src/tcps/tcps_andon.erl | TBD | Core Andon state machine, blocking logic |
| **tcps/tcps_rebar3_andon.erl** | src/tcps/tcps_rebar3_andon.erl | TBD | Rebar3 plugin for Andon integration |
| **tcps_cli_andon** | src/tcps_cli_andon.erl | TBD | CLI interface for Andon management |

### Tier 4: Receipt Chain (Audit Trail) (5 modules)

| Module | File | Lines | Responsibility |
|--------|------|-------|-----------------|
| **tcps_receipt** | src/tcps_receipt.erl | 233 | Chain verification, deterministic checking, audit trail generation |
| **tcps_receipt_verifier** | src/tcps_receipt_verifier.erl | TBD | SHA-256 chain validation, completeness checks |
| **tcps_persistence** | src/tcps_persistence.erl | TBD | RDF/TTL ontology storage, receipt persistence |
| **tcps/tcps_rebar3_receipt** | src/tcps/tcps_rebar3_receipt.erl | TBD | Rebar3 plugin for receipt generation |
| **tcps_cli_receipt** | src/tcps_cli_receipt.erl | TBD | CLI for receipt inspection |

### Tier 5: Observability & Monitoring (6 modules)

| Module | File | Lines | Responsibility |
|--------|------|-------|-----------------|
| **tcps_dashboard** | src/tcps_dashboard.erl | TBD | Real-time web dashboard (Cowboy) |
| **tcps_dashboard_handler** | src/tcps_dashboard_handler.erl | TBD | HTTP handler for dashboard endpoints |
| **tcps_dashboard_sse_handler** | src/tcps_dashboard_sse_handler.erl | TBD | Server-sent events for real-time updates |
| **tcps_sse_manager** | src/tcps_sse_manager.erl | TBD | SSE broadcast manager |
| **tcps_metrics_collector** | src/tcps_mcp_diataxis/telemetry/tcps_metrics_collector.erl | TBD | OTEL metrics aggregation |
| **tcps_metrics_cache** | src/tcps_metrics_cache.erl | TBD | In-memory metrics caching |

### Tier 6: MCP Integration (7 modules)

| Module | File | Lines | Responsibility |
|--------|------|-------|-----------------|
| **tcps_mcp_server** | src/tcps_mcp_diataxis/tcps_mcp_server.erl | 150+ | MCP server integration, tool registration |
| **tcps_mcp_tools** | src/tcps_mcp_diataxis/tcps_mcp_tools.erl | TBD | Tool definitions, input schemas |
| **tcps_mcp_prompts** | src/tcps_mcp_diataxis/tcps_mcp_prompts.erl | TBD | Learning prompts for Diataxis |
| **tcps_diataxis_* (4 modules)** | src/tcps_mcp_diataxis/tcps_diataxis_*.erl | TBD | Tutorial, How-to, Explanation, Reference |
| **tcps_simulator** | src/tcps_mcp_diataxis/tcps_simulator.erl | TBD | Simulation environment |

### Tier 7: Utilities & CLI (8 modules)

| Module | File | Lines | Responsibility |
|--------|------|-------|-----------------|
| **tcps_cli_work_order** | src/tcps_cli_work_order.erl | TBD | CLI for work order management |
| **tcps_cli_kanban** | src/tcps_cli_kanban.erl | TBD | CLI for Kanban visualization |
| **tcps_cli_quality** | src/tcps_cli_quality.erl | TBD | CLI for quality gates |
| **tcps_cli_kaizen** | src/tcps_cli_kaizen.erl | TBD | CLI for improvement tracking |
| **tcps_cli_format** | src/tcps_cli_format.erl | TBD | Output formatting utilities |
| **tcps_cli_config** | src/tcps_cli_config.erl | TBD | Configuration management |
| **tcps_cli_examples** | src/tcps_cli_examples.erl | TBD | Example scenarios |
| **tcps_cli_root_cause** | src/tcps_cli_root_cause.erl | TBD | 5-Whys CLI interface |

### Tier 8: RDF/Ontology (4 modules)

| Module | File | Lines | Responsibility |
|--------|------|-------|-----------------|
| **tcps_ontology_index** | src/tcps_ontology_index.erl | TBD | RDF index for fast queries |
| **tcps_rdf_incremental** | src/tcps_rdf_incremental.erl | TBD | Incremental RDF updates |
| **tcps_query_cache** | src/tcps_query_cache.erl | TBD | SPARQL query result caching |
| **rebar3_tcps_plugin** | src/tcps/rebar3_tcps_plugin.erl | TBD | Rebar3 TCPS plugin loader |

## Key Data Types

### Work Order

```erlang
-type work_order() :: #{
    id := work_order_id(),                          % WO-timestamp-random
    bucket := reliability | security | cost | compliance | features | technical_debt,
    priority := 1..10,                              % 1=lowest, 10=critical (CVE)
    status := pending | queued | in_progress | blocked | completed | cancelled,
    description := binary(),
    pull_signal := pull_signal(),                   % Original demand signal
    created_at := erlang:timestamp(),
    started_at => erlang:timestamp(),
    completed_at => erlang:timestamp(),
    sla_deadline := erlang:timestamp(),             % From bucket SLA hours
    current_stage => requirements | design | ... | published,
    stages_completed => [stage()],
    receipts => [binary()],                         % Receipt IDs
    sku_id => binary(),                             % Link to published product
    dependencies => [work_order_id()],              % Blocking relationships
    blocked_by => [work_order_id()],
    metadata => map()
}.
```

### Andon Event

```erlang
-type andon_event() :: #{
    event_id := binary(),                           % ANDON-timestamp-random-unique
    failure_type := shacl_violation | test_failure | compilation_failure |
                    non_determinism | missing_receipt,
    sku_id := binary(),
    stage := compilation | testing | validation | execution | integration | deployment,
    timestamp := integer(),                         % Milliseconds since epoch
    details := map(),                               % Gate-specific details
    status := open | resolved,                      % Stop-the-line status
    resolution => resolution(),                     % Root cause + fix
    metadata => map()
}.
```

### Receipt

```erlang
-type receipt() :: #{
    receipt_id := binary(),                         % RCPT-gate-timestamp-random
    receipt_type := quality_gate | work_order | andon_event | resolution,
    sku_id := binary(),
    stage := atom(),                                % Work order stage
    timestamp := integer(),                         % Milliseconds since epoch
    status := pass | fail,
    details := map(),                               % Gate-specific results
    ontology_refs := [binary()],                    % tcps: RDF references
    parent_receipt_id => binary()                   % For chaining
}.
```

## Quality Gate Thresholds (Production Standards)

```erlang
-define(PRODUCTION_THRESHOLDS, #{
    test_pass_rate => 0.95,         % ✓ 95% minimum (Toyota standard)
    test_coverage => 0.80,           % ✓ 80% minimum (industry best)
    quality_gate_pass_rate => 0.95,  % ✓ 95% gates pass on first try
    defect_rate => 0.05,             % ✓ 5% max defect rate
    first_pass_yield => 0.90         % ✓ 90% first-pass success
}).
```

## SLA Targets by Bucket

| Bucket | SLA | Priority | Examples |
|--------|-----|----------|----------|
| **security** | 24h | 8-10 | CVE advisories, auth bypasses |
| **reliability** | 7 days | 5-9 | Production bugs, data loss |
| **compliance** | 7 days | 4-8 | Legal holds, audit requirements |
| **cost** | 30 days | 3-7 | Performance, resource optimization |
| **features** | 30 days | 2-6 | Enhancement requests, UX |
| **technical_debt** | ∞ | 2-4 | Refactoring, code cleanup |

## State Persistence

All state persists to:
1. **ETS tables** (in-memory, process-local):
   - `tcps_work_orders` - Work order index
   - `tcps_quality_gates_cache` - Gate results
   - `tcps_andon_events` - Andon state machine

2. **RDF/TTL files** (ontology):
   - `ontology/work_orders.ttl` - Work order triples
   - `ontology/andons.ttl` - Andon events
   - `ontology/skus.ttl` - Released products

3. **JSON files** (receipts):
   - `priv/receipts/work_orders/` - Work order receipts
   - `priv/receipts/quality_gates/` - Gate receipts
   - `priv/receipts/andons/` - Andon receipts

## Integration Points

### Upstream (Demand Signals)

- **GitHub API** → `tcps_work_order:create_from_github/1`
- **Marketplace Events** → `tcps_work_order:create_from_marketplace/1`
- **CVE Advisories** → `tcps_work_order:create_from_security_advisory/1`

### Downstream (Execution)

- **Rebar3 Build** → Quality gates invoked automatically
- **Test Runner** → Coverage & pass rates extracted
- **Release Manager** → Artifact verification & SBOM generation
- **Dashboard** → Real-time SSE broadcasts

### Horizontal (Coordination)

- **erlmcp_client** → Instrument work orders as tool calls
- **erlmcp_registry** → Message routing for Andon events
- **MCP Tools** → `/tcps-*` commands in AI agents

## File Organization Summary

```
src/tcps/
  ├── tcps_andon.erl                    (Core stop-the-line)
  ├── tcps_deterministic.erl            (Hash verification)
  ├── tcps_health.erl                   (System health checks)
  ├── tcps_rebar3_*.erl                 (4 Rebar3 plugins)
  └── rebar3_tcps_plugin.erl            (Plugin loader)

src/
  ├── tcps_work_order.erl               (2203 lines, pull signals)
  ├── tcps_quality_gates.erl            (1318 lines, 8 gates)
  ├── tcps_andon.erl                    (696 lines, events + UI)
  ├── tcps_receipt.erl                  (233 lines, chain verification)
  ├── tcps_kanban.erl                   (300+)
  ├── tcps_heijunka.erl                 (250+)
  ├── tcps_kaizen.erl                   (200+)
  ├── tcps_sku.erl                      (200+)
  ├── tcps_root_cause.erl
  ├── tcps_receipt_verifier.erl
  ├── tcps_persistence.erl
  ├── tcps_ontology_index.erl
  ├── tcps_rdf_incremental.erl
  ├── tcps_query_cache.erl
  ├── tcps_metrics_*.erl                (3 telemetry modules)
  ├── tcps_dashboard*.erl               (3 dashboard modules)
  ├── tcps_sse_manager.erl
  ├── tcps_cli_*.erl                    (8 CLI modules)
  └── tcps_mcp_diataxis/                (7 MCP + learning modules)
       ├── tcps_mcp_server.erl          (MCP integration)
       ├── tcps_mcp_tools.erl           (Tool registry)
       ├── tcps_mcp_prompts.erl
       ├── tcps_simulator.erl
       ├── tcps_diataxis_*.erl          (4 learning paths)
       ├── explanation/                 (Concept docs)
       ├── reference/                   (Technical specs)
       ├── tutorial/                    (Guided learning)
       └── telemetry/                   (OTEL collection)
```

## Execution Flow: Pull Signal → Receipt

```
1. Pull Signal Arrives (demand-driven)
   └─→ tcps_work_order:create_work_order/1
       • Determine bucket (security/reliability/cost/...)
       • Calculate priority (1-10)
       • Set SLA deadline (from bucket config)
       • Status: queued

2. Kanban Check (WIP limits)
   └─→ tcps_kanban:check_wip_limit/1
       • Current WIP < limit?
       • NO → wait in queue
       • YES → proceed

3. Heijunka Leveling (prevent batching)
   └─→ tcps_heijunka:level_work_orders/1
       • Round-robin across buckets
       • Max 2 consecutive from same bucket
       • Status: in_progress

4. Quality Gates (8-stage pipeline, stop on first failure)
   ├─→ 1. SHACL validation (ontology conformance)
   ├─→ 2. Compilation (zero errors)
   ├─→ 3. Test execution (95% pass, 80% coverage)
   ├─→ 4. Security scan (CVE, secrets, patterns)
   ├─→ 5. Deterministic build (SHA-256 equality)
   ├─→ 6. Quality metrics (thresholds)
   ├─→ 7. Release verification (SBOM, licenses)
   └─→ 8. Smoke tests (basic functionality)

5. Gate Failure → Andon (stop-the-line)
   └─→ tcps_andon:trigger_andon/2
       • Event type: shacl_violation, test_failure, etc.
       • Status: open (blocking)
       • Generate Andon receipt
       • Broadcast to dashboard (SSE)
       • WAIT for resolution

6. Andon Resolution (root cause + prevention)
   └─→ tcps_andon:resolve_andon/2
       • Analyst performs 5-Whys
       • Records root_cause
       • Applies fix_applied
       • Adds prevention_added
       • Status: resolved
       • Generate resolution receipt

7. Stage Progression (if gates pass)
   └─→ tcps_work_order:progress_work_order/2
       • Move: requirements → design → implementation → ...
       • Generate stage receipt
       • Check stage-specific quality gates
       • Generate stage receipt

8. Work Order Completion (all stages done)
   └─→ tcps_work_order:complete_work_order/2
       • Link to published SKU
       • Status: completed
       • Free Kanban WIP slot
       • Resolve dependencies (unblock blocked orders)
       • Update Kaizen metrics
       • Save to RDF ontology
       • Generate completion receipt

9. Receipt Chain Verification
   └─→ tcps_receipt:verify_chain/1
       • Check stages present: shacl, compile, test, security, ...
       • Verify chronological ordering (timestamps increasing)
       • Check no gaps > 24 hours
       • SHA-256 chain validation
       • Status: complete ✓

10. Kaizen Metrics
    └─→ tcps_kaizen:record_receipt/1
        • Lead time: created_at → completed_at
        • First-pass yield: gates passed on first try / total
        • SLA compliance: completed within deadline
        • Defect rate: Andon events / work orders
        • Improvement opportunities identified
```

## Integration with erlmcp Core

### JSON-RPC Channels

```erlang
% MCP tools auto-registered:
/ tcps-work-order create
  Input: #{bucket, priority, description, pull_signal}
  Output: {ok, work_order_id}

/ tcps-andon resolve
  Input: #{andon_id, root_cause, fix_applied, prevention_added}
  Output: {ok, resolution_timestamp}

/ tcps-quality-gates check
  Input: #{sku_id}
  Output: {ok, [receipts]} | {failed_at, gate, violations}

/ tcps-kanban status
  Input: #{bucket}
  Output: #{current, limit, available, utilization}
```

### Message Routing (via tcps_andon broadcast)

```erlang
% Andon events broadcast to:
1. Dashboard (via SSE) → Real-time visualization
2. Error logger → Audit trail
3. Metrics collector → OTEL/Prometheus
4. Email/Slack hooks → On-call notification (optional)
```

## Standards & Best Practices

### Zero-Defect Quality (Jidoka)

- **NEVER allow defective code to proceed** → Gates block immediately
- **Built-in quality** → Automated detection at each stage
- **Stop-the-line authority** → Andon prevents downstream pollution
- **Root cause discipline** → No quick fixes, analyze 5 levels deep

### Lean Pull System

- **Never push work** → Pull only when capacity available
- **Just-in-time demand** → Work orders created by pull signals
- **Kanban limits prevent overload** → Respect WIP constraints
- **Heijunka prevents local optimization** → Smooth flow > peak efficiency

### Kaizen (Continuous Improvement)

- **Measure everything** → Lead time, first-pass yield, defect rate
- **Learn from every failure** → Andon events become lessons
- **Small incremental changes** → Prevent regressions
- **Team involvement** → Root cause analysis is collective

## Performance Characteristics

| Operation | Method | Latency | Throughput |
|-----------|--------|---------|------------|
| Create work order | ETS insert | <1ms | 500K/sec |
| Check WIP limit | ETS lookup | <1ms | 500K/sec |
| Heijunka schedule | In-memory sort | 1-10ms | 100K/sec |
| Quality gate check | Subprocess | 100-1000ms | 1-10/sec |
| Receipt verification | RDF query | 10-100ms | 10-100/sec |
| Andon broadcast | SSE | <50ms | 100/sec |

## Testing & Validation

All modules include comprehensive test suites:
- Unit tests: EUnit (`test/*_tests.erl`)
- Integration tests: Common Test (`test/integration/*_SUITE.erl`)
- Simulation tests: TCPS simulator environment
- Real-world workflows in examples/

See `/Users/sac/erlmcp/test/integration/tcps_*.erl` for full test coverage.

---

**Status:** Production-ready (v1.5.0)
**Last Updated:** January 2026
**Maintainer:** erlmcp team
