# erlmcp Pricing/Plans Subsystem - C4 Level 3 Component Diagram

**Status:** Production (v0.6.0+)
**Derived from:** Pricing/plan module analysis (src/erlmcp_*.erl, plans/*.plan.json, shapes/pricing_plan.schema.json)
**Last Updated:** 2026-01-27

## 1. System Purpose

The pricing/plans subsystem enforces deterministic, tier-based service limitations through:
- **Plan Envelope Definitions** - Throughput, connections, latency, queue depth, failover SLA
- **Refusal Behavior** - Deterministic responses when limits are exceeded
- **Evidence Chain** - Immutable audit trail with SHA-256 hash continuity
- **SLA Monitoring** - Real-time compliance tracking against plan specifications
- **Upgrade/Downgrade Path** - Controlled plan migrations with safety gates

## 2. Component Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    PRICING/PLANS SUBSYSTEM                              │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │ PLAN DEFINITION & LOADING (Data Layer)                          │  │
│  ├──────────────────────────────────────────────────────────────────┤  │
│  │                                                                  │  │
│  │  ┌─────────────────────────────┐                               │  │
│  │  │  erlmcp_plan_loader         │  Load team/enterprise/gov     │  │
│  │  │  (src/erlmcp_plan_loader.erl)│  specs from JSON files      │  │
│  │  └──────────────┬──────────────┘                               │  │
│  │                 │ load_plan(PlanName)                          │  │
│  │                 ▼                                               │  │
│  │  ┌─────────────────────────────┐                               │  │
│  │  │  erlmcp_pricing_plan        │  Deterministic validators     │  │
│  │  │ (src/erlmcp_pricing_plan.erl)│  • load_plan/1             │  │
│  │  │                             │  • get_envelope/1            │  │
│  │  │                             │  • check_refusal/2           │  │
│  │  │                             │  • validate_plan/1           │  │
│  │  └──────────────┬──────────────┘                               │  │
│  │                 │ {ok, plan_spec} ← plans/*.plan.json         │  │
│  │                 ▼                                               │  │
│  │  ┌─────────────────────────────┐                               │  │
│  │  │  erlmcp_plan                │  Legacy simple interface      │  │
│  │  │ (src/erlmcp_plan.erl)        │  • current_plan/0           │  │
│  │  │                             │  • get_limits/1             │  │
│  │  │                             │  • verify_sla/1             │  │
│  │  └─────────────────────────────┘                               │  │
│  │                                                                  │  │
│  │  Plan Files (JSON):                                            │  │
│  │  • plans/team.plan.json        (450 req/s, 25K conns)         │  │
│  │  • plans/enterprise.plan.json  (1500 req/s, 100K conns)       │  │
│  │  • plans/gov.plan.json         (900 req/s, 256 conns, audit)  │  │
│  │                                                                  │  │
│  │  Schema: shapes/pricing_plan.schema.json (JSON-Schema v7)      │  │
│  └──────────────────────────────────────────────────────────────────┘  │
│                                                                           │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │ VALIDATION & COMPLIANCE (Poka-Yoke Layer)                       │  │
│  ├──────────────────────────────────────────────────────────────────┤  │
│  │                                                                  │  │
│  │  ┌─────────────────────────────┐                               │  │
│  │  │  erlmcp_pricing_poka_yoke   │  CI/CD quality gates          │  │
│  │  │ (src/erlmcp_pricing_poka_yoke│  • validate_plan/1          │  │
│  │  │         .erl)               │  • validate_plan_schema/1    │  │
│  │  │                             │  • validate_envelope_consistency│ │
│  │  │  Uses: erlmcp_refusal.hrl   │  • validate_refusal_codes/1  │  │
│  │  │                             │  • validate_evidence_req/1    │  │
│  │  └──────────────┬──────────────┘                               │  │
│  │                 │ {error, [validation_error]} with hints       │  │
│  │                 │                                               │  │
│  │                 ├─ schema_error (missing fields)                │  │
│  │                 ├─ envelope_error (bounds unrealistic)          │  │
│  │                 ├─ refusal_error (codes don't exist)            │  │
│  │                 └─ evidence_error (artifacts missing)            │  │
│  │                                                                  │  │
│  │  ┌─────────────────────────────┐                               │  │
│  │  │  erlmcp_refusal_plan_validator  │  Refusal code validation   │  │
│  │  │ (src/erlmcp_refusal_plan_vali   │                          │  │
│  │  │           dator.erl)           │                          │  │
│  │  └─────────────────────────────┘                               │  │
│  │                                                                  │  │
│  │  ┌─────────────────────────────┐                               │  │
│  │  │  erlmcp_chaos_plan_validator │  Failure injection testing    │  │
│  │  │ (src/erlmcp_chaos_plan_vali   │  • Bounded refusal frames   │  │
│  │  │           dator.erl)         │  • Recovery time < 5s       │  │
│  │  └─────────────────────────────┘                               │  │
│  │                                                                  │  │
│  │  ┌─────────────────────────────┐                               │  │
│  │  │  erlmcp_bench_plan_validator │  Performance benchmarking     │  │
│  │  │ (src/erlmcp_bench_plan_vali   │  • Verify envelope metrics  │  │
│  │  │           dator.erl)         │  • <10% regression bound     │  │
│  │  └─────────────────────────────┘                               │  │
│  │                                                                  │  │
│  └──────────────────────────────────────────────────────────────────┘  │
│                                                                           │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │ EVIDENCE & RECEIPT CHAIN (Audit Trail Layer)                    │  │
│  ├──────────────────────────────────────────────────────────────────┤  │
│  │                                                                  │  │
│  │  ┌─────────────────────────────┐                               │  │
│  │  │  erlmcp_pricing_receipt      │  Immutable audit trail        │  │
│  │  │ (src/erlmcp_pricing_receipt.e│  with SHA-256 hash chains   │  │
│  │  │          rl)                │                              │  │
│  │  │                             │  API:                         │  │
│  │  │                             │  • create_receipt/2,3         │  │
│  │  │  Receipt Storage:           │  • add_refusal/3,4            │  │
│  │  │  priv/receipts/             │  • verify_receipt/1           │  │
│  │  │    <plan>/                  │  • verify_receipt_chain/1     │  │
│  │  │      <version>/             │  • verify_conformance/2       │  │
│  │  │        <timestamp>.json      │  • export_receipt/2,3        │  │
│  │  │                             │                              │  │
│  │  │  Hash: previous_receipt_hash│                              │  │
│  │  │        + current_hash       │                              │  │
│  │  │                             │                              │  │
│  │  │  Fields:                    │                              │  │
│  │  │  • receipt_id (UUID)        │                              │  │
│  │  │  • envelope_claim (snapshot)│                              │  │
│  │  │  • refusal_trigger (events) │                              │  │
│  │  │  • hash_chain (integrity)   │                              │  │
│  │  │  • audit_fields (context)   │                              │  │
│  │  │  • conformance_events       │                              │  │
│  │  └─────────────────────────────┘                               │  │
│  │                                                                  │  │
│  │  Schema: shapes/pricing_receipt.schema.json                    │  │
│  │  Export: JSON, CSV, TSV for compliance audits                  │  │
│  │                                                                  │  │
│  └──────────────────────────────────────────────────────────────────┘  │
│                                                                           │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │ STATE MANAGEMENT (Runtime State Layer)                          │  │
│  ├──────────────────────────────────────────────────────────────────┤  │
│  │                                                                  │  │
│  │  ┌─────────────────────────────┐                               │  │
│  │  │  erlmcp_pricing_state       │  ETS-backed state store       │  │
│  │  │ (src/erlmcp_pricing_state.erl│                             │  │
│  │  │                             │  • get_current_plan/0         │  │
│  │  │  ETS Table:                 │  • set_current_plan/1         │  │
│  │  │  erlmcp_pricing_state_table │  • get_last_upgrade_time/1    │  │
│  │  │                             │  • get_certification_valid/1  │  │
│  │  │  Keys:                      │  • get_upgrade_timestamp/0    │  │
│  │  │  • current_plan             │  • get_all_state/0            │  │
│  │  │  • {last_upgrade_time, Plan}│                              │  │
│  │  │  • {certification_valid, P} │  RWC: write_concurrency      │  │
│  │  │  • upgrade_timestamp        │  RRC: read_concurrency       │  │
│  │  └─────────────────────────────┘                               │  │
│  │                                                                  │  │
│  └──────────────────────────────────────────────────────────────────┘  │
│                                                                           │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │ SLA MONITORING (Real-Time Compliance Layer)                     │  │
│  ├──────────────────────────────────────────────────────────────────┤  │
│  │                                                                  │  │
│  │  ┌─────────────────────────────┐                               │  │
│  │  │  erlmcp_plan_sla_monitor    │  gen_server: SLA enforcement  │  │
│  │  │ (src/erlmcp_plan_sla_monitor│                              │  │
│  │  │         .erl)               │  Plan Envelopes:              │  │
│  │  │                             │  • team: 450 req/s, p99≤150ms │  │
│  │  │  API:                       │  • enterprise: 1500 req/s,    │  │
│  │  │  • monitor_envelope/2       │    p99≤100ms                  │  │
│  │  │  • check_throughput/1       │  • gov: 900 req/s, p99≤80ms  │  │
│  │  │  • check_latency/1          │                              │  │
│  │  │  • check_failover/1         │  SLA Violations:              │  │
│  │  │  • alert_sla_violation/2    │  • throughput_exceeded        │  │
│  │  │  • export_sla_metrics/2     │  • latency_violated           │  │
│  │  │  • get_sla_status/1         │  • failover_timeout           │  │
│  │  │  • get_sla_dashboard/1      │                              │  │
│  │  │                             │  Monitoring Interval: 5min    │  │
│  │  │  Integration:               │  Violation Window: 60min      │  │
│  │  │  ├─ erlmcp_metrics_server   │                              │  │
│  │  │  ├─ erlmcp_pricing_receipt  │                              │  │
│  │  │  └─ HTTP dashboard endpoint │                              │  │
│  │  └─────────────────────────────┘                               │  │
│  │                                                                  │  │
│  │  ┌─────────────────────────────┐                               │  │
│  │  │  erlmcp_plan_sla_monitor_ext│  Extended monitoring          │  │
│  │  │ (src/erlmcp_plan_sla_monit  │  • Continuous metrics         │  │
│  │  │        or_extended.erl)     │  • Trend analysis             │  │
│  │  │                             │  • Predictive alerts          │  │
│  │  └─────────────────────────────┘                               │  │
│  │                                                                  │  │
│  │  ┌─────────────────────────────┐                               │  │
│  │  │  erlmcp_sla_continuous_monitor  │  Background monitoring    │  │
│  │  │ (src/erlmcp_sla_continuous_    │                          │  │
│  │  │        monitor.erl)            │                          │  │
│  │  └─────────────────────────────┘                               │  │
│  │                                                                  │  │
│  │  HTTP Endpoints (erlmcp_sla_http_handler):                     │  │
│  │  GET /sla/status/<plan>       → compliance status              │  │
│  │  GET /sla/dashboard/<plan>    → dashboard data                 │  │
│  │  POST /sla/export/<plan>      → export metrics (JSON/CSV/TSV)  │  │
│  │                                                                  │  │
│  └──────────────────────────────────────────────────────────────────┘  │
│                                                                           │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │ UPGRADE/DOWNGRADE PATH (Plan Migration Layer)                   │  │
│  ├──────────────────────────────────────────────────────────────────┤  │
│  │                                                                  │  │
│  │  ┌─────────────────────────────┐                               │  │
│  │  │  erlmcp_pricing_upgrade     │  Plan transitions with gates  │  │
│  │  │ (src/erlmcp_pricing_upgrade │                              │  │
│  │  │         .erl)               │  Allowed Upgrades:            │  │
│  │  │                             │  • team → enterprise          │  │
│  │  │  API:                       │  • enterprise → gov           │  │
│  │  │  • can_upgrade/2            │                              │  │
│  │  │  • can_downgrade/2 (always) │  Forbidden Downgrades:        │  │
│  │  │  • get_upgrade_path/2       │  • any to lower tier          │  │
│  │  │  • list_possible_upgrades/1 │                              │  │
│  │  │  • simulate_upgrade/2       │  Safety Gates:                │  │
│  │  │  • apply_upgrade/2          │  1. certification_valid       │  │
│  │  │  • verify_upgrade/1         │  2. infrastructure_headroom   │  │
│  │  │  • calculate_migration_time │  3. clean_receipt_state       │  │
│  │  │  • check_upgrade_cooldown   │  4. sla_compliance           │  │
│  │  │  • snapshot_system_state/1  │  5. resource_availability    │  │
│  │  │  • restore_system_state/1   │                              │  │
│  │  │                             │  Cooldown: Plan-specific      │  │
│  │  │  Migration Steps:           │  Rollback: Available if       │  │
│  │  │  ├─ pre_upgrade_checks      │            rollback_compatible│  │
│  │  │  ├─ upgrade_phase           │                              │  │
│  │  │  └─ post_upgrade_verification│                              │  │
│  │  │                             │  Envelope Expansion:          │  │
│  │  │  Limits Updated:            │  • old_envelope              │  │
│  │  │  • update_team_limits()     │  • new_envelope              │  │
│  │  │  • update_enterprise_limits()│                              │  │
│  │  │  • update_gov_limits()      │  Logging:                     │  │
│  │  │                             │  • log_upgrade_event/3        │  │
│  │  │                             │  • record_upgrade_timestamp/1 │  │
│  │  │                             │  • update_plan_metadata/1     │  │
│  │  │                             │  • get_upgrade_history/1      │  │
│  │  └─────────────────────────────┘                               │  │
│  │                                                                  │  │
│  │  Upgrade Paths (JSON):                                         │  │
│  │  • plans/upgrade_team_to_enterprise.json                       │  │
│  │  • plans/upgrade_enterprise_to_gov.json                        │  │
│  │                                                                  │  │
│  └──────────────────────────────────────────────────────────────────┘  │
│                                                                           │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │ CLI & UTILITIES (User Interface Layer)                          │  │
│  ├──────────────────────────────────────────────────────────────────┤  │
│  │                                                                  │  │
│  │  ┌─────────────────────────────┐                               │  │
│  │  │  erlmcp_plan_cli            │  Command-line interface       │  │
│  │  │ (src/erlmcp_plan_cli.erl)    │                             │  │
│  │  │                             │  Commands:                    │  │
│  │  │                             │  • plan show <tier>           │  │
│  │  │                             │  • plan list                  │  │
│  │  │                             │  • plan validate <file>       │  │
│  │  │                             │  • upgrade simulate           │  │
│  │  │                             │  • upgrade apply              │  │
│  │  └─────────────────────────────┘                               │  │
│  │                                                                  │  │
│  │  ┌─────────────────────────────┐                               │  │
│  │  │  erlmcp_plan_docs_generator │  Documentation generation     │  │
│  │  │ (src/erlmcp_plan_docs_gener │                             │  │
│  │  │         ator.erl)           │  • Generates Markdown docs   │  │
│  │  │                             │  • Portal HTML/CSS            │  │
│  │  │                             │  • Plan comparisons           │  │
│  │  └─────────────────────────────┘                               │  │
│  │                                                                  │  │
│  │  ┌─────────────────────────────┐                               │  │
│  │  │  erlmcp_sla_dashboard_handler  │  HTTP dashboard handler    │  │
│  │  │ (src/erlmcp_sla_dashboard_ha   │                          │  │
│  │  │         ndler.erl)             │                          │  │
│  │  └─────────────────────────────┘                               │  │
│  │                                                                  │  │
│  │  Portal Template:                                              │  │
│  │  templates/pricing_portal.html                                 │  │
│  │                                                                  │  │
│  └──────────────────────────────────────────────────────────────────┘  │
│                                                                           │
└─────────────────────────────────────────────────────────────────────────┘
```

## 3. Component Dependencies

```
EXTERNAL DEPENDENCIES:
├── erlmcp_metrics_server      ← SLA monitoring reads live metrics
├── erlmcp_registry            ← State lookups, queue depth
├── erlmcp_circuit_breaker     ← Failover time tracking
├── erlmcp_config              ← Runtime limits configuration
├── erlmcp_refusal.hrl         ← Refusal code definitions
├── erlmcp.hrl                 ← Common types/macros
├── erlmcp_receipt_chain       ← Upgrade event logging
└── jsx (JSON), file (I/O)     ← Core libraries

INTERNAL DEPENDENCIES (within subsystem):
erlmcp_pricing_plan
  ├─ erlmcp_plan_loader       (loads JSON)
  └─ erlmcp_pricing_poka_yoke (validates schemas)

erlmcp_pricing_receipt
  ├─ erlmcp_pricing_plan      (get envelope)
  └─ erlmcp_pricing_state     (previous hash chain)

erlmcp_plan_sla_monitor
  ├─ erlmcp_metrics_server    (real-time metrics)
  ├─ erlmcp_pricing_receipt   (log violations)
  └─ erlmcp_plan              (envelope definitions)

erlmcp_pricing_upgrade
  ├─ erlmcp_pricing_state     (track upgrade time)
  ├─ erlmcp_pricing_receipt   (log events)
  └─ erlmcp_metrics           (verify envelope)
```

## 4. Data Flow

### A. Plan Definition to Enforcement

```
1. Load Phase:
   plans/team.plan.json
      │
      ├─ erlmcp_plan_loader:load_plan(team)
      │
      └─> erlmcp_pricing_plan:load_plan(team)
          ├─ Validate schema: shapes/pricing_plan.schema.json
          ├─ Extract envelope bounds
          ├─ Parse refusal behaviors
          └─ {ok, plan_spec}

2. Validation Phase (CI/CD):
   erlmcp_pricing_poka_yoke:validate_plan(PlanMap)
   ├─ Validator 1: Plan schema (required fields)
   ├─ Validator 2: Envelope consistency (realistic bounds)
   ├─ Validator 3: Refusal codes exist in erlmcp_refusal.hrl
   └─ Validator 4: Evidence artifacts defined
      {error, [validation_error]} with hints

3. Runtime Enforcement:
   erlmcp_plan_sla_monitor:check_throughput(team)
      ├─ Get plan envelope: throughput_req_s=450
      ├─ Read live metric: erlmcp_metrics_server:get_metrics()
      └─> {ok, actual_throughput} | {violated, min, actual}
         ├─ On violation: erlmcp_pricing_receipt:add_refusal()
         └─ Alert + log to receipt chain
```

### B. SLA Violation to Refusal Response

```
Live Request:
  Client sends message at 600 req/s (team limit: 450)
    │
    ├─ erlmcp_plan_sla_monitor:check_throughput(team)
    │  └─> {violated, 450, 600}
    │
    ├─ erlmcp_pricing_plan:check_refusal(team, throughput_exceeded)
    │  └─> {ok, #{
    │       http_status => 429,
    │       error_code => "rate_limit_exceeded",
    │       message => "Request rate exceeds tier limit",
    │       retry_after_seconds => 60
    │     }}
    │
    ├─ Create refusal receipt:
    │  erlmcp_pricing_receipt:add_refusal(
    │    ReceiptId, 1001, throughput_exceeded, <<"inbound_message">>
    │  )
    │  └─> Receipt with hash chain updated
    │
    └─> Send 429 response to client (with Retry-After: 60)
```

### C. Upgrade Flow

```
Request: upgrade team → enterprise
│
├─ erlmcp_pricing_upgrade:can_upgrade(team, enterprise) → true
│
├─ erlmcp_pricing_upgrade:simulate_upgrade(team, enterprise)
│  ├─ Load upgrade path: plans/upgrade_team_to_enterprise.json
│  ├─ Validate prerequisites (dry-run):
│  │  ├─ certification_valid ✓
│  │  ├─ infrastructure_headroom ✓
│  │  ├─ clean_receipt_state ✓
│  │  ├─ sla_compliance ✓
│  │  └─ resource_availability ✓
│  └─> {ok, #{simulated => true, will_succeed => true}}
│
├─ erlmcp_pricing_upgrade:apply_upgrade(team, enterprise)
│  ├─ Snapshot system state (for rollback)
│  ├─ Execute migration steps:
│  │  ├─ Pre-checks: flush queue, verify state
│  │  ├─ Upgrade: update limits, expand envelope
│  │  └─ Post-verify: check new envelope metrics
│  ├─ Verify upgrade succeeded:
│  │  └─ erlmcp_pricing_upgrade:verify_upgrade(enterprise)
│  ├─ Log upgrade event:
│  │  └─ erlmcp_pricing_receipt:create_receipt(enterprise, version)
│  └─> {ok, #{upgraded => true, actual_downtime_ms => 150}}
│
└─ State updated:
   erlmcp_pricing_state:set_current_plan(enterprise)
   erlmcp_pricing_state:record_upgrade_timestamp(enterprise)
```

### D. Receipt Chain Verification

```
Audit Request: verify_receipt_chain(team)
│
├─ erlmcp_pricing_receipt:list_receipts(team)
│  └─> [Receipt1, Receipt2, Receipt3, ...] (sorted by timestamp)
│
├─ For each Receipt in chain:
│  ├─ Get stored hash: Receipt.hash_chain.current_hash
│  ├─ Recompute hash: erlmcp_pricing_receipt:compute_hash(Receipt)
│  ├─ Match? → integrity verified
│  └─ Mismatch? → {error, hash_mismatch}
│
├─ Verify linkage:
│  └─ Receipt[i].hash_chain.previous_receipt_hash =:= Receipt[i-1].hash_chain.current_hash
│
└─> {ok, complete} | {error, chain_broken}
```

## 5. Key Patterns

### A. Poka-Yoke (Error-Proofing)

**4-Validator Gate for Plans:**
1. **Schema Validator** - All required fields present (tier, envelope, limits, features, evidence)
2. **Envelope Consistency Validator** - Bounds realistic (throughput < 2x baseline, concurrent < 200K)
3. **Refusal Code Validator** - All codes in refusal_behavior exist in erlmcp_refusal.erl
4. **Evidence Requirement Validator** - SBOM, provenance, chaos, benchmark defined

Each validator returns:
```
{error, [{gate, field, message, hint, line_number}]}
```

**Example Error:**
```erlang
{error, [
  {envelope, concurrent_connections,
    <<"Concurrent connections > 200K (unsupported)">>,
    <<"Reduce concurrent_connections to ≤200K">>,
    11}
]}
```

### B. Hash Chain for Immutability

Receipt structure preserves audit trail:
```erlang
Receipt = #{
  receipt_id => <<"unique-id">>,
  plan_id => team,
  version => <<"0.6.0">>,
  timestamp => <<"2026-01-27T12:34:56Z">>,
  envelope_claim => #{
    throughput_req_s => 450,
    concurrent => 25000,
    queue_depth => 100000,
    latency_p99_ms => 150.0,
    failover_s => 5.0
  },
  refusal_trigger => #{
    code => 1001,
    reason => throughput_exceeded,
    attempted_action => <<"inbound_message">>,
    timestamp => <<"2026-01-27T12:34:57Z">>
  },
  hash_chain => #{
    previous_receipt_hash => <<"abc123...">>,  % SHA-256 of prior receipt
    current_hash => <<"def456...">>" % SHA-256 of immutable fields
  },
  audit_fields => #{
    requestor_id => null,
    machine_id => <<"node@host">>,
    erlang_version => <<"26.0.2">>,
    otp_version => <<"26">>,
    hostname => <<"prod-server-01">>
  }
}
```

**Hash = SHA256(JSON({**
- receipt_id
- plan_id
- version
- timestamp
- envelope_claim
- refusal_trigger (if present)
- audit_fields
- previous_receipt_hash
**}))**

Any tampering → hash mismatch → detected.

### C. Deterministic Plan Envelopes

```erlang
PLAN_ENVELOPES = #{
  team => #{
    min_throughput_req_s => 450,      % Guaranteed minimum
    max_latency_p99_ms => 150,        % Target SLA
    max_failover_s => 5,              % Standalone failover time
    description => "Team plan..."
  },
  enterprise => #{
    min_throughput_req_s => 1500,
    max_latency_p99_ms => 100,
    max_failover_s => 2,              % 3-node cluster
    description => "Enterprise plan..."
  },
  gov => #{
    min_throughput_req_s => 900,
    max_latency_p99_ms => 80,
    max_failover_s => 1,              % High-speed failover
    description => "Gov plan..."
  }
}
```

All limits are **deterministic** (same input → same output).

### D. Safety Gates for Upgrades

Upgrade cannot proceed unless ALL gates pass:

| Gate | Check |
|------|-------|
| `certification_valid` | Current plan certification hasn't expired |
| `infrastructure_headroom` | Cluster can handle target envelope |
| `clean_receipt_state` | No unresolved refusals in receipt chain |
| `sla_compliance` | Current system meets source plan SLA |
| `resource_availability` | Memory, CPU available for expansion |

Example gate result:
```erlang
{passed, #{
  target_throughput => 1500,
  available_throughput => 2000,
  target_connections => 512,
  available_connections => 1024
}}
```

Failure:
```erlang
{failed, "Insufficient infrastructure headroom"}
```

## 6. Integration Points

### A. With erlmcp_metrics_server

```erlang
% SLA monitoring reads live metrics
erlmcp_metrics_server:get_metrics()
  → #{
    message_rate_per_sec => 523.5,
    latency_stats => #{p99 => 145.2},
    current_connections => 18234,
    available_throughput_req_s => 2000
  }

% Plan SLA monitor compares:
plan_envelope.min_throughput_req_s = 450
actual_throughput = 523.5
523.5 >= 450 → {ok, 523.5}
```

### B. With erlmcp_registry

```erlang
% Upgrade safety gate checks queue depth
erlmcp_registry:get_queue_depth()
  → {ok, 512}

% Compares to target envelope
target_queue_depth = 2048
512 =< 2048 → safe to upgrade
```

### C. With erlmcp_pricing_receipt (Evidence)

```erlang
% Upgrade logs to receipt chain
erlmcp_pricing_receipt:add_refusal(
  ReceiptId, 1001, throughput_exceeded, <<"request">>
)
  → {ok, Receipt#{hash_chain => #{current_hash => <<"abc...">>}}}

% Receipt stored for compliance audits
% priv/receipts/team/0.6.0/20260127-123456.receipt.json
```

## 7. What to Preserve in v2

### Core Structures (DO NOT CHANGE)
1. **Plan Envelope Definition** - Throughput, connections, queue depth, latency, failover SLA
   - Already production-validated
   - Real benchmarks: tcp_sustained_25k_1kib, tcp_sustained_100k_1kib

2. **Refusal Behavior Mapping** - Error codes, HTTP status, retry hints
   - Deterministic: same limit exceeded → same response
   - Used by clients for retry logic

3. **Evidence Chain** - SBOM, provenance, chaos, benchmark
   - Compliance requirement (esp. gov tier)
   - Links to real artifacts in docs/

4. **Receipt Hash Chain** - SHA-256 continuity for audit trail
   - Immutability foundation
   - Already tested for tampering detection

5. **SLA Envelope Definitions** - Team/enterprise/gov specific metrics
   - Derived from real workload testing
   - Keep baseline values

### Patterns Worth Preserving
1. **4-Layer Validation (Poka-Yoke)**
   - Schema → Consistency → Codes → Evidence
   - Error hints guide remediation

2. **Safety Gates for Upgrades**
   - certification_valid, infrastructure_headroom, clean_receipt_state, sla_compliance, resource_availability
   - Prevents breaking changes

3. **Deterministic Limits**
   - No randomness, no environment-dependent behavior
   - Same plan → same envelope always

4. **Role-Based Evidence**
   - team: basic (sbom, provenance, chaos, benchmark)
   - enterprise: above + audit_schema
   - gov: above + fips_certification, compliance_report

### Optional Optimizations for v2
1. **Plan Caching** - Load once, cache in memory (read-heavy)
2. **Metrics Batching** - Collect violations in windows before alerting
3. **Receipt Rotation** - Archive old receipts, compress, retire to cold storage
4. **SLA Dashboard** - WebSocket real-time updates instead of polling
5. **Upgrade Dry-Run** - Parallel execution to predict downtime more accurately
6. **Compliance Reports** - Auto-generate PDF summaries from receipt chain

## 8. File Quick Reference

| File | Purpose | Key Functions |
|------|---------|---|
| `src/erlmcp_plan_loader.erl` | Load plans from JSON | `load_plan/1`, `load_plan_file/1` |
| `src/erlmcp_pricing_plan.erl` | Deterministic validators | `load_plan/1`, `get_envelope/1`, `check_refusal/2`, `validate_plan/1` |
| `src/erlmcp_plan.erl` | Legacy simple API | `current_plan/0`, `get_limits/1`, `verify_sla/1` |
| `src/erlmcp_pricing_poka_yoke.erl` | CI/CD quality gates | `validate_plan/1`, `validate_envelope_consistency/1`, `validate_refusal_codes_exist/1`, `validate_evidence_requirements/1` |
| `src/erlmcp_pricing_receipt.erl` | Immutable audit trail | `create_receipt/2-3`, `add_refusal/3-4`, `verify_receipt/1`, `verify_receipt_chain/1`, `export_receipt/2-3` |
| `src/erlmcp_pricing_state.erl` | Runtime state (ETS) | `get_current_plan/0`, `set_current_plan/1`, `get_last_upgrade_time/1` |
| `src/erlmcp_plan_sla_monitor.erl` | SLA enforcement (gen_server) | `monitor_envelope/2`, `check_throughput/1`, `check_latency/1`, `get_sla_status/1` |
| `src/erlmcp_plan_sla_monitor_extended.erl` | Extended SLA features | Continuous monitoring, trend analysis |
| `src/erlmcp_sla_continuous_monitor.erl` | Background SLA monitoring | Persistent monitoring across restarts |
| `src/erlmcp_pricing_upgrade.erl` | Plan migrations | `can_upgrade/2`, `apply_upgrade/2`, `verify_upgrade/1`, `get_upgrade_path/2`, `check_upgrade_cooldown/1` |
| `src/erlmcp_refusal_plan_validator.erl` | Refusal validation | Refusal code range validation |
| `src/erlmcp_chaos_plan_validator.erl` | Failure injection | Bounded refusal frame validation, recovery time checks |
| `src/erlmcp_bench_plan_validator.erl` | Performance benchmarking | Envelope metric verification, regression detection |
| `src/erlmcp_plan_cli.erl` | Command-line interface | `plan show`, `plan list`, `plan validate`, `upgrade simulate`, `upgrade apply` |
| `src/erlmcp_plan_docs_generator.erl` | Documentation generation | Markdown docs, portal HTML, plan comparisons |
| `src/erlmcp_sla_http_handler.erl` | HTTP endpoints | `/sla/status/<plan>`, `/sla/dashboard/<plan>`, `/sla/export/<plan>` |
| `src/erlmcp_sla_dashboard_handler.erl` | Dashboard handler | Real-time compliance visualization |
| `plans/team.plan.json` | Team tier specification | 450 req/s, 25K conns, p99 ≤150ms |
| `plans/enterprise.plan.json` | Enterprise tier specification | 1500 req/s, 100K conns, p99 ≤100ms |
| `plans/gov.plan.json` | Government tier specification | 900 req/s, 256 conns, p99 ≤80ms, full audit |
| `shapes/pricing_plan.schema.json` | Plan JSON schema | Envelope, limits, features, refusal, evidence, compliance definitions |
| `shapes/pricing_receipt.schema.json` | Receipt JSON schema | Receipt structure, hash chain, audit fields |
| `templates/pricing_portal.html` | Pricing portal UI | Plan comparison, feature matrix |

## 9. Testing Strategy

### Unit Tests (EUnit)
- `test/erlmcp_pricing_plan_SUITE.erl` - Plan loading/validation
- `test/erlmcp_pricing_poka_yoke_SUITE.erl` - Poka-yoke validators
- `test/erlmcp_pricing_receipt_*.erl` - Receipt creation/verification
- `test/erlmcp_plan_sla_monitor_*SUITE.erl` - SLA violation detection
- `test/erlmcp_plan_conformance_*SUITE.erl` - Envelope conformance
- `test/erlmcp_pricing_upgrade_extended_SUITE.erl` - Upgrade paths

### Compliance Tests
- `test/erlmcp_pricing_docs_SUITE.erl` - Documentation generation
- `test/metrology/plan_spec_conformance_tests.erl` - Metrology compliance

### Integration Tests
- `test/erlmcp_pricing_receipt_extended_SUITE.erl` - Full receipt chain
- `test/erlmcp_plan_sla_monitor_extended_SUITE.erl` - Continuous monitoring

### Performance Benchmarks
- `bench/erlmcp_bench_plan_validator.erl` - Plan validator performance
- Envelope conformance benchmarks in suite tests

## 10. Known Limitations & Future Work

### Current Limitations
1. **Single-Node Failover** - Team tier failover SLA (5s) assumes single node; no automatic failover
2. **Manual Downgrade Prevention** - Downgrades forbidden but not enforced at application level
3. **Upgrade Cooldown** - Fixed per plan; no dynamic adjustment
4. **Receipt Storage** - Local filesystem; not replicated across cluster
5. **Metrics Integration** - Tight coupling to erlmcp_metrics_server; would benefit from abstraction

### Future Work (v0.7.0+)
1. **Distributed Receipt Chain** - Replicate receipts to multiple nodes
2. **Metrics Abstraction** - Support multiple metrics backends (Prometheus, CloudWatch, etc.)
3. **Plan A/B Testing** - Shadow envelope for canary deployments
4. **Dynamic Limits** - Time-based envelope changes (e.g., burst limits)
5. **Compliance Automation** - Auto-generate compliance reports from receipt chain
6. **Predictive Scaling** - Use trend analysis to warn before envelope violations

---

**Version:** 0.6.0
**Stability:** Production
**Last Updated:** 2026-01-27
**Author:** erlmcp research agent
