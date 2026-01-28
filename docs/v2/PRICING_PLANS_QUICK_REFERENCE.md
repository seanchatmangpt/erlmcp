# erlmcp Pricing/Plans - Quick Reference

**TL;DR:** Production pricing/plans system with 3 tiers, deterministic enforcement, immutable audit trail, and controlled upgrades.

## 15 Core Modules

### Plan Definition & Loading (3)
| Module | File | Purpose |
|--------|------|---------|
| `erlmcp_plan_loader` | `src/erlmcp_plan_loader.erl` | Load team/enterprise/gov specs from JSON |
| `erlmcp_pricing_plan` | `src/erlmcp_pricing_plan.erl` | Deterministic validators (load, envelope, refusal, validate) |
| `erlmcp_plan` | `src/erlmcp_plan.erl` | Legacy simple API (current_plan, get_limits, verify_sla) |

### Validation & Compliance (4)
| Module | File | Purpose |
|--------|------|---------|
| `erlmcp_pricing_poka_yoke` | `src/erlmcp_pricing_poka_yoke.erl` | 4-layer CI/CD quality gates (schema → consistency → codes → evidence) |
| `erlmcp_refusal_plan_validator` | `src/erlmcp_refusal_plan_validator.erl` | Refusal code validation (1001-1095 range) |
| `erlmcp_chaos_plan_validator` | `src/erlmcp_chaos_plan_validator.erl` | Failure injection (bounded refusal frames, recovery < 5s) |
| `erlmcp_bench_plan_validator` | `src/erlmcp_bench_plan_validator.erl` | Performance benchmarking (envelope verification, <10% regression) |

### Evidence & Audit Trail (1)
| Module | File | Purpose |
|--------|------|---------|
| `erlmcp_pricing_receipt` | `src/erlmcp_pricing_receipt.erl` | Immutable receipt chain with SHA-256 hash continuity |

### State Management (1)
| Module | File | Purpose |
|--------|------|---------|
| `erlmcp_pricing_state` | `src/erlmcp_pricing_state.erl` | ETS runtime state (current_plan, upgrade_time, certification_valid) |

### SLA Monitoring (3)
| Module | File | Purpose |
|--------|------|---------|
| `erlmcp_plan_sla_monitor` | `src/erlmcp_plan_sla_monitor.erl` | gen_server SLA enforcement (check throughput/latency/failover) |
| `erlmcp_plan_sla_monitor_extended` | `src/erlmcp_plan_sla_monitor_extended.erl` | Advanced monitoring (trend analysis, predictive alerts) |
| `erlmcp_sla_continuous_monitor` | `src/erlmcp_sla_continuous_monitor.erl` | Background continuous monitoring |

### Plan Migrations (1)
| Module | File | Purpose |
|--------|------|---------|
| `erlmcp_pricing_upgrade` | `src/erlmcp_pricing_upgrade.erl` | Controlled upgrades with 5 safety gates (certification, infra, receipt, SLA, resources) |

### CLI & Utilities (3)
| Module | File | Purpose |
|--------|------|---------|
| `erlmcp_plan_cli` | `src/erlmcp_plan_cli.erl` | CLI commands (show, list, validate, upgrade) |
| `erlmcp_plan_docs_generator` | `src/erlmcp_plan_docs_generator.erl` | Auto-generate Markdown docs, portal HTML |
| `erlmcp_sla_http_handler` | `src/erlmcp_sla_http_handler.erl` | HTTP endpoints (/sla/status, /sla/dashboard, /sla/export) |

## 3 Pricing Tiers

| Tier | Throughput | Connections | Queue | Latency p99 | Failover | Features | Evidence |
|------|-----------|-------------|-------|-------------|----------|----------|----------|
| **team** | 450 req/s | 25K | 100K | 150ms | 5s | Basic transports, rate limiting, circuit breaker | sbom, provenance, chaos, benchmark |
| **enterprise** | 1500 req/s | 100K | 500K | 100ms | 2s | Above + pooling, audit logging, HA, comprehensive OTEL | Above + audit_schema |
| **gov** | 900 req/s | 256 | 4K | 80ms | 1s | Above + FIPS 140-2, immutable audit logs | Above + fips_certification, compliance_report |

**Files:**
- `plans/team.plan.json` (900 msg/s with detailed workload definition)
- `plans/enterprise.plan.json` (3000 msg/s with HA details)
- `plans/gov.plan.json` (1800 msg/s with compliance)

## Key APIs

### Load & Validate Plans
```erlang
% Load plan
{ok, Spec} = erlmcp_pricing_plan:load_plan(team)

% Get envelope bounds
{ok, Envelope} = erlmcp_pricing_plan:get_envelope(team)
% #{throughput_req_s => 450, concurrent_connections => 25000, ...}

% Check refusal response
{ok, Refusal} = erlmcp_pricing_plan:check_refusal(team, throughput_exceeded)
% #{http_status => 429, error_code => "rate_limit_exceeded", ...}

% Validate plan (CI/CD)
ok = erlmcp_pricing_plan:validate_plan(team)
{error, [{envelope, concurrent_connections, "...", "hint", 11}]} % validation_error
```

### Check SLA Compliance (Runtime)
```erlang
% Start monitoring
erlmcp_plan_sla_monitor:start_link()

% Check if throughput meets plan envelope
{ok, 523.5} = erlmcp_plan_sla_monitor:check_throughput(team)  % >= 450 req/s
{violated, 450, 523.5} = erlmcp_plan_sla_monitor:check_latency(team)  % 523 > 150ms limit

% Get dashboard data
Dashboard = erlmcp_plan_sla_monitor:get_sla_dashboard(team)
% #{plan => team, overall_status => 'PASS', throughput => #{...}, latency => #{...}}
```

### Evidence & Audit Trail
```erlang
% Create receipt (snapshot envelope bounds)
{ok, Receipt} = erlmcp_pricing_receipt:create_receipt(team, <<"0.6.0">>)

% Log refusal event
{ok, UpdatedReceipt} = erlmcp_pricing_receipt:add_refusal(
  ReceiptId, 1001, throughput_exceeded, <<"inbound_message">>
)

% Verify receipt integrity (hash check)
{ok, verified} = erlmcp_pricing_receipt:verify_receipt(ReceiptId)

% Export for compliance
{ok, JSON} = erlmcp_pricing_receipt:export_receipt(ReceiptId, json)
{ok, CSV} = erlmcp_pricing_receipt:export_receipt(ReceiptId, csv)
```

### Controlled Upgrades
```erlang
% Check if upgrade allowed
true = erlmcp_pricing_upgrade:can_upgrade(team, enterprise)
false = erlmcp_pricing_upgrade:can_upgrade(enterprise, team)  % Downgrade forbidden

% List possible upgrades
[enterprise] = erlmcp_pricing_upgrade:list_possible_upgrades(team)

% Simulate upgrade (dry-run)
{ok, #{simulated => true, will_succeed => true}} =
  erlmcp_pricing_upgrade:simulate_upgrade(team, enterprise)

% Apply upgrade (with 5 safety gates)
{ok, #{upgraded => true, actual_downtime_ms => 150}} =
  erlmcp_pricing_upgrade:apply_upgrade(team, enterprise)

% Check cooldown
true = erlmcp_pricing_upgrade:check_upgrade_cooldown(enterprise)
```

### State Management
```erlang
% Get/set current plan
{ok, team} = erlmcp_pricing_state:get_current_plan()
ok = erlmcp_pricing_state:set_current_plan(enterprise)

% Track upgrade time
1674841496 = erlmcp_pricing_state:get_last_upgrade_time(enterprise)
ok = erlmcp_pricing_state:set_last_upgrade_time(enterprise, 1674841496)
```

## 4-Layer Poka-Yoke Validation

```
LAYER 1: Schema Validation
├─ Required: tier, name, description, pricing, envelope, limits, features,
│            refusal_behavior, evidence, compliance
└─ Error: "Required field missing: tier"

LAYER 2: Envelope Consistency
├─ concurrent_connections ≤ 200K (hardware limit)
├─ throughput ≤ 2× baseline (math limit)
├─ queue_depth × msg_size ≤ 100GB (memory limit)
├─ p99_latency: 10ms ≤ X ≤ 60s (realistic range)
├─ failover_sla: 1s ≤ X ≤ 300s
└─ connection_timeout ≥ failover_sla
Error: "Concurrent connections > 200K (unsupported)"

LAYER 3: Refusal Code Validation
├─ All codes in refusal_behavior exist in erlmcp_refusal.erl
├─ Range: 1001-1095
└─ Error: "Error code 9999 does not exist in erlmcp_refusal.hrl"

LAYER 4: Evidence Requirement Validation
├─ Team: sbom, provenance, chaos_report, benchmark_report
├─ Enterprise: ↑ + audit_schema
├─ Gov: ↑ + fips_certification, compliance_report
└─ Error: "Gov tier missing required compliance evidence"

Output: {error, [{gate, field, message, remediation_hint, line}]}
```

## Receipt Chain (Immutability)

```
Receipt = {
  receipt_id: "abc-123",
  plan_id: team,
  timestamp: "2026-01-27T12:34:56Z",
  envelope_claim: {throughput: 450, ...},
  refusal_trigger: {code: 1001, reason: throughput_exceeded, ...},
  hash_chain: {
    previous_receipt_hash: "xyz789...",  ← SHA256(prior receipt)
    current_hash: "abc456..."            ← SHA256(this receipt)
  },
  audit_fields: {machine_id, erlang_version, hostname, ...}
}

Hash = SHA256(JSON({receipt_id, plan_id, version, timestamp, envelope_claim,
                    refusal_trigger, audit_fields, previous_receipt_hash}))

Verification: Recompute hash; if mismatch → tampered
Chain Integrity: Receipt[i-1].current_hash =:= Receipt[i].previous_receipt_hash
```

## Upgrade Safety Gates (All 5 Must Pass)

| Gate | Check | Fail Reason |
|------|-------|-------------|
| `certification_valid` | erlmcp_pricing_state:get_certification_valid(CurrentPlan) | Cert expired |
| `infrastructure_headroom` | available_throughput ≥ target_throughput AND available_conns ≥ target_conns | Not enough capacity |
| `clean_receipt_state` | 0 unresolved refusals in receipt_chain | Pending violations |
| `sla_compliance` | current_latency ≤ plan_sla_latency | SLA not met now |
| `resource_availability` | memory headroom, CPU available | Resource exhausted |

**Failure blocks upgrade entirely; all must pass for migration.**

## Data Flows

### 1. Plan Definition → Enforcement
```
plans/team.plan.json
  ↓
erlmcp_plan_loader:load_plan(team)
  ↓
erlmcp_pricing_plan:validate_plan(team)  ← 4-layer validation
  ↓
erlmcp_pricing_poka_yoke:validate_plan()  ← CI/CD gate
  ↓
{ok, plan_spec} ready for runtime
```

### 2. SLA Violation → Refusal Response
```
Client: 600 req/s (team limit: 450)
  ↓
erlmcp_plan_sla_monitor:check_throughput(team)
  ↓
{violated, 450, 600}
  ↓
erlmcp_pricing_plan:check_refusal(team, throughput_exceeded)
  ↓
{ok, #{http_status => 429, message => "Rate limit exceeded"}}
  ↓
erlmcp_pricing_receipt:add_refusal(...)  ← Log to audit trail
  ↓
Response: HTTP 429 + Retry-After: 60s
```

### 3. Upgrade Flow
```
request upgrade(team → enterprise)
  ↓
erlmcp_pricing_upgrade:can_upgrade(team, enterprise) → true
  ↓
erlmcp_pricing_upgrade:simulate_upgrade(...)  ← Dry-run
  ↓
erlmcp_pricing_upgrade:apply_upgrade(...)
  ├─ Validate 5 safety gates
  ├─ Snapshot system state
  ├─ Execute migration steps
  ├─ Verify envelope
  └─ Log upgrade event
  ↓
erlmcp_pricing_state:set_current_plan(enterprise)
  ↓
{ok, upgraded}
```

## CLI Commands

```bash
# Show plan specification
rebar3 shell
> erlmcp_plan_cli:show_plan(team).
#{tier => team, envelope => #{...}, ...}

# List all plans
> erlmcp_plan_cli:list_plans().
[team, enterprise, gov]

# Validate plan file
> erlmcp_plan_cli:validate_file("plans/team.plan.json").
ok

# Upgrade simulation
> erlmcp_plan_cli:simulate_upgrade(team, enterprise).
#{simulated => true, will_succeed => true}

# Upgrade application
> erlmcp_plan_cli:upgrade(team, enterprise).
#{upgraded => true, actual_downtime_ms => 150}
```

## HTTP API

```
GET /sla/status/{plan}
  → {plan: "team", overall_status: "PASS", throughput: {...}, latency: {...}}

GET /sla/dashboard/{plan}
  → Dashboard with compliance status, violations, SLA window

POST /sla/export/{plan}?format=json|csv|tsv
  → Export metrics for compliance audit
```

## Files

| File | Size | Purpose |
|------|------|---------|
| `src/erlmcp_pricing_plan.erl` | 16K | Core deterministic validators |
| `src/erlmcp_pricing_receipt.erl` | 25K | Receipt chain implementation |
| `src/erlmcp_pricing_upgrade.erl` | 28K | Upgrade path management |
| `src/erlmcp_plan_sla_monitor.erl` | 14K | SLA enforcement gen_server |
| `src/erlmcp_pricing_poka_yoke.erl` | 20K | 4-layer validation |
| `plans/team.plan.json` | ~2K | Team tier spec |
| `plans/enterprise.plan.json` | ~2K | Enterprise tier spec |
| `plans/gov.plan.json` | ~2K | Government tier spec |
| `shapes/pricing_plan.schema.json` | 13K | JSON Schema v7 |
| `shapes/pricing_receipt.schema.json` | 8K | Receipt schema |

**Total:** ~155K of production pricing/plans subsystem

## Key Patterns

1. **Deterministic by Design** - Same plan always produces same envelope
2. **Schema Validation First** - JSON schemas gate all plan mutations
3. **Poka-Yoke Layers** - 4 independent validators catch different error classes
4. **Hash Chain Immutability** - SHA-256 prevents tampering
5. **Safety-First Upgrades** - 5 gates, all must pass
6. **Role-Based Evidence** - Team (basic), Enterprise (audit), Gov (compliance)

## Testing

- `test/erlmcp_pricing_plan_SUITE.erl` - Plan loading/validation
- `test/erlmcp_pricing_poka_yoke_SUITE.erl` - Validator gates
- `test/erlmcp_pricing_receipt_*.erl` - Receipt chain
- `test/erlmcp_plan_sla_monitor_*SUITE.erl` - SLA monitoring
- `test/erlmcp_pricing_upgrade_extended_SUITE.erl` - Upgrade paths
- Target: 90%+ coverage (unit), 85%+ (integration), 100% (compliance)

## Status

- **Version:** 0.6.0
- **Stability:** Production
- **Maturity:** High (comprehensive validation, real benchmarks, audit trail)
- **Next:** v0.7.0 - Persistent receipt storage, metrics abstraction, dashboard upgrades

---

See `/Users/sac/erlmcp/docs/v2/C4/L3-components-pricing-plans.md` for full architecture.
