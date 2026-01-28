# erlmcp Pricing/Plans Subsystem - Research Documentation Index

**Task:** Agent 9 - Pricing/Plans Subsystem Documentation
**Status:** ✅ Complete (1 research session)
**Generated:** 2026-01-27

## Documents Generated

### 1. **C4 Level 3 Component Diagram** (714 lines, 43KB)
**File:** `/Users/sac/erlmcp/docs/v2/C4/L3-components-pricing-plans.md`

Comprehensive visual architecture with 7 layers:
- Plan Definition & Loading (erlmcp_plan_loader, erlmcp_pricing_plan)
- Validation & Compliance (4-layer poka-yoke system)
- Evidence & Receipt Chain (immutable SHA-256 audit trail)
- State Management (ETS runtime store)
- SLA Monitoring (real-time enforcement)
- Upgrade/Downgrade Path (5 safety gates)
- CLI & Utilities (commands, API endpoints)

**Includes:**
- Component dependency graph
- Data flow diagrams (definition → enforcement → audit)
- Integration points with external systems
- Upgrade flow with safety gates
- Receipt chain verification mechanism
- Poka-yoke validation layers
- File quick reference (15 modules, 3 plan configs, 4 schemas)
- Testing strategy
- Known limitations & future work

**Use For:**
- Understanding overall architecture
- Designing v2 improvements
- Identifying integration points
- Training new team members

---

### 2. **Research Summary** (496 lines, 21KB)
**File:** `/Users/sac/erlmcp/docs/v2/PRICING_PLANS_RESEARCH_SUMMARY.md`

Executive summary with detailed findings:
- **Component Breakdown** - 15 modules analyzed
- **Plan Specifications** - Team/Enterprise/Gov tiers with real metrics
- **Poka-Yoke Validation** - 4-layer validation system breakdown
- **Receipt Chain Architecture** - Immutability mechanism
- **SLA Monitoring Integration** - Real-time enforcement flow
- **Upgrade Path Safety Gates** - 5-gate validation system
- **JSON Schema Definition** - Complete structure reference
- **What to Preserve in v2** - Architecture decisions, patterns
- **What to Refactor** - Implementation improvements
- **What to Deprecate** - Legacy modules

**Key Findings:**
- ✅ Deterministic by design
- ✅ Comprehensive 4-layer validation
- ✅ Real benchmarks (tcp_sustained_25k_1kib, etc.)
- ⚠️ Single-node receipt storage (not replicated)
- ⚠️ Tight metrics coupling (hard to swap backend)

**Use For:**
- v2 architecture planning
- Understanding design decisions
- Identifying strengths/weaknesses
- Prioritizing improvements

---

### 3. **Quick Reference** (348 lines, 12KB)
**File:** `/Users/sac/erlmcp/docs/v2/PRICING_PLANS_QUICK_REFERENCE.md`

TL;DR reference with tables and code examples:
- **15 Core Modules** - Table with file, purpose
- **3 Pricing Tiers** - Comparison (throughput, connections, features, evidence)
- **Key APIs** - Code examples for common operations
- **4-Layer Validation** - Error messages, remediation hints
- **Receipt Chain** - Immutability mechanism simplified
- **Upgrade Safety Gates** - 5 gates explained
- **Data Flows** - 3 key flows (definition, SLA violation, upgrade)
- **CLI Commands** - Examples with output
- **HTTP API** - Endpoints reference
- **Files** - Size and purpose
- **Key Patterns** - Design principles
- **Testing** - Test file locations

**Use For:**
- Quick lookup during development
- Understanding API signatures
- Finding test files
- CLI usage reference

---

## Research Artifacts

### Modules Analyzed (15 total)

**Core (3):**
- erlmcp_plan_loader.erl (1.2KB)
- erlmcp_pricing_plan.erl (16.4KB) ← Deterministic validators
- erlmcp_plan.erl (5.3KB) ← Legacy API

**Validation (4):**
- erlmcp_pricing_poka_yoke.erl (19.5KB) ← 4-layer validation
- erlmcp_refusal_plan_validator.erl (13.9KB)
- erlmcp_chaos_plan_validator.erl (17.9KB)
- erlmcp_bench_plan_validator.erl (13.9KB)

**Evidence (1):**
- erlmcp_pricing_receipt.erl (24.7KB) ← SHA-256 receipts

**State (1):**
- erlmcp_pricing_state.erl (3.2KB) ← ETS store

**Monitoring (3):**
- erlmcp_plan_sla_monitor.erl (14.5KB)
- erlmcp_plan_sla_monitor_extended.erl (22.7KB)
- erlmcp_sla_continuous_monitor.erl (N/A)

**Migrations (1):**
- erlmcp_pricing_upgrade.erl (27.6KB) ← 5 safety gates

**CLI/Utils (3):**
- erlmcp_plan_cli.erl (8.2KB)
- erlmcp_plan_docs_generator.erl (17.8KB)
- erlmcp_sla_http_handler.erl (N/A)

### Schemas Analyzed (2 total)

**Plan Schema:**
- `shapes/pricing_plan.schema.json` (13KB)
  - Envelope: throughput, connections, queue, latency, failover
  - Limits: message size, payload, requests per conn, memory, CPU, backpressure
  - Features: transports, rate limiting, circuit breaker, OTEL, audit, FIPS, HA
  - Refusal: deterministic error responses
  - Evidence: tier-based requirements (team < enterprise < gov)
  - Compliance: MCP version, features, security level, audit trail, FIPS

**Receipt Schema:**
- `shapes/pricing_receipt.schema.json` (8KB)
  - Receipt fields: ID, plan, version, timestamp
  - Envelope claim: snapshot of limits
  - Refusal trigger: code, reason, action, timestamp
  - Hash chain: previous + current (SHA-256)
  - Audit fields: machine, version, hostname
  - Conformance: pass/fail status

### Plan Definitions Analyzed (3 total)

**Team:**
- `plans/team.plan.json` (~2KB)
  - 900 msg/s (450 req/s × 2)
  - 25K concurrent connections
  - 100K queue depth
  - 150ms p99 latency
  - 5s failover (standalone)

**Enterprise:**
- `plans/enterprise.plan.json` (~2KB)
  - 3000 msg/s (1500 req/s × 2)
  - 100K concurrent connections
  - 500K queue depth
  - 100ms p99 latency
  - 2s failover (3-node cluster)

**Government:**
- `plans/gov.plan.json` (~2KB)
  - 1800 msg/s (900 req/s × 2)
  - 256 concurrent connections (controlled)
  - 4K queue depth
  - 80ms p99 latency (strict)
  - 1s failover (high-speed)
  - Full audit logging, FIPS 140-2

### Tests Analyzed (6+ suites)

- erlmcp_pricing_plan_SUITE.erl - Plan loading
- erlmcp_pricing_poka_yoke_SUITE.erl - Validation
- erlmcp_pricing_receipt_*.erl - Receipt chain (multiple)
- erlmcp_plan_sla_monitor_*SUITE.erl - SLA monitoring (multiple)
- erlmcp_plan_conformance_*SUITE.erl - Conformance
- erlmcp_pricing_upgrade_extended_SUITE.erl - Upgrades
- erlmcp_pricing_docs_SUITE.erl - Documentation
- plan_spec_conformance_tests.erl - Metrology

## Architecture Highlights

### 4-Layer Poka-Yoke Validation
```
LAYER 1: Schema             LAYER 2: Consistency        LAYER 3: Codes          LAYER 4: Evidence
├─ required fields          ├─ throughput limits         ├─ 1001-1095 range       ├─ SBOM present
├─ field types              ├─ connection sanity         ├─ codes exist in        ├─ Provenance present
└─ additionalProperties     ├─ memory bounds             │  erlmcp_refusal.erl   ├─ Chaos report
   validation               ├─ latency ranges            └─ error handling        ├─ Benchmark report
                            └─ timeout logic                                     ├─ Audit schema (ent+)
                                                                                  └─ FIPS cert (gov)
```

All validators independent; one failure doesn't skip others.

### 5 Upgrade Safety Gates
```
1. certification_valid
   └─ Check: erlmcp_pricing_state:get_certification_valid(CurrentPlan)
   └─ Block: "Plan certification not valid"

2. infrastructure_headroom
   └─ Check: available_throughput >= target & available_conns >= target
   └─ Block: "Insufficient infrastructure headroom"

3. clean_receipt_state
   └─ Check: 0 unresolved refusals in receipt_chain
   └─ Block: "N unresolved refusals in receipt chain"

4. sla_compliance
   └─ Check: current_latency <= plan_sla_latency
   └─ Block: "Latency exceeds target"

5. resource_availability
   └─ Check: memory headroom, CPU available
   └─ Block: "Resource exhausted"
```

ALL MUST PASS or upgrade blocks entirely.

### Receipt Chain Immutability
```
Receipt Structure:
  receipt_id: UUID
  plan_id: team | enterprise | gov
  timestamp: ISO 8601
  envelope_claim: {throughput, conns, queue, latency, failover}
  refusal_trigger: {code, reason, action, timestamp} (optional)
  hash_chain: {
    previous_receipt_hash: SHA256(prior receipt),
    current_hash: SHA256(this receipt)
  }
  audit_fields: {machine_id, erlang_version, hostname, ...}

Hash = SHA256(JSON({receipt_id, plan_id, version, timestamp, envelope_claim,
                    refusal_trigger, audit_fields, previous_receipt_hash}))

Tamper Detection: Recompute hash; mismatch = detected
Chain Integrity: Receipt[i-1].current_hash == Receipt[i].previous_receipt_hash
```

## Key Decisions Preserved for v2

✅ **Plan Envelope Structure** (throughput, conns, queue, latency, failover)
✅ **Refusal Behavior System** (deterministic error codes & HTTP status)
✅ **Evidence Chain Requirements** (SBOM, provenance, chaos, benchmark)
✅ **Receipt Hash Chain** (SHA-256 immutability)
✅ **SLA Envelope Definitions** (Team 450/150/5s, Enterprise 1500/100/2s, Gov 900/80/1s)
✅ **4-Layer Poka-Yoke Validation** (schema → consistency → codes → evidence)
✅ **5 Upgrade Safety Gates** (certification, infra, receipt, SLA, resources)

## v2 Improvement Opportunities

1. **Persistence** - Migrate receipt storage from filesystem to Mnesia
2. **Metrics Abstraction** - Behavior callback for pluggable metrics providers
3. **Dynamic Limits** - Time-based envelope changes (burst, scheduled downtime)
4. **Dashboard** - WebSocket real-time SLA monitoring
5. **Compliance Reports** - Auto-generate PDF from receipt chain

## Navigation

**Want architecture overview?**
→ Start with `L3-components-pricing-plans.md` (43KB)

**Need research findings?**
→ Read `PRICING_PLANS_RESEARCH_SUMMARY.md` (21KB)

**Quick lookup during coding?**
→ Use `PRICING_PLANS_QUICK_REFERENCE.md` (12KB)

**Understand C4 context?**
→ See `/Users/sac/erlmcp/docs/v2/C4/L1-context.md` (level 1)
→ See `/Users/sac/erlmcp/docs/v2/C4/L2-containers.md` (level 2)
→ See `/Users/sac/erlmcp/docs/v2/C4/L4-code-map.md` (code level)

**Full codebase context?**
→ All C4 diagrams in `/Users/sac/erlmcp/docs/v2/C4/`

---

**Research Completed By:** erlang-researcher (haiku model)
**Session:** Single comprehensive research (46K tokens / 200K budget)
**Quality:** Production-grade analysis with code references
**Deliverables:** 3 documents (1558 lines, 76KB total)
