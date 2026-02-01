# erlmcp-flow Architecture Index & Navigation

**Status**: FINAL | **Version**: 1.0.0 | **Date**: 2026-02-01

---

## Overview

This index provides navigation through the complete erlmcp-flow architecture documentation. erlmcp-flow is a multi-agent orchestration system built on erlmcp, comprising **4 subsystems** across **4 OTP applications** with **32+ modules**, **90+ test suites**, and **comprehensive security, learning, and coordination capabilities**.

---

## 📚 Documentation Structure

### Core Architecture Documents

#### 1. **Main Architecture Design** (PRIMARY READ)
**File**: `/home/user/erlmcp/docs/erlmcp-flow-architecture-design.md` (70+ pages)

**Contents**:
- Executive summary
- 4 subsystems detailed (Agent Foundation, Coordination, Intelligence, Security)
- Core APIs with type signatures
- Inter-subsystem communication protocols
- 20+ invariants and safety properties
- 19 failure modes with recovery procedures
- 7 Architecture Decision Records (ADRs)
- Testing strategy and quality gates
- Deployment architecture (cloud + local)
- Monitoring and observability strategy

**When to Read**: Start here for complete understanding.

---

#### 2. **Quick Reference Guide** (5-MINUTE READ)
**File**: `/home/user/erlmcp/docs/erlmcp-flow-quick-reference.md` (15 pages)

**Contents**:
- 4 subsystems at a glance
- Key functions per subsystem (with signatures)
- Data structures (type records)
- Supervision trees (ASCII art)
- Message flow examples
- Failure scenario quick table
- Testing quick start (command cheat sheet)
- Key invariants (reminder)
- Deployment checklist
- Performance targets (SLOs)
- Emergency procedures
- Useful commands

**When to Read**: Quick lookup, before meetings, during development.

---

#### 3. **C4 Architecture Diagrams** (VISUAL READ)
**File**: `/home/user/erlmcp/docs/erlmcp-flow-c4-architecture.md` (20 pages, Mermaid diagrams)

**Contents**:
- C1: System Context (user, flow, LLM, monitoring)
- C2: Container Architecture (4 apps + storage)
- C3: Agent Foundation components
- C4: Coordination components
- C4: Intelligence components
- C4: Security components
- Subsystem integration diagram
- Failure recovery workflows (visual)
- Technology stack
- Deployment architecture (Kubernetes)

**When to Read**: Visual learners, architecture reviews, presentations.

---

#### 4. **Implementation Guide** (DETAILED TECHNICAL)
**File**: `/home/user/erlmcp/docs/erlmcp-flow-implementation-guide.md` (40 pages)

**Contents**:
- Module implementation roadmap (32 modules mapped)
- Phase 1: Agent Foundation (8 modules, Week 1-2)
- Phase 2: Coordination (8 modules, Week 3-4)
- Phase 3: Intelligence (8 modules, Week 5-6)
- Phase 4: Security (8 modules, Week 7-8)
- Quality gates (compilation, tests, coverage, types, xref, format)
- Documentation requirements (per subsystem, per module, global)
- 10-week implementation schedule
- Success metrics
- File paths for each module to create

**When to Read**: Implementation planning, developer onboarding, sprint planning.

---

### Architecture Decision Records (ADRs)

#### ADR-001: Agent Spawn Synchronicity
**File**: `/home/user/erlmcp/docs/adr/ADR-001-agent-spawn-synchronicity.md` (10 pages)

**Decision**: Spawn is synchronous, returns {ok, agent()} immediately.

**Rationale**:
- Caller needs PID immediately for routing
- No race conditions (registry ready before return)
- Simpler API (no result channels)
- Fail-fast principle

**Alternatives Rejected**:
- Async spawn (complexity)
- Lazy spawn (unpredictable latency)

**Testing**: 5+ test suites included

---

#### ADR-002–ADR-007 (Placeholders in architecture doc)

**Included in**: `/home/user/erlmcp/docs/erlmcp-flow-architecture-design.md` (Part VII)

- ADR-002: Topology Redundancy via Multiple Paths
- ADR-003: Routing via Learned Models vs Rule-Based
- ADR-004: Immutable Audit Log
- ADR-005: PII Detection Strategy
- ADR-006: Learning Consolidation (SONA + EWC++)
- ADR-007: Vector Embedding Backend (ONNX + Poincaré)

---

## 🗂️ Mapping: Subsystems → Apps → Modules

### Subsystem 1: Agent Foundation (erlmcp_core)

**Responsibility**: Spawn, manage, supervise agents.

**Modules to Implement**:
| Module | Type | Purpose | Tests |
|--------|------|---------|-------|
| erlmcp_agent_spawner | gen_server | Spawn agents | 7 |
| erlmcp_agent_registry | wrapper | Global registry (gproc) | 3 |
| erlmcp_agent_state_manager | gen_server | Persistent state (ETS/DETS) | 5 |
| erlmcp_agent_lifecycle_fsm | gen_statem | State machine (idle→active→learning) | 5 |
| erlmcp_agent_health_monitor | gen_server | Health checks & recovery | 4 |
| erlmcp_routing_engine | gen_server | Intelligent routing | 5 |
| erlmcp_core_sup | supervisor | Supervision tree | 2 |

**Location**: `apps/erlmcp_core/src/`
**Tests**: `apps/erlmcp_core/test/`

---

### Subsystem 2: Coordination (erlmcp_transports)

**Responsibility**: Swarm topologies, message routing, consensus.

**Modules to Implement**:
| Module | Type | Purpose | Tests |
|--------|------|---------|-------|
| erlmcp_swarm_manager | gen_server | Create/manage swarms | 4 |
| erlmcp_topology_manager | gen_server | Topology graphs (nodes+edges) | 5 |
| erlmcp_mesh_topology_worker | gen_server | Fully-connected topology | 4 |
| erlmcp_hierarchical_topology_worker | gen_server | Leader-follower | 4 |
| erlmcp_ring_topology_worker | gen_server | Circular chain | 4 |
| erlmcp_star_topology_worker | gen_server | Hub-spoke | 4 |
| erlmcp_message_router | gen_server | FIFO message routing | 5 |
| erlmcp_consensus_manager | gen_server | Raft consensus | 5 |

**Location**: `apps/erlmcp_transports/src/`
**Tests**: `apps/erlmcp_transports/test/`

---

### Subsystem 3: Intelligence (erlmcp_observability)

**Responsibility**: Routing, learning, semantic memory, metrics.

**Modules to Implement**:
| Module | Type | Purpose | Tests |
|--------|------|---------|-------|
| erlmcp_learning_system | gen_server | Learning cycles | 4 |
| erlmcp_trajectory_recorder | gen_server | Record trajectories | 3 |
| erlmcp_sona_consolidator | gen_server | SONA learning | 3 |
| erlmcp_ewc_consolidator | gen_server | EWC++ consolidation | 3 |
| erlmcp_memory_subsystem | gen_server | Semantic memory | 4 |
| erlmcp_semantic_store | - | sql.js + HNSW | 3 |
| erlmcp_embeddings | - | ONNX embeddings | 3 |
| erlmcp_metrics_collector | gen_server | Metrics aggregation | 4 |

**Location**: `apps/erlmcp_observability/src/`
**Tests**: `apps/erlmcp_observability/test/`

---

### Subsystem 4: Security (erlmcp_validation)

**Responsibility**: Threat detection, audit, compliance, encryption.

**Modules to Implement**:
| Module | Type | Purpose | Tests |
|--------|------|---------|-------|
| erlmcp_aidefence_engine | gen_server | Threat detection | 5 |
| erlmcp_threat_detector | - | Soufflé rules | 4 |
| erlmcp_pattern_matcher | - | Pattern matching | 3 |
| erlmcp_pii_scanner | - | PII detection | 4 |
| erlmcp_audit_manager | gen_server | Immutable audit log | 5 |
| erlmcp_audit_logger | - | Append-only storage | 3 |
| erlmcp_compliance_validator | gen_server | Policy enforcement | 4 |
| erlmcp_secret_manager | gen_server | Encryption & keys | 4 |

**Location**: `apps/erlmcp_validation/src/`
**Tests**: `apps/erlmcp_validation/test/`

---

## 📊 Key Diagrams

### Architecture Levels (C4 Model)

```
C1: System Context
  ↓
C2: Container Architecture (4 apps + storage)
  ↓
C3: Component Level (per app subsystems)
  ↓
C4: Code Level (modules + types)
```

All diagrams in: `/home/user/erlmcp/docs/erlmcp-flow-c4-architecture.md`

---

## 🔄 Message Flow Examples

### Agent Spawn → Route Task → Learn

```
spawn_agent(config)
  → [Agent Foundation: erlmcp_agent_spawner]
      → validate, create, register, init state
        → {ok, agent{id, pid, state, metrics}}

route_task(context)
  → [Intelligence: erlmcp_routing_engine]
      → load metrics, check capability, HNSW match
        → {ok, routing_decision{agent_id, confidence}}

assign_task(agent_id, task)
  → [Agent Foundation: erlmcp_agent_spawner]
      → check load, send task, start timeout
        → {ok, task_id}

(agent executes)

task_completed(result)
  → [Agent Foundation: health monitor]
      → update metrics
        → [Intelligence: erlmcp_learning_system]
            → record trajectory, compute reward
              → schedule learning cycle
```

See diagrams: `/home/user/erlmcp/docs/erlmcp-flow-c4-architecture.md`

---

## ⚠️ Failure Modes (20+ Scenarios)

### Quick Reference

| ID | Failure | RTO | RPO | Recovery |
|----|---------|-----|-----|----------|
| F-1 | Agent crash | <5s | <1s | Restart (supervisor) |
| F-5 | Message loss | <30s | <100ms | Retry (3×) |
| F-10 | Routing stale | <1s | <5min | Fallback + refresh |
| F-15 | AIDefence timeout | <100ms | <1min | Allow + warn |
| F-17 | Threat detected | <100ms | 0 | Log + block + alert |

Complete failure analysis: `/home/user/erlmcp/docs/erlmcp-flow-architecture-design.md` (Part IV)

---

## 🚀 Deployment

### Local Development
```bash
cd /home/user/erlmcp
rebar3 compile          # Gate 1: Compile
rebar3 eunit --all      # Gate 2: Unit tests
rebar3 ct               # Gate 3: Integration tests
rebar3 cover            # Gate 4: Coverage ≥80%
rebar3 dialyzer         # Gate 5: Type check
rebar3 xref             # Gate 6: Undefined functions
```

### Cloud (Kubernetes)
```yaml
StatefulSet: erlmcp-flow
├── Pod-0: primary (leader)
├── Pod-1: replica
└── Pod-2: replica
Services:
├── erlmcp-flow (headless, DNS)
├── erlmcp-flow-api (LoadBalancer, :8080)
└── erlmcp-flow-audit (write-only)
Storage:
├── PVC: agent_state (EBS)
├── PVC: audit_log (S3)
└── PVC: memory_store (SSD)
```

See deployment details: `/home/user/erlmcp/docs/erlmcp-flow-implementation-guide.md` (Part IV)

---

## 📋 Checklists

### Pre-Implementation

- [ ] Read main architecture design
- [ ] Review C4 diagrams (visual understanding)
- [ ] Review 7 ADRs (design decisions)
- [ ] Understand 4 subsystems (roles + APIs)
- [ ] Understand supervision tree (no orphans)
- [ ] Map modules to files (32 modules)

### During Implementation (Per Phase)

- [ ] Implement 8 modules
- [ ] Write 20+ test suites
- [ ] Achieve ≥80% coverage
- [ ] Pass compilation gate
- [ ] Pass dialyzer (0 warnings)
- [ ] Pass xref (0 undefined)
- [ ] Code review by team lead

### Post-Implementation

- [ ] All 4 phases complete
- [ ] 90+ test suites passing
- [ ] ≥80% coverage (all subsystems)
- [ ] Integration tests (cross-subsystem)
- [ ] Chaos testing (20+ scenarios)
- [ ] Performance benchmarks
- [ ] Documentation complete
- [ ] SLOs defined and monitored
- [ ] Deployment runbooks ready

---

## 📈 Success Metrics

### Code Quality
- 32+ modules implemented
- 90+ test suites
- ≥80% line coverage
- 0 compilation errors
- 0 type errors (Dialyzer)
- 0 undefined functions (Xref)

### Architecture
- 4 subsystems clearly defined
- APIs documented (signatures + examples)
- 7+ ADRs completed
- C1-C4 diagrams (Mermaid)
- Supervision verified (no orphans)

### Testing
- 500+ test cases
- All phases tested (unit + integration)
- Chaos scenarios (20+)
- Property tests (HNSW search, consensus, etc.)

### Performance
- Spawn <100ms p95
- Route <50ms p95
- Message <10ms p95
- AIDefence <100ms p95
- Audit <10ms p95

---

## 📞 Quick Help

### I want to understand...

**The high-level architecture**
→ Start: `/home/user/erlmcp/docs/erlmcp-flow-architecture-design.md` (Intro)

**The visual architecture**
→ Read: `/home/user/erlmcp/docs/erlmcp-flow-c4-architecture.md` (all diagrams)

**How to implement it**
→ Read: `/home/user/erlmcp/docs/erlmcp-flow-implementation-guide.md`

**Quick facts (5 min)**
→ Check: `/home/user/erlmcp/docs/erlmcp-flow-quick-reference.md`

**A specific design decision**
→ Look: `/home/user/erlmcp/docs/adr/ADR-001-*.md`

**What happens when X fails**
→ Search: "Failure Modes" in main architecture doc

**Module details**
→ See: Implementation guide (Phase 1-4 sections)

---

## 🔗 Related Files

**Existing erlmcp Documentation**:
- `/home/user/erlmcp/ARCHITECTURE_ANALYSIS.md` (erlmcp base architecture)
- `/home/user/erlmcp/CLAUDE.md` (project specifications)
- `/home/user/erlmcp/docs/` (850+ docs)

**Code Locations**:
- `/home/user/erlmcp/apps/erlmcp_core/src/` (Agent Foundation)
- `/home/user/erlmcp/apps/erlmcp_transports/src/` (Coordination)
- `/home/user/erlmcp/apps/erlmcp_observability/src/` (Intelligence)
- `/home/user/erlmcp/apps/erlmcp_validation/src/` (Security)

---

## 📅 Implementation Timeline

- **Week 1-2**: Agent Foundation (Phase 1)
- **Week 3-4**: Coordination (Phase 2)
- **Week 5-6**: Intelligence (Phase 3)
- **Week 7-8**: Security (Phase 4)
- **Week 9**: Integration & Hardening
- **Week 10**: Code Review & Release

See full schedule: `/home/user/erlmcp/docs/erlmcp-flow-implementation-guide.md` (Part IV)

---

## ✅ Final Deliverables

✓ Main architecture design (70+ pages)
✓ C4 diagrams (visual architecture)
✓ Quick reference guide (5-min read)
✓ Implementation guide (detailed roadmap)
✓ ADRs (7+ decision records)
✓ Module mapping (32 modules)
✓ Quality gates (7 gates)
✓ Testing strategy (500+ tests)
✓ Deployment guide (cloud + local)
✓ Emergency procedures (5+ scenarios)

---

## 📊 Document Statistics

| Document | Pages | Purpose |
|----------|-------|---------|
| erlmcp-flow-architecture-design.md | 70 | Complete architecture |
| erlmcp-flow-c4-architecture.md | 20 | Visual diagrams |
| erlmcp-flow-quick-reference.md | 15 | Quick lookup |
| erlmcp-flow-implementation-guide.md | 40 | Implementation roadmap |
| ADR-001-*.md | 10 | Design decision |
| **TOTAL** | **~155** | Complete specification |

---

## 🎯 Next Steps

1. **Read**: Main architecture design (`erlmcp-flow-architecture-design.md`)
2. **Review**: C4 diagrams (visual understanding)
3. **Plan**: 10-week implementation schedule
4. **Implement**: Phase 1 (Agent Foundation), then Phase 2-4
5. **Test**: Each phase with 20+ test suites
6. **Monitor**: Via OTEL + Prometheus
7. **Iterate**: Based on learnings and feedback

---

**Version**: 1.0.0
**Status**: FINAL
**Last Updated**: 2026-02-01
**Author**: System Architecture Designer

---

## Table of All Documents

```
/home/user/erlmcp/docs/
├── erlmcp-flow-ARCHITECTURE-INDEX.md          ← YOU ARE HERE
├── erlmcp-flow-architecture-design.md         (Main, 70+ pages)
├── erlmcp-flow-c4-architecture.md             (Diagrams, 20 pages)
├── erlmcp-flow-quick-reference.md             (Quick ref, 15 pages)
├── erlmcp-flow-implementation-guide.md        (Implementation, 40 pages)
└── adr/
    └── ADR-001-agent-spawn-synchronicity.md   (Decision record, 10 pages)
```

**Start Reading**: `erlmcp-flow-architecture-design.md`

