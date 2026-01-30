# SPARC Quick Reference: MCP Compliance + Consolidation

**Status**: Phase 1-3 Complete ✅ | Phase 4 Ready to Begin
**Timeline**: 14 weeks total | 11 weeks remaining
**Current Week**: 4 (Task Management begins)

---

## 🎯 Project Goals

| Objective | Target | Status |
|-----------|--------|--------|
| MCP Feature Gaps | 5/5 implemented | 0/5 ⚠️ |
| Benchmark Files | 93 → 47 (50% reduction) | 93 |
| Test Coverage | ≥80% | TBD |
| Performance | <10% regression | Baseline TBD |

---

## 📅 Timeline Snapshot

```
COMPLETE ✅          IN PROGRESS 🔄              PENDING ⏳
Weeks 1-3            Week 4-5                    Week 6+
┌─────────┐         ┌─────────┐                ┌─────────┐
│ Spec    │────────▶│  Task   │───────────────▶│ Cancel  │
│ Pseudo  │         │  Mgmt   │                │ Complete│
│ Arch    │         │         │                │ Sample  │
└─────────┘         └─────────┘                │ Elicit  │
                    [YOU ARE HERE]             │ Consolidate
                                                └─────────┘
```

### Phase Breakdown

| Week | Phase | Focus | Owner | Status |
|------|-------|-------|-------|--------|
| 1 | Specification | Requirements | plan-designer | ✅ DONE |
| 2 | Pseudocode | Algorithm design | plan-designer | ✅ DONE |
| 3 | Architecture | System design | erlang-architect | ✅ DONE |
| **4-5** | **Refinement** | **Task Management** | **erlang-otp-developer** | **🔄 START** |
| 6-7 | Refinement | Cancellation + Completions | erlang-otp-developer | ⏳ |
| 8-9 | Refinement | Sampling + Elicitation | erlang-otp-developer | ⏳ |
| 10-11 | Refinement | MCP Integration | erlang-otp-developer | ⏳ |
| 12-13 | Refinement | Benchmark Consolidation | erlang-performance | ⏳ |
| 14 | Completion | Quality Gates + Release | code-reviewer | ⏳ |

---

## 🔀 Critical Path

```mermaid
graph LR
    A[Week 1-3: Planning ✅] --> B[Week 4-5: Task Manager 🔄]
    B --> C[Week 6-7: Cancel + Complete]
    C --> D[Week 8-9: Sample + Elicit]
    D --> E[Week 10-11: Integration]
    E --> F[Week 12-13: Consolidation]
    F --> G[Week 14: Release]
```

**Critical Dependencies**:
- Task Manager MUST complete before Completions API (blocks Week 6)
- MCP features MUST complete before Consolidation (prevents churn)
- Baseline MUST be established Week 4 (enables regression detection)

---

## 🎯 Week 4-5: Task Management Implementation

### Deliverables

| Module | Path | Status |
|--------|------|--------|
| Task Manager | `apps/erlmcp_core/src/erlmcp_task_manager.erl` | □ |
| Supervisor | `apps/erlmcp_core/src/erlmcp_task_manager_sup.erl` | □ |
| EUnit Tests | `apps/erlmcp_core/test/erlmcp_task_manager_tests.erl` | □ |
| CT Suite | `apps/erlmcp_core/test/erlmcp_task_manager_SUITE.erl` | □ |

### Daily Checklist (10 days)

**Day 1-2: Skeleton**
- [ ] Create gen_server skeleton
- [ ] Create supervisor
- [ ] Write startup tests
- [ ] Quality gate: `rebar3 compile` (0 errors)

**Day 3-4: Core API**
- [ ] Implement `create_task/2`
- [ ] Implement `list_tasks/1`
- [ ] Implement `get_task/1`
- [ ] Write API tests
- [ ] Quality gate: `rebar3 eunit --module=erlmcp_task_manager_tests`

**Day 5-6: State Machine**
- [ ] Implement task execution
- [ ] Implement state transitions (pending → working → completed/failed)
- [ ] Write state machine tests
- [ ] Quality gate: Coverage ≥70%

**Day 7-8: Cancellation**
- [ ] Implement `cancel_task/2`
- [ ] Handle graceful shutdown
- [ ] Write cancellation tests
- [ ] Quality gate: Coverage ≥80%

**Day 9-10: Integration**
- [ ] Integrate with supervisor tree
- [ ] Write integration tests (CT)
- [ ] Run benchmarks
- [ ] Quality gate: All gates pass

### Quality Gates (Run Daily)

```bash
# Compilation (MANDATORY - blocks all work)
TERM=dumb rebar3 compile

# Unit tests (MANDATORY - must pass 100%)
rebar3 eunit --module=erlmcp_task_manager_tests

# Type checking (fix warnings daily)
rebar3 dialyzer

# Performance (track regression)
make benchmark-quick

# Integration (final days only)
rebar3 ct --suite=erlmcp_task_manager_SUITE
```

### Success Criteria

| Metric | Target | Status |
|--------|--------|--------|
| Compilation Errors | 0 | □ |
| Test Pass Rate | 100% | □ |
| Code Coverage | ≥80% | □ |
| Dialyzer Warnings | 0 | □ |
| Xref Issues | 0 | □ |
| Performance | <10% regression | □ |
| Documentation | Complete | □ |

---

## 🚀 Parallel Work Streams

### Weeks 6-7: Two Streams

```
Developer A                    Developer B
┌─────────────────┐           ┌─────────────────┐
│ Cancellation    │           │ Completions API │
│ erlmcp_         │           │ erlmcp_         │
│ cancellation    │           │ completion      │
└─────────────────┘           └─────────────────┘
        │                             │
        └──────── Merge ──────────────┘
                  Week 7
```

### Weeks 8-9: Two Streams

```
Developer A                    Developer B
┌─────────────────┐           ┌─────────────────┐
│ Sampling        │           │ Elicitation     │
│ erlmcp_sampling │           │ erlmcp_         │
│                 │           │ elicitation     │
└─────────────────┘           └─────────────────┘
        │                             │
        └──────── Merge ──────────────┘
                  Week 9
```

---

## ⚠️ Risk Mitigation

| Risk | Mitigation | Owner |
|------|------------|-------|
| Task Manager delays | Start Week 4, 2-week buffer | erlang-otp-developer |
| Consolidation test churn | Do AFTER MCP (Week 12) | erlang-performance |
| Performance regression | Baseline Week 4, check daily | All |
| Integration breaks features | Backward compat tests | erlang-test-engineer |

---

## 📊 Consolidation Strategy (Weeks 12-13)

### Why AFTER MCP Implementation?

```
❌ WRONG (Causes Churn)           ✅ RIGHT (Minimizes Churn)
┌─────────────────────┐          ┌─────────────────────┐
│ Week 4-11:          │          │ Week 4-11:          │
│ Implement MCP       │          │ Implement MCP       │
│ + Consolidate       │          │ (stable benchmarks) │
│ (tests change!)     │          │                     │
└─────────────────────┘          └─────────────────────┘
                                           ↓
                                 ┌─────────────────────┐
                                 │ Week 11: BASELINE   │
                                 │ (freeze point)      │
                                 └─────────────────────┘
                                           ↓
                                 ┌─────────────────────┐
                                 │ Week 12-13:         │
                                 │ Consolidate         │
                                 │ (compare to baseline)│
                                 └─────────────────────┘
```

### Consolidation Phases

| Week | Phase | Action | LOC Reduction |
|------|-------|--------|---------------|
| 12.1 | Cleanup | Delete 9 redundant files | -750 |
| 12.2 | Merge | Consolidate 4 file groups | -600 |
| 12.3 | Standardize | Fix measurements | 0 |
| 13.1 | Framework | Build infrastructure | +800 |
| 13.2 | CI | GitHub Actions | +100 |

**Net Result**: 93 files → 47 files (50% reduction)

---

## 📝 Communication Protocol

### Daily (Async)
Post to team channel:
- ✅ Completed yesterday
- 🔄 Working today
- ⚠️ Blockers

### Weekly (Sync, 30 min)
- Demo completed work
- Integration planning
- Risk review
- Next week goals

### Quality Gates (After Each Module)
- Run full validation
- Post results
- No proceed until pass

---

## 🎯 Next Actions (Week 4 - Starting Now)

### Day 1 (Today)
1. [ ] Create feature branch: `git checkout -b feature/task-management`
2. [ ] Run baseline benchmarks: `make benchmark-all`
3. [ ] Store baseline: Save results to `baseline_week4.json`
4. [ ] Create module: `apps/erlmcp_core/src/erlmcp_task_manager.erl`
5. [ ] Create tests: `apps/erlmcp_core/test/erlmcp_task_manager_tests.erl`

### Day 2
1. [ ] Implement gen_server skeleton
2. [ ] Write startup tests (TDD)
3. [ ] Create supervisor
4. [ ] Quality gate: compile + tests

### Day 3-4
1. [ ] Implement create_task/2 (TDD)
2. [ ] Implement list_tasks/1 (TDD)
3. [ ] Implement get_task/1 (TDD)
4. [ ] Quality gate: coverage ≥70%

### Day 5
1. [ ] Implement task execution
2. [ ] State machine: pending → working
3. [ ] Quality gate: tests pass

---

## 📚 Reference Documents

| Document | Path | Purpose |
|----------|------|---------|
| **Full SPARC Plan** | `docs/SPARC_MCP_COMPLIANCE_CONSOLIDATION_PLAN.md` | Complete execution plan |
| **This Reference** | `docs/SPARC_QUICK_REFERENCE.md` | Quick lookup |
| **MCP Spec Gaps** | `docs/MCP_2025-11-25_SPECIFICATION_GAPS.md` | Requirements |
| **MCP Pseudocode** | `docs/MCP_2025-11-25_PSEUDOCODE_DESIGN.md` | Algorithms |
| **MCP Architecture** | `docs/MCP_2025-11-25_ARCHITECTURE_DESIGN.md` | System design |
| **Consolidation** | `BENCHMARK_CONSOLIDATION_ROADMAP.md` | Benchmark cleanup |

---

## ✅ Quality Gate Checklist (Copy for Each Module)

```bash
# 1. Compilation (MANDATORY)
TERM=dumb rebar3 compile
# ✅ Expected: Compiled X modules, 0 errors, 0 warnings

# 2. Unit Tests (MANDATORY)
rebar3 eunit --module=MODULE_tests
# ✅ Expected: X/X tests passed

# 3. Integration Tests (if applicable)
rebar3 ct --suite=MODULE_SUITE
# ✅ Expected: X/X tests passed

# 4. Coverage (MANDATORY)
rebar3 cover
# ✅ Expected: ≥80% coverage

# 5. Type Checking (SHOULD)
rebar3 dialyzer
# ✅ Expected: 0 warnings

# 6. Cross-reference (SHOULD)
rebar3 xref
# ✅ Expected: 0 issues

# 7. Performance (MANDATORY if code changes critical path)
make benchmark-quick
# ✅ Expected: <10% regression from baseline
```

---

## 🏁 Final Validation (Week 14)

### Pre-Release Checklist

- [ ] All 5 MCP gaps implemented
- [ ] All benchmarks consolidated
- [ ] All quality gates pass
- [ ] Backward compatibility verified
- [ ] Documentation complete
- [ ] Migration guide written
- [ ] Release notes prepared
- [ ] PR created and approved
- [ ] Git tag created: v0.7.0
- [ ] Release published

---

**Document Version**: 1.0
**Last Updated**: 2026-01-30
**Status**: Phase 4 Ready to Begin

https://claude.ai/code/session_018igga7R3JKL2TCSDqy27ro
