# Makefile Refactoring - Task Delegation & Dependency Graph

**Plan**: MAKEFILE_REFACTORING_PLAN_SPARC.md
**Total Tasks**: 18
**Total Effort**: 7.5 hours (parallel execution)
**Total Cost**: $1.10 (cloud execution)

---

## Visual Dependency Graph

```
LEGEND:
  [T#.#] = Task ID
  ──→    = Depends on
  ═══    = Critical path
  ║      = Parallel execution possible

PHASE 1: FOUNDATION (2 hours, $0.30)
═══════════════════════════════════════════════════════════════
[T1.1] Create mk/ directory structure
   ║
   ╠══→ [T1.2] Extract state management (mk/state.mk)
   ║       └──→ [T2.3] Add state to gates
   ║       └──→ [T3.2] Extract governance.mk
   ║
   ╠══→ [T1.3] Extract parallel primitives (mk/parallel.mk) ═══ CRITICAL PATH
   ║       └──→ [T2.2] Implement parallel check ═══════════════ CRITICAL PATH
   ║       └──→ [T3.4] Extract benchmark.mk
   ║
   ╚══→ [T1.4] Extract core targets (mk/core.mk)
           └──→ [T3.3] Extract cli.mk
           └──→ [T4.2] Extract dev.mk


PHASE 2: QUALITY GATES (2.5 hours, $0.35)
═══════════════════════════════════════════════════════════════
[T2.1] Extract quality gates (mk/quality.mk)
   ║
   ╠══→ [T2.2] Implement parallel check ═══════════════════════ CRITICAL PATH
   ║       └──→ [T4.3] Add inline docs ════════════════════════ CRITICAL PATH
   ║
   ╠══→ [T2.3] Add state persistence to gates
   ║
   ╠══→ [T2.4] Implement incremental testing
   ║
   ╚══→ [T3.1] Extract TCPS targets
        └──→ [T4.1] Extract release.mk


PHASE 3: ADVANCED FEATURES (1 hour, $0.15)
═══════════════════════════════════════════════════════════════
[T3.1] Extract TCPS (mk/tcps.mk) ─┐
[T3.2] Extract governance ────────┤
[T3.3] Extract CLI ───────────────┤─→ [Phase 4]
[T3.4] Extract benchmark ─────────┘
         (ALL PARALLEL)


PHASE 4: POLISH & DOCUMENTATION (2 hours, $0.30)
═══════════════════════════════════════════════════════════════
[T4.1] Extract release.mk ────────┐
[T4.2] Extract dev.mk ────────────┤
[T4.3] Add inline docs ═══════════╡═══ CRITICAL PATH
[T4.4] Create migration guide ────┤
[T4.5] Update CLAUDE.md ──────────┤─→ DONE
[T4.6] Create rollback procedure ─┘
         (ALL PARALLEL)
```

---

## Critical Path Analysis

**Critical Path**: T1.1 → T1.3 → T2.2 → T4.3

| Task | Duration | Agent | Blocking? |
|------|----------|-------|-----------|
| T1.1 | 0.5h | erlang-architect | YES (all Phase 1) |
| T1.3 | 1.5h | erlang-otp-developer | YES (T2.2) |
| T2.2 | 1h | erlang-performance | YES (major optimization) |
| T4.3 | 1h | erlang-otp-developer | YES (documentation) |

**Total Critical Path**: 4 hours (sequential)
**Total Effort**: 7.5 hours (with parallelization)
**Speedup**: 1.875x (critical path vs. total effort)

---

## Agent Delegation

### Phase 1: Foundation

| Task ID | Task | Agent | Duration | Dependencies | Artifacts |
|---------|------|-------|----------|--------------|-----------|
| **T1.1** | Create mk/ directory structure | erlang-architect | 0.5h | - | mk/*.mk (empty files) |
| **T1.2** | Extract state.mk | erlang-otp-developer | 1.5h | T1.1 | mk/state.mk (150 lines) |
| **T1.3** | Extract parallel.mk | erlang-otp-developer | 1.5h | T1.1 | mk/parallel.mk (120 lines) |
| **T1.4** | Extract core.mk | erlang-otp-developer | 0.5h | T1.1 | mk/core.mk (150 lines) |

**Parallelization**:
```
T1.1 (0.5h)
  ├─ T1.2 (1.5h) ─┐
  ├─ T1.3 (1.5h) ─┤─ PARALLEL (max 1.5h)
  └─ T1.4 (0.5h) ─┘

Total: 0.5h + 1.5h = 2h
```

**Commands**:
```bash
# T1.1 (erlang-architect)
Task("Create Makefile Module Structure",
     "Create mk/ directory with placeholder .mk files",
     "erlang-architect")

# T1.2, T1.3, T1.4 (parallel)
Task("Extract State Management",
     "Extract state primitives to mk/state.mk",
     "erlang-otp-developer")

Task("Extract Parallel Execution",
     "Extract parallel primitives to mk/parallel.mk",
     "erlang-otp-developer")

Task("Extract Core Targets",
     "Extract compile, test, clean to mk/core.mk",
     "erlang-otp-developer")
```

### Phase 2: Quality Gates

| Task ID | Task | Agent | Duration | Dependencies | Artifacts |
|---------|------|-------|----------|--------------|-----------|
| **T2.1** | Extract quality.mk | erlang-otp-developer | 2h | T1.2, T1.3 | mk/quality.mk (180 lines) |
| **T2.2** | Implement parallel check | erlang-performance | 1h | T1.3, T2.1 | Parallel `make check` |
| **T2.3** | Add state to gates | erlang-otp-developer | 0.5h | T1.2, T2.1 | State persistence |
| **T2.4** | Implement incremental testing | erlang-otp-developer | 0.5h | T1.2, T2.1 | `make test-changed` |

**Parallelization**:
```
T2.1 (2h)
  ├─ T2.2 (1h) ──┐
  ├─ T2.3 (0.5h) ┤─ PARALLEL (max 1h)
  └─ T2.4 (0.5h) ┘

Total: 2h + 1h = 3h
Actual: 2.5h (overlap optimization)
```

**Commands**:
```bash
# T2.1 (erlang-otp-developer)
Task("Extract Quality Gates",
     "Extract validate-* targets to mk/quality.mk",
     "erlang-otp-developer")

# T2.2, T2.3, T2.4 (parallel)
Task("Implement Parallel Check",
     "Add parallel execution to make check (3x speedup)",
     "erlang-performance")

Task("Add State Persistence",
     "Persist quality gate state to .erlmcp/state/gates.json",
     "erlang-otp-developer")

Task("Implement Incremental Testing",
     "Detect changed modules and test only those",
     "erlang-otp-developer")
```

### Phase 3: Advanced Features

| Task ID | Task | Agent | Duration | Dependencies | Artifacts |
|---------|------|-------|----------|--------------|-----------|
| **T3.1** | Extract tcps.mk | erlang-otp-developer | 1h | T2.1 | mk/tcps.mk (160 lines) |
| **T3.2** | Extract governance.mk | erlang-otp-developer | 1h | T1.2 | mk/governance.mk (140 lines) |
| **T3.3** | Extract cli.mk | erlang-otp-developer | 1h | T1.4 | mk/cli.mk (120 lines) |
| **T3.4** | Extract benchmark.mk | erlang-performance | 1h | T1.3 | mk/benchmark.mk (100 lines) |

**Parallelization**:
```
T3.1 (1h) ─┐
T3.2 (1h) ─┤─ PARALLEL (max 1h)
T3.3 (1h) ─┤
T3.4 (1h) ─┘

Total: 1h (all parallel)
```

**Commands**:
```bash
# All parallel (4 agents)
Task("Extract TCPS Manufacturing",
     "Extract jidoka, andon, poka-yoke to mk/tcps.mk",
     "erlang-otp-developer")

Task("Extract Governance System",
     "Extract hooks-validate, settings-validate to mk/governance.mk",
     "erlang-otp-developer")

Task("Extract CLI Management",
     "Extract cli-version, cli-release to mk/cli.mk",
     "erlang-otp-developer")

Task("Extract Benchmarking",
     "Extract bench-quick, benchmark to mk/benchmark.mk",
     "erlang-performance")
```

### Phase 4: Polish & Documentation

| Task ID | Task | Agent | Duration | Dependencies | Artifacts |
|---------|------|-------|----------|--------------|-----------|
| **T4.1** | Extract release.mk | erlang-otp-developer | 0.5h | T2.1 | mk/release.mk (100 lines) |
| **T4.2** | Extract dev.mk | erlang-otp-developer | 0.5h | T1.4 | mk/dev.mk (80 lines) |
| **T4.3** | Add inline docs | erlang-otp-developer | 1h | ALL | Comprehensive help |
| **T4.4** | Create migration guide | code-reviewer | 1h | ALL | MAKEFILE_MIGRATION.md |
| **T4.5** | Update CLAUDE.md | code-reviewer | 0.5h | ALL | Updated docs |
| **T4.6** | Create rollback procedure | erlang-architect | 0.5h | ALL | Rollback script |

**Parallelization**:
```
T4.1 (0.5h) ─┐
T4.2 (0.5h) ─┤
T4.3 (1h) ───┤─ PARALLEL (max 1h)
T4.4 (1h) ───┤
T4.5 (0.5h) ─┤
T4.6 (0.5h) ─┘

Total: 2h (with parallelization)
```

**Commands**:
```bash
# All parallel (6 agents)
Task("Extract Release Management",
     "Extract release, release-validate to mk/release.mk",
     "erlang-otp-developer")

Task("Extract Dev Tools",
     "Extract console, observer, deps to mk/dev.mk",
     "erlang-otp-developer")

Task("Add Comprehensive Help",
     "Add inline documentation for all targets",
     "erlang-otp-developer")

Task("Create Migration Guide",
     "Document migration from old to new Makefile",
     "code-reviewer")

Task("Update CLAUDE.md",
     "Update project documentation with new structure",
     "code-reviewer")

Task("Create Rollback Procedure",
     "Document and test rollback to Makefile.backup",
     "erlang-architect")
```

---

## Work Order Templates

### Template: Phase 1 Work Orders

```json
[
  {
    "id": "wo-makefile-001",
    "task": "Create mk/ directory structure",
    "agent": "erlang-architect",
    "priority": "high",
    "dependencies": [],
    "constraints": {
      "time_budget": 1800,
      "files": ["Makefile", "mk/*.mk"]
    },
    "deliverables": [
      "mk/state.mk (empty)",
      "mk/parallel.mk (empty)",
      "mk/core.mk (empty)",
      "mk/quality.mk (empty)",
      "mk/tcps.mk (empty)",
      "mk/governance.mk (empty)",
      "mk/cli.mk (empty)",
      "mk/release.mk (empty)",
      "mk/dev.mk (empty)",
      "mk/benchmark.mk (empty)"
    ]
  },
  {
    "id": "wo-makefile-002",
    "task": "Extract state management to mk/state.mk",
    "agent": "erlang-otp-developer",
    "priority": "high",
    "dependencies": ["wo-makefile-001"],
    "constraints": {
      "time_budget": 5400,
      "files": ["Makefile", "mk/state.mk"]
    },
    "deliverables": [
      "mk/state.mk (150 lines)",
      "Functions: mark_running, mark_done, should_skip",
      "State file: .erlmcp/state/gates.json schema",
      "Smoke test: tests/mk/test_state.mk.sh"
    ]
  }
]
```

### Template: Quality Gate

```makefile
# mk/quality.mk
# Quality gates with state management and parallel execution

.PHONY: validate validate-compile validate-test validate-coverage validate-quality validate-bench

# Main quality gate (runs all gates in parallel)
validate: check-state
	@$(call log_start,validate)
	@$(MAKE) -j4 validate-compile validate-test validate-coverage validate-quality validate-bench
	@$(call log_done,validate)
	@echo "$(GREEN)✅ ALL QUALITY GATES PASSED$(NC)"

# Individual gates (with state management)
validate-compile:
	@$(call gate_wrapper,compile,TERM=dumb rebar3 compile)

validate-test:
	@$(call gate_wrapper,test,rebar3 eunit && rebar3 ct)

validate-coverage:
	@$(call gate_wrapper,coverage,rebar3 cover && ./scripts/check_coverage_threshold.sh 80)

# Helper: Gate wrapper with state management
define gate_wrapper
	$(eval GATE_NAME := $(1))
	$(eval GATE_CMD := $(2))
	@if $(call should_skip,$(GATE_NAME)); then \
		echo "$(YELLOW)⏩ Skipping $(GATE_NAME) (inputs unchanged)$(NC)"; \
	else \
		$(call mark_running,$(GATE_NAME)); \
		if $(GATE_CMD) > .erlmcp/logs/$(GATE_NAME)_$(shell date +%s).log 2>&1; then \
			$(call mark_done,$(GATE_NAME),0); \
			echo "$(GREEN)✅ $(GATE_NAME) passed$(NC)"; \
		else \
			$(call mark_done,$(GATE_NAME),1); \
			echo "$(RED)❌ $(GATE_NAME) failed$(NC)"; \
			exit 1; \
		fi; \
	fi
endef
```

---

## Validation Strategy

### Per-Task Validation

**Template**:
```bash
#!/usr/bin/env bash
# Validate Task T#.#

set -euo pipefail

echo "Validating Task T#.#..."

# 1. File exists
test -f mk/<module>.mk || exit 1

# 2. Syntax check
make -n -f mk/<module>.mk > /dev/null 2>&1 || exit 1

# 3. Smoke test
make <target> > /dev/null 2>&1 || exit 1

# 4. Idempotence
make <target> > /tmp/run1.log 2>&1
make <target> > /tmp/run2.log 2>&1
diff /tmp/run1.log /tmp/run2.log || exit 1

echo "✅ Task T#.# validated"
```

### Per-Phase Validation

**Phase 1**:
```bash
make compile  # Should work
make test     # Should work
make check    # Should work
```

**Phase 2**:
```bash
time make check                # Should be ≤ 120s
cat .erlmcp/state/gates.json   # Should exist
make test-changed              # Should detect changes
```

**Phase 3**:
```bash
make jidoka           # Should run 8 gates
make hooks-validate   # Should validate hooks
make cli-version      # Should show version
make bench-quick      # Should run (local)
```

**Phase 4**:
```bash
make help | wc -l     # Should be ~100 lines
make release          # Should build release
make rollback         # Should rollback
```

---

## Progress Tracking

### Phase Completion Checklist

**Phase 1: Foundation**
- [ ] T1.1: mk/ directory created
- [ ] T1.2: mk/state.mk extracted (150 lines)
- [ ] T1.3: mk/parallel.mk extracted (120 lines)
- [ ] T1.4: mk/core.mk extracted (150 lines)
- [ ] All smoke tests pass
- [ ] `make check` passes
- [ ] Cloud validation passes

**Phase 2: Quality Gates**
- [ ] T2.1: mk/quality.mk extracted (180 lines)
- [ ] T2.2: Parallel check implemented (3x speedup)
- [ ] T2.3: State persistence working
- [ ] T2.4: Incremental testing working
- [ ] Performance: `make check` ≤ 120s
- [ ] Stress test: 100 parallel runs pass
- [ ] Cloud validation passes

**Phase 3: Advanced Features**
- [ ] T3.1: mk/tcps.mk extracted (160 lines)
- [ ] T3.2: mk/governance.mk extracted (140 lines)
- [ ] T3.3: mk/cli.mk extracted (120 lines)
- [ ] T3.4: mk/benchmark.mk extracted (100 lines)
- [ ] All TCPS targets work
- [ ] All governance targets work
- [ ] All CLI targets work
- [ ] Benchmarks run (local)

**Phase 4: Polish & Documentation**
- [ ] T4.1: mk/release.mk extracted (100 lines)
- [ ] T4.2: mk/dev.mk extracted (80 lines)
- [ ] T4.3: Comprehensive help added
- [ ] T4.4: Migration guide created
- [ ] T4.5: CLAUDE.md updated
- [ ] T4.6: Rollback procedure documented
- [ ] `make help` comprehensive
- [ ] All documentation reviewed

---

## Execution Timeline

### Week 1: Foundation

| Day | Tasks | Agent | Duration | Deliverables |
|-----|-------|-------|----------|--------------|
| Mon | T1.1 | erlang-architect | 0.5h | mk/ structure |
| Mon | T1.2, T1.3, T1.4 | erlang-otp-developer (3x) | 1.5h | state.mk, parallel.mk, core.mk |
| Tue | Validation | erlang-test-engineer | 1h | Smoke tests |
| Wed | Cloud testing | erlang-otp-developer | 1h | Cloud validation |
| Thu | Review | code-reviewer | 1h | Phase 1 review |
| Fri | Merge | erlang-github-ops | 0.5h | Merge to main |

**Total**: 5.5 hours (wall-clock: 5 days)

### Week 2: Quality Gates

| Day | Tasks | Agent | Duration | Deliverables |
|-----|-------|-------|----------|--------------|
| Mon | T2.1 | erlang-otp-developer | 2h | quality.mk |
| Tue | T2.2, T2.3, T2.4 | erlang-performance, erlang-otp-developer (3x) | 1h | Parallel check, state, incremental |
| Wed | Performance testing | erlang-performance | 1h | Benchmarks |
| Thu | Stress testing | erlang-test-engineer | 1h | 100 runs |
| Fri | Review & merge | code-reviewer, erlang-github-ops | 1h | Phase 2 done |

**Total**: 6 hours (wall-clock: 5 days)

### Week 3: Advanced Features

| Day | Tasks | Agent | Duration | Deliverables |
|-----|-------|-------|----------|--------------|
| Mon | T3.1, T3.2, T3.3, T3.4 | 4x agents | 1h | tcps.mk, governance.mk, cli.mk, benchmark.mk |
| Tue | Validation | erlang-test-engineer | 1h | All smoke tests |
| Wed | Integration testing | erlang-test-engineer | 1h | End-to-end |
| Thu | Review | code-reviewer | 1h | Phase 3 review |
| Fri | Merge | erlang-github-ops | 0.5h | Phase 3 done |

**Total**: 4.5 hours (wall-clock: 5 days)

### Week 4: Polish & Documentation

| Day | Tasks | Agent | Duration | Deliverables |
|-----|-------|-------|----------|--------------|
| Mon | T4.1, T4.2, T4.3 | erlang-otp-developer | 2h | release.mk, dev.mk, help |
| Tue | T4.4, T4.5 | code-reviewer | 1.5h | Migration guide, CLAUDE.md |
| Wed | T4.6 | erlang-architect | 0.5h | Rollback procedure |
| Thu | Final validation | erlang-test-engineer | 1h | Full test suite |
| Fri | Release | erlang-github-ops | 1h | PR creation, announcement |

**Total**: 6 hours (wall-clock: 5 days)

---

## Summary

**Total Effort**: 22 hours (wall-clock: 4 weeks)
**Total Cost**: $1.10 (cloud execution of tasks only)
**Agents Required**: 7 (erlang-architect, erlang-otp-developer, erlang-performance, erlang-test-engineer, erlang-github-ops, code-reviewer, plan-designer)
**Deliverables**: 10 .mk modules, 18 smoke tests, migration guide, updated documentation

**Key Milestones**:
- Week 1: Foundation complete (state, parallel, core)
- Week 2: Quality gates complete (3x speedup achieved)
- Week 3: Advanced features complete (TCPS, governance, CLI, benchmarks)
- Week 4: Production-ready (documentation, rollback, release)

**Success Criteria**:
- [ ] All existing targets work identically
- [ ] Parallel execution: 3x speedup
- [ ] State management: Incremental builds
- [ ] Backward compatible: Zero breaking changes
- [ ] Cloud validated: Works in cloud environment
- [ ] Documented: Comprehensive help and migration guide

**Next Step**: Execute Phase 1 (Foundation)
