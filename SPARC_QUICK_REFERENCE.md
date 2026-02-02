# SPARC erlmcp-flow Quick Reference
## 5 Phases, 20 Agents, 4-Minute Cloud Gate

---

## THE 5 PHASES

```
Phase 1: AGENT FOUNDATION (Days 1-3)
├─ 13 modules, 1,570 LOC, 45 EUnit tests
├─ Process-per-connection with gen_server pool
├─ gproc registry (O(log N) routing)
└─ Request correlation with UUID tracking

Phase 2: SWARM COORDINATION (Days 4-8)
├─ 12 modules, 1,470 LOC, 38 EUnit + 15 CT tests
├─ Topology management (mesh/hierarchical/ring/star)
├─ Gossip protocol for state propagation
└─ Heartbeat monitoring and failover

Phase 3: CONSENSUS PROTOCOLS (Days 9-13)
├─ 11 modules, 1,540 LOC, 52 EUnit + 20 CT tests
├─ Raft consensus (leader election, log replication)
├─ Byzantine fault tolerance
└─ Safety + liveness verification

Phase 4: INTELLIGENCE & ROUTING (Days 14-17)
├─ 14 modules, 2,000 LOC, 41 EUnit + 18 CT tests
├─ HNSW index (O(log N) semantic search)
├─ MoE routing (Mixture of Experts)
└─ EWC++ consolidation (prevent catastrophic forgetting)

Phase 5: SECURITY & OBSERVABILITY (Days 18-21)
├─ 13 modules, 1,830 LOC, 36 EUnit + 22 CT tests
├─ TLS 1.3 encryption
├─ HMAC-SHA256 signing
├─ OTEL distributed tracing
└─ Rate limiting + circuit breaker

TOTAL: 63 modules, 8,410 LOC, 259 tests, <4 min gate
```

---

## 20 AGENT WORKERS

| # | Agent | Role | Availability |
|---|-------|------|--------------|
| 1 | erlang-researcher | Code analysis | Phase 1-5 |
| 2 | erlang-architect | System design | Phase 1-5 |
| 3 | erlang-otp-developer | Implementation | Phase 1-5 |
| 4 | erlang-test-engineer | Chicago TDD | Phase 1-5 |
| 5 | erlang-transport-builder | Transport layer | Phase 1,2,5 |
| 6 | code-reviewer | Quality review | Phase 1-5 |
| 7 | erlang-performance | Benchmarking | Phase 1-5 |
| 8 | build-engineer | Constrained writes | Phase 1-5 |
| 9 | verifier | Gate verification | Phase 1-5 |
| 10 | agent-01-compile-gate | Compilation | Phase 1-5 |
| 11 | agent-06-test-eunit | EUnit tests | Phase 1-5 |
| 12 | agent-07-test-ct | Common Test | Phase 1-5 |
| 13 | agent-11-coverage | Coverage report | Phase 1-5 |
| 14 | agent-12-dialyzer | Type checking | Phase 1-5 |
| 15 | agent-13-xref | Cross-reference | Phase 1-5 |
| 16 | agent-14-format | Code formatting | Phase 1-5 |
| 17 | agent-15-benchmark | Performance regression | Phase 1-5 |
| 18 | erlang-github-ops | Git + CI/CD | Phase 1-5 |
| 19 | sparc-orchestrator | SPARC methodology | Phase 1-5 |
| 20 | plan-designer | Implementation planning | Phase 1-5 |

---

## GOLDEN RULE: 1 MESSAGE = ALL OPERATIONS

```
✓ Spawn 20+ agents in SINGLE message
✓ Batch 20+ todos together
✓ Read/Edit 20+ files in parallel
✗ NEVER sequential operations

Expected speedup: 2.8x - 4.4x (EPIC 9 workflow)
```

---

## PHASE 1 CHECKLIST: AGENT FOUNDATION

### Day 1: Specification
- [ ] Research gen_server patterns in erlmcp (erlang-researcher)
- [ ] Design 3-tier supervision tree (erlang-architect)
- [ ] Define 13 modules with dependencies (plan-designer)
- [ ] Create architecture diagram (erlang-architect)
- [ ] Write 45 EUnit tests (erlang-test-engineer)

### Day 2: Pseudocode & Architecture
- [ ] Agent state machine algorithm (erlang-architect)
- [ ] Request routing O(log N) algorithm (erlang-architect)
- [ ] Pool scaling algorithm (plan-designer)
- [ ] Implement 13 modules (erlang-otp-developer)
- [ ] Integrate with transports (erlang-transport-builder)

### Day 3: Refinement & Completion
- [ ] Run TDD cycle: RED → GREEN → REFACTOR (erlang-test-engineer)
- [ ] Code review for OTP compliance (code-reviewer)
- [ ] Benchmark agent routing (erlang-performance)
- [ ] Run quality gates: compile, test, dialyzer, xref, coverage (verifier)
- [ ] Commit Phase 1 work (erlang-github-ops)

**Success Criteria**:
- [ ] make check passes (all gates)
- [ ] Coverage ≥92%
- [ ] Dialyzer: 0 warnings
- [ ] Xref: 0 undefined
- [ ] Code reviewed + approved
- [ ] Commit pushed to feature branch

---

## QUALITY GATES (ALL REQUIRED)

```bash
make compile                # Gate 1: errors = 0 (30s)
make eunit                  # Gate 2: failures = 0, coverage ≥80% (60s)
make ct                     # Gate 3: pass_rate = 100% (120s)
make dialyzer               # Gate 4: warnings = 0 (90s)
make xref                   # Gate 5: undefined = ∅ (30s)
make check                  # All gates parallel (180s)
```

**Pass/Fail Decision**:
- If ANY gate fails → DO NOT COMMIT
- Fix root cause (do not use --no-verify)
- Create NEW commit (not amend)
- Re-run make check

---

## KEY ALGORITHMS & COMPLEXITY

| Algorithm | Complexity | Phase | Implementation |
|-----------|-----------|-------|-----------------|
| Spawn agent | O(1) | 1 | gen_server:start_link/3 |
| Route request | O(log N) | 1 | gproc registry lookup |
| Correlation lookup | O(1) | 1 | maps:get/2 |
| Pool scaling | O(1) | 1 | dynamic min/max |
| Raft consensus | O(N) | 3 | broadcast + quorum |
| HNSW search | O(log N) | 4 | layer navigation |
| TLS handshake | O(1) | 5 | ssl:connect/2 |
| HMAC signing | O(1) | 5 | crypto:mac/4 |

---

## PERFORMANCE TARGETS

| Metric | Target | Method |
|--------|--------|--------|
| Agent spawn | <50ms | erlang-performance |
| Request routing p99 | <50ms | benchmark: 1000 requests |
| Raft consensus | <500ms | chaos: leader election timeout |
| HNSW search (1M patterns) | <100ms | benchmark: 10K searches |
| TLS handshake | <2s (local), <5s (cloud) | benchmark: 100 handshakes |
| Memory | <512MB baseline | MemoryAnalyzer |
| Availability | 99.9% | chaos: kill leader node |

---

## CRITICAL RULES (FROM CLAUDE.MD)

| # | Rule | Implication |
|---|------|------------|
| 1 | ¬done ⟺ ¬(compile ∧ test) | Quality gates MANDATORY |
| 2 | perf_changed → benchmark | Detect regression immediately |
| 3 | {errors=0, failures=0, coverage≥0.8, regression<0.1} | All quality metrics required |
| 4 | {gen_server, supervision, isolation, let-it-crash} | OTP patterns enforced |
| 5 | Output ⊆ {.erl, .hrl} unless explicit_request | No random file generation |
| 6 | cloud(command) → idempotent(command) | All commands cloud-safe |
| 7 | NEVER REBASE EVER | Merge-only, preserve history |
| 8 | ALWAYS use agents | Never work without Task() |
| 9 | NEVER USE --no-verify | Fix root cause, create new commit |

---

## GIT WORKFLOW (MERGE-ONLY)

### Feature Branch
```bash
git checkout -b feature/erlmcp-flow-complete
```

### After Each Phase
```bash
git add apps/erlmcp_flow/src/*.erl
git commit -m "feat(erlmcp-flow): Phase X - [Title]

[Detailed message]

https://claude.ai/code/session_XXXXX"

git push origin feature/erlmcp-flow-complete
```

### Final Merge to Main
```bash
git checkout main
git pull origin main
git merge --no-ff feature/erlmcp-flow-complete
git push origin main
```

**Critical Rules**:
- ✓ Merge commits (preserve history)
- ✓ Signed commits (git config)
- ✗ Rebase (rewrites history)
- ✗ Force push (destructive)
- ✗ --no-verify (bypasses quality)

---

## DOCUMENTS (COMPLETE REFERENCE)

| Document | Purpose | Length |
|----------|---------|--------|
| SPARC_ERLMCP_FLOW_SPECIFICATION.md | 5-phase specification with all details | 40 pages |
| SPARC_AGENT_ORCHESTRATION_GUIDE.md | How to orchestrate 20 agents | 25 pages |
| SPARC_IMPLEMENTATION_EXECUTION_PLAN.md | Day-by-day execution (Phase 1-5) | 30 pages |
| SPARC_QUICK_REFERENCE.md | This file - quick lookup | 3 pages |

---

## QUICK START: TRIGGER PHASE 1

**Command**: Spawn all 20 agents in 1 message

```erlang
%% ORCHESTRATION MESSAGE (all agents in parallel)

Task("Erlang Researcher",
     "Analyze erlmcp gen_server patterns, registry usage",
     "erlang-researcher").

Task("Erlang Architect",
     "Design 3-tier supervision tree for erlmcp-flow",
     "erlang-architect").

Task("Erlang OTP Developer",
     "Implement 13 Phase 1 modules (1570 LOC)",
     "erlang-otp-developer").

Task("Erlang Test Engineer",
     "Write 45 EUnit tests (Chicago TDD)",
     "erlang-test-engineer").

Task("Erlang Transport Builder",
     "Integrate transport layer with agent pool",
     "erlang-transport-builder").

Task("Code Reviewer",
     "Review OTP compliance and let-it-crash semantics",
     "code-reviewer").

Task("Erlang Performance",
     "Benchmark agent routing (target: <10ms p99)",
     "erlang-performance").

Task("Build Engineer",
     "Set up git branch and constrained writes",
     "build-engineer").

Task("Verifier",
     "Run quality gates (compile, test, coverage, dialyzer, xref)",
     "verifier").

%% ... 11 more agents for quality testing & verification ...
```

---

## SUCCESS = ALL GATES PASS

```
make check  [180s parallel]
  ├─ Compile: 0 errors ✓
  ├─ EUnit: failures = 0 ✓
  ├─ CT: pass_rate = 100% ✓
  ├─ Dialyzer: 0 warnings ✓
  ├─ Xref: 0 undefined ✓
  └─ Coverage: ≥80% ✓
```

---

## TIMELINE

```
Week 1:  Phase 1 (Agent Foundation) - 3 days
Week 2:  Phase 1 completion + Phase 2 start - 5 days
Week 3:  Phase 2/3 (Swarm + Consensus) - 5 days
Week 4:  Phase 4/5 (Intelligence + Security) - 5 days
Week 5:  Final integration + merge - 3 days

TOTAL: 21 days (3 weeks)
```

---

## CONTACT & ESCALATION

### If Quality Gate Fails
1. **DO NOT** use `--no-verify` (forbidden by CLAUDE.md rule 9)
2. Analyze root cause
3. Fix issue
4. Re-stage files
5. Create NEW commit (not amend)

### If Agent Gets Stuck
1. Use claims handoff protocol
2. Transfer to different agent type
3. Preserve progress context

### If Performance Regression
1. Profile bottleneck (erlang-performance)
2. Optimize critical path
3. Re-benchmark
4. Ensure regression < 10%

---

## FINAL VERIFICATION CHECKLIST

- [ ] All 63 modules compiling
- [ ] 259 tests passing
- [ ] Coverage ≥80% average
- [ ] Dialyzer: 0 warnings
- [ ] Xref: 0 undefined functions
- [ ] Performance: all targets met
- [ ] Security: audit passed
- [ ] Documentation: 100% of public APIs
- [ ] Merged to main (no rebase)
- [ ] Release ready

---

## REFERENCE LINKS

**Specification Document**:
`/home/user/erlmcp/SPARC_ERLMCP_FLOW_SPECIFICATION.md`
- Full requirements, pseudocode, architecture

**Orchestration Guide**:
`/home/user/erlmcp/SPARC_AGENT_ORCHESTRATION_GUIDE.md`
- How to orchestrate 20 agents, parallel execution

**Execution Plan**:
`/home/user/erlmcp/SPARC_IMPLEMENTATION_EXECUTION_PLAN.md`
- Day-by-day breakdown of all 5 phases

**Project Standards**:
`/home/user/erlmcp/CLAUDE.md`
- OTP compliance, TDD rules, quality gates

**Rebar Config**:
`/home/user/erlmcp/rebar.config`
- App structure, dependencies, compiler options

