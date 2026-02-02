# SPARC Agent Orchestration Guide
## erlmcp-flow Implementation Workflow

**Status**: Executable Orchestration Plan
**Version**: 3.0.0
**Target**: 5 Phases, 20 Agent Workers, 4-minute Cloud Gate

---

## THE GOLDEN RULE: 1 MESSAGE = ALL OPERATIONS

```
For non-trivial tasks (5+ files, 3+ systems):
  ✓ Spawn 20+ agents in SINGLE message
  ✓ Batch 20+ todos together
  ✓ Read/Edit 20+ files together
  ✗ NEVER sequential operations

Expected Speedup: 2.8x - 4.4x (EPIC 9 Workflow)
```

---

## PHASE 1: AGENT FOUNDATION (Days 1-3)

### 1.1 Orchestration Message: Specification

**Parallel Task Batch** (all 20 agents):

```
================================================================================
PHASE 1 SPECIFICATION: AGENT FOUNDATION
================================================================================

TASK: Erlang Researcher
  Description: Analyze erlmcp codebase for gen_server patterns, registry usage,
               supervision tree structure. Document findings for agent design.
  Agent: erlang-researcher
  Expected Output: Research report (15 pages), code patterns (20 snippets)
  Success Criteria: Identified ≥10 gen_server patterns, registry patterns clear

TASK: Plan Designer
  Description: Design Phase 1 implementation plan with 13 modules, dependencies,
               git branching strategy, integration points.
  Agent: plan-designer
  Expected Output: Implementation plan (Markdown), module dependency graph
  Success Criteria: All 13 modules listed, dependencies clear, no cycles

TASK: Erlang Architect
  Description: Design 3-tier supervision tree for erlmcp-flow. Define:
               - TIER 1: Connection pool (ranch/cowboy)
               - TIER 2: Agent pool (gen_server per connection)
               - TIER 3: Routing & monitoring (gproc + health checks)
  Agent: erlang-architect
  Expected Output: Architecture diagram (SVG), supervision tree (text)
  Success Criteria: Diagram complete, no blocking issues, safety verified

TASK: Erlang OTP Developer
  Description: Implement Phase 1 agent foundation:
               1. erlmcp_flow_agent.erl (gen_server) - 250 LOC
               2. erlmcp_flow_agent_sup.erl (supervisor) - 80 LOC
               3. erlmcp_flow_registry.erl (gproc wrapper) - 150 LOC
               4. erlmcp_flow_pool_manager.erl (scaling) - 200 LOC
               5. erlmcp_flow_request_tracker.erl (correlation) - 120 LOC
  Agent: erlang-otp-developer
  Expected Output: 5 modules, compiling, basic tests pass
  Success Criteria: No compilation errors, gen_server callbacks correct

TASK: Erlang Test Engineer
  Description: Write Chicago TDD tests for Phase 1:
               - Test 1: Agent spawn (O(1) latency)
               - Test 2: Request correlation (UUID tracking)
               - Test 3: Pool scaling (min=5, max=100)
               - Test 4: Request timeout handling
               - Test 5: Agent heartbeat (5s intervals)
  Agent: erlang-test-engineer
  Expected Output: 45 EUnit tests (erlmcp_flow_*_tests.erl)
  Success Criteria: Tests written BEFORE code (TDD red-green-refactor)

TASK: Erlang Transport Builder
  Description: Integrate transport layer with agent pool:
               - stdio → erlmcp_flow_agent
               - TCP (ranch) → pool_manager
               - HTTP → connection handler
               - WebSocket → upgrade path
  Agent: erlang-transport-builder
  Expected Output: Transport adapter modules, integration complete
  Success Criteria: Transport → agent pool messages flowing correctly

TASK: Code Reviewer
  Description: Review Phase 1 code for:
               - OTP compliance (gen_server callbacks, initialization)
               - let-it-crash semantics (no catches, error propagation)
               - Concurrency safety (no shared mutable state)
               - Idempotency (all operations side-effect free)
  Agent: code-reviewer
  Expected Output: Review report (20 items), approval checklist
  Success Criteria: No critical issues, ≥90% approval

TASK: Erlang Performance
  Description: Benchmark Phase 1 agent routing:
               - Measure: Agent spawn latency (target: <50ms)
               - Measure: Request routing latency (target: <10ms p99)
               - Measure: Registry lookup (target: O(log N), <100µs)
               - Establish baseline for regression testing
  Agent: erlang-performance
  Expected Output: Benchmark report (charts, numbers)
  Success Criteria: All targets met, baseline established

TASK: Build Engineer
  Description: Set up constrained writes for Phase 1 modules:
               - Create new git branch: feature/erlmcp-flow-phase1
               - Add modules to rebar.config
               - Create app files (.app.src)
               - Ensure compilation order correct
  Agent: build-engineer
  Expected Output: Buildable codebase, rebar3 compile succeeds
  Success Criteria: make compile → 0 errors

TASK: Verifier
  Description: Run quality gates for Phase 1:
               1. Compilation: rebar3 compile
               2. EUnit: rebar3 eunit
               3. Dialyzer: rebar3 dialyzer
               4. Xref: rebar3 xref
               5. Coverage: coverage report ≥80%
  Agent: verifier
  Expected Output: Test results (pass/fail), coverage report
  Success Criteria: All tests pass, coverage ≥80%, 0 Dialyzer warnings

TASK: Agent-01-Compile-Gate
  Description: First quality gate - ensure compilation without errors
  Agent: agent-01-compile-gate
  Expected Output: Compilation log, error summary
  Success Criteria: Errors = 0

TASK: Agent-06-Test-EUnit
  Description: Execute EUnit tests for Phase 1 (45 tests)
  Agent: agent-06-test-eunit
  Expected Output: Test results, failure report
  Success Criteria: Failures = 0, coverage ≥80%

TASK: Agent-07-Test-CT
  Description: Execute Common Test integration tests (10 tests)
  Agent: agent-07-test-ct
  Expected Output: CT report.html, trace logs
  Success Criteria: Pass rate = 100%

TASK: Agent-11-Coverage
  Description: Generate code coverage report, ensure ≥80%
  Agent: agent-11-coverage
  Expected Output: Coverage report (HTML), coverage percentages
  Success Criteria: Coverage ≥80% on all Phase 1 modules

TASK: Agent-12-Dialyzer
  Description: Run Dialyzer type checker for Phase 1
  Agent: agent-12-dialyzer
  Expected Output: Dialyzer report, warnings list
  Success Criteria: Warnings = 0

TASK: Agent-13-Xref
  Description: Run Xref cross-reference analysis for Phase 1
  Agent: agent-13-xref
  Expected Output: Xref report, undefined function list
  Success Criteria: Undefined functions = 0

TASK: Agent-14-Format
  Description: Format Phase 1 code to standard (rebar3 format)
  Agent: agent-14-format
  Expected Output: Formatted source files
  Success Criteria: Code style consistent

TASK: Agent-15-Benchmark
  Description: Run benchmarks, detect regression
  Agent: agent-15-benchmark
  Expected Output: Benchmark results, regression report
  Success Criteria: Regression < 10%

TASK: Erlang GitHub Ops
  Description: Commit Phase 1 work and prepare for merge:
               - Stage all Phase 1 modules
               - Create commit message (signed)
               - Push to feature/erlmcp-flow-phase1
               - Prepare for merge-only PR (NEVER rebase, NEVER --no-verify)
  Agent: erlang-github-ops
  Expected Output: Git log, commit hash
  Success Criteria: Commit pushed, ready for PR

================================================================================
```

### 1.2 TodoList: Phase 1 Specification

```
TODO BATCH: Phase 1 Specification
================================================================================

[in_progress] Research erlmcp codebase for gen_server patterns
  activeForm: "Researching erlmcp codebase for gen_server patterns"

[pending] Design 3-tier supervision tree
  activeForm: "Designing 3-tier supervision tree"

[pending] Write 45 EUnit tests (Chicago TDD)
  activeForm: "Writing 45 EUnit tests (Chicago TDD)"

[pending] Implement 13 Phase 1 modules (~1570 LOC)
  activeForm: "Implementing 13 Phase 1 modules"

[pending] Integrate transport layer with agent pool
  activeForm: "Integrating transport layer with agent pool"

[pending] Review OTP compliance and let-it-crash semantics
  activeForm: "Reviewing OTP compliance and let-it-crash semantics"

[pending] Benchmark agent routing (spawn, request, lookup)
  activeForm: "Benchmarking agent routing"

[pending] Run quality gates (compile, test, coverage, dialyzer, xref)
  activeForm: "Running quality gates"

[pending] Commit Phase 1 work (merge-only, no rebase)
  activeForm: "Committing Phase 1 work"

================================================================================
```

### 1.3 File Operations: Phase 1 Specification

**Read Files** (parallel):
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl` (gen_server pattern)
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_registry.erl` (registry pattern)
- `/home/user/erlmcp/CLAUDE.md` (project standards)
- `/home/user/erlmcp/rebar.config` (app configuration)

**Create Directories**:
- `/home/user/erlmcp/apps/erlmcp_flow/src/` (source code)
- `/home/user/erlmcp/apps/erlmcp_flow/test/` (test files)
- `/home/user/erlmcp/apps/erlmcp_flow/include/` (header files)

---

## PHASE 2: SWARM COORDINATION (Days 4-8)

### 2.1 Orchestration Message: Pseudocode + Architecture

```
================================================================================
PHASE 2 SWARM COORDINATION: PSEUDOCODE & ARCHITECTURE
================================================================================

TASK: Erlang Researcher
  Description: Analyze topology algorithms (mesh, hierarchical, ring, star).
               Study gossip protocols for state propagation.
  Agent: erlang-researcher
  Expected Output: Algorithm comparison (2000 words), implementation hints

TASK: Erlang Architect
  Description: Design swarm coordination layer:
               - Topology selection (config-driven)
               - Gossip message format and propagation
               - Heartbeat monitoring (5s intervals)
               - Failure detection and recovery
  Agent: erlang-architect
  Expected Output: Component diagram, interface specification

TASK: Erlang OTP Developer
  Description: Implement Phase 2 swarm modules:
               1. erlmcp_flow_swarm.erl (orchestration) - 280 LOC
               2. erlmcp_flow_topology_*.erl (4 modules) - 460 LOC
               3. erlmcp_flow_gossip.erl (propagation) - 220 LOC
               4. Remaining modules (heartbeat, failover, ordering) - 430 LOC
  Agent: erlang-otp-developer
  Expected Output: 12 modules, compiling, integration with Phase 1

TASK: Erlang Test Engineer
  Description: Write 38 EUnit + 15 CT tests for Phase 2:
               - Topology initialization and routing
               - Gossip message delivery (eventual consistency)
               - Heartbeat timeout and failover
               - Partition healing
  Agent: erlang-test-engineer
  Expected Output: 53 test cases (erlmcp_flow_swarm_tests.erl, etc.)

[... 17 more tasks (same pattern as Phase 1) ...]
```

### 2.2 Integration Points

```erlang
%% Phase 1 → Phase 2 Integration
erlmcp_flow_agent:add_observer(erlmcp_flow_swarm),
erlmcp_flow_swarm:subscribe_topology_changes(erlmcp_flow_agent_pool),
erlmcp_flow_registry:link_to_swarm(erlmcp_flow_swarm),

%% Phase 2 Gossip: Agent → Swarm → Peers
erlmcp_flow_agent:on_state_change(NewState) →
  erlmcp_flow_gossip:broadcast(state_change, NewState) →
    [send_to_peer(Peer, NewState) || Peer <- get_neighbors()].
```

---

## PHASE 3: CONSENSUS (Days 9-13)

### 3.1 Key Algorithms to Verify

```erlang
%% BEFORE implementing Phase 3, verify:
%% 1. Phase 1 agents can send messages (✓ Phase 1)
%% 2. Phase 2 swarm maintains topology (✓ Phase 2)
%% 3. Raft requires: term, log, voted_for (implement Phase 3)

PHASE_3_BLOCKERS_RESOLVED =
  Phase1_Complete AND
  Phase2_Topology_Stable AND
  ElectionTimeout_Randomized_150_300ms.
```

### 3.2 Critical Safety Properties

```
SAFETY (no split-brain):
  ∀term. ∃!leader(term)  % Only one leader per term
  log_consistency: leader.log ⊇ follower.log  % Strong consistency

LIVENESS (progress):
  ∀timeout. ∃new_leader_elected < 2s  % Fast failover
  ∀msg. delivered(msg) < 5s  % Message delivery
```

---

## PHASE 4: INTELLIGENCE & ROUTING (Days 14-17)

### 4.1 Prerequisites

```
Phase 4 depends on Phase 1 (agent pool) for specialist agents:
  - Agent pool can spawn 8-16 specialist experts
  - Each expert processes domain-specific requests
  - MoE router selects experts via gating network
```

### 4.2 HNSW Index Setup

```erlang
%% HNSW Configuration
Config = #{
    max_neighbors => 8,           % M parameter
    max_layers => 16,             % mL parameter
    ef => 200,                    % Search parameter
    embedding_dim => 384,         % ALL-MiniLM-L6-v2
    metric => cosine_distance
}.

%% Index operations: O(log N) with HNSW
index:add(pattern) → {ok, id}.         %% O(log N)
index:search(query, topK=10) → [docs].  %% O(log N + k)
```

---

## PHASE 5: SECURITY & OBSERVABILITY (Days 18-21)

### 5.1 TLS Configuration

```erlang
%% TLS 1.3 only (STRICT)
TLSConfig = #{
    versions => [tlsv1_3],
    cert_file => "/path/to/cert.pem",
    key_file => "/path/to/key.pem",
    verify => verify_peer,
    ca_file => "/path/to/ca-bundle.pem",
    ciphers => [
        {aes_256_gcm, chacha20_poly1305},
        {aes_128_gcm}
    ]
}.

%% HMAC-SHA256 Signing
hmac:sign(Data, Secret) → Signature.
hmac:verify(Data, Signature, Secret) → ok | {error, invalid}.
```

### 5.2 Rate Limiting

```erlang
%% Token bucket: O(1) per request
RateLimitConfig = #{
    requests_per_second => 1000,
    burst_size => 100,
    timeout => 5000
}.

rate_limiter:allow(Connection) → ok | {error, rate_limit_exceeded}.
```

### 5.3 OTEL Tracing

```erlang
%% Distributed tracing with parent-child links
Trace = otel:start_trace(operation_name, #{parent => ParentTraceId}),
otel:add_event(Trace, 'request_received', #{method => Method}),
otel:end_trace(Trace).
```

---

## ORCHESTRATION CHECKLIST

### Pre-Orchestration (Day 0)

- [ ] Create feature branch: `git checkout -b feature/erlmcp-flow-complete`
- [ ] Create `.claude-flow/` agents directory (if not exists)
- [ ] Ensure OTP 28.3.1 installed: `erl -version`
- [ ] Verify Makefile can run: `make verify-fast` (should pass)
- [ ] Confirm 20 agent workers available in `.claude/agents/`
- [ ] Set `CLAUDE_CODE_REMOTE=true` (for cloud execution)

### Phase 1 Execution (Days 1-3)

```bash
# TRIGGER PARALLEL ORCHESTRATION
# All 20 agents spawn in 1 message (see Orchestration Message above)

# Monitor progress
watch -n 5 'ls -la apps/erlmcp_flow/src/*.erl | wc -l'

# Verify gate status
make check  # compile + eunit + ct + dialyzer + xref (180s parallel)
```

### Phase 1 Completion (Day 3 EOD)

```bash
# Expected artifacts
✓ apps/erlmcp_flow/src/*.erl (13 modules, ~1570 LOC)
✓ apps/erlmcp_flow/test/*_tests.erl (45 EUnit tests)
✓ log/eunit/*.log (test results)
✓ log/ct/report.html (integration tests)
✓ dialyzer.log (0 warnings)
✓ xref.log (0 undefined)
✓ coverage.html (≥80%)

# Commit Phase 1
git add apps/erlmcp_flow/
git commit -m "feat(erlmcp-flow): Phase 1 - Agent Foundation

Implements:
- gen_server per connection (Process-per-Connection)
- gproc-based registry (O(log N) routing)
- Agent pool with dynamic scaling (min=5, max=100)
- Request correlation with UUID tracking
- Heartbeat monitoring and health checks

Tests:
- 45 EUnit tests (agent spawn, routing, pooling)
- 10 Common Test integration tests
- Coverage: 92%
- Dialyzer: 0 warnings
- Xref: 0 undefined functions

https://claude.ai/code/session_XXXXX"

git push origin feature/erlmcp-flow-complete
```

### Phase 2 Execution (Days 4-8)

Same pattern as Phase 1, but with 12 new modules and 53 tests.

### Final Integration (Day 21)

```bash
# All phases complete and integrated
make check  # Full suite: 259 tests, <4 min

# Artifacts
✓ ~8,410 LOC production code
✓ ~250 EUnit tests
✓ ~85 Common Test suites
✓ ~80%+ coverage
✓ 0 Dialyzer warnings
✓ 0 Xref undefined functions
✓ All benchmarks passing (<10% regression)

# Create release
rebar3 as prod release

# Deploy
./erlmcp-flow start
```

---

## AGENT AVAILABILITY TABLE

| Agent | Phase 1 | Phase 2 | Phase 3 | Phase 4 | Phase 5 | Total |
|-------|---------|---------|---------|---------|---------|-------|
| erlang-researcher | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| erlang-architect | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| erlang-otp-developer | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| erlang-test-engineer | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| erlang-transport-builder | ✓ | ✓ | - | - | ✓ | 3 |
| code-reviewer | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| erlang-performance | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| build-engineer | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| verifier | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| agent-01-compile-gate | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| agent-06-test-eunit | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| agent-07-test-ct | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| agent-11-coverage | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| agent-12-dialyzer | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| agent-13-xref | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| agent-14-format | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| agent-15-benchmark | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| erlang-github-ops | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| sparc-orchestrator | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |
| plan-designer | ✓ | ✓ | ✓ | ✓ | ✓ | 5 |

---

## SUCCESS METRICS

### Per-Phase Metrics

```
Phase 1: 13 modules, 1570 LOC, 45 EUnit, 92% coverage
Phase 2: 12 modules, 1470 LOC, 38 EUnit, 88% coverage
Phase 3: 11 modules, 1540 LOC, 52 EUnit, 95% coverage
Phase 4: 14 modules, 2000 LOC, 41 EUnit, 85% coverage
Phase 5: 13 modules, 1830 LOC, 36 EUnit, 98% coverage

TOTAL:
  ✓ 63 modules
  ✓ 8410 LOC
  ✓ 212 EUnit tests
  ✓ 85+ Common Test suites
  ✓ 80%+ average coverage
  ✓ <4 min cloud gate
```

### Quality Gates (All Required)

```
✓ Compile: errors = 0
✓ EUnit: failures = 0
✓ Common Test: pass_rate = 100%
✓ Coverage: ≥80% across all modules
✓ Dialyzer: warnings = 0
✓ Xref: undefined_functions = {}
✓ Benchmarks: regression < 10%
✓ Security: TLS 1.3, HMAC-SHA256, AES-256
✓ Performance: p99 latency < 100ms
✓ Documentation: 100% public APIs
```

---

## TROUBLESHOOTING

### If Agent Fails Mid-Phase

```erlang
%% NEVER restart agent in-place
%% Use handoff protocol instead

claims:handoff(issue_id, from => AgentA, to => AgentB),
AgentB:accept_handoff(issue_id).
```

### If Quality Gate Fails

```bash
# DO NOT use --no-verify (forbidden by CLAUDE.md rule 9)
# Instead: fix root cause, re-stage, create new commit

git add <modified files>
git commit -m "fix: ..."  # New commit, not amend
```

### If Network Partition During Push

```bash
# Idempotent push (safe to retry)
git push origin feature/erlmcp-flow-complete

# Verify remote
git fetch origin
git log origin/feature/erlmcp-flow-complete
```

---

## NEXT STEPS

1. **Freeze this specification** (sign off Phase 1 requirements)
2. **Trigger orchestration message** (spawn 20 agents in 1 message)
3. **Monitor agent progress** (daily standups)
4. **Verify quality gates** (continuous gate checks)
5. **Merge to main** (merge-only, never rebase)

