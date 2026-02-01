# erlmcp Codebase Research Summary - Makefile Refactoring Patterns

## Executive Summary
The erlmcp codebase demonstrates sophisticated coordination patterns across:
- **4-layer Governance System** (Policy → Execution → Verification → Receipt)
- **OTP Supervision Trees** (3-tier hierarchical model)
- **Hook-Based State Management** (SessionStart → PostToolUse → Receipt)
- **Incremental Change Detection** (SHA256-based file tracking)
- **Work Order Protocol** (Kanban-limited async coordination)

These patterns can be directly applied to Makefile refactoring.

---

## 1. PATTERN DISCOVERY

### 1.1 Shell Script Organization

**File Structure:**
- `./.claude/hooks/` (11 hooks, 1895 LOC)
  - SessionStart.sh (425 LOC) - OTP bootstrap & cache management
  - policy-*.sh (3 pre-commit validators)
  - post-*.sh (async CI, task validation)
  - receipt.sh (session receipt generation)

- `./tools/` (40+ scripts)
  - quality-gate-enforcer.sh - Enforcement validation
  - test-runner.sh, coverage-checker.sh - Quality gates
  - auto-fix/ (11 scripts) - Jidoka auto-repair orchestration
  - incremental/ (5 scripts) - Change detection & cost estimation
  - metrics/ (quality reporting)
  - bench/ (performance benchmarks)

- `./scripts/` (50+ scripts)
  - build_and_test.sh - Orchestration template
  - test/*.sh (smoke.sh, quick.sh, full.sh) - Test tiers
  - bench/*.sh (performance validation)
  - dev/doctor.sh (environment health check)

**Pattern: Hierarchical Script Organization**
```
Entry Point (Makefile)
  ↓
Orchestrator (./tools/auto-fix/orchestrator.sh)
  ├─ Gate Dispatcher (gate-failure-dispatcher.sh)
  │  ├─ Syntax Fix Agent (syntax-fix-agent.sh)
  │  ├─ Type Fix Agent (type-fix-agent.sh)
  │  ├─ Test Fix Agent (test-fix-agent.sh)
  │  ├─ Coverage Fix Agent (test-coverage-agent.sh)
  │  ├─ XRef Fix Agent (xref-fix-agent.sh)
  │  └─ Performance Agent (performance-agent.sh)
  └─ Validation (post-*.sh hooks)
```

### 1.2 Coordination Mechanisms

**A. Settings-Based Configuration (./.claude/settings.json v3.1.0)**

```json
{
  "hooks": {
    "SessionStart": [...],      // Initialization (OTP, cache)
    "PreToolUse": [...],        // Pre-execution policies
    "PostToolUse": [...],       // Post-execution validation
    "Stop": [...],              // Quality verification
    "SessionEnd": [...]         // Receipt generation
  },
  "subagents": {
    "verifier": {...},          // Read-only verification
    "build-engineer": {...},    // Constrained writes
    "release-scout": {...}      // Research specialist
  },
  "environment": {
    "ERLMCP_PROFILE": "cloud",
    "ERLMCP_CACHE": "/home/user/erlmcp/.erlmcp/cache/",
    "TERM": "dumb"
  }
}
```

**B. State Management via JSON Files**

- `.erlmcp/cache/sessionstart.lock` - Session bootstrap lock
- `.erlmcp/cache/file-hashes.json` - Incremental change tracking
- `.erlmcp/cache/changes.json` - Detected changes (added/modified/removed)
- `.erlmcp/receipts/*.json` - Session execution receipts
- `.erlmcp/transcripts/*.log` - Audit trail

**C. Work Order Protocol Pattern** (CLAUDE.md §Agent Orchestration)

Work order structure (JSON-serializable):
```erlang
#{
    id => binary(),                    % Unique ID
    task => binary(),                  % "Implement X" | "Test Y"
    agent => atom(),                   % erlang_otp_developer, etc
    priority => high | normal | low,
    dependencies => [work_order_id()], % Blocking dependencies
    constraints => #{
        time_budget => pos_integer(),
        cost_budget => float(),
        files => [file_path()],        % File-level locks
        branch => binary()
    },
    status => queued | wip | done | failed,
    result => #{
        exit_code => integer(),
        gates => gate_results(),
        artifacts => [file_path()],
        duration => pos_integer()
    }
}
```

**Kanban Principle:**
- ∀agent. |WIP(agent)| ≤ 1 (single-tasking for quality)
- Coordination via file locking and dependency graphs
- NO concurrent execution of blocking tasks

### 1.3 App Interdependencies

**Dependency Graph** (rebar.config:18-50):
```
erlmcp_core
  ├─ jsx (JSON codec)
  ├─ jesse (JSON Schema validation)
  ├─ gproc (process registry, O(log N))
  ├─ jose (JWT validation)
  ├─ bbmustache (templates)
  └─ opentelemetry_* (tracing)

erlmcp_transports
  └─ erlmcp_core (depends on core)
  └─ gun, ranch, cowboy (HTTP/TCP/WS)

erlmcp_observability
  └─ erlmcp_core (depends on core)
  └─ opentelemetry (metrics/tracing)

erlmcp_validation
  ├─ erlmcp_core
  └─ erlmcp_transports (spec compliance)
```

**Compilation Order Constraint:**
```
erlmcp_core (no deps on other apps)
  → erlmcp_transports (depends on core)
  → erlmcp_observability (depends on core)
  → erlmcp_validation (depends on core, transports)
```

### 1.4 OTP Supervision Patterns

**3-Tier Supervision Tree** (erlmcp_sup.erl:136-150):

```
TIER 1: Core (registry + infrastructure)
  erlmcp_sup (one_for_one)
    └─ erlmcp_core_sup
        ├─ erlmcp_registry (gproc-based process discovery)
        ├─ erlmcp_cache
        ├─ erlmcp_session_manager
        └─ erlmcp_auth

TIER 2: Protocol (servers, isolated per-connection)
  erlmcp_server_sup (simple_one_for_one)
    ├─ erlmcp_server (pid1)
    ├─ erlmcp_server (pid2)
    └─ erlmcp_server (pidN)

TIER 3: Observability (isolated, doesn't affect core)
  erlmcp_observability_sup
    ├─ erlmcp_health_monitor
    ├─ erlmcp_metrics_server
    └─ erlmcp_dashboard
```

**Strategy: one_for_one** - No cascading failures
- Core failure → only core restarts, not transports
- Server failure → isolated, doesn't affect other servers
- Observability failure → doesn't affect core protocol

---

## 2. EXISTING INFRASTRUCTURE

### 2.1 Build/Test Runners

**Quality Gate Hierarchy** (Makefile:370-595):

| Gate | Implementation | Blocking | Time | Cost |
|------|---|---|---|---|
| compile | `rebar3 compile` | ✅ | 30s | $0.01 |
| eunit | `rebar3 eunit` | ✅ | 60s | $0.02 |
| ct | `rebar3 ct` | ✅ | 120s | $0.04 |
| dialyzer | `rebar3 dialyzer` | ✅ | 90s | $0.03 |
| xref | `rebar3 xref` | ⚠️ | 30s | $0.01 |
| coverage | `rebar3 cover` | ✅ | 30s | $0.01 |
| benchmark | `./scripts/bench/quick.sh` | ⚠️ | 300s | (local) |

**Makefile Targets** (Makefile:1-11, 371):
```makefile
validate: validate-profile validate-compile validate-test \
          validate-coverage validate-quality validate-bench
quick: doctor compile smoke-tests
verify: compile xref dialyzer test
ci-local: compile xref dialyzer eunit ct coverage
```

**Test Tier System** (Makefile:284-295):
```makefile
test-smoke    # ≤2 min: codec, lifecycle, transport basics
test-quick    # ≤10 min: smoke + core integration
test-full     # Full suite: EUnit + CT + coverage
```

### 2.2 Hook System Implementation

**Hook Lifecycle** (.claude/settings.json:31-146):

```
SessionStart (600s timeout)
  ↓
  ./.claude/hooks/SessionStart.sh
    Phase 1: Cache check (O(1) if cached)
    Phase 2: Download/Build OTP 28.3.1 (30s or 6m)
    Phase 3: Verify build
    Phase 4: Cache binaries
    Phase 5: Environment setup
    Phase 6: Lock file creation
  ↓
[Main Task Execution]
  ↓
PreToolUse Hooks (10s timeout)
  - policy-bash.sh (network + filesystem governance)
  - policy-write.sh (file pattern validation)
  - policy-websearch.sh (domain filtering)
  ↓
[Tool Execution]
  ↓
PostToolUse Hooks (120s timeout, ASYNC)
  - post-write-ci.sh (incremental tests)
  - post-git-commit.sh (audit logging)
  ↓
Stop Hook (120s timeout)
  - Verification subagent (read-only tests)
  ↓
SessionEnd Hook (30s timeout)
  - receipt.sh (generate session receipt)
```

### 2.3 Quality Gates Implementation

**Example: Validation Gate** (Makefile:403-420):

```makefile
validate-compile:
    @echo "🔨 Quality Gate: Compilation"
    @echo "  Target: 0 compilation errors"
    @if TERM=dumb rebar3 compile 2>&1 | tee /tmp/erlmcp_compile.log; then
        echo "✅ Compilation passed - 0 errors"
    else
        echo "❌ COMPILATION FAILED"
        cat /tmp/erlmcp_compile.log
        exit 1
    fi
```

### 2.4 Session/State Management

**Cache Management** (tools/incremental/cache-manager.sh):
```bash
# Persistent cache locations
.erlmcp/cache/                       # Session cache (24h)
.erlmcp/cache/file-hashes.json       # File hash tracking
.erlmcp/cache/changes.json           # Incremental changes
.erlmcp/otp-28.3.1/bin/erl          # Pre-built OTP
```

**Change Detection** (tools/incremental/detect-changes.sh:26-78):
```bash
# Algorithm:
# 1. Compute SHA256 hashes of all .erl/.hrl/.app.src files
# 2. Compare with previous run via .erlmcp/cache/file-hashes.json
# 3. Output: .changed[], .added[], .removed[], .all_changed
```

---

## 3. CODEBASE ARCHITECTURE

### 3.1 App Dependencies

**erlmcp_core** (97 modules, foundation with no dependencies on other erlmcp apps)
**erlmcp_transports** (23 modules, depends on erlmcp_core)
**erlmcp_observability** (31 modules, isolated from transports, depends only on core)
**erlmcp_validation** (13 modules, depends on core + transports)

### 3.2 Module Interdependencies

```
erlmcp_core (foundation, no deps on other erlmcp apps)
  │
  ├─ Protocol: erlmcp_json_rpc → erlmcp_client/server
  ├─ Session: erlmcp_session_manager → erlmcp_session_backend
  ├─ Security: erlmcp_auth → erlmcp_secrets
  └─ Discovery: erlmcp_registry (gproc-based)

erlmcp_transports (depends on core)
  └─ Transport Layer: erlmcp_transport_sup → erlmcp_registry

erlmcp_observability (depends on core, ISOLATED from transports)
  └─ Monitoring: erlmcp_otel, erlmcp_health_monitor

erlmcp_validation (depends on core + transports)
  └─ Compliance: erlmcp_protocol_validator, erlmcp_transport_validator
```

### 3.3 OTP Testing Pattern: Chicago TDD

**Real Processes, No Mocks** (apps/erlmcp_core/test/erlmcp_registry_tests.erl:21-35):

```erlang
registry_test_() ->
    {foreach,
     fun setup/0,        %% Start REAL registry process
     fun cleanup/1,      %% Cleanup REAL processes
     [
         fun test_registry_startup/1,
         fun test_server_registration/1,
         fun test_concurrent_registration/1
     ]}.
```

---

## 4. CLAUDE.MD PATTERNS

### 4.1 Work Order Protocol

```erlang
-type work_order() :: #{
    id := binary(),                    % UUID
    task := binary(),                  % "Implement X", "Test Y", "Review Z"
    agent := atom(),                   % erlang_otp_developer, erlang_test_engineer
    priority := high | normal | low,
    dependencies := [work_order_id()], % Blocking dependencies
    constraints := #{
        time_budget => pos_integer(),  % Seconds
        cost_budget => float(),        % USD
        files => [file_path()],        % File-level locks
        branch => binary()             % Auto-managed git branch
    },
    status := queued | wip | done | failed,
    result := #{
        exit_code => integer(),
        gates => gate_results(),
        artifacts => [file_path()],
        duration => pos_integer()
    }
}.
```

### 4.2 Kanban Coordination

**Kanban Limits: ∀agent. |WIP(agent)| ≤ 1**

Work order lifecycle:
1. Queue → Write to .erlmcp/work-orders/queue.json
2. Acquire → Agent checks Kanban limit → move to WIP
3. Execute → Run task on unique branch
4. Complete → Write results to completed.json
5. Merge → Rebase onto main

Example execution:
```
t=0s:   wo-001 (architect) starts
t=120s: wo-001 done → wo-002 (developer) + wo-003 (test-engineer) start (parallel)
t=300s: wo-002, wo-003 done → wo-004 (github-ops) starts
t=360s: wo-004 done (PR created) → wo-005 (reviewer) starts
t=420s: wo-005 done → Report to user
```

### 4.3 Cloud vs Local Semantics

**Parallel Execution Strategy:**
```makefile
# Sequential: compile(30s) → dialyzer(90s) → xref(30s) → eunit(60s) → ct(120s)
# = 330s

# Parallel: compile(30s) → max(dialyzer, xref, eunit, ct)(120s)
# = 150s (2.2x speedup)

.PHONY: check-parallel
check-parallel: compile
    @$(MAKE) -j4 dialyzer xref eunit ct coverage
```

### 4.4 Governance Hooks (4-Layer Architecture)

```
Layer 1: Policy (hooks/settings.json)
  ├─ SessionStart (OTP 28.3.1 bootstrap)
  ├─ PreToolUse (network/filesystem governance)
  └─ PostToolUse (async validation)

Layer 2: Execution (main task)
  └─ Tool invocations

Layer 3: Verification (Stop hook)
  └─ Subagent verification (read-only tests)

Layer 4: Receipt (SessionEnd hook)
  └─ Session receipt generation
```

---

## 5. REFACTORING OPPORTUNITIES

### 5.1 Work Order Protocol → Makefile

Introduce work order semantics for dependency management:

```makefile
.PHONY: validate-with-workorders
validate-with-workorders:
    @./tools/work-order-orchestrator.sh orchestrate \
        --work-orders "$(WO_COMPILE)" "$(WO_EUNIT)" "$(WO_CT)" \
        --max-wip 1 \
        --output .erlmcp/work-orders/results.json
```

### 5.2 Kanban Limits → Makefile Parallelism

Constrain to WIP ≤ 1 for critical gates:

```makefile
.PHONY: validate-wip-limited
validate-wip-limited:
    @# Phase 1: Sequential gates
    @for gate in $(GATE_GROUP_SEQUENTIAL); do \
        $(MAKE) $$gate || exit 1; \
    done
    @# Phase 2: Parallel gates
    @$(MAKE) -j2 $(GATE_GROUP_PARALLEL) || exit 1
```

### 5.3 Supervision Patterns → Makefile Dependencies

Hierarchical target dependencies with isolation:

```makefile
# TIER 1: Core
.PHONY: core-gates
core-gates: validate-compile validate-test
    @echo "✓ Core gates passed"

# TIER 2: Transports (depends on core)
.PHONY: transport-gates
transport-gates: core-gates
    @$(MAKE) test-transports

# TIER 3: Observability (isolated)
.PHONY: observability-gates
observability-gates:
    @$(MAKE) test-observability || echo "⚠ Observability tests skipped"

# Master gate (one_for_one: failure isolation)
.PHONY: validate-hierarchical
validate-hierarchical: core-gates transport-gates observability-gates
    @echo "✅ All hierarchical gates passed"
```

### 5.4 Hook Integration → Makefile

Extensible pre/post target execution:

```makefile
PRE_COMPILE_HOOKS := ./.claude/hooks/pre-compile-otp28.sh \
                      ./.claude/hooks/policy-bash.sh
POST_COMPILE_HOOKS := ./.claude/hooks/post-write-ci.sh

.PHONY: compile-with-hooks
compile-with-hooks:
    @for hook in $(PRE_COMPILE_HOOKS); do \
        [[ -x $$hook ]] && $$hook || true; \
    done
    @TERM=dumb rebar3 compile
    @for hook in $(POST_COMPILE_HOOKS); do \
        [[ -x $$hook ]] && $$hook & \
    done
```

### 5.5 Incremental Testing → Makefile

Cost-optimized CI workflow:

```makefile
.PHONY: test-changed
test-changed: detect-changes
    @./tools/incremental/detect-changes.sh
    @CHANGES=$$(cat .erlmcp/cache/changes.json | jq -r '.changed[]' | wc -l); \
    if [[ $$CHANGES -eq 0 ]]; then \
        echo "✓ No changes, skipping tests"; \
    else \
        echo "Running tests for $$CHANGES changed files..."; \
        ./tools/incremental/select-gates.sh .erlmcp/cache/changes.json; \
    fi
```

---

## 6. KEY FILES WITH LINE REFERENCES

### Configuration & Governance
- **CLAUDE.md** (full specification, §Agent Orchestration ~730)
- **./.claude/settings.json** (303 LOC, hooks at lines 31-146)
- **rebar.config** (290 LOC, dependencies at lines 18-50)
- **Makefile** (1500 LOC, gates at lines 252-595)

### Hook Implementation
- **./.claude/hooks/SessionStart.sh** (425 LOC, orchestration at lines 337-412)
- **./.claude/hooks/policy-bash.sh** (204 LOC)
- **./.claude/hooks/receipt.sh** (195 LOC)

### Shell Scripts
- **./tools/auto-fix/orchestrator.sh** (iteration loop)
- **./tools/incremental/detect-changes.sh** (161 LOC, detection at lines 26-78)

### Erlang Code
- **apps/erlmcp_core/src/erlmcp_sup.erl** (200+ LOC, supervision at lines 136-150)
- **apps/erlmcp_core/test/erlmcp_registry_tests.erl** (Chicago TDD at lines 21-35)

---

## 7. RECOMMENDATIONS FOR MAKEFILE REFACTORING

### Phase 1: Work Order Infrastructure (Week 1)
1. Create `tools/work-order-orchestrator.sh` (200 LOC)
   - Parse JSON work orders
   - Implement Kanban limit enforcement (WIP ≤ 1)
   - Execute with dependency graph resolution

2. Define work order schema (JSON)
   - Extend with Makefile-specific fields
   - Support file locking

### Phase 2: Hook Integration (Week 2)
1. Refactor Makefile targets to use pre/post hooks
2. Extend settings.json with Makefile hook matchers

### Phase 3: Hierarchical Gates (Week 3)
1. Refactor quality gates into supervision tiers
2. Implement `one_for_one` failure semantics

### Phase 4: Incremental Testing (Week 4)
1. Integrate change detection into Makefile
2. Cost-optimized CI workflow

---

## CONCLUSION

The erlmcp codebase demonstrates sophisticated **policy-based governance**, **dependency-aware coordination**, and **hierarchical supervision** patterns that can directly improve Makefile architecture. The refactoring can achieve:

1. **Work Order Protocol** for async task coordination
2. **Hook-based extensibility** for pre/post target execution
3. **Hierarchical dependency management** (TIER 1/2/3 gates)
4. **Incremental testing** for 50% cost optimization
5. **Cloud-deterministic execution** (environment parity)

These patterns align with Armstrong principles (correct-by-construction) and Joe Armstrong's "let-it-crash" philosophy.
