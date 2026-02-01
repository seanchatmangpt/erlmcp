CODE LIKE A JOE ARMSTRONG AGI SWARM!!!

# CLAUDE.md - erlmcp Formal Specification v2.1.0

## Σ : System Kernel

**erlmcp** := Erlang/OTP MCP SDK | JSON-RPC 2.0 | Client⊕Server | OTP 28.3.1 (STRICT)

**Custom OTP** : `/Users/sac/.erlmcp/otp-28.3.1/` → `ERLMCP_OTP_BIN="/Users/sac/.erlmcp/otp-28.3.1/bin"`

| App | |Σ| | Purpose | Tests |
|-----|-----|---------|-------|
| core | 97 | Protocol, sessions, auth, secrets, LLM | 84 EUnit |
| transports | 23 | stdio, tcp, http, ws, sse | CT |
| observability | 31 | OTEL, chaos, metrics, tracing | CT |
| validation | 13 | Compliance, spec parser | CT |

**164 modules | 84+ test suites | 850+ docs | 40+ examples**

---

## Commands (Gate Sequence)

```bash
# CORE GATES (🌐 = cloud | 💻 = local-only)
TERM=dumb rebar3 compile           # Gate₁: errors = 0        | 🌐 30s
rebar3 eunit --module=M_tests      # Gate₂: failures = 0      | 🌐 60s
rebar3 ct --suite=test/S           # Gate₃: pass_rate = 1.0   | 🌐 120s
make check                         # compile + xref + dialyzer + tests | 🌐 180s (parallel)
make console | make observer       # REPL | Process visualization | 💻
rebar3 dialyzer | rebar3 xref      # Types → 0 | Undefined → ∅ | 🌐 90s

# OPTIMIZED
make test-changed                  # Incremental tests (50% cost) | 🌐 45s
make verify-fast                   # compile + eunit (quick) | 🌐 90s
make benchmark-quick               # regression < 0.1 | 💻 300s
```

**Cloud Notes**: `make check` runs 4 gates in parallel (3x speedup). Full suite ≤ 4 min in cloud.

---

## Critical Rules

| # | Rule |
|---|------|
| 1 | ¬done ⟺ ¬(compile ∧ test) — Completion Gate |
| 2 | perf_changed → benchmark — Performance Gate |
| 3 | {errors=0, failures=0, coverage≥0.8, regression<0.1} — Quality |
| 4 | {gen_server, supervision, isolation, let-it-crash} — OTP |
| 5 | Output ⊆ {.erl, .hrl} unless explicit_request — Code Only |
| 6 | cloud(command) → idempotent(command) — Cloud Safety |
| 7 | **NEVER REBASE EVER — Merge only (merge over rebase)** |
| 8 | **ALWAYS use agents — Never appropriate to work without Task()** |
| 9 | **NEVER USE --no-verify — All quality gates must pass, fix the root cause** |

---

## Development Rules

| Rule | Constraint |
|------|------------|
| Chicago TDD | ∀code. ∃!test. test ⊢ code |
| NO Mocks | Mocks ∪ Fakes ∪ Placeholders = ∅ |
| Real Processes | ∀test. uses_real_erlmcp_processes = true |
| Coverage | ∀module. coverage ≥ 0.8 |
| gen_server | init/1 never blocks → async cast |
| Supervision | ∀child. ∃supervisor. supervises(child) |
| spawn | ∀proc. supervised(proc) = true |

**Anti-Patterns (⊣)**: init/1 blocking | Large messages | Unmonitored procs | Timeouts <5000ms | Unsupervised spawn | Mocks | Testing impl details | Placeholder code | Non-idempotent cloud commands

---

## Supervision (3-Tier)

```
TIER₁ (one_for_all)        : erlmcp_sup ⊃ {erlmcp_core_sup, erlmcp_registry(gproc)}
TIER₂ (simple_one_for_one) : {server,client,session}_sup → isolated per-connection
TIER₃ (isolated)           : erlmcp_observability_sup ⊃ {metrics, dashboard, tracing}
```

---

## Invariants

| Invariant | Definition |
|-----------|------------|
| Process-per-Connection | ∀c ∈ Conn. ∃!p ∈ GenServer. handles(p,c) |
| Request-ID Correlation | ∀req. ∃!id ∈ UUID. State.pending[id] = req |
| Registry Routing | gproc : Name × Pid → Route. O(log N) |
| Let-It-Crash | failure(Child) → restart(Child) ⊬ failure(Sibling) |
| Transport Polymorphism | τ : {stdio,tcp,http,ws,sse} → Behavior{init/2,send/2,close/1} |
| Black-Box Testing | Test ⊨ Observable(Behavior) ⊬ Implementation |
| Cloud Determinism | ∀test. result(cloud) = result(local) |

---

## Transport τ-Interface

**Behavior** : `-behaviour(erlmcp_transport)` | Template: `erlmcp_transport_tcp.erl`

| Callback | Signature |
|----------|-----------|
| init | (Type, Opts) → {ok, State} \| {error, Reason} |
| send | (Data, State) → {ok, State'} \| {error, Reason} |
| close | State → ok |

**Messages** : `{transport_data, Bin}` | `{transport_connected, Info}` | `{transport_disconnected, Reason}`

---

## Agent Orchestration

**CRITICAL** : ∀agents ∈ Set. launch(agents) ∈ SingleMessage → Parallel O(1)

### GOLDEN RULE: 1 MESSAGE = ALL OPERATIONS

**Always batch everything in ONE message:**
- Spawn 20+ agents/tools via `Task()` tool
- Create 20+ todos via `TodoWrite()`
- Read/Edit 20+ files together
- Never sequential operations!

### All 20 Available Agents/Tools

**Core Development (7)**:
| Agent | Domain | Purpose |
|-------|--------|---------|
| erlang-otp-developer | gen_server, supervisor | OTP behaviors |
| erlang-transport-builder | stdio, tcp, http, ws, sse | Transport layer |
| erlang-test-engineer | EUnit, CT, Proper | Chicago TDD |
| erlang-architect | Supervision trees | System design |
| erlang-researcher | Codebase exploration | Pattern research |
| erlang-performance | Benchmarking | Optimization |
| code-reviewer | Quality gates | Pre-completion |

**Build & Validation (7)**:
| Agent | Domain | Purpose |
|-------|--------|---------|
| build-engineer | Constrained writes | Code editing |
| verifier | Test execution | Validation |
| agent-01-compile-gate | Compile validation | Entry gate |
| agent-02-compile-core | Core compilation | Core app |
| agent-03-compile-transports | Transport compilation | Transports |
| agent-04-compile-observability | Observability compilation | OTEL |
| agent-05-compile-validation | Validation compilation | Compliance |

**Testing (5)**:
| Agent | Domain | Purpose |
|-------|--------|---------|
| agent-06-test-eunit | EUnit tests | Unit testing |
| agent-07-test-ct | Common Test | Integration |
| agent-08-test-smoke | Smoke tests | Quick validation |
| agent-09-test-quick | Fast tests | Rapid feedback |
| agent-10-test-proper | Property tests | Generative |

**Quality (6)**:
| Agent | Domain | Purpose |
|-------|--------|---------|
| agent-11-coverage | Code coverage | 80% gate |
| agent-12-dialyzer | Type checking | Dialyzer |
| agent-13-xref | Cross-reference | Undefined functions |
| agent-14-format | Code formatting | rebar3 format |
| agent-15-benchmark | Performance | Regression tests |
| agent-16-jidoka | Built-in quality | Auto-stop on error |

**Workflow & Operations (7)**:
| Agent | Domain | Purpose |
|-------|--------|---------|
| erlang-github-ops | Git, PR, CI/CD (MERGE ONLY - NEVER REBASE - NEVER --no-verify) | Release workflow |
| sparc-orchestrator | SPARC methodology | Feature workflow |
| plan-designer | Implementation planning | Research→Plan→Execute |
| agent-17-poka-yoke | Error-proofing | Mistake-proofing |
| agent-18-andon | Andon signals | Stop-the-line |
| agent-19-tcps | TPS quality system | Lean manufacturing |
| agent-20-release | Release management | Deployment |

### How to Spawn Agents (Parallel Execution)

```javascript
// ONE MESSAGE - spawn all agents in parallel
Task("Erlang Researcher", "Explore codebase for JSON-RPC patterns", "erlang-researcher")
Task("Erlang Architect", "Design supervision tree for new feature", "erlang-architect")
Task("Erlang OTP Developer", "Implement gen_server with proper callbacks", "erlang-otp-developer")
Task("Erlang Test Engineer", "Write EUnit/CT tests (Chicago TDD)", "erlang-test-engineer")
Task("Erlang Transport Builder", "Build transport using gun/ranch", "erlang-transport-builder")
Task("Code Reviewer", "Review code for OTP compliance", "code-reviewer")
Task("Erlang Performance", "Benchmark critical paths", "erlang-performance")
Task("Build Engineer", "Constrained source file writes", "build-engineer")
Task("Verifier", "Run test suite", "verifier")

// Batch 20+ todos together
TodoWrite { todos: [
    {id: "1", content: "Research codebase patterns", status: "in_progress"},
    {id: "2", content: "Design supervision tree", status: "in_progress"},
    {id: "3", content: "Create implementation plan", status: "in_progress"},
    {id: "4", content: "Implement gen_server", status: "pending"},
    {id: "5", content: "Build transport layer", status: "pending"},
    {id: "6", content: "Write EUnit tests", status: "pending"},
    {id: "7", content: "Write CT tests", status: "pending"},
    {id: "8", content: "Review OTP compliance", status: "pending"},
    {id: "9", content: "Benchmark performance", status: "pending"},
    {id: "10", content: "Verify coverage >= 80%", status: "pending"},
    {id: "11", content: "Run Dialyzer", status: "pending"},
    {id: "12", content: "Run Xref", status: "pending"},
    {id: "13", content: "Format code", status: "pending"},
    {id: "14", content: "Generate receipt", status: "pending"},
    {id: "15", content: "Run make check", status: "pending"},
    {id: "16", content: "Prepare PR", status: "pending"},
    {id: "17", content: "Apply Poka-yoke", status: "pending"},
    {id: "18", content: "Check Andon signals", status: "pending"},
    {id: "19", content: "Apply TPS quality", status: "pending"},
    {id: "20", content: "Prepare release", status: "pending"}
]}

// All file ops together
Read "apps/erlmcp_core/src/module1.erl"
Read "apps/erlmcp_core/src/module2.erl"
Read "apps/erlmcp_transports/src/transport.erl"
Grep "gen_server" "apps/**/*.erl"
Grep "handle_call" "apps/**/*.erl"
```

### EPIC 9 Workflow (Non-Trivial Tasks)

**Trigger**: 5+ files, 3+ systems, or multiple approaches

**Phases**: fan_out → independent_construction → collision_detection → convergence → refactoring → closure

**Expected Speedup**: 2.8x - 4.4x

### Work Order Protocol

```erlang
-type work_order() :: #{
    id := binary(),
    task := binary(),
    agent := agent_role(),
    priority := high | normal | low,
    dependencies := [work_order_id()],
    status := queued | wip | done | failed
}.
```

**Kanban** : ∀agent. |WIP(agent)| ≤ 1

**Lifecycle** : Queue → Acquire → Execute (unique branch) → Complete → **Quality gates PASS → Merge (NEVER rebase, NEVER --no-verify)**

### Subagent Configuration

See `.claude/settings.json` for 13 configured subagents with:
- Tool access controls (allow/deny)
- Permission modes (read/write/execute)
- Preload paths (agent definitions, skills)

### Skills

- `otp-manager` - OTP 28.3.1 installation and verification
- `chicago-tdd-erlang` - Chicago School TDD for Erlang
- `sparc` - SPARC methodology workflows

---

## Quality Gates

| Gate | Pass (⊢) | Cloud | Local | Time |
|------|----------|-------|-------|------|
| Compile | errors = 0 | ✅ | ✅ | 30s |
| Tests | failures = 0 | ✅ | ✅ | 180s |
| Coverage | ≥ 80% | ✅ | ✅ | 30s |
| Dialyzer | warnings → 0 | ✅ | ✅ | 90s |
| Xref | undefined = ∅ | ✅ | ✅ | 30s |
| Benchmark | regression < 10% | ⚠️ | ✅ | 300s |

**Enforcement** : Pre-commit hooks | CI/CD workflows | `./tools/claude-md-{sync,enforcer}.sh`

---

## Quick Index

| Task | Module | App |
|------|--------|-----|
| MCP protocol | erlmcp_json_rpc | core |
| Resources | erlmcp_server:subscribe_resource* | core |
| Session | erlmcp_session_backend | core |
| Secrets | erlmcp_secrets | core |
| Tools (server) | erlmcp_server:add_tool* | core |
| Tools (client) | erlmcp_client:call_tool | core |
| Transport template | erlmcp_transport_tcp | transports |
| Routing | erlmcp_registry | core |
| OTEL | erlmcp_otel | observability |
| Chaos | erlmcp_chaos | observability |
| Dashboard | erlmcp_dashboard_server | observability |
| Compliance | erlmcp_compliance_report | validation |

---

## TPS Quality System

| 行灯 Andon | ポカヨケ Poka-Yoke | 自働化 Jidoka | 改善 Kaizen |
|------------|-------------------|---------------|-------------|
| erlmcp_health_monitor | jesse schema validation | Pre-commit hooks | erlmcp_chaos |
| `/andon` dashboard | Transport behavior types | CI/CD 20 workflows | Benchmarking |
| Circuit breakers | Message size bounds | Coverage ≥80% blocking | Receipt chain |

---

## Module Taxonomy

**Core (97)**: Protocol (client, server, registry, json_rpc) | Session (backend, ets/dets/mnesia, manager, failover) | Security (auth, mtl, rate_limiter, secrets) | MCP (capabilities, resources, tool, prompt_template) | LLM | Resilience | Cache | Messages | Utilities | Resources | Distributed | Quality

**Transports (23)**: Interface (behavior, adapter) | Impl (stdio, tcp, http, ws, sse) | Infra (pool, pipeline, registry, health) | Security (header_validator, origin_validator, tls)

**Observability (31)**: OTEL | Tracing | Metrics | Dashboard | Monitor | Debug | Chaos | Audit | Bench

**Validation (13)**: Report | Test | Validators (protocol, transport, security, performance) | Spec | Resource | CLI

---

## Configuration

**Session Backend** : erlmcp_session_backend → {ETS (O(1)), DETS (durable), Mnesia (distributed)}
**Secrets** : erlmcp_secrets → {Vault (prod), AWS SecretsManager, LocalEncrypted (AES-256)}

---

## Dependencies

| Category | Packages |
|----------|----------|
| Core | jsx 3.1.0, jesse 1.8.1, gproc 0.9.0, gun 2.0.1, ranch 2.1.0, poolboy 1.5.2, cowboy 2.10.0 |
| OTEL | opentelemetry_api 1.5.0, opentelemetry 1.7.0, opentelemetry_exporter 1.10.0 |
| Test | proper 1.4.0, meck 0.9.2, coveralls 2.2.0 |

---

## Performance Baseline (Jan 2026)

| Metric | Value |
|--------|-------|
| Registry | 553K msg/s |
| Queue | 971K msg/s |
| Connections/node | 40-50K |

---

## Cloud Execution Summary

**erlmcp** runs autonomously on Claude Code web sessions.

| Aspect | Cloud (🌐) | Local (💻) |
|--------|-----------|-----------|
| Environment | Ubuntu VM, OTP 28.3.1 (auto-installed) | User hardware |
| Session | 2-4 hours (ephemeral) | Unlimited |
| State | Git + branch sync | Git + local |
| Cost | $0.10/hour + $0.01/GB | Free |

**Key Workflows**:
- Development: `Edit → make verify-fast (90s) → make check (120s parallel) → commit`
- Pre-PR: `make quality-report (240s) → gh pr create`
- Optimization: `make test-changed` for incremental (50% cost reduction)

**SessionStart Hook** (`.claude/hooks/SessionStart.sh`): Auto-installs OTP 28.3.1, fetches deps, pre-compiles core, sets env vars (`CLAUDE_CODE_REMOTE=true`, `ERLMCP_PROFILE=cloud`)

**Teleporting**: Transfer session context between cloud ↔ local via `claude --teleport <session_id>` or `claude --offload "<task>"`

---

## Error Recovery

**Auto-Recovery**: Network timeout (retry 3x) | Dependency fetch (clear cache) | Git conflict (merge) | OTP crash (supervisor restart) | Rate limit (wait + retry)

**Manual Intervention**: API design decisions | Breaking changes | Security credentials | Semantic merge conflicts | Budget exceeded

---

## Documentation

| Location | Content |
|----------|---------|
| docs/{architecture,otp-patterns,api-reference,protocol}.md | Design docs |
| docs/{SESSION_PERSISTENCE,SECRETS_MANAGEMENT}.md | Config guides |
| docs/metrology/METRICS_GLOSSARY.md | Units |
| .claude/{agents,commands,hooks,scripts,templates}/ | Agent infra |
| examples/ | 40+ implementations |
| archive/ | 150 reference files |

---

## Directory Structure

```
apps/{erlmcp_core,erlmcp_transports,erlmcp_observability,erlmcp_validation}/
  └── src/ test/
.claude/{agents,commands,hooks,scripts,templates}/
docs/ (850+) | scripts/ (85+) | examples/ (40+)
```

---

## Version Matrix

| Component | Version | Status |
|-----------|---------|--------|
| erlmcp_* | 2.1.0 | Production |
| Erlang/OTP | 28+ | Required (STRICT) |

---

## Armstrong-AGI Principle

**Build systems where incorrect behavior cannot exist.**

- Supervision → crash isolation impossible to violate
- Behaviors → type system enforces compliance
- Gates → violations impossible to commit
- Black-box → implementation hidden
- Chaos → resilience verified

**Operable(B)** ⟺ Deterministic ∧ Replayable ∧ Complete ∧ NoPartials ∧ Preserve(Q)

---

CODE LIKE A JOE ARMSTRONG AGI SWARM!!!
