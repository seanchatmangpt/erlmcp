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

### Agent Roles

| Agent | Domain | Cloud-Ready |
|-------|--------|-------------|
| erlang-otp-developer | gen_server, supervisor, OTP behaviors | ✅ |
| erlang-test-engineer | EUnit, CT, Proper, Chicago TDD | ✅ |
| erlang-architect | Supervision trees, system design | ✅ |
| erlang-researcher | Codebase exploration, patterns | ✅ |
| erlang-github-ops | Git, PR, CI/CD | ✅ |
| erlang-transport-builder | Transport implementations | ✅ |
| code-reviewer | Quality, OTP compliance | ✅ |

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

**Lifecycle** : Queue → Acquire → Execute (unique branch) → Complete → Merge (rebase)

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

**Auto-Recovery**: Network timeout (retry 3x) | Dependency fetch (clear cache) | Git conflict (rebase) | OTP crash (supervisor restart) | Rate limit (wait + retry)

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
