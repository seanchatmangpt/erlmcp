CODE LIKE A JOE ARMSTRONG AGI SWARM!!!

# CLAUDE.md - erlmcp Formal Specification v2.1.0

## Σ : System Kernel

**erlmcp** := Erlang/OTP MCP SDK | JSON-RPC 2.0 | Client⊕Server | OTP 28+ (STRICT)

| App | |Σ| | Purpose | Tests |
|-----|-----|---------|-------|
| core | 97 | Protocol, sessions, auth, secrets, LLM | 84 EUnit |
| transports | 23 | stdio, tcp, http, ws, sse | CT |
| observability | 31 | OTEL, chaos, metrics, tracing | CT |
| validation | 13 | Compliance, spec parser | CT |

**Totals** : 164 modules | 84+ test suites | 850+ docs | 40+ examples

## Commands (Gate Sequence)

```bash
# LOCAL + CLOUD (🌐 = cloud-optimized, 💻 = local-only)
TERM=dumb rebar3 compile           # Gate₁: errors = 0        | 🌐 30s | $0.01
rebar3 eunit --module=M_tests      # Gate₂: failures = 0      | 🌐 60s | $0.02
rebar3 ct --suite=test/S           # Gate₃: pass_rate = 1.0   | 🌐 120s | $0.04
make benchmark-quick               # Gate₄: regression < 0.1  | 💻 300s | (local hardware only)
make check                         # All: compile ∧ xref ∧ dialyzer ∧ tests | 🌐 180s | $0.08 (parallel)
make console | make observer       # REPL | Process visualization | 💻 (interactive)
rebar3 dialyzer | rebar3 xref      # Types → 0 | Undefined → ∅ | 🌐 90s | $0.03

# CLOUD-OPTIMIZED WORKFLOWS
make test-changed                  # Incremental test suite (cost-optimized) | 🌐 45s | $0.015
make quality-report                # Full TPS gates + report generation | 🌐 240s | $0.10
make verify-fast                   # compile + eunit (quick feedback) | 🌐 90s | $0.03
```

**Benchmarks** : `erlmcp_bench_{core_ops,network_real,stress,chaos,integration}:run/1`

**Cloud Execution Notes**:
- 🌐 Commands run autonomously in cloud VMs (OTP 28.3.1 pre-installed via SessionStart hook)
- 💻 Commands require local hardware or interactive access
- Parallel execution: `make check` runs compile, xref, dialyzer, eunit, ct in parallel (3x speedup)
- Cost optimization: Use `make test-changed` for iterative development (50% cost reduction)
- Time budget: Full quality gate suite ≤ 4 minutes in cloud (vs 6-8 minutes local)

## Supervision (3-Tier)

```
TIER₁ (one_for_all)        : erlmcp_sup ⊃ {erlmcp_core_sup, erlmcp_registry(gproc)}
TIER₂ (simple_one_for_one) : {server,client,session}_sup → isolated per-connection
TIER₃ (isolated)           : erlmcp_observability_sup ⊃ {metrics, dashboard, tracing}
```

## Invariants

| Invariant | Definition |
|-----------|------------|
| Process-per-Connection | ∀c ∈ Conn. ∃!p ∈ GenServer. handles(p,c) |
| Request-ID Correlation | ∀req. ∃!id ∈ UUID. State.pending[id] = req |
| Registry Routing | gproc : Name × Pid → Route. O(log N) |
| Let-It-Crash | failure(Child) → restart(Child) ⊬ failure(Sibling) |
| Transport Polymorphism | τ : {stdio,tcp,http,ws,sse} → Behavior{init/2,send/2,close/1} |
| Black-Box Testing | Test ⊨ Observable(Behavior) ⊬ Implementation |
| Cloud Determinism | ∀test. result(cloud) = result(local) (environment-agnostic) |

## Transport τ-Interface

**Behavior** : `-behaviour(erlmcp_transport)` | Template: `erlmcp_transport_tcp.erl`

| Callback | Signature |
|----------|-----------|
| init | (Type, Opts) → {ok, State} \| {error, Reason} |
| send | (Data, State) → {ok, State'} \| {error, Reason} |
| close | State → ok |

**Messages** : `{transport_data, Bin}` | `{transport_connected, Info}` | `{transport_disconnected, Reason}`

**State** : transport_module ∈ τ | capabilities ⊆ {resources,tools,prompts} | req_id_map | pending_map

## Configuration

**Session Backend** : erlmcp_session_backend → {ETS (O(1)), DETS (durable), Mnesia (distributed)}
**Secrets** : erlmcp_secrets → {Vault (prod), AWS SecretsManager, LocalEncrypted (AES-256)}
**Refs** : docs/{SESSION_PERSISTENCE.md, SECRETS_MANAGEMENT.md}

## TPS Quality System

| 行灯 Andon | ポカヨケ Poka-Yoke | 自働化 Jidoka | 改善 Kaizen |
|------------|-------------------|---------------|-------------|
| erlmcp_health_monitor | jesse schema validation | Pre-commit hooks | erlmcp_chaos |
| `/andon` dashboard | Transport behavior types | CI/CD 20 workflows | Benchmarking |
| Circuit breakers | Message size bounds | Coverage ≥80% blocking | Receipt chain |
| Threshold alerts | Refusal codes [1001-1089] | Test → ⊣ = ¬Commit | Evidence bundles |

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
| erlang-performance | Benchmarking, optimization | ⚠️ (local hardware preferred) |
| erlang-transport-builder | Transport implementations | ✅ |
| code-reviewer | Quality, OTP compliance | ✅ |

### Work Order Protocol

**Definition** : WorkOrder := {Task, Agent, Priority, Dependencies, Constraints}

```erlang
%% Work order specification
-type work_order() :: #{
    id := binary(),                    % Unique work order ID
    task := binary(),                  % "Implement X" | "Test Y" | "Review Z"
    agent := agent_role(),             % Target agent from table above
    priority := high | normal | low,   % Execution priority
    dependencies := [work_order_id()], % Must complete before this task
    constraints := #{                  % Execution constraints
        time_budget => pos_integer(),  % Seconds
        cost_budget => float(),        % USD
        files => [file_path()],        % File-level locks
        branch => binary()             % Auto-managed branch name
    },
    status := queued | wip | done | failed,
    result := #{                       % Written on completion
        exit_code => integer(),
        gates => gate_results(),
        artifacts => [file_path()],
        duration => pos_integer()      % Seconds
    }
}.
```

### Coordination Semantics

**Kanban Limits** : ∀agent. |WIP(agent)| ≤ 1 (single-tasking for quality)

**Work Order Lifecycle**:
1. **Queue** : Write to `.erlmcp/work-orders/queue.json`
2. **Acquire** : Agent checks Kanban limit → move to WIP
3. **Execute** : Run task on unique branch (auto-created)
4. **Complete** : Write results to `.erlmcp/work-orders/completed.json`
5. **Merge** : Rebase onto main (conflict resolution via git rerere)

**File-Level Locking**:
```erlang
%% Before modifying src/erlmcp_client.erl
Lock = erlmcp_work_order:acquire_lock("src/erlmcp_client.erl").
%% Returns {ok, LockToken} | {error, {locked_by, OtherAgent}}
```

**Dependency Resolution**:
```erlang
%% Example: Test depends on implementation
WorkOrders = [
    #{id => "wo-001", task => "Implement FSM", agent => 'erlang-architect',
      dependencies => [], priority => high},
    #{id => "wo-002", task => "Test FSM", agent => 'erlang-test-engineer',
      dependencies => ["wo-001"], priority => high},
    #{id => "wo-003", task => "Review FSM", agent => 'code-reviewer',
      dependencies => ["wo-001", "wo-002"], priority => normal}
].
%% Execution order: wo-001 → {wo-002, wo-003} (wo-003 waits for wo-002)
```

### Multi-Agent Coordination Example

```bash
# Terminal (local)
$ claude "Implement Armstrong-style FSMs with full TDD"

# Claude spawns work orders:
# 1. erlang-architect: Design FSM supervision tree
# 2. erlang-otp-developer: Implement gen_statem behavior
# 3. erlang-test-engineer: Write EUnit + CT tests
# 4. erlang-github-ops: Create PR with quality gates
# 5. code-reviewer: Final OTP compliance check

# Cloud execution (parallel where possible):
# t=0s:   wo-001 (architect) starts
# t=120s: wo-001 done → wo-002 (developer) + wo-003 (test-engineer) start in parallel
# t=300s: wo-002, wo-003 done → wo-004 (github-ops) starts
# t=360s: wo-004 done (PR created) → wo-005 (reviewer) starts
# t=420s: wo-005 done → Report back to user

# Result: Complete FSM implementation + tests + PR in 7 minutes
```

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
| File Output | ⊆ {.erl, .hrl, .app.src, test/*.erl} |
| No Markdown | ¬Write(.md) unless explicit_request |
| Cloud Safety | ∀command. idempotent(command) ∨ guarded(command) |

## Anti-Patterns (⊣)

1. init/1 blocking  2. Large messages (use refs)  3. Unmonitored procs  4. Missing timeouts (<5000ms)
5. Unsupervised spawn  6. Untested chaos  7. Ignored health  8. Mock usage  9. Testing impl details
10. Placeholder code  11. Markdown output without request  12. Non-idempotent cloud commands

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

## Module Taxonomy

### Core (97)

**Protocol** : client, server, registry, registry_dist, json_rpc
**Session** : session, session_backend, session_{ets,dets,mnesia}, session_manager, session_failover, session_replicator
**Security** : auth, auth_mtls, auth_rate_limiter, secrets
**MCP** : capabilities, resources, resource, resource_subscriptions, tool, prompt_template, prompt_list_change_notifier, progress, sampling, completion
**LLM** : llm_provider_{anthropic,openai,local}, mock_llm
**Resilience** : circuit_breaker, rate_limiter, rate_limit_middleware, connection_{monitor,limiter}
**Cache** : cache, cache_warmer, icon_cache
**Messages** : message_{handler,parser,size}, notification_handler, change_notifier, subscription, sse_event_store
**Utilities** : batch, cancellation, pagination, path_canonicalizer, request_id, refusal, errors, logging, health, hooks, schema_registry
**Resources** : memory_{guard,monitor}, cpu_{guard,quota}, graceful_drain
**Distributed** : code_reload, node_monitor, split_brain_detector, failover_worker, test_sync
**Quality** : tcps_quality_gates

### Transports (23)

**Interface** : transport_behavior, transport_adapter
**Impl** : transport_{stdio,tcp,http,http_server,ws,sse,sse_manager}
**Infra** : transport_{pool,pipeline,registry,health,validation,discovery}, pool_{manager,strategy}
**Security** : http_header_validator, origin_validator, security_headers, tls_validation
**OTP** : transports_app, transport_sup

### Observability (31)

**OTEL** : otel, otel_middleware, otel_{datadog,honeycomb,jaeger}
**Tracing** : tracing, trace_analyzer
**Metrics** : metrics, metrics_server, metrics_aggregator, metrology_validator
**Dashboard** : dashboard_server, dashboard_http_handler
**Monitor** : health_monitor, process_monitor
**Debug** : debugger, profiler, memory_analyzer
**Chaos** : chaos, chaos_{network,process,resource}, chaos_worker, chaos_worker_sup, recovery_manager
**Audit** : audit_log, receipt_chain, evidence_path
**Bench** : bench_rate_limit
**OTP** : observability_app, observability_sup

### Validation (13)

**Report** : compliance_report, compliance_report_{html,json}
**Test** : test_client
**Validators** : protocol_validator, transport_validator, security_validator, performance_validator
**Spec** : spec_parser, uri_validator
**Resource** : memory_manager
**CLI** : validate_cli
**OTP** : validation_app

## Quality Gates

| Gate | Pass (⊢) | Fail (⊣) | Cloud | Local | Cost | Time (cloud) |
|------|----------|----------|-------|-------|------|--------------|
| Compile | errors = 0 | errors > 0 | ✅ | ✅ | $0.01 | 30s |
| Tests | failures = 0 | failures > 0 | ✅ | ✅ | $0.06 | 180s |
| Coverage | ≥ 80% | < 80% | ✅ | ✅ | $0.01 | 30s |
| Bench | regression < 10% | regression ≥ 10% | ⚠️ | ✅ | - | 300s (local) |
| Dialyzer | warnings → 0 | (advisory) | ✅ | ✅ | $0.03 | 90s |
| Xref | undefined = ∅ | (advisory) | ✅ | ✅ | $0.01 | 30s |

**Enforcement** : Pre-commit hooks | CI/CD 20 workflows | ./tools/claude-md-{sync,enforcer}.sh

**Cloud Optimization Strategy**:
- **Parallel Execution** : Run compile, dialyzer, xref, eunit, ct concurrently (3x speedup)
- **Incremental Testing** : `make test-changed` runs only tests for modified modules
- **Cached Dependencies** : rebar3 deps cached in `/home/user/erlmcp/.rebar3/` (persist across sessions)
- **Benchmark Exclusion** : Hardware-dependent benchmarks run locally only (avoid VM variance)

## Dependencies

| Category | Packages |
|----------|----------|
| Core | jsx 3.1.0, jesse 1.8.1, gproc 0.9.0, gun 2.0.1, ranch 2.1.0, poolboy 1.5.2, cowboy 2.10.0, bbmustache 1.12.2, jose 1.11.1 |
| OTEL | opentelemetry_api 1.5.0, opentelemetry 1.7.0, opentelemetry_exporter 1.10.0 |
| Test | proper 1.4.0, meck 0.9.2, coveralls 2.2.0 |
| Dev | recon 2.5.3, observer_cli 1.7.4, rebar3_{format,lint,proper,hex,auto} |

## Performance Baseline (Jan 2026)

| Metric | Value |
|--------|-------|
| Registry | 553K msg/s |
| Queue | 971K msg/s |
| Pool | 149K msg/s |
| Session | 242K msg/s |
| Network I/O | 43K msg/s |
| Sustained | 372K msg/s |
| Connections/node | 40-50K |
| >100K connections | Mnesia clustering |

**Metrology** : throughput_msg_per_s | latency_p{50,95,99}_us | memory_{heap_mib_per_conn,rss_mib_per_node}

## Version Matrix

| Component | Version | Status |
|-----------|---------|--------|
| erlmcp_* | 2.1.0 | Production |
| Erlang/OTP | 28+ | Required (STRICT) |

## Critical Rules

1. ¬done ⟺ ¬(compile ∧ test) — Completion Gate
2. perf_changed → benchmark — Performance Gate
3. {⊢: ✅, ⊣: ❌, ⚠️: advisory} — Report Format
4. {errors=0, failures=0, coverage≥0.8, regression<0.1} — Quality
5. {gen_server, supervision, isolation, let-it-crash} — OTP
6. {Andon, Poka-Yoke, Jidoka, Kaizen} — TPS
7. Output ⊆ {.erl, .hrl} unless explicit_request — Code Only
8. cloud(command) → idempotent(command) — Cloud Safety

## Armstrong-AGI Principle

**Core** : Build systems where incorrect behavior cannot exist.

- Supervision → crash isolation impossible to violate
- Behaviors → type system enforces compliance
- Gates → violations impossible to commit
- Black-box → implementation hidden
- Chaos → resilience verified

**Operable(B)** ⟺ Deterministic ∧ Replayable ∧ Complete ∧ NoPartials ∧ Preserve(Q)

## Cloud Execution

**erlmcp** is designed for autonomous execution on Claude Code on the web.

### Cloud vs Local Semantics

| Aspect | Cloud (🌐) | Local (💻) |
|--------|-----------|-----------|
| Environment | Ubuntu 20.04 VM, 4 vCPU, 8GB RAM | User hardware (varies) |
| OTP Version | 28.3.1 (auto-installed) | User-managed |
| Network | Outbound HTTPS (443) allowed | Unrestricted |
| Filesystem | /home/user/erlmcp (ephemeral) | Persistent |
| Session Duration | 2-4 hours (auto-extend on activity) | Unlimited |
| State Persistence | Git commits + branch sync | Git + local changes |
| Cost Model | $0.10/hour compute + $0.01/GB transfer | Free (user hardware) |

### SessionStart Hook

**Path** : `.claude/hooks/SessionStart.sh`

**Purpose** : Initialize cloud environment for erlmcp development

**Execution** : Automatic on cloud session creation | O(60s) | Idempotent

```bash
#!/usr/bin/env bash
# .claude/hooks/SessionStart.sh

set -euo pipefail

echo "🌐 erlmcp Cloud Session Initialization"

# 1. Detect/Install OTP 28.3.1
if ! command -v erl &> /dev/null || ! erl -eval 'erlang:system_info(otp_release)' -noshell -s init stop | grep -q "28"; then
    echo "📦 Installing Erlang/OTP 28.3.1..."
    wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
    sudo dpkg -i erlang-solutions_2.0_all.deb
    sudo apt-get update
    sudo apt-get install -y esl-erlang=1:28.3.1-1
fi

# 2. Fetch dependencies (with caching)
if [ ! -d "_build" ]; then
    echo "📚 Fetching rebar3 dependencies..."
    rebar3 get-deps
fi

# 3. Pre-compile core modules (warm cache)
echo "🔥 Pre-compiling core modules..."
TERM=dumb rebar3 compile apps/erlmcp_core

# 4. Validate environment
echo "✅ Environment validation..."
erl -eval 'io:format("OTP: ~s~n", [erlang:system_info(otp_release)]), halt(0).' -noshell
rebar3 version

# 5. Set cloud-specific env vars
export CLAUDE_CODE_REMOTE=true
export ERLMCP_PROFILE=cloud
export ERLMCP_CACHE="/home/user/erlmcp/.erlmcp/cache/"
mkdir -p "$ERLMCP_CACHE"

echo "🚀 erlmcp ready for cloud development!"
```

**Caching Strategy**:
- rebar3 dependencies → `/home/user/erlmcp/_build/` (persist 24h)
- BEAM files → `/home/user/erlmcp/_build/default/lib/` (recompile on source change)
- Test artifacts → `/home/user/erlmcp/_build/test/` (cleaned on `make clean`)

### Environment Variables

| Variable | Value (Cloud) | Purpose |
|----------|---------------|---------|
| `CLAUDE_CODE_REMOTE` | `true` | Detect cloud execution context |
| `ERLMCP_PROFILE` | `cloud` | Load cloud-optimized configuration |
| `ERLMCP_CACHE` | `/home/user/erlmcp/.erlmcp/cache/` | Persistent cache directory |
| `TERM` | `dumb` | Disable ANSI escape codes (rebar3 compatibility) |
| `ERL_AFLAGS` | `-kernel shell_history enabled` | Enable shell history |
| `REBAR_COLOR` | `none` | Disable color output (cloud logs) |

**Configuration Loading** (src/erlmcp.app.src):
```erlang
{env, [
    {profile, case os:getenv("ERLMCP_PROFILE") of
        "cloud" -> cloud;
        _ -> local
    end},
    {cache_dir, os:getenv("ERLMCP_CACHE", "/tmp/erlmcp-cache/")},
    {cloud_mode, os:getenv("CLAUDE_CODE_REMOTE") =:= "true"}
]}.
```

### Network Access Requirements

**Allowed** (Cloud VMs):
- HTTPS (443) → packages.erlang-solutions.com (OTP installation)
- HTTPS (443) → hex.pm (rebar3 package manager)
- HTTPS (443) → github.com (git operations)
- HTTPS (443) → api.anthropic.com (LLM provider tests - optional)

**Blocked** (Security):
- Inbound connections (no server listening on public IPs)
- SMTP (25, 587) → Email sending prohibited
- Arbitrary outbound TCP → Only HTTPS allowed

**Workarounds**:
- Local LLM testing → Use mock_llm (no network required)
- Database testing → Use ETS/DETS (in-memory, no external DB)
- External API testing → Record/replay with stored fixtures

### Cost Optimization Hints

| Strategy | Savings | Implementation |
|----------|---------|----------------|
| Incremental testing | 50% | `make test-changed` (only modified modules) |
| Parallel gates | 67% time | `make check` (concurrent execution) |
| Dependency caching | 80% | SessionStart hook pre-fetches deps |
| Early failure | 90% | Compile before tests (fail fast on syntax errors) |
| Branch reuse | 30% | Reuse existing branch (avoid fresh clones) |

**Budget Calculation**:
```erlang
%% Full quality gate suite
Cost = (Compile + EUnit + CT + Dialyzer + Xref + Coverage) =
       ($0.01 + $0.02 + $0.04 + $0.03 + $0.01 + $0.01) = $0.12

%% Incremental (on 20% code change)
IncrementalCost = Cost * 0.2 = $0.024

%% Parallel execution (3x speedup, same cost)
ParallelTime = 240s / 3 = 80s
ParallelCost = $0.12 (same, but 3x faster feedback)
```

## Validation in the Cloud

**Principle** : ∀gate. result(cloud) = result(local) (deterministic, environment-agnostic)

### Gate Execution Matrix

| Gate | Cloud Time | Local Time | Cost | Parallel? | Deterministic? |
|------|-----------|-----------|------|-----------|----------------|
| Compile | 30s | 30s | $0.01 | ✅ | ✅ |
| EUnit | 60s | 30s | $0.02 | ✅ | ✅ |
| CT | 120s | 60s | $0.04 | ✅ | ✅ |
| Dialyzer | 90s | 90s | $0.03 | ✅ | ✅ |
| Xref | 30s | 30s | $0.01 | ✅ | ✅ |
| Coverage | 30s | 20s | $0.01 | ✅ | ✅ |
| Benchmark | ❌ | 300s | - | ⚠️ | ❌ (hardware-dependent) |

**Total** : 360s sequential | 120s parallel | $0.12

### Cloud-Only Gates

**None** — All quality gates run identically in cloud and local environments.

**Rationale** : Armstrong principle → tests must be deterministic and environment-agnostic.

### Local-Only Gates

**Benchmark** (make benchmark-quick):
- **Reason** : Performance metrics depend on CPU architecture, clock speed, memory bandwidth
- **Cloud Variance** : VM performance varies by host load (10-30% variance)
- **Local Preference** : Consistent hardware → reproducible benchmarks
- **Workaround** : Cloud can run benchmarks for functional correctness (not performance regression)

**Observer** (make observer):
- **Reason** : Requires X11 graphical display (not available in cloud VMs)
- **Cloud Alternative** : Use `observer_cli` (terminal-based observer)

### Expected Time Budgets

**Development Loop** (iterative):
```
Edit code → make verify-fast (90s) → feedback
         ↓ (if passing)
         → make check (120s parallel) → commit
```

**Pre-Commit** (full quality):
```
make check (120s parallel) → All gates pass → git commit
```

**Pre-PR** (comprehensive):
```
make quality-report (240s) → Generate TPS evidence bundle → gh pr create
```

### Cost Estimates Per Gate

**Compute Model** : $0.10/hour VM + $0.01/GB network transfer

| Operation | Compute (s) | Network (MB) | Total Cost |
|-----------|-------------|--------------|------------|
| Compile | 30 | 0 | $0.0008 |
| EUnit (84 suites) | 60 | 0 | $0.0017 |
| CT (23 suites) | 120 | 0 | $0.0033 |
| Dialyzer | 90 | 0 | $0.0025 |
| Xref | 30 | 0 | $0.0008 |
| Coverage | 30 | 50 | $0.0013 |
| **Total (sequential)** | 360 | 50 | $0.0104 |
| **Total (parallel)** | 120 | 50 | $0.0038 |

**Real-World Budget**:
- 100 iterations/day × $0.0038 = $0.38/day
- 22 working days/month = $8.36/month (cloud development)

### Parallel Execution Strategy

**Makefile Target** (Makefile):
```make
check-parallel: compile
	@echo "Running quality gates in parallel..."
	@$(MAKE) -j4 dialyzer xref eunit ct coverage
	@echo "✅ All gates passed"

.PHONY: dialyzer
dialyzer:
	@rebar3 dialyzer

.PHONY: xref
xref:
	@rebar3 xref

.PHONY: eunit
eunit:
	@rebar3 eunit

.PHONY: ct
ct:
	@rebar3 ct

.PHONY: coverage
coverage:
	@rebar3 cover -v
```

**Parallel Speedup**:
- Sequential: compile(30s) → dialyzer(90s) → xref(30s) → eunit(60s) → ct(120s) = 330s
- Parallel: compile(30s) → max(dialyzer, xref, eunit, ct)(120s) = 150s
- Speedup: 2.2x (limited by CT duration)

## Cloud-Native Development Workflow

**Principle** : Autonomous execution with human-in-the-loop for decisions

### Terminal (Local) → Cloud Session Handoff

```bash
# Local terminal (developer machine)
$ claude "Fix the race condition in erlmcp_session_manager"

# Claude spawns web session (cloud VM)
# → SessionStart hook runs (60s)
# → Analysis phase (30s)
# → Implementation phase (120s)
# → Testing phase (180s)
# → Total: 390s (6.5 minutes)

# Cloud session reports back:
✅ Fixed race condition in erlmcp_session_manager:handle_call/3
✅ Added EUnit tests (100% coverage)
✅ All quality gates passed (0 errors, 0 warnings)
📊 Cost: $0.065 | Time: 6m 30s
🔗 Branch: claude/fix-session-race-DNaeK
💬 Ready for review. Create PR? (yes/no)
```

### Monitoring Progress

```bash
# Check task status (local terminal)
$ claude /tasks

Active Tasks:
1. [WIP] Fix session race condition (erlang-otp-developer)
   - Status: Testing phase (60% complete)
   - ETA: 2m 30s
   - Branch: claude/fix-session-race-DNaeK

2. [QUEUED] Review changes (code-reviewer)
   - Dependencies: Task #1
   - ETA: +3m after #1 completes
```

### Decision Points (Human-in-the-Loop)

**Scenario 1** : Ambiguity in requirements
```
❓ Claude (cloud): Found two race conditions:
   A) gen_server:call timeout in handle_call/3
   B) ETS table access in concurrent workers

   Which should I fix? (A/B/both)

👤 User (local): Both, but prioritize A (user-facing)
```

**Scenario 2** : Breaking change detected
```
⚠️ Claude (cloud): Fix requires API change:
   - Old: session_manager:get_session(Id)
   - New: session_manager:get_session(Id, Timeout)

   This breaks 3 call sites. Proceed? (yes/no)

👤 User (local): Yes, update all call sites
```

### Autonomous Execution Flow

```
┌─────────────────────────────────────────────────────────┐
│ PHASE 1: Analysis (Cloud)                               │
│ - Read codebase (erlang-researcher)                     │
│ - Identify issue root cause                             │
│ - Design solution (erlang-architect)                    │
│ Time: 30-60s                                             │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│ PHASE 2: Implementation (Cloud)                         │
│ - Create branch (auto-named)                            │
│ - Write code (erlang-otp-developer)                     │
│ - Write tests (erlang-test-engineer)                    │
│ Time: 120-240s                                           │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│ PHASE 3: Validation (Cloud)                             │
│ - make check (parallel quality gates)                   │
│ - Coverage verification (≥80%)                           │
│ - Chicago TDD compliance check                          │
│ Time: 120-180s                                           │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│ PHASE 4: Review (Cloud)                                 │
│ - OTP compliance review (code-reviewer)                 │
│ - Armstrong principles check                            │
│ - Generate quality report                               │
│ Time: 60-90s                                             │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│ PHASE 5: PR Creation (Cloud) [OPTIONAL]                │
│ - Commit changes with quality report                    │
│ - Push branch to origin                                 │
│ - Create GitHub PR (gh pr create)                       │
│ Time: 30s                                                │
└─────────────────────────────────────────────────────────┘
                          ↓
                   Report to User
```

### Session Handoff Protocol

**Cloud → Local** (Teleporting):
```bash
# User decides to refine locally
👤 User (local): claude --teleport session_015jLVUqHSQc86isYfzL4Byp

# Claude fetches cloud state:
📥 Fetching branch claude/fix-session-race-DNaeK...
📥 Loading 23 work orders...
📥 Syncing chat history (142 messages)...
✅ Session teleported to local environment

# User continues locally:
$ make console
Erlang/OTP 28 [erts-15.0] [source] [64-bit] [smp:8:8]

1> erlmcp_session_manager:get_session(<<"test">>).
{ok, #{id => <<"test">>, ...}}
```

**Local → Cloud** (Offloading):
```bash
# User offloads to cloud for autonomous completion
👤 User (local): claude --offload "Complete the implementation and create PR"

# Claude resumes in cloud:
🌐 Resuming in cloud session...
📤 Pushing local changes to branch...
🚀 Continuing autonomous execution...
```

## Teleporting Sessions

**Definition** : Session teleportation := Transfer of complete development context (code, history, state) between cloud and local environments.

### When to Use

| Scenario | Cloud → Local | Local → Cloud |
|----------|---------------|---------------|
| Complex debugging | ✅ (local debugger) | ❌ |
| Interactive REPL | ✅ (observer, shell) | ❌ |
| Hardware profiling | ✅ (consistent hardware) | ❌ |
| Autonomous completion | ❌ | ✅ (cost-effective) |
| Parallel experiments | ❌ | ✅ (multi-session) |
| Network-dependent tests | ❌ | ✅ (cloud network) |

### Requirements

**Pre-Teleport Checklist**:
1. ✅ Clean git state (no uncommitted changes) OR branch pushed to origin
2. ✅ All work orders completed or in deterministic state
3. ✅ No running processes (erlmcp application stopped)
4. ✅ Session ID available (from cloud session URL)

**Cloud State**:
```erlang
%% .erlmcp/session-state.json (auto-generated)
#{
    session_id => <<"session_015jLVUqHSQc86isYfzL4Byp">>,
    branch => <<"claude/fix-session-race-DNaeK">>,
    work_orders => [#{id => <<"wo-001">>, status => done, ...}],
    chat_history => [#{role => user, content => <<"Fix race...">>}, ...],
    quality_gates => #{compile => pass, eunit => pass, ...},
    environment => #{otp_version => "28.3.1", erlmcp_version => "2.1.0"}
}.
```

### Safety Guarantees

**Invariant** : ∀teleport. state(cloud, t) ≡ state(local, t+δ) (state preservation)

| Guarantee | Implementation |
|-----------|----------------|
| No data loss | Git branch + pushed commits |
| Deterministic state | Work orders serialized to JSON |
| Chat continuity | Full history synced (Claude API) |
| Environment parity | OTP version checked (fail if mismatch) |
| File consistency | Git tree SHA verified post-teleport |

**Rollback** : If teleport fails → stay in source environment (no partial state)

### Teleport Process

**Cloud → Local**:
```bash
# Step 1: Request teleport (local terminal)
$ claude --teleport session_015jLVUqHSQc86isYfzL4Byp

# Step 2: Claude validates (cloud)
Validating teleport prerequisites...
✅ Git state: clean (all changes committed)
✅ OTP version: 28.3.1 (matches local)
✅ Work orders: 3 completed, 0 in progress
✅ Branch: claude/fix-session-race-DNaeK (pushed to origin)

# Step 3: Fetch state (local)
Fetching branch claude/fix-session-race-DNaeK...
Downloading session state (.erlmcp/session-state.json)...
Syncing chat history (142 messages)...

# Step 4: Verify integrity (local)
Verifying git tree SHA: abc123def456... ✅
Verifying work order checksums... ✅

# Step 5: Ready
✅ Session teleported successfully!
📂 Branch: claude/fix-session-race-DNaeK
💬 Chat history: 142 messages loaded
🎯 Next: Continue development locally
```

**Local → Cloud**:
```bash
# Step 1: Request offload (local terminal)
$ claude --offload "Complete implementation and create PR"

# Step 2: Push local state (local)
Checking git status... ✅ (clean)
Pushing branch to origin... ✅
Uploading session state... ✅

# Step 3: Create cloud session (Claude spawns)
🌐 Cloud session created: session_017kMNOpQrSTuv
📥 Loading local state...
✅ Ready for autonomous execution

# Step 4: Resume (cloud)
Resuming work orders:
- wo-004: Complete FSM implementation (in progress)
- wo-005: Create PR with quality gates (queued)

# Step 5: Autonomous completion (cloud)
[... execution continues autonomously ...]

# Step 6: Report back (cloud → local notification)
✅ Implementation completed
✅ PR created: https://github.com/user/erlmcp/pull/42
💰 Cost: $0.12 | Time: 8m 30s
```

## Error Recovery Guarantees

**Principle** : ∀error. recoverable(error) ∨ human_intervention(error) (no silent failures)

### Auto-Recovery Errors

| Error Class | Recovery Strategy | Timeout |
|-------------|-------------------|---------|
| Compilation error | Report to user → await fix | ∞ (blocking) |
| Test failure | Report to user → await fix | ∞ (blocking) |
| Network timeout | Retry 3x (exponential backoff) | 30s |
| Dependency fetch | Clear cache → retry | 60s |
| Git conflict | Attempt rebase → manual if fails | 120s |
| OTP crash | Supervisor restart → continue | 5s |
| Rate limit | Wait + retry (respect 429 headers) | 300s |

**Example** : Network timeout recovery
```erlang
%% erlmcp_cloud_recovery:handle_network_timeout/1
handle_network_timeout(Url) ->
    Backoff = [1000, 2000, 4000],  % Exponential backoff
    retry_with_backoff(Url, Backoff).

retry_with_backoff(Url, [Delay|Rest]) ->
    timer:sleep(Delay),
    case httpc:request(Url) of
        {ok, Response} -> {ok, Response};
        {error, timeout} when Rest =/= [] -> retry_with_backoff(Url, Rest);
        {error, timeout} -> {error, network_unreachable}
    end.
```

### Manual Intervention Errors

| Error Class | Reason | User Action Required |
|-------------|--------|---------------------|
| API design decision | Ambiguity | Choose between options A/B |
| Breaking change | Risk assessment | Approve/reject change |
| Security credential | Missing secret | Provide API key/token |
| Merge conflict | Semantic conflict | Resolve manually |
| License violation | Legal review | Approve dependency |
| Budget exceeded | Cost limit | Increase budget OR abort |

**Example** : Merge conflict requiring manual intervention
```
⚠️ Merge conflict detected in src/erlmcp_client.erl

Conflict: Lines 42-58 (both branches modified handle_call/3)

Branch A (yours): Added timeout parameter
Branch B (main): Added error handling

🤖 Auto-merge failed (semantic conflict)
👤 Manual intervention required

Options:
1. Resolve locally (claude --teleport)
2. Cherry-pick changes (keep A, apply B manually)
3. Abort (discard branch)

Your choice (1/2/3)?
```

### Contributing New Recovery Hooks

**Hook Location** : `.claude/hooks/recovery/`

**Hook Interface**:
```bash
#!/usr/bin/env bash
# .claude/hooks/recovery/handle_dependency_error.sh
#
# Args: $1 = error_type, $2 = error_message, $3 = context_json
# Returns: 0 (recovered) | 1 (manual intervention) | 2 (fatal)

ERROR_TYPE="$1"
ERROR_MSG="$2"
CONTEXT="$3"

if [[ "$ERROR_TYPE" == "dependency_not_found" ]]; then
    # Attempt recovery: update rebar.config with hex.pm latest
    PACKAGE=$(echo "$CONTEXT" | jq -r '.package')
    rebar3 update "$PACKAGE"
    exit 0  # Recovered
fi

exit 1  # Manual intervention required
```

**Registration** (CLAUDE.md section):
```markdown
## Error Recovery Hooks

| Error Type | Hook | Auto-Recover? |
|------------|------|---------------|
| dependency_not_found | recovery/handle_dependency_error.sh | ✅ |
| compilation_error | recovery/handle_compilation_error.sh | ❌ |
| test_failure | recovery/handle_test_failure.sh | ❌ |
```

## Armstrong Principles in Cloud

**Integration** : All Armstrong principles enforced identically in cloud and local environments.

| Principle | Cloud Implementation | Verification |
|-----------|---------------------|--------------|
| Chicago TDD | ✅ Real processes in cloud VMs | Pre-commit hooks |
| NO Mocks | ✅ Enforced by pre-commit hook | `make check` scans for meck usage |
| Real Processes | ✅ Full Erlang VM in cloud (OTP 28.3.1) | SessionStart hook validates |
| Coverage ≥80% | ✅ Tracked in cloud via rebar3 cover | Blocking quality gate |
| Let-It-Crash | ✅ Supervision trees run identically | CT suite tests crash isolation |
| gen_server init/1 | ✅ Dialyzer checks for blocking calls | Advisory warning |
| Supervision | ✅ All spawns supervised | Xref analysis |
| Black-Box Testing | ✅ Tests observe behavior, not impl | Code review checks |

### TDD in Cloud

**Workflow**:
1. **Red** (30s) : Write failing test in cloud → `make eunit` fails
2. **Green** (60s) : Implement minimal code → `make eunit` passes
3. **Refactor** (90s) : Clean code → `make check` verifies quality

**Example** (cloud session):
```bash
# Red: Write failing test
$ cat > apps/erlmcp_core/test/erlmcp_example_tests.erl <<EOF
-module(erlmcp_example_tests).
-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(42, erlmcp_example:answer()).
EOF

$ rebar3 eunit --module=erlmcp_example_tests
==> Testing erlmcp_example_tests
  Failed: 0.  Skipped: 0.  Passed: 0.

  undefined function: erlmcp_example:answer/0
❌ FAIL (expected - TDD red phase)

# Green: Implement minimal code
$ cat > apps/erlmcp_core/src/erlmcp_example.erl <<EOF
-module(erlmcp_example).
-export([answer/0]).

answer() -> 42.
EOF

$ rebar3 eunit --module=erlmcp_example_tests
==> Testing erlmcp_example_tests
  Test passed.
✅ PASS (TDD green phase)

# Refactor: (no changes needed for this simple example)
$ make check
✅ All quality gates passed
```

### Mock Detection in Cloud

**Pre-Commit Hook** (.git/hooks/pre-commit):
```bash
#!/usr/bin/env bash
# Enforce NO MOCKS policy

if git diff --cached --name-only | grep -q '\.erl$'; then
    if git diff --cached | grep -E '(meck:|test_server:|mock_)' > /dev/null; then
        echo "❌ COMMIT BLOCKED: Mock usage detected"
        echo "Policy: Mocks ∪ Fakes ∪ Placeholders = ∅"
        echo "Use real processes instead (Chicago TDD)"
        exit 1
    fi
fi

exit 0
```

### Coverage Tracking in Cloud

**Automatic** : Coverage report generated on every `make check`

**Report Format** (.erlmcp/coverage-report.txt):
```
Module                      | Coverage | Lines | Covered
----------------------------|----------|-------|--------
erlmcp_client              | 95%      | 234   | 222
erlmcp_server              | 92%      | 189   | 174
erlmcp_session_manager     | 88%      | 145   | 128
erlmcp_registry            | 100%     | 67    | 67
----------------------------|----------|-------|--------
TOTAL                      | 93%      | 635   | 591

✅ PASS (≥80% required)
```

**Cloud Optimization** : Coverage tracked incrementally (only changed modules)

## Documentation

| Location | Content |
|----------|---------|
| docs/architecture.md | System design |
| docs/otp-patterns.md | Supervision |
| docs/api-reference.md | API spec |
| docs/protocol.md | MCP spec |
| docs/SESSION_PERSISTENCE.md | Session backends |
| docs/SECRETS_MANAGEMENT.md | Vault |
| docs/metrology/METRICS_GLOSSARY.md | Units |
| .claude/{agents,commands,hooks,scripts,templates}/ | Agent infra |
| examples/ | 40+ implementations |
| archive/{phase-reports,quality-reports,benchmarks,troubleshooting,tasks,misc}/ | Reference (150 files) |

**Root Files** : CLAUDE.md, README.md, DEVELOPMENT.md, CHANGELOG.md, CONTRIBUTING.md, DOCUMENTATION_GUIDE.md, CODE_QUALITY_REPORT_V2.1.md, CODE_REVIEW_QUICK_REFERENCE.md, CLI_USAGE.md, RELEASE_NOTES_v2.1.0.md

## Directory Structure

```
apps/{erlmcp_core,erlmcp_transports,erlmcp_observability,erlmcp_validation}/
  └── src/ test/
.claude/{agents,commands,hooks,scripts,templates}/
docs/ (850+) | scripts/ (85+) | examples/ (40+)
```

CODE LIKE A JOE ARMSTRONG AGI SWARM!!!
