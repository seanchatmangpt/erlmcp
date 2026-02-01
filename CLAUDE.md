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
TERM=dumb rebar3 compile           # Gate₁: errors = 0
rebar3 eunit --module=M_tests      # Gate₂: failures = 0
rebar3 ct --suite=test/S           # Gate₃: pass_rate = 1.0
make benchmark-quick               # Gate₄: regression < 0.1
make check                         # All: compile ∧ xref ∧ dialyzer ∧ tests
make console | make observer       # REPL | Process visualization
rebar3 dialyzer | rebar3 xref      # Types → 0 | Undefined → ∅
```

**Benchmarks** : `erlmcp_bench_{core_ops,network_real,stress,chaos,integration}:run/1`

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

| Agent | Domain |
|-------|--------|
| erlang-otp-developer | gen_server, supervisor, OTP behaviors |
| erlang-test-engineer | EUnit, CT, Proper, Chicago TDD |
| erlang-architect | Supervision trees, system design |
| erlang-researcher | Codebase exploration, patterns |
| erlang-github-ops | Git, PR, CI/CD |
| erlang-performance | Benchmarking, optimization |
| erlang-transport-builder | Transport implementations |
| code-reviewer | Quality, OTP compliance |

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

## Anti-Patterns (⊣)

1. init/1 blocking  2. Large messages (use refs)  3. Unmonitored procs  4. Missing timeouts (<5000ms)
5. Unsupervised spawn  6. Untested chaos  7. Ignored health  8. Mock usage  9. Testing impl details
10. Placeholder code  11. Markdown output without request

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

| Gate | Pass (⊢) | Fail (⊣) |
|------|----------|----------|
| Compile | errors = 0 | errors > 0 |
| Tests | failures = 0 | failures > 0 |
| Coverage | ≥ 80% | < 80% |
| Bench | regression < 10% | regression ≥ 10% |
| Dialyzer | warnings → 0 | (advisory) |
| Xref | undefined = ∅ | (advisory) |

**Enforcement** : Pre-commit hooks | CI/CD 20 workflows | ./tools/claude-md-{sync,enforcer}.sh

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

## Armstrong-AGI Principle

**Core** : Build systems where incorrect behavior cannot exist.

- Supervision → crash isolation impossible to violate
- Behaviors → type system enforces compliance
- Gates → violations impossible to commit
- Black-box → implementation hidden
- Chaos → resilience verified

**Operable(B)** ⟺ Deterministic ∧ Replayable ∧ Complete ∧ NoPartials ∧ Preserve(Q)

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
