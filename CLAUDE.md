CODE LIKE A JOE ARMSTRONG AGI SWARM!!!

# CLAUDE.md - erlmcp Formal Specification

## System Identity Kernel (Σ)

**erlmcp** := Erlang/OTP Model Context Protocol SDK. Bidirectional AI-to-service message bridge. Client⊕Server duality over JSON-RPC 2.0.

**Basis** : B ≔ ⟨Σ, τ, O, Δ, Λ⟩
- Σ = {core, transports, observability, validation} : 164 modules : OTP 25-28
- Build = rebar3 (umbrella) : Test ∈ {EUnit, CommonTest, Proper}
- Version = 2.1.0 : Status = ProductionReady

**Module Cardinality** : |Σ| = 164
- |core| = 97 ∧ |transports| = 23 ∧ |observability| = 31 ∧ |validation| = 13
- |tests| = 84 EUnit + CT suites : |docs| = 850+ : |examples| = 40+

## Command Operators (Mandatory Gate Sequence)

**Gate_Quality** : {Compile, Test, Bench} → {⊢, ⊣}

```bash
# MANDATORY sequence before ⊢ (construct/done)
TERM=dumb rebar3 compile           # Gate₁: Compilation ⊢ ⟺ errors = 0
rebar3 eunit --module=M_tests      # Gate₂: Unit tests ⊢ ⟺ failures = 0
rebar3 ct --suite=test/S           # Gate₃: Integration ⊢ ⟺ pass_rate = 1.0
make benchmark-quick               # Gate₄: Performance ⊢ ⟺ regression < 0.1

# Development operators
make console                       # REPL : Erlang shell
make check                         # Total gate: compile ∧ xref ∧ dialyzer ∧ tests
make observer                      # Process topology visualization
rebar3 dialyzer                   # Type checking: warnings → 0
rebar3 xref                       # Cross-reference: undefined → ∅

# Performance measurement operators (v1.5.0)
erlmcp_bench_core_ops:run(<<"core_ops_100k">>)         # μ = 2.69M ops/sec
erlmcp_bench_network_real:run(<<"tcp_sustained_10k">>) # Real sockets
erlmcp_bench_stress:run(<<"stress_5min_100k_ops">>)    # Sustained load
erlmcp_bench_chaos:run(<<"chaos_memory_exhaustion">>)  # Failure injection
erlmcp_bench_integration:run(<<"mcp_tool_sequence">>)  # MCP e2e
./scripts/bench/run_all_benchmarks.sh                  # Full suite (10-15min)
```

## System Topology (Σ-Structure)

**Σ** = {core, transports, observability, validation}

| Σᵢ | Purpose | |Σᵢ| | Test Coverage | v |
|-----|---------|------|---------------|---|
| core | Protocol implementation | 97 | 84 EUnit | 2.1.0 |
| transports | Transport polymorphism | 23 | + CT | 2.1.0 |
| observability | Telemetry & chaos | 31 | + CT | 2.1.0 |
| validation | Compliance gates | 13 | + CT | 2.1.0 |

## Supervision Hierarchy (3-Tier Invariant)

**Supervision** : TIER₁ ⊃ TIER₂ ⊃ TIER₃ (strict containment)

```
TIER₁ (one_for_all) : Registry + Infrastructure
  erlmcp_sup ⊃ {erlmcp_core_sup, erlmcp_registry(gproc)}

TIER₂ (simple_one_for_one) : Protocol servers
  ∀conn ∈ Connections. ∃proc ∈ Processes. isolated(proc, conn)
  erlmcp_server_sup → erlmcp_server*
  erlmcp_client_sup → erlmcp_client*
  erlmcp_session_manager → erlmcp_session*

TIER₃ (isolated) : Observability (failure ⊬ cascade)
  erlmcp_observability_sup ⊃ {metrics_server, dashboard_server, tracing}
```

## Architecture Invariants (Q : Preserve(Q))

**Process-per-Connection** : ∀c ∈ Connections. ∃!p ∈ GenServers. handles(p, c)

**Request-ID Correlation** : ∀req ∈ Requests. ∃!id ∈ UUID. tracks(State.pending, id, req)

**Registry-Based Routing** : gproc : Name × Pid → Route. O(log N) lookup.

**Let-It-Crash Semantics** : failure(Child) → restart(Child) ⊬ failure(Sibling). Bounded intensity.

**Transport Polymorphism** : τ : {stdio, tcp, http, ws, sse} → Behavior{init/2, send/2, close/1}

**Black-Box Observability** : Test ⊨ ∀interface ∈ {JSON-RPC, stdio, HTTP, WS, TCP}. observable_behavior(interface)

## Transport Behavior (τ-Interface)

**Behavior Contract** : `-behaviour(erlmcp_transport)`

**Message Algebra** :
- {transport_data, Binary} : Data ingress
- {transport_connected, Info} : Connection ⊢
- {transport_disconnected, Reason} : Connection ⊣

**Callback Signatures** :
- init : (TransportType × Options) → {ok, State} | {error, Reason}
- send : (Data × State) → {ok, State'} | {error, Reason}
- close : State → ok

**State Record Invariant** :
- transport_module ∈ τ
- capabilities ⊆ {resources, tools, prompts}
- req_id_map : UUID → Request
- pending_map : UUID → AsyncOp

## Configuration Spaces

### Session Persistence Backend Hierarchy

**Backend** : erlmcp_session_backend → {ETS, DETS, Mnesia}

```erlang
% ETS: In-memory (fastest) - O(1) access
{erlmcp_session, [
    {backend, erlmcp_session_ets},
    {backend_opts, #{table_name => T, cleanup_interval => 60000}}
]}.

% DETS: Disk persistence - Durability over speed
{erlmcp_session, [
    {backend, erlmcp_session_dets},
    {backend_opts, #{
        table_name => T,
        file_path => "data/T.dets",
        auto_save => 60000,
        cleanup_interval => 60000
    }}
]}.

% Mnesia: Distributed cluster - CAP theorem trade-offs
{erlmcp_session, [
    {backend, erlmcp_session_mnesia},
    {backend_opts, #{
        table_name => T,
        nodes => [n₁@h, n₂@h],
        disc_copies => true,
        cleanup_interval => 60000
    }}
]}.
```

### Secrets Management (Vault Abstraction)

**Secrets** : Backend → {Vault, AWS, LocalEncrypted}

```erlang
% HashiCorp Vault (production)
{erlmcp_secrets, [
    {backend, vault},
    {backend_config, #{
        address => "https://vault:8200",
        auth_method => approle,
        role_id => R, secret_id => S,
        engine => "kv", mount => "secret"
    }},
    {ttl_seconds => 300}
]}.

% AWS Secrets Manager
{erlmcp_secrets, [
    {backend, aws_secrets_manager},
    {backend_config, #{
        region => "us-east-1",
        access_key_id => K₁,
        secret_access_key => K₂
    }},
    {ttl_seconds => 300}
]}.

% Local encrypted (fallback) - AES-256
{erlmcp_secrets, [
    {backend, local_encrypted},
    {encryption_key => {env_var, "ERLMCP_SECRET_KEY"}},
    {storage_path => "priv/secrets/secrets.enc"},
    {ttl_seconds => 300}
]}.
```

**References** : docs/{SESSION_PERSISTENCE.md, SECRETS_MANAGEMENT.md}

## Toyota Production System as Quality Operators

**TPS** : {Andon, PokaYoke, Jidoka, Kaizen} → Quality(System)

### Andon (行灯) : Visible Error Signaling

**Andon** : erlmcp_health_monitor → Alert(Degradation)
- Real-time dashboard : `/andon` endpoint
- Circuit breakers ≡ andon cords
- Threshold violations → immediate visibility

### Poka-Yoke (ポカヨケ) : Mistake-Proofing

**PokaYoke** : ∀layer ∈ Layers. validation(layer) = mandatory
- Schema : jesse (JSON Schema) - structural impossibility of invalid messages
- Transport : behavior compliance - type system prevents non-conformance
- Message size : bounded validation - DoS impossible
- URI : erlmcp_uri_validator - injection impossible
- Refusal codes : [1001, 1089] - bounded error space

### Jidoka (自働化) : Built-in Quality

**Jidoka** : Test(X) = ⊣ → ¬Commit(X)
- Pre-commit hooks : blocking quality gates
- CI/CD : 20 workflows, failure → halt
- Test execution : automatic on every build
- Coverage : ≥80% (blocking)

### Kaizen (改善) : Continuous Improvement

**Kaizen** : Incremental(Improvement) → Cumulative(Excellence)
- Chaos engineering : erlmcp_chaos - resilience through controlled failure
- Benchmarking : performance optimization via measurement
- Receipt chain : immutable audit trail - cryptographic proof
- Evidence bundles : release artifacts - reproducibility

## Agent Orchestration (Parallel Execution Invariant)

**CRITICAL INVARIANT** : ∀agents ∈ AgentSet. launch(agents) ∈ SingleMessage

### Correct Pattern (⊢)

```
Message₁ : {Task(a₁), Task(a₂), ..., Task(aₙ), TodoWrite(todos), Read*, Write*}
         ⟹ Parallel execution, maximum throughput
```

### Incorrect Pattern (⊣)

```
Message₁ : Task(a₁)
Message₂ : Task(a₂)  ⟹ Sequential execution, n× latency
Messageₙ : Task(aₙ)
```

### Agent Type Mapping

**Agents** : erlmcp → {OTPDev, TestEng, Architect, Researcher, GitHubOps, Perf, TransportBuilder, Reviewer}

| Agent | Domain |
|-------|--------|
| erlang-otp-developer | gen_server, supervisor, OTP behaviors |
| erlang-test-engineer | EUnit, CT, Proper, Chicago TDD |
| erlang-architect | Supervision trees, system design |
| erlang-researcher | Codebase exploration, pattern analysis |
| erlang-github-ops | Git workflows, PR, CI/CD |
| erlang-performance | Benchmarking, optimization |
| erlang-transport-builder | Transport behavior implementation |
| code-reviewer | Quality review, OTP compliance |

### 20-Agent Parallel Launch Template

```
[Single Message - ∀i ∈ [1,20]. Task(aᵢ) ∈ Message₁]:

# Foundation layer (3 agents)
Task("Spec Parser", "erlmcp_spec_parser.erl ⊢ MCP 2025-11-25", "erlang-otp-developer")
Task("Test Client", "erlmcp_test_client.erl ⊢ multi-transport", "erlang-otp-developer")
Task("Compliance Report", "erlmcp_compliance_report.erl ⊢ evidence", "erlang-otp-developer")

# Validator layer (4 agents)
Task("Protocol Validator", "erlmcp_protocol_validator.erl ⊢ JSON-RPC+MCP", "erlang-otp-developer")
Task("Transport Validator", "erlmcp_transport_validator.erl ⊢ behavior", "erlang-otp-developer")
Task("Security Validator", "erlmcp_security_validator.erl ⊢ auth+secrets", "erlang-otp-developer")
Task("Performance Validator", "erlmcp_performance_validator.erl ⊢ metrics", "erlang-otp-developer")

# Test layer (11 agents)
Task("Lifecycle Tests", "|Tests| = 10 ⊢ lifecycle", "erlang-test-engineer")
Task("Tools API Tests", "|Tests| = 12 ⊢ tools", "erlang-test-engineer")
Task("Resources API Tests", "|Tests| = 14 ⊢ resources", "erlang-test-engineer")
Task("Prompts API Tests", "|Tests| = 8 ⊢ prompts", "erlang-test-engineer")
Task("Transport Tests", "|Tests| = 15 ⊢ transport", "erlang-test-engineer")
Task("Error Codes Tests", "|Tests| = 12 ⊢ errors", "erlang-test-engineer")
Task("Protocol Validator Tests", "|Tests| ≥ 50", "erlang-test-engineer")
Task("Transport Validator Tests", "|Tests| ≥ 40", "erlang-test-engineer")
Task("Security Validator Tests", "|Tests| ≥ 60", "erlang-test-engineer")
Task("Performance Validator Tests", "|Tests| ≥ 30", "erlang-test-engineer")
Task("Spec Parser Tests", "|Tests| ≥ 20", "erlang-test-engineer")

# Integration layer (2 agents)
Task("CLI Integration", "erlmcp_validate_cli.erl ⊢ validators", "erlang-otp-developer")
Task("CI/CD Integration", ".github/workflows/ ⊢ quality-gates", "erlang-github-ops")

# Batch operations (single TodoWrite)
TodoWrite{todos: [t₁, t₂, ..., t₂₅]}
```

**Execution Semantics** :
1. Single message → all agents launch in parallel
2. Independent context → no blocking dependencies
3. Maximum throughput → O(1) latency instead of O(n)

## Development Invariants (Chicago School TDD)

**TDD** : ∀code ∈ Implementation. ∃!test ∈ Tests. test ⊢ code

**ABSOLUTE PROHIBITION** : Mocks ∪ Fakes ∪ Placeholders = ∅

**Black-Box Testing** : Test ⊨ Observable(Behavior) ⊬ Implementation(Details)

**Real Processes** : ∀test ∈ TestSuite. uses_real_erlmcp_processes(test) = true

**All Interfaces** : Coverage ⊇ {JSON-RPC, stdio, HTTP, WebSocket, TCP}

**Property-Based** : Proper for generative testing - ∀x ∈ Domain. property(x) = true

## OTP Pattern Enforcement

**gen_server** : Behavior compliance mandatory. init/1 never blocks → async cast initialization.

**Supervision** : ∀child ∈ Children. ∃supervisor ∈ Supervisors. supervises(supervisor, child)

**Monitoring** : ∀critical ∈ Processes. monitor(critical) ∨ link(critical)

**spawn/1** : ∀p ∈ NewProcesses. supervised(p) = true (unsupervised spawn → ⊣)

## Transport Implementation Contract

**Behavior** : `-behaviour(erlmcp_transport)` (mandatory)

**Callbacks** : {init/2, send/2, close/1} → all must be implemented

**Template** : apps/erlmcp_transports/src/erlmcp_transport_tcp.erl

**Registration** : erlmcp_transport_registry - automatic discovery

**Testing** : Real transport instances only - NO MOCKS

## Code Style Gates

**Format** : rebar3_format : line_length ≤ 100

**Dialyzer** : type_warnings → 0 (strict mode)

**Xref** : undefined_functions = ∅

**Coverage** : ∀module ∈ Modules. coverage(module) ≥ 0.8

## File Writing Policy (CRITICAL)

**WritePolicy** : Output ⊆ {.erl, .hrl, .app.src, test/*.erl}

**PROHIBITION** : ¬Write({.md, .txt, .log}) unless explicit_request(User)

**Rationale** : CI/CD captures outputs. Repository clutter → ⊣. Code only → ⊢.

**Exception** : docs/*.md ⟺ explicit_request(User) = true

## Chaos Engineering (Controlled Failure Injection)

**Chaos** : erlmcp_chaos → Failure(Controlled) → Resilience(Measured)

**Scenarios** : |FailureTypes| = 11
- Network : {latency, packet_loss}
- Process : {kill, spawn_storm}
- Resource : {memory_exhaustion, cpu_saturation}

**Recovery Target** : ∀failure ∈ Failures. recovery_time(failure) < 5s

**Validation** : Circuit breakers trigger appropriately ∧ bounded refusals

## Observability Architecture

**OpenTelemetry** : erlmcp_otel → {Datadog, Honeycomb, Jaeger}

**Tracing** : Distributed spans with correlation across process boundaries

**Metrics** : erlmcp_metrics → canonical units (zero ambiguity)

**Health** : erlmcp_health_monitor → `/metrics` endpoint

**Dashboard** : erlmcp_dashboard_server → real-time visibility

## Common Anti-Patterns (⊣)

1. init/1 blocking → ⊣ (use async cast)
2. Large messages → ⊣ (reference shared data)
3. Unmonitored processes → ⊣ (always monitor critical dependencies)
4. Missing timeouts → ⊣ (default 5000ms minimum)
5. Unsupervised spawn → ⊣ (always use supervisor)
6. Untested chaos → ⊣ (production failures inevitable)
7. Ignored health checks → ⊣ (andon detection too late)
8. Mock usage → ⊣ (Chicago School violation)
9. Testing implementation → ⊣ (test observable behavior only)
10. Placeholder code → ⊣ (real implementations only)
11. Markdown output → ⊣ (code only unless requested)

## Quick Index (Task → File Mapping)

| Task | Module | App |
|------|--------|-----|
| MCP protocol | erlmcp_json_rpc.erl | core |
| Resource subscriptions | erlmcp_server.erl:subscribe_resource* | core |
| Session persistence | erlmcp_session_backend.erl | core |
| Secrets management | erlmcp_secrets.erl | core |
| Server tool registration | erlmcp_server.erl:add_tool* | core |
| Client tool invocation | erlmcp_client.erl:call_tool | core |
| Transport template | erlmcp_transport_tcp.erl | transports |
| Message routing | erlmcp_registry.erl | core |
| Test debug | rebar3 eunit --module=M --verbose | - |
| OTEL integration | erlmcp_otel.erl | observability |
| Chaos testing | erlmcp_chaos.erl | observability |
| Health dashboard | erlmcp_dashboard_server.erl | observability |
| Compliance validation | erlmcp_compliance_report.erl | validation |

## Core Module Taxonomy (|Σ| = 164)

### Protocol Layer (|core| = 97)

**Message Flow** :
- erlmcp_client.erl : Request-response correlation via State.pending : UUID → Request
- erlmcp_server.erl : Resources/tools/prompts/subscriptions : Handler functions
- erlmcp_registry.erl : gproc-based routing : O(log N) lookup
- erlmcp_registry_dist.erl : Distributed coordination across nodes
- erlmcp_json_rpc.erl : JSON-RPC 2.0 encode/decode with validation

**Session Management** :
- erlmcp_session.erl : State management
- erlmcp_session_backend.erl : Persistence behavior interface
- erlmcp_session_ets.erl : In-memory backend (fastest)
- erlmcp_session_dets.erl : Disk backend (durable)
- erlmcp_session_mnesia.erl : Cluster backend (distributed)
- erlmcp_session_manager.erl : Lifecycle coordination
- erlmcp_session_failover.erl : Failover orchestration
- erlmcp_session_replicator.erl : Replication protocol

**Security** :
- erlmcp_auth.erl : Authentication/authorization
- erlmcp_auth_mtls.erl : Mutual TLS
- erlmcp_auth_rate_limiter.erl : Auth-specific rate limiting
- erlmcp_secrets.erl : Vault/AWS/local encrypted secrets

**MCP Capabilities** :
- erlmcp_capabilities.erl : Negotiation
- erlmcp_resources.erl : Resource management
- erlmcp_resource.erl : Individual resource handler
- erlmcp_resource_subscriptions.erl : Subscription protocol
- erlmcp_tool.erl : Tool invocation
- erlmcp_prompt_template.erl : Prompt management
- erlmcp_prompt_list_change_notifier.erl : Change notifications
- erlmcp_progress.erl : Progress tokens
- erlmcp_sampling.erl : Sampling strategies
- erlmcp_completion.erl : Completion handling

**LLM Integration** :
- erlmcp_llm_provider_anthropic.erl : Claude integration
- erlmcp_llm_provider_openai.erl : GPT integration
- erlmcp_llm_provider_local.erl : Local model support
- erlmcp_mock_llm.erl : Test harness

**Resilience** :
- erlmcp_circuit_breaker.erl : Circuit breaker pattern
- erlmcp_rate_limiter.erl : Rate limiting
- erlmcp_rate_limit_middleware.erl : Middleware injection
- erlmcp_connection_monitor.erl : Health monitoring
- erlmcp_connection_limiter.erl : Connection limits

**Cache** :
- erlmcp_cache.erl : Caching layer
- erlmcp_cache_warmer.erl : Pre-warming
- erlmcp_icon_cache.erl : Icon caching

**Message Handling** :
- erlmcp_message_handler.erl : Routing
- erlmcp_message_parser.erl : Parsing
- erlmcp_message_size.erl : Size validation
- erlmcp_notification_handler.erl : Notifications
- erlmcp_change_notifier.erl : Change events
- erlmcp_subscription.erl : Subscription management
- erlmcp_sse_event_store.erl : SSE storage

**Utilities** :
- erlmcp_batch.erl : Batch operations
- erlmcp_cancellation.erl : Request cancellation
- erlmcp_pagination.erl : Pagination
- erlmcp_path_canonicalizer.erl : Path normalization
- erlmcp_request_id.erl : UUID generation
- erlmcp_refusal.erl : Refusal code management [1001-1089]
- erlmcp_errors.erl : Error handling
- erlmcp_logging.erl : Structured logging
- erlmcp_health.erl : Health checks
- erlmcp_hooks.erl : Hook system
- erlmcp_schema_registry.erl : Schema management

**Resource Protection** :
- erlmcp_memory_guard.erl : Memory protection
- erlmcp_memory_monitor.erl : Memory monitoring
- erlmcp_cpu_guard.erl : CPU protection
- erlmcp_cpu_quota.erl : Quota management
- erlmcp_graceful_drain.erl : Graceful shutdown

**Distributed** :
- erlmcp_code_reload.erl : Hot code reloading
- erlmcp_node_monitor.erl : Node health
- erlmcp_split_brain_detector.erl : Split-brain detection
- erlmcp_failover_worker.erl : Failover coordination
- erlmcp_test_sync.erl : Test synchronization

**Quality** :
- tcps_quality_gates.erl : TCPS enforcement

### Transport Layer (|transports| = 23)

**Interface** :
- erlmcp_transport_behavior.erl : Behavior contract
- erlmcp_transport_adapter.erl : Adapter pattern

**Implementations** :
- erlmcp_transport_stdio.erl : STDIO (process I/O)
- erlmcp_transport_tcp.erl : TCP (ranch acceptors)
- erlmcp_transport_http.erl : HTTP client (gun)
- erlmcp_transport_http_server.erl : HTTP server (cowboy)
- erlmcp_transport_ws.erl : WebSocket
- erlmcp_transport_sse.erl : Server-Sent Events
- erlmcp_transport_sse_manager.erl : SSE connection manager

**Infrastructure** :
- erlmcp_transport_pool.erl : Connection pooling
- erlmcp_transport_pipeline.erl : Pipeline pattern
- erlmcp_transport_registry.erl : Transport discovery
- erlmcp_transport_health.erl : Health checks
- erlmcp_transport_validation.erl : Compliance validation
- erlmcp_transport_discovery.erl : Service discovery (K8s, Consul)
- erlmcp_pool_manager.erl : Pool strategies
- erlmcp_pool_strategy.erl : Strategy patterns

**Security** :
- erlmcp_http_header_validator.erl : Header validation
- erlmcp_origin_validator.erl : CORS validation
- erlmcp_security_headers.erl : Security header management
- erlmcp_tls_validation.erl : Certificate validation

**OTP** :
- erlmcp_transports_app.erl : Application callback
- erlmcp_transport_sup.erl : Supervisor

### Observability Layer (|observability| = 31)

**OpenTelemetry** :
- erlmcp_otel.erl : OTEL integration
- erlmcp_otel_middleware.erl : Middleware injection
- erlmcp_otel_datadog.erl : Datadog exporter
- erlmcp_otel_honeycomb.erl : Honeycomb exporter
- erlmcp_otel_jaeger.erl : Jaeger exporter

**Tracing** :
- erlmcp_tracing.erl : Distributed tracing
- erlmcp_trace_analyzer.erl : Trace analysis

**Metrics** :
- erlmcp_metrics.erl : Collection
- erlmcp_metrics_server.erl : HTTP server
- erlmcp_metrics_aggregator.erl : Aggregation
- erlmcp_metrology_validator.erl : Validation (canonical units)

**Dashboard** :
- erlmcp_dashboard_server.erl : Web interface
- erlmcp_dashboard_http_handler.erl : HTTP handler

**Monitoring** :
- erlmcp_health_monitor.erl : Health monitoring
- erlmcp_process_monitor.erl : Process monitoring

**Debugging** :
- erlmcp_debugger.erl : Debug tools
- erlmcp_profiler.erl : Performance profiling
- erlmcp_memory_analyzer.erl : Memory analysis

**Chaos Engineering** :
- erlmcp_chaos.erl : Coordinator
- erlmcp_chaos_network.erl : Network failures
- erlmcp_chaos_process.erl : Process failures
- erlmcp_chaos_resource.erl : Resource exhaustion
- erlmcp_chaos_worker.erl : Worker processes
- erlmcp_chaos_worker_sup.erl : Worker supervisor
- erlmcp_recovery_manager.erl : Recovery orchestration

**Audit** :
- erlmcp_audit_log.erl : Audit logging
- erlmcp_receipt_chain.erl : Immutable receipts (cryptographic)
- erlmcp_evidence_path.erl : Evidence tracking

**Benchmarking** :
- erlmcp_bench_rate_limit.erl : Rate limit benchmarks

**OTP** :
- erlmcp_observability_app.erl : Application callback
- erlmcp_observability_sup.erl : Supervisor

### Validation Layer (|validation| = 13)

**Reporting** :
- erlmcp_compliance_report.erl : Compliance reporting
- erlmcp_compliance_report_html.erl : HTML generation
- erlmcp_compliance_report_json.erl : JSON generation

**Test Infrastructure** :
- erlmcp_test_client.erl : Multi-transport test client

**Validators** :
- erlmcp_protocol_validator.erl : JSON-RPC + MCP compliance
- erlmcp_transport_validator.erl : Transport behavior compliance
- erlmcp_security_validator.erl : Auth + secrets + input validation
- erlmcp_performance_validator.erl : Latency + throughput + memory

**Specification** :
- erlmcp_spec_parser.erl : MCP spec parser (2025-11-25)
- erlmcp_uri_validator.erl : URI validation

**Resource Management** :
- erlmcp_memory_manager.erl : Memory validation

**CLI** :
- erlmcp_validate_cli.erl : Command-line interface

**OTP** :
- erlmcp_validation_app.erl : Application callback

## Quality Gates (Mandatory Gate Sequence)

**Gate** : Compile ∧ Test ∧ Coverage ∧ Bench → {⊢, ⊣}

### Gate₁: Compilation

```
⊢ : errors = 0 ∧ |warnings| → min
⊣ : errors > 0
```

### Gate₂: Tests

```
⊢ : pass_rate = 1.0 ∧ failures = 0
⊣ : failures > 0
```

### Gate₃: Coverage

```
⊢ : ∀m ∈ Modules. coverage(m) ≥ 0.8
⊣ : ∃m. coverage(m) < 0.8
```

### Gate₄: Benchmarks (if perf code changed)

```
⊢ : regression < 0.1
⊣ : regression ≥ 0.1
```

**Targets** : {errors = 0, failures = 0, coverage ≥ 0.8, regression < 0.1}

## Automatic Validation Rules (Machine-Enforceable)

**Enforcement** : Hooks ∪ CI/CD → Quality(Mandatory)

- ☑ Compilation: errors = 0 (blocking)
- ☑ Tests: pass_rate = 1.0 (blocking)
- ☑ Coverage: ≥80% (blocking)
- ☑ Dialyzer: warnings → 0 (advisory)
- ☑ Xref: undefined = ∅ (advisory)
- ☑ Benchmarks: regression < 10% (blocking if perf code)

**Enforcement Points** :
- Pre-commit: ./tools/claude-md-sync.sh
- Post-task: Agent completion hooks
- CI/CD: 20 GitHub Actions workflows
- Manual: ./tools/claude-md-enforcer.sh

**Philosophy** : "Quality is not an act, it is a habit." - Aristotle

Violations → impossible to commit (Poka-Yoke principle)

## Dependency Graph (v2.1.0)

### Core Dependencies

**Essential** :
- jsx 3.1.0 : JSON encode/decode
- jesse 1.8.1 : JSON Schema validation
- gproc 0.9.0 : Process registry (O(log N))
- gun 2.0.1 : HTTP client
- ranch 2.1.0 : TCP acceptor pool
- poolboy 1.5.2 : Pool management
- cowboy 2.10.0 : HTTP server
- bbmustache 1.12.2 : Template engine
- jose 1.11.1 : JWT validation (CRITICAL - security dependency)

### Observability Dependencies

**Telemetry** :
- opentelemetry_api 1.5.0 : OTEL API
- opentelemetry 1.7.0 : OTEL SDK
- opentelemetry_exporter 1.10.0 : Exporters

### Test Dependencies

**Testing** :
- proper 1.4.0 : Property-based testing
- meck 0.9.2 : Mocking (use sparingly - Chicago TDD)
- coveralls 2.2.0 : Coverage reporting

### Dev Dependencies

**Development** :
- recon 2.5.3 : Runtime debugging
- observer_cli 1.7.4 : CLI observer
- rebar3_format : Code formatting
- rebar3_lint 3.0.1 : Linting
- rebar3_proper 0.12.1 : Property testing plugin
- rebar3_hex : Hex.pm publishing
- rebar3_auto : Auto-compilation

## Benchmark Suite (v1.5.0)

**Benchmarks** : {core_ops, network_real, stress, chaos, integration}

### Module Specifications

1. **erlmcp_bench_core_ops** : Registry/queue/pool/session. Workloads ∈ {1K, 10K, 100K, 1M}. μ = 2.69M ops/sec.
2. **erlmcp_bench_network_real** : TCP/HTTP real sockets. 7 workloads. TCP: [100, 100K]. HTTP: [100, 5K].
3. **erlmcp_bench_stress** : Sustained load. Durations ∈ {30s, 5min, 1hr, 24hr}. Time-series monitoring.
4. **erlmcp_bench_chaos** : 11 failure scenarios. Bounded refusal validation. Recovery < 5s target.
5. **erlmcp_bench_integration** : 5 MCP workflows. E2e latency. Protocol compliance verification.

### Performance Baseline (Jan 2026)

**Throughput** :
- Registry: 553K msg/s
- Queue: 971K msg/s
- Pool: 149K msg/s
- Session: 242K msg/s
- Network I/O: 43K msg/s (bottleneck: 4KB real packets)
- Sustained: 372K msg/s (60M ops / 30s)

**Capacity** : 40-50K concurrent connections per node (honest measurement)

**Scaling** : >100K connections → requires Mnesia clustering

## Metrology Compliance (v1.5.0)

**Units** : Canonical, unambiguous, validated before write

**Mandatory Fields** :
- throughput_msg_per_s (NOT "req/s")
- latency_p{50,95,99}_us (raw microseconds)
- memory_heap_mib_per_conn (scope: per_connection_heap)
- memory_rss_mib_per_node (scope: per_node_total)
- {workload_id, transport, duration_s, scope, precision}

**Validation** : erlmcp_metrology_validator → zero ambiguities

**Reference** : docs/metrology/METRICS_GLOSSARY.md

## Version Matrix

| Component | v | Status |
|-----------|---|--------|
| erlmcp_core | 2.1.0 | Production |
| erlmcp_transports | 2.1.0 | Production |
| erlmcp_observability | 2.1.0 | Production |
| erlmcp_validation | 2.1.0 | Production |
| Erlang/OTP | 25-28 | Supported |
| Release | 2.1.0 | Production Ready |

## Critical Rules (Invariant Enforcement)

1. **Completion Gate** : ¬done ⟺ ¬(compile ∧ test ∧ report)
2. **Performance Gate** : perf_code_changed → benchmark_required
3. **Report Format** : {⊢: ✅, ⊣: ❌, Advisory: ⚠️}
4. **Quality Non-Negotiable** : {errors = 0, failures = 0, coverage ≥ 0.8, regression < 0.1}
5. **OTP Patterns** : {gen_server, supervision, isolation, let-it-crash}
6. **TPS Integration** : {Andon, Poka-Yoke, Jidoka, Kaizen} → Quality(System)
7. **Code Only** : Output ⊆ {.erl, .hrl} unless explicit_request(docs)

## Armstrong-AGI Principle

**Core Tenet** : Don't build systems that promise correct behavior. Build systems where incorrect behavior cannot exist.

**Application to erlmcp** :
- Supervision trees → crash isolation impossible to violate
- Behavior contracts → type system enforces compliance
- Quality gates → violations impossible to commit
- Black-box testing → implementation details hidden
- Chaos engineering → resilience verified, not assumed

**Operable(B)** ⟺ ∀x ∈ Inputs. Deterministic ∧ Replayable ∧ Complete ∧ NoPartials ∧ Preserve(Q)

## Documentation Topology

### Primary References

**Architecture** :
- docs/architecture.md : System design
- docs/otp-patterns.md : Supervision patterns

**API** :
- docs/api-reference.md : API specification
- examples/ : 40+ implementations

**Protocol** :
- docs/protocol.md : MCP specification
- docs/SESSION_PERSISTENCE.md : Session backends
- docs/SECRETS_MANAGEMENT.md : Vault integration
- docs/metrology/METRICS_GLOSSARY.md : Canonical units

### Agent Infrastructure

**Claude Code Integration** (.claude/) :
- AGENT_INDEX.md : 10+ specialized erlmcp agents
- COMMAND_INDEX.md : 30+ TCPS commands
- ERLANG_OTP_AGENT_GUIDE.md : OTP development guide
- SYSTEM_GUIDE.md : System overview

**Directories** :
- agents/ : Agent definitions
- commands/ : TCPS implementations
- hooks/ : Quality gate validators
- scripts/ : Automation
- templates/ : Code generation

### Testing

**Methodology** : Chicago School TDD (STRICT)

**Principles** :
- Tests FIRST (test/*_tests.erl)
- NO mocks/fakes/placeholders
- Real erlmcp processes only
- All interfaces: {JSON-RPC, stdio, HTTP, WS, TCP}
- Coverage ≥ 80%

**References** : apps/*/test/ for patterns

### CI/CD

**Workflows** : 20 GitHub Actions in .github/workflows/

**Enforcement** : Quality gates on every commit

**Hooks** : Pre-commit validation prevents violations

## Directory Structure

```
apps/
├── erlmcp_core/              # |modules| = 97
│   ├── src/                  # Protocol, sessions, auth, secrets, LLM
│   └── test/                 # 84 EUnit modules
├── erlmcp_transports/        # |modules| = 23
│   ├── src/                  # stdio, tcp, http, ws, sse
│   └── test/                 # CT suites
├── erlmcp_observability/     # |modules| = 31
│   ├── src/                  # OTEL, chaos, metrics, tracing
│   └── test/                 # CT suites
├── erlmcp_validation/        # |modules| = 13
│   ├── src/                  # Validators, compliance, spec parser
│   └── test/                 # CT suites
.claude/                      # Agent orchestration
├── agents/                   # Specialized agents
├── commands/                 # TCPS commands
├── hooks/                    # Quality gates
├── scripts/                  # Automation
└── templates/                # Code generation
docs/                         # |files| = 850+
scripts/                      # |files| = 85+
examples/                     # |files| = 40+
```

CODE LIKE A JOE ARMSTRONG AGI SWARM!!!
