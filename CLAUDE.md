CODE LIKE A JOE ARMSTRONG AGI SWARM!!!

# CLAUDE.md - erlmcp Development Guide

## Project Identity
**erlmcp**: Erlang/OTP MCP SDK. Client+server for AI-to-service protocol.
**Language**: Erlang/OTP 25-28
**Build**: rebar3 (umbrella project)
**Test**: EUnit, Common Test, Proper
**Architecture**: 4 applications (core, transports, observability, validation)

## Essential Commands
```bash
# Build & Test (MANDATORY BEFORE "DONE")
TERM=dumb rebar3 compile                    # Compile (MUST pass)
rebar3 eunit --module=<module>_tests        # Unit tests (MUST run)
rebar3 ct --suite=test/<suite>              # Integration (if applicable)
make benchmark-quick                         # Benchmark (if perf-critical, < 2min)

# Development
make console        # Erlang shell
make check          # Full: compile, xref, dialyzer, tests
make observer       # Process visualization
rebar3 dialyzer     # Type checking
rebar3 xref         # Cross-reference

# Benchmarks (v1.5.0)
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).        # In-memory: 2.69M ops/sec
erlmcp_bench_network_real:run(<<"tcp_sustained_10k">>). # TCP sockets
erlmcp_bench_stress:run(<<"stress_5min_100k_ops">>).   # Sustained load
erlmcp_bench_chaos:run(<<"chaos_memory_exhaustion">>).  # Failure injection
erlmcp_bench_integration:run(<<"mcp_tool_sequence">>).  # MCP e2e
./scripts/bench/run_all_benchmarks.sh                   # Full suite (10-15min)
```

## Quick Reference

| Application | Purpose | Modules | Tests | Version |
|-------------|---------|---------|-------|---------|
| erlmcp_core | Protocol implementation | 97 | 84 EUnit | 2.1.0 |
| erlmcp_transports | Transport layer | 23 | + CT suites | 2.1.0 |
| erlmcp_observability | Monitoring & metrics | 31 | + CT suites | 2.1.0 |
| erlmcp_validation | Compliance & validation | 13 | + CT suites | 2.1.0 |

## Directory Layout

```
apps/
├── erlmcp_core/              # Protocol implementation
│   ├── src/                  # 97 modules: Client, Server, Registry, JSON-RPC, Sessions, Secrets
│   │   ├── erlmcp_client.erl         # Request-response correlation
│   │   ├── erlmcp_server.erl         # Resources/tools/prompts/subscriptions
│   │   ├── erlmcp_registry.erl       # Central message routing
│   │   ├── erlmcp_json_rpc.erl       # JSON-RPC 2.0 encode/decode
│   │   ├── erlmcp_session.erl        # Session management
│   │   ├── erlmcp_session_backend.erl # Session persistence behavior
│   │   ├── erlmcp_session_ets.erl    # ETS session backend (in-memory)
│   │   ├── erlmcp_session_dets.erl   # DETS session backend (disk)
│   │   ├── erlmcp_session_mnesia.erl # Mnesia session backend (cluster)
│   │   ├── erlmcp_session_manager.erl # Session lifecycle manager
│   │   ├── erlmcp_auth.erl           # Authentication
│   │   ├── erlmcp_auth_mtls.erl      # Mutual TLS authentication
│   │   ├── erlmcp_secrets.erl        # Secrets management (Vault/AWS/local)
│   │   ├── erlmcp_circuit_breaker.erl # Circuit breakers
│   │   ├── erlmcp_rate_limiter.erl   # Rate limiting
│   │   ├── erlmcp_llm_provider_anthropic.erl # Anthropic LLM integration
│   │   ├── erlmcp_llm_provider_openai.erl    # OpenAI LLM integration
│   │   ├── erlmcp_llm_provider_local.erl     # Local LLM support
│   │   └── ...
│   └── test/                 # 84 EUnit test modules
│
├── erlmcp_transports/         # Transport implementations
│   ├── src/                  # 23 modules: stdio, tcp, http, websocket, sse
│   │   ├── erlmcp_transport_behavior.erl   # Transport behavior
│   │   ├── erlmcp_transport_stdio.erl      # STDIO transport
│   │   ├── erlmcp_transport_tcp.erl        # TCP (ranch)
│   │   ├── erlmcp_transport_http.erl       # HTTP (gun/cowboy)
│   │   ├── erlmcp_transport_http_server.erl # HTTP server
│   │   ├── erlmcp_transport_ws.erl         # WebSocket
│   │   ├── erlmcp_transport_sse.erl        # Server-Sent Events
│   │   ├── erlmcp_transport_sse_manager.erl # SSE connection manager
│   │   ├── erlmcp_transport_pool.erl       # Connection pooling
│   │   ├── erlmcp_transport_discovery.erl  # Service discovery (Kubernetes)
│   │   └── ...
│   └── test/                 # Transport tests + CT suites
│
├── erlmcp_observability/      # Monitoring & observability
│   ├── src/                  # 31 modules
│   │   ├── erlmcp_dashboard_server.erl    # Web dashboard
│   │   ├── erlmcp_metrics.erl             # Metrics collection
│   │   ├── erlmcp_metrics_server.erl      # Metrics server
│   │   ├── erlmcp_metrics_aggregator.erl  # Metrics aggregation
│   │   ├── erlmcp_tracing.erl             # Distributed tracing
│   │   ├── erlmcp_trace_analyzer.erl      # Trace analysis
│   │   ├── erlmcp_health_monitor.erl      # Health monitoring
│   │   ├── erlmcp_otel.erl                # OpenTelemetry integration
│   │   ├── erlmcp_otel_datadog.erl        # Datadog exporter
│   │   ├── erlmcp_otel_honeycomb.erl      # Honeycomb exporter
│   │   ├── erlmcp_otel_jaeger.erl         # Jaeger exporter
│   │   ├── erlmcp_chaos.erl               # Chaos engineering
│   │   ├── erlmcp_chaos_network.erl       # Network chaos
│   │   ├── erlmcp_chaos_process.erl       # Process chaos
│   │   ├── erlmcp_chaos_resource.erl      # Resource chaos
│   │   ├── erlmcp_recovery_manager.erl    # Recovery orchestration
│   │   ├── erlmcp_debugger.erl            # Debugging tools
│   │   ├── erlmcp_profiler.erl            # Performance profiling
│   │   ├── erlmcp_memory_analyzer.erl     # Memory analysis
│   │   ├── erlmcp_metrology_validator.erl # Metrics validation
│   │   └── ...
│   └── test/                 # Observability tests + CT suites
│
├── erlmcp_validation/         # Compliance & validation
│   ├── src/                  # 13 modules
│   │   ├── erlmcp_compliance_report.erl      # Compliance reporting
│   │   ├── erlmcp_compliance_report_html.erl # HTML report generation
│   │   ├── erlmcp_compliance_report_json.erl # JSON report generation
│   │   ├── erlmcp_test_client.erl            # Test client for validation
│   │   ├── erlmcp_protocol_validator.erl     # Protocol compliance
│   │   ├── erlmcp_transport_validator.erl    # Transport compliance
│   │   ├── erlmcp_security_validator.erl     # Security compliance
│   │   ├── erlmcp_performance_validator.erl  # Performance validation
│   │   ├── erlmcp_spec_parser.erl            # MCP spec parser
│   │   ├── erlmcp_uri_validator.erl          # URI validation
│   │   ├── erlmcp_validate_cli.erl           # CLI validation tool
│   │   ├── erlmcp_memory_manager.erl         # Memory management validation
│   │   └── ...
│   └── test/                 # Validation tests + CT suites
│
.claude/                      # Claude Code integration
├── agents/                   # Specialized agent definitions
├── commands/                 # TCPS command implementations
├── hooks/                    # Git hooks and validators
├── scripts/                  # Automation scripts
├── templates/                # Code generation templates
├── AGENT_INDEX.md            # Agent registry
├── COMMAND_INDEX.md          # Command reference
└── ERLANG_OTP_AGENT_GUIDE.md # OTP development guide

docs/                         # 850+ documentation files
scripts/                      # 85+ automation scripts
examples/                     # 40+ example implementations
```

## Architecture Fundamentals

### 3-Tier Supervision Architecture

```
TIER 1: CORE (Registry + Infrastructure)
├── erlmcp_sup (one_for_all)
│   ├── erlmcp_core_sup (supervisor of supervisors)
│   └── erlmcp_registry (gproc-based routing)

TIER 2: PROTOCOL SERVERS (simple_one_for_one)
├── erlmcp_server_sup
│   └── erlmcp_server (per-connection)
├── erlmcp_client_sup
│   └── erlmcp_client (per-connection)
└── erlmcp_session_manager
    └── erlmcp_session (per-session)

TIER 3: OBSERVABILITY (Isolated)
├── erlmcp_observability_sup
│   ├── erlmcp_metrics_server
│   ├── erlmcp_dashboard_server
│   └── erlmcp_tracing
```

### Core Patterns

**Process-per-Connection**: Each client/server connection is a separate gen_server process.
**Request ID Correlation**: All requests tracked via unique request IDs in state maps.
**Registry-Based Routing**: gproc registry for efficient message routing.
**Let-It-Crash + Supervisor Recovery**: Processes crash and restart via supervision trees.
**Transport Behavior Interface**: Pluggable transports via `-behaviour(erlmcp_transport)`.
**Black-Box Testing**: Test ALL observable behavior through ALL interfaces (JSON-RPC, stdio, HTTP, WebSocket, TCP)

### Transport Interface

**Behavior**: `erlmcp_transport`
**Messages**:
- `{transport_data, Binary}` - Incoming data
- `{transport_connected, Info}` - Connection established
- `{transport_disconnected, Reason}` - Connection closed

**Callbacks**:
- `init(TransportType, Options) -> {ok, State}`
- `send(Data, State) -> {ok, NewState} | {error, Reason}`
- `close(State) -> ok`

### State Management

**Record-based state** tracks:
- Transport module and configuration
- Supported capabilities (resources, tools, prompts)
- Request ID correlation map
- Pending requests map for async operations

## Configuration Examples

### Session Persistence Configuration

```erlang
%% ETS Backend (In-Memory, Fastest)
{erlmcp_session, [
    {backend, erlmcp_session_ets},
    {backend_opts, #{
        table_name => erlmcp_sessions_ets,
        cleanup_interval => 60000
    }}
]}.

%% DETS Backend (Disk Persistence)
{erlmcp_session, [
    {backend, erlmcp_session_dets},
    {backend_opts, #{
        table_name => erlmcp_sessions_dets,
        file_path => "data/erlmcp_sessions.dets",
        auto_save => 60000,
        cleanup_interval => 60000
    }}
]}.

%% Mnesia Backend (Distributed Cluster)
{erlmcp_session, [
    {backend, erlmcp_session_mnesia},
    {backend_opts, #{
        table_name => erlmcp_session,
        nodes => [node1@host, node2@host],
        disc_copies => true,
        cleanup_interval => 60000
    }}
]}.
```

### Secrets Management Configuration

```erlang
%% HashiCorp Vault
{erlmcp_secrets, [
    {backend, vault},
    {backend_config, #{
        address => "https://vault.example.com:8200",
        auth_method => approle,
        role_id => "your-role-id",
        secret_id => "your-secret-id",
        engine => "kv",
        mount => "secret"
    }},
    {ttl_seconds => 300}
]}.

%% AWS Secrets Manager
{erlmcp_secrets, [
    {backend, aws_secrets_manager},
    {backend_config, #{
        region => "us-east-1",
        access_key_id => "AKIAIOSFODNN7EXAMPLE",
        secret_access_key => "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
    }},
    {ttl_seconds => 300}
]}.

%% Local Encrypted Storage (Fallback)
{erlmcp_secrets, [
    {backend, local_encrypted},
    {encryption_key => {env_var, "ERLMCP_SECRET_KEY"}},
    {storage_path => "priv/secrets/secrets.enc"},
    {ttl_seconds => 300}
]}.
```

See:
- `docs/SESSION_PERSISTENCE.md` - Complete session persistence guide
- `docs/SECRETS_MANAGEMENT.md` - Complete secrets management guide

## Toyota Production System Integration

erlmcp integrates Toyota Production System principles for manufacturing-grade quality:

### Andon (行灯) - Stop-the-Line Signaling
**Implementation**: Visible error signaling via `erlmcp_health_monitor`
- Real-time health status dashboard
- Automatic alerts on degradation
- Circuit breakers trigger andon cords
- Visual indicator: `/andon` endpoint shows system health

### Poka-Yoke (ポカヨケ) - Mistake-Proofing
**Implementation**: Built-in validation at every layer
- Schema validation (jesse JSON Schema)
- Transport behavior compliance checks
- Message size limits and validation
- URI validation (`erlmcp_uri_validator`)
- Refusal code enforcement (1001-1089)

### Jidoka (自働化) - Built-in Quality
**Implementation**: Quality tests stop production on failure
- Pre-commit hooks enforce quality gates
- CI/CD workflows block on test failures
- Automatic test execution on every build
- Coverage requirements (≥80%)

### Kaizen (改善) - Continuous Improvement
**Implementation**: Incremental improvement workflow
- Chaos engineering for resilience testing
- Performance benchmarking for optimization
- Receipt chain for immutable audit trail
- Evidence bundles for every release

## 🚨 CRITICAL: Parallel Agent Execution

**ABSOLUTE RULE: ALL AGENTS MUST BE LAUNCHED IN ONE MESSAGE**

### The Only Correct Pattern

```
✅ CORRECT: Launch ALL agents in a SINGLE message
[Single Message - Parallel Agent Execution]:

  Task("Agent 1", "Task description 1", "agent-type-1")
  Task("Agent 2", "Task description 2", "agent-type-2")
  Task("Agent 3", "Task description 3", "agent-type-3")
  ...
  Task("Agent N", "Task description N", "agent-type-N")

  TodoWrite { todos: [10+ todos in ONE call] }

  [Batch all file operations]
  Read "file1.erl"
  Read "file2.erl"
  ...

  [Batch all writes]
  Write "output1.erl"
  Write "output2.erl"
  ...
```

### ❌ WRONG Patterns

```
❌ WRONG: Multiple messages (breaks parallelism)
Message 1: Task("Agent 1", ...)
Message 2: Task("Agent 2", ...)
Message 3: Task("Agent 3", ...)
```

### Available Agent Types for erlmcp

| Agent Type | Use For |
|------------|---------|
| `erlang-otp-developer` | Implementing Erlang/OTP behaviors, gen_server, supervisor |
| `erlang-test-engineer` | Creating EUnit/Common Test suites with Chicago TDD |
| `erlang-architect` | Designing OTP supervision trees, architecture |
| `erlang-researcher` | Exploring codebase, finding patterns |
| `erlang-github-ops` | Git workflows, PRs, CI/CD |
| `erlang-performance` | Benchmarking, performance optimization |
| `erlang-transport-builder` | Transport implementation (stdio, tcp, http, websocket) |
| `code-reviewer` | Code quality review (Erlang-specific) |

### Example: 20-Agent Parallel Launch

```
[Single Message - All 20 agents launched at once]:

  # Phase 1: Foundation
  Task("Spec Parser", "Implement erlmcp_spec_parser.erl with hardcoded MCP 2025-11-25 spec metadata", "erlang-otp-developer")
  Task("Test Client", "Enhance erlmcp_test_client.erl for multi-transport validation", "erlang-otp-developer")
  Task("Compliance Report", "Enhance erlmcp_compliance_report.erl with evidence collection", "erlang-otp-developer")

  # Phase 2: Validators
  Task("Protocol Validator", "Implement erlmcp_protocol_validator.erl for JSON-RPC 2.0 and MCP validation", "erlang-otp-developer")
  Task("Transport Validator", "Implement erlmcp_transport_validator.erl for transport behavior compliance", "erlang-otp-developer")
  Task("Security Validator", "Implement erlmcp_security_validator.erl for auth, input validation, secrets", "erlang-otp-developer")
  Task("Performance Validator", "Implement erlmcp_performance_validator.erl for latency, throughput, memory", "erlang-otp-developer")

  # Phase 3: Test Suites
  Task("Lifecycle Tests", "Create 10 lifecycle tests in erlmcp_spec_compliance_SUITE.ct", "erlang-test-engineer")
  Task("Tools API Tests", "Create 12 tools API tests in erlmcp_spec_compliance_SUITE.ct", "erlang-test-engineer")
  Task("Resources API Tests", "Create 14 resources API tests in erlmcp_spec_compliance_SUITE.ct", "erlang-test-engineer")
  Task("Prompts API Tests", "Create 8 prompts API tests in erlmcp_spec_compliance_SUITE.ct", "erlang-test-engineer")
  Task("Transport Tests", "Create 15 transport behavior tests in erlmcp_spec_compliance_SUITE.ct", "erlang-test-engineer")
  Task("Error Codes Tests", "Create 12 error code tests in erlmcp_spec_compliance_SUITE.ct", "erlang-test-engineer")
  Task("Protocol Validator Tests", "Create 50+ protocol validator test suite", "erlang-test-engineer")
  Task("Transport Validator Tests", "Create 40+ transport validator test suite", "erlang-test-engineer")
  Task("Security Validator Tests", "Create 60+ security validator test suite", "erlang-test-engineer")
  Task("Performance Validator Tests", "Create 30+ performance validator test suite", "erlang-test-engineer")
  Task("Spec Parser Tests", "Create 20+ spec parser unit tests", "erlang-test-engineer")

  # Phase 4: Integration
  Task("CLI Integration", "Wire up validators in erlmcp_validate_cli.erl", "erlang-otp-developer")
  Task("CI/CD Integration", "Enhance .github/workflows/validation-quality-gates.yml", "erlang-github-ops")
  Task("Documentation", "Create docs/SPEC_PARSER_GUIDE.md, VALIDATOR_GUIDE.md, SPEC_COMPLIANCE_TESTING.md", "erlang-architect")

  # Batch todos
  TodoWrite { todos: [
    {id: "1", content: "Implement erlmcp_spec_parser.erl with hardcoded spec", status: "in_progress"},
    {id: "2", content: "Enhance erlmcp_test_client.erl for multi-transport", status: "pending"},
    {id: "3", content: "Enhance erlmcp_compliance_report.erl with evidence", status: "pending"},
    {id: "4", content: "Implement erlmcp_protocol_validator.erl", status: "pending"},
    {id: "5", content: "Implement erlmcp_transport_validator.erl", status: "pending"},
    {id: "6", content: "Implement erlmcp_security_validator.erl", status: "pending"},
    {id: "7", content: "Implement erlmcp_performance_validator.erl", status: "pending"},
    {id: "8", content: "Create 10 lifecycle tests", status: "pending"},
    {id: "9", content: "Create 12 tools API tests", status: "pending"},
    {id: "10", content: "Create 14 resources API tests", status: "pending"},
    {id: "11", content: "Create 8 prompts API tests", status: "pending"},
    {id: "12", content: "Create 15 transport tests", status: "pending"},
    {id: "13", content: "Create 12 error code tests", status: "pending"},
    {id: "14", content: "Create protocol validator test suite", status: "pending"},
    {id: "15", content: "Create transport validator test suite", status: "pending"},
    {id: "16", content: "Create security validator test suite", status: "pending"},
    {id: "17", content: "Create performance validator test suite", status: "pending"},
    {id: "18", content: "Create spec parser unit tests", status: "pending"},
    {id: "19", content: "Wire up CLI validators", status: "pending"},
    {id: "20", content: "Integrate CI/CD workflows", status: "pending"},
    {id: "21", content: "Create comprehensive documentation", status: "pending"},
    {id: "22", content: "Run TERM=dumb rebar3 compile", status: "pending"},
    {id: "23", content: "Run rebar3 eunit", status: "pending"},
    {id: "24", content: "Run rebar3 ct", status: "pending"},
    {id: "25", content: "Verify ≥80% coverage", status: "pending"}
  ]}
```

### Key Points

1. **ONE message** contains ALL `Task()` calls
2. **ONE message** contains ALL `TodoWrite()` calls
3. **ONE message** contains ALL file operations
4. Agents run in parallel for maximum speed
5. Each agent gets full context and works independently
6. Use specialized agent types for best results

## Best Practices

### TDD: Chicago School (STRICT ENFORCEMENT)
- Write tests FIRST (test/*_tests.erl)
- **ABSOLUTELY NO MOCKS, FAKE, OR PLACEHOLDER IMPLEMENTATIONS**
- Test ALL observable behavior through ALL interfaces (JSON-RPC, stdio, HTTP, WebSocket, TCP)
- Use real erlmcp processes, never mocked/stubbed versions
- Test boundaries, not implementation details
- Property-based tests with Proper

### OTP Patterns
- Follow gen_server/supervisor behaviors strictly
- NEVER block init/1 - use async cast initialization
- ALWAYS monitor critical processes (monitor/2, link/1)
- ALWAYS use supervisors for child processes
- NEVER use unsupervised spawn/1

### Transport Implementation
- Implement `-behaviour(erlmcp_transport)`
- Callbacks: `init/2`, `send/2`, `close/1`
- Template: `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl`
- Auto-registration via `erlmcp_transport_registry`
- **TEST WITH REAL TRANSPORTS, NEVER MOCKED**

### Code Style
- rebar3_format (100-char line length)
- Dialyzer strict type checking
- xref clean (0 undefined functions)
- 80%+ minimum test coverage

### File Writing Policy
- **ONLY WRITE CODE** - Write .erl, .hrl, .app.src, and test files ONLY
- **NEVER write markdown reports, logs, or summary files** unless explicitly requested
- **NO temporary .txt, .md, .log files** - These clutter the repository
- Quality gate results, test outputs, and benchmark results are captured by CI/CD
- Exception: Official documentation in docs/ when explicitly requested

### Chaos Engineering
- Built-in failure injection via `erlmcp_chaos`
- Test recovery scenarios (network, process, memory)
- Validate circuit breakers trigger appropriately
- Recovery time <5s target

### Observability Patterns
- OpenTelemetry integration for distributed tracing
- Structured metrics via `erlmcp_metrics`
- Health monitoring via `erlmcp_health_monitor`
- Dashboard at `/metrics` endpoint

## Common Pitfalls

1. **Blocking init/1** → Use async cast initialization
2. **Large messages** → Reference shared data instead
3. **Unmonitored processes** → Clean up on dependency death
4. **Missing timeouts** → Default 5000ms for clients
5. **Unsupervised spawns** → Always use supervisor
6. **Not testing chaos scenarios** → System fails in production
7. **Ignoring health checks** → Andon catches problems too late
8. **Using mocks in tests** → **VIOLATION** - Chicago School requires real processes
9. **Testing implementation details** → **VIOLATION** - Test only observable behavior
10. **Fake/placeholder capabilities** → **VIOLATION** - Use real MCP implementations
11. **Writing markdown reports/logs** → **VIOLATION** - Only write code (.erl/.hrl) unless explicitly requested

## Key Files Quick Index

| Task | File | Location |
|------|------|----------|
| MCP protocol | docs/protocol.md, erlmcp_json_rpc.erl | core |
| Resource subscriptions | docs/protocol.md, erlmcp_server.erl:subscribe_resource* | core |
| Session persistence | docs/SESSION_PERSISTENCE.md, erlmcp_session_backend.erl | core |
| Secrets management | docs/SECRETS_MANAGEMENT.md, erlmcp_secrets.erl | core |
| Add server tool | erlmcp_server.erl:add_tool* | core |
| Call tool from client | erlmcp_client.erl:call_tool | core |
| New transport | erlmcp_transport_tcp.erl (template) | transports |
| Message routing | erlmcp_registry.erl, docs/otp-patterns.md | core |
| Test failure debug | rebar3 eunit --module=MODULE --verbose | - |
| OTEL integration | erlmcp_otel.erl, erlmcp_tracing.erl | observability |
| Chaos testing | erlmcp_chaos.erl | observability |
| Health dashboard | erlmcp_dashboard_server.erl | observability |
| Compliance validation | erlmcp_compliance_report.erl | validation |

## Core Modules

### Protocol (erlmcp_core) - 97 Modules
- `erlmcp_client.erl` - Request-response correlation via #state.pending map
- `erlmcp_server.erl` - Resources/tools/prompts/subscriptions management with handler functions
- `erlmcp_registry.erl` - Central message routing (gproc-based)
- `erlmcp_registry_dist.erl` - Distributed registry coordination
- `erlmcp_registry_utils.erl` - Registry utility functions
- `erlmcp_json_rpc.erl` - JSON-RPC 2.0 encode/decode
- `erlmcp_session.erl` - Session management and state
- `erlmcp_session_backend.erl` - Session persistence behavior interface
- `erlmcp_session_ets.erl` - ETS session backend (in-memory, fast)
- `erlmcp_session_dets.erl` - DETS session backend (disk persistence)
- `erlmcp_session_mnesia.erl` - Mnesia session backend (distributed cluster)
- `erlmcp_session_manager.erl` - Session lifecycle and failover management
- `erlmcp_session_failover.erl` - Session failover coordination
- `erlmcp_session_replicator.erl` - Session replication
- `erlmcp_auth.erl` - Authentication and authorization
- `erlmcp_auth_mtls.erl` - Mutual TLS authentication
- `erlmcp_auth_rate_limiter.erl` - Auth-specific rate limiting
- `erlmcp_secrets.erl` - Secrets management (Vault/AWS/local encrypted)
- `erlmcp_capabilities.erl` - Capability negotiation
- `erlmcp_resources.erl` - Resource management
- `erlmcp_resource.erl` - Individual resource handling
- `erlmcp_resource_subscriptions.erl` - Resource subscription management
- `erlmcp_tool.erl` - Individual tool handling
- `erlmcp_prompt_template.erl` - Prompt template management
- `erlmcp_prompt_list_change_notifier.erl` - Prompt change notifications
- `erlmcp_progress.erl` - Progress token support
- `erlmcp_sampling.erl` - Sampling strategies
- `erlmcp_completion.erl` - Completion request handling
- `erlmcp_tasks.erl` - Background task management
- `erlmcp_circuit_breaker.erl` - Circuit breaker pattern
- `erlmcp_rate_limiter.erl` - Rate limiting
- `erlmcp_rate_limit_middleware.erl` - Rate limit middleware
- `erlmcp_connection_monitor.erl` - Connection health monitoring
- `erlmcp_connection_limiter.erl` - Connection limiting
- `erlmcp_cache.erl` - Caching layer
- `erlmcp_cache_warmer.erl` - Cache pre-warming
- `erlmcp_icon_cache.erl` - Icon caching
- `erlmcp_llm_provider_anthropic.erl` - Anthropic Claude integration
- `erlmcp_llm_provider_openai.erl` - OpenAI GPT integration
- `erlmcp_llm_provider_local.erl` - Local LLM support
- `erlmcp_mock_llm.erl` - Mock LLM for testing
- `erlmcp_elicitation.erl` - Elicitation strategies
- `erlmcp_batch.erl` - Batch request handling
- `erlmcp_cancellation.erl` - Request cancellation
- `erlmcp_message_handler.erl` - Message routing
- `erlmcp_message_parser.erl` - Message parsing
- `erlmcp_message_size.erl` - Message size validation
- `erlmcp_notification_handler.erl` - Notification handling
- `erlmcp_change_notifier.erl` - Change notifications
- `erlmcp_subscription.erl` - Subscription management
- `erlmcp_sse_event_store.erl` - Server-Sent Events storage
- `erlmcp_pagination.erl` - Pagination support
- `erlmcp_path_canonicalizer.erl` - Path canonicalization
- `erlmcp_request_id.erl` - Request ID generation
- `erlmcp_refusal.erl` - Refusal code management
- `erlmcp_errors.erl` - Error handling
- `erlmcp_logging.erl` - Structured logging
- `erlmcp_health.erl` - Health checks
- `erlmcp_hooks.erl` - Hook system
- `erlmcp_schema_registry.erl` - Schema management
- `erlmcp_memory_guard.erl` - Memory protection
- `erlmcp_memory_monitor.erl` - Memory monitoring
- `erlmcp_cpu_guard.erl` - CPU protection
- `erlmcp_cpu_quota.erl` - CPU quota management
- `erlmcp_graceful_drain.erl` - Graceful shutdown
- `erlmcp_code_reload.erl` - Hot code reloading
- `erlmcp_node_monitor.erl` - Node monitoring
- `erlmcp_split_brain_detector.erl` - Split-brain detection
- `erlmcp_failover_worker.erl` - Failover worker
- `erlmcp_test_sync.erl` - Test synchronization
- `tcps_quality_gates.erl` - TCPS quality gates

### Transports (erlmcp_transports) - 23 Modules
- `erlmcp_transport_behavior.erl` - Transport behavior interface
- `erlmcp_transport_adapter.erl` - Transport adapter pattern
- `erlmcp_transport_stdio.erl` - STDIO transport (I/O)
- `erlmcp_transport_tcp.erl` - TCP transport (ranch)
- `erlmcp_transport_http.erl` - HTTP client transport (gun)
- `erlmcp_transport_http_server.erl` - HTTP server transport (cowboy)
- `erlmcp_transport_ws.erl` - WebSocket transport
- `erlmcp_transport_sse.erl` - Server-Sent Events transport
- `erlmcp_transport_sse_manager.erl` - SSE connection manager
- `erlmcp_transport_pool.erl` - Connection pooling
- `erlmcp_transport_pipeline.erl` - Transport pipeline
- `erlmcp_transport_registry.erl` - Transport registration
- `erlmcp_transport_health.erl` - Transport health checks
- `erlmcp_transport_validation.erl` - Transport compliance validation
- `erlmcp_transport_discovery.erl` - Service discovery (Kubernetes, Consul)
- `erlmcp_pool_manager.erl` - Pool strategy manager
- `erlmcp_pool_strategy.erl` - Pooling strategies
- `erlmcp_http_header_validator.erl` - HTTP header validation
- `erlmcp_origin_validator.erl` - CORS origin validation
- `erlmcp_security_headers.erl` - Security header management
- `erlmcp_tls_validation.erl` - TLS certificate validation
- `erlmcp_transports_app.erl` - Application callback
- `erlmcp_transport_sup.erl` - Transport supervisor

### Observability (erlmcp_observability) - 31 Modules
- `erlmcp_otel.erl` - OpenTelemetry integration
- `erlmcp_otel_middleware.erl` - OTEL middleware
- `erlmcp_otel_datadog.erl` - Datadog exporter
- `erlmcp_otel_honeycomb.erl` - Honeycomb exporter
- `erlmcp_otel_jaeger.erl` - Jaeger exporter
- `erlmcp_tracing.erl` - Distributed tracing
- `erlmcp_trace_analyzer.erl` - Trace analysis
- `erlmcp_metrics.erl` - Metrics collection
- `erlmcp_metrics_server.erl` - Metrics HTTP server
- `erlmcp_metrics_aggregator.erl` - Metrics aggregation
- `erlmcp_metrology_validator.erl` - Metrics validation
- `erlmcp_dashboard_server.erl` - Web dashboard
- `erlmcp_dashboard_http_handler.erl` - Dashboard HTTP handler
- `erlmcp_health_monitor.erl` - Health monitoring
- `erlmcp_process_monitor.erl` - Process monitoring
- `erlmcp_debugger.erl` - Debugging tools
- `erlmcp_profiler.erl` - Performance profiling
- `erlmcp_memory_analyzer.erl` - Memory analysis
- `erlmcp_chaos.erl` - Chaos engineering coordinator
- `erlmcp_chaos_network.erl` - Network chaos (latency, packet loss)
- `erlmcp_chaos_process.erl` - Process chaos (kill, spawn)
- `erlmcp_chaos_resource.erl` - Resource chaos (memory, CPU)
- `erlmcp_chaos_worker.erl` - Chaos worker
- `erlmcp_chaos_worker_sup.erl` - Chaos worker supervisor
- `erlmcp_recovery_manager.erl` - Recovery orchestration
- `erlmcp_audit_log.erl` - Audit logging
- `erlmcp_receipt_chain.erl` - Immutable receipt chain
- `erlmcp_evidence_path.erl` - Evidence path tracking
- `erlmcp_bench_rate_limit.erl` - Rate limit benchmarking
- `erlmcp_observability_app.erl` - Application callback
- `erlmcp_observability_sup.erl` - Observability supervisor

### Validation (erlmcp_validation) - 13 Modules
- `erlmcp_compliance_report.erl` - Compliance reporting
- `erlmcp_compliance_report_html.erl` - HTML report generation
- `erlmcp_compliance_report_json.erl` - JSON report generation
- `erlmcp_test_client.erl` - Test client for validation
- `erlmcp_protocol_validator.erl` - Protocol compliance validation
- `erlmcp_transport_validator.erl` - Transport compliance validation
- `erlmcp_security_validator.erl` - Security compliance validation
- `erlmcp_performance_validator.erl` - Performance validation
- `erlmcp_spec_parser.erl` - MCP specification parser
- `erlmcp_uri_validator.erl` - URI validation
- `erlmcp_memory_manager.erl` - Memory management validation
- `erlmcp_validate_cli.erl` - Validation CLI
- `erlmcp_validation_app.erl` - Application callback

## Quality Gates (MANDATORY)

**BEFORE saying "done", MUST execute + report:**

### 1. Compilation
```
✅ Compiled: X modules, Y BEAM files
⚠️ Warnings: [list]
❌ Errors: [list]
```

### 2. Tests
```
✅ Tests: X/X passed (0 failures)
⚠️ Skipped: [reason]
❌ Failed: [module:line]
```

### 3. Benchmarks (if perf code changed)
```
✅ Benchmark: core_ops_100k - 2.69M ops/sec (no regression)
⚠️ Not run: [reason]
❌ Regression: -X% throughput
```

**Targets**: 0 errors, 100% test pass, ≥80% coverage, <10% perf regression.

## Automatic Validation Rules (Machine-Enforceable)

**These rules are AUTOMATICALLY enforced by hooks and CI/CD:**

- [ ] **Compilation:** MUST have 0 errors (blocking)
- [ ] **Tests:** MUST have 100% pass rate (0 failures)
- [ ] **Coverage:** MUST have ≥80% code coverage
- [ ] **Dialyzer:** SHOULD have 0 type warnings (reported)
- [ ] **Xref:** SHOULD have 0 cross-reference issues (reported)
- [ ] **Benchmarks:** MUST have <10% performance regression (if perf code changed)

**Enforcement Points:**
- ✅ Pre-commit hooks: Validate before git commit
- ✅ Post-task hooks: Validate after agent task completion
- ✅ CI/CD workflows: Validate in GitHub Actions (20 workflows)
- ✅ Manual validation: Run `./tools/claude-md-enforcer.sh`

**Hook Installation:**
```bash
# Install/sync all hooks
./tools/claude-md-sync.sh

# Manual validation anytime
./tools/claude-md-enforcer.sh
```

**Quality Gate Philosophy:**
> "Quality is not an act, it is a habit." - Aristotle
>
> These gates ensure consistent quality by making violations impossible to commit.

## Dependencies (v2.1.0)

### Core Dependencies
- **jsx 3.1.0** - JSON encoding/decoding
- **jesse 1.8.1** - JSON Schema validation
- **gproc 0.9.0** - Process registry
- **gun 2.0.1** - HTTP client
- **ranch 2.1.0** - TCP acceptor pool
- **poolboy 1.5.2** - Pool management
- **cowboy 2.10.0** - HTTP server
- **bbmustache 1.12.2** - Template engine
- **jose 1.11.1** - JWT validation (CRITICAL security dependency)

### Observability Dependencies
- **opentelemetry_api 1.5.0** - OTEL API
- **opentelemetry 1.7.0** - OTEL SDK
- **opentelemetry_exporter 1.10.0** - OTEL exporters

### Test Dependencies
- **proper 1.4.0** - Property-based testing
- **meck 0.9.2** - Mocking library (use sparingly - Chicago School TDD)
- **coveralls 2.2.0** - Coverage reporting

### Dev Dependencies
- **recon 2.5.3** - Runtime debugging
- **observer_cli 1.7.4** - CLI observer
- **rebar3_format** - Code formatting
- **rebar3_lint 3.0.1** - Linting
- **rebar3_proper 0.12.1** - Property-based testing plugin
- **rebar3_hex** - Hex.pm publishing
- **rebar3_auto** - Auto-compilation

## Benchmarks (v1.5.0)

**5 consolidated modules (from 15+ legacy):**

1. **core_ops** - Registry/queue/pool/session. 4 workloads (1K→1M ops). Real: 2.69M ops/sec.
2. **network_real** - TCP/HTTP real sockets. 7 workloads. TCP: 100→100K conn. HTTP: 100→5K conn.
3. **stress** - Sustained load. 4 durations (30s→24hr). Time-series monitoring. Degradation detection.
4. **chaos** - 11 failure scenarios. Bounded refusal validation. Recovery <5s.
5. **integration** - 5 MCP workflows. E2e latency. Protocol compliance.

**Performance baseline (Jan 2026):**
- Registry: 553K msg/s
- Queue: 971K msg/s
- Pool: 149K msg/s
- Session: 242K msg/s
- Network I/O: 43K msg/s (bottleneck: 4KB real packets)
- Sustained: 372K msg/s (60M ops/30s)

**Honest capacity:** 40-50K concurrent active connections per node. 100K+ requires clustering.

## Metrology Compliance (v1.5.0)

All benchmarks output canonical units:
- `throughput_msg_per_s` (NOT ambiguous "req/s")
- `latency_p50_us`, `latency_p95_us`, `latency_p99_us` (raw microseconds)
- `memory_heap_mib_per_conn` (scope: per_connection_heap)
- `memory_rss_mib_per_node` (scope: per_node_total)
- Required: workload_id, transport, duration_s, scope, precision

Validated by erlmcp_metrology_validator before write. Zero ambiguities. See `docs/metrology/METRICS_GLOSSARY.md`.

## Version & Status

| Application | Version | Status |
|-------------|---------|--------|
| erlmcp_core | 2.1.0 | Stable |
| erlmcp_transports | 2.1.0 | Stable |
| erlmcp_observability | 2.1.0 | Stable |
| erlmcp_validation | 2.1.0 | Stable |
| **Erlang** | OTP 25-28 | Supported |
| **Release** | 2.1.0 | Production Ready |

## Critical Rules

1. **NEVER say "done" without running:** compile + tests + report status
2. **Run benchmarks if perf-critical code changed**
3. **Report format:** ✅ Compiled / ✅ Tests / ✅ Benchmark (or ⚠️/❌ with details)
4. **Quality non-negotiable:** 0 errors, 0 test failures, ≥80% coverage, <10% regression
5. **Follow OTP patterns:** gen_server, supervision, process isolation, let-it-crash
6. **Toyota Production System:** Andon, Poka-Yoke, Jidoka, Kaizen principles apply
7. **ONLY WRITE CODE:** Never write .md/.txt/.log reports unless explicitly requested. Write .erl/.hrl code only

## Help Resources

### Documentation
- Architecture: `docs/architecture.md`, `docs/otp-patterns.md`
- API: `docs/api-reference.md` + `examples/`
- Protocol: `docs/protocol.md`
- Session Persistence: `docs/SESSION_PERSISTENCE.md`
- Secrets Management: `docs/SECRETS_MANAGEMENT.md`
- Metrology: `docs/metrology/METRICS_GLOSSARY.md`

### Agents & Commands
- **Agents**: `.claude/AGENT_INDEX.md` (10+ specialized erlmcp agents)
- **Commands**: `.claude/COMMAND_INDEX.md` (30+ TCPS commands)
- **TCPS Methodology**: `docs/tcps/TCPS.md`
- **Agent Guide**: `.claude/ERLANG_OTP_AGENT_GUIDE.md`
- **System Guide**: `.claude/SYSTEM_GUIDE.md`

### Claude Code Integration (.claude/)
- **agents/** - Specialized agent definitions for erlmcp development
- **commands/** - TCPS command implementations
- **hooks/** - Git hooks and quality gate validators
- **scripts/** - Automation scripts
- **templates/** - Code generation templates
- **helpers/** - Utility scripts

### Testing
- Test patterns: `apps/*/test/` for examples
- TDD guide: Chicago School (STRICT - NO MOCKS, FAKE, OR PLACEHOLDER IMPLEMENTATIONS)
- Coverage: Minimum 80% enforced
- **ALL TESTS MUST USE REAL ERLMCP PROCESSES, NEVER MOCKED**
- **TEST ALL INTERFACES: JSON-RPC, STDIO, HTTP, WEBSOCKET, TCP**

### CI/CD
- 20 GitHub Actions workflows in `.github/workflows/`
- Quality gates enforced on every commit
- Pre-commit hooks prevent violations

CODE LIKE A JOE ARMSTRONG AGI SWARM!!!
