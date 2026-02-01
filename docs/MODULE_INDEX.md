# erlmcp Module Index v2.1.0

## Overview

This document provides a complete index of all 164 modules across the 4 OTP applications in erlmcp v2.1.0.

**Module Statistics:**
| Application | Modules | Purpose |
|-------------|---------|---------|
| **erlmcp_core** | 97 | Protocol implementation, sessions, auth, LLM integration |
| **erlmcp_transports** | 23 | Transport polymorphism (stdio, tcp, http, ws, sse) |
| **erlmcp_observability** | 31 | Telemetry, chaos engineering, metrics, tracing |
| **erlmcp_validation** | 13 | Compliance validation, spec parsing, test infrastructure |
| **Total** | **164** | Complete MCP SDK |

**Visual Reference:**
- See [`diagrams/module-dependencies.mmd`](diagrams/module-dependencies.mmd) for dependency graph
- See [`diagrams/system-architecture.mmd`](diagrams/system-architecture.mmd) for system overview

---

## erlmcp_core (97 modules)

### Protocol Layer (8 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_json_rpc` | JSON-RPC 2.0 codec | gen_server |
| `erlmcp_message_handler` | Message routing & dispatch | gen_server |
| `erlmcp_message_parser` | Parse & validate messages | gen_server |
| `erlmcp_message_size` | Message size validation | - |
| `erlmcp_request_id` | UUID generation for requests | - |
| `erlmcp_refusal` | Refusal code management [1001-1089] | gen_server |
| `erlmcp_errors` | Error handling utilities | - |
| `erlmcp_logging` | Structured logging | - |

### Session Management (10 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_session_manager` | Session lifecycle coordination | supervisor |
| `erlmcp_session` | Session state management | gen_server |
| `erlmcp_session_backend` | Backend behavior interface | - |
| `erlmcp_session_ets` | In-memory backend (fastest) | gen_server |
| `erlmcp_session_dets` | Disk backend (durable) | gen_server |
| `erlmcp_session_mnesia` | Cluster backend (distributed) | gen_server |
| `erlmcp_session_failover` | Failover orchestration | gen_server |
| `erlmcp_session_replicator` | Replication protocol | gen_server |
| `erlmcp_pagination` | Pagination support | - |
| `erlmcp_cancellation` | Request cancellation | - |

### MCP Capabilities (12 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_capabilities` | Capability negotiation | - |
| `erlmcp_resources` | Resource management | gen_server |
| `erlmcp_resource` | Individual resource handler | gen_server |
| `erlmcp_resource_subscriptions` | Subscription protocol | gen_server |
| `erlmcp_tool` | Tool invocation | gen_server |
| `erlmcp_prompt_template` | Prompt management | gen_server |
| `erlmcp_prompt_list_change_notifier` | Change notifications | gen_server |
| `erlmcp_progress` | Progress token support | gen_server |
| `erlmcp_sampling` | Sampling strategies | gen_server |
| `erlmcp_completion` | Completion handling | gen_server |
| `erlmcp_subscription` | Subscription management | gen_server |
| `erlmcp_sse_event_store` | SSE event storage | gen_server |

### Security & Auth (5 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_auth` | Authentication/authorization | gen_server |
| `erlmcp_auth_mtls` | Mutual TLS support | gen_server |
| `erlmcp_auth_rate_limiter` | Auth-specific rate limiting | gen_server |
| `erlmcp_secrets` | Secrets management (Vault/AWS/Local) | gen_server |
| `erlmcp_uri_validator` | URI validation (injection prevention) | gen_server |

### Resilience & Protection (10 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_circuit_breaker` | Circuit breaker pattern | gen_server |
| `erlmcp_rate_limiter` | Rate limiting | gen_server |
| `erlmcp_connection_monitor` | Health monitoring | gen_server |
| `erlmcp_connection_limiter` | Connection limits | gen_server |
| `erlmcp_memory_guard` | Memory protection | gen_server |
| `erlmcp_memory_monitor` | Memory monitoring | gen_server |
| `erlmcp_cpu_guard` | CPU protection | gen_server |
| `erlmcp_cpu_quota` | Quota management | gen_server |
| `erlmcp_graceful_drain` | Graceful shutdown | gen_server |
| `erlmcp_cache` | Multi-level caching | gen_server |

### Client & Server (6 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_client` | MCP client implementation | gen_server |
| `erlmcp_client_sup` | Client supervisor | supervisor |
| `erlmcp_server` | MCP server implementation | gen_server |
| `erlmcp_server_sup` | Server supervisor | supervisor |
| `erlmcp_registry` | gproc-based routing | gen_server |
| `erlmcp_registry_dist` | Distributed coordination | gen_server |

### LLM Integration (4 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_llm_provider_anthropic` | Claude integration | gen_server |
| `erlmcp_llm_provider_openai` | GPT integration | gen_server |
| `erlmcp_llm_provider_local` | Local model support | gen_server |
| `erlmcp_mock_llm` | Test harness | gen_server |

### Message & Batch Processing (6 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_batch` | Batch operations | - |
| `erlmcp_notification_handler` | Notification routing | gen_server |
| `erlmcp_change_notifier` | Change events | gen_server |
| `erlmcp_path_canonicalizer` | Path normalization | - |
| `erlmcp_hooks` | Hook system | - |
| `erlmcp_schema_registry` | Schema management | gen_server |

### Distributed Systems (8 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_code_reload` | Hot code reload | gen_server |
| `erlmcp_node_monitor` | Node health | gen_server |
| `erlmcp_split_brain_detector` | Split-brain detection | gen_server |
| `erlmcp_failover_worker` | Failover coordination | gen_server |
| `erlmcp_failover_worker_sup` | Failover supervisor | supervisor |
| `erlmcp_test_sync` | Test synchronization | - |
| `erlmcp_cluster_sup` | Cluster supervisor | supervisor |
| `erlmcp_reload_sup` | Code reload supervisor | supervisor |

### Utilities & Support (6 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_cache_warmer` | Cache pre-warming | gen_server |
| `erlmcp_cache_warmer_sup` | Cache warmer supervisor | supervisor |
| `erlmcp_notification_handler_sup` | Notification supervisor | supervisor |
| `erlmcp_health` | Health checks | - |
| `tcps_quality_gates` | TCPS enforcement | gen_server |
| `erlmcp_app` | Application callback | application |
| `erlmcp_sup` | Top-level supervisor | supervisor |
| `erlmcp_core_sup` | Core supervisor | supervisor |

**See also:** [`diagrams/system-architecture.mmd`](diagrams/system-architecture.mmd) - Core Layer section

---

## erlmcp_transports (23 modules)

### Transport Interface (2 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_transport_behavior` | Transport behavior contract | - |
| `erlmcp_transport_adapter` | Adapter pattern | gen_server |

### Transport Implementations (6 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_transport_stdio` | STDIO transport (process I/O) | gen_server |
| `erlmcp_transport_tcp` | TCP transport (ranch) | gen_server |
| `erlmcp_transport_http` | HTTP client (gun) | gen_server |
| `erlmcp_transport_http_server` | HTTP server (cowboy) | gen_server |
| `erlmcp_transport_ws` | WebSocket transport | gen_server |
| `erlmcp_transport_sse` | Server-Sent Events | gen_server |

### Infrastructure (11 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_transport_pool` | Connection pooling | supervisor |
| `erlmcp_transport_pipeline` | Pipeline pattern | gen_server |
| `erlmcp_transport_registry` | Transport discovery | gen_server |
| `erlmcp_transport_health` | Health checks | gen_server |
| `erlmcp_transport_validation` | Compliance validation | gen_server |
| `erlmcp_transport_discovery` | Service discovery (K8s, Consul) | gen_server |
| `erlmcp_pool_manager` | Pool strategies | gen_server |
| `erlmcp_pool_strategy` | Strategy patterns | - |
| `erlmcp_sse_manager` | SSE connection manager | gen_server |
| `erlmcp_transport_sup` | Transport supervisor | supervisor |
| `erlmcp_transports_app` | Application callback | application |

### Security (4 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_http_header_validator` | Header validation | gen_server |
| `erlmcp_origin_validator` | CORS validation | gen_server |
| `erlmcp_security_headers` | Security header management | gen_server |
| `erlmcp_tls_validation` | Certificate validation | gen_server |

**See also:** [`diagrams/transport-interfaces.mmd`](diagrams/transport-interfaces.mmd)

---

## erlmcp_observability (31 modules)

### OpenTelemetry Integration (5 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_otel` | OTEL integration | gen_server |
| `erlmcp_otel_middleware` | Middleware injection | - |
| `erlmcp_otel_datadog` | Datadog exporter | gen_server |
| `erlmcp_otel_honeycomb` | Honeycomb exporter | gen_server |
| `erlmcp_otel_jaeger` | Jaeger exporter | gen_server |

### Metrics & Tracing (6 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_metrics` | Metrics collection | gen_server |
| `erlmcp_metrics_server` | HTTP server | gen_server |
| `erlmcp_metrics_aggregator` | Aggregation | gen_server |
| `erlmcp_tracing` | Distributed tracing | gen_server |
| `erlmcp_trace_analyzer` | Trace analysis | gen_server |
| `erlmcp_metrology_validator` | Unit validation | gen_server |

### Dashboard & Monitoring (4 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_dashboard_server` | Web interface | gen_server |
| `erlmcp_dashboard_http_handler` | HTTP handler | - |
| `erlmcp_health_monitor` | Health monitoring | gen_server |
| `erlmcp_process_monitor` | Process monitoring | gen_server |

### Chaos Engineering (7 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_chaos` | Chaos coordinator | gen_server |
| `erlmcp_chaos_network` | Network failures | gen_server |
| `erlmcp_chaos_process` | Process failures | gen_server |
| `erlmcp_chaos_resource` | Resource exhaustion | gen_server |
| `erlmcp_chaos_worker` | Worker processes | gen_server |
| `erlmcp_chaos_worker_sup` | Worker supervisor | supervisor |
| `erlmcp_recovery_manager` | Recovery orchestration | gen_server |

### Audit & Debugging (5 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_audit_log` | Audit logging | gen_server |
| `erlmcp_receipt_chain` | Immutable receipts (cryptographic) | gen_server |
| `erlmcp_evidence_path` | Evidence tracking | gen_server |
| `erlmcp_debugger` | Debug tools | gen_server |
| `erlmcp_profiler` | Performance profiling | gen_server |

### Benchmarking (1 module)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_bench_rate_limit` | Rate limit benchmarks | - |

### OTP Application (3 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_observability_app` | Application callback | application |
| `erlmcp_observability_sup` | Observability supervisor | supervisor |
| `erlmcp_otel` | OTEL integration | gen_server |

**See also:** [`diagrams/observability/telemetry-flow.mmd`](diagrams/observability/telemetry-flow.mmd)

---

## erlmcp_validation (13 modules)

### Validators (4 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_protocol_validator` | JSON-RPC + MCP compliance | gen_server |
| `erlmcp_transport_validator` | Transport behavior compliance | gen_server |
| `erlmcp_security_validator` | Auth + secrets + input validation | gen_server |
| `erlmcp_performance_validator` | Latency + throughput + memory | gen_server |

### Compliance Reporting (3 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_compliance_report` | Compliance reporting | gen_server |
| `erlmcp_compliance_report_html` | HTML generation | gen_server |
| `erlmcp_compliance_report_json` | JSON generation | gen_server |

### Test Infrastructure (2 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_test_client` | Multi-transport test client | gen_server |
| `erlmcp_memory_manager` | Memory validation | gen_server |

### Specification & CLI (3 modules)

| Module | Purpose | Behavior |
|--------|---------|----------|
| `erlmcp_spec_parser` | MCP spec parser (2025-11-25) | gen_server |
| `erlmcp_validate_cli` | Command-line interface | - |
| `erlmcp_validation_app` | Application callback | application |

**See also:** [`diagrams/validation/validator-architecture.mmd`](diagrams/validation/validator-architecture.mmd)

---

## Module Dependency Categories

### Foundation Layer (No Internal Dependencies)

```
erlmcp_json_rpc
erlmcp_message_handler
erlmcp_message_parser
erlmcp_registry
erlmcp_auth
erlmcp_secrets
erlmcp_circuit_breaker
erlmcp_rate_limiter
```

### Core Services (Depend on Foundation)

```
erlmcp_client
erlmcp_server
erlmcp_session_manager
erlmcp_resources
erlmcp_tool
erlmcp_prompt_template
```

### Transport Layer (Depend on Foundation + Core)

```
erlmcp_transport_stdio
erlmcp_transport_tcp
erlmcp_transport_http
erlmcp_transport_ws
erlmcp_transport_sse
```

### Observability Layer (Independent, Monitored by Core)

```
erlmcp_metrics
erlmcp_otel
erlmcp_dashboard_server
erlmcp_chaos
erlmcp_recovery_manager
```

### Validation Layer (Depends on All)

```
erlmcp_protocol_validator
erlmcp_transport_validator
erlmcp_security_validator
erlmcp_performance_validator
erlmcp_compliance_report
```

---

## Quick Reference Tables

### By Behavior Type

| Behavior | Count | Examples |
|----------|-------|----------|
| **gen_server** | 120 | client, server, registry, auth |
| **supervisor** | 20 | server_sup, transport_sup, observability_sup |
| **application** | 4 | erlmcp_app, transports_app, observability_app, validation_app |
| **module** (utilities) | 20 | json_rpc, message_size, logging |

### By Supervision Strategy

| Strategy | Purpose | Examples |
|----------|---------|----------|
| **one_for_all** | Application-level | erlmcp_sup, core_sup |
| **one_for_one** | Independent services | transport_sup, observability_sup |
| **simple_one_for_one** | Dynamic workers | server_sup, client_sup, session_manager |
| **rest_for_one** | Pipeline dependencies | (rare, used in v1.3.0) |

### By Criticality

| Level | Count | Description |
|-------|-------|-------------|
| **Critical** | 15 | Registry, client, server, auth, session |
| **High** | 40 | Transports, resources, tools, observability |
| **Medium** | 60 | Validators, monitors, cache, resilience |
| **Low** | 49 | Utilities, helpers, test infrastructure |

---

## Module Diagram References

**System Architecture:**
- [`diagrams/system-architecture.mmd`](diagrams/system-architecture.mmd) - All 164 modules visualized

**Layer-Specific Diagrams:**
- [`diagrams/protocol/`](diagrams/protocol/) - Protocol layer modules
- [`diagrams/transports/`](diagrams/transports/) - Transport modules
- [`diagrams/observability/`](diagrams/observability/) - Observability modules
- [`diagrams/validation/`](diagrams/validation/) - Validation modules

**Cross-Cutting Diagrams:**
- [`diagrams/module-dependencies.mmd`](diagrams/module-dependencies.mmd) - Dependency graph
- [`diagrams/reference/module-index.mmd`](diagrams/reference/module-index.mmd) - Complete index

---

## Usage Examples

### Finding a Module by Purpose

**Question:** "Which module handles JSON-RPC encoding?"

**Answer:** See Protocol Layer table → `erlmcp_json_rpc`

**Question:** "Where is the TCP transport implemented?"

**Answer:** See Transport Implementations → `erlmcp_transport_tcp`

**Question:** "How do I validate MCP protocol compliance?"

**Answer:** See Validators → `erlmcp_protocol_validator`

### Understanding Module Relationships

**Example 1: Client Request Flow**
```
erlmcp_client (发起请求)
  → erlmcp_json_rpc (编码)
  → erlmcp_registry (路由)
  → erlmcp_transport_* (发送)
```

**Example 2: Session Lifecycle**
```
erlmcp_session_manager (协调)
  → erlmcp_session (状态)
  → erlmcp_session_ets/dets/mnesia (持久化)
  → erlmcp_session_failover (故障转移)
```

**Example 3: Observability Flow**
```
erlmcp_server (处理请求)
  → erlmcp_otel (创建 span)
  → erlmcp_metrics (记录指标)
  → erlmcp_receipt_chain (审计日志)
```

---

## Index Maintenance

**Last Updated:** 2026-01-31

**Version:** erlmcp v2.1.0

**Module Count:** 164

**Update Procedure:**
1. Run `find apps -name "*.erl" -type f | wc -l` to verify count
2. Update module counts in each section
3. Add new modules to appropriate category
4. Update dependency graph
5. Regenerate Mermaid diagrams

**Related Documentation:**
- [`architecture.md`](architecture.md) - System architecture overview
- [`otp-patterns.md`](otp-patterns.md) - OTP design patterns
- [`diagrams/`](diagrams/) - Complete Mermaid diagram suite
