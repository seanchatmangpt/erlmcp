# erlmcp Architecture - v2.1.0

## Executive Summary

erlmcp v2.1.0 is a **production-grade umbrella application** implementing the Model Context Protocol (MCP) in Erlang/OTP. It consists of 4 independent OTP applications with 164 modules organized by domain boundaries, enabling AI-to-service communication with fault-tolerant supervision, pluggable transports, and comprehensive observability.

**Architecture Principles:**
- **Separation of Concerns** - 4 focused OTP applications with clear boundaries
- **OTP Supervision Trees** - Fault isolation via bulkhead pattern
- **Library Integration** - Battle-tested Erlang libraries (gproc, gun, ranch, poolboy)
- **Process-per-Connection** - Concurrent, isolated request handling
- **Optional Features** - TCPS quality system can be excluded for minimal deployments
- **Visual Documentation** - Complete Mermaid diagram suite for architecture understanding

## Mermaid Diagram References

This architecture documentation is enhanced with comprehensive Mermaid diagrams located in `/docs/diagrams/`:

| Diagram | Path | Purpose |
|---------|------|---------|
| **System Architecture** | `system-architecture.mmd` | Complete system overview with all 4 layers |
| **Supervision Tree** | `supervision-tree.mmd` | 3-tier supervision hierarchy |
| **Data Flow** | `data-flow.mmd` | Request/response flow with all components |
| **Module Dependencies** | `module-dependencies.mmd` | Inter-module dependency graph |

### Protocol Layer Diagrams

| Diagram | Path | Purpose |
|---------|------|---------|
| **Client-Server Interaction** | `protocol/client-server-interaction.mmd` | Protocol message exchange |
| **Session Lifecycle** | `protocol/session-lifecycle.mmd` | Session state machine |
| **JSON-RPC Flow** | `protocol/json-rpc-flow.mmd` | JSON-RPC 2.0 processing |
| **Capability Negotiation** | `protocol/capability-negotiation.mmd` | MCP capability exchange |
| **Error Handling** | `protocol/error-handling.mmd` | Error flow and recovery |

### Transport Layer Diagrams

| Diagram | Path | Purpose |
|---------|------|---------|
| **Transport Types** | `transports/transport-types.mmd` | All transport implementations |
| **Transport Interfaces** | `transport-interfaces.mmd` | Transport behavior polymorphism |
| **Connection Pooling** | `transports/connection-pooling.mmd` | Pool management strategies |
| **Transport Failover** | `transports/transport-failover.mmd` | Failover mechanisms |
| **Protocol Handlers** | `transports/protocol-handlers.mmd` | Transport-specific handlers |
| **Transport Security** | `transports/transport-security.mmd` | TLS and security |

### Observability Diagrams

| Diagram | Path | Purpose |
|---------|------|---------|
| **Telemetry Flow** | `observability/telemetry-flow.mmd` | OTEL data pipeline |
| **Metrics Collection** | `observability/metrics-collection.mmd` | Metrics aggregation |
| **Tracing Span Tree** | `observability/tracing-span-tree.mmd` | Distributed tracing |
| **Health Monitoring** | `observability/health-monitoring.mmd` | Health check system |
| **Chaos Testing** | `observability/chaos-testing.mmd` | Resilience testing |

### Validation Diagrams

| Diagram | Path | Purpose |
|---------|------|---------|
| **Quality Gates** | `validation/quality-gates.mmd` | Validation pipeline |
| **Test Coverage** | `validation/test-coverage.mmd` | Coverage strategy |
| **Compliance Reporting** | `validation/compliance-reporting.mmd` | Report generation |
| **Validator Architecture** | `validation/validator-architecture.mmd` | Validator design |
| **Benchmarking Framework** | `validation/benchmarking-framework.mmd` | Performance validation |

### Testing Diagrams

| Diagram | Path | Purpose |
|---------|------|---------|
| **Test Coverage Map** | `testing/test-coverage-map.mmd` | Test organization |
| **Unit Test Flow** | `testing/unit-test-flow.mmd` | Unit testing process |
| **Integration Tests** | `testing/integration-tests.mmd` | Integration strategy |
| **Property Testing** | `testing/property-testing.mmd` | Proper testing |
| **Test Data Management** | `testing/test-data-management.mmd` | Test data fixtures |

### Error Handling Diagrams

| Diagram | Path | Purpose |
|---------|------|---------|
| **Error Flow** | `errors/error-flow.mmd` | Error propagation |
| **Circuit Breakers** | `errors/circuit-breakers.mmd` | Circuit breaker pattern |
| **Retry Mechanisms** | `errors/retry-mechanisms.mmd` | Retry strategies |
| **Graceful Degradation** | `errors/graceful-degradation.mmd` | Degradation modes |
| **Monitoring Alerts** | `errors/monitoring-alerts.mmd` | Alert generation |

### Deployment Diagrams

| Diagram | Path | Purpose |
|---------|------|---------|
| **Cluster Topology** | `deployment/cluster-topology.mmd` | Cluster architecture |
| **Load Balancing** | `deployment/load-balancing.mmd` | Load distribution |
| **Failover Mechanisms** | `deployment/failover-mechanisms.mmd` | Failover strategies |
| **Infrastructure Components** | `deployment/infrastructure-components.mmd` | Deployment stack |
| **Scaling Strategies** | `deployment/scaling-strategies.mmd` | Scaling approach |

### Configuration Diagrams

| Diagram | Path | Purpose |
|---------|------|---------|
| **Configuration Hierarchy** | `configuration/configuration-hierarchy.mmd` | Config layers |
| **Environment Variables** | `configuration/environment-variables.mmd` | Environment config |
| **Feature Flags** | `configuration/feature-flags.mmd` | Feature toggles |
| **Validation Pipeline** | `configuration/validation-pipeline.mmd` | Config validation |
| **Runtime Config** | `configuration/runtime-config.mmd` | Dynamic configuration |

### Security Diagrams

| Diagram | Path | Purpose |
|---------|------|---------|
| **Authentication Flow** | `security/authentication-flow.mmd` | Auth process |
| **Secrets Management** | `security/secrets-management.mmd` | Secret storage |
| **Transport Security** | `security/transport-security.mmd` | TLS implementation |
| **Data Protection** | `security/data-protection.mmd` | Data encryption |
| **Audit Logging** | `security/audit-logging.mmd` | Audit trail |

### API Diagrams

| Diagram | Path | Purpose |
|---------|------|---------|
| **API Endpoints** | `api/api-endpoints.mmd` | HTTP API structure |
| **Request-Response Flow** | `api/request-response-flow.mmd` | API message flow |
| **Authentication Flow** | `api/authentication-flow.mmd` | API authentication |
| **Rate Limiting** | `api/rate-limiting.mmd` | API rate limiting |
| **Versioning Strategy** | `api/versioning-strategy.mmd` | API versioning |

### Development Diagrams

| Diagram | Path | Purpose |
|---------|------|---------|
| **TDD Workflow** | `development/tdd-workflow.mmd` | Test-driven development |
| **CI/CD Pipeline** | `development/ci-cd-pipeline.mmd` | Build automation |
| **Code Review Process** | `development/code-review-process.mmd` | Review workflow |
| **Deployment Flow** | `development/deployment-flow.mmd` | Deployment process |
| **Debugging Workflow** | `development/debugging-workflow.mmd` | Debug process |

### Monitoring Diagrams

| Diagram | Path | Purpose |
|---------|------|---------|
| **Metrics Collection** | `monitoring/metrics-collection.mmd` | Metrics pipeline |
| **Dashboard Structure** | `monitoring/dashboard-structure.mmd` | Dashboard layout |
| **Alerting Workflow** | `monitoring/alerting-workflow.mmd` | Alert routing |
| **Performance Monitoring** | `monitoring/performance-monitoring.mmd` | Performance tracking |
| **Log Aggregation** | `monitoring/log-aggregation.mmd` | Log collection |

### Integration Diagrams

| Diagram | Path | Purpose |
|---------|------|---------|
| **External Services** | `integration/external-services.mmd` | Service integration |
| **API Gateway** | `integration/api-gateway.mmd` | Gateway patterns |
| **Message Bus** | `integration/message-bus.mmd` | Messaging integration |
| **Database Integration** | `integration/database-integration.mmd` | Database connections |
| **Monitoring Integration** | `integration/monitoring-integration.mmd` | Monitoring stack |

### Roadmap Diagrams

| Diagram | Path | Purpose |
|---------|------|---------|
| **Development Roadmap** | `roadmap/development-roadmap.mmd` | Feature roadmap |
| **Feature Timeline** | `roadmap/feature-timeline.mmd` | Release schedule |
| **Technology Stack** | `roadmap/technology-stack.mmd` | Technology choices |
| **Community Contribution** | `roadmap/community-contribution.mmd` | Contribution guide |
| **Maintenance Plan** | `roadmap/maintenance-plan.mmd` | Maintenance strategy |

### Reference Diagrams

| Diagram | Path | Purpose |
|---------|------|---------|
| **Module Index** | `reference/module-index.mmd` | Complete module catalog |
| **Function Signatures** | `reference/function-signatures.mmd` | API signatures |
| **Error Codes** | `reference/error-codes.mmd` | Error code reference |
| **Configuration Reference** | `reference/configuration-reference.mmd` | Config options |
| **Troubleshooting Guide** | `reference/troubleshooting-guide.mmd` | Issue resolution |

### Example Diagrams

| Diagram | Path | Purpose |
|---------|------|---------|
| **Basic Request** | `examples/basic-request.mmd` | Simple request flow |
| **Complex Workflow** | `examples/complex-workflow.mmd` | Multi-step example |
| **Error Scenario** | `examples/error-scenario.mmd` | Error handling example |
| **Performance Benchmark** | `examples/performance-benchmark.mmd` | Benchmark example |
| **Deployment Example** | `examples/deployment-example.mmd` | Deployment scenario |

**Total Diagrams**: 85+ Mermaid diagrams covering all aspects of the system

**Viewing Diagrams:**
- GitHub/Markdown: Native Mermaid rendering supported
- CLI: `mermaid-cli` package (`npm install -g @mermaid-js/mermaid-cli`)
- Web: [Mermaid Live Editor](https://mermaid.live)

## System Overview (Visual)

```mermaid
graph TB
    subgraph "erlmcp System Architecture v2.1.0"
        direction TB

        subgraph "Client Layer"
            AI_CLIENT[AI Runtime<br/>Claude, GPT-4, Local LLMs]
            CLIENT_TOOLS[Client Tools<br/>VSCode, CLI, Browser]
        end

        subgraph "Transport Layer"
            direction LR
            STDIO[STDIO Transport<br/>Process I/O]
            TCP[TCP Transport<br/>Ranch Acceptors]
            HTTP[HTTP Transport<br/>Gun Client/Cowboy Server]
            WS[WebSocket Transport<br/>Full-Duplex]
            SSE[SSE Transport<br/>Server-Sent Events]
        end

        subgraph "Core Layer (97 modules)"
            direction TB

            subgraph "Protocol Processing"
                JSON_RPC[erlmcp_json_rpc<br/>JSON-RPC 2.0 Codec]
                MSG_HANDLER[erlmcp_message_handler<br/>Router & Dispatcher]
                MSG_PARSER[erlmcp_message_parser<br/>Parse & Validate]
            end

            subgraph "Session Management"
                SESSION_MGR[erlmcp_session_manager<br/>Lifecycle Coordinator]
                SESSION[erlmcp_session<br/>State Management]
                SESSION_BACKENDS[Session Backends<br/>ETS | DETS | Mnesia]
                SESSION_FAILOVER[erlmcp_session_failover<br/>Failover Orchestrator]
            end

            subgraph "MCP Capabilities"
                RESOURCES[erlmcp_resources<br/>Resource Management]
                TOOLS[erlmcp_tool<br/>Tool Invocation]
                PROMPTS[erlmcp_prompt_template<br/>Prompt Management]
                SUBSCRIPTIONS[erlmcp_resource_subscriptions<br/>Change Notifications]
            end

            subgraph "Security"
                AUTH[erlmcp_auth<br/>Authentication/Authorization]
                AUTH_MTLS[erlmcp_auth_mtls<br/>Mutual TLS]
                AUTH_RATE[erlmcp_auth_rate_limiter<br/>Auth-Specific Limits]
                SECRETS[erlmcp_secrets<br/>Vault/AWS/Local Encrypted]
            end

            subgraph "Resilience"
                CIRCUIT[erlmcp_circuit_breaker<br/>Circuit Breaker Pattern]
                RATE_LIMIT[erlmcp_rate_limiter<br/>Rate Limiting]
                CONN_MONITOR[erlmcp_connection_monitor<br/>Health Monitoring]
                CONN_LIMITER[erlmcp_connection_limiter<br/>Connection Limits]
            end

            subgraph "Server & Client"
                SERVER[erlmcp_server<br/>MCP Server Implementation]
                SERVER_SUP[erlmcp_server_sup<br/>Server Supervisor]
                CLIENT[erlmcp_client<br/>MCP Client Implementation]
                CLIENT_SUP[erlmcp_client_sup<br/>Client Supervisor]
            end

            subgraph "Registry"
                REGISTRY[erlmcp_registry<br/>gproc-based Routing]
                REGISTRY_DIST[erlmcp_registry_dist<br/>Distributed Coordination]
            end

            subgraph "LLM Integration"
                LLVM_ANTHROPIC[erlmcp_llm_provider_anthropic<br/>Claude Integration]
                LLVM_OPENAI[erlmcp_llm_provider_openai<br/>GPT Integration]
                LLVM_LOCAL[erlmcp_llm_provider_local<br/>Local Model Support]
            end
        end

        subgraph "Observability Layer (31 modules)"
            direction TB

            subgraph "OpenTelemetry"
                OTEL[erlmcp_otel<br/>OTel Integration]
                OTEL_DATADOG[erlmcp_otel_datadog<br/>Datadog Exporter]
                OTEL_HONEYCOMB[erlmcp_otel_honeycomb<br/>Honeycomb Exporter]
                OTEL_JAEGER[erlmcp_otel_jaeger<br/>Jaeger Exporter]
            end

            subgraph "Metrics & Tracing"
                METRICS[erlmcp_metrics<br/>Metrics Collection]
                METRICS_SERVER[erlmcp_metrics_server<br/>HTTP Server]
                METRICS_AGG[erlmcp_metrics_aggregator<br/>Aggregation]
                TRACING[erlmcp_tracing<br/>Distributed Tracing]
                TRACE_ANALYZER[erlmcp_trace_analyzer<br/>Trace Analysis]
            end

            subgraph "Chaos Engineering"
                CHAOS[erlmcp_chaos<br/>Coordinator]
                CHAOS_NETWORK[erlmcp_chaos_network<br/>Network Failures]
                CHAOS_PROCESS[erlmcp_chaos_process<br/>Process Failures]
                CHAOS_RESOURCE[erlmcp_chaos_resource<br/>Resource Exhaustion]
                RECOVERY[erlmcp_recovery_manager<br/>Recovery Orchestration]
            end

            subgraph "Monitoring"
                HEALTH[erlmcp_health_monitor<br/>Health Monitoring]
                DASHBOARD[erlmcp_dashboard_server<br/>Web Interface]
                PROCESS_MONITOR[erlmcp_process_monitor<br/>Process Monitoring]
                AUDIT_LOG[erlmcp_audit_log<br/>Audit Logging]
            end
        end

        subgraph "Validation Layer (13 modules)"
            direction TB

            PROTO_VALIDATOR[erlmcp_protocol_validator<br/>JSON-RPC + MCP Compliance]
            TRANSPORT_VALIDATOR[erlmcp_transport_validator<br/>Transport Behavior Compliance]
            SECURITY_VALIDATOR[erlmcp_security_validator<br/>Auth + Secrets + Input Validation]
            PERF_VALIDATOR[erlmcp_performance_validator<br/>Latency + Throughput + Memory]

            COMPLIANCE_REPORT[erlmcp_compliance_report<br/>Compliance Reporting]
            COMPLIANCE_HTML[erlmcp_compliance_report_html<br/>HTML Generation]
            COMPLIANCE_JSON[erlmcp_compliance_report_json<br/>JSON Generation]

            SPEC_PARSER[erlmcp_spec_parser<br/>MCP Spec Parser 2025-11-25]
            TEST_CLIENT[erlmcp_test_client<br/>Multi-Transport Test Client]
            VALIDATE_CLI[erlmcp_validate_cli<br/>Command-Line Interface]
        end

        subgraph "External Dependencies"
            GPROC[gproc<br/>Process Registry]
            JSX[jsx<br/>JSON Codec]
            JESSE[jesse<br/>JSON Schema Validation]
            GUN[gun<br/>HTTP Client]
            RANCH[ranch<br/>TCP Acceptors]
            COWBOY[cowboy<br/>HTTP Server]
            POOLBOY[poolboy<br/>Pool Management]
            JOSE[jose<br/>JWT Validation]
        end

        %% Connections
        AI_CLIENT -->|stdio, tcp, http, ws, sse| Transport_Layer
        CLIENT_TOOLS -->|stdio, tcp, http, ws, sse| Transport_Layer

        Transport_Layer --> MSG_HANDLER
        MSG_HANDLER --> MSG_PARSER
        MSG_PARSER --> JSON_RPC
        JSON_RPC --> REGISTRY

        REGISTRY --> SERVER
        REGISTRY --> CLIENT

        SERVER --> RESOURCES
        SERVER --> TOOLS
        SERVER --> PROMPTS
        SERVER --> SUBSCRIPTIONS

        CLIENT --> SESSION
        SESSION --> SESSION_BACKENDS
        SESSION --> SESSION_FAILOVER
        SESSION_MGR --> SESSION

        SERVER --> AUTH
        AUTH --> AUTH_MTLS
        AUTH --> AUTH_RATE
        AUTH --> SECRETS

        SERVER --> CIRCUIT
        SERVER --> RATE_LIMIT
        SERVER --> CONN_MONITOR
        SERVER --> CONN_LIMITER

        SERVER --> LLVM_ANTHROPIC
        SERVER --> LLVM_OPENAI
        SERVER --> LLVM_LOCAL

        %% Observability Connections
        OTEL --> OTEL_DATADOG
        OTEL --> OTEL_HONEYCOMB
        OTEL --> OTEL_JAEGER

        METRICS --> METRICS_SERVER
        METRICS --> METRICS_AGG

        CHAOS --> CHAOS_NETWORK
        CHAOS --> CHAOS_PROCESS
        CHAOS --> CHAOS_RESOURCE
        CHAOS --> RECOVERY

        HEALTH --> DASHBOARD
        HEALTH --> PROCESS_MONITOR

        %% Validation Connections
        PROTO_VALIDATOR --> COMPLIANCE_REPORT
        TRANSPORT_VALIDATOR --> COMPLIANCE_REPORT
        SECURITY_VALIDATOR --> COMPLIANCE_REPORT
        PERF_VALIDATOR --> COMPLIANCE_REPORT

        COMPLIANCE_REPORT --> COMPLIANCE_HTML
        COMPLIANCE_REPORT --> COMPLIANCE_JSON

        SPEC_PARSER --> PROTO_VALIDATOR
        TEST_CLIENT --> TRANSPORT_VALIDATOR
        VALIDATE_CLI --> COMPLIANCE_REPORT

        %% Dependencies
        REGISTRY -.-> GPROC
        JSON_RPC -.-> JSX
        MSG_PARSER -.-> JESSE
        HTTP -.-> GUN
        TCP -.-> RANCH
        HTTP -.-> COWBOY
        CIRCUIT -.-> POOLBOY
        AUTH -.-> JOSE
    end

    classDef clientStyle fill:#e1f5fe,stroke:#01579b,stroke-width:2px
    classDef transportStyle fill:#f3e5f5,stroke:#4a148c,stroke-width:2px
    classDef coreStyle fill:#e8f5e9,stroke:#1b5e20,stroke-width:2px
    classDef observabilityStyle fill:#fff3e0,stroke:#e65100,stroke-width:2px
    classDef validationStyle fill:#fce4ec,stroke:#880e4f,stroke-width:2px
    classDef depStyle fill:#f5f5f5,stroke:#424242,stroke-width:1px,stroke-dasharray: 5 5

    class AI_CLIENT,CLIENT_TOOLS clientStyle
    class STDIO,TCP,HTTP,WS,SSE transportStyle
    class JSON_RPC,MSG_HANDLER,MSG_PARSER,SESSION_MGR,SESSION,SESSION_BACKENDS,SESSION_FAILOVER,RESOURCES,TOOLS,PROMPTS,SUBSCRIPTIONS,AUTH,AUTH_MTLS,AUTH_RATE,SECRETS,CIRCUIT,RATE_LIMIT,CONN_MONITOR,CONN_LIMITER,SERVER,SERVER_SUP,CLIENT,CLIENT_SUP,REGISTRY,REGISTRY_DIST,LLVM_ANTHROPIC,LLVM_OPENAI,LLVM_LOCAL coreStyle
    class OTEL,OTEL_DATADOG,OTEL_HONEYCOMB,OTEL_JAEGER,METRICS,METRICS_SERVER,METRICS_AGG,TRACING,TRACE_ANALYZER,CHAOS,CHAOS_NETWORK,CHAOS_PROCESS,CHAOS_RESOURCE,RECOVERY,HEALTH,DASHBOARD,PROCESS_MONITOR,AUDIT_LOG observabilityStyle
    class PROTO_VALIDATOR,TRANSPORT_VALIDATOR,SECURITY_VALIDATOR,PERF_VALIDATOR,COMPLIANCE_REPORT,COMPLIANCE_HTML,COMPLIANCE_JSON,SPEC_PARSER,TEST_CLIENT,VALIDATE_CLI validationStyle
    class GPROC,JSX,JESSE,GUN,RANCH,COWBOY,POOLBOY,JOSE depStyle
```

**See also:** [`system-architecture.mmd`](diagrams/system-architecture.mmd) for the standalone diagram file.

## v2.0.0 Umbrella Structure

```
erlmcp/ (umbrella root)
├── rebar.config                  # Umbrella configuration
├── apps/
│   ├── erlmcp_core/              [14 modules] REQUIRED
│   │   ├── src/erlmcp_app.erl           # Application behavior
│   │   ├── src/erlmcp_sup.erl           # Top-level supervisor (one_for_one)
│   │   ├── src/erlmcp_core_sup.erl      # Core infrastructure supervisor
│   │   ├── src/erlmcp_client.erl        # MCP client gen_server
│   │   ├── src/erlmcp_server.erl        # MCP server gen_server
│   │   ├── src/erlmcp_registry.erl      # gproc-based routing
│   │   ├── src/erlmcp_json_rpc.erl      # JSON-RPC 2.0
│   │   └── ... [7 more modules]
│   │
│   ├── erlmcp_transports/        [8 modules]  REQUIRED
│   │   ├── src/erlmcp_transports_app.erl    # Application behavior
│   │   ├── src/erlmcp_transport_sup.erl     # Transport supervisor
│   │   ├── src/erlmcp_transport_stdio.erl   # STDIO transport
│   │   ├── src/erlmcp_transport_tcp.erl     # TCP transport (ranch)
│   │   ├── src/erlmcp_transport_http.erl    # HTTP/2 transport (gun)
│   │   ├── src/erlmcp_transport_ws.erl      # WebSocket transport
│   │   └── ... [2 more modules]
│   │
│   ├── erlmcp_observability/     [9 modules]  REQUIRED
│   │   ├── src/erlmcp_observability_app.erl # Application behavior
│   │   ├── src/erlmcp_observability_sup.erl # Observability supervisor
│   │   ├── src/erlmcp_metrics.erl           # Metrics collection
│   │   ├── src/erlmcp_otel.erl              # OpenTelemetry integration
│   │   ├── src/erlmcp_receipt_chain.erl     # SHA-256 receipts
│   │   └── ... [4 more modules]
│   │
│   └── tcps_erlmcp/              [63 modules] OPTIONAL
│       ├── src/tcps_erlmcp_app.erl          # Application behavior
│       ├── src/tcps_erlmcp_sup.erl          # TCPS supervisor
│       ├── src/tcps_quality_gates.erl       # Quality enforcement
│       ├── src/tcps_shacl_validator.erl     # SHACL ontology validation
│       ├── src/tcps_kanban.erl              # WIP limits
│       └── ... [58 more modules]
│
└── Total: 94 modules across 4 apps
```

## Application Boundaries and Responsibilities

### 1. erlmcp_core - Core Protocol Engine

**Purpose:** MCP protocol primitives, JSON-RPC, client/server, registry, and routing

**Responsibilities:**
- JSON-RPC 2.0 message encoding/decoding
- MCP capability negotiation and protocol compliance
- Client gen_server with request correlation and response handling
- Server gen_server with resource/tool/prompt management
- gproc-based process registration and discovery
- Message routing between clients, servers, and transports
- Session management, task tracking, resource subscriptions
- Hot code reload infrastructure

**External Dependencies:** jsx, jesse, gproc

**Internal Dependencies:** None (foundation layer)

**Supervision Tree:**
```
erlmcp_sup (one_for_one)
├── erlmcp_core_sup (one_for_one)
│   ├── erlmcp_cluster_sup
│   ├── erlmcp_registry
│   ├── erlmcp_reload_sup
│   ├── erlmcp_session_manager
│   ├── erlmcp_task_manager
│   ├── erlmcp_resource_subscriptions
│   ├── erlmcp_sse_event_store
│   ├── erlmcp_icon_cache
│   ├── erlmcp_cache (multi-level: ETS, Mnesia, external)
│   ├── erlmcp_session_replicator
│   └── erlmcp_session_failover
├── erlmcp_server_sup (simple_one_for_one)
│   └── [erlmcp_server instances]
└── erlmcp_observability_sup (one_for_one)
    └── [See erlmcp_observability below]
```

**Key Modules:**
- `erlmcp_app.erl` - Application behavior callback
- `erlmcp_sup.erl` - Top-level supervisor (one_for_one strategy)
- `erlmcp_core_sup.erl` - Core infrastructure supervisor
- `erlmcp_client.erl` - MCP client gen_server
- `erlmcp_server.erl` - MCP server gen_server
- `erlmcp_server_sup.erl` - Dynamic server supervisor (simple_one_for_one)
- `erlmcp_registry.erl` - gproc-based process registry
- `erlmcp_json_rpc.erl` - JSON-RPC 2.0 codec
- `erlmcp_capabilities.erl` - Capability negotiation
- `erlmcp_session_manager.erl` - Session lifecycle management
- `erlmcp_task_manager.erl` - Task execution tracking
- `erlmcp_resource_subscriptions.erl` - Resource change notifications
- `erlmcp_cache.erl` - Multi-level intelligent caching
- `erlmcp_cluster_sup.erl` - Distributed cluster management

**Configuration (erlmcp_core.app.src):**
```erlang
{env, [
    {client_defaults, #{
        timeout => 5000,
        strict_mode => false,
        max_pending_requests => 100
    }},
    {server_defaults, #{
        max_subscriptions_per_resource => 1000,
        max_progress_tokens => 10000
    }},
    {registry_defaults, #{
        sharding_strategy => none,
        health_check_interval => 30000
    }},
    {cluster_enabled, false}
]}
```

---

### 2. erlmcp_transports - Network Transport Layer

**Purpose:** Pluggable transport implementations (STDIO, TCP, HTTP/2, WebSocket, SSE, GraphQL)

**Responsibilities:**
- Transport behavior definition and lifecycle management
- STDIO transport (pipes, standard I/O)
- TCP transport with ranch acceptor pools and connection management
- HTTP/2 transport with gun client (multiplexing, keepalive)
- WebSocket transport for bidirectional streaming
- Server-Sent Events (SSE) for unidirectional streaming
- GraphQL transport with query/mutation/subscription support
- Transport-specific error handling and recovery
- Connection pooling via poolboy
- Automatic reconnection with exponential backoff

**External Dependencies:** gun, ranch, poolboy

**Internal Dependencies:** erlmcp_core (registry, routing)

**Supervision Tree:**
```
erlmcp_transports_app
└── erlmcp_transport_sup (one_for_one)
    └── [Dynamic transport instances via start_child/3]
        ├── erlmcp_transport_stdio
        ├── erlmcp_transport_tcp (ranch listener)
        ├── erlmcp_transport_http (gun connection)
        ├── erlmcp_transport_ws (gun WebSocket)
        ├── erlmcp_transport_sse (Cowboy SSE)
        └── erlmcp_transport_graphql (Cowboy GraphQL)
```

**Key Modules:**
- `erlmcp_transports_app.erl` - Application behavior callback
- `erlmcp_transport_sup.erl` - Transport supervisor (one_for_one strategy)
- `erlmcp_transport.erl` - Transport behavior definition
- `erlmcp_transport_stdio.erl` - Standard I/O transport
- `erlmcp_transport_tcp.erl` - TCP transport (ranch)
- `erlmcp_transport_http.erl` - HTTP/2 transport (gun)
- `erlmcp_transport_ws.erl` - WebSocket transport (gun)
- `erlmcp_transport_sse.erl` - Server-Sent Events transport
- `erlmcp_transport_graphql.erl` - GraphQL transport

**Transport Behavior Interface:**
```erlang
-callback init(TransportId :: atom(), Config :: map()) ->
    {ok, State} | {error, Reason}.
-callback send(State, Data :: iodata()) ->
    ok | {error, Reason}.
-callback close(State) -> ok.

-optional_callbacks([get_info/1, handle_transport_call/2]).
```

**Configuration (erlmcp_transports.app.src):**
```erlang
{env, [
    {transport_defaults, #{
        tcp => #{
            connect_timeout => 5000,
            keepalive => true,
            nodelay => true,
            port => 3000
        },
        http => #{
            connect_timeout => 5000,
            request_timeout => 30000,
            max_connections => 100,
            port => 3001
        },
        stdio => #{
            buffer_size => 65536,
            read_timeout => infinity
        }
    }}
]}
```

**Restart Strategies:**
- **stdio:** temporary (single-use, don't restart)
- **tcp/http/ws/sse:** transient (restart on abnormal exit)
- **Shutdown timeouts:** stdio (2s), network transports (5s)

---

### 3. erlmcp_observability - Metrics, Traces, Receipts

**Purpose:** Comprehensive observability with metrics, OpenTelemetry tracing, and deterministic receipts

**Responsibilities:**
- Performance metrics collection and aggregation
- OpenTelemetry trace context propagation
- Span creation and correlation for MCP requests
- SHA-256 deterministic receipt chains for audit trails
- Receipt persistence and validation
- Health monitoring and component status tracking
- Recovery management with circuit breakers
- Chaos engineering framework for resilience testing
- Real-time metrics dashboard (Cowboy + WebSocket)
- HTTP metrics endpoint (/metrics)
- Time-series aggregation and percentile calculations

**External Dependencies:** opentelemetry_api, opentelemetry, opentelemetry_exporter

**Internal Dependencies:** erlmcp_core (registry, routing)

**Supervision Tree:**
```
erlmcp_observability_app
└── erlmcp_observability_sup (one_for_one)
    ├── erlmcp_metrics (metrics collection)
    ├── erlmcp_metrics_server (HTTP /metrics endpoint)
    ├── erlmcp_metrics_aggregator (time-series, percentiles)
    ├── erlmcp_dashboard_server (real-time dashboard)
    ├── erlmcp_health_monitor (component health tracking)
    ├── erlmcp_recovery_manager (circuit breakers, auto-recovery)
    └── erlmcp_chaos (resilience testing)
```

**Key Modules:**
- `erlmcp_observability_app.erl` - Application behavior callback
- `erlmcp_observability_sup.erl` - Observability supervisor (one_for_one, isolated)
- `erlmcp_metrics.erl` - Metrics collection (throughput, latency, errors)
- `erlmcp_metrics_server.erl` - HTTP metrics endpoint server
- `erlmcp_metrics_aggregator.erl` - Time-series aggregation, percentiles
- `erlmcp_dashboard_server.erl` - Real-time WebSocket dashboard
- `erlmcp_otel.erl` - OpenTelemetry integration (traces, spans)
- `erlmcp_receipt_chain.erl` - SHA-256 hash chain for audit trails
- `erlmcp_health_monitor.erl` - Component health tracking
- `erlmcp_recovery_manager.erl` - Automatic recovery, circuit breakers
- `erlmcp_chaos.erl` - Chaos engineering framework

**Configuration (erlmcp_observability.app.src):**
```erlang
{env, [
    {otel_defaults, #{
        service_name => <<"erlmcp">>,
        exporter => {otlp, #{
            endpoint => "http://localhost:4318",
            protocol => http_protobuf
        }},
        sampling_rate => 1.0
    }},
    {metrics_defaults, #{
        interval => 60000,
        backend => simple
    }},
    {receipt_defaults, #{
        hash_algorithm => sha256,
        storage_backend => file,
        verification_on_read => false
    }}
]}
```

**Failure Isolation:**
Observability failures do NOT affect core MCP protocol operation. This is achieved via:
- Independent supervision tree (no dependencies on protocol layer)
- `one_for_one` strategy - isolated worker restarts
- ETS-based receipt storage (survives process restarts)

---

### 4. tcps_erlmcp - Toyota Code Production System (Optional)

**Purpose:** Zero-defect software delivery via quality gates, SHACL validation, and manufacturing principles

**Responsibilities:**
- 8 sequential quality gates (SHACL, compile, test, security, build, metrics, release, smoke)
- SHACL ontology-based validation for RDF/Turtle
- Work order management (JIT pull, Heijunka leveling, Kanban WIP limits)
- Quality enforcement with stop-the-line authority (Jidoka)
- Receipt chain with SHA-256 hash chains for immutable audit trails
- SKU release management with evidence bundles
- Real-time dashboard with SSE broadcasting
- Metrics aggregation and visualization
- Andon event system for quality violations
- Kaizen continuous improvement tracking

**External Dependencies:** bbmustache, cowboy, jobs, fs

**Internal Dependencies:** erlmcp_core, erlmcp_observability

**Supervision Tree:**
```
tcps_erlmcp_app
└── tcps_erlmcp_sup (one_for_one)
    ├── tcps_work_order (work order management)
    ├── tcps_kanban (WIP limits, visual management)
    ├── tcps_quality_gates (8 quality gates)
    ├── tcps_sku (SKU release management)
    ├── tcps_dashboard (web dashboard - Cowboy)
    ├── tcps_sse_manager (SSE broadcasting)
    └── tcps_metrics_aggregator (metrics collection)
```

**Key Modules (63 total):**
- `tcps_erlmcp_app.erl` - Application behavior callback
- `tcps_erlmcp_sup.erl` - TCPS supervisor (one_for_one strategy)
- `tcps_quality_gates.erl` - 8-gate quality enforcement
- `tcps_shacl_validator.erl` - SHACL ontology validation
- `tcps_work_order.erl` - Work order lifecycle management
- `tcps_kanban.erl` - WIP limits and flow control
- `tcps_heijunka.erl` - Production leveling
- `tcps_receipt_chain.erl` - SHA-256 hash chain receipts
- `tcps_sku.erl` - SKU release management
- `tcps_dashboard.erl` - Web dashboard server
- `tcps_sse_manager.erl` - Server-Sent Events broadcasting
- `tcps_metrics_aggregator.erl` - Metrics collection
- `+ 51 additional modules` - Complete manufacturing system

**Configuration (tcps_erlmcp.app.src):**
```erlang
{env, [
    {quality_gates, #{
        min_test_pass_rate => 0.80,
        min_coverage => 0.80,
        max_cyclomatic_complexity => 15,
        max_function_length => 50
    }},
    {shacl_defaults, #{
        shapes_dir => "shapes",
        ontology_dir => "ontology",
        strict_mode => true
    }},
    {tcps_auto_integration, true},
    {tcps_quality_gates_enabled, [1,2,3,4,5,6,7,8]},
    {tcps_andon_on_sla_violation, true}
]}
```

**8 Quality Gates:**
1. **SHACL Validation** - Ontology conformance (100% required)
2. **Compilation** - Zero-error builds (0 errors tolerated)
3. **Test Execution** - 95% pass rate, 80% coverage minimum
4. **Security Scan** - Zero critical vulnerabilities
5. **Deterministic Build** - Reproducibility verification
6. **Quality Metrics** - Production thresholds enforcement
7. **Release Verification** - SBOM, licenses, dependencies
8. **Smoke Test** - Basic functionality validation

---

## Dependency Graph

```
┌─────────────────────┐
│  tcps_erlmcp        │  [OPTIONAL - Can be excluded]
│  (63 modules)       │
└──────┬─────┬────────┘
       │     │
       ▼     ▼
┌──────────────────┐   ┌──────────────────────┐
│ erlmcp_observ... │   │  erlmcp_core         │  [FOUNDATION]
│ (9 modules)      │◄──┤  (14 modules)        │
└──────────────────┘   └──────┬───────────────┘
       ▲                       │
       │                       │
       │                       ▼
       │               ┌──────────────────┐
       └───────────────│ erlmcp_transports│
                       │ (8 modules)      │
                       └──────────────────┘

External Libraries:
  erlmcp_core       → jsx, jesse, gproc
  erlmcp_transports → gun, ranch, poolboy
  erlmcp_observ...  → opentelemetry_api, opentelemetry, opentelemetry_exporter
  tcps_erlmcp       → bbmustache, cowboy, jobs, fs
```

**Dependency Rules:**
- ✅ `erlmcp_core` has NO internal dependencies (foundation layer)
- ✅ `erlmcp_transports` depends ONLY on `erlmcp_core`
- ✅ `erlmcp_observability` depends ONLY on `erlmcp_core`
- ✅ `tcps_erlmcp` depends on `erlmcp_core` + `erlmcp_observability`
- ✅ All apps specify external deps in their `.app.src` files

## v2.0.0 Supervision Tree Architecture

### Complete Supervision Hierarchy

```
erlmcp (umbrella)
│
├── erlmcp_core_app
│   └── erlmcp_sup (one_for_one) ←─────────────┐
│       ├── erlmcp_core_sup (one_for_one)      │ TIER 1: CORE
│       │   ├── erlmcp_cluster_sup             │ Foundation layer
│       │   ├── erlmcp_registry (gproc)        │ No external deps
│       │   ├── erlmcp_reload_sup              │
│       │   ├── erlmcp_session_manager         │
│       │   ├── erlmcp_task_manager            │
│       │   ├── erlmcp_resource_subscriptions  │
│       │   ├── erlmcp_sse_event_store         │
│       │   ├── erlmcp_icon_cache              │
│       │   ├── erlmcp_cache (L1/L2/L3)        │
│       │   ├── erlmcp_session_replicator      │
│       │   └── erlmcp_session_failover        │
│       │                                      │
│       ├── erlmcp_server_sup (simple_one_for_one) ←─┐ TIER 2: PROTOCOL
│       │   └── [Dynamic erlmcp_server instances]    │ MCP servers
│       │                                            │
│       └── erlmcp_observability_sup (one_for_one) ←─┘ TIER 3: OBSERVABILITY
│           ├── erlmcp_metrics                        Isolated from core
│           ├── erlmcp_metrics_server
│           ├── erlmcp_metrics_aggregator
│           ├── erlmcp_dashboard_server
│           ├── erlmcp_health_monitor
│           ├── erlmcp_recovery_manager
│           └── erlmcp_chaos
│
├── erlmcp_transports_app
│   └── erlmcp_transport_sup (one_for_one) ←────────┐ TRANSPORT LAYER
│       └── [Dynamic transport instances]            │ Network I/O
│           ├── erlmcp_transport_stdio (temporary)   │ Pluggable
│           ├── erlmcp_transport_tcp (transient)     │
│           ├── erlmcp_transport_http (transient)    │
│           ├── erlmcp_transport_ws (transient)      │
│           ├── erlmcp_transport_sse (transient)     │
│           └── erlmcp_transport_graphql (transient) │
│
├── erlmcp_observability_app
│   └── [Supervisor tree included in erlmcp_core]
│
└── tcps_erlmcp_app [OPTIONAL]
    └── tcps_erlmcp_sup (one_for_one) ←────────────┐ QUALITY SYSTEM
        ├── tcps_work_order                         │ Manufacturing
        ├── tcps_kanban                             │ Jidoka
        ├── tcps_quality_gates                      │ Optional
        ├── tcps_sku                                │
        ├── tcps_dashboard                          │
        ├── tcps_sse_manager                        │
        └── tcps_metrics_aggregator                 │
```

**Supervision Strategies:**

| Supervisor | Strategy | Rationale |
|------------|----------|-----------|
| `erlmcp_sup` | `one_for_one` | Each subsystem fails independently - no cascading failures |
| `erlmcp_core_sup` | `one_for_one` | Core components isolated - registry failure doesn't restart sessions |
| `erlmcp_server_sup` | `simple_one_for_one` | Dynamic MCP server instances - individual failures don't affect others |
| `erlmcp_transport_sup` | `one_for_one` | Transport failures isolated - TCP crash doesn't affect HTTP |
| `erlmcp_observability_sup` | `one_for_one` | Observability failures NEVER affect protocol operation |
| `tcps_erlmcp_sup` | `one_for_one` | Quality gate failures isolated - independent workers |

**Failure Isolation Guarantees:**

| Component Crash | Restart Scope | Recovery Time | Impact |
|-----------------|---------------|---------------|--------|
| Registry | Registry only | ~500ms | New routing fails; existing connections continue |
| Session Manager | Session Manager only | ~1s | New sessions fail; existing sessions continue |
| Transport (TCP) | That transport only | ~2s | TCP connections drop; HTTP/STDIO unaffected |
| Metrics Server | Metrics Server only | ~500ms | Metrics temporarily unavailable; protocol unaffected |
| Observability | Observability workers | ~500ms | Zero impact on core MCP protocol |
| Quality Gate | That gate only | ~1s | Other gates continue; SKU validation may fail |

**Key Design Principles:**
1. **Bulkhead Pattern** - Failures contained within application boundaries
2. **No Cascading Restarts** - `one_for_one` prevents domino effects
3. **Observability Isolation** - Monitoring failures don't affect protocol
4. **Transport Independence** - Each transport type fails independently
5. **Dynamic Workers** - Servers and transports added/removed at runtime

---

## System Components (v2.0.0)

```
┌──────────────────────────────────────────────────────────────┐
│                     erlmcp Umbrella                          │
├──────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌─────────────────┐         ┌─────────────────┐            │
│  │  MCP Client     │         │  MCP Server     │            │
│  │  (erlmcp_core)  │ ◄────► │  (erlmcp_core)  │            │
│  │  gen_server     │         │  gen_server     │            │
│  └────────┬────────┘         └────────┬────────┘            │
│           │                           │                     │
│           ▼                           ▼                     │
│  ┌──────────────────────────────────────────────┐           │
│  │   erlmcp_registry (gproc-based routing)      │           │
│  │   (erlmcp_core)                              │           │
│  └──────────────────────────────────────────────┘           │
│           │                           │                     │
│           ▼                           ▼                     │
│  ┌─────────────────────────────────────────────────┐        │
│  │     Transport Layer (erlmcp_transports)         │        │
│  ├─────────────────────────────────────────────────┤        │
│  │ STDIO │ TCP (ranch) │ HTTP/2 (gun) │ WebSocket │        │
│  └─────────────────────────────────────────────────┘        │
│           │                           │                     │
│           ▼                           ▼                     │
│  ┌─────────────────────────────────────────────────┐        │
│  │   Observability (erlmcp_observability)          │        │
│  ├─────────────────────────────────────────────────┤        │
│  │ OTEL Traces │ Metrics │ Receipt Chains          │        │
│  └─────────────────────────────────────────────────┘        │
│           │                           │                     │
│           ▼                           ▼                     │
│  ┌─────────────────────────────────────────────────┐        │
│  │   TCPS Quality System (tcps_erlmcp) [OPTIONAL]  │        │
│  ├─────────────────────────────────────────────────┤        │
│  │ SHACL │ Quality Gates │ Jidoka │ Kanban         │        │
│  └─────────────────────────────────────────────────┘        │
│                                                              │
└──────────────────────────────────────────────────────────────┘
```

## App Modules by Layer

### erlmcp_core (14 modules)

**Protocol Layer:**
- **erlmcp_json_rpc.erl** - JSON-RPC 2.0 message encoding/decoding
- **erlmcp.hrl** - Protocol type definitions and records
- **erlmcp_capabilities.erl** - Capability negotiation
- **erlmcp_types.erl** - MCP type definitions

**Client Components:**
- **erlmcp_client.erl** - Main client gen_server with request correlation
- **erlmcp_client_sup.erl** - Client supervisor (simple_one_for_one)

**Server Components:**
- **erlmcp_server.erl** - Main server gen_server with resource/tool/prompt management
- **erlmcp_server_sup.erl** - Server supervisor (simple_one_for_one)

**Registry & Routing:**
- **erlmcp_registry.erl** - gproc-based process registration and discovery
- **erlmcp_router.erl** - Message routing between clients/servers/transports

**Application:**
- **erlmcp_app.erl** - OTP application behavior
- **erlmcp_sup.erl** - Top-level supervisor (rest_for_one)
- **erlmcp_schema.erl** - JSON Schema validation (jesse)
- **erlmcp_utils.erl** - Common utilities

### erlmcp_transports (8 modules)

**Transport Implementations:**
- **erlmcp_transport_stdio.erl** - Standard I/O transport (pipes)
- **erlmcp_transport_tcp.erl** - TCP socket transport (ranch acceptor pool)
- **erlmcp_transport_http.erl** - HTTP transport (gun client with HTTP/2)
- **erlmcp_transport_ws.erl** - WebSocket transport (gun)

**Infrastructure:**
- **erlmcp_transport.erl** - Transport behavior definition
- **erlmcp_transport_sup.erl** - Transport supervisor

**Application:**
- **erlmcp_transports_app.erl** - OTP application callback
- **erlmcp_transports_sup.erl** - Top-level supervisor

### erlmcp_observability (9 modules)

**Metrics:**
- **erlmcp_metrics.erl** - Metrics collection and export
- **erlmcp_routing_metrics.erl** - Registry routing metrics

**OpenTelemetry:**
- **erlmcp_otel.erl** - OpenTelemetry integration
- **erlmcp_tracer.erl** - Span creation and context propagation

**Receipt Chains:**
- **erlmcp_receipt_chain.erl** - SHA-256 deterministic hash chains
- **erlmcp_receipt_storage.erl** - Receipt persistence
- **erlmcp_receipt_validator.erl** - Chain verification

**Application:**
- **erlmcp_observability_app.erl** - OTP application callback
- **erlmcp_observability_sup.erl** - Observability supervisor

### tcps_erlmcp (63 modules)

See [apps/tcps_erlmcp/README.md](../apps/tcps_erlmcp/README.md) for complete module list. Key components:

- **SHACL Validation** (3 modules) - Ontology-based validation
- **Quality Gates** (4 modules) - Automated quality enforcement
- **Work Orders** (3 modules) - Kanban, Heijunka, WIP limits
- **Receipt Chains** (2 modules) - SHA-256 hash chains
- **Dashboard** (2 modules) - Web UI for quality visualization
- **Application** (2 modules) - OTP app and supervisor
- **+ 47 additional modules** - Complete manufacturing system

## Inter-Application Communication

### Message Flow Patterns

#### 1. Client Request Flow (Cross-App Communication)

```
Client API Call
    ↓
[erlmcp_core] erlmcp_client:call_tool(...)
    ↓
[erlmcp_core] Encode JSON-RPC request (erlmcp_json_rpc)
    ↓
[erlmcp_core] Registry lookup: gproc:lookup_local_name({mcp, transport, TransportId})
    ↓
[erlmcp_transports] erlmcp_transport:send(TransportPid, EncodedMsg)
    ↓
[erlmcp_transports] Transport-specific send (gun/ranch/stdio)
    ↓
Network I/O
    ↓
[erlmcp_transports] Transport receives response
    ↓
[erlmcp_core] Registry routes to client: erlmcp_registry:route_to_client(ClientId, Response)
    ↓
[erlmcp_core] erlmcp_client decodes JSON-RPC response
    ↓
[erlmcp_observability] Record metrics: erlmcp_metrics:record_latency(...)
    ↓
Return to caller
```

**Key Points:**
- Registry-based routing via `gproc` (automatic process monitoring)
- No direct process coupling between apps
- Transport abstraction via behavior interface
- Observability hooks via side effects (not blocking)

---

#### 2. Server Request Handling (Inbound Flow)

```
Network I/O
    ↓
[erlmcp_transports] Transport receives data
    ↓
[erlmcp_core] Registry lookup: gproc:lookup_local_name({mcp, server, ServerId})
    ↓
[erlmcp_core] erlmcp_server:handle_message(ServerPid, RawData)
    ↓
[erlmcp_core] Decode JSON-RPC request (erlmcp_json_rpc)
    ↓
[erlmcp_core] Route to handler (resource/tool/prompt)
    ↓
[erlmcp_core] Execute handler function
    ↓
[erlmcp_observability] Create OTEL span: erlmcp_otel:start_span(...)
    ↓
[erlmcp_core] Encode JSON-RPC response
    ↓
[erlmcp_transports] Transport sends response
    ↓
[erlmcp_observability] Record receipt: erlmcp_receipt_chain:append(...)
```

**Key Points:**
- Transport-agnostic server logic
- Handler functions are user-provided callbacks
- OTEL tracing via side effects (non-blocking)
- Receipt chains for audit trails (async)

---

#### 3. Registry-Based Routing (Cross-Process Communication)

**Registration Pattern (gproc-based):**

```erlang
%% Server registration (erlmcp_core)
gproc:add_local_name({mcp, server, ServerId}),
gproc:reg({p, l, {mcp_server_config, ServerId}}, Config).

%% Transport registration (erlmcp_transports)
gproc:add_local_name({mcp, transport, TransportId}),
gproc:reg({p, l, {mcp_transport_config, TransportId}}, Config).

%% Client registration (erlmcp_core)
gproc:add_local_name({mcp, client, ClientId}),
gproc:reg({p, l, {mcp_client_config, ClientId}}, Config).
```

**Lookup and Routing:**

```erlang
%% Find server by ID
case gproc:lookup_local_name({mcp, server, ServerId}) of
    undefined -> {error, not_found};
    Pid -> {ok, Pid}
end.

%% Route message to client
erlmcp_registry:route_to_client(ClientId, TransportId, Message) ->
    case gproc:lookup_local_name({mcp, client, ClientId}) of
        undefined -> {error, client_not_found};
        ClientPid -> gen_server:cast(ClientPid, {transport_data, TransportId, Message})
    end.

%% Route message to server
erlmcp_registry:route_to_server(ServerId, TransportId, Message) ->
    case gproc:lookup_local_name({mcp, server, ServerId}) of
        undefined -> {error, server_not_found};
        ServerPid -> gen_server:cast(ServerPid, {transport_data, TransportId, Message})
    end.
```

**Benefits of gproc:**
- Automatic process monitoring (no manual `monitor`/`demonitor`)
- Automatic cleanup on process death
- O(1) lookups via ETS
- Distributed registry support (if clustering enabled)
- Property storage for configuration

---

#### 4. Observability Integration (Non-Blocking Side Effects)

**Metrics Collection:**

```erlang
%% Called from erlmcp_client after request completion
erlmcp_metrics:record_request(
    #{method => <<"tools/call">>,
      latency_us => 1234,
      status => success,
      transport => http}).

%% Metrics aggregator processes asynchronously
%% No blocking of protocol operation
```

**OpenTelemetry Tracing:**

```erlang
%% Automatic span creation in erlmcp_server
handle_call({call_tool, ToolName, Args}, From, State) ->
    Span = erlmcp_otel:start_span(
        <<"mcp.tools.call">>,
        #{tool_name => ToolName,
          client_id => maps:get(client_id, State)}),

    Result = execute_tool(ToolName, Args),

    erlmcp_otel:end_span(Span),
    {reply, Result, State}.
```

**Receipt Chain:**

```erlang
%% Async receipt recording (does NOT block response)
erlmcp_receipt_chain:append_async(
    #{event => tool_call,
      tool_name => ToolName,
      timestamp => erlang:system_time(microsecond),
      hash_inputs => [ToolName, Args, Result]}).
```

**Key Guarantees:**
- Observability failures NEVER block protocol operations
- Metrics/traces/receipts are async side effects
- Isolated supervision tree prevents cascading failures
- ETS-based storage survives process restarts

---

#### 5. TCPS Quality Gate Integration (Optional)

**Auto-Integration Hook (if tcps_erlmcp loaded):**

```erlang
%% In erlmcp_server after successful tool execution
case application:get_env(tcps_erlmcp, tcps_auto_integration, false) of
    true ->
        %% Non-blocking quality gate validation
        tcps_quality_gates:validate_async(
            #{sku_id => SkuId,
              gate => runtime_quality,
              metrics => #{
                  success_rate => SuccessRate,
                  latency_p95 => LatencyP95
              }});
    false ->
        ok
end.
```

**Quality Gate Flow:**

```
MCP Operation
    ↓
[erlmcp_core] Execute tool/resource handler
    ↓
[tcps_erlmcp] Async validation (if enabled)
    ↓
[tcps_erlmcp] Check against quality thresholds
    ↓
[tcps_erlmcp] Trigger Andon if violation (async)
    ↓
[tcps_erlmcp] Record receipt in chain
    ↓
Return to MCP client (not blocked by quality checks)
```

**Key Points:**
- TCPS is entirely optional - no runtime dependency if not loaded
- Quality checks are async - don't block MCP responses
- Andon events broadcast via SSE (non-blocking)
- Receipt chain provides audit trail

---

## Application Startup and Shutdown

### Startup Sequence

```
1. Umbrella Start
   rebar3 shell / rebar3 release

2. Application Dependencies (auto-started by OTP)
   ↓ jsx, jesse, gproc
   ↓ gun, ranch, poolboy
   ↓ opentelemetry_api, opentelemetry, opentelemetry_exporter
   ↓ bbmustache, cowboy, jobs, fs (if TCPS enabled)

3. erlmcp_core_app:start/2
   ↓ Start erlmcp_sup
   ↓ Start erlmcp_core_sup
   ↓   ↓ Start erlmcp_cluster_sup
   ↓   ↓ Start erlmcp_registry (gproc ready)
   ↓   ↓ Start erlmcp_reload_sup
   ↓   ↓ Start erlmcp_session_manager
   ↓   ↓ Start erlmcp_task_manager
   ↓   ↓ Start erlmcp_resource_subscriptions
   ↓   ↓ Start erlmcp_sse_event_store
   ↓   ↓ Start erlmcp_icon_cache
   ↓   ↓ Start erlmcp_cache
   ↓   ↓ Start erlmcp_session_replicator
   ↓   ↓ Start erlmcp_session_failover
   ↓ Start erlmcp_server_sup (ready for dynamic servers)
   ↓ Start erlmcp_observability_sup
   ↓   ↓ Start erlmcp_metrics
   ↓   ↓ Start erlmcp_metrics_server
   ↓   ↓ Start erlmcp_metrics_aggregator
   ↓   ↓ Start erlmcp_dashboard_server
   ↓   ↓ Start erlmcp_health_monitor
   ↓   ↓ Start erlmcp_recovery_manager
   ↓   ↓ Start erlmcp_chaos

4. erlmcp_transports_app:start/2
   ↓ Start erlmcp_transport_sup (ready for dynamic transports)

5. erlmcp_observability_app:start/2
   ↓ (Supervisor already started by erlmcp_core)

6. tcps_erlmcp_app:start/2 [IF ENABLED]
   ↓ Start tcps_erlmcp_sup
   ↓   ↓ Start tcps_work_order
   ↓   ↓ Start tcps_kanban
   ↓   ↓ Start tcps_quality_gates
   ↓   ↓ Start tcps_sku
   ↓   ↓ Start tcps_dashboard
   ↓   ↓ Start tcps_sse_manager
   ↓   ↓ Start tcps_metrics_aggregator

7. System Ready
   ✅ Registry operational
   ✅ Transports can be started dynamically
   ✅ Servers/clients can be spawned
   ✅ Metrics collection active
   ✅ Quality gates ready (if enabled)
```

### Graceful Shutdown Sequence

```
1. Application Stop Signal (Ctrl-C, SIGTERM, rebar3 release stop)

2. OTP Stops Applications in Reverse Order
   ↓ tcps_erlmcp_app:stop/1 [IF ENABLED]
   ↓   ↓ Supervisor:terminate_child for all TCPS workers
   ↓   ↓ Flush receipt chain to disk
   ↓   ↓ Close dashboard connections
   ↓ erlmcp_observability_app:stop/1
   ↓   ↓ Flush pending metrics
   ↓   ↓ Close OTEL exporters
   ↓   ↓ Shutdown dashboard WebSockets
   ↓ erlmcp_transports_app:stop/1
   ↓   ↓ Close all active transports
   ↓   ↓ Ranch listeners stop accepting
   ↓   ↓ Gun connections closed gracefully
   ↓ erlmcp_core_app:stop/1
   ↓   ↓ Stop all MCP servers
   ↓   ↓ Stop all MCP clients
   ↓   ↓ Flush session state to Mnesia
   ↓   ↓ Deregister from gproc
   ↓   ↓ Close cache (ETS/Mnesia)

3. Dependency Shutdown
   ↓ gproc cleanup
   ↓ gun/ranch/poolboy cleanup
   ↓ opentelemetry cleanup

4. VM Shutdown
```

**Shutdown Timeouts:**
- Workers: 5000ms (default)
- Transports: 2000ms (stdio) / 5000ms (network)
- Supervisors: infinity (wait for children)
- Total graceful shutdown target: <10 seconds

---

## Design Principles

### 1. Process Isolation
Each connection runs in its own process, ensuring:
- Fault isolation
- Independent state management
- Concurrent operation

### 2. Umbrella-Based Modular Architecture

erlmcp v2.0.0 adopts the **umbrella application pattern** for:
- **Focused Dependencies** - Each app declares only what it needs
- **Independent Testing** - Test apps in isolation or together
- **Optional Features** - Exclude TCPS for minimal deployments
- **Clear Boundaries** - Apps communicate via registry, not direct calls
- **Parallel Development** - Teams can work on different apps independently

**Minimal Deployment (no TCPS):**
```erlang
{relx, [
    {release, {erlmcp, "2.0.0"},
     [erlmcp_core,
      erlmcp_transports,
      erlmcp_observability]}
]}.
```

**Full Deployment (with TCPS):**
```erlang
{relx, [
    {release, {erlmcp, "2.0.0"},
     [erlmcp_core,
      erlmcp_transports,
      erlmcp_observability,
      tcps_erlmcp]}
]}.
```

---

### 3. Supervision Trees (Bulkhead Pattern)

**Tier 1: Umbrella Level**

```
erlmcp (umbrella)
├── erlmcp_core
│   └── erlmcp_sup (rest_for_one)
├── erlmcp_transports
│   └── erlmcp_transports_sup (one_for_one)
├── erlmcp_observability
│   └── erlmcp_observability_sup (one_for_one)
└── tcps_erlmcp [OPTIONAL]
    └── tcps_erlmcp_sup (one_for_one)
```

**Tier 2: erlmcp_core Supervision (rest_for_one strategy)**

```
erlmcp_sup (rest_for_one)
├── TIER 1: erlmcp_registry_sup (one_for_one)
│   └── erlmcp_registry (gproc-based)
├── TIER 2: erlmcp_infrastructure_sup (one_for_one)
│   └── erlmcp_router
├── TIER 3: erlmcp_server_sup (simple_one_for_one)
│   └── erlmcp_server (dynamic)
└── TIER 4: erlmcp_client_sup (simple_one_for_one)
    └── erlmcp_client (dynamic)
```

**Tier 3: Transport Supervision (per-transport)**

```
erlmcp_transports_sup (one_for_one)
└── erlmcp_transport_sup (simple_one_for_one)
    ├── erlmcp_transport_stdio (dynamic)
    ├── erlmcp_transport_tcp (dynamic, ranch pools)
    ├── erlmcp_transport_http (dynamic, gun connections)
    └── erlmcp_transport_ws (dynamic)
```

### 3. Message Flow

**Client Request Flow:**
1. API call → gen_server:call
2. Encode request (JSON-RPC)
3. Send via transport
4. Await response
5. Decode response
6. Return to caller

**Server Request Flow:**
1. Receive message from transport
2. Decode request
3. Route to handler
4. Execute handler
5. Encode response
6. Send response

### 4. State Management

**Client State:**
```erlang
#state{
    transport :: module(),
    transport_state :: term(),
    capabilities :: #mcp_server_capabilities{},
    request_id :: integer(),
    pending_requests :: map(),
    subscriptions :: sets:set()
}
```

**Server State:**
```erlang
#state{
    transport :: module(),
    transport_state :: term(),
    capabilities :: #mcp_server_capabilities{},
    resources :: map(),
    tools :: map(),
    prompts :: map(),
    subscriptions :: map()
}
```

## Library Integration (v0.6.0)

### Production-Grade Libraries

erlmcp v0.6.0 replaces ~770 LOC of custom code with battle-tested Erlang libraries:

| Component | Library | Version | Purpose |
|-----------|---------|---------|---------|
| **Registry** | gproc | 0.9.0 | Process registration and discovery |
| **HTTP Client** | gun | 2.0.1 | HTTP/1.1 and HTTP/2 transport |
| **TCP Handler** | ranch | 2.1.0 | TCP connection pooling and supervision |
| **Connection Pool** | poolboy | 1.5.2 | Worker pool management |

#### Why These Libraries?

**gproc (Registry)**
- Distributed process registry with automatic cleanup
- Built-in monitoring eliminates manual process tracking
- Used across Erlang ecosystem for production workloads
- Reduces registry code from 411 LOC → 120 LOC

**gun (HTTP Transport)**
- Modern HTTP/1.1 and HTTP/2 support
- Better connection reuse and keepalive
- Production-grade error handling
- Reduces HTTP transport from 461 LOC → 180 LOC

**ranch (TCP Transport)**
- Battle-tested by EMQX and Cowboy
- Built-in connection pooling and supervisor integration
- Excellent socket lifecycle management
- Reduces TCP transport from 349 LOC → 150 LOC

**poolboy (Connection Pooling)**
- Efficient worker pool management
- Queue management under load
- Better resource utilization
- New feature in v0.6.0

### Registry Architecture with gproc

```erlang
%% Server registration
gproc:add_local_name({mcp, server, ServerId}),
gproc:reg({p, l, {mcp_server_config, ServerId}}, Config).

%% Transport registration
gproc:add_local_name({mcp, transport, TransportId}),
gproc:reg({p, l, {mcp_transport_config, TransportId}}, Config).

%% Lookup (automatic monitoring)
case gproc:lookup_local_name({mcp, server, ServerId}) of
    undefined -> {error, not_found};
    Pid -> {ok, Pid}
end.
```

**Benefits:**
- Automatic process monitoring and cleanup
- No manual `monitor`/`demonitor` code
- Distributed registry support (if needed)
- Proven reliability

### HTTP Transport with gun

```erlang
%% Initialize gun connection
{ok, GunPid} = gun:open(Host, Port, #{
    protocols => [http2, http],
    retry => 5,
    retry_timeout => 1000
}),

%% Send request
StreamRef = gun:request(GunPid, <<"POST">>, Path, Headers, Body),

%% Handle responses
handle_info({gun_response, GunPid, StreamRef, IsFin, Status, Headers}, State) ->
    %% Process response
    {noreply, State};
handle_info({gun_data, GunPid, StreamRef, IsFin, Data}, State) ->
    %% Route to server via registry
    erlmcp_registry:route_to_server(ServerId, TransportId, Data),
    {noreply, State}.
```

**Features:**
- ✅ HTTP/1.1 and HTTP/2 support (automatic)
- ✅ Connection reuse and keepalive
- ✅ TLS/SSL support
- ✅ Timeout handling
- ✅ Better error recovery

### TCP Transport with ranch

```erlang
%% Server mode - accept connections
ranch:start_listener(
    TransportId,
    ranch_tcp,
    #{port => Port, num_acceptors => 10},
    erlmcp_transport_tcp,
    [TransportId, Config]
).

%% Client mode - connect outbound
{ok, Socket} = gen_tcp:connect(Host, Port, [
    binary,
    {active, true},
    {packet, 0}
]).
```

**Features:**
- ✅ Built-in connection pooling
- ✅ Supervisor integration (ranch handles restarts)
- ✅ TCP_NODELAY and keepalive built-in
- ✅ Backpressure handling
- ✅ Graceful shutdown

### Connection Pooling with poolboy

```erlang
%% Start connection pool
poolboy:start_link([
    {name, {local, http_pool}},
    {worker_module, erlmcp_http_worker},
    {size, 10},
    {max_overflow, 5}
]).

%% Use pooled connection
poolboy:transaction(http_pool, fun(Worker) ->
    erlmcp_http_worker:request(Worker, Url, Method)
end).
```

**Benefits:**
- Better performance under load
- Resource limiting
- Queue management
- Connection reuse

## Extension Points

### Custom Transports
Implement the enhanced transport behavior:
```erlang
-callback init(TransportId :: atom(), Config :: map()) ->
    {ok, State} | {error, Reason}.
-callback send(State, Data :: iodata()) ->
    ok | {error, Reason}.
-callback close(State) -> ok.

%% Optional callbacks
-callback get_info(State) ->
    #{type => atom(), status => atom(), peer => term()}.
-callback handle_transport_call(Request :: term(), State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} | {error, Reason}.

-optional_callbacks([get_info/1, handle_transport_call/2]).
```

### Resource Handlers
```erlang
-type resource_handler() :: fun((Uri :: binary()) ->
    binary() | #mcp_content{}).
```

### Tool Handlers
```erlang
-type tool_handler() :: fun((Args :: map()) ->
    binary() | #mcp_content{} | [#mcp_content{}]).
```

## Performance Considerations

1. **Process Pooling** - poolboy manages connection pools
2. **Registry Performance** - gproc optimized for lookups
3. **HTTP/2** - gun provides HTTP/2 multiplexing
4. **ETS for Caching** - Cache frequently accessed resources
5. **Binary Handling** - Use binary strings for efficiency
6. **Lazy Evaluation** - Handlers are called only when needed

### Performance Characteristics

**Registry Lookups (gproc)**
- Local lookups: O(1) via ETS
- Distributed lookups: O(1) with gproc_dist
- Automatic cleanup on process death

**HTTP Transport (gun)**
- HTTP/2 multiplexing: Multiple streams per connection
- Connection reuse: Reduces handshake overhead
- Keepalive: Maintains persistent connections

**TCP Transport (ranch)**
- Connection pooling: Reuses established connections
- Backpressure: Handles overload gracefully
- Supervisor integration: Fast recovery

## Security Model

- Transport-level security (TLS for TCP/HTTP via gun and ranch)
- Input validation via JSON Schema
- Capability-based access control
- No direct code execution
- Library-provided security features (TLS, certificate validation)

---

## Summary: v2.0.0 Architecture Highlights

### What's New in v2.0.0

**1. Umbrella Application Structure**
- Migrated from monolithic app to 4 independent OTP applications
- Clear separation of concerns: core, transports, observability, quality
- Optional TCPS feature - can be excluded for minimal deployments
- Focused dependencies per app - only declare what you need

**2. Enhanced Supervision Architecture**
- `one_for_one` strategy throughout - no cascading failures
- Bulkhead pattern - failures isolated within app boundaries
- Observability failures never affect core MCP protocol
- Transport independence - TCP crash doesn't affect HTTP

**3. Production-Grade Libraries**
- **gproc** - Distributed registry with automatic process monitoring
- **gun** - HTTP/2 support with multiplexing and connection reuse
- **ranch** - Production TCP pooling and supervision
- **poolboy** - Efficient worker pool management
- ~770 LOC reduction - less custom code, more reliability

**4. Advanced Features**
- Multi-level caching (L1: ETS, L2: Mnesia, L3: External)
- OpenTelemetry integration with automatic span injection
- SHA-256 receipt chains for immutable audit trails
- Real-time dashboard with WebSocket broadcasting
- Chaos engineering framework for resilience testing
- 6 transport types: STDIO, TCP, HTTP/2, WebSocket, SSE, GraphQL

**5. Quality Enforcement (TCPS)**
- 8 sequential quality gates with stop-the-line authority
- SHACL ontology validation for RDF/Turtle
- Kanban WIP limits and Heijunka production leveling
- Jidoka built-in quality with Andon events
- SKU release management with evidence bundles

### Component Count

| Layer | Components | Modules |
|-------|------------|---------|
| **Core Protocol** | Client, Server, Registry, JSON-RPC, Sessions, Tasks | 14 |
| **Transports** | STDIO, TCP, HTTP/2, WS, SSE, GraphQL + Behavior | 8 |
| **Observability** | Metrics, OTEL, Receipts, Health, Recovery, Chaos | 9 |
| **Quality System** | SHACL, Gates, Kanban, SKU, Dashboard, Metrics | 63 |
| **TOTAL** | **94 modules across 4 apps** | **94** |

### Performance Characteristics

| Metric | Target | Achieved (v1.5.0 benchmarks) |
|--------|--------|------------------------------|
| Registry Throughput | >500K msg/s | 553K msg/s |
| Queue Operations | >900K ops/s | 971K ops/s |
| Pool Management | >100K ops/s | 149K ops/s |
| Session Handling | >200K ops/s | 242K ops/s |
| Network I/O | >40K msg/s | 43K msg/s (4KB packets) |
| Sustained Load | >300K msg/s | 372K msg/s (60M ops/30s) |
| Concurrent Connections | >40K | 40-50K per node |

### Production Deployment Options

**Minimal Deployment (Core + Transports + Observability):**
```bash
# Release config (rebar.config)
{relx, [
    {release, {erlmcp, "2.0.0"},
     [erlmcp_core,
      erlmcp_transports,
      erlmcp_observability]}
]}.

# Build
rebar3 as prod release

# Result: ~50MB release, 31 modules
```

**Full Deployment (with TCPS Quality System):**
```bash
# Release config
{relx, [
    {release, {erlmcp, "2.0.0"},
     [erlmcp_core,
      erlmcp_transports,
      erlmcp_observability,
      tcps_erlmcp]}
]}.

# Build
rebar3 as prod release

# Result: ~65MB release, 94 modules
```

### Migration from v1.x to v2.0

**Breaking Changes:**
- Application structure changed from single app to umbrella
- Module locations changed: `src/` → `apps/*/src/`
- Application dependencies now per-app (not umbrella-level)
- TCPS is now optional (separate app)

**Migration Steps:**
1. Update `rebar.config` dependencies to reference umbrella
2. Change application start from `erlmcp` to `[erlmcp_core, erlmcp_transports, erlmcp_observability]`
3. If using TCPS, add `tcps_erlmcp` to applications list
4. Update supervision tree references (no functional changes)
5. Run `rebar3 compile` to verify

**Compatibility:**
- ✅ API unchanged - `erlmcp_client`, `erlmcp_server` work as before
- ✅ Transport interface unchanged - custom transports still compatible
- ✅ Configuration format unchanged - same `sys.config` structure
- ✅ Protocol unchanged - MCP 1.0 compliance maintained

### Testing Strategy

**Per-App Testing:**
```bash
# Test core only
rebar3 eunit --app erlmcp_core

# Test transports only
rebar3 eunit --app erlmcp_transports

# Test observability only
rebar3 eunit --app erlmcp_observability

# Test TCPS only
rebar3 eunit --app tcps_erlmcp
```

**Integration Testing:**
```bash
# Test all apps together
rebar3 eunit

# Common Test (cross-app integration)
rebar3 ct

# Property-based testing
rebar3 proper
```

**Quality Gates:**
```bash
# Full validation
make check

# Quality gates (if TCPS enabled)
rebar3 tcps check-all-gates --sku=$(git rev-parse --short HEAD)
```

### Future Roadmap

**v2.1.0 (Planned):**
- [ ] Distributed clustering with gproc_dist
- [ ] HTTP/3 transport (QUIC)
- [ ] GraphQL subscriptions over WebSocket
- [ ] Advanced caching strategies (LRU, TTL)
- [ ] Circuit breaker patterns for external calls

**v2.2.0 (Planned):**
- [ ] Kubernetes operator for deployment
- [ ] Prometheus exporter for metrics
- [ ] Jaeger/Zipkin distributed tracing
- [ ] gRPC transport support
- [ ] Multi-region replication

**Long-Term:**
- [ ] WASM plugin support for custom handlers
- [ ] GraphQL federation
- [ ] Service mesh integration
- [ ] AI/ML model serving via MCP

---

## v1.3.0: Supervision Tree with Bulkheads (Failure Isolation)

### Problem Addressed
The original flat supervision tree used `one_for_all` strategy, meaning any component failure could cascade and restart the entire system. This caused:
- Transport failures → entire system restart
- Registry failures → all connections dropped
- Monitoring failures → protocol layer restart

### Solution: Five-Tier Isolation Model

The v1.3.0 redesign implements **bulkhead pattern** with `rest_for_one` at the top level:

```
erlmcp_sup (rest_for_one)
├── TIER 1: erlmcp_registry_sup (one_for_one)
│   └── No dependencies → Can fail independently
├── TIER 2: erlmcp_infrastructure_sup (one_for_one)
│   └── Depends on: Registry
├── TIER 3: erlmcp_server_sup (simple_one_for_one)
│   └── Depends on: Registry, Infrastructure
├── TIER 4: erlmcp_transport_sup (simple_one_for_one)
│   └── Depends on: All above
└── TIER 5: erlmcp_monitoring_sup (one_for_one) [INDEPENDENT]
    └── No protocol dependencies
```

### Failure Mode Analysis

| Failure | Isolation | Recovery | Impact |
|---------|-----------|----------|--------|
| **Registry crash** | Restart registry only (TIER 2-4 continue) | <500ms | New routing fails; existing connections continue |
| **Infrastructure crash** | Restart infrastructure (TIER 3-4 continue) | <1s | New sessions fail; existing connections continue |
| **Transport crash** | Restart transport only | <2s | Network connections lost; protocol servers unaffected |
| **Monitoring crash** | Isolated from protocol layer | <500ms | No observability; protocol layer 100% unaffected |
| **Cascade attempt** | Prevented by rest_for_one | N/A | No cascading failures observed |

### Key Improvements

**Before (v1.2.0)**:
```
one_for_all strategy
├─ Registry crash → Restart ALL (servers, transports, monitoring)
├─ Transport crash → Restart ALL
└─ Impact: ~3-5s downtime, all connections drop
```

**After (v1.3.0)**:
```
rest_for_one strategy with tiered architecture
├─ Registry crash → Restart Registry only (~500ms)
├─ Transport crash → Restart Transport only (~2s)
└─ Monitoring crash → Isolated, NO protocol impact (~500ms)
```

### Testing
See `test/erlmcp_supervision_SUITE.erl` for comprehensive failure scenario testing:
- Tree structure validation
- Failure isolation verification
- Recovery time measurement
- Connection survival rates
- Cascading failure prevention
- System stability under repeated crashes

Run tests: `rebar3 ct --suite=erlmcp_supervision_SUITE`

### Documentation
Complete architecture with diagrams: `docs/c4/supervision-v1.3.0.md`