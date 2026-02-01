# erlmcp Architecture Diagrams

Comprehensive Mermaid architecture diagrams for the erlmcp (Erlang/OTP Model Context Protocol) SDK v2.1.0.

## Overview

This directory contains production-ready Mermaid diagrams that document the complete system architecture, supervision hierarchy, data flow, module dependencies, and transport interfaces of erlmcp.

**Total Diagrams**: 5 comprehensive visualizations
**Total Lines**: 1,173 lines of Mermaid markup
**Coverage**: 164 modules across 4 layers (core, transports, observability, validation)

## Diagram Files

### 1. system-architecture.mmd (225 lines)

**Purpose**: Overall system topology showing the 4 main components

**Contents**:
- Client Layer (AI Runtime, Client Tools)
- Transport Layer (stdio, tcp, http, ws, sse)
- Core Layer (97 modules)
  - Protocol Processing
  - Session Management
  - MCP Capabilities (resources, tools, prompts, subscriptions)
  - Security (auth, mTLS, secrets)
  - Resilience (circuit breaker, rate limiting, connection monitoring)
  - Server & Client
  - Registry (gproc-based routing)
  - LLM Integration (Claude, GPT-4, Local)
- Observability Layer (31 modules)
  - OpenTelemetry (Datadog, Honeycomb, Jaeger)
  - Metrics & Tracing
  - Chaos Engineering
  - Monitoring & Health
- Validation Layer (13 modules)
  - Protocol, Transport, Security, Performance validators
  - Compliance reporting
  - MCP spec parser
- External Dependencies

**Use Case**: High-level system understanding for architects, technical leads, and onboarding

### 2. supervision-tree.mmd (204 lines)

**Purpose**: Detailed supervision hierarchy showing all OTP supervisors and their children

**Contents**:
- **TIER 1**: Application Supervisors (one_for_all strategy)
  - Root supervisor
  - Core, Transports, Observability, Validation application supervisors
- **TIER 2**: Service Supervisors (simple_one_for_one strategy)
  - Server/Client/Session supervisors (process-per-connection)
  - Chaos worker supervisor
  - Cache, notification, failover, reload, cluster supervisors
- **TIER 3**: Isolated Workers
  - Server processes (isolated per connection)
  - Client processes (request correlation)
  - Session processes (state management)
  - Chaos workers (failure injection)
  - Failover workers (recovery coordination)
- **Standalone Processes** (monitored, not linked)
  - Registry, health monitors, metrics servers
  - Circuit breakers, rate limiters, connection limiters
  - Auth, secrets, OpenTelemetry
  - Chaos coordinator, recovery manager
  - Change notifiers, memory/CPU guards
  - Node monitor, split-brain detector
  - Debugger, profiler

**Use Case**: Understanding OTP supervision patterns, fault isolation, and process topology

### 3. data-flow.mmd (174 lines)

**Purpose**: Data flow between components showing request/response patterns

**Contents**:
- **Request Flow**: AI Runtime → Transport → Message Handler → Parser → JSON-RPC → Validators → Registry → Server
- **Server Processing**: Session loading, capability invocation (tools/resources/prompts), state persistence
- **Response Flow**: JSON-RPC encoding → OpenTelemetry tracing → Transport → AI Runtime
- **Subscription Flow**: Resource change detection → Publisher → Subscriber lookup → Push notifications
- **Chaos Injection**: Network latency, process kill, resource exhaustion scenarios
- **Session Failover**: Backend failure detection → Replica switch → Processing resume
- **Metrics Collection**: Latency/throughput/memory/error tracking → Aggregation → Export
- **Health Checks**: Self-check → Heartbeat → Connectivity check → Recovery/healing
- **Security Flow**: Authentication → Secrets fetch → Authorization → Request processing/refusal
- **Rate Limiting**: Quota check → Limit enforcement → Request processing/refusal
- **Validation Compliance**: Protocol/security/performance validation → Compliance reporting

**Use Case**: Understanding message flow, request lifecycle, and cross-cutting concerns

### 4. module-dependencies.mmd (348 lines)

**Purpose**: Module dependency graph for the 164 modules

**Contents**:
- **Core Protocol Layer**: JSON-RPC, message handler/parser, size validation
- **Registry Layer**: gproc-based routing, distributed registry, utilities
- **Server/Client Layer**: Supervisors and processes
- **Session Layer**: ETS/DETS/Mnesia backends, failover, replication
- **Capabilities Layer**: Resources, tools, prompts, subscriptions, sampling, completion, progress
- **Security Layer**: Auth, mTLS, rate limiting, secrets management
- **Resilience Layer**: Circuit breaker, rate limiting middleware, connection monitoring/limiting
- **LLM Integration Layer**: Anthropic, OpenAI, local providers, mock LLM
- **Transport Layer**: All transport implementations and infrastructure
- **Observability Layer**: OTEL, metrics, tracing, chaos, monitoring, audit
- **Validation Layer**: All validators and compliance reporters
- **Utility Layer**: Batch, cancellation, pagination, logging, health, hooks, cache, guards, etc.

**Use Case**: Understanding module coupling, dependency chains, and refactoring impact analysis

### 5. transport-interfaces.mmd (222 lines)

**Purpose**: All supported transport interfaces and their relationships

**Contents**:
- **Transport Behavior Contract**: Callback interface (init/2, send/2, close/1)
- **STDIO Transport**: Process I/O, bidirectional JSON-RPC, synchronous request/response, VSCode integration
- **TCP Transport**: Ranch acceptor pool, full-duplex, connection pooling, 43K msg/s throughput
- **HTTP Transport**: Gun client, RESTful endpoints, bidirectional streaming, Cowboy server
- **WebSocket Transport**: Full-duplex persistent, low latency, binary/text support, auto-reconnection
- **SSE Transport**: Unidirectional push, auto-reconnection, event streaming, change notifications
- **Transport Infrastructure**: Pool manager, pipeline, health monitoring, service discovery, validation
- **Transport Security**: TLS validation, CORS validation, header validation, security headers
- **Integration Points**: Client/server transport integration
- **Benchmarks**: Per-transport benchmarks (stdio, tcp, http, ws, sse, integration)
- **Message Flow Algorithm**: 8-step transport message handling

**Use Case**: Understanding transport polymorphism, implementing new transports, transport selection

## Rendering the Diagrams

### Online Rendering (Recommended)

1. **Mermaid Live Editor**: https://mermaid.live
   - Copy the content of any `.mmd` file
   - Paste into the editor
   - Export as SVG/PNG

2. **GitHub/GitLab Rendering**: Native support in Markdown
   ```markdown
   ```mermaid
   #include system-architecture.mmd
   ```
   ```

3. **VS Code Extension**: "Markdown Preview Mermaid Support"
   - Install extension
   - Open `.mmd` file
   - Preview with `Cmd+Shift+V` (Mac) or `Ctrl+Shift+V` (Windows/Linux)

### Command-Line Rendering

```bash
# Using mermaid-cli (npm install -g @mermaid-js/mermaid-cli)
mmdc -i system-architecture.mmd -o system-architecture.svg
mmdc -i supervision-tree.mmd -o supervision-tree.svg
mmdc -i data-flow.mmd -o data-flow.svg
mmdc -i module-dependencies.mmd -o module-dependencies.svg
mmdc -i transport-interfaces.mmd -o transport-interfaces.svg

# Batch convert all diagrams
for f in *.mmd; do
  mmdc -i "$f" -o "${f%.mmd}.svg"
  mmdc -i "$f" -o "${f%.mmd}.png"
done
```

### Documentation Integration

The diagrams are designed to integrate with:

- **Main Documentation**: `docs/architecture.md`
- **API Reference**: `docs/api-reference.md`
- **OTP Patterns**: `docs/otp-patterns.md`
- **Protocol Docs**: `docs/protocol.md`

## Diagram Standards

### Color Coding

All diagrams use consistent color schemes:

- **Blue tones**: Protocol, communication layers
- **Green tones**: Capabilities, features
- **Red tones**: Security, validation
- **Purple tones**: Registry, infrastructure
- **Orange tones**: Resilience, monitoring
- **Yellow tones**: Transports, utilities
- **Gray tones**: External dependencies, standalone processes

### Notation Conventions

- **Solid arrows (`-->`)**: Direct dependencies, synchronous calls
- **Dashed arrows (`-.->`)**: Monitoring relationships, weak references
- **Bidirectional arrows (`<-->`)**: Bidirectional communication
- **Subgraphs (`subgraph`)**: Logical groupings, layer boundaries

### Naming Conventions

- Module names: `erlmcp_*` format
- Process names: Capitalized with description
- External dependencies: Lowercase (gproc, jsx, jesse)
- Annotations: Descriptive text in brackets `[...]`

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 2.1.0 | 2026-01-31 | Initial comprehensive diagram suite (5 diagrams, 1,173 lines) |

## Maintenance

When updating code:

1. **New Modules**: Add to `module-dependencies.mmd` first
2. **New Transports**: Update `transport-interfaces.mmd` and `system-architecture.mmd`
3. **Supervision Changes**: Update `supervision-tree.mmd`
4. **Protocol Changes**: Update `data-flow.mmd`
5. **Architecture Changes**: Update `system-architecture.mmd`

## Contributing

When adding new diagrams:

1. Use the established color scheme
2. Follow naming conventions
3. Include comprehensive subgraphs for logical grouping
4. Add usage case in this README
5. Update diagram count (total lines) in README header

## References

- **erlmcp Specification**: `CLAUDE.md`
- **OTP Patterns**: `docs/otp-patterns.md`
- **Module Taxonomy**: `CLAUDE.md` (Core Module Taxonomy section)
- **Transport Behavior**: `apps/erlmcp_transports/src/erlmcp_transport_behavior.erl`

## License

Same as erlmcp project (Apache 2.0)
