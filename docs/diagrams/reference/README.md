# erlmcp Reference Documentation Diagrams

This directory contains comprehensive Mermaid reference documentation diagrams for the erlmcp (Erlang/OTP Model Context Protocol SDK) system.

## Diagrams

### 1. Module Index (`module-index.mmd`)
**Size:** 503 lines | **22KB

Complete module organization showing all 164 modules across 4 applications:
- **Application Layer** - Entry points for erlmcp_core, erlmcp_transports, erlmcp_observability, erlmcp_validation
- **Core Protocol (97 modules)** - Message flow, client/server, registry, sessions, capabilities, security, resilience, utilities, LLM integration, distributed systems
- **Transport Layer (23 modules)** - Behavior interface, implementations (stdio, TCP, HTTP, WebSocket, SSE), infrastructure, pooling, security
- **Observability Layer (31 modules)** - OpenTelemetry, metrics, tracing, chaos engineering, monitoring, debugging, audit trails
- **Validation Layer (13 modules)** - Protocol validators, compliance reports, test tools, CLI interface

**Key Features:**
- Visualizes module dependencies with directional arrows
- Color-coded by functional layer
- Shows behavior implementations (e.g., session backends)
- Cross-layer integration patterns

### 2. Function Signatures (`function-signatures.mmd`)
**Size:** 445 lines | **19KB

Core API surface showing 93 function signatures across 9 major APIs:

**API Sections:**
1. **Client API (19 functions)** - Lifecycle, resources, prompts, tools, completions, batching, handlers
2. **Server API (18 functions)** - Lifecycle, resource/tool/prompt management, subscriptions, progress, handlers
3. **Registry API (8 functions)** - Server/client registration, lookup, listing
4. **Session API (12 functions)** - Lifecycle, state management, capabilities, metadata
5. **Transport API (6 functions)** - Behavior callbacks, message types
6. **JSON-RPC API (8 functions)** - Encoding/decoding, validation
7. **Refusal API (5 functions)** - Error lookup, formatting
8. **Metrics API (10 functions)** - Counters, gauges, histograms, timing
9. **Auth API (7 functions)** - Authentication, authorization, session management

**Key Features:**
- Function signatures with arity and return types
- Call flow relationships (direct calls vs. dependencies)
- API variant relationships (e.g., `add_tool` vs. `add_tool_with_schema`)
- Cross-module dependencies

### 3. Error Codes (`error-codes.mmd`)
**Size:** 307 lines | **16KB

Complete refusal taxonomy showing all 89 error codes (1001-1089):

**Error Categories:**
1. **Queue & Backpressure (1001-1005)** - 5 codes, HTTP 429/503
2. **Authentication (1011-1016)** - 6 codes, HTTP 401/403
3. **Parameter Validation (1021-1029)** - 9 codes, HTTP 400
4. **Path Security (1036-1040)** - 5 codes, HTTP 400
5. **Resource/Entity (1046-1052)** - 7 codes, HTTP 404/409
6. **Rate Limiting (1056-1060)** - 5 codes, HTTP 429
7. **Protocol/Transport (1066-1070)** - 5 codes, HTTP 400/413/415/503
8. **Server State (1076-1080)** - 5 codes, HTTP 503
9. **Circuit Breaker/Health (1086-1089)** - 4 codes, HTTP 503

**Key Features:**
- Error code hierarchy with severity levels (warn, error, critical)
- HTTP status code mappings
- Remediation hints for each error
- Severity classification summary
- Error category counts

### 4. Configuration Reference (`configuration-reference.mmd`)
**Size:** 515 lines | **23KB

Complete configuration parameter relationships from `config/sys.config`:

**Configuration Sections:**
1. **Client/Server Defaults** - Timeouts, strict mode, limits
2. **Message Size Limits** - Per-transport bounds (16MB default)
3. **Transport Defaults** - TCP/HTTP connection settings
4. **HTTP Security** - CORS, session timeout, HTTPS enforcement
5. **Localhost Binding** - Security policy (Gap #32)
6. **HTTPS/TLS** - Certificate validation, cipher suites, HSTS
7. **Session Manager** - Lifecycle, cleanup intervals
8. **Session Replication** - Distributed state sync (cluster mode)
9. **WebSocket** - Backpressure, connection limits (Gap #46)
10. **SSE** - Keepalive, retry timeouts (Gap #29)
11. **Rate Limiting** - Token bucket, DoS protection (Gap #47)
12. **Connection Pool** - 100K concurrent connections config
13. **Queue Limits** - Backpressure bounds (v1.3.0)
14. **Backpressure** - Adaptive rate limiting (v1.3.0)
15. **Circuit Breaker** - Failure thresholds (v1.3.0)
16. **Lifecycle Management** - TTL, cleanup, resource limits
17. **Logger** - OTP 21+ logger configuration
18. **OpenTelemetry** - Telemetry exporter settings

**Key Features:**
- Configuration hierarchy and dependencies
- Data type annotations (boolean, integer, binary, atom, list, map)
- Security notes (verify_mode, localhost_only, HTTPS enforcement)
- Performance tuning guidelines
- Default values with explanations

### 5. Troubleshooting Guide (`troubleshooting-guide.mmd`)
**Size:** 501 lines | **21KB

Comprehensive diagnostic flowchart covering 8 problem categories:

**Troubleshooting Categories:**
1. **Connection Issues** - Server not running, binding problems, firewall, TLS
2. **Authentication Failures** - Credentials, tokens, session IDs, permissions
3. **Rate Limiting** - Overall, per-second, per-minute, quota, connection limits
4. **Performance Issues** - High latency, CPU/memory, queue depths, circuit breaker
5. **Message/Protocol Errors** - Invalid parameters, URI errors, message size, protocol errors
6. **Resource/Subscription Issues** - Not found, duplicates, subscription limits, notifications
7. **Crashes/Restarts** - Pattern matching errors, timeouts, supervisor loops
8. **Memory Leaks** - Process growth, ETS tables, binary heap, port accumulation

**Key Features:**
- Decision tree flowcharts with { } decision nodes
- Actionable remediation steps
- Error code cross-references
- Diagnostic tools reference (Observer, Metrics, Logger, Recon, Debugger, Profiler)
- Quick reference commands for common operations

## Usage

### Viewing Diagrams

These Mermaid diagrams can be viewed using:

1. **VS Code** - Install "Markdown Preview Mermaid Support" extension
2. **GitHub/GitLab** - Native Mermaid rendering in markdown files
3. **Mermaid Live Editor** - https://mermaid.live
4. **Command Line** - `npx @mermaid-js/mermaid-cli -i diagram.mmd -o diagram.png`

### Integrating with Documentation

Include in markdown:

````markdown
```mermaid
{{include:docs/diagrams/reference/module-index.mmd}}
```
````

Or link to diagrams:

````markdown
See [Module Organization Diagram](diagrams/reference/module-index.mmd)
````

### Generating PNG/SVG

```bash
# Install mermaid-cli
npm install -g @mermaid-js/mermaid-cli

# Generate PNG
mmdc -i docs/diagrams/reference/module-index.mmd -o docs/diagrams/reference/module-index.png

# Generate SVG
mmdc -i docs/diagrams/reference/module-index.mmd -o docs/diagrams/reference/module-index.svg
```

## Maintenance

When updating erlmcp:

1. **Module Changes** - Update `module-index.mmd` when adding/removing modules
2. **API Changes** - Update `function-signatures.mmd` for new/deprecated functions
3. **Error Codes** - Update `error-codes.mmd` when adding refusal codes
4. **Configuration** - Update `configuration-reference.mmd` for new config parameters
5. **Troubleshooting** - Update `troubleshooting-guide.mmd` for new issues/fixes

## Statistics

- **Total Lines:** 2,271 lines of Mermaid DSL
- **Total Size:** 101KB of diagram definitions
- **Modules Covered:** 164 modules across 4 applications
- **Functions Documented:** 93 core API functions
- **Error Codes:** 89 refusal codes with remediation
- **Config Parameters:** 100+ configuration options
- **Troubleshooting Flows:** 8 problem categories with diagnostic paths

## Related Documentation

- [../module-dependencies.mmd](../module-dependencies.mmd) - Detailed module dependency graph
- [../supervision-tree.mmd](../supervision-tree.mmd) - OTP supervision hierarchy
- [../system-architecture.mmd](../system-architecture.mmd) - High-level architecture
- [../data-flow.mmd](../data-flow.mmd) - Message flow patterns
- [../../architecture.md](../../architecture.md) - Textual architecture documentation
- [../../api-reference.md](../../api-reference.md) - Complete API reference

## License

These diagrams are part of the erlmcp project and follow the same license terms.
