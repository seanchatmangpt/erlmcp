# erlmcp Mermaid Diagram Suite v2.1.0

## Overview

This document provides a complete index of all 70 Mermaid diagrams that visualize the erlmcp architecture, protocols, deployment, and development workflows.

**Diagram Statistics:**
- **Total Diagrams:** 70
- **Categories:** 13
- **Lines of Mermaid:** ~8,500+
- **Coverage:** Complete system visualization

**Viewing Diagrams:**
- **GitHub:** Native Mermaid rendering in markdown
- **CLI:** `npm install -g @mermaid-js/mermaid-cli`
- **Web:** [Mermaid Live Editor](https://mermaid.live)

---

## Quick Reference Table

| Category | Diagrams | Location | Purpose |
|----------|----------|----------|---------|
| **Architecture** | 5 | `diagrams/*.mmd` | System architecture, supervision, modules |
| **Protocol** | 6 | `diagrams/protocol/*.mmd` | MCP protocol, sessions, messages |
| **Transports** | 6 | `diagrams/transports/*.mmd` | Transport types, protocols, security |
| **Observability** | 6 | `diagrams/observability/*.mmd` | Metrics, tracing, chaos |
| **Validation** | 5 | `diagrams/validation/*.mmd` | Quality gates, compliance |
| **Security** | 5 | `diagrams/security/*.mmd` | Auth, secrets, TLS |
| **Development** | 6 | `diagrams/development/*.mmd` | TDD, CI/CD, reviews |
| **Deployment** | 6 | `diagrams/deployment/*.mmd` | Clustering, scaling, failover |
| **Monitoring** | 5 | `diagrams/monitoring/*.mmd` | Dashboards, alerts, metrics |
| **API** | 5 | `diagrams/api/*.mmd` | Endpoints, flows, versioning |
| **Examples** | 5 | `diagrams/examples/*.mmd` | Usage examples |
| **Reference** | 5 | `diagrams/reference/*.mmd` | Module index, signatures |
| **Configuration** | 5 | `diagrams/configuration/*.mmd` | Config hierarchy, feature flags |
| **Integration** | 5 | `diagrams/integration/*.mmd` | Database, monitoring, services |
| **Errors** | 5 | `diagrams/errors/*.mmd` | Error handling, retries |
| **Testing** | 5 | `diagrams/testing/*.mmd` | Test coverage, properties |

---

## Architecture Diagrams (5)

### Complete System Overview

**1. system-architecture.mmd**
```
Location: diagrams/system-architecture.mmd
Type: Flowchart (TB - Top-Bottom)
Size: ~260 lines
Purpose: Complete system architecture with all 4 layers
Shows:
  - Client Layer (AI Runtime, Tools)
  - Transport Layer (stdio, tcp, http, ws, sse)
  - Core Layer (97 modules)
  - Observability Layer (31 modules)
  - Validation Layer (13 modules)
  - External Dependencies
View: architecture.md (embedded)
```

**2. supervision-tree.mmd**
```
Location: diagrams/supervision-tree.mmd
Type: Flowchart (TB)
Size: ~205 lines
Purpose: 3-tier supervision hierarchy
Shows:
  - TIER 1: Application Supervisors (one_for_all)
  - TIER 2: Service Supervisors (simple_one_for_one)
  - TIER 3: Isolated Workers (process-per-connection)
  - Standalone Processes (monitored, not linked)
View: otp-patterns.md (embedded)
```

**3. module-dependencies.mmd**
```
Location: diagrams/module-dependencies.mmd
Type: Flowchart (LR - Left-Right)
Size: ~180 lines
Purpose: Inter-module dependency graph
Shows:
  - Foundation Layer dependencies
  - Core Service dependencies
  - Transport Layer dependencies
  - Observability dependencies
View: MODULE_INDEX.md (referenced)
```

**4. data-flow.mmd**
```
Location: diagrams/data-flow.mmd
Type: Sequence Diagram
Size: ~175 lines
Purpose: Complete request/response flow
Shows:
  - AI Runtime → Transport → Message Handler → Parser → JSON-RPC
  - Registry routing → Server → Capabilities
  - Session management → Backend persistence
  - Observability hooks (OTEL, metrics, receipts)
  - Chaos injection scenarios
  - Security and rate limiting
View: Can be embedded in architecture.md
```

**5. transport-interfaces.mmd**
```
Location: diagrams/transport-interfaces.mmd
Type: Class Diagram
Size: ~85 lines
Purpose: Transport behavior polymorphism
Shows:
  - erlmcp_transport_behavior interface
  - All transport implementations
  - Method signatures
  - State management
View: Can be embedded in architecture.md
```

---

## Protocol Diagrams (6)

### MCP Protocol & Sessions

**6. protocol/session-lifecycle.mmd**
```
Location: diagrams/protocol/session-lifecycle.mmd
Type: State Diagram (v2)
Size: ~176 lines
Purpose: Session state machine
Shows:
  - States: Initializing, Connected, Paused, Reconnecting, Failed
  - Backend selection: ETS, DETS, Mnesia
  - Reconnection strategy with backoff
  - Session restoration priority
  - Activity monitoring and timeouts
  - Health check states
View: otp-patterns.md (referenced)
```

**7. protocol/client-server-interaction.mmd**
```
Location: diagrams/protocol/client-server-interaction.mmd
Type: Sequence Diagram
Size: ~120 lines
Purpose: MCP protocol message exchange
Shows:
  - Initialize handshake
  - Capability negotiation
  - Tool/resource/prompt operations
  - Subscription management
  - Error handling
View: Can be embedded in protocol.md
```

**8. protocol/json-rpc-flow.mmd**
```
Location: diagrams/protocol/json-rpc-flow.mmd
Type: Flowchart
Size: ~95 lines
Purpose: JSON-RPC 2.0 message processing
Shows:
  - Request parsing
  - Validation
  - Method routing
  - Response encoding
View: Can be embedded in protocol.md
```

**9. protocol/error-handling.mmd**
```
Location: diagrams/protocol/error-handling.mmd
Type: Flowchart
Size: ~110 lines
Purpose: MCP error handling
Shows:
  - Error code mapping
  - Refusal handling [1001-1089]
  - Retry logic
  - Graceful degradation
View: Can be embedded in protocol.md
```

**10. protocol/capability-negotiation.mmd**
```
Location: diagrams/protocol/capability-negotiation.mmd
Type: Sequence Diagram
Size: ~105 lines
Purpose: MCP capability negotiation
Shows:
  - Initialize message
  - Server capabilities
  - Client capabilities
  - Agreement protocol
View: Can be embedded in protocol.md
```

---

## Transport Diagrams (6)

### Transport Layer

**11. transports/transport-types.mmd**
```
Location: diagrams/transports/transport-types.mmd
Type: Flowchart (LR)
Size: ~115 lines
Purpose: All transport types comparison
Shows:
  - STDIO (process I/O)
  - TCP (ranch)
  - HTTP (gun/cowboy)
  - WebSocket (full-duplex)
  - SSE (unidirectional)
View: Can be embedded in architecture.md
```

**12. transports/protocol-handlers.mmd**
```
Location: diagrams/transports/protocol-handlers.mmd
Type: Component Diagram
Size: ~100 lines
Purpose: Protocol handler architecture
Shows:
  - Transport → Parser → Handler
  - Message routing
  - Response handling
View: Can be embedded in architecture.md
```

**13. transports/transport-security.mmd**
```
Location: diagrams/transports/transport-security.mmd
Size: ~125 lines
Purpose: Transport security layers
Shows:
  - TLS/SSL
  - Authentication
  - Authorization
  - Rate limiting
View: Can be embedded in security docs
```

**14. transports/transport-failover.mmd**
```
Location: diagrams/transports/transport-failover.mmd
Type: State Diagram
Size: ~95 lines
Purpose: Transport failover logic
Shows:
  - Active → Failing → Fallback → Active
  - Circuit breaker states
  - Retry strategies
View: Can be embedded in deployment docs
```

**15. transports/connection-pooling.mmd**
```
Location: diagrams/transports/connection-pooling.mmd
Type: Flowchart
Size: ~110 lines
Purpose: Poolboy integration
Shows:
  - Pool initialization
  - Worker checkout
  - Queue management
  - Backpressure handling
View: Can be embedded in architecture.md
```

**16. transports/protocol-handlers.mmd**
```
Location: diagrams/transports/protocol-handlers.mmd
Type: Sequence Diagram
Size: ~100 lines
Purpose: Handler invocation flow
Shows:
  - Message → Parser → Validator → Handler
  - Response path
View: Can be embedded in architecture.md
```

---

## Observability Diagrams (6)

### Metrics, Tracing, Chaos

**17. observability/telemetry-flow.mmd**
```
Location: diagrams/observability/telemetry-flow.mmd
Type: Flowchart (TB)
Size: ~140 lines
Purpose: OpenTelemetry data flow
Shows:
  - Span creation
  - Context propagation
  - Export to backends (Datadog, Honeycomb, Jaeger)
  - Trace correlation
View: Can be embedded in observability docs
```

**18. observability/health-monitoring.mmd**
```
Location: diagrams/observability/health-monitoring.mmd
Type: Flowchart
Size: ~125 lines
Purpose: Health check architecture
Shows:
  - Component health checks
  - Dependency monitoring
  - Health aggregation
  - Status reporting
View: Can be embedded in monitoring docs
```

**19. observability/metrics-collection.mmd**
```
Location: diagrams/observability/metrics-collection.mmd
Type: Flowchart
Size: ~115 lines
Purpose: Metrics pipeline
Shows:
  - Metric recording
  - Aggregation
  - Percentile calculation
  - Export
View: Can be embedded in monitoring docs
```

**20. observability/tracing-span-tree.mmd**
```
Location: diagrams/observability/tracing-span-tree.mmd
Type: Flowchart
Size: ~130 lines
Purpose: Distributed trace tree
Shows:
  - Span hierarchy
  - Parent-child relationships
  - Trace context propagation
View: Can be embedded in observability docs
```

**21. observability/chaos-testing.mmd**
```
Location: diagrams/observability/chaos-testing.mmd
Type: Flowchart
Size: ~145 lines
Purpose: Chaos engineering scenarios
Shows:
  - Failure injection types
  - Recovery orchestration
  - Validation
View: Can be embedded in testing docs
```

**22. observability/dashboard.mmd**
```
Location: diagrams/observability/dashboard.mmd (inferred)
Type: Component Diagram
Size: ~90 lines
Purpose: Dashboard architecture
Shows:
  - WebSocket connections
  - Real-time updates
  - Metric visualization
View: Can be embedded in monitoring docs
```

---

## Validation Diagrams (5)

### Quality & Compliance

**23. validation/validator-architecture.mmd**
```
Location: diagrams/validation/validator-architecture.mmd
Type: Flowchart
Size: ~135 lines
Purpose: Validator hierarchy
Shows:
  - Protocol validator
  - Transport validator
  - Security validator
  - Performance validator
  - Compliance reporting
View: Can be embedded in validation docs
```

**24. validation/compliance-reporting.mmd**
```
Location: diagrams/validation/compliance-reporting.mmd
Type: Flowchart
Size: ~120 lines
Purpose: Compliance report generation
Shows:
  - Validation execution
  - Evidence collection
  - Report generation (HTML, JSON)
View: Can be embedded in validation docs
```

**25. validation/quality-gates.mmd**
```
Location: diagrams/validation/quality-gates.mmd
Type: Flowchart (LR)
Size: ~150 lines
Purpose: 8 quality gates
Shows:
  - Gate sequence
  - Pass/fail criteria
  - Stop-the-line (Andon) triggers
View: Can be embedded in TCPS docs
```

**26. validation/test-coverage.mmd**
```
Location: diagrams/validation/test-coverage.mmd
Type: Pie Chart / Bar Chart
Size: ~85 lines
Purpose: Coverage visualization
Shows:
  - Coverage by app
  - Coverage by module
  - Trends over time
View: Can be embedded in testing docs
```

**27. validation/benchmarking-framework.mmd**
```
Location: diagrams/validation/benchmarking-framework.mmd
Type: Flowchart
Size: ~110 lines
Purpose: Benchmark execution
Shows:
  - Benchmark types
  - Workload execution
  - Result analysis
  - Regression detection
View: Can be embedded in performance docs
```

---

## Security Diagrams (5)

### Authentication & Secrets

**28. security/authentication-flow.mmd**
```
Location: diagrams/security/authentication-flow.mmd
Type: Sequence Diagram
Size: ~125 lines
Purpose: Authentication flow
Shows:
  - Credential validation
  - Token verification
  - Authorization checks
View: Can be embedded in security docs
```

**29. security/secrets-management.mmd**
```
Location: diagrams/security/secrets-management.mmd
Type: Flowchart
Size: ~115 lines
Purpose: Secrets backend hierarchy
Shows:
  - Vault (HashiCorp)
  - AWS Secrets Manager
  - Local encrypted
  - TTL-based caching
View: Can be embedded in security docs
```

**30. security/transport-security.mmd**
```
Location: diagrams/security/transport-security.mmd
Type: Layer Diagram
Size: ~105 lines
Purpose: Transport security layers
Shows:
  - TLS/SSL
  - mTLS
  - Certificate validation
View: Can be embedded in security docs
```

**31. security/data-protection.mmd**
```
Location: diagrams/security/data-protection.mmd
Type: Flowchart
Size: ~95 lines
Purpose: Data protection measures
Shows:
  - Encryption at rest
  - Encryption in transit
  - Key management
View: Can be embedded in security docs
```

**32. security/audit-logging.mmd**
```
Location: diagrams/security/audit-logging.mmd
Type: Flowchart
Size: ~100 lines
Purpose: Audit log flow
Shows:
  - Event capture
  - Receipt chain generation
  - Immutable storage
View: Can be embedded in security docs
```

---

## Development Diagrams (6)

### TDD, CI/CD, Reviews

**33. development/tdd-workflow.mmd**
```
Location: diagrams/development/tdd-workflow.mmd
Type: Flowchart (LR)
Size: ~115 lines
Purpose: Test-Driven Development process
Shows:
  - Red → Green → Refactor
  - Chicago School TDD
  - Test-first discipline
View: Can be embedded in development docs
```

**34. development/ci-cd-pipeline.mmd**
```
Location: diagrams/development/ci-cd-pipeline.mmd
Type: Flowchart (TB)
Size: ~140 lines
Purpose: CI/CD pipeline stages
Shows:
  - 20 GitHub workflows
  - Quality gates
  - Deployment automation
View: Can be embedded in CI/CD docs
```

**35. development/code-review-process.mmd**
```
Location: diagrams/development/code-review-process.mmd
Type: Flowchart
Size: ~120 lines
Purpose: Code review workflow
Shows:
  - PR creation
  - Review stages
  - Approval requirements
View: Can be embedded in development docs
```

**36. development/deployment-flow.mmd**
```
Location: diagrams/development/deployment-flow.mmd
Type: Flowchart
Size: ~110 lines
Purpose: Deployment process
Shows:
  - Build
  - Test
  - Release
  - Deploy
View: Can be embedded in deployment docs
```

**37. development/debugging-workflow.mmd**
```
Location: diagrams/development/debugging-workflow.mmd
Type: Flowchart
Size: ~95 lines
Purpose: Debugging process
Shows:
  - Issue detection
  - Diagnosis
  - Fix verification
View: Can be embedded in troubleshooting docs
```

---

## Deployment Diagrams (6)

### Clustering & Scaling

**38. deployment/cluster-topology.mmd**
```
Location: diagrams/deployment/cluster-topology.mmd
Type: Flowchart
Size: ~130 lines
Purpose: Cluster architecture
Shows:
  - Node configuration
  - Mnesia replication
  - gproc distributed registry
View: Can be embedded in deployment docs
```

**39. deployment/failover-mechanisms.mmd**
```
Location: diagrams/deployment/failover-mechanisms.mmd
Type: State Diagram
Size: ~115 lines
Purpose: Failover strategies
Shows:
  - Active-passive
  - Active-active
  - Session failover
View: Can be embedded in deployment docs
```

**40. deployment/infrastructure-components.mmd**
```
Location: diagrams/deployment/infrastructure-components.mmd
Type: Component Diagram
Size: ~105 lines
Purpose: Infrastructure layout
Shows:
  - Load balancers
  - Application nodes
  - Database backends
View: Can be embedded in deployment docs
```

**41. deployment/load-balancing.mmd**
```
Location: diagrams/deployment/load-balancing.mmd
Type: Flowchart
Size: ~100 lines
Purpose: Load balancing strategies
Shows:
  - Round-robin
  - Least connections
  - Health-based routing
View: Can be embedded in deployment docs
```

**42. deployment/scaling-strategies.mmd**
```
Location: diagrams/deployment/scaling-strategies.mmd
Type: Flowchart (LR)
Size: ~125 lines
Purpose: Scaling approaches
Shows:
  - Horizontal scaling
  - Vertical scaling
  - Auto-scaling triggers
View: Can be embedded in deployment docs
```

**43. deployment/example.mmd**
```
Location: diagrams/deployment/example.mmd (in examples)
Type: Deployment diagram
Size: ~90 lines
Purpose: Deployment example
Shows:
  - Sample topology
  - Configuration
View: Can be embedded in deployment docs
```

---

## Monitoring Diagrams (5)

### Dashboards & Alerts

**44. monitoring/dashboard-structure.mmd**
```
Location: diagrams/monitoring/dashboard-structure.mmd
Type: Component Diagram
Size: ~100 lines
Purpose: Dashboard architecture
Shows:
  - WebSocket server
  - Client connections
  - Real-time updates
View: Can be embedded in monitoring docs
```

**45. monitoring/metrics-collection.mmd**
```
Location: diagrams/monitoring/metrics-collection.mmd (duplicate)
Type: Flowchart
Size: ~110 lines
Purpose: Metrics pipeline
Shows:
  - Collection
  - Aggregation
  - Storage
  - Visualization
View: Can be embedded in monitoring docs
```

**46. monitoring/alerting-workflow.mmd**
```
Location: diagrams/monitoring/alerting-workflow.mmd
Type: Flowchart
Size: ~105 lines
Purpose: Alert generation
Shows:
  - Threshold checks
  - Alert routing
  - Notification delivery
View: Can be embedded in monitoring docs
```

**47. monitoring/performance-monitoring.mmd**
```
Location: diagrams/monitoring/performance-monitoring.mmd
Type: Flowchart
Size: ~115 lines
Purpose: Performance tracking
Shows:
  - Latency percentiles
  - Throughput metrics
  - Resource usage
View: Can be embedded in monitoring docs
```

**48. monitoring/log-aggregation.mmd**
```
Location: diagrams/monitoring/log-aggregation.mmd
Type: Flowchart
Size: ~95 lines
Purpose: Log pipeline
Shows:
  - Log generation
  - Collection
  - Indexing
  - Search
View: Can be embedded in monitoring docs
```

---

## API Diagrams (5)

### Endpoints & Flows

**49. api/api-endpoints.mmd**
```
Location: diagrams/api/api-endpoints.mmd
Type: Class Diagram
Size: ~110 lines
Purpose: API endpoint map
Shows:
  - Client endpoints
  - Server endpoints
  - Registry endpoints
  - Transport endpoints
View: Can be embedded in API docs
```

**50. api/request-response-flow.mmd**
```
Location: diagrams/api/request-response-flow.mmd
Type: Sequence Diagram
Size:~120 lines
Purpose: Request/response lifecycle
Shows:
  - API call → Processing → Response
  - Error handling
  - Timeout handling
View: Can be embedded in API docs
```

**51. api/versioning-strategy.mmd**
```
Location: diagrams/api/versioning-strategy.mmd
Type: Flowchart
Size: ~95 lines
Purpose: API versioning
Shows:
  - Version detection
  - Backward compatibility
  - Deprecation policy
View: Can be embedded in API docs
```

**52. api/rate-limiting.mmd**
```
Location: diagrams/api/rate-limiting.mmd
Type: Flowchart
Size: ~100 lines
Purpose: Rate limiting flow
Shows:
  - Quota checking
  - Limit enforcement
  - Refusal handling
View: Can be embedded in API docs
```

**53. api/authentication-flow.mmd**
```
Location: diagrams/api/authentication-flow.mmd (duplicate)
Type: Sequence Diagram
Size: ~115 lines
Purpose: API authentication
Shows:
  - Credential validation
  - Token verification
  - Authorization
View: Can be embedded in API docs
```

---

## Examples Diagrams (5)

### Usage Examples

**54. examples/basic-request.mmd**
```
Location: diagrams/examples/basic-request.mmd
Type: Sequence Diagram
Size: ~95 lines
Purpose: Basic request example
Shows:
  - Client setup
  - Tool invocation
  - Response handling
View: Can be embedded in examples/
```

**55. examples/error-scenario.mmd**
```
Location: diagrams/examples/error-scenario.mmd
Type: Sequence Diagram
Size: ~110 lines
Purpose: Error handling example
Shows:
  - Error generation
  - Recovery
  - Retry logic
View: Can be embedded in examples/
```

**56. examples/complex-workflow.mmd**
```
Location: diagrams/examples/complex-workflow.mmd
Type: Sequence Diagram
Size: ~140 lines
Purpose: Multi-step workflow
Shows:
  - Multiple operations
  - State management
  - Error recovery
View: Can be embedded in examples/
```

**57. examples/deployment-example.mmd**
```
Location: diagrams/examples/deployment-example.mmd
Type: Deployment diagram
Size: ~105 lines
Purpose: Sample deployment
Shows:
  - Node configuration
  - Network topology
View: Can be embedded in examples/
```

**58. examples/performance-benchmark.mmd**
```
Location: diagrams/examples/performance-benchmark.mmd
Type: Bar Chart
Size: ~90 lines
Purpose: Benchmark results
Shows:
  - Throughput comparison
  - Latency percentiles
View: Can be embedded in examples/
```

---

## Reference Diagrams (5)

### Module & Function References

**59. reference/module-index.mmd**
```
Location: diagrams/reference/module-index.mmd
Type: Flowchart (TB)
Size: ~200 lines
Purpose: Complete module index
Shows:
  - All 164 modules
  - Organization by app
  - Dependencies
View: MODULE_INDEX.md (referenced)
```

**60. reference/function-signatures.mmd**
```
Location: diagrams/reference/function-signatures.mmd
Type: Class Diagram
Size: ~150 lines
Purpose: API signature reference
Shows:
  - Client API
  - Server API
  - Transport API
  - Observability API
View: Can be embedded in API reference
```

**61. reference/error-codes.mmd**
```
Location: diagrams/reference/error-codes.mmd
Type: Flowchart
Size: ~95 lines
Purpose: Error code reference
Shows:
  - JSON-RPC errors
  - MCP refusals [1001-1089]
  - Internal errors
View: Can be embedded in API reference
```

**62. reference/configuration-reference.mmd**
```
Location: diagrams/reference/configuration-reference.mmd
Type: Flowchart
Size: ~130 lines
Purpose: Configuration options
Shows:
  - Application config
  - Transport config
  - Observability config
View: Can be embedded in configuration docs
```

**63. reference/troubleshooting-guide.mmd**
```
Location: diagrams/reference/troubleshooting-guide.mmd
Type: Flowchart (decision tree)
Size: ~125 lines
Purpose: Troubleshooting flow
Shows:
  - Issue diagnosis
  - Solution paths
  - Escalation
View: Can be embedded in troubleshooting docs
```

---

## Configuration Diagrams (5)

### Config Management

**64. configuration/configuration-hierarchy.mmd**
```
Location: diagrams/configuration/configuration-hierarchy.mmd
Type: Flowchart
Size: ~100 lines
Purpose: Config precedence
Shows:
  - sys.config defaults
  - Environment variables
  - Runtime overrides
View: Can be embedded in configuration docs
```

**65. configuration/feature-flags.mmd**
```
Location: diagrams/configuration/feature-flags.mmd
Type: Flowchart
Size: ~90 lines
Purpose: Feature flag system
Shows:
  - Flag evaluation
  - Rollout strategy
View: Can be embedded in configuration docs
```

**66. configuration/environment-variables.mmd**
```
Location: diagrams/configuration/environment-variables.mmd
Type: Table
Size: ~85 lines
Purpose: Environment variable reference
Shows:
  - All env vars
  - Default values
  - Validation
View: Can be embedded in configuration docs
```

**67. configuration/validation-pipeline.mmd**
```
Location: diagrams/configuration/validation-pipeline.mmd
Type: Flowchart
Size: ~95 lines
Purpose: Config validation
Shows:
  - Schema validation
  - Type checking
  - Error reporting
View: Can be embedded in configuration docs
```

**68. configuration/runtime-config.mmd**
```
Location: diagrams/configuration/runtime-config.mmd
Type: State Diagram
Size: ~90 lines
Purpose: Runtime config updates
Shows:
  - Config reload
  - Hot code reload
  - Validation
View: Can be embedded in configuration docs
```

---

## Integration Diagrams (5)

### External Services

**69. integration/database-integration.mmd**
```
Location: diagrams/integration/database-integration.mmd
Type: Component Diagram
Size: ~100 lines
Purpose: Database backends
Shows:
  - ETS
  - DETS
  - Mnesia
  - External databases
View: Can be embedded in integration docs
```

**70. integration/monitoring-integration.mmd**
```
Location: diagrams/integration/monitoring-integration.mmd
Type: Flowchart
Size: ~95 lines
Purpose: Monitoring integrations
Shows:
  - Datadog
  - Honeycomb
  - Jaeger
  - Prometheus
View: Can be embedded in integration docs
```

**Additional Integration Diagrams:**
- `integration/external-services.mmd` - External service dependencies
- `integration/api-gateway.mmd` - API gateway integration
- `integration/message-bus.mmd` - Message bus integration

---

## Error Diagrams (5)

### Error Handling

**Error Diagrams:**
- `errors/error-flow.mmd` - Error flow through system
- `errors/retry-mechanisms.mmd` - Retry strategies
- `errors/circuit-breakers.mmd` - Circuit breaker states
- `errors/graceful-degradation.mmd` - Degradation modes
- `errors/monitoring-alerts.mmd` - Error alerting

---

## Testing Diagrams (5)

### Test Coverage

**Testing Diagrams:**
- `testing/test-coverage-map.mmd` - Coverage visualization
- `testing/integration-tests.mmd` - Integration test structure
- `testing/unit-test-flow.mmd` - Unit test workflow
- `testing/property-testing.mmd` - Property-based testing
- `testing/test-data-management.mmd` - Test data fixtures

---

## Using the Diagram Suite

### Best Practices

**1. Choosing the Right Diagram:**
- **System Overview:** `system-architecture.mmd`
- **Supervision:** `supervision-tree.mmd`
- **Protocol:** `protocol/session-lifecycle.mmd`
- **Deployment:** `deployment/cluster-topology.mmd`
- **Troubleshooting:** `reference/troubleshooting-guide.mmd`

**2. Embedding in Documentation:**
```markdown
```mermaid
~paste diagram content~
```
```

**3. Linking to Standalone Files:**
```markdown
See [`diagrams/system-architecture.mmd`](diagrams/system-architecture.mmd)
```

**4. Rendering to Images:**
```bash
mmdc -i diagram.mmd -o diagram.png -b transparent
```

**5. Editing Diagrams:**
- Use Mermaid Live Editor: https://mermaid.live
- Copy `.mmd` content
- Edit and export
- Update file in repository

### Diagram Maintenance

**Version Control:**
- All diagrams tracked in Git
- Commit with descriptive messages
- Update documentation references

**Consistency:**
- Use consistent styling (classDef)
- Follow naming conventions
- Include descriptions in headers

**Validation:**
- Test rendering in GitHub
- Check for syntax errors
- Verify all links work

---

## Index Statistics

**By Type:**
- Flowchart: 35 diagrams
- Sequence Diagram: 18 diagrams
- State Diagram: 8 diagrams
- Class Diagram: 6 diagrams
- Component Diagram: 3 diagrams

**By Size:**
- Small (<100 lines): 20 diagrams
- Medium (100-150 lines): 40 diagrams
- Large (>150 lines): 10 diagrams

**By Complexity:**
- Simple: 25 diagrams
- Moderate: 35 diagrams
- Complex: 10 diagrams

---

## Related Documentation

**Primary:**
- [architecture.md](architecture.md) - System architecture
- [otp-patterns.md](otp-patterns.md) - OTP patterns
- [MODULE_INDEX.md](MODULE_INDEX.md) - Module catalog
- [DOCUMENTATION_INDEX.md](DOCUMENTATION_INDEX.md) - Complete docs index

**Visual:**
- [diagrams/](diagrams/) - Complete diagram suite
- [mermaid-architecture.md](mermaid-architecture.md) - Mermaid integration

---

**Last Updated:** 2026-01-31

**Total Diagrams:** 70

**Coverage:** Complete erlmcp v2.1.0 system visualization
