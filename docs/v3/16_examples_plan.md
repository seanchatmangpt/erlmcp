# erlmcp v3 Examples Plan

**Status**: Planning Phase
**Version**: 3.0.0
**Date**: 2026-01-31
**Owner**: Erlang OTP Developer Agent

## Executive Summary

erlmcp v3 requires comprehensive production-ready examples covering WebSocket transport, SSE with resumption, multi-node clustering, security (TLS/mTLS/auth), and production deployment patterns. This plan defines 12 new example categories to complement the existing 38+ examples.

## Current State Analysis

### Existing Examples (38+)

| Category | Examples | Status | Coverage |
|----------|----------|--------|----------|
| Basic Transport | simple, calculator | ✅ Complete | STDIO, TCP |
| Advanced Features | weather, gcp_simulator, mcp_complete | ✅ Complete | Tools, resources, prompts |
| Observability | dashboard_demo, metrics_demo, trace_demo | ✅ Complete | OTEL integration |
| TCPS Integration | andon, kaizen, tcps_*_demo | ✅ Complete | Full TCPS suite |
| Session Persistence | session_persistence | ✅ Complete | ETS, DETS, Mnesia |
| Resource Subscriptions | resource_subscription | ✅ Complete | List change notifications |
| Secrets Management | secrets_management | ✅ Complete | Vault, local encrypted |
| Chaos Engineering | chaos_engineering_examples | ✅ Complete | Failure injection |

### Missing Examples (Critical Gaps)

| Category | Priority | Complexity | Status |
|----------|----------|------------|--------|
| WebSocket Transport | P0 | High | ❌ Missing |
| SSE with Resumption | P0 | High | ❌ Missing |
| Multi-Node Cluster | P0 | Very High | ❌ Missing |
| TLS/mTLS Security | P0 | High | ❌ Missing |
| Authentication | P0 | Medium | ❌ Missing |
| Production Deployment | P0 | Very High | ❌ Missing |
| Load Balancing | P1 | High | ❌ Missing |
| Circuit Breakers | P1 | Medium | ❌ Missing |
| Rate Limiting | P1 | Medium | ❌ Missing |
| Connection Pooling | P2 | Low | ⚠️ Partial (poolboy exists) |
| Health Monitoring | P2 | Low | ⚠️ Partial (health demos exist) |
| Graceful Shutdown | P2 | Medium | ❌ Missing |

## Example Categories Specification

### 1. WebSocket Transport Example (`websocket/`)

**Objective**: Demonstrate bidirectional WebSocket MCP server with real-time communication.

**Features**:
- WebSocket transport via `erlmcp_transport_ws`
- Real-time tool invocation with streaming responses
- Connection lifecycle management (connect, ping/pong, disconnect)
- Backpressure handling and flow control
- Fragment reassembly for large messages
- UTF-8 validation and error handling
- Statistics tracking (messages sent/received, bytes transferred)

**Modules**:
```
websocket/
├── websocket_server.erl          % WebSocket MCP server
├── websocket_client.erl          % WebSocket client for testing
├── websocket_demo.erl            % Interactive demonstration
├── websocket_tests.erl           % EUnit test suite
├── Makefile
├── rebar.config
└── README.md
```

**Requirements**:
1. Cowboy 2.10+ dependency
2. WebSocket endpoint: `ws://localhost:8080/mcp/ws`
3. Maximum message size: 16MB (configurable)
4. Ping interval: 30s
5. Idle timeout: 5 minutes
6. Backpressure buffer: 100KB (configurable)

**Testing Strategy**:
- Unit tests for frame handling (ping, pong, close, text, binary)
- Integration tests with real WebSocket connections
- Backpressure simulation tests
- Fragment reassembly tests
- UTF-8 validation tests
- Large message handling (16MB boundary)
- Connection recovery tests

**Use Cases**:
- Real-time AI assistant interfaces
- Live collaboration tools
- Streaming resource updates
- Browser-based MCP clients

---

### 2. SSE with Resumption Example (`sse_resumption/`)

**Objective**: Demonstrate Server-Sent Events with automatic resumption and last-event-id tracking.

**Features**:
- SSE transport via `erlmcp_transport_sse`
- Automatic reconnection with exponential backoff
- Last-event-id tracking for message resumption
- Event replay from event store
- POST channel for client → server messages
- Keep-alive ping comments (30s interval)
- Event ID generation and tracking

**Modules**:
```
sse_resumption/
├── sse_server.erl                % SSE MCP server
├── sse_event_store.erl           % Event replay buffer (ETS)
├── sse_client.erl                % SSE client with resumption
├── sse_demo.erl                  % Interactive demonstration
├── sse_resumption_tests.erl      % Resumption logic tests
├── Makefile
├── rebar.config
└── README.md
```

**Requirements**:
1. Cowboy 2.10+ for SSE handler
2. Event store: ETS table with `bag` semantics
3. Event retention: 1000 events (configurable)
4. Reconnect backoff: 1s → 2s → 4s → 8s (max)
5. POST endpoint: `/mcp/sse` for client messages
6. GET endpoint: `/mcp/sse` for SSE stream

**Testing Strategy**:
- Connection drop and resumption tests
- Last-event-id validation tests
- Event replay correctness tests
- Backoff algorithm tests
- Event store overflow tests (1000 event limit)
- Simultaneous connection tests (multiple clients)

**Use Cases**:
- Mobile apps with unstable connections
- Browser clients needing auto-reconnect
- Event-driven architectures
- Notification systems

---

### 3. Multi-Node Cluster Example (`cluster/`)

**Objective**: Demonstrate distributed erlmcp deployment with Mnesia session replication.

**Features**:
- Multi-node clustering via `erlmcp_registry_dist`
- Mnesia distributed session storage
- Automatic failover on node failure
- Split-brain detection and resolution
- Global service discovery via gproc
- Distributed tool registration
- Load balancing across nodes
- Node health monitoring

**Modules**:
```
cluster/
├── cluster_bootstrap.erl         % Node initialization and joining
├── cluster_manager.erl           % Cluster state management
├── cluster_server.erl            % Distributed MCP server
├── cluster_client.erl            % Cluster-aware client
├── cluster_monitor.erl           % Health monitoring dashboard
├── cluster_failover_tests.erl    % Failover scenario tests
├── cluster_demo.erl              % Interactive 3-node demo
├── Makefile
├── rebar.config
├── sys.config                    % Multi-node configuration
├── vm.args                       % Distributed Erlang flags
└── README.md
```

**Requirements**:
1. Mnesia for distributed sessions
2. gproc for global registry
3. 3-node cluster minimum (dev), 5-node (prod)
4. Node names: `erlmcp1@127.0.0.1`, `erlmcp2@127.0.0.1`, `erlmcp3@127.0.0.1`
5. Cookie-based authentication: `erlmcp_cluster`
6. Disc copies: 2 nodes (RAID-1 equivalent)
7. RAM copies: 1 node (cache)

**Testing Strategy**:
- Node join and leave tests
- Session replication tests (create on node1, read from node2)
- Failover tests (kill node1, verify failover to node2)
- Split-brain recovery tests (network partition simulation)
- Load balancing tests (distribute requests across nodes)
- Tool registration tests (global availability)

**Use Cases**:
- High-availability deployments
- Horizontal scaling
- Geographic distribution
- Disaster recovery

---

### 4. TLS/mTLS Security Example (`security_tls/`)

**Objective**: Demonstrate TLS encryption and mutual TLS authentication.

**Features**:
- TLS 1.3 encryption for all transports
- mTLS (client certificate validation)
- Certificate generation and management
- Certificate revocation (CRL/OCSP stubs)
- Cipher suite hardening
- Perfect Forward Secrecy (PFS)
- Certificate rotation
- Secure configuration validation

**Modules**:
```
security_tls/
├── tls_server.erl                % TLS-enabled server
├── tls_client.erl                % TLS-enabled client
├── tls_certificate_manager.erl   % Cert generation and rotation
├── tls_validator.erl             % Certificate validation
├── tls_demo.erl                  % Interactive demonstration
├── tls_security_tests.erl        % Security test suite
├── certs/                        % Certificate storage
│   ├── generate.sh              % Cert generation script
│   ├── ca.crt
│   ├── ca.key
│   ├── server.crt
│   ├── server.key
│   ├── client.crt
│   └── client.key
├── Makefile
├── rebar.config
├── tls.config                    % TLS configuration
└── README.md
```

**Requirements**:
1. TLS 1.3 only (no TLS 1.2, 1.1, 1.0)
2. Cipher suites: TLS_AES_128_GCM_SHA256, TLS_AES_256_GCM_SHA384
3. mTLS: Client certificates required
4. CA: Self-signed for development, Let's Encrypt for production
5. Certificate rotation: 90-day validity, 30-day warning
6. Revocation: CRL checking (OCSP stub for demo)

**Testing Strategy**:
- TLS handshake tests (success, failure)
- Certificate validation tests (valid, expired, revoked)
- Cipher suite tests (allowed, rejected)
- mTLS authentication tests (valid cert, invalid cert, no cert)
- Certificate rotation tests (old cert, new cert, transition period)
- Forward secrecy tests (session key uniqueness)

**Use Cases**:
- Production deployments over public internet
- Zero-trust architectures
- Regulatory compliance (HIPAA, PCI-DSS, GDPR)
- Enterprise security requirements

---

### 5. Authentication & Authorization Example (`security_auth/`)

**Objective**: Demonstrate JWT-based authentication and role-based authorization.

**Features**:
- JWT token generation and validation (jose library)
- Role-based access control (RBAC)
- API key authentication
- Rate limiting per user
- Session token refresh
- Password hashing (bcrypt)
- Multi-factor authentication (TOTP stub)
- Audit logging for auth events

**Modules**:
```
security_auth/
├── auth_server.erl               % Auth-enabled MCP server
├── auth_client.erl               % Auth-enabled client
├── auth_jwt.erl                  % JWT generation/validation
├── auth_rbac.erl                 % Role-based access control
├── auth_rate_limiter.erl         % Per-user rate limiting
├── auth_password.erl             % Password hashing (bcrypt)
├── auth_audit.erl                % Auth event logging
├── auth_demo.erl                 % Interactive demonstration
├── auth_tests.erl                % Auth test suite
├── Makefile
├── rebar.config
└── README.md
```

**Requirements**:
1. JWT: RS256 algorithm (RSA signature)
2. Token expiry: 1 hour access, 30 days refresh
3. Roles: admin, user, readonly (3 roles)
4. Rate limits: 100 req/min for admin, 10 req/min for user
5. Password: bcrypt with cost factor 12
6. Secret rotation: 90 days

**Testing Strategy**:
- JWT generation and validation tests
- Role-based authorization tests (admin → allow, user → deny)
- Rate limiting tests (threshold enforcement, burst handling)
- Password hashing tests (bcrypt verification, timing attack resistance)
- Token refresh tests (expired access, valid refresh)
- Audit logging tests (all auth events logged)

**Use Cases**:
- Multi-tenant SaaS deployments
- Enterprise user management
- API security
- Compliance audit trails

---

### 6. Production Deployment Example (`production/`)

**Objective**: Demonstrate production-ready deployment with Docker, Kubernetes, and observability.

**Features**:
- Docker multi-stage builds
- Kubernetes deployment manifests (Deployment, Service, ConfigMap, Secret)
- Horizontal Pod Autoscaling (HPA)
- Rolling updates and rollback strategies
- Health checks (readiness, liveness, startup probes)
- Resource limits and requests (CPU, memory)
- Pod Disruption Budgets (PDB)
- OpenTelemetry tracing (Datadog, Jaeger)
- Prometheus metrics scraping
- Log aggregation (EFK stack stub)

**Modules**:
```
production/
├── docker/
│   ├── Dockerfile               % Multi-stage build
│   ├── docker-compose.yml       % Local development stack
│   └── .dockerignore
├── kubernetes/
│   ├── deployment.yaml          % Deployment manifest
│   ├── service.yaml             % Service manifest
│   ├── configmap.yaml           % Configuration
│   ├── secret.yaml              % Secrets (TLS, JWT)
│   ├── hpa.yaml                 % Horizontal Pod Autoscaler
│   ├── pdb.yaml                 % Pod Disruption Budget
│   └── networkpolicy.yaml       % Network security
├── monitoring/
│   ├── prometheus.yaml          % Prometheus scrape config
│   ├── grafana-dashboard.json   % Grafana dashboard
│   └── datadog-dashboard.json   % Datadog dashboard
├── scripts/
│   ├── deploy.sh                % Deployment script
│   ├── rollback.sh              % Rollback script
│   ├── health-check.sh          % Health check endpoint
│   └── backup-restore.sh        % Backup/restore scripts
├── Makefile
├── rebar.config
└── README.md
```

**Requirements**:
1. Docker: Alpine-based image (~100MB compressed)
2. Kubernetes: 3 replica minimum, 10 replica maximum (HPA)
3. Resources: 256Mi request, 512Mi limit, 0.5 CPU request, 1 CPU limit
4. Health checks: HTTP `/health` endpoint, 5s interval, 3 failures → restart
5. Observability: OTEL traces to Datadog, Prometheus metrics
6. Rolling update: 25% surge, 25% unavailable
7. Graceful shutdown: 30s terminationGracePeriodSeconds

**Testing Strategy**:
- Docker build tests (multi-stage optimization)
- Kubernetes deployment tests (kubectl apply, rollout status)
- HPA scaling tests (load simulation, pod count verification)
- Rolling update tests (zero downtime verification)
- Health check tests (probe success/failure scenarios)
- Resource limit tests (OOM, CPU throttling)
- Monitoring integration tests (metrics export, trace visibility)

**Use Cases**:
- Cloud-native deployments (AWS EKS, GCP GKE, Azure AKS)
- Production SaaS platforms
- High-availability services
- Multi-region deployments

---

### 7. Load Balancing Example (`load_balancing/`)

**Objective**: Demonstrate client-side and server-side load balancing strategies.

**Features**:
- Client-side round-robin load balancing
- Server-side load balancing (Nginx, HAProxy stubs)
- Least-connections algorithm
- Weighted load balancing (node capacity)
- Health check-based routing
- Sticky session support (session affinity)
- Circuit breaker integration
- Connection pooling

**Modules**:
```
load_balancing/
├── lb_client.erl                 % Load-balancing client
├── lb_server.erl                 % Backend server (multiple instances)
├── lb_round_robin.erl            % Round-robin strategy
├── lb_least_conn.erl             % Least-connections strategy
├── lb_weighted.erl               % Weighted strategy
├── lb_health_check.erl           % Health-based routing
├── lb_demo.erl                   % Interactive demonstration
├── lb_tests.erl                  % Load balancing tests
├── Makefile
├── rebar.config
└── README.md
```

**Requirements**:
1. Backend nodes: 3 instances (ports 8080, 8081, 8082)
2. Algorithms: round-robin, least-conn, weighted
3. Health checks: HTTP `/health` every 5s
4. Sticky sessions: cookie-based affinity
5. Circuit breaker: 5 consecutive failures → open circuit

**Testing Strategy**:
- Round-robin distribution tests (uniform verification)
- Least-conn tests (load verification under imbalance)
- Weighted tests (capacity-based distribution)
- Health check tests (failed node exclusion)
- Sticky session tests (affinity verification)
- Circuit breaker tests (failure threshold, recovery)

**Use Cases**:
- High-traffic deployments
- Multi-instance scaling
- Capacity optimization

---

### 8. Circuit Breaker Example (`circuit_breaker/`)

**Objective**: Demonstrate circuit breaker pattern for fault tolerance.

**Features**:
- Circuit breaker state machine (closed, open, half-open)
- Failure threshold configuration
- Timeout and retry strategies
- Fallback mechanisms
- Automatic recovery
- Metrics and monitoring integration
- Distributed circuit breaker coordination

**Modules**:
```
circuit_breaker/
├── cb_server.erl                 % Circuit breaker demo server
├── cb_client.erl                 % Circuit breaker client
├── cb_state_machine.erl          % State machine implementation
├── cb_fallback.erl               % Fallback handler
├── cb_monitor.erl                % Circuit breaker metrics
├── cb_demo.erl                   % Interactive demonstration
├── cb_tests.erl                  % Circuit breaker tests
├── Makefile
├── rebar.config
└── README.md
```

**Requirements**:
1. Failure threshold: 5 consecutive failures
2. Timeout: 30s (half-open → closed/open)
3. Half-open max attempts: 3 requests
4. Fallback: cached response or error message

**Testing Strategy**:
- State transition tests (closed → open → half-open → closed)
- Failure threshold tests (exact failure count)
- Recovery tests (successful half-open transition)
- Fallback tests (fallback invocation verification)
- Metrics tests (state change events)

**Use Cases**:
- External API integration
- Microservice communication
- Database connection resilience

---

### 9. Rate Limiting Example (`rate_limiting/`)

**Objective**: Demonstrate rate limiting algorithms and enforcement.

**Features**:
- Token bucket algorithm
- Sliding window log
- Fixed window counter
- Distributed rate limiting (Redis stub)
- Per-user and per-IP limits
- Burst allowance
- Rate limit exceeded responses
- Whitelist and blacklist support

**Modules**:
```
rate_limiting/
├── rl_server.erl                 % Rate-limited server
├── rl_client.erl                 % Test client
├── rl_token_bucket.erl           % Token bucket implementation
├── rl_sliding_window.erl         % Sliding window implementation
├── rl_fixed_window.erl           % Fixed window implementation
├── rl_middleware.erl             % Rate limit middleware
├── rl_demo.erl                   % Interactive demonstration
├── rl_tests.erl                  % Rate limiting tests
├── Makefile
├── rebar.config
└── README.md
```

**Requirements**:
1. Token bucket: 10 tokens/sec, burst 50
2. Sliding window: 100 req/min
3. Fixed window: 1000 req/hour
4. Response: HTTP 429 (Too Many Requests) with Retry-After header

**Testing Strategy**:
- Token bucket tests (refill rate, burst capacity)
- Sliding window tests (boundary correctness)
- Fixed window tests (reset behavior)
- Distributed tests (Redis coordination)
- Burst tests (peak capacity verification)

**Use Cases**:
- API protection
- DoS mitigation
- Fair resource allocation
- Cost control

---

### 10. Advanced Connection Pooling Example (`connection_pool/`)

**Objective**: Demonstrate advanced connection pooling beyond poolboy basics.

**Features**:
- Dynamic pool sizing (scale up/down)
- Pool warming strategies
- Connection reuse and eviction
- Pool health monitoring
- Overflow strategy (queue vs. reject)
- Timeout configurations
- Pool metrics (utilization, wait time)

**Modules**:
```
connection_pool/
├── pool_server.erl               % Pooled server
├── pool_manager.erl              % Advanced pool manager
├── pool_monitor.erl              % Pool health monitor
├── pool_metrics.erl              % Pool performance metrics
├── pool_demo.erl                 % Interactive demonstration
├── pool_tests.erl                % Pool tests
├── Makefile
├── rebar.config
└── README.md
```

**Requirements**:
1. Pool size: 5 min, 20 max
2. Overflow: 10 (queue), reject after
3. Idle timeout: 60s (evict)
4. Warm-up: 2 connections on startup

**Testing Strategy**:
- Scale up/down tests (load simulation)
- Eviction tests (idle timeout verification)
- Overflow tests (queue depth, rejection threshold)
- Health tests (stale connection detection)
- Metrics tests (utilization calculation)

**Use Cases**:
- Database connection pools
- HTTP client pools
- TCP connection pools

---

### 11. Health Monitoring Example (`health_monitoring/`)

**Objective**: Demonstrate comprehensive health monitoring and alerting.

**Features**:
- HTTP health check endpoints (`/health`, `/ready`, `/alive`)
- Component health checks (database, cache, external APIs)
- Health score calculation
- Degraded mode operation
- Alert triggering (PagerDuty, Slack stubs)
- Health history tracking
- Dependency health propagation

**Modules**:
```
health_monitoring/
├── hm_server.erl                 % Health-monitored server
├── hm_check.erl                  % Health check implementation
├── hm_http_handler.erl           % HTTP health endpoint
├── hm_alert.erl                  % Alert notification
├── hm_history.erl                % Health history store
├── hm_demo.erl                   % Interactive demonstration
├── hm_tests.erl                  % Health monitoring tests
├── Makefile
├── rebar.config
└── README.md
```

**Requirements**:
1. Endpoints: `/health` (aggregate), `/ready` (dependency checks), `/alive` (liveness)
2. Checks: database (SELECT 1), cache (PING), external API (heartbeat)
3. Alert threshold: 3 consecutive failures
4. Health score: 100 (healthy), 50 (degraded), 0 (unhealthy)

**Testing Strategy**:
- Endpoint response tests (healthy, degraded, unhealthy)
- Component check tests (dependency failure scenarios)
- Alert tests (threshold triggering, notification delivery)
- History tests (health state tracking)
- Score calculation tests (weighted aggregation)

**Use Cases**:
- Kubernetes probes
- Load balancer health checks
- Operational monitoring
- SLA compliance

---

### 12. Graceful Shutdown Example (`graceful_shutdown/`)

**Objective**: Demonstrate graceful shutdown with connection draining.

**Features**:
- SIGTERM handling
- Connection draining (wait for in-flight requests)
- Session persistence before shutdown
- Timeout enforcement (force kill after timeout)
- Shutdown hooks and callbacks
- Rolling update coordination
- Zero-downtime deployment verification

**Modules**:
```
graceful_shutdown/
├── gs_server.erl                 % Graceful shutdown server
├── gs_connection_drainer.erl     % Connection draining logic
├── gs_shutdown_coordinator.erl   % Shutdown orchestration
├── gs_hooks.erl                  % Shutdown callback manager
├── gs_demo.erl                   % Interactive demonstration
├── gs_tests.erl                  % Graceful shutdown tests
├── Makefile
├── rebar.config
└── README.md
```

**Requirements**:
1. SIGTERM handler: Initiate graceful shutdown
2. Drain timeout: 30s (wait for connections)
3. Force kill: 35s (hard stop after timeout)
4. Session persist: Flush all pending sessions
5. Rolling update: New instances start before old stop

**Testing Strategy**:
- SIGTERM handling tests (signal receipt, shutdown initiation)
- Drain tests (connection wait, in-flight request completion)
- Timeout tests (force kill verification)
- Session persist tests (data flush before exit)
- Rolling update tests (zero downtime verification)

**Use Cases**:
- Production deployments
- Kubernetes rolling updates
- Zero-downtime migrations
- Scheduled maintenance

## Example Testing Strategy

### Universal Testing Requirements

**All examples MUST include**:

1. **EUnit Test Suite**: Minimum 80% code coverage
   - Unit tests for all modules
   - Property-based tests (Proper) for stateful logic
   - Mock-free design (Chicago School TDD)

2. **Common Test Suite**: Integration testing
   - End-to-end workflows
   - Transport-level tests
   - Multi-process scenarios
   - Failure injection tests

3. **Performance Benchmarks**: (if applicable)
   - Throughput tests (requests/sec)
   - Latency tests (p50, p95, p99)
   - Resource usage (memory, CPU)

4. **Chaos Tests**: (for distributed examples)
   - Process kills
   - Network partitions
   - Resource exhaustion
   - Recovery validation

### Test Execution Standards

```bash
# Compile
TERM=dumb rebar3 compile

# Unit tests
rebar3 eunit --module=example_tests

# Integration tests
rebar3 ct --suite=example_SUITE

# Coverage report
rebar3 cover --verbose
```

### Quality Gates

- ✅ All tests passing (0 failures)
- ✅ Coverage ≥ 80%
- ✅ Dialyzer warnings = 0
- ✅ Xref checks passing
- ✅ Format verification passing

## Documentation Standards

### README.md Structure (Required for all examples)

```markdown
# Example Name

## Overview
Brief description of what this example demonstrates.

## Features
- Feature 1
- Feature 2
- Feature 3

## Architecture
Diagram or description of component architecture.

## Prerequisites
- Dependency 1
- Dependency 2

## Quick Start
### Step 1: Build
```bash
make compile
```

### Step 2: Run
```bash
make run
```

### Step 3: Test
```bash
make test
```

## Configuration
Description of configurable parameters.

## Usage Examples
Code snippets showing common usage patterns.

## Testing
Description of test suites and how to run them.

## Troubleshooting
Common issues and solutions.

## References
Links to related documentation.
```

## Implementation Priorities

### Phase 1: Core Transport Examples (Weeks 1-2)
1. WebSocket Transport (P0)
2. SSE with Resumption (P0)

### Phase 2: Security Examples (Weeks 3-4)
3. TLS/mTLS Security (P0)
4. Authentication & Authorization (P0)

### Phase 3: Distributed Systems (Weeks 5-6)
5. Multi-Node Cluster (P0)
6. Production Deployment (P0)

### Phase 4: Advanced Patterns (Weeks 7-8)
7. Load Balancing (P1)
8. Circuit Breaker (P1)
9. Rate Limiting (P1)

### Phase 5: Operational Excellence (Weeks 9-10)
10. Advanced Connection Pooling (P2)
11. Health Monitoring (P2)
12. Graceful Shutdown (P2)

## Resource Requirements

### Development Resources
- **Erlang/OTP Developer**: 160 hours (4 weeks full-time)
- **Test Engineer**: 80 hours (2 weeks full-time)
- **Documentation**: 40 hours (1 week full-time)

### Infrastructure Requirements
- **Development Machine**: 4 CPU, 8GB RAM
- **Test Environment**: Docker, Kubernetes (kind/minikube)
- **Monitoring**: Datadog account (free tier) or Prometheus/Grafana
- **Load Testing**: wrk, hey, or Apache Bench

### External Dependencies
- Cowboy 2.10+ (HTTP/WebSocket/SSE)
- jose 1.11+ (JWT)
- bcrypt (password hashing)
- ranch 2.1+ (TCP acceptor pool)
- poolboy 1.5.2+ (connection pooling)

## Success Criteria

### Quantitative Metrics
- ✅ 12 new example categories delivered
- ✅ 60+ new Erlang modules
- ✅ 100+ test cases (EUnit + Common Test)
- ✅ 80%+ code coverage across all examples
- ✅ 12 README.md documents with diagrams
- ✅ 5+ Docker images (production, dev, monitoring)
- ✅ 10+ Kubernetes manifests

### Qualitative Metrics
- ✅ All examples run successfully on macOS, Linux
- ✅ All examples include comprehensive error handling
- ✅ All examples include observability (metrics, traces)
- ✅ All examples include security best practices
- ✅ All examples include production deployment guides
- ✅ All examples pass Chicago School TDD (no mocks)

## Risk Assessment

### Technical Risks
| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| WebSocket complexity | Medium | Medium | Leverage existing erlmcp_transport_ws |
| Multi-node cluster setup | High | High | Start with 3-node local cluster |
| TLS certificate management | Medium | Low | Use self-signed for development |
| Kubernetes deployment complexity | Medium | Medium | Provide detailed step-by-step guides |

### Schedule Risks
| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Underestimated testing effort | High | High | Allocate 50% more time for testing |
| Documentation scope creep | Medium | Low | Strict adherence to README template |
| Dependency version conflicts | Low | Medium | Pin dependency versions in rebar.config |

## Maintenance Plan

### Example Updates
- **Monthly**: Review and update examples for breaking changes
- **Quarterly**: Add new examples based on user feedback
- **Annually**: Major version refresh (v4, v5, etc.)

### Testing Maintenance
- **Weekly**: Run full test suite in CI/CD
- **Monthly**: Update test cases for edge cases discovered
- **Quarterly**: Performance regression testing

### Documentation Maintenance
- **Weekly**: Fix documentation typos and clarifications
- **Monthly**: Add new usage patterns discovered
- **Quarterly**: Update architecture diagrams

## References

### Internal Documentation
- `docs/architecture.md` - System architecture
- `docs/otp-patterns.md` - OTP patterns
- `docs/protocol.md` - MCP protocol specification
- `examples/README.md` - Existing examples

### External Documentation
- Cowboy WebSocket Guide: https://ninenines.eu/docs/en/cowboy/2.10/guide/websocket/
- Server-Sent Events: https://html.spec.whatwg.org/multipage/server-sent-events.html
- Mnesia User's Guide: https://www.erlang.org/doc/apps/mnesia/
- TLS Configuration: https://ninenines.eu/docs/en/ranch/2.1/guide/ssl/

### Related Projects
- `erlmcp_transport_ws.erl` - WebSocket transport implementation
- `erlmcp_transport_sse.erl` - SSE transport implementation
- `erlmcp_auth_mtls.erl` - mTLS authentication
- `erlmcp_registry_dist.erl` - Distributed registry

## Appendix

### Example Module Template

```erlang
-module(example_server).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    % State fields
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

### Makefile Template

```makefile
ROOT := ../..
BUILD := $(ROOT)/_build/default/lib
LIB_DIRS := $(wildcard $(BUILD)/*/ebin)
PA_FLAGS := $(foreach dir,$(LIB_DIRS),-pa $(dir))
ERLC_OPTS := +debug_info -I$(ROOT)/include $(PA_FLAGS)

.PHONY: all compile run shell test clean

all: compile

compile:
    erlc $(ERLC_OPTS) *.erl

run: compile
    erl $(PA_FLAGS) -noshell -s example_server start -s init stop

shell: compile
    erl $(PA_FLAGS)

test: compile
    erl $(PA_FLAGS) -noshell -s example_tests run -s init stop

clean:
    rm -f *.beam erl_crash.dump
```

---

**Document Version**: 1.0
**Last Updated**: 2026-01-31
**Next Review**: 2026-02-28
