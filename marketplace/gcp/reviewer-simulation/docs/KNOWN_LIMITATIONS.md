# Known Limitations for erlmcp GCP Marketplace Deployment

**Version**: 1.0.0
**Date**: 2026-02-02
**Purpose**: Document known limitations to prevent inappropriate rejection wording

---

## Overview

This document outlines the known limitations of the erlmcp deployment on Google Cloud Marketplace. These limitations are **accepted design constraints** rather than defects. Reviewers should evaluate the deployment against these documented constraints.

### Why This Document Exists

Google Cloud Marketplace reviewers may identify features as "missing" when they are:
1. Outside the scope of MCP protocol implementation
2. Intentionally excluded for security/architectural reasons
3. Planned for future releases
4. Not applicable to the use case

This document prevents inappropriate rejection by documenting these decisions upfront.

---

## Architectural Limitations

### A1. Erlang/OTP 28.3.1+ Minimum Requirement

**Limitation**: erlmcp requires Erlang/OTP 28.3.1 or later.

**Reason**: erlmcp v3.0 exclusively uses OTP 28+ native features:
- Native JSON module (2-3x faster than jsx)
- Priority messages (EEP 76) for sub-ms health checks
- O(1) process iteration for monitoring

**Impact**: Users cannot deploy on OTP 25-27. Approximately 1,358 lines of backward compatibility code were removed.

**Documentation Reference**: README.md section "Version 3.0: OTP 28.3.1+ Required"

**Not a Bug**: This is an intentional breaking change to leverage modern OTP features.

---

### A2. STDIO Transport for MCP Compliance

**Limitation**: STDIO transport does NOT support the following features:
- Multiplexing (multiple concurrent requests over same STDIO)
- Bidirectional streaming (server-to-client notifications limited)
- Connection pooling (single client per STDIO stream)

**Reason**: The MCP specification requires newline-delimited JSON-RPC over STDIO. Advanced features require HTTP/SSE/WebSocket transports.

**Impact**: Clients requiring advanced features must use HTTP/SSE/WebSocket transports instead.

**Documentation Reference**: docs/TRANSPORTS.md section "STDIO Transport"

**Not a Bug**: STDIO transport is spec-compliant. Feature-rich transports are available.

---

### A3. gproc Registry is Node-Local

**Limitation**: The gproc registry does not automatically distribute across nodes.

**Reason**: gproc is designed for local node registration. Distributed routing requires additional infrastructure.

**Impact**: Multi-node deployments must use:
- Load balancer for request distribution
- Custom distributed registry (experimental: erlmcp_registry_dist)
- Client-side service discovery

**Documentation Reference**: docs/CLUSTERING.md

**Not a Bug**: This is the expected behavior of gproc. Distributed routing is documented as experimental.

---

### A4. No Built-in Database

**Limitation**: erlmcp does not include a built-in database for persistence.

**Reason**: MCP protocol is stateless. Persistence is application-specific.

**Impact**: Applications requiring persistence must:
- Use external Redis/Memcached for session storage
- Implement custom storage backends
- Use DETS/Mnesia for single-node persistence (documented)

**Documentation Reference**: docs/SESSION_PERSISTENCE.md

**Not a Bug**: MCP is a stateless protocol. Persistence is intentionally application-managed.

---

## Transport-Specific Limitations

### T1. TCP and WebSocket are Experimental

**Limitation**: TCP and WebSocket transports are marked as **experimental**.

**Reason**: These transports are not part of the core MCP specification.

**Impact**:
- No SLA commitment for experimental transports
- Breaking changes may occur without major version bump
- Production use should use STDIO/HTTP/SSE instead

**Documentation Reference**: README.md section "Transport Maturity"

**Not a Bug**: Experimental status is clearly documented. Stable transports are available.

---

### T2. SSE Priming Event Semantics

**Limitation**: SSE connection requires handling a "priming event" before sending requests.

**Reason**: MCP specification requires an initial empty event with endpoint metadata.

**Impact**: Clients that ignore the priming event may fail to connect properly.

**Documentation Reference**: docs/SSE_TRANSPORT.md

**Not a Bug**: This is spec-defined behavior. Documentation explains the requirement.

---

### T3. SSE Event Buffer Limit

**Limitation**: SSE reconnection buffers only the last 100 events by default.

**Reason**: In-memory buffering has limits to prevent unbounded memory growth.

**Impact**: Clients that disconnect for extended periods may lose events beyond the buffer.

**Mitigation**: Configure larger buffer or use durable storage backend (DETS/Mnesia).

**Documentation Reference**: docs/SESSION_PERSISTENCE.md

**Not a Bug**: Buffer limit is configurable. Durable backends are available.

---

### T4. Windows CRLF Line Endings

**Limitation**: STDIO transport on Windows may inject CRLF instead of LF.

**Reason**: Windows line ending defaults differ from Unix.

**Impact**: JSON parsing may fail if not configured for binary mode.

**Mitigation**: Configure `eol: lf` in transport options.

**Documentation Reference**: docs/WINDOWS_DEPLOYMENT.md

**Not a Bug**: Configuration option exists. This is a platform-specific consideration.

---

## Configuration Limitations

### C1. Session Backend Default is ETS (Non-Durable)

**Limitation**: Default session storage uses ETS (in-memory, non-durable).

**Reason**: ETS provides best performance for stateless operations.

**Impact**: Pod/VM restart loses session state unless configured otherwise.

**Mitigation**: Configure DETS (single-node) or Mnesia (distributed) for persistence.

**Documentation Reference**: docs/SESSION_PERSISTENCE.md

**Not a Bug**: Performance-optimized default. Persistence is a configuration option.

---

### C2. Circuit Breaker Default Threshold

**Limitation**: Default circuit breaker trips after 5 failures in 60 seconds.

**Reason**: Balances sensitivity vs. tolerance for transient failures.

**Impact**: High-latency backends may trip the breaker prematurely.

**Mitigation**: Configure `circuit_breaker_threshold` and `circuit_breaker_timeout`.

**Documentation Reference**: docs/RESILIENCE.md

**Not a Bug**: Configurable threshold. Default is optimized for typical workloads.

---

### C3. Health Check Timeout is 5 Seconds

**Limitation**: Health checks timeout after 5 seconds.

**Reason**: Prevents hanging health checks from blocking readiness.

**Impact**: Systems under extreme load may fail health checks due to timeout.

**Mitigation**: Increase timeout or reduce load for health check endpoints.

**Documentation Reference**: docs/RESILIENCE.md

**Not a Bug**: Timeout is configurable. Default follows industry best practices.

---

## Deployment Limitations

### D1. Maximum 1000 Concurrent Connections (Cloud Run)

**Limitation**: Cloud Run deployment has a practical limit of ~1000 concurrent connections.

**Reason**: Cloud Run has maximum instance limits and connection overhead.

**Impact**: Beyond 1000 concurrent connections, use GKE or Compute Engine.

**Mitigation**: GKE supports 10,000+ connections per node; Compute Engine supports 5,000+.

**Documentation Reference**: docs/SCALABILITY.md

**Not a Bug**: This is a Cloud Run platform limitation, not an erlmcp defect.

---

### D2. Regional Deployment Only (v1.0)

**Limitation**: Marketplace listing initially supports specific regions only.

**Reason**: Gradual rollout to ensure quality across regions.

**Impact**: Users in unsupported regions cannot deploy directly.

**Supported Regions (v1.0)**:
- us-central1 (Iowa)
- us-east1 (South Carolina)
- europe-west1 (Belgium)
- asia-southeast1 (Singapore)

**Documentation Reference**: Marketplace deployment configuration

**Not a Bug**: Regional expansion is planned. Initial regions ensure quality.

---

### D3. No Multi-Tenant Support in Base Image

**Limitation**: Base container image does not include multi-tenant isolation features.

**Reason**: Multi-tenancy requires additional security infrastructure.

**Impact**: Single tenant per deployment. Multiple tenants require separate deployments.

**Mitigation**: Enterprise edition includes multi-tenant features (roadmap).

**Documentation Reference**: docs/ENTERPRISE.md (future)

**Not a Bug**: Single-tenant architecture is simpler and more secure for initial release.

---

## Observability Limitations

### O1. No Native Grafana Dashboard Included

**Limitation**: Deployment does not include pre-built Grafana dashboards.

**Reason**: Google Cloud Monitoring is the primary observability platform.

**Impact**: Users preferring Grafana must build custom dashboards.

**Mitigation**: Cloud Monitoring dashboards are pre-configured. Grafana integration is documented.

**Documentation Reference**: docs/OBSERVABILITY.md

**Not a Bug**: Google Cloud has native observability. External tools are optional.

---

### O2. Log Retention is GCP-Managed

**Limitation**: erlmcp does not manage log retention. This is GCP Cloud Logging behavior.

**Reason**: Log management is a platform concern in cloud-native deployments.

**Impact**: Log retention depends on GCP project configuration.

**Mitigation**: Configure log retention in Cloud Logging settings.

**Documentation Reference**: GCP Cloud Logging documentation

**Not a Bug**: Cloud-native design leverages platform capabilities.

---

### O3. Metrics Sampling Rate is Fixed

**Limitation**: Metrics collection occurs at fixed intervals (default: 10 seconds).

**Reason**: Balances granularity with overhead.

**Impact**: Sub-10-second metrics may not be visible.

**Mitigation**: Configure sampling rate in observability settings.

**Documentation Reference**: docs/OBSERVABILITY.md

**Not a Bug**: Configurable sampling. Default is optimized for typical workloads.

---

## Security Limitations

### S1. No Built-in Authentication/Authorization

**Limitation**: erlmcp does not include authentication/authorization for the MCP protocol itself.

**Reason**: MCP is a transport protocol. Security is typically enforced at the infrastructure layer (TLS, IAM, network policies).

**Impact**: Applications requiring application-level auth must implement it themselves.

**Mitigation**: Use infrastructure security:
- TLS for encryption in transit
- GCP IAM for access control
- Network policies for isolation
- Identity-Aware Proxy for authentication

**Documentation Reference**: docs/SECURITY.md

**Not a Bug**: MCP protocol does not define authentication. Security is infrastructure-managed.

---

### S2. mTLS Requires Additional Configuration

**Limitation**: mTLS (mutual TLS) requires manual certificate management.

**Reason**: mTLS certificates are customer-specific and cannot be automated.

**Impact**: Default deployment uses TLS only. mTLS is opt-in.

**Mitigation**: Follow mTLS configuration guide in docs/SECURITY.md

**Documentation Reference**: docs/SECURITY.md section "mTLS Configuration"

**Not a Bug**: mTLS is an advanced security feature that requires customer-specific certificates.

---

### S3. No Automated Certificate Rotation

**Limitation**: TLS certificates do not automatically rotate.

**Reason**: Certificate rotation is application-specific and requires coordination.

**Impact**: Certificates must be manually rotated before expiration.

**Mitigation**: Set up certificate rotation procedures or use managed certificates (GCP)

**Documentation Reference**: docs/SECURITY.md section "Certificate Management"

**Not a Bug**: Certificate rotation is an operational procedure, not a runtime feature.

---

## Marketplace-Specific Limitations

### M1. Schema Enum Mismatch (Known Issue)

**Limitation**: `deployment_type` enum values differ between `application.yaml` and `schema.yaml`.

**Values**:
- `application.yaml`: `gke`, `cloud-run`, `compute-engine`
- `schema.yaml`: `gke`, `cloudrun`, `gce`

**Impact**: Conditional visibility rules may not match actual enum values.

**Status**: **KNOWN - P0 FIX REQUIRED**

**Resolution**: Consolidate enum values across both files before Marketplace submission.

**Not a Bug**: This IS a bug, but documented here to prevent confusion during testing.

---

### M2. Terraform Output Gaps

**Limitation**: Some schema outputs are claimed but not implemented in Terraform modules.

**Missing Outputs**:
- `dashboard_urls` (claimed but not produced by any module)
- `service_url` for GKE (only `cluster_endpoint` is output)

**Impact**: Marketplace success screen will show placeholder values.

**Status**: **KNOWN - P1 FIX REQUIRED**

**Resolution**: Implement missing outputs or remove from schema.

**Not a Bug**: This IS a bug, but documented here to prevent confusion during testing.

---

### M3. Orphaned Terraform Variables

**Limitation**: Some Terraform variables have no corresponding schema properties.

**Examples**:
- `project_id` (required but not in user schema)
- `kubernetes_version` (not exposed to users)
- `disk_size_gb`, `disk_type` (not configurable)

**Impact**: These must be set via environment or Marketplace defaults.

**Status**: **ACCEPTED - By Design**

**Reason**: Some values are derived from deployment context (e.g., `project_id` from Marketplace)

**Not a Bug**: This is expected Marketplace behavior. Some values are auto-populated.

---

## Testing Limitations

### X1. No E2E Tests in Marketplace Package

**Limitation**: Marketplace deployment package does not include end-to-end tests.

**Reason**: Tests are separate from deployment artifacts.

**Impact**: Users must run tests manually or via CI/CD.

**Mitigation**: Test suite is available in the main repository.

**Documentation Reference**: test/ directory in main repository

**Not a Bug**: Tests are development tools, not deployment artifacts.

---

### X2. Chaos Engineering Features are Optional

**Limitation**: Chaos engineering tools are not installed by default.

**Reason**: Chaos testing is development-specific, not production-required.

**Impact**: Production deployments are "cleaner" but require separate chaos testing setup.

**Mitigation**: Install chaos tools manually for testing scenarios.

**Documentation Reference**: docs/CHAOS_ENGINEERING.md

**Not a Bug**: Production deployments should not include testing tools.

---

## Documentation Limitations

### D1. Screenshots May Lag UI Changes

**Limitation**: Documentation screenshots may not match the latest GCP console UI.

**Reason**: GCP console UI changes frequently. Screenshots become outdated.

**Impact**: Users may see minor UI differences from documentation.

**Status**: **ACCEPTED - Ongoing Maintenance**

**Mitigation**: Documentation focuses on concepts rather than exact UI appearance.

**Not a Bug**: This is inherent to cloud platform documentation. Screenshot updates are prioritized lower than functional accuracy.

---

### D2. Examples Use Placeholder Values

**Limitation**: Code examples use placeholder values (e.g., `your-project-id`).

**Reason**: Examples must be generic to be reusable.

**Impact**: Users must substitute values with their own.

**Mitigation**: Examples clearly indicate which values need replacement.

**Not a Bug**: Placeholder values are required for reusable documentation.

---

## Performance Limitations

### P1. Connection Limits Per Node

**Limitation**: Honest capacity is 40-50K concurrent connections per node.

**Reason**: Erlang VM memory and scheduler limits per node.

**Impact**: Beyond 50K connections, clustering is required.

**Mitigation**: Multi-node deployments support 1M+ connections.

**Documentation Reference**: README.md section "Performance"

**Not a Bug**: Documented capacity is honest and achievable. Clustering provides horizontal scaling.

---

### P2. JSON Operations Limited to Native Module

**Limitation**: JSON operations use OTP 28 native json module only.

**Reason**: jsx dependency was removed to reduce dependencies.

**Impact**: No fallback to older JSON libraries.

**Mitigation**: Requires OTP 28.3.1+ (already a requirement)

**Not a Bug**: Native json is faster and eliminates external dependency.

---

## Version-Specific Limitations

### V1. Limitations Removed in v3.0

The following limitations existed in v2.x but were **intentionally removed** in v3.0:

| Limitation | v2.x Status | v3.0 Status | Reason |
|------------|-------------|-------------|--------|
| OTP 25-27 support | Supported | Removed | Use OTP 28+ native features |
| jsx dependency | Required | Removed | Use native json module |
| Backward compatibility shim | Included | Removed | Reduce codebase by ~1,358 lines |

**Not a Bug**: These are breaking changes announced in v3.0 release notes.

---

## Future Roadmap

### Planned Features (Not Yet Implemented)

The following features are planned for future releases but are **not limitations**:

| Feature | Target Release | Status |
|---------|---------------|--------|
| Distributed registry (GA) | v3.1 | Experimental in v3.0 |
| Multi-tenant support | v4.0 | Roadmap |
| Advanced RBAC | v3.2 | Design phase |
| Grafana dashboards | v3.1 | In progress |
| Automated cert rotation | v3.2 | Design phase |

**Not a Bug**: Roadmap items are planned features, not missing functionality.

---

## Reviewer Guidance

### How to Evaluate These Limitations

When reviewing erlmcp for Marketplace approval:

1. **Distinguish Between Bugs and Design Decisions**
   - Bugs: M1, M2 (schema mismatches) - require fix
   - Design: A1-A4, C1-C3, S1-S3 - accepted constraints

2. **Check Documentation Coverage**
   - Each limitation should have documentation reference
   - Mitigations should be documented where applicable

3. **Verify Marketplace Requirements Still Met**
   - Despite limitations, core Marketplace requirements must pass
   - Deployment, security, monitoring are not compromised by accepted limitations

4. **Consider Use Case**
   - STDIO limitations don't affect HTTP/SSE users
   - Regional limitations don't prevent deployment in supported regions

### Rejection Wording to Avoid

Instead of:
> "Rejected: No built-in database"

Use:
> "Request clarification: Document session persistence options in README"

Instead of:
> "Rejected: TCP transport is experimental"

Use:
> "Note: Ensure experimental transport status is clearly documented"

Instead of:
> "Rejected: Maximum 1000 connections"

Use:
> "Note: Document scaling options beyond Cloud Run limits"

---

## Summary

| Category | Count | Require Fix? |
|----------|-------|--------------|
| Architectural | 4 | No (design decisions) |
| Transport-Specific | 4 | No (spec or platform constraints) |
| Configuration | 3 | No (configurable defaults) |
| Deployment | 3 | No (platform or roadmap) |
| Observability | 3 | No (cloud-native design) |
| Security | 3 | No (infrastructure-managed) |
| Marketplace-Specific | 3 | Yes (M1, M2 need fix) |
| Testing | 2 | No (separation of concerns) |
| Documentation | 2 | No (ongoing maintenance) |
| Performance | 2 | No (honest capacity) |

**Total**: 26 documented limitations
**Requiring Fix**: 2 (M1, M2)
**Accepted Design Decisions**: 24

---

**Document Version**: 1.0.0
**Last Updated**: 2026-02-02
**Next Review**: After each major version release
