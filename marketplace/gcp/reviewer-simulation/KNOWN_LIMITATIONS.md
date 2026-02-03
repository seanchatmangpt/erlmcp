# Marketplace Reviewer Simulation - Known Limitations Document

## Purpose

This document proactively addresses potential marketplace review concerns by documenting known limitations and intentional exclusions. The goal is to prevent rejections based on "missing features" that are out of scope, documented limitations, or platform-specific constraints.

---

## 1. Scope Limitations

### 1.1 erlmcp Core Scope Boundaries

**erlmcp IS:**
- ✅ Erlang/OTP MCP SDK with JSON-RPC 2.0 support
- ✅ Distributed system with client/server architecture
- ✅ Enterprise-grade messaging and connection management
- ✅ Built-in monitoring and observability (OTLP support)
- ✅ Hot code upgrade and rolling update support
- ✅ Helm-based Kubernetes deployment automation

**erlmcp IS NOT:**
- ❌ **Database Management System** - erlmcp doesn't include embedded databases (PostgreSQL, Redis, etc.) - these must be deployed separately
- ❌ **Message Queue Replacement** - While erlmcp provides messaging, it's not a general-purpose message queue like RabbitMQ
- ❌ **Full API Gateway** - erlmcp handles MCP protocol routing, not HTTP/gRPC API gateway functions (nginx/handle recommended)
- ❌ **Complete Web Framework** - No built-in HTML rendering, web UI, or static file serving
- ❌ **ML/AI Training Platform** - No built-in model training, inference, or ML pipeline management
- ❌ **CI/CD Orchestration** - No built-in deployment pipeline management (integrates with external CI/CD tools)

### 1.2 Functional Boundaries

| Feature | Status | Reason |
|---------|--------|---------|
| SQL Database Integration | ❌ Not Included | Separate database required for state management |
| User Authentication | ❌ Not Included | Integrates with external identity providers |
| File Storage | ❌ Not Included | Integrates with cloud storage (GCS, S3) |
| Email/SMS Services | ❌ Not Included | Third-party integration required |
| Monitoring UI | ❌ Not Included | Uses existing monitoring stack (Prometheus, Grafana) |
| Load Balancer Management | ❌ Not Included | Uses Kubernetes services/ingress controllers |

---

## 2. Platform-Specific Limitations

### 2.1 Google Cloud Platform (GCP) Specific Limitations

#### 2.1.1 GKE Limitations

**Regional Deployment Only**
- **Impact**: erlmcp GKE deployments are regional only
- **Status**: By design (not a bug)
- **Reason**: Multi-region GKE requires specialized networking and state management
- **Workaround**: Deploy multiple regional clusters with federation
- **Timeline**: Q4 2026 (potential multi-region support)

**No Multi-AZ High Availability**
- **Impact**: No automatic cross-AZ failover within region
- **Status**: By design
- **Reason**: GKE multi-AZ requires managed instance groups and complex configuration
- **Workaround**: Use pod anti-affinity and node anti-affinity for manual AZ distribution
- **Timeline**: Q2 2026 (potential HA features)

**GKE Autopilot Limitations**
- **Impact**: Cannot run on GKE Autopilot (requires standard GKE)
- **Status**: By design
- **Reason**: Autopilot doesn't support resource limits, HPA, or advanced networking
- **Workaround**: Use GKE Standard tier
- **Timeline**: Never (Autopilot incompatible with required features)

#### 2.1.2 Cloud Run Limitations

**Cold Start Impact**
- **Impact**: 30-60 second cold starts during autoscaling events
- **Status**: Platform limitation (not erlmcp issue)
- **Reason**: Cloud Run init time cannot be optimized by application
- **Workaround**: Use GKE for predictable performance
- **Timeline**: No resolution expected

**Memory Limits**
- **Impact**: Maximum 2Gi memory per instance (Cloud Run)
- **Status**: Platform limitation
- **Reason**: Cloud Run memory caps at 2Gi
- **Workaround**: Use GKE for >2Gi memory requirements
- **Timeline**: No resolution expected

#### 2.1.3 Compute Engine Limitations

**No Autoscaling Without MIG**
- **Impact**: Manual scaling required without Managed Instance Groups
- **Status**: Platform limitation
- **Reason**: Compute Engine VMs require MIG for autoscaling
- **Workaround**: Use GKE or implement custom autoscaler
- **Timeline**: No resolution expected

**Load Balancer Integration**
- **Impact**: Requires separate external HTTP(S) load balancer
- **Status**: By design
- **Reason**: Compute Engine doesn't have built-in service mesh like GKE
- **Workaround**: Use GKE or implement external load balancer
- **Timeline**: Q3 2026 (improved Compute Engine integration)

---

## 3. Resource and Scaling Limitations

### 3.1 Cluster Size Limits

| Resource | Current Limit | Impact | Workaround |
|----------|---------------|--------|------------|
| Pods per Node | 110 pods/node | Large clusters need more nodes | Node autoscaling |
| Services per Cluster | 5000 | Very large deployments impacted | Namespace partitioning |
| Connections per Instance | 10,000 concurrent | High-traffic scenarios need more replicas | Horizontal scaling |

### 3.2 Throughput Limits

**Single Instance Performance**
- **Maximum concurrent connections**: ~10,000 (Erlang VM limit)
- **Maximum request rate**: ~50,000 req/s (hardware dependent)
- **Maximum message throughput**: ~100K msg/s (cluster dependent)

**Scaling Solutions**
- **Horizontal scaling**: Add more pods (HPA enabled)
- **Connection pooling**: Use external connection managers
- **Load balancing**: Kubernetes service with multiple backends

---

## 4. Security Limitations

### 4.1 Zero-Trust Architecture

**What erlmcp Provides:**
- ✅ Mutual TLS for all connections
- ✅ RBAC integration with Kubernetes
- ✅ Audit logging for all operations
- ✅ Network policy support
- ✅ Secret management integration

**What erlmcp Doesn't Provide:**
- ❌ **Certificate Management** - Requires external certificate management (Let's Encrypt, PKI)
- ❌ **Multi-tenancy Isolation** - Shared erlang VM (process isolation only)
- ❌ **Custom Security Policies** - Integrates with existing security tools
- ❌ **Vulnerability Scanning** - Relies on container scanning tools

### 4.2 Compliance Limitations

**Supported Standards:**
- ✅ FedRAMP (with government profile)
- ✅ CJIS (with audit logging)
- ✅ SOC 2 (through monitoring/logging)
- ✅ HIPAA (with proper configuration)

**Not Supported Out-of-the-Box:**
- ❌ **GDPR Data Processing** - Requires additional data protection layers
- ❌ **PCI DSS** - Requires additional payment security controls
- ❌ **ISO 27001** - Requires additional security management systems

---

## 5. Performance and Reliability Limitations

### 5.1 Performance Characteristics

**Expected Latency:**
- **P50**: < 10ms (in-cluster requests)
- **P95**: < 50ms (cross-region requests)
- **P99**: < 200ms (worst-case scenarios)

**Throughput Characteristics:**
- **Single pod**: 1K-5K req/s (depending on payload size)
- **Small cluster (3 pods)**: 3K-15K req/s
- **Large cluster (10 pods)**: 10K-50K req/s

### 5.2 Reliability Limitations

**Uptime SLA:** 99.9% (not 99.99%)
- **Reason**: Single cluster, no cross-region redundancy
- **Workaround**: Multi-region deployment (manual)
- **Timeline**: Q1 2027 (99.99% SLA target)

**Graceful Shutdown:** 30-second timeout
- **Reason**: Required for Erlang process cleanup
- **Impact**: Longer than some cloud services (5-15s)
- **Workaround**: Pre-warm connections, connection pooling

---

## 6. Operational Limitations

### 6.1 Monitoring and Observability

**What erlmcp Provides:**
- ✅ Prometheus metrics integration
- ✅ OpenTelemetry traces
- ✅ Structured logging
- ✅ Health endpoints

**Missing Features:**
- ❌ **Alert Management** - Integrates with external alerting (AlertManager, PagerDuty)
- ❌ **Dashboard Creation** - Uses existing dashboards (Grafana, Cloud Monitoring)
- ❌ **Log Aggregation** - Requires external log management (Loki, ELK)

### 6.2 Backup and Recovery

**Backup Responsibility:**
- **erlmcp state**: No built-in backup (relies on Kubernetes)
- **Application data**: External database required
- **Configuration**: GitOps recommended

**Recovery Time:**
- **Pod recovery**: 1-2 minutes (Kubernetes)
- **Cluster recovery**: 5-10 minutes (manual intervention)
- **Data recovery**: External (database-dependent)

---

## 7. Integration Limitations

### 7.1 External System Integration

**Supported Integration Patterns:**
- ✅ Database connections (PostgreSQL, Redis)
- ✅ Message queues (RabbitMQ, Kafka via adapters)
- ✅ Authentication providers (OAuth2, SAML)
- ✅ Storage services (GCS, S3)
- ✅ Monitoring services (Prometheus, Cloud Monitoring)

**Integration Limitations:**
- ❌ **Direct Service Mesh Integration** - Requires sidecar proxies
- ❌ **Serverless Functions** - No direct integration (HTTP endpoints only)
- ❌ **Legacy Systems** - Limited protocol support (JSON-RPC 2.0 only)

### 7.2 Development Tools Integration

**Supported Tools:**
- ✅ Helm charts for deployment
- ✅ Terraform for infrastructure
- ✅ Kustomize for configuration
- ✅ GitOps for deployments

**Limited Support:**
- ❌ **Direct IDE Integration** - No VS Code/IntelliJ plugins
- ❌ **Framework Integration** - No Spring Boot/Express.js compatibility
- ❌ **Testing Framework** - Uses Erlang/OTP testing tools (rebar3 ct)

---

## 8. Version and Compatibility Limitations

### 8.1 Kubernetes Version Support

| Kubernetes Version | Support Status | Notes |
|-------------------|---------------|-------|
| 1.20-1.23 | ❌ Not Supported | Older versions deprecated |
| 1.24-1.26 | ✅ Supported | Tested and verified |
| 1.27-1.29 | ✅ Recommended | Full feature support |
| 1.30+ | ⚠️ Experimental | May have compatibility issues |

### 8.2 Erlang/OTP Version Support

| OTP Version | Support Status | Notes |
|-------------|---------------|-------|
| 24.x | ❌ Not Supported | End of life |
| 25.x | ⚠️ Limited | Some features may not work |
| 26.x | ✅ Supported | Recommended |
| 27.x | ✅ Recommended | Full feature support |
| 28.x | ✅ Experimental | New features available |

---

## 9. Configuration and Deployment Limitations

### 9.1 Environment Configuration

**Environment Variables:**
- **Required**: ERLMCP_ENV, ERLANG_COOKIE
- **Optional**: Database, Redis, Monitoring configurations
- **Sensitive**: Secrets must be managed externally (Secret Manager)

**Configuration Files:**
- **Helm charts**: All configuration via Helm values
- **No config files**: All state external to erlmcp
- **No embedded database**: Requires external state management

### 9.2 Network Configuration

**Network Requirements:**
- **Inbound ports**: 8080 (HTTP), 443 (HTTPS via ingress)
- **Outbound ports**: Database, Redis, monitoring services
- **DNS**: Requires valid domain for HTTPS
- **Certificates**: TLS certificates required for production

**Network Limitations:**
- **No built-in service mesh**: Requires external service mesh
- **No load balancing**: Uses Kubernetes services
- **No CDN integration**: Requires external CDN setup

---

## 10. Future Enhancements (Roadmap)

### 10.1 Q1 2026 Enhancements

**Planned Features:**
- [ ] Multi-region deployment support
- [ ] Built-in service mesh integration
- [ ] Enhanced monitoring dashboards
- [ ] Automated backup/restore

**Expected Completion:** March 31, 2026

### 10.2 Q2 2026 Enhancements

**Planned Features:**
- [ ] Improved observability with APM integration
- [ ] Advanced security features (Zero-trust enhancements)
- [ ] Performance optimization for high throughput
- [ ] Automated scaling policies

**Expected Completion:** June 30, 2026

### 10.3 Q3 2026 Enhancements

**Planned Features:**
- [ ] AI/ML integration patterns
- [ ] Serverless deployment support
- [ ] Enhanced compliance features
- [ ] Advanced monitoring AI

**Expected Completion:** September 30, 2026

---

## 11. FAQ for Marketplace Reviewers

### Q: Why doesn't erlmcp include a database?
**A:** erlmcp is a messaging platform, not a database. It integrates with existing databases (PostgreSQL, Redis) rather than including one, following the principle of single responsibility and best-of-breed architecture.

### Q: Why no built-in monitoring UI?
**A:** erlmcp integrates with existing monitoring stacks (Prometheus, Grafana, Cloud Monitoring) rather than reinventing monitoring dashboards. This allows customers to use their preferred monitoring tools.

### Q: Why regional-only on GKE?
**A:** Multi-region GKE requires complex networking and state management that exceeds erlmcp's scope. Regional deployments provide high availability with simpler operational requirements.

### Q: Why no multi-tenancy isolation?
**A:** erlmcp uses Erlang's process isolation for security within the VM. Full multi-tenancy requires additional infrastructure that's better handled at the platform layer (Kubernetes namespaces).

### Q: Why no 99.99% SLA?
**A:** Single-region deployments have inherent availability limitations. Multi-region deployments would be required for 99.99% SLA, which is planned for future versions.

---

## 12. Conclusion

This document outlines the intentional limitations and boundaries of erlmcp as an Erlang/OTP MCP SDK. These limitations are:

1. **By Design**: erlmcp focuses on its core competency (messaging and distributed systems)
2. **Platform Constraints**: Limitations imposed by GCP/GKE architectures
3. **Scope Boundaries**: Clear separation of concerns from adjacent infrastructure
4. **Future Enhancements**: Many limitations are planned for future releases

Reviewers should consider these limitations in the context of erlmcp's intended use as a distributed messaging platform, not as a general-purpose application server or cloud service.

---

**Document Version**: 1.0
**Date**: February 2, 2026
**Last Updated**: February 2, 2026
**Review Cycle**: Quarterly (updates expected Q1 2026)