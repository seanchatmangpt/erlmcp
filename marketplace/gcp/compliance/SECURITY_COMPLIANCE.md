# Security and Compliance Documentation for erlmcp on GCP Marketplace

## Overview

This document outlines the security architecture and compliance posture of erlmcp when deployed on Google Cloud Platform via the Cloud Marketplace.

## Security Architecture

### Zero-Trust Security Model

erlmcp implements a zero-trust security architecture:
- **Never trust, always verify**: All requests are authenticated and authorized
- **Least privilege**: Minimal IAM roles granted
- **Assume breach**: Defense in depth with multiple security layers

### Security Layers

#### 1. Network Security

**Private Clusters (GKE)**
```yaml
# Control plane private endpoint
enablePrivateEndpoint: true
# Nodes without public IPs
enablePrivateNodes: true
# Authorized networks for master access
masterAuthorizedNetworks:
  - 10.0.0.0/8
```

**Network Policies (Calico)**
```yaml
# Default deny all ingress
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: default-deny-ingress
spec:
  podSelector: {}
  policyTypes:
    - Ingress
```

**VPC Service Controls**
- Enforce perimeter boundaries
- Control egress to external services
- Prevent data exfiltration

#### 2. Identity and Access Management

**Workload Identity (GKE)**
```yaml
# KSA to GSA mapping
iam.gke.io/gcp-service-account: "erlmcp@project.iam.gserviceaccount.com"
```

**IAM Roles**
```hcl
# Minimal roles for GKE nodes
resource "google_project_iam_member" "node_roles" {
  for_each = toset([
    "roles/logging.logWriter",
    "roles/monitoring.metricWriter",
    "roles/secretmanager.secretAccessor",
    "roles/stackdriver.resourceWriter",
  ])
  project = var.project_id
  role    = each.value
  member  = "serviceAccount:${google_service_account.nodes.email}"
}
```

**Service Account Impersonation**
```bash
# Use gcloud CLI with impersonation
gcloud config set auth/impersonate_service_account erlmcp@project.iam.gserviceaccount.com
```

#### 3. Secrets Management

**Google Secret Manager Integration**
```yaml
# Secrets stored in Secret Manager
secrets:
  erlang_cookie:
    type: SecretManager
    project: my-project
    name: erlmcp-erlang-cookie
    version: latest
```

**Secret Rotation**
```yaml
# Automatic rotation policy
rotation:
  period: 7776000s  # 90 days
  rotation_before_expiry: 2592000s  # 30 days
```

**Secret Access Logging**
```yaml
# Audit all secret access
logging:
  secret_access:
    audit: true
    include_version: true
```

#### 4. Data Encryption

**Encryption at Rest (GCP Default)**
- Compute Engine: AES-256 by default
- Cloud Storage: AES-256 by default
- Secret Manager: AES-256 by default
- Customer Managed Keys (CMEK) supported

**Encryption in Transit**
- TLS 1.3 minimum for external communication
- mTLS for service-to-service communication
- TLS for database connections

```yaml
tls:
  minVersion: "1.3"
  cipherSuites:
    - "TLS_AES_128_GCM_SHA256"
    - "TLS_AES_256_GCM_SHA384"
    - "TLS_CHACHA20_POLY1305_SHA256"
```

#### 5. Container Security

**Image Security**
```yaml
# Non-root user
USER erlmcp
# Read-only root filesystem
RUN chmod -R a-w /usr/local/bin
# Minimal base image
FROM alpine:3.20
```

**Vulnerability Scanning**
```bash
# Scan image for vulnerabilities
gcloud artifacts docker images scan \
  us-central1-docker.pkg.dev/project/erlmcp/erlmcp:3.0.0 \
  --severity-critical --format=json
```

**Runtime Security**
```yaml
# Security context
securityContext:
  runAsNonRoot: true
  runAsUser: 1000
  runAsGroup: 1000
  readOnlyRootFilesystem: true
  allowPrivilegeEscalation: false
  capabilities:
    drop:
      - ALL
```

#### 6. Application Security

**Input Validation**
- JSON-RPC request validation
- Schema enforcement
- Size limits on payloads

**Rate Limiting**
```yaml
rateLimit:
  enabled: true
  requests: 100
  window: 1m
  burst: 10
```

**Authentication**
```yaml
authentication:
  methods:
    - jwt
    - tls
    - api_key
  jwt:
    algorithm: RS256
    issuer: erlmcp
    audience: erlmcp-users
```

## Compliance Frameworks

### SOC 2 Type II

**Control Implementation**
- **CC6.1**: Logical and physical access controls
- **CC6.6**: Confidentiality of information
- **CC7.2**: System monitoring
- **CC8.1**: Incident response

### ISO 27001

**Controls**
- **A.12.3**: Backup
- **A.13.1**: Network security
- **A.14.1**: Information transfer
- **A.18.1**: Compliance with legal requirements

### HIPAA (Optional)

**Protected Health Information (PHI) Handling**
```yaml
hipaa:
  enabled: false
  phi_encryption: true
  audit_log_retention: 6 years
  business_associate_agreement: required
```

### GDPR

**Data Protection**
```yaml
gdpr:
  data_residency: EU  # For EU deployments
  data_portability: enabled
  right_to_erasure: supported
  consent_management: required
```

### PCI DSS (If handling payment data)

```yaml
pci_dss:
  enabled: false
  saq_a: supported
  pan_storage: prohibited
  cvv_storage: prohibited
```

## Security Controls Matrix

| Control | GKE | Cloud Run | Compute Engine |
|---------|-----|-----------|----------------|
| Private Cluster | ✓ | ✓ (internal ingress) | ✓ (VPC) |
| Workload Identity | ✓ | ✓ | ✓ |
| Secret Manager | ✓ | ✓ | ✓ |
| Network Policies | ✓ | N/A | Firewall Rules |
| Binary Authorization | ✓ | ✓ | N/A |
| Shielded Nodes | ✓ | N/A | Shielded VM |
| Confidential Computing | ✓ (Confidential GKE) | N/A | Confidential VM |

## Audit Logging

### Cloud Audit Logs

**Admin Activity Logs**
- Always enabled, no cost
- Tracks administrative operations
- Includes identity and access changes

**Data Access Logs**
- Enabled for sensitive operations
- Logs access to:
  - Secret Manager secrets
  - Cloud Storage objects
  - BigQuery tables

```yaml
# Enable data access logs
gcloud projects update project-id \
  --enable-data-access-logs
```

### Application Audit Logs

```json
{
  "timestamp": "2024-01-01T00:00:00Z",
  "event_type": "rpc.request",
  "user_id": "user@example.com",
  "resource": "/mcp/tools/call",
  "status": "success",
  "latency_ms": 45
}
```

### Log Retention

```yaml
log_retention:
  audit_logs: 400 days  # SOC 2 requirement
  access_logs: 90 days
  application_logs: 30 days
```

## Incident Response

### Incident Categories

1. **Critical**: Service outage, data breach
2. **High**: Significant degradation, security incident
3. **Medium**: Partial degradation, minor security issue
4. **Low**: Cosmetic issues, documentation errors

### Response Times

| Severity | Response Time | Resolution Time |
|----------|---------------|-----------------|
| Critical | 15 minutes | 4 hours |
| High | 1 hour | 24 hours |
| Medium | 4 hours | 3 days |
| Low | 1 business day | 1 week |

### Escalation Matrix

```
Level 1: Automated systems
Level 2: On-call engineer
Level 3: Engineering lead
Level 4: CTO/VP Engineering
```

## Penetration Testing

### Testing Scope

- Application layer (JSON-RPC API)
- Authentication and authorization
- Input validation
- Session management
- Error handling

### Testing Restrictions

- No DDoS testing
- No social engineering
- No physical access testing
- Coordinate with GCP Security Team

### Testing Tools

- OWASP ZAP
- Burp Suite
- Nessus
- Custom security scans

## Vulnerability Management

### Vulnerability Disclosure

**Responsible Disclosure Process**
1. Report vulnerability via security@erlmcp.dev
2. Initial response within 48 hours
3. Remediation timeline provided
4. Coordinated disclosure

### SLA for Remediation

| Severity | SLA |
|----------|-----|
| Critical | 48 hours |
| High | 7 days |
| Medium | 30 days |
| Low | 90 days |

### Patch Management

```yaml
patch_management:
  automatic_updates: false  # Manual testing required
  security_patches:
    priority: high
    testing: required
    rollout: canary
  regular_updates:
    frequency: monthly
    maintenance_window: Sunday 3:00 AM UTC
```

## References

- [GCP Security Best Practices](https://cloud.google.com/security/best-practices)
- [GCP Compliance](https://cloud.google.com/compliance)
- [GKE Security](https://cloud.google.com/kubernetes-engine/docs/concepts/security)
- [Secret Manager](https://cloud.google.com/secret-manager/docs)
- [Workload Identity](https://cloud.google.com/kubernetes-engine/docs/how-to/workload-identity)
